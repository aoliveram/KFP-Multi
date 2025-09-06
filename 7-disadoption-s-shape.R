## ================================================================
## Stable disadoption S-curve from TOA_derivado_general
## Requirements:
## - Object TOA_derivado_general already in the workspace
## - Uses the first 11 time steps (t1..t11), to match adoption analyses
## - "Modern" = c("Loop","Oral Pill","Condom","Vasectomy","TL","Injection")
## Output:
## - Console summary
## - PDF: disadoption_s_curve.pdf
## ================================================================

# --- 0) Safety checks
TOA_derivado_general <- readRDS("TOA_derivado_general.rds")
obj <- TOA_derivado_general

stopifnot(
  all(c("meta","states","codes","periods","status_code") %in% names(obj)),
  "label_to_code" %in% names(obj$codes),
  "code_to_label" %in% names(obj$codes)
)

# --- 1) Basic handles
id_vec        <- obj$meta$specific_id
label_to_code <- obj$codes$label_to_code         # Named numeric: names are labels
code_to_label <- obj$codes$code_to_label         # Named character: names are codes as chars
all_states    <- obj$states

# Use t1..t11 (match prior work)
Tt <- 11L
status_mat_full <- obj$status_code               # 1047 x 12 (t1..t12) per your printout
stopifnot(ncol(status_mat_full) >= Tt)
mat <- status_mat_full[, 1:Tt, drop = FALSE]     # 1047 x 11 integer codes (NAs allowed)
n <- nrow(mat)
mat_m <- as.matrix(mat)
n <- nrow(mat_m)

# --- 2) Define "modern" (6-method set)
modern6_names <- c("Loop", "Oral Pill", "Condom", "Vasectomy", "TL", "Injection")
missing_modern <- setdiff(modern6_names, names(label_to_code))
if (length(missing_modern)) {
  stop(sprintf("These modern6 labels not found in label_to_code: %s",
               paste(missing_modern, collapse = ", ")))
}
modern6_codes <- unname(label_to_code[modern6_names])

# --- 3) Helper: row-wise TOD (stable disadoption) with your convention
# Definition you gave:
#  - Let last_modern = last time t in {1..11} with a modern code.
#  - If no modern ever: TOD = NA (cannot define a disadoption time)
#  - If last_modern == 11 and modern at t=11: TOD = 11 (never stops)
#  - Else (last_modern < 11): if all subsequent observed periods are non-modern (by definition),
#       we set TOD = last_modern.
#  - IMPORTANT on missing data after last_modern:
#       If there are NO observed statuses after last_modern (only NAs), we cannot confirm permanence.
#       We set TOD = NA (conservative). You can relax this if desired.

compute_TOD_one <- function(row_codes, modern_codes, Tt = 11L) {
  # Indices that are observed
  obs_idx <- which(!is.na(row_codes))
  if (length(obs_idx) == 0L) return(NA_integer_)
  
  # All modern time indices that are observed
  mod_idx <- obs_idx[row_codes[obs_idx] %in% modern_codes]
  if (length(mod_idx) == 0L) {
    # never modern -> undefined disadoption time
    return(NA_integer_)
  }
  
  last_modern <- max(mod_idx)
  
  if (last_modern == Tt) {
    # still modern at the final observed time -> per your rule, TOD=11
    return(Tt)
  }
  
  # Periods after last modern
  after_idx <- (last_modern + 1L):Tt
  after_obs <- after_idx[!is.na(row_codes[after_idx])]
  
  if (length(after_obs) == 0L) {
    # No observed data after last modern -> cannot confirm stable exit
    return(NA_integer_)
  }
  
  # If any modern after, last_modern wouldn’t be the last; but check defensively
  if (any(row_codes[after_obs] %in% modern_codes)) {
    # There exists a later modern, so last_modern wasn't final; recompute would catch this normally.
    # Treat as not stably disadopted; no TOD found under stability rule.
    return(NA_integer_)
  }
  
  # All observed after last_modern are non-modern -> stable disadoption at last_modern
  return(last_modern)
}

# Vectorize over all rows
TOD <- vapply(seq_len(n), function(i)
  compute_TOD_one(mat_m[i, ], modern6_codes, Tt = Tt),
  FUN.VALUE = integer(1)
)

# --- 4) Summaries
matches <- (mat_m %in% modern6_codes)
if (is.null(dim(matches))) {
  matches <- matrix(matches, nrow = nrow(mat_m), ncol = ncol(mat_m))
}
matches[is.na(matches)] <- FALSE
ever_modern <- rowSums(matches) > 0

num_never_modern <- sum(is.na(TOD) & !ever_modern, na.rm = TRUE)
num_stable       <- sum(!is.na(TOD) & TOD < Tt, na.rm = TRUE)
num_never_stop   <- sum(TOD == Tt, na.rm = TRUE)  # per your rule, these count at t=11
num_TOD_NA_tail  <- sum(is.na(TOD), na.rm = TRUE) - num_never_modern

cat("\n================= TOD summary =================\n")
cat(sprintf("N                       : %d\n", n))
cat(sprintf("Stable disadopters      : %d\n", num_stable))
cat(sprintf("Never stop (TOD=11)     : %d\n", num_never_stop))
cat(sprintf("Never modern (TOD=NA)   : %d\n", num_never_modern))
cat(sprintf("TOD=NA (insufficient tail obs): %d\n", num_TOD_NA_tail))

# --- 5) Build cumulative S-curve
# By your convention (TOD=11 for never stop), cumulative “disadopters” includes them at t=11.
# If you prefer to exclude them, drop the TOD==11 cases below before cumulating.
times <- 1:Tt
cum_disadopters <- sapply(times, function(t) sum(!is.na(TOD) & TOD <= t))

# Proportion relative to all n (you might prefer only those ever modern; optional alt below)
cum_prop_all <- cum_disadopters / n

# Optional alternate denominator: only those who were ever modern
denom_ever  <- max(1L, sum(ever_modern))
cum_prop_evermodern <- sapply(times, function(t) sum(ever_modern & !is.na(TOD) & TOD <= t)) / denom_ever

# Print a small table to the console
tab <- data.frame(
  t = times,
  cum_disadopters = cum_disadopters,
  cum_prop_all = round(cum_prop_all, 4),
  cum_prop_evermodern = round(cum_prop_evermodern, 4)
)
cat("\nTime-by-time cumulative counts/proportions:\n")
print(tab, row.names = FALSE)

# --- 6) Plot to PDF
pdf("disadoption_s_shape.pdf", width = 6, height = 4.2)
op <- par(mar = c(4.2, 4.5, 3.0, 1.2))
plot(times, cum_prop_all,
     type = "l", lwd = 2,
     xlab = "Time step (t)",
     ylab = "Cumulative disadopters (proportion)",
     main = "S-curve of Stable Disadoption (modern6 definition)",
     ylim = c(0, 1)
)
lines(times, cum_prop_evermodern, lwd = 2, lty = 2)
legend("topleft",
       legend = c("Denominator: all individuals", "Denominator: ever-modern only"),
       lwd = 2, lty = c(1, 2), bty = "n")
par(op)
dev.off()

cat('\nSaved plot: "disadoption_s_curve.pdf"\n')

## ================================================================
## Notes on conventions (important for interpretation)
## ------------------------------------------------
## 1) TOD = last time step with modern use IF there is at least one
##    observed post-last_modern status AND all those observed statuses
##    are non-modern. Otherwise TOD=NA (cannot confirm stable exit).
## 2) If modern at t=11, TOD = 11 (never stops), as per your request.
##    This means the cumulative line jumps at t=11 to include them.
##    If you want to EXCLUDE “never stop” from being counted as
##    disadopters at t=11, replace the cumulation with:
##        sum(!is.na(TOD) & TOD <= t & TOD < 11)
## 3) Missing data after last modern: conservative handling (TOD=NA).
##    Switch to a lenient rule by treating trailing NAs as non-modern
##    if you prefer; just change `compute_TOD_one()` accordingly.
## ================================================================