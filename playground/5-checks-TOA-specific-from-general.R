# ============================================================
# Rebuild per-method TOAs from TOA_derivado_general
# (Loop, Pill, Condom, Otros Modernos v2)
# ------------------------------------------------------------
# Logic:
# 1) Use TOA_derivado_general$status_code + source to find periods where
#    status was observed in fpt (source==1) on the focal method.
# 2) Map those periods to the 1..10 time index via byrtX (year_map).
# 3) If still NA, fill using cfp/cbyr for that same method (fallback).
# 4) Plot the four histograms with common breaks/limits.
# ============================================================

# Packages / data -------------------------------------------------------------
library(netdiffuseR)
data(kfamily)

# Load the general object (adjust path if needed)
TOA_derivado_general <- readRDS("TOA_derivado_general.rds")

# Helpers ---------------------------------------------------------------------
P <- length(TOA_derivado_general$periods)  # should be 12
n <- nrow(TOA_derivado_general$meta)

# Year map: last digit code -> full year, then convert to 1..10 index
year_map <- c("4"=1964,"5"=1965,"6"=1966,"7"=1967,"8"=1968,
              "9"=1969,"0"=1970,"1"=1971,"2"=1972,"3"=1973)
yr_to_idx <- function(y) ifelse(!is.na(y), as.integer(y) - 1963L, NA_integer_)

# Build byrt_index (n x P) with 1..10 indices (NA outside 1..10)
byrt_index <- matrix(NA_integer_, nrow = n, ncol = P,
                     dimnames = list(NULL, paste0("t", 1:P)))
for (p in 1:P) {
  col <- paste0("byrt", p)
  if (col %in% names(kfamily)) {
    yrs <- year_map[as.character(kfamily[[col]])]
    idx <- yr_to_idx(yrs)
    idx[!(idx >= 1L & idx <= 10L)] <- NA_integer_
    byrt_index[, p] <- idx
  }
}

# Method dictionaries from the general object ---------------------------------
label_to_code <- TOA_derivado_general$codes$label_to_code
states_avail  <- names(label_to_code)

# Define modern method set using the names you used earlier
modern_names_target <- c("Loop","Condom","Oral Pill","Vasectomy","TL","Injection","Rhythm","Withdrawal")
modern_names <- intersect(modern_names_target, states_avail)
modern_codes <- as.integer(unname(label_to_code[modern_names]))

# Focal methods + "Otros Modernos v2" (everything modern except those three)
code_loop   <- as.integer(label_to_code[["Loop"]])
code_pill   <- as.integer(label_to_code[["Oral Pill"]])
code_condom <- as.integer(label_to_code[["Condom"]])

codes_otros_v2 <- setdiff(modern_codes, c(code_loop, code_pill, code_condom))

# Short alias to status/source matrices
STAT <- TOA_derivado_general$status_code  # n x 12 (codes)
SRC  <- TOA_derivado_general$source       # n x 12 (1=fpt, 2=cfp-imputed, 0=NA)

# Core function: derive per-person TOA for a set of codes ---------------------
derive_toa_for_codes <- function(target_codes) {
  # 1) Try fpt/byrt anchor (source==1 and status in target)
  is_target_fpt <- (SRC == 1L) & !is.na(STAT) & (STAT %in% target_codes)
  # where true, collect byrt_index (1..10). We need the earliest (min) per i.
  # Replace non-true cells with NA to allow row-wise minima.
  cand_idx <- ifelse(is_target_fpt, byrt_index, NA_integer_)
  # Earliest 1..10 over periods
  toa <- apply(cand_idx, 1, function(v) {
    vv <- v[!is.na(v) & v >= 1L & v <= 10L]
    if (length(vv)) min(vv) else NA_integer_
  })
  # 2) Fallback to cfp/cbyr if still NA
  need_fb <- is.na(toa)
  if (any(need_fb)) {
    cfp  <- kfamily$cfp[need_fb]
    cbyr <- kfamily$cbyr[need_fb]
    # Only fill if cfp is in target_codes and cbyr is valid 1..10
    ok_method <- !is.na(cfp) & (as.integer(cfp) %in% target_codes)
    yrs       <- year_map[as.character(cbyr)]
    idx_fb    <- yr_to_idx(yrs)
    ok_idx    <- !is.na(idx_fb) & idx_fb >= 1L & idx_fb <= 10L
    fill_ok   <- ok_method & ok_idx
    if (any(fill_ok, na.rm = TRUE)) {
      toa_tmp <- toa[need_fb]
      toa_tmp[fill_ok] <- as.integer(idx_fb[fill_ok])
      toa[need_fb] <- toa_tmp
    }
  }
  # Constrain to 1..10; leave NA otherwise (no adopters for that method)
  toa[!(toa >= 1L & toa <= 10L)] <- NA_integer_
  toa
}

# Derive the four TOAs --------------------------------------------------------
TOA_loop   <- derive_toa_for_codes(code_loop)
TOA_pill   <- derive_toa_for_codes(code_pill)
TOA_condom <- derive_toa_for_codes(code_condom)
TOA_otros  <- derive_toa_for_codes(codes_otros_v2)

# Quick summaries (counts by time step + N non-NA) ----------------------------
cat("\n== Counts per step ==\n")
print(table(TOA_loop,   useNA="ifany"));   cat("N(Loop)  =", sum(!is.na(TOA_loop)),   "\n\n")
print(table(TOA_pill,   useNA="ifany"));   cat("N(Pill)  =", sum(!is.na(TOA_pill)),   "\n\n")
print(table(TOA_condom, useNA="ifany"));   cat("N(Condom)=", sum(!is.na(TOA_condom)), "\n\n")
print(table(TOA_otros,  useNA="ifany"));   cat("N(Otros) =", sum(!is.na(TOA_otros)),  "\n\n")

# Plot (same style as your earlier figure) ------------------------------------
toa_list <- list(
  Loop  = TOA_loop,
  Pill  = TOA_pill,
  Condom = TOA_condom,
  `Otros Modernos v2` = TOA_otros
)

# common y-limit based on max frequency across panels
max_freq <- 0L
for (v in toa_list) {
  cnt <- table(v, useNA="no")
  if (length(cnt)) max_freq <- max(max_freq, max(cnt))
}
ylim <- c(0, ceiling(max_freq * 1.05))

min_toa <- 1; max_toa <- 10
hist_breaks <- seq(min_toa - 0.5, max_toa + 0.5, by = 1)

# Optional colors
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  hist_cols <- rep("gray", 4L)
} else {
  hist_cols <- RColorBrewer::brewer.pal(n = 4, name = "Set2")
}

pdf("toa_specific_histograms_from_general.pdf", width = 6, height = 5)
op <- par(mfrow = c(2,2), mar = c(4,4,3,1))
for (i in seq_along(toa_list)) {
  nm  <- names(toa_list)[i]
  vec <- toa_list[[i]]
  hist(vec,
       main = paste("TOA", nm),
       xlab = "Time step",
       ylab = "Users",
       breaks = hist_breaks,
       ylim = ylim,
       col  = hist_cols[i],
       xaxt = "n")
  axis(1, at = min_toa:max_toa, labels = min_toa:max_toa)
}
par(op)
dev.off()

cat("PDF written: toa_specific_histograms_from_general.pdf\n")