# ================================================================
# Stable Disadoption regressions (modern-6)
# Fully self-contained; no external objects required
# ================================================================

suppressPackageStartupMessages({
  library(netdiffuseR)
})

# --------------------------- Load & prep ------------------------
data(kfamily, package = "netdiffuseR")

# Keep ties only among surveyed ids (mirrors your replication logic)
netvars   <- grep("^net", names(kfamily), value = TRUE)
surveyed  <- kfamily$id
for (v in netvars) kfamily[[v]][ !(kfamily[[v]] %in% surveyed) ] <- NA

# Build diffnet (original adoption timing exists but we don't use it here)
dn <- survey_to_diffnet(
  toavar   = "toa",
  netvars  = netvars,
  idvar    = "id",
  groupvar = if ("village" %in% names(kfamily)) "village" else NULL,
  dat      = kfamily
)

# ----- Helper to get the available fpt columns in numeric order -----
Tt <- 11L     # number of observed time steps (often 11)
n  <- nrow(kfamily)

get_fpt_cols <- function(df) {
  fpt <- grep("^fpt\\d+$", names(df), value = TRUE)
  idx <- as.integer(sub("^fpt", "", fpt))
  fpt[order(idx)]
}
fp_cols <- get_fpt_cols(kfamily)
fp_cols <- fp_cols[seq_len(Tt)]
if (length(fp_cols) < 2L) stop("Could not find at least two fpt columns in kfamily.")

vill <- if ("village" %in% names(kfamily)) kfamily$village else rep(1L, n)

# ----------------- Degrees per period (in/out) ------------------
deg_out <- deg_in <- matrix(NA_real_, n, Tt)
for (tt in seq_len(Tt)) {
  A <- as.matrix(dn$graph[[tt]])
  if (!is.matrix(A)) A <- matrix(0, nrow = n, ncol = n)
  deg_out[, tt] <- rowSums(A != 0, na.rm = TRUE)
  deg_in[,  tt] <- colSums(A != 0, na.rm = TRUE)
}

# ---------------- Media exposure & children (from scratch) -----
# Heuristic: "media1:media5" -> own availability; "media6:media19" -> exposure via alters
own_cols <- intersect(paste0("media", 1:5),  names(kfamily))
exp_cols <- intersect(paste0("media", 6:19), names(kfamily))
own_sum  <- if (length(own_cols)) rowSums(kfamily[, own_cols, drop = FALSE], na.rm = TRUE) else rep(NA_real_, n)
exp_sum  <- if (length(exp_cols)) rowSums(kfamily[, exp_cols, drop = FALSE], na.rm = TRUE) else rep(NA_real_, n)
media_exposure <- ifelse(!is.na(exp_sum) & !is.na(own_sum), exp_sum / pmax(own_sum, 1), NA_real_)

children_vec <- if (all(c("sons","daughts") %in% names(kfamily))) {
  as.numeric(kfamily$sons) + as.numeric(kfamily$daughts)
} else {
  rep(NA_real_, n)
}

# --------------- Redefine MODERN methods (6) --------------------
labtab <- attr(kfamily, "label.table")$fpstatus
if (is.null(labtab)) stop("No label.table$fpstatus found in kfamily attributes.")

label_to_code <- unclass(labtab)             # named numeric, names=labels, values=codes
code_to_label <- setNames(names(labtab), labtab)

modern6_names <- c("Loop", "Oral Pill", "Condom", "Vasectomy", "TL", "Injection")
if (!all(modern6_names %in% names(label_to_code))) {
  stop("Some of the modern-6 method labels are missing in kfamily labels: ",
       paste(setdiff(modern6_names, names(label_to_code)), collapse = ", "))
}
modern6_codes <- unname(label_to_code[modern6_names])

trad_names <- intersect(c("Rhythm","Withdrawal","Jelly","Foam"), names(label_to_code))
trad_codes <- if (length(trad_names)) unname(label_to_code[trad_names]) else integer(0)

# ----------- Pull fpt1..fptTt into matrix of codes --------------
fp_mat <- as.matrix(kfamily[, fp_cols, drop = FALSE])

# ----------- Build meta states (2=Modern, 1=Traditional, 0=None)-
is_modern      <- (fp_mat %in% modern6_codes)
is_traditional <- (fp_mat %in% trad_codes)

# SAFETY: ensure matrix dims (prevents the apply() error you saw)
if (is.null(dim(is_modern)))      is_modern      <- matrix(is_modern,      nrow = n, ncol = Tt)
if (is.null(dim(is_traditional))) is_traditional <- matrix(is_traditional, nrow = n, ncol = Tt)

meta_state <- matrix(0L, n, Tt)
meta_state[is_traditional] <- 1L
meta_state[is_modern]      <- 2L

# ----------- Stable TOD (time of last modern use; else NA) ------
last_modern <- apply(is_modern, 1, function(z) {
  if (any(z, na.rm = TRUE)) max(which(z), na.rm = TRUE) else NA_integer_
})
TOD <- last_modern  # leave NA for never-modern; we drop them downstream

# Restrict to **ever-modern** for disadoption analysis
ever_modern <- rowSums(is_modern, na.rm = TRUE) > 0
keep_idx    <- which(ever_modern)
if (!length(keep_idx)) stop("No ever-modern users found; cannot build disadoption panel.")

# ----------------- Neighbor exposure (E and E^D) ----------------
E_adopt <- matrix(0, n, Tt)   # Ei(t) ∈ [0,1], define as 0 at t=1 by convention
E_dis   <- matrix(0, n, Tt)   # ED_i(t) ∈ [0,1]

for (tt in 2:Tt) {
  A  <- as.matrix(dn$graph[[tt]])         # adjacency at t
  if (!is.matrix(A)) A <- matrix(0, n, n)
  ki <- pmax(rowSums(A != 0, na.rm = TRUE), 0)
  
  # Neighbor modern indicator at t-1 (lagged exposure)
  neigh_mod_t1 <- as.numeric(A %*% (meta_state[, tt-1] == 2L))
  
  # Cohesion adoption exposure Ei(t) = (# modern alters at t-1)/degree_t
  Ei_t <- ifelse(ki > 0, neigh_mod_t1 / ki, 0)
  
  E_adopt[, tt] <- Ei_t
}

# Running peak Emax_i(t) and disadoption exposure ED_i(t) = Emax_i(t) - Ei(t)
E_max <- t(apply(E_adopt, 1, function(z) cummax(ifelse(is.finite(z), z, 0))))
E_dis <- pmax(E_max - E_adopt, 0)

# ------------- Village-level cumulative disadoption -------------
Gs <- sort(unique(vill))
event_T <- rep(NA_integer_, n)
event_T[keep_idx] <- TOD[keep_idx]                  # stable disadoption time for ever-modern

cum_dis_vil <- matrix(NA_real_, n, Tt)
for (tt in seq_len(Tt)) {
  counts_by_g <- sapply(Gs, function(g) {
    idx_g <- which(vill == g)
    sum(!is.na(event_T[idx_g]) & event_T[idx_g] <= tt, na.rm = TRUE)
  })
  cum_dis_vil[, tt] <- counts_by_g[vill]
}

# ---------------- Age by time & Fecundability index -------------
# Age reported in 1973; map t=1..Tt to calendar year = 1963 + t (so t=11 -> 1974)
age_1973 <- if ("age" %in% names(kfamily)) as.numeric(kfamily$age) else rep(NA_real_, n)
year_t   <- 1963 + (1:Tt)
age_t    <- matrix(NA_real_, n, Tt)
for (tt in seq_len(Tt)) age_t[, tt] <- age_1973 - (1973 - year_t[tt])

fec_ratio_from_age <- function(a) {
  ifelse(is.na(a), NA_real_,
         ifelse(a <= 24, 1.00, #a >= 21 & a <= 24, 1.00,
                ifelse(a >= 25 & a <= 27, 0.91,
                       ifelse(a >= 28 & a <= 30, 0.88,
                              ifelse(a >= 31 & a <= 33, 0.87,
                                     ifelse(a >= 34 & a <= 36, 0.82,
                                            ifelse(a >= 37 & a <= 39, 0.60,
                                                   ifelse(a >= 40 & a <= 45, 0.40,
                                                        ifelse(a >= 46, 0.00, NA_real_)))))))))
}
fec_t <- matrix(fec_ratio_from_age(c(age_t)), nrow = n, ncol = Tt)

# --------------------- Build hazard panel -----------------------
# Discrete-time hazard robustly (no t = 1 allowed):
#   Y_{it} = 1 if i’s stable disadoption occurs at time t (i.e., TOD_i == t)
# At-risk periods: t = 2..Tt for ever-modern rows; use covariates at t-1

at_risk_rows <- lapply(keep_idx, function(i) {
  t_star <- TOD[i]
  # Skip if TOD is NA or TOD < 2; otherwise make 2..min(TOD, Tt)
  if (is.na(t_star) || t_star < 2L) return(NULL)
  data.frame(i = i, t = seq.int(2L, min(t_star, Tt)))
})
at_risk_df <- do.call(rbind, at_risk_rows)
row.names(at_risk_df) <- NULL

# Final sanity checks on indices
stopifnot(nrow(at_risk_df) > 0L)
stopifnot(all(at_risk_df$t >= 2L & at_risk_df$t <= Tt))

#lag_pick <- function(M) M[cbind(at_risk_df$i, at_risk_df$t - 1L)]
lag_pick <- function(M) {
  idx <- cbind(at_risk_df$i, at_risk_df$t - 1L)
  bad <- idx[, 1L] < 1L | idx[, 1L] > nrow(M) | idx[, 2L] < 1L | idx[, 2L] > ncol(M)
  if (any(bad)) {
    nb <- sum(bad)
    # If you ever want to inspect them:
    # print(head(idx[bad, , drop = FALSE], 10))
    stop(sprintf("lag_pick: %d invalid (i, t-1) pairs detected; check at_risk_df and Tt", nb))
  }
  M[idx]
}

panel <- within(at_risk_df, {
  Y            <- as.integer(TOD[i] == t)     # event occurs at t
  per          <- t
  E_adopt_lag  <- lag_pick(E_adopt)
  E_dis_lag    <- lag_pick(E_dis)
  deg_in_lag   <- lag_pick(deg_in)
  deg_out_lag  <- lag_pick(deg_out)
  cumdis_g_lag <- lag_pick(cum_dis_vil)
  media_lag    <- media_exposure[i]           # time-constant
  children     <- children_vec[i]             # time-constant
  age_lag      <- lag_pick(age_t)
  fec_lag      <- lag_pick(fec_t)
  g            <- vill[i]
})

# Drop rows with missing key predictors
panel <- subset(panel, !is.na(E_adopt_lag) & !is.na(E_dis_lag) &
                  !is.na(deg_in_lag)  & !is.na(deg_out_lag) &
                  !is.na(cumdis_g_lag))

# Time term as a factor if possible
panel$time_term <- {
  f <- factor(panel$per)
  if (nlevels(f) >= 2L) f else as.numeric(panel$per)
}

# ---------------------- Fit three models ------------------------

m_vil_mod6_dis        <- glm(formula = Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag + time_term + cumdis_g_lag + media_lag + children,
                             data = panel, family = binomial(link = "logit"))
m_vil_mod6_dis_b      <- glm(formula = Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag + time_term + cumdis_g_lag + children,
                             data = panel, family = binomial(link = "logit"))
m_vil_mod6_dis_c_age  <- glm(formula = Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag + time_term + cumdis_g_lag + children + age_lag,
                             data = panel, family = binomial(link = "logit"))
m_vil_mod6_dis_c_fec  <- glm(formula = Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag + time_term + cumdis_g_lag + children + fec_lag,
                             data = panel, family = binomial(link = "logit"))

cat("\n=== m_vil_mod6_dis (with media) ===\n");    print(summary(m_vil_mod6_dis))
cat("\n=== m_vil_mod6_dis_b (no media) ===\n");   print(summary(m_vil_mod6_dis_b))
cat("\n=== m_vil_mod6_dis_c_age (add age) ===\n");print(summary(m_vil_mod6_dis_c_age))
cat("\n=== m_vil_mod6_dis_c_fec (add fec) ===\n");print(summary(m_vil_mod6_dis_c_fec))

# ---------------------- Diagnostics & compare -------------------
cat("\n# Rows in panel:", nrow(panel), "\n")
cat("# Unique individuals in panel:", length(unique(panel$i)), "\n")
cat("# Events (sum Y):", sum(panel$Y, na.rm = TRUE), "\n")
cat("Time distribution kept:\n"); print(table(panel$per))

fits <- list(
  m1      = m_vil_mod6_dis,
  m2      = m_vil_mod6_dis_b,
  m3_age  = m_vil_mod6_dis_c_age,
  m3_fec  = m_vil_mod6_dis_c_fec
)
comp <- data.frame(
  model  = names(fits),
  AIC    = sapply(fits, AIC),
  BIC    = sapply(fits, BIC),
  events = sapply(fits, function(m) sum(model.frame(m)$Y, na.rm = TRUE)),
  rows   = sapply(fits, function(m) nrow(model.frame(m)))
)
cat("\nModel fit comparison (lower is better):\n"); print(comp, row.names = FALSE)
