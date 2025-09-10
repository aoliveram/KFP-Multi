# ================================================================
# Temporary disadoption (flow) — lagged neighbor-disadoption exposure
# Self-contained: only needs netdiffuseR::kfamily
# ================================================================

library(netdiffuseR)

# --------------------------- Load & prep ------------------------
data(kfamily, package = "netdiffuseR")
n  <- nrow(kfamily)
Tt <- 11L                      # years 1963..1973 (+ tech year), columns fpt1..fpt11

# Keep ties only among surveyed ids (to mirror replication logic)
netvars   <- grep("^net", names(kfamily), value = TRUE)
surveyed  <- kfamily$id
for (v in netvars) kfamily[[v]][ !(kfamily[[v]] %in% surveyed) ] <- NA

# Build dynamic network
dn <- survey_to_diffnet(
  toavar   = "toa",
  netvars  = netvars,
  idvar    = "id",
  groupvar = if ("village" %in% names(kfamily)) "village" else NULL,
  dat      = kfamily
)

vill <- if ("village" %in% names(kfamily)) kfamily$village else rep(1L, n)

# Helper: get fpt columns (codes for FP status by wave)
get_fpt_cols <- function(df) {
  fpt <- grep("^fpt\\d+$", names(df), value = TRUE)
  idx <- as.integer(sub("^fpt", "", fpt))
  fpt[order(idx)]
}
fp_cols <- get_fpt_cols(kfamily)
stopifnot(length(fp_cols) >= Tt)
fp_cols <- fp_cols[seq_len(Tt)]

# --------------- Method codes: define Modern / Traditional ------
labtab <- attr(kfamily, "label.table")$fpstatus
stopifnot(!is.null(labtab))
label_to_code <- unclass(labtab)                  # named vector: label -> code
code_to_label <- setNames(names(labtab), labtab)

modern6_names <- c("Loop","Oral Pill","Condom","Vasectomy","TL","Injection")
stopifnot(all(modern6_names %in% names(label_to_code)))
modern6_codes <- unname(label_to_code[modern6_names])

trad_names <- intersect(c("Rhythm","Withdrawal","Jelly","Foam"), names(label_to_code))
trad_codes <- if (length(trad_names)) unname(label_to_code[trad_names]) else integer(0)

# ----------- FP state matrix & meta-state (0=None, 1=Trad, 2=Modern)
fp_mat <- as.matrix(kfamily[, fp_cols, drop = FALSE])
is_modern      <- (fp_mat %in% modern6_codes)
is_traditional <- (fp_mat %in% trad_codes)

if (is.null(dim(is_modern)))      is_modern      <- matrix(is_modern,      nrow = n, ncol = Tt)
if (is.null(dim(is_traditional))) is_traditional <- matrix(is_traditional, nrow = n, ncol = Tt)

meta_state <- matrix(0L, n, Tt)
meta_state[is_traditional] <- 1L
meta_state[is_modern]      <- 2L

# ---------------- Degrees per period (in/out) -------------------
deg_out <- deg_in <- matrix(0, n, Tt)
for (tt in seq_len(Tt)) {
  A <- as.matrix(dn$graph[[tt]])
  if (!is.matrix(A)) A <- matrix(0, n, n)
  deg_out[, tt] <- rowSums(A != 0, na.rm = TRUE)
  deg_in[,  tt] <- colSums(A != 0, na.rm = TRUE)
}

# ---------------- Media exposure & children (from scratch) -----
own_cols <- intersect(paste0("media", 1:5),  names(kfamily))
exp_cols <- intersect(paste0("media", 6:19), names(kfamily))
own_sum  <- if (length(own_cols)) rowSums(kfamily[, own_cols, drop = FALSE], na.rm = TRUE) else rep(NA_real_, n)
exp_sum  <- if (length(exp_cols)) rowSums(kfamily[, exp_cols, drop = FALSE], na.rm = TRUE) else rep(NA_real_, n)
media_exposure <- ifelse(!is.na(exp_sum) & !is.na(own_sum), exp_sum / pmax(own_sum, 1), NA_real_)

children_vec <- if (all(c("sons","daughts") %in% names(kfamily))) {
  as.numeric(kfamily$sons) + as.numeric(kfamily$daughts)
} else rep(NA_real_, n)

# ---------------- Age by time & Fecundability index -------------
age_1973 <- if ("age" %in% names(kfamily)) as.numeric(kfamily$age) else rep(NA_real_, n)
year_t   <- 1963 + (1:Tt)
age_t    <- matrix(NA_real_, n, Tt)
for (tt in seq_len(Tt)) age_t[, tt] <- age_1973 - (1973 - year_t[tt])

fec_ratio_from_age <- function(a) {
  ifelse(is.na(a), NA_real_,
         ifelse(a <= 24, 1.00,
                ifelse(a <= 27, 0.91,
                       ifelse(a <= 30, 0.88,
                              ifelse(a <= 33, 0.87,
                                     ifelse(a <= 36, 0.82,
                                            ifelse(a <= 39, 0.60,
                                                   ifelse(a <= 45, 0.40, 0.00))))))))
}
fec_t <- matrix(fec_ratio_from_age(c(age_t)), nrow = n, ncol = Tt)

# ================================================================
# FLOW-STYLE TRANSITIONS (period-by-period)
# ------------------------------------------------
# Disadopt at time t (t>=2): Modern at t-1 AND Not-Modern at t
# Adopt   at time t (t>=2): Not-Modern at t-1 AND Modern at t
# We’ll build neighbor exposure at time t using *last period* (t-1) transitions,
# so exposures will be available from t>=3 (one-period lag).
# ================================================================

# Transition indicators per period
disadopt_t <- matrix(FALSE, n, Tt)  # TRUE if disadopt between (t-1)->t
adopt_t    <- matrix(FALSE, n, Tt)  # TRUE if adopt    between (t-1)->t

for (t in 2:Tt) {
  disadopt_t[, t] <- (meta_state[, t-1] == 2L) & (meta_state[, t] != 2L) & !is.na(meta_state[, t-1]) & !is.na(meta_state[, t])
  adopt_t[,    t] <- (meta_state[, t-1] != 2L) & (meta_state[, t] == 2L) & !is.na(meta_state[, t-1]) & !is.na(meta_state[, t])
}

# Neighbor exposure to *last period's* disadoptions/adoptions:
# E_disflow_i(t) = (# neighbors who disadopted at t-1)/(degree at t-1)
# E_adopflow_i(t)= (# neighbors who adopted   at t-1)/(degree at t-1)

E_disflow <- matrix(0, n, Tt)
E_adopflow<- matrix(0, n, Tt)

for (t in 3:Tt) {
  A_prev <- as.matrix(dn$graph[[t-1]])     # ties observed at t-1
  if (!is.matrix(A_prev)) A_prev <- matrix(0, n, n)
  ki_prev <- pmax(rowSums(A_prev != 0, na.rm = TRUE), 0)
  
  neigh_dis_prev <- A_prev %*% as.numeric(disadopt_t[, t-1])
  neigh_adop_prev<- A_prev %*% as.numeric(adopt_t[,    t-1])
  
  E_disflow[,  t] <- ifelse(ki_prev > 0, as.numeric(neigh_dis_prev)/ki_prev, 0)
  E_adopflow[, t] <- ifelse(ki_prev > 0, as.numeric(neigh_adop_prev)/ki_prev, 0)
}

# Village-level cumulative disadoptions (flow-based, up to period u)
# cum_dis_vil[g, u] = number of disadoption events in village g up to time u
Gs <- sort(unique(vill))
cum_dis_vil <- matrix(0, n, Tt)
cum_by_g <- rep(0L, length(Gs))
for (t in 2:Tt) {
  # count events at t by village
  add_g <- sapply(Gs, function(g) {
    idx_g <- which(vill == g)
    sum(disadopt_t[idx_g, t], na.rm = TRUE)
  })
  cum_by_g <- cum_by_g + add_g
  # assign to egos by their village
  cum_dis_vil[, t] <- cum_by_g[ match(vill, Gs) ]
}

# ================================================================
# Build the *risk* panel:
# Rows are (i, t) where ego was Modern at (t-1). Outcome: disadopt at t.
# Covariates: lagged (t-1) network, exposures, degrees, etc.
# ================================================================

risk_rows <- lapply(seq_len(n), function(i) {
  idx <- which(meta_state[i, 1:(Tt-1)] == 2L) + 1L  # times t where s_{i,t-1}==Modern
  idx <- idx[idx >= 3L]                             # need lagged exposure (t-1 >= 2)
  if (length(idx) == 0) return(NULL)
  data.frame(i = i, t = idx)
})
risk_df <- do.call(rbind, risk_rows)
row.names(risk_df) <- NULL
stopifnot(nrow(risk_df) > 0)

# helper to pick lagged values at (i, t-1)
lag_pick <- function(M) M[cbind(risk_df$i, risk_df$t - 1L)]

panel <- within(risk_df, {
  Y            <- as.integer(disadopt_t[cbind(i, t)])     # ego disadopts at t
  per          <- factor(t)                               # period FE
  E_dis_lag    <- lag_pick(E_disflow)
  E_adopt_lag  <- lag_pick(E_adopflow)
  deg_in_lag   <- lag_pick(deg_in)
  deg_out_lag  <- lag_pick(deg_out)
  cumdis_g_lag <- lag_pick(cum_dis_vil)
  media_lag    <- media_exposure[i]
  children     <- children_vec[i]
  age_lag      <- lag_pick(age_t)
  fec_lag      <- lag_pick(fec_t)
  g            <- vill[i]
})

# Drop rows with missing key covariates (very few)
panel <- subset(panel, 
                !is.na(E_dis_lag) & !is.na(E_adopt_lag) &
                  !is.na(deg_in_lag) & !is.na(deg_out_lag) &
                  !is.na(cumdis_g_lag)
)

# ================================================================
# Fit models
# ================================================================
fam <- binomial("logit")

m_flow_base <- glm(
  Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag + per + cumdis_g_lag + children,
  data = panel, family = fam
)

m_flow_age <- glm(
  Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag + per + cumdis_g_lag + children + age_lag,
  data = panel, family = fam
)

m_flow_fec <- glm(
  Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag + per + cumdis_g_lag + children + fec_lag,
  data = panel, family = fam
)

m_flow_media <- glm(
  Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag + per + cumdis_g_lag + children + media_lag,
  data = panel, family = fam
)

cat("\n=== Flow-style (lagged neighbor disadoptions) — model summaries ===\n")
print(summary(m_flow_base))
print(summary(m_flow_age))
print(summary(m_flow_fec))
print(summary(m_flow_media))

# Comparison table
fits <- list(
  flow_base  = m_flow_base,
  flow_age   = m_flow_age,
  flow_fec   = m_flow_fec,
  flow_media = m_flow_media
)
cmp <- data.frame(
  model = names(fits),
  AIC   = sapply(fits, AIC),
  BIC   = sapply(fits, BIC),
  rows  = sapply(fits, function(m) nrow(model.frame(m))),
  events= sapply(fits, function(m) sum(model.frame(m)$Y, na.rm = TRUE))
)
cat("\n=== Flow-style comparison ===\n"); print(cmp, row.names = FALSE)

# Quick sanity
cat("\n# Risk rows:", nrow(panel),
    "| Unique egos:", length(unique(panel$i)),
    "| Events (sum Y):", sum(panel$Y), "\n")
cat("Corr(E_dis_lag, E_adopt_lag) =", round(cor(panel$E_dis_lag, panel$E_adopt_lag, use="complete.obs"),3), "\n")