# ================================================================
# Temporary disadoption (recurrent): lagged transition models
# Requires objects created by 7-disadoption-regression.R:
# n, Tt, meta_state, E_dis, E_adopt, deg_in, deg_out, cum_dis_vil,
# children_vec, age_t, fec_t, media_exposure
# ================================================================

stopifnot(
  exists("n"), exists("Tt"),
  exists("meta_state"), exists("E_dis"), exists("E_adopt"),
  exists("deg_in"), exists("deg_out"), exists("cum_dis_vil"),
  exists("children_vec"), exists("age_t"), exists("fec_t"),
  exists("media_exposure")
)

# 1) Build the risk set: all (i,t) with Modern at t-1 (i.e., meta_state[i,t-1]==2)
risk_rows <- do.call(
  rbind,
  lapply(2:Tt, function(t) {
    idx <- which(meta_state[, t-1L] == 2L)           # Modern at t-1
    if (length(idx) == 0L) return(NULL)
    data.frame(i = idx, t = t)
  })
)
if (is.null(risk_rows) || nrow(risk_rows) == 0L) stop("No Modern-at-(t-1) rows found.")

row.names(risk_rows) <- NULL

# 2) Outcome at time t: exit Modern? (1 if state at t is != 2; 0 if remain Modern)
Y_tmp <- {
  s_t <- meta_state[cbind(risk_rows$i, risk_rows$t)]
  as.integer(!is.na(s_t) & s_t != 2L)
}

# 3) Helper to pick lagged covariates at t-1 for rows in risk_rows
lag_pick <- function(M) {
  idx <- cbind(risk_rows$i, risk_rows$t - 1L)
  bad <- idx[,1L] < 1L | idx[,1L] > nrow(M) | idx[,2L] < 1L | idx[,2L] > ncol(M)
  if (any(bad)) stop(sprintf("lag_pick: %d invalid (i, t-1) pairs.", sum(bad)))
  M[idx]
}

# 4) Assemble the panel with LAGGED covariates (all measured at t-1)
Tpanel <- within(risk_rows, {
  Y            <- Y_tmp
  per          <- factor(t)                    # period factor
  E_dis_lag    <- lag_pick(E_dis)
  E_adopt_lag  <- lag_pick(E_adopt)
  deg_in_lag   <- lag_pick(deg_in)
  deg_out_lag  <- lag_pick(deg_out)
  cumdis_g_lag <- lag_pick(cum_dis_vil)
  age_lag      <- lag_pick(age_t)
  fec_lag      <- lag_pick(fec_t)
  children     <- children_vec[i]              # time-invariant
  media_lag    <- media_exposure[i]            # time-invariant
})

# 5) Keep complete cases on main regressors (same rule you used before)
Tpanel <- subset(Tpanel,
                 !is.na(Y) & !is.na(E_dis_lag) & !is.na(E_adopt_lag) &
                   !is.na(deg_in_lag) & !is.na(deg_out_lag) & !is.na(cumdis_g_lag)
)

# (Optional) also drop if age/fec to be used and are NA for those models
Tpanel_age <- subset(Tpanel, !is.na(age_lag))
Tpanel_fec <- subset(Tpanel, !is.na(fec_lag))

fam <- binomial(link = "logit")

# 6) Fit a small family of models (all LAGGED)
m_tmp_base  <- glm(Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
                     per + cumdis_g_lag + children,
                   data = Tpanel, family = fam)

m_tmp_age   <- glm(Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
                     per + cumdis_g_lag + children + age_lag,
                   data = Tpanel_age, family = fam)

m_tmp_fec   <- glm(Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
                     per + cumdis_g_lag + children + fec_lag,
                   data = Tpanel_fec, family = fam)

m_tmp_media <- glm(Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
                     per + cumdis_g_lag + children + media_lag,
                   data = Tpanel, family = fam)

cat("\n=== (T1-lagged) Temporary disadoption — summaries ===\n")
print(summary(m_tmp_base))
print(summary(m_tmp_age))
print(summary(m_tmp_fec))
print(summary(m_tmp_media))

# 7) Quick model comparison
fits <- list(
  T1_lag_base  = m_tmp_base,
  T1_lag_age   = m_tmp_age,
  T1_lag_fec   = m_tmp_fec,
  T1_lag_media = m_tmp_media
)
cmp <- data.frame(
  model = names(fits),
  AIC   = sapply(fits, AIC),
  BIC   = sapply(fits, BIC),
  rows  = sapply(fits, function(m) nrow(model.frame(m))),
  events= sapply(fits, function(m) sum(model.frame(m)$Y, na.rm = TRUE))
)
cat("\n=== (T1-lagged) Comparison ===\n"); print(cmp, row.names = FALSE)

# 8) Sanity checks
cat("\n# Risk rows:", nrow(Tpanel), 
    "| Unique egos:", length(unique(Tpanel$i)),
    "| Events (sum Y):", sum(Tpanel$Y), "\n")
cat("Pairwise corr (E_dis_lag vs E_adopt_lag):\n")
print(cor(Tpanel$E_dis_lag, Tpanel$E_adopt_lag, use = "complete.obs"))

