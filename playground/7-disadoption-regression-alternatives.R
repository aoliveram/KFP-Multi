# Run after 7-disadoption-regression.R

# ===================================================================
# Sensitivity analyses for E_dis: short-memory Emax, lag alignment,
# and "drop-one" collinearity checks
# (append below your last line)
# ===================================================================

cat("\n================ SENSITIVITY ANALYSES: START ================\n")

# --- helper: row-wise rolling max over a window L (t = 1..Tt) ----
row_roll_max <- function(M, L) {
  n <- nrow(M); Tt <- ncol(M)
  out <- matrix(0, n, Tt)
  for (i in seq_len(n)) {
    z <- M[i, ]
    for (t in seq_len(Tt)) {
      s <- max(1L, t - L + 1L)
      zz <- z[s:t]
      # if all NA / non-finite in window, treat as 0 (no adopting neighbors)
      out[i, t] <- if (any(is.finite(zz))) max(zz, na.rm = TRUE) else 0
    }
  }
  out
}

# -------------------- (A) SHORT-MEMORY Emax -----------------------
# Construct ED^(L)_it = max_{u in [t-L+1, t]} E_i(u) - E_i(t)
Ls <- c(2L, 3L, 4L)
shortmem_models <- list()
for (L in Ls) {
  Emax_L <- row_roll_max(E_adopt, L = L)
  Edis_L <- pmax(Emax_L - E_adopt, 0)
  Edis_L_lag <- lag_pick(Edis_L)  # align to t-1 like main models
  
  # Build a copy of the same panel but swap the lagged E_dis
  panel_L <- panel
  panel_L$E_disL_lag <- Edis_L_lag
  
  # Fit with the same control set as m_vil_mod6_dis_b (no media) to isolate exposure effects
  f_L <- Y ~ E_disL_lag + E_adopt_lag + deg_in_lag + deg_out_lag + per + cumdis_g_lag + children
  fit_L <- glm(f_L, data = panel_L, family = binomial("logit"))
  shortmem_models[[paste0("L", L)]] <- fit_L
  cat(sprintf("\n--- Short-memory L=%d ---\n", L)); print(summary(fit_L)$coefficients[c("E_disL_lag","E_adopt_lag"), , drop=FALSE])
}

# -------------------- (B) LAG ALIGNMENT ---------------------------
# (B1) "contemporaneous" alignment: pick exposure at time t instead of t-1
cur_pick <- function(M) {
  idx <- cbind(at_risk_df$i, at_risk_df$t)  # note: t, not t-1
  bad <- idx[,1L] < 1L | idx[,1L] > nrow(M) | idx[,2L] < 1L | idx[,2L] > ncol(M)
  if (any(bad)) stop("cur_pick: invalid (i,t) pairs")
  M[idx]
}
E_adopt_cur <- cur_pick(E_adopt)
E_dis_cur   <- cur_pick(E_dis)

panel_cur <- panel
panel_cur$E_adopt_cur <- E_adopt_cur
panel_cur$E_dis_cur   <- E_dis_cur

form_cur <- Y ~ E_dis_cur + E_adopt_cur + deg_in_lag + deg_out_lag + per + cumdis_g_lag + children
m_align_cur <- glm(formula = form_cur, data = panel_cur, family = binomial("logit"))
cat("\n--- Lag alignment: contemporaneous (t) ---\n")
print(summary(m_align_cur)$coefficients[c("E_dis_cur","E_adopt_cur"), , drop=FALSE])

# (B2) two-lag alignment: exposures at t-2
# restrict to rows with t >= 3 for valid t-2
at_risk_df_l2 <- subset(at_risk_df, t >= 3L)
lag2_pick <- function(M) {
  idx <- cbind(at_risk_df_l2$i, at_risk_df_l2$t - 2L)
  bad <- idx[,1L] < 1L | idx[,1L] > nrow(M) | idx[,2L] < 1L | idx[,2L] > ncol(M)
  if (any(bad)) stop("lag2_pick: invalid (i,t-2) pairs")
  M[idx]
}
panel_l2 <- within(at_risk_df_l2, {
  Y            <- as.integer(TOD[i] == t)
  per          <- factor(t)  # keep same coding as main
  E_adopt_l2   <- lag2_pick(E_adopt)
  E_dis_l2     <- lag2_pick(E_dis)
  deg_in_lag   <- lag_pick(deg_in)[t >= 3L]    # reuse lag=1 covariates aligned by subsetting
  deg_out_lag  <- lag_pick(deg_out)[t >= 3L]
  cumdis_g_lag <- lag_pick(cum_dis_vil)[t >= 3L]
  children     <- children_vec[i]
})
# Make sure vectors are aligned after subsetting
stopifnot(nrow(panel_l2) == length(panel_l2$E_adopt_l2))
form_l2 <- Y ~ E_dis_l2 + E_adopt_l2 + deg_in_lag + deg_out_lag + per + cumdis_g_lag + children
m_align_l2 <- glm(formula = form_l2, data = panel_l2, family = binomial("logit"))
cat("\n--- Lag alignment: two-lag (t-2) ---\n")
print(summary(m_align_l2)$coefficients[c("E_dis_l2","E_adopt_l2"), , drop=FALSE])

# -------------------- (C) DROP-ONE CHECKS ------------------------
# Compare: (i) only E_dis, (ii) only E_adopt, (iii) both (baseline m_vil_mod6_dis_b)
m_only_Edis   <- glm(Y ~ E_dis_lag + deg_in_lag + deg_out_lag + per + cumdis_g_lag + children,
                     data = panel, family = binomial("logit"))
m_only_Eadopt <- glm(Y ~ E_adopt_lag + deg_in_lag + deg_out_lag + per + cumdis_g_lag + children,
                     data = panel, family = binomial("logit"))

cat("\n--- Drop-one models ---\n")
print(summary(m_only_Edis)$coefficients["E_dis_lag", , drop=FALSE])
print(summary(m_only_Eadopt)$coefficients["E_adopt_lag", , drop=FALSE])

# -------------------- Quick comparison table ---------------------
sens_fits <- list(
  base_noMedia = m_vil_mod6_dis_b,
  short_L2     = shortmem_models[["L2"]],
  short_L3     = shortmem_models[["L3"]],
  short_L4     = shortmem_models[["L4"]],
  align_cur    = m_align_cur,
  align_lag2   = m_align_l2,
  only_Edis    = m_only_Edis,
  only_Eadopt  = m_only_Eadopt
)

cmp <- data.frame(
  model = names(sens_fits),
  beta_Edis = sapply(sens_fits, function(m) coef(m)[grep("E_dis", names(coef(m)))[1]] %||% NA_real_),
  se_Edis   = sapply(sens_fits, function(m) { nm <- grep("E_dis", names(coef(m)))[1]; if (is.na(nm)) NA_real_ else sqrt(diag(vcov(m)))[nm] }),
  beta_E    = sapply(sens_fits, function(m) coef(m)[grep("E_adopt", names(coef(m)))[1]] %||% NA_real_),
  se_E      = sapply(sens_fits, function(m) { nm <- grep("E_adopt", names(coef(m)))[1]; if (is.na(nm)) NA_real_ else sqrt(diag(vcov(m)))[nm] }),
  AIC       = sapply(sens_fits, AIC),
  BIC       = sapply(sens_fits, BIC),
  rows      = sapply(sens_fits, function(m) nrow(model.frame(m)))
)
cat("\nSensitivity comparison (coef, SE, AIC/BIC):\n"); print(cmp, row.names = FALSE)

# also inspect correlation between the two main regressors in the base panel
cat("\nPairwise correlation in base panel (lagged exposures):\n")
print(cor(panel$E_dis_lag, panel$E_adopt_lag, use = "complete.obs"))

cat("\n================ SENSITIVITY ANALYSES: END ==================\n")


# ---------- Extra diagnostics (paste below) ----------
library(sandwich); library(lmtest)

# (1) Keep only rows with degree > 0 at t-1
panel_degpos <- subset(panel, deg_in_lag + deg_out_lag > 0)
m_degpos <- glm(Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
                  per + cumdis_g_lag + children,
                data = panel_degpos, family = binomial("logit"))

# (2) Village share disadopted
vill_sizes <- as.numeric(table(vill))[ match(panel$g, sort(unique(vill))) ]
panel$share_cumdis <- panel$cumdis_g_lag / pmax(vill_sizes, 1)
m_share <- glm(Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
                 per + share_cumdis + children,
               data = panel, family = binomial("logit"))

# (3) Village FE + clustered SE by ego
m_vfe <- glm(Y ~ E_dis_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
               per + factor(g) + children,
             data = panel, family = binomial("logit"))
cl_vcov <- function(fit, cluster) {
  meat <- sandwich::vcovCL(fit, cluster = cluster, type = "HC1")
  list(coef = coef(fit), vcov = meat)
}
vfe_rob <- cl_vcov(m_vfe, panel$i)  # vfe_rob$coef ; sqrt(diag(vfe_rob$vcov))

# (4) Tenure in Modern up to t-1
# Build tenure matrix from meta_state (2=Modern)
tenure_mod <- matrix(0L, nrow = nrow(meta_state), ncol = ncol(meta_state))
for (i in seq_len(nrow(meta_state))) {
  run <- 0L
  for (tt in seq_len(ncol(meta_state))) {
    if (meta_state[i, tt] == 2L) { run <- run + 1L } else { run <- 0L }
    tenure_mod[i, tt] <- run
  }
}
tenure_lag <- tenure_mod[cbind(at_risk_df$i, at_risk_df$t - 1L)]
panel$tenure_lag <- tenure_lag
m_tenure <- glm(Y ~ E_dis_lag + E_adopt_lag + tenure_lag +
                  deg_in_lag + deg_out_lag + per + cumdis_g_lag + children,
                data = panel, family = binomial("logit"))

# (5) Interaction with time (linear trend alternative if per is factor)
# If 'per' is a factor, create a numeric trend:
panel$time_num <- as.numeric(panel$time)
m_inter <- glm(Y ~ E_dis_lag * time_num + E_adopt_lag +
                 deg_in_lag + deg_out_lag + per + cumdis_g_lag + children,
               data = panel, family = binomial("logit"))
summary(m_inter)

# Quick compare
extra <- list(
  degpos   = m_degpos,
  share    = m_share,
  vfe      = m_vfe,
  tenure   = m_tenure,
  interT   = m_inter
)
extra_cmp <- data.frame(
  model    = names(extra),
  beta_Edis= sapply(extra, function(m) coef(m)[grep("^E_dis", names(coef(m)))[1]]),
  AIC      = sapply(extra, AIC),
  BIC      = sapply(extra, BIC),
  rows     = sapply(extra, function(m) nrow(model.frame(m)))
)
print(extra_cmp, row.names = FALSE)

# Clustered SE printout for the VFE model (E_dis term)
edis_idx <- grep("^E_dis", names(vfe_rob$coef))[1]
cat("\nVillage FE (clustered by ego) — E_dis:\n")
cat("beta =", vfe_rob$coef[edis_idx],
    "se =", sqrt(diag(vfe_rob$vcov))[edis_idx], "\n")
