# ================================================================
# 7-disadoption-regression-final.R
# Stable disadoption (A) vs Temporary/Flow exposure (B)
# Using covariates: cumdis_g_lag, children, age_lag, agemar_lag, comop2_yes
# + Village Fixed Effects (VFE)
# ================================================================

library(netdiffuseR)

# -------------------- Load & prep base data ---------------------
data(kfamily, package = "netdiffuseR")
n <- nrow(kfamily)

# Keep ties only among surveyed ids
netvars  <- grep("^net", names(kfamily), value = TRUE)
surveyed <- kfamily$id
for (v in netvars) kfamily[[v]][ !(kfamily[[v]] %in% surveyed) ] <- NA

# Build diffnet with groups if present
dn <- survey_to_diffnet(
  toavar   = "toa",
  netvars  = netvars,
  idvar    = "id",
  groupvar = if ("village" %in% names(kfamily)) "village" else NULL,
  dat      = kfamily
)

vill <- if ("village" %in% names(kfamily)) kfamily$village else rep(1L, n)

# ----- Time axis from fpt1..fptK (limit to 11 survey periods) -----
get_fpt_cols <- function(df) {
  fpt <- grep("^fpt\\d+$", names(df), value = TRUE)
  idx <- as.integer(sub("^fpt", "", fpt))
  fpt[order(idx)]
}
Tt <- 11L
fp_cols <- get_fpt_cols(kfamily)
fp_cols <- fp_cols[seq_len(Tt)]
stopifnot(length(fp_cols) >= 2L)

# -------------------- Method coding (modern vs others) -----------
labtab <- attr(kfamily, "label.table")$fpstatus
stopifnot(!is.null(labtab))
label_to_code <- unclass(labtab)

modern6_names <- c("Loop","Oral Pill","Condom","Vasectomy","TL","Injection")
trad_names    <- intersect(c("Rhythm","Withdrawal","Jelly","Foam"), names(label_to_code))
modern6_codes <- unname(label_to_code[modern6_names])
trad_codes    <- if (length(trad_names)) unname(label_to_code[trad_names]) else integer(0)

fp_mat <- as.matrix(kfamily[, fp_cols, drop = FALSE])
is_modern      <- (fp_mat %in% modern6_codes)
is_traditional <- (fp_mat %in% trad_codes)
if (is.null(dim(is_modern)))      is_modern      <- matrix(is_modern,      nrow = n, ncol = Tt)
if (is.null(dim(is_traditional))) is_traditional <- matrix(is_traditional, nrow = n, ncol = Tt)

# meta_state: 2=Modern, 1=Traditional, 0=None/other
meta_state <- matrix(0L, n, Tt)
meta_state[is_traditional] <- 1L
meta_state[is_modern]      <- 2L

# Stable disadoption time = last modern time (TOD)
last_modern <- apply(is_modern, 1, function(z) if (any(z, na.rm=TRUE)) max(which(z)) else NA_integer_)
TOD <- last_modern

# Ever-modern subset (only those who were modern at least once)
ever_modern <- rowSums(is_modern, na.rm = TRUE) > 0
keep_idx    <- which(ever_modern)
stopifnot(length(keep_idx) > 0)

# ----------------- Degrees per period (in/out) ------------------
deg_out <- deg_in <- matrix(0, n, Tt)
for (tt in seq_len(Tt)) {
  A <- as.matrix(dn$graph[[tt]])
  if (!is.matrix(A)) A <- matrix(0, n, n)
  deg_out[, tt] <- rowSums(A != 0, na.rm = TRUE)
  deg_in[,  tt] <- colSums(A != 0, na.rm = TRUE)
}

# ---------------- Simple controls (children) --------------------
children_vec <- if (all(c("sons","daughts") %in% names(kfamily))) {
  as.numeric(kfamily$sons) + as.numeric(kfamily$daughts)
} else rep(NA_real_, n)

# ----------------- Adoption exposure E(t) and E_max -------------
# E_adopt (cohesion): share of alters modern at t-1 among current alters at t
E_adopt <- matrix(0, n, Tt)
for (tt in 2:Tt) {
  A  <- as.matrix(dn$graph[[tt]])
  ki <- pmax(rowSums(A != 0, na.rm = TRUE), 0)
  neigh_mod_t1 <- as.numeric(A %*% (meta_state[, tt-1] == 2L))
  E_adopt[, tt] <- ifelse(ki > 0, neigh_mod_t1 / ki, 0)
}
E_max <- t(apply(E_adopt, 1, function(z) cummax(ifelse(is.finite(z), z, 0))))
E_gap <- pmax(E_max - E_adopt, 0) # classic "gap" disadoption exposure

# ----------------- Flow exposure at each transition --------------
# Flow indicator at transition tau: among current alters (at tau),
# fraction that were modern at tau-1 and *not* modern at tau.
E_flow_prop <- matrix(0, n, Tt)
for (tau in 2:Tt) {
  A   <- as.matrix(dn$graph[[tau]])
  ki  <- pmax(rowSums(A != 0, na.rm = TRUE), 0)
  trans_vec <- (meta_state[, tau-1] == 2L) & (meta_state[, tau] != 2L)
  num <- as.numeric(A %*% trans_vec)
  E_flow_prop[, tau] <- ifelse(ki > 0, num / ki, 0)
}

# ----------------- Rolling-window flow exposures -----------------
windows <- 1:6
roll_sum <- function(M, W) {
  out <- matrix(0, nrow(M), ncol(M))
  for (tt in 1:ncol(M)) {
    lo <- max(2L, tt - W + 1L); hi <- tt
    if (lo <= hi) out[, tt] <- rowSums(M[, lo:hi, drop=FALSE], na.rm=TRUE)
  }
  out
}
flowW_prop_list <- lapply(windows, function(W) roll_sum(E_flow_prop, W))
names(flowW_prop_list) <- paste0("W", windows)

# ----------------- Village cumulative disadoptions ---------------
Gs <- sort(unique(vill))
event_T <- rep(NA_integer_, n)
event_T[keep_idx] <- TOD[keep_idx]
cum_dis_vil <- matrix(NA_real_, n, Tt)
for (tt in 1:Tt) {
  counts_by_g <- sapply(Gs, function(g) {
    idx_g <- which(vill == g)
    sum(!is.na(event_T[idx_g]) & event_T[idx_g] <= tt, na.rm = TRUE)
  })
  cum_dis_vil[, tt] <- counts_by_g[vill]
}

# ----------------- Risk panel for stable disadoption -------------
at_risk_rows <- lapply(keep_idx, function(i) {
  t_star <- TOD[i]
  if (is.na(t_star) || t_star < 2L) return(NULL)
  data.frame(i = i, t = seq.int(2L, min(t_star, Tt)))
})
at_risk_df <- do.call(rbind, at_risk_rows)
row.names(at_risk_df) <- NULL
stopifnot(nrow(at_risk_df) > 0L)

lag_pick <- function(M) {
  idx <- cbind(at_risk_df$i, at_risk_df$t - 1L)
  bad <- idx[,1] < 1L | idx[,1] > nrow(M) | idx[,2] < 1L | idx[,2] > ncol(M)
  if (any(bad)) stop("lag_pick: invalid (i,t-1) pairs.")
  M[idx]
}

# Baseline covariates we’ll use (repeated by row via i)
age_vec    <- if ("age"    %in% names(kfamily)) as.numeric(kfamily$age)    else rep(NA_real_, n)
agemar_vec <- if ("agemar" %in% names(kfamily)) as.numeric(kfamily$agemar) else rep(NA_real_, n)
# Spousal communication on FP: comop2 (1=No; 2=Yes) -> binary 0/1 (Yes=1)
comop2_yes_vec <- if ("comop2" %in% names(kfamily)) as.integer(kfamily$comop2 == 2L) else rep(NA_integer_, n)

panel <- within(at_risk_df, {
  Y            <- as.integer(TOD[i] == t)
  per          <- factor(t)
  E_adopt_lag  <- lag_pick(E_adopt)
  E_gap_lag    <- lag_pick(E_gap)
  deg_in_lag   <- lag_pick(deg_in)
  deg_out_lag  <- lag_pick(deg_out)
  cumdis_g_lag <- lag_pick(cum_dis_vil)
  children     <- children_vec[i]
  g            <- vill[i]
  age_lag      <- age_vec[i]
  agemar_lag   <- agemar_vec[i]
  comop2_yes   <- comop2_yes_vec[i]
})

# drop rows with missing key covariates
panel <- subset(panel, !is.na(E_adopt_lag) & !is.na(E_gap_lag) &
                  !is.na(deg_in_lag)  & !is.na(deg_out_lag) &
                  !is.na(cumdis_g_lag) &
                  !is.na(age_lag) & !is.na(agemar_lag) & !is.na(comop2_yes))

fam <- binomial("logit")

#=== SAMPLE SIZES ===
cat("# panel rows:", nrow(panel), "| unique egos:", length(unique(panel$i)),
    "| events:", sum(panel$Y), "\n")

# Interpretation: Each row of the panel is an ego–period at risk of disadoption (total = 1634).
# These rows come from 377 distinct egos (all ever-modern users).
# There are 377 events, meaning each ego disadopt in at the end.
# On average, each ego contributes about 4–5 risk periods until the disadoption occurs.

cat("cor(age_lag, agemar_lag) =", cor(panel$age_lag, panel$agemar_lag, use="complete.obs"))
# Interpretation: No high correlation here.

# =================================================================
# (A) STABLE DISADOPTION: GAP + VFE with added covariates
# =================================================================

print_model <- function(mod, label) {
  cat("\n===============\n= ", label, "=\n===============\n", sep = "")
  print(summary(mod))
}

# A0: GAP+VFE (reference)
A0 <- glm(Y ~ E_gap_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
            per + cumdis_g_lag + children + factor(g),
          data = panel, family = fam)
print_model(A0, "A0: GAP+VFE")
# Interpretation: The GAP term (E_max−E) is significantly protective (OR < 1).
# Controlling for village FE and time (per) this association persists.

# A1: + age
A1 <- glm(Y ~ E_gap_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
            per + cumdis_g_lag + children + factor(g) +
            age_lag,
          data = panel, family = fam)
print_model(A1, "A1: GAP+VFE + age")
# Interpretation: Older age increases disadoption odds (significant).
# GAP remains protective.

# A2: + agemar
A2 <- glm(Y ~ E_gap_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
            per + cumdis_g_lag + children + factor(g) +
            agemar_lag,
          data = panel, family = fam)
print_model(A2, "A2: GAP+VFE + agemar")
# Interpretation: Higher age at marriage is associated with lower disadoption odds (significant).
# GAP remains protective.

# A3: + comop2 (spousal communication)
A3 <- glm(Y ~ E_gap_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
            per + cumdis_g_lag + children + factor(g) +
            comop2_yes,
          data = panel, family = fam)
print_model(A3, "A3: GAP+VFE + comop2")
# Interpretation: Direction suggests communication may reduce risk (OR < 1) but not significant here.

# A4: + age + agemar
A4 <- glm(Y ~ E_gap_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
            per + cumdis_g_lag + children + factor(g) +
            age_lag + agemar_lag,
          data = panel, family = fam)
print_model(A4, "A4: GAP+VFE + age + agemar")
# Interpretation: Both age (+) and age at marriage (−) move as before; E_gap_lag remains protective.
# This was the best-performing (A) in prior runs by AIC.

# A5: + age + comop2
A5 <- glm(Y ~ E_gap_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
            per + cumdis_g_lag + children + factor(g) +
            age_lag + comop2_yes,
          data = panel, family = fam)
print_model(A5, "A5: GAP+VFE + age + comop2")
# Interpretation: Age remains strong; communication remains negative but non-significant.

# A6: + agemar + comop2
A6 <- glm(Y ~ E_gap_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
            per + cumdis_g_lag + children + factor(g) +
            agemar_lag + comop2_yes,
          data = panel, family = fam)
print_model(A6, "A6: GAP+VFE + agemar + comop2")
# Interpretation: Age at marriage significant (protective); communication ns.

# A7: + age + agemar + comop2 (full A)
A7 <- glm(Y ~ E_gap_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
            per + cumdis_g_lag + children + factor(g) +
            age_lag + agemar_lag + comop2_yes,
          data = panel, family = fam)
print_model(A7, "A7: GAP+VFE + age + agemar + comop2")
# Interpretation: This “full A” retains: GAP protective; age risk↑ (sig); 
# age at marriage protective (marginal); spousal communication negative but ns. 

# --- Overall, (A4) and (A7) are the strongest. --- !

print_model(A4, "A4: GAP+VFE + age + agemar")
print_model(A7, "A7: GAP+VFE + age + agemar + comop2")

exp(coef(A4)["E_gap_lag"])
(1 - exp(coef(A4)["E_gap_lag"]))

# Interpretation: Within villages, a larger prior gap between maximum and individual 
# exposure to disadoption (E_max−E) is associated with a significantly lower probability 
# of disadopting a modern contraceptive method. OR=0.347 
# (i.e., each unit increase in the gap reduces the odds of disadoption by 65%).

# =================================================================
# (B) TEMPORARY / FLOW-STYLE DISADOPTION (with VFE) ACROSS WINDOWS
# =================================================================

# (B) measures recent "neighbor transitions out of modern" (modern --> non-modern).
# For each ego–period, E_flowW_lag sums the fraction of alters who disadopted
# in the last W transitions ending at t−1 (windows W=1..6). This captures
# whether "recent local churn away from modern" increases the ego’s disadoption risk.

do_flow_vfe <- function(W, flowW_prop) {
  E_flowW_lag <- lag_pick(flowW_prop)
  dat <- within(panel, { E_flowW_lag <- E_flowW_lag })
  vfe_form <- Y ~ E_flowW_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
    per + cumdis_g_lag + children + age_lag + agemar_lag + comop2_yes + factor(g)
  m <- glm(vfe_form, data = dat, family = fam)
  co <- summary(m)$coefficients
  b  <- unname(co["E_flowW_lag","Estimate"])
  se <- unname(co["E_flowW_lag","Std. Error"])
  z  <- unname(co["E_flowW_lag","z value"])
  p  <- unname(co["E_flowW_lag","Pr(>|z|)"])
  or <- exp(b)
  cat(sprintf("[VFE E_flowW_lag] W=%d  beta=%.3f (OR=%.3f), SE=%.3f, z=%.2f, p=%.3g, AIC=%.3f\n",
              W, b, or, se, z, p, AIC(m)))
  invisible(m)
}

B_models <- list()
best_AIC <- Inf
best_W   <- NA_integer_
for (k in seq_along(windows)) {
  W <- windows[k]
  m <- do_flow_vfe(W, flowW_prop_list[[k]])
  B_models[[paste0("W",W)]] <- m
  if (AIC(m) < best_AIC) { best_AIC <- AIC(m); best_W <- W }
}

# Interpretation: Flow-style exposures (neighbors who recently left modern methods) 
# show a pattern where the sign of the effect changes with the window size. 
# With very short windows (W=1–2), coefficients are positive (OR > 1) but not 
# significant, suggesting a possible—but weak—signal that very recent disadoptions 
# might increase ego’s risk. As the window expands (W=3–6), the coefficients turn 
# negative (OR < 1), indicating that accumulated or slightly older neighbor 
# disadoptions are associated with lower odds of ego disadoption. 
# None of the results reach strong significance, but the consistency of the sign 
# shift implies that the immediate versus accumulated context of disadoptions 
# could matter. Within-village fixed effects (VFE) preserve the same pattern, 
# reinforcing that these dynamics are not purely driven by between-village 
# heterogeneity.

# Best model (lower p-value for E_flowW_lag): (BV6.W5)
print(summary(B_models$W5))