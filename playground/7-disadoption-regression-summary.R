# ================================================================
# Disadoption exposures with rolling windows (flow-style) 
# + VFE (Village Fixed Effects)
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
#Tt <- 9L
Tt <- 11L

fp_cols <- get_fpt_cols(kfamily)
fp_cols <- fp_cols[seq_len(Tt)]
stopifnot(length(fp_cols) >= 2L)

# -------------------- Method coding (modern vs others) -----------
labtab <- attr(kfamily, "label.table")$fpstatus
stopifnot(!is.null(labtab))

label_to_code <- unclass(labtab)
modern6_names <- c("Loop", "Oral Pill", "Condom", "Vasectomy", "TL", "Injection")
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

# ---------------- Simple controls (children, media) -------------
own_cols <- intersect(paste0("media", 1:5),  names(kfamily))
exp_cols <- intersect(paste0("media", 6:19), names(kfamily))
own_sum  <- if (length(own_cols)) rowSums(kfamily[, own_cols, drop = FALSE], na.rm = TRUE) else rep(NA_real_, n)
exp_sum  <- if (length(exp_cols)) rowSums(kfamily[, exp_cols, drop = FALSE], na.rm = TRUE) else rep(NA_real_, n)
media_exposure <- ifelse(!is.na(exp_sum) & !is.na(own_sum), exp_sum / pmax(own_sum, 1), NA_real_)

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
E_flow_num  <- matrix(0, n, Tt)
for (tau in 2:Tt) {
  A   <- as.matrix(dn$graph[[tau]])
  ki  <- pmax(rowSums(A != 0, na.rm = TRUE), 0)
  # transition indicator on alters
  trans_vec <- (meta_state[, tau-1] == 2L) & (meta_state[, tau] != 2L)
  num <- as.numeric(A %*% trans_vec)
  E_flow_num[,  tau] <- num
  E_flow_prop[, tau] <- ifelse(ki > 0, num / ki, 0)
}

# ----------------- Rolling-window flow exposures -----------------
# For a window W, at (i,t) we'll use the sum of flow proportions over the last W transitions
# ending at t-1: tau in [max(2, t-W) .. (t-1)]. (Panel uses lag_pick so we store as "ending at t")
windows <- 1:6

roll_sum <- function(M, W) {
  out <- matrix(0, nrow(M), ncol(M))
  for (tt in 1:ncol(M)) {
    # transitions available for a window ending at tt
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

panel <- within(at_risk_df, {
  Y            <- as.integer(TOD[i] == t)
  per          <- factor(t)
  E_adopt_lag  <- lag_pick(E_adopt)
  E_gap_lag    <- lag_pick(E_gap)
  deg_in_lag   <- lag_pick(deg_in)
  deg_out_lag  <- lag_pick(deg_out)
  cumdis_g_lag <- lag_pick(cum_dis_vil)
  media_lag    <- media_exposure[i]
  children     <- children_vec[i]
  g            <- vill[i]
})

# drop rows with missing key covariates
panel <- subset(panel, !is.na(E_adopt_lag) & !is.na(E_gap_lag) &
                  !is.na(deg_in_lag)  & !is.na(deg_out_lag) &
                  !is.na(cumdis_g_lag))

fam <- binomial("logit")

cat("\n=== SAMPLE SIZES ===\n")
cat("# panel rows:", nrow(panel), "| unique egos:", length(unique(panel$i)),
    "| events:", sum(panel$Y), "\n")

# Interpretation: Each row of the panel is an ego–period at risk of disadoption (total = 1634).
# These rows come from 377 distinct egos (all ever-modern users).
# There are 377 events, meaning each ego disadopt in at the end.
# On average, each ego contributes about 4–5 risk periods until the disadoption occurs.

# ----------------- (A) Stable disadoption: GAP + Village FE -------
m_gap_vfe <- glm(Y ~ E_gap_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
                   per + cumdis_g_lag + children + factor(g),
                 data = panel, family = fam)
cat("\n--- GAP + Village FE ---\n")
print(summary(m_gap_vfe))

exp(coef(m_gap_vfe)["E_gap_lag"])
(1 - exp(coef(m_gap_vfe)["E_gap_lag"]))

# Interpretation: Within villages, a larger prior gap between maximum and individual 
# exposure to disadoption (E_max−E) is associated with a significantly lower probability 
# of disadopting a modern contraceptive method. OR=0.328 
# (i.e., each unit increase in the gap reduces the odds of disadoption by 67%).

# ----------------- (B) FLOW windows: baseline + VFE ---------------
do_flow_models <- function(W, flowW_prop) {
  # add windowed exposure (lagged via lag_pick on window-sum that ends at t-1)
  E_flowW_lag <- lag_pick(flowW_prop)
  dat <- within(panel, { E_flowW_lag <- E_flowW_lag })
  
  base_form <- Y ~ E_flowW_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
    per + cumdis_g_lag + children
  vfe_form  <- update(base_form, . ~ . + factor(g))
  
  m_base <- glm(base_form, data = dat, family = fam)
  m_vfe  <- glm(vfe_form,  data = dat, family = fam)
  
  # Short interpretation right after each fit
  b  <- coef(m_base)["E_flowW_lag"];  or  <- exp(b)
  bv <- coef(m_vfe )["E_flowW_lag"];  orv <- exp(bv)
  
  
  pb  <- summary(m_base)$coefficients["E_flowW_lag", "Pr(>|z|)"]
  pbv <- summary(m_vfe )$coefficients["E_flowW_lag", "Pr(>|z|)"]

  cat(sprintf("[Baseline]  W=%d, beta=%.3f (OR=%.3f), AIC=%.3f, p=%.3g.\n", W, b, or, AIC(m_base), pb))
  cat(sprintf("[VFE]       W=%d, beta=%.3f (OR=%.3f), AIC=%.3f, p=%.3g.\n", W, bv, orv, AIC(m_vfe), pbv))
  
  invisible(list(base = m_base, vfe = m_vfe))
}

flow_models <- lapply(seq_along(windows), function(k) {
  W <- windows[k]
  do_flow_models(W, flowW_prop_list[[k]])
})
names(flow_models) <- paste0("W", windows)

# Summary of the best model
summary(flow_models$W5$vfe) 

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







# ================================================================
# Add Age, Age at marriage, and Spousal communication to:
# (A) Stable disadoption (GAP + VFE)
# (B) FLOW windows (baseline + VFE), for each W in `windows`
# Prints full model summaries (summary(model)).
# ================================================================

# ---- Build covariates on the panel (pull from kfamily by ego id) ----
stopifnot(exists("panel"), exists("kfamily"))

# Age & Age at marriage (baseline; constant over time)
panel$age_lag    <- kfamily$age[panel$i]
panel$agemar_lag <- kfamily$agemar[panel$i]

# Spousal communication on FP (codebook: comop2 1=No, 2=Yes)
panel$comop2_yes <- as.integer(kfamily$comop2[panel$i] == 2)

# Small helper to pretty print a model header then its full summary
print_model <- function(mod, title) {
  cat("\n", paste(rep("=", nchar(title) + 4), collapse=""), "\n", sep="")
  cat("= ", title, " =\n", sep="")
  cat(paste(rep("=", nchar(title) + 4), collapse=""), "\n", sep="")
  print(summary(mod))
}

# -------------------------- (A) GAP + VFE ---------------------------
# Base (from your script): Y ~ E_gap_lag + controls + factor(g)
base_A <- Y ~ E_gap_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
  per + cumdis_g_lag + children + factor(g)

# All combinations of the three covariates:
#  - none
#  - age
#  - agemar
#  - comop2
#  - age + agemar
#  - age + comop2
#  - agemar + comop2
#  - age + agemar + comop2
A_forms <- list(
  "A0: GAP+VFE"                                   = base_A,
  "A1: GAP+VFE + age"                              = update(base_A, . ~ . + age_lag),
  "A2: GAP+VFE + agemar"                           = update(base_A, . ~ . + agemar_lag),
  "A3: GAP+VFE + comop2"                           = update(base_A, . ~ . + comop2_yes),
  "A4: GAP+VFE + age + agemar"                     = update(base_A, . ~ . + age_lag + agemar_lag),
  "A5: GAP+VFE + age + comop2"                     = update(base_A, . ~ . + age_lag + comop2_yes),
  "A6: GAP+VFE + agemar + comop2"                  = update(base_A, . ~ . + agemar_lag + comop2_yes),
  "A7: GAP+VFE + age + agemar + comop2"            = update(base_A, . ~ . + age_lag + agemar_lag + comop2_yes)
)

cat("\n\n==================== (A) STABLE DISADOPTION: GAP + VFE ====================\n")
A_models <- lapply(names(A_forms), function(lbl) {
  fm <- A_forms[[lbl]]
  mod <- glm(fm, data = panel, family = fam)
  print_model(mod, lbl)
  invisible(mod)
})
names(A_models) <- names(A_forms)

# ----------------------- (B) FLOW windows (1..W) --------------------
# For each W, build E_flowW_lag, then fit baseline and VFE versions
# with the same set of covariate combinations as in (A).

stopifnot(exists("windows"), exists("flowW_prop_list"), length(flowW_prop_list) == length(windows))

fit_flow_block <- function(W, flow_prop_mat) {
  # lagged window exposure (ends at t-1 by construction in your script)
  E_flowW_lag <- lag_pick(flow_prop_mat)
  
  dat <- within(panel, { E_flowW_lag <- E_flowW_lag })
  
  base_B <- Y ~ E_flowW_lag + E_adopt_lag + deg_in_lag + deg_out_lag +
    per + cumdis_g_lag + children
  vfe_B  <- update(base_B, . ~ . + factor(g))
  
  # Build the list of formulas
  B_formulas <- list(
    base_B,
    update(base_B, . ~ . + age_lag),
    update(base_B, . ~ . + agemar_lag),
    update(base_B, . ~ . + comop2_yes),
    update(base_B, . ~ . + age_lag + agemar_lag),
    update(base_B, . ~ . + age_lag + comop2_yes),
    update(base_B, . ~ . + agemar_lag + comop2_yes),
    update(base_B, . ~ . + age_lag + agemar_lag + comop2_yes),
    vfe_B,
    update(vfe_B, . ~ . + age_lag),
    update(vfe_B, . ~ . + agemar_lag),
    update(vfe_B, . ~ . + comop2_yes),
    update(vfe_B, . ~ . + age_lag + agemar_lag),
    update(vfe_B, . ~ . + age_lag + comop2_yes),
    update(vfe_B, . ~ . + agemar_lag + comop2_yes),
    update(vfe_B, . ~ . + age_lag + agemar_lag + comop2_yes)
  )
  
  B_names <- c(
    paste0("B0.W",W,": FLOW W=",W," (Baseline)"),
    paste0("B1.W",W,": FLOW W=",W," + age (Baseline)"),
    paste0("B2.W",W,": FLOW W=",W," + agemar (Baseline)"),
    paste0("B3.W",W,": FLOW W=",W," + comop2 (Baseline)"),
    paste0("B4.W",W,": FLOW W=",W," + age + agemar (Baseline)"),
    paste0("B5.W",W,": FLOW W=",W," + age + comop2 (Baseline)"),
    paste0("B6.W",W,": FLOW W=",W," + agemar + comop2 (Baseline)"),
    paste0("B7.W",W,": FLOW W=",W," + age + agemar + comop2 (Baseline)"),
    paste0("BV0.W",W,": FLOW W=",W," + VFE"),
    paste0("BV1.W",W,": FLOW W=",W," + age + VFE"),
    paste0("BV2.W",W,": FLOW W=",W," + agemar + VFE"),
    paste0("BV3.W",W,": FLOW W=",W," + comop2 + VFE"),
    paste0("BV4.W",W,": FLOW W=",W," + age + agemar + VFE"),
    paste0("BV5.W",W,": FLOW W=",W," + age + comop2 + VFE"),
    paste0("BV6.W",W,": FLOW W=",W," + agemar + comop2 + VFE"),
    paste0("BV7.W",W,": FLOW W=",W," + age + agemar + comop2 + VFE")
  )
  
  B_forms <- setNames(B_formulas, B_names)
  
  cat("\n\n==================== (B) FLOW WINDOWS: W =", W, "====================\n")
  mods <- lapply(names(B_forms), function(lbl) {
    fm <- B_forms[[lbl]]
    mod <- glm(fm, data = dat, family = fam)
    print_model(mod, lbl)
    invisible(mod)
  })
  names(mods) <- names(B_forms)
  invisible(mods)
}

B_models <- lapply(5:6, function(k) {
  W <- windows[k]
  flow_prop_mat <- flowW_prop_list[[k]]
  fit_flow_block(W, flow_prop_mat)
})
names(B_models) <- paste0("W", windows)

# ---------------------------- Done ----------------------------------
cat("\n\n[Done] Fitted all combinations for (A) GAP+VFE and (B) FLOW windows (Baseline + VFE).\n")