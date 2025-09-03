# Replication of Valente 2010 Table 10-2 using netdiffuseR

library(netdiffuseR)

## 1) From survey to diffnet (per the “Reading data” + showcase docs)
data(kfamily, package="netdiffuseR")
netvars <- grep("^net", names(kfamily), value = TRUE)
surveyed <- kfamily$id
for (v in netvars) kfamily[[v]][ !(kfamily[[v]] %in% surveyed) ] <- NA

dn <- survey_to_diffnet(
  toavar   = "toa",
  netvars  = netvars,
  idvar    = "id",
  groupvar = if ("village" %in% names(kfamily)) "village" else NULL,
  dat     = kfamily
)

## 2) Add exposures as dynamic attributes (workshop recipe)
dn[["exposure"]] <- exposure(dn)  # cohesion exposure (proportion of direct adopters at t-1)

## structural equivalence exposure (workshop version)
dn[["exposure_se"]] <- exposure(
  dn,
  alt.graph = "se",              # let netdiffuseR compute SE internally
  groupvar  = "village",         # if you have village/community
  valued    = TRUE
)

## 3) Add adoption/cumulative adoption as an attribute (workshop pattern)
dn[["adoption"]] <- dn$cumadopt  # <- THIS LINE avoids the obj-missing error

## 4) Optional: per (time) factor, and any other covariates
# If sons/pregs/media12 exist in kfamily, attach them as dynamic attrs:
Tt <- 11L#nperiods(dn)
n <- nvertices(dn)
expand <- function(x) if (is.null(x)) NULL else matrix(rep(x, Tt), nrow=n, ncol=Tt)

if ("sons"    %in% names(kfamily)) dn[["sons"]]    <- expand(kfamily$sons)
if ("pregs"   %in% names(kfamily)) dn[["pregs"]]   <- expand(kfamily$pregs)   # total pregnancies
if ("media12" %in% names(kfamily)) dn[["media12"]] <- expand(kfamily$media12) # FP poster exposure

## Period as factor (workshop uses factor(per) in the formula)
per <- matrix(rep(seq_len(Tt), each=n), nrow=n, ncol=Tt)
dn[["per"]] <- per

## 5) Fit models (workshop uses probit; you can switch to logit if you prefer)
# Model aligned to your snippet:
model_1 <- diffreg(
  dn ~ exposure + exposure_se + media12 + sons + pregs + factor(per),
  type = "probit"
)
summary(model_1)

# ==============================================================================

## ---- Degrees per period (number sent/received) ---------------------------
deg_out <- deg_in <- matrix(NA_real_, n, Tt)
for (tt in seq_len(Tt)) {
  A <- as.matrix(dn$graph[[tt]])
  deg_out[,tt] <- rowSums(A != 0, na.rm = TRUE)
  deg_in[,tt]  <- colSums(A != 0, na.rm = TRUE)
}
dn[["deg_out"]] <- deg_out  # Number sent
dn[["deg_in"]]  <- deg_in   # Number received

## ---- Media exposure (composite) -------------------------------------------
# Identify the ownership (media1:media5) and FP campaign exposure items (media6:media19)
own_cols <- intersect(paste0("media", 1:5),  names(kfamily))
exp_cols <- intersect(paste0("media", 6:19), names(kfamily))

# Build indices (avoid divide-by-zero using pmax)
own_idx <- if (length(own_cols)) rowSums(kfamily[, own_cols, drop = FALSE], na.rm = TRUE) else NULL
exp_sum <- if (length(exp_cols)) rowSums(kfamily[, exp_cols, drop = FALSE], na.rm = TRUE) else NULL

media_exposure <- if (!is.null(exp_sum) && !is.null(own_idx)) exp_sum / pmax(own_idx, 1) else NULL

# Attach as dynamic attribute (n x T)
if (!is.null(media_exposure)) {
  dn[["media_exposure"]] <- matrix(rep(media_exposure, Tt), nrow = n, ncol = Tt)
}

## ---- Number of children ----------------------------------------------------
kids <- NULL
if (all(c("sons","daughts") %in% names(kfamily))) {
  kids <- kfamily$sons + kfamily$daughts
} else if ("children" %in% names(kfamily)) {
  kids <- kfamily$children
}

if (!is.null(kids)) {
  dn[["children"]] <- matrix(rep(kids, Tt), nrow = n, ncol = Tt)
}

## ---- Cumulative adoption (system-level, 'num') -----------------------------
cum_tbl <- cumulative_adopt_count(dn)             # 3 x T (rows: num/prop/rate)
cum_num <- as.numeric(cum_tbl["num", ])           # length T
# Expand to n x T so it can enter the diffreg formula
dn[["cum_num"]] <- matrix(rep(cum_num, each = n), nrow = n, ncol = Tt)


## ---- Table 10-2 style regressions -----------------------------------------
# Minimal core (close to the book’s highlighted rows)
m_tbl102_core <- diffreg(
  dn ~ exposure + exposure_se + deg_in + deg_out + factor(per),
  type = "logit"
)
summary(m_tbl102_core)

# Add Media exposure and Number of children
m_tbl102_plus <- diffreg(
  dn ~ exposure + exposure_se + deg_in + deg_out + media_exposure + children + factor(per),
  type = "logit"
)
summary(m_tbl102_plus)

# ------------------------------------------------------------------------------
# Strategy A — Use system-level cumulative adoption as a time proxy (no FE)
# (Drop factor(per) to avoid collinearity with cum_* which only varies by period)

m_tbl102_sys_count <- diffreg(
  dn ~ exposure + exposure_se + deg_in + deg_out + media_exposure + children + cum_num,
  type = "logit"
)
summary(m_tbl102_sys_count)

# ------------------------------------------------------------------------------
# Strategy B — Village-level cumulative adoption with period fixed effects
# (Varies across villages within a period, so it is identified alongside factor(per))

# Build village-level cumulative adoption (count and proportion) by period
vill <- kfamily$village
toa  <- dn$toa
# adoption matrix: 1 if adopted by period t, else 0
adopt_mat <- matrix(0L, nrow = n, ncol = Tt)
for (tt in seq_len(Tt)) adopt_mat[, tt] <- as.integer(!is.na(toa) & toa <= tt)

Gs <- sort(unique(vill))
g_sizes <- setNames(sapply(Gs, function(g) sum(vill == g)), Gs)

cum_num_vil  <- matrix(NA_real_, nrow = n, ncol = Tt)
cum_prop_vil <- matrix(NA_real_, nrow = n, ncol = Tt)
for (tt in seq_len(Tt)) {
  counts_by_g <- setNames(sapply(Gs, function(g) sum(adopt_mat[vill == g, tt], na.rm = TRUE)), Gs)
  cum_num_vil[, tt]  <- counts_by_g[as.character(vill)]
  cum_prop_vil[, tt] <- counts_by_g[as.character(vill)] / g_sizes[as.character(vill)]
}
# Attach and optionally scale
dn[["cum_num_vil"]]   <- cum_num_vil

m_tbl102_vil <- diffreg(
  dn ~ exposure + exposure_se + deg_in + deg_out + media_exposure + children + factor(per) + cum_num_vil,
  type = "logit"
)
summary(m_tbl102_vil)