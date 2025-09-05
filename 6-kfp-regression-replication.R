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


# Parametric time trend version of the village model (linear Time instead of FE)
m_tbl102_vil_b <- diffreg(
  dn ~ exposure + exposure_se + deg_in + deg_out + media_exposure + children + per + cum_num_vil,
  type = "logit"
)
summary(m_tbl102_vil_b)

# ============================================================================
# REDEFINE "MODERN METHODS" (6 methods) AND RE-RUN VILLAGE CUM-ADOPTION MODEL
# ----------------------------------------------------------------------------
# New modern set: Loop, Oral Pill, Condom, Vasectomy, TL, Injection
# Goal: recompute TOA under this definition, rebuild exposures, and refit
#       the Table 10-2-style model with village-level cumulative adoption.
# ----------------------------------------------------------------------------

# Safety: infer T and n from the existing object
Tt <- 11L
n  <- nrow(kfamily)

# 1) Map fpstatus labels <-> codes
stopifnot(!is.null(attr(kfamily, "label.table")$fpstatus))
labtab <- attr(kfamily, "label.table")$fpstatus
label_to_code <- unclass(labtab)                 # named numeric
code_to_label <- setNames(names(labtab), labtab)

modern6_names <- c("Loop", "Oral Pill", "Condom", "Vasectomy", "TL", "Injection")
modern6_codes <- unname(label_to_code[modern6_names])

# 2) Derive TOA (first period 1..Tt where fpstatus is one of modern6)
fp_cols <- paste0("fpt", 1:Tt)
stopifnot(all(fp_cols %in% names(kfamily)))

# Build an n x T matrix of statuses (numeric codes)
fp_mat <- as.matrix(kfamily[, fp_cols])

# First time index with a modern6 code
toa6 <- rep(NA_integer_, n)
for (i in seq_len(n)) {
  row_codes <- fp_mat[i, ]
  if (all(is.na(row_codes))) next
  hit <- which(row_codes %in% modern6_codes)
  if (length(hit)) toa6[i] <- min(hit)
}

# Optional fallback using cfp/cbyr if available
if (all(c("cfp","cbyr") %in% names(kfamily))) {
  # Year map used earlier in your scripts (byr digit -> calendar year)
  year_map <- c("4"=1964,"5"=1965,"6"=1966,"7"=1967,"8"=1968,
                "9"=1969,"0"=1970,"1"=1971,"2"=1972,"3"=1973)
  for (i in seq_len(n)) {
    if (!is.na(toa6[i])) next
    if (is.na(kfamily$cfp[i]) || is.na(kfamily$cbyr[i])) next
    if (kfamily$cfp[i] %in% modern6_codes) {
      yr_chr <- as.character(kfamily$cbyr[i])
      if (!is.na(year_map[yr_chr])) {
        # Map to step 1..10 (keep bounded within 1..Tt)
        step <- as.integer(year_map[yr_chr] - 1963L)
        if (!is.na(step)) toa6[i] <- max(1L, min(Tt, step))
      }
    }
  }
}

# 3) Build a new diffnet object by cloning dn but replacing adoption timing
#    (we keep the same graphs/networks as before)
dn6 <- dn
attr(dn6, "name") <- "KFP (modern6)"
dn6$toa <- toa6

# 4) Recompute exposures (cohesion and structural equivalence) for dn6
#    NOTE: exposure() uses dn6$toa internally
try({ dn6[["exposure"]]    <- exposure(dn6) }, silent = TRUE)
try({ dn6[["exposure_se"]] <- exposure(dn6, alt.graph = "se", groupvar = "village", valued = TRUE) }, silent = TRUE)

# 5) Carry over / (re)attach covariates that don’t depend on adoption timing
#    Graph-based degrees are the same networks, so reuse from dn
if (!is.null(dn[["deg_in"]]))  dn6[["deg_in"]]  <- dn[["deg_in"]]
if (!is.null(dn[["deg_out"]])) dn6[["deg_out"]] <- dn[["deg_out"]]
# Dynamic period factor and individual covariates
if (!is.null(dn[["per"]]))            dn6[["per"]]            <- dn[["per"]]
if (!is.null(dn[["media_exposure"]])) dn6[["media_exposure"]] <- dn[["media_exposure"]]
if (!is.null(dn[["children"]]))       dn6[["children"]]       <- dn[["children"]]

# 6) Village-level cumulative adoption under the modern6 definition
vill6 <- kfamily$village
adopt_mat6 <- matrix(0L, nrow = n, ncol = Tt)
for (tt in seq_len(Tt)) adopt_mat6[, tt] <- as.integer(!is.na(toa6) & toa6 <= tt)

Gs <- sort(unique(vill6))
g_sizes <- setNames(sapply(Gs, function(g) sum(vill6 == g)), Gs)

cum_num_vil6 <- matrix(NA_real_, nrow = n, ncol = Tt)
for (tt in seq_len(Tt)) {
  counts_by_g <- setNames(sapply(Gs, function(g) sum(adopt_mat6[vill6 == g, tt], na.rm = TRUE)), Gs)
  cum_num_vil6[, tt] <- counts_by_g[as.character(vill6)]
}

dn6[["cum_num_vil6"]] <- cum_num_vil6

# 7) Refit the Table 10-2 style model with the new modern6 definition
m_tbl102_vil_mod6 <- diffreg(
  dn6 ~ exposure + exposure_se + deg_in + deg_out + media_exposure + children + factor(per) + cum_num_vil6,
  type = "logit"
)
cat("\n==================== Modern(6) model ====================\n")
print(summary(m_tbl102_vil_mod6))


# Parametric time trend version under modern(6) definition

m_tbl102_vil_mod6_b <- diffreg(
  dn6 ~ exposure + exposure_se + deg_in + deg_out + media_exposure + children + per + cum_num_vil6,
  type = "logit"
)
summary(m_tbl102_vil_mod6_b)

# ============================================================================
# 8) Quick side-by-side AIC/BIC compare (original 8-method vs new 6-method)
if (exists("m_tbl102_vil")) {
  cat("\nModel fit comparison (lower is better):\n")
  aic_old <- tryCatch(AIC(m_tbl102_vil), error = function(e) NA)
  aic_new <- tryCatch(AIC(m_tbl102_vil_mod6), error = function(e) NA)
  bic_old <- tryCatch(BIC(m_tbl102_vil), error = function(e) NA)
  bic_new <- tryCatch(BIC(m_tbl102_vil_mod6), error = function(e) NA)
  print(data.frame(
    model = c("modern(8)", "modern(6)"),
    AIC   = c(aic_old, aic_new),
    BIC   = c(bic_old, bic_new)
  ))
}
