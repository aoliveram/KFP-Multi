# ============================================================
# TOA_derivado_general: objeto no colapsado de trayectorias
# - Preserva QUIEN (meta), CUANDO (periodo t=1..12) y QUE (estado)
# - Incluye matriz de transiciones por periodo (S x S x 11)
# - Imputa con cfp/cbyr SOLO donde falta fpt, sin sobreescribir fpt
# ============================================================


library(netdiffuseR)


data(kfamily)

# ----------------------------
# 0) Parametrización y helpers
# ----------------------------

P <- 12  # numero de periodos fpt1..fpt12 (fijo a 12)
n <- nrow(kfamily)

# Mapa de "ultimo dígito" de año -> año completo (como en tus scripts previos)
# Luego para construir un indice de periodo: idx = year - 1963 -> 1..10
year_map <- c(
  "4" = 1964, "5" = 1965, "6" = 1966, "7" = 1967, "8" = 1968,
  "9" = 1969, "0" = 1970, "1" = 1971, "2" = 1972, "3" = 1973
)

# ----------------------------
# 1) Diccionarios de estados
# ----------------------------

fpstatus_labels_raw <- attr(kfamily, "label.table")$fpstatus

# Normalizamos un typo común: "0ther" -> "Other"
normalize_label <- function(x) sub("^0ther$", "Other", x)
all_labels_raw <- names(fpstatus_labels_raw)
all_labels <- normalize_label(all_labels_raw)

# Vector de codigos segun las etiquetas originales
all_codes <- as.numeric(fpstatus_labels_raw)

# Construimos ambos diccionarios
# - label_to_code: nombres = etiquetas, valores = codigos
label_to_code <- stats::setNames(all_codes, all_labels)
# - code_to_label: nombres = codigos (como texto), valores = etiquetas
code_to_label <- stats::setNames(all_labels, as.character(all_codes))

# Conjunto de estados ordenados alfabeticamente (para fija el orden de la base)
states <- sort(all_labels)

# ----------------------------
# 2) META (IDs)
# ----------------------------

global_id   <- seq_len(n)
specific_id <- paste(kfamily$comm, kfamily$id, sep = "_")

meta <- data.frame(
  global_id   = global_id,
  specific_id = specific_id,
  comm        = kfamily$comm,
  id          = kfamily$id,
  stringsAsFactors = FALSE
)

# -------------------------------------
# 3) STATUS por periodo y fuente (source)
# -------------------------------------
# status_code: matriz n x P (enteros/NA) con el estado observado/imputado
# source     : matriz n x P (0=NA, 1=fpt, 2=cfp_imputed)

status_code <- matrix(NA_integer_, nrow = n, ncol = P,
                      dimnames = list(NULL, paste0("t", 1:P)))
source_mat  <- matrix(0L, nrow = n, ncol = P,
                      dimnames = list(NULL, paste0("t", 1:P)))

# (A) Cargamos lo OBSERVADO en fpt1..fpt12 (sin imputar)
for (p in 1:P) {
  fpt_col <- paste0("fpt", p)
  if (fpt_col %in% names(kfamily)) {
    vals <- kfamily[[fpt_col]]
    # Solo asignar donde no sea NA
    notna <- !is.na(vals)
    status_code[notna, p] <- as.integer(vals[notna])
    source_mat [notna, p] <- 1L  # observado en fpt
  }
}

# (B) Imputación desde cfp/cbyr (SOLO donde falta fpt; NO sobreescribe fpt)
#     Rellenamos desde el indice cbyr_idx (= year_map - 1963) hacia adelante
cfp_vals  <- kfamily$cfp
cbyr_vals <- kfamily$cbyr

for (i in 1:n) {
  cfp_i  <- cfp_vals[i]
  cbyr_i <- cbyr_vals[i]
  
  if (!is.na(cfp_i) && !is.na(cbyr_i)) {
    yr <- year_map[as.character(cbyr_i)]
    if (!is.na(yr)) {
      start_idx <- as.integer(yr - 1963L)  # 1..10
      if (!is.na(start_idx) && start_idx >= 1 && start_idx <= P) {
        for (p in start_idx:P) {
          if (is.na(status_code[i, p])) {
            status_code[i, p] <- as.integer(cfp_i)
            source_mat [i, p] <- 2L  # imputado desde cfp
          }
        }
      }
    }
  }
}

# -------------------------------------
# 4) ARRAY de TRANSICIONES (S x S x (P-1))
# -------------------------------------

S <- length(states)
transitions <- array(0L, dim = c(S, S, P - 1L),
                     dimnames = list(From = states, To = states, Step = paste0("t", 1:(P - 1L))))

# Lógica: SOLO parejas no-NA y from != to
for (p in 1:(P - 1L)) {
  from_codes <- status_code[, p]
  to_codes   <- status_code[, p + 1L]
  
  valid <- !is.na(from_codes) & !is.na(to_codes) & (from_codes != to_codes)
  if (!any(valid)) next
  
  from_labels <- code_to_label[as.character(from_codes[valid])]
  to_labels   <- code_to_label[as.character(to_codes[valid])]
  
  # Aseguramos que existan y mapeamos a indices de 'states'
  from_idx <- match(from_labels, states)
  to_idx   <- match(to_labels,   states)
  
  # Filtramos por seguridad
  ok <- !is.na(from_idx) & !is.na(to_idx)
  if (!any(ok)) next
  
  # Acumular conteos
  for (k in which(ok)) {
    transitions[from_idx[k], to_idx[k], p] <- transitions[from_idx[k], to_idx[k], p] + 1L
  }
}

# -------------------------------------
# 5) Empaquetar el objeto general
# -------------------------------------

TOA_derivado_general <- list(
  meta          = meta,
  states        = states,
  # 'codes' como ambos diccionarios, para claridad y bidireccionalidad:
  codes = list(
    label_to_code = label_to_code,  # ej: "Oral Pill" -> 6 (p.ej.)
    code_to_label = code_to_label   # ej: "6" -> "Oral Pill"
  ),
  periods       = 1:P,
  status_code   = status_code,
  source        = source_mat,
  transitions   = transitions
)

# -------------------------------------
# 6) Guardar
# -------------------------------------

saveRDS(TOA_derivado_general, file = "TOA_derivado_general.rds")

# -------------------------------------
# 7) Mensajes de control y TODO
# -------------------------------------

message("TOA_derivado_general construido y guardado en 'TOA_derivado_general.rds'.")
message("Dim status_code: ", paste(dim(status_code), collapse = " x "),
        " ; Dim transitions: ", paste(dim(transitions), collapse = " x "))

# TODO (futuro):
# - Versión alternativa de 'transitions' que incluya autotransiciones (from == to)
#   y/o estrategias diferentes frente a NAs (carry-forward vs. break).

# -------------------------------------
# 8) Checks: Build TOA_standard_from_general (BYRT/CBYR-anchored)
# -------------------------------------

# Objetivo: reconstruir un TOA en escala 1..10 replicando la lógica temporal del derivado/original
#  - Usar SIEMPRE byrtX para anclar el índice 1..10 cuando fptX indique método moderno.
#  - Si no hay byrtX válido, usar cfp/cbyr para anclar el índice 1..10.
#  - Si nada aplica, asignar 11 (no adoptante) y origen = "none".

# 8.1) Matriz byrt_index (n x P) con índices 1..10 a partir de byrt1..byrt12
n <- nrow(TOA_derivado_general$meta)
P <- length(TOA_derivado_general$periods)

byrt_index <- matrix(NA_integer_, nrow = n, ncol = P,
                     dimnames = list(NULL, paste0("t", 1:P)))
for (p in 1:P) {
  byrt_col <- paste0("byrt", p)
  if (byrt_col %in% names(kfamily)) {
    vals <- kfamily[[byrt_col]]
    yrs  <- year_map[as.character(vals)]                    # 1964..1973
    idx  <- ifelse(!is.na(yrs), as.integer(yrs) - 1963L, NA_integer_) # 1..10
    idx[!(idx >= 1L & idx <= 10L)] <- NA_integer_           # limitar a 1..10
    byrt_index[, p] <- idx
  }
}

# 8.2) Definir métodos modernos y mapear a códigos
modern_methods_names <- c("Loop", "Condom", "Oral Pill", "Vasectomy", "TL", "Injection", "Rhythm", "Withdrawal")
modern_codes <- unname(TOA_derivado_general$codes$label_to_code[modern_methods_names])
modern_codes <- as.integer(modern_codes[!is.na(modern_codes)])

# 8.3) Construir TOA_standard_from_general (1..10 o 11) + origen
TOA_standard <- rep(NA_integer_, n)
origen_vec   <- rep(NA_character_, n)

for (i in seq_len(n)) {
  # Candidatos desde fptX + byrtX: fuente fpt (1) y estado moderno
  fpt_is_modern <- (TOA_derivado_general$source[i, ] == 1L) &
                   (!is.na(TOA_derivado_general$status_code[i, ])) &
                   (TOA_derivado_general$status_code[i, ] %in% modern_codes)
  idx_candidates <- byrt_index[i, fpt_is_modern]
  idx_candidates <- idx_candidates[!is.na(idx_candidates)]  # 1..10

  if (length(idx_candidates) > 0) {
    t0 <- min(idx_candidates)
    TOA_standard[i] <- as.integer(t0)
    origen_vec[i]   <- "fptX"
  } else {
    # Fallback: cfp/cbyr
    cfp_i  <- kfamily$cfp[i]
    cbyr_i <- kfamily$cbyr[i]
    if (!is.na(cfp_i) && cfp_i %in% modern_codes && !is.na(cbyr_i)) {
      yr  <- year_map[as.character(cbyr_i)]
      idx <- ifelse(!is.na(yr), as.integer(yr) - 1963L, NA_integer_)
      if (!is.na(idx) && idx >= 1L && idx <= 10L) {
        TOA_standard[i] <- as.integer(idx)
        origen_vec[i]   <- "cfp"
      } else {
        TOA_standard[i] <- 11L
        origen_vec[i]   <- "none"
      }
    } else {
      TOA_standard[i] <- 11L
      origen_vec[i]   <- "none"
    }
  }
}

# --- Alineación opcional de NA con TOA_derivado_full ---

TOA_derivado_full <- readRDS("TOA_derivado_full.rds")
# Construimos llaves para hacer el cruce sin merge pesado
key_std <- paste(TOA_derivado_general$meta$global_id, TOA_derivado_general$meta$specific_id)
key_der <- paste(TOA_derivado_full$global_id, TOA_derivado_full$specific_id)
na_mask <- key_std %in% key_der[is.na(TOA_derivado_full$TOA_derivado)]
n_aligned <- sum(na_mask, na.rm = TRUE)
if (n_aligned > 0) {
  TOA_standard[na_mask] <- NA_integer_
  message(sprintf("[std] Alineación NA aplicada: %d casos establecidos en NA para TOA_standard (según TOA_derivado NA).", n_aligned))
} else {
  message("[std] Alineación NA: 0 casos a ajustar.")
}

TOA_standard_from_general <- data.frame(
  global_id    = TOA_derivado_general$meta$global_id,
  specific_id  = TOA_derivado_general$meta$specific_id,
  TOA_standard = TOA_standard,
  origen       = origen_vec,
  stringsAsFactors = FALSE
)

saveRDS(TOA_standard_from_general, file = "TOA_standard_from_general.rds")
write.csv(TOA_standard_from_general, file = "TOA_standard_from_general.csv", row.names = FALSE)

message("TOA_standard_from_general creado: ", nrow(TOA_standard_from_general), " filas.")

# 8.4) Comparaciones mínimas
# (i) vs TOA_original (kfamily$toa)
TOA_original_netdiffuseR <- data.frame(
  global_id   = TOA_derivado_general$meta$global_id,
  specific_id = TOA_derivado_general$meta$specific_id,
  TOA_original = as.integer(kfamily$toa),
  stringsAsFactors = FALSE
)

comp_std_orig <- merge(
  TOA_standard_from_general, TOA_original_netdiffuseR,
  by = c("global_id","specific_id"), all = TRUE
)
comp_std_orig$agree_toa <- with(comp_std_orig, TOA_standard == TOA_original)

n_both_so <- sum(!is.na(comp_std_orig$TOA_standard) & !is.na(comp_std_orig$TOA_original))
n_agree_so <- sum(comp_std_orig$agree_toa, na.rm = TRUE)
message(sprintf("[std] vs original — presentes ambos: %d; coincidencias: %d (%.2f%%)",
                n_both_so, n_agree_so, 100*n_agree_so/max(1, n_both_so)))
message("[std] difs (TOA_standard - TOA_original):")
print(table(comp_std_orig$TOA_standard - comp_std_orig$TOA_original, useNA = "ifany"))

# (ii) vs TOA_derivado_full (si existe)

comp_std_full <- merge(
  TOA_standard_from_general, TOA_derivado_full,
  by = c("global_id","specific_id"), all = TRUE
)
comp_std_full$agree_toa <- with(comp_std_full, TOA_standard == TOA_derivado)
n_both_sf <- sum(!is.na(comp_std_full$TOA_standard) & !is.na(comp_std_full$TOA_derivado))
n_agree_sf <- sum(comp_std_full$agree_toa, na.rm = TRUE)
message(sprintf("[std] vs derivado_full — presentes ambos: %d; coincidencias: %d (%.2f%%)",
                n_both_sf, n_agree_sf, 100*n_agree_sf/max(1, n_both_sf)))
message("[std] difs (TOA_standard - TOA_derivado):")
print(table(comp_std_full$TOA_standard - comp_std_full$TOA_derivado, useNA = "ifany"))
