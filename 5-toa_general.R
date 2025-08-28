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