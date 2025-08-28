# run after 5-toa_general, line ~170 

# -------------------------------------
# 8) Checks: Build TOA_standard_from_general and compare
# -------------------------------------

# 8.1) Reconstruct standard TOA (first period adopting a "modern" method)
modern_methods_names <- c("Loop", "Condom", "Oral Pill", "Vasectomy", "TL", "Injection", "Rhythm", "Withdrawal")
modern_codes <- unname(TOA_derivado_general$codes$label_to_code[modern_methods_names])
modern_codes <- as.integer(modern_codes[!is.na(modern_codes)])

n <- nrow(TOA_derivado_general$meta)
P <- length(TOA_derivado_general$periods)

TOA_standard <- rep(NA_integer_, n)
origen_vec    <- rep(NA_character_, n)

for (i in seq_len(n)) {
  row_codes   <- TOA_derivado_general$status_code[i, ]
  row_sources <- TOA_derivado_general$source[i, ]  # 0=NA,1=fpt,2=cfp
  adopts_idx  <- which(!is.na(row_codes) & row_codes %in% modern_codes)
  if (length(adopts_idx) > 0) {
    t0 <- min(adopts_idx)
    TOA_standard[i] <- as.integer(t0)  # period index 1..P
    origen_vec[i]   <- if (row_sources[t0] == 1L) "fptX" else if (row_sources[t0] == 2L) "cfp" else "none"
  } else {
    TOA_standard[i] <- 11L           # no adopters marked as 11, para compatibilidad
    origen_vec[i]   <- "none"
  }
}

TOA_standard_from_general <- data.frame(
  global_id   = TOA_derivado_general$meta$global_id,
  specific_id = TOA_derivado_general$meta$specific_id,
  TOA_standard = TOA_standard,
  origen = origen_vec,
  stringsAsFactors = FALSE
)

# saveRDS(TOA_standard_from_general, file = "TOA_standard_from_general.rds")
# write.csv(TOA_standard_from_general, file = "TOA_standard_from_general.csv", row.names = FALSE)

message("TOA_standard_from_general creado: ", nrow(TOA_standard_from_general), " filas.")

# 8.2) Comparar con TOA_derivado_full.rds

TOA_derivado_full <- readRDS("TOA_derivado_full.rds")
# Esperado: columnas (global_id, specific_id, TOA_derivado, origen)
colnames(TOA_derivado_full) <- sub("^TOA_derivado$", "TOA_derivado", colnames(TOA_derivado_full))

comp <- merge(
  TOA_standard_from_general, TOA_derivado_full,
  by = c("global_id", "specific_id"), all = TRUE,
  suffixes = c("_std", "_full")
)

comp$agree_toa    <- with(comp, TOA_standard == TOA_derivado)
comp$agree_origen <- with(comp, origen_std == origen_full)

# Resumen
n_all <- nrow(comp)
n_both_present <- sum(!is.na(comp$TOA_standard) & !is.na(comp$TOA_derivado))
n_agree_toa    <- sum(comp$agree_toa, na.rm = TRUE)
n_agree_origen <- sum(comp$agree_origen, na.rm = TRUE)

message(sprintf("Comparación con TOA_derivado_full.rds (n=%d; presentes en ambos=%d)", n_all, n_both_present))
message(sprintf(" - Coincidencias TOA: %d (%.2f%%)", n_agree_toa, 100 * n_agree_toa / max(1, n_both_present)))
message(sprintf(" - Coincidencias origen: %d (%.2f%%)", n_agree_origen, 100 * n_agree_origen / max(1, n_both_present)))

# Tablas útiles
message("Tabla diferencias TOA (TOA_standard - TOA_derivado):")
dif_toa <- comp$TOA_standard - comp$TOA_derivado
print(table(dif_toa, useNA = "ifany"))

message("Tabla de origen (std vs full):")
print(table(comp$origen_std, comp$origen_full, useNA = "ifany"))

# Guardar comparación completa
# saveRDS(comp, file = "TOA_compare_standard_vs_full.rds")
# write.csv(comp, file = "TOA_compare_standard_vs_full.csv", row.names = FALSE)

# -------------------------------------
# 8.3) Comparar con TOA original de netdiffuseR (kfamily$toa)
# -------------------------------------

# Construir TOA_original_netdiffuseR
TOA_original_netdiffuseR <- data.frame(
  global_id   = TOA_derivado_general$meta$global_id,
  specific_id = TOA_derivado_general$meta$specific_id,
  TOA_original = as.integer(kfamily$toa),
  stringsAsFactors = FALSE
)

message("TOA_original_netdiffuseR creado: ", nrow(TOA_original_netdiffuseR), " filas.")

# Comparación: TOA_standard_from_general vs TOA_original_netdiffuseR
comp_orig <- merge(
  TOA_standard_from_general, TOA_original_netdiffuseR,
  by = c("global_id", "specific_id"), all = TRUE
)

comp_orig$agree_toa <- with(comp_orig, TOA_standard == TOA_original)

n_all_o <- nrow(comp_orig)
n_both_present_o <- sum(!is.na(comp_orig$TOA_standard) & !is.na(comp_orig$TOA_original))
n_agree_toa_o    <- sum(comp_orig$agree_toa, na.rm = TRUE)

message(sprintf("Comparación con TOA_original_netdiffuseR (n=%d; presentes en ambos=%d)", n_all_o, n_both_present_o))
message(sprintf(" - Coincidencias TOA: %d (%.2f%%)", n_agree_toa_o, 100 * n_agree_toa_o / max(1, n_both_present_o)))

message("Tabla diferencias TOA (TOA_standard - TOA_original):")
dif_toa_o <- comp_orig$TOA_standard - comp_orig$TOA_original
print(table(dif_toa_o, useNA = "ifany"))

# Guardados opcionales
# saveRDS(TOA_original_netdiffuseR, file = "TOA_original_netdiffuseR.rds")
# write.csv(TOA_original_netdiffuseR, file = "TOA_original_netdiffuseR.csv", row.names = FALSE)
# saveRDS(comp_orig, file = "TOA_compare_standard_vs_original.rds")
# write.csv(comp_orig, file = "TOA_compare_standard_vs_original.csv", row.names = FALSE)

# -------------------------------------
# 8.4) Comparar TOA_original_netdiffuseR vs TOA_derivado_full
# -------------------------------------

# Reutiliza TOA_derivado_full leído en 8.2 y TOA_original_netdiffuseR construido en 8.3
comp_orig_full <- merge(
  TOA_original_netdiffuseR, TOA_derivado_full,
  by = c("global_id", "specific_id"), all = TRUE,
  suffixes = c("_orig", "_full")
)

comp_orig_full$agree_toa <- with(comp_orig_full, TOA_original == TOA_derivado)

n_all_of <- nrow(comp_orig_full)
n_both_present_of <- sum(!is.na(comp_orig_full$TOA_original) & !is.na(comp_orig_full$TOA_derivado))
n_agree_toa_of    <- sum(comp_orig_full$agree_toa, na.rm = TRUE)

message(sprintf("Comparación TOA_original_netdiffuseR vs TOA_derivado_full (n=%d; presentes en ambos=%d)", n_all_of, n_both_present_of))
message(sprintf(" - Coincidencias TOA: %d (%.2f%%)", n_agree_toa_of, 100 * n_agree_toa_of / max(1, n_both_present_of)))

message("Tabla diferencias TOA (TOA_original - TOA_derivado):")
dif_toa_of <- comp_orig_full$TOA_original - comp_orig_full$TOA_derivado
print(table(dif_toa_of, useNA = "ifany"))

# Guardados opcionales
# saveRDS(comp_orig_full, file = "TOA_compare_original_vs_full.rds")
# write.csv(comp_orig_full, file = "TOA_compare_original_vs_full.csv", row.names = FALSE)

# -------------------------------------
# 9) Checks: Build TOA_standard_from_general v2 (BYRT/CBYR-anchored)
# -------------------------------------

# Objetivo: replicar la lógica temporal de TOA_derivado_full/original
#  - Usar SIEMPRE byrtX para anclar el índice 1..10 cuando fptX indique método moderno.
#  - Si no hay byrtX válido, usar cfp/cbyr para anclar el índice 1..10.
#  - Si no existe ninguno, asignar 11 (no adoptante), origen = "none".
#  - Nota: El índice resultante es 1..10 (años), NO 1..12 (periodos del panel).

# 9.1) Construir matriz byrt_index (n x P) con índices 1..10 a partir de byrt1..byrt12
byrt_index <- matrix(NA_integer_, nrow = n, ncol = P,
                     dimnames = list(NULL, paste0("t", 1:P)))
for (p in 1:P) {
  byrt_col <- paste0("byrt", p)
  if (byrt_col %in% names(kfamily)) {
    vals <- kfamily[[byrt_col]]
    # mapear ultimo dígito a año y luego a índice 1..10
    yrs <- year_map[as.character(vals)]
    idx <- ifelse(!is.na(yrs), as.integer(yrs) - 1963L, NA_integer_)
    # limitar a 1..10
    idx[!(idx >= 1L & idx <= 10L)] <- NA_integer_
    byrt_index[, p] <- idx
  }
}

# 9.2) Definir modernos y obtener códigos (reutilizamos label_to_code)
modern_methods_names <- c("Loop", "Condom", "Oral Pill", "Vasectomy", "TL", "Injection", "Rhythm", "Withdrawal")
modern_codes <- unname(TOA_derivado_general$codes$label_to_code[modern_methods_names])
modern_codes <- as.integer(modern_codes[!is.na(modern_codes)])

# 9.3) Calcular TOA_standard_v2 anclado en byrt/cbyr (resultado en 1..10)
TOA_standard_v2 <- rep(NA_integer_, n)
origen_v2       <- rep(NA_character_, n)

for (i in seq_len(n)) {
  # (a) Candidatos desde fptX + byrtX (solo donde la fuente es fpt y estado moderno)
  fpt_is_modern <- (TOA_derivado_general$source[i, ] == 1L) &
    (!is.na(TOA_derivado_general$status_code[i, ])) &
    (TOA_derivado_general$status_code[i, ] %in% modern_codes)
  idx_candidates <- byrt_index[i, fpt_is_modern]
  idx_candidates <- idx_candidates[!is.na(idx_candidates)]  # en 1..10
  
  if (length(idx_candidates) > 0) {
    t0 <- min(idx_candidates)
    TOA_standard_v2[i] <- as.integer(t0)
    origen_v2[i]       <- "fptX"
  } else {
    # (b) Fallback: cfp/cbyr
    cfp_i  <- kfamily$cfp[i]
    cbyr_i <- kfamily$cbyr[i]
    if (!is.na(cfp_i) && cfp_i %in% modern_codes && !is.na(cbyr_i)) {
      yr  <- year_map[as.character(cbyr_i)]
      idx <- ifelse(!is.na(yr), as.integer(yr) - 1963L, NA_integer_)
      if (!is.na(idx) && idx >= 1L && idx <= 10L) {
        TOA_standard_v2[i] <- as.integer(idx)
        origen_v2[i]       <- "cfp"
      } else {
        TOA_standard_v2[i] <- 11L
        origen_v2[i]       <- "none"
      }
    } else {
      # (c) No hay evidencia para anclar adopción
      TOA_standard_v2[i] <- 11L
      origen_v2[i]       <- "none"
    }
  }
}

TOA_standard_from_general_v2 <- data.frame(
  global_id    = TOA_derivado_general$meta$global_id,
  specific_id  = TOA_derivado_general$meta$specific_id,
  TOA_standard_v2 = TOA_standard_v2,
  origen_v2       = origen_v2,
  stringsAsFactors = FALSE
)

# Guardados opcionales
# saveRDS(TOA_standard_from_general_v2, file = "TOA_standard_from_general_v2.rds")
# write.csv(TOA_standard_from_general_v2, file = "TOA_standard_from_general_v2.csv", row.names = FALSE)

message("TOA_standard_from_general_v2 creado: ", nrow(TOA_standard_from_general_v2), " filas.")

# 9.4) Comparaciones
# (i) v2 vs TOA_original_netdiffuseR
comp_v2_orig <- merge(
  TOA_standard_from_general_v2, TOA_original_netdiffuseR,
  by = c("global_id", "specific_id"), all = TRUE
)
comp_v2_orig$agree_toa <- with(comp_v2_orig, TOA_standard_v2 == TOA_original)

n_all_v2o  <- nrow(comp_v2_orig)
n_both_v2o <- sum(!is.na(comp_v2_orig$TOA_standard_v2) & !is.na(comp_v2_orig$TOA_original))
n_agree_v2o <- sum(comp_v2_orig$agree_toa, na.rm = TRUE)

message(sprintf("[v2] Comparación con TOA_original (n=%d; presentes en ambos=%d)", n_all_v2o, n_both_v2o))
message(sprintf("[v2]  - Coincidencias TOA: %d (%.2f%%)", n_agree_v2o, 100 * n_agree_v2o / max(1, n_both_v2o)))

message("[v2] Tabla difs TOA (TOA_standard_v2 - TOA_original):")
dif_v2_o <- comp_v2_orig$TOA_standard_v2 - comp_v2_orig$TOA_original
print(table(dif_v2_o, useNA = "ifany"))

# (ii) v2 vs TOA_derivado_full
comp_v2_full <- merge(
  TOA_standard_from_general_v2, TOA_derivado_full,
  by = c("global_id", "specific_id"), all = TRUE,
  suffixes = c("_v2", "_full")
)
comp_v2_full$agree_toa <- with(comp_v2_full, TOA_standard_v2 == TOA_derivado)

n_all_v2f  <- nrow(comp_v2_full)
n_both_v2f <- sum(!is.na(comp_v2_full$TOA_standard_v2) & !is.na(comp_v2_full$TOA_derivado))
n_agree_v2f <- sum(comp_v2_full$agree_toa, na.rm = TRUE)

message(sprintf("[v2] Comparación con TOA_derivado_full (n=%d; presentes en ambos=%d)", n_all_v2f, n_both_v2f))
message(sprintf("[v2]  - Coincidencias TOA: %d (%.2f%%)", n_agree_v2f, 100 * n_agree_v2f / max(1, n_both_v2f)))

message("[v2] Tabla difs TOA (TOA_standard_v2 - TOA_derivado):")
dif_v2_f <- comp_v2_full$TOA_standard_v2 - comp_v2_full$TOA_derivado
print(table(dif_v2_f, useNA = "ifany"))

# ¿De donde los NA? ==========

# Diagnóstico mínimo:
with(comp_v2_full, table(is.na(TOA_standard_v2), is.na(TOA_derivado)))

sum(is.na(comp_v2_full$TOA_standard_v2))  # debería ser 0
sum(is.na(comp_v2_full$TOA_derivado))     # debería ser 76 en tu salida

# Ver ejemplos concretos (ajusta 1:10 si quieres más):
na_rows <- which(is.na(comp_v2_full$TOA_derivado))
comp_v2_full[na_rows[1:min(10, length(na_rows))],
             c("global_id","specific_id","TOA_standard_v2","TOA_derivado","origen_v2")]

# Imponer NA en los ID con valor NA en TOA_derivado_full y 11 en el TOA_standard_v2 ====

# 1) IDs con NA en TOA_derivado (desde comp_v2_full ya construido)
na_ids <- comp_v2_full[is.na(comp_v2_full$TOA_derivado), c("global_id","specific_id")]

# 2) Marcar NA en TOA_standard_v2 para esos IDs
key_v2  <- paste(TOA_standard_from_general_v2$global_id,
                 TOA_standard_from_general_v2$specific_id)
key_na  <- paste(na_ids$global_id, na_ids$specific_id)

TOA_standard_from_general_v2$TOA_standard_v2[ key_v2 %in% key_na ] <- NA_integer_

# 3) Recomparar con TOA_original_netdiffuseR
comp_v2_orig_aligned <- merge(
  TOA_standard_from_general_v2, TOA_original_netdiffuseR,
  by = c("global_id","specific_id"), all = TRUE
)

comp_v2_orig_aligned$agree_toa <- with(comp_v2_orig_aligned,
                                       TOA_standard_v2 == TOA_original)

n_all   <- nrow(comp_v2_orig_aligned)
n_both  <- sum(!is.na(comp_v2_orig_aligned$TOA_standard_v2) &
                 !is.na(comp_v2_orig_aligned$TOA_original))
n_agree <- sum(comp_v2_orig_aligned$agree_toa, na.rm = TRUE)

message(sprintf("[v2 aligned] Comparación con TOA_original (n=%d; presentes en ambos=%d)",
                n_all, n_both))
message(sprintf("[v2 aligned]  - Coincidencias TOA: %d (%.2f%%)",
                n_agree, 100 * n_agree / max(1, n_both)))

message("[v2 aligned] Tabla difs TOA (TOA_standard_v2 - TOA_original):")
print(table(comp_v2_orig_aligned$TOA_standard_v2 - comp_v2_orig_aligned$TOA_original,
            useNA = "ifany"))
