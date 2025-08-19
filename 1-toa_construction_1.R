library(netdiffuseR)
data(kfamily)       

# --- 0. Preparación y Definición de Métodos Modernos ---
toa_original <- kfamily$toa

sum(is.na(toa_original))  # NO hay NA's
min(toa_original, na.rm = TRUE)

attr(kfamily, "var.labels")
attr(kfamily, "label.table")

attr(attr(kfamily, "label.table")$fpstatus, "names")

sort(unique(kfamily$fpt1))

table(kfamily$fpt1, useNA = "ifany")
table(kfamily$fpt2, useNA = "ifany")
table(kfamily$fpt3, useNA = "ifany")
table(kfamily$fpt4, useNA = "ifany")
table(kfamily$fpt5, useNA = "ifany")
table(kfamily$fpt6, useNA = "ifany")
table(kfamily$fpt7, useNA = "ifany")
table(kfamily$fpt8, useNA = "ifany")
table(kfamily$fpt9, useNA = "ifany")
table(kfamily$fpt10, useNA = "ifany")
table(kfamily$fpt11, useNA = "ifany")
table(kfamily$fpt12, useNA = "ifany")

# Obtener las etiquetas de los valores para fpstatus
fpstatus_labels <- attr(kfamily, "label.table")$fpstatus

# Consideramos métodos contraceptivos "modernos":
# Loop (03), Condom (04), Oral Pill (05), Vasectomy (06), Tubal Ligation (15), Injection (18)
# Estos TAMBIÉN (extraño):
# Rhythm (14) y Withdrawal (16).
# Además, excluimos estos:
# Excluimos: Pregnant (01), Normal Birth (02), Menopause (07), Want more/No more (08,09), Infertile (10),
# Stillbirth (11), Newlywed (12), Abortion (13), Pessary (17), Jelly (19), Foam (20), Other (21)

# Extraer los códigos numéricos correspondientes a los nombres de los métodos modernos
modern_methods_names <- c("Loop", "Condom", "Oral Pill", "Vasectomy", "Tubal Ligation", "Injection", "Rhythm", "Withdrawal")
modern_methods_codes <- as.numeric(fpstatus_labels[names(fpstatus_labels) %in% modern_methods_names])

# --- Derivamos TOA a partir de variables fptX y byrtX ---

num_periods <- 12 # Hay hasta fpt12 y byrt12
n_obs <- nrow(kfamily)
toa_from_fpt <- rep(NA, n_obs)

for (i in 1:n_obs) {
  for (period in 1:num_periods) {
    fpt_var_name <- paste0("fpt", period)
    byrt_var_name <- paste0("byrt", period) # Start of period (Year)
    
    # Verificar que las columnas existen
    if (!(fpt_var_name %in% names(kfamily)) || !(byrt_var_name %in% names(kfamily))) {
      message(paste("Advertencia: No se encontraron las columnas", fpt_var_name, "o", byrt_var_name, "para el periodo", period))
      next # Saltar a la siguiente iteración del periodo
    }
    
    current_fpt_status <- kfamily[i, fpt_var_name]
    current_byrt <- kfamily[i, byrt_var_name]
    
    # Si el estado es uno de los métodos modernos y aún no hemos asignado un toa_from_fpt para esta persona
    if (!is.na(current_fpt_status) && current_fpt_status %in% modern_methods_codes && is.na(toa_from_fpt[i])) {
      year_map <- c(
        "4" = 1964,
        "5" = 1965,
        "6" = 1966,
        "7" = 1967,
        "8" = 1968,
        "9" = 1969,
        "0" = 1970,
        "1" = 1971,
        "2" = 1972,
        "3" = 1973
      )
      
      if (!is.na(current_byrt) && as.character(current_byrt) %in% names(year_map)) {
        toa_from_fpt[i] <- year_map[as.character(current_byrt)]
        # break # Salimos del bucle de periodos una vez encontrado el primero
      } else if (!is.na(current_byrt)) {
        # message(paste("Advertencia: Valor de byrt no mapeado:", current_byrt, "para individuo", kfamily$id[i], "periodo", period))
      }
    }
  }
}

min(toa_from_fpt, na.rm = TRUE)
toa_from_fpt <- toa_from_fpt - 1963

# Comparación con toa_original

valid_indices_fpt <- !is.na(toa_from_fpt) # quitamos los NA de toa_from_fpt

cor(toa_original[valid_indices_fpt], toa_from_fpt[valid_indices_fpt]) # Alta correlación

# Diferencias directas
diff_fpt <- toa_original[valid_indices_fpt] - toa_from_fpt[valid_indices_fpt]
print(summary(diff_fpt))
message(paste("Número de coincidencias exactas:", sum(diff_fpt == 0, na.rm = TRUE), "de", sum(valid_indices_fpt)))

# --- Identificar casos NO capturados ---

indices_no_capturados <- which(is.na(toa_from_fpt))
num_no_capturados <- length(indices_no_capturados)
message(paste("\nNúmero de TOAs originales que NO fueron capturados por la derivación fptX/byrtX:", num_no_capturados))

# Obtenemos los toa_original para estos casos
toa_original_no_capturados <- toa_original[indices_no_capturados]

print(table(toa_original_no_capturados, useNA = "ifany"))
print(summary(toa_original_no_capturados))

# NINGUNO de los TOAs originales no capturados por fptX/byrtX es igual a 0.

# Datos de los primeros individuos no capturados
print(head(data.frame(id = kfamily$id[indices_no_capturados],
                     toa_original = toa_original_no_capturados,
                     fpt1 = kfamily$fpt1[indices_no_capturados],
                     fpt2 = kfamily$fpt2[indices_no_capturados],
                     fpt3 = kfamily$fpt3[indices_no_capturados],
                     fpt4 = kfamily$fpt4[indices_no_capturados],
                     fpt5 = kfamily$fpt5[indices_no_capturados],
                     fpt6 = kfamily$fpt6[indices_no_capturados],
                     fpt7 = kfamily$fpt7[indices_no_capturados],
                     fpt8 = kfamily$fpt8[indices_no_capturados],
                     fpt9 = kfamily$fpt9[indices_no_capturados],
                     fpt10 = kfamily$fpt10[indices_no_capturados],
                     fpt11 = kfamily$fpt11[indices_no_capturados],
                     fpt12 = kfamily$fpt12[indices_no_capturados],
                     fp1 = kfamily$fp1[indices_no_capturados],  # Experience with an F.P. practice
                     fp6 = kfamily$fp6[indices_no_capturados]  # time between decision and adoption
                     )))

# Some strange cases, like id=2 : toa_original = 11, but fptX = NA always.

# Recordatorio de cuántos TOA tiene el original y cuántos derivamos
message(paste("\nNúmero total de observaciones en kfamily:", nrow(kfamily)))
message(paste("Número de TOAs no NA en toa_original:", sum(!is.na(toa_original)))) # Debería ser 1047
message(paste("Número de TOAs derivados y ajustados de fptX/byrtX:", sum(!is.na(toa_from_fpt)))) # Debería ser 422


# ------------------------- Other atempts --------------------------------------


# --- 2. Derivación de TOA a partir de cfp y cbyr/cbmnth ---

toa_from_cfp <- rep(NA, n_obs)

# El codebook indica que cbyr son años directamente (misma codificación que byrtX)
year_map_cfp <- c("0" = 1970, "1" = 1971, "2" = 1972, "3" = 1973,
                  "4" = 1964, "5" = 1965, "6" = 1966, "7" = 1967,
                  "8" = 1968, "9" = 1969) # Misma que para byrtX

for (i in 1:n_obs) {
  current_cfp_status <- kfamily$cfp[i]
  current_cbyr <- kfamily$cbyr[i]
  # cbmnth podría usarse para mayor precisión si fuera necesario, pero toa es anual.
  
  if (!is.na(current_cfp_status) && current_cfp_status %in% modern_methods_codes) {
    if (!is.na(current_cbyr) && as.character(current_cbyr) %in% names(year_map_cfp)) {
      toa_from_cfp[i] <- year_map_cfp[as.character(current_cbyr)]
    } else if (!is.na(current_cbyr)) {
      # message(paste("Advertencia: Valor de cbyr no mapeado:", current_cbyr, "para individuo", kfamily$id[i]))
    }
  }
}

min(toa_from_cfp, na.rm = TRUE)
toa_from_cfp <- toa_from_cfp - 1963

# Comparación y Correlación
valid_indices_cfp <- !is.na(toa_original) & !is.na(toa_from_cfp)
if (sum(valid_indices_cfp) > 1) {
  correlation_cfp <- cor(toa_original[valid_indices_cfp], toa_from_cfp[valid_indices_cfp])
  message(paste("Correlación entre toa_original y toa_from_cfp (derivado de cfp, cbyr):", round(correlation_cfp, 4)))
  
  diff_cfp <- toa_original[valid_indices_cfp] - toa_from_cfp[valid_indices_cfp]
  message("Resumen de diferencias (toa_original - toa_from_cfp):")
  print(summary(diff_cfp))
  message(paste("Número de coincidencias exactas:", sum(diff_cfp == 0, na.rm = TRUE), "de", sum(valid_indices_cfp)))
} else {
  message("No hay suficientes datos válidos para calcular la correlación para toa_from_cfp.")
}
message(paste("Número de TOAs derivados de cfp/cbyr:", sum(!is.na(toa_from_cfp))))

# ------------------------- END Other atempts ----------------------------------


# --- 3. Análisis cualitativo de fp1 (Experience with an FP practice) ---
# fp1: 0=NA, 1=Never, 2=Ever
# Aquellos con toa != NA deberían tener fp1 == 2 (Ever)
# Aquellos con toa == NA podrían tener fp1 == 1 (Never) o fp1 == 0 (NA en fp1)
fp1_vs_toa_table <- table(kfamily$fp1, is.na(kfamily$toa), dnn = c("fp1 (0=NA,1=Never,2=Ever)", "is.na(toa_original)"), useNA="ifany")
message("Tabla de contingencia fp1 vs. is.na(toa_original):")
print(fp1_vs_toa_table)

# Casos inconsistentes: fp1 dice "Never" pero tienen un TOA, o fp1 dice "Ever" y no tienen TOA
inconsistent_fp1_never_has_toa <- sum(kfamily$fp1 == 1 & !is.na(kfamily$toa), na.rm = TRUE)
inconsistent_fp1_ever_no_toa <- sum(kfamily$fp1 == 2 & is.na(kfamily$toa), na.rm = TRUE)
message(paste("Número de casos donde fp1=Never pero tienen TOA:", inconsistent_fp1_never_has_toa))
message(paste("Número de casos donde fp1=Ever pero NO tienen TOA:", inconsistent_fp1_ever_no_toa))
# fp1 por sí solo no da el año, pero es un buen indicador de si hubo adopción.


# --- 4. Análisis cualitativo de fp6 (time between decision and adoption) ---
# fp6: 0=NA, 1=Immediately, 2=W/n 1 month, 3=2-6 Months, 4=7-12 months, 5=More than 12 months
# Si hay un valor en fp6 (distinto de NA/0), implica que hubo una adopción.
# No da el año directamente, pero indica que el evento ocurrió.
fp6_has_value <- !is.na(kfamily$fp6) & kfamily$fp6 != 0
fp6_vs_toa_table <- table(fp6_has_value, is.na(kfamily$toa), dnn = c("fp6_tiene_valor", "is.na(toa_original)"), useNA="ifany")
message("Tabla de contingencia fp6_tiene_valor vs. is.na(toa_original):")
print(fp6_vs_toa_table)

inconsistent_fp6_value_no_toa <- sum(fp6_has_value & is.na(kfamily$toa), na.rm = TRUE)
message(paste("Número de casos donde fp6 tiene valor pero NO tienen TOA:", inconsistent_fp6_value_no_toa))
# fp6 necesitaría una fecha de "decisión" para poder derivar un TOA.


# Podríamos también querer ver cuántos NA hay en toa_original
message(paste("\nNúmero total de observaciones:", n_obs))
message(paste("Número de TOAs no NA en toa_original:", sum(!is.na(toa_original))))

# --- ADO (variable NO original) -------

toa_derived_from_ado <- kfamily$ado # Asumiendo que ado=1 es 1963, y toa=1 es 1964

min(toa_derived_from_ado, na.rm = TRUE) # NO hay NA's
sum(!is.na(toa_derived_from_ado))

valid_indices_ado <- !is.na(toa_original) & !is.na(toa_derived_from_ado)
if(sum(valid_indices_ado) > 1) {
  correlation_ado <- cor(toa_original[valid_indices_ado], toa_derived_from_ado[valid_indices_ado])
  message(paste("Correlación entre toa_original y toa_derived_from_ado (kfamily$ado):", round(correlation_ado, 4)))
  diff_ado <- toa_original[valid_indices_ado] - toa_derived_from_ado[valid_indices_ado]
  message("Resumen de diferencias (toa_original - (kfamily$ado)):")
  print(summary(diff_ado))
  message(paste("Número de coincidencias exactas:", sum(diff_ado == 0, na.rm = TRUE), "de", sum(valid_indices_ado)))
} else {
  message("No hay suficientes datos para comparar toa_original con kfamily$ado.")
}
