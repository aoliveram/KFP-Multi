# --- RESUMEN FINAL Y CONSTRUCCIÓN DE TOA_DERIVADO ---

library(netdiffuseR)
data(kfamily)       

# TOA original
toa_original <- kfamily$toa
toa_without_11 <- toa_original[toa_original != 11]

# TOA derivado de cfp/cbyr
num_periods <- 12
n_obs <- nrow(kfamily)
toa_from_fpt <- rep(NA, n_obs)

for (i in 1:n_obs) {
  for (period in 1:num_periods) {
    fpt_var_name <- paste0("fpt", period) # Family planing Status
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
        break # Salimos del bucle (no)
      } else if (!is.na(current_byrt)) {
        # message(paste("Advertencia: Valor de byrt no mapeado:", current_byrt, "para individuo", kfamily$id[i], "periodo", period))
      }
    }
  }
}

min(toa_from_fpt, na.rm = TRUE)
toa_from_fpt <- toa_from_fpt - 1963

# TOA derivado de cfp/cbyr

toa_from_cfp <- rep(NA, n_obs)
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

# --- Construcción del TOA_derivado final ---

n_total_obs <- length(toa_original) # Total de observaciones = 1047

# 1. TOA combinado priorizando fptX, luego cfp
toa_combinado_adoptadores <- ifelse(!is.na(toa_from_fpt), toa_from_fpt, toa_from_cfp)
length(toa_combinado_adoptadores)
sum(is.na(toa_combinado_adoptadores)) # tiene aún 450 NAs

# 2. Crear el TOA_derivado final
TOA_derivado <- rep(NA, n_total_obs)

# a) Casos donde fptX derivó el TOA
indices_fpt_validos <- which(!is.na(toa_from_fpt))
TOA_derivado[indices_fpt_validos] <- toa_from_fpt[indices_fpt_validos]
num_from_fpt <- length(indices_fpt_validos)

# b) Casos donde SOLO cfp derivó el TOA (y fptX no pudo)
# Estos son los que rellenan los NAs de toa_from_fpt
indices_cfp_relleno <- which(is.na(toa_from_fpt) & !is.na(toa_from_cfp))
TOA_derivado[indices_cfp_relleno] <- toa_from_cfp[indices_cfp_relleno]
num_from_cfp_relleno <- length(indices_cfp_relleno)

# c) Casos que originalmente eran TOA = 11 (no adoptadores)
num_toa_11_original <- sum(toa_original == 11) # Debería ser 374

# Asignar 11 a los que quedaron NA en TOA_derivado.
# Esto asume que si no pudimos derivar un TOA de adopción (1-10), entonces son no-adoptadores (11).
indices_11_original <- which(toa_original == 11)
TOA_derivado[indices_11_original] <- 11 # Asignamos 11 a los restantes
num_asignados_como_11 <- length(indices_11_original)


# --- Resumen de la Composición de TOA_derivado ---
message("\n--- Composición de TOA_derivado Final ---")
message(paste("Total de observaciones:", n_total_obs))
message(paste("Número de TOAs derivados de fptX/byrtX:", num_from_fpt,
              paste0("(", round(num_from_fpt / n_total_obs * 100, 2), "%)")))
message(paste("Número de TOAs derivados (como relleno) de cfp/cbyr:", num_from_cfp_relleno,
              paste0("(", round(num_from_cfp_relleno / n_total_obs * 100, 2), "%)")))
message(paste("Número de TOAs asignados como '11' (no adoptadores restantes):", num_asignados_como_11,
              paste0("(", round(num_asignados_como_11 / n_total_obs * 100, 2), "%)")))
message(paste("En total suman:", num_from_fpt + num_from_cfp_relleno + num_asignados_como_11,
              paste0("(", round((num_from_fpt + num_from_cfp_relleno + num_asignados_como_11) / n_total_obs * 100, 2), "%)")))




# Comparación final de TOA_derivado con toa_original
# (Esto es solo para verificar qué tan bien replicamos el original)
diff_final <- TOA_derivado - toa_original
coincidencias_finales_exactas <- sum(diff_final == 0, na.rm = TRUE) # na.rm por si acaso, aunque no deberían quedar NAs
porcentaje_final_exacto <- (coincidencias_finales_exactas / n_total_obs) * 100

message(paste("\nComparación de TOA_derivado final con toa_original:"))
message(paste("Número de coincidencias exactas:", coincidencias_finales_exactas, "de", n_total_obs,
              paste0("(", round(porcentaje_final_exacto, 2), "%)")))
message("Resumen de diferencias (TOA_derivado - toa_original):")
print(summary(diff_final))
print(table(diff_final, useNA="ifany"))