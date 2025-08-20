# Calcula TOA para cada método específico

# Cargamos lo necesario
library(netdiffuseR)
data(kfamily)

fpstatus_labels <- attr(kfamily, "label.table")$fpstatus
num_periods <- 12
n_obs <- nrow(kfamily)

# Mapa de años (byrtX/cbyr -> año)
year_map <- c("4"=1964,"5"=1965,"6"=1966,"7"=1967,"8"=1968,"9"=1969,"0"=1970,"1"=1971,"2"=1972,"3"=1973)

# Códigos de métodos modernos (según nombres en fpstatus)
modern_methods_names_in_fpstatus <- c("Loop","Condom","Oral Pill","Vasectomy","TL","Injection","Rhythm","Withdrawal")
keep <- names(fpstatus_labels) %in% modern_methods_names_in_fpstatus                                                    # 8
actual_method_labels_sorted <- sort(names(fpstatus_labels)[keep])
actual_method_codes_numeric_sorted <- unname(fpstatus_labels[actual_method_labels_sorted])                              # 10

# ------------------------------------------------------------------------------
# Estrategia 1 (Loop - Pill - Others)
# ------------------------------------------------------------------------------

code_loop <- as.numeric(fpstatus_labels[['Loop']])
code_pill <- as.numeric(fpstatus_labels[['Oral Pill']])
codes_other_moder_1 <- setdiff(actual_method_codes_numeric_sorted, c(code_loop, code_pill))

# Inicializar los vectores TOA_method
TOA_loop <- rep(NA, n_obs)
TOA_pill <- rep(NA, n_obs)
TOA_other_moder_1 <- rep(NA, n_obs)

# Definir el year_map (copiado de tu script anterior, verifica que es el correcto para byrtX)
year_map <- c( "4"=1964, "5"=1965, "6"=1966, "7"=1967, "8"=1968,
                    "9"=1969, "0"=1970, "1"=1971, "2"=1972, "3"=1973 )

# Bucle para derivar TOA_method usando fptX y byrtX
for (i in 1:n_obs) {
  for (period in 1:num_periods) {
    fpt_var_name <- paste0("fpt", period)
    byrt_var_name <- paste0("byrt", period)
    
    current_fpt_status <- kfamily[i, fpt_var_name]
    current_byrt_char <- as.character(kfamily[i, byrt_var_name]) # Convertir a caracter para el mapeo
    
    year_of_adoption_this_period <- year_map[current_byrt_char] - 1963 # Ajustar a escala 1-10
    
    # TOA para Loop
    if (is.na(TOA_loop[i]) && isTRUE(current_fpt_status == code_loop)) {
      TOA_loop[i] <- year_of_adoption_this_period
    }
    
    # TOA para Oral Pill
    if (is.na(TOA_pill[i]) && isTRUE(current_fpt_status == code_pill)) {
      TOA_pill[i] <- year_of_adoption_this_period
    }
    
    # TOA para Resto de Métodos Modernos
    if (is.na(TOA_other_moder_1[i]) && current_fpt_status %in% codes_other_moder_1) {
      TOA_other_moder_1[i] <- year_of_adoption_this_period
    }
  }
}

# Bucle para RELLENAR usando cfp y cbyr 
for (i in 1:n_obs) {
  current_cfp_status <- kfamily$cfp[i]
  current_cbyr_char <- as.character(kfamily$cbyr[i])
  
  year_of_adoption_cfp_period <- year_map[current_cbyr_char] - 1963 # Ajustar a escala 1-10
  
  # Relleno para TOA_loop
  if (is.na(TOA_loop[i]) && isTRUE(current_cfp_status == code_loop)) {
    TOA_loop[i] <- year_of_adoption_cfp_period
  }
  
  # Relleno para TOA_pill
  if (is.na(TOA_pill[i]) && isTRUE(current_cfp_status == code_pill)) {
    TOA_pill[i] <- year_of_adoption_cfp_period
  }
  
  # Relleno para TOA_other_moder_1
  if (is.na(TOA_other_moder_1[i]) && current_cfp_status %in% codes_other_moder_1) {
    TOA_other_moder_1[i] <- year_of_adoption_cfp_period
  }
}

# Resumen de los TOA_method derivados
print(table(TOA_loop, useNA = "ifany"))
message(paste("Número de usuarias con TOA_loop:", sum(!is.na(TOA_loop))))

print(table(TOA_pill, useNA = "ifany"))
message(paste("Número de usuarias con TOA_pill:", sum(!is.na(TOA_pill))))

print(table(TOA_other_moder_1, useNA = "ifany"))
message(paste("Número de usuarias con TOA_other_moder_1:", sum(!is.na(TOA_other_moder_1))))


hist(TOA_loop)
hist(TOA_pill)
hist(TOA_condom)

# ------------------------------------------------------------------------------
# Estrategia 2 (Loop - Pill - Condom - Others)
# ------------------------------------------------------------------------------


code_loop <- as.numeric(fpstatus_labels[['Loop']])
code_pill <- as.numeric(fpstatus_labels[['Oral Pill']])
code_condom <- as.numeric(fpstatus_labels[['Condom']])
codes_other_moder_2 <- setdiff(actual_method_codes_numeric_sorted, c(code_loop, code_pill, code_condom))

# Inicializar los vectores TOA_method
TOA_loop <- rep(NA, n_obs)
TOA_pill <- rep(NA, n_obs)
TOA_condom <- rep(NA, n_obs)
TOA_other_moder_2 <- rep(NA, n_obs)

# Definir el year_map (copiado de tu script anterior, verifica que es el correcto para byrtX)
year_map <- c( "4"=1964, "5"=1965, "6"=1966, "7"=1967, "8"=1968,
                    "9"=1969, "0"=1970, "1"=1971, "2"=1972, "3"=1973 )


# Bucle para derivar TOA_method usando fptX y byrtX
for (i in 1:n_obs) {
  for (period in 1:num_periods) {
    fpt_var_name <- paste0("fpt", period)
    byrt_var_name <- paste0("byrt", period)
    
    current_fpt_status <- kfamily[i, fpt_var_name]
    current_byrt_char <- as.character(kfamily[i, byrt_var_name]) # Convertir a caracter para el mapeo
    
    year_of_adoption_this_period <- year_map[current_byrt_char] - 1963 # Ajustar a escala 1-10
    
    # TOA para Loop
    if (is.na(TOA_loop[i]) && isTRUE(current_fpt_status == code_loop)) {
      TOA_loop[i] <- year_of_adoption_this_period
    }
    
    # TOA para Oral Pill
    if (is.na(TOA_pill[i]) && isTRUE(current_fpt_status == code_pill)) {
      TOA_pill[i] <- year_of_adoption_this_period
    }
    
    # TOA para Condom
    if (is.na(TOA_condom[i]) && isTRUE(current_fpt_status == code_condom)) {
      TOA_condom[i] <- year_of_adoption_this_period
    }
    
    # TOA para Resto de Métodos Modernos
    if (is.na(TOA_other_moder_2[i]) && current_fpt_status %in% codes_other_moder_2) {
      TOA_other_moder_2[i] <- year_of_adoption_this_period
    }
  }
}
# Bucle para RELLENAR usando cfp y cbyr 
for (i in 1:n_obs) {
  current_cfp_status <- kfamily$cfp[i]
  current_cbyr_char <- as.character(kfamily$cbyr[i])
  
  year_of_adoption_cfp_period <- year_map[current_cbyr_char] - 1963 # Ajustar a escala 1-10
  
  # Relleno para TOA_loop
  if (is.na(TOA_loop[i]) && isTRUE(current_cfp_status == code_loop)) {
    TOA_loop[i] <- year_of_adoption_cfp_period
  }
  
  # Relleno para TOA_pill
  if (is.na(TOA_pill[i]) && isTRUE(current_cfp_status == code_pill)) {
    TOA_pill[i] <- year_of_adoption_cfp_period
  }
  
  # Relleno para TOA_condom
  if (is.na(TOA_condom[i]) && isTRUE(current_cfp_status == code_condom)) {
    TOA_condom[i] <- year_of_adoption_cfp_period
  }
  
  # Relleno para TOA_other_moder_1
  if (is.na(TOA_other_moder_1[i]) && current_cfp_status %in% codes_other_moder_1) {
    TOA_other_moder_1[i] <- year_of_adoption_cfp_period
  }
}

# Resumen de los TOA_method derivados
print(table(TOA_loop, useNA = "ifany"))
message(paste("Número de usuarias con TOA_loop:", sum(!is.na(TOA_loop))))

print(table(TOA_pill, useNA = "ifany"))
message(paste("Número de usuarias con TOA_pill:", sum(!is.na(TOA_pill))))

print(table(TOA_condom, useNA = "ifany"))
message(paste("Número de usuarias con TOA_condom:", sum(!is.na(TOA_condom))))

print(table(TOA_other_moder_2, useNA = "ifany"))
message(paste("Número de usuarias con TOA_other_moder_2:", sum(!is.na(TOA_other_moder_2))))


# Plot --------------------------------------------------------------------------------

toa_list_strategy2 <- list(
  Loop = TOA_loop,
  Pill = TOA_pill,
  Condom = TOA_condom,
  Otros_Modernos_v2 = TOA_other_moder_2
)

# frecuencia máxima
max_freq <- 0
for (toa_vec in toa_list_strategy2) {
  if (sum(!is.na(toa_vec)) > 0) { # Solo si hay datos no-NA
    counts <- table(toa_vec)
    if (length(counts) > 0 && max(counts) > max_freq) {
      max_freq <- max(counts)
    }
  }
}
y_limit <- c(0, max_freq + max_freq * 0.05) # Añade un 5% de margen


min_toa_val <- 1
max_toa_val <- 10
hist_breaks <- seq(min_toa_val - 0.5, max_toa_val + 0.5, by = 1)

# Colores para los histogramas (opcional)
hist_colors <- RColorBrewer::brewer.pal(n = 4, name = "Set2") # Necesita el paquete RColorBrewer

pdf("toa_specific_histograms.pdf", width = 6, height = 5)

op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (i in 1:length(toa_list_strategy2)) {
  method_name <- names(toa_list_strategy2)[i]
  toa_data_for_hist <- toa_list_strategy2[[i]]
  
  hist(toa_data_for_hist,
       main = paste("TOA ", gsub("_", " ", method_name)),
       xlab = "Time step",
       ylab = "Users",
       ylim = y_limit,
       breaks = hist_breaks, # Usar los mismos breaks para todos
       col = hist_colors[i],
       xaxt = 'n' # Suprimir el eje X por defecto
  )
  axis(1, at = min_toa_val:max_toa_val, labels = min_toa_val:max_toa_val) #(años 1 a 10)
  
}
par(op)  # restaurar parámetros gráficos
dev.off()


# ------------------------------------------------------------------------------------
# --- 4. Construcción del TOA derivado con MÉTODO ---
# ------------------------------------------------------------------------------------

n_obs <- length(toa_list_strategy2[[1]])
TOA_derivado <- rep(NA, n_obs)
TOA_derivado_metodo <- rep(NA, n_obs)

method_order <- c("Loop", "Pill", "Condom", "Otros_Modernos_v2")

for (i in 1:n_obs) {
  for (method in method_order) {
    toa_val <- toa_list_strategy2[[method]][i]
    if (!is.na(toa_val)) {
      TOA_derivado[i] <- toa_val
      TOA_derivado_metodo[i] <- method
      break
    }
  }
  # Si ninguno es moderno, queda NA
}

TOA_derivado_obj <- data.frame(
  id = 1:n_obs,
  TOA_derivado = TOA_derivado,
  metodo_origen = TOA_derivado_metodo
)

# Guardar como .csv y .rds
write.csv(TOA_derivado_obj, "TOA_derivado_from_strategy2.csv", row.names = FALSE)
saveRDS(TOA_derivado_obj, "TOA_derivado_from_strategy2.rds")

# Para cargar en otros scripts:
# TOA_derivado_obj <- read.csv("TOA_derivado_from_strategy2.csv")
#

# Comparar ----------------------------------------------------------------------------------

TOA_derivado_full <- read.csv("TOA_derivado_full.csv")
TOA_derivado_orig <- TOA_derivado_full$TOA_derivado

TOA_derivado_new <- TOA_derivado_obj$TOA_derivado

# Filtrar índices donde TOA_derivado_orig es distinto de 11 y no es NA
valid_indices <- which(!is.na(TOA_derivado_orig) & TOA_derivado_orig != 11)

# Comparar, tratando NA en TOA_derivado_new como FALSE
comparison <- mapply(function(orig, new) {
  if (is.na(new)) {
    FALSE
  } else {
    orig == new
  }
}, TOA_derivado_orig[valid_indices], TOA_derivado_new[valid_indices])

# Resumen de la comparación
cat("Coinc:", sum(comparison), "/", length(comparison), ". Diff:", sum(!comparison), "\n")

# Opcional: mostrar los casos donde difieren
if (sum(!comparison) > 0) {
  cat("Índices con diferencias:\n")
  print(valid_indices[!comparison])
  cat("Valores originales:\n")
  print(TOA_derivado_orig[valid_indices[!comparison]])
  cat("Valores nuevos:\n")
  print(TOA_derivado_new[valid_indices[!comparison]])
}
