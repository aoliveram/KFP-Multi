# Calcula TOA para cada método específico

# kfamily, fpstatus_labels, num_periods, n_obs, year_map,
# y actual_method_codes_numeric_sorted ya existen. (correr parte 1)

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

# ------------------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------------------

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


par(mfrow = c(2, 2), mar = c(4, 4, 3, 1)) # mar: bottom, left, top, right margins

min_toa_val <- 1
max_toa_val <- 10
hist_breaks <- seq(min_toa_val - 0.5, max_toa_val + 0.5, by = 1)

# Colores para los histogramas (opcional)
hist_colors <- RColorBrewer::brewer.pal(n = 4, name = "Set2") # Necesita el paquete RColorBrewer

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
  # Añadir un eje X personalizado con etiquetas en los centros de las barras (años 1 a 10)
  axis(1, at = min_toa_val:max_toa_val, labels = min_toa_val:max_toa_val)
  
}

# --- 4. Restaurar los parámetros gráficos originales (opcional) ---
# par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1)) # Valores por defecto de R