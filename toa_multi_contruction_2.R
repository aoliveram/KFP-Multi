# --- GENERACIÓN DE TOA ESPECÍFICOS POR MÉTODO (Estrategia 1) ---

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
year_map_byrt <- c( "4"=1964, "5"=1965, "6"=1966, "7"=1967, "8"=1968,
                    "9"=1969, "0"=1970, "1"=1971, "2"=1972, "3"=1973 )


# Bucle para derivar TOA_method usando fptX y byrtX
for (i in 1:n_obs) {
  for (period in 1:num_periods) {
    fpt_var_name <- paste0("fpt", period)
    byrt_var_name <- paste0("byrt", period)
    
    current_fpt_status <- kfamily[i, fpt_var_name]
    current_byrt_char <- as.character(kfamily[i, byrt_var_name]) # Convertir a caracter para el mapeo
    
    year_of_adoption_this_period <- year_map_byrt[current_byrt_char] - 1963 # Ajustar a escala 1-10
    
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

# Resumen de los TOA_method derivados
print(table(TOA_loop, useNA = "ifany"))
message(paste("Número de usuarias con TOA_loop:", sum(!is.na(TOA_loop))))

print(table(TOA_pill, useNA = "ifany"))
message(paste("Número de usuarias con TOA_pill:", sum(!is.na(TOA_pill))))

print(table(TOA_other_moder_1, useNA = "ifany"))
message(paste("Número de usuarias con TOA_other_moder_1:", sum(!is.na(TOA_other_moder_1))))

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
year_map_byrt <- c( "4"=1964, "5"=1965, "6"=1966, "7"=1967, "8"=1968,
                    "9"=1969, "0"=1970, "1"=1971, "2"=1972, "3"=1973 )


# Bucle para derivar TOA_method usando fptX y byrtX
for (i in 1:n_obs) {
  for (period in 1:num_periods) {
    fpt_var_name <- paste0("fpt", period)
    byrt_var_name <- paste0("byrt", period)
    
    current_fpt_status <- kfamily[i, fpt_var_name]
    current_byrt_char <- as.character(kfamily[i, byrt_var_name]) # Convertir a caracter para el mapeo
    
    year_of_adoption_this_period <- year_map_byrt[current_byrt_char] - 1963 # Ajustar a escala 1-10
    
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

# Resumen de los TOA_method derivados
print(table(TOA_loop, useNA = "ifany"))
message(paste("Número de usuarias con TOA_loop:", sum(!is.na(TOA_loop))))

print(table(TOA_pill, useNA = "ifany"))
message(paste("Número de usuarias con TOA_pill:", sum(!is.na(TOA_pill))))

print(table(TOA_condom, useNA = "ifany"))
message(paste("Número de usuarias con TOA_condom:", sum(!is.na(TOA_condom))))

print(table(TOA_other_moder_2, useNA = "ifany"))
message(paste("Número de usuarias con TOA_other_moder_2:", sum(!is.na(TOA_other_moder_2))))

