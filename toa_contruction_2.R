library(netdiffuseR)
data(kfamily)       

# 1) --- Importamos y definimos métodos modernos ---
toa_original <- kfamily$toa

table(toa_original)
hist(toa_original)
hist(toa_original[toa_original<11])

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

# ------------------------------------------------------------------------------
# 2) --- Derivamos TOA a partir de variables fptX y byrtX ---
# ------------------------------------------------------------------------------

num_periods <- 12 # Hay hasta fpt12 y byrt12
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

table(toa_from_fpt, useNA = "ifany")
hist(toa_from_fpt)

# Comparación con toa_original

valid_indices_fpt <- !is.na(toa_from_fpt) # quitamos los NA de toa_from_fpt

toa_without_11 <- kfamily$toa[kfamily$toa != 11]

cor(toa_original[valid_indices_fpt], toa_from_fpt[valid_indices_fpt]) # Alta correlación

# Diferencias directas
diff_fpt <- toa_original[valid_indices_fpt] - toa_from_fpt[valid_indices_fpt]
print(summary(diff_fpt))
message(paste("Número de coincidencias exactas:", sum(diff_fpt == 0, na.rm = TRUE), "de", sum(valid_indices_fpt)))

# 3) --- Identificar casos NO capturados ---

max(toa_from_fpt, na.rm = TRUE) # No se obtuvo NINGUNO con TOA=11

indices_no_capturados <- which(is.na(toa_from_fpt))
num_no_capturados <- length(indices_no_capturados)
message(paste("\nNúmero de TOAs originales que NO fueron capturados por la derivación fptX/byrtX:", num_no_capturados))

# Obtenemos los toa_original para estos casos
toa_original_no_capturados <- toa_original[indices_no_capturados]

print(table(toa_original_no_capturados, useNA = "ifany"))
print(summary(toa_original_no_capturados))

# NINGUNO de los TOAs originales no capturados por fptX/byrtX es igual a 0.

# Datos de los primeros individuos no capturados
not_in_toa_from_fpt_df <- data.frame(
  id = kfamily$id[indices_no_capturados],
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
)
head(not_in_toa_from_fpt_df, 15)

# Some strange cases, like id=4 : toa_original = 7, but no fptX after time 4.

# Índices de los usuarios cuyo toa_original es DISTINTO de 11
indices_toa_no_11 <- which(toa_original != 11)

toa_no_11_df <- data.frame(
  id = kfamily$id[indices_toa_no_11],
  toa_original = toa_original[indices_toa_no_11],
  fpt1 = kfamily$fpt1[indices_toa_no_11],
  fpt2 = kfamily$fpt2[indices_toa_no_11],
  fpt3 = kfamily$fpt3[indices_toa_no_11],
  fpt4 = kfamily$fpt4[indices_toa_no_11],
  fpt5 = kfamily$fpt5[indices_toa_no_11],
  fpt6 = kfamily$fpt6[indices_toa_no_11],
  fpt7 = kfamily$fpt7[indices_toa_no_11],
  fpt8 = kfamily$fpt8[indices_toa_no_11],
  fpt9 = kfamily$fpt9[indices_toa_no_11],
  fpt10 = kfamily$fpt10[indices_toa_no_11],
  fpt11 = kfamily$fpt11[indices_toa_no_11],
  fpt12 = kfamily$fpt12[indices_toa_no_11],
  fp1 = kfamily$fp1[indices_toa_no_11],
  fp6 = kfamily$fp6[indices_toa_no_11],
  cft = kfamily$cfp[indices_toa_no_11]
)
head(toa_no_11_df, 15)

sort(unique(toa_no_11_df$cft)) # TODO presente, solo FALTAN Stillbirth (11), Newlywed (12), Injection (18), Jelly (19), Foam (20)

# Índices de los usuarios cuyo toa_original es IGUAL de 11
indices_toa_11 <- which(toa_original == 11)

toa_11_df <- data.frame(
  id = kfamily$id[indices_toa_11],
  toa_original = toa_original[indices_toa_11],
  fpt1 = kfamily$fpt1[indices_toa_11],
  fpt2 = kfamily$fpt2[indices_toa_11],
  fpt3 = kfamily$fpt3[indices_toa_11],
  fpt4 = kfamily$fpt4[indices_toa_11],
  fpt5 = kfamily$fpt5[indices_toa_11],
  fpt6 = kfamily$fpt6[indices_toa_11],
  fpt7 = kfamily$fpt7[indices_toa_11],
  fpt8 = kfamily$fpt8[indices_toa_11],
  fpt9 = kfamily$fpt9[indices_toa_11],
  fpt10 = kfamily$fpt10[indices_toa_11],
  fpt11 = kfamily$fpt11[indices_toa_11],
  fpt12 = kfamily$fpt12[indices_toa_11],
  fp1 = kfamily$fp1[indices_toa_11],
  fp6 = kfamily$fp6[indices_toa_11],
  cft = kfamily$cfp[indices_toa_11]
)
head(toa_11_df, 15)

sort(unique(toa_11_df$cft)) # PRESENTES: Pregnant (01), Normal Birth (02), Menopause (07), 
                            # Want more/No more (08,09), Infertile (10), Other (21)

# ---> Así que al parecer TOA=11 quiere decir que no adoptó.

# Consideramos métodos contraceptivos "modernos":
# Loop (03), Condom (04), Oral Pill (05), Vasectomy (06), Tubal Ligation (15), Injection (18)
# Estos TAMBIÉN (extraño):
# Rhythm (14) y Withdrawal (16).
# Además, excluimos estos:
# Excluimos: Pregnant (01), Normal Birth (02), Menopause (07), Want more/No more (08,09), Infertile (10),
# Stillbirth (11), Newlywed (12), Abortion (13), Pessary (17), Jelly (19), Foam (20), Other (21)


# --- Reconstrucción de TOA, pero sin 11 ----

indices_without_11 <- which(toa_original != 11)
toa_original_without11 <- toa_original[indices_without_11]

length(toa_original_without11)
print(table(toa_original_without11))

num_periods <- 10
toa_from_fpt_without11 <- rep(NA, length(indices_without_11)) # Vector para almacenar los TOA derivados

for (j in 1:length(indices_without_11)) {
  i <- indices_without_11[j] # 'i' es el índice original en kfamily
  
  for (period in 1:num_periods) {
    fpt_var_name <- paste0("fpt", period)
    byrt_var_name <- paste0("byrt", period)
    
    if (!(fpt_var_name %in% names(kfamily)) || !(byrt_var_name %in% names(kfamily))) {
      next
    }
    
    current_fpt_status <- kfamily[i, fpt_var_name]
    current_byrt <- kfamily[i, byrt_var_name]
    
    if (!is.na(current_fpt_status) && current_fpt_status %in% modern_methods_codes && is.na(toa_from_fpt_without11[j])) {
      year_map <- c(
        "4" = 1964, "5" = 1965, "6" = 1966, "7" = 1967, "8" = 1968,
        "9" = 1969, "0" = 1970, "1" = 1971, "2" = 1972, "3" = 1973
      )
      if (!is.na(current_byrt) && as.character(current_byrt) %in% names(year_map)) {
        toa_from_fpt_without11[j] <- year_map[as.character(current_byrt)]
        break
      }
    }
  }
}

toa_from_fpt_without11 <- toa_from_fpt_without11 - 1963

sum(!is.na(toa_from_fpt_without11))  # 422 datos 
sum(is.na(toa_from_fpt_without11)) # 251 NA's

table(toa_from_fpt, useNA = "ifany")
table(toa_from_fpt_without11, useNA="ifany") # TODOS los valores quedaron iguales, solo NA se redujo

# Es decir, si expluimos a los TOA=11, solo me quedarían <NA>=251 poe identificar su TOA.

# ¿Cuáles eran los valores de TOA dentro de kfamily, para los casos NA que tenemos en TOA reconstruido?
table(toa_original_without11[which(is.na(toa_from_fpt_without11))], useNA = "ifany")

## O SEA QUE LOS TOA NO IDENTIFICADOS TIENEN VALORES ORIGINALES EN TODO EL ESPECTRO, DESDE 1 A 10.

# Solo por si acaso, IDs originales de kfamily para estos casos
#ids_originales_para_na_reconstruido <- kfamily$id[indices_without_11[indices_na_en_reconstruido_subset]]


# ------------------------------------------------------------------------------
# 3) Derivación de TOA a partir de cfp y cbyr/cbmnth
# ------------------------------------------------------------------------------

# cfp: ver en variables 63-64 de codebook original, Deck 5
# los labels son los mismos que attr(kfamily, "label.table")$fpstatus

# cbyr, cbmnth: ver en variables 68-69-70 de codebook original, Deck 5

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

sum(!is.na(toa_from_cfp))  # 363 datos 
sum(is.na(toa_from_cfp))   # 684 NA's

# Comparación y Correlación
valid_indices_cfp <- !is.na(toa_from_cfp) # quitamos los NA de toa_from_fpt

cor(toa_original[valid_indices_cfp], toa_from_cfp[valid_indices_cfp]) # Alta correlación

# Diferencias directas
diff_cfp <- toa_original[valid_indices_cfp] - toa_from_cfp[valid_indices_cfp]
print(summary(diff_cfp))
message(paste("Número de coincidencias exactas:", sum(diff_cfp == 0, na.rm = TRUE), "de", sum(valid_indices_cfp)))

# --- Reconstrucción de TOA, pero sin 11 ----

indices_without_11 <- which(toa_original != 11)
toa_original_without11 <- toa_original[indices_without_11]

num_periods <- 10
toa_from_cfp_without11 <- rep(NA, length(indices_without_11)) # Vector para almacenar los TOA derivados

for (j in 1:length(indices_without_11)) {
  
  i <- indices_without_11[j]
  
  current_cfp_status <- kfamily$cfp[i]
  current_cbyr <- kfamily$cbyr[i]
  # cbmnth podría usarse para mayor precisión si fuera necesario, pero toa es anual.
  
  if (!is.na(current_cfp_status) && current_cfp_status %in% modern_methods_codes) {
    if (!is.na(current_cbyr) && as.character(current_cbyr) %in% names(year_map_cfp)) {
      toa_from_cfp_without11[i] <- year_map_cfp[as.character(current_cbyr)]
    } else if (!is.na(current_cbyr)) {
      # message(paste("Advertencia: Valor de cbyr no mapeado:", current_cbyr, "para individuo", kfamily$id[i]))
    }
  }
}
for (j in 1:length(indices_without_11)) {
  i <- indices_without_11[j] # 'i' es el índice original en kfamily
  
  for (period in 1:num_periods) {
    current_cfp_status <- paste0("fpt", period)
    current_cbyr <- paste0("byrt", period)
    
    if (!(current_cfp_status %in% names(kfamily)) || !(current_cbyr %in% names(kfamily))) {
      next
    }
    
    current_fpt_status <- kfamily[i, current_cfp_status]
    current_byrt <- kfamily[i, current_cbyr]
    
    if (!is.na(current_fpt_status) && current_fpt_status %in% modern_methods_codes && is.na(toa_from_cfp_without11[j])) {
      year_map <- c(
        "4" = 1964, "5" = 1965, "6" = 1966, "7" = 1967, "8" = 1968,
        "9" = 1969, "0" = 1970, "1" = 1971, "2" = 1972, "3" = 1973
      )
      if (!is.na(current_byrt) && as.character(current_byrt) %in% names(year_map)) {
        toa_from_cfp_without11[j] <- year_map[as.character(current_byrt)]
        break
      }
    }
  }
}

toa_from_cfp_without11 <- toa_from_cfp_without11 - 1963

sum(!is.na(toa_from_cfp_without11))  # 645 datos 
sum(is.na(toa_from_cfp_without11))   # 399 NA's

table(toa_from_cfp, useNA = "ifany")
table(toa_from_cfp_without11, useNA="ifany") 

# Al igual que con fptX, solo cambia MÍNIMAMENTE <NA>: 684 (contando a todos) -> 681 (without 11)

# ¿Cuáles eran los valores de TOA dentro de kfamily, para los casos NA que tenemos en TOA reconstruido?
table(toa_original_without11[which(is.na(toa_from_cfp_without11))], useNA = "ifany")

## O SEA QUE LOS TOA NO IDENTIFICADOS TIENEN VALORES ORIGINALES EN TODO EL ESPECTRO, DESDE 1 A 10.


# ------------------------------------------------------------------------------
# Crucemos los datos
# ------------------------------------------------------------------------------


# indicadores booleanos para cada método
has_toa_fpt <- !is.na(toa_from_fpt)
has_toa_cfp <- !is.na(toa_from_cfp)

# 1) solapamiento --
n_obs <- length(toa_original)

# a) Casos capturados por AMBOS métodos a la vez # 191
sum(has_toa_fpt & has_toa_cfp)

# b) Casos capturados EXCLUSIVAMENTE por fptX.   # 231
sum(has_toa_fpt & !has_toa_cfp)

# c) Casos capturados EXCLUSIVAMENTE por cfp.    # 172
sum(!has_toa_fpt & has_toa_cfp)


# d) Casos NO capturados por NINGÚN método       # 453
sum(!has_toa_fpt & !has_toa_cfp)


# Combinamos ambos métodos para un único TOA 

toa_combinado <- ifelse(!is.na(toa_from_fpt), toa_from_fpt, toa_from_cfp) # Esta línea prioriza fptX: si fptX tiene un valor, lo usa. Si no, intenta usar el de cfp.

num_combinados_na <- sum(is.na(toa_combinado))
num_combinados_validos <- sum(!is.na(toa_combinado))

message(paste("\nAl combinar ambos métodos, se derivó un TOA para", num_combinados_validos, "individuos."))
message(paste("Esto deja", num_combinados_na, "individuos sin un TOA derivado."))

# 2) Análisis donde AMBOS métodos capturan un TOA 

# ¿Coinciden en el año de adopción?
indices_both <- which(has_toa_fpt & has_toa_cfp)
diff_both_methods <- toa_from_fpt[indices_both] - toa_from_cfp[indices_both]

print(summary(diff_both_methods))
coincidencias_exactas_ambos <- sum(diff_both_methods == 0, na.rm = TRUE)
message(paste("Número de coincidencias exactas entre los dos métodos:", coincidencias_exactas_ambos, "de", sum(has_toa_fpt & has_toa_cfp)))


# 3) Análisis casos TOA NA

# Estos son los "casos misteriosos"
indices_misteriosos <- which(is.na(toa_combinado))
toa_original_misteriosos <- toa_original[indices_misteriosos]

print(table(toa_original_misteriosos, useNA = "ifany"))

# Solo tenemos 79 casos donde TOA != 11. --> un 7.5%.       ---> BKN
length(toa_original_misteriosos[toa_original_misteriosos!=11])
length(toa_original_misteriosos[toa_original_misteriosos!=11])/n_obs

# 4) Los TOA_fpt válidos coinciden exactamente. ¿Pero los de relleno cfp?

indices_relleno_cfp <- which(!has_toa_fpt & has_toa_cfp)
length(indices_relleno_cfp)

toa_original_para_relleno <- toa_original[indices_relleno_cfp]
toa_derivado_cfp_para_relleno <- toa_from_cfp[indices_relleno_cfp]

# Comparamos ESOS TOAs relleno con ESOS TOAs originales
valid_comparison_relleno <- !is.na(toa_original_para_relleno) & !is.na(toa_derivado_cfp_para_relleno)
toa_original_para_relleno_valid <- toa_original_para_relleno[valid_comparison_relleno]
toa_derivado_cfp_para_relleno_valid <- toa_derivado_cfp_para_relleno[valid_comparison_relleno]

num_valid_relleno_comparisons <- length(toa_original_para_relleno_valid)

diff_relleno <- toa_original_para_relleno_valid - toa_derivado_cfp_para_relleno_valid
diff_relleno

coincidencias_exactas_relleno <- sum(diff_relleno == 0, na.rm = TRUE)
porcentaje_coincidencias_relleno <- (coincidencias_exactas_relleno / num_valid_relleno_comparisons) * 100

print(table(diff_relleno, useNA = "ifany"))

message(paste("Número de coincidencias exactas para los TOAs de relleno (de cfp):",
              coincidencias_exactas_relleno, "de", num_valid_relleno_comparisons,
              paste0("(", round(porcentaje_coincidencias_relleno, 2), "%)")))

