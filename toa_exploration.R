# --- ANÁLISIS DE CAMBIOS DE MÉTODO ANTICONCEPTIVO ---

# Asumiendo que kfamily, fpstatus_labels, num_periods, n_obs,
# actual_method_codes_numeric_sorted (con los códigos de métodos modernos que se analizan),
# code_loop, code_pill (códigos numéricos específicos) ya existen.

# correr parte 1 y 2 primero toa_construction.

if (!exists("kfamily") || !exists("fpstatus_labels") || !exists("actual_method_codes_numeric_sorted") ||
    !exists("code_loop") || !exists("code_pill") ) {
  stop("Una o más variables necesarias no se encontraron. Asegúrate de haber ejecutado los bloques anteriores.")
}

# --- 1. Inicializar contadores y estructuras de datos ---
n_obs <- nrow(kfamily)
num_periods <- 12 # Para fpt1 a fpt12

# Contadores
total_switches_modern_to_modern <- 0
switches_from_loop_to_pill_fpt <- 0
switches_to_loop_from_other_modern_fpt <- 0
switches_to_pill_from_other_modern_fpt <- 0

# Para un análisis más detallado, podríamos guardar los IDs de quienes cambian
# o la secuencia de métodos de cada individuo. Por ahora, nos centraremos en los conteos.

# Dataframe para rastrear el primer y último método moderno usado en fptX por individuo
# y el método en cfp si es moderno
individual_method_trajectory <- data.frame(
  id = kfamily$id,
  first_modern_method_fpt = NA,
  first_modern_method_code_fpt = NA,
  last_modern_method_fpt = NA,
  last_modern_method_code_fpt = NA,
  method_in_cfp = NA,
  method_in_cfp_code = NA,
  switched_modern_to_modern_fpt = FALSE,
  switched_loop_to_pill_fpt = FALSE
)

# Obtener los nombres de las etiquetas para los códigos
# (fpstatus_labels tiene nombres=etiquetas, valores=códigos; necesitamos lo inverso para lookup rápido)
code_to_label_map <- setNames(names(fpstatus_labels), fpstatus_labels)


# --- 2. Analizar la secuencia de fptX para cada individuo ---
for (i in 1:n_obs) {
  previous_modern_method_code <- NA
  sequence_has_modern_method <- FALSE
  
  for (period in 1:num_periods) {
    fpt_var_name <- paste0("fpt", period)
    if (!(fpt_var_name %in% names(kfamily))) next
    
    current_fpt_code <- kfamily[i, fpt_var_name]
    
    # Verificar si el método actual es uno de los modernos que estamos analizando
    if (!is.na(current_fpt_code) && current_fpt_code %in% actual_method_codes_numeric_sorted) {
      current_modern_method_code <- current_fpt_code
      
      if (!sequence_has_modern_method) { # Primera vez que vemos un método moderno para este individuo en fptX
        individual_method_trajectory$first_modern_method_code_fpt[i] <- current_modern_method_code
        sequence_has_modern_method <- TRUE
      }
      
      # Registrar este como el último método moderno visto (se sobrescribirá si hay más)
      individual_method_trajectory$last_modern_method_code_fpt[i] <- current_modern_method_code
      
      # Verificar si hubo un cambio DESDE un método moderno ANTERIOR A OTRO método moderno ACTUAL
      if (!is.na(previous_modern_method_code) && previous_modern_method_code != current_modern_method_code) {
        total_switches_modern_to_modern <- total_switches_modern_to_modern + 1
        individual_method_trajectory$switched_modern_to_modern_fpt[i] <- TRUE
        
        # Contabilizar cambio específico de Loop a Pill
        if (previous_modern_method_code == code_loop && current_modern_method_code == code_pill) {
          switches_from_loop_to_pill_fpt <- switches_from_loop_to_pill_fpt + 1
          individual_method_trajectory$switched_loop_to_pill_fpt[i] <- TRUE
        }
        # Contabilizar llegadas a Loop o Pill desde otro moderno
        if (current_modern_method_code == code_loop && previous_modern_method_code != code_loop) {
          switches_to_loop_from_other_modern_fpt <- switches_to_loop_from_other_modern_fpt + 1
        }
        if (current_modern_method_code == code_pill && previous_modern_method_code != code_pill) {
          switches_to_pill_from_other_modern_fpt <- switches_to_pill_from_other_modern_fpt + 1
        }
      }
      previous_modern_method_code <- current_modern_method_code # Actualizar para la siguiente iteración de periodo
      
    } else if (!is.na(current_fpt_code) && !(current_fpt_code %in% actual_method_codes_numeric_sorted)){
      # Si el método actual NO es uno de los "modernos" que analizamos (podría ser "Pregnant", "No more", etc.)
      # o si es NA, reseteamos previous_modern_method_code para indicar una discontinuación o un periodo sin uso de PF moderno.
      # Esto asegura que un cambio se cuente solo si es de un método moderno a *otro* método moderno directamente.
      previous_modern_method_code <- NA
    }
  } # Fin bucle periodos
  
  # Registrar el método en cfp si es moderno
  current_cfp_code <- kfamily$cfp[i]
  if (!is.na(current_cfp_code) && current_cfp_code %in% actual_method_codes_numeric_sorted) {
    individual_method_trajectory$method_in_cfp_code[i] <- current_cfp_code
  }
} # Fin bucle individuos

# Convertir códigos a etiquetas para el dataframe final
individual_method_trajectory$first_modern_method_fpt <- code_to_label_map[as.character(individual_method_trajectory$first_modern_method_code_fpt)]
individual_method_trajectory$last_modern_method_fpt  <- code_to_label_map[as.character(individual_method_trajectory$last_modern_method_code_fpt)]
individual_method_trajectory$method_in_cfp         <- code_to_label_map[as.character(individual_method_trajectory$method_in_cfp_code)]


# --- 3. Resumen de los Cambios ---
message("\n--- Resumen de Cambios de Métodos Anticonceptivos (basado en fptX) ---")
message(paste("Número total de individuos que cambiaron de un método moderno a otro moderno (en fpt1-fpt12):",
              sum(individual_method_trajectory$switched_modern_to_modern_fpt, na.rm = TRUE)))
message(paste("Número de cambios específicos de 'Loop' a 'Oral Pill' (en fpt1-fpt12):",
              sum(individual_method_trajectory$switched_loop_to_pill_fpt, na.rm = TRUE)))
message(paste("Número de llegadas a 'Loop' desde otro método moderno (en fpt1-fpt12):", switches_to_loop_from_other_modern_fpt))
message(paste("Número de llegadas a 'Oral Pill' desde otro método moderno (en fpt1-fpt12):", switches_to_pill_from_other_modern_fpt))


# --- 4. Análisis de Cambios considerando `cfp` como estado final ---
# Comparamos el último método moderno en fptX con el método en cfp (si ambos son modernos)
switches_fpt_to_cfp <- 0
switches_fpt_loop_to_cfp_pill <- 0

for (i in 1:n_obs) {
  last_fpt_code <- individual_method_trajectory$last_modern_method_code_fpt[i]
  cfp_code <- individual_method_trajectory$method_in_cfp_code[i]
  
  # Si ambos son métodos modernos válidos y diferentes
  if (!is.na(last_fpt_code) && !is.na(cfp_code) && last_fpt_code != cfp_code) {
    switches_fpt_to_cfp <- switches_fpt_to_cfp + 1
    
    if (last_fpt_code == code_loop && cfp_code == code_pill) {
      switches_fpt_loop_to_cfp_pill <- switches_fpt_loop_to_cfp_pill + 1
    }
  }
}
message("\n--- Cambios del Último Método en fptX al Método en cfp ---")
message(paste("Número de individuos que cambiaron de su último método moderno en fptX a un método moderno diferente en cfp:", switches_fpt_to_cfp))
message(paste("De estos, cambios de 'Loop' (en fptX) a 'Oral Pill' (en cfp):", switches_fpt_loop_to_cfp_pill))

# --- 5. Tabla de transición más detallada (opcional, puede ser grande) ---
# Contar las transiciones específicas entre métodos en la secuencia fptX
message("\n--- Tabla de Transiciones entre Métodos Modernos (fptX) ---")
# Esta es una simplificación; un análisis de Markov completo sería más complejo
# Aquí solo contamos pares de (método_anterior, método_actual)
transitions_list <- list()

for (i in 1:n_obs) {
  previous_modern_method_code <- NA
  for (period in 1:num_periods) {
    fpt_var_name <- paste0("fpt", period)
    if (!(fpt_var_name %in% names(kfamily))) next
    current_fpt_code <- kfamily[i, fpt_var_name]
    
    if (!is.na(current_fpt_code) && current_fpt_code %in% actual_method_codes_numeric_sorted) {
      if (!is.na(previous_modern_method_code) && previous_modern_method_code != current_fpt_code) {
        from_method_label <- code_to_label_map[as.character(previous_modern_method_code)]
        to_method_label <- code_to_label_map[as.character(current_fpt_code)]
        transition_pair <- paste(from_method_label, "->", to_method_label)
        
        if (transition_pair %in% names(transitions_list)) {
          transitions_list[[transition_pair]] <- transitions_list[[transition_pair]] + 1
        } else {
          transitions_list[[transition_pair]] <- 1
        }
      }
      previous_modern_method_code <- current_fpt_code
    } else {
      previous_modern_method_code <- NA # Resetea si no es un método moderno o es NA
    }
  }
}
# Convertir lista a dataframe y ordenar
if(length(transitions_list) > 0){
  transitions_df <- data.frame(Transition = names(transitions_list), Count = unlist(transitions_list))
  transitions_df <- transitions_df[order(-transitions_df$Count), ]
  print(transitions_df)
} else {
  message("No se registraron transiciones directas entre métodos modernos en la secuencia fptX.")
}

message("\nNota: Los 'actual_method_codes_numeric_sorted' usados para definir métodos modernos en este script son:")
print(data.frame(Nombre = actual_method_labels_sorted, Codigo = actual_method_codes_numeric_sorted))
