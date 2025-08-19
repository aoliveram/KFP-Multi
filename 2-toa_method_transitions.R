# Calcula matriz de transición de métodos anticonceptivos

# Asumiendo que kfamily, fpstatus_labels, num_periods, n_obs,
# actual_method_codes_numeric_sorted (con los códigos de métodos modernos que se analizan),
# code_loop, code_pill (códigos numéricos específicos) ya existen.


library(netdiffuseR)
data(kfamily)   

# ------------------------------------------------------------------------------
# Construimos -- actual_method_codes_numeric_sorted -- code_loop -- code_pill
# ------------------------------------------------------------------------------

fpstatus_labels <- attr(kfamily, "label.table")$fpstatus

num_periods <- 12
n_obs <- nrow(kfamily)

# métodos modernos
modern_methods_names_in_fpstatus <- c("Loop", "Condom", "Oral Pill", "Vasectomy", "TL",
                                      "Injection", "Rhythm", "Withdrawal")

# nombres excluidos
excluded_categories <- setdiff(names(fpstatus_labels), modern_methods_names_in_fpstatus)


actual_method_labels <- character(0)      # Guardará las etiquetas de texto de los métodos
actual_method_codes_numeric <- numeric(0) # Guardará los códigos numéricos de los métodos

# Iteramos sobre los NOMBRES de fpstatus_labels
for (method_label_text in names(fpstatus_labels)) {
  
  print(method_label_text)
  
  # Verificamos si esta etiqueta de texto NO está en excluidos y NO está ya en nuestra lista
  if (!(method_label_text %in% excluded_categories) && !(method_label_text %in% actual_method_labels)) {
    
    actual_method_labels <- c(actual_method_labels, method_label_text) # etiqueta de texto
    
    actual_method_codes_numeric <- c(actual_method_codes_numeric, unname(fpstatus_labels[method_label_text])) # código numérico asociado. unname() obtiene solo el valor numérico
  }
}

# Ordenar por nombre para consistencia (opcional pero bueno para visualización)
order_methods <- order(actual_method_labels)
actual_method_labels_sorted <- actual_method_labels[order_methods] ## -----------> WHY?
actual_method_codes_numeric_sorted <- actual_method_codes_numeric[order_methods]

# Códigos específicos para Loop y Oral Pill
code_loop <- as.numeric(fpstatus_labels[['Loop']])
code_pill <- as.numeric(fpstatus_labels[['Oral Pill']])

# ------------------------------------------------------------------------------
# TOA exploration
# ------------------------------------------------------------------------------

# --- 1. Inicializar variables y estructuras para el análisis de cambios ---

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


# Resumen de los Cambios de Metodos Anticonceptivos (basado en fptX)
message(paste("Número total de individuos que cambiaron de un método moderno a otro moderno (en fpt1-fpt12):",
              sum(individual_method_trajectory$switched_modern_to_modern_fpt, na.rm = TRUE)))
message(paste("Número de cambios específicos de 'Loop' a 'Oral Pill' (en fpt1-fpt12):",
              sum(individual_method_trajectory$switched_loop_to_pill_fpt, na.rm = TRUE)))
message(paste("Número de llegadas a 'Loop' desde otro método moderno (en fpt1-fpt12):", switches_to_loop_from_other_modern_fpt))
message(paste("Número de llegadas a 'Oral Pill' desde otro método moderno (en fpt1-fpt12):", switches_to_pill_from_other_modern_fpt))


# --- 3. Análisis de Cambios considerando `cfp` como estado final ---
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

# Cambios del Último Método en fptX al Método en cfp ---
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

# Nota: Los 'actual_method_codes_numeric_sorted' usados para definir métodos modernos en este script son:"
print(data.frame(Nombre = actual_method_labels_sorted, Codigo = actual_method_codes_numeric_sorted))
