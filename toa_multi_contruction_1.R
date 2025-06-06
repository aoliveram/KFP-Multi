library(ggplot2)
library(dplyr)  
library(netdiffuseR)

data(kfamily)       

fpstatus_labels <- attr(kfamily, "label.table")$fpstatus

# ------------------------------------------------------------------------------
# 1) Construimos vector de nombres y de índices
# ------------------------------------------------------------------------------

num_periods <- 12
n_obs <- nrow(kfamily)

# Excluir categorías que claramente no son métodos de PF o son estados
excluded_categories <- c("Pregnant", "NormalB", "Menopause",
                         "Want more children", "Don't want more children but not practicing",
                         "Infertile", "Still birth", "Newly wed", "Abortion",
                         "Pessary", "Jelly", "Foam tablet", "Other", "0ther", NA) # Añadido NA por si acaso

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


message("Métodos de PF que se analizarán (corregido):")
print(data.frame(Nombre = actual_method_labels_sorted, Codigo = actual_method_codes_numeric_sorted))

# ------------------------------------------------------------------------------
# 2) Prevalencia de CADA MÉTODO en cada periodo fptX
# ------------------------------------------------------------------------------

# Matriz para guardar conteos:  filas = métodos,                      columnas = periodos
prevalence_matrix <- matrix(0, nrow = length(actual_method_labels_sorted), ncol = num_periods,
                            dimnames = list(actual_method_labels_sorted, paste0("Periodo_", 1:num_periods)))

for (period in 1:num_periods) {
  
  fpt_var_name <- paste0("fpt", period)
  
  # Obtener los códigos de fpt para el periodo actual
  current_period_codes_in_data <- kfamily[[fpt_var_name]]
  
  # Contar la frecuencia de cada CÓDIGO de método relevante en el periodo actual
  for (j in 1:length(actual_method_codes_numeric_sorted)) {
    method_code_to_count <- actual_method_codes_numeric_sorted[j]
    method_label_for_matrix <- actual_method_labels_sorted[j]
    
    # Sumar cuántas veces aparece este código en los datos del periodo actual
    count_for_method <- sum(current_period_codes_in_data == method_code_to_count, na.rm = TRUE)
    prevalence_matrix[method_label_for_matrix, period] <- count_for_method
  }
}


prevalence_matrix
