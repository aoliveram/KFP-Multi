library(netdiffuseR)
library(ggplot2)
library(dplyr)  

data(kfamily)       

fpstatus_labels <- attr(kfamily, "label.table")$fpstatus

# ------------------------------------------------------------------------------
# 1) Construimos vector de nombres y de índices
# ------------------------------------------------------------------------------

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


message("Métodos de PF que se analizarán (corregido):")
print(data.frame(Nombre = actual_method_labels_sorted, Codigo = actual_method_codes_numeric_sorted))

# ------------------------------------------------------------------------------
# 2) Prevalencia de CADA MÉTODO en cada periodo ( fptX )
# ------------------------------------------------------------------------------

# Matriz para guardar conteos:  filas = métodos,                      columnas = periodos
prevalence_matrix <- matrix(0, nrow = length(actual_method_labels_sorted), ncol = num_periods,
                            dimnames = list(actual_method_labels_sorted, paste0("Periodo_", 1:num_periods)))

for (period in 1:num_periods) {
  
  fpt_var_name <- paste0("fpt", period)
  
  # Obtener info de fpt para el periodo actual
  current_period_codes_in_data <- kfamily[[fpt_var_name]]
  
  # Contar la frecuencia de cada CÓDIGO de método relevante en el periodo actual
  for (j in 1:length(actual_method_codes_numeric_sorted)) {
    method_code_to_count <- actual_method_codes_numeric_sorted[j] # identificamos código
    method_label_for_matrix <- actual_method_labels_sorted[j]     # y nombre del método
    
    # Sumar cuántas veces aparece este código en los datos del periodo actual
    count_for_method <- sum(current_period_codes_in_data == method_code_to_count, na.rm = TRUE)
    prevalence_matrix[method_label_for_matrix, period] <- count_for_method
  }
}

prevalence_matrix

# En PORCENTAJE de usuarias que reportan cada método en cada periodo
prevalence_percentage_of_users <- prevalence_matrix
for(period in 1:num_periods) {
  fpt_var_name <- paste0("fpt", period)
  if (!(fpt_var_name %in% names(kfamily))) next
  
  total_users_in_period <- sum(kfamily[[fpt_var_name]] %in% actual_method_codes_numeric_sorted, na.rm = TRUE)
  if (total_users_in_period > 0) {
    prevalence_percentage_of_users[, period] <- (prevalence_matrix[, period] / total_users_in_period) * 100
  } else {
    prevalence_percentage_of_users[, period] <- 0
  }
}
print(round(prevalence_percentage_of_users, 1))

# TOTAL de reportes por método
total_reports_per_method <- rowSums(prevalence_matrix)
sort(total_reports_per_method, decreasing = TRUE)

# --- Visualización prevalencia por periodo ( fptX ) ---------------------------

library(ggplot2)
library(tidyr)

prevalence_df_long <- as.data.frame(prevalence_matrix)
prevalence_df_long$Metodo <- rownames(prevalence_df_long) # Ahora rownames son las etiquetas correctas
prevalence_df_long <- pivot_longer(prevalence_df_long,
                                   cols = starts_with("Periodo_"),
                                   names_to = "Periodo",
                                   values_to = "Conteo",
                                   names_prefix = "Periodo_")
prevalence_df_long$Periodo <- as.integer(prevalence_df_long$Periodo)

plot_bar_counts <- ggplot(prevalence_df_long, aes(x = Periodo, y = Conteo, fill = Metodo)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(breaks = 1:num_periods) +
  labs(title = "Prevalence of modern methods per period (fptX)",
       x = "Period (fptX)",
       y = "Users") +
  theme_minimal()
print(plot_bar_counts)

ggsave("preval-methos-per-period.pdf", plot = plot_bar_counts)

# ------------------------------------------------------------------------------
# 2) Prevalencia de CADA MÉTODO en cada periodo ( cfp )
# ------------------------------------------------------------------------------

message("\n--- Prevalencia de Métodos según 'cfp' (Estado Actual Deck 5) ---")
cfp_data_codes <- kfamily$cfp
cfp_prevalence <- numeric(length(actual_method_labels_sorted))
names(cfp_prevalence) <- actual_method_labels_sorted

for (j in 1:length(actual_method_codes_numeric_sorted)) {
  method_code_to_count <- actual_method_codes_numeric_sorted[j]
  method_label_for_vector <- actual_method_labels_sorted[j]
  
  count_for_method_cfp <- sum(cfp_data_codes == method_code_to_count, na.rm = TRUE)
  cfp_prevalence[method_label_for_vector] <- count_for_method_cfp
}
sort(cfp_prevalence, decreasing = TRUE)
