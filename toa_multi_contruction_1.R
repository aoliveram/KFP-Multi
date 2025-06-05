library(ggplot2)
library(dplyr)  
library(netdiffuseR)

data(kfamily)       

fpstatus_labels <- attr(kfamily, "label.table")$fpstatus

# --- 1. Prevalencia en los datos longitudinales (fpt1 a fpt12) ---

num_periods <- 12
n_obs <- nrow(kfamily)

# Crear un dataframe para almacenar los conteos por método y periodo
# Usaremos los nombres de los métodos directamente de fpstatus_labels
method_names_from_labels <- as.character(fpstatus_labels)
# Algunos códigos pueden tener el mismo nombre de etiqueta si hay errores en las etiquetas, usamos unique
unique_method_names <- sort(unique(method_names_from_labels))

# Excluir categorías que claramente no son métodos de PF o son estados (ej. Pregnant, Menopause)
# o categorías ambiguas como "Other" por ahora, a menos que quieras analizarlas.
# También excluimos NA si está como una etiqueta explícita, aunque table() lo maneja.
excluded_categories <- c("Pregnant", "Normal birth", "Menopause",
                         "Want more children", "Don't want more children but not practicing",
                         "Infertile", "Still birth", "Newly wed", "Abortion",
                         "Pessary", "Jelly", "Foam tablet", "Other", "NA") # "Other" puede ser interesante de ver

# Filtrar los nombres de métodos para incluir solo los que son realmente métodos de PF
# y que tienen un código numérico asociado en fpstatus_labels
actual_method_names <- character(0)
actual_method_codes <- numeric(0)

for(code in names(fpstatus_labels)){
  print(code)
  name <- fpstatus_labels[code]
  if(!(name %in% excluded_categories) && !(name %in% actual_method_names)){
    actual_method_names <- c(actual_method_names, name)
    actual_method_codes <- c(actual_method_codes, as.numeric(code))
  }
}
# Ordenar por si acaso
order_methods <- order(actual_method_names)
actual_method_names <- actual_method_names[order_methods]
actual_method_codes <- actual_method_codes[order_methods]


message("Métodos de PF que se analizarán:")
print(data.frame(Nombre = actual_method_names, Codigo = actual_method_codes))

# Matriz para guardar conteos: filas=métodos, columnas=periodos
prevalence_matrix <- matrix(0, nrow = length(actual_method_names), ncol = num_periods,
                            dimnames = list(actual_method_names, paste0("Periodo_", 1:num_periods)))

# --- 2. Contar la prevalencia de cada método en cada periodo fptX ---
for (period in 1:num_periods) {
  fpt_var_name <- paste0("fpt", period)
  if (!(fpt_var_name %in% names(kfamily))) {
    message(paste("Advertencia: Columna", fpt_var_name, "no encontrada. Saltando periodo."))
    next
  }
  
  # Contar la frecuencia de cada código de método en el periodo actual
  current_period_usage <- table(factor(kfamily[[fpt_var_name]], levels = names(fpstatus_labels)))
  
  # Sumar los conteos a nuestra matriz de prevalencia
  for (j in 1:length(actual_method_codes)) {
    method_code_char <- as.character(actual_method_codes[j])
    if (method_code_char %in% names(current_period_usage)) {
      prevalence_matrix[actual_method_names[j], period] <- current_period_usage[method_code_char]
    }
  }
}

# --- 3. Mostrar la Matriz de Prevalencia ---
message("\n--- Matriz de Prevalencia de Uso de Métodos por Periodo (Conteos Absolutos) ---")
print(prevalence_matrix)

# También puede ser útil verla en porcentajes (del total de NAs no excluidos para ese fptX)
# o del total de usuarios de CUALQUIER método en ese periodo
prevalence_percentage_of_users <- prevalence_matrix
for(period in 1:num_periods) {
  fpt_var_name <- paste0("fpt", period)
  if (!(fpt_var_name %in% names(kfamily))) next
  
  # Total de mujeres usando CUALQUIER método de la lista 'actual_method_codes' en este periodo
  total_users_in_period <- sum(kfamily[[fpt_var_name]] %in% actual_method_codes, na.rm = TRUE)
  if (total_users_in_period > 0) {
    prevalence_percentage_of_users[, period] <- (prevalence_matrix[, period] / total_users_in_period) * 100
  } else {
    prevalence_percentage_of_users[, period] <- 0 # Evitar división por cero
  }
}
message("\n--- Matriz de Prevalencia de Uso de Métodos por Periodo (% sobre usuarias de PF en el periodo) ---")
print(round(prevalence_percentage_of_users, 1))


# --- 4. Visualización: Histograma Apilado o Gráfico de Líneas ---
# Para la visualización, puede ser más fácil con un formato 'largo' de datos
if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("tidyr", quietly = TRUE)) {
  library(ggplot2)
  library(tidyr)
  
  prevalence_df_long <- as.data.frame(prevalence_matrix)
  prevalence_df_long$Metodo <- rownames(prevalence_df_long)
  prevalence_df_long <- pivot_longer(prevalence_df_long,
                                     cols = starts_with("Periodo_"),
                                     names_to = "Periodo",
                                     values_to = "Conteo",
                                     names_prefix = "Periodo_")
  prevalence_df_long$Periodo <- as.integer(prevalence_df_long$Periodo)
  
  # Gráfico de Barras Apiladas (Conteos Absolutos)
  plot_bar_counts <- ggplot(prevalence_df_long, aes(x = Periodo, y = Conteo, fill = Metodo)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_x_continuous(breaks = 1:num_periods) +
    labs(title = "Prevalencia de Métodos de PF por Periodo (Conteos)",
         x = "Periodo Longitudinal (fptX)",
         y = "Número de Usuarias") +
    theme_minimal()
  print(plot_bar_counts)
  
  # Gráfico de Líneas (Conteos Absolutos)
  plot_line_counts <- ggplot(prevalence_df_long, aes(x = Periodo, y = Conteo, color = Metodo, group = Metodo)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 1:num_periods) +
    labs(title = "Tendencia de Uso de Métodos de PF por Periodo (Conteos)",
         x = "Periodo Longitudinal (fptX)",
         y = "Número de Usuarias") +
    theme_minimal()
  print(plot_line_counts)
  
  # Usando los porcentajes sobre usuarias de PF
  prevalence_perc_df_long <- as.data.frame(prevalence_percentage_of_users)
  prevalence_perc_df_long$Metodo <- rownames(prevalence_perc_df_long)
  prevalence_perc_df_long <- pivot_longer(prevalence_perc_df_long,
                                          cols = starts_with("Periodo_"),
                                          names_to = "Periodo",
                                          values_to = "Porcentaje",
                                          names_prefix = "Periodo_")
  prevalence_perc_df_long$Periodo <- as.integer(prevalence_perc_df_long$Periodo)
  
  plot_line_perc <- ggplot(prevalence_perc_df_long, aes(x = Periodo, y = Porcentaje, color = Metodo, group = Metodo)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 1:num_periods) +
    labs(title = "Tendencia de Uso de Métodos de PF por Periodo (% sobre usuarias de PF)",
         x = "Periodo Longitudinal (fptX)",
         y = "% de Usuarias de PF") +
    theme_minimal()
  print(plot_line_perc)
  
  
} else {
  message("\nPara visualizaciones más elaboradas, instala los paquetes ggplot2 y tidyr.")
  message("Como alternativa, se muestra un barplot base de R para el primer y último periodo con datos.")
  
  # Barplot base R para el primer periodo con datos de uso
  first_period_with_users <- which(colSums(prevalence_matrix, na.rm=TRUE) > 0)[1]
  if(!is.na(first_period_with_users)){
    barplot(prevalence_matrix[, first_period_with_users],
            main = paste("Prevalencia de Métodos - Periodo", first_period_with_users),
            ylab = "Número de Usuarias",
            las = 2, cex.names = 0.7, col = rainbow(length(actual_method_names)))
    legend("topright", legend=actual_method_names, fill=rainbow(length(actual_method_names)), cex=0.7)
  }
  
  # Barplot base R para el último periodo con datos de uso
  last_period_with_users <- tail(which(colSums(prevalence_matrix, na.rm=TRUE) > 0),1)
  if(length(last_period_with_users) > 0 && !is.na(last_period_with_users)){
    barplot(prevalence_matrix[, last_period_with_users],
            main = paste("Prevalencia de Métodos - Periodo", last_period_with_users),
            ylab = "Número de Usuarias",
            las = 2, cex.names = 0.7, col = rainbow(length(actual_method_names)))
  }
}

# --- 5. Prevalencia General (sumando todos los periodos para cada método) ---
# Esto da una idea de cuántas veces cada método fue reportado en total a lo largo de todos los periodos
# No es "número de usuarias únicas por método", sino "número de reportes de uso por método"
total_reports_per_method <- rowSums(prevalence_matrix)
message("\n--- Número Total de Reportes de Uso por Método (sumando todos los periodos fptX) ---")
print(sort(total_reports_per_method, decreasing = TRUE))

# --- 6. Prevalencia usando `cfp` (estado actual del Deck 5) ---
message("\n--- Prevalencia de Métodos según 'cfp' (Estado Actual Deck 5) ---")
cfp_usage_table <- table(factor(kfamily$cfp, levels = names(fpstatus_labels)))
cfp_prevalence <- numeric(length(actual_method_names))
names(cfp_prevalence) <- actual_method_names

for (j in 1:length(actual_method_codes)) {
  method_code_char <- as.character(actual_method_codes[j])
  if (method_code_char %in% names(cfp_usage_table)) {
    cfp_prevalence[actual_method_names[j]] <- cfp_usage_table[method_code_char]
  }
}
print(sort(cfp_prevalence, decreasing = TRUE))
barplot(sort(cfp_prevalence, decreasing=TRUE), main="Prevalencia de Métodos (Estado Actual cfp)", las=2, cex.names=0.7, ylab="Número de Usuarias", col=rainbow(sum(cfp_prevalence>0)))