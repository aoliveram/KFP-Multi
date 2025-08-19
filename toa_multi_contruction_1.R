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
# 3) Prevalencia de CADA MÉTODO en cada periodo ( cfp )
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

# ----------------------------------------------------------------------------
# 3.1) Matriz de prevalencia SOLO con cfp + cbyr
# ----------------------------------------------------------------------------
# En lugar de replicar la distribución de cfp en todas las columnas, usamos cbyr
# para ubicar a cada observación en su periodo correspondiente.

# Mapa de cbyr -> año calendario (mismo que BYRtx)
year_map_cfp <- c(
  "0" = 1970, "1" = 1971, "2" = 1972, "3" = 1973,
  "4" = 1964, "5" = 1965, "6" = 1966, "7" = 1967,
  "8" = 1968, "9" = 1969
)

# Calculamos el PERIODO a partir de cbyr
cbyr_vec <- kfamily$cbyr
# TOA calendario y periodo relativo (1963 -> 0)
cfp_year_from_cbyr <- rep(NA_integer_, n_obs)
mapped_years <- year_map_cfp[as.character(cbyr_vec)]
cfp_year_from_cbyr[!is.na(mapped_years)] <- as.integer(mapped_years[!is.na(mapped_years)])
cfp_period_from_cbyr <- as.integer(cfp_year_from_cbyr - 1963L)  # 1964->1, ..., 1973->10

# Filtramos a quienes reportan un método moderno en cfp
is_cfp_modern <- kfamily$cfp %in% actual_method_codes_numeric_sorted
rows_use <- which(is_cfp_modern & !is.na(cfp_period_from_cbyr) & cfp_period_from_cbyr >= 1 & cfp_period_from_cbyr <= num_periods)

# Matriz vacía para cfp + cbyr
prevalence_matrix_cfp <- matrix(
  0L,
  nrow = length(actual_method_labels_sorted),
  ncol = num_periods,
  dimnames = list(actual_method_labels_sorted, paste0("Periodo_", 1:num_periods))
)
# Conteo por método y periodo, con carry-forward
method_code_to_row <- setNames(seq_along(actual_method_codes_numeric_sorted), actual_method_codes_numeric_sorted)
for (i in rows_use) {
  mcode <- as.character(kfamily$cfp[i])
  r <- method_code_to_row[mcode]
  p <- cfp_period_from_cbyr[i]
  prevalence_matrix_cfp[r, p:num_periods] <- prevalence_matrix_cfp[r, p:num_periods] + 1L
}
print(prevalence_matrix_cfp)

# Porcentaje (denominador: total modernas cfp en ese periodo)
prevalence_cfp_percentage_of_users <- prevalence_matrix_cfp
denom_period_cfp <- colSums(prevalence_matrix_cfp)
for (p in seq_len(num_periods)) {
  if (denom_period_cfp[p] > 0) {
    prevalence_cfp_percentage_of_users[, p] <- (prevalence_matrix_cfp[, p] / denom_period_cfp[p]) * 100
  } else {
    prevalence_cfp_percentage_of_users[, p] <- 0
  }
}
print(round(prevalence_cfp_percentage_of_users, 2))

# -------------------------------------------------------------------------------------
# Visualización
# -------------------------------------------------------------------------------------

# Data frame largo para graficar
prevalence_cfp_df_long <- as.data.frame(prevalence_matrix_cfp)
prevalence_cfp_df_long$Metodo <- factor(rownames(prevalence_cfp_df_long), levels = rownames(prevalence_matrix_cfp))
prevalence_cfp_df_long <- tidyr::pivot_longer(
  prevalence_cfp_df_long,
  cols = starts_with("Periodo_"),
  names_to = "Periodo",
  values_to = "Conteo",
  names_prefix = "Periodo_"
)
prevalence_cfp_df_long$Periodo <- as.integer(prevalence_cfp_df_long$Periodo)

# Gráfico
plot_bar_cfp <- ggplot(prevalence_cfp_df_long, aes(x = Periodo, y = Conteo, fill = Metodo)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(breaks = 1:num_periods) +
  labs(title = "Prevalence of modern methods per period (cfp + cbyr)",
       subtitle = "Cada observación se ubica en el periodo según cbyr",
       x = "Period (1..12)",
       y = "Users") +
  theme_minimal()
print(plot_bar_cfp)

ggsave("preval-methods-cfp-cbyr.pdf", plot = plot_bar_cfp, width = 9, height = 6)

# ----------------------------------------------------------------------------
# 3.2) Opción B: sumar cfp SOLO para quienes nunca reportaron moderno en fptX
# ----------------------------------------------------------------------------
# Identificamos quiénes NUNCA reportaron un método moderno en fpt1..fpt12
fpt_vars <- paste0("fpt", 1:num_periods)
is_modern_fpt <- sapply(fpt_vars, function(v) kfamily[[v]] %in% actual_method_codes_numeric_sorted)
never_modern_in_fpt <- rowSums(is_modern_fpt, na.rm = TRUE) == 0

# De ellos, contamos cfp SOLO si es Método Moderno
is_cfp_modern <- kfamily$cfp %in% actual_method_codes_numeric_sorted
cfp_only_never_fpt_idx <- which(never_modern_in_fpt & is_cfp_modern)

# Conteo adicional por método (solo este subconjunto)
cfp_only_counts <- setNames(rep(0L, length(actual_method_labels_sorted)), actual_method_labels_sorted)
for (j in seq_along(actual_method_codes_numeric_sorted)) {
  method_code <- actual_method_codes_numeric_sorted[j]
  method_label <- actual_method_labels_sorted[j]
  cfp_only_counts[method_label] <- sum(kfamily$cfp[cfp_only_never_fpt_idx] == method_code, na.rm = TRUE)
}

message(paste("N de individuos 'cfp modernos' que NUNCA reportaron moderno en fptX:",
              length(cfp_only_never_fpt_idx)))
print(sort(cfp_only_counts, decreasing = TRUE))

# Construimos el complemento de cfp **por periodo** usando cbyr, con carry-forward
cfp_only_rows <- which(never_modern_in_fpt & is_cfp_modern)
cfp_only_matrix <- matrix(
  0L,
  nrow = nrow(prevalence_matrix), ncol = num_periods,
  dimnames = list(rownames(prevalence_matrix), paste0("Periodo_", 1:num_periods))
)
for (i in cfp_only_rows) {
  mcode <- as.character(kfamily$cfp[i])
  r <- method_code_to_row[mcode]
  p <- cfp_period_from_cbyr[i]
  cfp_only_matrix[r, p:num_periods] <- cfp_only_matrix[r, p:num_periods] + 1L
}
print(cfp_only_matrix)

# Matriz total = fptX + complemento cfp (solo nunca-modernos en fptX, por periodo)
prevalence_matrix_tot <- prevalence_matrix + cfp_only_matrix

# Denominadores por periodo para % (fptX + complemento cfp)
# prevalence_tot_percentage_of_users <- prevalence_matrix_tot
# for (period in 1:num_periods) {
#   fpt_var_name <- paste0("fpt", period)
#   if (!(fpt_var_name %in% names(kfamily))) next

#   # Total usuarias modernas en fptX del periodo + el complemento cfp de no-modernos
#   total_users_in_period <- sum(kfamily[[fpt_var_name]] %in% actual_method_codes_numeric_sorted, na.rm = TRUE)
#   total_users_in_period <- total_users_in_period + sum(cfp_only_matrix[, period])
#   if (total_users_in_period > 0) {
#     prevalence_tot_percentage_of_users[, period] <- (prevalence_matrix_tot[, period] / total_users_in_period) * 100
#   } else {
#     prevalence_tot_percentage_of_users[, period] <- 0
#   }
# }

# Graficamos la prevalencia total: conteos y %
prevalence_tot_df_long <- as.data.frame(prevalence_matrix_tot)
prevalence_tot_df_long$Metodo <- factor(rownames(prevalence_tot_df_long), levels = rownames(prevalence_matrix))
prevalence_tot_df_long <- tidyr::pivot_longer(
  prevalence_tot_df_long,
  cols = starts_with("Periodo_"),
  names_to = "Periodo",
  values_to = "Conteo",
  names_prefix = "Periodo_"
)
prevalence_tot_df_long$Periodo <- as.integer(prevalence_tot_df_long$Periodo)

# prevalence_tot_pct_df_long <- as.data.frame(prevalence_tot_percentage_of_users)
# prevalence_tot_pct_df_long$Metodo <- factor(rownames(prevalence_tot_pct_df_long), levels = rownames(prevalence_matrix))
# prevalence_tot_pct_df_long <- tidyr::pivot_longer(
#   prevalence_tot_pct_df_long,
#   cols = starts_with("Periodo_"),
#   names_to = "Periodo",
#   values_to = "Porcentaje",
#   names_prefix = "Periodo_"
# )
# prevalence_tot_pct_df_long$Periodo <- as.integer(prevalence_tot_pct_df_long$Periodo)

plot_bar_total <- ggplot(prevalence_tot_df_long, aes(x = Periodo, y = Conteo, fill = Metodo)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(breaks = 1:num_periods) +
  labs(title = "Prevalence of modern methods per period (fptX + cfp para no-modernos)",
       subtitle = "cfp solo aporta para quienes jamás reportaron moderno en fptX",
       x = "Period (fptX)",
       y = "Users") +
  theme_minimal()
print(plot_bar_total)

ggsave("preval-methods-total.pdf", plot = plot_bar_total, width = 9, height = 6)


# --- Gráfico comparativo: fptX vs cfp-only vs Total -------------
prevalence_fpt_df_long <- prevalence_df_long %>%
  dplyr::mutate(Scenario = "fptX")
prevalence_cfp_df_long2 <- prevalence_cfp_df_long %>%
  dplyr::mutate(Scenario = "cfp-only")
prevalence_tot_df_long2 <- prevalence_tot_df_long %>%
  dplyr::mutate(Scenario = "Total (fptX + cfp)")

prevalence_compare_long <- dplyr::bind_rows(
  prevalence_fpt_df_long,
  prevalence_cfp_df_long2,
  prevalence_tot_df_long2
)

plot_bar_compare <- ggplot(prevalence_compare_long, aes(x = Periodo, y = Conteo, fill = Metodo)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(breaks = 1:num_periods) +
  labs(title = "Comparativo de prevalencias por escenario",
       subtitle = "fptX, cfp replicado, y fptX + complemento cfp",
       x = "Period",
       y = "Users") +
  theme_minimal() +
  facet_wrap(~ Scenario, ncol = 1)
print(plot_bar_compare)

ggsave("preval-methods-compare.pdf", plot = plot_bar_compare, width = 10, height = 12)
