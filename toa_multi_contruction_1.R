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
