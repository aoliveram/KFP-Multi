library(ggplot2)
#install.packages('gganimate')
library(gganimate)


# [EL T=12 ES LO MISMO QUE T=1]
#
# Según esto, el tiempo en donde fue más común conocer 'dos métodos'
# es en T=3 --> 1966.


# Crear una lista con los datos para cada tiempo t
data_list <- list(
  t01 = KFP$vertex.static.attrs$awe2t1,
  t02 = KFP$vertex.static.attrs$awe2t2,
  t03 = KFP$vertex.static.attrs$awe2t3,
  t04 = KFP$vertex.static.attrs$awe2t4,
  t05 = KFP$vertex.static.attrs$awe2t5,
  t06 = KFP$vertex.static.attrs$awe2t6,
  t07 = KFP$vertex.static.attrs$awe2t7,
  t08 = KFP$vertex.static.attrs$awe2t8,
  t09 = KFP$vertex.static.attrs$awe2t9,
  t10 = KFP$vertex.static.attrs$awe2t10,
  t11 = KFP$vertex.static.attrs$awe2t11,
  t12 = KFP$vertex.static.attrs$awe2t12
)

# Convertir los datos en un marco de datos largo
data_long <- do.call(rbind, lapply(names(data_list), function(t) {
  data.frame(
    Time = t,
    MethodsKnown = ifelse(is.na(data_list[[t]]), 0, data_list[[t]])
  )
}))

# Crear el gráfico animado
p <- ggplot(data_long, aes(x = factor(MethodsKnown), fill = factor(MethodsKnown))) +
  geom_bar() +
  labs(
    title = "Distribución de Métodos Conocidos por Tiempo: {closest_state}",
    x = "Número de Métodos Conocidos",
    y = "Frecuencia"
  ) +
  scale_y_continuous(limits = c(0, 1050)) + # Fijar el eje y con máximo de 1000
  theme_minimal() +
  transition_states(Time, transition_length = 1, state_length = 1)

# Mostrar la animación en RStudio
animate(p, fps = 1, nframes = length(data_list), renderer = gifski_renderer())

