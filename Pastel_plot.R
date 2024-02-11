####################################
#
#  pastel_plot
#
# En este script voy a crear una gráfica que va a representar una comparativa entre la cantidad de embriones que llegaron a blastocisto entre las dos incubadoras utilizadas en el estudio: la convencional y la MIRI, para así observar a simple vista cuál es más favorable para posteriores cultivos.
#
####################################
# Libraries
library(ggplot2)

# Datos
porcentaje_convencional <- 47.68 #porcentaje sobre 100 de los éxitos en la incubadora convencional
porcentaje_miri <- 52.32 #porcentaje sobre 100 de los éxitos en la incubadora MIRI

# Crea un dataframe con los datos
datos <- data.frame(
  Incubadora = c("Incubadora convencional", "MIRI"), #crea una de las "columnas" con los datos y se le asigna un vector on los nombres de las dos incubadoras
  Porcentaje = c(porcentaje_convencional, porcentaje_miri) # crea una segunda "columna" llamada porcentaje y se le asigna un vector con los porcentajes de cada incubadora
)

## Crea la gráfica de pastel
ggplot(datos, aes(x = "", y = Porcentaje, fill = Incubadora)) + #permite crear un dibujo base con los datos establecidos anteriormente
  geom_bar(stat = "identity", width = 1, color = "white") + #agrega las barras al gráfico, indicando altura, ancho y color
  geom_text(aes(label = paste0(Porcentaje, "%")), #agrega las etiquetas correspondientes
            position = position_stack(vjust = 0.5), 
            color = "white", size = 6,
            vjust = 0, angle = 0) +  # Ajusta la posición vertical y el ángulo del texto
  coord_polar("y") + #transforma el gráfico en un gráfico circular
  theme_void() + #deja las barrs y el texto sin añadir más elementos
  scale_fill_manual(values = c("coral2", "dodgerblue1")) + #colores de cada sector
  labs(title = "Comparación de la eficacia entre incubadoras",
       fill = "Tipo de Incubadora",
       y = "Porcentaje") + #etiquetas del título,leyenda y eje
  theme(legend.text = element_text(size = 12),  # Ajusta el tamaño del texto de las etiquetas
        legend.title = element_text(size = 14))  # Ajusta el tamaño del texto del título de la leyenda

# Guarda la gráfica con mayor resolución
ggsave("grafica_pastel.png", plot = last_plot(), dpi = 1000)
