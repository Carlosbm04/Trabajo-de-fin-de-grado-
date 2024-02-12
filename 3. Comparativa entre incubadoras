####################################
#
#  tornado_plot
#
# En este script voy a crear una gráfica que va a representar una comparativa entre las dos incubadoras utilizadas en el estudio: la convencional y la MIRI.
#
####################################

#Libraries --------------------------------------------------------------------
library(openxlsx)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(patchwork)
install.packages("cowplot")
library(cowplot)

#Data setup
incubadores <- openxlsx::read.xlsx("../raw_data/incubadores.xlsx") #cargo la base de datos (bd) con toda la información

## Butterfly plot
# Convierte la columna 'blasto' a factor con los niveles ordenados para el plot
incubadores$blasto <- factor(incubadores$blasto, levels = c("Totales", "Blasto")) #Crea una nueva columna llamada "blasto" en el marco de datos "incubadores" y permite que la columna "blasto" se ahora tratada como una variable categórica con dos niveles: "Totales" y "Blasto".
incubadores$experimento <- factor(incubadores$experimento, levels = c("Expto 7", "Expto 6", "Expto 5", "Expto 4", "Expto 3", "Expto 2", "Expto 1"), ordered =  T) #Parecido al comando anterior, permite crear una nueva columna llamada "experimento" en el marco de datos "incubadores" y establece un orden específico.

# Cambia el orden de los niveles en la variable 'incubador'
incubadores$incubador <- factor(incubadores$incubador, levels = c("miri", "normal")) #transforma la columna "incubador" en un factor con niveles específicos, permitiendo que sea interpretada como una variable categórica con dos categorías: "miri" y "normal".
# Valores del miri en negativo
incubadores <- incubadores %>%
  mutate(embriones = ifelse(incubador == "miri", -embriones, embriones)) #hace que los datos correspondientes a la incubadora MIRI sean negativos (se multiplican por -1) para que aparezcan en valores negativos del gráfico.

###Creamos la gráfica en cuestión-----------------------------------------------
#Vamos a crear dos gráficas, una que corresponderá a los datos del MIRI y otra a la incubadora convencional. Una vez creadas se unirán de manera manual.

## ggplot miri
miri <- ggplot(incubadores %>% filter(incubador == "miri"), aes(x = factor(experimento), y = embriones, fill = blasto)) + #especifica el valor que van a tomar los ejes x e y y de dónde tomarán la información
  geom_bar(stat = "identity", position = "identity", alpha = 0.7) + #permite agregar barras al gráfico, indica que los valores se tomarán de la columna "embriones" y establece la transparencia de las barras
  coord_flip() + #voltea las cordenadas para que las barras aparezcan en horizontal
  theme_minimal() +
  labs(title = "Comparativa entre incubadoras",
       y = "Número de embriones",
       x = "Experimento") + #agraga las etiquetas correspondientes al título y a los ejes
  scale_fill_manual(values = c("Totales" = "dodgerblue1", "Blasto" = "midnightblue")) + #establece los colores para totales (más claro) y para blasto (más oscuro)
  scale_y_continuous(breaks = seq(-40, 0, by = 5),
                     labels = c(40, 35, 30, 25, 20, 15, 10, 5, 0))+ #establece las ubicaciones y etiquetas del eje y
  guides(fill = guide_legend(title = "Blasto")) + #añade una leyenda con el título "Blasto"
  theme(
    text = element_text(size = 18),  # Tamaño de letra global
    axis.title = element_text(size = 18),  # Tamaño de letra del título del eje
    legend.title = element_text(size = 20)  # Tamaño de letra del título de la leyenda
  ) 

## ggplot normal
normal <-  ggplot(incubadores %>% filter(incubador == "normal"), aes(x = factor(experimento), y = embriones, fill = blasto)) + #especifica el valor que van a tomar los ejes x e y y de dónde tomarán la información
  geom_bar(stat = "identity", position = "identity", alpha = 0.7) + #permite agregar barras al gráfico, indica que los valores se tomarán de la columna "embriones" y establece la transparencia de las barras
  coord_flip() + #voltea las cordenadas para que las barras aparezcan en horizontal
  theme_minimal() +
  labs(title = "Comparativa entre incubadoras",
       y = "Número de embriones",
       x = "Experimento") + #agraga las etiquetas correspondientes al título y a los ejes
  scale_fill_manual(values = c("Totales" = "coral2", "Blasto" = "red")) + #establece los colores para totales (más claro) y para blasto (más oscuro)
  scale_y_continuous(breaks = seq(0, 60, by = 5))+ #establece las ubicaciones y etiquetas del eje y
  guides(fill = guide_legend(title = "Blasto")) + #añade una leyenda con el título "Blasto"
  theme(
    text = element_text(size = 18),  # Tamaño de letra global
    axis.title = element_text(size = 18),  # Tamaño de letra del título del eje
    legend.title = element_text(size = 20)  # Tamaño de letra del título de la leyenda
  ) 


