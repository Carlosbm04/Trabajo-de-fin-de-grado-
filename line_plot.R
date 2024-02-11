####################################
#
# line_plot
#
# En este script voy a crear una gráfica que va a representar el tiempo (en horas) del desarrollo de los embriones que condiguen llegar hasta la fase de blastocisto que han sido cultivados en la incubadora MIRI
#
####################################

#Libraries
library(openxlsx)
library(ggplot2)
library(esquisse)
library(tidyverse) #dplyr y pipes


# Data setup
raw_bd2 <- read.xlsx("../raw_data/Embriones viables.xlsx", sheet = 2) #cargo la base de datos (bd) con toda la información

# Transponemos primero la bd
bd <- t(raw_bd2) %>% as.data.frame() #se transponen las filas y columnas de la base de datos y convierte el resultado en un nuevo marco de datos que queda asignado a la variable "bd"

# Conocer dimensiones y estructura de bd
dim(bd) #dimensión
str(bd) #estructura

#Renombranos nuestras columnas
colnames(bd) <- bd[2,]

# Eliminamos las filas que no nos interesan
bd <- bd[-c(1,2),] #eliminamos filas 1 y 2
nombre_filas <- rownames(bd) #extraemos los nombres de las filas del marco de datos "bd" y los asignamos a la variable "nombre_filas"
# Convertimos en numerica una unica variable
bd$`2 CÉLULAS` <- bd$`2 CÉLULAS` %>% as.numeric()

# Convertimos todas las variables de un data frame en numerico
bd <- bd %>% lapply(as.numeric) %>% as.data.frame() #convierte todas las columnas a tipo numérico
rownames(bd) <- nombre_filas #restaura los nombres de las filas originales
bd <- rownames_to_column(bd) #agrega una nueva columna con los nombres de las filas

nuevo_data_frame <- pivot_longer(bd, 
                                 cols = -rowname, # c(2:ncol(bd)) # Indica la columna que servirá como identificador de embriones
                                 names_to = "División",  # Nombre de la nueva columna para las divisiones
                                 values_to = "Horas"  # Nombre de la nueva columna para las horas
)

## Creamos la gráfica en cuestión:----------------------------------------------
ggplot2::ggplot(nuevo_data_frame, aes(x=factor(División, levels = c("X2.CÉLULAS", 
                                                           "X4.CÉLULAS", 
                                                           "X8.CÉLULAS", 
                                                           "MÓRULA", 
                                                           "BLASTOCISTO")), #establece que en el eje x aparecerán los 5 tiempos de división
                             y=Horas, group=rowname, color=rowname)) + #estable que en el eje y aparecerá la variable "horas", agrupa los datos por la variable y (pues se trazan varias líneas juntas (cada una corresponde a un embrión)) y establece un color específico para cada una
  geom_line()+ #traza las líneas que conectan los puntos de datos
  geom_point() + #traza los puntos en el gráfico. Cuando las dos se juntan permiten trazar tanto las líneas como los puntos en el mismo gráfico
  # establecemos todos los títulos que van a aparecer en el gráfico:
    labs(title = "Desarrollo de embriones viables",
       subtitle = "Tiempo en horas al momento de la división",
       x = "Estado de desarrollo",
       y = "Tiempo (horas)",
       color = "Embriones viables")+
  scale_x_discrete(labels=c("X2.CÉLULAS" = "2 células", 
                            "X4.CÉLULAS" = "4 células",
                            "X8.CÉLULAS" = "8 células",
                            "MÓRULA" = "Mórula",
                            "BLASTOCISTO" = "Blastocisto"))+ #con este código podemos personalizar las etiquetas en el eje x en el gráfico a nuestro gusto
  # scale_colour_viridis_d()+
  scale_color_discrete(name = "Embriones viables", labels = c("Embrión 1",
                               "Embrión 2",
                               "Embrión 3",
                               "Embrión 4",
                               "Embrión 5",
                               "Embrión 6",
                               "Embrión 7",
                               "Embrión 8",
                               "Embrión 9",
                               "Embrión 10",
                               "Embrión 11"
                               ))+ # Este código permite personalizar la leyenda de colores del gráfico
  scale_y_continuous(limits = c(0,120), breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120))+ #este código personaliza el eje y, estableciendo los valores (horas) en los que quedará dividido el eje y.
  
  # Para terminar ajustamos el tamaño de los títulos del gráfico
  theme_minimal()+
  theme(
    text = element_text(size = 18),  # Tamaño de letra global
    axis.title = element_text(size = 18),  # Tamaño de letra del título del eje
    legend.title = element_text(size = 20)  # Tamaño de letra del título de la leyenda
  ) 

