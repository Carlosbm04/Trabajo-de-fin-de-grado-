####################################
#
#  survival_rate
#
# En este script voy a crear una gráfica que va a representar la tasa de supervivencia de los embriones totales cultivados
#
####################################

#Libraries --------------------------------------------------------------------
library(openxlsx)
library(ggplot2)
library(tidyverse) #dplyr y pipes

#Data setup -------------------------------------------------------------------
raw_bd <- read.xlsx("../raw_data/RESUMEN.xlsx") #cargo la base de datos (bd) con toda la información
bd <- raw_bd[,c(9:12,16)] #bd con todos los embriones puestos en cultivo
bd_div <- bd[complete.cases(bd[ ,3]), ] #bd solo con los embriones que se han dividido al menos 1 vez
#Survival rate bd --------------------------------------------------------------
survival_bd <- data.frame(tiempo = seq(0:100), #da nombre a survival_bd estableciendo en ella una secuencia de números del 0 al 100 (horas)
                          survival_all = NA, #todos los embriones
                          survival_div = NA) #solo aquellos que se han dividido

## Calculo el tiempo de muerte final ----------------------------------------------
# Tiempo final de muerte bd
bd$tiempo_supervivencia <- NA #Establezco qué es bd

# Esto recorre cada fila y calcula el tiempo de muerte en bd
# Iterar sobre las filas
for (i in 1:nrow(bd)) {
  # Iterar sobre las columnas
  for (j in 1:5) {
    # Verifico si el valor no es NA
    if (!is.na(bd[i, j])) {
      # Si la columna es la 5, estableco el tiempo_supervivencia en 200
      if (j == 5) {
        bd[i, "tiempo_supervivencia"] <- 200
      } else {
        # Si no es la columna 5, estableco el tiempo_supervivencia en el valor de la columna actual
        bd[i, "tiempo_supervivencia"] <- bd[i, j]
      }
    }
  }
}

# Tiempo final de muerte bd
bd_div$tiempo_supervivencia <- NA #Establezco qué es bd_div

#Volvemos a hacer lo mismo que hicimos para bd anteriormente
# Iterar sobre las filas
for (i in 1:nrow(bd_div)) {
  # Iterar sobre las columnas
  for (j in 1:5) {
    # Verifico si el valor no es NA
    if (!is.na(bd_div[i, j])) {
      # Si la columna es la 5, estableco el tiempo_supervivencia en 200
      if (j == 5) {
        bd_div[i, "tiempo_supervivencia"] <- 200
      } else {
        # Si no es la columna 5, estableco el tiempo_supervivencia en el valor de la columna actual
        bd_div[i, "tiempo_supervivencia"] <- bd_div[i, j]
      }
    }
  }
}



## Calculo la tasa de supervivencia de los embriones totales puestos en cultivo----------------------------------------------
tiempo_rango <- 0:100 #el tiempo máximo al que todos los embriones ya han llegado a la fase de blastocisto es en el tiempo 100 h, por lo que el límite de tiempo de la gráfica va a ser hasta 100 h.
# Inicializar un vector para almacenar las tasas de supervivencia.
tasas_supervivencia_all <- numeric(length(tiempo_rango))

# Calculo la tasa de supervivencia para cada punto de tiempo en el rango.
for (t in tiempo_rango) {
  embriones_sobrevivientes <- sum(bd$tiempo_supervivencia >= t) #Calcula el número de embriones que han sobrevivido hasta el tiempo t o más ## CAMBIAR BD PARA OBTENER LAS DIFERENTES TASAS
  tasa_supervivencia <- embriones_sobrevivientes / nrow(bd) #Calcula la tasa de superviviencia diviendo los embriones sobrevivientes entre le total
  tasas_supervivencia_all[t + 1] <- tasa_supervivencia  #almacena la tasa de superviviencia calculada en el vector "tasas_supervivencia_all"  #añado +1 para ajustar el índice de R
}

## Calculo la tasa de supervivencia para los embriones que han sido capaces de dividirse al menos una vez----------------------------------------------
#Para ello hago lo mimso que hice anteriormente pero con los datos de los embriones que se han dividio al menos una vez
tiempo_rango <- 0:100
# Inicializar un vector para almacenar las tasas de supervivencia.
tasas_supervivencia_div <- numeric(length(tiempo_rango))

# Calcular la tasa de supervivencia para cada punto de tiempo en el rango.
for (t in tiempo_rango) {
  embriones_sobrevivientes <- sum(bd_div$tiempo_supervivencia >= t) ## CAMBIAR BD PARA OBTENER LAS DIFERENTES TASAS
  tasa_supervivencia <- embriones_sobrevivientes / nrow(bd_div)
  tasas_supervivencia_div[t + 1] <- tasa_supervivencia  # añado +1 para ajustar el índice de R
}

# Añado las tasas de supervivencias a la bd
survival_bd$survival_all <- tasas_supervivencia_all #tasa de supervivencia del total de embriones
survival_bd$survival_div <- tasas_supervivencia_div #tasa de supervivencia de los embriones que al menos se han dividido una vez

## Calculo la mediana en cada división del embrión------------------------------------------
# 2 cels
tiempo_2cel <- median(bd_div$t2)
# 4 cels
bd_div4 <- bd[complete.cases(bd[, 2]), ] #bd con los embriones que se han dividido al menos 1 vez
tiempo_4cel <- median(bd_div4$t3)
# 8 cels
bd_div8 <- bd[complete.cases(bd[, 3]), ] #bd con los embriones que se han dividido al menos 1 vez
tiempo_8cel <- median(bd_div8$t4)
# morula
bd_mor <- bd[complete.cases(bd[, 4]), ] #bd con los embriones que se han dividido al menos 1 vez
tiempo_mor <- median(bd_mor$t5)
# blasto
bd_blasto <- bd[complete.cases(bd),] #bd con los embriones que se han dividido al menos 1 vez
tiempo_blasto <- median(bd_blasto$Morula)

#Vector con los tiempos de división, estableciendo los 5 tiempos de división más importantes:
tiempos_destacados <- c(tiempo_2cel, tiempo_4cel, tiempo_8cel, tiempo_mor, tiempo_blasto) #crea dicho vector que contiene los tiempos destacados
tiempos_destacados <- tiempos_destacados %>% as.integer() # Estamos truncando los valores porque el gráfico sólo acepta valores enteros. "as.integer" te convierte numeros flotantes en enteros y nos sirve para este cometido
etiquetas <- c("2 cél", "4 cél", "8 cél", "Mórula", "Blastocisto") #crea un marco de datos para posteriormente poder añadir las etiquetas al gráfico
data_etiquetas <- data.frame(x = tiempos_destacados, etiquetas, y = 1.05)

# Ahora creo la gráfica que muestra la tasa de superviviencia embrionaria en cuestión:------------------------------------------------------------------------
ggplot(survival_bd, aes(x = tiempo)) +
  geom_vline(xintercept = tiempos_destacados, linetype = 6, color = "black", size = 0.8, alpha = .5)+ #este código crea las líneas verticales correspondientes a los tiempos destacados nomrbados anteriormente
  geom_line(aes(y = survival_all, color = "Grupo All"), size = 1) + #línea que representa los embriones totales
  geom_line(aes(y = survival_div, color = "Grupo Div"), size = 1) + #línea que representa aquellos que se han dividido al menos una vez
  scale_color_manual(values = c("Grupo All" = "blue", "Grupo Div" = "red")) + #la línea de color azul representará los embriones totales puestos en cultivo, mientras que la roja representa aquellosque se han dividio al menos una vez
  labs(x = "Tiempo en horas", y = "Tasa de Supervivencia", #establezco qué representa el eje de abcisas y ordenadas
       title = "Tasa de supervivencia de embriones en el incubador MIRI") + #título principal
  scale_color_discrete(name = "Grupos de estudio", labels = c("Todos los embriones",
                                                              "Al menos 1 división"
  ))+ # Cambiamos las características de la leyenda a nuestro gusto:
  geom_text(data = data_etiquetas, aes(x = x, y = y, label = etiquetas), vjust = -1, size = 4, color = "black")+  
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) +
  theme(
    text = element_text(size = 16),  # Tamaño de letra global
    axis.title = element_text(size = 16),  # Tamaño de letra del título del eje
    legend.title = element_text(size = 18),  # Tamaño de letra del título de la leyenda
    axis.title.x = element_text(margin = margin(t = 10)), #ajusto el margen del título del eje x
    axis.title.y = element_text(margin = margin(r = 20)), #ajusto el margen del título del eje y
    plot.title = element_text(hjust = 0.3, margin = margin(t = 20, b = 10)) #ajusto el margen del título principal
  ) 

