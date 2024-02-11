####################################
#
#  p-value
#
# En este script voy a averiar cuál es el p-value para comprobar si tengo una diferencia significativa entre los resultados obtenidos entre las dos incubadoras utilizadas

####################################
## Establecemos los datos para cada grupo
exito_grupo1 <- 15  # número de éxitos en el grupo 1
n_grupo1 <- 187    # tamaño de la muestra del grupo 1

exito_grupo2 <- 11  # número de éxitos en el grupo 2
n_grupo2 <- 125    # tamaño de la muestra del grupo 2

# Realizamos el test de proporciones
resultado_prop_test <- prop.test(c(exito_grupo1, exito_grupo2), c(n_grupo1, n_grupo2)) #realiza una prueba de proporciones comparando los éxitos y los tamaños de muestra de ambos grupos

# Imprimir el resultado
print(resultado_prop_test) #así obtenemos los resultados que se pedían en el comando anterior, obteniendo diversos valores, entre ellos el valor p, que es el que nos interesa

###p-value = 0.9722