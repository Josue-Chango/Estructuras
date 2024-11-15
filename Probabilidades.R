#########################3333
# PRACTICA PROBABILIDADES

############################
# CARGA DE DATOS
library(ISLR)
Auto
datos = Auto 
#####################
# EJEMPLO 2.8
tabla = matrix(c(21,48,36,26,30,19), nrow = 2)
rownames(tabla) = c("H", "NH")
colnames(tabla) = c("NF", "FM", "FE")
tabla

addmargins(tabla) # TABLA DE CONTINGENCIA

addmargins(prop.table(tabla))


#calcular probabiildades
#no fumador = 0.3833
#hipertenso = 0.4833
#fumador empedernido e impertenso = 0.166
#no fumador o hipertenso = p(NF) + P(H) - P(NF, H) = 0.383 + 0.483 - 0.1166
# que no fume = 0.383
#que fume = 1-P(NF) = 1 - 0.3833 = 0.6167

# las condicionales no se dividen para el total, sino para los subtotales (totales columnas o filas)

?prop.table

addmargins(prop.table(tabla, margin = 1)) # CONDICIONALES DE FILA
addmargins(prop.table(tabla, margin = 2)) # CONDICIONALES DE COLUMNA

