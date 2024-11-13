########################
#PRACTIVA DE CESTADISTICA DESCRIPTIVA

#########################
#CARGA DE DATOS

library(ISLR)
Auto
datos=Auto

#########################
#ANALISIS EXPLORATORIO DE DATOS NUMERICOS
millas = datos$mpg #datos numericos : n = 392 observaciones
min(millas)#calcular minimo
max(millas)#calcular maximo
# n  = tamaño muestral
n = length(millas)

#GRAFICOS DESCRIPTIVOS
plot(millas)

#Grafico o diagrama de puntos
??stripchart
stripchart(millas, method = 'stack', ylim = c(0,50))

#-------------------------------------------------
#Histograma
??hist
histo = hist(millas, breaks = 4, main = 'Histograma de mpg', col = 'green',
             ylab = 'Frecuencias')

#podemos guardar el objeto y mostrar la info
histo$breaks #limites de clase
histo$counts #frecuencias absolutas
histo$mids #marca de clase

sum(histo$counts)
histo$counts / n #frecuencias acumuladas

frec.acum = cumsum(histo$counts) #frecuencias acumuladas
plot(histo$mids, frec.acum)
plot(histo$mids, frec.acum, type = 'l')


#---------------------------------------
#MEDIDAS DESCRIPTIVAS

#medidas de localizacion o tendencia central: Media o promedio, Mediana, Moda

ejm = c(21, 21, 21, 22, 20, 19, 23) # edades de estudiantes

#Media aritmetica o promedio
(21+21+21+22+20+19+23)/7
sum(ejm)/7
mean(ejm) #funcion automatica de la media
stripchart(ejm, method = 'stack')

#Mediana
sort(ejm) # ordena los datos
median(ejm)

ejm2 = c(21, 21, 21, 22, 20, 19, 23, 50) # edades de estudiantes
stripchart(ejm2, method = 'stack')
mean(ejm2)
median(ejm2)
sort(ejm2)

#--------------------
#Desviaciones
ejm
ejm - 21


#----------------------------------------------
#PROMEDIO, MEDIANA Y MODA DE LAS MILLAS
mean(millas)
median(millas)
sum(millas - mean(millas))

stripchart(millas, method = 'stack', ylim = c(0,50))
abline(v = mean(millas), col = 2)
abline(v = median(millas), col = 3)

#-------------------------------------------------
#MEDIDAS DE DISPERCION: rango, varianza muestral, desviacion tipica
#trata de medir el grado de dispercion de los datos

#rango
max = max(millas)
min = min(millas)
rango = max - min 
rango 

#varianza
stripchart(ejm, method = "stack")
sum((ejm - 21)^2)/(7-1)
var(ejm)

#--------------------
sqrt(var(ejm))
sd(ejm)

#-------------------------------
# MEdidas de posicion
#quantile()
quantile(millas, probs = 0.05)
quantile(millas, probs = 0.50) # quartil 2
quantile(millas, probs = 0.25) # quartil 1
quantile(millas, probs = 0.75) # quartil 3

# Diagrama de cajas
boxplot(millas)
box = boxplot(datos$acceleration)
boxplot #identificar los outlaiers


#paquete psych
library(psych)
#describe
describe(millas)
describe(datos$acceleration)
describe(datos)

hist(datos$horsepower)

##############################
# Analisis exploratorio de datos categoricos
?Auto
origen = datos$origin # dato categorico codificado numericamente
summary(origen) #variable mal declarada

origen = as.factor(origen) # convirtiendo en factor
summary(origen)

# paso previo para analizar datos es construir datos de grfecuencias
?table
tabla = table(origen) #tiene que estar "origen " ya como factor
addmargins(tabla) # talba de frecuencias absolutas

prop.table(tabla) # frecuencias relativas
addmargins(prop.table(tabla)) # tabla de frecuencias relativas

# graficos de barra
?barplot
barplot(tabla)
barplot(tabla, main = "grafico de barras", xlab = "origen", ylab = "Frec. Abs", Col = c(1,2,3))

# grafico circular
?pie
pie(tabla)
pie(table(datos$name))

# ---------------------------------
# analisis bindimensional con valriables numericas
# Correlacion de pearson o coeficiente R
?cor
potencia = datos$horsepower
describe(potencia)
hist(potencia)
cor(millas, potencia)

# diagrama de dispersion
plot(millas, potencia)

#------------------------------
# analizis de datos cruzados para variables categoricas
datos$year
hist(datos$year)
modelo = as.factor(datos$year)
table(modelo)

#tabla de contingencia 
table(origen, modelo)
addmargins(table(origen, modelo)) # frecuencias absolutas
round(100*addmargins(prop.table(table(origen, modelo))),1)  # frecuencia relativas porcentual


# grafico de barras apiladas
barplot(table(origen, modelo))

# analizis entre numerico y catecorico
#numerico = millas (mpg)
# categorico = origen

?describeBy
describeBy(millas, origen)      

?boxplot
boxplot(millas ~ origen)
