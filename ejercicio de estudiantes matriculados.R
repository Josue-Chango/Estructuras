###############################3
#PRACTICA DE VARIABLES ALEATORIAS

############################################
# v.a  DISCRETA

# EJERCICIOS DE CURSOS MATRICULADOS
x=1:7
fx= c(0.01, 0.03, 0.13, 0.25, 0.39, 0.17, 0.02)
sum(fx)
plot(x, fx, type = "h")  # MASA DE PROBABILIDAD

Fx= cumsum(fx) # FUNCION  DE DISTRIBOUCION ACUMUKADA
Fx

plot(x, Fx, type = "l")  #FUNCOIN DE DISTRBUCION ACUM

# CALCULO DE PROBABILIDAD
#PROBABILIDAD DE QUE UN ESTUDIANTE TOME 2 CURSOS

fx[2] # VALOR DE LA FUNCION EN LA SEGUNCDA POSICION

#PROBABILIDAD QUE TOME ENTRE 3 Y 5 CURSOS
fx[3] + fx[4] + fx[5] #FDP
fx[5] - fx[2] #FDA

#MEDIA O ESPERANZA
mu = sum(x*fx)
mu # VALOR MAS PROBABLE DE X

#  VARIANNZA
var.x = sum((x-mu)^2 * fx)  # POR DEFINICION DE LA VARIANZA
var.x

sum(x^2 *fx) - mu^2  # PROPIEDAD DE LA VARIANZA


# MEDIA Y VARIANZA DE UNA FUNCION DE UNA VARRIABLE ALEATORIA 
y=60*x+10  # COMO FUNCION DE X

sum(y*fx) # ESPERANZA DE UNA FUNCION

60*mu +10 # USANDO PROPIEDADES DE LA ESPERANZA MATEMATICA


#################################################
# VARIABLE  CONTINUA

#EJERCICIO DE VENTA DE GRAVA SEMANAL

#construir la funcion de densidad

y=function(x){
  y= (3/3)*(1-x^2)
  y[x<0 | x>1]= 0
  return (y)
}

curve (y, -1, 2)  #graficar funciones

# comprobar l apropiedad del area total = 1

fx=function(x){(3/2)*(1-x^2)}
integrate(fx, 0, 1)  #integral de fx entre 0 y 1
integrate(fx, 0, 1)$value


#probabilidad de vender enre 0,3 a 0,5 toneladas de grava a la semana
integrate(fx, 0.3,0.5)
integrate(fx, 0.3,0.5)$value

# media o esperanza matematica de x
fx.mu=function(x){(3/2)*(1-x^2)}
mu = integrate(fx.mu, 0, 1)$value
mu

#varianza de x
fx.2 =function(x){(x^2)(3/2)*(1-x^2)}
ex2 = integrate(fx2, 0, 1)$value
#sigma = 2 ex2 


#permutaciones

permutaciones <- function(n, r) {
  factorial <- function(x) {
    if (x == 0) return(1)
    return(prod(1:x))
  }
  
  return(factorial(n) / factorial(n - r))
}

# Ejemplo de uso
n <- 5  # Total de elementos
r <- 3  # Elementos a seleccionar
resultado <- permutaciones(n, r)
cat("El número de permutaciones de", n, "tomando", r, "es:", resultado)
