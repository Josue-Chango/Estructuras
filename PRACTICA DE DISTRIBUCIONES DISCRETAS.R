###############################################
#----PRACTICA DE DISTRIBUCIONES DISCRETAS -----
###############################################

#------------------
# V.A. BINOMIAL
#------------------
?choose

fx = function(x,n,p) {choose(n,x)*p^x*(1-p)^(n-x)}

# a. probab de que 2pc de 3 en total no tenga virus, p = 0.70
fx(2,3,0.7)

# b. probab de que maximo 1 tenga virus
fx(0,3,0.7) + fx(1,3,0.7)

#   #   #   #   #   #   #   #   #   #   #   #   #   #   
#   FUNCIONES BASICAS OARA VARIABLES ALEATORIAS
#   PREFIJOS (masa,densidad), p(acumulada), q(cuantil), r(random)
#     NOMBRE DE LA DISTRIB: Binomial = "dbinom"

?dbinom

# a. probab de que 2pc de 3 en total no tenga virus, p = 0.70
dbinom(2,3,0.7)

# b. probab de que maximo 1 tenga virus
dbinom(0,3,0.7) + dbinom(1,3,0.7)
pbinom(1,3,0.7) # solo sirve si se acumula desde cero a x

# c. Media = n*p
mu= 3*0.7
mu


# Grafico de  f.d.p binomial

x = 0:3
fx = dbinom(0:3,3,0.7)
plot(x, fx, type="h")



#------------------
# HIPER GEOMETRICA
#------------------
?dhyper # f.d.p v.a. hipergeometrica

# hyper(X,N-k,k,n)

# Ejercicio en clase
N = 15 # 15 compresosres en total
k = 2 # 2 compresores defectuosos en total
n = 5 # compresores en la muestra

# literal a.) 1 sea deferctusoso
X= 1 # cuantos son defectuosos

# Con la funcion de la hyper geometrica
dhyper(X, k, N-k,n)

# De forma manual usando combinatoriaas
choose(2,1)*choose(13,4)/choose(15,5)


# literal b.) 2 sea deferctusoso
X= 2 # cuantos son defectuosos
dhyper(X, k, N-k,n)

# Calculo de la media
mu = n*(k/N)
mu

# Calculo de la Varianza
var = ((N-n)/(N-1))*n*(k/N)*(1-(k/N))
var

#------------------
# POISSON
#------------------
?dpois # f.d.p de la v.a. de POISSON 

lambda = 5 # 5 pacientes en una hora 
# Literal 1. p (llegan mad de 10 pacientes en una hora )
p0 = dpois(0, lambda)
p1 = dpois(1, lambda)
p2 = dpois(2, lambda)
p3 = dpois(3, lambda)
p4 = dpois(4, lambda)
p5 = dpois(5, lambda)
p6 = dpois(6, lambda)
p7 = dpois(7, lambda)
p8 = dpois(8, lambda)
p9 = dpois(9, lambda)
p10 = dpois(10, lambda)
1 - (p0+p1+p2+p3+p4+p5+p6+p7+p8+p9+p10)


# literal a.)
1 - ppois(10,5)

# Graficos
plot(0:12, dpois(0:12,5), type="h") # f.d.p
plot(0:12, ppois(0:12,5) ,type="l") # f.a.p

# literal b.)
1 - ppois(20,15)

# Graficos
plot(0:12, dpois(0:12,15), type="h") # f.d.p
plot(0:12, ppois(0:12,15) ,type="l") # f.a.p


#---------------------------
# V.A. uniforme
#---------------------------

?dunif
?punif #acumulada

#Ejercicio en clases: A=0, B=7, f(x) = 1/10
dunif(7,0,10)

# acumulada
punif(7,0,10) # F(7) = P(X <=7)
1-punif(7,0,10)

#P(2<= X <= 7) = F(7)-F(2)
punif(7,0,10)-punif(2,0,10)


#---------------------------
# V.A. NORMAL
#---------------------------

fx = function(x,mu,sigma) 
  {(1/sqrt(2*pi*sigma^2))*exp((-(x-mu)^2)/(2*sigma^2))}

# campana normal curva de gaus
curve(fx(x,5,2),-5,15) #mu = 5, sigma = 2

#es simeetrica respecto a la media
abline(v=5, col=2 ) # tien un maximo en la media


# puntos de inflexion = media-1*sigma; media+1*sigma

abline(v = c(5-(2*1),5+(2*1)), col=3 ) # tres desviaciones de la media

abline(v = c(5-(2*2),5+(2*2)), col=4 ) # dos desviaciones de la media

abline(v = c(5-(2*3),5+(2*3)), col=5 ) # tres desviaciones de la media

# -----------------------------------------------
# EFECTO DE LA MEDIA - mu
# curva normal de x1 con media = 5, sigma = 2
curve(fx(x,5,2),-5,15)

# curva normal de x1 con media = 8, sigma = 2
curve(fx(x,8,2),-5,15, add = TRUE, col=2)
# mu representa un efecto de desplazamiento

# ------------------------------------------------
# EFECTO DE LA DESVIACION ESTANDAR - SIGMA
# curva normal de x1 con media = 5, sigma = 2
curve(fx(x,5,2),-5,15)

# curva normal de x1 con media = 8, sigma = 2
curve(fx(x,5,4),-5,15, add = TRUE, col=2)
# mu representa un efecto de desplazamiento

# ------------------------------------------------
# DISTRIBUCION NORMAL ESTANDAR (z)
# ------------------------------------------------

# v.a. Normal con media = cero , sigma =1

# curva normal z
curve(fx(x,0,2),-5,5) # curva Z


# x v.a. normal con su mu=5, sigma = 2, P(1 < x < 8)
fx = function(x) 
  {(1/sqrt(2*pi*2^2))*exp(-((x-5)^2)/(2*2^2))}
integrate(fx,1,8)

# usando la v.a.
za = (1-5)/2
zb = (8-5)/2

fz = function(z) {(1/sqrt(2*pi)*exp(-(z^2)/2))}
integrate(fz,za,zb)

# -----------------------------------------
?dnorm
?pnorm

# grafica de la funcion
curve(dnorm(x,5,2),0,10) # x con mu=5, sigma=2

# Usando f(x)
# P(1 < X < 8) = F(8) - F(1)
pnorm(8,5,2) - pnorm(1,5,2)

# Usando f(z)
# P(1 < X < 8) = F(8) - F(1)
pnorm(zb,0,1) - pnorm(za,0,1)
pnorm(zb) - pnorm(za) # si no damos ni mu ni sigma, la funcion asume la estandarizacion


