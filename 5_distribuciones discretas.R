####################
#PRACTICA DE DISTRIBUCIONES DISCRETAS 
########################################

###############################
# V.A. Binomial 
?choose #Combinaciones

fx = function (x,n,p){choose(n,x)*p^x*(1-p)^(n-x)}
#a. Probabilidad de que 2 pc de 3 en total no tengan virus, p = 0.70

fx(2,3,0.7)
#b. probabilidad que maximo 1 no tenga virus
fx(0,3,0.7)+fx(1,3,0.7)

### Funciones basicas, automaticas 

?dbinom

#a. probabilidad de que 2 PC de 3 en total no tengan virus 
dbinom(2,3,0.7)
#b. probabilidad de que 2 PC de 3 en total no tengan virus 
dbinom(0,3,0.7) + dbinom(1,3,0.7)
pbinom(1,3,0.7)


#c Media = n*p

mu = 3*0.7
mu

##########Grafico de f.d.p. binomial 
plot(0:3, dbinom(0:3,3,0.7),type = "h")
abline(v = mu, col = 2)




