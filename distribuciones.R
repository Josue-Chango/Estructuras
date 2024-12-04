######### DISTRIBUCIONES DISCRETAS

#practica de distribuenids

#variable binomial
choose  # funcion para hacer combinaciones
fx = function(x,n,p) {choose9n.p}*p^x*(1-p)^(n-x)

fx(2,3,0,7);

#probabilidad que maaximo 1 no tenga virus
fx(0,3,0,7) + fx(1, 3, 0, 7)


#dbinom
#probabilidad de que 2 pcs de 3 3n total no tnega g¨=0,70
dbinomial(0,3,0, 7) + dbinomial(1,3,0, 7);
pbinomial(1,3,0, 7);

polinom (1,3,0,7)

#c. media
mu=3*0.7;
mu;


#grafico de f.d.p. binomaial
plot(0:3, dbinomio(0:3,3,0,7), type = "h")
abline(v = mu, col=2)

