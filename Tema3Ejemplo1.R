library(faraway)
data(savings)
attach(savings)

#Ajuste lineal
g1=lm(sr~ddpi)
summary(g1)

#Ajuste polinómico de grado 2
ddpi2=ddpi*ddpi
g2=lm(sr~ddpi+ddpi2)
summary(g2)

#Ajuste polinomico de grado 3
ddpi3=ddpi2*ddpi
g3=lm(sr~ddpi+ddpi2+ddpi3)
summary(g3) #Vemos que los coef dejan de ser significativos