install.packages("faraway")
library(faraway)
data(savings)
attach(savings)

#Ajuste del modelo
modelo=lm(sr~pop15+pop75+dpi+ddpi)
summary(modelo)

#Coeficientes del modelo ajustado
beta=coef(modelo)

#Matriz de diseño
X=model.matrix(modelo)
n=nrow(X)
p=ncol(X)
n;p

#Otra forma de calcular los estimadores de los coeficientes
Y=sr
#Matriz hat
#Primero calculamos (X'X)^(-1)
XtXi=solve(t(X)%*%X) #la funcion solve calcula la inversa
H=X%*%XtXi%*%t(X)
hcoef=XtXi%*%t(X)%*%Y
hcoef

#Estimador de la varianza
#Matriz generadora de residuos
M=diag(1,nrow=nrow(H))-H
#Suma residual de cuadrados
RSS=t(Y)%*%M%*%Y #Tambien se puede calcular como deviance(modelo)
#Estimador
Sigmastd=RSS/(n-p)

#Para calcular los intervalos de confianza debemos obtener previamente los errores típicos de los estimadores,
#como la raíz cuadrada de la varianza por la diagonal de (X'X)^-1. Después obtendremos
#los cuantiles de la distribución t, para un nivel de confianza fijado (en este ejemplo,
#1-alfa= 0'95) y finalmente calcularemos los extremos superiores e inferiores 
#de los intervalos.
ET=sqrt(Sigmastd*diag(XtXi))
niv=0.95
t=qt(1-(1-niv)/2,n-p)
#Extremos inferiores de los intervalos de confianza para los coeficientes
betainf=beta-t*ET
#Extremos superiores de los intervalos de confianza para los coeficientes
betasup=beta+t*ET
#Tambien se puede obtener con
confint(modelo,level=niv)

#Matriz de covarianzas entre los estimadores de los coeficientes
covbeta=as.numeric(Sigmastd)*XtXi

#Las regiones de confianza cuando consideramos dos parámetros (p = 2)
#tienen forma de elipse. Podemos representar esta región a partir del modelo ajustado, y también
#comprobar se se puede aceptar la hipótesis de que ambos coeficientes valen cero. Podemos observar
#en la Figura 4 que el (0; 0) cae fuera de la región, por lo que no podríamos aceptar la hipótesis
#de que ambos coeficientes son nulos.

library(ellipse)
plot(ellipse(modelo,c(2,3)),type="l",xlim=c(-1,0))
points(beta[2],beta[3],pch=18)
points(0,0)

#Test F
modelo1=lm(sr~pop15+pop75+dpi,savings)
anova(modelo1,modelo)
#Se puede calcular tambien así
modelo2=lm(sr~pop15+ddpi,savings)
rss0=deviance(modelo2)
rss=deviance(modelo)
f=((rss0-rss)/2)/(rss/(n-p))
pvalue=1-pf(f,2,n-p) #y da el mismo resultado que calculando anova(modelo2, modelo)

#Predicciones de la tasa de ahorro considerando los valores observados en la muestra
predict(modelo)
#Supongamos que disponemos de la información sobre otro país donde el 30% de la población es
#menor de 15 años, el 2% es mayor de 75, la renta per cápita es de 1000 (unidades monetarias)
#y la tasa de crecimiento de la renta es 5. Con estos datos, podemos obtener la predicción con el
#modelo ajustado.
predict(modelo,data.frame(pop15=30,pop75=2,dpi=1000,ddpi=5))
#Predicciones para varios individuos
predict(modelo,data.frame(pop15=c(30,40),pop75=c(2,1.5),dpi=c(1000,500),ddpi=c(5,4)),interval="prediction")



