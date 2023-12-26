datos=read.table("oxigeno.txt",header=T)
attach(datos)

#Como tenemos una variable discreta
Lugar=factor(Lugar)

#Medias locales
mu_local=c()
mu_local[1]=mean(Oxigeno[Lugar=="1"])
mu_local[2]=mean(Oxigeno[Lugar=="2"])
mu_local[3]=mean(Oxigeno[Lugar=="3"])
mu_local[4]=mean(Oxigeno[Lugar=="4"])
mu_local

#Modelo
lm(Oxigeno~Lugar-1)

#Media global y desviaciones
mu_global=mean(Oxigeno)
alfa=mu_local-mu_global
mu_global
alfa

#Grupo de referencia y desviaciones
#tomando el primer lugar como grupo de referencia
mod=lm(Oxigeno~Lugar)
summary(mod)
#el intercepto es la media del Lugar 1, mientras los demás coeficientes son 
#las desviaciones de los otros lugares respecto del Lugar 1.
#El nivel crítico del intercepto corresponde al contraste de que la media del Lugar 1 vale
#cero, mientras que el nivel crítico del coeficiente de desviación del Lugar 2 (por ejemplo)
#se reduce al contraste de que la media de este lugar es igual a la del Lugar 1.

#Cambiar grupo de referencia
Lugar_2ref=relevel(Lugar,ref="2")
mod_2ref=lm(Oxigeno~Lugar_2ref)
summary(mod_2ref)

#F-test
anova(mod)
#Como es un F valor grande, se rechaza la hip. de que las medias son iguales

#Método de Bonferroni para comparaciones múltiples
n=nrow(datos)
I=length(levels(Lugar))
ni=table(Lugar)
sd=sqrt(deviance(mod)/(n-I))
ncomp=I*(I-1)/2
ct=qt(1-0.05/(2*ncomp),n-I)
Bonferroni=data.frame(diff=rep(0,6),lwr=rep(0,6),upr=rep(0,6))
rownames(Bonferroni)=c("2-1","3-1","4-1","3-2","4-2","4-3")
Bonferroni$diff[1]=mu_local[2]-mu_local[1]
Bonferroni$lwr[1]=mu_local[2]-mu_local[1]-ct*sd*sqrt(1/ni[2]+1/ni[1])
Bonferroni$upr[1]=mu_local[2]-mu_local[1]+ct*sd*sqrt(1/ni[2]+1/ni[1])
Bonferroni$diff[2]=mu_local[3]-mu_local[1]
Bonferroni$lwr[2]=mu_local[3]-mu_local[1]-ct*sd*sqrt(1/ni[3]+1/ni[1])
Bonferroni$upr[2]=mu_local[3]-mu_local[1]+ct*sd*sqrt(1/ni[3]+1/ni[1])
Bonferroni$diff[3]=mu_local[4]-mu_local[1]
Bonferroni$lwr[3]=mu_local[4]-mu_local[1]-ct*sd*sqrt(1/ni[4]+1/ni[1])
Bonferroni$upr[3]=mu_local[4]-mu_local[1]+ct*sd*sqrt(1/ni[4]+1/ni[1])
Bonferroni$diff[4]=mu_local[3]-mu_local[2]
Bonferroni$lwr[4]=mu_local[3]-mu_local[2]-ct*sd*sqrt(1/ni[3]+1/ni[2])
Bonferroni$upr[4]=mu_local[3]-mu_local[2]+ct*sd*sqrt(1/ni[3]+1/ni[2])
Bonferroni$diff[5]=mu_local[4]-mu_local[2]
Bonferroni$lwr[5]=mu_local[4]-mu_local[2]-ct*sd*sqrt(1/ni[4]+1/ni[2])
Bonferroni$upr[5]=mu_local[4]-mu_local[2]+ct*sd*sqrt(1/ni[4]+1/ni[2])
Bonferroni$diff[6]=mu_local[4]-mu_local[3]
Bonferroni$lwr[6]=mu_local[4]-mu_local[3]-ct*sd*sqrt(1/ni[4]+1/ni[3])
Bonferroni$upr[6]=mu_local[4]-mu_local[3]+ct*sd*sqrt(1/ni[4]+1/ni[3])
Bonferroni
#la columna titulada diff contiene las diferencias, la columna lwr los extremos
#inferiores de los intervalos de confianza y la columna upr los extremos superiores
#Si los dos
extremos del intervalo tienen el mismo signo, entonces las dos medias afectadas 
#son significativamente distintas. En este caso, las únicas medias que podrían 
#ser iguales son las de los lugares 1 y 4.

#Metodo Tukey
TukeyHSD(aov(Oxigeno~Lugar))

#Validación del modelo
#Contraste de igualdad de varianzas
#Test de Levene
abs_res=abs(residuals(mod))
levene=lm(abs_res~Lugar)
anova(levene)
#El nivel crítico resultante es de 0’4769, lo cual indica que no hay pruebas significativas contra la suposición
#de homocedasticidad entre los cuatro lugares del río.
