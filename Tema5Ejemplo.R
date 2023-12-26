library(MASS)
data(whiteside)
attach(whiteside)

Insul=factor(Insul)
plot(Temp,Gas,type="n")
ib=(Insul=="Before")
ia=(Insul=="After")
points(Temp[ib],Gas[ib], col="red")
points(Temp[ia],Gas[ia], col="blue")

#Modelo sin interacción
modsi=lm(Gas~Insul+Temp)
summary(modsi)
beta=coef(modsi)
abline(a=beta["(Intercept)"],b=beta["Temp"],col="red")
abline(a=beta["(Intercept)"]+beta["InsulAfter"],b=beta["Temp"],col="blue")

#Contraste de significacion de la variable continua
mod0=lm(Gas~Insul)
anova(mod0,modsi)
#El valor elevado de F quiere decir que la Temp es muy significativa en el modelo

#Conraste de significación de la variable discreta
mod1=lm(Gas~Temp)
anova(mod1,modsi)

#Modelo con interaccion
modci=lm(Gas~Insul+Temp+Temp:Insul)
summary(modci)
windows()
plot(Temp,Gas,type="n",xlab="Temperature",ylab="Gas consumption")
ib=(Insul=="Before")
ia=(Insul=="After")
points(Temp[ib],Gas[ib])
points(Temp[ia],Gas[ia],pch=18)
beta=coef(modci)
abline(a=beta["(Intercept)"],b=beta["Temp"],col="pink",lwd=2)
abline(a=beta["(Intercept)"]+beta["InsulAfter"],
b=beta["Temp"]+beta["InsulAfter:Temp"],col="pink",lwd=2)

#Contraste de la interacción
anova(modsi,modci)