datos=read.table("datos1.txt",header=T)
set.seed(7256)
ind<-sample(1:768, size=150)
datos<-datos[ind,]
attach(datos)
summary(datos)

#Apartado 1
mod_logit=glm(test~age,family=binomial(link="logit"))
summary(mod_logit)
exp(coef(mod_logit))

#Apartado 2
windows()
plot(test~age,main="Ajuste logístico")
beta=coef(mod_logit)
curve(exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x)),add=TRUE,col="red")

#Apartado 3
lin1=beta[1]+beta[2]*30
exp(lin1)/(1+exp(lin1))

lin2=beta[1]+beta[2]*40
exp(lin2)/(1+exp(lin2))

#Apartado 4
mod_logit2=glm(test~age+bmi,family=binomial(link="logit"))
summary(mod_logit2)
peso=60
altura=1.62
bmi_persona=peso/(altura)^2
beta2=coef(mod_logit2)
lin3=beta2[1]+beta2[2]*30+beta2[3]*bmi_persona
exp(lin3)/(1+exp(lin3))