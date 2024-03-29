#--------------------------#
#LECTURA DE DATOS
#--------------------------#
library(MASS)
data(whiteside)
attach(whiteside)
View(whiteside)


#--------------------------------#
# Como infl�e o illamento t�rmico
# sobre o consumo de Gas
# Modelo ANOVA
#--------------------------------#

summary(Gas)
windows()
par(mfrow=c(1,2))
hist(Gas); boxplot(Gas)

summary(Insul)

windows()
plot(Gas~Insul)

#Axustamos modelo ANOVA
#Medias locais
modelo.anova.0 <- lm(Gas~Insul-1)
summary(modelo.anova.0)

#Grupo de referencia
modelo.anova <- lm(Gas~Insul)
summary(modelo.anova)


#Axustamos modelo RLS
windows()
plot(Gas~Temp,pch=16)
cor(Gas,Temp)

modelo.RLS <- lm(Gas~Temp)
summary(modelo.RLS)
abline(modelo.RLS,lwd=2)


#Debuxar puntos After/Before
ia <- Insul=="After" #casas "After" illamento
ib <- !ia #casas "Before" illamento

points(Gas[ia]~Temp[ia],col=2,pch=16) #puntos despois de illamento
points(Gas[ib]~Temp[ib],col=4,pch=16) #puntos antes de illamento


#AXUSTAMOS MODELO ANCOVA (SEN INTERACCI�N)
modelo.ancova <- lm (Gas~Insul+Temp)
summary(modelo.ancova)

beta.hat <- coef(modelo.ancova)
#Recta "Before"
abline(a=beta.hat[1],b=beta.hat[3],col=4,lwd=2)
#Recta "After"
abline(a=beta.hat[1]+beta.hat[2],b=beta.hat[3],col=2,lwd=2)

#AXUSTAMOS ANCOVA CON INTERACCI�N
modelo.ancova.int <- lm(Gas~Insul*Temp)
summary(modelo.ancova.int)

#DEBUXAR
#comprobaci�n con RLS
windows()
plot(Gas~Temp,pch=16)
points(Gas[ia]~Temp[ia],col=2,pch=16) #puntos despois de illamento
points(Gas[ib]~Temp[ib],col=4,pch=16) #puntos antes de illamento


abline(lm(Gas[ia]~Temp[ia]),col=2,lwd=2) #RLS
abline(lm(Gas[ib]~Temp[ib]),col=4,lwd=2) #RLS

#Pintamos por enriba as rectas axustadas co ANCOVA
beta.hat <- coef(modelo.ancova.int)
beta.hat
abline(a=beta.hat[1],b=beta.hat[3],lwd=2) #pinta de negro
abline(a=beta.hat[1]+beta.hat[2],b=beta.hat[3]+beta.hat[4],lwd=2) #pinta de negro

#Contrastes dos efectos das variables

#Efecto da variable categ�rica
#H0: RLS vs. Ha: ANCOVA


#Efecto da variable continua
#H0: ANOVA vs. Ha: ANCOVA


#Efecto interacci�n
#H0: ANCOVA vs Ha: ANCOVA con interacci�n

