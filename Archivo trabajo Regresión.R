#abrimos los datos:
library(readr)
storms <- read_csv("Mahaigaina/storms", col_types = cols(...1 = col_skip()))
View(storms)
attach(storms)

#vamos a analizar mas detenidamente los NA
indT <-is.na(tropicalstorm_force_diameter)
sum(indT)
indH <- is.na(hurricane_force_diameter)
sum(indH)
sum(indT*indH)

#vamos a quitar estos datos y modificar la base de datos
storms_new <- storms[!indT,]
dim(storms_new)
storms_new$tropicalstorm_force_diameter <- storms_new$tropicalstorm_force_diameter + 0.01
storms_new$hurricane_force_diameter <- storms_new$hurricane_force_diameter + 0.01
storms_new$lat <- storms_new$lat +90.05
storms_new$long <-storms_new$long +180.1
storms_new$hour <- storms_new$hour +0.01
attach(storms_new)
summary(storms_new)

######################
#1.RLM:
modelom<- lm(pressure~hour+lat+long+wind+tropicalstorm_force_diameter+hurricane_force_diameter, storms_new)
summary(modelom)


beta <- coef(modelom) #la matriz de coeficientes
beta
X <- model.matrix(modelom) #matriz de diseño y a continuacion veremos su tamaño nxp
n <-nrow(X)
p<-ncol(X)
M<-diag(1,nrow=nrow(H))-H #matriz generadora de residuos
RSS <- deviance(modelom) #suma residual de cuadrados
RSS
sigma2 <- RSS/(n-p) #varianza residual
sigma2
covbeta <- as.numeric(sigma2)*XtXi
round(covbeta,3)

# vamos a calcular los intervalos de confianza
XtXi <- solve(t(X)%*%X)
H<-X%*%XtXi%*%t(X) #matriz hat
ET <- sqrt(sigma2*diag(XtXi)) #error tipico de los estimadores de los coeficientes
ET

alfa <- 0.05
niv <- 1-alfa 
t = qt(1-alfa/2,n-p) #calculo del cuantil de la t student

betainf<-beta-t*ET #extremos inferiores
betainf
betasup<-beta+t*ET #extremos superiores
betasup
confint(modelom,level=0.95)

#analisis de colinealidad
modelom.co<- lm(log(pressure)~log(hour)+log(lat)+log(long)+log(wind)+log(tropicalstorm_force_diameter)+ log(hurricane_force_diameter),storms_new)
summary(modelom.co)

#matriz de covarianza de las variables explicativas
x<- cbind(log(storms_new$hour),log(storms_new$lat),log(storms_new$long), log(storms_new$wind),  log(storms_new$tropicalstorm_force_diameter), log(storms_new$hurricane_force_diameter))
colnames(x)=c("hour","lat","long","wind","ts_force_diameter","h_force_diameter") 
round(cor(x),3)

#selección de variables global
step(modelom.co)
modelo_simplificado <- step(modelom.co)
summary(modelo_simplificado)

#analisis de colinealidad
VIF=c()
VIF["loglong"] <- 1 / ( 1 - cor( log(long),fitted(lm(log(long)~log(hour)+log(lat)+log(wind)+log(tropicalstorm_force_diameter)+log(hurricane_force_diameter) ) )^2 ))
VIF["loglat"] <- 1 /( 1 - cor( log(lat),fitted(lm(log(lat)~log(hour)+log(long)+log(wind)+log(tropicalstorm_force_diameter)+log(hurricane_force_diameter) ) )^2 ))
VIF["logwind"] <- 1 /( 1 - cor( log(wind),fitted(lm(log(wind)~log(hour)+log(lat)+log(long)+log(tropicalstorm_force_diameter)+log(hurricane_force_diameter) ) )^2 ))
VIF["logtropicaldiam"] <- 1 /( 1 - cor( log(tropicalstorm_force_diameter),fitted(lm(log(tropicalstorm_force_diameter)~log(hour)+log(lat)+log(wind)+log(long)+log(hurricane_force_diameter) ) )^2 ))
VIF["loghurricanediam"] <- 1 /( 1 - cor( log(hurricane_force_diameter),fitted(lm(log(hurricane_force_diameter)~log(hour)+log(lat)+log(wind)+log(tropicalstorm_force_diameter)+log(long) ) )^2 ))
VIF

#otro criterio de ajuste global
summary(lm(log(pressure)~log(lat)+log(wind)+log(tropicalstorm_force_diameter)+log(hurricane_force_diameter),storms_new))
summary(lm(log(pressure)~log(long)+log(lat)+log(tropicalstorm_force_diameter)+log(hurricane_force_diameter),storms_new))
summary(lm(log(pressure)~log(long)+log(wind)+log(lat)+log(hurricane_force_diameter),storms_new))
summary(lm(log(pressure)~log(long)+log(wind)+log(tropicalstorm_force_diameter)+log(lat),storms_new))
summary(lm(log(pressure)~log(long)+log(wind)+log(tropicalstorm_force_diameter)+log(hurricane_force_diameter),storms_new))

#validacion y diagnosis

#grafica
par(mfrow=c(2,2))
plot(modelo_simplificado,pch=20,cex=0.5)

#distancia de cooks
plot(cooks.distance(modelo_simplificado),pch=20)
savePlot("DEskargak/cook")

#residuos:
res1 <- residuals(modelo_simplificado) # Residuos "brutos"
res2 <- rstandard(modelo_simplificado) # Residuos estandarizados
res3 <- rstudent(modelo_simplificado) #residuos estudentizados

hist(res2)
hist(res3)
q1<-quantile(res2,.95)
q2<-quantile(res3,.95)
atip1<-which(abs(res2) >q1 ) 
atip2<-which(abs(res3) > q2)
#los puntos atipicos son los atip1 y atip2 que resultan ser los mismos.

#eliminamos los puntos atipicos y creamos el modelo nuevo
storms_new2 <- storms_new[-atip1,]
modelo3 <- lm(log(pressure)~log(lat)+log(wind)+log(long)+log(tropicalstorm_force_diameter)+log(hurricane_force_diameter),storms_new2)
summary(modelo3)

#homocedasticidad
bptest(modelo3) 

#normalidad
shapiro.test(rstandard(modelo3))
shapiro.test(rstudent(modelo3))

#linealidad
resettest(modelo3)


############################
#2.ANOVA
storms_new$status<-factor(storms_new$status)
datosanova<-storms_new[,c(8,11)]
View(datosanova)
attach(datosanova)

#grafico
windows()
plot(pressure~status,pch=16)
summary(datosanova$status)

#parametrizacion por media locales:
mu_local <- c()
mu_local[1] <- mean(pressure[status=="hurricane"])
mu_local[2] <- mean(pressure[status=="tropical depression"])
mu_local[3] <- mean(pressure[status=="tropical storm"])
mu_local
modelo_local <- lm(pressure~status-1)
summary(modelo_local)

#parametrizacion respecto a una media global
mu_global <- mean(pressure)
alpha <- mu_local - mu_global
mu_global
alpha

#parametrizacion respecto a un grupo de referencia
modelo_ref <- lm(pressure~status) #respecto al primer grupo
summary(modelo_ref)

#respecto al grupo tropical depression
levels(status) 
status2 <- relevel(status,ref="tropical depression")
modelo_ref2 <- lm(pressure~status2)
summary(modelo_ref2)

#igualdad de medias de los grupos
anova(lm(pressure ~ status))

#Tukey
TukeyHSD(aov(pressure ~ status)) 

#Bonferroni
n=nrow(datosanova)
I=length(levels(status))
ni=table(status)
sd=sqrt(deviance(modeloan)/(n-I))
ncomp=I*(I-1)/2
ct=qt(1-0.05/(2*ncomp),n-I)
Bonferroni=data.frame(diff=rep(0,3),lwr=rep(0,3),upr=rep(0,3))
rownames(Bonferroni)=c("tropical depression-hurricane","tropical storm-hurricane","tropical storm-tropical depression")
Bonferroni$diff[1]=mu_local[2]-mu_local[1]
Bonferroni$lwr[1]=mu_local[2]-mu_local[1]-ct*sd*sqrt(1/ni[2]+1/ni[1])
Bonferroni$upr[1]=mu_local[2]-mu_local[1]+ct*sd*sqrt(1/ni[2]+1/ni[1])
Bonferroni$diff[2]=mu_local[3]-mu_local[1]
Bonferroni$lwr[2]=mu_local[3]-mu_local[1]-ct*sd*sqrt(1/ni[3]+1/ni[1])
Bonferroni$upr[2]=mu_local[3]-mu_local[1]+ct*sd*sqrt(1/ni[3]+1/ni[1])
Bonferroni$diff[3]=mu_local[3]-mu_local[2]
Bonferroni$lwr[3]=mu_local[3]-mu_local[2]-ct*sd*sqrt(1/ni[3]+1/ni[2])
Bonferroni$upr[3]=mu_local[3]-mu_local[2]+ct*sd*sqrt(1/ni[3]+1/ni[2])
Bonferroni

#Validacion y diagnosis

#normalidad
O1 <- pressure[status=="hurricane"]
O2 <- pressure[status=="tropical depression"]
O3 <- pressure[status=="tropical storm"]
shapiro.test(O1)
shapiro.test(O2)
shapiro.test(O3)

#igualdad de varianzas
abs_res <- abs(residuals(modeloan))
levene=lm(abs_res~status)
anova(levene)

##################################
#3.MODELO ANCOVA

status <- factor(status)

#Dibujamos puntos hurricane/tropicaldepression/tropicalstorm
ia <- status=="hurricane"
ib <- status=="tropical depression"
ic <- status=="tropical storm"
plot(pressure~wind)
points(pressure[ia]~wind[ia],col=2,pch=1) 
points(pressure[ib]~wind[ib],col=4,pch=1)
points(pressure[ic]~wind[ic],col=3,pch=1)

#AJUSTAMOS MODELO ANCOVA (SIN INTERACCION)
modelo.ancova <- lm(pressure~status+wind,storms_new)
summary(modelo.ancova)

#Representación gráfica
beta.hat <- coef(modelo.ancova) #Matriz de coeficientes
abline(a=beta.hat[1],b=beta.hat[4],col=2,lwd=2) #Recta "hurricane"
abline(a=beta.hat[1]+beta.hat[2],b=beta.hat[4],col=4,lwd=2) #Recta "Tropical depression"
abline(a=beta.hat[1]+beta.hat[3],b=beta.hat[4],col=3,lwd=2) #Recta "Tropical storm"

#Contraste del efecto de la variable continua
mod_cont <- lm(pressure~status,storms_new)
anova(mod_cont,modelo.ancova)


#AJUSTAMOS MODELO ANCOVA (CON INTERACCION)
modelo.ancova.int <- lm(pressure~status*wind,storms_new)
summary(modelo.ancova.int)

#Representacion grafica
plot(pressure~wind)
points(pressure[ia]~wind[ia],col=2,pch=1) 
points(pressure[ib]~wind[ib],col=4,pch=1)
points(pressure[ic]~wind[ic],col=3,pch=1)
beta.hat.int <- coef(modelo.ancova.int)
#Recta "hurricane"
abline(a=beta.hat.int[1],b=beta.hat.int[4],col=2,lwd=2)
#Recta "Tropical depression"
abline(a=beta.hat.int[1]+beta.hat.int[2],b=beta.hat.int[4]+beta.hat.int[5],col=4,lwd=2)
#Recta "Tropical storm"
abline(a=beta.hat.int[1]+beta.hat.int[3],b=beta.hat.int[4]+beta.hat.int[6],col=3,lwd=2)

#Contraste del modelo con y sin interacción
anova(modelo.ancova,modelo.ancova.int)


