datos=read.table("bridge.txt",header=T)
attach(datos)
mod=lm(log(Time)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))
summary(mod)

plot(log(Time)~fitted(mod),xlab="Valores ajustados")
abline(lm(log(Time)~fitted(mod)), col=2)
#Usamos fitted para representar los valores ajustados
#porque no podemos graficar el modelo múltiple

#Representacion de los residuos estandarizados vs cada variable y los ajustados
windows()
par(mfrow=c(2,3))
res_est=rstandard(mod)
plot(res_est~log(DArea),ylab="Residuos estandarizados")
plot(res_est~log(CCost),ylab="Residuos estandarizados")
plot(res_est~log(Dwgs),ylab="Residuos estandarizados")
plot(res_est~log(Length),ylab="Residuos estandarizados")
plot(res_est~log(Spans),ylab="Residuos estandarizados")
plot(res_est~fitted(mod),xlab="Valores ajustados",ylab="Residuos estandarizados")
par(mfrow=c(1,1))
#Como hay patrón aleatorio, se ajustan bien al modelo que tenemos
#Para ver las observaciones atípicas e influyentes
plot(mod)


#Correlación entre las variables
x=cbind(log(DArea),log(CCost),log(Dwgs),log(Length),log(Spans)) 
#Comando cbind se usa para formar una matriz con esos vectores y que quede en forma de tabla
colnames(x)=c("logDArea","logCCost","logDwgs","logLength","logSpans")
cor(x) #Matriz de correlación
round(cor(x),3)

#Factores de inflación de la varianza
VIF=c()
VIF["logDArea"]=1 / ( 1 - cor( log(DArea),
fitted(lm(log(DArea)~log(CCost)+log(Dwgs)+log(Length)+log(Spans)) ) )^2 )
VIF["logCCost"]=1 / ( 1 - cor( log(CCost),
fitted(lm(log(CCost)~log(DArea)+log(Dwgs)+log(Length)+log(Spans)) ) )^2 )
VIF["logDwgs"]=1 / ( 1 - cor( log(Dwgs),
fitted(lm(log(Dwgs)~log(DArea)+log(CCost)+log(Length)+log(Spans)) ) )^2 )
VIF["logLength"]=1 / ( 1 - cor( log(Length),
fitted(lm(log(Length)~log(DArea)+log(CCost)+log(Dwgs)+log(Spans)) ) )^2 )
VIF["logSpans"]=1 / ( 1 - cor( log(Spans),
fitted(lm(log(Spans)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)) ) )^2 )
VIF
#Se consideran preocupantes valores superiores a 5


#Selección de variables
modr=step(mod)
summary(modr)