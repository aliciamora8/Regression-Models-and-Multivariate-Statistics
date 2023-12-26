datos=read.table("Greene.txt",skip=1)
names(datos)=c("Año","PNB","IN","IPC","TI")
datos_real=datos
names(datos_real)=c("Año","PNB","IN","IPC","TI")
datos_real$Año=1:15
datos_real$PNB=(datos$PNB/datos$IPC)*100
datos_real$IN=(datos$IN/datos$IPC)*100
datos_real$IPC[1]=(datos$IPC[1]/79.06-1)*100
datos_real$IPC[2:15]=(datos$IPC[2:15]/datos$IPC[1:14]-1)*100

modelo1=lm(IN~Año,datos_real)
summary(modelo1)
modelo2=lm(IN~Año+PNB+IPC+TI,datos_real)
summary(modelo2)

#Regresión particionada
r_IN=residuals(lm(IN~PNB+IPC+TI,datos_real))
r_Año=residuals(lm(Año~PNB+IPC+TI,datos_real))
coef(lm(r_IN~r_Año))

#Regresión sin término independiente
m0=lm(IN~Año-1,datos_real)
summary(m0)

#Representacion modelo1 vs m0
plot(IN~Año,datos_real,xlim=c(0,15),ylim=c(0,250))
abline(a=coef(modelo1)[1],b=coef(modelo1)[2],lty=1,col=4)
abline(a=0,b=coef(m0),lty=1,col=2)
points(datos_real)

#Regresión sin término independiente, con las variables centradas
Año_c=datos_real$Año-mean(datos_real$Año)
IN_c=datos_real$IN-mean(datos_real$IN)
mc=lm(IN_c~Año_c-1)
summary(mc)

plot(IN_c~Año_c,datos_real)
abline(a=0,b=coef(mc),lty=1,col=4)
points(datos_real)

#Coeficientes de correlación simple
cor(datos_real)
#Coeficiente de correlación múltiple de la Inversión sobre las otras variables
cor(datos_real$IN,fitted(modelo2))
#Coeficientes de correlación parcial entre la Inversión y una de las variables, dadas las otras tres
r_IN=residuals(lm(IN~PNB+IPC+TI,datos_real))
r_Año=residuals(lm(Año~PNB+IPC+TI,datos_real))
cor(r_IN,r_Año)







