set.seed(123456)
# Generación de los datos
n=20
x=1:n
y=1+0.5*x+rnorm(n)

# Representación de los datos originales
mod=lm(y~x)
plot(y~x,xlab="",ylab="",asp=1)
abline(mod,col="blue")
points(mean(x),mean(y),col="blue",pch="X")
summary(mod)


windows()
par(mfrow=c(2,2))
xmarco=c(1,30)
ymarco=c(0,16)

# Con un dato más en el centro
x0=mean(x)
y0p=predict(mod,data.frame(x=c(x0)))
names(y0p)=c()
y0=y0p
x_ampliado=c(x,x0)
y_ampliado=c(y,y0)
mod_a1=lm(y_ampliado~x_ampliado)
plot(xmarco,ymarco,type="n",xlab="(a1)",ylab="",asp=1)
points(x,y)
points(mean(x_ampliado),mean(y_ampliado),col="blue",pch="X")
abline(mod,col="blue")
points(x0,y0,col="red",pch=19)
abline(mod_a1,col="red")
summary(mod_a1)
# Con un dato más, sin capacidad de palanca y fuera de la recta
y0=y0p-8
x_ampliado=c(x,x0)
y_ampliado=c(y,y0)
mod_a2=lm(y_ampliado~x_ampliado)
plot(xmarco,ymarco,type="n",xlab="(a2)",ylab="",asp=1)
points(x,y)
points(mean(x_ampliado),mean(y_ampliado),col="red",pch="X")
abline(mod,col="blue")
points(x0,y0,col="red",pch=19)
abline(mod_a2,col="red")
summary(mod_a2)
# Con un dato más, con capacidad de palanca pero dentro de la recta
x0=30
y0p=predict(mod,data.frame(x=c(x0)))
names(y0p)=c()
y0=y0p
x_ampliado=c(x,x0)
y_ampliado=c(y,y0)
mod_b1=lm(y_ampliado~x_ampliado)
plot(xmarco,ymarco,type="n",xlab="(b1)",ylab="",asp=1)
points(x,y)
points(mean(x_ampliado),mean(y_ampliado),col="red",pch="X")
abline(mod,col="blue")
points(x0,y0,col="red",pch=19)
abline(mod_b1,col="red")
summary(mod_b1)
# Con un dato más, con capacidad de palanca y fuera de la recta
y0=y0p-8
x_ampliado=c(x,x0)
y_ampliado=c(y,y0)
mod_b2=lm(y_ampliado~x_ampliado)
plot(xmarco,ymarco,type="n",xlab="(b2)",ylab="",asp=1)
points(x,y)
points(mean(x_ampliado),mean(y_ampliado),col="red",pch="X")
abline(mod,col="blue")
points(x0,y0,col="red",pch=19)
abline(mod_b2,col="red")
summary(mod_b2)

hat(model.matrix(mod_a2)) #se calcula la matriz del diseño con el comando
#model.matrix después se extrae la diagonal de la matriz hat con el 
#comando hat (son los elementos hii)

#Calculo de los residuos, residuos estandarizados, y estudentizados
residuals(mod_b2)
rstandard(mod_b2)
rstudent(mod_b2)

cooks.distance(mod_b2) # Distancia de Cook
plot(mod_b2) #Si en el QQ-plot los puntos estan proximos a la recta se acepta normalidad

shapiro.test(rstandard(mod_b2)) #Si el p-value es pequeño, se rechaza normalidad
