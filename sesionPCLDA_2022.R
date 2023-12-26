install.packages("ellipse",dep=TRUE)
library(ellipse)
?ellipse
data(iris)
summary(iris)
pairs(iris[,1:4],col=iris$Species)
gmean=apply(iris[,1:2],2,mean) #media global
Sigma=cov(iris[,1:2])   #Sigma global
plot(ellipse(Sigma/nrow(iris),centre=gmean),
		type="l",lwd=2,col=2) #Ellipse confianza para test mu=mu0
mu0=c(5.8,3.0)
points(mu0[1],mu0[2],cex=2)


k=combn(3,2) # Todos los pares
SS=by(iris[,1:2],iris$Species,cov) # Cov por Species
xmean=by(iris[,1:2],iris$Species,apply,2,mean) # Media por Species
n=table(iris$Species) # n por Species

plot(c(-1,1),c(-1,1),type="n") #Gráfico con escala para dibujar ellipses y puntos
for (i in 1:ncol(k)){
l1=k[1,i]  # Grupo 1
l2=k[2,i]  # Grupo 2
ST=((n[l1]-1)*SS[[l1]]+(n[l2]-1)*SS[[l2]])/(n[l1]+n[l2]-2)
lines(ellipse(ST*(n[l1]+n[l2])/(n[l1]*n[l2])),col=i)  #Ellipse diferencia de medias
points(xmean[[l1]][1]-xmean[[l2]][1],xmean[[l1]][2]-xmean[[l2]][2],
		pch=19,col=i,cex=2) # Ptos media grupo 1 - media grupo 2
}
#### Componentes principales
pr.iris=princomp(iris[,1:4])
summary(pr.iris,loadings=TRUE)  # Proporcion de variabilidad + loadings
names(pr.iris)
plot(pr.iris$scores[,1:2],col=iris$Species) # Grafico con las componentes
biplot(pr.iris)  #Grafico variables y componentes
screeplot(pr.iris)

#### Factorial discriminante
library(MASS)
?lda
iris.lda=lda(iris[,1:4],iris$Species)
names(iris.lda)
iris.lda$scaling  # Componentes discriminantes 
#discri=as.matrix(iris[,1:4])%*%iris.lda$scaling # Factores discriminantes calculado para cada fila 
discri=scale(iris[,1:4],scale=FALSE)%*%iris.lda$scaling # Mejor recentrando los datos
plot(discri,col=iris$Species)

pr.lda=predict(iris.lda)
# discri = pr.lda$x
table(iris$Species,pr.lda$class)
