install.packages("glmnet",dep=TRUE)
library(glmnet)
library(MASS)
library(car)
n=200
X=mvrnorm(n,mu=c(0,0),Sigma=diag(2))
X2=mvrnorm(n,mu=c(0,0),Sigma=diag(2))
XX=cbind(X,X2*0.05+0.95*X)
pairs(XX)
round(cor(XX),3)
beta=c(2,-3,0,0)
y=XX%*%beta+rnorm(n,sd=.25)
datos=data.frame(y=y,XX=XX)
#------------ Modelo lineal
reslm=lm(y~.,data=datos)
summary(reslm)
vif(reslm)

#------------ Regresión Ridge
resridge=cv.glmnet(XX,y,alpha=0,lambda=seq(0.05,1,len=51),standardize=FALSE)
summary(resridge)  # Esto no muestra nada 
plot(resridge)  # Gráfico MSE en función de lambda
resridge$lambda.min
resridge$lambda.1se
coef(resridge,s=resridge$lambda.min)
plot(resridge$glmnet.fit,xvar="lambda") #Gráfico path das variables.
abline(v=log(resridge$lambda.min),lty=2)
abline(v=log(resridge$lambda.1se),lty=2)

# Otras opciones para ridge

library(ridge) #Debe estar instalada
res.ridge=linearRidge(y~.,datos,lambda="automatic",scaling="none")
plot(res.ridge)
coef(res.ridge)   # Non se parece ao  anterior, los lambda no parecen estar en la misma escala
summary(res.ridge) 

library(lmridge) #Debe estar instalada
res.lmridge=lmridge(y~.,datos,K=seq(0.05,1,len=21),scaling="sc")  #Los lambda no parecen estar en la misma escala
summary(res.lmridge)
plot(res.lmridge)
cv.plot(res.lmridge)

