library(glmnet)
data(airquality)
summary(airquality)
airq=na.omit(airquality[,1:4])
pairs(airq)
res.lm=lm(log(Ozone)~.,data=airq)
summary(res.lm)
plot(res.lm)
y=log(airq$Ozone)
resRR=cv.glmnet(as.matrix(airq[,2:4]),y,alpha=0)
plot(resRR)
coef(res.lm)
coef(resRR,s=resRR$lambda.min)
resLSO=cv.glmnet(as.matrix(airq[,2:4]),y,alpha=1)
plot(resLSO)
coef(resLSO,s=resLSO$lambda.min)
# Semiparametrico
X=airq$Temp
MO=outer(X,X,"-")
h=4  #h inicial de prueba.


Sh=dnorm(MO/h)
Sh=sweep(Sh,1,apply(Sh,1,sum),"/")
sum(diag(Sh))  # Nº equivalente de grados de libertad consumidos
ISh=diag(nrow(Sh))-Sh

Zh=ISh%*%as.matrix(airq[,2:3])
yh=ISh%*%y
betah=solve(t(Zh)%*%Zh)%*%t(Zh)%*%yh
mh=Sh%*%(y-as.matrix(airq[,2:3])%*%betah)
ytilde=as.matrix(airq[,2:3])%*%betah+mh
plot(ytilde,y)
abline(c(0,1))

h=seq(0.1,2,len=101) # Probamos con una rejilla de h más ajustada
MSE=numeric(length(h))
glib=numeric(length(h))

for (i in 1:length(h)){
Sh=dnorm(MO/h[i])
Sh=sweep(Sh,1,apply(Sh,1,sum),"/")
glib[i]=sum(diag(Sh))
ISh=diag(nrow(Sh))-Sh

Zh=ISh%*%as.matrix(airq[,2:3])
yh=ISh%*%y
betah=solve(t(Zh)%*%Zh)%*%t(Zh)%*%yh
mh=Sh%*%(y-as.matrix(airq[,2:3])%*%betah)
ytilde=as.matrix(airq[,2:3])%*%betah+mh
MSE[i]=mean((y-ytilde)^2)
glib[i]=glib[i]+2
}
plot(h,MSE/(length(y)-glib))
h[which.min(MSE/(length(y)-glib))]
