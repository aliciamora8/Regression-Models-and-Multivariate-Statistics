library(glmnet)
library(MASS)
n=200
X=mvrnorm(n,mu=c(0,0),Sigma=diag(2))
X2=mvrnorm(n,mu=c(0,0),Sigma=diag(2))
XX=cbind(X,X2*0.05+0.95*X)
beta=c(2,-3,0,0)
y=XX%*%beta+rnorm(n,sd=.25)
datos=data.frame(y=y,XX=XX)
resridge=cv.glmnet(XX,y,alpha=0,lambda=seq(0.1,2,len=51),standardize=FALSE)
plot(resridge)
coef(resridge,s=resridge$lambda.min)
coef(resridge,s=resridge$lambda.1se)
plot(resridge$glmnet.fit,xvar="lambda")
reslasso=cv.glmnet(XX,y,alpha=1,lambda=seq(0.01,0.2,len=51),standardize=FALSE)
plot(reslasso)
coef(reslasso,s=reslasso$lambda.min)
coef(reslasso,s=reslasso$lambda.1se)
plot(reslasso$glmnet.fit,xvar="lambda")
resenet=cv.glmnet(XX,y,alpha=0.5,lambda=seq(0.01,0.2,len=51),standardize=FALSE)
plot(resenet)
coef(resenet,s=resenet$lambda.min)
coef(resenet,s=resenet$lambda.1se)
plot(resenet$glmnet.fit,xvar="lambda")
#RKHS
install.packages("kernlab",dep=TRUE) #Se instala una vez por ordenador
library(kernlab)
kpoly=polydot(degree=3)  #Kernel polinómico de grado 3
lambda=0.02
K=kernelMatrix(kpoly,XX)
c=drop(solve(K+lambda*nrow(K)*diag(nrow(K)))%*%y)
ytilde=kernelMult(kpoly,XX,XX,c)
plot(ytilde,y)
nfold=10
fold=sample(1:nfold,nrow(XX),replace=TRUE)
table(fold)
lambda=seq(0.001,0.05,len=51)
MSE=numeric(length(lambda))
ytilde=numeric(nrow(XX))
for (i in 1:length(lambda)){
for (j in 1:nfold){
K=kernelMatrix(kpoly,XX[fold!=j,])
c=drop(solve(K+lambda[i]*nrow(K)*diag(nrow(K)))%*%y[fold!=j])
ytilde[fold==j]=kernelMult(kpoly,XX[fold==j,],XX[fold!=j,],c)
}
MSE[i]=mean((y-ytilde)^2)
}
plot(lambda,MSE)
lambda[which.min(MSE)]





