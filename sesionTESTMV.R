comarcas=read.csv2("ComarGal2.csv",dec=".",string=TRUE)
summary(comarcas)
rownames(comarcas)=comarcas$Comarca
comar2=na.omit(comarcas)
prov=comar2$Prov
n=table(prov)
colnames(comar2)
comarc=as.matrix(comar2[,-c(1:3)])
lvar=c("Tnat","Tmort","X.Abs")
qhot=function(u,p,nu){nu*p*qf(u,p,nu-p+1)/(nu-p+1)}
qhot(0.95,3,40)
qchisq(0.95,3)

xmean=apply(comarc[,lvar],2,mean)
xmean
mu0=c(5.6,15.1,0.3)  # Valor con el que voy a comparar
dxm=matrix(xmean-mu0,ncol=1)
Sig=cov(comarc[,lvar])
T2=sum(n)*t(dxm)%*%solve(Sig)%*%dxm
T2
qhot(0.95,3,sum(n)-1) # Valor crítico

SS=by(comarc[,lvar],prov,cov) # Calculo matriz covarianzas por provincias
SS[["C"]]
SS[[1]]
names(SS)
xm=by(comarc[,lvar],prov,apply,2,mean) # Calculo media por provincias
ST=((n[2]-1)*SS[[2]]+(n[3]-1)*SS[[3]])/(n[2]+n[3]-2)
dxm=matrix(xm[[2]]-xm[[3]],ncol=1)
T2d=((n[2]*n[3])/(n[2]+n[3]))*t(dxm)%*%solve(ST)%*%dxm
T2d
qhot(0.95,3,n[2]+n[3]-2)
#######
# Test igualdad de varianzas
p=3
k=length(n)
nu=n-1
ST=nu[1]*SS[[1]]
for (i in 2:k){
ST=ST+nu[i]*SS[[i]]
}
ST=ST/sum(nu)
lM=(nu[1]/2)*log(det(SS[[1]]))
for (i in 2:k){
lM=lM+(nu[i]/2)*log(det(SS[[i]]))
}
lM=lM-(sum(nu)/2)*log(det(ST))
c1=(sum(1/nu)-1/sum(nu))*((2*p^2+3*p-1)/(6*(p+1)*(k-1)))
U=-2*(1-c1)*lM
qchisq(0.95,0.5*(k-1)*p*(p+1))
#####
# MANOVA
reslm=lm(comarc[,lvar]~prov)
summary(reslm)
resmanova=manova(reslm)
summary(resmanova,test="Wilks")
summary(resmanova,test="Hot")
summary(resmanova,test="Pillai")