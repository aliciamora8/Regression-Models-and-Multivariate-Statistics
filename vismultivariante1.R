comarcas=read.csv2("ComarGal2.csv",sep=";",dec=".",stringsAsFactors=TRUE,encoding="latin1")
summary(comarcas)

comnum=comarcas[,c("Tnat","Tmort","Dens2017","X.Univ","X.Abs","RendMedio","PIBCap")]
rownames(comnum)=comarcas[,3]
summary(comnum)

unos=rep(1,nrow(comnum))
xmean=t(comnum)%*%unos/nrow(comnum)  #Calculo de las medias usando op. matriciales. Ojo a los NA!
xmean=unlist(lapply(comnum,mean,na.rm=TRUE))  #lapply con data.frame(lista)
xmean=apply(as.matrix(comnum),2,mean,na.rm=TRUE) # apply con matrices

S=cov(comnum,use="pairwise")
R=cor(comnum,use="pairwise")

diag(S)  # Vector de varianzas
round(R,3)
heatmap(as.matrix(comnum)) #Para heatmap hay que usar matrices. No se ve nada por la escala

library(ellipse)
plotcorr(R)
library(GGally)
ggpairs(comnum,upper=list(continuous="density"))

symbols(comnum$Tnat,comnum$X.Univ,comnum$Tmort/100,inches=FALSE)  #Ojo a la escala del radio
text(comnum$Tnat,comnum$X.Univ,comarcas$Comarca,cex=.75)

heatmap(as.matrix(comnum)) # Sin estandarizar no se ve nada
heatmap(as.matrix(scale(comnum))) #Estandarizar variable a variable

Xcen=sweep(comnum,2,xmean,"-")  # Centrar todas las variables
apply(Xcen,2,mean,na.rm=TRUE)

# Descomposición en autovalores
auto=eigen(S)
S.5=auto$vectors%*%diag(sqrt(auto$values))
S.5=solve(S.5)
Z1=t(S.5%*%t(Xcen))
colnames(Z1)=colnames(Xcen)
round(cov(Z1,use="pair"),3)
heatmap(na.omit(Z1))

#A=UDV^t  Descomposición singular (ver detalles en Wikipedia)
sv=svd(S)
S.5=diag(1/sqrt(sv$d))%*%t(sv$v)  # La 
Z2=t(S.5%*%t(Xcen))
colnames(Z2)=colnames(comnum)
heatmap(na.omit(Z2))
#######

library(MASS)

tasas=kde2d(comnum[,4],comnum[,5],h=c(bandwidth.nrd(comnum[,4]),bandwidth.nrd(comnum[,5])))
image(tasas,xlab=colnames(comnum)[4],ylab=colnames(comnum)[5])
contour(tasas,add=TRUE)


X1=mvrnorm(500,mu=c(0,0),Sigma=cbind(c(1,0.75),c(0.75,1))) #simulación de datos normales multivariantes
estn2=kde2d(X1[,1],X1[,2],h=c(bandwidth.nrd(X1[,1]),bandwidth.nrd(X1[,2])))
image(estn2)
contour(estn2,add=TRUE)

mvdnorm=function(x,mu=rep(0,ncol(x)),Sig=diag(ncol(x))){
if (is.vector(x)) x=matrix(x,nrow=1)
p=ncol(x)
mmu=matrix(mu,ncol=p,nrow=nrow(x),byrow=TRUE)
cte=sqrt(2*pi)^p*sqrt(det(Sig))
delta=diag((x-mmu)%*%solve(Sig)%*%t(x-mmu))
res=exp(-0.5*delta)/cte
}

x1=seq(-4,4,len=51)
x2=seq(-4,4,len=51)
x=as.matrix(expand.grid(x1,x2))
Sig1=cbind(c(1,-.65),c(-.65,.75))
fx1=matrix(mvdnorm(x,Sig=Sig1),nrow=length(x1),ncol=length(x2),byrow=TRUE)
Sig2=cbind(c(1,.25),c(.25,.75))
fx2=matrix(mvdnorm(x,Sig=Sig2),nrow=length(x1),ncol=length(x2),byrow=TRUE)


par(mfrow=c(2,2))
persp(x1,x2,fx1,theta=15,phi=30)
persp(x1,x2,fx2,theta=15,phi=30)
image(x1,x2,fx1,main=paste0("rho=",round(-0.65/sqrt(1*0.75),3)))
contour(x1,x2,fx1,nlev=10,add=TRUE)
image(x1,x2,fx2,main=paste0("rho=",round(0.25/sqrt(1*0.75),3)))
contour(x1,x2,fx2,nlev=10,add=TRUE)

