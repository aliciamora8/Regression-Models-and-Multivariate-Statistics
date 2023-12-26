
# Práctica 2. Regresión lineal: aplicación a datos

#--------------------------------------------------------------------
# 0. LECTURA DE DATOS
#--------------------------------------------------------------------

library(faraway)
data(savings)
head(savings)
names(savings)
dim(savings)

# datos <- read.table("DatosChoiva.txt",sep="\t",header=TRUE) # Para ler datos dunha táboa

pop15 #savings$pop15
attach(savings)
pop15


#------------------------------------------------------------------
# 1. REGRESIÓN LINEAL SIMPLE
#------------------------------------------------------------------

# 1.a) Asignación de variables e diagrama de dispersión

y <- sr
x <- pop15
n <- dim(savings)[1]
plot(y ~ x, xlab="% de población menor de 15 años", ylab="Tasa de ahorro", pch=16) 

# Axuste do modelo:

mod <- lm(y ~ x) # mod <- lm(sr ~ pop15) 
abline(mod, col=2) #engade a liña de axuste ao plot anterior
summary(mod) # Coeficientes moi significativos
# Pendente negativa non moi pronunciada (proporcionalidad inversa)


# 1.b) Estimación dos coeficientes da tendencia

mod$coefficients;mod$coef

# Estimación da varianza (parámetro nuisance)
  # A partir dos residuos:
    res <- mod$residuals
    sigma2_hat <- sum(res^2)/(n-2); sigma2_hat
  # A partir da deviance:
    deviance(mod)/(n-2)
  # Con graos de liberdade:
    deviance(mod)/mod$df.residual


# 1.c) Intervalos de confianza para os coeficientes (confint)
  confint(mod,level=0.95)
  confint(mod,level=0.9)
  confint(mod,level=0.99)
  # Conteñen ó parámetro cunha certa probabilidade dada por level
  
  
# 1.d) e 1.e) Prediccións do modelo axustado e intervalos
  
newx <- c(15,20,30,40,50,60) 
# newx <- c(-3,0,5,80,90,100) # Ollo con extrapolar fora do rango orixinal da explicativa
ICmedia <- predict(mod,newdata=data.frame(x=newx),interval="confidence",level=0.95); ICmedia
ICpred <- predict(mod,newdata=data.frame(x=newx),interval="prediction",level=0.95); ICpred

# Representación das predicións

par(mfrow=c(1,2))

# Datos reais
plot(y ~ x, xlab="% de población menor de 15 años", ylab="Tasa de ahorro", pch=16, xlim=c(15,60),ylim=c(-5,25))
abline(mod, lty=2)
points(newx, ICpred[,1], col=2, pch=19)

# Prediccións e datos reais
vx<-newx
vy<-seq(min(ICpred[,2]),max(ICpred[,3]),l=length(newx))
plot(vy~vx,type="n",main="Intervalos",xaxt="n",xlab="% de población menor de 15 años", ylab="Tasa de ahorro", pch=16, xlim=c(15,60),ylim=c(-5,25))
segments(vx,ICpred[,2],vx,ICpred[,3],col=2,lwd=2)
segments(vx,ICmedia[,2],vx,ICmedia[,3],col=4,lwd=3)
points(newx,ICpred[,1],pch=16)
abline(mod, lty=2)
# abline(h = 0)
points(x,y)




#--------------------------------------------------------------------
# 2. REGRESIÓN LINEAL MÚLTIPLE
#--------------------------------------------------------------------

# 2.a) Axuste do modelo:

modelo <- lm(sr ~ pop15+pop75+dpi+ddpi)
summary(modelo) # Interpretación

# Revisar táboa de coeficientes: interpretar valor dos coeficientes estimados.
# - Cales son significativos?
# Hai dous coeficientes non significativos: os asociados a pop75 y dpi
# Ademais, o coeficiente asociado a ddpi non é significativo ó 1%
# - Interpretación do valor do estimador do coeficiente
# Por exemplo, o coeficiente asociado a pop15 é -0.46. Esto quere dicir que, por
# cada punto porcentual que se incrementa a poboación menor de 15 anos (e se os valores
# das restantes variables se manteñen constantes), entón a taxa de aforro do país 
# redúcese en 0.46.


# 2.b) Elementos do modelo:

  # Coeficientes del modelo
  beta<-coef(modelo)
  beta

  # Matriz de deseño
  X <- model.matrix(modelo)
  n <- nrow(X)
  p <- ncol(X)
  X

  # Cálculo da inversa de (X'X)
  XtXi <- solve(t(X)%*%X)

  # Obtención de estimadores por la fórmula
  beta_hat <-  XtXi %*% t(X) %*% y
  beta_hat; beta # Vemos que é o mesmo que calcula a función lm

  # Matriz Hat
  H <- X %*% XtXi %*% t(X)
  yhat <- H %*% y
  # Podemos comprobar que da o mesmo que coa función lm
  modelo$fitted.values
  yhat - modelo$fitted.values #vemos que as diferenzas son de orde 10e-13

  # Matriz xeradora de residuos
  M <- diag(1,nrow=nrow(H)) - H
  res <- M %*% y
  # Podemos comprobar que da o mesmo que cona función lm
  res - modelo$residuals #vemos que as diferenzas son de orde 10e-13

  
# 2.c) Varianza residual (nuisance, para inferencia)
  
  # A estimación da varianza do erro (varianza residual) obtense como
  # RSS/(n-p). Calculamos RSS de distintas maneiras

  RSS <- t(y-X%*%beta)%*%(y-X%*%beta); RSS
  RSS <- sum((y-yhat)^2); RSS
  RSS <- sum((M%*%y)^2); RSS

  deviance(modelo)
  # Esta función é xeral, tamén serve para modelos que non se axustan por mínimos
  # cadrados. No caso da estimación por mínimos cadrados, devolve RSS.

  # Estimacion da varianza residual
  sigma2.hat <- RSS/(n-p)
  sigma2.hat

  # Se facemos a raiz cadrada vemos que sae na táboa resumo do modelo
  sqrt(sigma2.hat)
  summary(modelo) # Graos de liberdade n-p

  
# 2.d) Covarianza entre os coeficientes:
  
  # Matriz de varianzas-covarianzas dos estimadores dos coeficientes
  covbeta <- as.numeric(sigma2.hat)*XtXi  
  round(covbeta,3)
  
  # Erro típico dos estimadores dos coeficientes:
  ET <- sqrt(diag(covbeta)) # Para inferencia
  
  
# 2.e) Intervalos de confianza basados na distribución teórica (fórmula vista nas expositivas):

  # Cálculo del cuantil de la T de Student
  alfa <- 0.05
  niv <- 1-alfa # Nivel de confianza
  t = qt(1-alfa/2,n-p) # Como T Student es simétrica basta cambiarle el signo
  # qt(alfa/2,n-p)

  # Extremos inferiores
  betainf=beta-t*ET

  # Extremos superiores
  betasup=beta+t*ET

  # Intervalos de confianza para os parámetros da función de regresión;
  betainf; betasup

  # Usando confint
  confint(modelo,level=niv) 
  # Como comprobamos para o caso da regresión lineal simple a función confint
  # aplicada a axustes tipo lm calcula os intervalos de confianza baseados na
  # distribución asintótica teórica.

  
# 2.f) Representación gráfica elipse de confianza para beta1 y beta2

  library(ellipse)  
  # Elipse de confianza para beta1 e beta2 (coeficientes 2 e 3):
  plot(ellipse(modelo,c(2,3)),type="l",xlim=c(-1,0),main="95% confidence region")  
  points(beta[2],beta[3],pch=16,col=4) # Estimadores dos coeficientes

  # Representamos o punto (0,0)
  points(0,0,pch=16,col=2)  
  # Se este punto está dentro da rexión de confianza significa que non podemos
  # descartar que ambos coeficientes sexan 0 (non efecto).
  summary(modelo) # pop15 é moi significativo
  # Representamos os límites dos intervalos de confianza individuais:
  abline(v=betainf[2],col=2,lwd=2)
  abline(v=betasup[2],col=2,lwd=2)
  abline(h=betainf[3],col=2,lwd=2)
  abline(h=betasup[3],col=2,lwd=2)
  # Rexeitamos a hipótese nula.
  
  # Para dpi y ddpi (no significativos)
  plot(ellipse(modelo,c(4,5)),type="l",main="95% confidence region")  
  points(beta[4],beta[5],pch=16,col=4)
  points(0,0,pch=16,col=2)  
  abline(v=betainf[4],col=2,lwd=2)
  abline(v=betasup[4],col=2,lwd=2)
  abline(h=betainf[5],col=2,lwd=2)
  abline(h=betasup[5],col=2,lwd=2)
  #En este caso, el (0,0) cae en la elipse (podemos admitir que ambos
  #coeficientes son nulos al mismo tiempo), y sin embargo no está en la
  #región que conforman los intervalos para cada coeficiente.
  
  
  # Para pop75 y dpi (non significativos):
  plot(ellipse(modelo,c(3,4)),type="l",main="95% confidence region")  
  points(beta[3],beta[4],pch=16,col=4)
  points(0,0,pch=16,col=2)  
  # Representamos os límites dos intervalos de confianza individuais:
  abline(v=betainf[3],col=2,lwd=2)
  abline(v=betasup[3],col=2,lwd=2)
  abline(h=betainf[4],col=2,lwd=2)
  abline(h=betasup[4],col=2,lwd=2)
  
  # Vemos que (0,0) está na rexión de confianza. 
  # Tamén está dentro do produto cartesiano dos intervalos de confianza
  # individuais de cada un dos coeficiente, pero isto non nos serve para
  # decidir xa que hai zonas dentro da rexión cadrada e non dentro da
  # elipse (individualmente parece que ambos poderían ser cero pero
  # conxuntamente descartaríamos esta hipótese)

  # Para pop75 y ddpi (no significativos)
  plot(ellipse(modelo,c(3,5)),type="l",main="95% confidence region", xlim=c(-5,2), ylim=c(-0.2,1))  
  points(beta[3],beta[5],pch=16,col=4)
  points(0,0,pch=16,col=2)  
  abline(v=betainf[3],col=2,lwd=2)
  abline(v=betasup[3],col=2,lwd=2)
  abline(h=betainf[5],col=2,lwd=2)
  abline(h=betasup[5],col=2,lwd=2)
  
  # Para un nivel do 99%: si que entra dentro da rexión de confianza
  # Para pop75 y ddpi (ddpi significativo)
  plot(ellipse(modelo,c(3,5),level=0.99),type="l",main="99% confidence region", xlim=c(-5,2), ylim=c(-0.2,1))  
  points(beta[3],beta[5],pch=16,col=4)
  points(0,0,pch=16,col=2)  
  
  # Obtemos os IC individuais nivel 99%
  alfa2 <- 0.01
  t2 = qt(1-alfa2/2,n-p)
  betainf2 = beta-t2*ET
  betasup2 = beta+t2*ET
  abline(v=betainf2[3],col=2,lwd=2)
  abline(v=betasup2[3],col=2,lwd=2)
  abline(h=betainf2[5],col=2,lwd=2)
  abline(h=betasup2[5],col=2,lwd=2)


# 2.g) ANOVA - F-test
  
  # Se queremos saber se un coeficiente é nulo -> test de significación
  # Se queremos saber se un vector de dous coefs son nulos -> graficamente (elipse)
  # (acabamos de ver que chequear un hipótese bivariante no equivale a solapar
  # os resultados univariantes para ambos coefs, prod cartesiano de intervalos
  # individuais != rexión de confianza bivariante -elipse-)
  # Se queremos saber se tres ou máis son nulos? -> F-test
  
# Podemos quedarnos únicamente con pop15?
  
  # CÁLCULO MANUAL:
  modelo # H1 (modelo complexo)
  modelo0 <- lm(sr ~ pop15) # H0: modelo restrinxido
  
  # H1:
  RSS <- sum(modelo$residuals^2)
  X <- model.matrix(modelo) # n x p 
  n <- nrow(X)
  p <- ncol(X)
  
  # H0:
  RSS0 <- sum(modelo0$residuals^2)
  X0 <- model.matrix(modelo0) # n x r
  r <- ncol(X0)
  
  # Estatístico F:
  q = p-r
  F <- ( (RSS0-RSS)/q ) / ( RSS/(n-p) )
  
  pval <- pf(F, df1=q, df2=n-p, lower.tail = FALSE); pval
  
  # CÁLCULO CON R:
  
  modelo0 <- lm(sr~pop15)
  anova(modelo0,modelo)
  
  # Saída do comando anova: 
  # Resultado do F-test, devolve a táboa de análise da varianza
  # H0: modelo máis simple
  # H1: modelo máis complexo
  
  # Táboa:
  # Res.Df: graos de liberdade de cada modelo (n-nºparametros)
  # RSS: sumas de residuos ó cadrado (RSS0 para o modelo máis simple)
  # Df: diferencia de grados de libertad entre o modelo simplificado e o xeral
  # Sum of Sq.: Diferenza RSS0-RSS (numerador do estatístico de contraste)
  # F: estatítico de contraste
  # Pr(>F): p-valor do test F
  
  # pvalor do Ftest = 0.04
  # Para un nivel de significación do 5% -> Rexeitamos H0 (preferimos modelo completo)
  # Para un nivel de significación do 1% -> Non podemos rexeitar H0 (preferimos modelo simple)

# Podemos eliminar a renta per cápita do modelo?
  
  summary(modelo) # Test de sigificación (dpi non significativa)
  modelo1 <- lm(sr ~ pop15+pop75+ddpi)
  anova(modelo1,modelo) # Modelos anidados
  # Non hai evidencias para descartar H0 (o modelo máis simple)
  # Sí, podemos eliminar a renta per cápita

# Podemos eliminar las dos variables con coeficientes no significativos
  
  summary(modelo) # Son pop75, dpi
  plot(ellipse(modelo,c(3,4)),type="l",main="95% confidence region")  
  points(beta[3],beta[4],pch=16,col=4)
  points(0,0,pch=16,col=2)
  
  modelo2 <- lm(sr ~ pop15+ddpi)
  anova(modelo2,modelo)
  # p-valor do Ftest = 0.19
  # Non hai evidencias en contra do modelo simple: é razoable eliminar esas dúas
  # variables.

  
# 2.h) Predicións para cada país:
  
  modelo$fitted.values # Para países observados chámanse axustes
  predict(modelo) 
  predict(modelo,interval="confidence")
  predict(modelo,interval="prediction") # Danos un warning recordando que son axustes

  
# 2.i) Intervalos de confianza e predicción para novos países:
  
predict(modelo,data.frame(pop15=30,pop75=2,dpi=1000,ddpi=5)) 
predict(modelo,data.frame(pop15=30,pop75=2,dpi=1000,ddpi=5),interval = "confidence")
predict(modelo,data.frame(pop15=30,pop75=2,dpi=1000,ddpi=5),interval = "prediction") 


# 2.j) Estimacións/prediccións para dúas novas observacións simultáneamente:

NObs_conf<-predict(modelo,data.frame(pop15=c(30,40),pop75=c(2,1.5),dpi=c(1000,500),ddpi=c(5,4)),interval="confidence")  
NObs_conf
NObs<-predict(modelo,data.frame(pop15=c(30,40),pop75=c(2,1.5),dpi=c(1000,500),ddpi=c(5,4)),interval="prediction")  
NObs




################################################################################
# COEFICIENTES DE CORRELACIÓN SIMPLE, MÚLTIPLE E PARCIAL
################################################################################

# Correlación simple:
cor(savings)

# Coeficientes de determinación:
R2 <- cor(savings)^2; R2

# REGRESIÓN LINEAR SIMPLE:

# Correlación simple entre sr e pop15:
cor(savings)[1,2]
# cov(sr,pop15)/sqrt(var(sr)*var(pop15))

# mod <- lm(sr ~ pop15)
summary(modelo0)
# A pendente (coeficiente asociado a pop15) ten o mesmo signo
# que a correlación (ambos son negativos).

# O coeficiente de determinación coincide co cadrado da correlación linear
# de Pearson:
cor(savings)[1,2]^2

# Fixarnos sobre todo no coeficiente de determinación AXUSTADO (ten en conta
# a cantidade de parámetros, permite comparar modelos con distintos número
# de parámetros).
TSS <- sum((sr - mean(sr))^2)
nonmodelo <- lm(sr ~ 1)
TSS <- sum((nonmodelo$residuals)^2)
R2_ax <- 1 - (RSS0/(n-r))/(TSS/(n-1)); R2_ax


# REGRESIÓN LINEAR MÚLTIPLE:

# Coeficiente de correlación lineal múltiple:
# Cálculo como correlación simple entre a resposta e o axuste do modelo (yhat):
cor(sr, modelo$fitted.values)
cor(sr ,fitted(modelo))
cor(sr ,fitted(modelo))^2 # Coeficiente de determinación
# R^2 calculado coa función lm de R:
summary(modelo)
# R^2 axustado:
R2_ax <- 1 - (RSS/(n-p))/(TSS/(n-1)); R2_ax

# Coeficiente de correlación parcial de sr e pop15:
# Cálculo como correlación simple dos residuos do axuste de sr sobre as demais
# variables e de pop15 sobre as demais variables.
mod1 <- lm(sr ~ pop75 + dpi + ddpi)
mod2 <- lm(pop15 ~ pop75 + dpi + ddpi)
cor(mod1$residuals,mod2$residuals)
# Vemos que o signo do coeficiente de correlación parcia coincide co da
# estimación do coeficiente asociado a pop15 no modelo de regresión múltiple.


# O coeficiente asociado a unha variable nun modelo de regresión lineal simple
# pode ter distinto signo que o asociado a esa mesma variable no modelo de
# regresión lineal, o efecto verdadeiro non cambia pero hai FENÓMENO DE CONFUSIÓN.

# Coeficiente asociado a pop75 en regresión linear simple:
mod_pop75 <- lm(sr ~ pop75)
summary(mod_pop75) # Coeficiente positivo
cor(sr,pop75)

# Coeficiente asociado a pop75 en regresión linear múltiple:
summary(modelo) # Coeficiente negativo

# Cálculo como correlación simple dos residuos do axuste de sr sobre as demais
# variables e de pop75 sobre as demais variables.
mod1 <- lm(sr ~ pop15 + dpi + ddpi)
mod2 <- lm(pop75 ~ pop15 + dpi + ddpi)
cor(mod1$residuals,mod2$residuals) # Negativo
# Vemos que o signo do coeficiente de correlación parcial coincide co da
# estimación do coeficiente.

# Sobre a interpretación deste tipo de fenómenos ver o exemplo dos
# apuntes coa base de datos de Greene.





