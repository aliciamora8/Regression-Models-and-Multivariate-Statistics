################################################################################

# MODELOS DE REGRESIÓN E ANÁLISE MULTIVARIANTE
# PRÁCTICA 1: REPASO MODELO LINEAR SIMPLE

################################################################################

set.seed(13)

# ------------------------------------------------------------------------------
# 0. Simulación de datos do modelo.
# ------------------------------------------------------------------------------

n <- 100
x <- 0.01*(1:n) # Deseño fixo

beta0 <- 1
beta1 <- 2
sigma <- 0.3

erro <- rnorm(n, mean = 0, sd = sigma)

y <- beta0 + beta1 * x # Parte determinista

# Representación gráfica:
#par(mfrow=c(1,2))
plot(y ~ x, pch=19, col=2)
abline(a=beta0,b=beta1,lty=1)

# Parte determinista do modelo (erro=0) -> Efecto da pendente
# Intercepto
# Pendentes: positiva, negativa, cero (non efecto)

plot(y ~ x, pch=19)
y <- y + erro # Erro aleatorio
points(x,y,col=2,pch=19)

# Efecto do modelo que describe a aleatoriedade -> Efecto de sigma^2


# AXUSTE DUN MODELO LINEAR SIMPLE:

plot(y ~ x, pch=19)

mod <- lm(y ~ x)
summary(mod)
str(mod)
mod$coefficients

  # Cálculo manual:
  cov(x,y)/var(x) # pendente (var -> quasi-variance)
  mean(y) - cov(x,y)/var(x) * mean(x) # intercepto

abline(mod, col=2, lwd=2) # abline(a=mod$coefficients[1],b=mod$coefficients[2],lty=2)
abline(a=beta0,b=beta1,lty=2)
legend("bottomright", legend=c("Modelo teórico", "Modelo axustado"), col=c("black","red"), lty=2:1, lwd=1:2, cex=0.8)


# ------------------------------------------------------------------------------
# 1. Efecto da dispersión.
# ------------------------------------------------------------------------------

valores_sigma <- c(0.01,0.1,0.3,2,10)

windows()
par(mfrow=c(2,3))

for (k in 1:length(valores_sigma)){
  sigma <- valores_sigma[k]
  y <- beta0 + beta1*x + rnorm(n, mean=0, sd=sigma)
  plot(y ~ x)
  abline(a = beta0, b = beta1, lty=2) # Recta teórica
  mod <- lm(y ~ x)
  abline(mod, col="red", lwd=2)
  legend("bottomright", legend=c("Real", "Axustado"), col=c("black","red"), lty=2:1, lwd=1:2, cex=0.8)
}

# Reescalando

windows()
par(mfrow=c(2,3))

for (k in 1:length(valores_sigma)){
  sigma <- valores_sigma[k]
  y <- beta0 + beta1*x + rnorm(n, mean=0, sd=sigma)
  print(beta0)
  print(beta1)
  plot(y ~ x, ylim=c(0,4))
  abline(a = beta0, b = beta1, lty=2) # Recta teórica
  mod <- lm(y ~ x)
  abline(mod, col="red", lwd=2)
  legend("bottomright", legend=c("Real", "Axustado"), col=c("black","red"), lty=2:1, lwd=1:2, cex=0.8)
}


# ------------------------------------------------------------------------------
# 2. Hipótesis do modelo
# ------------------------------------------------------------------------------

sigma <- 0.3
y <- beta0 + beta1*x + rnorm(n, mean=0, sd=sigma)

# a. Non normalidad:
y <- beta0 + beta1*x + rexp(n) # rate=1, 0.1, 0.001
plot(y ~ x, pch=19)
abline(a=beta0, b=beta1, lty=2)
mod <- lm(y ~ x)
abline(mod, col="red", lwd=2)
legend("bottomright", legend=c("Real", "Axustado"), col=c("black","red"), lty=2:1, lwd=1:2, cex=0.8)

# b. Heterocedasticidade:
sigma <- 5*x
sigma <- (x-0.5)^2
y <- beta0 + beta1*x + rnorm(n, mean=0, sd=sigma)
plot(y ~ x, pch=19)
abline(a=beta0, b=beta1, lty=2)
mod <- lm(y ~ x)
abline(mod, col="red", lwd=2)
legend("bottomright", legend=c("Real", "Axustado"), col=c("black","red"), lty=2:1, lwd=1:2, cex=0.8)

# c. Dependencia: MA(1)
error <- numeric(n)
error[1] <- rnorm(1, mean=0, sd=0.3)
for (j in 2:n){
  error[j] <- error[j-1] + rnorm(1, mean=0, sd=0.3)
}
y <- beta0 + beta1*x + error
plot(y ~ x, pch=19)
abline(a=beta0, b=beta1, lty=2)
mod <- lm(y ~ x)
abline(mod, col="red", lwd=2)
legend("bottomright", legend=c("Real", "Axustado"), col=c("black","red"), lty=2:1, lwd=1:2, cex=0.8)


# ------------------------------------------------------------------------------
# 3. Estimación da pendente
# ------------------------------------------------------------------------------

sigma <- 0.3
B <- 500

beta1_hat <- numeric(B)

for(b in 1:B){
  y <- beta0 + beta1*x + rnorm(n, sd = sigma)
  mod <- lm(y ~ x)
  beta1_hat[b] <- mod$coefficients[2]
}

hist(beta1_hat, freq=F) # Histograma
abline(v = beta1, lty=2) # Verdadeiro valor da pendente
abline(v = mean(beta1_hat), lwd=2, col=2) # Promedio estimacións da pendente

#Curva teórica 
mu <- beta1
S2x <- ((n-1)/n)*var(x)
s.teor <- sigma/(sqrt(n*S2x))
curve(dnorm(x,mu,s.teor),add=TRUE)
lines(density(beta1_hat),col=4,lwd=2)

# Estimando a varianza:
sigma_hat <- sqrt(sum(mod$residuals^2)/(n-2)) # Por mínimos cadrados
curve(dnorm(x,mu,s.teor),add=TRUE)


# ------------------------------------------------------------------------------
# 4. Propiedades do estimador da pendente: 
#    Sesgadez, consistencia e erro cadrático medio.
# ------------------------------------------------------------------------------

n_vec <- c(25,50,100,200,500,1000)

# Insesgadez:

	mean(beta1_hat)
	# Repetir varias veces
	
	
# Consistencia e erro cadrático medio:
	
	ecm <- numeric(length(n_vec))

	windows()
	par(mfrow=c(2,3))
	for(k in 1:length(n_vec)){
	   n <- n_vec[k]
	   x <- 0.01*(1:n)
	for(b in 1:B){
		eps <- rnorm(n, sd=sigma)
		y <- beta0 + beta1*x + eps
		mod <- lm(y~x)
		beta1_hat[b] <- mod$coefficients[2]
	}
	   hist(beta1_hat, freq=FALSE, main=paste("n=",n_vec[k]), xlim=c(1,3))
	   abline(v=beta1, lty=2)
	   ecm[k] <- (beta1 - mean(beta1_hat))^2 + var(beta1_hat)
	}
	# Nas gráficas ilustrase a consistencia do estimador: a medida que aumenta
	# n a variabilidade do estimador redúcese e os valores das nosas simulacións
	# concéntranse cada vez máis no parámetro teórico.

	library(ggplot2)
	windows()
	plot(ecm ~ n_vec, type="l")
	qplot(n_vec, ecm, size=ecm)
	# O erro cadrático medio diminúe con n.


# ------------------------------------------------------------------------------
# 5. Inferencia sobre a pendente: intervalos de confianza
# ------------------------------------------------------------------------------

n <- 100
x <- 0.01*(1:n) # Deseño fixo

beta0 <- 1
beta1 <- 2
sigma <- 0.3
y <- beta0 + beta1 * x + rnorm(n, mean = 0, sd = sigma)

mod <- lm(y ~ x)
beta1_hat <- mod$coef[2]

# IC teóricos: basados en distribución normal.
alfa <- 0.05
sigma_hat <- sqrt(sum(mod$residuals^2)/(n-2))
c <- sqrt((n-1)*var(x)) # sqrt(sum(x^2) - (1/n)*(sum(x)^2))
L <- mod$coef[2] - (sigma_hat/c) * qt(p = 1-alfa/2, df = n-2, lower.tail = TRUE);L
U <- mod$coef[2] - (sigma_hat/c) * qt(p = alfa/2, df = n-2, lower.tail = TRUE) ;U
# Centrados en beta_hat (t-student é simétrica)

  # Inciso: estimación da desviación típica de beta1_hat:
  sigma_hat/c
  summ <- summary(mod); summ$coefficients[2 , 2] 

confint(mod) #level=0.9,level=0.99

# Análise da cobertura:
# Matrices para almacenar os intervalos
beta1_mat <- matrix(0,nrow=B,ncol=2)

for(b in 1:B){
  y <- beta0 + beta1*x + rnorm(n,sd=sigma)
  mod <- lm(y~x)
  beta1_mat[b,] <- confint(mod)[2,]
}

# Proporción de intervalos que conteñen ó verdadeiro parámetro:
cob_beta1<-sum((beta1_mat[,1]<beta1)*(beta1_mat[,2]>beta1))/B;cob_beta1 

# Graficamente:

windows()
plot(1:B, ylim=c(1,3), type="n",xlab="B",ylab="IC", main = "IC para beta1")
for(b in 1:B){
  segments(b, beta1_mat[b,1], b, beta1_mat[b,2], xlab="1:B", ylab="Intervalo beta1")
}
abline(h=beta1,col=2,lwd=2)



# ------------------------------------------------------------------------------
# 6. Inferencia sobre a pendente: test de non efecto
# ------------------------------------------------------------------------------

# Test de significación da pendente
# H0: beta1 = 0 TEST DE NON EFECTO

# Comprobamos que se respeta o nivel de significación alfa;
# P(rexeitar H0 | H0 falsa) = P(erro tipo I) <= alfa

# Para iso simulamos varias mostras baixo a H0 e vemos en que proporción dos
# casos o test rexeita H0:

beta1 <- 0 # Baixo H0
p_val <- numeric(B) # Almacenamos os p-valores

for(b in 1:B){
  y <- beta0 + beta1*x + rnorm(n,sd=sigma) # Simulamos datos baixo a hipótese nula (beta1=0)
  mod <- lm(y~x)
  p_val[b] <- summary(mod)$coef[2,4] # p-valor do test de significación da pendente (rexeito se p_val < alfa)
}

# Estimación da probabilidade de cometer erro tipo I:
sum(p_val<0.05)/B # Rexeito se p_val < alfa
sum(p_val<0.01)/B; sum(p_val<0.10)/B # Para outros niveis de significación

# Potencia (capacidade de detectar que a hipótese nula é falsa):

	# Afastámonos de H0:
	n <- 100
	x <- 0.01*(1:n)
	beta1 <- 0.01 # Repetimos as probas de calibrado
	# Aínda que H0 é falsa, o valor de beta1 é tan próximo o que tomaría baixo
	# H0 (beta1=0) que o test ten dificultades para detectar que é mentira.
	# Por tanto rexeitamos H0 case coa mesma probabilidade que baixo H0.
	
	# Se agora repetimos as probas de calibrado nun escenario no que nos afastamos
	# moito da hipótese nula:
	beta1 <- 10 # Repetimos as probas de calibrado
	# Agora rexeitamos H0 en todos os casos.



# ------------------------------------------------------------------------------
# 7. Repetir apartados 3,4,5 e 6 para a pendente
# ------------------------------------------------------------------------------


