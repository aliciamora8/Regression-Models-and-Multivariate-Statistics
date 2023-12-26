
# Practica 3: DIAGNOSE DE OBSERVACIONS ATIPICAS E INFLUINTES

# Parte 1.

# 1. Simulación:

n <- 20
x <- c(1:n)

set.seed(123456)

y <- 1 + 0.5*x + rnorm(n)

windows()
par(mfrow=c(2,4))
plot(y ~ x, pch=19) # Gráfico de dispersión
points(mean(x),mean(y),col="darkblue",pch=4,lwd=3)

modelo <- lm(y ~ x)
abline(modelo, lty=2, col="darkblue")
# A recta axustada pasa polo vector de medias.


# a). 
# Nova observación sobre o vector de medias:

x0 <- mean(x); y0 <- mean(y)
# Representamos a nova observación en vermello:
points(x0,y0,col=2,pch=19) 

# Axustamos unha nova recta de regresion
xnew <- c(x,x0); ynew <- c(y,y0)
modelo1 <- lm(ynew ~ xnew)

# Representamos o novo axuste:
abline(modelo1, col=2, lty=2)
# A recta axustada non varía con respecto ó axuste inicial.

# Predicción do modelo para x0:
# x0 é a última observacion do vector de covariables
# así que unha forma de obter esto é simplemente:
y0_pred <- modelo1$fitted.values[n+1] ; y0_pred
mean(y) # Comprobamos que en efecto son iguais

# Tamén se podería obter coa función predict:
predict(modelo1, data.frame(xnew=x0))

# Aínda que as estimacións dos coeficientes non se ven 
# afectadas, si o fai a estimación da varianza residual:
summary(modelo)
summary(modelo1)
# E, por tanto, afecta á inferencia sobre os parámetros.

# O motivo é que na estimación de sigma se ten en conta o
# número de observacións n e a suma de residuos ó cadrado
# do modelo RSS.
# Por tanto á hora de estimar sigma:
sqrt(sum(modelo$residuals^2)/(n-2)) # Para o modelo orixinal
sqrt(sum(modelo1$residuals^2)/(n+1-2)) # Para o novo


# b).
# Nova observación con abscisa sobre o vector de medias
# e ordenada por debaixo do mínimo das observacións:

x0 <- mean(x)
y0 <- min(y) - 0.1

plot(y ~ x, pch=19) # Observacións orixinais
points(mean(x),mean(y),col="darkblue",pch=4,lwd=3) # Vector de medias
abline(modelo, lty=2, col="darkblue") # Axuste sobre as obs orixinais

# Representamos o novo punto en vermello:
points(x0,y0, pch=19, col=2)

xnew2 <- c(x,x0); ynew2 <- c(y,y0)  

# Axustamos unha nova recta de regresion
modelo2 <- lm(ynew2 ~ xnew2)

# Representamos o novo axuste:

abline(modelo2, col=2, lty=2) # Novo axuste
# Vemos que neste novo axuste o estimador do intercepto é
# notablemente menor que no axuste orixinal. A estimación da
# pendente parece non sufrir alteracións. En efecto:
coef(modelo) # Estimacións axuste inicial
coef(modelo2) # Estimacións novo axuste

# Tamén se ve afectada a estimación da varianza residual.

# c.
# Nova observación sobre a recta axustada orixinal:

x0 <- max(x); y0 <- modelo$fitted.values[n]

plot(y ~ x, pch=19) # Observacións orixinais
points(mean(x),mean(y),col="darkblue",pch=4,lwd=3) # Vector de medias
abline(modelo, lty=2, col="darkblue") # Axuste sobre as obs orixinais

# Representamos o novo punto en vermello:
points(x0,y0, pch=19, col=2)

# Axustamos unha nova recta de regresion:
xnew3 <- c(x,x0); ynew3 <- c(y,y0)
modelo3 <- lm(ynew3 ~ xnew3)

# Representamos o novo axuste:
abline(modelo3, col=2, lty=2) # Novo axuste
# Idéntico ó axuste orixinal:
coef(modelo) # Estimacións axuste inicial
coef(modelo3) # Estimacións novo axuste

summary(modelo3)
summary(modelo)
# Vemos que a adición deste punto si afecta á estimación da
# varianza residual (mesma explicación que no apartado a).

# d.
# Nova observación:
x0 <- max(x); y0 <- min(y)-0.1 # Igual que en (b)

plot(y ~ x, pch=19) # Observacións orixinais
points(mean(x),mean(y),col="darkblue",pch=4,lwd=3) # Vector de medias
abline(modelo, lty=2, col="darkblue") # Axuste sobre as obs orixinais

# Representamos o novo punto en vermello:
points(x0,y0, pch=19, col=2)

# Axustamos unha nova recta de regresion:
xnew4 <- c(x,x0); ynew4 <- c(y,y0)
modelo4 <- lm(ynew4 ~ xnew4)

# Representamos o novo axuste:
abline(modelo4, col=2, lty=2) # Novo axuste

# Vemos que a inclusión deste punto altera moito a estimación
# do intercepto e, máis importante aínda, da pendente:
coef(modelo) # Estimacións axuste inicial
coef(modelo4) # Estimacións novo axuste

# Como é lóxico tamén se ve afectada a estimación da 
# varianza residual

# e.

# Cálculo dos residuos:

modelo$residuals; residuals(modelo)
summary(modelo$residuals)

# Caso (a).
modelo1$residuals; # Cambian todos os residuos porque cambia o axuste
modelo1$residuals[n+1] # O residuo do novo punto é cero
summary(modelo1$residuals)

# Caso (b).
modelo2$residuals;
modelo2$residuals[n+1]
summary(modelo2$residuals)
# Se o valor de y estivese máis próximo ó axuste orixinal
# o residuo sería moito menor (probar).

# Caso (c).
modelo3$residuals
modelo3$residuals[n+1] # O residuo do novo punto é cero 
summary(modelo3$residuals) 
# Conforme ó comentado no apartado a, como agora
# o novo punto está sobre a recta o seu residuo asociado
# é cero.

# Caso (d).
modelo4$residuals;
modelo4$residuals[n+1]
summary(modelo4$residuals)
# Agora o novo punto ten un residuo asociado moito máis
# grande (en termos absolutos) que os demais. 
# A resposta observada (y0) é a mesma que no apartado (b),
# sen embargo o residuo é moito máis grande xa que o nova
# observación está máis lonxe da predicción que se faría
# co noso axuste para ese valor de x0.


# Residuos estandarizados:

rstandard(modelo)
summary(rstandard(modelo))

# Caso (a)
rstandard(modelo1)
# Residuo estandarizado da última observación: 0

# Caso (b)
rstandard(modelo2)
# Residuo estandarizado da última observación moito maior que
# o demais.

# Caso (c)
rstandard(modelo3)
# Residuo estandarizado da última observación: 0

# Caso (d)
rstandard(modelo4)
# Novamente vemos que a última observación ten un residuo
# estandarizado moito maior que as demais.

# CONCLUSIÓN: observamos os mesmos efectos que vimos analizando
# os residuos. Os residuos estandarizados están máis cerca da
# hipótese de homocedasticidade.

# Representación gráfica global:

# Residuos:

# (a).
plot(c(1:(n+1)), residuals(modelo1), ylim=c(-9,5), lwd=2)
points(c(1:(n+1)), rstandard(modelo1), col=2, pch=19) # Estandarizados
abline(h = 2, lty=2, col=2)
abline(h = -2, lty=2, col=2)

# (b).
plot(c(1:(n+1)), residuals(modelo2), ylim=c(-9,5), lwd=2)
points(c(1:(n+1)), rstandard(modelo2), col=2, pch=19) # Estandarizados
abline(h = 2, lty=2, col=2)
abline(h = -2, lty=2, col=2)

# (c).
plot(c(1:(n+1)), residuals(modelo3), ylim=c(-9,5), lwd=2)
points(c(1:(n+1)), rstandard(modelo3), col=2, pch=19) # Estandarizados
abline(h = 2, lty=2, col=2)
abline(h = -2, lty=2, col=2)

# (d).
plot(c(1:(n+1)), residuals(modelo4), ylim=c(-9,5), lwd=2)
points(c(1:(n+1)), rstandard(modelo4), col=2, pch=19) # Estandarizados
abline(h = 2, lty=2, col=2)
abline(h = -2, lty=2, col=2)

























