#---------------------#
#Lectura de datos
#---------------------#
datos <- read.table("oxigeno.txt",header=T)
head(datos)
attach(datos)


#Gráfica
windows()
plot(Oxigeno~Lugar,pch=16)


#Modelo "mal"
modelo_mal <- lm(Oxigeno~Lugar)
summary(modelo_mal)
abline(modelo_mal,lwd=2,col=2)


#Axuste dun modelo ANOVA
#Clase de Lugar
class(Lugar)
Lugar <- factor(Lugar) #cambiamos clase
class(Lugar)
summary(Lugar)


#Con respecto a medias locais
mu_local <- c()
mu_local[1] <- mean(Oxigeno[Lugar=="1"])
mu_local[2] <- mean(Oxigeno[Lugar=="2"])
mu_local[3] <- mean(Oxigeno[Lugar=="3"])
mu_local[4] <- mean(Oxigeno[Lugar=="4"])
mu_local

modelo_local <- lm(Oxigeno~Lugar-1)
summary(modelo_local)


#Con respecto a media global
mu_global <- mean(Oxigeno)
alpha <- mu_local - mu_global
mu_global
alpha


#Con respecto a un grupo de referencia
modelo_ref <- lm(Oxigeno~Lugar)
summary(modelo_ref)



#Cambiar grupo de referencia
levels(Lugar)
Lugar2 <- relevel(Lugar,ref="2")
modelo_ref2 <- lm(Oxigeno~Lugar2)
summary(modelo_ref2)



# 2. ¿Se puede asumir que las medias de todos los grupos son iguales?

anova(lm(Oxigeno ~ Lugar))

# Vemos que coincide con lo obtenido en el summary para la parametrización
# respecto a un grupo de referencia.

# Descartamos que las medias de todos los grupos son iguales para los niveles
# de significación habituales.
# (pvalor = 2.646e-09)


# 3. Calculamos intervalos de confianza para la diferencia de medias:

modelo <- lm(Oxigeno ~ Lugar)

# Método de Tukey:
TukeyHSD(aov(Oxigeno ~ Lugar))
# Compara las diferencias de medias por pares de grupos y proporciona
# una estimación de esas diferencias (diff), los límites de un
# intervalo de confianza del 95% y el p-valor ajustado a la
# realización de comparaciones múltiples, que permite
# decidir si se rechaza o no la hipótesis de igualdad de medias.

#Contrastes múltiples
pairwise.t.test(Oxigeno,Lugar) # Método Holm por defecto
?pairwise.t.test
pairwise.t.test(Oxigeno,Lugar,p.adjust.method="bonferroni") # Método Bonferroni
pairwise.t.test(Oxigeno,Lugar,p.adjust.method="BH") # Método Benjamini & Hochberg



# 4. Validación do modelo.

# Diagnosis de atípicos en cada grupo (en este caso no porque hay
# pocas observaciones)

# Validación:

# Normalidad dentro de cada grupo:

O1 <- Oxigeno[Lugar=="1"]
O2 <- Oxigeno[Lugar=="2"]
O3 <- Oxigeno[Lugar=="3"]
O4 <- Oxigeno[Lugar=="4"]

shapiro.test(O1)
shapiro.test(O2)
shapiro.test(O3)
shapiro.test(O4)

# Homogeneidad de las varianzas de todos los grupos:

# TEST DE LEVENE:
# H0: sigma_1 = sigma_2 = ... = sigma_I

abs_res <- abs(modelo$residuals)
library(car)
leveneTest(lm(abs_res ~ Lugar))

# En este caso podemos asumir que se cumplirían las hipótesis del modelo.


