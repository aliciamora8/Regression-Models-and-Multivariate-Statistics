
# PRÁCTICA REGRESIÓN LOXÍSTICA

########################################################
# Exercicio 1
########################################################

# Lectura de datos:

datos=read.table("chdage.txt",header=TRUE)
attach(datos)
head(datos)

# 1.

plot(CHD ~ Age, main="Diagrama de dispersi?n")

mod_lineal = lm(CHD ~ Age)
summary(mod_lineal)
windows()
plot(CHD ~ Age, main="Ajuste lineal")
abline(mod_lineal)

# Por que non é adecuado este axuste?


# 2.

# Idade agrupada en intervalos: [0,45) y [45,+infty)
Age_g=Age
Age_g[Age<45]="[0,45)"
Age_g[Age>=45]="[45,infty)"
Age_g=factor(Age_g)

# Modelo loxístico:
mod_logit_g = glm(CHD~Age_g,family=binomial(link="logit"))
summary(mod_logit_g)

coef(mod_logit_g)


# 3.

exp(coef(mod_logit_g)[1])
exp(coef(mod_logit_g)[1] + coef(mod_logit_g)[2])
exp(coef(mod_logit_g)[2])

# OR manualmente:
exp(coef(mod_logit_g)[1] + coef(mod_logit_g)[2])/exp(coef(mod_logit_g)[1])

# IC para a odds no grupo de ref e a OR:
exp(confint(mod_logit_g))

# Comparar con asintóticos (exemplo para beta0)
beta0.hat <- coef(mod_logit_g)[1]; beta0.hat
# Empregamos a estimación do erro típico 
# de beta0.hat
inf <- beta0.hat-0.3396*qnorm(0.975)
sup <- beta0.hat+0.3396*qnorm(0.975)
intervalo <- c(inf, sup) 
intervalo # IC para beta0.hat
exp(intervalo)

# Comprobar asimetría do intervalo por
# profile-likelihood:
beta0.hat-confint(mod_logit_g)[1]
confint(mod_logit_g)[2]-beta0.hat


# 4.

mod_logit_c = glm(CHD ~ Age, family=binomial(link="logit"))
summary(mod_logit_c)
exp(coef(mod_logit_c))

summary(mod_logit_c)
coef(mod_logit_c)
beta0.hat <- coef(mod_logit_c)[1]
beta1.hat <- coef(mod_logit_c)[2]

exp(beta0.hat)
exp(beta1.hat)

# Probabilidade de padecer a enfermidade para unha 
# persoa de 20 anos:
exp(beta0 + beta1*20)/(1+exp(beta0 + beta1*20)) # Prob

# OR de aumentar a idade en 60 anos:
exp(beta1*60)

# Intervalos de confianza para os coeficientes basados
# en Profile Likelihood do 90%
int_beta = confint(mod_logit_c, level=0.9) # IC para os coeficientes
int_beta
# IC do 90% para a odds se x=0 e para a Odds Ratio  
int_expbeta = exp(int_beta)
int_expbeta


# 5.
# Representación do axuste obtido:

windows()
plot(CHD ~ Age, main="Ajuste logístico")
beta = coef(mod_logit_c)
curve(exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x)), add=TRUE)


#######################################################
# Exercicio 2
######################################################


# Lectura dos datos
datos=read.table("lowbwt.txt",header=TRUE)
head(datos)
datos$race=factor(datos$race)

# Modelo de regresión loxística múltiple
mod_logit_2=glm(low~age+lwt+race+ftv,data=datos,family=binomial(link="logit"))
summary(mod_logit_2)
exp(coef(mod_logit_2))
# Exponenciais dos coeficientes, interprétanse como
# a odds ratio para o efecto de cada unha das variables
# fixados os valores das demais.

# Odds no grupo de referencia da categórica (race=1)
# para individuos nos que o resto de covariables 
# (continuas) toman o valor 0
exp(coef(mod_logit_2)[1])

# Odds ratio ó pasar do grupo de referencia ó grupo race=2
# mantendo constante o valor no resto das covariables:
exp(coef(mod_logit_2)[4])

# Odds ratio dentro do mesmo grupo no relativo á variable
# race ó aumentar a idade da nai nun ano mantendo 
# o resto constantes:
exp(coef(mod_logit_2)[2])
# A odds diminúe (contraintuitivo, ver explicación
# nos apuntes do tema 6)

# Odds ratio dentro do mesmo grupo da categórica ó aumentar
# o número de visitas ó médico no primeiro trimestre
# do embarazo (variable ftv) nunha unidade:
exp(coef(mod_logit_2)[6])
# A odds diminúe.

# Apartado 2.

# No axuste anterior as variables age e ftv (número
# de visitas ó médico no primeiro trimestre) non 
# resultan significativas.

# Vexamos se se poden quitar age e ftv planteando
# un contraste baseado na deviance:

mod_reducido = glm(low ~ lwt+race, data=datos, family=binomial(link="logit"))
anova(mod_reducido, mod_logit_2)

# Anova non saca o nivel crítico, hai que facelo a man:
1-pchisq(0.68618,2)
# O argumento 2 obtense como a diferencia no número
# de parámetros entre o modelo simplificado e o
# máis completo.

# En vista do p-valor non hai evidencias significativa
# para rexeitar H0 (o modelo máis simple) para ningún
# dos niveis de significación habituais.

# En principio é razoable o modelo simplificado
# proposto.

# A man:
dev = deviance(mod_logit_2)
dev_reducido = deviance(mod_reducido)
estadistico = dev_reducido-dev

p_logit_2 = length(coef(mod_logit_2))
p_reducido = length(coef(mod_reducido))
pvalor = 1-pchisq(deviance(mod_reducido)-deviance(mod_logit_2), p_logit_2-p_reducido)

# Apartado 3.

# Búsqueda automática do modelo cun criterio global
# AIC, backward
step(mod_logit_2)

modelo_seleccionado <- step(mod_logit_2)
modelo_seleccionado

# O modelo seleccionado pola función step é o mesmo
# que o seleccionado no apartado anterior.

coef(modelo_seleccionado)
coef(mod_logit_2)
