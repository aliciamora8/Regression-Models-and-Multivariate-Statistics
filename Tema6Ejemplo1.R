datos=read.table("chdage.txt",header=TRUE)
attach(datos)

#Modelo logístico agrupando la edad en dos intervalos
Age_g=Age
Age_g[Age<45]="[0,45)"
Age_g[Age>=45]="[45,infty)"
Age_g=factor(Age_g)
mod_logit_g=glm(CHD~Age_g,family=binomial(link="logit")) #Modelo logístico
summary(mod_logit_g) #z value es el cociente de Estimate y Std. Error
coef(mod_logit_g)
exp(coef(mod_logit_g))
#al exponenciar el intercepto, se obtiene la odds para el grupo de referencia, 
#y al exponenciar el coeficiente del otro grupo, se obtiene la odds-ratio.


#Modelo logístico sin agrupación
mod_logit=glm(CHD~Age,family=binomial(link="logit"))
summary(mod_logit)
exp(coef(mod_logit))
windows()
plot(CHD~Age,main="Ajuste logístico")
beta=coef(mod_logit)
curve(exp(beta[1]+beta[2]*x)/(1+exp(beta[1]+beta[2]*x)),add=TRUE)

#En el summary, la columna de z value son los valores del estadistico de Wald

#Intervalos de confianza para los coeficientes
confint(mod_logit)
#Al aplicar confint a un glm, construye los intervalos mediante profile likelihood.


#Intervalos de confianza para la odds y la odds-ratio
int_beta=confint(mod_logit)
int_expbeta=exp(int_beta)

