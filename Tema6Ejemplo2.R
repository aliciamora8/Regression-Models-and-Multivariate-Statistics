datos=read.table("lowbwt.txt",header=TRUE)
attach(datos)
low=factor(low)

mod_logit_2=glm(low~age+lwt+race+ftv,family=binomial(link="logit"))
summary(mod_logit_2)

exp(coef(mod_logit_2))

#Como age y ftv presentan coef poco significativos haremos un modelo reducido
mod_reducido=glm(low~lwt+race,family=binomial(link="logit"))
#Comparandolo con el otro
anova(mod_reducido,mod_logit_2)
#Y para ver el nivel critico, lo calculamos manualmente a partir de la deviance y
#los grados de libertad
1-pchisq(0.68618,2)
#Como el nivel critico es grande, se acepta usar el modelo simplificado

#Tambien podriamos obtenerlo de la siguiente forma
step(mod_logit_2)
#y llegamos al mismo modelo reducido


