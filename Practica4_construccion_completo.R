
# TEMA 3. CONSTRUCCIÓN DUN MODELO DE REGRESIÓN

# Práctica 4.

bridge <- read.table("bridge.txt", header=TRUE)
head(bridge)
attach(bridge)

# Representamos as seis variables para constatar
# que son positivas e asimétricas:

windows()
par(mfrow=c(2,3))
for (k in 1:6){
  plot(bridge[,k+1], pch=19, main=names(bridge)[k+1])
}
# Observacións máis ben concentradas en valores
# baixos.

# A transformación logarítmica emprégase para simetrizar variables positivas
# coma neste caso.


# 1. 

modelo <- lm(log(Time) ~ log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))
summary(modelo)

# Coeficiente de determinación R2 = 0.7762 aceptablemente alto.
# Nivel crítico do F-test 1.043e-11 moi significativo, as variables
# parecen servir para explicar a resposta.

# Significación: (3 problemas)
# log(Dwgs) moi significativo (o número de planos necesarios)
# O resto non significativas contra o que cabería esperar.
# Máis aínda, as estimacións dos coeficientes asociados a 
# DArea (área baixo a ponte) e Length (lonxitude da ponte) son 
# negativos, contraintuitivo.
# Ademais as estimacións dos erros típicos vemos que son en xeral bastante
# altas.



# 2. 
# Realizamos unha análise para ver se hai problemas de colinealidade:
  
  # Sospeitamos porque:
  # - Falta de significación dalgúns coeficientes.
  # - Estimacións dos erros típicos moi altos.
  # - (Podería explicar os signos pouco intuitivos, pero isto pode ser por
  #   outras causas).
  
  # Vexamos como son as correlacións entre as covaribles:
  x <- cbind(log(DArea),log(CCost),log(Dwgs),log(Length),log(Spans))
  colnames(x)=c("logDArea","logCCost","logDwgs","logLength","logSpans")
  round(cor(x),3)
  # Vemos que as correlacións entre todas as variables son moi altas,
  # todas nos están dando información semellante sobre o modelo.
  # Parece razoable sospeitar de problemas de colinealidade no axuste.
  
  # FACTOR DE INFLACIÓN DA VARIANZA:
  # FIV asociados a todas as covariables.
  VIF=c()
  VIF["logDArea"] <- 1 / ( 1 - cor( log(DArea),fitted(lm(log(DArea)~log(CCost)+log(Dwgs)+log(Length)+log(Spans)) ) )^2 )
  # Estamos empregando a relación entre R2 e o coeficiente de correlación múltiple
  # do axuste da variable en cuestión sobre as demais.
  VIF["logCCost"] <- 1 / ( 1 - cor( log(CCost),fitted(lm(log(CCost)~log(DArea)+log(Dwgs)+log(Length)+log(Spans)) ) )^2 )
  VIF["logDwgs"] <- 1 / ( 1 - cor( log(Dwgs),fitted(lm(log(Dwgs)~log(DArea)+log(CCost)+log(Length)+log(Spans)) ) )^2 )
  VIF["logLength"] <- 1 / ( 1 - cor( log(Length),fitted(lm(log(Length)~log(DArea)+log(CCost)+log(Dwgs)+log(Spans)) ) )^2 )
  VIF["logSpans"] <- 1 / ( 1 - cor( log(Spans),fitted(lm(log(Spans)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)) ) )^2 )
  VIF
  # Todos os valores son bastante altos, tres deles superan o 
  # umbral preocupante de 5.
  
  # Existe un problema de colinealidade neste modelo.
  
  
# 3.
# Simplificamos o modelo empregando un método de selección de variables
# global.
  
  # Xa teñen en conta o balance entre o axuste e a complexidade do
  # modelo (p).
  
  # Criterio de Información de Akaike (AIC)
  # Método backward: parto do modelo complexo e vou quitando
  # termos buscando minimizar o AIC
  
  step(modelo)
  # Interpretación
  
  modelo_simplificado <- step(modelo)
  summary(modelo_simplificado)  
  
  # REVISAR EXEMPLOS APUNTES:
  # - A función step respeta a construcción xerárquica
  # - Se construimos o termo fóra do función non porque trata
  #   as variables como se foran distintas.
  

# 4. 
  
  # Antes de proceder coa validación e diagnose hai que comprobar se solucionamos
  # o problema de colinealidade.
  
  # Vexamos como son as correlacións entre as covariables:
  # (xa a vimos antes)
  x <- cbind(log(CCost),log(Dwgs),log(Spans))
  colnames(x)=c("logCCost","logDwgs","logSpans")
  round(cor(x),3)
  # Como vimos antes, as correlacións son altas.
  
  # FACTOR DE INFLACIÓN DA VARIANZA:
  VIF=c()
  VIF["logCCost"] <- 1 / ( 1 - cor( log(CCost),fitted(lm(log(CCost)~log(DArea)+log(Dwgs)+log(Length)+log(Spans)) ) )^2 )
  VIF["logDwgs"] <- 1 / ( 1 - cor( log(Dwgs),fitted(lm(log(Dwgs)~log(DArea)+log(CCost)+log(Length)+log(Spans)) ) )^2 )
  VIF["logSpans"] <- 1 / ( 1 - cor( log(Spans),fitted(lm(log(Spans)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)) ) )^2 )
  VIF
  # Segue habendo un problema de colinealidade, vemos que o VIF
  # asociado a log(CCost), o coste de construcción, supera o valor
  # umbral: 5.
  
  # Habería que seguir simplificando ata acabar co problema de colinealidade.
  # Como co método empleado na función step non podemos seguir simplificando 
  # podemo probar con outro criterio de axuste global:
  
  # R2 axustado: o mellor é o que temos
  summary(modelo_simplificado) # R2 axustado = 0.7582
  
  m1 <- lm(log(Time) ~ log(Dwgs) + log(Spans))  
  summary(m1) # Peor R2 ajustado
  
  m2 <- lm(log(Time) ~ log(CCost) + log(Spans))  
  summary(m2)
  
  m3 <- lm(log(Time) ~ log(CCost) + log(Dwgs))  
  summary(m3)
  
  # Como recurrindo a criterios de axuste global non solucionamos o problema
  # empregaremos criterios de significación individual:
  summary(modelo_simplificado)
  
  # Imos sacar do modelo a covariable menos significativa (que ademais é
  # a mesma que amosa un VIF preocupantemente alto). Sacamos por tanto
  # a variable log(CCost).
  
  modelo_simplificado2 <- lm(log(Time) ~ log(Dwgs)+log(Spans))
  summary(modelo_simplificado2) # Agora non hai falta de significación.
  
  # Comprobamos novamente as correlacións e o VIF:
  x <- cbind(log(Dwgs),log(Spans))
  colnames(x)=c("logDwgs","logSpans")
  round(cor(x),3)
  
  # FACTOR DE INFLACIÓN DA VARIANZA:
  VIF=c()
  VIF["logDwgs"] <- 1 / ( 1 - cor( log(Dwgs),fitted(lm(log(Dwgs)~log(DArea)+log(CCost)+log(Length)+log(Spans)) ) )^2 )
  VIF["logSpans"] <- 1 / ( 1 - cor( log(Spans),fitted(lm(log(Spans)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)) ) )^2 )
  VIF
  # Agora non hai problema de colinealidade.
  
  
# Validación e diagnose:
  
  bptest(modelo_simplificado2) 
  hmctest(modelo_simplificado2) # Pasa homocedasticidade cos dous tests
  
  shapiro.test(rstandard(modelo_simplificado2))
  shapiro.test(rstudent(modelo_simplificado2)) # Pasa normalidade cos dous
  
  resettest(modelo_simplificado2) # Depende do nivel de significación
  harvtest(modelo_simplificado2) # Non rexeito (pasa linealidade)
  
  windows()
  plot(modelo_simplificado)
  # A observación máis influinte é a 22.
  
# Vemos que en xeral se cumpren todas as hipótesis co que podemos dar por
# validado o modelo axustado.
  
  
  
  
  


