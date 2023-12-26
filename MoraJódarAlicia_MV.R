midni=7256
load("concrete.RData")
attach(concrete)

#APARTADO 1

#Definimos las variables
Y=FuerzaCompresiva
X=as.matrix(concrete[,1:9])

#Aplicamos un modelo LASSO
library(glmnet) #Cargamos la biblioteca para hacer el modelo
mod_LASSO=cv.glmnet(X,Y,family = "gaussian",alpha=1,standardize=TRUE) #Construimos el modelo
plot(mod_LASSO) #Representamos ln(λ_óptimo)
mod_LASSO$lambda.min #λ_óptimo: 0.001496027 (mínimo)
log(mod_LASSO$lambda.min) #=0.03023851
coef(mod_LASSO,s=mod_LASSO$lambda.min)
plot(mod_LASSO$glmnet.fit,xvar="lambda") #Evolución de los coeficientes en función de ln(λ)

#El modelo tiene los siguientes coeficientes:
#(Intercept)  63.560941443
#Cemento       0.082571500
#Escoria       .          
#Ceniza        0.071064881
#Agua         -0.148644197
#SP            0.133746707
#Ag.Grueso    -0.025671371
#Ag.Fino      -0.007342394
#Asentamiento -0.183071439
#Flujo         0.050379394

#Podemos observar que se ha eliminado la variable Escoria, por no ser lo bastante 
#significativa. Debe de ser muy poco significativa, pues en el gráfico de la evolución 
#de los coeficientes para el λ_óptimo nunca parece apenas variar de 0 para cualquier 
#λ visible en el gráfico. (Parece dar igual la penalización)



#APARTADO 2
#Calculamos las componentes principales de las variables de la composición del hormigón
comp_pr=princomp(concrete[,c(2,1:7)]); comp_pr
#Para interpretar aquellas que conjuntamente expliquen al menos un 80% de la variabilidad
total, haremos:
summary(comp_pr,loadings=TRUE) #La variabilidad explicada acumulada está en la fila
#"Cumulative Proportion". La 1ª componente explica un 38.81845%, las 2 primeras en su 
#conjunto, un 65.09299%, y las 3 primeras en su conjunto, un 86.05571%. 
#Los datos con mayor 1ª componente principal se caracterizan porque ese hormigón 
#se compone principalmente por Cemento, un poco de Escoria y un poco de Ag.Fino, sin embargo
#le quitamos bastante Ceniza y bastate Ag.Grueso.
#Los datos con mayor 2ª componente principal se caracterizan porque en ese hormigón tiene
#mucha Escoria, pero se le quita Cemento y Ag.Fino
#Los datos con mayor 3ª componente principal se caracterizan por que ese hormigón se compone
#por bastante Ag.Grueso, mucho Cemento, y le quitamos bastante Ceniza y Ag. Fino. 
#(Esta interpretación se debe a los signos de cada variable en los loadings que se han
#calculado. El resto de variables no intervienen significativamente en las 3 primeras 
#componentes principales).