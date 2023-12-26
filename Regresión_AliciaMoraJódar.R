datos=read.table("datos_june.txt")
set.seed(7256)
ind<-sample(1:768, size=200)
datos<-datos[ind,]
attach(datos)
summary(datos)

#APARTADO 1
#Se trata de un modelo de regresión logístico cuyo ajuste se expresa:
mod=glm(test~age+bmi+glucose,family=binomial(link="logit"))
coef(mod) #Con el comando coef obtenemos los coeficientes del modelo que vamos a interpretar
#Tambien nos ayudaremos en el summary para ver su significación
summary(mod)
#El intercepto vale -8.39374304, es decir, es negativo. Esto nos indica que cuando Age=0, 
#la probabilidad de paceder la enfermedad era menor del 0,5. Pero en este caso, no es muy
#interesante este dato, ya que no tenemos datos de pacientes recien nacidos.
#Además, si observamos los niveles críticos de cada estimación de parámetro, podemos
#ver que son todos muy pequeños, lo que indica que todos los parámetros son muy significativos.
#El valor del estimador de la edad es 0.030157, el del bmi es 0.081590, y el de la glucosa
#es de 0.032512. Todos ellos son positivos, aunque valores relativamente pequeños, por lo que
#cuando se produce un aumento en la edad, el bmi y la glucosa hacen que la probabilidad de
#padecer la enfermedad vaya aumentando ligeramente.

#APARTADO 2
#Como todos los coeficientes eran muy significativos, no creo que debamos eliminar ninguna
#variable, sin embargo podemos comprobarlo meidante la funcion step 
step(mod) #Comprobando así que efectivamente nuestro modelo es el adecuado, ya que no se
#ha eliminado ninguna variable

#APARTADO 3
#PRIMER CASO
#Primero definimos el valor que toma cada variable explicativa
años=30
indice=mean(bmi)
gluc=mean(glucose)
coef=coef(mod)
predic=coef[1]+coef[2]*años+coef[3]*indice+coef[4]*gluc #Este valor será la predicción del valor
#de la variable explicativa si este ajuste siguiera un modelo lineal. Solo lo calculo a parte
#para una más clara lectura de las fórmulas posteriores
#Finalmente la probabilidad de que la variable respuesta valga 1, es decir, que se padezca
#la enfermedad, será la siguiente, por tratarse de un modelo logístico:
exp(predic)/(1+exp(predic)) #Obteniendo un valor 0.2792286, es decir, una probabilidad de
#padecer la enfermedad del 27.92286% bajo tales supuestos
#SEGUNDO CASO
#Redefinimos las dos variables
indice=quantile(x=bmi,prob=0.9) #Indica el cuantil del 90% para el indice de masa
gluc=quantile(x=glucose,prob=0.9) #Indica el cuantil del 90% para el nivel de glucosa
#Procedemos de igual forma que en el primer caso
predic=coef[1]+coef[2]*años+coef[3]*indice+coef[4]*gluc
exp(predic)/(1+exp(predic)) #Obteniendo un valor 0.7902412, es decir, una probabilidad de
#padecer la enfermedad del 79.02412% bajo tales supuestos