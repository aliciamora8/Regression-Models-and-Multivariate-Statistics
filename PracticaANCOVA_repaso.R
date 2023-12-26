
# PRÁCTICA ANCOVA REPASO

################################################################################
# Parte 1: Modelo ANCOVA con 3 grupos:
################################################################################

library(datarium)
data(anxiety)
head(anxiety)

# Apartado 1.

modelo1=lm(anxiety$t3 ~ anxiety$group + anxiety$t1) # Sin interacción
summary(modelo1)

# Veamos si tiene sentido o no considerar la presencia
# de los grupos:

modelo2 <- lm(anxiety$t3 ~ anxiety$t1) # RLS
anova(modelo1,modelo2)
# En este caso rechazamos el modelo simple en favor
# del modelo que recoge la existencia de tres grupos.

# Veamos si tiene sentido o no considerar la variable
# t1:
modelo3 <- lm(anxiety$t3 ~ anxiety$group) # Modelo ANOVA
anova(modelo1,modelo3)
# Rechazamos el modelo que no incluye la variable t1.

# Consideramos la necesidad de meter un término de interacción:

modelo4=lm(anxiety$t3 ~ anxiety$group*anxiety$t1) # Con interacción
summary(modelo4)

# En el caso del modelo con interacción la diferencia
# en el intercepto no es significativa (solo a nivel 0.05
# entre el grupo de referencia y el grupo 3). El efecto 
# de la variable t1 es muy significativo pero las diferencias
# entre grupos no.

# Contrastamos si es necesario meter la interacción:
anova(modelo1,modelo4)
# No podemos rechazar H0: el modelo más simple (sin interacción)

# Concluimos que el efecto del nivel de ansiedad basal
# (t1) podemos asumir que es igual en los tres grupos,
# (es decir, ajustamos rectas paralelas en cada grupo,
# solo varía el intercepto).

# Gráficamente:
# Represento las observaciones del grupo 1 en negro, las del grupo 2 en azul y 
# las del grupo 3 en rojo.
plot(anxiety[anxiety$group=="grp1",]$t1, anxiety[anxiety$group=="grp1",]$t3, xlim=c(12,20), ylim=c(10,20), pch=19, xlab="t1", ylab="t3")
points(anxiety[anxiety$group=="grp2",]$t1, anxiety[anxiety$group=="grp2",]$t3, pch=19, col="blue")
points(anxiety[anxiety$group=="grp3",]$t1, anxiety[anxiety$group=="grp3",]$t3, pch=19, col="red")

# Representamos ahora en el color correspondiente la recta ajustada en cada grupo
# por el modelo seleccionado, un ancova sin interacción:
mu_hat <- coef(modelo1)[1] 
alfa2_hat <- coef(modelo1)[2] 
alfa3_hat <- coef(modelo1)[3]
gamma_hat <- coef(modelo1)[4]

abline(a=mu_hat, b=gamma_hat, lty=2) # Grupo grp1 (de referencia)
abline(a=mu_hat + alfa2_hat, b=gamma_hat, lty=2, col="blue") # Grupo grp2
abline(a=mu_hat + alfa3_hat, b=gamma_hat, lty=2, col="red") # Grupo grp3


# Apartado 2.

modelo5 <- lm(anxiety$t3 ~ anxiety$group + anxiety$t1 + anxiety$t2) # Sin interacción
summary(modelo5) # Sin interacción

modelo6 <- lm(anxiety$t3 ~ anxiety$group*anxiety$t1 + anxiety$group*anxiety$t2) 
summary(modelo6) # Con interacción de t1 y t2 con la covariable categórica

modelo7 <- lm(anxiety$t3 ~ anxiety$group*anxiety$t1 + anxiety$t2)
# Con interacción solo en t1

modelo8 <- lm(anxiety$t3 ~ anxiety$group*anxiety$t2 + anxiety$t1)
# Con interacción solo en t2

anova(modelo5,modelo6)
# No rechazo el modelo simple, sin interacción.
anova(modelo5, modelo7)
anova(modelo5, modelo8)
# En ambos casos nos quedamos con el modelo sin interacción.

anova(modelo1,modelo5)
# A la vista del p-valor del test F rechazamos la hipótesis nula para cualquiera
# de los niveles de significación habituales, preferimos el modelo que incluye
# la covariable t2.

# Se podría contrastar también frente al mejor modelo que solo incluya como
# covariable continua a t2.

# Ejercicio: ¿cómo sería la recta ajustada en cada grupo por el modelo ancova
# con dos covariables cuando en ambas hay interacción con la covariable
# categórica (modelo6)? ¿Podrías representar el ajuste para cada grupo? Si es 
# así hazlo.
# Repite este ejercicio para el modelo4 (ancova con una sola covariable
# continua e interacción).



################################################################################
# Parte 2: simulación del modelo ANCOVA
################################################################################

grupo <- c(rep("A",20),rep("B",30))
x <- c(seq(from = 0, to = 1, length.out = 20),seq(from = 0, to = 1, length.out = 30))

set.seed(123)
y_grupoA <- 1 + 2*x[grupo=="A"] + rnorm(20, sd=0.5)
y_grupoB <- 2 + 2*x[grupo=="B"] + rnorm(30, sd=0.5)
y <- c(y_grupoA,y_grupoB)  

windows()
plot(x[grupo=="A"], y_grupoA, pch=19, main="Situación de partida")  
points(x[grupo=="B"], y_grupoB, pch=19, col=2)  

modelo_si <- lm(y ~ grupo + x) # Sin interacción
summary(modelo_a)

modelo_ci <- lm(y ~ grupo*x) # Con interacción
summary(modelo_a_2)

anova(modelo_a_si, modelo_a_ci) # Nos quedaríamos con el modelo sin interacción,
# es decir, podríamos un modelo en el que estaríamos ajustando una recta en
# cada grupo con distinto intercepto y la misma pendiente.

  
# 2.a
# Aumenta la variabilidad, manteniéndola igual en ambos grupos.

# Al aumentar la variabilidad:
set.seed(123)
y_grupoA <- 1 + 2*x[grupo=="A"] + rnorm(20, sd=2)
y_grupoB <- 2 + 2*x[grupo=="B"] + rnorm(30, sd=2)
y <- c(y_grupoA,y_grupoB)  

windows()
par(mfrow=c(2,2))
plot(x[grupo=="A"], y_grupoA, pch=19, main="Apartado a")  
points(x[grupo=="B"], y_grupoB, pch=19, col=2) 

# Vemos que las observaciones de ambos grupos ahora están más entremezcladas,
# parece que será más difícil diferenciarlas.

modelo_a_si <- lm(y ~ grupo + x)
summary(modelo_a_si)

# Al aumentar la variabilidad perdemos significación (comparar con el summary
# del modelo_si). En particular vemos que las diferencias en los interceptos
# entre los grupos pasa de ser significativamente distinta de 0 en el modelo_si
# a no ser significativamente distinta de 0 en el modelo_a_si (pval = 0.24117).

modelo_a_ci <- lm(y ~ grupo * x)
summary(modelo_a_ci)

# Conclusiones similares, no parece justificado meter interacción en el modelo.

anova(modelo_a_si, modelo_a_ci)

# Sería conveniente contrastar la necesidad de incluir la covariable categórica
# en este caso.

# 2.b
# Aumenta la variabilidad en el primer grupo y disminúyela en el segundo.
# Probamos sd de 2 y 0.1.

set.seed(123)
y_grupoA <- 1 + 2*x[grupo=="A"] + rnorm(20, sd=2)
y_grupoB <- 2 + 2*x[grupo=="B"] + rnorm(30, sd=0.1)
y <- c(y_grupoA,y_grupoB) 

plot(x[grupo=="A"], y_grupoA, pch=19, main="Apartado b")  
points(x[grupo=="B"], y_grupoB, pch=19, col=2)  

# Aunque se aprecia una diferencia en el comportamiento de las observaciones 
# en ambos grupos, esta diferencia no afecta tanto al intercepto y pendiente
# de las rectas que podríamos ajustar en cada grupo (si tuviéramos que ajustar
# una recta en cada grupo, ambos ajustes serían parecidos, lo que hace que
# nos sea difícil captar las diferencias entre grupos usando un ANCOVA).

modelo_b_si <- lm(y ~ grupo + x)
summary(modelo_b_si)
# Nuevamente se reduce la significación de la diferencia entre los interceptos
# de ambos grupos con respecto a la situación de partida, conclusiones similares
# al apartado anterior.

modelo_b_ci <- lm(y ~ grupo * x)
summary(modelo_b_ci)

anova(modelo_b_si, modelo_b_ci)

# Mismas conclusiones que en el apartado anterior.

# 2.c
# Disminuye la diferencia entre interceptos y comprueba qué ocurre con los 
# p.valores asociados.
set.seed(123)
y_grupoA <- 1 + 2*x[grupo=="A"] + rnorm(20, sd=0.5)
y_grupoB <- 1.1 + 2*x[grupo=="B"] + rnorm(30, sd=0.5)
y <- c(y_grupoA,y_grupoB)
# Nuevamente parece que no seremos capaces de diferenciar la
# existencia de dos grupos viendo su efecto en el intercepto y
# la pendiente

plot(x[grupo=="A"], y_grupoA, pch=19, main="Apartado c")  
points(x[grupo=="B"], y_grupoB, pch=19, col=2)

modelo_c_si <- lm(y ~ grupo + x)
summary(modelo_c_si)

# De nuevo las diferencias en el intercepto son menos significativas que en la
# situación de partida

modelo_c_ci <- lm(y ~ grupo * x)
summary(modelo_c_ci)

anova(modelo_c_si, modelo_c_ci)

# Conclusiones similares al apartado a.

# 2.d
set.seed(123)
y_grupoA <- 1 + 2*x[grupo=="A"] + rnorm(20, sd=0.5)
y_grupoB <- 1.1 + 2*x[grupo=="B"] + 10*x[grupo=="B"]^2 + rnorm(30, sd=0.5)
y <- c(y_grupoA,y_grupoB)

plot(x[grupo=="A"], y_grupoA, ylim=c(0,13.5), pch=19, main="Apartado d")  
points(x[grupo=="B"], y_grupoB, pch=19, col=2)

# Por como xeramos os datos é evidente que neste caso será preferible un
# axuste que nos permita variar a pendente en ambos grupos.

modelo_d_si <- lm(y ~ grupo + x)
summary(modelo_d_si) # Agora as diferencias no intercepto entre os dous grupos
# seguen sendo moi significativas

modelo_d_ci <- lm(y ~ grupo*x)
summary(modelo_d_ci)
# Agora as diferenzas na pendente entre grupos tamén son moi significativas

anova(modelo_d_si, modelo_d_ci)

# Ahora preferimos el modelo con interacción como es
# lógico (gráficamente ya se ve que ajustes lineales
# en los dos grupos tendrían pendientes distintas)

# Un exemplo menos esaxerado poderíamolo obter co seguinte código:
set.seed(123)
y_grupoA <- 1 + 2*x[grupo=="A"] + rnorm(20, sd=0.5)
y_grupoB <- 1.1 + 2*x[grupo=="B"] + x[grupo=="B"]^2 + rnorm(30, sd=0.5)
y <- c(y_grupoA,y_grupoB)



  
  