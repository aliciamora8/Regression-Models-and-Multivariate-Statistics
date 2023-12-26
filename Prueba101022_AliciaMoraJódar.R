datos <- read.table("C:/Users/alici/Desktop/Universidad/4o Curso/Modelos de Regresión/Tema 1/diamantes.txt",header=TRUE)
attach(datos)
set.seed(7766) 
price <- price + rnorm(200,sd=40)

#Apartado 1

modelo <- lm(price~x+y+depth)
summary(modelo)

#El coeficiente de la variable profundiad es 280.9
#Al ser positivo, significa que aumenta, y por su valor
#nos indicara que, por cada unidad que aumenta la profundidad, 
#el precio aumenta un 280.9 por ciento.
#Ademas, el coeficiente es significativo (significativamnte distinto de cero)


#Apartado 2

mod1 <- lm(price~x+y)
mod2 <- lm(depth~x+y)
cor(mod1$residuals,mod2$residuals) #Coeficiente de correlacion parcial entre price y depth
cov(price,depth)/sqrt(var(price)*var(depth)) #Coeficiente de correlacion simple

#Si hay fenómeno de confusión, ya que el signo del coef de correlación parcial es distinto
#al signo del coef de correlación simple.


#Apartado 3

summary(modelo)
#Se puede observar en el summary que el coeficiente de determinación es 0.8069
cor(price,fitted(modelo))^2 #También se puede calcular así

#El coeficiente de determinación es bastante cercano a 1, por lo que el modelo
#nos aportará predicciones muy precisas del precio del diamante.


#Apartado 4

n <- 100
x1 <- 0.01*(1:n)
x2 <- 0.01*(1:n)
beta1_hat <- c(2,3,2)
sigma <- 0.5
error <- rnorm(n, mean = 0, sd = sigma)
y <- 2+3*x1+2*x2+error
hist(beta1_hat, freq=F)
abline(v = mean(beta1_hat), lwd=2, col=2)