
# PROBA 2021

# Lectura dos datos:

datos <- read.table("fat.txt",header=TRUE)
# Se vos dan problemas os nomes das variables: check.names = F) 
# Lembrade que o arquivo de datos ten que estar no directorio de traballo.

# Podedes facer un attach:
attach(datos)

# Ou indexar directamente as variables, pode ser cómodo renomealas:
# siri <- datos$siri 
# weight <- datos$weight
# neck <- datos$neck
# abdom <- datos$abdom
# thigh <- datos$thigh

# Se quixérades renomear as variables directamente no dataframe:
# names(datos) <- c("x1","x2",...) # Vector cos novos nomes para as variables

# Aleatorización da variables resposta:
set.seed(123)
siri.new<-siri+rnorm(length(siri),mean=20,sd=0.35) # Variable resposta

# 1.

modelo <- lm(siri.new ~ weight + neck + abdom + thigh)
summary(modelo)

# Se o valor das outras covariables se manteñen constantes,
# o valor do coeficiente asociado ó peso
# indica que por cada unidade que aumenta o peso,
# a porcentaxe de graxa corporal (resposta) diminúe
# en 0.15 unidades.

# Á vista do p-valor asociado a este coeficiente, pval = 1.07e-06, 
# vemos que hai evidencias estatisticamente significativas de que
# dito parámetro é distinto de cero para calquera dos niveis
# de significación habituais (0.1, 0.5 e 0.01). Por tanto, en principio
# descartaríamos a posibilidade de eliminar esta variable do axuste.

# 2.

# O R2 vale 0.7259

# Interpretación: o modelo axustado explica un 72.59% da
# varianza total da resposta.

# Cálculo directo: coeficiente de correlación múltiple:
cor(siri.new, modelo$fitted.values)^2

# Comentarios sobre o R2 axustado:
# O R2 axustado vale 0.7215, emprégase sobre todo en comparación de modelos.
# Ten en conta o número de parámetros do modelo:
TSS <- sum((siri.new - mean(siri.new))^2)
RSS <- sum(modelo$residuals^2)
n <- nrow(model.matrix(modelo)) # length(siri) # manualmente
p <- ncol(model.matrix(modelo))
R2_ax <- 1 - (RSS/(n-p))/(TSS/(n-1)); R2_ax

# 3. TEMA 2

# 4. Validación gráfica das hipóteses do modelo:

# Só se preguntaría polas hipóteses que vimos como validar graficamente en
# clase:
# Heterocedasticidade, linealidade, (normalidade e independencia
# son máis difíciles de ver gráficamente).

