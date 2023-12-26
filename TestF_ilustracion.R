

library(datarium)
data(anxiety)
head(anxiety)


# Sin interacción:

modelo <- lm(anxiety$t3 ~ anxiety$t1 + anxiety$group)

table(anxiety$group) # I = 3
n <- dim(anxiety)[1] # n = 45
df <- n - 3 - 1; df

# Para calcular las sumas de residuos al cuadrado:
coef(modelo)
mu_hat = -0.353465 # Intercepto grupo 1 (referencia)
alpha_2_hat = -0.5458335 
alpha_3_hat = -2.8743056 
gamma_hat = 0.9867420

y_hat_grupo1 <- mu_hat + gamma_hat * anxiety[anxiety$group == "grp1",]$t1 
y_hat_grupo2 <- mu_hat + alpha_2_hat + gamma_hat * anxiety[anxiety$group == "grp2",]$t1 
y_hat_grupo3 <- mu_hat + alpha_3_hat + gamma_hat * anxiety[anxiety$group == "grp3",]$t1 
y_hat <- c(y_hat_grupo1, y_hat_grupo2, y_hat_grupo3) # Ya en orden en este caso

RSS = sum((anxiety$t3 - y_hat)^2)

# Contraste efecto grupo:

modelo0 <- lm(anxiety$t3 ~ anxiety$t1)
df0 <- n-2; df0
RSS0 <- sum((anxiety$t3 - coef(modelo0)[1] - coef(modelo0)[2]*anxiety$t1 )^2)

F = ((RSS0-RSS)/(df0-df)) / (RSS/df); F
pf(F, df0-df, df, lower.tail=FALSE)

anova(modelo0,modelo)


# Contraste covariable:

modelo0 <- lm(anxiety$t3 ~ anxiety$group)
df0 <- n - 3; df0

coef(modelo0)
y_hat_grupo1 <- 16.506667 
y_hat_grupo2 <- 16.506667  -0.980000 
y_hat_grupo3 <- 16.506667  -2.946667 
y_hat <- c(rep(y_hat_grupo1,15), rep(y_hat_grupo2,15), rep(y_hat_grupo3,15)) # Ya en orden en este caso

RSS0 = sum((anxiety$t3 - y_hat)^2)

F = ((RSS0 - RSS)/(df0-df))/(RSS/df); F

anova(modelo0,modelo)
pf(F, df0-df, df, lower.tail=FALSE)


# Modelo con interacción:

# Ahora RSS0 son los residuos del modelo sin interacción:
# H0: modelo sencillo (sin interacción)
RSS0 = RSS
df0 = df 

modelo1 <- lm(anxiety$t3 ~ anxiety$t1*anxiety$group)
summary(modelo1)

df = 45 - 2*3; df

coef(modelo1)
y_hat_grupo1 <- 1.0100851  + 0.9069400 * anxiety[anxiety$group == "grp1",]$t1
y_hat_grupo2 <- 1.0100851  -3.0730814 + (0.9069400 + 0.1497078) * anxiety[anxiety$group == "grp2",]$t1
y_hat_grupo3 <- 1.0100851  -4.6309712 + (0.9069400 + 0.1029083) * anxiety[anxiety$group == "grp3",]$t1 
y_hat <- c(y_hat_grupo1,y_hat_grupo2,y_hat_grupo3) # Ya en orden en este caso

RSS = sum((anxiety$t3 - y_hat)^2)

q = df0-df
F = ((RSS0-RSS)/q)/(RSS/df)
pf(F, df0-df, df, lower.tail=FALSE)

anova(modelo,modelo1)
