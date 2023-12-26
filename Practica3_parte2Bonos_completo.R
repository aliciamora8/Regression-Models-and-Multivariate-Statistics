
# Practica 3: DIAGNOSE DE OBSERVACIONS ATIPICAS E INFLUINTES

# Parte 2. Exemplo bonos.


# 0. Lectura de datos.

  bonds=read.table("bonds.txt",header=TRUE)
  attach(bonds)
  
  head(bonds)
  

# 1. Axuste do modelo:
  
  plot(BidPrice ~ CouponRate, pch=19)
  # Comportamento lineal na dereita, tres puntos "raros" á esquerda
  
  modelo = lm(BidPrice ~ CouponRate)
  abline(modelo, lty=2)
  
  summary(modelo)

  
# 2. Validación:
  
  # Empregaremos as librarías:
  library(car)
  library(lmtest)
  
  # Normalidade:
  
    # Test de normalidade: empregando residuos estandarizados (ri)
    # ou estudentizados (ti).
    # H0: normalidade
  
    res <- rstandard(modelo)  # Residuos estandarizados
    # res <- rstudent(modelo) # Residuos estudentizados
  
    # Test de Shapiro-Wilk:
    shapiro.test(res)
    # Cun pvalor=0.1568 maior que os niveis de significación
    # habituais, non atopamos evidencias significativas para 
    # rexeitar a hipótese de normalidade dos residuos.
    
    # (Para os estudentizados pval=0.067 a conclusión depende do
    # nivel de significación que se fixe, para un nivel de 0.1
    # rexeitamos a hipótese de normalidade, para niveis de
    # significación máis esixentes como 0.05 ou 0.01 a conclusión
    # é a mesma que para os estandarizados).
    
    # Graficamente:
    hist(res,freq=F) # Histograma
    qqPlot(res) # qqnorm(res) # Función alternativa
    # Observacións problemáticas: 13 e 35
    plot(BidPrice ~ CouponRate, pch=19)
    points(CouponRate[c(13,35)], BidPrice[c(13,35)], col=2, pch=19)
  
  # Homocedasticidade:
    
    # Test de Breusch-Pagan: (ver presentación seminario)
    # H0: homocedast vs Ha: varianza dependente da explicativa
    bptest(modelo)
    
    # Test de Harrison-McCabe: (ver presentación seminario)
    # H0: homocedast vs Ha: varianza ten un punto de cambio.
    hmctest(modelo)

    # Gráficos de apoio:
    plot(res ~ CouponRate, pch=19)
    abline(h=0, lty=2)
    
    plot(res ~ modelo$fitted.values, pch=19)
    # Alternativa para regresión múltiple. Mesma conclusión.
    
  # Linealidade
    
    # Test de Ramsey:
    # H0: linealidade; Ha: polinomio de grao 2 ou 3
    resettest(modelo)
    
    # Test de Harvey-Collier:
    # H0: linealidade; Ha: non linealidade (concavidad/convexidade)
    harvtest(modelo)
    
    # Test contra non-paramétrica (métodos de suavizado):
    # H0: linealidade; Ha: axuste non paramétrico (segue os puntos)
    library(sm)
    sm.regression(CouponRate, BidPrice, model="linear")
    # Cun pvalor de 0.001, rexeitamos H0. Sería preferible un
    # axuste non paramétrico.
    
    # De novo as conclusións son diferentes porque o plantexamento
    # dos distintos tests é diferente.
    
    # Graficamente:
    plot(BidPrice ~ CouponRate, pch=19) # Para RLS
    plot(BidPrice ~ modelo$fitted.values, pch=19) # Xeral
    abline(a=0,b=1,lty=2) # Diagonal
    # Se hai linealidade os puntos deben estar arredor da diagonal.
    
# 3. Diagnose:
    
    # PUNTOS CON CAPACIDADE DE INFLUENCIA:
    # (Capacidade de influencia non é o mesmo que influinte)
    
      leverages <- hat(model.matrix(modelo)) # Diagonal da matriz
      # hat do modelo: h_ii
      n <- length(BidPrice)
      # Buscamos os puntos cun leverage "alto"
      which(leverages >= 4/n)
      # Observacions con capacidade de influencia alta.
      
      # Na gráfica:
      plot(BidPrice~CouponRate, pch=19)
      abline(modelo, lty=2)
      ind <- which(leverages >= 4/n)
      points(BidPrice[ind] ~ CouponRate[ind],col=2,pch=16)
      # O leverage depende dos valores de x.
      
    # ATÍPICOS:
      
      # Análise de residuos:
      res1 <- residuals(modelo) # Residuos "brutos"
      res2 <- rstandard(modelo) # Residuos estandarizados
      res3 <- rstudent(modelo) # Residuos estudentizados
      
      # Os residuos brutos non proveñen todos da mesma distribución,
      # teñen distinta varianza (depende do leverage).
      # Vexamos en que escala varían:
      hist(res1)
      rug(res1)

      # Empregamos os residuos estandarizados ou estudentizados
      # (reescalados para que todos teñan a mesma varianza).
      # Como agora consideramos que proveñen dunha N(0,1), podemos
      # considerar atípicas aquelas observacións cuxos residuos
      # están por arriba ou por abaixo do 95% de valores centrais.
      # Os cuantís para a normal estándar para esos niveis son +/-1.96
      which(abs(res2) > 1.96)
      which(abs(res3) > 1.96) 
      # As observacións atípicas serían a 13, 34 e 35.
      
      # O habitual é que identifiquemos os mesmos puntos atípicos
      # usando ambos residuos (aínda que en rigor podería non ser así).
      
      # Representámolas:
      plot(BidPrice ~ CouponRate, pch=19)
      indr <- which(abs(res2) > 1.96)
      points(BidPrice[indr] ~ CouponRate[indr], col=4, pch=16)
      abline(modelo,lty=2)
      # Vemos que son os puntos máis afastados do axuste.
      
      indt <- which(abs(res3) > 1.96)
      points(BidPrice[indt] ~ CouponRate[indt], col=4, pch=16)
      abline(modelo,lty=2)
      
    # INFLUINTES:
      
      # Distancia de Cook: (ver presentación seminario)
      # Ten en conta os residuos estandarizados (diferencia entre obs
      # e o predito polo modelo) e o leverage (capacidade de influencia).
      # Valores preocupantes son os que superan a mediana dunha F_{p,n-p}
      
      cooks.distance(modelo)
      round(cooks.distance(modelo),3)
      # As máis grandes son as das observacións 13, 35 e 4.
      
      # Vexamos cales son influintes (i.e. o suficientemente grandes)
      med <- qf(0.5, 2, n-2)
      which(cooks.distance(modelo) > med)
      # Só temos unha observación influinte, a 13.
      
      # Graficamente:
      hist(cooks.distance(modelo))
      rug(cooks.distance(modelo))      
      # Cook tamén indica que se deben ter en conta os puntos que
      # se afastan dos demais, observacións 13, 35 e 4.
      
      # Vemos cales son estas observacións:
      plot(BidPrice ~ CouponRate, pch=19)
      indc <- which(cooks.distance(modelo) > med) # Obs influintes
      indp <- c(4, 13, 5) # Obs das que convén estar pendentes
      points(BidPrice[indp] ~ CouponRate[indp], col="orange", pch=16)
      points(BidPrice[indc] ~ CouponRate[indc], col="red", pch=16)
      

# GRÁFICAS DE APOIO: VALIDACIÓN E DIAGNOSE
      
  windows()
  par(mfrow=c(2,2))
  plot(modelo)      
  
  
# CONCLUSIÓNS DA VALIDACIÓN E DIAGNOSE:
  
  # Despois do estudo anterior, vemos que as observacións que parecen
  # dar problemas en conxunto no modelo son as observacións 4, 13, 34 
  # e 35.
  
  # Tendo iso en conta e tamén o diagrama de dispersión inicial
  par(mfrow=c(1,1))
  plot(BidPrice ~ CouponRate, pch=19)
  abline(modelo, lty=2)
  points(CouponRate[c(4,13,34,35)],BidPrice[c(4,13,34,35)], col=2, pch=19)
  
  # Das observacións problemáticas, vemos que as tres da esquerda
  # amosan un comportamento claramente diferenciado do resto de
  # puntos.
    
# A maneira de proceder en xeral é a seguinte:
  
  # 1. Axuste do modelo
  # 2. Diagnose de atípicos e influintes -> medidas
  # 3. Validación -> ¿medidas?
  
  # Neste caso fixemos primeiro a validación para mostrar como as
  # observacións influintes ou atípicas poden condicionar o cumpri
  # mento das hipóteses.
  
  # Agora axustaremos un novo modelo eliminando as tres observacións
  # problemáticas. Faremos unha nova diagnose de atípicos e influintes
  # e por último validaremos o modelo.
  

# 4. Novo axuste:
  
  n <- n-3
  ind <- (CouponRate > 6)
  
  BidPrice_r <- BidPrice[ind]
  CouponRate_r <- CouponRate[ind]

  windows()
  plot(BidPrice_r ~ CouponRate_r, pch=19) # Comparar co anterior
  modelo2 <- lm(BidPrice_r ~ CouponRate_r)    
  abline(modelo2, lty=2)    
    
  # DIAGNOSE DE ATÍPICOS E INFLUINTES:
    
    # Observacións con capacidade de influencia:
    leverages <- hat(model.matrix(modelo2))
    hist(leverages); rug(leverages)
    
    # ATÍPICOS:
    which(abs(rstandard(modelo2))>1.96) # 32
    which(abs(rstudent(modelo2))>1.96) # 4, 32
    
    # INFLUINTES:
    which(cooks.distance(modelo2) > qf(0.5,2,n-2))
    # Non se detectan influintes.
    
  # GRÁFICOS DE VALIDACIÓN E DIAGNOSE:
    
    windows()
    par(mfrow=c(2,2))
    plot(modelo2)
    
    # A observación 4 é case influinte. Poderíase argumentar a favor
    # de descartala ou mantela.
    
  # VALIDACIÓN:
    # Ademais de apoiarnos nas gráficas anteriores:
    
    bptest(modelo2);hmctest(modelo2) #pasamos homocedasticidade
    reset(modelo2);harvtest(modelo2) #pasamos linealidade
    shapiro.test(rstandard(modelo2)) #non pasamos normalidade para 0.05 ou 0.01
    shapiro.test(rstudent(modelo2)) #non pasamos normalidade
    hist(rstudent(modelo2))
    qqPlot(modelo2) #Parece que o problema está nos puntos 4-32
    # (pódese ver tamén na gráfica QQPlot dos gráficos de validación
    # e diagnose). Víamos que eran atípicos.
    
    # Podémonos plantexar eliminar estes dous puntos da mostra.
    
# 5. NOVO AXUSTE
    
    ind<-c(4,32)
    BidPrice_r2=BidPrice_r[-ind]
    CouponRate_r2=CouponRate_r[-ind]
    
    windows()
    plot(BidPrice_r2~CouponRate_r2, pch=19)
    m3=lm(BidPrice_r2~CouponRate_r2, lty=2)
    abline(m3)
    
    leverages=hat(model.matrix(m3))
    which(abs(rstandard(m3))>1.96)
    which(abs(rstudent(m3))>1.96)
    #Identificamos atípicos, 1, 24 e 28
    
    n<-n-2
    which(cooks.distance(m3)>qf(0.5,2,n-2))
    #Non se detectan puntos influ?ntes.
    
    windows()
    par(mfrow=c(2,2))
    plot(m3)
    par(mfrow=c(1,1))
    
    #Validaci?n
    bptest(m3);hmctest(m3) #pasamos homocedasticidade con HMC
    reset(m3);harvtest(m3) #pasamos linealidade
    
    #Pero non temos normalidade...
    shapiro.test(rstandard(m3)) #pasamos normalidade
    shapiro.test(rstudent(m3)) #pasamos normalidade
    hist(rstudent(m3))
    qqPlot(m3) # 1, 24 (atípicos)
    
# Outras ferramentas de apoio:
    
    dfbetas(modelo)
    # Graficamente:
    influencePlot(modelo)

    
    
    
    