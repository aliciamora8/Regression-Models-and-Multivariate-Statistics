ca<-c(750,800,850,900,950,1000,1050,1100,1150,1200)
d<-c(280, 300,440,350,470,320, 380, 450, 500, 360)
hueso<-c(1.06,1.08,1.04,1.04,1.15,1.04,1.15,1.21,1.33,1.21)

#Modelo sin interacción
g1=lm(hueso~ca+d)
summary(g1)

#Modelo con interacción
cad=ca*d
g2a=lm(hueso~ca+d+cad)
summary(g2)
#Tambien se puede expresar el mismo modelo de otras dos formas
g2b<-lm(hueso~ca+d+ca:d)
g2c<-lm(hueso~ca*d)

#Selección de variables
g2r=step(g2c) 
#Pero si usamos g2a, no detecta que haya interaccion, ya que son tres variables
step(g2a)
#Como conclusión, si se desea respetar el carácter jerárquico del modelo, se debe emplear
#la notación ca:d o ca*d a la hora de definir el modelo, antes de aplicar la función step.