# Implementation of the methodology with fake generated data.

# install.packages("depth")
# install.packages("fda.usc")
# install.packages("ddalpha")
# install.packages("DepthProc")

library(fda.usc)
library(ggplot2)
library(depth)
library(DepthProc)
# library(MASS)

n <- 400
set.seed(42)

s1 <- mvrnorm(n, c(1,1), diag(2)*3)
s2 <- mvrnorm(n, c(0,0), diag(2))

s <- rbind(s1,s2) # concatenamos

depx <- numeric(2*n)  # vector de profundidad en X
depy <- numeric(2*n)

for(i in 1:400) {
  
  depx[i]<- depthMah(s[i,],s1) # tomamos cada fila de s y medimos la 
  # profundidad en s1
  depy[i]<- depthMah(s[i,],s2) # tomamos cada fila de s y medimos la 
  # profundidad en s2
  
}

g<-c(rep(1,n),rep(0,n))

appended<-data.frame(cbind(depx,depy),g)

dev.new()
ggplot(appended,aes(x=depx,y=depy,color=g)) + geom_point() +
  
  coord_cartesian(xlim = c(0,1),ylim = c(0,1)) +
  
  geom_abline(intercept = 0,slope = 1) +
  
  xlab("Profundidad con respecto a X") +
  
  ylab("Profundidad con respecto a Y")

library(class)
library(fda.usc)
library(ddalpha)
nb<-194
data("biomed")
iib<-sample(1:nb,ceiling(nb*0.80))  # seleccionamos aleatoriamente el 80% de 
#las obs para entren
gb.train<-factor(biomed[iib,5]) # identificamos los grupos en la muestra de entrenam
biomed.train<-biomed[iib,1:4] # Entrenamos con el 80% de las observaciones
# seleccionadas aleatoriamente 
biomed.test<-biomed[-iib,1:4] # el 20% de observaciones restantes como prueba
gb.test<-factor(biomed[-iib,5]) # identificamos los grupos en la muestra de prueba      
library(class)
set.seed(1)
knn.pred=knn(biomed.train,biomed.test,gb.train,k=4)
table(knn.pred,gb.test)
mean(knn.pred==gb.test)

