# Implementation of the methodology with fake generated data.

# install.packages("depth")
# install.packages("fda.usc")
# install.packages("ddalpha")
# install.packages("DepthProc")

rm(list=ls())
library(ggplot2)
library(depth)
library(DepthProc)
library(MASS)

################# DD-plot #################
n<-400
set.seed(42)
s1<-mvrnorm(n,c(5,5),diag(2)*8)
s2<-mvrnorm(n,c(0,0),diag(2))
s<- rbind(s1,s2)
depx<- numeric(2*n)
depy<- numeric(2*n)

for(i in 1:800) {
  
  depx[i]<- depthMah(s[i,],s1) # tomamos cada fila de s y medimos la 
  # profundidad en s1
  depy[i]<- depthMah(s[i,],s2) # tomamos cada fila de s y medimos la 
  # profundidad en s2
  
}

g<-c(rep(1,n),rep(0,n)) # creamos un vector binario n unos y n ceros
#  para diferenciar
# mas adelante el color con la funcion ggplot


# creamos un dataframe con tres columnas las dos primeras coorrepondientes
# a las profundidades y la tercera a la variable binaria
appended<-data.frame(cbind(depx,depy),g)


dev.new()
ggplot(appended,aes(x=depx,y=depy,color=g)) + geom_point() +
  
  coord_cartesian(xlim = c(0,1),ylim = c(0,1)) +
  
  geom_abline(intercept = 0,slope = 1) +
  
  xlab("Profundidad con respecto a X") +
  
  ylab("Profundidad con respecto a Y")

################ Predicción usando KNN #################
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


############## Entrenamiento y predicción con DD-Classifier e iris data #############
library('fda.usc')

data(iris) # Importamos los datos.

iris<-iris[1:100,] # Tomamos 100 entradas.

ii<-sample(1:100,80) # Dividimos 80-20 para training y testeado.

group.train<-factor(iris[ii,5]) # y de entrenamiento.
x.train<-iris[ii,1:4] # x de entrenamiento.

out1=classif.DD(group.train,x.train,depth="MhD",classif="lda") # Clasificador 1.
out2=classif.DD(group.train,x.train,depth="MhD",classif="DD3") # Clasificador 2.

summary(out1)
summary(out2)

x.test<-iris[-ii,1:4] # x de prueba.
group.test<-iris[-ii,5] # y de prueba.

# Predicciones.
pred1=predict(out1,x.test)
pred2=predict(out2,x.test)

# Matrices de confusión.
table(pred1,group.test)
table(pred2,group.test)


############## Entrenamiento y predicción con DD-Classifier y datos aleatorios #############

###### Igual media y dispersión ######
library('fda.usc')

n<-400
set.seed(42)

d1 <-mvrnorm(n,c(1,1,1),diag(3))
d2 <-mvrnorm(n,c(1,1,1),diag(3))

data<- rbind(d1,d2)

g<-c(rep(1,n),rep(0,n))

appended<-data.frame(cbind(data),g)

ii<-sample(1:800,640) # Dividimos 80-20 para training y testeado.

group.train<-factor(appended[ii,4]) # y de entrenamiento.
x.train<-appended[ii,1:3] # x de entrenamiento.

out1=classif.DD(group.train,x.train,depth="MhD",classif="lda") # Clasificador 1.
out2=classif.DD(group.train,x.train,depth="MhD",classif="DD3") # Clasificador 2.

summary(out1)
summary(out2)

x.test<-appended[-ii,1:3] # x de prueba.
group.test<-appended[-ii,4] # y de prueba.

# Predicciones.
pred1=predict(out1,x.test)
pred2=predict(out2,x.test)

# Matrices de confusión.
table(pred1,group.test)
table(pred2,group.test)


###### Igual media y diferente dispersión ######
library('fda.usc')

n<-400
set.seed(42)

d1 <-mvrnorm(n,c(1,1,1),diag(3)*5)
d2 <-mvrnorm(n,c(1,1,1),diag(3))

data<- rbind(d1,d2)

g<-c(rep(1,n),rep(0,n))

appended<-data.frame(cbind(data),g)

ii<-sample(1:800,640) # Dividimos 80-20 para training y testeado.

group.train<-factor(appended[ii,4]) # y de entrenamiento.
x.train<-appended[ii,1:3] # x de entrenamiento.

dev.new()
out1=classif.DD(group.train,x.train,depth="MhD",classif="lda") # Clasificador 1.
out2=classif.DD(group.train,x.train,depth="HS",classif="DD2") # Clasificador 2.

summary(out1)
summary(out2)

x.test<-appended[-ii,1:3] # x de prueba.
group.test<-appended[-ii,4] # y de prueba.

# Predicciones.
pred1=predict(out1,x.test)
pred2=predict(out2,x.test)

# Matrices de confusión.
table(pred1,group.test)
table(pred2,group.test)




###### Diferente media e igual dispersión ######
library('fda.usc')

n<-400
set.seed(42)

d1 <-mvrnorm(n,c(3,3,3),diag(3))
d2 <-mvrnorm(n,c(1,1,1),diag(3))

data<- rbind(d1,d2)

g<-c(rep(1,n),rep(0,n))

appended<-data.frame(cbind(data),g)

ii<-sample(1:800,640) # Dividimos 80-20 para training y testeado.

group.train<-factor(appended[ii,4]) # y de entrenamiento.
x.train<-appended[ii,1:3] # x de entrenamiento.

dev.new()
out1=classif.DD(group.train,x.train,depth="HS",classif="DD2") # Clasificador 1.
summary(out1)

out2=classif.DD(group.train,x.train,depth="LD",classif="DD2") # Clasificador 2.
summary(out2)



x.test<-appended[-ii,1:3] # x de prueba.
group.test<-appended[-ii,4] # y de prueba.

# Predicciones.
pred1=predict(out1,x.test)
pred2=predict(out2,x.test)

# Matrices de confusión.
table(pred1,group.test)
table(pred2,group.test)
###### Diferente media y dispersión ######
library('fda.usc')

n<-400
set.seed(42)

d1 <-mvrnorm(n,c(3,3,3),diag(3)*5)
d2 <-mvrnorm(n,c(1,1,1),diag(3))

data<- rbind(d1,d2)

g<-c(rep(1,n),rep(0,n))

appended<-data.frame(cbind(data),g)

ii<-sample(1:800,640) # Dividimos 80-20 para training y testeado.

group.train<-factor(appended[ii,4]) # y de entrenamiento.
x.train<-appended[ii,1:3] # x de entrenamiento.

dev.new()
out1=classif.DD(group.train,x.train,depth="HS",classif="DD3") # Clasificador 1.
summary(out1)

out2=classif.DD(group.train,x.train,depth="LD",classif="DD2") # Clasificador 2.
summary(out2)


x.test<-appended[-ii,1:3] # x de prueba.
group.test<-appended[-ii,4] # y de prueba.

# Predicciones.
pred1=predict(out1,x.test)
pred2=predict(out2,x.test)

# Matrices de confusión.
table(pred1,group.test)
table(pred2,group.test)