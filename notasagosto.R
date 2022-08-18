rm(list=ls())
#############################################################################
#GRAFICO DE CONTORNO
#############################################################################
library(depth)
library(ddalpha)
data("hemophilia")
dep <- numeric(75) 

for (i in 1:75){ dep[i] <- depth(hemophilia[i,1:2],hemophilia[,1:2] , method='Liu') } 

#con este comando calculamos por ejemplo la profundidad de liu para cada uno de los individuos.
sort(dep,decreasing = TRUE,index.return=TRUE)

library(ddalpha)
dev.new()
par(mfrow=c(2,2))
rownames(hemophilia)
plot(hemophilia[,1:2])
text(hemophilia[,1:2],labels = abbreviate(rownames(hemophilia)))
for(depth in c("Mahalanobis","halfspace","simplicial")){
  z<-depth.contours(hemophilia[,1:2],depth = depth,main = depth)
}
#############################################################################
## CURVA DE ESCALA
############################################################################
n<-200
set.seed(42)
library(MASS)
library(DepthProc)
s1<-mvrnorm(n,c(0,0),9*diag(2))
s2<-mvrnorm(n,c(0,0),diag(2))
dev.new()
scaleCurve(s1,s2,name="s1",name_y ="s2")



########################################################################
#  DD-plot
######################################################################
rm(list=ls())
library(ggplot2)
library(depth)
library(DepthProc)
library(MASS)
n<-200
set.seed(42)
s1<-mvrnorm(n,c(0,0),diag(2))
s2<-mvrnorm(n,c(0,0),diag(2))  # parametros iguales
#s2<-mvrnorm(n,c(2,0),diag(2))  #cambio en localizacion
#s2<-mvrnorm(n,c(0,0),9*diag(2)) # cambio en dispersion
s<- rbind(s1,s2) # concatenamos
depx<- numeric(2*n)  # vector de profundidad en X
depy<- numeric(2*n)  # vector de profundidad en Y
# calculamos profundidades

for(i in 1:400) {
  
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

appended<-data.frame(cbind(depx,depy),g) # los unos hacen referencia
# a las profundidades en grupo 1
# y  grupo2(azul claro) de las primeras n1 obs
# y los ceros a las profundiades
# grupo 1 y grupo2  de n2 observaciones(azul oscuro)

# vamos a graficar usando ggplot.

#(usando la función aes() dentro de ggplot()) para seleccionar las 
#variables a graficar y especificar cómo deben ser presentadas, 
#por ejemplo, en los ejes x/y o como característica tales como tamaño, 
#forma, color, etc. aes() le dice a ggplot cómo se relaciona cada una de 
#las variables en los datos con las propiedades aesthetic (estéticas) 
#de la figura , 
#geom_point() = para representar visualmente la relación 
#entre x y y como un gráfico de dispersión de puntos
# coord_cartesian() establecemos sistema de coordenadas cartesiano

# geom_abline() # trazamos una linea diagonal, con intercepto en el origen

# y pendiente 1

dev.new()
ggplot(appended,aes(x=depx,y=depy,color=g)) + geom_point() +
  
  coord_cartesian(xlim = c(0,1),ylim = c(0,1)) +
  
  geom_abline(intercept = 0,slope = 1) +
  
  xlab("Profundidad con respecto a X") +
  
  ylab("Profundidad con respecto a Y")


#############################################################################################
#Consiste en cuatro mediciones de sangre diferentes para 127(2)
#personas normales y 67(1) portadores de un trastorno genético raro
library(fda.usc)
library(ddalpha)
library(lattice)
nb<-194
data("biomed")
set.seed(42)
iib<-sample(1:nb,ceiling(nb*0.80))  # seleccionamos aleatoriamente el 80% de 
#las obs para entren
gb.train<-factor(biomed[iib,5]) # identificamos los grupos en la muestra de entrenam
biomed.train<-biomed[iib,1:4] # Entrenamos con el 80% de las observaciones
# seleccionadas aleatoriamente 
biomed.test<-biomed[-iib,1:4] # el 20% de observaciones restantes como prueba
gb.test<-factor(biomed[-iib,5])       # identificamos los grupos en la muestra de prueba

#table(predb1,gb.test)
#tc<-sum(diag(table(predb1,gb.test)))/sum(table(predb1,gb.test))
###################################################################################################
library(MASS)
lda.fit=lda(biomed.train,gb.train)

lda.fit 
dev.new()
plot(lda.fit) # produce un histograma de la funcion discriminante para cada uno de los grupos
lda.pred=predict(lda.fit,biomed.test)
names(lda.pred)
lda.class=lda.pred$class          # predicciones acerca de la pertencencia a un grupo
lda.posterior=lda.pred$posterior  # una matriz cuya k-esima columna contiene la probabilidad
# posterior de que la correspondiente observacion pertenezca
# a la k-esima clase
lda.scores= lda.pred$x            # scores para los datos de prueba

table(lda.class,gb.test)   # matriz de confusion
tc<-mean(lda.class==gb.test)   

#***************************ANALISIS DISCRIMINANTE CUADRATICO**************************************************
qda.fit=qda(biomed.train,gb.train)

qda.fit 
# produce un histograma de la funcion discriminante para cada uno de los grupos
qda.pred=predict(qda.fit,biomed.test)
names(qda.pred)
qda.class=qda.pred$class          # predicciones acerca de la pertencencia a un grupo
qda.posterior=qda.pred$posterior  # una matriz cuya k-esima columna contiene la probabilidad
# posterior de que la correspondiente observacion pertenezca
# a la k-esima clase
qda.scores= qda.pred$x            # scores para los datos de prueba

table(qda.class,gb.test)   # matriz de confusion
tc<-mean(qda.class==gb.test)   
#Esto sugiere que  la forma cuadrática asumida por QDA puede capturar la 
# verdadera relación con mayor precisión que las formas lineales asumidas por LDA 
####################################################################################################
rm(list = ls())
data(iris)
ni<-150
set.seed(42)
iii<-sample(1:ni,ceiling(ni*0.80))  # seleccionamos aleatoriamente el 80% de 
#las obs para entren
gi.train<-factor(iris[iii,5]) # identificamos los grupos en la muestra de entrenam
iris.train<-iris[iii,1:4] # Entrenamos con el 80% de las observaciones
# seleccionadas aleatoriamente 
iris.test<-iris[-iii,1:4] # el 20% de observaciones restantes como prueba
gi.test<-iris[-iii,5]   
lda.fit=lda(iris.train,gi.train)
lda.fit 
dev.new()
plot(lda.fit) 
lda.pred=predict(lda.fit,iris.test)
names(lda.pred)
lda.class=lda.pred$class          # predicciones acerca de la pertencencia a un grupo
lda.posterior=lda.pred$posterior  # una matriz cuya k-esima columna contiene la probabilidad
# posterior de que la correspondiente observacion pertenezca
# a la k-esima clase
lda.scores= lda.pred$x            # scores para los datos de prueba

table(lda.class,gi.test)   # matriz de confusion
tc<-mean(lda.class==gi.test)   
#############################################################################################################
rm(list=ls())
head<- read.delim("C:/Users/fzuluag2/Google Drive/R/head1.txt",header=FALSE)
colnames(head)<-c("grupo","wdim","circum","fbeye","eyehd","earhd","jaw")
n<-90

###############################################################################################################

set.seed(42)
h<-sample(1:n,ceiling(n*0.80))  # seleccionamos aleatoriamente el 80% de 
#las obs para entren
gh.train<-factor(head[h,1]) # identificamos los grupos en la muestra de entrenam
h.train<-head[h,2:7] # Entrenamos con el 80% de las observaciones
# seleccionadas aleatoriamente 
h.test<-head[-h,2:7] # el 20% de observaciones restantes como prueba
gh.test<-factor(head[-h,1])   
qda.fith=qda(h.train,gh.train)
qda.fith 

qda.pred=predict(qda.fith,h.test)
names(lda.pred)
qda.class=qda.pred$class          # predicciones acerca de la pertencencia a un grupo
qda.posterior=qda.pred$posterior  # una matriz cuya k-esima columna contiene la probabilidad
# posterior de que la correspondiente observacion pertenezca
# a la k-esima clase
qda.scores= qda.pred$x            # scores para los datos de prueba

table(qda.class,gh.test)   # matriz de confusion
tc<-mean(qda.class==gh.test)   
#####################################################################################################3
#MODELO LOGISTICO
########################################################################################333
library(fda.usc)
library(ddalpha)
nb<-194
data("biomed")
set.seed(42)
iib<-sample(1:nb,ceiling(nb*0.80))  # seleccionamos aleatoriamente el 80% de 
#las obs para entren
# identificamos los grupos en la muestra de entrenam
grupo<-ifelse(biomed$C>=2,0,1)  # Convertimos 1= Raro , 0=normales
biomed1<-cbind(biomed[,1:4],grupo)
biomed.train<-biomed1[iib,1:5] # Entrenamos con el 80% de las observaciones
#seleccionadas aleatoriamente 
biomed.test<-biomed1[-iib,1:5] # el 20% de observaciones restantes como prueba
# identificamos los grupos en la muestra de prueba y de entrenamiento
gb.test<-factor(biomed1[-iib,5])
gb.train<-factor(biomed1[iib,5])
glm.fit=glm(grupo~X1+X2+X3+X4,family =binomial,data=biomed.train)

glm.probs=predict(glm.fit,biomed.test,type="response")
glm.pred=rep("0",38)      # se crea un vector de 0  de 38 elementos(numero
# de datos de prueba) 
glm.pred[glm.probs>.5]=1   # tranforma en 1 todos 
#los elementos en los cuales la probabilidad predecida> 0.5
table(glm.pred,gb.test) # se calcula la matriz de confusion





##############################################################################################

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


