
#installing some packages to read the csv data
install.packages("tidyverse")
library(tidyverse)
install.packages("visdat")
library(visdat)

install.packages("depth")
install.packages("fda.usc")
install.packages("ddalpha")
install.packages("DepthProc")

library(ggplot2)
library(depth)
library(DepthProc)
library(MASS)

library(class)
library(fda.usc)
library(ddalpha)


#read csv of the stroke dataset
stroke <- read_csv("healthcare-dataset-stroke-data.csv")

############### checking the data, converting into categorial values ##############
head(stroke)
str(stroke)

#Cleaning the data to make a more accurate classification
#BMI has some empty data (n/a)
stroke$bmi <- as.numeric(stroke$bmi)
vis_dat(stroke)
colSums(is.na(stroke)) #there are 201 n/a's, which is an acceptable amount to drop
stroke <- drop_na(stroke) #drop n/a's

#Gender contains 'other', we remove this for more easy classification
stroke <- stroke %>%
  filter(gender != 'Other')

# Develop Levels to our ordinal variable of smoking type
# stroke$smoking_status <- factor(stroke$smoking_status, order = TRUE,
#                                    levels = c('never smoked', 'formerly smoked', 'smokes', 'Unknown'))

# male = 0, female = 1
stroke <- stroke %>%
  mutate(gender = recode(gender, 'Male' = 0, 'Female' = 1))

stroke$gender <- as.double(stroke$gender)

# married = 1, not married = 0
stroke <- stroke %>%
  mutate(ever_married = recode(ever_married, 'Yes' = 1, 'No' = 0))

stroke$ever_married <- as.double(stroke$ever_married)

# Residence; 1 = Urban, 0 = Rural

stroke <- stroke %>%
  mutate(Residence_type = recode(Residence_type, 'Rural' = 0, 'Urban' = 1))

stroke$Residence_type <- as.double(stroke$Residence_type)

# Never smoked = 0, formerly smoked = 1, smokes = 2
# Unknown = 1, here we are just taking the average value because we don't know

stroke <- stroke %>%
  mutate(smoking_status = recode(smoking_status, 'never smoked' = 0, 'formerly smoked' = 1,
                                 'smokes' = 2, 'Unknown' = 1))

stroke$smoking_status <- as.double(stroke$smoking_status)

#Removing work_type because can't make it numeric
stroke <- stroke[, colnames(stroke) != "work_type"]

#check that there is now nothing categorial
vis_dat(stroke)
view(stroke)
############## Entrenamiento y predicción con DD-Classifier #############

amount = nrow(stroke)
print(amount)
ii<-sample(1:amount,0.8*amount) # Dividimos 80-20 para training y testeado.
stroke = as.data.frame(stroke)
group.train<-factor(stroke[ii,11]) # y de entrenamiento.
x.train<-stroke[ii,2:10] # x de entrenamiento.

out1=classif.DD(group.train,x.train,depth="HS",classif="lda") # Clasificador 1.
out2=classif.DD(group.train,x.train,depth="HS",classif="DD3") # Clasificador 2.

summary(out1)
summary(out2)

x.test<-stroke[-ii,2:10] # x de prueba.
group.test<-stroke[-ii,11] # y de prueba.

# Predicciones.
pred1=predict(out1,x.test)
pred2=predict(out2,x.test)

# Matrices de confusión.
table(pred1,group.test)
table(pred2,group.test)


