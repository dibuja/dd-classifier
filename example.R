
# install.packages("fda.usc")
library('fda.usc')


# Cross-validation with different depths.
# Classify with fake data.
# Classify with the real data.
# Compare with SVM or KNN.


# DD-classif for functional data
data(iris)
group<-iris[,5]
x<-iris[,1:4]
out10=classif.DD(group,x,depth="LD",classif="lda")
summary(out10)
out11=classif.DD(group,list(x,x),depth=c("MhD"),classif="lda")
summary(out11)
