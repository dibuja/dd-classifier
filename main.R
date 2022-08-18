help.start()
install.packages("fda.usc")
fda.usc.help
include('fda.usc')
# DD-classif for functional data
data(tecator)
# DD-classif for functional data
data(iris)
group<-iris[,5]
View(iris)
View(iris)
x<-iris[,1:4]
out10=classif.DD(group,x,depth="LD",classif="lda")
import('fda.usc')
library('fda.usc')
out10=classif.DD(group,x,depth="LD",classif="lda")
View(out10)
summary(out10)
out11=classif.DD(group,list(x,x),depth=c("MhD","LD"),classif="lda")
summary(out11)