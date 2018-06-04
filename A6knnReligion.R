library(corrplot)
library(caret)
library(dplyr)
library(class)
library(kknn)

setwd("~/Documents/data analytics/assignment 6")
source("A6cleaning.R")

rm(prof)

#############################################################################################################
## weighted knn
#############################################################################################################

colnames(dummy.prof)
dummy.prof <-select(dummy.prof[dummy.prof$religion != "NA",], -seriousness)

dummy.prof$religion <-droplevels(dummy.prof$religion)

set.seed(12345)

fit.kknn <- cv.kknn(religion ~., dummy.prof, kcv = 5)
fit.kknn <- as.data.frame(fit.kknn)
y <- levels(dummy.prof$religion)[fit.kknn$y]
yhat <- levels(dummy.prof$religion)[fit.kknn$yhat] 
tab <-table(yhat, y)

confusionMatrix(tab)

t <-chisq.test(tab)
t$residuals ## there's more on the diagonals than random...
corrplot(t$residuals, is.cor = FALSE)
