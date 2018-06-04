library(gbm)
library(caret)
library(corrplot)
setwd("~/Documents/data analytics/assignment 6")
source("A6cleaning.R")

prof <- prof[prof$seriousness != "NA",]
prof$seriousness <-droplevels(prof$seriousness)

summary(prof)
prof <- prof[sample(nrow(prof)),]  # shuffle

BST = gbm(seriousness~.,data=prof,
          distribution='multinomial',
          n.trees=100,
          interaction.depth=4,
          cv.folds=5,
          shrinkage=0.005)

best.iter <- gbm.perf(BST,method="cv")
print(best.iter)
summary(BST)
sum <- summary(BST)
sum <-sum[sum$rel.inf>0,]
plot(sum$rel.inf)
lines(sum$rel.inf)
title("Relative Influence on seriousness, GBM")
plot(BST$cv.error) ## there's only one

sum <- as.data.frame(sum)
rownames(sum)[sum$rel.inf>20] ## religion

pred <- as.data.frame(BST$fit)
p.predBST <- apply(pred, 1, which.max)
head(p.predBST)
p.predBst <-levels(prof$seriousness)[p.predBST]
p.predBst <- as.factor(p.predBst)

levels(p.predBst) <- levels(prof$seriousness)

confusionMatrix(prof$seriousness, p.predBst) ## Accuracy 0.4987
tab <-table(p.predBst, prof$seriousness)

summary(as.factor(prof$seriousness)) ## true numbers

numright <-as.numeric(diag(tab))
true <- summary(as.factor(prof$seriousness))
true <- as.numeric(true)
tot <- length(prof$seriousness)

acc <-numright/true 



t <-chisq.test(tab)
t$residuals ## there's more on the diagonals than random...
corrplot(t$residuals, is.cor = FALSE)
print(BST) ## There were 31 predictors of which 22 had non-zero influence.
