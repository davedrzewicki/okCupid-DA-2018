library(gbm)
library(caret)
library(corrplot)
setwd("~/Documents/data analytics/assignment 6")
source("A6cleaning.R")

prof <- prof[prof$religion != "NA",]
prof$religion <-droplevels(prof$religion)

summary(prof)
prof <- prof[sample(nrow(prof)),]  # shuffle

BST = gbm(religion~.,data=prof,
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
title("Relative Influence on Religion, GBM")
plot(BST$cv.error)

sum <- as.data.frame(sum)
rownames(sum)[sum$rel.inf>5]

pred <- as.data.frame(BST$fit)
p.predBST <- apply(pred, 1, which.max)
head(p.predBST)
p.predBst <-levels(prof$religion)[p.predBST]
p.predBst <- as.factor(p.predBst)

levels(p.predBst) <- levels(prof$religion)

confusionMatrix(prof$religion, p.predBst)
tab <-table(p.predBst, prof$religion)

summary(as.factor(prof$religion)) ## true numbers

numright <-as.numeric(diag(tab))
true <- summary(as.factor(prof$religion))
true <- as.numeric(true)
tot <- length(prof$religion)

acc <-numright/true 



t <-chisq.test(tab)
t$residuals ## there's more on the diagonals than random...
corrplot(t$residuals, is.cor = FALSE)
print(BST) ## There were 31 predictors of which 22 had non-zero influence.
