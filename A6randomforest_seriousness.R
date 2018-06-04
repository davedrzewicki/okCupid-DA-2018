setwd("~/Documents/data analytics/assignment 6")
source("A6cleaning.R")

#############################################################################################################
## ctree
#############################################################################################################
library(dplyr)
require(rpart)

prof.serious <- select(prof[prof$seriousness != "NA",], -religion)
prof.serious$seriousness <-droplevels(prof.serious$seriousness)

prof.rpart <- rpart(seriousness ~ ., data = prof.serious)
# Error in plot.rpart(prof.rpart) : fit is not a tree, just a root
library(rpart.plot)
rpart.plot(prof.rpart)
title("Seriousness and Drug Use")
levels(prof.serious$seriousness)

## Drug use is the most importand factor in determination
## The Probabilities of eavh of the 4 categories are in hte bottom
## "laughing about it" "no too serious"    "somewhat serious"  "very serious" 
## This is no smoking gun


#############################################################################################################
## randomForest
#############################################################################################################

require(randomForest)
fitRF <- randomForest(seriousness ~. , data=prof.serious)
print(fitRF) 	# view results
importance(fitRF) # importance of each predictor
plot(fitRF)

# Call:
#   randomForest(formula = seriousness ~ ., data = prof.serious) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 5
# 
# OOB estimate of  error rate: 51.83%
# Confusion matrix:
#   laughing about it no too serious somewhat serious very serious class.error
# laughing about it              3805           5077                9           11   0.5725680
# no too serious                 2611           9420               39           11   0.2202632
# somewhat serious               1209           3202               36           11   0.9919246
# very serious                    738           1369               27           44   0.9797980
# > 

tab <-fitRF$confusion
numright <-diag(fitRF$confusion)
true <- table(prof.serious$seriousness) 
tot <- length(prof.serious$seriousness)

t <-chisq.test(tab)
t$residuals ## there's more on the diagonals than random...
corrplot(t$residuals, is.cor = FALSE)


acc <-numright/true 
randprob <- true/tot
randprob

round((acc-randprob)*100) ## percentage better than random
## Better than random for lauging about it and not too serious. 
## Not better than random for somewhat serious and very serious

sort <-as.data.frame(fitRF$importance)
sort <-sort[order(-sort$MeanDecreaseGini) , , drop = FALSE]
title("Importance for Seriousness, Random Forests")
plot(sort$MeanDecreaseGini) ## the first 7 are most important...
sort[1:7, , drop=FALSE]


