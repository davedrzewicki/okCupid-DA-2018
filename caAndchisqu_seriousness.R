library(ca)
library(corrplot)
library(gplots)
library(FactoMineR)

# [1] "age"         "body_type"   "diet"        "drinks"      "drugs"       "education"  
# [7] "essay0"      "essay1"      "essay2"      "essay3"      "essay4"      "essay5"     
# [13] "essay6"      "essay7"      "essay8"      "essay9"      "ethnicity"   "height"     
# [19] "income"      "job"         "last_online" "location"    "offspring"   "orientation"
# [25] "pets"        "religion"    "sex"         "sign"        "smokes"      "speaks"     
# [31] "status"

setwd("~/Documents/data analytics/assignment 6")
source("A6cleaning.R")

#############################################################################################################
## age
#############################################################################################################

x <-table(prof$seriousness, prof$age) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and Age", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
## NA's and 25/30s
corrplot(chi$residuals, is.cor = FALSE)
## Other and 45+ is a little larger than random

#############################################################################################################
## Body_type
#############################################################################################################

x <-table(prof$seriousness, prof$body_type) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and body_type", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Diet
#############################################################################################################

x <-table(prof$seriousness, prof$diet) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and diet", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Drinks
#############################################################################################################

x <-table(prof$seriousness, prof$drinks) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and drinks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Drugs
#############################################################################################################

x <-table(prof$seriousness, prof$drugs) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and drugs", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Education
#############################################################################################################

x <-table(prof$seriousness, prof$education) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and education", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Essay 4
#############################################################################################################

x <-table(prof$seriousness, prof$essay4) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and essay4", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Ethnicity
#############################################################################################################

x <-table(prof$seriousness, prof$ethnicity) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and ethnicity", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Height
#############################################################################################################

x <-table(prof$seriousness, prof$height) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and height", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Income
#############################################################################################################

x <-table(prof$seriousness, prof$income) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and income", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Job
#############################################################################################################

x <-table(prof$seriousness, prof$job) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and job", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Last Online
#############################################################################################################

x <-table(prof$seriousness, prof$last_online) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and last_online", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Location
#############################################################################################################

x <-table(prof$seriousness, prof$location) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and location", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Offspring
#############################################################################################################

x <-table(prof$seriousness, prof$offspring) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and offspring", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Orientation
#############################################################################################################

x <-table(prof$seriousness, prof$orientation) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and orientation", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Pets
#############################################################################################################

x <-table(prof$seriousness, prof$pets) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and pets", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## seriousness
#############################################################################################################


#############################################################################################################
## Sex
#############################################################################################################

x <-table(prof$seriousness, prof$sex) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and sex", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Sign
#############################################################################################################

x <-table(prof$seriousness, prof$sign) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and sign", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Smokes
#############################################################################################################

x <-table(prof$seriousness, prof$smokes) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and smokes", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Speaks
#############################################################################################################

x <-table(prof$seriousness, prof$speaks) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and speaks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(t(chi$residuals), is.cor = FALSE)

#############################################################################################################
## Status
#############################################################################################################

x <-table(prof$seriousness, prof$status) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="seriousness and status", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Seriousness
#############################################################################################################





######################################################################################################33######
# [1] "age"         "body_type"   "diet"        "drinks"      "drugs"       "education"  
# [7] "essay0"      "essay1"      "essay2"      "essay3"      "essay4"      "essay5"     
# [13] "essay6"      "essay7"      "essay8"      "essay9"      "ethnicity"   "height"     
# [19] "income"      "job"         "last_online" "location"    "offspring"   "orientation"
# [25] "pets"        "religion"    "sex"         "sign"        "smokes"      "speaks"     
# [31] "status"


