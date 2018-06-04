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

x <-table(prof$religion, prof$age) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and Age", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
## NA's and 25/30s
corrplot(chi$residuals, is.cor = FALSE)
## Other and 45+ is a little larger than random

#############################################################################################################
## Body_type
#############################################################################################################

x <-table(prof$religion, prof$body_type) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and body_type", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Diet
#############################################################################################################

x <-table(prof$religion, prof$diet) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and diet", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Drinks
#############################################################################################################

x <-table(prof$religion, prof$drinks) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and drinks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Drugs
#############################################################################################################

x <-table(prof$religion, prof$drugs) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and drugs", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Education
#############################################################################################################

x <-table(prof$religion, prof$education) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and education", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Essay 4
#############################################################################################################

x <-table(prof$religion, prof$essay4) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and essay4", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Ethnicity
#############################################################################################################

x <-table(prof$religion, prof$ethnicity) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and ethnicity", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Height
#############################################################################################################

x <-table(prof$religion, prof$height) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and height", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Income
#############################################################################################################

x <-table(prof$religion, prof$income) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and income", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Job
#############################################################################################################

x <-table(prof$religion, prof$job) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and job", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Last Online
#############################################################################################################

x <-table(prof$religion, prof$last_online) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and last_online", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Location
#############################################################################################################

x <-table(prof$religion, prof$location) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and location", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Offspring
#############################################################################################################

x <-table(prof$religion, prof$offspring) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and offspring", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)

#############################################################################################################
## Orientation
#############################################################################################################

x <-table(prof$religion, prof$orientation) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and orientation", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Pets
#############################################################################################################

x <-table(prof$religion, prof$pets) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and pets", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Religion
#############################################################################################################


#############################################################################################################
## Sex
#############################################################################################################

x <-table(prof$religion, prof$sex) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and sex", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Sign
#############################################################################################################

x <-table(prof$religion, prof$sign) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and sign", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Smokes
#############################################################################################################

x <-table(prof$religion, prof$smokes) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and smokes", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Speaks
#############################################################################################################

x <-table(prof$religion, prof$speaks) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and speaks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(t(chi$residuals), is.cor = FALSE)

#############################################################################################################
## Status
#############################################################################################################

x <-table(prof$religion, prof$status) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and status", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


#############################################################################################################
## Seriousness
#############################################################################################################


x <-table(prof$religion, prof$seriousness) 
a <-CA(x)
plot(a, autoLab = "no") 
a$eig
summary(a)
chi <-chisq.test(x) 
chi$residuals ## warning? 

balloonplot(t(x), main ="Religion and seriousness", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
corrplot(chi$residuals, is.cor = FALSE)


######################################################################################################33######
# [1] "age"         "body_type"   "diet"        "drinks"      "drugs"       "education"  
# [7] "essay0"      "essay1"      "essay2"      "essay3"      "essay4"      "essay5"     
# [13] "essay6"      "essay7"      "essay8"      "essay9"      "ethnicity"   "height"     
# [19] "income"      "job"         "last_online" "location"    "offspring"   "orientation"
# [25] "pets"        "religion"    "sex"         "sign"        "smokes"      "speaks"     
# [31] "status"


