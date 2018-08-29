list.of.packages <- c("readr", "tree", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(okcupiddata)
data("profiles")

prof <- profiles
rm(profiles)

colnames(prof)
#[1] "age"         "body_type"   "diet"        "drinks"      "drugs"       "education"   "ethnicity"   "height"     
#[9] "income"      "job"         "last_online" "location"    "offspring"   "orientation" "pets"        "religion"   
#[17] "sex"         "sign"        "smokes"      "speaks"      "status"      "essay0"     

hist(prof$age) ## heavy tail on higher ages
plot(as.factor(prof$body_type)) 
plot(as.factor(prof$diet)) ## bin?
plot(as.factor(prof$drinks))
plot(as.factor(prof$drugs))

library(ggplot2)
ggplot(data=subset(prof, !is.na(drugs)), aes(x=drugs)) + 
  geom_bar(stat="count")
ggplot(data=prof, aes(x=drugs)) + geom_bar(stat="count")

ggplot(data=subset(prof, !is.na(education)), aes(x=education)) + 
  geom_bar(stat="count") ## bin ?
ggplot(data=prof, aes(x=education)) + geom_bar(stat="count")

hist(nchar(prof$essay0))
hist(log(nchar(prof$essay0)))


plot(as.factor(prof$ethnicity))
length(levels(as.factor(prof$ethnicity))) ## 217 levels
## maybe use top 5 or so?

hist(prof$height)
summary(prof$height) ## min 1.0 ... maybe look at shortest height possible, and remove
## anyone below that
hist(prof$income)
hist(log(prof$income)) ## not a "good" distribution. Most poeple didn't answer
fivenum(prof$income)

plot(as.factor(prof$job))## 20 or so categories bin

ggplot(data=subset(prof, !is.na(last_online)), aes(x=last_online)) + 
  geom_histogram(binwidth = 1e6)

ggplot(data=subset(prof, !is.na(location)), aes(x=location)) + 
  geom_bar(stat="count")
length(levels(as.factor(prof$location))) ## 199 locations, 5 main ones 

ggplot(data=subset(prof, !is.na(offspring)), aes(x=offspring)) + 
  geom_bar(stat="count")
ggplot(data=subset(prof, !is.na(pets)), aes(x=pets)) + 
  geom_bar(stat="count")
ggplot(data=subset(prof, !is.na(religion)), aes(x=religion)) + 
  geom_bar(stat="count") ## even distributions
length(levels(as.factor(prof$religion))) ## 45 levels, I'm going to want to bin this

ggplot(data=subset(prof, !is.na(sex)), aes(x=sex)) + 
  geom_bar(stat="count")## not 50-50. More men
ggplot(data=subset(prof, !is.na(sign)), aes(x=sign)) + 
  geom_bar(stat="count") ## bin this

length(levels(as.factor(prof$sign))) ## 48 levels
ggplot(data=prof, aes(x=smokes)) + 
  geom_bar(stat="count")
#ggplot(data=prof, aes(x=speaks)) + 
#  geom_bar(stat="count")
length(levels(as.factor(prof$speaks)))## way too many. Maybe take top candidate (english)


ggplot(data=prof, aes(x=status)) + 
  geom_bar(stat="count")
#rm(list=ls())
#dev.off()
