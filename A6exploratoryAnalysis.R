library(readr)
setwd("~/Documents/data analytics/assignment 6")
profiles <- read_csv("profiles.csv")

prof <- profiles
str(prof) ## ints and char
rm(profiles)

cols <- c("essay0", "essay1", "essay2", "essay3", "essay4", "essay5", "essay6", "essay7",
  "essay8", "essay9")
prof[cols] <- lapply(prof[cols], nchar)
prof[cols] <- lapply(prof[cols], log)
prof[cols] <- lapply(prof[cols], round)
rm(cols)

str(prof) ## ints and char
prof[prof==""] <- NA
cols <- c("body_type", "diet", "drinks", "drugs", "education", "ethnicity",
  "job", "location", "offspring", "orientation", "pets", "religion", "sex",
  "sign", "smokes", "speaks", "status")
prof[cols] <- lapply(prof[cols], factor)
prof$last_online <- as.Date(prof$last_online)


hist(prof$age)
hist(as.numeric(prof$body_type)) ## weird distribution, bin
hist(as.numeric(prof$diet)) ## bin?
hist(as.numeric(prof$drinks))
hist(as.numeric(prof$drugs))
hist(as.numeric(prof$education)) ## bin?
hist(prof$essay0)


hist(as.numeric(prof$ethnicity)) ## hundreds of ethnicity factors.
## maybe use top 5 or so?
hist(prof$height)
summary(prof$height) ## min 1.0 ... maybe look at shortest height possible, and remove
## anyone below that
hist(prof$income)
hist(log(prof$income)) ## not a "good" distribution. Most poeple didn't answer
fivenum(prof$income)

hist(as.numeric(prof$job)) ## 20 or so categories bin
hist(as.numeric(prof$last_online)) ## exponential distribution
hist(as.numeric(prof$location)) ## hundreds of locations. 
hist(as.numeric(prof$offspring))
hist(as.numeric(prof$pets))
hist(as.numeric(prof$religion)) ## over 40, I'm going to want to bin this
hist(as.numeric((prof$sex))) ## not 50-50. More men
hist(as.numeric(prof$sign)) ## bin this
hist(as.numeric(prof$smokes))
hist(as.numeric(prof$speaks)) ## way too many. Maybe take top candidate (english)
hist(as.numeric(prof$status))

rm(cols)

