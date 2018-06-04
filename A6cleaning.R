library(readr)
library(lubridate)
setwd("~/Documents/data analytics/assignment 6")
profiles <- read_csv("profiles.csv")


colnames(profiles)

# [1] "age"         "body_type"   "diet"        "drinks"      "drugs"       "education"  
# [7] "essay0"      "essay1"      "essay2"      "essay3"      "essay4"      "essay5"     
# [13] "essay6"      "essay7"      "essay8"      "essay9"      "ethnicity"   "height"     
# [19] "income"      "job"         "last_online" "location"    "offspring"   "orientation"
# [25] "pets"        "religion"    "sex"         "sign"        "smokes"      "speaks"     
# [31] "status"

prof <- profiles
rm(profiles)



#plot(sort(rowMeans(is.na(prof)))) ## elbow at about 0.4
prof <- prof[-which(rowMeans(is.na(prof)) > 0.4), ]

cols <- c("essay0", "essay1", "essay2", "essay3", "essay4", "essay5", "essay6", "essay7",
          "essay8", "essay9")


prof[cols] <- lapply(prof[cols], nchar)
prof[cols] <- lapply(prof[cols], log)
prof[cols] <- lapply(prof[cols], round)
prof[cols][is.na(prof[cols])] <-0
rm(cols)

       

#############################################################################################################
## age
#############################################################################################################

prof$age <- as.numeric(prof$age)
prof$age <-as.factor(5*round(prof$age/5)) 

#############################################################################################################
## Body_type
#############################################################################################################

prof[(prof$body_type=="curvy" | prof$body_type=="full figured" )
     & !is.na(prof$body_type), ]$body_type <- "curvy"

prof[(prof$body_type=="a little extra" | prof$body_type=="overweight" 
     | prof$body_type=="rather not say")
     & !is.na(prof$body_type), ]$body_type <- "plus"

prof[(prof$body_type=="athletic" | prof$body_type=="jacked" | prof$body_type=="fit" )
     & !is.na(prof$body_type), ]$body_type <- "fit"

prof[(prof$body_type=="skinny" | prof$body_type=="thin" )
     & !is.na(prof$body_type), ]$body_type <- "thin"

#############################################################################################################
## Diet
#############################################################################################################

prof[(prof$diet=="anything" | prof$diet=="mostly anything" 
            | prof$diet=="strictly anything")
           & !is.na(prof$diet), ]$diet <- "anything"

prof[(prof$diet=="halal" | prof$diet=="mostly halal" 
            | prof$diet=="strictly halal")
           & !is.na(prof$diet), ]$diet <- "halal"

prof[(prof$diet=="kosher" | prof$diet=="mostly kosher" 
            | prof$diet=="strictly kosher")
           & !is.na(prof$diet), ]$diet <- "kosher"

prof[(prof$diet=="other" | prof$diet=="mostly other" 
            | prof$diet=="strictly other")
           & !is.na(prof$diet), ]$diet <- "other"

prof[(prof$diet=="vegan" | prof$diet=="mostly vegan" 
            | prof$diet=="strictly vegan")
           & !is.na(prof$diet), ]$diet <- "vegan"

prof[(prof$diet=="vegetarian" | prof$diet=="mostly vegetarian" 
            | prof$diet=="strictly vegetarian")
           & !is.na(prof$diet), ]$diet <- "vegetarian"

#############################################################################################################
## Education
#############################################################################################################

prof[(prof$education=="graduated from space camp"
      | prof$education=="dropped out of space camp"
      | prof$education=="space camp"
      | prof$education=="working on space camp"
)& !is.na(prof$education), ]$education <- "smart ass"

prof[(prof$education=="dropped out of high school" )
     & !is.na(prof$education), ]$education <- "some high school"

prof[(prof$education=="graduated from high school"
      | prof$education=="dropped out of two-year college"
      | prof$education=="dropped out of college/university"
      | prof$education=="high school"
      | prof$education=="working on high school"
)& !is.na(prof$education), ]$education <- "high school"

prof[(prof$education=="graduated from two-year college"
      | prof$education=="working on two-year college"
      | prof$education=="two-year college"
)& !is.na(prof$education), ]$education <- "two year college"

prof[(prof$education=="graduated from college/university"
      | prof$education=="dropped out of law school"
      | prof$education=="dropped out of med school"
      | prof$education=="dropped out of masters program"
      | prof$education=="college/university"
      | prof$education=="working on college/university"
)& !is.na(prof$education), ]$education <- "college"

prof[(prof$education=="graduated from masters program"
      | prof$education=="dropped out of ph.d program"
      | prof$education=="masters program"
      | prof$education=="working on masters program"
)& !is.na(prof$education), ]$education <- "masters"

prof[(prof$education=="graduated from med school"
      | prof$education=="med school"
      | prof$education=="working on med school"
)& !is.na(prof$education), ]$education <- "med school"

prof[(prof$education=="graduated from ph.d program"
      | prof$education=="ph.d program"
      | prof$education=="working on ph.d program"
)& !is.na(prof$education), ]$education <- "phd"

prof[(prof$education=="graduated from law school"
      | prof$education=="law school"
      | prof$education=="working on law school"
)& !is.na(prof$education), ]$education <- "law school"

#############################################################################################################
## Ethnicity
#############################################################################################################

prof$ethnicity[which(prof$ethnicity != "white"  & prof$ethnicity != "asian"
  & prof$ethnicity != "hispanic / latin" & prof$ethnicity != "black") ] <- "other"


#############################################################################################################
## Height
#############################################################################################################

prof$height <- as.numeric(prof$height)
prof$height[prof$height < 50 ] <- NA
prof$height[prof$height > 85 ] <- NA
prof$height <-as.factor(5*round(prof$height/5))

#############################################################################################################
## Income
#############################################################################################################

prof$income <- as.factor(round(log(prof$income)))
prof$income[prof$income=="NaN"] <- NA


#############################################################################################################
## Job
#############################################################################################################

prof$job[which(prof$job != "education / academia"  & prof$job != "medicine / health"
  & prof$job != "sales / marketing / biz dev" & prof$job != "artistic / musical / writer"
  & prof$job != "computer / hardware / software" & prof$job != "science / tech / engineering"
  & prof$job != "student")] <- "other"


#############################################################################################################
## Last Online
#############################################################################################################

DATE1 <- as.Date("2012-06-27")
prof$last_online[as.Date(prof$last_online) < DATE1] <- NA
prof$last_online <-as.factor(round(as.Date(prof$last_online)))
summary(prof$last_online)
rm(DATE1)

#############################################################################################################
## Location
#############################################################################################################

prof$location[which(prof$location != "berkeley, california"  
                    & prof$location != "oakland, california"
                    & prof$location != "san francisco, california" )] <- "other"

#############################################################################################################
## Offspring
#############################################################################################################

prof[(prof$offspring=="doesn&rsquo;t have kids, and doesn&rsquo;t want any" 
      | prof$offspring=="doesn&rsquo;t want kids"
      | prof$offspring=="doesn&rsquo;t want kids"
      |prof$offspring=="has a kid, but doesn&rsquo;t want more"
      |prof$offspring=="has kids, but doesn&rsquo;t want more"
)& !is.na(prof$offspring), ]$offspring <- "does not want kid"

prof[(prof$offspring=="doesn&rsquo;t have kids, but might want them" 
      | prof$offspring=="doesn&rsquo;t have kids, but wants them"
      | prof$offspring=="has a kid, and might want more"
      |prof$offspring=="has a kid, and wants more"
      |prof$offspring=="has kids, and might want more"
      |prof$offspring=="has kids, and wants more"
      |prof$offspring=="might want kids"
      |prof$offspring=="wants kids"
)& !is.na(prof$offspring), ]$offspring <- "wants kid"

prof[(prof$offspring=="doesn&rsquo;t have kids"
      |prof$offspring=="doesn&rsquo;t have kids, and doesn&rsquo;t want any" 
      |prof$offspring=="doesn&rsquo;t have kids, but might want them"
      |prof$offspring=="doesn&rsquo;t have kids, but wants them"
)& !is.na(prof$offspring), ]$offspring <- "does not have kid"

prof[(prof$offspring=="has a kid" 
      | prof$offspring=="has a kid, and might want more"
      | prof$offspring=="has a kid, and wants more"
      | prof$offspring=="has a kid, but doesn&rsquo;t want more"
      |prof$offspring=="has kids"
      |prof$offspring=="has kids, and might want more"
      |prof$offspring=="has kids, and wants more"
      |prof$offspring=="has kids, but doesn&rsquo;t want more"
)& !is.na(prof$offspring), ]$offspring <- "has kid"

#############################################################################################################
## Pets
#############################################################################################################

prof$pets[which(prof$pets != "likes dogs and dislikes cats"  
  & prof$pets != "likes dogs and dislikes cats"
  & prof$pets != "has dogs" & prof$pets != "likes dogs and has cats"
  & prof$pets != "likes dogs" & prof$pets != "likes dogs and likes cats")] <- "other"

#############################################################################################################
## Religion
#############################################################################################################

prof["seriousness"] <- NA

prof[prof$religion=="agnosticism and laughing about it" & !is.na(prof$religion), ]$seriousness <- "laughing about it"
prof[prof$religion=="atheism and laughing about it" & !is.na(prof$religion), ]$seriousness <- "laughing about it"
prof[prof$religion=="buddhism and laughing about it" & !is.na(prof$religion), ]$seriousness <- "laughing about it"
prof[prof$religion=="catholicism and laughing about it" & !is.na(prof$religion), ]$seriousness <- "laughing about it"
prof[prof$religion=="christianity and laughing about it" & !is.na(prof$religion), ]$seriousness <- "laughing about it"
prof[prof$religion=="hinduism and laughing about it" & !is.na(prof$religion), ]$seriousness <- "laughing about it"
prof[prof$religion=="islam and laughing about it" & !is.na(prof$religion), ]$seriousness <- "laughing about it"
prof[prof$religion=="judaism and laughing about it" & !is.na(prof$religion), ]$seriousness <- "laughing about it"
prof[prof$religion=="other and laughing about it" & !is.na(prof$religion), ]$seriousness <- "laughing about it"

prof[prof$religion=="agnosticism but not too serious about it" & !is.na(prof$religion), ]$seriousness <- "no too serious"
prof[prof$religion=="atheism but not too serious about it" & !is.na(prof$religion), ]$seriousness <- "no too serious"
prof[prof$religion=="buddhism but not too serious about it" & !is.na(prof$religion), ]$seriousness <- "no too serious"
prof[prof$religion=="catholicism but not too serious about it" & !is.na(prof$religion), ]$seriousness <- "no too serious"
prof[prof$religion=="christianity but not too serious about it" & !is.na(prof$religion), ]$seriousness <- "no too serious"
prof[prof$religion=="hinduism but not too serious about it" & !is.na(prof$religion), ]$seriousness <- "no too serious"
prof[prof$religion=="islam but not too serious about it" & !is.na(prof$religion), ]$seriousness <- "no too serious"
prof[prof$religion=="judaism but not too serious about it" & !is.na(prof$religion), ]$seriousness <- "no too serious"
prof[prof$religion=="other but not too serious about it" & !is.na(prof$religion), ]$seriousness <- "no too serious"

prof[prof$religion=="agnosticism and somewhat serious about it" & !is.na(prof$religion), ]$seriousness <- "somewhat serious"
prof[prof$religion=="atheism and somewhat serious about it" & !is.na(prof$religion), ]$seriousness <- "somewhat serious"
prof[prof$religion=="buddhism and somewhat serious about it" & !is.na(prof$religion), ]$seriousness <- "somewhat serious"
prof[prof$religion=="catholicism and somewhat serious about it" & !is.na(prof$religion), ]$seriousness <- "somewhat serious"
prof[prof$religion=="christianity and somewhat serious about it" & !is.na(prof$religion), ]$seriousness <- "somewhat serious"
prof[prof$religion=="hinduism and somewhat serious about it" & !is.na(prof$religion), ]$seriousness <- "somewhat serious"
prof[prof$religion=="islam and somewhat serious about it" & !is.na(prof$religion), ]$seriousness <- "somewhat serious"
prof[prof$religion=="judaism and somewhat serious about it" & !is.na(prof$religion), ]$seriousness <- "somewhat serious"
prof[prof$religion=="other and somewhat serious about it" & !is.na(prof$religion), ]$seriousness <- "somewhat serious"  

prof[prof$religion=="agnosticism and very serious about it" & !is.na(prof$religion), ]$seriousness <- "very serious"
prof[prof$religion=="atheism and very serious about it" & !is.na(prof$religion), ]$seriousness <- "very serious"
prof[prof$religion=="buddhism and very serious about it" & !is.na(prof$religion), ]$seriousness <- "very serious"
prof[prof$religion=="catholicism and very serious about it" & !is.na(prof$religion), ]$seriousness <- "very serious"
prof[prof$religion=="christianity and very serious about it" & !is.na(prof$religion), ]$seriousness <- "very serious"
prof[prof$religion=="hinduism and very serious about it" & !is.na(prof$religion), ]$seriousness <- "very serious"
prof[prof$religion=="islam and very serious about it" & !is.na(prof$religion), ]$seriousness <- "very serious"
prof[prof$religion=="judaism and very serious about it" & !is.na(prof$religion), ]$seriousness <- "very serious"
prof[prof$religion=="other and very serious about it" & !is.na(prof$religion), ]$seriousness <- "very serious"

prof[prof$religion=="agnosticism" & !is.na(prof$religion), ]$religion <- "agnosticism"
prof[prof$religion=="agnosticism and laughing about it" & !is.na(prof$religion), ]$religion <- "agnosticism"
prof[prof$religion=="agnosticism but not too serious about it" & !is.na(prof$religion), ]$religion <- "agnosticism"
prof[prof$religion=="agnosticism and somewhat serious about it" & !is.na(prof$religion), ]$religion <- "agnosticism"
prof[prof$religion=="agnosticism and very serious about it" & !is.na(prof$religion), ]$religion <- "agnosticism"

prof[prof$religion=="atheism" & !is.na(prof$religion), ]$religion <- "atheism"
prof[prof$religion=="atheism and laughing about it" & !is.na(prof$religion), ]$religion <- "atheism"
prof[prof$religion=="atheism but not too serious about it" & !is.na(prof$religion), ]$religion <- "atheism"
prof[prof$religion=="atheism and somewhat serious about it" & !is.na(prof$religion), ]$religion <- "atheism"
prof[prof$religion=="atheism and very serious about it" & !is.na(prof$religion), ]$religion <- "atheism"

prof[prof$religion=="buddhism" & !is.na(prof$religion), ]$religion <- "buddhism"
prof[prof$religion=="buddhism and laughing about it" & !is.na(prof$religion), ]$religion <- "buddhism"
prof[prof$religion=="buddhism but not too serious about it" & !is.na(prof$religion), ]$religion <- "buddhism"
prof[prof$religion=="buddhism and somewhat serious about it" & !is.na(prof$religion), ]$religion <- "buddhism"
prof[prof$religion=="buddhism and very serious about it" & !is.na(prof$religion), ]$religion <- "buddhism"

prof[prof$religion=="catholicism" & !is.na(prof$religion), ]$religion <- "catholicism"
prof[prof$religion=="catholicism and laughing about it" & !is.na(prof$religion), ]$religion <- "catholicism"
prof[prof$religion=="catholicism but not too serious about it" & !is.na(prof$religion), ]$religion <- "catholicism"
prof[prof$religion=="catholicism and somewhat serious about it" & !is.na(prof$religion), ]$religion <- "catholicism"
prof[prof$religion=="catholicism and very serious about it" & !is.na(prof$religion), ]$religion <- "catholicism"

prof[prof$religion=="christianity" & !is.na(prof$religion), ]$religion <- "christianity"
prof[prof$religion=="christianity and laughing about it" & !is.na(prof$religion), ]$religion <- "christianity"
prof[prof$religion=="christianity but not too serious about it" & !is.na(prof$religion), ]$religion <- "christianity"
prof[prof$religion=="christianity and somewhat serious about it" & !is.na(prof$religion), ]$religion <- "christianity"
prof[prof$religion=="christianity and very serious about it" & !is.na(prof$religion), ]$religion <- "christianity"

prof[prof$religion=="hinduism" & !is.na(prof$religion), ]$religion <- "hinduism"
prof[prof$religion=="hinduism and laughing about it" & !is.na(prof$religion), ]$religion <- "hinduism"
prof[prof$religion=="hinduism but not too serious about it" & !is.na(prof$religion), ]$religion <- "hinduism"
prof[prof$religion=="hinduism and somewhat serious about it" & !is.na(prof$religion), ]$religion <- "hinduism"
prof[prof$religion=="hinduism and very serious about it" & !is.na(prof$religion), ]$religion <- "hinduism"

prof[prof$religion=="islam" & !is.na(prof$religion), ]$religion <- "islam"
prof[prof$religion=="islam and laughing about it" & !is.na(prof$religion), ]$religion <- "islam"
prof[prof$religion=="islam but not too serious about it" & !is.na(prof$religion), ]$religion <- "islam"
prof[prof$religion=="islam and somewhat serious about it" & !is.na(prof$religion), ]$religion <- "islam"
prof[prof$religion=="islam and very serious about it" & !is.na(prof$religion), ]$religion <- "islam"

prof[prof$religion=="judaism" & !is.na(prof$religion), ]$religion <- "judaism"
prof[prof$religion=="judaism and laughing about it" & !is.na(prof$religion), ]$religion <- "judaism"
prof[prof$religion=="judaism but not too serious about it" & !is.na(prof$religion), ]$religion <- "judaism"
prof[prof$religion=="judaism and somewhat serious about it" & !is.na(prof$religion), ]$religion <- "judaism"
prof[prof$religion=="judaism and very serious about it" & !is.na(prof$religion), ]$religion <- "judaism"

prof[prof$religion=="other" & !is.na(prof$religion), ]$religion <- "other"
prof[prof$religion=="other and laughing about it" & !is.na(prof$religion), ]$religion <- "other"
prof[prof$religion=="other but not too serious about it" & !is.na(prof$religion), ]$religion <- "other"
prof[prof$religion=="other and somewhat serious about it" & !is.na(prof$religion), ]$religion <- "other"
prof[prof$religion=="other and very serious about it" & !is.na(prof$religion), ]$religion <- "other"

#############################################################################################################
## Sign
#############################################################################################################

prof[(prof$sign=="aquarius" 
      | prof$sign=="aries"
      | prof$sign=="cancer"
      |prof$sign=="capricorn"
      |prof$sign=="gemini"
      |prof$sign=="leo"
      |prof$sign=="libra"
      |prof$sign=="pisces"
      |prof$sign=="sagittarius"
      |prof$sign=="scorpio"
      |prof$sign=="taurus"
      |prof$sign=="virgo"
)& !is.na(prof$sign), ]$sign <- "sign given"

prof[(prof$sign=="aquarius but it doesn&rsquo;t matter" 
      | prof$sign=="aries but it doesn&rsquo;t matter"
      | prof$sign=="cancer but it doesn&rsquo;t matter"
      |prof$sign=="capricorn but it doesn&rsquo;t matter"
      |prof$sign=="gemini but it doesn&rsquo;t matter"
      |prof$sign=="leo but it doesn&rsquo;t matter"
      |prof$sign=="libra but it doesn&rsquo;t matter"
      |prof$sign=="pisces but it doesn&rsquo;t matter"
      |prof$sign=="sagittarius but it doesn&rsquo;t matter"
      |prof$sign=="scorpio but it doesn&rsquo;t matter"
      |prof$sign=="taurus but it doesn&rsquo;t matter"
      |prof$sign=="virgo but it doesn&rsquo;t matter"
)& !is.na(prof$sign), ]$sign <- "doesnt matter"

prof[(prof$sign=="aquarius and it matters a lot" 
      | prof$sign=="aries and it matters a lot"
      | prof$sign=="cancer and it matters a lot"
      |prof$sign=="capricorn and it matters a lot"
      |prof$sign=="gemini and it matters a lot"
      |prof$sign=="leo and it matters a lot"
      |prof$sign=="libra and it matters a lot"
      |prof$sign=="pisces and it matters a lot"
      |prof$sign=="sagittarius and it matters a lot"
      |prof$sign=="scorpio and it matters a lot"
      |prof$sign=="taurus and it matters a lot"
      |prof$sign=="virgo and it matters a lot"
)& !is.na(prof$sign), ]$sign <- "matters"

prof[(prof$sign=="aquarius and it&rsquo;s fun to think about" 
      | prof$sign=="aries and it&rsquo;s fun to think about"
      | prof$sign=="cancer and it&rsquo;s fun to think about"
      |prof$sign=="capricorn and it&rsquo;s fun to think about"
      |prof$sign=="gemini and it&rsquo;s fun to think about"
      |prof$sign=="leo and it&rsquo;s fun to think about"
      |prof$sign=="libra and it&rsquo;s fun to think about"
      |prof$sign=="pisces and it&rsquo;s fun to think about"
      |prof$sign=="sagittarius and it&rsquo;s fun to think about"
      |prof$sign=="scorpio and it&rsquo;s fun to think about"
      |prof$sign=="taurus and it&rsquo;s fun to think about"
      |prof$sign=="virgo and it&rsquo;s fun to think about"
)& !is.na(prof$sign), ]$sign <- "fun to think about"

#############################################################################################################
## Speaks
#############################################################################################################

prof$speaks[which(prof$speaks != "english"  & prof$speaks != "english (fluently)"
  & prof$speaks != "english (fluently), spanish (poorly)" 
  & prof$speaks != "english (fluently), spanish (okay)"
  & prof$speaks != "english (fluently), spanish (fluently)")] <- "other"

######################################################################################################33######
# [1] "age"         "body_type"   "diet"        "drinks"      "drugs"       "education"  
# [7] "essay0"      "essay1"      "essay2"      "essay3"      "essay4"      "essay5"     
# [13] "essay6"      "essay7"      "essay8"      "essay9"      "ethnicity"   "height"     
# [19] "income"      "job"         "last_online" "location"    "offspring"   "orientation"
# [25] "pets"        "religion"    "sex"         "sign"        "smokes"      "speaks"     
# [31] "status"


# prof <- prof[c("age", "body_type", "diet", "drinks", "drugs", "education",
#                "height", "income", "job", "last_online", "offspring", "orientation", "sex",
#                "sign", "smokes", "status", "religion", "seriousness")]

prof <- as.data.frame(prof)


f <- sapply(prof, is.character)
prof[f] <- lapply(prof[f], factor)
rm(f)

library(tree)
prof <- na.tree.replace(prof)
library(dummies)
library(dplyr)
select(prof, -religion)
dummy.prof <- dummy.data.frame(select(prof, -religion, -seriousness))
dummy.prof <- cbind(dummy.prof, select(prof, religion, seriousness))

str(prof)
