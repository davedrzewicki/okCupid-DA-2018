library(FactoMineR)

setwd("~/Documents/data analytics/assignment 6")
source("A6cleaning.R")

str(prof) ## essay 0 through essay 9 are numeric


res<-MCA(prof, quanti.sup=c(which(colnames(prof)=="essay0"), 
  which(colnames(prof)=="essay1"), which(colnames(prof)=="essay2"), 
  which(colnames(prof)=="essay3"), which(colnames(prof)=="essay4"),
  which(colnames(prof)=="essay5"), which(colnames(prof)=="essay6"), 
  which(colnames(prof)=="essay7"), which(colnames(prof)=="essay8"), 
  which(colnames(prof)=="essay9")), 
  quali.sup=c(which(colnames(prof)=="religion"),
  which(colnames(prof)=="seriousness")))

summary(res)
dimdesc(res)
plot(res,invisible=c("var","quali.sup"),cex=0.7)
## there's a tail. hmm
plot(res,invisible=c("ind","quali.sup"), autoLab="y", cex=0.6)
plot(res,invisible=c("ind","quali.sup"), autoLab="y", cex=0.6, axes = (3:4))
plot(res,invisible=c("ind","quali.sup"), autoLab="y", cex=0.6, axes = (3:4), 
  selectMod = "contrib 20")
## note that the NA's play a starring role
## need to analyze without the NA's too
## see that drinks desperately, some high school, drugs are farthest away
## from the others, and close to each other !
plot(res,invisible=c("ind"),cex=0.7)
plot(res,invisible=c("ind", "var"), autoLab="y", cex=0.8)


plot(res,invisible=c("ind","quali.sup"), autoLab="y", cex=0.8, selectMod = "cos2 10")
plot(res,invisible=c("ind","quali.sup"), autoLab="y", cex=0.8, selectMod = "contrib 10")
plot(res,invisible=c("ind","quali.sup"), autoLab="y", cex=0.8, selectMod = "contrib 20")

samp <-sample_n(prof, 500)

resSmall<-MCA(samp, quanti.sup=c(which(colnames(prof)=="essay0"), 
  which(colnames(prof)=="essay1"), which(colnames(prof)=="essay2"), 
  which(colnames(prof)=="essay3"), which(colnames(prof)=="essay4"),
  which(colnames(prof)=="essay5"), which(colnames(prof)=="essay6"), 
  which(colnames(prof)=="essay7"), which(colnames(prof)=="essay8"), 
  which(colnames(prof)=="essay9")), 
  quali.sup=c(which(colnames(prof)=="religion"),
  which(colnames(prof)=="seriousness")))

plotellipses(resSmall, keepvar = "religion", 
  keepnames = FALSE, label = "none", xlim = c(-5,5))

plotellipses(resSmall, keepvar = "seriousness", 
  keepnames = FALSE, label = "none", xlim = c(-10,5))

