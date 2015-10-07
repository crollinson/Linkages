rm(list = ls())

library(reshape2)
library(ggplot2)
library(Matrix)

iplot = 1
nyear = 50
nspec = 10
fc = 38.8
dry = 20
bgs = 120
egs = 273
max.ind=15000
plat = 43.6

source("input.R")
load("switch.mat.Rdata")
input(nyear = nyear) #add tables with species parameter values and initial 
#conditions and temperature and precip means

source("linkages.R")
linkages.out <- linkages(iplot = iplot, nyear = nyear,nspec = nspec, fc = fc, dry = dry,
                         bgs = bgs, egs = egs, max.ind=max.ind, plat = plat)

dat<-linkages.out$ntrees.kill / linkages.out$ntrees.birth
dat<-linkages.out$bar

rownames(dat)<-spp.params[1:nspec,1]
  
plotdata = as.data.frame(dat[order(dat[,round(nyear/2),1],decreasing = TRUE),,1])

df <- melt(plotdata[1:10,])  #the function melt reshapes it from wide to long
df$rowid <- rownames(plotdata[1:10,])  #add a rowid identifying variable

quartz()
ggplot(df, aes(variable, value, group=factor(rowid))) + geom_line(aes(color=factor(rowid)))

nnzero(linkages.out$nogro.save)

