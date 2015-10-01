rm(list = ls())

#library(reshape2)
#library(ggplot2)

iplot = 1
nyear = 5
nspec = 50
fc = 27
dry = 18
bgs = 127
egs = 256
max.ind=15000
plat = 45

source("linkages.R")
linkages.out <- linkages(iplot = iplot, nyear = nyear,nspec = nspec, fc = fc, dry = dry,
                         bgs = bgs, egs = egs, max.ind=max.ind, plat = plat)

plotdata = as.data.frame(linkages.out$bar[,,1])

df <- melt(plotdata)  #the function melt reshapes it from wide to long
df$rowid <- spp.params[1:5,1]  #add a rowid identifying variable

ggplot(df, aes(variable, value, group=factor(rowid))) + geom_line(aes(color=factor(rowid)))
