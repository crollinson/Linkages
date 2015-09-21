rm(list = ls())
linkages <- function(){
  
  iplot = 0
  
  source("input.R")
  
  for(k in 1:klast){
    plotin(iplot = 1,basesc = 74,basesn = 1.64) # initializes storage matrices with zeros
    kyr = 0 #current year is zero
    
    for(i in 1:nyear){
      kyr = i
      temp(temp.vec = temp.mat[kyr,]) #calculates degree days for the year
      moist(kyr) 
      decomp(fdat = fdat, aet = 30, ncohrt = 12, fc = 7, dry = 10)
      gmult(egs = 134, bgs = 250, availn = .4, degd = 120, dmin = rep(1,12),
            dmax = rep(3,12), d3 = rep(4,12),fj = .3, cm1 = rep(1.3,12),
            mc3 = rep(9,12), avlmc = 14, cm2 = rep(.3,12), cm4 = rep(3,12),
            cm5 = rep(.5,12), nspec = 12)
      birth(nspec = 12,ntrees = seq(1,100,1),frt = rep(2,12), iage  = rep(1,1500), 
            slta = rep(.5,12), sltb = rep(.3,12), dbh = rep(1,1500),
            fwt = rep(10,12), switch.mat = matrix(TRUE,12,5),
            degd = 120, dmin = rep(5,12), dmax = rep(5,12), frost = rep(-5,12),
            rt = rep(10,12), itol =rep(2,12), mplant = rep(20,75), nogro = rep(2,1500),
            ksprt = seq(1,100,1))
      grow(nspec = 12, ntrees = seq(1,100,1),frt = rep(2,12), slta = rep(.5,12),
           sltb = rep(.3,12), dbh = rep(1,1500), fwt = rep(10,12), b2 = rep(10,12),
           b3 = rep(10,12), itol =rep(2,12), g = rep(1.2,12), degdgf = rep(120,12),
           smgf = rep(.2,12), sngf= rep(.2,12))
      kill(ntrees= seq(1,100,1),slta = rep(.5,12), sltb = rep(.3,12),
           dbh = rep(1,1500), agemx = rep(100,12),ksprt = seq(1,100,1),
           sprtmn = rep(5,12), sprtmx = rep(5,12), iage  = rep(1,1500),
           nogro  = rep(1,1500),tl = rep(3,12),rtst = rep(9,12))
    }
    
  }
  
}