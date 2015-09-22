rm(list = ls())
linkages <- function(){
  
  iplot = 0 #starting the counter for number of plots
  klast = 1 #number of plots
  
  input(nyear = 2) #add tables with species parameter values and initial conditions and temperature and precip means
  
  for(k in 1:klast){ #loop over plots
    plotin(iplot = 1, basesc = 74, basesn = 1.64, max.ind = 1500) # initializes storage matrices with zeros for each plot
    kyr = 0 #current year is zero
    
    for(i in 1:nyear){
      kyr = i
      tempe(temp.vec = temp.mat[i,]) #calculates degree days for the year
      
      moist(kyr = kyr, temp.vec = temp.mat[i,], precip.vec = precip.mat[i,],
            fc = 28, dry = 14, bgs = 127, egs = 275, plat = 45, clat = clat) #calculates aet
      
      decomp(fdat = fdat, aet = aet, ncohrt = ncohrt, fc = 28, dry = 14,tyl = rep(2,20))
      
      gmult(bgs = 127, egs = 275, availn = availn, degd = 2000, dmin = spp.params$DMIN,
            dmax = spp.params$DMAX, d3 = spp.params$D3, fj = fj, cm1 = spp.params$CM1,
            cm3 = spp.params$CM3, cm2 = spp.params$CM2, cm4 = spp.params$CM4,
            cm5 = spp.params$CM5, nspec = 12)
      
      birth(nspec = 12, ntrees = ntrees, frt = spp.params$FRT, iage = iage, 
            slta = spp.params$SLTA, sltb = spp.params$SLTB, dbh = dbh,
            fwt = spp.params$FWT, switch.mat = matrix(TRUE,12,5),
            degd = degd, dmin = spp.params$DMIN, dmax = spp.params$DMAX,
            frost = spp.params$FROST, rt = temp.vec[i,], itol = spp.params$ITOL,
            mplant = spp.params$MPLANT, nogro = nogro,
            ksprt = ksprt)
      
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