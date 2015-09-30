rm(list = ls())
linkages <- function(klast = 1,nyear = 2){
  
  klast = 5
  nyear = 5
  
  iplot = 0 #starting the counter for number of plots
  klast = klast #number of plots
  nspec = 12
  fc = 28
  dry = 14
  bgs = 127
  egs = 256
  max.ind = 1500
  plat = 45
  
  source("input.R")
  input(nyear = nyear) #add tables with species parameter values and initial conditions and temperature and precip means
  
  source("plotin.R")
  source("tempe.R")
  source("moist.R")
  source("decomp.R")
  source("gmult.R")
  source("birth.R")
  source("grow.R")
  source("kill.R")
  source("output.R")
  
  #Storage
  
  tstem = matrix(0,nyear,klast) #number of stems
  tab = matrix(0,nyear,klast) #total aboveground biomass
  fl = matrix(0,nyear,klast) #leaf litter
  totl = matrix(0,nyear,klast) #leaf litter N
  tnap = matrix(0,nyear,klast) #net aboveground production
  avln = matrix(0,nyear,klast) #available nitrogen
  cn = matrix(0,nyear,klast) #humus C:N ratio
  sco2c = matrix(0,nyear,klast) #soil co2 evolution
  som = matrix(0,nyear,klast) #soil organic matter

  for(k in 1:klast){ #loop over plots
    
    iplot <- iplot + 1 #counter for plots
    plotin.out <- plotin(iplot = iplot, basesc = 74, basesn = 1.64, max.ind = max.ind) # initializes storage matrices with zeros for each plot
    
    kyr = 0 #current year is zero
    ncohrt <- unlist(plotin.out$ncohrt)
    tyl <- unlist(plotin.out$tyl)
    C.mat <- unlist(plotin.out$C.mat)
    ntrees <- unlist(plotin.out$ntrees)
    dbh <- unlist(plotin.out$dbh)
    nogro <- unlist(plotin.out$nogro)
    ksprt <- unlist(plotin.out$ksprt)
    iage <- unlist(plotin.out$iage)
    
    
    for(i in 1:nyear){
      kyr = i
      
      tempe.out <- tempe(temp.vec = temp.mat[i,]) #calculates degree days for the year
      
      degd = unlist(tempe.out$degd)
      
      moist.out <- moist(kyr = kyr, temp.vec = temp.mat[i,], precip.vec = precip.mat[i,],
            fc = fc, dry = dry, bgs = bgs, egs = egs, plat = plat, clat = clat) #calculates aet
      
      aet <- unlist(moist.out$aet) ###### START HERE ###### AET TOO SMALL
      fj <- unlist(moist.out$fj)
      
      decomp.out <- decomp(fdat = fdat, aet = aet,
                           ncohrt = ncohrt, fc = 28, dry = 14,
                           tyl = tyl, C.mat = C.mat)
      
      ff <- unlist(decomp.out$ff)
      availn <- unlist(decomp.out$availn)
      tyln <- unlist(decomp.out$tyln)
      hcn <- unlist(decomp.out$hcn)
      sco2 <- unlist(decomp.out$sco2)
      
      gmult.out <- gmult(bgs = bgs, egs = egs, availn = availn,
                        degd = degd, dmin = spp.params$DMIN,
                        dmax = spp.params$DMAX, d3 = spp.params$D3, fj = fj,
                        cm1 = spp.params$CM1, cm3 = spp.params$CM3, cm2 = spp.params$CM2,
                        cm4 = spp.params$CM4, cm5 = spp.params$CM5, nspec = nspec)
      
      smgf <- unlist(gmult.out$smgf)
      sngf <- unlist(gmult.out$sngf)
      degdgf <- unlist(gmult.out$degdgf)
      availn <- unlist(gmult.out$availn)
      
     birth.out <- birth(nspec = nspec, ntrees = ntrees, frt = spp.params$FRT, iage = iage, 
            slta = spp.params$SLTA, sltb = spp.params$SLTB, dbh = dbh,
            fwt = spp.params$FWT, switch.mat = switch.mat,
            degd = degd, dmin = spp.params$DMIN, dmax = spp.params$DMAX,
            frost = spp.params$FROST, rt = temp.mat[i,], itol = spp.params$ITOL,
            mplant = spp.params$MPLANT, nogro = nogro,
            ksprt = ksprt, sprtnd = spp.params$SPRTND)
     
     ntrees <- unlist(birth.out$ntrees)
     dbh <- unlist(birth.out$dbh)
     nogro <- unlist(birth.out$nogro)
     ksprt <- unlist(birth.out$ksprt)
     iage <- unlist(birth.out$iage)
      
     grow.out <- grow(max.ind = max.ind, nspec = nspec, ntrees = ntrees, frt = spp.params$FRT, slta = spp.params$SLTA,
           sltb = spp.params$SLTB, dbh = dbh, fwt = spp.params$FWT, b2 = spp.params$B2,
           b3 = spp.params$B3, itol =spp.params$ITOL, g = spp.params$G, degdgf = degdgf,
           smgf = smgf, sngf= sngf,frost = spp.params$FROST, rt = temp.mat[i,])
     
     ntrees <- unlist(grow.out$ntrees)
     dbh <- unlist(grow.out$dbh)
     awp <- unlist(grow.out$awp)
      
    kill.out<- kill(nspec = nspec, ntrees= ntrees,slta = spp.params$SLTA, sltb = spp.params$SLTB,
           dbh = dbh, agemx = spp.params$AGEMX, ksprt = ksprt,
           sprtmn = spp.params$SPRTMN, sprtmx = spp.params$SPRTMX, iage  = iage,
           nogro  = nogro,tl = spp.params$TL,rtst = spp.params$RTST, fwt = spp.params$FWT)
   
    ntrees <- unlist(kill.out$ntrees)
    dbh <- unlist(kill.out$dbh)
    nogro <- unlist(kill.out$nogro)
    ksprt <- unlist(kill.out$ksprt)
    iage <- unlist(kill.out$iage)
    ncohrt <- unlist(kill.out$ncohrt)
    tyl <- unlist(kill.out$tyl)
    
    output.out <- output(availn = availn, tyln = tyln, nspec = nspec, frt=spp.params$FRT,
                         iage = iage,slta = spp.params$SLTA,
                         sltb = spp.params$SLTB,dbh = dbh,fwt = spp.params$FWT,tyl = tyl)
    
    tstem[kyr,iplot] = unlist(output.out$atot) #number of stems
    tab[kyr,iplot] = unlist(output.out$tbar) #total aboveground biomass
    fl[kyr,iplot] = unlist(kill.out$tyl)[17] #leaf litter
    totl[kyr,iplot] = unlist(output.out$tyln) #leaf litter N
    tnap[kyr,iplot] = unlist(output.out$tynap) #net aboveground production
    avln[kyr,iplot] = unlist(gmult.out$availn) #available nitrogen
    cn[kyr,iplot] = unlist(decomp.out$hcn) #humus C:N ratio
    sco2c[kyr,iplot] = unlist(decomp.out$sco2) #soil co2 evolution
    som[kyr,iplot] = unlist(decomp.out$ff[19,2]) #soil organic matter
    
    }
    
  }
  
}