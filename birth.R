birth <- function(nspec,ntrees,frt,iage,slta,sltb,dbh,fwt,switch.mat,
                  degd,dmin,dmax,frost,rt,itol,mplant,nogro,ksprt,sprtnd){
  #initialize foliage biomass (folw) and foliage area (fola)
  folw = 0
  fola = 0
  nl = 1
  
  #calculate leaf weight in G/plot and leaf area index
  for(i in 1:nspec){
    if(ntrees[i] != 0){
      nu = nl + ntrees[i] - 1
      ret = frt[i]
      
      for(k in nl:nu){
        age = iage[k]
        if(age<ret){
          ret = age
          folw = folw + ((slta[i]+sltb[i]*dbh[k])/2)^2*(3.14*fwt[i]*ret)
          fola = fola + ((1.9283295) * 10^-4)*((dbh[k])^2.129)
        }
      }
      nl = nl + ntrees[i]
    }
  }
  
  #calculate amount of light at forest floor
  al = 1 * exp(-folw/93750)
  #calculate number of trees in stand
  ntot = nl - 1
  #determine which species are eligible for planting this year
  swtch = rep(NA,5) #added this AMR
  #switch 1 is true if the spp requires leaf litter for successful recruitment
  #switch 2 is true if the spp requires minerl soil
  #switch 3 is true if the spp recruitment is reduced by hot year
  #switch 4 is true if the species is a preferred food of deer or small mammals
  #switch 5 reduces seedling rate of desirable mast
  for(j in 1:5){
    swtch[j] = TRUE
  }
  swtch[3] = FALSE
  
  #set switches based on value of leaf area, degree day, and random number
  if(fola>=1) swtch[1] = FALSE
  if(fola<=2) swtch[2] = FALSE
  
  #browse - a random number simulating the occurence of browsing
  yfl = runif(1,1,10) #is this the right distribution? AMR
  browse = yfl
  if(browse > .5) swtch[4] = FALSE
  if(fola < .05) swtch[5] = FALSE
  nw = 0
  
  newtr = matrix(0,1,100) #added this AMR
  
  # end recruitment of aspen, pin cherry, and most pine if available light is < 60% of full sunlight and recruitment of paper birch and white pine if available light is <30% of full sunlight
  for(i in 1:nspec){
#     if(al<.60 & i == 20) break
#     if(al<.30 & i == 25) break
#     if(al<.60 & i == 58) break
#     if(al<.60 & i == 59) break
#     if(al<.60 & i == 60) break
#     if(al<.60 & i == 30) break
#     if(al<.60 & i == 31) break
#     if(al<.60 & i == 32) break
#     if(al<.30 & i == 33) break
#     if(al<.60 & i == 34) break
#     if(al<.60 & i == 35) break
#     if(al<.30 & i == 55) break
    for(k in 3:5){
      if(switch.mat[i,k] & swtch[k]) next
    }
    #allow only those spp whose degree day tolerances span the simulated degree days this year to be eligible for seeding
    if(degd < dmin[i] | degd > dmax[i]) next
    #allow only those species whose frost tolerance is less than the January mean temperature to be eligible for seeding
    if(frost[i] > rt[1]) next
    #place eligible species numbers in array newtr
    nw = nw + 1
    newtr[nw] = i
  }
  
  #check to see if there are any new trees
  itemp = iage; dtemp = iage; ntemp = iage # added this AMR
  if(nw != 0){
    #place iage, dbh, and nogro into temporary arrays 
    for(i in 1:ntot){
      itemp[i] = iage[i] #added storage above AMR
      dtemp[i] = dbh[i]
      ntemp[i] = nogro[i]
    }
    #begin main loop for planting
    for(k in 1:nw){
      nsp = newtr[k]
      #calculatee seedling light multipliers
      slite = 1.5 * (1 - exp(-1.136*(al-.08)))
      if(itol[nsp] < 2) slite = 1 - exp(-4.64*(al-.05))
      if(slite <= 0) slite = 0
      #reduce max number of seedlings to the extent that light, soil moisture, and degree days are less than optimum for growth of each species
      yfl = runif(1,1,10)
      nplant = mplant[nsp] * slite * smgf[nsp] * degdgf[nsp] * yfl
      #see if any stumps of this spp are available for sprouting
      if(ksprt[nsp] > 0 & sprtnd[nsp] > 0){
        yfl = runif(1,1,10)
        #if available light is greater than 50% of full sunlight determine number of stump sprouts and add to nplant
        if(al > .5) nplant = nplant + (sprtnd[nsp]*slite*smgf[nsp]*degdgf[nsp]*ksprt[nsp]*yfl)
      }
      nsum = 0
      for(i in 1:nsp){
        nsum = nsum + ntrees[i]
      }
      #plant seedlings and sprouts
        nl = nsum + 1
        nup = ntot
        if(nplant == 0) next
        for(j in 1:nplant){
          ntot = ntot + 1
          if(ntot > 1500) print("too many trees")
          nsum = nsum + 1
          ntrees[nsp] = ntrees[nsp] + 1
          itemp[nsum] = 0
          #calculate dbh for new trees
          size = 1.27
          yfl = runif(1,0,1)
          dtemp[nsum] = size + .3 * (1 - yfl)^3
          ntemp[nsum] = 0
        }
        if(nl <= nup){
          n1 = nsum + 1
          for(l in nl:nup){
            dtemp[n1] = dbh[l]
            itemp[n1] = iage[l]
            ntemp[n1] = nogro[l]
            n1 = n1 + 1
          }
        }
        #reinitialize original dbh and age arrays - including new trees
        for(i in 1:ntot){
          iage[i] = itemp[i]
          dbh[i] = dtemp[i]
          nogro[i] = ntemp[i]
        }
    }
  }
  #increment ages by one year
  for(i in 1:ntot){
    iage[i] = iage[i] + 1
  }
  #reinitialize array ksprt
  for(i in 1:nspec){
    ksprt[i] = 0
  }
  return(list(iage=iage, dbh=dbh, nogro=nogro, ntrees=ntrees, newtr = newtr,ksprt=ksprt))
}

