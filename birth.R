birth <- function(nspec,ntrees,frt,iage,slta,sltb,dbh,fwt,switch.mat,
                  degd,dmin,dmax,frost,rt,itol,mplant,nogro,ksprt){
  folw = 0
  fola = 0
  nl = 1
  
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
  
  al = 1 * exp(-folw/93750)
  ntot = nl - 1
  swtch = rep(NA,5)
  
  for(j in 1:5){
    swtch[j] = TRUE
  }
  swtch[3] = FALSE
  
  if(fola>=1) swtch[1] = FALSE
  if(fola<=2) swtch[2] = FALSE
  
  yfl = runif(1,1,10) #is this the right distribution?
  
  browse = yfl
  if(browse > .5) swtch[4] = FALSE
  if(fola < .05) swtch[5] = FALSE
  nw = 0
  
  newtr = matrix(0,1,100)
  
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
    if(degd < dmin[i] | degd > dmax[i]) next
    if(frost[i] > rt[1]) next
    nw = nw + 1
    newtr[nw] = i
  }
  
  if(nw != 0){
    for(i in 1:ntot){
      itemp[i] = iage[i]
      dtemp[i] = dbh[i]
      ntemp[i] = nogro[i]
    }
    for(k in 1:nw){
      nsp = newtr[k]
      slite = 1.5 * (1 - exp(-1.136*(al-.08)))
      if(itol[nsp] < 2) slite = 1 - exp(-4.64*(al-.05))
      if(slite <= 0) slite = 0
      
      yfl = runif(1,1,10)
      nplant = mplant[nsp] * slite * smgf[nsp] * degdgf[nsp] * yfl
      
      if(ksprt[nsp] > 0 & sprtnd[nsp] > 0){
        yfl = runif(1,1,10)
        if(al > .5) nplant = nplant + (sprtnd[nsp]*slite*smgf[nsp]*degdgf[nsp]*ksprt[nsp]*yfl)
      }
      nsum = 0
      for(i in 1:nsp){
        nsum = nsum + ntrees[i]
      }
        nl = nsum + 1
        nup = ntot
        if(nplant == 0) next
        for(j in 1:nplant){
          ntot = ntot + 1
          if(ntot > 1500) print("too many trees")
          nsum = nsum + 1
          ntrees[nsp] = ntrees[nsp] + 1
          itemp[nsum] = 0
          size = 1.27
          yfl = runif(1,1,10)
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
        for(i in 1:ntot){
          iage[i] = itemp[i]
          dbh[i] = dtemp[i]
          nogro[i] = ntemp[i]
        }
    }
  }
  
  for(i in 1:ntot){
    iage[i] = iage[i] + 1
  }
  for(i in 1:nspec){
    ksprt[i] = 0
  }
  
}

