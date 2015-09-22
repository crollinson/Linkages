decomp <- function(fdat,aet,ncohrt,fc,dry,tyl){
  
  #Initialization
  
  fco2 = 0
  hco2 = 0
  sco2 = 0
  ffw = 0 
  tnimob = 0
  availn = 0 
  fnmin = 0
  hnmin = 0
  tnmin = 0
  tyln = 0 
  ff = matrix(0,20,3)
  
  #Calculate Litter N
  for(i in 1:12){
    tyln = tyln + tyl[i]*fdat[i,2]
  }
  
  #Calculate AET Multiplier
  xaet = aet
  if(xaet > 600) xaet = 600
  aetm = (-1 * xaet) / (-1200 + xaet)
  
  for(i in 1:16){
    if(tyl[i]!=0){
      ncohrt = ncohrt + 1
      if(ncohrt>100) print("ncohrt error")
      C.mat[ncohrt,1] = tyl[i] * fdat[i,10]
      C.mat[ncohrt,2] = tyl[i] * fdat[i,2]
      for(j in 3:9){
        C.mat[ncohrt,j] = fdat[i,j]
      }
      C.mat[ncohrt,10] = tyl[i] * fdat[i,10]
      C.mat[ncohrt,11] = fdat[i,2]
      C.mat[ncohrt,12] = fdat[i,7] * 1.7039 + .0955
      if(C.mat[ncohrt,5]==14) C.mat[ncohrt,12]=.3
      if(C.mat[ncohrt,5]==15) C.mat[ncohrt,12]=.3
      if(C.mat[ncohrt,5]==16) C.mat[ncohrt,12]=.3
    }
  }
  
  tyll = tyl[17]
  ccll = 1.54 + .0457 * (fc - dry) #ccll = 1?
  if(tyll > ccll) tyll = ccll
  decmlt = 1 + (-.5+.075*(fc-dry))*(1-tyll/ccll)
  
  if(ncohrt!=1){
    for(i in 2:ncohrt){
      pwtlos = (.9804+.09352*aet)-((-.4956+.00193*aet)*(C.mat[i,7]/C.mat[i,11]))
      pwtlos = (decmlt*pwtlos)/100
      if(pwtlos>.99) pwtlos = .99
      
      lt = C.mat[i,5]
      if(lt==14) pwtlos = .1
      if(lt==15) pwtlos = .03
      if(lt==17) pwtlos = .05
      if(lt==16 & pwtlos>.2) pwtlos = .2
      
      wtloss = pwtlos*C.mat[i,1]
      pomr = (C.mat[i,1]-wtloss)/C.mat[i,10]
      
      C.mat[i,11] = C.mat[i,3] - C.mat[i,4] * pomr
      
      if(pomr<C.mat[i,12]){
        wtloss = C.mat[i,1] - C.mat[i,12]*C.mat[i,10]
        C.mat[i,11] = C.mat[i,3] - C.mat[i,4]*C.mat[i,12]
        
        deltan = C.mat[i,2] - C.mat[i,11] * (C.mat[i,1] - wtloss)
        if(deltan<0) tnimob = tnimob - deltan
        if(deltan>0) fnmin = fnmin + deltan
        
        if(C.mat[i,6]==1){
          C.mat[1,1] = C.mat[1,1] + C.mat[i,1] - wtloss
          C.mat[1,2] = C.mat[1,2] + C.mat[i,11] * (C.mat[i,1]-wtloss)
          C.mat[i,1] = 0
        }
        ffw = ffw + C.mat[i,1] - wtloss
        C.mat[i,1] = 0
      }
      
      if(C.mat[i,1]!=0){
        C.mat[i,1] = C.mat[i,1] - wtloss
        C.mat[i,2] = C.mat[i,1] * C.mat[i,11]
        C.mat[i,7] = C.mat[i,8] - C.mat[i,9] * (C.mat[i,1]/C.mat[i,10])
      }
      
      fco2 = fco2 + (wtloss*.48)
      tnimob = tnimob - .16 * tyln
    }
  }
  
  hnmin = C.mat[1,2] * .035 * decmlt * aetm
  hnnew = C.mat[1,2] - hnmin
  homnew = C.mat[1,1] * (hnnew/C.mat[1,2])
  hco2 = (C.mat[1,1] - homnew) * .48
  C.mat[1,1] = homnew
  C.mat[1,2] = hnnew
  hcn = (.48*C.mat[1,1])/C.mat[1,2]
  tnmin = fnmin - tnimob
  availn <<- tnmin - tnimob
  sco2 = fco2+hco2
  
  ix = 0
  for(i in 1:ncohrt){
    if(C.mat[i,1]!=0){
      for(j in 1:12){
        C.mat[(i-ix),j] = C.mat[i,j]
      }
    }
    ix = ix + 1
  }
  ncohrt = ncohrt - ix
  
  if(ffw != 0 ){
    ncohrt = ncohrt + 1
    if(ncohrt>1500) print("too many ncohrt")
    C.mat[ncohrt,1] = ffw
    C.mat[ncohrt,2] = ffw * fdat[17,2]
    for(j in 3:9){
      C.mat[ncohrt,j] = fdat[17,j]
    }
    C.mat[ncohrt,10] = ffw
    C.mat[ncohrt,11] = fdat[17,2]
    C.mat[ncohrt,12] = .5
  }
  
  for(i in 1:ncohrt){
    lt = C.mat[i,5]
    ff[lt,1] = C.mat[i,5]
    ff[lt,2] = ff[lt,2] + C.mat[i,1]
    ff[lt,3] = ff[lt,3] + C.mat[i,2]
  }
  ff[19,1] = 19
  for(lt in 1:12){
    ff[19,2] = ff[19,2] + ff[lt,2]
    ff[19,3] = ff[19,3] + ff[lt,3]
  }
  
  ff[19,2] = ff[19,2] + ff[18,2] + ff[13,2]
  ff[19,3] = ff[19,3] + ff[18,3] + ff[13,3]
}









