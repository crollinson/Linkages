moist <- function(temp.vec,precip.vec,VR,fc,dry,bgs,egs,plat){
  lat = (plat + .5) - 24
  xfc = 10*fc
  water = fc
  
  aet = 0
  accpwl = 0
  te = 0
  rsave = temp.vec[1]
  
  for(k in 1:12){
    if(temp.vec[k]<0) temp.vec[k] = 0
    te = te + (.2 * temp.vec[k]) * 1.514
  }
  
  a = .675 * te * 3 - 77.1 * te * 2 + 17920 * te + 492390
  a = .000001 * a
  
  dd = 0
  cday = 15
  
  yr = kyr
  
  for(k in 1:12){
    owater = water
    nct = nct + 1
    if(nct = 2) nct = 0
    
    rain = precip.vec
  }
  
}