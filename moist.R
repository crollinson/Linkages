moist <- function(kyr,temp.vec,precip.vec,fc,dry,bgs,egs,plat,clat){
  # adjust latitude pointer
  days = c(31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31.)
  lat = round((plat + .5) - 24) 
  
  # initialize water content of soil in january to fc
  xfc = 10*fc 
  water = fc
  
  #initialize thornthwaite parameters
  aet = 0 #actual evapotranspiration
  accpwl = 0 #accumulated potential water loss
  te = 0 #temperature efficiency
  rsave = temp.vec[1] 
  
  for(k in 1:12){
    if(temp.vec[k]<0) temp.vec[k] = 0
    te = te + (.2 * temp.vec[k]) * 1.514
  }
  a = .675 * te * 3 - 77.1 * te * 2 + 17920 * te + 492390
  a = .000001 * a
  
  #initializ the number of dry days (dd), and current day of year (cday)
  dd = 0
  cday = 15
  nct = 0
  
  yr = kyr
  
  #main loop for yearly water balance calculation by month
  for(k in 1:12){
    owater = water
    nct = nct + 1
    if(nct == 2) nct = 0
    #calculate this month's rainfall
    rain = precip.vec[k]
    ttmp = temp.vec[k]
    #calculate potential evapotranspiration (U)
    u = 1.6 * ((10*ttmp/te)^a)*clat[lat,k]
    #calculate potential water loss this month
    pwl = rain - u
    if(pwl < 0){
      accpwl = accpwl +pwl
      xacpwl = accpwl * 10
      water = fc*(exp((.000461-1.10559/xfc)*(-1*xacpwl)))
      if(water<0) water = 0
      csm = water - owater
      aet = aet + (rain-csm)
    } else {
      if(water>=fc) water = fc
      csm = water-owater
      accpwl = accpwl +csm
      if(water>=fc) accpwl = 0
      aet = aet + u
    }
    ocday = cday
    cday = cday + days[k]
    ddi = 0
    
    if(cday <= bgs) next
    if(ocday >= egs) next
    if(owater >= dry & water >= dry) next
    if(owater>dry & water<dry){
      ddi = days[k] *(dry-water)/(owater-water)
      if(ocday < bgs & cday >bgs) ddi = min(ddi,(cday-bgs))
      if(ocday<egs & cday>egs) ddi = egs - cday + ddi
      if(ddi < 0) ddi = 0
      dd = dd + ddi
    }
    if(owater<dry & water>dry){
      ddi = days[k] * (dry-owater)/(water-owater)
      if(ocday<bgs & cday>bgs) ddi = ocday + ddi - bgs
      if(ddi<0) ddi = 0
      if(ocday<egs & cday >egs) ddi = mis(ddi, (egs-ocday))
      dd = dd + ddi
    }
    fj <<- dd
    temp.vec[1] = rsave
    aet <<- aet * 10
  }
  return(list(aet=aet,fj=fj))
}