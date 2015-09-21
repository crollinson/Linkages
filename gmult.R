gmult <- function(egs,bgs,availn,degd,dmin,dmax,d3,fj,cm1,mc3,avlmc,cm2,cm4,cm5,nspec){
  tgs = egs - bgs + 1 
  
  availn = availn + .005
  avlmc = -170 + 4*(availn*1000)
  
  degdgf = matrix(0,1,nspec)
  smgf = matrix(0,1,nspec)
  sngf = matrix(0,1,nspec)
  
  for(i in 1:nspec){
    degdgf[i] = (4*(degd - dmin[i])*(dmax[i] - degd)) / ((dmax[i] - dmin[i]) ^ 2)
    
    if(degdgf[i] < 0 ) degdgf[i] = 0
    if(degdgf[i] != 0){
      drout = d3[i] * tgs
      if(drout < fj) drout = fj
      smgf[i] = sqrt((drout - fj)/drout)
      if(smgf[i] != 0){
        conn = cm1[i] * (1 - 10^((-1*cm3[i])*avlmc+cm2[i]))
        sngf[i] = cm4[i] + cm5[i] * conn
        if(sngf[i]<0) sngf[i] = 0
      }
    } 
  }
  

  
  
}