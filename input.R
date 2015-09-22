input <- function(nyear){
  fdat <<- read.csv("fdat.csv") #litter quality parameters
  spp.params <<- read.csv("spp_matrix.csv") #species parameter matrix
  temp.mat <<- matrix(c(seq(0,27,length.out = 6),rev(seq(0,27,length.out = 6))),nyear,12,byrow = TRUE)
  precip.mat <<- matrix(rep(10,12),nyear,12)
  clat <<- as.matrix(read.csv("clat.csv",header = FALSE))
  
  }