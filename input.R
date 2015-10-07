##' @title LINKAGES input function
##' @author Ann Raiho
input <- function(nyear){
  fdat <<- read.csv("fdat.csv") #litter quality parameters
  spp.params <<- read.csv("spp_matrix.csv") #species parameter matrix
  temp.mat <<- matrix(c(-8.6,-7.6,-1.9,6.9,13.7,19,21.6,20.5,15.9,9.6,.8,-6.1),nyear,12,byrow = TRUE)
  precip.mat <<- matrix(c(2.9,2.7,4.2,7,9.2,11.2,8,8.9,8.9,5.7,5.5,2.9),nyear,12,byrow=TRUE)
  clat <<- as.matrix(read.csv("clat.csv",header = FALSE))
  load("switch.mat.Rdata")
  
  }