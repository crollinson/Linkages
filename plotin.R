plotin <- function(iplot,basesc,basesn){
  iplot <<- iplot + 1
  max.ind <<- 1500
  
  ntrees <<- matrix(0,1,max.ind)
  dbh <<- matrix(0,1,max.ind)
  nogro <<- matrix(0,1,max.ind)
  ksprt <<- matrix(0,1,max.ind)
  iage <<- matrix(0,1,max.ind)
  
  C.mat <<- matrix(0,100,15)
  C.mat[1,1] <<- basesc
  C.mat[1,2] <<- basesn
  C.mat[1,5] <<- 18
  ncohrt <<- 1
  
  tyl <<- matrix(0,1,20)

}