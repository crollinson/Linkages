plotin <- function(iplot,basesc,basesn,max.ind){
  iplot <<- iplot + 1 #counter for plots
  
  ntrees <<- matrix(0,1,max.ind) #contains number of trees for each species
  dbh <<- matrix(0,1,max.ind) #contains diameter at breast height for each tree
  nogro <<- matrix(0,1,max.ind) #used to flag slow growing trees
  ksprt <<- matrix(0,1,max.ind) #used to flag trees eligible to sprout
  iage <<- matrix(0,1,max.ind) #contains the age of each tree
  
  C.mat <<- matrix(0,100,15) #contains data on litter cohorts
  C.mat[1,1] <<- basesc #initial humus weight
  C.mat[1,2] <<- basesn #initial humus nitrogen content
  C.mat[1,5] <<- 18 #"litter type" for humus
  ncohrt <<- 1 #if ncohrt = 1 only humus is present
  
  tyl <<- matrix(0,1,20) #contains this year's litter

}