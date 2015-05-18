# Useful files: Output description.txt
# 
# Note: for many plots, have to change plotting parameter "mar" to 1,5,1,1
# 
# Use this one:
#   LOAD WORKSPACE
# 
# then load  [e.g. to get "imapge.plot"]
library(fields)
### Practice space for JMc shite
#magPCA1 holds PCA1 difs over time:
magPCA1 <- matrix(data = NA, nrow = 256, ncol = 1, byrow = FALSE, dimnames = NULL)

for(i in 1:256){magPCA1[i,] <- max(rPCA[i,1,]) - min(rPCA[i,1,])}

#Map magPCA1
magPCA1Map = function(rMat,locs,max=0.4,restrict=FALSE,legend=TRUE){
  if(!restrict){
    used=1:S
    m1=m2=sqrt(S)
  } else{
    used=49:(256-64)
    m1=16;m2=9
  }
  tmp=list()
  tmp$x=unique(locs[used,1])
  tmp$y=unique(locs[used,2])
  m1=length(tmp$x)
  m2=length(tmp$y)
  tmp$z=matrix(rMat[used],m1,m2)
  if(legend){
    image.plot(tmp$x,tmp$y,tmp$z,xaxt='n',yaxt='n',col=tim.colors(32),xlab="",ylab="")
  } else{
    image(tmp$x,tmp$y,tmp$z,col=tim.colors(32),xaxt='n',yaxt='n',xlab="",ylab="")
  }
  lines(ma$x,ma$y,lwd=2)
  lines(ct$x,ct$y,lwd=2)
  lines(ri$x,ri$y,lwd=2)
  lines(nh$x,nh$y,lwd=2)
  lines(ny$x,ny$y,lwd=2)
}

#Run magPCA1Map for restricted dmain
magPCA1Map(magPCA1,gridLocs,max=0.6,restrict=TRUE)

## Plotting pollen%/time (vegPlusPollenDiagram isn't working)
ages<-SilverAges
props<-SilverProp

par(mfrow=c(10,1),mar=c(1,5,1,1)) 

for(pp in 1:9){
  plot(ages,props[,pp],type='l',lwd=3,ylab=as.name(taxa[pp]),xlim=c(-50,2800),ylim=c(0,0.5),xaxt="n")
}

plot(ages,props[,10],type='l',lwd=3,ylab=as.name(taxa[10]),xlim=c(-50,2800),ylim=c(0,0.5))

## Next tree composition from STEPPS
Ncell<-136
ages<-(1:28) #*100+200

par(mfrow=c(10,1),mar=c(1,5,1,1)) 

for(pp in 1:9){
  plot(ages,rMean[Ncell,pp,],type='l',lwd=3,ylab=as.name(taxa[pp]),ylim=c(0,.5),xaxt="n")
  lines(ages,rLow[Ncell,pp,], col=2)
  lines(ages,rUp[Ncell,pp,], col=2)
  
}

plot(ages,rMean[Ncell,10,],type='l',lwd=3,ylab=as.name(taxa[10]),ylim=c(0,.5))
lines(ages,rLow[Ncell,10,], col=2)
lines(ages,rUp[Ncell,10,], col=2)

####