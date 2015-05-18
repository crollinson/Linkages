# basic plotting functions for assessing output

require('fields')

taxaNames = c("beech","birch","chestnut","hemlock","hickory","maple","oak","pine","spruce","other")

taxaSignif=function(p1,p2,rIts,locs,restrict=FALSE,ylab='',xlab=''){
  # compares significance of differences in composition between two predictions
  # red colors indicate more of the taxon in the older time period; blue in the newer
  nComparisons=length(rIts[1,1,])
  less=rIts[,p1,]<rIts[,p2,]
  less=matrix(apply(less,1,sum),sqrt(S),sqrt(S))
  more=nComparisons-less
  if(!restrict){
    used=1:S
    m1=m2=sqrt(S)
    rows=1:16
  } else{
    used=49:(256-64)
    m1=16;m2=9
    rows=4:12
  }
  
  xs=sort(unique(locs[,1]))
  ys=sort(unique(locs[,2]))[rows]
  image(xs,ys,more[,rows],zlim=c(.9*nComparisons,nComparisons),xlab=xlab,ylab=ylab,col=tim.colors(32)[21:32])
  image(xs,ys,less[,rows],zlim=c(.9*nComparisons,nComparisons),xlab=xlab,ylab=ylab,col=tim.colors(32)[12:1],add=TRUE)
  
  lines(ma$x,ma$y,lwd=1)
  lines(ct$x,ct$y,lwd=1)
  lines(ri$x,ri$y,lwd=1)
  lines(ny$x,ny$y,lwd=1)
  lines(nh$x,nh$y,lwd=1)
}

timeSignif=function(p,rItsNew,rItsOld,locs,restrict=FALSE,ylab='',xlab=''){
  # compares significance of differences in composition between two predictions
  # red colors indicate more of the taxon in the older time period; blue in the newer
  nComparisons=length(rItsNew[1,1,])
  less=rItsNew[,p,]<rItsOld[,p,]
  less=matrix(apply(less,1,sum),sqrt(S),sqrt(S))
  more=nComparisons-less
  if(!restrict){
    used=1:S
    m1=m2=sqrt(S)
    rows=1:16
  } else{
    used=49:(256-64)
    m1=16;m2=9
    rows=4:12
  }
  
  xs=sort(unique(locs[,1]))
  ys=sort(unique(locs[,2]))[rows]
  image(xs,ys,more[,rows],zlim=c(.9*nComparisons,nComparisons),xlab=xlab,ylab=ylab,col=tim.colors(32)[21:32])
  image(xs,ys,less[,rows],zlim=c(.9*nComparisons,nComparisons),xlab=xlab,ylab=ylab,col=tim.colors(32)[12:1],add=TRUE)
  
  lines(ma$x,ma$y,lwd=1)
  lines(ct$x,ct$y,lwd=1)
  lines(ri$x,ri$y,lwd=1)
  lines(ny$x,ny$y,lwd=1)
  lines(nh$x,nh$y,lwd=1)
}

feature=function(p,less,locs,restrict=FALSE,nComparisons=10000){
  # plots feature significance for one prediction for one taxon
  par.old=par()
  less=less[[p]]
  more=nComparisons-less
  diag(more)=0
  if(!restrict){
    used=1:S
    m1=m2=sqrt(S)
    rowseq=16:1
  } else{
    used=49:(256-64)
    m1=16;m2=9
    rowseq=12:4
  }
  par(mfrow=c(m2,m1),mai=c(0,0,0,0),xaxt='n',yaxt='n')
  for(row in rowseq){
    for(col in 1:16){
      image(1:m1,1:m2,matrix(more[((row-1)*16+col),][used],m1,m2),zlim=c(.9*nComparisons,nComparisons),xlab='',ylab='',col=tim.colors(32)[21:32],cex.axis=2,yaxt='n',xaxt='n')
      image(1:m1,1:m2,matrix(less[((row-1)*16+col),][used],m1,m2),zlim=c(.9*nComparisons,nComparisons),xlab='',ylab='',col=tim.colors(32)[12:1],cex.axis=2,yaxt='n',xaxt='n',add=TRUE)
      text(col,row-min(rowseq)+1.001,'x',cex=1.4)  # can't figure out why this is not printing 'x's on bottom row
    }
  }
  par(par.old)
}



# modify this to deal better with input format
timePlot=function(times,mean,lower=NULL,upper=NULL,ylim=NULL,xlab='',ylab=''){
  # plots time series with confidence bars (e.g., to plot coefficient values over time)
  par(mfrow=c(2,5))
  for(p in 1:ncol(mean)){
    plot(-1*times,mean[,p],type='l',ylim=range(c(lower,upper,mean)),ylab=ylab,xlab=xlab)
    points(-1*times,mean[,p],pch=16,cex=.8)
    lines(-1*times,lower[,p],col=2,lty=2)
    lines(-1*times,upper[,p],col=2,lty=2)
    abline(h=0,col='gray',lty=3)
    title(taxa[p])
  }
}

timePlotDecomp=function(times,mean,lower=NULL,upper=NULL,its=NULL,ylim=NULL,xlab='',ylab=''){
  # plots time series with confidence bars (e.g., to plot coefficient values over time)
  mns=its
  tmp=apply(its,c(2,3),mean)
  for(t in 1:length(times)){
    mns[t,,]=tmp
  }
  its=its-mns
  devMean=apply(its,c(1,2),mean)
  devLower=apply(its,c(1,2),quantile,.025)
  devUpper=apply(its,c(1,2),quantile,.975)
  
  par(mfrow=c(2,5))
  for(p in 1:ncol(mean)){
    plot(-1*times,mean[,p],type='n',ylim=range(c(lower,upper,mean)),ylab=ylab,xlab=xlab)
    #    points(-1*times,mean[,p],pch=16,cex=.8)
    polygon(c(rev(-1*times),-1*times),c(rev(lower[,p]),upper[,p]),col='gray80',border='gray80')
    lines(-1*times,mean[,p])
    lines(-1*times,devLower[,p]+mean(mean[,p]),col=2)
    lines(-1*times,devUpper[,p]+mean(mean[,p]),col=2)
    abline(h=0,col='gray50',lty=3)
    title(taxa[p])
  }
}

# modify to deal better with input format
vegDiagram=function(cell,times){
  # analogous to a pollen diagram - plots vegetation predictions over time
  mean=matrix(0,nr=length(times),nc=P)
  lower=upper=mean
  for(tt in 1:length(times)){
    mean[tt,]=eval(as.name(paste('rMean',times[tt],sep='')))[cell,]
    lower[tt,]=eval(as.name(paste('rLow',times[tt],sep='')))[cell,]
    upper[tt,]=eval(as.name(paste('rUp',times[tt],sep='')))[cell,]
  }
  maxvals=apply(upper,2,max)
  maxvals=signif(maxvals,digits=1)+.1
  for(p in 1:P){
    if(maxvals[p]<.2){maxvals[p]=.2}
  }
  par(mfrow=c(1,P),mai=c(0.4,0.15,0.4,0.05),mgp=c(0,0.3,0))
  p=1
  plot(mean[,p],-1*times,type='p',xlim=c(0,maxvals[p]),pch=16,cex=.3,xlab='',ylab='',tck=-.04)
  title(taxaNames[p])
  for(vv in seq(.1,.9,by=.1)){
    abline(v=vv,lty=2,col='grey')
  }
  lines(mean[,p],-1*times,pch=16,cex=0.3)
  lines(lower[,p],-1*times,col=2)
  lines(upper[,p],-1*times,col=2)
  par(mai=c(0.4,0.15,0.4,0.05))
  for(p in 2:P){
    plot(mean[,p],-1*times,type='p',xlim=c(0,maxvals[p]),yaxt='n',pch=16,cex=.3,xlab='',ylab='',tck=-.04)
    title(taxaNames[p])
    for(vv in seq(.1,.9,by=.1)){
      abline(v=vv,lty=2,col='grey')
    }
    lines(mean[,p],-1*times)
    lines(lower[,p],-1*times,col=2)
    lines(upper[,p],-1*times,col=2)
  }
}

vegPlusPollenDiagram=function(cell,times,pollenAges,pollenProp){
  # analogous to a pollen diagram - plots vegetation predictions over time with pollen overlaid; pollen might go outside figure limits...
  # pollenProp should have rows equal ages and columns be the 10 taxa with the data being proportions
  mean=matrix(0,nr=length(times),nc=P)
  lower=upper=mean
  for(tt in 1:length(times)){
    mean[tt,]=eval(as.name(paste('rMean',times[tt],sep='')))[cell,]
    lower[tt,]=eval(as.name(paste('rLow',times[tt],sep='')))[cell,]
    upper[tt,]=eval(as.name(paste('rUp',times[tt],sep='')))[cell,]
  }
  maxvals=apply(upper,2,max)
  maxvals=signif(maxvals,digits=1)+.1
  for(p in 1:P){
    if(maxvals[p]<.2){maxvals[p]=.2}
  }
  par(mfrow=c(1,P),mai=c(0.4,0.15,0.4,0.05),mgp=c(0,0.3,0))
  p=1
  plot(mean[,p],-1*times,type='p',xlim=c(0,maxvals[p]),pch=16,cex=.3,xlab='',ylab='',tck=-.04)
  title(taxaNames[p])
  for(vv in seq(.1,.9,by=.1)){
    abline(v=vv,lty=2,col='grey')
  }
  lines(mean[,p],-1*times,pch=16,cex=0.3)
  lines(lower[,p],-1*times,col=2)
  lines(upper[,p],-1*times,col=2)
  lines(pollenProp[,p],pollenAges,col='blue')
  par(mai=c(0.4,0.15,0.4,0.05))
  for(p in 2:P){
    plot(mean[,p],-1*times,type='p',xlim=c(0,maxvals[p]),yaxt='n',pch=16,cex=.3,xlab='',ylab='',tck=-.04)
    title(taxaNames[p])
    for(vv in seq(.1,.9,by=.1)){
      abline(v=vv,lty=2,col='grey')
    }
    lines(mean[,p],-1*times)
    lines(lower[,p],-1*times,col=2)
    lines(upper[,p],-1*times,col=2)
    lines(pollenProp[,p],pollenAges,col='blue')
  }
}

vegPlusPollenDiagramDecomp=function(cell,timeInd,times,pollenAges,pollenProp,maxvals=NULL){
  # analogous to a pollen diagram - plots vegetation predictions over time with pollen overlaid but now with long-term avg + deviations; pollen might go outside figure limits...
  # pollenProp should have rows equal ages and columns be the 10 taxa with the data being proportions
  
  smps=rIts[cell,,timeInd,]
  lterm=matrix(0,nr=P,nc=3)
  lterm[,1]=apply(smps,1,mean)
  lterm[,2]=apply(smps,1,quantile,c(.025))
  lterm[,3]=apply(smps,1,quantile,c(.975))
  
  mns=smps
  for(t in 1:length(times)){
    mns[,t,]=apply(smps,c(1,3),mean)
  }
  smps=smps-mns
  tv=array(0,c(P,length(times),3))
  tv[,,1]=apply(smps,c(1,2),mean)
  tv[,,2]=apply(smps,c(1,2),quantile,.025)
  tv[,,3]=apply(smps,c(1,2),quantile,.975)
  
  mean=rMean[cell,,timeInd]
  lower=rLow[cell,,timeInd]
  upper=rUp[cell,,timeInd]
  
  if(is.null(maxvals)){
    pollMax=apply(tmp,2,max)
    maxvals=apply(upper,1,max)
    maxvals=apply(cbind(pollMax,maxvals),1,max)
    maxvals=signif(maxvals,digits=1)+.1
    for(p in 1:P){
      if(maxvals[p]<.2){maxvals[p]=.2}
    }
  }
  par(mfrow=c(1,P),mai=c(0.4,0.15,0.4,0.05),mgp=c(0,0.3,0))
  p=1
  plot(mean[p,],-1*times,type='n',xlim=c(0,maxvals[p]),pch=16,cex=.3,xlab='',ylab='',tck=-.04)
  title(taxaNames[p])
  #  lines(lower[,p],-1*times,col=2)
  #  lines(upper[,p],-1*times,col=2)
  polygon(c(rev(lower[p,]),upper[p,]),c(rev(-1*times),-1*times),col='gray80',border='gray80')
  for(vv in seq(.1,.9,by=.1)){
    abline(v=vv,lty=2,col='gray50')
  }
  lines(mean[p,],-1*times,pch=16,cex=0.3)
  #  points(mean[p,],-1*times,pch=16,cex=0.3)
  lines(lterm[p,1]+tv[p,,1],-1*times,pch=16,cex=0.3)
  lines(lterm[p,1]+tv[p,,2],-1*times,col=2)
  lines(lterm[p,1]+tv[p,,3],-1*times,col=2)
  lines(pollenProp[,p],pollenAges,col='blue')
  points(pollenProp[,p],pollenAges,col='blue',pch=16,cex=.3)
  par(mai=c(0.4,0.15,0.4,0.05))
  for(p in 2:P){
    plot(mean[p,],-1*times,type='n',xlim=c(0,maxvals[p]),yaxt='n',pch=16,cex=.3,xlab='',ylab='',tck=-.04)
    title(taxaNames[p])
    polygon(c(rev(lower[p,]),upper[p,]),c(rev(-1*times),-1*times),col='gray80',border='gray80')
    for(vv in seq(.1,.9,by=.1)){
      abline(v=vv,lty=2,col='grey50')
    }
    lines(mean[p,],-1*times)
    #  points(mean[p,],-1*times,pch=16,cex=0.3)
    #lines(lower[,p],-1*times,col=2)
    #lines(upper[,p],-1*times,col=2)
    lines(lterm[p,1]+tv[p,,1],-1*times)
    lines(lterm[p,1]+tv[p,,2],-1*times,col=2)
    lines(lterm[p,1]+tv[p,,3],-1*times,col=2)
    lines(pollenProp[,p],pollenAges,col='blue')
    points(pollenProp[,p],pollenAges,col='blue',pch=16,cex=.3)
  }
  
}

surfMap=function(p,rMat,locs,max=0.4,restrict=FALSE,legend=TRUE){
  # surface map for a single taxon
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
  tmp$z=matrix(rMat[used,p],m1,m2)
  if(legend){
    image.plot(tmp$x,tmp$y,tmp$z,xaxt='n',yaxt='n',col=tim.colors(32),zlim=c(0,max),xlab="",ylab="")
  } else{
    image(tmp$x,tmp$y,tmp$z,col=tim.colors(32),xaxt='n',yaxt='n',xlab="",ylab="",zlim=c(0,max))
  }
  lines(ma$x,ma$y,lwd=2)
  lines(ct$x,ct$y,lwd=2)
  lines(ri$x,ri$y,lwd=2)
  lines(nh$x,nh$y,lwd=2)
  lines(ny$x,ny$y,lwd=2)
}



pieMap=function(proportions,centers,restrict=FALSE,xlim=c(629000-6000,809000+6000),ylim=c(4621000-6000,4801000+6000),radius=NULL,scale=1,xlab='x',ylab='y',...){
  # plots multiple pie composition charts as a map
  if(!restrict){
    used=1:nrow(centers)
  } else{ # assumes subset of grid
    used=49:(256-64)
  }
  centers=as.matrix(centers[used,])
  proportions=as.matrix(proportions[used,])
  if(is.null(xlim)){
    rg=range(centers[,1])
    df=(scale-1)*diff(range(centers[,1]))
    xlim=c(rg[1]-df,rg[2]+df)
  }
  if(is.null(ylim)){
    rg=range(centers[,2])
    df=(scale-1)*diff(range(centers[,2]))
    ylim=c(rg[1]-df,rg[2]+df)
  }
  plot(centers,type='n',xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,...)
  n=length(centers[,1])
  if(is.null(radius)){
    radius=.025*diff(range(centers[,1]))
  }
  minVal=min(proportions)
  # eps=1e-10*max(proportions)
  # if(minVal==0){
  #   warning("Some proportions are zero; proceeding with jittering.")
  #   proportions=proportions+eps
  # }
  if(minVal<0){
    stop("Some proportions are less than zero.")
  }
  if(length(radius)==1){ radius=rep(radius,n)}
  for(i in 1:n){
    if(sum(proportions[i,])>0){
      minVal=min(proportions[i,])
      if(minVal==0){
        warning("Some proportions are zero; proceeding with jittering.")
        eps=1e-10*max(proportions[i,])
        proportions[i,]=proportions[i,]+eps
      }
      pieAdd(as.vector(proportions[i,]),as.vector(centers[i,]),radius=radius[i],...)
    } else{
      points(centers[i,],pch='X')
    }
  }
  lines(ma$x,ma$y,lwd=1)
  lines(ct$x,ct$y,lwd=1)
  lines(ri$x,ri$y,lwd=1)
  lines(ny$x,ny$y,lwd=1)
  lines(nh$x,nh$y,lwd=1)
}


pieAdd= function (x, center, labels = names(x), edges = 200, radius = 0.8, density = NULL, angle = 45, col = NULL, border = NULL, lty = NULL, ...) # modified from the pie() function in R
{
  if (!is.numeric(x) || any(is.na(x) | x <= 0)) 
    stop("pie: `x' values must be positive.")
  if (is.null(labels)) 
    labels <- rep("",length(x))
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  
  pin <- par("pin")
  nx <- length(dx)
  if (is.null(col)) 
    col <- if (is.null(density)) 
      c("white", "black","lightblue", "red","darkblue","yellow",
        "purple","orange","lightgreen","darkgreen")
  else par("fg")
  col <- rep(col, length.out = nx)
  border <- rep(border, length.out = nx)
  lty <- rep(lty, length.out = nx)
  angle <- rep(angle, length.out = nx)
  density <- rep(density, length.out = nx)
  for (i in 1:nx) {
    n <- max(2, floor(edges * dx[i]))
    t2p <- 2 * pi * seq(x[i], x[i + 1], length = n)
    xc <- c(cos(t2p), 0) * radius + center[1]
    yc <- c(sin(t2p), 0) * radius + center[2]
    polygon(xc, yc, density = density[i], angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i],...)
    t2p <- 2 * pi * mean(x[i + 0:1])
    xc <- cos(t2p) * radius + center[1]
    yc <- sin(t2p) * radius + center[2]
    if (!is.na(lab <- labels[i]) && lab != "") {
      lines(c(1, 1.05) * xc, c(1, 1.05) * yc)
      text(1.1 * xc, 1.1 * yc, lab, xpd = TRUE, adj = ifelse(xc < 
                                                               0, 1, 0), ...)
    }
  }
  invisible(NULL)
}

#########
timeSignifSynch=function(p1,p2,rItsNew,rItsOld,locs,restrict=FALSE,ylab='',xlab=''){
  # compares synchrony of significance of differences in composition between two predictions in time and two taxa
  # red colors indicate both taxa show lower abundance in the newer time period; blue both higher abundance in the newer; green that the first taxa shows higher in the older and second higher in the newer, and brown that the first shows higher in the new and second higher in the older
  nComparisons=length(rItsNew[1,1,])
  
  bothLess=rItsNew[,p1,]<rItsOld[,p1,] & rItsNew[,p2,]<rItsOld[,p2,]
  bothLess=matrix(apply(bothLess,1,sum),sqrt(S),sqrt(S))
  bothMore=rItsNew[,p1,]>rItsOld[,p1,] & rItsNew[,p2,]>rItsOld[,p2,]
  bothMore=matrix(apply(bothMore,1,sum),sqrt(S),sqrt(S))
  firstLess=rItsNew[,p1,]<rItsOld[,p1,] & rItsNew[,p2,]>rItsOld[,p2,]
  firstLess=matrix(apply(firstLess,1,sum),sqrt(S),sqrt(S))
  secondLess=rItsNew[,p1,]>rItsOld[,p1,] & rItsNew[,p2,]<rItsOld[,p2,]
  secondLess=matrix(apply(secondLess,1,sum),sqrt(S),sqrt(S))
  
  #more=nComparisons-less
  
  if(!restrict){
    used=1:S
    m1=m2=sqrt(S)
    rows=1:16
  } else{
    used=49:(256-64)
    m1=16;m2=9
    rows=4:12
  }
  
  xs=sort(unique(locs[,1]))
  ys=sort(unique(locs[,2]))[rows]
  more=nComparisons-bothLess
  image(xs,ys,bothMore[,rows],zlim=c(.9*nComparisons,nComparisons),xlab=xlab,ylab=ylab,col=tim.colors(32)[21:32])
  image(xs,ys,bothLess[,rows],zlim=c(.9*nComparisons,nComparisons),xlab=xlab,ylab=ylab,col=tim.colors(32)[12:1],add=TRUE)
  
  ramp <- colorRamp(c("lightgreen", "darkgreen"))
  greens=rgb( ramp(seq(0, 1, length =12)), max = 255)
  
  ramp <- colorRamp(c("wheat", "chocolate4"))
  browns=rgb( ramp(seq(0, 1, length =12)), max = 255)
  
  image(xs,ys,firstLess[,rows],zlim=c(.9*nComparisons,nComparisons),xlab=xlab,ylab=ylab,col=greens,add=T)
  image(xs,ys,secondLess[,rows],zlim=c(.9*nComparisons,nComparisons),xlab=xlab,ylab=ylab,col=browns,add=TRUE)
  
  lines(ma$x,ma$y,lwd=1)
  lines(ct$x,ct$y,lwd=1)
  lines(ri$x,ri$y,lwd=1)
  lines(ny$x,ny$y,lwd=1)
  lines(nh$x,nh$y,lwd=1)
}
################

# template code for using above functions to analyze output

if(FALSE){
  
  # piemap of predictions for third time point (500 in 300-3000 predictions), restricting to approximate region with ponds
  pieMap(rMean[,,3],gridLocs,restrict=TRUE)
  # piemap of aggregated pollen for third time point
  pieMap(cMat[,,3],pondLocs)
  
  # surface maps for third time point
  surfMap(1,rMean[,,7],gridLocs,max=0.6,restrict=TRUE)
  
  par(mfrow=c(3,3),mar=c(2.1,2.1,3.1,1.1)) 
  for(p in 1:(P-1)){
    surfMap(p,rMean[,,3],gridLocs,max=0.6,restrict=TRUE)
    title(taxa[p])
  }
  
  # feature significance map
  
  feature(2,less[[3]],gridLocs,restrict=TRUE,nComparisons=nComparisons) # 2nd taxon, 3rd time period
  # ignore the 'graphical parameter' warning messages
  
  # maps of significant differences between two time periods (the third and fourth time periods) for 2nd taxon
  timeSignif(2,rIts[,,3,],rIts[,,4,],gridLocs,restrict=TRUE)
  
  par(mfrow=c(3,3),mar=c(2.1,2.1,3.1,1.1)) 
  for(p in 1:(P-1)){
    timeSignif(p,rIts[,,3,],rIts[,,4,],gridLocs,restrict=TRUE)
    title(taxa[p])
  }
  
  # time series plots of coefficients
  whichts=1:23 # use 300-2500 time interval
  times = seq(300,3500,length = 24)
  timePlotDecomp(times[whichts],b1sMean[whichts,],b1sLow[whichts,],b1sUp[whichts,],b1s[whichts,,])
  
  # maps of significant differences between two taxa (2nd and 7th here) for 3rd time period
  taxaSignif(3,7,rIts[,,3,],gridLocs,restrict=TRUE)
  
  
  # vegetation + pollen diagam for 300-2500 time interval for cell 139 (Snake Pond I think)
  dat=read.table(paste0("STEPPS output/",'PollenTimeSeries.csv'),sep=',',header=T)
  i=20
  subdat=dat[dat$sitenumber==i,]
  age=-subdat$'cal.age'
  tmp=subdat[,5:14]
  tmp=tmp/apply(tmp,1,sum)
  whichTimeIndices=1:23
  quartz()
  vegPlusPollenDiagramDecomp(139,whichTimeIndices,seq(300,2500,by=100),age,tmp)
  
  # map of significant sychrony between two taxa (pine and oak) between two time periods (the third and fourth time periods)
  timeSignifSynch(8,7,rIts[,,3,],rIts[,,4,],gridLocs,restrict=TRUE)
}