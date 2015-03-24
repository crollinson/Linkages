rm(list=ls())

setwd("/Users/paleolab/Documents/linkagesdocs/data/linkages_v1-0")
#setwd("/Users/paleolab/Downloads/raiho")

kprnt = 50 #year interval for output
klast = 90 #number of plots
nyear = 1150 #number of years to simulate
ipolat_nums = seq(0,nyear,500) #years for climate interpolation
ipolat = length(ipolat_nums)-1 #number of years for climate interpolation
plat = 42.6 #latitude
plong = 72.4 #longitude
bgs = 127 #julian day to begin growing season
egs = 275 #julian day to end growing season
fc = 27 #field capacity
dry = 17 #wilting point
temp_vec = c(c(-6.3,-4.7,-0.3,6.6,12.7,17.7,20.5,19.5,14.7,8.0,2.9,-3.0),c(-6.3,-4.7,-0.3,6.6,12.7,17.7,20.5,19.5,14.7,8.0,2.9,-3.0),c(-6.3,-4.7,-0.3,6.6,12.7,17.7,20.5,19.5,14.7,8.0,2.9,-3.0)) 
temp_means = matrix(round(rnorm(12*ipolat,temp_vec,0),1),ipolat,12,byrow=TRUE)# monthly mean temperature
temp_sd = matrix(1,ipolat,12) #monthly temperature standard deviation
precip_vec = c(c( 8.5,7.9,9.9,9.8,9.7,11.1,11.7,9.4,9.4,11.5,10.7,9.9),c( 8.5,7.9,9.9,9.8,9.7,11.1,11.7,9.4,9.4,11.5,10.7,9.9),c( 8.5,7.9,9.9,9.8,9.7,11.1,11.7,9.4,9.4,11.5,10.7,9.9))#
precip_means = matrix(round(rnorm(12*ipolat,precip_vec,0),1),ipolat,12,byrow=TRUE) #monthly mean precipitation
precip_sd = temp_sd #monthly standard deviation precipitation

sink("test_text.txt")
cat(kprnt,klast,nyear,sep=",")
cat("\n")
cat(ipolat)
cat("\n")
cat(ipolat_nums,sep=",")
cat("\n")
cat(plat,plong,bgs,egs,fc,dry,sep=",")
sink()

write.table(file="test_text1.txt",rbind(temp_means,temp_sd,precip_means,precip_sd),sep=",",col.names=FALSE,row.names=FALSE)
#file.show("test_text1.txt")


################ use terminal to compile linkages.f

link = as.matrix(read.csv("/Users/paleolab/Linkages/OUT.csv",head=FALSE))

hist(link)

if(nrow(link) > 1) print("keep going!")

tree_choices = load(file="tree_choices.RData")

tree_names = c("Acer","Betula","Carya","Castanea dentata","Fagus grandifolia",
                "Picea","Pinus","Tsuga canadensis","Quercus") #tree_choices[link[50,2:11],1]

par(mfrow=c(1,2))
test_biomass=link[51:74,1:10]
colnames(test_biomass) = c("Year",tree_names)
biomass_cis = link[67:87,1:10]
x=seq(0,1150,50)
plot(x,test_biomass[,2],type="l",lwd=4,main=NA,xlab="Years",ylab="Average Biomass",ylim=c(0,max(test_biomass[,2:10])))

lines(x,test_biomass[,3],col="red",lwd=4)
lines(x,test_biomass[,4],col="yellow",lwd=4)
lines(x,test_biomass[,5],col="blue",lwd=4)
lines(x,test_biomass[,6],col="green",lwd=4)
lines(x,test_biomass[,7],col="purple",lwd=4)
lines(x,test_biomass[,8],col="gray",lwd=4)
lines(x,test_biomass[,9],col="orange",lwd=4)
lines(x,test_biomass[,10],col="lightblue",lwd=4)
plot.new()
legend("center",c(colnames(test_biomass[,2:10])),lwd=rep(4,9),lty=rep(1,9),col=c("black","red","yellow","blue","green","purple","gray","orange","lightblue","pink"),xpd=TRUE)

library(lattice)
library(stats)

test_other=link[1:24,]
colnames(test_other) = c("year","num stems","ag biomass","leaf litter","leaf litter N","ag npp","avail n","humus C:N","soil co2-c","soil OM","aet")
params_cis = link[25:48,]
biomass_cis = link[66:86,]

par(mfrow=c(3,4))
for(i in 2:11){
plot(test_biomass[,i],typ="l",ylim=c(min(test_biomass[,i]-biomass_cis[,i]),max(test_biomass[,i]+biomass_cis[,i])),main=colnames(test_biomass)[i],ylab="Biomass")
lines(test_biomass[,i]-biomass_cis[,i],lty=3,col="blue")
lines(test_biomass[,i]+biomass_cis[,i],lty=3,col="blue")
}

par(mfrow=c(3,3))
for(i in 2:10){
plot(x,test_other[,i],typ="l",ylim=c(min(test_other[,i]-params_cis[,i]),max(test_other[,i]+params_cis[,i])),main=colnames(test_other)[i],ylab=NA,xlab="Year")
lines(x,test_other[,i]-params_cis[,i],lty=3,col="blue")
lines(x,test_other[,i]+params_cis[,i],lty=3,col="blue")
}

#pairs(test_other)

#common
#*Acer rubrum 3
#*Acer saccharum 4
#*Quercus rubra 44
#*Pinus strobus 33
#Quercus alba 37
#Quercus velutina 47
#Quercus coccinea 38
#*Tsuga canadensis 50
#*Fagus grandifolia 16
#*Betula alleghaniensis 69
#*Betula lenta 6
#Betula papyrifera 55

3,4,44,33,47,38,50,16,69,13


#Rare, but relevant in STEPPS1
#Castanea dentata 13
#Picea rubens 29
#Carya ovata 10
#Carya glabra 8

#Other (in STEPPS)
#Fraxinus americana 17
#Prunus serotina 68
#Ostrya virginiana 67
#Populus tremuloides 60