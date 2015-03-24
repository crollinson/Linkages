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

sink("settings.txt")
cat(kprnt,klast,nyear,sep=",")
cat("\n")
cat(ipolat)
cat("\n")
cat(ipolat_nums,sep=",")
cat("\n")
cat(plat,plong,bgs,egs,fc,dry,sep=",")
sink()

write.table(file="climate.txt",rbind(temp_means,temp_sd,precip_means,precip_sd),sep=",",col.names=FALSE,row.names=FALSE)
#file.show("test_text1.txt")

nspec = 9 
bmspec = 9
spec_nums = seq(1,bmspec)
DMAX = c(4700,2500,5993,4571,5537,1911,3165,3800,4571) #- MAXIMUM GROWING DEGREE DAYS
DMIN = c(1600,1100,1910,1910,1326,280,1100,1324,1100) #- MINIMUM GROWING DEGREE DAYS
B3 = c(.1988,.5013,.2663,.1495,.2863,.7105,.1495,.1495,.2863) # - INDIVIDUAL SPECIES CONSTANT USED IN GROW
B2 = c(47.72,76.40,53.26,44.84,57.26,90.96,44.84,44.84,57.26) # - INDIVIDUAL SPECIES CONSTANT USED IN GROW
ITOL = c(2,1,1,1,1,1,2,1,1) # - LIGHT TOLERANCE CLASS
AGEMX = c(125,250,300,300,366,200,450,650,400) # - MAXIMUM AGE OF SPECIES
G = c(212,106,82,102,72,132,68,47,66) # - SCALES THE GROWTH RATE OF EACH SPECIES
SPRTND = c(31,43,631,63,172,3,37,0,32) #- TENDENCY TO STUMP SPROUT
SPRTMN = c(6,12,12,12,6,0,0,0,12) # - MINIMUM SIZE TREE THAT WILL SPROUT
SPRTMX = c(50,100,200,200,30,0,0,0,40) # - MAXIMUM SIZE TREE THAT WILL SPROUT
MPLANT = c(20,120,20,20,40,16,140,8,40) # - MAXIMUM NUMBER OF SEEDLINGS TO PLANT
D3 = c(.268,.2,.3,.3,.2,.309,.31,.18,.225) # - PROPORTION OF GROWING SEASON SPECIES CAN WITHSTAND DROUGHT
FROST = c(-12,-18,-4,-2,-12,-30,-20,-12,-12) # - MINIMUM JANUARY TEMPERATURE SPECIES CAN WITHSTAND
TL = c(2,4,4,2,8,11,12,6,9) # - LEAF LITTER TYPE
CM1 = c(2.79,2.94,2.94,2.99,2.94,2.79,2.79,2.79,2.94) #THROUGH CM5 - PARAMETERS TO CALCULATE NITROGEN GROWTH FACTORS
CM2 = c(219.77,117.52,117.52,207.43,117.52,219.77,291.77,219.77,117.52)
CM3 = c(.00179,.00234,.00234,.00175,.00234,.00179,.00179,.00179,.00234)
CM4 = c(-0.6,-1.2,-1.2,-5.0,-1.2,-0.6,-0.6,-0.6,-1.2)
CM5 = c(1.0,1.3,1.3,2.9,1.3,1.0,1.0,1.0,1.3)
FWT = c(440,248,248,440,440,440,440,440,440) #- LEAF WEIGHT/UNIT CROWN AREA
SLTA = c(.814,.804,.804,.814,.904,.804,.804,.804,.904) #- PARAMETERS TO CALCULATE CROWN AREA
SLTB = c(.078,.069,.069,.078,.095,.069,.069,.069,.095)
RTST = c(1.0,.8,.8,1,1,1,1,1,1) # - ROOT/SHOOT RATIO
FRT = c(1,1,1,1,1,3,2,3,1) #- FOLIAGE RETENTION TIME IN YEARS

spp_params = cbind(DMAX,DMIN,B3,B2,ITOL,AGEMX,G,SPRTND,SPRTMN,SPRTMX,MPLANT,D3,FROST,TL,CM1,CM2,CM3,CM4,CM5,FWT,SLTA,SLTB,RTST,FRT)

sink("spp.txt")
cat(nspec,bmspec,sep=",")
cat("\n")
cat(spec_nums)
cat("\n")
write.table(spp_params,sep=",",col.names=FALSE,row.names=FALSE)
sink()

switch_chars = c("FFTFF","FFTTF","TFTFF","TFFFF","FFTFF","FFFFF","TTTFF","FFTTF","TFFFF")
sink("switch.txt")
cat(switch_chars,sep="\n")
sink()

NLVAR = 10
NLT = 17
NCOHRT = 1

init_litter_wt = c(rep(0,17)) #The weight of an incoming cohort of litter (initialized to zero)
init_perc_N = c(.0068,.0076,.0109,.0106,.0079,.0081,.0085,.0057,
                .0090,.0056,.0063,.0046,.0096,.0038,.0038,.0038,.0050) #Initial percent of nitrogen
g_N_per_g_wt_loss = c(.0251,.0315,.0574,.0377,.0256,.0286,.0336,
                      .0477,.0341,.0326,.0220,.0163,.0284,.0195,
                      .0195,.0195,.0364) #Grams of nitrogen immobilized per gram weight loss;
crit_perc_N = c(.0183,.0239,.0465,.0271,.0177,.0205,.0251,
                .0420,.0251,.0270,.0157,.0117,.0188,.0157,
                .0157,.0157,.0314) #Critical percent of nitrogen
litter_type = seq(1,17,1) #Litter type: 1 through 12 are the 12 leaf-litter types in order of decreasing decay rate and increasing nitrogen-immobilization rate and correspond to species parameter TL. Thirteen is root litter. Fourteen and fifteen are fresh wood from trees less than or greater than 10 cm dbh, respectively. Sixteen is twig litter. Seventeen is well-decayed wood not yet humus;
dest = c(1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1) #Destination when cohort reaches critical percent to nitrogen (1 = humus; 2 = well-decayed wood);
init_perc_lignin = c(.039,.121,.193,.158,.187,.206,.214,
                     .241,.248,.280,.216,.283,.253,.173,
                     .173,.173,.423) #Initial percent of lignin;
lignin_decay_param1 = c(.5217,.5219,.7787,.6693,.5194,.6839,.7059,
                        1.1967,.6105,.5926,.9052,.5646,.7000,.4831,
                        .4831,.4831,.7222) #Lignin decay parameters [see Eq. B-8, Appendix 2, in Pastor and Post (1985)]
lignin_decay_param2 = c(.336,.400,.508,.435,.315,.475,.460,.790,.359,
                        .383,.594,.327,.456,.299,.299,.299,.299)
ash_corr_factor = c(.90,.90,.92,.92,.93,.96,.94,.91,.95,.97,.97,.96,.98,.99,.99,.96,.99)

dirt_params = cbind(init_litter_wt,init_perc_N,g_N_per_g_wt_loss,crit_perc_N,
                    litter_type,dest,init_perc_lignin,lignin_decay_param1,lignin_decay_param2,
                    ash_corr_factor)
basesc = 74. #starting humus weight
basesn = 1.640 #starting N content

sink("dirt.txt")
cat(NLVAR,NLT,NCOHRT,sep=" ")
cat("\n")
write.table(dirt_params,sep=",",col.names=FALSE,row.names=FALSE)
cat("\n")
cat(basesc,basesn,sep=" ")
sink()

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