rm(list=ls())


#####

##### FIRST WRITE .txt files for LINKAGES

#####

#model2netcdf.LINKAGES(PFTs = pick_spp, outdir = outdir, sitelat = plat, sitelon = -plong, start_date=NULL, end_date=NULL,force=FALSE)

site = "PHA" #don't change

# met2model.LINKAGES(in.path = paste0("/Users/paleolab/Linkages/phase1a_met_drivers_v4.2/",site),
#                    in.prefix = site, 
#                    outfolder = paste0("/Users/paleolab/linkages/UNDERC/old/"), #always need last "/"
#                    start_date = "0850/01/01",
#                    end_date = "2010/12/31", 
#                    overwrite=FALSE,verbose=FALSE)

#####
##### Set up directories #####
#####

outdir = paste0("/Users/paleolab/Linkages/Harvard Forest/")
spp_list_site = read.csv("/Users/paleolab/Linkages/Harvard Forest/spp_list_site.csv",stringsAsFactors=FALSE)
texture =  read.csv("/Users/paleolab//Linkages/Harvard Forest/texture.csv")
env_drivers = read.csv("/Users/paleolab/Linkages/Harvard Forest/PalEON_Phase1a_sites.csv",stringsAsFactors=FALSE)

pick_site1 = which(colnames(spp_list_site)==site)

#####
##### Set up printing intervals DONT CHANGE #####
#####

kprnt = 2 #year interval for output
klast = 90 #number of plots
nyear = 1661 #number of years to simulate 500 spin up + 1160 met data
ipolat_nums = seq(2,nyear,25) #years for climate interpolation
ipolat = length(ipolat_nums)-1 #number of years for climate interpolation

###### Read climate data created from MIP drivers #####
climate_dat = read.csv(paste0("/Users/paleolab/Linkages/Harvard Forest/climate.txt"),stringsAsFactors=FALSE)

#####
##### Set initial conditions #####
#####

bgs = 127 #DOY to begin growing season
egs = 275 #DOY to end growing season

pick_site = which(colnames(env_drivers)==site)
plat = as.numeric(env_drivers[3,pick_site]) #latitude
plong = abs(as.numeric(env_drivers[2,pick_site])) #longitude

sand = as.numeric(env_drivers[24,pick_site])/100
clay = as.numeric(env_drivers[19,pick_site])/100

soil.texture <- function(sand,clay){
  silt = 1 - sand - clay
  
  sand.keep = which(texture$xsand < sand + .1 & texture$xsand > sand - .1) 
  clay.keep = which(texture$xclay[sand.keep] < clay + .1 & texture$xclay[sand.keep] > clay - .1)
  silt.keep = which(texture$xsilt[sand.keep[clay.keep]] < silt + .1 & texture$xsilt[sand.keep[clay.keep]] > silt - .1)
  
  row.keep = sand.keep[clay.keep[silt.keep]]
  
  return(texture[round(mean(row.keep)),c(8,14)]*100) # might need to divide by 3 or something because linkages wants cm water/30cm soil...
}
fc = round(as.numeric(unlist(soil.texture(sand = sand, clay = clay)[2])),digits = 2)
dry = round(as.numeric(unlist(soil.texture(sand = sand, clay = clay)[1])),digits = 2)

#####
##### Write initial condition file #####
#####

sink(paste0(outdir,"/settings.txt"))
cat(kprnt,klast,nyear,sep=",")
cat("\n")
cat(ipolat)
cat("\n")
cat(ipolat_nums,sep=",")
cat("\n")
cat(plat,plong,bgs,egs,fc,dry,sep=",")
sink()

#####
##### Write species data table #####
#####

nspec = 9 
bmspec = nspec
all_spp_params = read.csv("/Users/paleolab/Linkages/Harvard Forest/spp_matrix.csv")
pick_spp = c(1:9)
spp_params = all_spp_params[which(all_spp_params$Spp_Number%in%pick_spp),3:ncol(all_spp_params)]
spec_nums = all_spp_params[which(all_spp_params$Spp_Number%in%pick_spp),2]

sink(paste0(outdir,"/spp.txt"))
cat(nspec,bmspec,sep=",")
cat("\n")
cat(spec_nums)
cat("\n")
write.table(spp_params,sep=",",col.names=FALSE,row.names=FALSE)
sink()

#####
##### Write switch text file #####
#####

switch_chars_list = read.csv("/Users/paleolab/Linkages/Harvard Forest/switch.csv")
switch_chars = as.character(switch_chars_list[spec_nums,3])
sink(paste0(outdir,"/switch.txt"))
cat(switch_chars,sep="\n")
sink()

#####
##### Write underground parameters file #####
#####

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

sink(paste0(outdir,"/dirt.txt"))
cat(NLVAR,NLT,NCOHRT,sep=" ")
cat("\n")
write.table(dirt_params,sep=",",col.names=FALSE,row.names=FALSE)
cat("\n")
cat(basesc,basesn,sep=" ")
sink()





################ use terminal to compile linkages.f

link = as.matrix(read.csv(paste0(outdir,"/OUT.csv"),head=FALSE))

head(link)

#if(nrow(link) > 1) print("keep going!")

#tree_choices = load(file="tree_choices.RData")

#tree_names = c("Acer","Betula","Carya","Castanea dentata","Fagus grandifolia",
#                "Picea","Pinus","Tsuga canadensis","Quercus") #tree_choices[link[50,2:11],1]


test_biomass=as.matrix(link[1665:(1665+(1661/2)),1:10])
colnames(test_biomass) = c("Year",tree_names)
biomass_cis = link[67:87,1:10]
x=seq(0,1660,2)
quartz()
par(mfrow=c(1,2))
plot(x,test_biomass[,2],type="l",lwd=4,main=NA,xlab="Years",ylab="Average Biomass",ylim=c(0,max(test_biomass[,2:4])))

lines(x,test_biomass[,3],col="red",lwd=4)
lines(x,test_biomass[,4],col="yellow",lwd=4)
lines(x,test_biomass[,5],col="blue",lwd=4)
lines(x,test_biomass[,6],col="green",lwd=4)
lines(x,test_biomass[,7],col="purple",lwd=4)
lines(x,test_biomass[,8],col="gray",lwd=4)
lines(x,test_biomass[,9],col="orange",lwd=4)
lines(x,test_biomass[,10],col="lightblue",lwd=4)
plot.new()
legend("center",as.character(all_spp_params[which(all_spp_params$Spp_Name%in%pick_spp),1]),lwd=rep(4,9),lty=rep(1,9),
       col=c("black","red","yellow","blue","green","purple","gray","orange","lightblue","pink"),xpd=TRUE)

library(lattice)
library(stats)

test_other=link[1:((1660/2)+1),]
colnames(test_other) = c("year","num stems","ag biomass","leaf litter","leaf litter N","ag npp","avail n","humus C:N","soil co2-c","soil OM","aet")
params_cis = link[1107:(1107+1105),]
#biomass_cis = link[75:98,]

quartz()
par(mfrow=c(3,3))
for(i in 2:10){
  plot(x,test_other[,i],typ="l",ylim=c(min(test_other[,i]),max(test_other[,i])),main=colnames(test_other)[i],ylab=NA,xlab="Year")
  #lines(x,test_other[,i]-params_cis[,i],lty=3,col="blue")
  #lines(x,test_other[,i]+params_cis[,i],lty=3,col="blue")
}

par(mfrow=c(3,4))
for(i in 2:10){
plot(test_biomass[,i],typ="l",ylim=c(min(test_biomass[,i]-biomass_cis[,i]),max(test_biomass[,i]+biomass_cis[,i])),main=colnames(test_biomass)[i],ylab="Biomass")
lines(test_biomass[,i]-biomass_cis[,i],lty=3,col="blue")
lines(test_biomass[,i]+biomass_cis[,i],lty=3,col="blue")
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