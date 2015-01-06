library(ncdf4)

setwd("/Users/paleolab/Downloads/phase1a_met_drivers_v2/PBL/precipf")

year = seq(850,2010,1)
month = seq(1,12,1)

character_hold = matrix(0,length(month),length(year))
for(y in 1:150){
  for(m in 1:9){
   character_hold[m,y] <- paste("PBL_precipf_0",year[y],"_0",month[m],sep="")
  }
  for(m in 10:12){
   character_hold[m,y] <- paste("PBL_precipf_0",year[y],"_",month[m],sep="")
  }
}

for(y in 151:length(year)){
  for(m in 1:9){
    character_hold[m,y] <- paste("PBL_precipf_",year[y],"_0",month[m],sep="")
  }
  for(m in 10:12){
    character_hold[m,y] <- paste("PBL_precipf_",year[y],"_",month[m],sep="")
  }
}

character_hold_new <- matrix(character_hold,length(year)*length(month),1,byrow=FALSE)

#ncprecipf = matrix(0,112,length(character_hold_new)) can't store because they are different lengths...
summary_ncprecipf = matrix(0,length(character_hold_new),2)

for(i in 1:length(character_hold_new)){
  ncin <- nc_open(paste(character_hold_new[i],".nc",sep=""))
  #print(ncin)
  ncprecipf = ncvar_get(ncin, "precipf") #units are kg m-2 s-1 
  summary_ncprecipf[i,1] <- mean(ncprecipf)
  summary_ncprecipf[i,2] <- sd(ncprecipf)
  nc_close(ncin)
  if(i%%1200==0) cat(i," "); flush.console()
}

#save(summary_ncprecipf,file="summary_ncprecipf.Rdata")
tail(summary_ncprecipf)
plot(summary_ncprecipf[,1])

start = 6.872218e-05 # kg m-2 s-1
sec = 2.62974e6 #seconds in a month
start/10000 #kg cm-2 s-1
(start/10000)*sec #kg cm-2 month-1

###### TEMPERATURE ######

setwd("/Users/paleolab/Downloads/phase1a_met_drivers_v2/PBL/tair")

character_hold1 = matrix(0,length(month),length(year))
for(y in 1:150){
  for(m in 1:9){
    character_hold1[m,y] <- paste("PBL_tair_0",year[y],"_0",month[m],sep="")
  }
  for(m in 10:12){
    character_hold1[m,y] <- paste("PBL_tair_0",year[y],"_",month[m],sep="")
  }
}

for(y in 151:length(year)){
  for(m in 1:9){
    character_hold1[m,y] <- paste("PBL_tair_",year[y],"_0",month[m],sep="")
  }
  for(m in 10:12){
    character_hold1[m,y] <- paste("PBL_tair_",year[y],"_",month[m],sep="")
  }
}

character_hold_new1 <- matrix(character_hold1,length(year)*length(month),1,byrow=FALSE)

summary_nctair = matrix(0,length(character_hold_new1),2)

for(i in 1:length(character_hold_new1)){
  ncin <- nc_open(paste(character_hold_new1[i],".nc",sep=""))
  #print(ncin)
  nctair = ncvar_get(ncin, "tair") #units are kg m-2 s-1 
  summary_nctair[i,1] <- mean(nctair)
  summary_nctair[i,2] <- sd(nctair)
  nc_close(ncin)
  if(i%%1200==0) cat(i," "); flush.console()
}


tail(summary_nctair)
plot(summary_nctair[,1]-273) #C = K - 273.15 #temperature conversion from K to c.
#save(summary_nctair,file="summary_nctair.Rdata")


