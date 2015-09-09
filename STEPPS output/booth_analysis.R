libary(mgcv)
load("Linkages/STEPPS Output/LOAD WORKSPACE Sun night Boston Dec 7.RData")
sid_wtd_dat = read.csv("~/Downloads/Sid_WTD.csv")

sid_wtd = as.data.frame(cbind(sid_wtd_dat$age_best,sid_wtd_dat$WTD))
colnames(sid_wtd) <- c("age","wtd")

age_wtd_mod = gam(wtd ~ s(age,k=10), data = sid_wtd)
pred_age_wtd = predict(age_wtd_mod,newdata=data.frame(age=seq(100,2800,100)),type="response")
plot(seq(100,2800,100),pred_age_wtd,ylim=c(0,40),typ="l",lwd=4, xlab = "Age Before Present",ylab="WTD")
points(sid_wtd[,1],sid_wtd[,2],pch=19,cex=.5,col="blue")

plot(rMean[137,4,],ylab="Percent Hemlock",xlab="Time Before Present x100",pch=19)
plot(pred_age_wtd,rMean[137,4,],pch=19,xlab="WTD",ylab="Percent Hemlock")
plot(pred_age_wtd[2:28] - pred_age_wtd[1:27],ylab="Change in WTD between 100 years",xlab ="Time")

plot(pred_age_wtd[2:28] - pred_age_wtd[1:27],
     rMean[137,4,2:28] - rMean[137,4,1:27], pch=19,
     ylab="Change in Percent Hemlock", xlab = "Change in WTD",
     xlim=c(-4,4),ylim=c(-.04,.04))

wtd_change = pred_age_wtd[2:28] - pred_age_wtd[1:27]
hem_change = rMean[137,4,2:28] - rMean[137,4,1:27]

pdf("/Users/paleolab/Linkages/sens_try1.pdf")
plot(gam(hem_change ~ s(wtd_change,k=8)),
     ylab="Change in Percent Hemlock", xlab = "Change in WTD")
points(pred_age_wtd[2:28] - pred_age_wtd[1:27],
       rMean[137,4,2:28] - rMean[137,4,1:27],pch=19)
dev.off()

aino_1pca = rPCA[170,1,]
wickett_1pca = rPCA[136,1,]

index = seq(100,2800,100)
roundput = matrix(0,28,1)
for(i in 1:28){
  roundput[i,] = mean(sid_wtd[which(sid_wtd[,1] > (index[i]-100) & sid_wtd[,1] < index[i]),2])
}

plot(roundput,aino_1pca,ylim=c(-2,.5))
plot(roundput,wickett_1pca,ylim=c(-2,.5))

gam_aino = gam(aino_1pca ~ s(roundput,k=4))
gam_wickett = gam(wickett_1pca ~ s(roundput,k=4))

pdf("/Users/paleolab/Linkages/STEPPS Output/PCA_v_WTD_100yr.pdf")
par(mfrow=c(2,1))
plot(gam_wickett,xlab = "WTD", ylab = "PCA", main = "Wickett", ylim = c(-.5,1.5))
plot(gam_aino,xlab = "WTD", ylab = "PCA", main = "Aino", ylim = c(-.5,1.5))
dev.off()

PHA_output = read.csv("/Users/paleolab/Linkages/met2model_output_v4.2/PHA/OUT.csv")
PHA_output_trees = PHA_output[1664:2494,1:10]
PHA_output_trees = PHA_output_trees[251:nrow(PHA_output_trees),]
colnames(PHA_output_trees) = c("year","maple","birch","hickory","chestnut",
                               "beech","spruce","pine","hemlock","oak")
PHA_output_trees[,1] = 1950 - seq(850,2010,2)

roundput1 = matrix(0,28,9)
for(i in 1:28){
  roundput1[i,] = colMeans(PHA_output_trees[which(PHA_output_trees[,1] > (index[i]-100) & PHA_output_trees[,1] < index[i]),2:10])
}

roundput1 = cbind(index,roundput1)

colnames(roundput1) = c("year","maple","birch","hickory","chestnut","beech",
                        "spruce","pine","hemlock","oak")
rownames(roundput1[1:11,1:10]) <- seq(1,11,1)
mod_pca = princomp(as.data.frame(roundput1[1:11,2:10]))
first_pca_mod = mod_pca$scores[,1]

gam_mod = gam(first_pca_mod ~ s(roundput[1:11],k=4))

pdf("/Users/paleolab/Linkages/STEPPS Output/mod_PCA_v_WTD_100yr.pdf")
plot(gam_mod,xlab = "WTD", ylab = "PCA of modeled composition")
dev.off()

rownames(PHA_output_trees) <- PHA_output_trees[,1]
mod_pca_big = princomp(as.data.frame(PHA_output_trees[,2:10]))
first_pca_mod_big = mod_pca_big$scores[,1]
cbind(PHA_output_trees[,1],first_pca_mod_big)

sid_wtd_round = sid_wtd[which(sid_wtd[,1]<1100 & sid_wtd[,1]>-60),]
sid_wtd_round[,1] = round(sid_wtd_round[,1])
mod_spec_yrs = first_pca_mod_big[which(PHA_output_trees[,1]%in%sid_wtd_round[,1])]
sid_wtd_round1 = sid_wtd_round[which(sid_wtd_round[,1]%in%PHA_output_trees[,1]),2]

yy = as.numeric(rev(mod_spec_yrs))
xx = as.numeric(sid_wtd_round1)

pdf("/Users/paleolab/Linkages/STEPPS Output/mod_PCA_v_WTD_smallerTS.pdf")
gam_mod_annual = gam(yy ~ s(xx))
plot(gam_mod_annual,xlab="WTD Semi Annual TS",ylab = "PCA of modeled comp")
dev.off()

plot(xx,yy)

climate_dat = read.csv(paste0("STEPPS output/","climate.txt")) #MIP climate data used in LINKAGES runs
time_bp = seq(-1600,60,25) #time interval before present #we can have higher resolution if we want
temp = cbind(time_bp[2:length(time_bp)],climate_dat[1:66,]) #mean temperature matrix
precip = cbind(time_bp[2:length(time_bp)],climate_dat[135:200,]) #mean precipitation matrix

setwd("/Users/paleolab/Downloads/phase1a_met_drivers_v2/PHA/precipf")

year = seq(850,2010,1)
month = seq(1,12,1)

character_hold = matrix(0,length(month),length(year))
for(y in 1:150){
  for(m in 1:9){
    character_hold[m,y] <- paste("PHA_precipf_0",year[y],"_0",month[m],sep="")
  }
  for(m in 10:12){
    character_hold[m,y] <- paste("PHA_precipf_0",year[y],"_",month[m],sep="")
  }
}

for(y in 151:length(year)){
  for(m in 1:9){
    character_hold[m,y] <- paste("PHA_precipf_",year[y],"_0",month[m],sep="")
  }
  for(m in 10:12){
    character_hold[m,y] <- paste("PHA_precipf_",year[y],"_",month[m],sep="")
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

pdf("/Users/paleolab/Linkages/STEPPS Output/mod_PCA_v_modPrecip_smallerTS.pdf")
par(mfrow=c(2,1))
gam1=gam(first_pca_mod~s(summary_ncprecipf[seq(1,13932,length=11),1],k=4))
plot(gam1,ylim=c(-60,60),xlab = "Modeled Precip (climate scale)", ylab = "PCA")

gam2=gam(first_pca_mod_big~s(summary_ncprecipf[seq(1,13932,24),1],k=4))
plot(gam2,ylim=c(-60,60),xlab = "Modeled Precip (annual scale)", ylab = "PCA")
dev.off()

precip_hf = matrix(summary_ncprecipf,1161,12)
summary_ncprecipf[1:12]
precip_hf[1,]

hf_climate = read.table("/Users/paleolab/Linkages/met2model_output_v4.2/PHA/climate.txt",sep=",")
hf_temp = hf_climate[1:67,]

hf_temp1 = hf_temp[19:nrow(hf_temp),]

hf_temp2 = n.colmeans(hf_temp1, 4)
rownames(hf_temp2) <- seq(800,2000,100)

n.colmeans = function(df, n = 50){
  aggregate(x = df,
            by = list(gl(ceiling(nrow(df)/n), n)[1:nrow(df)]),
            FUN = mean)
}

hf_precip = hf_climate[135:201,]
rownames(hf_precip)<-seq(350,2010,25)
colnames(hf_precip)<-c("Jan","Feb","March","April","May","June","July","August","September","October","November","December")

hf_precip1 = hf_precip[19:nrow(hf_precip),]
hf_precip2 = matrix(0,13,12)

hf_precip2[1,] <- colMeans(hf_precip1[1:4,])
hf_precip2[2,] <- colMeans(hf_precip1[5:8,])
hf_precip2[3,] <- colMeans(hf_precip1[9:12,])
hf_precip2[4,] <- colMeans(hf_precip1[13:16,])
hf_precip2[5,] <- colMeans(hf_precip1[17:20,])
hf_precip2[6,] <- colMeans(hf_precip1[21:24,])
hf_precip2[7,] <- colMeans(hf_precip1[25:28,])
hf_precip2[8,] <- colMeans(hf_precip1[29:32,])
hf_precip2[9,] <- colMeans(hf_precip1[33:36,])
hf_precip2[10,] <- colMeans(hf_precip1[37:40,])
hf_precip2[11,] <- colMeans(hf_precip1[41:44,])
hf_precip2[12,] <- colMeans(hf_precip1[45:48,])
hf_precip2[13,] <- as.numeric(hf_precip1[49,])

rownames(hf_precip2) <- seq(800,2000,100)
colnames(hf_precip2)<-c("Jan","Feb","March","April","May","June","July","August","September","October","November","December")

hf_out = read.csv("/Users/paleolab/Linkages/met2model_output_v4.2/PHA/out.csv",sep=",")

hf_biomass = hf_out[1664:2494,1:10]
colnames(hf_biomass)<-c("Year","Maple","Birch","Hickory","Chestnut","Beech","Spruce","Pine","Hemlock","Oak")
rownames(hf_biomass)<-seq(350,2010,2)
hf_biomass1 = hf_biomass[226:nrow(hf_biomass),]

hf_biomass2 = n.colmeans(hf_biomass1, 50)[,3:11]
rownames(hf_biomass2) <- seq(800,2000,100)

save(hf_precip2,file="/Users/paleolab/Linkages/Harvard Forest/hf_precip_100yrs.Rdata")
save(hf_biomass2,file="/Users/paleolab/Linkages/Harvard Forest/hf_biomass_100yrs.Rdata")
save(hf_temp2,file="/Users/paleolab/Linkages/Harvard Forest/hf_temp_100yrs.Rdata")



