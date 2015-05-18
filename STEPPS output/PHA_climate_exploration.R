
climate_dat = read.csv(paste0("STEPPS output/","climate.txt")) #MIP climate data used in LINKAGES runs
time_bp = seq(-1600,60,25) #time interval before present #we can have higher resolution if we want
temp = cbind(time_bp[2:length(time_bp)],climate_dat[1:66,]) #mean temperature matrix
precip = cbind(time_bp[2:length(time_bp)],climate_dat[135:200,]) #mean precipitation matrix
boxplot(temp[,2:13]) #xaxis is month
boxplot(precip[,2:13])

par(mfrow=c(3,4))
for(i in 2:13){
  plot(temp[,1],temp[,i],xlab = "time before present", 
       ylab = paste0("Average ",i," Temperature (C)"),ylim = c(min(temp[,i])-10,max(temp[,i])+10),pch = 19, cex = 1)
  temp = as.data.frame(temp)
  mod = lm(temp[,i]~temp[,1])
  abline(mod,lwd = 2,lty = 2)
  text(-250,max(temp[,i])+5,paste("slope = ",round(mod$coefficients[2],digits = 5),sep = " "))
}

par(mfrow=c(3,4))
for(i in 2:13){
  plot(precip[,1],precip[,i],xlab = "time before present", 
       ylab = paste0("Average ",i," precipitation (C)"),ylim = c(min(precip[,i])-10,max(precip[,i])+10),pch = 19, cex = 1)
  precip = as.data.frame(precip)
  mod = lm(precip[,i]~precip[,1])
  abline(mod,lwd = 2,lty = 2)
  text(-250,max(precip[,i])+5,paste("slope = ",round(mod$coefficients[2],digits = 5),sep = " "))
}



