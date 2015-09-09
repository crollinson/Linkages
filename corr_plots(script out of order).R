silver = rIts[137,,,]
wickett = rIts[136,,,]

#10 taxa, 28 time bins, 250 iters
plot(silver[10,1,])

mean_silver = rMean[137,,]
mean_wickett = rMean[136,,]

library(corrplot)

rownames(mean_silver)<-c("beech","birch","chestnut","hemlock","hickory","maple","oak","pine","spruce","other")
rownames(mean_wickett)<-c("beech","birch","chestnut","hemlock","hickory","maple","oak","pine","spruce","other")

pdf("corr_silver.pdf")
corrplot(cor(t(mean_silver)), order = "hclust", addrect = 3)
corrplot(cor(t(mean_wickett)),order = "hclust", addrect = 3)

corrplot(cor(t(mean_silver),t(mean_wickett)),order = "FPC")

pdf("HA_scatters_climate.pdf")
par(mfrow=c(3,4))
for(i in 1:10){
  plot(temp_mean[get_temp,3],mean_wickett[i,1:14],pch = 19,
       ylim = c(0,(max(mean_wickett[i,1:14])+.05)),
       main = rownames(mean_wickett[,1:14])[i],
       ylab = "Pollen Proportion",xlab = "Average March Temperture") 
  points(temp_mean[get_temp,3],mean_silver[i,1:14],pch = 19)
}
dev.off()


points(mean_silver[4,]/mean_silver[9,],pch=19)
plot(mean_wickett[4,],mean_silver[4,])

climate = read.table("met2model_output_v4.2/PHA/climate.txt",sep=",")

temp_mean = climate[1:67,]
precip_mean = climate[135:201,]
rownames(temp_mean)<- -(seq(350,2010,25) - 1950)
rownames(precip_mean)<- -(seq(350,2010,25) - 1950)
colnames(mean_wickett) <- seq(300,3000,100)
head(mean_wickett)

get_temp = na.omit(match(colnames(mean_wickett),rownames(temp_mean)))
quartz()
  plot(temp_mean[get_temp,4],rMean[15,4,1:14],ylim=c(0,.6))
  points(temp_mean[get_temp,4],rMean[156,4,1:14])


for(i in c(154,156)){
  points(temp_mean[get_temp,7],rMean[i,1,1:14])
}

cors = matrix(0,10,12)

for(i in 1:10){
  for(m in 1:12){
    cors[i,m] = cor(temp_mean[get_temp,m],rMean[155,i,1:14])
  }
}

rownames(cors)<-c("beech","birch","chestnut","hemlock","hickory","maple","oak","pine","spruce","other")

cors1 = matrix(0,10,12)

for(i in 1:10){
  for(m in 1:12){
    cors1[i,m] = cor(temp_mean[get_temp,m],rMean[15,i,1:14])
  }
}

rownames(cors1)<-c("beech","birch","chestnut","hemlock","hickory","maple","oak","pine","spruce","other")

to_plot = cbind(cors1[,c(3,9)],cors[,c(3,9)])

colnames(to_plot) <- c("March - South","September - South","March - North","September - North")
pdf("corr_plot_NS.pdf")
corrplot(to_plot)
dev.off()

cors = matrix(0,10,12)

for(i in 1:10){
  for(m in 1:12){
    cors[i,m] = cor(precip_mean[get_temp,m],rMean[155,i,1:14])
  }
}

rownames(cors)<-c("beech","birch","chestnut","hemlock","hickory","maple","oak","pine","spruce","other")

cors1 = matrix(0,10,12)

for(i in 1:10){
  for(m in 1:12){
    cors1[i,m] = cor(precip_mean[get_temp,m],rMean[15,i,1:14])
  }
}

rownames(cors1)<-c("beech","birch","chestnut","hemlock","hickory","maple","oak","pine","spruce","other")

to_plot = cbind(cors1[,8],cors[,8])

colnames(to_plot) <- c("August - South","August - North")
pdf("corr_plot_NS_precip.pdf")
corrplot(to_plot)
dev.off()


