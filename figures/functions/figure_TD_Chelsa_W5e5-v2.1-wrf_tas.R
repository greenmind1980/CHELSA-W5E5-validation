library("raster")
library("rgdal")
library("dplyr")
library("plotrix")
library(data.table)
library("RColorBrewer")


#Read in file 
tas_wrf_init <- fread("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/tas_all_10_2000_09_2013.txt")
colnames(tas_wrf_init)<-c('id','ghcn','ghcn_id','wrf','chelsa_w5e5','chelsa_V2.1','long','lat','year','month','day')
tas_wrf <- as.data.frame(tas_wrf_init)
tas_wrf$ghcn <- tas_wrf$ghcn/10
tas_wrf$wrf <- (tas_wrf$wrf)-273.15
tas_wrf$chelsa_w5e5 <- tas_wrf$chelsa_w5e5/10-273.15
tas_wrf$chelsa_V2.1 <- tas_wrf$chelsa_V2.1/10-273.15
tas_wrf_mon <- aggregate(.~month+long+lat, tas_wrf[c(2,4:10)], mean)
write.table(tas_wrf_mon,"/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/climatologies_tas.txt")

tas_wrf_mon <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/climatologies_tas.txt")


colo <- c("#1b9e77", "#d95f02", "#7570b3")

for (n in 1:12)
{
  assign(paste0('tas_wrf_',n), tas_wrf[tas_wrf$month==n,]) 
}

pdf("./figures/pdfs/tas_taylorplots_wrf.pdf", height=5, width = 10)
par(mfrow=c(1,2))

for (i in 1:2)
{
  if (i == 1)
  {
    taylor.diagram(tas_wrf_1$ghcn, tas_wrf_1[,3 + i], normalize = TRUE, pch=16, col=paste0(colo[i]), main = "Mean Temperature - daily", pcex = 1.2)
  }
  else {
    taylor.diagram(tas_wrf_1$ghcn, tas_wrf_1[,3 + i], normalize = TRUE, pch=16, col=paste0(colo[i]), add = T, pcex = 1.2)
  }
  
  taylor.diagram(tas_wrf_2$ghcn, tas_wrf_2[,3 + i], pch=16, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(tas_wrf_12$ghcn, tas_wrf_12[,3 + i], pch=16, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  
  taylor.diagram(tas_wrf_3$ghcn, tas_wrf_3[,3 + i], pch=17, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(tas_wrf_4$ghcn, tas_wrf_4[,3 + i], pch=17, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(tas_wrf_5$ghcn, tas_wrf_5[,3 + i], pch=17, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  
  taylor.diagram(tas_wrf_6$ghcn, tas_wrf_6[,3 + i], pch=18, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(tas_wrf_7$ghcn, tas_wrf_7[,3 + i], pch=18, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(tas_wrf_8$ghcn, tas_wrf_8[,3 + i], pch=18, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  
  taylor.diagram(tas_wrf_9$ghcn, tas_wrf_9[,3 + i], pch=15, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(tas_wrf_10$ghcn, tas_wrf_10[,3 + i], pch=15, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(tas_wrf_11$ghcn, tas_wrf_11[,3 + i], pch=15, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  
  text(1.2,0.4,"WRF", cex=1.2, col="#1b9e77")
  text(0.6,0.5,"CHELSA_W5E5", cex=1.2, col="#d95f02")
  #text(1,0.7,"CHELSA_V2.1", cex=1.2, col="#7570b3")
  legend(1.5,1.5, legend=c("Winter","Spring","Summer", "Autumn"), pch=c(16,17,18,15),cex=0.8)
  
}
# dev.off()



#### Figure 2 ###################################################################################

####################################################### monthly aggregations
# pdf("./figures/pdfs/tas_taylorplot_monthly.pdf", height=5, width = 5)

dfx<-tas_wrf_mon[tas_wrf_mon$month==1,]
taylor.diagram(dfx$ghcn, dfx$wrf, normalize=T, col=paste0(colo[i]), main="Mean Temperature - monthly", pcex = 1.2)
for(n in c(2,12))
{
  dfx<-tas_wrf_mon[tas_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$wrf, normalize=T, add=T, col="#1b9e77", pch=16, pcex = 1.2)
}
for(n in c(3:5))
{
  dfx<-tas_wrf_mon[tas_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$wrf, normalize=T, add=T, col="#1b9e77", pch=17, pcex = 1.2)
}
for(n in c(6:8))
{
  dfx<-tas_wrf_mon[tas_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$wrf, normalize=T, add=T, col="#1b9e77", pch=18, pcex = 1.2)
}
for(n in c(9:11))
{
  dfx<-tas_wrf_mon[tas_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$wrf, normalize=T, add=T, col="#1b9e77", pch=15, pcex = 1.2)
}

text(x=1.4,y=0.3,"WRF",col="#1b9e77",cex=1.2)
text(x=1.4,y=0.3,"WRF",col="#1b9e77",cex=1.2)

# CHELSA_W5E5 ######################################################################

for(n in c(1,2,12))
{
  dfx<-tas_wrf_mon[tas_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$chelsa_w5e5, normalize=T, add=T, col="#d95f02", pch=16, pcex = 1.2)
}
for(n in c(3:5))
{
  dfx<-tas_wrf_mon[tas_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$chelsa_w5e5, normalize=T, add=T, col="#d95f02", pch=17, pcex = 1.2)
}
for(n in c(6:8))
{
  dfx<-tas_wrf_mon[tas_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$chelsa_w5e5, normalize=T, add=T, col="#d95f02", pch=18, pcex = 1.2)
}
for(n in c(9:11))
{
  dfx<-tas_wrf_mon[tas_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$chelsa_w5e5, normalize=T, add=T, col="#d95f02", pch=15, pcex = 1.2)
}
text(x=0.7,y=0.6,"CHELSA_W5E5",col="#d95f02",cex=1.2)
text(x=0.7,y=0.6,"CHELSA_W5E5",col="#d95f02",cex=1.2)

# CHELSA_V2.1 ######################################################################

for(n in c(1,2,12))
{
  dfx<-tas_wrf_mon[tas_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$chelsa_V2.1, normalize=T, add=T, col="#7570b3", pch=16, pcex = 1.2)
}
for(n in c(3:5))
{
  dfx<-tas_wrf_mon[tas_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$chelsa_V2.1, normalize=T, add=T, col="#7570b3", pch=17, pcex = 1.2)
}
for(n in c(6:8))
{
  dfx<-tas_wrf_mon[tas_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$chelsa_V2.1, normalize=T, add=T, col="#7570b3", pch=18, pcex = 1.2)
}
for(n in c(9:11))
{
  dfx<-tas_wrf_mon[tas_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$chelsa_V2.1, normalize=T, add=T, col="#7570b3", pch=15, pcex = 1.2)
}

text(x=0.5,y=0.35,"CHELSA_V2.1",col="#7570b3",cex=1.2)
text(x=0.5,y=0.35,"CHELSA_V2.1",col="#7570b3",cex=1.2)

legend(1.8,1.8, legend=c("Winter","Spring","Summer", "Autumn"), pch=c(16,17,18,15),cex=0.8)


dev.off()

