library("raster")
library("rgdal")
library("dplyr")
library("plotrix")
library("RColorBrewer")
library("data.table")

#Read in file 
pr_wrf_init <- fread("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/pr_all_10_2000_09_2013.txt")
colnames(pr_wrf_init)<-c('id','ghcn','ghcn_id','wrf','chelsa_w5e5','chelsa_V2.1','long','lat','year','month','day')
pr_wrf <- as.data.frame(pr_wrf_init)


# create the data table






# pr_wrf_mon <- aggregate(.~month+long+lat, pr_wrf, mean)

pr_wrf_mon <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/climatologies_pr.txt")

qc_pr <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/qc_pr_all_10_2000_09_2013.txt")
sum_range_check <- sum(qc_pr$V2); sum_range_check
sum_na_check <- sum(qc_pr$V3); sum_na_check

pdf("/home/harichan/temps/pr_taylorplots_wrf.pdf", height=5, width = 10)
par(mfrow=c(1,2))
colo <- c("#1b9e77", "#d95f02", "#7570b3")

for (n in 1:12)
{
  assign(paste0('pr_wrf_',n), pr_wrf[pr_wrf$month==n,]) 
}

# pdf("./figures/pdfs/pr_taylorplot.pdf", height=5, width = 5)

for (i in 1:2)
{
  if (i == 1)
  {
    taylor.diagram(pr_wrf_1$ghcn, pr_wrf_1[,3 + i], normalize = TRUE, pch=16, col=paste0(colo[i]), main = "Precipitation - daily", pcex = 1.2)
  }
  else {
    taylor.diagram(pr_wrf_1$ghcn, pr_wrf_1[,3 + i], normalize = TRUE, pch=16, col=paste0(colo[i]), add = T, pcex = 1.2)
  }
  
  taylor.diagram(pr_wrf_2$ghcn, pr_wrf_2[,3 + i], pch=16, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(pr_wrf_12$ghcn, pr_wrf_12[,3 + i], pch=16, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  
  taylor.diagram(pr_wrf_3$ghcn, pr_wrf_3[,3 + i], pch=17, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(pr_wrf_4$ghcn, pr_wrf_4[,3 + i], pch=17, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(pr_wrf_5$ghcn, pr_wrf_5[,3 + i], pch=17, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  
  taylor.diagram(pr_wrf_6$ghcn, pr_wrf_6[,3 + i], pch=18, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(pr_wrf_7$ghcn, pr_wrf_7[,3 + i], pch=18, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(pr_wrf_8$ghcn, pr_wrf_8[,3 + i], pch=18, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  
  taylor.diagram(pr_wrf_9$ghcn, pr_wrf_9[,3 + i], pch=15, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(pr_wrf_10$ghcn, pr_wrf_10[,3 + i], pch=15, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  taylor.diagram(pr_wrf_11$ghcn, pr_wrf_11[,3 + i], pch=15, col=paste0(colo[i]), add=T, normalize = T, pcex = 1.2)
  
  text(0.4,1,"WRF", cex=1.2, col="#1b9e77")
  text(0.75,0.85,"CHELSA_W5E5", cex=1.2, col="#d95f02")
  #text(0.9,0.7,"CHELSA_V2.1", cex=1.2, col="#7570b3")
  legend(1.5,1.5, legend=c("Winter","Spring","Summer", "Autumn"), pch=c(16,17,18,15),cex=0.8)
  
}
# dev.off()



#### Figure 2 ###################################################################################

####################################################### monthly aggregations
# pdf("./figures/pdfs/pr_taylorplot_monthly.pdf", height=5, width = 5)

dfx<-pr_wrf_mon[pr_wrf_mon$month==1,]
taylor.diagram(dfx$ghcn, dfx$wrf, normalize=T, col=paste0(colo[i]), main=" Precipitation - monthly", pcex = 1.2)
for(n in c(2,12))
{
  dfx<-pr_wrf_mon[pr_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$wrf, normalize=T, add=T, col="#1b9e77", pch=16, pcex = 1.2)
}
for(n in c(3:5))
{
  dfx<-pr_wrf_mon[pr_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$wrf, normalize=T, add=T, col="#1b9e77", pch=17, pcex = 1.2)
}
for(n in c(6:8))
{
  dfx<-pr_wrf_mon[pr_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$wrf, normalize=T, add=T, col="#1b9e77", pch=18, pcex = 1.2)
}
for(n in c(9:11))
{
  dfx<-pr_wrf_mon[pr_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$wrf, normalize=T, add=T, col="#1b9e77", pch=15, pcex = 1.2)
}

text(x=0.3,y=0.45,"WRF",col="#1b9e77",cex=1.2)

# CHELSA_W5E5 ######################################################################

for(n in c(1,2,12))
{
  dfx<-pr_wrf_mon[pr_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$chelsa_w5e5, normalize=T, add=T, col="#d95f02", pch=16, pcex = 1.2)
}
for(n in c(3:5))
{
  dfx<-pr_wrf_mon[pr_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$chelsa_w5e5, normalize=T, add=T, col="#d95f02", pch=17, pcex = 1.2)
}
for(n in c(6:8))
{
  dfx<-pr_wrf_mon[pr_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$chelsa_w5e5, normalize=T, add=T, col="#d95f02", pch=18, pcex = 1.2)
}
for(n in c(9:11))
{
  dfx<-pr_wrf_mon[pr_wrf_mon$month==n,]
  taylor.diagram(dfx$ghcn, dfx$chelsa_w5e5, normalize=T, add=T, col="#d95f02", pch=15, pcex = 1.2)
}
text(x=0.95,y=0.6,"CHELSA_W5E5",col="#d95f02",cex=1.2)

legend(1.5,1.5, legend=c("Winter","Spring","Summer", "Autumn"), pch=c(16,17,18,15),cex=0.8)


dev.off()

