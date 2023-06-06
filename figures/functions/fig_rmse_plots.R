library(data.table)
library(gridExtra)
library(grid)
library(colorspace)
library(plotrix)
library(viridis)
library(gplots)
library(raster)
library(scales)
library(multcompView)
library(RColorBrewer)
library(PairedData)
library(scales)
library(Metrics)
library(dplyr)
library(rgeos)
source("./figures/functions/cscl.R")
source("./figures/functions/lseq.R")


shpfile <- shapefile("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/shapefile_bias_plots")
shp_us <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
shp_cropped <- crop(shp_us, raster::extent(shpfile))
crs(shp_cropped) <- "+proj=longlat +datum=WGS84 +no_defs"

#### Alternative projections to try out ####
# crs(p_gg2_shp) <- "+proj=ortho +lat_0=39.9784n +lon_0=-105.1737w +x_0=6184.292811785623 +y_0=4505.490982981965 +ellps=GRS80"
# crs(agg2_shp) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
#####

# var <- c("pr")
# for (i in var){
# y <- fread(paste0("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/w5e5_wrf_all_",i,".txt"))
# colnames(y)<-c('id','ghcn','ghcn_id','wrf','chelsa_w5e5','chelsa_V2.1','long','lat','year','month','day','w5e5')
# 
# if (i == 'tas'){
#     y$ghcn <- y$ghcn/10
#     y$wrf <- y$wrf-273.15
#     y$chelsa_V2.1 <- y$chelsa_V2.1/10-273.15
#     y$chelsa_w5e5 <- y$chelsa_w5e5/10-273.15
#     y$w5e5 <- y$w5e5-273.15
# }
# 
# if (i == 'pr'){
#     y$ghcn <- y$ghcn/10
#     y$wrf <- y$wrf
#     y$chelsa_V2.1 <- y$chelsa_V2.1/100
#     y$chelsa_w5e5 <- y$chelsa_w5e5/10
#     y$w5e5 <- y$w5e5*60*60*24
# }
# 
# ghcn_c <- y[,c(1:2,4:8,12)]
# 
# agg2 <- aggregate(.~long+lat+id,ghcn_c,FUN='mean')
# write.table(agg2, paste0("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/agg2_",i))
# }
par(mfrow=c(2,3))
pdf(paste0("/home/harichan/mean_rmse_plots"), height = 11, width = 18)
par(mfrow=c(2,3),mar=c(0,4,0,1),oma=c(0,0,0,0))


i="tas"
t_agg2 <- read.table(paste0("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/agg2_",i))
# 
# agg2_shp<-agg2
# coordinates(agg2_shp)<-~long+lat
# crs(agg2_shp) <- "+proj=longlat +datum=WGS84 +no_defs"
# shapefile(agg2_shp, "/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/shapefile_rmse_plots")
for (n in 1:length(t_agg2$ghcn)){
 t_agg2$chelsa_w5e5_rmse[[n]] <- rmse(t_agg2$ghcn[[n]],t_agg2$chelsa_w5e5[[n]]) 
 t_agg2$chelsa_V2.1_rmse[[n]] <- rmse(t_agg2$ghcn[[n]],t_agg2$chelsa_V2.1[[n]])
 t_agg2$w5e5_rmse[[n]] <- rmse(t_agg2$ghcn[[n]],t_agg2$w5e5[[n]])
 t_agg2$chelsa_w5e5_rmse_re[[n]] <- t_agg2$chelsa_w5e5_rmse[[n]] - t_agg2$w5e5_rmse[[n]] 
}


# pdf(paste0(".figures/pdfs/mean_rmse_",i), height = 6, width = 12)
#     par(mfrow=c(1,3), mar=c(1,0,1,0), oma=c(4,-1,1,-1))
#### rmse plots ####
rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 

cx1 = 2
lg1 = -2
lb1 = 1
tg1 = 0.8
cxm = 1.45
pch1 <- 19
cex1 <- 0.25
cords <- c(-115,-75,7.5,10)

if (i == 'tas')
{  
  bbreaks <- c(0,0.1,0.25,0.5,1,1.5,2,3,4,99999)
  cols<-rbPal(length(bbreaks)+1)
  
  
  #### W5E5 #### 
  shp_us <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
  shp_cropped <- crop(shp_us, raster::extent(shpfile))
  crs(shp_cropped) <- "+proj=longlat +datum=WGS84 +no_defs"
  # crs(shp_cropped) <- "+proj=ortho +lat_0=39.9784n +lon_0=-105.1737w +x_0=6184.292811785623 +y_0=4505.490982981965 +ellps=GRS80"
  # crs(shp_cropped) <- "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"
  plot(shp_cropped,col='white',cex.main=0.7)
  title(ylab="Mean Daily 2m Air-Temperature", line=0, cex.lab=2)
  title(main="W5E5", line = -6, cex.main=2)
  t_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(t_agg2$w5e5_rmse),breaks = bbreaks))]
  plot(shp_cropped, border="dark grey", add=T)
  points(t_agg2$long,t_agg2$lat,pch = pch1,col = t_agg2$Col,cex=t_agg2$w5e5_rmse*0.2)
  
  try(cscl(cols,cords,zrng=c(0,4),at = c(),tria = "u", horiz = T, lablag=1, cx=cxm, title = " rmse"))
  
  try(cscl(cols[3:(length(cols)-1)],cords,zrng=c(0,4),at = seq(0,4,by=0.5),labs=c("0","0.1","0.25","0.5","1","1.5","2","3",">4"),tria = "u", horiz = T,
           cx=cxm,
           title= expression(paste("rmse [°C]" )) ,titlag=-2,lablag=1))
  
  #### CHELSA_W5E5 #### 
  shp_cropped <- crop(shp_us, raster::extent(shpfile))
  plot(shp_cropped,col='white',cex.main=0.7)
  
  title(main="CHELSA-W5E5", line = -6, cex.main=2)
  t_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(t_agg2$chelsa_w5e5_rmse),breaks = bbreaks))]
  plot(shp_cropped, border="dark grey", add=T)
  points(t_agg2$long,t_agg2$lat,pch = pch1,col = t_agg2$Col,cex=t_agg2$chelsa_w5e5_rmse*0.2)
  
  try(cscl(cols,cords,zrng=c(0,4),at = c(),tria = "u", horiz = T, lablag=1, cx=cxm, title = "rmse"))
  
  try(cscl(cols[3:(length(cols)-1)],cords,zrng=c(0,4),at = seq(0,4,by=0.5),labs=c("0","0.1","0.25","0.5","1","1.5","2","3",">4"),tria = "u", horiz = T,
           cx=cxm,
           title=expression(paste("rmse [°C]" )),titlag=-2,lablag=1))
  
  #### rmse reduction plot ####
  rbPal <- colorRampPalette(c('firebrick','darkorange3','gold1','grey60','grey85'))
  bbreaks_re<-c(0,-0.1,-0.25,-0.5,-1,-1.5,-2,-3,-4,99999)
  cbreaks_re <-c(0,1,2,3,4,5,6,7,999999)
  cols_re <- rbPal(length(bbreaks_re)+1)
  
  shp_cropped <- crop(shp_us, raster::extent(shpfile))
  plot(shp_cropped,col='white',cex.main=0.7)
  
  title(main="Absolute RMSE Reduction", line = -6, cex.main=2)
  
  t_agg2$Col <- rbPal(length(bbreaks_re)-1)[as.numeric(cut(abs(t_agg2$chelsa_w5e5_rmse_re)*(-1),breaks = bbreaks_re))]
  plot(shp_cropped, border="dark grey", add=T)
  points(t_agg2$long,t_agg2$lat,pch = pch1,col = t_agg2$Col, cex=abs(t_agg2$chelsa_w5e5_rmse_re)*0.3)
  
  try(cscl(cols_re,cords,zrng=c(-4,0),at = c(),tria = "b", horiz = T, lablag=1, cx=cxm))
  
  try(cscl(cols_re[3:(length(cols_re)-1)],cords,zrng=c(-4,0),at = seq(-4,0,by=0.5),tria = "b", horiz = T,
           cx=cxm, labs=c("<-4","-3","-2","-1.5","-1","-0.5","-0.25","-0.1","0"),
           title=expression(paste("abs. rmse high res. - abs. rmse coarse res. [°C]" )),titlag=-2,lablag=1))
}

# dev.off()

i="pr"
prec_agg2 <- read.table(paste0("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/agg2_",i))
# 
# agg2_shp<-agg2
# coordinates(agg2_shp)<-~long+lat
# crs(agg2_shp) <- "+proj=longlat +datum=WGS84 +no_defs"
# shapefile(agg2_shp, "/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/shapefile_rmse_plots")

for (n in 1:length(prec_agg2$ghcn)){
  prec_agg2$chelsa_w5e5_rmse[[n]] <- rmse(prec_agg2$ghcn[[n]],prec_agg2$chelsa_w5e5[[n]]) 
  prec_agg2$chelsa_V2.1_rmse[[n]] <- rmse(prec_agg2$ghcn[[n]],prec_agg2$chelsa_V2.1[[n]])
  prec_agg2$w5e5_rmse[[n]] <- rmse(prec_agg2$ghcn[[n]],prec_agg2$w5e5[[n]])
  prec_agg2$chelsa_w5e5_rmse_re[[n]] <- prec_agg2$chelsa_w5e5_rmse[[n]] - prec_agg2$w5e5_rmse[[n]] 
}


#### rmse plots ####
rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 

cx1 = 2
lg1 = -2
lb1 = 1
tg1 = 0.8
cxm = 1.45
pch1 <- 19
cex1 <- 0.25
cords <- c(-115,-75,7.5,10)

if (i == 'pr'){
  bbreaks <- c(0,0.01,0.1,0.2,0.3,0.5,0.75,1,2,99999)
  cbreaks <- c(0,1,2,3,4,5,6,7,99999900)
  
  #### W5E5 #### 

  shp_cropped <- crop(shp_us, raster::extent(shpfile))
  plot(shp_cropped,col='white',cex.main=0.7)
  title(ylab="Mean Daily Precipitation", line=0, cex.lab=2)
  prec_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(prec_agg2$w5e5_rmse),breaks = bbreaks))]
  plot(shp_cropped, border="dark grey", add=T)
  points(prec_agg2$long,prec_agg2$lat,pch = pch1,col = prec_agg2$Col,cex=0.4)
  
  cols<-rbPal(length(bbreaks)+1)
  
  try(cscl(cols,cords,zrng=c(0,2),at = c(),tria = "u", horiz = T, lablag=1, cx=cx1))
  
  try(cscl(cols[3:(length(cols)-1)],cords,zrng=c(0,2),at = seq(0,2,by=0.25),tria = "u", horiz = T,
           labs=c("0","0.01","0.1","0.2","0.3","0.5","0.75","1",">2"),  cx=cxm,
           title=expression(paste("rmse"," [", kg," ",m^{-2},'',day^{-1},']')),titlag=lg1,lablag=lb1))
  
  #### CHELSA_W5E5 #### 

  shp_cropped <- crop(shp_us, raster::extent(shpfile))
  plot(shp_cropped,col='white',cex.main=0.7)
  prec_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(prec_agg2$chelsa_w5e5_rmse),breaks = bbreaks))]
  plot(shp_cropped, border="dark grey", add=T)
  points(prec_agg2$long,prec_agg2$lat,pch = pch1,col = prec_agg2$Col,cex=0.4)
  
  cols<-rbPal(length(bbreaks)+1)
  
  try(cscl(cols,cords,zrng=c(0,2),at = c(),tria = "u", horiz = T, lablag=1, cx=cx1))
  
  try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(0,2),at = seq(0,2,by=0.25),tria = "u", horiz = T,
           labs=c("0","0.01","0.1","0.2","0.3","0.5","0.75","1",">2"),  cx=cxm,
           title=expression(paste("rmse"," [", kg," ",m^{-2},'',day^{-1},']')),titlag=lg1,lablag=lb1))
  
  #### rmse reduction plot ####
  rbPal <- colorRampPalette(c('firebrick','darkorange3','gold1','grey60','grey85'))
  bbreaks<-c(0,-0.01,-0.1,-0.2,-0.3,-0.4,-0.5,-0.75,-1,99999)
  cbreaks=c(0,1,2,3,4,5,6,7,99999900)
  
  shp_cropped <- crop(shp_us, raster::extent(shpfile))
  plot(shp_cropped,col='white',cex.main=0.7)
  
  prec_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(abs(prec_agg2$chelsa_w5e5_rmse_re)*(-1),breaks = bbreaks))]
  plot(shp_cropped, border="dark grey", add=T)
  points(prec_agg2$long,prec_agg2$lat,pch = pch1,col = prec_agg2$Col,cex=0.6)
  
  cols<-rev(rbPal(length(bbreaks)+1))
  try(cscl(cols_re,cords,zrng=c(-4,0),at = c(),tria = "b", horiz = T, lablag=1, cx=cx1))
  
  try(cscl(cols_re[3:(length(cols_re)-1)],cords,zrng=c(-4,0),at = seq(-4,0,by=0.5),tria = "b", horiz = T,
           labs=c("<-1","-0.45","-0.5","-0.4","-0.3","-0.2","-0.1","-0.01","0"),  cx=cxm,
           title=expression(paste("abs. rmse high res. - abs. rmse coarse res.", " [", kg," ",m^{-2},'',day^{-1},']')),titlag=lg1,lablag=lb1))
  
}
dev.off()
