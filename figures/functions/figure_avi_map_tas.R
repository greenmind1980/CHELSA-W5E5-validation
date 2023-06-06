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

pdf("./figures/pdfs/avi_plot_tas_bs_all")
par(mfrow=c(3,2))

y <- fread("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/tas_av_ind_bs_001.txt")

# #plotting
y_shp <- as.data.frame(y)
colnames(y) <- c("x","ai1","lat1","lon1","n","ghcn_id")
# agg2 <- aggregate(.~lon1+lat1,y, mean)
agg2 <- y
agg2_shp<-agg2
coordinates(agg2_shp)<-~lon1+lat1
# 
# pdf("/home/harichan/Documents/W5E5_validation/WRF/avi_plot_tas_bs_001", height = 5.5, width = 5.5)


library(RColorBrewer)
rbPal <- colorRampPalette(c('blue','grey80','red')) #colorRampPalette(c('#d8b365','#f5f5f5','#5ab4ac'))
bbreaks <- c(-99999,seq(-1.5,1.5,0.2),9999999)
cxm = 1.5
pch1 <- 19
cex1 <- 0.2
lg1 = 3.5
lb1 = 2
tg1 = 1.4

# cols <- c('#d8b365','#f5f5f5','#5ab4ac')
shp_us <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
shp_cropped <- crop(shp_us, raster::extent(agg2_shp))


agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(agg2$ai1),breaks = bbreaks))]
plot(agg2_shp,col='white',cex.main=0.7)
title(main="Mean Temperature Binsize = 0.01")
plot(shp_cropped, border="black", add=T)
points(agg2$lon1,agg2$lat1,pch = pch1,col = agg2$Col,cex=cex1)
cords <- c(-115,-75,7.5,10)

cols<-rbPal(length(bbreaks)+1)

try(cscl(cols,cords,zrng=c(-1.5,1.5),at = c(),tria = "b", horiz = T, lablag=1, cx=0.5, title = "added value"))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-1.5,1.5),at = seq(-1.5,1.5,by=0.0997),labs=c("<-0.4","-0.3","-0.2","-0.1","0","0.1","0.2","0.3",">0.4"),tria = "n", horiz = T,
         cx=0.5,
         title="added value",titlag=lg1,lablag=1))
# dev.off()


y <- fread("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/tas_av_ind_bs_01.txt")

# #plotting
y_shp <- as.data.frame(y)
colnames(y) <- c("x","ai1","lat1","lon1","n","ghcn_id")
# agg2 <- aggregate(.~lon1+lat1,y, mean)
agg2 <- y
agg2_shp<-agg2
coordinates(agg2_shp)<-~lon1+lat1

# pdf("/home/harichan/Documents/W5E5_validation/WRF/avi_plot_tas_bs_01", height = 5.5, width = 5.5)


library(RColorBrewer)
rbPal <- colorRampPalette(c('blue','grey80','red')) #colorRampPalette(c('#d8b365','#f5f5f5','#5ab4ac'))
bbreaks <- c(-99999,seq(-0.6,0.6,0.2),9999999)
cxm = 1.5
pch1 <- 19
cex1 <- 0.2
lg1 = 3.5
lb1 = 2
tg1 = 1.4

# cols <- c('#d8b365','#f5f5f5','#5ab4ac')
shp_us <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
shp_cropped <- crop(shp_us, raster::extent(agg2_shp))


agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(agg2$ai1),breaks = bbreaks))]
plot(agg2_shp,col='white',cex.main=0.7)
title(main="Mean Temperature Binsize = 0.1")
plot(shp_cropped, border="black", add=T)
points(agg2$lon1,agg2$lat1,pch = pch1,col = agg2$Col,cex=cex1)
cords <- c(-115,-75,7.5,10)

cols<-rbPal(length(bbreaks)+1)

try(cscl(cols,cords,zrng=c(-0.6,0.6),at = c(),tria = "b", horiz = T, lablag=1, cx=0.5, title = "added value"))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-0.6,0.6),at = seq(-0.6,0.6,by=0.2),labs=c("<-0.6","-0.4","-0.2","0","0.2","0.4",">0.6"),tria = "n", horiz = T,
         cx=0.5,
         title="added value",titlag=lg1,lablag=1))
# dev.off()

y <- fread("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/tas_av_ind_bs_05.txt")

# #plotting
y_shp <- as.data.frame(y)
colnames(y) <- c("x","ai1","lat1","lon1","n","ghcn_id")
# agg2 <- aggregate(.~lon1+lat1,y, mean)
agg2 <- y
agg2_shp<-agg2
coordinates(agg2_shp)<-~lon1+lat1

# pdf("/home/harichan/Documents/W5E5_validation/WRF/avi_plot_tas_bs_05", height = 5.5, width = 5.5)


library(RColorBrewer)
rbPal <- colorRampPalette(c('blue','grey80','red')) #colorRampPalette(c('#d8b365','#f5f5f5','#5ab4ac'))
bbreaks <- c(-99999,seq(-1.5,1.5,0.3),9999999)
cxm = 1.5
pch1 <- 19
cex1 <- 0.2
lg1 = 3.5
lb1 = 2
tg1 = 1.4

# cols <- c('#d8b365','#f5f5f5','#5ab4ac')
shp_us <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
shp_cropped <- crop(shp_us, raster::extent(agg2_shp))


agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(agg2$ai1),breaks = bbreaks))]
plot(agg2_shp,col='white',cex.main=0.7)
title(main="Mean Temperature Binsize = 0.5")
plot(shp_cropped, border="black", add=T)
points(agg2$lon1,agg2$lat1,pch = pch1,col = agg2$Col,cex=0.2)
cords <- c(-115,-75,7.5,10)

cols<-rbPal(length(bbreaks)+1)

try(cscl(cols,cords,zrng=c(-1.5,1.5),at = c(),tria = "b", horiz = T, lablag=1, cx=0.5, title = "added value"))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-1.5,1.5),at = seq(-1.5,1.5,by=0.3),labs=c("<-1.5","-1.2","-0.9","-0.6","-0.3","0","0.3","0.6","0.9","1.2",">1.5"),tria = "n", horiz = T,
         cx=0.5,
         title="added value",titlag=lg1,lablag=1))
# dev.off()


#pdf("/home/harichan/Documents/W5E5_validation/WRF/avi_plot_tas_bs_1", height = 5.5, width = 5.5)
y <- fread("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/tas_av_ind.txt")

# #plotting
y_shp <- as.data.frame(y)
colnames(y) <- c("x","ai1","lat1","lon1","n","ghcn_id")
# agg2 <- aggregate(.~lon1+lat1,y, mean)
agg2 <- y
agg2_shp<-agg2
coordinates(agg2_shp)<-~lon1+lat1


library(RColorBrewer)
rbPal <- colorRampPalette(c('blue','grey80','red')) #colorRampPalette(c('#d8b365','#f5f5f5','#5ab4ac'))
bbreaks <- c(-99999,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,9999999)
cxm = 1.5
pch1 <- 19
cex1 <- 0.2
lg1 = 3.5
lb1 = 2
tg1 = 1.4

# cols <- c('#d8b365','#f5f5f5','#5ab4ac')
shp_us <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
shp_cropped <- crop(shp_us, raster::extent(agg2_shp))


agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(agg2$ai1),breaks = bbreaks))]
plot(agg2_shp,col='white',cex.main=0.7)
title(main="Mean Temperature Binsize = 1")
plot(shp_cropped, border="black", add=T)
points(agg2$lon1,agg2$lat1,pch = pch1,col = agg2$Col,cex=cex1)
cords <- c(-115,-75,7.5,10)

cols<-rbPal(length(bbreaks)+1)

try(cscl(cols,cords,zrng=c(-0.4,0.4),at = c(),tria = "b", horiz = T, lablag=1, cx=0.5, title = "added value"))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-0.4,0.4),at = seq(-0.4,0.4,by=0.0997),labs=c("<-0.4","-0.3","-0.2","-0.1","0","0.1","0.2","0.3",">0.4"),tria = "n", horiz = T,
         cx=0.5,
         title="added value",titlag=lg1,lablag=1))
# dev.off()

y <- fread("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/tas_av_ind_bs_2.txt")

# #plotting
y_shp <- as.data.frame(y)
colnames(y) <- c("x","ai1","lat1","lon1","n","ghcn_id")
# agg2 <- aggregate(.~lon1+lat1,y, mean)
agg2 <- y
agg2_shp<-agg2
coordinates(agg2_shp)<-~lon1+lat1

# pdf("/home/harichan/Documents/W5E5_validation/WRF/avi_plot_tas_bs_2", height = 5.5, width = 5.5)

library(RColorBrewer)
rbPal <- colorRampPalette(c('blue','grey80','red')) #colorRampPalette(c('#d8b365','#f5f5f5','#5ab4ac'))
bbreaks <- c(-99999,seq(-0.04,0.04,0.01),9999999)
cxm = 1.5
pch1 <- 19
cex1 <- 0.2
lg1 = 3.5
lb1 = 2
tg1 = 1.4

# cols <- c('#d8b365','#f5f5f5','#5ab4ac')
shp_us <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
shp_cropped <- crop(shp_us, raster::extent(agg2_shp))

agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(agg2$ai1),breaks = bbreaks))]
plot(agg2_shp,col='white',cex.main=0.7)
title(main="Mean Temperature Binsize = 2")
plot(shp_cropped, border="black", add=T)
points(agg2$lon1,agg2$lat1,pch = pch1,col = agg2$Col,cex=0.2)
cords <- c(-115,-75,7.5,10)

cols<-rbPal(length(bbreaks)+1)

try(cscl(cols,cords,zrng=c(-1.5,1.5),at = c(),tria = "b", horiz = T, lablag=1, cx=0.5, title = "added value"))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-0.04,0.04),at = seq(-0.04,0.04,by=0.00999),labs=c("<-0.04","-0.03","-0.02","-0.01","0","0.01","0.02","0.03",">0.04"),tria = "n", horiz = T,
         cx=0.5,
         title="added value",titlag=lg1,lablag=1))

y <- fread("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/tas_av_ind_bs_3.txt")

# #plotting
y_shp <- as.data.frame(y)
colnames(y) <- c("x","ai1","lat1","lon1","n","ghcn_id")
# agg2 <- aggregate(.~lon1+lat1,y, mean)
agg2 <- y
agg2_shp<-agg2
coordinates(agg2_shp)<-~lon1+lat1

# pdf("/home/harichan/Documents/W5E5_validation/WRF/avi_plot_tas_bs_2", height = 5.5, width = 5.5)

library(RColorBrewer)
rbPal <- colorRampPalette(c('blue','grey80','red')) #colorRampPalette(c('#d8b365','#f5f5f5','#5ab4ac'))
bbreaks <- c(-99999,seq(-0.04,0.04,0.01),9999999)
cxm = 1.5
pch1 <- 19
cex1 <- 0.2
lg1 = 3.5
lb1 = 2
tg1 = 1.4

# cols <- c('#d8b365','#f5f5f5','#5ab4ac')
shp_us <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
shp_cropped <- crop(shp_us, raster::extent(agg2_shp))

agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(agg2$ai1),breaks = bbreaks))]
plot(agg2_shp,col='white',cex.main=0.7)
title(main="Mean Temperature Binsize = 3")
plot(shp_cropped, border="black", add=T)
points(agg2$lon1,agg2$lat1,pch = pch1,col = agg2$Col,cex=0.2)
cords <- c(-115,-75,7.5,10)

cols<-rbPal(length(bbreaks)+1)

try(cscl(cols,cords,zrng=c(-1.5,1.5),at = c(),tria = "b", horiz = T, lablag=1, cx=0.5, title = "added value"))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-0.04,0.04),at = seq(-0.04,0.04,by=0.00999),labs=c("<-0.04","-0.03","-0.02","-0.01","0","0.01","0.02","0.03",">0.04"),tria = "n", horiz = T,
         cx=0.5,
         title="added value",titlag=lg1,lablag=1))
dev.off()



