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

pdf("./figures/pdfs/avi_plot_pr_bs_all", height = 6, width = 20)
par(mfrow=c(1,3))

y <- fread("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/pr_av_ind_bs_001.txt")

# #plotting
y_shp <- as.data.frame(y)
colnames(y) <- c("x","ai1","lat1","lon1","n","ghcn_id")
# agg2 <- aggregate(.~lon1+lat1,y, mean)
agg2 <- y
agg2_shp<-agg2
coordinates(agg2_shp)<-~lon1+lat1

# pdf("/home/harichan/Documents/W5E5_validation/WRF/avi_plot_pr_bs_001")

library(RColorBrewer)
rbPal <- colorRampPalette(c('blue','grey80','red')) #colorRampPalette(c('#d8b365','#f5f5f5','#5ab4ac'))
bbreaks <- c(-99999,seq(-0.2,0.2,0.05),9999999)
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
title(main="Precipitation Binsize = 0.01", line =0)
plot(shp_cropped, border="black", add=T)
points(agg2$lon1,agg2$lat1,pch = pch1,col = agg2$Col,cex=cex1)
cords <- c(-115,-75,7.5,10)

cols<-rbPal(length(bbreaks)+1)

try(cscl(cols,cords,zrng=c(-0.2,0.2),at = c(),tria = "b", horiz = T, lablag=1, cx=0.5, title = "added value"))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-0.2,0.2),at = seq(-0.2,0.2,by=0.05),labs=c("<-0.2","-0.15","-0.1","-0.05","0","0.05","0.1","0.15",">0.2"),tria = "n", horiz = T,
         cx=0.5,
         title="added value",titlag=lg1,lablag=1))
# dev.off()


y <- fread("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/pr_av_ind_bs_01.txt")

# #plotting
y_shp <- as.data.frame(y)
colnames(y) <- c("x","ai1","lat1","lon1","n","ghcn_id")
# agg2 <- aggregate(.~lon1+lat1,y, mean)
agg2 <- y
agg2_shp<-agg2
coordinates(agg2_shp)<-~lon1+lat1


library(RColorBrewer)
rbPal <- colorRampPalette(c('blue','grey80','red')) #colorRampPalette(c('#d8b365','#f5f5f5','#5ab4ac'))
bbreaks <- c(-99999,seq(-0.2,0.2,0.05),9999999)
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
title(main="Precipitation Binsize = 0.1", line =0)
plot(shp_cropped, border="black", add=T)
points(agg2$lon1,agg2$lat1,pch = pch1,col = agg2$Col,cex=cex1)
cords <- c(-115,-75,7.5,10)

cols<-rbPal(length(bbreaks)+1)

try(cscl(cols,cords,zrng=c(-0.2,0.2),at = c(),tria = "b", horiz = T, lablag=1, cx=0.5, title = "added value"))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-0.2,0.2),at = seq(-0.2,0.2,by=0.05),labs=c("<-0.2","-0.15","-0.1","-0.05","0","0.05","0.1","0.15",">0.2"),tria = "n", horiz = T,
         cx=0.5,
         title="added value",titlag=lg1,lablag=1))
# dev.off()



y <- fread("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/pr_av_index_bs_1.txt")

# #plotting
y_shp <- as.data.frame(y)
colnames(y) <- c("x","ai1","lat1","lon1","n","ghcn_id")
# agg2 <- aggregate(.~lon1+lat1,y, mean)
agg2 <- y
agg2_shp<-agg2
coordinates(agg2_shp)<-~lon1+lat1


library(RColorBrewer)
rbPal <- colorRampPalette(c('blue','grey80','red')) #colorRampPalette(c('#d8b365','#f5f5f5','#5ab4ac'))
bbreaks <- c(-99999,seq(-0.5,0.5,0.1),9999999)
cxm = 1.5
pch1 <- 19
cex1 <-  as.numeric(agg2$ai1)*1.5
lg1 = 3.5
lb1 = 2
tg1 = 1.4

#cols <- c('#d8b365','#f5f5f5','#5ab4ac')
shp_us <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
shp_cropped <- crop(shp_us, raster::extent(agg2_shp))

agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(agg2$ai1),breaks = bbreaks))]
plot(agg2_shp,col='white',cex.main=0.7)
title(main="Precipitation Binsize = 1", line =0)
plot(shp_cropped, border="black", add=T)
points(agg2$lon1,agg2$lat1,pch = pch1,col = agg2$Col,cex=abs(cex1)*1.3)
cords <- c(-115,-75,7.5,10)

cols<-rbPal(length(bbreaks)+1)
try(cscl(cols,cords,zrng=c(-0.5,0.5),at = c(),tria = "b", horiz = T, lablag=1, cx=0.5, title = "added value"))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-0.5,0.5),at = seq(-0.5,0.5,by=0.0997),labs=c("<-0.5","0.4","-0.3","-0.2","-0.1","0","0.1","0.2","0.3","0.4",">0.5"),tria = "n", horiz = T,
         cx=0.5,
         title="added value",titlag=3.5,lablag=1))
dev.off()

