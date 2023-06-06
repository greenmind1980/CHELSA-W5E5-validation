packages <- c("data.table", "gridExtra", "grid", "colorspace", "plotrix", "viridis", 
              "gplots", "raster", "scales","multcompView","RColorBrewer","PairedData","Metrics","dplyr","rgeos")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

# Load required packages
l <- sapply(packages, require, character.only = TRUE); rm(packages, l)


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

var <- c("tas","tasmax","tasmin","pr")
for (i in unique(var))
{

if (i == 'tas')
{  
    y <- fread(paste0("/storage/harichan/chelsa_V2/W5E5_validation/global/chelsa_w5e5_all_",i,"_merged.txt"), fill=T)
    y <- y[complete.cases(y),]
    colnames(y)<-c('id','ghcn','ghcn_id','chelsa_w5e5','long','lat','w5e5')
}
if ( i == 'pr')
{  
    y <- fread(paste0("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/w5e5_wrf_all_",i,".txt"), fill=T)
    colnames(y)<-c('id','ghcn','ghcn_id','wrf','chelsa_w5e5','chelsa_V2.1','long','lat','year','month','day','w5e5')
    y <- y[complete.cases(y),]
}

if (i == 'tas'){
    y$ghcn <- y$ghcn/10
    y$wrf <- y$wrf-273.15
    y$chelsa_V2.1 <- y$chelsa_V2.1/10-273.15
    y$chelsa_w5e5 <- y$chelsa_w5e5/10-273.15
    y$w5e5 <- y$w5e5-273.15
    #ghcn_c <- y[,c(2,4:8,12)]
    ghcn_c <- y[,c(2,4:7)]
    ghcn_c <- ghcn_c[ghcn_c$lat >= 25 & ghcn_c$lat <= 75 & ghcn_c$long <= -55 & ghcn_c$long >= -170,]
}

if (i == 'pr'){
    y$ghcn <- y$ghcn/10
    y$wrf <- y$wrf
    y$chelsa_V2.1 <- y$chelsa_V2.1/100
    y$chelsa_w5e5 <- y$chelsa_w5e5/10
    y$w5e5 <- y$w5e5*60*60*24
    ghcn_c <- y[,c(2,4:8,12)]
    ghcn_c <- ghcn_c[ghcn_c$lat >= 25 & ghcn_c$lat <= 75 & ghcn_c$long <= -55 & ghcn_c$long >= -170,]
}

if ( i == 'tasmax' || i == 'tasmin')
{
    y <- fread(paste0("/storage/harichan/chelsa_V2/W5E5_validation/global/chelsa_w5e5_all_",i,"_merged.txt"), fill=T)
    y <- y[complete.cases(y),]
    colnames(y)<-c('id','ghcn','ghcn_id','chelsa_w5e5','long','lat','w5e5')
    y$ghcn <- as.numeric(y$ghcn/10)
    # y$wrf <- y$wrf-273.15
    #y$chelsa_V2.1 <- as.numeric(y$chelsa_V2.1/10-273.15)
    y$chelsa_w5e5 <- as.numeric(y$chelsa_w5e5/10-273.15)
    y$w5e5 <- as.numeric(y$w5e5-273.15)
    ghcn_c <- y[,c(2,4:7)]
    ghcn_c <- ghcn_c[ghcn_c$lat >= 25 & ghcn_c$lat <= 75 & ghcn_c$long <= -55 & ghcn_c$long >= -170,]
}

 
 agg2 <- aggregate(.~long+lat,ghcn_c,FUN='mean')
 write.table(agg2, paste0(paste0("~/agg2_",i,"_NA.csv")))
}





cex_points <- 0.5
pdf(paste0("~/mean_bias_plots_all.pdf"), height = 22, width = 18)

par(mfrow=c(4,3),mar=c(0,4,0,1),oma=c(0,0,0,0))


i="tas"
for (i in c('tas','tasmax','tasmin'))
{
t_agg2 <- read.csv(paste0("~/agg2_",i,"_NA.csv"), sep=",")

t_agg2$chelsa_w5e5_bias <- t_agg2$ghcn - t_agg2$chelsa_w5e5 
#t_agg2$chelsa_V2.1_bias <- t_agg2$ghcn - t_agg2$chelsa_V2.1 
t_agg2$w5e5_bias <- t_agg2$ghcn - t_agg2$w5e5
t_agg2$chelsa_w5e5_bias_re <- abs(t_agg2$chelsa_w5e5_bias) - abs(t_agg2$w5e5_bias)

t_agg2 <- t_agg2[!abs(t_agg2$w5e5_bias) >= 10,]

coordinates(t_agg2) <- ~long+lat
t_agg2 <- crop(t_agg2, raster::extent(shpfile))
t_agg2 <- as.data.frame(t_agg2)

rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 
cx1 = 2
lg1 = -2
lb1 = 1
tg1 = 0.8
cxm = 1.45
pch1 <- 19
cex1 <- 0.25
cords <- c(-115,-75,7.5,10)
bbreaks <- c(-99999,-4,-3,-1,-0.5,-0.1,0.1,0.5,1,3,4,9999999)
cbreaks <- c(0,1,2,3,4,5,6,7,99999900)
cols<-rbPal(length(bbreaks)+1)
#### CHELSA_W5E5 #### 
shp_cropped <- crop(shp_us, raster::extent(shpfile))
plot(shp_cropped,col='white',cex.main=0.7)
if (i == 'tas')
{
title(main="W5E5", line = -6, cex.main=2)
}
t_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(t_agg2$w5e5_bias),breaks = bbreaks))]
plot(shp_cropped, border="dark grey", add=T)
points(t_agg2$long,t_agg2$lat,pch = pch1,col = t_agg2$Col,cex=abs(t_agg2$w5e5_bias)*0.2)

if (i == 'tas')
{
ytitle = "Mean Daily 2m Air-Temperature"
}

if (i == 'tasmax')
{
ytitle = "Maximum Daily 2m Air-Temperature"
}

if (i == 'tasmin')
{
ytitle = "Minimum Daily 2m Air-Temperature"
}


title(ylab=ytitle, line=0, cex.lab=2)

try(cscl(cols,cords,zrng=c(-4,4),at = c(),tria = "b", horiz = T, lablag=1, cx=cxm, title = "bias"))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-4,4),at = seq(-4,4,by=0.887),labs=c("<-4","-3","-1","-0.5","0.1","0.1","0.5","1","3",">4"),tria = "n", horiz = T,
         cx=cxm,
         title=expression(paste("Bias [°C]" )),titlag=-2,lablag=1))

#### bias plots ####
rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 
cx1 = 2
lg1 = -2
lb1 = 1
tg1 = 0.8
cxm = 1.45
pch1 <- 19
cex1 <- 0.25
cords <- c(-115,-75,7.5,10)

bbreaks <- c(-99999,-4,-3,-1,-0.5,-0.1,0.1,0.5,1,3,4,9999999)
cbreaks <- c(0,1,2,3,4,5,6,7,99999900)
cols<-rbPal(length(bbreaks)+1)

#### CHELSA_W5E5 #### 
# shp_us <- shapefile('/home/harichan/Downloads/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
shp_cropped <- crop(shp_us, raster::extent(shpfile))
plot(shp_cropped,col='white',cex.main=0.7)
if (i == 'tas')
{
title(main="CHELSA-W5E5", line = -6, cex.main=2)
}
t_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(t_agg2$chelsa_w5e5_bias),breaks = bbreaks))]
plot(shp_cropped, border="dark grey", add=T)
points(t_agg2$long,t_agg2$lat,pch = pch1,col = t_agg2$Col,cex=abs(t_agg2$chelsa_w5e5_bias)*0.15)

try(cscl(cols,cords,zrng=c(-4,4),at = c(),tria = "b", horiz = T, lablag=1, cx=cxm, title = "bias"))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-4,4),at = seq(-4,4,by=0.887),labs=c("<-4","-3","-1","-0.5","0.1","0.1","0.5","1","3",">4"),tria = "n", horiz = T,
         cx=cxm,
         title=expression(paste("Bias [°C]" )),titlag=-2,lablag=1))

#### bias reduction plot ####

rbPal <- colorRampPalette(c('#00441b','#1b7837','#5aae61','#a6dba0','#d9f0d3','#f7f7f7','#e7d4e8','#c2a5cf','#9970ab','#762a83','#40004b')) 
bbreaks<- c(-99999,-2,-1.5,-1,-0.5,-0.1,0.1,0.5,1,1.5,2,9999999)
cbreaks <- c(0,1,2,3,4,5,6,7,99999900)
cols<-rbPal(length(bbreaks)+1)

shp_cropped <- crop(shp_us, raster::extent(shpfile))
plot(shp_cropped,col='white',cex.main=0.7)
if (i == 'tas')
{
title(main="Absolute Bias Reduction", line = -6, cex.main=2)
}
t_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(t_agg2$chelsa_w5e5_bias_re),breaks = bbreaks))]
plot(shp_cropped, border="dark grey", add=T)
points(t_agg2$long,t_agg2$lat,pch = pch1,col = t_agg2$Col, cex=abs(t_agg2$chelsa_w5e5_bias_re)*0.3)

try(cscl(cols,cords,zrng=c(-2,2),at = c(),tria = "b", horiz = T, lablag=1, cx=cxm))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-2,2),at = seq(-2,2,by=0.442),labs=c("<-2","-1.5","-1","-0.5","-0.1","0.1","0.5","1","1.5",">2"),tria = "n", horiz = T,
         cx=cxm,
         title=expression(paste("abs. bias high res. - abs. bias coarse res. [°C]")),titlag=-2,lablag=1))


}
# dev.off()


i="pr"
p_agg2 <- read.csv(paste0("~/agg2_",i,"_NA.csv"), sep=",")

p_agg2$chelsa_w5e5_bias <- p_agg2$ghcn - p_agg2$chelsa_w5e5 
#p_agg2$chelsa_V2.1_bias <- p_agg2$ghcn - p_agg2$chelsa_V2.1 
p_agg2$w5e5_bias <- p_agg2$ghcn - p_agg2$w5e5
p_agg2$chelsa_w5e5_bias_re <- abs(p_agg2$chelsa_w5e5_bias) - abs(p_agg2$w5e5_bias)


rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 
cx1 = 2
lg1 = -2
lb1 = 1
tg1 = 0.8
cxm = 1.45
pch1 <- 19
cex1 <- 0.25
cords <- c(-115,-75,7.5,10)
bbreaks <- c(-99999,-1.5,-1,-0.25,-0.1,0.1,0.25,1,1.5,9999999)
cbreaks <- c(0,1,2,3,4,5,6,7,99999900)
cols<-rbPal(length(bbreaks)+1)
y <- p_agg2$w5e5_bias[p_agg2$w5e5_bias > 5 ] <- 5
y <- p_agg2$w5e5_bias[p_agg2$w5e5_bias < -5 ] <- -5



#### CHELSA_W5E5 #### 
shp_cropped <- crop(shp_us, raster::extent(shpfile))
plot(shp_cropped,col='white',cex.main=0.7)


p_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(p_agg2$w5e5_bias),breaks = bbreaks))]
plot(shp_cropped, border="dark grey", add=T)
points(p_agg2$long,p_agg2$lat,pch = pch1,col = p_agg2$Col,cex=abs(p_agg2$w5e5_bias)*0.1)
title(ylab="Mean Daily Precipitation", line=0, cex.lab=2)

try(cscl(cols,cords,zrng=c(-1,1),at = c(),tria = "b", horiz = T, lablag=1, cx=cx1))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-1.5,1.5),at = seq(-1.5,1.5,by=3/7),tria = "n", horiz = T,
         labs=c("<-1.5","-1","-0.25","-0.1","0.1","0.25","1",">1.5"),  cx=cxm,
         title=expression(paste("Bias"," [", kg," ",m^{-2},'',day^{-1},']')),titlag=lg1,lablag=lb1))


#### bias plots ####
rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 
cx1 = 2
lg1 = -2
lb1 = 1
tg1 = 0.8
cxm = 1.45
pch1 <- 19
cex1 <- 0.25
cords <- c(-115,-75,7.5,10)
y <- p_agg2$chelsa_w5e5_bias[p_agg2$chelsa_w5e5_bias > 5 ] <- 5
y <- p_agg2$chelsa_w5e5_bias[p_agg2$chelsa_w5e5_bias < -5 ] <- -5


bbreaks <- c(-99999,-1.5,-1,-0.25,-0.1,0.1,0.25,1,1.5,9999999)
cbreaks <- c(0,1,2,3,4,5,6,7,99999900)
cols<-rbPal(length(bbreaks)+1)

#### CHELSA_W5E5 #### 
# shp_us <- shapefile('/home/harichan/Downloads/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
shp_cropped <- crop(shp_us, raster::extent(shpfile))
plot(shp_cropped,col='white',cex.main=0.7)


p_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(p_agg2$chelsa_w5e5_bias),breaks = bbreaks))]
plot(shp_cropped, border="dark grey", add=T)
points(p_agg2$long,p_agg2$lat,pch = pch1,col = p_agg2$Col,cex=abs(p_agg2$chelsa_w5e5_bias)*0.1)

try(cscl(cols,cords,zrng=c(-1.5,1.5),at = c(),tria = "b", horiz = T, lablag=1, cx=cx1))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-1.5,1.5),at = seq(-1.5,1.5,by=3/7),tria = "n", horiz = T,
         labs=c("<-1.5","-1","-0.25","-0.1","0.1","0.25","1",">1.5"),  cx=cxm,
         title=expression(paste("Bias"," [", kg," ",m^{-2},'',day^{-1},']')),titlag=lg1,lablag=lb1))

#### bias reduction plot ####
p_agg2$chelsa_w5e5_bias_re[p_agg2$chelsa_w5e5_bias_re > 5 ] <- 5
p_agg2$chelsa_w5e5_bias_re[p_agg2$chelsa_w5e5_bias_re < -5 ] <- -5

rbPal <- colorRampPalette(c('#00441b','#1b7837','#5aae61','#a6dba0','#d9f0d3','#f7f7f7','#e7d4e8','#c2a5cf','#9970ab','#762a83','#40004b')) 
bbreaks<- c(-99999,-1,-0.7,-0.5,-0.2,-0.1,0.1,0.2,0.5,0.7,1,9999999)
cbreaks <- c(0,1,2,3,4,5,6,7,99999900)
cols<-rbPal(length(bbreaks)+1)

shp_cropped <- crop(shp_us, raster::extent(shpfile))
plot(shp_cropped,col='white',cex.main=0.7)

p_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(p_agg2$chelsa_w5e5_bias_re),breaks = bbreaks))]
plot(shp_cropped, border="dark grey", add=T)
points(p_agg2$long,p_agg2$lat,pch = pch1,col = p_agg2$Col, cex=abs(p_agg2$chelsa_w5e5_bias_re)*0.3)

try(cscl(cols,cords,zrng=c(-1,1),at = c(),tria = "b", horiz = T, lablag=1, cx=cx1))

try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-1,1),at = seq(-1,1,by=0.221),tria = "b", horiz = T,labs=c("<-1","-0.7","-0.5","-0.2","-0.1","0.1","0.2","0.5","0.7",">1"),  cx=cxm, title=expression(paste("abs. bias high res. - abs. bias coarse res.", " [", kg," ",m^{-2},'',day^{-1},']')),titlag=lg1,lablag=lb1))

dev.off()





#####
#tasmax 
# tasmax <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/chelsa_w5e5_all_tasmax_merged.txt")
# colnames(tasmax)<-c('id','ghcn','ghcn_id','chelsa_w5e5','long','lat','w5e5')
# coordinates(tasmax)<-~long+lat
# tasmax_cropped <- crop(tasmax, raster::extent(shp_cropped))
# agg_tasmax_crop <- aggregate(.~long+lat,tasmax_cropped,FUN='quantile', probs=0.95)
# write.table(agg_tasmax_crop, "/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/agg_tasmax_crop")

#### Extreme Percentiles ####
#Temperature 
par(mfrow=c(4,3))
pdf(paste0("/home/harichan/extreme_bias_plots.pdf"), height = 22, width = 18)
par(mfrow=c(4,3),mar=c(0,4,0,1),oma=c(0,0,0,0))

var <- c("tasmax")
for (i in var){
  
  max_agg2 <-  read.table(paste0("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/agg_tasmax_crop"))
  # max_agg2 <- max_agg2[,c(1:2,4:7)]
  max_agg2 <- max_agg2[complete.cases(max_agg2),]
  
  
  if (i == 'tasmax'){
    max_agg2$ghcn <- max_agg2$ghcn/10
    max_agg2$chelsa_w5e5 <- max_agg2$chelsa_w5e5/10-273.15
    max_agg2$w5e5 <- max_agg2$w5e5-273.15
  }
  
  max_agg2_shp <- max_agg2
  max_agg2$w5e5_bias <- max_agg2$ghcn - max_agg2$w5e5
  
  max_agg2$chelsa_w5e5_bias <- max_agg2$ghcn - max_agg2$chelsa_w5e5 
  
  max_agg2$w5e5_bias[max_agg2$w5e5_bias> 10 ] <- 10
  max_agg2$w5e5_bias[max_agg2$w5e5_bias< -10 ] <- -10
  
  max_agg2$chelsa_w5e5_bias[max_agg2$chelsa_w5e5_bias> 10 ] <- 10
  max_agg2$chelsa_w5e5_bias[max_agg2$chelsa_w5e5_bias< -10 ] <- -10
  
  max_agg2$chelsa_w5e5_bias_re <- abs(max_agg2$chelsa_w5e5_bias) - abs(max_agg2$w5e5_bias)
  merged_tasmax <- merge(t_agg2, max_agg2, by=c("long","lat"))
  merged_tasmax$bias_reduction_m_p <- merged_tasmax$chelsa_w5e5_bias_re.x - merged_tasmax$chelsa_w5e5_bias_re.y
  
  
  coordinates(max_agg2) <- ~long+lat
  crs(max_agg2) <- "+proj=longlat +datum=WGS84 +no_defs"
  max_agg2 <- (crop(max_agg2, raster::extent(shp_cropped)))
  
  #### Plots Extreme Percentiles Aggregated  #### 
  
  # pdf(paste0("/home/harichan/Documents/W5E5_validation/chelsa_w5e5/perc_abs_bias_",i), height = 6, width = 12)
  # par(mfrow=c(1,3), mar=c(1,0,1,0), oma=c(4,-1,1,-1))
  shp_us <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
  shp_cropped <- crop(shp_us, raster::extent(shpfile))
  crs(shp_cropped) <- "+proj=longlat +datum=WGS84 +no_defs"
  #### bias plots ####
  rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 
  
  cx1 = 0.8
  lg1 = -2
  lb1 = 1
  tg1 = 0.8
  cxm = 1.45
  pch1 <- 19
  cex1 <- 0.25
  cords <- c(-115,-75,7.5,10)
  
  if (i == 'tasmax')
  {       
    bbreaks <- c(-99999,-4,-2,-1,-0.5,-0.1,0.1,0.5,1,2,4,9999999)
    cbreaks <- c(0,1,2,3,4,5,6,7,99999900)
    cols<-rbPal(length(bbreaks)+1)
    
    #### W5E5 #### 
    plot(shp_cropped,col='white',cex.main=0.7)
    title(ylab="95th Perc. Daily Maximum", line=0.5, cex.lab=2)
    title(ylab="2m Air-Temperature", line=-1.5, cex.lab=2)
    title(main="W5E5", line = -6, cex.main=2)
    text(-135,62, "a)", cex=2)
    max_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(max_agg2$w5e5_bias),breaks = bbreaks))]
    plot(shp_cropped, border="dark grey", add=T)
    points(max_agg2$long,max_agg2$lat,pch = pch1,col = max_agg2$Col,cex=abs(max_agg2$w5e5_bias)*0.1)
    
    try(cscl(cols,cords,zrng=c(-4,4),at = c(),tria = "b", horiz = T, lablag=1, cx=cxm, title = "bias"))
    
    try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-4,4),at = seq(-4,4,by=0.887),labs=c("<-4","-2","-1","-0.5","0.1","0.1","0.5","1","2",">4"),tria = "n", horiz = T,
             cx=cxm,
             title= expression(paste("Bias [°C]" )) ,titlag=-2,lablag=1))
    
    #### CHELSA_W5E5 #### 
    plot(shp_cropped,col='white',cex.main=0.7)
    
    title(main="CHELSA-W5E5", line = -6, cex.main=2)
    
    
    max_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(max_agg2$chelsa_w5e5_bias),breaks = bbreaks))]
    plot(shp_cropped, border="dark grey", add=T)
    points(max_agg2$long,max_agg2$lat,pch = pch1,col = max_agg2$Col,cex=abs(max_agg2$chelsa_w5e5_bias)*0.1)
    
    try(cscl(cols,cords,zrng=c(-4,4),at = c(),tria = "b", horiz = T, lablag=1, cx=cxm, title = "bias"))
    
    try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-4,4),at = seq(-4,4,by=0.887),labs=c("<-4","-2","-1","-0.5","0.1","0.1","0.5","1","2",">4"),tria = "n", horiz = T,
             cx=cxm,
             title=expression(paste("Bias [°C]" )),titlag=-2,lablag=1))
    
    #### bias reduction plot ####
  
    
    rbPal <- colorRampPalette(c('#00441b','#1b7837','#5aae61','#a6dba0','#d9f0d3','#f7f7f7','#e7d4e8','#c2a5cf','#9970ab','#762a83','#40004b')) 
    bbreaks_re <-c(-99999,-2,-1.5,-1,-0.5,-0.1,0.1,0.5,1,1.5,2,9999999)
    cbreaks_re <-c(0,1,2,3,4,5,6,7,999999)
    cols_re <- rbPal(length(bbreaks_re)+1)
    
    plot(shp_cropped,col='white',cex.main=0.7)
    
    title(main="Absolute Bias Reduction", line = -6, cex.main=2)
    
    max_agg2$Col <- rbPal(length(bbreaks_re)-1)[as.numeric(cut(as.numeric(max_agg2$chelsa_w5e5_bias_re),breaks = bbreaks))]
    plot(shp_cropped, border="dark grey", add=T)
    points(max_agg2$long,max_agg2$lat,pch = pch1,col = max_agg2$Col, cex=abs(max_agg2$chelsa_w5e5_bias_re)*0.3)
    
    try(cscl(cols_re,cords,zrng=c(-2,2),at = c(),tria = "b", horiz = T, lablag=1, cx=cxm))
    
    try(cscl(cols_re[3:(length(cols_re)-2)],cords,zrng=c(-2,2),at = seq(-2,2,by=0.442),tria = "b", horiz = T,
             cx=cxm, labs=c("<-2","-1.5","-1","-0.5","-0.1","0.1","0.5","1","1.5",">2"),
             title=expression(paste("abs. bias high res. - abs. bias coarse res. [°C]" )),titlag=-2,lablag=1))
  }
  # dev.off()
}

#### Plot Minimum Extreme Bias ####

#tasmin
# tasmin <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/chelsa_w5e5_all_tasmin_merged.txt")
# colnames(tasmin)<-c('id','ghcn','ghcn_id','chelsa_w5e5','long','lat','w5e5')
# coordinates(tasmin)<-~long+lat
# 
# shpfile <- shapefile("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/shapefile_bias_plots")
# shp_cropped <- crop(shp_us, raster::extent(shpfile))
# crs(shp_cropped) <- "+proj=longlat +datum=WGS84 +no_defs"
# 
# tasmin_cropped <- crop(tasmin, raster::extent(shp_cropped))
# tasmin_cropped <- tasmin_cropped[,c(1:2,4:5)]
# agg_tasmin_crop <- aggregate(.~long+lat,tasmin_cropped, FUN='mean', probs=0.05)
# write.table(agg_tasmin_crop, "/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/agg_tasmin_crop")

var <- c("tasmin")
for (i in var){
  
  min_agg2 <-  read.table(paste0("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/agg_tasmin_crop"))
  min_agg2 <- min_agg2[complete.cases(min_agg2),]
  
  
  if (i == 'tasmin'){
    min_agg2$ghcn <- min_agg2$ghcn/10
   
    min_agg2$chelsa_w5e5 <- min_agg2$chelsa_w5e5/10-273.15
    min_agg2$w5e5 <- min_agg2$w5e5-273.15
  }
  min_agg2_shp <- min_agg2
  min_agg2$chelsa_w5e5_bias <- min_agg2$ghcn - min_agg2$chelsa_w5e5 

  min_agg2$w5e5_bias <- min_agg2$ghcn - min_agg2$w5e5
  min_agg2$chelsa_w5e5_bias_re <- abs(min_agg2$chelsa_w5e5_bias) - abs(min_agg2$w5e5_bias)
  merged_tasmin <- merge(t_agg2, min_agg2, by=c("long","lat"))
  merged_tasmin$bias_reduction_m_p <- merged_tasmin$chelsa_w5e5_bias_re.x - merged_tasmin$chelsa_w5e5_bias_re.y
  
  coordinates(min_agg2) <- ~long+lat
  crs(min_agg2) <- "+proj=longlat +datum=WGS84 +no_defs"
  min_agg2 <- (crop(min_agg2, raster::extent(shp_cropped)))
  
  # pdf(paste0("/home/harichan/Documents/W5E5_validation/chelsa_w5e5/perc_abs_bias_",i), height = 6, width = 12)
  # par(mfrow=c(1,3), mar=c(1,0,1,0), oma=c(4,-1,1,-1))
  shp_us <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
  shp_cropped <- crop(shp_us, raster::extent(shpfile))
  crs(shp_cropped) <- "+proj=longlat +datum=WGS84 +no_defs"
  #### bias plots ####
  rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 
  
  cx1 = 2
  lg1 = -2
  lb1 = 1
  tg1 = 0.8
  cxm = 1.45
  pch1 <- 19
  cex1 <- 0.25
  cords <- c(-115,-75,7.5,10)
  
  if (i == 'tasmin')
  {       
    bbreaks <- c(-99999,-4,-2,-1,-0.5,-0.1,0.1,0.5,1,2,4,9999999)
    cbreaks <- c(0,1,2,3,4,5,6,7,99999900)
    cols<-rbPal(length(bbreaks)+1)
    
    #### W5E5 #### 
    plot(shp_cropped,col='white',cex.main=0.7)
    text(-135,62, "b)", cex=2)
    title(ylab="5th Perc. Daily Minimum", line=0.5, cex.lab=2)
    title(ylab="2m Air-Temperature", line=-1.5, cex.lab=2)
    
    min_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(min_agg2$w5e5_bias),breaks = bbreaks))]
    plot(shp_cropped, border="dark grey", add=T)
    points(min_agg2$long,min_agg2$lat,pch = pch1,col = min_agg2$Col,cex=abs(min_agg2$w5e5_bias)*0.1)
    
    try(cscl(cols,cords,zrng=c(-4,4),at = c(),tria = "b", horiz = T, lablag=1, cx=cxm, title = "bias"))
    
    try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-4,4),at = seq(-4,4,by=0.887),labs=c("<-4","-2","-1","-0.5","0.1","0.1","0.5","1","2",">4"),tria = "n", horiz = T,
             cx=cxm,
             title= expression(paste("Bias [°C]" )) ,titlag=-2,lablag=1))
    
    #### CHELSA_W5E5 #### 
    plot(shp_cropped,col='white',cex.main=0.7)
    
    min_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(min_agg2$chelsa_w5e5_bias),breaks = bbreaks))]
    plot(shp_cropped, border="dark grey", add=T)
    points(min_agg2$long,min_agg2$lat,pch = pch1,col = min_agg2$Col,cex=abs(min_agg2$chelsa_w5e5_bias)*0.1)
    
    try(cscl(cols,cords,zrng=c(-4,4),at = c(),tria = "b", horiz = T, lablag=1, cx=cx1, title = " bias"))
    
    try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-4,4),at = seq(-4,4,by=0.887),labs=c("<-4","-2","-1","-0.5","0.1","0.1","0.5","1","2",">4"),tria = "n", horiz = T,
             cx=cxm,
             title=expression(paste("Bias [°C]" )),titlag=-2,lablag=1))
    
    #### bias reduction plot ####
    
    rbPal <- colorRampPalette(c('#00441b','#1b7837','#5aae61','#a6dba0','#d9f0d3','#f7f7f7','#e7d4e8','#c2a5cf','#9970ab','#762a83','#40004b')) 
    bbreaks_re <- c(-99999,-2,-1.5,-1,-0.5,-0.1,0.1,0.5,1,1.5,2,9999999)
    cbreaks_re <-c(0,1,2,3,4,5,6,7,999999)
    cols_re <- rbPal(length(bbreaks_re)+1)
    
    plot(shp_cropped,col='white',cex.main=0.7)
    
    min_agg2$Col <- rbPal(length(bbreaks_re)-1)[as.numeric(cut(as.numeric(min_agg2$chelsa_w5e5_bias_re),breaks = bbreaks))]
    plot(shp_cropped, border="dark grey", add=T)
    points(min_agg2$long,min_agg2$lat,pch = pch1,col = min_agg2$Col, cex=abs(min_agg2$chelsa_w5e5_bias_re)*0.3)
    
    try(cscl(cols_re,cords,zrng=c(-2,2),at = c(),tria = "b", horiz = T, lablag=1, cx=cx1))
    
    try(cscl(cols_re[3:(length(cols_re)-2)],cords,zrng=c(-2,2),at = seq(-2,2,by=0.442),tria = "b", horiz = T,
             cx=cxm, labs=c("<-2","-1.5","-1","-0.5","-0.1","0.1","0.5","1","1.5",">2"),
             title=expression(paste("abs. bias high res. - abs. bias coarse res. [°C]" )),titlag=-2,lablag=1))
  }
  
  # dev.off()
}

#### Plot Wet Days Precipitation Extremes #### 
# var <- c("pr")
# for (i in var){
#   pr <- fread(paste0("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/w5e5_wrf_all_pr.txt"))
#   # colnames(pr)<-c('id','ghcn','ghcn_id','wrf','chelsa_w5e5','long','lat','w5e5')
# 
#   if (i == 'pr'){
#     pr$ghcn <- pr$ghcn/10
#     pr$wrf <- pr$wrf
#     pr$chelsa_w5e5 <- pr$chelsa_w5e5/10
#     pr$w5e5 <- pr$w5e5*60*60*24
#   }
# 
#   pr <- pr[,c(1:2,4:8)]
#   pr_filtered_cropped <- filter(pr, ghcn > 0.1)
#   agg_pr_crop_filtered <- aggregate(.~long+lat,pr_filtered_cropped, FUN='quantile', probs=0.95)
#   write.table(agg_pr_crop_filtered, "/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/pr_agg_cropped_filtered")
  
  prec_agg2 <-  read.table(paste0("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/pr_agg_cropped_filtered"))
  prec_agg2 <- prec_agg2[complete.cases(prec_agg2),]
  
  prec_agg2_shp <- prec_agg2
  prec_agg2$chelsa_w5e5_bias <- prec_agg2$ghcn - prec_agg2$chelsa_w5e5 
  prec_agg2$w5e5_bias <- prec_agg2$ghcn - prec_agg2$w5e5
  prec_agg2$chelsa_w5e5_bias_re <- abs(prec_agg2$chelsa_w5e5_bias) - abs(prec_agg2$w5e5_bias) 
  merged_pr <- merge(p_agg2, prec_agg2, by=c("long","lat"))
  merged_pr$bias_reduction_m_p <- merged_pr$chelsa_w5e5_bias_re.x - merged_pr$chelsa_w5e5_bias_re.y
  
  coordinates(prec_agg2) <- ~long+lat
  crs(prec_agg2) <- "+proj=longlat +datum=WGS84 +no_defs"
  prec_agg2 <- (crop(prec_agg2, raster::extent(shp_cropped)))
  # 
  # pdf(paste0("/home/harichan/Documents/W5E5_validation/chelsa_w5e5/perc_abs_bias_",i), height = 6, width = 12)
  # par(mfrow=c(1,3), mar=c(1,0,1,0), oma=c(4,-1,1,-1))
  shp_us <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
  shp_cropped <- crop(shp_us, raster::extent(shpfile))
  crs(shp_cropped) <- "+proj=longlat +datum=WGS84 +no_defs"
  #### bias plots ####
  rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 
  
  cx1 = 2
  lg1 = -2
  lb1 = 1
  tg1 = 0.8
  cxm = 1.45
  pch1 <- 19
  cex1 <- 0.25
  cords <- c(-115,-75,7.5,10)
  
 
    bbreaks <- c(-99999,-10,-5,-2.5,-0.1,0.1,2.5,5,10,99999)
    cbreaks <- c(0,1,2,3,4,5,6,7,99999900)
    
    #### W5E5 #### 
    # shp_us <- shapefile('/home/harichan/Downloads/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')
    shp_cropped <- crop(shp_us, raster::extent(shpfile))
    plot(shp_cropped,col='white',cex.main=0.7)
    text(-135,62, "c)", cex=2)
    title(ylab="95th Perc. Wet Days", line=0.5, cex.lab=2)
    
    prec_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(prec_agg2$w5e5_bias),breaks = bbreaks))]
    plot(shp_cropped, border="dark grey", add=T)
    points(prec_agg2$long,prec_agg2$lat,pch = pch1,col = prec_agg2$Col,cex=0.2)
    
    cols<-rbPal(length(bbreaks)+1)
    
    try(cscl(cols,cords,zrng=c(-10,10),at = c(),tria = "b", horiz = T, lablag=1, cx=cx1))
    
    try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-10,10),at = seq(-10,10,by=2.85),tria = "n", horiz = T,
             labs=c("<-10","-5","-2.5","-0.1","0.1","2.5","5",">10"),  cx=cxm,
             title=expression(paste( "Bias"," [", kg," ",m^{-2},'',day^{-1},']')),titlag=lg1,lablag=lb1))
    
    #### CHELSA_W5E5 #### 
    shp_cropped <- crop(shp_us, raster::extent(shpfile))
    plot(shp_cropped,col='white',cex.main=0.7)
    
    prec_agg2$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(as.numeric(prec_agg2$chelsa_w5e5_bias),breaks = bbreaks))]
    plot(shp_cropped, border="dark grey", add=T)
    points(prec_agg2$long,prec_agg2$lat,pch = pch1,col = prec_agg2$Col,cex=0.2)
    
    cols<-rbPal(length(bbreaks)+1)
    
    try(cscl(cols,cords,zrng=c(-10,10),at = c(),tria = "b", horiz = T, lablag=1, cx=cx1))
    
    try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-10,10),at = seq(-10,10,by=2.85),tria = "n", horiz = T,
             labs=c("<-10","-5","-2.5","-0.1","0.1","2.5","5",">10"),  cx=cxm,
             title=expression(paste("Bias"," [", kg," ",m^{-2},'',day^{-1},']')),titlag=lg1,lablag=lb1))
    
    #### bias reduction plot ####
    rm(cols)
    rbPal <- colorRampPalette(c('#00441b','#1b7837','#5aae61','#a6dba0','#d9f0d3','#f7f7f7','#e7d4e8','#c2a5cf','#9970ab','#762a83','#40004b')) 
    bbreaks<- c(-99999,-5,-2,-1,-0.5,-0.1,0.1,0.5,1,2,5,9999999)
    cbreaks <- c(0,1,2,3,4,5,6,7,99999900)
    cols<-rbPal(length(bbreaks)+1)
    shp_cropped <- crop(shp_us, raster::extent(shpfile))
    plot(shp_cropped,col='white',cex.main=0.7)
    
    prec_agg2$Col <- rbPal(length(bbreaks_re)-1)[as.numeric(cut(as.numeric(prec_agg2$chelsa_w5e5_bias_re),breaks = bbreaks))]
    plot(shp_cropped, border="dark grey", add=T)
    points(prec_agg2$long,prec_agg2$lat,pch = pch1,col = prec_agg2$Col,cex=0.2)
    
    try(cscl(cols,cords,zrng=c(-5,5),at = c(),tria = "b", horiz = T, lablag=1, cx=cx1))
    
    try(cscl(cols[3:(length(cols)-2)],cords,zrng=c(-5,5),at = seq(-5,5,by=1.11),tria = "b", horiz = T,
             labs=c("<-5","-2","-1","-0.5","-0.1","0.1","0.5","1","2",">5"),  cx=cxm,
             title=expression(paste("abs. bias high res. - abs. bias coarse res.", " [", kg," ",m^{-2},'',day^{-1},']')),titlag=lg1,lablag=lb1))
    
  
  # dev.off()

#### Plot Bias Reduction Mean - Percentile #### 

# pdf(paste0(".figures/pdfs/abs_bias_extremes"), height = 6, width = 12)
# par(mfrow=c(1,3), mar=c(1,0,1,0), oma=c(4,-1,1,-1))

#### bias plots ####
rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 

cx1 = 2
lg1 = -2
lb1 = 1
tg1 = 0.8
cxm = 1.45
pch1 <- 19
cex1 <- 0.25
cords <- c(-115,-75,7.5,10)

#### 95th Perc. Maximum Temp. ####
rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 

bbreaks_re <- c(-99999,-2,-1,-0.5,-0.1,0.1,0.5,1,2,99999)
cbreaks_re <-c(0,1,2,3,4,5,6,7,999999)
cols_re <- rbPal(length(bbreaks_re)+1)

shp_cropped <- crop(shp_us, raster::extent(shpfile))
plot(shp_cropped,col='white',cex.main=0.7)
text(-135,62, "d)", cex=2)
title(ylab="Bias Reduction Differences", cex.lab=2, line=0.5)
# title(ylab="Reduction Differences", cex.lab=2, line=-1.5)
title(main="Daily Maximum 2m Air-Temperature", line = -6, cex.main=2)
merged_tasmax$Col <- rbPal(length(bbreaks_re)-1)[as.numeric(cut(merged_tasmax$bias_reduction_m_p,breaks = bbreaks_re))]
plot(shp_cropped, border="dark grey", add=T)
points(merged_tasmax$long,merged_tasmax$lat,pch = pch1,col = merged_tasmax$Col, cex=abs(merged_tasmax$bias_reduction_m_p)*0.15)

try(cscl(cols_re,cords,zrng=c(-2,2),at = c(),tria = "b", horiz = T, lablag=1, cx=cx1))

try(cscl(cols_re[3:(length(cols_re)-2)],cords,zrng=c(-2,2),at = seq(-2,2,by=0.571),tria = "b", horiz = T,
         cx=cxm, labs=c("<-2","-1","-0.5","-0.1","0.1","0.5","1",">2"),
         title=expression(paste("bias red. mean - bias red. percent. [°C]" )),titlag=-2,lablag=1))

#### 5th Perc. Minimum Temp. ####
rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 
bbreaks<-c(-99999,-2,-1,-0.5,-0.1,0.1,0.5,1,2,99999)
cbreaks=c(0,1,2,3,4,5,6,7,99999900)

shp_cropped <- crop(shp_us, raster::extent(shpfile))
plot(shp_cropped,col='white',cex.main=0.7)

title(main="Daily Minimum 2m Air-Temperature", line = -6, cex.main=2)
merged_tasmin$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(merged_tasmin$bias_reduction_m_p,breaks = bbreaks))]
plot(shp_cropped, border="dark grey", add=T)
points(merged_tasmin$long,merged_tasmin$lat,pch = pch1,col = merged_tasmin$Col,cex=abs(merged_tasmin$bias_reduction_m_p)*0.15)

cols<-rev(rbPal(length(bbreaks)+1))
try(cscl(cols_re,cords,zrng=c(-2,2),at = c(),tria = "b", horiz = T, lablag=1, cx=cx1))

try(cscl(cols_re[3:(length(cols_re)-2)],cords,zrng=c(-2,2),at = seq(-2,2,by=0.571),tria = "b", horiz = T,
         labs=c("<-2","-1","-0.5","-0.1","0.1","0.5","1",">2"),  cx=cxm,
         title=expression(paste("bias red. mean - bias red. percent. [°C]")),titlag=lg1,lablag=lb1))

#### 95th Perc. Wet Days  ####
rbPal <- colorRampPalette(c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0','#f7f7f7','#fddbc7','#f4a582','#d6604d','#b2182b','#67001f')) 
bbreaks<-c(-99999,-6,-4,-2,-1,-0.5,-0.1,0.1,0.5,1,2,4,6,99999)
cbreaks=c(0,1,2,3,4,5,6,7,99999900)

shp_cropped <- crop(shp_us, raster::extent(shpfile))
plot(shp_cropped,col='white',cex.main=0.7)

title(main="Daily Precipitation of Wet Days", line = -6, cex.main=2)
merged_pr$Col <- rbPal(length(bbreaks)-1)[as.numeric(cut(merged_pr$bias_reduction_m_p,breaks = bbreaks))]
plot(shp_cropped, border="dark grey", add=T)
points(merged_pr$long,merged_pr$lat,pch = pch1,col = merged_pr$Col,cex=0.2)

cols<-rev(rbPal(length(bbreaks)+1))
try(cscl(cols_re,cords,zrng=c(-6,6),at = c(),tria = "b", horiz = T, lablag=1, cx=cx1))

try(cscl(cols_re[3:(length(cols_re)-2)],cords,zrng=c(-6,6),at = seq(-6,6,by=1.09),tria = "b", horiz = T,
         labs=c("<-6","-4","-2","-1","-0.5","-0.1","0.1","0.5","1","2","4",">6"),  cx=cxm,
         title=expression(paste("bias red. mean - bias red. percent.", " [", kg," ",m^{-2},'',day^{-1},']')),titlag=lg1,lablag=lb1))

dev.off()



