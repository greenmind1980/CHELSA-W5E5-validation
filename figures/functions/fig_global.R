
library(data.table)
library(gridExtra)
library(grid)
library(ggplot2)
library(viridis)
library(raster)
library(Metrics)
library(geosphere)
library(wsl.plot)
library(RColorBrewer)
var <- c("pr","tas", "tasmax", "tasmin", "rsds")
library(doParallel)
registerDoParallel(cl <- makeCluster(36))


for (i in unique(var))
{

#### Global ####

if (i != 'rsds')
{
y <- fread(paste0("/storage/harichan/chelsa_V2/W5E5_validation/global/chelsa_w5e5_all_",i,"_merged.txt"))
y <- y[complete.cases(y),]
}

if (i == 'rsds')
{
y <- fread('/home/admin1/rsds_geba_full_2.txt')
y$chelsa_w5e5 <- y$chelsa_w5e5/100
}
if (i == 'pr')
{
  colnames(y)<-c('id','ghcn','ghcn_id','chelsa_w5e5','chelsa_V2.1','long','lat','w5e5')
  y$ghcn <- as.numeric(y$ghcn)
  y$chelsa_V2.1 <- as.numeric(y$chelsa_V2.1)
  y$chelsa_w5e5 <- as.numeric(y$chelsa_w5e5)
  y$w5e5 <- as.numeric(y$w5e5)
  y <- y[,c(2,4:8)]
}

if (i == 'tas' || i == 'tasmax' ||i == 'tasmin')
{
  colnames(y)<-c('id','ghcn','ghcn_id','chelsa_w5e5','long','lat','w5e5')
  y$ghcn <- as.numeric(y$ghcn/10)
  y$chelsa_w5e5 <- as.numeric(y$chelsa_w5e5/10-273.15)
  y$w5e5 <- as.numeric(y$w5e5-273.15)
  y <- y[,c(2,4:7)]
}
if (i != 'rsds')
{
  agg1 <- aggregate(.~long+lat,y, mean)
}else{
  agg1 <- aggregate(.~station_id,y, mean)
}
if (i =='pr')
{
  Q <- quantile(agg1$ghcn, probs=c(.05, .95), na.rm = FALSE)
  iqr <- IQR(agg1$ghcn)
  eliminated<- subset(agg1, agg1$ghcn > (Q[1] - 1.5*iqr) & agg1$ghcn < (Q[2]+1.5*iqr))
  agg1 <- eliminated
}

aggt <- agg1
if (i == 'rsds')
{
colnames(aggt)[7]<-'ghcn'
}
if (i != 'rsds')
{
coordinates(aggt) <- ~long+lat
}else{
coordinates(aggt) <- ~lon+lat
}

aggt$id <- seq(1, length(aggt),1)

shapefile(aggt, file = paste0("~/",i,"_aggt.shp"), overwrite=T)
}









aggt$cor_chelsa <- rep(NA, length(aggt))
aggt$cor_w5e5 <- rep(NA, length(aggt))


rs <- foreach(n = 1:length(aggt[,1]), 
               .packages='geosphere') %dopar% {
    lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
    }
    aggt1 <- aggt
    aggt1$d <- distGeo(aggt[n,],aggt)
    aggt_sel <- aggt1[order(aggt1$d, decreasing=F)[1:8],]
    aggt_sel$bias_w5e5 <- aggt_sel$ghcn - aggt_sel$w5e5
    aggt_sel$bias_chelsa_w5e5 <- aggt_sel$ghcn - aggt_sel$chelsa_w5e5
    return(list(cor(aggt_sel$ghcn, aggt_sel$chelsa_w5e5), cor(aggt_sel$ghcn, aggt_sel$w5e5), lmp(lm(aggt_sel$ghcn ~ aggt_sel$chelsa_w5e5)) ,lmp(lm(aggt_sel$ghcn ~ aggt_sel$w5e5)),
    mean(aggt_sel$bias_w5e5), mean(aggt_sel$bias_chelsa_w5e5) )) 
}

rs2 <- data.frame(matrix(unlist(rs), nrow=length(rs), byrow=TRUE))
colnames(rs2) <- c("cor_chelsa", "cor_w5e5", "p_chelsa", "p_w5e5", "bias_w5e5", "bias_chelsa")
#rs2$dif <- abs(rs2$cor_chelsa) - abs(rs2$cor_w5e5)
rs2$dif <- abs(rs2$bias_chelsa) - abs(rs2$bias_w5e5)


rs2$lon <- coordinates(aggt)[,1]
rs2$lat <- coordinates(aggt)[,2]

coordinates(rs2) <- ~lon+lat

names(rs2) <- c("cor_chelsa", "cor_w5e5", "p_chelsa", "p_w5e5", "bias_w5e5", "bias_chelsa","dif")
shapefile(rs2, file = paste0("~/",i,"_loc_cor.shp"), overwrite=T)
boxplot(rs2$bias_chelsa, rs2$bias_w5e5)
}

stopCluster(cl)

#### Make the plots
ext <- extent(-180, 180, -90, 90)
gridsize <- 2
r_temp <- raster(ext, res=gridsize)
shp_world <- shapefile('/storage/harichan/chelsa_V2/natural_earth_vector/110m_cultural/ne_110m_admin_0_countries.shp')


pdf('~/fig_cor2.pdf', height=10, width=22)
par (mfrow = c(2,3), oma=c(2,2,1,1))

var <- c("pr","tas", "tasmax", "tasmin", "rsds")

for (i in unique(var)) {
  shp1 <- shapefile(paste0("~/",i,"_loc_cor.shp"))
  #shp1$dif_b <- shp1$bis_w55-shp1$bs_chls

  ## Rasterize the shapefile
  rr <- rasterize(shp1, r_temp, 'dif', fun=mean)
  values(rr) <- round(values(rr), 2)

  fun_count <- function(x){
    co <- NA
    if (length(x)>0){
      co <- length(x)
    }else{
      co <- 0
    }
    return(co)
  }

  fun_count <- function(x, ...) { if(all(is.na(x))) NA else length(x) }
  rr_count <- rasterize(shp1, r_temp, "dif", fun=fun_count, na.rm=T)
  #rr[values(rr_count)<gridsize*8] <- NA


  #### bias reduction plot ####
  if (i == 'tas') {
    main_t <- 'Mean Daily 2m Air-Temperature'
    }
  if (i == 'tasmax') {
    main_t <- 'Maximum Daily 2m Air-Temperature'
    }
  if (i == 'tasmin') {
    main_t <- 'Minimum Daily 2m Air-Temperature'
    } 
  if (i == 'pr') {
    main_t <- 'Mean Daily Precipitation'
    }
  if (i == 'rsds') {
    main_t <- 'Downwelling Shortwave Solar Radiation'
    }

  rbPal <- colorRampPalette(c('#00441b','#1b7837','#5aae61','#a6dba0','#d9f0d3','#f7f7f7','#e7d4e8','#c2a5cf','#9970ab','#762a83','#40004b')) 

  if (i == 'rsds') {
    bbreaks<- seq(-30, 30, 5)
    }
  #bbreaks<- c(-1,-0.2,-0.15,-0.1,-0.05,0, 0.05,0.1,0.15,0.2,1)


  cx1 = 2
  lg1 = 2
  lb1 = 3
  tg1 = 5
  cxm = 1.45
  pch1 <- 19
  cex1 <- 0.3
  cords <- c( -160, 160, -92,-100)

    cols <- brewer.pal(length(bbreaks), "PRGn")
    newcol <- colorRampPalette(cols)
    ncols <- length(bbreaks)+1
    cols <- rev(newcol(ncols))

    plot(rr, col =cols, breaks=bbreaks, legend=F, box=F, axes=F)
    title(main=main_t, line = 1, cex.main=2)

    plot(shp_world, border="dark grey", add=T)

    par(xpd=T)
    try(cscl(cols,cords,zrng=c(-0.2,0.2),at = c(),tria = "b", horiz = T, lablag=1, cx=cxm))
    labs <- bbreaks


    labs[1] <- "<= -30"
    labs[length(labs)]<- ">= 30"
    try(cscl(cols[2:((length(cols)-1))],cords,zrng=c(-30,30),at = bbreaks, labs=labs,tria = "n", horiz = T,
            cx=cxm, tickle=2,
            title=expression(paste("abs. bias high res. - abs. bias coarse res.")),titlag=tg1,lablag=lb1))
    par(xpd=NA)


  if (i == 'rsds') {  
    cols <- brewer.pal(length(bbreaks), "PRGn")
    newcol <- colorRampPalette(cols)
    ncols <- length(bbreaks)+1
    cols <- rev(newcol(ncols))

    plot(rr, col =cols, breaks=bbreaks, legend=F, box=F, axes=F)
    title(main=main_t, line = 1, cex.main=2)

    plot(shp_world, border="dark grey", add=T)

    par(xpd=T)
    try(cscl(cols,cords,zrng=c(-0.2,0.2),at = c(),tria = "b", horiz = T, lablag=1, cx=cxm))
    labs <- bbreaks


    labs[1] <- "<= -30"
    labs[length(labs)]<- ">= 30"
    try(cscl(cols[2:((length(cols)-1))],cords,zrng=c(-30,30),at = bbreaks, labs=labs,tria = "n", horiz = T,
            cx=cxm, tickle=2,
            title=expression(paste("abs. bias high res. - abs. bias coarse res.")),titlag=tg1,lablag=lb1))
    par(xpd=NA)
  }


#boxplot(shp1$cor_chelsa, shp1$cor_w5e5, ylab="Pearson r", names=c("CHELSA-W5E5", "W5E5"), ylim=c(-0.75, 1))
}
dev.off()


