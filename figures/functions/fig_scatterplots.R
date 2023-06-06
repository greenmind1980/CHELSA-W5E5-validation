################################################# Scatterplots ########################################################################################
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

library(data.table)
library(gridExtra)
library(grid)
library(ggplot2)
library(viridis)
library(raster)
library(Metrics)

var <- c("pr","tas", "tasmax", "tasmin", "rsds")
rsds <- fread('/home/admin1/rsds_geba_full_3.txt')
rsds <- rsds[complete.cases(rsds),]

for (i in unique(var))
{

#### North America ####
# y <- fread(paste0("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/w5e5_wrf_all_",i,".txt"))
# colnames(y)<-c('id','ghcn','ghcn_id','wrf','chelsa_w5e5','chelsa_V2.1','long','lat','year','month','day','w5e5')
# y_c <- y[,c(2,4:8,12)]
# y <- y_c[complete.cases(y_c),]

#### Global ####
if (i != 'rsds')
{
y <- fread(paste0("/storage/harichan/chelsa_V2/W5E5_validation/global/chelsa_w5e5_all_",i,"_merged.txt"))

y <- y[complete.cases(y),]
}

if (i == 'pr')
{
  colnames(y)<-c('id','ghcn','ghcn_id','chelsa_w5e5','chelsa_V2.1','long','lat','w5e5')
  y$ghcn <- as.numeric(y$ghcn)
  # y$wrf <- y$wrf
  y$chelsa_V2.1 <- as.numeric(y$chelsa_V2.1)
  y$chelsa_w5e5 <- as.numeric(y$chelsa_w5e5)
  y$w5e5 <- as.numeric(y$w5e5)
  y <- y[,c(2,4:8)]
}

if (i == 'tas' || i == 'tasmax' ||i == 'tasmin')
{
  colnames(y)<-c('id','ghcn','ghcn_id','chelsa_w5e5','long','lat','w5e5')
  y$ghcn <- as.numeric(y$ghcn/10)
  # y$wrf <- y$wrf-273.15
  #y$chelsa_V2.1 <- as.numeric(y$chelsa_V2.1/10-273.15)
  y$chelsa_w5e5 <- as.numeric(y$chelsa_w5e5/10-273.15)
  y$w5e5 <- as.numeric(y$w5e5-273.15)
  y <- y[,c(2,4:7)]
}

if (i == 'rsds')
{
  rsds$chelsa_w5e5 <- as.numeric(rsds$chelsa_w5e5/100)
  agg1 <- aggregate(.~lon+lat,rsds, mean)
}

if (i != 'rsds')
{
agg1 <- aggregate(.~long+lat,y, mean)
}

if (i =='pr')
{
  Q <- quantile(agg1$ghcn, probs=c(.05, .95), na.rm = FALSE)
  iqr <- IQR(agg1$ghcn)
  eliminated<- subset(agg1, agg1$ghcn > (Q[1] - 1.5*iqr) & agg1$ghcn < (Q[2]+1.5*iqr))
  agg1 <- eliminated
}

aggt <- agg1
if (i != 'rsds')
{
coordinates(aggt) <- ~long+lat
}else{
  coordinates(aggt) <- ~lon+lat
}
df3<-as.data.frame(agg1)
df2<-df3[sample(nrow(df3), 500000,replace = T), ]


n=1
if(i != 'rsds')
{
df2$density <- get_density(df2$ghcn, df2$w5e5, n = 1000)
}else{
 df2$density <- get_density(df2$geba, df2$w5e5, n = 1000) 
}
if (i == 'pr'){
  assign(paste0("p",n),  ggplot(df2) + geom_point(aes(ghcn, w5e5, color = density, alpha=1/10), size=0.01,show.legend = FALSE) + 
           scale_color_viridis() +
           xlim(0,10) +
           ylim(0,10) +
           xlab(expression(paste("GHCN" ," [", kg," ",m^{-2},'',day^{-1},']'))) +
           ylab(expression(paste("W5E5" ," [", kg," ",m^{-2},'',day^{-1},']'))) +
           ggtitle(paste0("Mean Daily Precipitation")) +
           geom_abline(intercept = 0, slope = 1))
}
if (i == 'tas'){
assign(paste0("t",n),  ggplot(df2) + geom_point(aes(ghcn, w5e5, color = density, alpha=1/10), size=0.01,show.legend = FALSE) + 
         scale_color_viridis() +
         xlim(-10,30) +
         ylim(-10,30) +
         xlab(expression(paste("GHCN [" * degree * C *"]"))) +
         ylab(expression(paste("W5E5 [" * degree * C *"]"))) +
         ggtitle(paste0("Mean Daily 2m Air-Temperature")) +
         geom_abline(intercept = 0, slope = 1))
}
if (i == 'tasmax'){
assign(paste0("tx",n),  ggplot(df2) + geom_point(aes(ghcn, w5e5, color = density, alpha=1/10), size=0.01,show.legend = FALSE) + 
         scale_color_viridis() +
         xlim(-10,30) +
         ylim(-10,30) +
         xlab(expression(paste("GHCN [" * degree * C *"]"))) +
         ylab(expression(paste("W5E5 [" * degree * C *"]"))) +
         ggtitle(paste0("Maximum Daily 2m Air-Temperature")) +
         geom_abline(intercept = 0, slope = 1))
}
if (i == 'tasmin'){
assign(paste0("tm",n),  ggplot(df2) + geom_point(aes(ghcn, w5e5, color = density, alpha=1/10), size=0.01,show.legend = FALSE) + 
         scale_color_viridis() +
         xlim(-10,30) +
         ylim(-10,30) +
         xlab(expression(paste("GHCN [" * degree * C *"]"))) +
         ylab(expression(paste("W5E5 [" * degree * C *"]"))) +
         ggtitle(paste0("Minimum Daily 2m Air-Temperature")) +
         geom_abline(intercept = 0, slope = 1))
}
if (i == 'rsds'){
assign(paste0("rs",n),  ggplot(df2) + geom_point(aes(geba, w5e5, color = density, alpha=1/10), size=0.01,show.legend = FALSE) + 
         scale_color_viridis() +
         xlim(0,400) +
         ylim(0,400) +
         xlab(expression(paste("GEBA [", W," ",m^{-2},"]"))) +
         ylab(expression(paste("W5E5 [", W," ",m^{-2},"]"))) +
         ggtitle(paste0("Downwelling shortwave solar radiation")) +
         geom_abline(intercept = 0, slope = 1))
}

n=2
if(i != 'rsds')
{
df2$density <- get_density(df2$ghcn, df2$chelsa_w5e5, n = 1000)
}else{
 df2$density <- get_density(df2$geba, df2$chelsa_w5e5, n = 1000) 
}
if (i == 'pr'){
  assign(paste0("p",n),  ggplot(df2) + geom_point(aes(ghcn, chelsa_w5e5, color = density, alpha=1/10), size=0.01,show.legend = FALSE) + 
           scale_color_viridis() +
           xlim(0,10) +
           ylim(0,10) +
           xlab(expression(paste("GHCN" ," [", kg," ",m^{-2},'',day^{-1},']'))) +
           ylab(expression(paste("CHELSA-W5E5" ," [", kg," ",m^{-2},'',day^{-1},']'))) +
           ggtitle(paste0("Mean Daily Precipitation")) +
           geom_abline(intercept = 0, slope = 1))
}
if (i == 'tas'){
   assign(paste0("t",n),  ggplot(df2) + geom_point(aes(ghcn, chelsa_w5e5, color = density, alpha=1/10), size=0.01,show.legend = FALSE) + 
           scale_color_viridis() +
           xlim(-10,30) +
           ylim(-10,30) +
           xlab(expression(paste("GHCN [" * degree * C *"]"))) +
           ylab(expression(paste("CHELSA-W5E5 [" * degree * C *"]"))) +
           ggtitle(paste0("Mean Daily 2m Air-Temperature")) +
           geom_abline(intercept = 0, slope = 1))
}
if (i == 'tasmax'){
   assign(paste0("tx",n),  ggplot(df2) + geom_point(aes(ghcn, chelsa_w5e5, color = density, alpha=1/10), size=0.01,show.legend = FALSE) + 
           scale_color_viridis() +
           xlim(-10,30) +
           ylim(-10,30) +
           xlab(expression(paste("GHCN [" * degree * C *"]"))) +
           ylab(expression(paste("CHELSA-W5E5 [" * degree * C *"]"))) +
           ggtitle(paste0("Maximum Daily 2m Air-Temperature")) +
           geom_abline(intercept = 0, slope = 1))
}
if (i == 'tasmin'){
   assign(paste0("tm",n),  ggplot(df2) + geom_point(aes(ghcn, chelsa_w5e5, color = density, alpha=1/10), size=0.01,show.legend = FALSE) + 
           scale_color_viridis() +
           xlim(-10,30) +
           ylim(-10,30) +
           xlab(expression(paste("GHCN [" * degree * C *"]"))) +
           ylab(expression(paste("CHELSA-W5E5 [" * degree * C *"]"))) +
           ggtitle(paste0("Minimum Daily 2m Air-Temperature")) +
           geom_abline(intercept = 0, slope = 1))
}

if (i == 'rsds'){
assign(paste0("rs",n),  ggplot(df2) + geom_point(aes(geba, chelsa_w5e5, color = density, alpha=1/10), size=0.01,show.legend = FALSE) + 
         scale_color_viridis() +
         xlim(0,400) +
         ylim(0,400) +
         xlab(expression(paste("GEBA [", W," ",m^{-2},"]"))) +
         ylab(expression(paste("CHELSA-W5E5 [", W," ",m^{-2},"]"))) +
         ggtitle(paste0("Downwelling shortwave solar radiation")) +
         geom_abline(intercept = 0, slope = 1))
}

}

# pdf(paste0("./figures/pdfs/NA_scatterplot_",i), height = 4, width = 8)
pdf(paste0("~/global_scatterplot.pdf"), height = 20, width = 8)
grid.arrange(
  p1,
  p2,
  t1,
  t2,
  tx1,
  tx2,
  tm1,
  tm2,
  rs1,
  rs2,
  nrow = 5,
  ncol = 2,
  top = "",
  bottom = textGrob(
    "",
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1
  )
)
dev.off()

