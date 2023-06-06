library("sp")
library("raster")
library("rgdal")
library(RPostgreSQL)
library("DBI")
library(ncdf4)
library(units)
library(data.table)

av.index <- function(obs, mod_coarse, mod_fine, binsize, ...){
  obs[obs<=0] <- 0
  obs[is.na(obs)] <- 0
  vbin <- seq(0, max(obs, mod_coarse), binsize)
  obs <- as.data.frame(obs)
  df <- data.frame(obs, stringsAsFactors = F)
  vbindf <- as.data.frame(seq(1,length(vbin)))
  vbindf$bins <- cut(as.data.frame(vbin)$vbin, breaks=vbin, right=FALSE)
  colnames(vbindf) <- c('id', 'bins')
  df$bins <- cut(df$obs, breaks=vbin, right=FALSE)
  obsagg <- aggregate(df$obs, list(df$bins), FUN=function(x){length(x)})
  colnames(obsagg) <- c('bins','obs_count')
  mdf <- merge(vbindf, obsagg, by='bins', all=T)
  df <- data.frame(mod_coarse, stringsAsFactors = F)
  vbindf <- as.data.frame(seq(1,length(vbin)))
  vbindf$bins <- cut(as.data.frame(vbin)$vbin, breaks=vbin, right=FALSE)
  colnames(vbindf) <- c('id', 'bins')
  df$bins <- cut(df$mod_coarse, breaks=vbin, right=FALSE)
  mods_coarseagg <- aggregate(df$mod_coarse, list(df$bins), FUN=function(x){length(x)})
  colnames(mods_coarseagg) <- c('bins','mod_coarse_count')
  mdf <- merge(mdf, mods_coarseagg, by='bins', all=T)
  mdf <- mdf[!is.na(mdf$bins),]
  mdf[is.na(mdf)] <- 0
  mdf$A <- abs((mdf$mod_coarse_count-mdf$obs_count)*binsize)
  mdf$D <- mdf$obs_count*binsize
  Dcoarse = sum(mdf$A, na.rm=T)/sum(mdf$D, na.rm=T)
  
  df <- data.frame(mod_fine, stringsAsFactors = F)
  vbindf <- as.data.frame(seq(1,length(vbin)))
  vbindf$bins <- cut(as.data.frame(vbin)$vbin, breaks=vbin, right=FALSE)
  colnames(vbindf) <- c('id', 'bins')
  df$bins <- cut(df$mod_fine, breaks=vbin, right=FALSE)
  mods_fineagg <- aggregate(df$mod_fine, list(df$bins), FUN=function(x){length(x)})
  colnames(mods_fineagg) <- c('bins','mod_fine_count')
  mdf <- merge(mdf, mods_fineagg, by='bins', all=T)
  mdf <- mdf[!is.na(mdf$bins),]
  mdf[is.na(mdf)] <- 0
  mdf$A <- abs((mdf$mod_fine_count-mdf$obs_count)*binsize)
  mdf$D <- mdf$obs_count*binsize
  Dfine = sum(mdf$A, na.rm=T)/sum(mdf$D, na.rm=T)
  return(Dcoarse - Dfine)
}

args <- commandArgs(trailingOnly = TRUE)
args_contents <- strsplit(args, ' ')

# Get first argument
i <- as.numeric(args_contents[[1]])

for var in c("pr", "tas"){
# function to calculate added value index
y <- fread(paste0("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/w5e5_wrf_all_",var,".txt"))
colnames(y)<-c('id','ghcn','ghcn_id','wrf','chelsa_w5e5','chelsa_V2.1','long','lat','year','month','day','w5e5')
print(head(y))

if (var == 'pr'){
    obs <- (y[y$ghcn_id==unique(y$ghcn_id)[i],]$ghcn)/10
    mod_coarse <- (y[y$ghcn_id==unique(y$ghcn_id)[i],]$w5e5)*60*60*24
    mod_fine <- (y[y$ghcn_id==unique(y$ghcn_id)[i],]$chelsa_w5e5)/10
    ai1 <- NA
    ai1 <- try(av.index(obs, mod_coarse, mod_fine, 0.05),TRUE)
    lat1 <- unique(y[y$ghcn_id==unique(y$ghcn_id)[i],]$lat)
    lon1 <- unique(y[y$ghcn_id==unique(y$ghcn_id)[i],]$long)
    n <- length(obs)
}

if (var == 'tas'){
    obs <- (y[y$ghcn_id==unique(y$ghcn_id)[i],]$ghcn)/10
    mod_coarse <- (y[y$ghcn_id==unique(y$ghcn_id)[i],]$w5e5)-273.15
    mod_fine <- (y[y$ghcn_id==unique(y$ghcn_id)[i],]$chelsa_w5e5/10)-273.15
    ai1 <- NA
    ai1 <- try(av.index(obs, mod_coarse, mod_fine, x),TRUE)
    lat1 <- unique(y[y$ghcn_id==unique(y$ghcn_id)[i],]$lat)
    lon1 <- unique(y[y$ghcn_id==unique(y$ghcn_id)[i],]$long)
    n <- length(obs)
}

print(Sys.time())

#x <- seq(0.001,1,0.01)

#plot(0, xlim=c(0,max(x)), ylim=c(-1,1))

#aii_v <- c()
#n_v <- c()
#for (n in x)
#{
#aii <- av.index(obs, mod_coarse, mod_fine, n)
#points(n, aii, col="red")
#n_v <- c(n_v, n)
#aii_v <- c(aii_v, aii)
#}

#lenght(unique(y$ghcn_id)) #38071

res <- as.data.frame(cbind(ai1, lat1, lon1, n, unique(y$ghcn_id)[i]))

write.table(res, paste0("/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF
/av_index/binsize_0.05/",var,"/res_av_",var,"_",i,"_bs_005.txt"))

}
