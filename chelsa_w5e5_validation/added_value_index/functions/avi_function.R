# function to calculate added value index
# obs = vector of observations
# mod_coarse = vector of model at coarse resolution
# mod_fine = vector of model at fine resolution
# binsize = size of the bin
av.index <- function(obs, mod_coarse, mod_fine, binsize, ...){
  obs[obs<=0] <- 0
  obs[is.na(obs)] <- 0
  vbin <- seq(0, max(c(obs, mod)), binsize)
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
  Dcoarse = sum(mdf$A)/sum(mdf$D)
  
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
  Dfine = sum(mdf$A)/sum(mdf$D)
  return(Dcoarse - Dfine)
}
# example data
obs <- as.vector(rpois(1000, lambda=5))
mod_coarse <- as.vector(rpois(1000, lambda=1))
mod_fine <- as.vector(rpois(1000, lambda=0.5))
binsize = 1
# test function
av.index(obs, mod_coarse, mod_fine, 1)
