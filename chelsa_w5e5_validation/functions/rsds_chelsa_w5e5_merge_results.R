library("sp")
library("raster")
library("rgdal")
library(RPostgreSQL)
library("DBI")
library(ncdf4)
# library(zoo)

# SPRING - AUTUMN ####
seasons <- c ('SON')
variables <- c("rsds")
year <- 1979:2016

for (n in 4)
{
  for (i in variables)
  {
    for (year in 1979:2016)
    {
      for (s in seasons)
      {
        
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year), readOGR(paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/rsds/chelsa_',n,'_',i,'_',year,'.shp')))
        #assign(get(paste0('chelsa_w5e5_',s,'_',i,'_',year))@proj4string@projasrunrgs, "+proj=longlat +datum=WGS84 +no_defs")
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_df'), as.data.frame(paste0('chelsa_w5e5_',s,'_',i,'_',year)))
        
        dx <- get(paste0('chelsa_w5e5_',s,'_',i,'_',year))
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year), dx)
        
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'), apply(as.data.frame(dx)[3:93], MARGIN =1, FUN=mean))
        dy <- get(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'))
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'), dy)
        dx$dy <- dy
        df <- as.data.frame(dx)
        dz <- df[-c(3:93)]
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_crop'), dz)
        
      }
    }
  }
  
}

#DJF mit Schaltjahr: 5:95, DJF ohne Schaltjahr: 5:94 
#MAM 5:96??
#JJA 5:96??
#SON 5:95??
 
datalist<-list()

for (i in 1:38)
{
  datalist[[i]] <- get(paste0('chelsa_w5e5_SON_rsds_',paste0(1978+i),'_crop'))
  print(datalist[[i]])
  
}

big_data = do.call(rbind, datalist)

write.table(big_data, file ="/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_4_rsds")
            
# WINTER ####
seasons <- c ('DJF')
variables <- c("rsds")
year <- 1980:2016

datalist <- list()

for (n in 1)
{
  for (i in variables)
  {
    for (year in 1980:2016)
    {
      for (s in seasons)
      {
        if((year %% 4) == 0) {
          if((year %% 100) == 0) {
            if((year %% 400) == 0) {
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year), readOGR(paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/rsds/chelsa_',n,'_',i,'_',year,'.shp')))
        #assign(get(paste0('chelsa_w5e5_',s,'_',i,'_',year))@proj4string@projasrunrgs, "+proj=longlat +datum=WGS84 +no_defs")
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_df'), as.data.frame(paste0('chelsa_w5e5_',s,'_',i,'_',year)))
        
        dx <- get(paste0('chelsa_w5e5_',s,'_',i,'_',year))
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year), dx)
        
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'), apply(as.data.frame(dx)[3:93], MARGIN =1, FUN=mean))
        dy <- get(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'))
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'), dy)
        dx$dy <- dy
        df <- as.data.frame(dx)
        dz <- df[-c(3:93)]
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_crop'), dz)
            } else {
              assign(paste0('chelsa_w5e5_',s,'_',i,'_',year), readOGR(paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/rsds/chelsa_',n,'_',i,'_',year,'.shp')))
              #assign(get(paste0('chelsa_w5e5_',s,'_',i,'_',year))@proj4string@projasrunrgs, "+proj=longlat +datum=WGS84 +no_defs")
              assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_df'), as.data.frame(paste0('chelsa_w5e5_',s,'_',i,'_',year)))
              
              dx <- get(paste0('chelsa_w5e5_',s,'_',i,'_',year))
              assign(paste0('chelsa_w5e5_',s,'_',i,'_',year), dx)
              
              assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'), apply(as.data.frame(dx)[3:92], MARGIN =1, FUN=mean))
              dy <- get(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'))
              assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'), dy)
              dx$dy <- dy
              df <- as.data.frame(dx)
              dz <- df[-c(3:92)]
              assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_crop'), dz)
            }
          } else {
            assign(paste0('chelsa_w5e5_',s,'_',i,'_',year), readOGR(paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/rsds/chelsa_',n,'_',i,'_',year,'.shp')))
            #assign(get(paste0('chelsa_w5e5_',s,'_',i,'_',year))@proj4string@projasrunrgs, "+proj=longlat +datum=WGS84 +no_defs")
            assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_df'), as.data.frame(paste0('chelsa_w5e5_',s,'_',i,'_',year)))
            
            dx <- get(paste0('chelsa_w5e5_',s,'_',i,'_',year))
            assign(paste0('chelsa_w5e5_',s,'_',i,'_',year), dx)
            
            assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'), apply(as.data.frame(dx)[3:93], MARGIN =1, FUN=mean))
            dy <- get(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'))
            assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'), dy)
            dx$dy <- dy
            df <- as.data.frame(dx)
            dz <- df[-c(3:93)]
            assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_crop'), dz)
          }
        } else {
          assign(paste0('chelsa_w5e5_',s,'_',i,'_',year), readOGR(paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/rsds/chelsa_',n,'_',i,'_',year,'.shp')))
          #assign(get(paste0('chelsa_w5e5_',s,'_',i,'_',year))@proj4string@projasrunrgs, "+proj=longlat +datum=WGS84 +no_defs")
          assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_df'), as.data.frame(paste0('chelsa_w5e5_',s,'_',i,'_',year)))
          
          dx <- get(paste0('chelsa_w5e5_',s,'_',i,'_',year))
          assign(paste0('chelsa_w5e5_',s,'_',i,'_',year), dx)
          
          assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'), apply(as.data.frame(dx)[3:92], MARGIN =1, FUN=mean))
          dy <- get(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'))
          assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'), dy)
          dx$dy <- dy
          df <- as.data.frame(dx)
          dz <- df[-c(3:92)]
          assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_crop'), dz)
            
        }
        datalist[[year]] <- dz
      }
      
    }
    
  }
}


big_data = do.call(rbind, datalist)


write.table(big_data, file ="/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_1_rsds")
