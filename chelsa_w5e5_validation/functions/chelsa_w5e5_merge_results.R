library("rgdal")



########################################
# Temperature Variables ####
seasons <- c('MAM')
temp_variables <- c("tas","tasmin","tasmax")
datalist <- list()
for (n in 2)
{
  for (i in temp_variables)
  {
    for (year in 1979:2016)
    {
      
      #SCHALTJAHR ODER NICHT?
      for (s in seasons)
      {
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year), readOGR(paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/',i,'/chelsa_w5e5_',n,'_',i,'_',year,'.shp')))
        #assign(get(paste0('chelsa_w5e5_',s,'_',i,'_',year))@proj4string@projasrunrgs, "+proj=longlat +datum=WGS84 +no_defs")
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_df'), as.data.frame(paste0('chelsa_w5e5_',s,'_',i,'_',year)))
        
        dx <- get(paste0('chelsa_w5e5_',s,'_',i,'_',year))
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year), dx)
        
        #Länge abhängig von Schaltjahr (Anzahl Tage!)
        #DJF mit Schaltjahr: 5:95, DJF ohne Schaltjahr: 5:94 
        #MAM 5:96??
        #JJA 5:96??
        #SON 5:95??
        
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'), apply(as.data.frame(dx)[5:96], MARGIN =1, FUN=mean))
        dy <- get(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'))
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new'), dy)
        dx$dy <- dy
        df <- as.data.frame(dx)
        # names(df)[names(df) =="dy"]<- paste0('chelsa_w5e5_',s,'_',i,'_',year,'_new')
        dz <- df[-c(5:96)]
        dz <- dz[-c(2)]
        assign(paste0('chelsa_w5e5_',s,'_',i,'_',year,'_crop'), dz)
    
      }
      datalist[[year]] <- dz
    }

  

big_data = do.call(rbind, datalist)

write.table(big_data, file =paste0("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_",n,"_",i))

# taylor.diagram(big_data$tvalue, big_data$dy, normalize=T, col="blue", main = "Mean Temperature")

  }
}
