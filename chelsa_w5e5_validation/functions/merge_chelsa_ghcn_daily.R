library("sp")
library("raster")
library("rgdal")
library("DBI")
library(ncdf4)

##################################################################################################
### Instructions ###
# The script ist intended to be run on the cluster with a slurm script and therefore,
# does not contain a loop over the variables.  

##################################################################################################


# Set arguments #### 
args <- commandArgs(trailingOnly=TRUE)

contents <- strsplit(args, " ")

day    <-contents[[1]]
month  <-contents[[2]]
year   <-contents[[3]]
day <-suppressWarnings(as.numeric(as.character(day)))
month <-suppressWarnings(as.numeric(as.character(month)))
year <-suppressWarnings(as.numeric(as.character(year)))

# Set variable ####
#var_dir <- c('tas','preccor','tasmax','tasmin')
#var <- c('tas','pr', 'tasmax','tasmin')
#var_dir <- 'tasmin'
var <- 'tasmax'

# Get daily GHCN-D file, chelsa-w5e5, chelsa_v2 ####
shpname <- paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/ghcn_daily/',var,'/',var,'_ghcn_',day,'_',month,'_',year,'.shp')
print(shpname)
shp1 <- readOGR(shpname, stringsAsFactors = F)
chelsa_w5e5 <- raster(paste0('/storage/karger/chelsa_V2/W5E5/',var,'/CHELSA_W5E5_',var,'_',sprintf("%02d",day),'_',sprintf("%02d",month),'_',year,'_V.1.0.tif'))
#chelsa_v2 <- raster(paste0('/storage/karger/chelsa_V2/OUTPUT_DAILY/',var_dir,'/CHELSA_',var,'_',sprintf("%02d",day),'_',sprintf("%02d",month),'_',year,'_V.2.1.tif'))


# Extract data based on ghcn-grid and merge to one table ####

shp1$chelsa_w5e5 <- raster::extract(chelsa_w5e5[[1]], shp1)
#shp1$chelsa_v2 <- raster::extract(chelsa_v2[[1]], shp1)

df1<-as.data.frame(shp1)
df1<-df1[complete.cases(df1),]

write.table(df1, paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/chelsa_w5e5/',var,'/complete_',var,'_all_',year,'_',month,'_',day,'.txt'))
