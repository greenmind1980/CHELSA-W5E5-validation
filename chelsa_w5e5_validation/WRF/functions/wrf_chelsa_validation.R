library("sp")
library("raster")
library("rgdal")
#library(RPostgreSQL)
library("DBI")
library(ncdf4)
library("units")

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
var_dir <- 'pr'
var <- 'pr'

# Get daily GHCN-D file, chelsa-w5e5, chelsa_v2 ####
shpname <- paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/ghcn_daily/',var,'/',var,'_ghcn_',day,'_',month,'_',year,'.shp')
print(shpname)
shp1 <- readOGR(shpname, stringsAsFactors = F)
chelsa_w5e5 <- raster(paste0('/storage/karger/chelsa_V2/W5E5/',var,'/CHELSA_W5E5_',var,'_',sprintf("%02d",day),'_',sprintf("%02d",month),'_',year,'_V.1.0.tif'))
#chelsa_v2 <- raster(paste0('/storage/karger/chelsa_V2/OUTPUT_DAILY/',var_dir,'/CHELSA_',var,'_',sprintf("%02d",day),'_',sprintf("%02d",month),'_',year,'_V.2.1.tif'))

m1v <- c(1,1,1,4,4,4,7,7,7,10,10,10)
m2v <- c(3,3,3,6,6,6,9,9,9,12,12,12)
m1 <- m1v[month]
m2 <- m2v[month]

# Built in quality check for chelsa_v2 precipitation data ####
if (var == 'pr')
{

  #####
  
  filename <- paste0('/storage/karger/WRF/',var,'/506303.PREC_ACC_NC.wrf2d_d01_CTRL_PREC_ACC_NC_',year,sprintf("%02d", m1),'-',year,sprintf("%02d", m2),'.nc')
    
  f1<-nc_open(filename)
  time<-ncvar_get(f1,"Time")
  tunits<-ncatt_get(f1,"Time",attname="units")
  tustr<-strsplit(tunits$value, " ")
  dates<-as.Date(time,format=c("%Y-%m-%d"), origin=unlist(tustr)[3])
  
  getNcTime <- function(nc, origin, ...) { ##NEW VERSION, with the units package
    require(units)
    require(ncdf4)
    options(warn=1) #show warnings by default
    if (is.character(nc)) nc <- nc_open(nc)
    ncdims <- names(nc$dim) #get netcdf dimensions
    timevar <- ncdims[which(ncdims %in% c("time", "Time", "datetime", "Datetime", "date", "Date"))] #find (first) time variable
    if (length(timevar) > 1) {
      warning(paste("Found more than one time var. Using the first:", timevar[1]))
      timevar <- timevar[1]
    }
    if (length(timevar)!=1) stop("ERROR! Could not identify the correct time variable")
    times <- ncvar_get(nc, timevar) #get time data
    timeatt <- ncatt_get(nc, timevar) #get attributes
    timeunit <- timeatt$units
    units(times) <- as_units(timeunit)
    as.POSIXct(times, origin=as.POSIXct(origin), format=c("%Y-%m-%d"))
  }
  
  time_v <- getNcTime(filename, unlist(tustr)[3])
  years <- as.numeric(format(time_v, format = "%Y"))
  months <- as.numeric(format(time_v, format = "%m"))
  days <- as.numeric(format(time_v, format = "%d"))
  selection <- which(years == year & months == month & days == day)

    st1<-stack()
  for (n in selection)
  {
    
    r <- raster(filename, band=n)
    st1<-stack(st1,r)
    
  }
  r1<-calc(st1, sum)
  
}



if (var == 'tas')
{
  filename <- paste0('/storage/karger/WRF/tas/505965.T2.wrf2d_d01_CTRL_T2_',year,sprintf("%02d", m1),'-',year,sprintf("%02d", m2),'.nc')
  
  
  f1<-nc_open(filename)
  time<-ncvar_get(f1,"Time")
  tunits<-ncatt_get(f1,"Time",attname="units")
  tustr<-strsplit(tunits$value, " ")
  dates<-as.Date(time,format=c("%Y-%m-%d"), origin=unlist(tustr)[3])
  
  getNcTime <- function(nc, origin, ...) { ##NEW VERSION, with the units package
    require(units)
    require(ncdf4)
    options(warn=1) #show warnings by default
    if (is.character(nc)) nc <- nc_open(nc)
    ncdims <- names(nc$dim) #get netcdf dimensions
    timevar <- ncdims[which(ncdims %in% c("time", "Time", "datetime", "Datetime", "date", "Date"))] #find (first) time variable
    if (length(timevar) > 1) {
      warning(paste("Found more than one time var. Using the first:", timevar[1]))
      timevar <- timevar[1]
    }
    if (length(timevar)!=1) stop("ERROR! Could not identify the correct time variable")
    times <- ncvar_get(nc, timevar) #get time data
    timeatt <- ncatt_get(nc, timevar) #get attributes
    timeunit <- timeatt$units
    units(times) <- as_units(timeunit)
    as.POSIXct(times, origin=as.POSIXct(origin), format=c("%Y-%m-%d"))
  }
  
  time_v <- getNcTime(filename, unlist(tustr)[3])
  years <- as.numeric(format(time_v, format = "%Y"))
  months <- as.numeric(format(time_v, format = "%m"))
  days <- as.numeric(format(time_v, format = "%d"))
  selection <- which(years == year & months == month & days == day)
  
  st1<-stack()
  for (n in selection)
  {
    
    r <- raster(filename, band=n)
    st1<-stack(st1,r)
    
  }
  r1<-calc(st1, mean)
  
}


# Extract data based on ghcn-grid and merge to one table ####
shp1$wrf<-raster::extract(r1,shp1)
shp1$chelsa_w5e5 <- raster::extract(chelsa_w5e5[[1]], shp1)
#shp1$chelsa_v2 <- raster::extract(chelsa_v2[[1]], shp1)

df1<-as.data.frame(shp1)
df1<-df1[complete.cases(df1),]

# df1$value <- df1$value/10
# df1$chelsa_w5e5 <- df1$chelsa_w5e5/10
# df1$chelsa_v2 <- df1$chelsa_v2/100

write.table(df1, paste0('/storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/',var,'_ghcn_chelsa-w5e5_chelsa-v2_wrf/',var,'_all_',year,'_',month,'_',day,'.txt'))
