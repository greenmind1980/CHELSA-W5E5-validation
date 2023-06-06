library("sp")
library("raster")
library("rgdal")
library("DBI")
library(ncdf4)
library("units")

##################################################################################################
### Instructions ###
# The script ist intended to be run on the cluster with a slurm script and therefore,
# does not contain a loop over the variables.
# The unit conversion in this script is only needed for the variable precipitation 
# as otherwise the values could not be extracted as integers. For temperature, the values 
# can also be written out as.numeric as the generated file is much smaller.

##################################################################################################


# Set arguments #### 
args <- commandArgs(trailingOnly=TRUE)

contents <- strsplit(args, " ")

day    <-contents[[3]]
month  <-contents[[2]]
year   <-contents[[1]]
day <-suppressWarnings(as.numeric(as.character(day)))
month <-suppressWarnings(as.numeric(as.character(month)))
year <-suppressWarnings(as.numeric(as.character(year)))


  # W5E5 ###########################################################################################################################
    filename <- paste0('/storage/karger/W5E5/tasmax_W5E5v1.0_20110101-20161231.nc')
  
  f1<-nc_open(filename)
  time<-ncvar_get(f1,"time")
  tunits<-ncatt_get(f1,"time",attname="units")
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
  
  st2<-stack()
  for (n in selection)
  {
    
    r2 <- raster(filename, band=n)
    st2<-stack(st2,r2)
    
  }
  rs2<-calc(st2, sum)

# Put together ####################################################################################################################
shp1 <- read.table(paste0("/storage/harichan/chelsa_V2/W5E5_validation/global/chelsa_w5e5/tasmax/complete_tasmax_all_",year,"_",month,"_",day,".txt")) 
coordinates(shp1) <- ~ coords.x1 + coords.x2

shp1$w5e5 <- raster::extract(rs2, shp1)

#df1$value <- df1$value/10
#df1$chelsa_w5e5 <- df1$chelsa_w5e5/10
#df1$chelsa_v2 <- df1$chelsa_v2/100
#df1$w5e5 <- df1$w5e5*60*60*24

df1<-as.data.frame(shp1)
df1<-df1[complete.cases(df1),]
df1$value <- as.integer(df1$value)
df1$chelsa_w5e5 <- as.integer(df1$chelsa_w5e5)
#df1$chelsa_v2 <- as.integer(df1$chelsa_v2)
df1$w5e5 <- as.integer(df1$w5e5)



write.table(df1, paste0("/storage/harichan/chelsa_V2/W5E5_validation/global/chelsa_w5e5/merged_tasmax/complete_chelsa_w5e5_tasmax_",year,month,day,".txt"))


