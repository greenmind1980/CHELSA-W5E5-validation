#install.packages("raster")
#install.packages("DBI")
#install.packages("sp")
library("sp")
library("raster")
library("rgdal")
library("dplyr")
library("ggplot2")
library(RPostgreSQL)
library("DBI")
library(ncdf4)
library("plotrix")
library("ACWD")
library("tidyr")
library(naniar)
library(sjmisc)
library(haven)

# #Longwave incoming radiation
# rsds_geba <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/GEBA/GEBAdata_2021-07-07_10-39-06.csv", sep=";", header = F)
# 
# df <- data.frame(rsds_geba)
# 
# #select only odd rows (even rows show QC code, odd include actual value)
# 
# df <- df %>% filter(row_number() %% 2==1)
# write.csv(df, file = "rsds_geba")
# df <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/GEBA/rsds_geba", sep=" ", header=F)
# df <- data.frame(df)
# dim(df)

#Direct solar radiation (sw)
rsds_geba_sw <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/GEBA/GEBAdata_2021-07-30_11-37-48.csv", sep=";", header = F)

df <- data.frame(rsds_geba_sw)

#select only odd rows (even rows show QC code, odd include actual value)

df <- df %>% filter(row_number() %% 2==1)
write.csv(df, file = "rsds_geba_sw")
df <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/GEBA/rsds_geba_sw", sep=" ", header=F)
df <- data.frame(df)
dim(df)


#seperate into columns 
df_sep <- tidyr::separate(df, col = 1, into = c("num","Station", "var", "year", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12","yearly"))
df <- df_sep[-1,]
df <- df[,-1]

#replace 99999 with NA

df <- df %>% set_na(na=99999)
df <- df[,-2]

df <- df[,-15]
df <- df[df$year !="1000",]

df[,1:length(df[1,])]<-as.numeric(as.matrix(df[,1:length(df[1,])]))
# divide df into seasonal components
df_season <- as.data.frame(df[,2])

df_season$DJF <- (df[,14]+df[,3]+df[,4])/3
df_season$MAM <- (df[,5]+df[,6]+df[,7])/3
df_season$JJA <- (df[,8]+df[,9]+df[,10])/3
df_season$SON <- (df[,11]+df[,12]+df[,13])/3
df_season$Station <- (df[,1])

# Station information ####
#Stations ####
rsds_stations <- read.csv("/storage/harichan/chelsa_V2/W5E5_validation/global/GEBA/stations.csv", header=T, sep=",")
rsds_stations <- as.data.frame(rsds_stations)
head(rsds_stations)

m1 <- merge(df_season,rsds_stations, by = "Station")
m1 <- as.data.frame(m1)

longitude <- m1$Longitude
latitude <- m1$Latitude
#transform to spatial object
m1 <- as.data.frame(m1)
coordinates(m1)<- ~Latitude + Longitude

#Winter ####
DJF <- cbind(tvalues=df_season[,2], Station=df_season[,6])
DJF_m <- merge(DJF, rsds_stations, by = "Station")
coordinates(DJF_m) <- ~Longitude + Latitude

DJF_m@proj4string@projargs<-"+proj=longlat +datum=WGS84 +no_defs"
shapefile(DJF_m, filename= paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/GEBA/Shapefiles/geba_1_rsds.shp'), overwrite=T)

#Spring ####
MAM <- cbind(tvalues=df_season[,3], Station=df_season[,6])
MAM_m <- merge(MAM, rsds_stations, by = "Station")
coordinates(MAM_m) <- ~Longitude + Latitude

MAM_m@proj4string@projargs<-"+proj=longlat +datum=WGS84 +no_defs"
shapefile(MAM_m, filename= paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/GEBA/Shapefiles/geba_2_rsds.shp'), overwrite=T)

#Summer ####
JJA <- cbind(tvalues=df_season[,4], Station=df_season[,6])
JJA_m <- merge(JJA, rsds_stations, by = "Station")
coordinates(JJA_m) <- ~Longitude + Latitude

JJA_m@proj4string@projargs<-"+proj=longlat +datum=WGS84 +no_defs"
shapefile(JJA_m, filename= paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/GEBA/Shapefiles/geba_3_rsds.shp'), overwrite=T)

#Fall ####
SON <- cbind(tvalues=df_season[,5], Station=df_season[,6])
SON_m <- merge(SON, rsds_stations, by = "Station")
coordinates(SON_m) <- ~Longitude + Latitude

SON_m@proj4string@projargs<-"+proj=longlat +datum=WGS84 +no_defs"
shapefile(SON_m, filename= paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/GEBA/Shapefiles/geba_4_rsds.shp'), overwrite=T)
