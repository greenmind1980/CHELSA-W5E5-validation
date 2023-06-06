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
library(RColorBrewer)

pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user="****", password="****" ,host="postgres.wsl.ch", port=5432, dbname="ghcn")

pdf("~/all_chelsa_w5e5_tp_2.pdf", height=15*0.6, width = 10*0.6)
par(mfrow=c(3,2))
# TAS ####

seasons_v <- c("(12, 1, 2)", "(3, 4, 5)","(6, 7, 8)","(9, 10, 11)")
for (n in 1:4)
{
  SQL_Statement <-paste0("SELECT count(*) AS n, avg(CAST(d.value AS numeric))/10 AS tvalue, s.latitude, s.longitude, d.year
                         FROM ghcnd.daydat d, ghcnd.stations s
                         WHERE d.id = s.id
                         AND d.element = \'TAVG\'
                         AND d.year < 2017
                         AND d.year > 1978
                         AND d.month IN ",seasons_v[n]," 
                         AND d.value IS NOT NULL
                         AND d.qflag = ''
                         GROUP BY s.id, d.element, d.year, d.month
                         HAVING count(*) > 25
                         ORDER BY s.latitude, s.longitude")
  
  ghcn<-dbGetQuery(con,SQL_Statement)
  

  SQL_Statement <-paste0("SELECT count(*) AS n, avg(CAST(d.value AS numeric))/10 AS tvalue, s.latitude, s.longitude, d.year, d.month
                         FROM ghcnd.daydat d, ghcnd.stations s
                         WHERE d.id = s.id
                         AND d.element = \'TAVG\'
                         AND d.year < 2017
                         AND d.year > 1978
                         AND d.value IS NOT NULL
                         AND d.qflag = ''
                         GROUP BY s.id, d.element, d.year, d.month
                         HAVING count(*) > 25
                         ORDER BY s.latitude, s.longitude")
  
  ghcn<-dbGetQuery(con,SQL_Statement)


  
  assign(paste0('ghcn_',n),ghcn)
}

#GHCN 1 - WINTER
ghcn_1 <- ghcn[ghcn$month %in% c(12,1,2), ]
ghcn_1 <- aggregate(.~longitude+latitude,ghcn_1, mean)
tas_W5E5 <- stack("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/W5E5_validation_tas_W5E5.nc")
coordinates(ghcn_1) <- ~longitude+latitude
ghcn_1$w5e5 <- raster::extract(tas_W5E5[[1]], ghcn_1)
ghcn_1$w5e5 <- ghcn_1$w5e5-273.15
ghcn_1a <- as.data.frame(ghcn_1)

#ghcn_1a<-aggregate(.~longitude+latitude, ghcn_1, mean)

cw_tas_1 <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_1_tas")
cw_tas_1$dy <- cw_tas_1$dy/10-273.15
cw_tas_1a <- aggregate(.~coords.x1+ coords.x2 ,cw_tas_1, mean)

taylor.diagram(ghcn_1a$tvalue, ghcn_1a$w5e5, normalize = T, col="#7570b3", main = "Mean Daily 2m Air-Temperature", pch=16, add=F, pcex = 1.2)
text(1.1,0.6,"W5E5", cex=1.2, col="#7570b3")
text(1.1,0.6,"W5E5", cex=1.2, col="#7570b3")
taylor.diagram(cw_tas_1a$tvalue, cw_tas_1a$dy, normalize = T, add=T, col="#d95f02", pch = 16, pcex=1.2)
text(0.6,0.35,"CHELSA_W5E5", cex=1.2, col="#d95f02")
text(0.6,0.35,"CHELSA_W5E5", cex=1.2, col="#d95f02")

# GHCN 2 - SPRING
ghcn_2 <- ghcn[ghcn$month %in% c(3,4,5), ]
ghcn_2 <- aggregate(.~longitude+latitude,ghcn_2, mean)
coordinates(ghcn_2) <-~longitude+latitude
ghcn_2$w5e5 <- raster::extract(tas_W5E5[[3]], ghcn_2)
ghcn_2$w5e5 <- ghcn_2$w5e5-273.15
ghcn_2a <- as.data.frame(ghcn_2)
#ghcn_2a <- aggregate(.~longitude+latitude, ghcn_2, mean)

cw_tas_2<- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_2_tas")
cw_tas_2$dy <- cw_tas_2$dy/10-273.15
cw_tas_2a <- aggregate(.~coords.x1+ coords.x2 ,cw_tas_2, mean)
taylor.diagram(ghcn_2a$tvalue, ghcn_2a$w5e5, normalize = T, add=T, col="#7570b3", pch=17, pcex=1.2)
taylor.diagram(cw_tas_2a$tvalue, cw_tas_2a$dy, normalize = T, add=T, col="#d95f02", pch=17, pcex=1.2)


#GHCN 3 - SUMMER
ghcn_3 <- ghcn[ghcn$month %in% c(6,7,8), ]
ghcn_3 <- aggregate(.~longitude+latitude,ghcn_3, mean)
coordinates(ghcn_3) <-~longitude+latitude
ghcn_3$w5e5 <- raster::extract(tas_W5E5[[2]], ghcn_3)
ghcn_3$w5e5 <- ghcn_3$w5e5-273.15
ghcn_3a <- as.data.frame(ghcn_3)
#ghcn_3a <- aggregate(.~longitude+latitude, ghcn_3, mean)

cw_tas_3<- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_3_tas")
cw_tas_3$dy <- cw_tas_3$dy/10-273.15
cw_tas_3a <- aggregate(.~coords.x1+ coords.x2 ,cw_tas_3, mean)
taylor.diagram(ghcn_3a$tvalue, ghcn_3a$w5e5,normalize = T, add = TRUE, col="#7570b3", pch=18, pcex=1.2)
taylor.diagram(cw_tas_3a$tvalue, cw_tas_3a$dy,normalize = T, add = TRUE, col="#d95f02",pch=18, pcex=1.2)

#GHCN 4 - FALL
ghcn_4 <- ghcn[ghcn$month %in% c(9,10,11), ]
ghcn_4 <- aggregate(.~longitude+latitude,ghcn_4, mean)
coordinates(ghcn_4) <-~longitude+latitude
ghcn_4$w5e5 <- raster::extract(tas_W5E5[[4]], ghcn_4)
ghcn_4$w5e5 <- ghcn_4$w5e5-273.15
ghcn_4a <- as.data.frame(ghcn_4)
#ghcn_4a <- aggregate(.~longitude+latitude, ghcn_4, mean)

cw_tas_4<- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_4_tas")
cw_tas_4$dy <- cw_tas_4$dy/10-273.15
cw_tas_4a <- aggregate(.~coords.x1+ coords.x2 ,cw_tas_4, mean)
taylor.diagram(ghcn_4a$tvalue, ghcn_4a$w5e5, normalize = T, add = T, col="#7570b3", pch=15, pcex=1.2)
taylor.diagram(cw_tas_4a$tvalue, cw_tas_4a$dy, normalize = T, add = T, col="#d95f02", pch=15, pcex=1.2)


# TASMIN ####

seasons_v <- c("(12, 1, 2)", "(3, 4, 5)","(6, 7, 8)","(9, 10, 11)")

for (n in 1:4)
{
  SQL_Statement <-paste0("SELECT count(*) AS n, avg(CAST(d.value AS numeric))/10 AS tvalue, s.latitude, s.longitude, d.year
                         FROM ghcnd.daydat d, ghcnd.stations s
                         WHERE d.id = s.id
                         AND d.element = \'TMIN\'
                         AND d.year < 2017
                         AND d.year > 1978
                         AND d.month IN ",seasons_v[n]," 
                         AND d.value IS NOT NULL
                         AND d.qflag = ''
                         GROUP BY s.id, d.element, d.year, d.month
                         HAVING count(*) > 25
                         ORDER BY s.latitude, s.longitude")
  
  ghcn<-dbGetQuery(con,SQL_Statement)
  
  assign(paste0('ghcn_',n),ghcn)
}

# pdf("./figures/pdfs/tasmin_chelsa_w5e5_tp.pdf", height=5, width = 5)
#GHCN 1 - WINTER
tasmin_W5E5 <- stack("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/W5E5_validation_tasmin_W5E5.nc")
coordinates(ghcn_1)<-~longitude+latitude
ghcn_1$w5e5 <- raster::extract(tasmin_W5E5[[1]], ghcn_1)
ghcn_1$w5e5<-ghcn_1$w5e5-273.15
ghcn_1<-as.data.frame(ghcn_1)
ghcn_1a<-aggregate(.~longitude+latitude, ghcn_1, mean)

cw_tasmin_1 <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_1_tasmin")
cw_tasmin_1$dy <- cw_tasmin_1$dy/10-273.15
cw_tasmin_1a <- aggregate(.~coords.x1+ coords.x2 ,cw_tasmin_1, mean)
taylor.diagram(ghcn_1a$tvalue, ghcn_1a$w5e5, normalize = T, col="#7570b3", main = "Minimum Daily 2m Air-Temperature", pch=16, add=F, pcex=1.2)
text(1.2,0.8,"W5E5", cex=1.2, col="#7570b3")
text(1.2,0.8,"W5E5", cex=1.2, col="#7570b3")
taylor.diagram(cw_tasmin_1a$tvalue, cw_tasmin_1a$dy, normalize = T, add=T, col="#d95f02", pch = 16, pcex=1.2)
text(0.68,0.6,"CHELSA_W5E5", cex=1.2, col="#d95f02")
text(0.68,0.6,"CHELSA_W5E5", cex=1.2, col="#d95f02")

# GHCN 2 - SPRING
coordinates(ghcn_2) <-~longitude+latitude
ghcn_2$w5e5 <- raster::extract(tasmin_W5E5[[3]], ghcn_2)
ghcn_2$w5e5 <- ghcn_2$w5e5-273.15
ghcn_2 <- as.data.frame(ghcn_2)
ghcn_2a <- aggregate(.~longitude+latitude, ghcn_2, mean)

cw_tasmin_2<- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_2_tasmin")
cw_tasmin_2$dy <- cw_tasmin_2$dy/10-273.15
cw_tasmin_2a <- aggregate(.~coords.x1+ coords.x2 ,cw_tasmin_2, mean)
taylor.diagram(ghcn_2a$tvalue, ghcn_2a$w5e5, normalize = T, add=T, col="#7570b3", pch=17, pcex=1.2)
taylor.diagram(cw_tasmin_2a$tvalue, cw_tasmin_2a$dy, normalize = T, add=T, col="#d95f02", pch=17, pcex=1.2)


#GHCN 3 - SUMMER
coordinates(ghcn_3) <-~longitude+latitude
ghcn_3$w5e5 <- raster::extract(tasmin_W5E5[[2]], ghcn_3)
ghcn_3$w5e5 <- ghcn_3$w5e5-273.15
ghcn_3 <- as.data.frame(ghcn_3)
ghcn_3a <- aggregate(.~longitude+latitude, ghcn_3, mean)

cw_tasmin_3<- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_3_tasmin")
cw_tasmin_3$dy <- cw_tasmin_3$dy/10-273.15
cw_tasmin_3a <- aggregate(.~coords.x1+ coords.x2 ,cw_tasmin_3, mean)
taylor.diagram(ghcn_3a$tvalue, ghcn_3a$w5e5,normalize = T, add = TRUE, col="#7570b3", pch=18, pcex=1.2)
taylor.diagram(cw_tasmin_3a$tvalue, cw_tasmin_3a$dy,normalize = T, add = TRUE, col="#d95f02",pch=18, pcex=1.2)

#GHCN 4 - FALL 
coordinates(ghcn_4) <-~longitude+latitude
ghcn_4$w5e5 <- raster::extract(tasmin_W5E5[[4]], ghcn_4)
ghcn_4$w5e5 <- ghcn_4$w5e5-273.15
ghcn_4 <- as.data.frame(ghcn_4)
ghcn_4a <- aggregate(.~longitude+latitude, ghcn_4, mean)

cw_tasmin_4<- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_4_tasmin")
cw_tasmin_4$dy <- cw_tasmin_4$dy/10-273.15
cw_tasmin_4a <- aggregate(.~coords.x1+ coords.x2 ,cw_tasmin_4, mean)
taylor.diagram(ghcn_4a$tvalue, ghcn_4a$w5e5, normalize = T, add = T, col="#7570b3", pch=15, pcex=1.2)
taylor.diagram(cw_tasmin_4a$tvalue, cw_tasmin_4a$dy, normalize = T, add = T, col="#d95f02", pch=15, pcex=1.2)

# TASMAX ####

seasons_v <- c("(12, 1, 2)", "(3, 4, 5)","(6, 7, 8)","(9, 10, 11)")
for (n in 1:4)
{
  SQL_Statement <-paste0("SELECT count(*) AS n, avg(CAST(d.value AS numeric))/10 AS tvalue, s.latitude, s.longitude, d.year
                         FROM ghcnd.daydat d, ghcnd.stations s
                         WHERE d.id = s.id
                         AND d.element = \'TMAX\'
                         AND d.year < 2017
                         AND d.year > 1978
                         AND d.month IN ",seasons_v[n]," 
                         AND d.value IS NOT NULL
                         AND d.qflag = ''
                         GROUP BY s.id, d.element, d.year, d.month
                         HAVING count(*) > 25
                         ORDER BY s.latitude, s.longitude")
  
  ghcn<-dbGetQuery(con,SQL_Statement)
  
  assign(paste0('ghcn_',n),ghcn)
}

#GHCN 1 - WINTER
tasmax_W5E5 <- stack("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/W5E5_validation_tasmax_W5E5.nc")
coordinates(ghcn_1)<-~longitude+latitude
ghcn_1$w5e5 <- raster::extract(tasmax_W5E5[[1]], ghcn_1)
ghcn_1$w5e5<-ghcn_1$w5e5-273.15
ghcn_1<-as.data.frame(ghcn_1)
ghcn_1a<-aggregate(.~longitude+latitude, ghcn_1, mean)

cw_tasmax_1 <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_1_tasmax")
cw_tasmax_1$dy <- cw_tasmax_1$dy/10-273.15
cw_tasmax_1a <- aggregate(.~coords.x1+ coords.x2 ,cw_tasmax_1, mean)
taylor.diagram(ghcn_1a$tvalue, ghcn_1a$w5e5, normalize = T, col="#7570b3", main = "Maximum Daily 2m Air-Temperature", pch=16, add=F, pcex=1.2)
text(1,0.7,"W5E5", cex=1, col="#7570b3")
text(1,0.7,"W5E5", cex=1, col="#7570b3")
taylor.diagram(cw_tasmax_1a$tvalue, cw_tasmax_1a$dy, normalize = T, add=T, col="#d95f02", pch = 16, pcex=1.2)
text(0.5,0.45,"CHELSA_W5E5", cex=1.2, col="#d95f02")
text(0.5,0.45,"CHELSA_W5E5", cex=1.2, col="#d95f02")

# GHCN 2 - SPRING
coordinates(ghcn_2) <-~longitude+latitude
ghcn_2$w5e5 <- raster::extract(tasmax_W5E5[[3]], ghcn_2)
ghcn_2$w5e5 <- ghcn_2$w5e5-273.15
ghcn_2 <- as.data.frame(ghcn_2)
ghcn_2a <- aggregate(.~longitude+latitude, ghcn_2, mean)

cw_tasmax_2<- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_2_tasmax")
cw_tasmax_2$dy <- cw_tasmax_2$dy/10-273.15
cw_tasmax_2a <- aggregate(.~coords.x1+ coords.x2 ,cw_tasmax_2, mean)
taylor.diagram(ghcn_2a$tvalue, ghcn_2a$w5e5, normalize = T, add=T, col="#7570b3", pch=17, pcex=1.2)
taylor.diagram(cw_tasmax_2a$tvalue, cw_tasmax_2a$dy, normalize = T, add=T, col="#d95f02", pch=17, pcex=1.2)

#GHCN 3 - SUMMER
coordinates(ghcn_3) <-~longitude+latitude
ghcn_3$w5e5 <- raster::extract(tasmax_W5E5[[2]], ghcn_3)
ghcn_3$w5e5 <- ghcn_3$w5e5-273.15
ghcn_3 <- as.data.frame(ghcn_3)
ghcn_3a <- aggregate(.~longitude+latitude, ghcn_3, mean)

cw_tasmax_3<- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_3_tasmax")
cw_tasmax_3$dy <- cw_tasmax_3$dy/10-273.15
cw_tasmax_3a <- aggregate(.~coords.x1+ coords.x2 ,cw_tasmax_3, mean)
taylor.diagram(ghcn_3a$tvalue, ghcn_3a$w5e5,normalize = T, add = TRUE, col="#7570b3", pch=18, pcex=1.2)
taylor.diagram(cw_tasmax_3a$tvalue, cw_tasmax_3a$dy,normalize = T, add = TRUE, col="#d95f02",pch=18, pcex=1.2)

#GHCN 4 - FALL 
coordinates(ghcn_4) <-~longitude+latitude
ghcn_4$w5e5 <- raster::extract(tasmax_W5E5[[4]], ghcn_4)
ghcn_4$w5e5 <- ghcn_4$w5e5-273.15
ghcn_4 <- as.data.frame(ghcn_4)
ghcn_4a <- aggregate(.~longitude+latitude, ghcn_4, mean)

cw_tasmax_4<- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_4_tasmax")
cw_tasmax_4$dy <- cw_tasmax_4$dy/10-273.15
cw_tasmax_4a <- aggregate(.~coords.x1+ coords.x2 ,cw_tasmax_4, mean)
taylor.diagram(ghcn_4a$tvalue, ghcn_4a$w5e5, normalize = T, add = T, col="#7570b3", pch=15, pcex=1.2)
taylor.diagram(cw_tasmax_4a$tvalue, cw_tasmax_4a$dy, normalize = T, add = T, col="#d95f02", pch=15, pcex=1.2)

# PREC ####
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user="*****", password="****" ,host="postgres.wsl.ch", port=5432, dbname="ghcn")

seasons_v <- c("(12, 1, 2)", "(3, 4, 5)","(6, 7, 8)","(9, 10, 11)")

for (n in 1:4)
{
  SQL_Statement <-paste0("SELECT count(*) AS n, avg(CAST(d.value AS numeric))/10 AS tvalue, s.latitude, s.longitude, d.year
                         FROM ghcnd.daydat d, ghcnd.stations s
                         WHERE d.id = s.id
                         AND d.element = \'PRCP\'
                         AND d.year < 2017
                         AND d.year > 1978
                         AND d.month IN ",seasons_v[n]," 
                         AND d.value IS NOT NULL
                         AND d.qflag = ''
                         GROUP BY s.id, d.element, d.year, d.month
                         HAVING count(*) > 25
                         ORDER BY s.latitude, s.longitude")
  
  ghcn<-dbGetQuery(con,SQL_Statement)
  
  assign(paste0('ghcn_',n),ghcn)
}

#GHCN 1 - WINTER
pr_W5E5 <- stack("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/W5E5_validation_pr_W5E5.nc")
coordinates(ghcn_1)<-~longitude+latitude
ghcn_1$w5e5 <- raster::extract(pr_W5E5[[1]], ghcn_1)
ghcn_1$tvalue <- ghcn_1$tvalue
ghcn_1$w5e5<-ghcn_1$w5e5*60*60*24
ghcn_1<-as.data.frame(ghcn_1)
ghcn_1a<-aggregate(.~longitude+latitude, ghcn_1, mean)

cw_pr_1 <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_1_pr", header=T)
cw_pr_1$tvalue <- cw_pr_1$tvalue
cw_pr_1$dy <- cw_pr_1$dy/10
cw_pr_1a <- aggregate(.~coords.x1+ coords.x2 ,cw_pr_1, mean)
taylor.diagram(ghcn_1a$tvalue, ghcn_1a$w5e5, normalize = T, col="#7570b3", main = "Mean Daily Precipitation", pch=16, pcex=1.2)
text(0.5,0.6,"W5E5", cex=1, col="#7570b3")
text(0.5,0.6,"W5E5", cex=1, col="#7570b3")
taylor.diagram(cw_pr_1a$tvalue, cw_pr_1a$dy, normalize = T, add=T, col="#d95f02", pch = 16, pcex=1.2)
text(0.4,0.2,"CHELSA_W5E5", cex=1.2, col="#d95f02")
text(0.4,0.2,"CHELSA_W5E5", cex=1.2, col="#d95f02")

# GHCN 2 - SPRING
coordinates(ghcn_2) <-~longitude+latitude
ghcn_2$w5e5 <- raster::extract(pr_W5E5[[3]], ghcn_2)
ghcn_2$w5e5 <- ghcn_2$w5e5*60*60*24
ghcn_2 <- as.data.frame(ghcn_2)
ghcn_2a <- aggregate(.~longitude+latitude, ghcn_2, mean)

cw_pr_2<- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_2_pr", header=T)
cw_pr_2$tvalue <- cw_pr_2$tvalue
cw_pr_2$dy <-cw_pr_2$dy/10
cw_pr_2a <- aggregate(.~coords.x1+ coords.x2 ,cw_pr_2, mean)
taylor.diagram(ghcn_2a$tvalue, ghcn_2a$w5e5, normalize = T, add=T, col="#7570b3", pch=17, pcex=1.2)
taylor.diagram(cw_pr_2a$tvalue/10, cw_pr_2a$dy/10, normalize = T, add=T, col="#d95f02", pch=17, pcex=1.2)

#GHCN 3 - SUMMER
coordinates(ghcn_3) <-~longitude+latitude
ghcn_3$w5e5 <- raster::extract(pr_W5E5[[2]], ghcn_3)
ghcn_3$w5e5 <- ghcn_3$w5e5*60*60*24
ghcn_3 <- as.data.frame(ghcn_3)
ghcn_3a <- aggregate(.~longitude+latitude, ghcn_3, mean)

cw_pr_3<- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_3_pr", header=T)
cw_pr_3$tvalue <- cw_pr_3$tvalue
cw_pr_3$dy <-cw_pr_3$dy/10
cw_pr_3a <- aggregate(.~coords.x1+ coords.x2 ,cw_pr_3, mean)
taylor.diagram(ghcn_3a$tvalue, ghcn_3a$w5e5,normalize = T, add = TRUE, col="#7570b3", pch=18, pcex=1.2)
taylor.diagram(cw_pr_3a$tvalue, cw_pr_3a$dy,normalize = T, add = TRUE, col="#d95f02",pch=18, pcex=1.2)

#GHCN 4 - FALL 
coordinates(ghcn_4) <-~longitude+latitude
ghcn_4$w5e5 <- raster::extract(pr_W5E5[[4]], ghcn_4)
ghcn_4$w5e5 <- ghcn_4$w5e5*60*60*24
ghcn_4 <- as.data.frame(ghcn_4)
ghcn_4a <- aggregate(.~longitude+latitude, ghcn_4, mean)

cw_pr_4<- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_4_pr", header=T)
cw_pr_4$tvalue <- cw_pr_4$tvalue
cw_pr_4$dy <-cw_pr_4$dy/10
cw_pr_4a <- aggregate(.~coords.x1+ coords.x2 ,cw_pr_4, mean)
taylor.diagram(ghcn_4a$tvalue, ghcn_4a$w5e5, normalize = T, add = T, col="#7570b3", pch=15, pcex=1.2)
taylor.diagram(cw_pr_4a$tvalue, cw_pr_4a$dy, normalize = T, add = T, col="#d95f02", pch=15, pcex=1.2)

# # RSDS ############################################
# #Winter 

rsds_stations <- read.csv("/storage/harichan/chelsa_V2/W5E5_validation/global/GEBA/stations.csv", header=T, sep=",")
rsds_W5E5 <- stack("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/W5E5_validation_rsds_W5E5.nc")
df_season <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/GEBA/df_season")



y <- fread('/home/admin1/rsds_geba_full_3.txt')
y$chelsa_w5e5 <- y$chelsa_w5e5/100

df_ana <- y[y$month %in% c(12, 1, 2)]
taylor.diagram(df_ana$geba, df_ana$w5e5, normalize = T, col="#7570b3", main = "Monthly Shortwave Downwelling Radiation", pch=16, pcex=1.2)
taylor.diagram(df_ana$geba, df_ana$chelsa_w5e5, normalize = T, col="#d95f02", add=T, pch=16, pcex=1.2)

seasons_v <- list(c(3, 4, 5),c(6, 7, 8),c(9, 10, 11))
for (n in seasons_v)
{
if (n == c(3, 4, 5))
{
  pch1 <- 17
}
if (n == c(6, 7, 8))
{
  pch1 <- 18
}
if (n == c(9, 10, 11))
{
  pch1 <- 15
}
df_ana <- y[y$month %in% n]
taylor.diagram(df_ana$geba, df_ana$w5e5, normalize = T, col="#7570b3", main = "Monthly Shortwave Downwelling Radiation", pch=pch1, pcex=1.2, add=T)
taylor.diagram(df_ana$geba, df_ana$chelsa_w5e5, normalize = T, col="#d95f02", add=T, pch=pch1, pcex=1.2)
}
text(0.75,0.15,"CHELSA-W5E5", cex=1.2, col="#d95f02")
text(1.2,0.25,"W5E5", cex=1.2, col="#7570b3")

par(xpd = T)
legend(1.7,1.7, legend=c("Winter","Spring","Summer", "Autumn"), pch=c(16,17,18,15),cex=1)
par(xpd = NA)



dev.off()

































DJF <- cbind(tvalues=df_season[,2], Station=df_season[,6])
DJF_m <- merge(DJF, rsds_stations, by = "Station")
coordinates(DJF_m) <- ~Longitude + Latitude

DJF_m$w5e5 <- raster::extract(rsds_W5E5$DJF,DJF_m)
DJF_m <- as.data.frame(DJF_m)
DJF_ma<-aggregate(.~Longitude+Latitude, DJF_m, mean)


taylor.diagram(y$geba, y$w5e5, normalize = T, col="#7570b3", main = "Monthly Shortwave Downwelling Radiation", pch=16, pcex=1.2)
text(1.25,0.7,"W5E5", cex=1.2, col="#7570b3")
text(1.25,0.7,"W5E5", cex=1.2, col="#7570b3")

rsds_1 <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_1_rsds")
rsds_1$dy <- rsds_1$dy/100
rsds_1a <- aggregate(.~coords.x1+coords.x2, rsds_1, mean)
taylor.diagram(rsds_1a$tvalues, rsds_1a$dy, normalize = T, col="#d95f02", add=T, pch=16, pcex=1.2)
text(0.55,0.2,"CHELSA_W5E5", cex=1.2, col="#d95f02")
text(0.55,0.2,"CHELSA_W5E5", cex=1.2, col="#d95f02")

#Spring 
MAM <- cbind(tvalues=df_season[,3], Station=df_season[,6])
MAM_m <- merge(MAM, rsds_stations, by = "Station")
coordinates(MAM_m) <- ~Longitude + Latitude
MAM_m$w5e5 <- raster::extract(rsds_W5E5$MAM,MAM_m)
MAM_m <- as.data.frame(MAM_m)
MAM_ma<-aggregate(.~Longitude+Latitude, MAM_m, mean)
taylor.diagram(MAM_ma$tvalues, MAM_ma$w5e5, normalize = T, col="#7570b3", pch=17, add = T, pcex=1.2)
rsds_2 <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_2_rsds")
rsds_2$dy <- rsds_2$dy/100
rsds_2a <- aggregate(.~coords.x1+coords.x2, rsds_2, mean)
taylor.diagram(rsds_2a$tvalues, rsds_2a$dy, normalize = T, col="#d95f02", add=T, pch=17, pcex=1.2)

#Summer 
JJA <- cbind(tvalues=df_season[,4], Station=df_season[,6])
JJA_m <- merge(JJA, rsds_stations, by = "Station")
coordinates(JJA_m) <- ~Longitude + Latitude
JJA_m$w5e5 <- raster::extract(rsds_W5E5$JJA,JJA_m)
JJA_m <- as.data.frame(JJA_m)
JJA_ma<-aggregate(.~Longitude+Latitude, JJA_m, mean)
taylor.diagram(JJA_ma$tvalues, JJA_ma$w5e5, normalize = T, col="#7570b3",pch=18, add = T, pcex=1.2)
rsds_3 <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_3_rsds")
rsds_3$dy <- rsds_3$dy/100
rsds_3a <- aggregate(.~coords.x1+coords.x2, rsds_3, mean)
taylor.diagram(rsds_3a$tvalues, rsds_3a$dy, normalize = T, col="#d95f02", add=T, pch=18, pcex=1.2)

#Fall 
SON <- cbind(tvalues=df_season[,5], Station=df_season[,6])
SON_m <- merge(SON, rsds_stations, by = "Station")
coordinates(SON_m) <- ~Longitude + Latitude
SON_m$w5e5 <- raster::extract(rsds_W5E5$SON,SON_m)
SON_m <- as.data.frame(SON_m)
SON_ma<-aggregate(.~Longitude+Latitude, SON_m, mean)
taylor.diagram(SON_ma$tvalues, SON_ma$w5e5, normalize = T, col="#7570b3", pch=15,  add = T, pcex=1.2)
rsds_4 <- read.table("/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_endfiles/chelsa_w5e5_4_rsds")
rsds_4$dy <- rsds_4$dy/100
rsds_4a <- aggregate(.~coords.x1+coords.x2, rsds_4, mean)
taylor.diagram(rsds_4a$tvalues, rsds_4a$dy, normalize = T, col="#d95f02", add=T, pch=15, pcex=1.2)

dev.off()
