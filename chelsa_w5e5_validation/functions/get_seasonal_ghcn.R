library("sp")
library("raster")
library("rgdal")
library(RPostgreSQL)
library(ncdf4)


#####
# Extract GHCN-data  ####
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user="***", password="***" ,host="postgres.wsl.ch", port=5432, dbname="ghcn")

args <- commandArgs(trailingOnly=TRUE)

contents <- strsplit(args, " ")
year   <-contents[[1]]
year <-suppressWarnings(as.numeric(as.character(year)))

seasons_v <- c("(12, 1, 2)", "(3, 4, 5)","(6, 7, 8)","(9, 10, 11)")
variables_v <- "TMAX"
variables <- "tasmax"


for (n in 1:4)
{
  for (i in 1)
  {
        SQL_Statement <-paste0("SELECT count(*) AS n, avg(CAST(d.value AS numeric))/10 AS tvalue, s.latitude, s.longitude, d.year, d.month
                         FROM ghcnd.daydat d, ghcnd.stations s
                         WHERE d.id = s.id
                         AND d.element = \'",variables_v,"\'
                         AND d.year = ",year,"
                         AND d.month IN ",seasons_v[n],"
                         AND d.qflag = ''
                         GROUP BY s.id, d.element, d.year, d.month
                         HAVING count(*) > 25
                         ORDER BY d.year")
        
        ghcn<-dbGetQuery(con,SQL_Statement)
        
        assign(paste0('ghcn_',variables_v[i],'_',n),ghcn)
        assign(paste0('ghcn_',variables_v[i],'_',n,'_new'), aggregate(.~latitude+longitude+month, get(paste0('ghcn_',variables_v[i],'_',n)), mean))
        
        dfx<-get(paste0('ghcn_',variables_v[i],'_',n,'_new'))
        coordinates(dfx)<-~longitude+latitude
        assign(paste0('ghcn_',variables_v[i],'_',n,'_new'), dfx)
        shapefile(dfx, filename= paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/',variables[i],'/ghcn_',n,'_',year,'_',variables[i],'.shp'), overwrite=T)
      
    }
  }
