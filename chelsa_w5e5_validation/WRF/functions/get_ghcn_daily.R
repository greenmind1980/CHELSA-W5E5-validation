library("sp")
library("raster")
library("rgdal")
library(RPostgreSQL)

# Extract GHCN-data  ####
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user="****", password="****" ,host="postgres.wsl.ch", port=5432, dbname="ghcn")

args <- commandArgs(trailingOnly=TRUE)

contents <- strsplit(args, " ")


day    <-contents[[1]]
month  <-contents[[2]]
year   <-contents[[3]]
day <-suppressWarnings(as.numeric(as.character(day)))
month <-suppressWarnings(as.numeric(as.character(month)))
year <-suppressWarnings(as.numeric(as.character(year)))

# Set variable ####
var <-'tasmin'
var_ghcn <- 'TMIN'


      SQL_Statement <-paste0("SELECT d.value, s.latitude, s.longitude, s.id
                            FROM ghcnd.daydat d, ghcnd.stations s
                            WHERE d.id = s.id
                            AND d.element = \'TMIN\'
                            AND d.year = ",year,"
                            AND d.month = ",month,"
                            AND d.day = ",day," 
                            AND d.qflag = ''")
      
      # dann kann man die Datenbank abrufen und das ganze in einen dataframe schreiben.
      
      ghcn<-dbGetQuery(con,SQL_Statement)
      
      # mit dem raster package kann man dann aus dem Dataframe ein spatial object machen
      
      coordinates(ghcn) <- ~ longitude + latitude
      
      # und dann noch das Koordinaten system setzen
      crs(ghcn) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      shapefile(ghcn, filename= paste0('/storage/harichan/chelsa_V2/W5E5_validation/global/ghcn_daily/',var,'/',var,'_ghcn_',day,'_',month,'_',year,'.shp'), overwrite=T)
