library(raster)
library(ncdf4)

# function to read GEBA data
read_geba_data <- function(file)
{
    df <- read.csv(file, sep=";", header=F)
    colnames(df) <- c("station_id", "var_id", "year", seq(1,12,1),"annual_mean")
    row_odd <- seq_len(nrow(df)) %% 2 
    df_odd <- df[row_odd == 1, ]
    return(df_odd)
}

# function to read GEBA station data
read_geba_stations <- function(file)
{
    df <- read.table(file, header = T, sep="\t")
    df$lon[df$lon>180] <- df$lon[df$lon>180]-360
    return(df)
}

# read in the GEBA data
full <- read_geba_data('/home/admin1/Downloads/geba_global_direct_diffuse.csv')

# read in the station metatdata
stat_glob <- read_geba_stations('/home/admin1/Downloads/stations_global.txt')
stat_diff <- read_geba_stations('/home/admin1/Downloads/stations_diffuse.txt')
stat_dire <- read_geba_stations('/home/admin1/Downloads/stations_direct.txt')

stations <- rbind(stat_glob, stat_diff, stat_dire)
colnames(stations)[1] <- 'station_id'
#coordinates(stations) <- ~lon+lat
#plot(stations)

dir_diff <- full[full$var_id %in% c(2,3),]

# create data frame including direct plus diffuse radiation
dirPdiff <- data.frame(station_id=integer(), var_id=integer(), 
                     year=integer(), m1=integer(), m2=integer(), 
                     m3=integer(), m4=integer(), m5=integer(), 
                     m6=integer(), m7=integer(), m8=integer(),
                      m9=integer(), m10=integer(), m11=integer(), 
                     m12=integer(), annual_mean=integer())

for (n in unique(dir_diff$station_id))
{
    df2 <- dir_diff[dir_diff$station_id == n, ]
    for (m in unique(df2$year))
    {
        direct_rad  <- df2[df2$var_id==2 & df2$year == m, ]
        diffuse_rad <- df2[df2$var_id==3 & df2$year == m, ]

        try(dirPdiff <- rbind(dirPdiff, data.frame(station_id=direct_rad[1], var_id="dirpdiff", 
                     year=direct_rad[3], m1=direct_rad[4]+diffuse_rad[4], m2=direct_rad[5]+diffuse_rad[5], 
                     m3=direct_rad[6]+diffuse_rad[6], m4=direct_rad[7]+diffuse_rad[7], m5=direct_rad[8]+diffuse_rad[8], 
                     m6=direct_rad[9]+diffuse_rad[9], m7=direct_rad[10]+diffuse_rad[10], m8=direct_rad[11]+diffuse_rad[11],
                     m9=direct_rad[12]+diffuse_rad[12], m10=direct_rad[13]+diffuse_rad[13], m11=direct_rad[14]+diffuse_rad[14], 
                     m12=direct_rad[15]+diffuse_rad[15], annual_mean=direct_rad[16]+diffuse_rad[16])))
    }
}

df_glob <- full[full$var_id == 1,]
colnames(df_glob) <- colnames(dirPdiff)
dirPdiff <- df_glob#rbind(df_glob, dirPdiff)

# combine data and station metadata
df_full <- merge(dirPdiff, stations, by.x='station_id', by.y='station_id')

# extract the chelsa-w5e5 and w5e5 data
# set the input directories
CHELSA_V2 <- '/storage/brunp/Data/CHELSA_V.2.1/RSDS/CHELSA/Monthly/'
W5E5 <- '/storage/karger/W5E5/'
INPUT <- '/storage/karger/chelsa_V2/W5E5/rsds/'

# read in the stacks
w5e5_st <- stack(paste0(W5E5, "rsds_W5E5v1.0_19790101-19801231.nc"),
                paste0(W5E5,"rsds_W5E5v1.0_19810101-19901231.nc"),
                paste0(W5E5,"rsds_W5E5v1.0_19910101-20001231.nc"),
                paste0(W5E5,"rsds_W5E5v1.0_20010101-20101231.nc"),
                paste0(W5E5,"rsds_W5E5v1.0_20110101-20161231.nc"))

lut <- seq(from = as.Date("1979-01-01"), to = as.Date("2016-12-31"), by = 'day')
lut_ym <- format(lut, "%Y-%m")

# create data frame with validation data
df_new <- data.frame(station_id=integer(), year=integer(), month=integer(), 
                     lon=double(), lat=double(), geba=double(), w5e5=double(), 
                     chelsa_w5e5=double(), chelsa_V2=double())


library(doParallel)
registerDoParallel(cl <- makeCluster(30))
#for (n in c(1979:2016))
#{
results_list <- foreach(n = 1979:2016,.packages=c("raster", "ncdf4")) %dopar% {
    df_ana <- df_full[df_full$year == n, ]
    coordinates(df_ana) <- ~lon+lat
    df_new <- data.frame(station_id=integer(), year=integer(), month=integer(), 
                     lon=double(), lat=double(), geba=double(), w5e5=double(), 
                     chelsa_w5e5=double(), chelsa_V2=double())
    for (month in c(1:12))
    {
        pos <- which(lut_ym == paste0(n, "-", sprintf("%02d", month)))
        r1 <- raster(paste0(CHELSA_V2,'CHELSA_rsds_',n,'_',sprintf("%02d", month),'_V.2.1.tif'))
        files <- list.files(INPUT, pattern = paste0(sprintf("%02d",month), '_', n, '_V.1.0.tif$'), full.names=T)
        chelsa_st <- stack(files)
        chelsa_w5e5_e <- extract(chelsa_st, df_ana)
        chelsa_w5e5_e_m <- rowMeans(chelsa_w5e5_e)
        w5e5_e <- extract(w5e5_st[[pos]], df_ana)
        w5e5_e_m <- rowMeans(w5e5_e)
        geba <- df_ana[,(3+month)]
        chelsa_v2 <- extract(r1, df_ana)
        df_new <- rbind(df_new, data.frame(station_id=df_ana$station_id, year=n, month=month, 
                            lon=df_ana$lon, lat=df_ana$lat, geba=as.data.frame(geba)[,1], w5e5=w5e5_e_m, 
                            chelsa_w5e5=chelsa_w5e5_e_m, chelsa_V2=chelsa_v2))
    }
    return(df_new)
}

df_res <- do.call("rbind", results_list)

stopCluster(cl)

write.table(df_res, file="~/rsds_geba_full_3.txt")




