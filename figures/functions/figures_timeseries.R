library(data.table)
library(gridExtra)
library(grid)
library(ggplot2)
library(viridis)
library(raster)
library(Metrics)
library(geosphere)
library(wsl.plot)
library(RColorBrewer)
library(doParallel)
library(stringr)
readjust_date_format <- function(x)
  {
  # easiest case
  if (nchar(x) == 8)
    {
    year <- substr(x,1,4)
    month <- substr(x,5,6)
    day <- substr(x,7,8)
    }
  # worse case
  if (nchar(x) == 7)
    {
      if (substr(x,5,6) == "10" | substr(x,5,6) == "11" | substr(x,5,6) == "12" )
        {
        year <- substr(x,1,4)
        month <- substr(x,5,6)
        day <- substr(x,7,7)
        }else{
        year <- substr(x,1,4)
        month <- substr(x,5,5)
        day <- substr(x,6,7)
        }   
    }
  # easy case
  if (nchar(x) == 6)
    {
    year <- substr(x,1,4)
    month <- substr(x,5,5)
    day <- substr(x,6,6)
    }
  newdate <- paste0(year, "_", sprintf("%02d",as.numeric(month)), "_", sprintf("%02d",as.numeric(day)))
  return(newdate)
}


library(doSNOW)
library(tcltk)

cl <- makeSOCKcluster(20)
registerDoSNOW(cl)



var <- c("pr", "tas", "tasmax", "tasmin")
for (i in var)
{
  print(paste0("processing: ",i, "...."))
  input <- paste0("/storage/harichan/chelsa_V2/W5E5_validation/global/chelsa_w5e5/merged_",i)
  listl <- list.files(input)
  df1 <- as.data.frame(str_split_fixed(listl, "_", 5))
  df1$V5 <- gsub(".txt","",df1$V5)
  colnames(df1) <- c("name","model","submodel","var","date")

  newdates <- as.data.frame(unlist(lapply(df1$date, FUN=function(x){readjust_date_format(x)})))
  dates_df <- as.data.frame(str_split_fixed(newdates[,1], "_", 3))
  colnames(dates_df) <- c("year", "month", "day")
  df1 <- cbind(df1, dates_df)

  ll <- length(df1[,1])
  ntasks <- ll
  pb <- tkProgressBar(max=ntasks)
  progress <- function(n) setTkProgressBar(pb, n)
  opts <- list(progress=progress)

  start <- proc.time()
  foreach(n = 1:ll, .options.snow=opts) %dopar% {
    filename <- paste0(input,"/",df1[n,1],"_",df1[n,2],"_",df1[n,3],"_",df1[n,4],"_",df1[n,5],".txt")
    dfx <- read.table(filename)
    dfx$year <- df1[n,6]
    dfx$month <- df1[n,7]
    dfx$day <- df1[n,8]
    write.table(dfx,file=paste0("/storage/karger/chelsa_w5e5_validation/",i,"/",df1[n,1],"_",df1[n,2],"_",df1[n,3],"_",df1[n,4],"_",df1[n,5],".txt"))
  }
  #write.table(fulldata, file=paste0("/storage/karger/chelsa_w5e5_validation/",i,"_fulldata.txt"))
  dopar_loop <- proc.time()-start
}
stopCluster(cl)


var <- c("tas", "tasmax", "tasmin")
for (i in var)
{
  print(i)
  dfana <- fread(paste0("/storage/karger/chelsa_w5e5_validation/",i,"_fulldata.txt"))

  if (i == 'pr')
  {
    colnames(dfana)<- c("no","value","id","chelsa_w5e5","chelsa_V2","x","y","w5e5","year","month","day")
  }

  if (i == 'tas' || i == 'tasmax' ||i == 'tasmin')
  {
    colnames(dfana)<- c("no","value","id","chelsa_w5e5","x","y","w5e5","year","month","day")
    dfana$value <- as.numeric(dfana$value)/10
    dfana$chelsa_w5e5 <- as.numeric(dfana$chelsa_w5e5)/10-273.15
    dfana$w5e5 <- as.numeric(dfana$w5e5)-273.15
  }

  dfana$year_month <- paste0(dfana$year,"_",dfana$month)
  resu <- data.frame(cor_chelsa_w5e5=numeric(), cor_w5e5=numeric(), bias_chelsa_w5e5=numeric(), bias_w5e5=numeric(), year=numeric(), month=numeric())
  m=0
  ll = unique(dfana$year_month)
  for (n in ll)
  {
    m=m+1
    print(paste0(round(m/length(ll)*100,2),"% finished"))
    dfx <- dfana[dfana$year_month==n, ]
    dfx <- dfx[complete.cases(dfx), ]
    cor_chelsa_w5e5 <- cor(dfx$value, dfx$chelsa_w5e5)
    cor_w5e5 <- cor(dfx$value, dfx$w5e5)
    bias_chelsa <- bias(dfx$value, dfx$chelsa_w5e5)
    bias_w5e5 <- bias(dfx$value, dfx$w5e5)
    res <- c(cor_chelsa_w5e5, cor_w5e5, bias_chelsa, bias_w5e5, str_split_fixed(n, "_", 2))
    resu <- rbind(resu, data.frame(cor_chelsa_w5e5=cor_chelsa_w5e5, cor_w5e5=cor_w5e5,bias_chelsa_w5e5=bias_chelsa, bias_w5e5=bias_w5e5, year=str_split_fixed(n, "_", 2)[1], month=str_split_fixed(n, "_", 2)[2]))
  }
  resu$year <- as.numeric(as.character(resu$year))
  resu$month <- as.numeric(as.character(resu$month))
  resu <- resu[order( resu[,3], resu[,4] ),]
  write.table(resu, file=paste0("/storage/karger/chelsa_w5e5_validation/", i, "_resu.txt"))
  rm(dfana)
}

dfana <- fread('/home/admin1/rsds_geba_full_3.txt')
dfana$chelsa_w5e5 <- dfana$chelsa_w5e5/100
dfana$year_month <- paste0(dfana$year,"_",dfana$month)
resu <- data.frame(cor_chelsa_w5e5=numeric(), cor_w5e5=numeric(), bias_chelsa_w5e5=numeric(), bias_w5e5=numeric(), year=numeric(), month=numeric())
colnames(dfana)[7]<-"value"
# for rsds
  m=0
  ll = unique(dfana$year_month)
  for (n in ll)
  {
    m=m+1
    print(paste0(round(m/length(ll)*100,2),"% finished"))

    dfx <- dfana[dfana$year_month==n, ]
    dfx <- dfx[complete.cases(dfx), ]
    cor_chelsa_w5e5 <- cor(dfx$value, dfx$chelsa_w5e5)
    cor_w5e5 <- cor(dfx$value, dfx$w5e5)
    bias_chelsa <- bias(dfx$value, dfx$chelsa_w5e5)
    bias_w5e5 <- bias(dfx$value, dfx$w5e5)
    res <- c(cor_chelsa_w5e5, cor_w5e5,str_split_fixed(n, "_", 2))
    resu <- rbind(resu, data.frame(cor_chelsa_w5e5=cor_chelsa_w5e5, cor_w5e5=cor_w5e5,bias_chelsa_w5e5=bias_chelsa, bias_w5e5=bias_w5e5,year=str_split_fixed(n, "_", 2)[1], month=str_split_fixed(n, "_", 2)[2]))
  }
  resu$year <- as.numeric(as.character(resu$year))
  resu$month <- as.numeric(as.character(resu$month))
  resu <- resu[order( resu[,3], resu[,4] ),]

res_rsds <- resu

var <- c("pr", "tas", "tasmax", "tasmin")
for (i in var)
{
assign(paste0("res_",i), read.table(paste0("/storage/karger/chelsa_w5e5_validation/", i, "_resu.txt"), header=T))
}

cols <- c(rgb(228,26,28, maxColorValue =255), 
rgb(55,126,184, maxColorValue =255),
rgb(77,175,74, maxColorValue =255),
rgb(152,78,163, maxColorValue =255),
rgb(255,127,0, maxColorValue =255))

#red, blue, green, violett, orange

res_pr <- res_pr[order( res_pr[,5], res_pr[,6] ),]
res_tas <- res_tas[order( res_tas[,5], res_tas[,6] ),]
res_tasmax <- res_tasmax[order( res_tasmax[,5], res_tasmax[,6] ),]
res_tasmin <- res_tasmin[order( res_tasmin[,5], res_tasmin[,6] ),]
res_rsds <- res_rsds[order( res_rsds[,5], res_rsds[,6] ),]


res_pr$rel_bias_cor <- 100*abs(res_pr$bias_chelsa_w5e5)/abs(res_pr$bias_w5e5)


# make the plot
pdf("~/scratch/trendlines.pdf", height=10, width=10)
par(mfrow = c(5,2), mar=c(3,5,2,1), oma=c(5,1,1,1))

vars <- c("pr", "tas", "tasmax", "tasmin", "rsds")
for (var in vars){
  res1 <- get(paste0("res_",var))
  xlab1 <- ""
  if (var == "rsds") {
    xlab1 <- "year"
  }
  plot(res1$cor_chelsa_w5e5, type="l", col="#d95f02", ylab="Pearson r", main=var, xaxt = "n", xlab=xlab1, 
       ylim=c(min(res1$cor_chelsa_w5e5,res1$cor_w5e5),max(c(res1$cor_chelsa_w5e5,res1$cor_w5e5))))
  points(res1$cor_w5e5,  type="l", col="#7570b3")

  if (var == "pr") {
  text(x=25, y=0.65,"CHELSA-W5E5", col = "#d95f02", pos=4 )
  text(x=25, y=0.615,"W5E5", col = "#7570b3", pos=4)
  }
  axis(1, at=c(seq(1, 456,by=12),456), labels=seq(1979, 2017, 1))

  if (var == "rsds") {
    title(xlab="year", line=2, cex.lab=1.2)
  }


  plot(abs(res1$bias_chelsa_w5e5), type="l", col="#d95f02", ylab="absolute bias", main=var, xaxt = "n", xlab=xlab1,
       ylim=c(0,max(c(abs(res1$bias_chelsa_w5e5),abs(res1$bias_w5e5)))))
  points(abs(res1$bias_w5e5),  type="l", col="#7570b3")
  if (var == "rsds") {
    title(xlab="year", line=2, cex.lab=1.2)
  }

  axis(1, at=c(seq(1, 456,by=12),456), labels=seq(1979, 2017, 1))
}

dev.off()



taylor.diagram(ghcn_2a$tvalue, ghcn_2a$w5e5, normalize = T, add=T, col="#7570b3", pch=17, pcex=1.2)
taylor.diagram(cw_tas_2a$tvalue, cw_tas_2a$dy, normalize = T, add=T, col="#d95f02", pch=17, pcex=1.2)

