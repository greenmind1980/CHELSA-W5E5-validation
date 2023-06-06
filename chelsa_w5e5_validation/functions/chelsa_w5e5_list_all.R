months<-c(c(12,1,2),c(3,4,5),c(6,7,8),c(9,10,11))


TAS <-"/storage/karger/chelsa_V2/W5E5/tas/"
TASMAX<-"/storage/karger/chelsa_V2/W5E5/tasmax/"
TASMIN<-"/storage/karger/chelsa_V2/W5E5/tasmin/"
PR<-"/storage/karger/chelsa_V2/W5E5/pr/"
# RSDS <- "/storage/karger/chelsa_V2/W5E5/rsds/"


# array_test.r
# Collect command arguments
args <- commandArgs(trailingOnly = TRUE)
args_contents <- strsplit(args, ' ')

year <- args_contents[[1]]

year <-suppressWarnings(as.numeric(as.character(year)))

for (n in 1)
{
    ####################################################
    # WINTER #
    
    ls_tas<-c()
    ls_tasmax <-c()
    ls_tasmin <-c()
    ls_pr <-c()
    # ls_rsds <-c()
    
    for (month in months[1])
    {
      for (day in 1:31)
      {
        ls_tas<-paste0(ls_tas,";",paste0(TAS,"CHELSA_W5E5_tas_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year-1,"_V.1.0.tif"))
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year-1,"_V.1.0.tif"))
        ls_tasmin <-paste0(ls_tasmin,";",paste0(TASMIN,"CHELSA_W5E5_tasmin_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year-1,"_V.1.0.tif"))
        ls_pr <-paste0(ls_pr,";",paste0(PR,"CHELSA_W5E5_pr_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year-1,"_V.1.0.tif"))
        # ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    for (month in months[2])
    {
      for (day in 1:31)
      {
        ls_tas<-paste0(ls_tas,";",paste0(TAS,"CHELSA_W5E5_tas_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmin <-paste0(ls_tasmin,";",paste0(TASMIN,"CHELSA_W5E5_tasmin_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_pr <-paste0(ls_pr,";",paste0(PR,"CHELSA_W5E5_pr_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        # ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    
    for (month in months[3])
    {
      if((year %% 4) == 0) {
        if((year %% 100) == 0) {
          if((year %% 400) == 0) {
            for (day in 1:29)
            {
              ls_tas<-paste0(ls_tas,";",paste0(TAS,"CHELSA_W5E5_tas_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
              ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
              ls_tasmin <-paste0(ls_tasmin,";",paste0(TASMIN,"CHELSA_W5E5_tasmin_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
              ls_pr <-paste0(ls_pr,";",paste0(PR,"CHELSA_W5E5_pr_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
              # ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
            }
          } else {
            for (day in 1:28)
            {
              ls_tas<-paste0(ls_tas,";",paste0(TAS,"CHELSA_W5E5_tas_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
              ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
              ls_tasmin <-paste0(ls_tasmin,";",paste0(TASMIN,"CHELSA_W5E5_tasmin_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
              ls_pr <-paste0(ls_pr,";",paste0(PR,"CHELSA_W5E5_pr_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
              # ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
            }
          }
        } else {
          for (day in 1:29)
          {
            ls_tas<-paste0(ls_tas,";",paste0(TAS,"CHELSA_W5E5_tas_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
            ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
            ls_tasmin <-paste0(ls_tasmin,";",paste0(TASMIN,"CHELSA_W5E5_tasmin_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
            ls_pr <-paste0(ls_pr,";",paste0(PR,"CHELSA_W5E5_pr_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
            # ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
          }
        }
      } else {
        for (day in 1:28)
        {
          ls_tas<-paste0(ls_tas,";",paste0(TAS,"CHELSA_W5E5_tas_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
          ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
          ls_tasmin <-paste0(ls_tasmin,";",paste0(TASMIN,"CHELSA_W5E5_tasmin_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
          ls_pr <-paste0(ls_pr,";",paste0(PR,"CHELSA_W5E5_pr_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
          # ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        }
      }
    }
    
    
    ls_tas<-paste0("\"",substring(ls_tas,2),"\"") 
    ls_tasmax<-paste0("\"",substring(ls_tasmax,2),"\"") 
    ls_tasmin<-paste0("\"",substring(ls_tasmin,2),"\"") 
    ls_pr<-paste0("\"",substring(ls_pr,2),"\"") 
    # ls_rsds<-paste0("\"",substring(ls_rsds,2),"\"") 
    
    # systemcommand<-paste0("saga_cmd shapes_grid 0 -SHAPES '/home/harichan/ghcn_2_1981_new.shp' -GRIDS ",ls_tas," -RESULT '/home/harichan/MAM_new",year,".shp'")
    # system(systemcommand)
    
    systemcommand_tas <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tas/ghcn_",n,"_",year,"_tas.shp' -GRIDS ",ls_tas," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tas/chelsa_w5e5_",n,"_tas_",year,".shp'")
    systemcommand_tasmax <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tasmax/ghcn_",n,"_",year,"_tasmax.shp' -GRIDS ",ls_tasmax," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tasmax/chelsa_w5e5_",n,"_tasmax_",year,".shp'")
    systemcommand_tasmin <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tasmin/ghcn_",n,"_",year,"_tasmin.shp' -GRIDS ",ls_tasmin," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tasmin/chelsa_w5e5_",n,"_tasmin_",year,".shp'")
    systemcommand_pr <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/pr/ghcn_",n,"_",year,"_pr.shp' -GRIDS ",ls_pr," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/pr/chelsa_w5e5_",n,"_pr_",year,".shp'")
    
    system(systemcommand_tas)
    system(systemcommand_tasmax)
    system(systemcommand_tasmin)
    system(systemcommand_pr)
}
    ####################################################
    # SPRING #
for (n in 2)
{   
    ls_tas<-c()
    ls_tasmax <-c()
    ls_tasmin <-c()
    ls_pr <-c()
    # ls_rsds <-c()
    for (month in c(months[4], months[6]))
    {
      for (day in 1:31)
      {
        ls_tas<-paste0(ls_tas,";",paste0(TAS,"CHELSA_W5E5_tas_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmin <-paste0(ls_tasmin,";",paste0(TASMIN,"CHELSA_W5E5_tasmin_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_pr <-paste0(ls_pr,";",paste0(PR,"CHELSA_W5E5_pr_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        # ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    for (month in months[5])
    {
      for (day in 1:30)
      {
        ls_tas <-paste0(ls_tas,";",paste0(TAS,"CHELSA_W5E5_tas_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmin <-paste0(ls_tasmin,";",paste0(TASMIN,"CHELSA_W5E5_tasmin_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_pr <-paste0(ls_pr,";",paste0(PR,"CHELSA_W5E5_pr_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        # ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    
    ls_tas<-paste0("\"",substring(ls_tas,2),"\"") 
    ls_tasmax<-paste0("\"",substring(ls_tasmax,2),"\"") 
    ls_tasmin<-paste0("\"",substring(ls_tasmin,2),"\"") 
    ls_pr<-paste0("\"",substring(ls_pr,2),"\"") 
    # ls_rsds<-paste0("\"",substring(ls_rsds,2),"\"") 
    
    # systemcommand<-paste0("saga_cmd shapes_grid 0 -SHAPES '/home/harichan/ghcn_2_1981_new.shp' -GRIDS ",ls_tas," -RESULT '/home/harichan/MAM_new",year,".shp'")
    # system(systemcommand)
    
    systemcommand_tas <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tas/ghcn_",n,"_",year,"_tas.shp' -GRIDS ",ls_tas," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tas/chelsa_w5e5_",n,"_tas_",year,".shp'")
    systemcommand_tasmax <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tasmax/ghcn_",n,"_",year,"_tasmax.shp' -GRIDS ",ls_tasmax," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tasmax/chelsa_w5e5_",n,"_tasmax_",year,".shp'")
    systemcommand_tasmin <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tasmin/ghcn_",n,"_",year,"_tasmin.shp' -GRIDS ",ls_tasmin," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tasmin/chelsa_w5e5_",n,"_tasmin_",year,".shp'")
    systemcommand_pr <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/pr/ghcn_",n,"_",year,"_pr.shp' -GRIDS ",ls_pr," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/pr/chelsa_w5e5_",n,"_pr_",year,".shp'")
    
    system(systemcommand_tas)
    system(systemcommand_tasmax)
    system(systemcommand_tasmin)
    system(systemcommand_pr)

}
    ####################################################
    # SUMMER #
for (n in 3)
{ 
    ls_tas<-c()
    ls_tasmax <-c()
    ls_tasmin <-c()
    ls_pr <-c()
    # ls_rsds <-c()
    for (month in c(months[8], months[9]))
    {
      for (day in 1:31)
      {
        ls_tas<-paste0(ls_tas,";",paste0(TAS,"CHELSA_W5E5_tas_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmin <-paste0(ls_tasmin,";",paste0(TASMIN,"CHELSA_W5E5_tasmin_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_pr <-paste0(ls_pr,";",paste0(PR,"CHELSA_W5E5_pr_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        # ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    for (month in months[7])
    {
      for (day in 1:30)
      {
        ls_tas <-paste0(ls_tas,";",paste0(TAS,"CHELSA_W5E5_tas_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmin <-paste0(ls_tasmin,";",paste0(TASMIN,"CHELSA_W5E5_tasmin_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_pr <-paste0(ls_pr,";",paste0(PR,"CHELSA_W5E5_pr_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        # ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    
    
    ls_tas<-paste0("\"",substring(ls_tas,2),"\"") 
    ls_tasmax<-paste0("\"",substring(ls_tasmax,2),"\"") 
    ls_tasmin<-paste0("\"",substring(ls_tasmin,2),"\"") 
    ls_pr<-paste0("\"",substring(ls_pr,2),"\"") 
    # ls_rsds<-paste0("\"",substring(ls_rsds,2),"\"") 
    
    # systemcommand<-paste0("saga_cmd shapes_grid 0 -SHAPES '/home/harichan/ghcn_2_1981_new.shp' -GRIDS ",ls_tas," -RESULT '/home/harichan/MAM_new",year,".shp'")
    # system(systemcommand)
    
    systemcommand_tas <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tas/ghcn_",n,"_",year,"_tas.shp' -GRIDS ",ls_tas," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tas/chelsa_w5e5_",n,"_tas_",year,".shp'")
    systemcommand_tasmax <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tasmax/ghcn_",n,"_",year,"_tasmax.shp' -GRIDS ",ls_tasmax," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tasmax/chelsa_w5e5_",n,"_tasmax_",year,".shp'")
    systemcommand_tasmin <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tasmin/ghcn_",n,"_",year,"_tasmin.shp' -GRIDS ",ls_tasmin," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tasmin/chelsa_w5e5_",n,"_tasmin_",year,".shp'")
    systemcommand_pr <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/pr/ghcn_",n,"_",year,"_pr.shp' -GRIDS ",ls_pr," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/pr/chelsa_w5e5_",n,"_pr_",year,".shp'")
    # systemcommand_rsds <- paste0("saga_cmd shapes_grid 0 -SHAPES '/home/harichan/ghcn_",n,"_",year,"_rsds.shp' -GRIDS ",ls_rsds," -RESULT '/home/harichan/MAM_rsds",year,".shp'")
    
    system(systemcommand_tas)
    system(systemcommand_tasmax)
    system(systemcommand_tasmin)
    system(systemcommand_pr)
    # system(systemcommand_rsds)
}    
    ####################################################
    # FALL #
for (n in 4)
{    
    ls_tas<-c()
    ls_tasmax <-c()
    ls_tasmin <-c()
    ls_pr <-c()
    # ls_rsds <-c()
    for (month in months[11])
    {
      for (day in 1:31)
      {
        ls_tas<-paste0(ls_tas,";",paste0(TAS,"CHELSA_W5E5_tas_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmin <-paste0(ls_tasmin,";",paste0(TASMIN,"CHELSA_W5E5_tasmin_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_pr <-paste0(ls_pr,";",paste0(PR,"CHELSA_W5E5_pr_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        # ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    for (month in c(months[10], months[12]))
    {
      for (day in 1:30)
      {
        ls_tas <-paste0(ls_tas,";",paste0(TAS,"CHELSA_W5E5_tas_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_tasmin <-paste0(ls_tasmin,";",paste0(TASMIN,"CHELSA_W5E5_tasmin_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        ls_pr <-paste0(ls_pr,";",paste0(PR,"CHELSA_W5E5_pr_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        # ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    
    
    ls_tas<-paste0("\"",substring(ls_tas,2),"\"") 
    ls_tasmax<-paste0("\"",substring(ls_tasmax,2),"\"") 
    ls_tasmin<-paste0("\"",substring(ls_tasmin,2),"\"") 
    ls_pr<-paste0("\"",substring(ls_pr,2),"\"") 
    # ls_rsds<-paste0("\"",substring(ls_rsds,2),"\"") 
    
    # systemcommand<-paste0("saga_cmd shapes_grid 0 -SHAPES '/home/harichan/ghcn_2_1981_new.shp' -GRIDS ",ls_tas," -RESULT '/home/harichan/MAM_new",year,".shp'")
    # system(systemcommand)
    
    systemcommand_tas <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tas/ghcn_",n,"_",year,"_tas.shp' -GRIDS ",ls_tas," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tas/chelsa_w5e5_",n,"_tas_",year,".shp'")
    systemcommand_tasmax <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tasmax/ghcn_",n,"_",year,"_tasmax.shp' -GRIDS ",ls_tasmax," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tasmax/chelsa_w5e5_",n,"_tasmax_",year,".shp'")
    systemcommand_tasmin <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tasmin/ghcn_",n,"_",year,"_tasmin.shp' -GRIDS ",ls_tasmin," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tasmin/chelsa_w5e5_",n,"_tasmin_",year,".shp'")
    systemcommand_pr <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/pr/ghcn_",n,"_",year,"_pr.shp' -GRIDS ",ls_pr," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/pr/chelsa_w5e5_",n,"_pr_",year,".shp'")
    # systemcommand_rsds <- paste0("saga_cmd shapes_grid 0 -SHAPES '/home/harichan/ghcn_",n,"_",year,"_rsds.shp' -GRIDS ",ls_rsds," -RESULT '/home/harichan/MAM_rsds",year,".shp'")
    
    system(systemcommand_tas)
    system(systemcommand_tasmax)
    system(systemcommand_tasmin)
    system(systemcommand_pr)
}   
    




