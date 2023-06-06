months<-c(c(12,1,2),c(3,4,5),c(6,7,8),c(9,10,11))

RSDS <- "/storage/karger/chelsa_V2/W5E5/rsds/"

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
    

    ls_rsds <-c()
    
    for (month in months[1])
    {
      for (day in 1:31)
      {
      ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year-1,"_V.1.0.tif"))
      }}
    
    for (month in months[2])
    {
      for (day in 1:31)
      {
        ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    
    for (month in months[3])
    {
      if((year %% 4) == 0) {
        if((year %% 100) == 0) {
          if((year %% 400) == 0) {
            for (day in 1:29)
            {

              ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
            }
          } else {
            for (day in 1:28)
            {

              ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
            }
          }
        } else {
          for (day in 1:29)
          {
          ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
          }
        }
      } else {
        for (day in 1:28)
        {

          ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        }
      }
    }
    
    ls_rsds<-paste0("\"",substring(ls_rsds,2),"\"")
 
    systemcommand_rsds <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/geba_shapefiles/rsds/geba_",n,"_",year,"_rsds.shp' -GRIDS ",ls_rsds," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/rsds/chelsa_",n,"_rsds_",year,".shp'")
    
    system(systemcommand_rsds)
}
    ####################################################
    # SPRING #
for (n in 2)
{    
    ls_rsds <-c()
    
    for (month in c(months[4], months[6]))
    {
      for (day in 1:31)
      {
 
        ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    for (month in months[5])
    {
      for (day in 1:30)
      {

        ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    
     ls_rsds<-paste0("\"",substring(ls_rsds,2),"\"")
    
   
     systemcommand_rsds <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/geba_shapefiles/rsds/geba_",n,"_",year,"_rsds.shp' -GRIDS ",ls_rsds," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/rsds/chelsa_",n,"_rsds_",year,".shp'")
     
    system(systemcommand_rsds)
}    
    ####################################################
    # SUMMER #
for (n in 3)
{    
    ls_rsds <-c()
    for (month in c(months[8], months[9]))
    {
      for (day in 1:31)
      {
        ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    for (month in months[7])
    {
      for (day in 1:30)
      {

        ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    
    ls_rsds<-paste0("\"",substring(ls_rsds,2),"\"")
  
    systemcommand_rsds <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/geba_shapefiles/rsds/geba_",n,"_",year,"_rsds.shp' -GRIDS ",ls_rsds," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/rsds/chelsa_",n,"_rsds_",year,".shp'")
    
  
    system(systemcommand_rsds)
}    
    ####################################################
    # FALL #
for (n in 4)
{    
    ls_rsds <-c()
    
    for (month in months[11])
    {
      for (day in 1:31)
      {

        ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    for (month in c(months[10], months[12]))
    {
      for (day in 1:30)
      {
         ls_rsds <-paste0(ls_rsds,";",paste0(RSDS,"CHELSA_W5E5_rsds_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
      }}
    
  
    ls_rsds<-paste0("\"",substring(ls_rsds,2),"\"")
    
    systemcommand_rsds <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/geba_shapefiles/rsds/geba_",n,"_",year,"_rsds.shp' -GRIDS ",ls_rsds," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/rsds/chelsa_",n,"_rsds_",year,".shp'")
    
    system(systemcommand_rsds)

}


