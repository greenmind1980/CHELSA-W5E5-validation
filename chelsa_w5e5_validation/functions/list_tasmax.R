months<-c(c(12,1,2),c(3,4,5),c(6,7,8),c(9,10,11))



TASMAX<-"/storage/karger/chelsa_V2/W5E5/tasmax/"



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
    

    ls_tasmax <-c()
  
    
    for (month in months[1])
    {
      for (day in 1:31)
      {
 
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year-1,"_V.1.0.tif"))

      }}
    for (month in months[2])
    {
      for (day in 1:31)
      {
    
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
    
      }}
    
    for (month in months[3])
    {
      if((year %% 4) == 0) {
        if((year %% 100) == 0) {
          if((year %% 400) == 0) {
            for (day in 1:29)
            {
 
              ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
     
            }
          } else {
            for (day in 1:28)
            {
             
              ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))

            }
          }
        } else {
          for (day in 1:29)
          {
 
            ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
 
          }
        }
      } else {
        for (day in 1:28)
        {
 
          ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
 
        }
      }
    }
    
    
  
    ls_tasmax<-paste0("\"",substring(ls_tasmax,2),"\"") 

    
    # systemcommand<-paste0("saga_cmd shapes_grid 0 -SHAPES '/home/harichan/ghcn_2_1981_new.shp' -GRIDS ",ls_tas," -RESULT '/home/harichan/MAM_new",year,".shp'")
    # system(systemcommand)
    
 
    systemcommand_tasmax <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tasmax/ghcn_",n,"_",year,"_tasmax.shp' -GRIDS ",ls_tasmax," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tasmax/chelsa_w5e5_",n,"_tasmax_",year,".shp'")
  
    
  
    system(systemcommand_tasmax)

}
    ####################################################
    # SPRING #
for (n in 2)
{   
 
    ls_tasmax <-c()
  
    for (month in c(months[4], months[6]))
    {
      for (day in 1:31)
      {
    
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
    
      }}
    for (month in months[5])
    {
      for (day in 1:30)
      {
        
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
     
      }}
    
  
    ls_tasmax<-paste0("\"",substring(ls_tasmax,2),"\"") 
  
    
    # systemcommand<-paste0("saga_cmd shapes_grid 0 -SHAPES '/home/harichan/ghcn_2_1981_new.shp' -GRIDS ",ls_tas," -RESULT '/home/harichan/MAM_new",year,".shp'")
    # system(systemcommand)
    
  
    systemcommand_tasmax <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tasmax/ghcn_",n,"_",year,"_tasmax.shp' -GRIDS ",ls_tasmax," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tasmax/chelsa_w5e5_",n,"_tasmax_",year,".shp'")
  

    system(systemcommand_tasmax)


}
    ####################################################
    # SUMMER #
for (n in 3)
{ 
 
    ls_tasmax <-c()

    for (month in c(months[8], months[9]))
    {
      for (day in 1:31)
      {
     
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
   
      }}
    for (month in months[7])
    {
      for (day in 1:30)
      {
       
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
    
      }}
    
    

    ls_tasmax<-paste0("\"",substring(ls_tasmax,2),"\"") 
  
    
    # systemcommand<-paste0("saga_cmd shapes_grid 0 -SHAPES '/home/harichan/ghcn_2_1981_new.shp' -GRIDS ",ls_tas," -RESULT '/home/harichan/MAM_new",year,".shp'")
    # system(systemcommand)
    
  
    systemcommand_tasmax <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tasmax/ghcn_",n,"_",year,"_tasmax.shp' -GRIDS ",ls_tasmax," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tasmax/chelsa_w5e5_",n,"_tasmax_",year,".shp'")
   
   
   
    system(systemcommand_tasmax)
 }    
    ####################################################
    # FALL #
for (n in 4)
{    

    ls_tasmax <-c()

    for (month in months[11])
    {
      for (day in 1:31)
      {
        
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
        
      }}
    for (month in c(months[10], months[12]))
    {
      for (day in 1:30)
      {
       
        ls_tasmax <-paste0(ls_tasmax,";",paste0(TASMAX,"CHELSA_W5E5_tasmax_",sprintf("%02d", day),"_",sprintf("%02d", month),"_",year,"_V.1.0.tif"))
  
      }}
    
    

    ls_tasmax<-paste0("\"",substring(ls_tasmax,2),"\"") 

    
    # systemcommand<-paste0("saga_cmd shapes_grid 0 -SHAPES '/home/harichan/ghcn_2_1981_new.shp' -GRIDS ",ls_tas," -RESULT '/home/harichan/MAM_new",year,".shp'")
    # system(systemcommand)
    
   
    systemcommand_tasmax <- paste0("saga_cmd shapes_grid 0 -SHAPES '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/ghcn_shapefiles/tasmax/ghcn_",n,"_",year,"_tasmax.shp' -GRIDS ",ls_tasmax," -RESULT '/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5/chelsa_w5e5_shapefiles/tasmax/chelsa_w5e5_",n,"_tasmax_",year,".shp'")
    

    system(systemcommand_tasmax)
 
}   
    




