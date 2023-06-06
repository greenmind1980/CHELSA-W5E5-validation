**get_seasonal_w5e5**
    Python script + class (get_seasonal_w5e5_class) to extract and aggregate w5e5 data to netcdf file containing seasonal data for each variable.

#########################################

***FOR VARIABLES: TAS, TASMIN, TASMAX, PR***

**get_ghcn**
    Extract GHCN-D grouped by seasons, aggregate over latitude, longitude and month and transform to SpatialPointsDataFrame, subset by year and save shapefile for each variable, season and year (), where n: 1 = DJF, 2 = MAM, 3 = JJA, 4 = SON

**chelsa_w5e5_list_all**
    Create a list with all daily files for each season and variable. The script is divided into the individual season to guarantee the extraction of the correct files, considering leap years and the different lengths of the months. At the end, a system command is set to stack the daily .tif-files to seasonal shapefiles for each variable in saga_gis. 

**chelsa_w5e5_merge_results**
    Read the in the last step created chelsa_w5e5 shapefiles. The seasonal mean is calculated and the units are converted respectively. The unused columns are cut to only display the relevant inforamtion. --> Taylor Diagram

 
***FOR VARIABLES: RSDS***

GHCN-D for rsds is not available. Thus, monthly data from GEBA were used. The shapefiles based on the GEBA data were generated in the script **rsds_chelsa_w5e5_shapefiles**.
Analogously to the scripts for the other files, the data was listed for each season and variable and thus prepared for saga_gis in the file **rsds_chelsa_w5e5_list_all** and the resulting shapefiles were merged and thus, prepared to plot Taylor Diagrams in script **rsds_chelsa_w5e5_merge_results**

#########################################

The above created scripts contain the global data seasonally aggregated. However, for the creation of the scatterplot as well as the score comparison tables, daily data was needed without any seasonal aggregation. Thus, the ghcn data was again extracted daily and merged with the chelsa_w5e5 data, as well as the w5e5 data. 

All output data can be found in the following direcotry: /storage/harichan/chelsa_V2/W5E5_validation/global_scatterplot_and_table/

For this purpose, the following three scripts were created: 

**get_daily_ghcn.R**
    Script to extract daily ghcn data (not seasonally aggregated)
    
**merge_chelsa_ghcn_daily** 
    Script written to merge daily and global GHCN-D, CHELSA_W5E5, CHELSA_V2 (not seasonally aggregated)

**extract_w5e5_global**
    This script adds the W5E5 data to the global dataset. 
