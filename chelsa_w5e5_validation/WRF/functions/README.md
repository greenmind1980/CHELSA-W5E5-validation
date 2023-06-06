The data generated with the scripts in this directory only cover the extent of North America. 

**I. get_ghcn_daily**: Extract daily GHCN-D data for precipitation and temperature for the period available in WRF (10.2000-09.2013)

**II. wrf_chelsa_validation**: Extract Chelsa_W5E5, Chelsa_v2, ghcn-shapefile and WRF data and create daily dataframe with all data. 

Then use: find $OUTPUT. -name "*.txt" | xargs -n 1 tail -n +2 >> /storage/harichan/chelsa_V2/W5E5_validation/NA_WRF/",variable,"_all_10_2000_09_2013.txt to create one big dataframe including the entire timespan. 

Based on this output file the taylor diagrams can be generated (figures/functions/figure_TD_Chelsa_W5e5-v2.1-wrf_",variable".R)

To generate scatterplots and score compariosn tables for Northern America only, the W5E5 data had to be extracted for the extent of North America. For this purpose, the following script was used: 

**III. extract_w5e5**: add w5e5 to the daily dataframe and create again daily .txt -files including the data from Chelsa_W5E5, Chelsa_V2, GHCN, WRF and W5E5 data. This data can then be used for all analyisis and the scatterplots (figures/functions/fig_scatterplots.R) only covering North America.

Then use: find $OUTPUT. -name "*.txt" | xargs -n 1 tail -n +2 >> /storage/harichan/chelsa_V2/WRF/w5e5_wrf_all_",i,".txt" --> to create one big dataframe including the entire timespan 



