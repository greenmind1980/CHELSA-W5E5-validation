#!/usr/bin/env python

import numpy as np
import pandas as pd
import xarray
import xarray as xr
import matplotlib.pyplot as plt
import netCDF4 as nc
import scipy
import scipy.integrate as integrate

import matplotlib.pyplot as PLT


# 1. Create seasonal Taylor plots for all variables
#import dataset, if necessary: merge period 1979-2003, group by seasons, export to R and plot Taylor diagram: repeat for all variables

INPUT = '/storage/karger/W5E5/'
OUTPUT ='/storage/harichan/chelsa_V2/W5E5_validation/global/W5E5'

# here the class gets initialized
tas = DataSet(INPUT=INPUT,
              OUTPUT=OUTPUT,
              var='tas')

# tas is now an object of class DataSet, it contains objects and functions

# to get the xarray dataset you can use
tas.ds

# to get the season dataset you can call the function attribute
tas.season

# this gives you nothing as the attribute self.season in the class is set to None when you initialize the class
# to you need to perform a function on the dataset ds first
tas.get_season()

# now the attribute season is set and you can call it again by
tas_season = tas.season

tas_season.to_netcdf(OUTPUT + '_tas' + '_W5E5.nc')

########################################################################################################

# here the class gets initialized

from classes import *
rsds = DataSet(INPUT=INPUT,
              OUTPUT=OUTPUT,
              var='rsds')

# tas is now an object of class DataSet, it contains objects and functions

# to get the xarray dataset you can use
rsds.ds

# to get the season dataset you can call the function attribute
rsds.get_monthly()
rsds.month

# this gives you nothing as the attribute self.season in the class is set to None when you initialize the class
# to you need to perform a function on the dataset ds first
rsds.get_season(rsds.month)

# now the attribute season is set and you can call it again by

rsds.season

rsds.season.to_netcdf(OUTPUT + "_rsds" + "W5E5.nc")
rsds.season.save_netcdf()

#############################################################################################

