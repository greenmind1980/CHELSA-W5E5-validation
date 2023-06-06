#!/usr/bin/env python

import xarray as xr


class DataSet:
    def __init__(self, INPUT, OUTPUT, var):
        self.INPUT = INPUT
        self.OUTPUT = OUTPUT
        self.var = var
        self.ds = xr.open_mfdataset([INPUT + self.var + '_W5E5v1.0_19790101-19801231.nc',
                                     INPUT + self.var + '_W5E5v1.0_19810101-19901231.nc',
                                     INPUT + self.var + '_W5E5v1.0_19910101-20001231.nc',
                                     INPUT + self.var + '_W5E5v1.0_20010101-20101231.nc',
                                     INPUT + self.var + '_W5E5v1.0_20110101-20161231.nc'])
        self.month = None
        self.season = None

    def get_monthly(self):
        setattr(self, 'month', self.ds.groupby('time.month').mean('time'))

    def get_season(self,ds):
        getattr(self, ds).groupby('time.season').mean('time')

    def save_netcdf(self, ds):
        getattr(self, ds).to_netcdf(self.INPUT + self.var + '_W5E5.nc')


