
import numpy as np
import pygrib 
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, addcyclic
from dateutils import daterange
import netCDF4

date1 = '2000081212'
date2 = '2000081212'

expt = 'gfsenkf_t126_1999iau_biascor10'
expt2 = 'gfsenkf_t126_1999iau_biascor9'

dates = daterange(date1,date2,6)
data1 = None
for date in dates:
    file1='/scratch2/scratchdirs/whitaker/%s/%s/sflxgrbensmeanfg_%s_fhr06' % (expt,date,date)
    file2='/scratch2/scratchdirs/whitaker/%s/%s/sflxgrbensmeanfg_%s_fhr06' % (expt2,date,date)
    print file1
    print file2
    file_nc1='/project/projectdirs/incite11/nwpara_20080412/fix/cfs_ice1x1monclim19822001.nc'
    print file_nc1
    
    grbs1 = pygrib.open(file1)
    grbs2 = pygrib.open(file2)
    
    file1 = netCDF4.Dataset(file_nc1)
    
    #grb1 = grbs1.select(shortName='prate',level=0)[0]
    #grb2 = grbs2.select(shortName='prate',level=0)[0]
    grb1 = grbs1.select(shortName='icec')[0]
    grb2 = grbs2.select(shortName='icec')[0]
    
    icec = file1.variables['ICE_C_GDS0_SFC_51'][:]
    latc = file1.variables['g0_lat_1'][:]
    lonc = file1.variables['g0_lon_2'][:]
    
    loncs,latcs = np.meshgrid(lonc ,latc)
    print icec.shape
    
    icemap = icec[0]
    #grb1 = grbs1[81]
    #grb2 = grbs2[83]
    #grb1 = grbs1.select(shortName='pwat')[0]
    #grb2 = grbs2.select(shortName='pwat')[0]
    #grb1 = grbs1.select(shortName='tcc',indicatorOfTypeOfLevel=234)[0]
    #grb2 = grbs2.select(shortName='tcc',indicatorOfTypeOfLevel=234)[0]
    #grb1 = grbs1.select(shortName='tcc')[0]
    #grb2 = grbs2.select(shortName='tcc')[0]
    
    if data1 is None:
        data1 = grb1.values/len(dates)
        data2 = grb2.values/len(dates)
    else:
        data1 = data1 + grb1.values/len(dates)
        data2 = data2 + grb2.values/len(dates)
    print data1.shape, data2.shape
    
    print data1.min(), data1.max()
    print data2.min(), data2.max()
    grbs1.close(); grbs2.close()

diff = data2-data1
print diff.min(), diff.max()
lats, lons = grb1.latlons()
coslats = np.cos((np.pi/180.)*lats)
print 'data2 global mean',(data2*coslats).sum()/coslats.sum()
print 'data1 global mean',(data1*coslats).sum()/coslats.sum()
print 'data2-data1 global mean',(diff*coslats).sum()/coslats.sum()
lons1 = lons[0,:]
lats1 = lats[:,0]



plt.figure()
map = Basemap(projection='cyl',lon_0=180)
#map = Basemap(projection='spstere',boundinglat=-60,lon_0=270)
map.drawcoastlines()

x,y = map(loncs,latcs)
#x = lons; y = lats
clevs = 20
#clevs = np.arange(-200,201,10)
print icemap.min(), icemap.max()
#CS=map.contourf(x,y,icemap,clevs,cmap=plt.cm.RdBu_r,extend='both')
CS=map.pcolormesh(x,y,icemap,cmap=plt.cm.RdBu_r,vmin=0,vmax=1.1,shading='flat')
plt.title('CFS Clim icec')
#map.fillcontinents()
plt.colorbar(CS,shrink=0.6)

plt.figure()
map = Basemap(projection='cyl',lon_0=180)
#map = Basemap(projection='spstere',boundinglat=-60,lon_0=270)
map.drawcoastlines()
lons,lons2 = addcyclic(lons, lons1)
lons[:,-1]=360
lats,lons2 = addcyclic(lats, lons1)
x,y = map(lons,lats)
#x = lons; y = lats
clevs = 20
#clevs = np.arange(-200,201,10)
data,lons2 = addcyclic(diff,lons1)
print data.min(), data.max()
CS=map.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both',shading='flat')
map.fillcontinents()
plt.title('data2-data1')
plt.colorbar(CS,shrink=0.6)

plt.figure()
map.drawcoastlines()
data,lons2 = addcyclic(data1,lons1)
print data.min(), data.max()
#CS=map.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both')
CS=map.pcolormesh(x,y,data,cmap=plt.cm.RdBu_r,vmin=0,vmax=1.1,shading='flat')
plt.title('data1')
#map.fillcontinents()
plt.colorbar(CS,shrink=0.6)




plt.figure()
map.drawcoastlines()
clevs = 20
data,lons2 = addcyclic(data2,lons1)
print data.min(), data.max()
#CS=map.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both')
CS=map.pcolormesh(x,y,data,cmap=plt.cm.RdBu_r,vmin=0,vmax=1.1)
plt.title('data2')
#map.fillcontinents()
plt.colorbar(CS,shrink=0.6)

plt.show()
