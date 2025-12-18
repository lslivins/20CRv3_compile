import numpy as np
import pygrib 
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, addcyclic
from dateutils import daterange
import sys


expt = 'gfsenkf_t126_1999iau_nobiascor'
#date = '1999090200'
date = sys.argv[1]

filefg='/scratch1/scratchdirs/whitaker/%s/%s/pgrbensmeanfg_%s_fhr06' % (expt,date,date)
fileanl='/scratch1/scratchdirs/whitaker/%s/%s/pgrbensmeananl_%s' % (expt,date,date)
grbsfg = pygrib.open(filefg)
grbsanl = pygrib.open(fileanl)
    
#grbfg = grbsfg.select(shortName='msl')[0]
#grbanl = grbsanl.select(shortName='msl')[0]
grbfg = grbsfg.select(shortName='sp')[0]
grbanl = grbsanl.select(shortName='sp')[0]
    
lats, lons = grbfg.latlons()
data = 0.01*(grbanl.values - grbfg.values)
lons1 = lons[0,:]
lats1 = lats[:,0]

obfile='/scratch1/scratchdirs/whitaker/%s/%s/psobs_posterior.txt' % (expt,date)
oblons = []; oblats = []
nskip = 0
for line in open(obfile):
    linesplit = line.split()
    lon = float(linesplit[2]) 
    lat = float(linesplit[3]) 
    skip = int(linesplit[6]) 
    oberr = float(linesplit[13])
    if not skip and oberr < 99.99:
       oblons.append(lon); oblats.append(lat)
    else:
       nskip = nskip + 1
    
print len(oblons),nskip

plt.figure()
#m = Basemap(projection='cyl',lon_0=180)
m = Basemap(projection='npstere',lon_0=270,boundinglat=30)
m.drawcoastlines()
lons,lons2 = addcyclic(lons, lons1)
lons[:,-1]=360
lats,lons2 = addcyclic(lats, lons1)
x,y = m(lons,lats)
clevs = np.linspace(-3,3,21)
data,lons2 = addcyclic(data,lons1)
print data.min(), data.max()
CS=m.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both')
obx,oby = m(oblons,oblats)
m.scatter(obx,oby,0.5,marker='o',color='k',zorder=10)
plt.title('ps inc (A-F) %s' % date)
plt.colorbar(CS,shrink=0.6)

plt.figure()
m = Basemap(projection='spstere',lon_0=270,boundinglat=-30)
m.drawcoastlines()
x,y = m(lons,lats)
clevs = np.linspace(-3,3,21)
CS=m.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both')
obx,oby = m(oblons,oblats)
m.scatter(obx,oby,0.5,marker='o',color='k',zorder=10)
plt.title('ps inc (A-F) %s' % date)
plt.colorbar(CS,shrink=0.6)

plt.figure(figsize=(12,3.5))
m = Basemap(projection='merc',urcrnrlat=30,llcrnrlat=-30,urcrnrlon=360,llcrnrlon=0)
m.drawcoastlines()
x,y = m(lons,lats)
clevs = np.linspace(-1.5,1.5,21)
CS=m.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both')
obx,oby = m(oblons,oblats)
m.scatter(obx,oby,0.5,marker='o',color='k',zorder=10)
plt.title('ps inc (A-F) %s' % date)
plt.colorbar(CS,shrink=0.6)

plt.show()
