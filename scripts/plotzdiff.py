import numpy as np
import pygrib 
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, addcyclic
from dateutils import daterange
import sys


expt1 = 'gfsenkf_20crV3_predoz'
expt2 = 'gfsenkf_20crV3_cmip5oz'
date = '1999100900'

filefg='/lustre/f1/unswept/Jeffrey.S.Whitaker/%s/%s/pgrbensmeananl_%s' % (expt1,date,date)
fileanl='/lustre/f1/unswept/Jeffrey.S.Whitaker/%s/%s/pgrbensmeananl_%s' % (expt2,date,date)
print filefg
print fileanl
grbsfg = pygrib.open(filefg)
grbsanl = pygrib.open(fileanl)
    
grbfg = grbsfg.select(shortName='t',level=50)[0]
grbanl = grbsanl.select(shortName='t',level=50)[0]
    
lats, lons = grbfg.latlons()
data = grbanl.values - grbfg.values
lons1 = lons[0,:]
lats1 = lats[:,0]

plt.figure()
m = Basemap(projection='cyl',lon_0=180)
m.drawcoastlines()
lons,lons2 = addcyclic(lons, lons1)
lons[:,-1]=360
lats,lons2 = addcyclic(lats, lons1)
x,y = m(lons,lats)
clevs = np.linspace(-100,100,21)
clevs = 21
data,lons2 = addcyclic(data,lons1)
print data.min(), data.max()
CS=m.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both')
plt.title('z diff (%s - %s) %s' % (expt2,expt1,date))
plt.colorbar(CS,shrink=0.6)
plt.savefig('zdiff.png')

plt.show()
