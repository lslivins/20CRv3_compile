import numpy as np
import pygrib 
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, addcyclic
from dateutils import daterange
import sys

expt = 'gfsenkf_t126_1999iau_biascor3'
date1 = sys.argv[1]
date2 = sys.argv[2]
hrinc = int(sys.argv[3])
dates = daterange(date1,date2,hrinc)

lats = None
for date in dates:
    filefg='/scratch1/scratchdirs/whitaker/%s/%s/pgrbensmeanfg_%s_fhr06' % (expt,date,date)
    fileanl='/scratch1/scratchdirs/whitaker/%s/%s/pgrbensmeananl_%s' % (expt,date,date)
    grbsfg = pygrib.open(filefg)
    grbsanl = pygrib.open(fileanl)
    grbfg = grbsfg.select(shortName='gh',level=500)[0]
    grbanl = grbsanl.select(shortName='gh',level=500)[0]
    psinc = grbanl.values - grbfg.values
    print date,psinc.min(), psinc.max()
    if lats == None:
        lats, lons = grbfg.latlons()
        data = psinc/len(dates)
        lons1 = lons[0,:]
        lats1 = lats[:,0]
    else:
        data = data + psinc/len(dates)
    grbsfg.close(); grbsanl.close()

plt.figure()
m = Basemap(projection='cyl',lon_0=180)
m.drawcoastlines()
lons,lons2 = addcyclic(lons, lons1)
lons[:,-1]=360
lats,lons2 = addcyclic(lats, lons1)
x,y = m(lons,lats)
clevs = np.linspace(-25,25,21)
data,lons2 = addcyclic(data,lons1)
print data.min(), data.max()
CS=m.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both')
plt.title('z500 inc (A-F) %s-%s' % (date1,date2))
plt.colorbar(CS,shrink=0.6)

plt.show()
