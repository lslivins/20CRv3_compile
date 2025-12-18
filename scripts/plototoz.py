import numpy as np
import pygrib 
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, addcyclic
from dateutils import daterange
import sys

def getmean(diff,coslats):
    meancoslats = coslats.mean()
    return (coslats*diff).mean()/meancoslats


date1 = sys.argv[1]
date2 = sys.argv[2]
expt = sys.argv[3]
coslats = None

for date in daterange(date1,date2,6):
    file='/lustre/f1/unswept/Jeffrey.S.Whitaker/%s/%s/pgrbensmeananl_%s' % (expt,date,date)
    try:
        grbs = pygrib.open(file)
    except:
        continue
    dateout = date
    # total column ozone
    grb = grbs.select(shortName='tco3')[0]
    data = grb.values
    # 10mb O3
    #grb = grbs[100] # 10 mb ozone
    #data = grb.values
    #data = data/1.665e-6 # convert to ppmv
    # 10 mb T
    #grb = grbs.select(shortName='t',level=10)[0]
    #data = grb.values

    if coslats is None:
        lats, lons = grb.latlons()
        lons1 = lons[0,:]
        lats1 = lats[:,0]
        coslats = np.cos((np.pi/180.)*lats)

    print date,data.min(),data.max(), getmean(data,coslats)

plt.figure()
m = Basemap(projection='cyl',lon_0=180)
m.drawcoastlines()
lons,lons2 = addcyclic(lons, lons1)
lons[:,-1]=360
lats,lons2 = addcyclic(lats, lons1)
x,y = m(lons,lats)
clevs = 21
data,lons2 = addcyclic(data,lons1)
print data.min(), data.max()
CS=m.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both')
plt.title('total column ozone %s' % dateout)
plt.colorbar(CS,shrink=0.6)
plt.savefig('tco3.png')

plt.show()
