import numpy as np
import pygrib 
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, addcyclic
from dateutils import daterange
import sys

expt = 'gfsenkf_t126_1999iau_biascor9'
date1 = sys.argv[1]
date2 = sys.argv[2]
dates = daterange(date1,date2,6)
varname = '10u'

lats = None
for date in dates:
    #file='/scratch2/scratchdirs/whitaker/%s/%s/sflxgrbensmeanfg_%s_fhr00' % (expt,date,date)
    file='/project/projectdirs/incite11/ensda_v321/ensda_%s/%s/sflxgrbfgensmean_%s_fhr06'%(2000,date,date)
    grbs = pygrib.open(file)
    grb = grbs.select(shortName=varname)[0]
    datain = grb.values
    print date,datain.min(), datain.max()
    if lats == None:
        lats, lons = grb.latlons()
        data = datain/len(dates)
        lons1 = lons[0,:]
        lats1 = lats[:,0]
    else:
        data = data + datain/len(dates)
    grbs.close()

plt.figure()
m = Basemap(projection='cyl',lon_0=180)
m.drawcoastlines()
lons,lons2 = addcyclic(lons, lons1)
lons[:,-1]=360
lats,lons2 = addcyclic(lats, lons1)
x,y = m(lons,lats)
clevs = np.linspace(-12,12,21)
#clevs = 15
data,lons2 = addcyclic(data,lons1)
print data.min(), data.max()
CS=m.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both')
plt.title('20CR %s mean %s-%s' % (varname,date1,date2))
plt.colorbar(CS,shrink=0.6)

plt.show()
