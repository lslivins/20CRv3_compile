import numpy as np
import pygrib 
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, addcyclic
from dateutils import daterange
import sys

expt = 'gfsenkf_t126_1999iau_biascor10'
expt2 = 'gfsenkf_t126_1999iau_biascor15'
date1 = sys.argv[1]
date2 = sys.argv[2]
dates = daterange(date1,date2,6)

lats = None
for date in dates:
    file='/scratch2/scratchdirs/whitaker/%s/%s/sflxgrbensmeanfg_%s_fhr06' % (expt,date,date)
    file2='/scratch2/scratchdirs/whitaker/%s/%s/sflxgrbensmeanfg_%s_fhr06' % (expt2,date,date)
   
    #file='/project/projectdirs/incite11/ensda_v338/ensda_%s/%s/sflxgrbfgensmean_%s_fhr06'%(2000,date,date)
    #file2='/project/projectdirs/incite11/ensda_v321/ensda_%s/%s/sflxgrbfgensmean_%s_fhr06'%(2000,date,date)

    grbs = pygrib.open(file)
    grb = grbs.select(shortName='t',level=0)[0]
    print grb
    datain = grb.values
    grbs.close()

    grbs = pygrib.open(file2)
    grb = grbs.select(shortName='t',level=0)[0]
    print grb
    datain2 = grb.values
    print date, datain.min(),datain.max(),datain2.min(),datain2.max()

    if lats == None:
        lats, lons = grb.latlons()
        data = (datain-datain2)/len(dates)
        lons1 = lons[0,:]
        lats1 = lats[:,0]
    else:
        data = data + (datain-datain2)/len(dates)
    
    grbs.close()

plt.figure()
m = Basemap(projection='cyl',lon_0=180)
m.drawcoastlines()

lons,lons2 = addcyclic(lons, lons1)
lons[:,-1]=360
lats,lons2 = addcyclic(lats, lons1)
x,y = m(lons,lats)
clevs = np.linspace(-2,2,21)
clevs = np.linspace(-1,1,21)
data,lons2 = addcyclic(data,lons1)
print data.min(), data.max()
CS=m.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both')
#m.fillcontinents()
plt.title('sfct diff %s-%s' % (date1,date2))
plt.colorbar(CS,shrink=0.6)

plt.show()
