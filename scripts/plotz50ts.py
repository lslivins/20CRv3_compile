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
latbound = 14
def getmean(diff,coslats):
    meancoslats = coslats.mean()
    return (coslats*diff).mean()/meancoslats

lats = None
datalist = []
for date in dates:
    #file='/scratch2/scratchdirs/whitaker/%s/%s/pgrbensmeananl_%s' % (expt,date,date)
    file='/project/projectdirs/incite11/ensda_v321/ensda_%s/%s/pgrbanlensmean_%s'%(1995,date,date)
    grbs = pygrib.open(file)
    grb = grbs.select(shortName='gh',level=50)[0]
    data = grb.values
    if lats == None:
        lats, lons = grb.latlons()
        lons1 = lons[0,:]
        lats1 = lats[:,0]
        latnh = lats1.tolist().index(latbound)
        latsh = lats1.tolist().index(-latbound)
        coslats = np.cos((np.pi/180.)*lats)
        coslatssh = coslats[:latsh+1,:]
        coslatsnh = coslats[latnh:,:]
        coslatstr = coslats[latsh:latnh+1,:]
    data_av = getmean(data[latsh:latnh+1,:],coslatstr)
    print date, data_av
    datalist.append(data_av)
    grbs.close()

data = np.array(datalist)
