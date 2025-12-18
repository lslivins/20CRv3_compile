import numpy as np
import matplotlib.pyplot as plt
from dateutils import daterange
from mpl_toolkits.basemap import Basemap, cm
import sys

#yr = sys.argv[1]
#stream = repr(int(yr)-5)
yr = '2000'
date1='%s010100' % yr
date2='%s123118' % yr
date1 = '1999110100'
date2 = '1999123118'
dates = daterange(date1,date2,6)

# stringout = '%-19s %3i %01s %7.2f %6.2f %5i %6.2f %7.1f %7.1f %10.3e %5.2f %30s %13s' % (obid,nceptype,obtype,longitude,latitude,elevation,analtime_offset,ob,slpob,bias,pserr,statname,statid)
biasmean={}; biascount={}; biasts=[]; times=[]; elevdiff={}
keysave = ' 240.30  36.78   103 181'
for date in dates:
    file='/scratch1/scratchdirs/whitaker/gfsenkf_t126_1999iau_biascor9/%s/psobs.txt' % date
    print date
    for line in open(file):
        linesplit = line.split()
        bias = float(linesplit[9])
        oblon = float(linesplit[3])
        oblat = float(linesplit[4])
        dateob = linesplit[0][0:12]
        elev = int(linesplit[5])
        obtyp = float(linesplit[1])
        #badflag = int(linesplit[16])
        if obtyp == 180 or obtyp > 200: continue
        #if badflag: continue
        key = '%7.2f %6.2f %5i %3i' % (oblon,oblat,elev,obtyp)
        if key == keysave:
            #print dateob, bias
            biasts.append(bias)
            times.append(dateob[0:10])
        #if bias < 1.e10:
        if bias > 1.e10: bias = 0.0
        if not biasmean.has_key(key):
            biasmean[key]=0.
            biascount[key]=0
        biasmean[key] = biasmean[key] + bias
        biascount[key] = biascount[key] + 1

lats = []; lons = []; bias = []; zdiff=[]
latsx = []; lonsx = []; biasx = []
for key in biasmean.keys():
    biasmean[key] = biasmean[key]/biascount[key]
    lat = float(key.split()[1])
    lon = float(key.split()[0])
    if lon > 180: lon = lon - 360.
    if biascount[key] < 60: continue
    print key, biasmean[key], biascount[key]
    lats.append(lat)
    lons.append(lon)
    bias.append(biasmean[key])
    if np.abs(biasmean[key]) < 0.1:
        print 'no bias', key, biasmean[key], biascount[key]
        latsx.append(lat); lonsx.append(lon); biasx.append(biasmean[key])

lats = np.array(lats)
lons = np.array(lons)
bias = np.array(bias)
absbias = np.abs(bias)
indx = np.argsort(absbias)
bias = bias[indx]
lats = lats[indx]
lons = lons[indx]

m = Basemap(lon_0=0)
m.drawcoastlines()
m.drawcountries()
if len(lonsx):
    #cm=m.scatter(lonsx,latsx,s=5,marker='o',facecolors='none',edgecolors='k',linewidths=0.5,alpha=1,zorder=3)
    cm=m.scatter(lonsx,latsx,s=5,marker='o',c='w',linewidths=0.5,zorder=2)
cm=m.scatter(lons,lats,c=bias,s=5,marker='o',edgecolors='none',cmap=plt.cm.bwr,zorder=3,vmin=-6,vmax=6)
#m.colorbar(orientation='vertical',shrink=0.75)
plt.title('new 20CR bias for %s' % yr)
plt.savefig('%s_bias_biascor9.png' % yr)
plt.show()
