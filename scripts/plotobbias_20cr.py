import numpy as np
import matplotlib.pyplot as plt
from dateutils import daterange
from mpl_toolkits.basemap import Basemap, cm
import sys

#yr = sys.argv[1]
#stream = repr(int(yr)-5)
yr = '2000'
stream = '1995'
date1='%s010100' % yr
date2='%s123118' % yr
dates = daterange(date1,date2,6)

biasmean={}; biascount={}; biasts=[]; times=[]; elevdiff={}
keysave = ' 240.30  36.78   103 181'
for date in dates:
    file='/project/projectdirs/incite11/ensda_v321/ensda_%s/%s/prepbufrobs_assim_%s.txt'%(stream,date,date)
    for line in open(file):
        linesplit = line.split()
        bias = float(linesplit[11])
        oblon = float(linesplit[3])
        oblat = float(linesplit[4])
        dateob = linesplit[0][0:12]
        elev = int(linesplit[5])
        elev2 = int(linesplit[6])
        obtyp = float(linesplit[1])
        badflag = int(linesplit[16])
        if obtyp == 180 or obtyp > 200: continue
        if badflag: continue
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
            elevdiff[key] = elev-elev2
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
    print key, biasmean[key], biascount[key], elevdiff[key]
    lats.append(lat)
    lons.append(lon)
    bias.append(biasmean[key])
    zdiff.append(elevdiff[key])
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
    cm=m.scatter(lonsx,latsx,s=5,marker='o',c='w',linewidths=0.25,zorder=2)
cm=m.scatter(lons,lats,c=bias,s=5,marker='o',edgecolors='none',cmap=plt.cm.bwr,zorder=3,vmin=-6,vmax=6)
plt.colorbar(orientation='horizontal')
plt.title('20CR bias for %s' % yr)
plt.savefig('%s_bias_20cr.png' % yr)
raise SystemExit

dates = daterange(date1,date2,1)
biasts2 = []; times2 = []
for date in dates:
    if date in times:
        n = times.index(date)
        biasts2.append(biasts[n])
    else:
        biasts2.append(np.nan)

#for date,bias in zip(dates,biasts2):
#    print date,bias

plt.figure()
plt.plot(np.arange(len(dates)), biasts2)
plt.title(keysave)

plt.figure()
plt.scatter(zdiff,bias,c='k',marker='o',s=5)
plt.xlim(-1000,1000)
plt.ylim(-10,10)
plt.title('elevation difference versus bias correction')
plt.show()
