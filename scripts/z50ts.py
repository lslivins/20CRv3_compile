import pygrib
from dateutils import daterange
import sys
import numpy as np
from numpy import ma
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, cm, addcyclic
date1 = sys.argv[1]
date2 = sys.argv[2]
dates = daterange(date1,date2,6)
expt = sys.argv[3]
var = sys.argv[4]
level = int(sys.argv[5])
datapath = '/scratch1/scratchdirs/whitaker/'+expt
def getmean(diff,coslats):
    meancoslats = coslats.mean()
    return (coslats*diff).mean()/meancoslats
lats=None; rmsnhall=[];rmsshall=[];rmstrall=[];rmsglall=[]
sprdnhall=[];sprdshall=[];sprdtrall=[];sprdglall=[]
latbound = 21.
if var == 'z':
    field1='z'
    field2='gh'
    scale = 9.8066
elif var == 't':
    field1='t'
    field2='t'
    scale = 1.
elif var == 'u':
    field1='u'
    field2='u'
    scale = 1.
elif var == 'v':
    field1='v'
    field2='v'
    scale = 1.
else:
    raise ValueError('var must be u,v,t or z')
yrold = date1[0:4]
grbs_era =\
pygrib.open('/global/scratch2/sd/whitaker/erainterim_%s_%smb_ztuv.grib2' % (yrold,level))
grbs_era_sel = grbs_era.select(shortName=field1,level=level)
#print grbs_era_sel
ncount = 0
for date in dates:
    if date[0:4] != yrold:
        yrold = date[0:4]
        grbs_era.close()
        grbs_era =\
        pygrib.open('/global/scratch2/sd/whitaker/erainterim_%s_%smb_ztuv.grib2' % (yrold,level))
        grbs_era_sel = grbs_era.select(shortName=field1,level=level)
    for grb in grbs_era_sel:
        grbdate = '%06i%04i' % (grb.dataDate,grb.dataTime)
        if grbdate == date+'00':
            z500_oper = grb.values/scale
            break
    try:
        grbs_psonly = pygrib.open(datapath+'/%s/pgrbensmeananl_%s' % (date,date))
        grbs_psonly2 = pygrib.open(datapath+'/%s/pgrbenssprdanl_%s' % (date,date))
    except:
        continue
    try:
        grb = grbs_psonly2.select(shortName=field2,level=level)[0]
    except:
        continue
    z500_psonly_sprd = grb.values  
    try:
        grb = grbs_psonly.select(shortName=field2,level=level)[0]
    except:
        continue
    z500_psonly = grb.values
    if lats is None:
        lats, lons = grb.latlons()
        lons1 = lons[0,:]
        errmean = np.zeros(lats.shape, np.float32)
        biasmean = np.zeros(lats.shape, np.float32)
        sprdmean = np.zeros(lats.shape, np.float32)
        coslats = np.cos((np.pi/180.)*lats)
        lats1 = lats[:,0]
        latnh = lats1.tolist().index(latbound)
        latsh = lats1.tolist().index(-latbound)
        coslatssh = coslats[:latsh+1,:]
        coslatsnh = coslats[latnh:,:]
        coslatstr = coslats[latsh:latnh+1,:]
    grbs_psonly.close(); grbs_psonly2.close()
    err = z500_psonly-z500_oper
    sprd = z500_psonly_sprd
    if (np.abs(err).max() < 1000):
        errmean = errmean + np.abs(err)
        biasmean = biasmean + err
        sprdmean = sprdmean + sprd
        ncount = ncount + 1
    rmssh = np.sqrt(getmean(err[:latsh+1,:]**2,coslatssh))
    rmsnh = np.sqrt(getmean(err[latnh:,:]**2,coslatsnh))
    rmstr = np.sqrt(getmean(err[latsh:latnh+1,:]**2,coslatstr))
    rmsgl = np.sqrt(getmean(err**2,coslats))
    rmsnhall.append(rmsnh)
    rmsshall.append(rmssh)
    rmstrall.append(rmstr)
    rmsglall.append(rmsgl)
    sprdsh = np.sqrt(getmean(sprd[:latsh+1,:]**2,coslatssh))
    sprdnh = np.sqrt(getmean(sprd[latnh:,:]**2,coslatsnh))
    sprdtr = np.sqrt(getmean(sprd[latsh:latnh+1,:]**2,coslatstr))
    sprdgl = np.sqrt(getmean(sprd**2,coslats))
    sprdnhall.append(sprdnh)
    sprdshall.append(sprdsh)
    sprdtrall.append(sprdtr)
    sprdglall.append(sprdgl)
    print '%s %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f' %\
    (date,rmsnh,sprdnh,rmstr,sprdtr,rmssh,sprdsh,rmsgl,sprdgl)
rmsnhall = np.array(rmsnhall)
rmsnh = ma.masked_where(rmsnhall > 1000,rmsnhall).mean()
rmsshall = np.array(rmsshall)
rmssh = ma.masked_where(rmsshall > 1000,rmsshall).mean()
rmstrall = np.array(rmstrall)
rmstr = ma.masked_where(rmstrall > 1000,rmstrall).mean()
rmsglall = np.array(rmsglall)
rmsgl = ma.masked_where(rmsglall > 1000,rmsglall).mean()
print '%s-%s %d %6.2f %6.2f %6.2f %6.2f' % (date1,date2,len(rmsglall),rmsnh,rmstr,rmssh,rmsgl)
sprdnhall = np.array(sprdnhall)
sprdnh = ma.masked_where(sprdnhall > 1000,sprdnhall).mean()
sprdshall = np.array(sprdshall)
sprdsh = ma.masked_where(sprdshall > 1000,sprdshall).mean()
sprdtrall = np.array(sprdtrall)
sprdtr = ma.masked_where(sprdtrall > 1000,sprdtrall).mean()
sprdglall = np.array(sprdglall)
sprdgl = ma.masked_where(sprdglall > 1000,sprdglall).mean()
print '%s-%s %d %6.2f %6.2f %6.2f %6.2f' % (date1,date2,len(sprdglall),sprdnh,sprdtr,sprdsh,sprdgl)
errmean = errmean/ncount
sprdmean = sprdmean/ncount
biasmean = biasmean/ncount
np.savez('z500psonly_sprderr_%s' % expt,errmean=errmean,sprdmean=sprdmean)
#raise SystemExit

plt.figure(figsize=(12,5))
plt.subplot(121)
#map = Basemap(projection='npstere',boundinglat=20.,lon_0=270)
map = Basemap(projection='cyl',lon_0=180)
map.drawcoastlines()
lons,lons2 = addcyclic(lons, lons1)
lons[:,-1]=360
lats,lons2 = addcyclic(lats, lons1)
#x,y = map(lons,lats)
x = lons; y = lats
clevs = np.arange(5,66,5)
#clevs = np.arange(0.2,4,0.2)
data,lons2 = addcyclic(errmean, lons1)
print data.min(), data.max()
CS=map.contourf(x,y,data,clevs,cmap=plt.cm.hot_r,extend='both')
plt.colorbar(CS,shrink=0.6)
plt.title('Z500 Analysis Error %s-%s' % (date1,date2))
plt.savefig('error.png')

plt.subplot(122)
print z500_psonly_sprd.min(), z500_psonly_sprd.max()
map.drawcoastlines()
data,lons2 = addcyclic(sprdmean, lons1)
print data.min(), data.max()
CS=map.contourf(x,y,data,clevs,cmap=plt.cm.hot_r,extend='both')
plt.colorbar(CS,shrink=0.6)
plt.title('Z500 Analysis Spread %s-%s' %  (date1,date2))
plt.savefig('spread.png')

plt.figure()
map.drawcoastlines()
data,lons2 = addcyclic(biasmean, lons1)
print data.min(), data.max()
clevs = np.linspace(-50,50,21)
#clevs = np.linspace(-3,3,21)
CS=map.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both')
plt.colorbar(CS,shrink=0.6)
plt.title('Z500 Analysis Bias %s-%s' %  (date1,date2))
plt.savefig('bias.png')

plt.show()
