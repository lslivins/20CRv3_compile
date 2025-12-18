import pygrib
from dateutils import daterange
import sys
import numpy as np
import matplotlib.pyplot as plt
from netCDF4 import Dataset
from mpl_toolkits.basemap import Basemap, cm, addcyclic
date1 = sys.argv[1]
date2 = sys.argv[2]
dates = daterange(date1,date2,6)
#expt = sys.argv[3]
var = sys.argv[3]
level = int(sys.argv[4])
def getmean(diff,coslats):
    meancoslats = coslats.mean()
    return (coslats*diff).mean()/meancoslats
lats=None; rmsnhall=[];rmsshall=[];rmstrall=[];rmsglall=[]
sprdnhall=[];sprdshall=[];sprdtrall=[];sprdglall=[]
latbound = 21.
if var == 'z':
    field1='z'
    field2='hgt'
    scale = 9.8066
elif var == 't':
    field1='t'
    field2='temp'
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
nc20CR = Dataset('/global/scratch2/sd/whitaker/%s%s_20CR.nc' % (field2,level))
nc20CRsprd = Dataset('/global/scratch2/sd/whitaker/%s%s_20CRsprd.nc' % (field2,level))
data20CR = nc20CR.variables[field2]
data20CRsprd = nc20CRsprd.variables[field2]
dates20CR = [str(date) for date in nc20CR.variables['date'][:].tolist()]
lats1 = nc20CR.variables['lat'][::-1]
lons1 = nc20CR.variables['lon'][:]
lons, lats = np.meshgrid(lons1,lats1)
latnh = lats1.tolist().index(latbound)
latsh = lats1.tolist().index(-latbound)
coslats = np.cos((np.pi/180.)*lats)
coslatssh = coslats[:latsh+1,:]
coslatsnh = coslats[latnh:,:]
coslatstr = coslats[latsh:latnh+1,:]
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
    nt = dates20CR.index(date)
    z500_psonly = data20CR[nt,::-1,:]
    z500_psonly_sprd = data20CRsprd[nt,::-1,:]
    if date == date1:
        errmean = np.zeros(lats.shape, np.float32)
        biasmean = np.zeros(lats.shape, np.float32)
        sprdmean = errmean
    err = z500_psonly-z500_oper
    sprd = z500_psonly_sprd
    errmean = errmean + np.abs(err)/len(dates)
    biasmean = biasmean + err/len(dates)
    sprdmean = sprdmean + sprd/len(dates)
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
rmsnh = np.array(rmsnhall).mean()
rmssh = np.array(rmsshall).mean()
rmstr = np.array(rmstrall).mean()
rmsgl = np.array(rmsglall).mean()
print '%s-%s %d %6.2f %6.2f %6.2f %6.2f' % (date1,date2,len(rmsglall),rmsnh,rmstr,rmssh,rmsgl)
sprdnh = np.array(sprdnhall).mean()
sprdsh = np.array(sprdshall).mean()
sprdtr = np.array(sprdtrall).mean()
sprdgl = np.array(sprdglall).mean()
print '%s-%s %d %6.2f %6.2f %6.2f %6.2f' % (date1,date2,len(sprdglall),sprdnh,sprdtr,sprdsh,sprdgl)
np.savez('z500psonly_sprderr_20cr',errmean=errmean,sprdmean=sprdmean)

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
