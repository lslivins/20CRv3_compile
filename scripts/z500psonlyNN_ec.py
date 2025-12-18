import pygrib
from dateutils import daterange
import sys
import numpy as np
from netCDF4 import Dataset
date1 = sys.argv[1]
date2 = sys.argv[2]
dates = daterange(date1,date2,6)
reanl = sys.argv[3]
var = sys.argv[4]
level = int(sys.argv[5])
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
pygrib.open('/lustre/f1/unswept/Jeffrey.S.Whitaker/erainterim/erainterim_%s_%smb_ztuv.grib2' % (yrold,level))
grbs_era_sel = grbs_era.select(shortName=field1,level=level)
filename = '/lustre/f1/unswept/Jeffrey.S.Whitaker/reanlnc/%s%s_%s_1999-2000.nc' % (field2,level,reanl)
#print filename
nc20CR = Dataset(filename)
data20CR = nc20CR.variables[field2]
dates20CR = [str(date) for date in nc20CR.variables['date'][:].tolist()]
lats1 = nc20CR.variables['lat'][::-1]
lons1 = nc20CR.variables['lon'][:]
lons, lats = np.meshgrid(lons1,lats1)
latnh = lats1.tolist().index(latbound)
latsh = lats1.tolist().index(-latbound)
#print latnh, lats1[latnh:]
#print latsh, lats1[latsh:latnh+1], lats1[:latsh+1]
coslats = np.cos((np.pi/180.)*lats)
coslatssh = coslats[:latsh+1,:]
coslatsnh = coslats[latnh:,:]
coslatstr = coslats[latsh:latnh+1,:]
for date in dates:
    if date[0:4] != yrold:
        yrold = date[0:4]
        grbs_era.close()
        grbs_era =\
        pygrib.open('/lustre/f1/unswept/Jeffrey.S.Whitaker/erainterim/erainterim_%s_%smb_ztuv.grib2' % (yrold,level))
        grbs_era_sel = grbs_era.select(shortName=field1,level=level)
    for grb in grbs_era_sel:
        grbdate = '%06i%04i' % (grb.dataDate,grb.dataTime)
        if grbdate == date+'00':
            z500_oper = grb.values/scale
            break
    nt = dates20CR.index(date)
    if reanl == 'CFSR':
        z500_psonly = data20CR[nt,::-1,:]
    else:
        z500_psonly = data20CR[nt,:,:]
    if date == date1:
        errmean = np.zeros(lats.shape, np.float32)
        biasmean = np.zeros(lats.shape, np.float32)
    err = z500_psonly-z500_oper
    #import matplotlib.pyplot as plt
    #from mpl_toolkits.basemap import Basemap, addcyclic
    #plt.figure()
    #m = Basemap(projection='cyl',lon_0=180)
    #m.drawcoastlines()
    #lons,lons2 = addcyclic(lons, lons1)
    #lons[:,-1]=360
    #lats,lons2 = addcyclic(lats, lons1)
    #x,y = m(lons,lats)
    #clevs = 21
    #data = err
    #data,lons2 = addcyclic(data,lons1)
    #print data.min(), data.max()
    #CS=m.contourf(x,y,data,clevs,cmap=plt.cm.RdBu_r,extend='both')
    #plt.colorbar(CS,shrink=0.6)
    #plt.show()
    #raise SystemExit
    #print z500_psonly.min(), z500_psonly.max(), z500_oper.min(), z500_oper.max()
    errmean = errmean + np.abs(err)/len(dates)
    biasmean = biasmean + err/len(dates)
    rmssh = np.sqrt(getmean(err[:latsh+1,:]**2,coslatssh))
    rmsnh = np.sqrt(getmean(err[latnh:,:]**2,coslatsnh))
    rmstr = np.sqrt(getmean(err[latsh:latnh+1,:]**2,coslatstr))
    rmsgl = np.sqrt(getmean(err**2,coslats))
    rmsnhall.append(rmsnh)
    rmsshall.append(rmssh)
    rmstrall.append(rmstr)
    rmsglall.append(rmsgl)
    print '%s %6.2f %6.2f %6.2f %6.2f' %\
    (date,rmsnh,rmstr,rmssh,rmsgl)
rmsnh = np.array(rmsnhall).mean()
rmssh = np.array(rmsshall).mean()
rmstr = np.array(rmstrall).mean()
rmsgl = np.array(rmsglall).mean()
print '%s-%s %d %6.2f %6.2f %6.2f %6.2f' % (date1,date2,len(rmsglall),rmsnh,rmstr,rmssh,rmsgl)
