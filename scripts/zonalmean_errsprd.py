from netCDF4 import Dataset, date2index
from datetime import datetime
import sys, pygrib
from dateutils import daterange, splitdate
import numpy as np
import matplotlib.pyplot as plt

date1 = sys.argv[1]
date2 = sys.argv[2]
expt = sys.argv[3]
var = sys.argv[4]

levtop = 10
if var == 't':
    field1 = 'temp'
    field2 = 'temp'
    field3 = 't'
    cmax = 12
elif var == 'u':
    field1 = 'uwnd'
    field2 = 'u'
    field3 = 'u'
    cmax = 15

yr1 = int(date1[0:4])
yr2 = int(date1[0:4])
if yr1  != yr2:
    raise ValueError('dates must be in same year')
else:
    yr = yr1
dates = daterange(date1,date2,6)

ncera = Dataset('/lustre/f1/unswept/Jeffrey.S.Whitaker/erainterim/erai_1p5x1p5_SN/%s.%s.nc' % (field1,yr))
time_era = ncera.variables['time']
yr1,mon1,day1,hr1 = splitdate(date1)
nt1_era = date2index(datetime(yr1,mon1,day1,hr1),time_era) 
yr2,mon2,day2,hr2 = splitdate(date2)
nt2_era = date2index(datetime(yr2,mon2,day2,hr2),time_era) 
lats = ncera.variables['lat'][:]
lons = ncera.variables['lon'][:]
print ncera.variables[field1].shape
print nt1_era,nt2_era

levels = ncera.variables['level'][:].tolist()
indxtop = levels.index(levtop)
nlevs = len(levels)
data_era = np.empty((len(dates),nlevs,len(lats),len(lons)),dtype=np.float32)
eravar = ncera.variables[field1]
for nlev,lev in enumerate(levels[::-1]):
    data_era[:,nlevs-nlev-1,:,:] = eravar[nt1_era:nt2_era+1,nlev,:,:]
print data_era.shape, data_era.min(), data_era.max()
ncera.close()

datapath = '/lustre/f1/unswept/Jeffrey.S.Whitaker/'+expt
data_mean = np.empty((len(dates),len(levels),len(lats),len(lons)),dtype=np.float32)
data_sprd = np.empty((len(dates),len(levels),len(lats),len(lons)),dtype=np.float32)
levelsr = levels[::-1]
for nt,date in enumerate(dates):
    print date
    grbs_mean = pygrib.open(datapath+'/%s/pgrbensmeananl_%s' % (date,date))
    grbs_sel = grbs_mean.select(shortName=field3,typeOfLevel='isobaricInhPa',level=levels)
    for nlev,grb in enumerate(grbs_sel):
        #print nlev,grb.level,levelsr[nlev]
        data_mean[nt,nlev,::-1,:] = grb.values
    grbs_mean.close()
    grbs_sprd = pygrib.open(datapath+'/%s/pgrbenssprdanl_%s' % (date,date))
    grbs_sel = grbs_sprd.select(shortName=field3,typeOfLevel='isobaricInhPa',level=levels)
    for nlev,grb in enumerate(grbs_sel):
        data_sprd[nt,nlev,::-1,:] = grb.values
    grbs_sprd.close()
print data_mean.shape, data_mean.min(), data_mean.max()
print data_sprd.shape, data_sprd.min(), data_sprd.max()
err = data_mean - data_era
bias = err.mean(axis=0)
rms = np.sqrt((err**2).mean(axis=0))
print bias.min(), bias.max()

biaszm = bias.mean(axis=-1)
rmszm = rms.mean(axis=-1)
print biaszm.shape, biaszm.min(), biaszm.max()
print rmszm.shape, rmszm.min(), rmszm.max()
logplot = True
if logplot:
    labels_log = np.log(levelsr)
else:
    labels_log = levels
print labels_log
fig = plt.figure(figsize=(16,8))
ax = fig.add_subplot(1,2,1)
clevs = np.linspace(-cmax,cmax,21)
plt.contour(lats,labels_log,biaszm,clevs,colors='k',linewidths=0.5)
plt.contour(lats,labels_log,biaszm,[0],colors='k',linewidths=1.5)
plt.contourf(lats,labels_log,biaszm,clevs,cmap=plt.cm.RdBu_r,extend='both')
plt.colorbar()
plt.ylim(labels_log[0],labels_log[-1])
plt.grid(True)
if logplot:
    plt.gca().set_yticks(labels_log)
    plt.gca().set_yticklabels(levelsr)
plt.gca().set_xticks([-90,-60,-30,0,30,60,90])
plt.title('bias')
plt.grid(True)

ax = fig.add_subplot(1,2,2)
clevs = np.linspace(0,cmax,21)
plt.contour(lats,labels_log,rmszm,clevs,colors='k',linewidths=0.5)
plt.contour(lats,labels_log,rmszm,[0],colors='k',linewidths=1.5)
plt.contourf(lats,labels_log,rmszm,clevs,cmap=plt.cm.hot_r,extend='max')
plt.colorbar()
plt.ylim(labels_log[0],labels_log[-1])
plt.grid(True)
if logplot:
    plt.gca().set_yticks(labels_log)
    plt.gca().set_yticklabels(levelsr)
plt.gca().set_xticks([-90,-60,-30,0,30,60,90])
plt.title('rms')
plt.grid(True)

plt.suptitle('%s %s error' % (expt,field1),fontsize=20,fontweight='bold')
plt.savefig('errbiaszm.png')

plt.show()
