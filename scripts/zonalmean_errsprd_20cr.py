from netCDF4 import Dataset, date2index
from datetime import datetime
import sys
from dateutils import daterange, splitdate
import numpy as np
import matplotlib.pyplot as plt

date1 = sys.argv[1]
date2 = sys.argv[2]
var = sys.argv[3]

levtop = 100

if var == 't':
    field1 = 'temp'
    field2 = 'temp'
    cmax = 12
elif var == 'u':
    field1 = 'uwnd'
    field2 = 'u'
    cmax = 15

yr1 = int(date1[0:4])
yr2 = int(date1[0:4])
if yr1  != yr2:
    raise ValueError('dates must be in same year')
else:
    yr = yr1
dates = daterange(date1,date2,6)

ncera = Dataset('/global/project/projectdirs/incite11/erai_1p5x1p5_SN/%s.%s.nc' % (field1,yr))
time_era = ncera.variables['time']
yr1,mon1,day1,hr1 = splitdate(date1)
nt1_era = date2index(datetime(yr1,mon1,day1,hr1),time_era) 
yr2,mon2,day2,hr2 = splitdate(date2)
nt2_era = date2index(datetime(yr2,mon2,day2,hr2),time_era) 
lats = ncera.variables['lat'][:]
lons = ncera.variables['lon'][:]
print ncera.variables[field1].shape
print nt1_era,nt2_era

nc20cr = Dataset('/global/scratch2/sd/whitaker/%s_20CRmean.nc' % field2)
time_20cr = nc20cr.variables['time']
levels_all = nc20cr.variables['level'][:].tolist()
indxtop = levels_all.index(levtop)
levels = levels_all[0:indxtop+1]
print levels
yr1,mon1,day1,hr1 = splitdate(date1)
nt1_20cr = date2index(datetime(yr1,mon1,day1,hr1),time_20cr) 
yr2,mon2,day2,hr2 = splitdate(date2)
nt2_20cr = date2index(datetime(yr2,mon2,day2,hr2),time_20cr) 
print nc20cr.variables[field2].shape
print nt1_20cr,nt2_20cr
mean20cr = nc20cr.variables[field2][nt1_20cr:nt2_20cr+1,0:indxtop+1,::-1,:]

nc20cr_sprd = Dataset('/global/scratch2/sd/whitaker/%s_20CRsprd.nc' % field2)
sprd20cr = nc20cr_sprd.variables[field2][nt1_20cr:nt2_20cr+1,0:indxtop+1,::-1,:]

levsera = ncera.variables['level'][:].tolist()
era = np.empty(mean20cr.shape, mean20cr.dtype)
eravar = ncera.variables[field1]
for nlev2,lev in enumerate(levels):
    nlev = levsera.index(lev)
    era[:,nlev2,:,:] = eravar[nt1_era:nt2_era+1,nlev,:,:]

print era.shape, era.min(), era.max()
print mean20cr.shape, mean20cr.min(), mean20cr.max()
print sprd20cr.shape, sprd20cr.min(), sprd20cr.max()
err = mean20cr - era
bias = err.mean(axis=0)
rms = np.sqrt((err**2).mean(axis=0))
print bias.min(), bias.max()

nc20cr_sprd.close()
nc20cr.close()
ncera.close()

biaszm = bias.mean(axis=-1)
rmszm = rms.mean(axis=-1)
print biaszm.shape, biaszm.min(), biaszm.max()
print rmszm.shape, rmszm.min(), rmszm.max()
logplot = False
levsout = [1000,850,700,600,500,400,300,250,200,150,100,70,50]
if logplot:
    labels_log = np.log(levels)
    labels2_log = np.log(np.array(levsout,np.float))
else:
    labels_log = levels
    labels2_log = levsout
print labels_log
print labels2_log
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
   plt.gca().set_yticks(labels2_log)
   plt.gca().set_yticklabels(levsout)
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
   plt.gca().set_yticks(labels2_log)
   plt.gca().set_yticklabels(levsout)
plt.gca().set_xticks([-90,-60,-30,0,30,60,90])
plt.title('rms')
plt.grid(True)

plt.suptitle('20CR %s error' % field1,fontsize=20,fontweight='bold')

plt.show()
