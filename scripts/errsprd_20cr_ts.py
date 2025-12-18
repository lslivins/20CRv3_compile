from netCDF4 import Dataset, date2index, num2date
from datetime import datetime
import sys
from dateutils import daterange, splitdate
import numpy as np

date1 = sys.argv[1]
date2 = sys.argv[2]
var = sys.argv[3]
level = int(sys.argv[4])

if var == 't':
    field1 = 'temp'
    field2 = 'temp'
elif var == 'gh':
    field1 = 'geopot'
    field2 = 'hgt'
elif var == 'u':
    field1 = 'uwnd'
    field2 = 'u'

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
def getmean(diff,coslats):
    meancoslats = coslats.mean()
    return (coslats*diff).mean()/meancoslats
latbound = 21.
latnh = lats.tolist().index(latbound)
latsh = lats.tolist().index(-latbound)
lons, lats = np.meshgrid(lons, lats)
coslats = np.cos((np.pi/180.)*lats)
coslatssh = coslats[:latsh+1,:]
coslatsnh = coslats[latnh:,:]
coslatstr = coslats[latsh:latnh+1,:]
levsera = ncera.variables['level'][:].tolist()
nlevera = levsera.index(level)
times = time_era[nt1_era:nt2_era+1]
#print levsera[nlevera]
#print ncera.variables[field1].shape
#print nt1_era,nt2_era

nc20cr = Dataset('/global/scratch2/sd/whitaker/%s_20CRmean.nc' % field2)
time_20cr = nc20cr.variables['time']
levels = nc20cr.variables['level'][:].tolist()
nlev = levels.index(level)
yr1,mon1,day1,hr1 = splitdate(date1)
nt1_20cr = date2index(datetime(yr1,mon1,day1,hr1),time_20cr) 
yr2,mon2,day2,hr2 = splitdate(date2)
nt2_20cr = date2index(datetime(yr2,mon2,day2,hr2),time_20cr) 
#print nc20cr.variables[field2].shape
#print nt1_20cr,nt2_20cr
mean20cr = nc20cr.variables[field2][nt1_20cr:nt2_20cr+1,nlev,::-1,:]

nc20cr_sprd = Dataset('/global/scratch2/sd/whitaker/%s_20CRsprd.nc' % field2)
sprd20cr = nc20cr_sprd.variables[field2][nt1_20cr:nt2_20cr+1,nlev,::-1,:]

era = ncera.variables[field1][nt1_era:nt2_era+1,nlevera,:,:]
if var == 'gh': era = era/9.8066

#print era.shape, era.min(), era.max()
#print mean20cr.shape, mean20cr.min(), mean20cr.max()
#print sprd20cr.shape, sprd20cr.min(), sprd20cr.max()

errall = mean20cr - era
#print errall.shape, errall.min(), errall.max()

rmsnhall=[];rmsshall=[];rmstrall=[];rmsglall=[]
sprdnhall=[];sprdshall=[];sprdtrall=[];sprdglall=[]
for nt in range(era.shape[0]):
    sprd = sprd20cr[nt]; err = errall[nt]
    date = num2date(times[nt],time_era.units).strftime('%Y%m%d%H')
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

rmsnh = np.array(rmsnhall)
rmssh = np.array(rmsshall)
rmstr = np.array(rmstrall)
rmsgl = np.array(rmsglall)
print '%s-%s %d %6.2f %6.2f %6.2f %6.2f' % (date1,date2,len(rmsglall),rmsnh.mean(),rmstr.mean(),rmssh.mean(),rmsgl.mean())
sprdnh = np.array(sprdnhall)
sprdsh = np.array(sprdshall)
sprdtr = np.array(sprdtrall)
sprdgl = np.array(sprdglall)
print '%s-%s %d %6.2f %6.2f %6.2f %6.2f' % (date1,date2,len(sprdglall),sprdnh.mean(),sprdtr.mean(),sprdsh.mean(),sprdgl.mean())

nc20cr_sprd.close()
nc20cr.close()
ncera.close()
