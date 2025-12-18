import pygrib
from dateutils import daterange, dateshift
import sys
import numpy as np
from numpy import ma
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, cm, addcyclic
date1 = sys.argv[1]
date2 = sys.argv[2]
dates = daterange(date1,date2,6)
expt = sys.argv[3]
level = int(sys.argv[4])
datapath = '/lustre/f1/unswept/Jeffrey.S.Whitaker/'+expt
def getmean(diff,coslats):
    meancoslats = coslats.mean()
    return (coslats*diff).mean()/meancoslats
lats=None; rmsnhall=[];rmsshall=[];rmstrall=[];rmsglall=[]
sprdnhall=[];sprdshall=[];sprdtrall=[];sprdglall=[]
latbound = 21.
yrold = date1[0:4]
grbs_era =\
pygrib.open('/lustre/f1/unswept/Jeffrey.S.Whitaker/erainterim/erainterim_%s_%smb_ztuv.grib2' % (yrold,level))
grbsu_era_sel = grbs_era.select(shortName='u',level=level)
grbsv_era_sel = grbs_era.select(shortName='v',level=level)
#print grbs_era_sel
ncount = 0
for date in dates:
    datem1 = dateshift(date,6)
    if date[0:4] != yrold:
        yrold = date[0:4]
        grbs_era.close()
        grbs_era.close()
        grbs_era =\
        pygrib.open('/lustre/f1/unswept/Jeffrey.S.Whitaker/erainterim/erainterim_%s_%smb_ztuv.grib2' % (yrold,level))
        grbsu_era_sel = grbs_era.select(shortName='u',level=level)
        grbsv_era_sel = grbs_era.select(shortName='v',level=level)
    for grbu,grbv in zip(grbsu_era_sel,grbsv_era_sel):
        grbdate = '%06i%04i' % (grbu.dataDate,grbu.dataTime)
        if grbdate == date+'00':
            u_oper = grbu.values
            v_oper = grbv.values
            break
    try:
        try:
            grbs_psonly = pygrib.open(datapath+'/%s/pgrbensmeananl_%s' % (date,date))
            grbs_psonly2 = pygrib.open(datapath+'/%s/pgrbenssprdanl_%s' % (date,date))
        except:
            grbs_psonly = pygrib.open(datapath+'/%s/pgrbensmeanfg_%s_fhr00' % (datem1,datem1))
            grbs_psonly2 = pygrib.open(datapath+'/%s/pgrbenssprdfg_%s_fhr00' % (datem1,datem1))
    except:
        continue
    try:
        grbu = grbs_psonly2.select(shortName='u',level=level)[0]
        grbv = grbs_psonly2.select(shortName='v',level=level)[0]
    except:
        continue
    u_psonly_sprd = grbu.values  
    v_psonly_sprd = grbv.values  
    wind_psonly_sprd = u_psonly_sprd**2 + v_psonly_sprd**2
    try:
        grbu = grbs_psonly.select(shortName='u',level=level)[0]
        grbv = grbs_psonly.select(shortName='v',level=level)[0]
    except:
        continue
    u_psonly = grbu.values
    v_psonly = grbv.values
    if lats is None:
        lats, lons = grbu.latlons()
        lons1 = lons[0,:]
        errmean = np.zeros(lats.shape, np.float32)
        biasmean = np.zeros(lats.shape, np.float32)
        sprdmean = np.zeros(lats.shape, np.float32)
        coslats = np.cos((np.pi/180.)*lats)
        lats1 = lats[:,0]
        latnh = lats1.tolist().index(latbound)
        latsh = lats1.tolist().index(-latbound)
        coslatsnh = coslats[:latnh+1,:]
        coslatssh = coslats[latsh:,:]
        coslatstr = coslats[latnh:latsh+1,:]
    grbs_psonly.close(); grbs_psonly2.close()
    err = (u_psonly-u_oper)**2 + (v_psonly-v_oper)**2
    sprd = wind_psonly_sprd
    if (np.abs(err).max() < 1000):
        errmean = errmean + np.abs(err)
        sprdmean = sprdmean + sprd
        ncount = ncount + 1
    rmsnh = np.sqrt(getmean(err[:latnh+1,:],coslatsnh))
    rmssh = np.sqrt(getmean(err[latsh:,:],coslatssh))
    rmstr = np.sqrt(getmean(err[latnh:latsh+1,:],coslatstr))
    rmsgl = np.sqrt(getmean(err,coslats))
    rmsnhall.append(rmsnh)
    rmsshall.append(rmssh)
    rmstrall.append(rmstr)
    rmsglall.append(rmsgl)
    sprdnh = np.sqrt(getmean(sprd[:latnh+1,:],coslatsnh))
    sprdsh = np.sqrt(getmean(sprd[latsh:,:],coslatssh))
    sprdtr = np.sqrt(getmean(sprd[latnh:latsh+1,:],coslatstr))
    sprdgl = np.sqrt(getmean(sprd,coslats))
    sprdnhall.append(sprdnh)
    sprdshall.append(sprdsh)
    sprdtrall.append(sprdtr)
    sprdglall.append(sprdgl)
    print '%s %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f' %\
    (date,rmsnh,sprdnh,rmstr,sprdtr,rmssh,sprdsh,rmsgl,sprdgl)
rmsnhall = np.array(rmsnhall)
rmsshall = np.array(rmsshall)
rmstrall = np.array(rmstrall)
rmsglall = np.array(rmsglall)
print '%s-%s %d %6.2f %6.2f %6.2f %6.2f' % (date1,date2,len(rmsglall),rmsnhall.mean(),rmstrall.mean(),rmsshall.mean(),rmsglall.mean())
sprdnhall = np.array(sprdnhall)
sprdshall = np.array(sprdshall)
sprdtrall = np.array(sprdtrall)
sprdglall = np.array(sprdglall)
print '%s-%s %d %6.2f %6.2f %6.2f %6.2f' % (date1,date2,len(sprdglall),sprdnhall.mean(),sprdtrall.mean(),sprdshall.mean(),sprdglall.mean())
errmean = errmean/ncount
sprdmean = sprdmean/ncount
