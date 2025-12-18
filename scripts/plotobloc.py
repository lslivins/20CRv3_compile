import numpy as np
import matplotlib.pyplot as plt
from dateutils import daterange
from mpl_toolkits.basemap import Basemap, cm
import sys

#yr = sys.argv[1]
#stream = repr(int(yr)-5)
#yr = '2000'
#date1='%s010100' % yr
#date2='%s123118' % yr
date = sys.argv[1]

# stringout = '%-19s %3i %01s %7.2f %6.2f %5i %6.2f %7.1f %7.1f %10.3e %5.2f %30s %13s' % (obid,nceptype,obtype,longitude,latitude,elevation,analtime_offset,ob,slpob,bias,pserr,statname,statid)
#      write(9,9802) indxassim(nob),stattype(nob),obloclon(nob),obloclat(nob),&
#      obtime(nob),ob(nob),iskip(nob),&
#      obfit_prior(nob),obfit_post(nob),obsprd_prior(nob),obsprd_post(nob),&
#      oberrvar_orig_out,oberrvar_out,oberrvaruse(nob),sqrt(corrlengthsq(nob)),lnsigl(nob)
covl=[]; lons = []; lats = []

file='/scratch1/scratchdirs/whitaker/gfsenkf_t126_1999iau_biascor10/%s/psobs_posterior.txt' % date
print file
for line in open(file):
    linesplit = line.split()
    obcovl = float(linesplit[14])
    oblon = float(linesplit[2])
    oblat = float(linesplit[3])
    oberr = float(linesplit[13])
    if oberr < 99.9:
        covl.append(obcovl)
        lons.append(oblon)
        lats.append(oblat)

lats = np.array(lats)
lons = np.array(lons)
print lats.min(), lats.max()
print lons.min(), lons.max()
covl = np.array(covl)
covl = 6371.2*covl
print covl.min(), covl.max()

m = Basemap(lon_0=180)
m.drawcoastlines()
m.drawcountries()
cm=m.scatter(lons,lats,c=covl,s=5,marker='o',edgecolors='none',cmap=plt.cm.hot_r,zorder=3,vmin=200,vmax=4000)
plt.colorbar(shrink=0.5)
plt.title('horizontal localization (km) %s' % date)
plt.show()
