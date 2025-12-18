import sys, os
from dateutils import *
from mpl_toolkits.basemap import Basemap, cm
import matplotlib.pyplot as plt
import numpy as N 
from pygrib import gaulats
from matplotlib.colors import LogNorm

nlons = 512; nlats=256; nlevs = 64; nvars = 4
ndim = nvars*nlevs + 2
lats = gaulats(nlats)
lons = (360./nlons)*N.arange(nlons)
date1 = sys.argv[1]
date2 = sys.argv[2]
nlev = int(sys.argv[3])
cmax = float(sys.argv[4])
covinflate_mn = N.zeros((ndim,nlats,nlons),N.float32)
expt = 'gfsenkf_t254_1999iau3_fixedlocal3500'
expt = 'gfsenkf_t254_1999iau3'
datapath = '../'+expt
dates = daterange(date1,date2,6)
for date in dates:
   filename = datapath+'/%s/covinflate.dat' % date
   print filename
   f = open(filename)
   covinflate=N.reshape(N.fromstring(f.read(nlons*nlats*ndim*4),'<f'),(ndim,nlats,nlons))
   covinflate_mn = covinflate_mn + covinflate/len(dates)
   f.close()
covinflate = covinflate_mn

for n in range(ndim):
    print n,covinflate[n].min(),covinflate[n].max()

# set up lambert azimuthal map centered on N. Pole.
plt.figure(figsize=(10,4))
m =\
Basemap(llcrnrlon=0,llcrnrlat=-90,urcrnrlon=360,urcrnrlat=90,projection='cyl')
lons, lats = N.meshgrid(lons, lats)
x, y = m(lons, lats)
#m.scatter(xo,yo,marker='o',c='k',s=5,zorder=10)
# draw coasts and fill continents.
m.drawcoastlines(linewidth=1.5,color='w')
#m.fillcontinents()
data = covinflate[nlev,:,:]
#norm=LogNorm(vmin=1,vmax=5)
#im = m.pcolormesh(x, y, data, cmap=plt.cm.jet,norm=norm)
clevs = N.linspace(1,cmax,21)
print clevs
im =\
m.contourf(x,y,data,clevs,extend='both',cmap=plt.cm.jet)
#cb=plt.colorbar(im,shrink=0.8,ticks=[1,1.5,2,3,4,5])
plt.colorbar(im,shrink=0.8)
#cb.ax.set_yticklabels(['1','1.5','2','3','4','5'])
# draw parallels and meridians.
delat = 30.
circles = N.arange(0.,90.,delat).tolist()+\
          N.arange(-delat,-90,-delat).tolist()
#m.drawparallels(circles,labels=[1,0,0,0])
delon = 60.
meridians = N.arange(0,360,delon)
#m.drawmeridians(meridians,labels=[0,0,0,1])
#plt.title('multiplicative inflation')
plt.savefig('covinflate.png')
plt.show()
