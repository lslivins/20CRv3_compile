import sys, os
from dateutils import *
from mpl_toolkits.basemap import Basemap, cm
import matplotlib.pyplot as plt
import numpy as N 
from pygrib import gaulats

nlons = 384; nlats=190
lats = gaulats(nlats)
lons = (360./nlons)*N.arange(nlons)
filename = sys.argv[1]
f = open(filename)
data=N.reshape(N.fromstring(f.read(nlons*nlats*4),'<f'),(nlats,nlons))
f.close()

plt.figure(figsize=(10,4))
m =\
Basemap(llcrnrlon=0,llcrnrlat=-90,urcrnrlon=360,urcrnrlat=90,projection='cyl')
lons, lats = N.meshgrid(lons, lats)
x, y = m(lons, lats)
# draw coasts and fill continents.
m.drawcoastlines(linewidth=1.5,color='w')
clevs = N.linspace(0.001,0.01,21)
im =\
m.contourf(x,y,data,clevs,extend='both',cmap=plt.cm.hot_r)
plt.colorbar(im,shrink=0.8)
# draw parallels and meridians.
delat = 30.
circles = N.arange(0.,90.,delat).tolist()+\
          N.arange(-delat,-90,-delat).tolist()
m.drawparallels(circles,labels=[1,0,0,0])
delon = 60.
meridians = N.arange(0,360,delon)
m.drawmeridians(meridians,labels=[0,0,0,1])
plt.title('lapse rate (unsmoothed)')

plt.show()
