import numpy as np
import matplotlib.pyplot as plt
from dateutils import daterange
from mpl_toolkits.basemap import Basemap, cm
import sys

date = '2001021712'
file='../gfsenkf_t254_1999iau3/%s/psobs_posterior.txt' % date
print file
lats = []; lons = []; covl_fact=[]
for line in open(file):
    linesplit = line.split()
    oberrvar = float(linesplit[13])
    if oberrvar < 99:
        lats.append(float(linesplit[3]))
        lons.append(float(linesplit[2]))
        covl_fact.append(float(linesplit[14]))

lats = np.array(lats)
lons = np.array(lons)
print lons.min(), lons.max()
covl_fact = np.array(covl_fact)
print covl_fact.mean()

m = Basemap(lon_0=180)
m.drawcoastlines()
m.drawcountries()
cm=m.scatter(lons,lats,c=covl_fact,s=5,marker='o',edgecolors='none',cmap=plt.cm.bwr,zorder=3,vmin=0,vmax=1)
m.colorbar()
plt.title('covl_fact for %s' % date)
plt.savefig('covl_fact_%s.png' % date)
plt.show()
