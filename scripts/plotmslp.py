from mpl_toolkits.basemap import Basemap, addcyclic, cm
import matplotlib.pyplot as plt
import numpy as np
import sys, pygrib

date = '1999122718'
datapath = '/scratch/scratchdirs/whitaker/gfsenkf_t126psonly_1999a/'+date
oblats=[]; oblons=[]; errp=[]
oblats1=[]; oblons1=[]; errp1=[]
oblats2=[]; oblons2=[]; errp2=[]
oblats3=[]; oblons3=[]; errp3=[]
oblats4=[]; oblons4=[]; errp4=[]
oblats5=[]; oblons5=[]; errp5=[]
for line in open(datapath+'/obstats.txt'):
    lat = float(line.split()[2])
    lon=float(line.split()[1])
    failbg = int(line.split()[10])
    #if failbg: continue
    probgerr = 100.*float(line.split()[9])
    if failbg: probgerr = 101
    if lon > 180: lon=lon-360
    oblats.append(lat); oblons.append(lon)
    errp.append(probgerr)
    if probgerr <= 25:
       oblats1.append(lat); oblons1.append(lon); errp1.append(probgerr)
    if probgerr > 25 and probgerr <= 50:
       oblats2.append(lat); oblons2.append(lon); errp2.append(probgerr)
    if probgerr > 50 and probgerr <= 75:
       oblats3.append(lat); oblons3.append(lon); errp3.append(probgerr)
    if probgerr > 75 and probgerr <= 100:
       oblats4.append(lat); oblons4.append(lon); errp4.append(probgerr)
    if probgerr > 100:
       oblats5.append(lat); oblons5.append(lon); errp5.append(probgerr)
    #if lon < 0 and lon > -4 and lat > 47 and lat < 51:
    #    print line[:-1]
#print len(oblons),len(oblats)
grbs = pygrib.open(datapath+'/pgrbensmeananl_%s' % date)
grb = grbs.select(parameterName='MSL Mean sea level pressure Pa')[0]
lats, lons = grb.latlons()
lats1 = lats[:,0]
lons1 = lons[0,:]
mslp = 0.01*grb.values
mslp, lons2 = addcyclic(mslp, lons1)
lons, lats = np.meshgrid(lons2,lats1)
grbs.close()
grbs = pygrib.open(datapath+'/pgrbenssprdfg_%s_fhr06' % date)
grb = grbs.select(parameterName='MSL Mean sea level pressure Pa')[0]
mslp_sprd = 0.01*grb.values
mslp_sprd, lons2 = addcyclic(mslp_sprd, lons1)
grbs.close()
lon_0=-4
lat_0=49
map = Basemap(lon_0=lon_0,lat_0=lat_0,projection='lcc',width=2200.e3,height=1400.e3,resolution='l')
#obx,oby = map(oblons,oblats)
#map.scatter(obx,oby,s=5,marker='o',c='k',zorder=2)
#map.scatter(obx,oby,s=25,marker='o',c=errp,zorder=2,cmap=plt.cm.jet,edgecolors='none',vmin=0,vmax=50)
obx,oby = map(oblons1,oblats1)
#if len(oblons1): map.scatter(obx,oby,s=25,marker='o',c='k',zorder=2,label='<25%')
if len(oblons1): map.scatter(obx,oby,s=25,marker='o',edgecolor='none',c='k',zorder=2,label='used')
obx,oby = map(oblons2,oblats2)
if len(oblons2): map.scatter(obx,oby,s=25,marker='o',edgecolor='none',c='g',zorder=2,label='25-50%')
obx,oby = map(oblons3,oblats3)
if len(oblons3): map.scatter(obx,oby,s=25,marker='o',edgecolor='none',c='b',zorder=2,label='50-75%')
obx,oby = map(oblons4,oblats4)
if len(oblons4): map.scatter(obx,oby,s=25,marker='o',edgecolor='none',c='r',zorder=2,label='>75%')
obx,oby = map(oblons5,oblats5)
if len(oblons5): map.scatter(obx,oby,s=25,marker='o',c='w',zorder=2,label='rejected')
leg=plt.legend(loc='upper left')
ltxt=leg.get_texts()
plt.setp(ltxt,fontsize=10)
map.drawcoastlines()
#map.fillcontinents()
x,y = map(lons,lats)
CS=map.contour(x,y,mslp,np.arange(900,1100,4),colors='k')
CS2=map.contour(x,y,mslp,[980],colors='k',linewidths=3.0)
CS3=map.contourf(x,y,mslp_sprd,np.arange(0,5.1,0.5),cmap=cm.GMT_haxby_r)
plt.colorbar(shrink=0.65)
map.drawparallels(np.arange(40,60.1,4),labels=[1,0,0,0])
map.drawmeridians(np.arange(-20,20.1,4),labels=[0,0,0,1])
#plt.title('Huber norm QC')
#plt.savefig('huber.png')
plt.title('Background check QC only')
plt.savefig('nohuber.png')
plt.show()
