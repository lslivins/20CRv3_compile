import matplotlib.pyplot as plt
import matplotlib as mpl
import numpy as np
from dateutils import datetohrs

name = 'Z500'
hem = 'NH'

filename1 = 'gfsenkf_t254_1999iau3_fixedlocal3000_z500_200001.out'
filename2 = 'gfsenkf_t254_1999iau3_fixedlocal2500_z500_200001.out'
filename3 = 'gfsenkf_t254_1999iau3_fixedlocal2000_z500_200001.out'

fig = plt.figure(figsize=(10,7))
fig.subplots_adjust(bottom=0.2)
ax = fig.add_subplot(1,1,1)
if hem == 'NH':
    indx = 1 # nh
elif hem == 'TR':
    indx = 3 # trop
elif hem == 'SH':
    indx = 5 # sh
dates = []; x20cr = []; expt = []
for nline,line in enumerate(open(filename1)):
    if line.startswith('#'): continue
    linesplit = line.split()
    dates.append(linesplit[0])
    x20cr.append(float(linesplit[indx]))
expt = []
for nline,line in enumerate(open(filename2)):
    if line.startswith('#'): continue
    linesplit = line.split()
    expt.append(float(linesplit[indx]))
expt2 = []
for nline,line in enumerate(open(filename3)):
    if line.startswith('#'): continue
    linesplit = line.split()
    expt2.append(float(linesplit[indx]))
#expt2 = []
#for nline,line in enumerate(open(filename3)):
#    if line.startswith('#'): continue
#    linesplit = line.split()
#    expt2.append(float(linesplit[indx]))
expt = np.array(expt)
expt2 = np.array(expt2)
#expt2 = np.array(expt2)
x20cr = np.array(x20cr)
times = [float(datetohrs(date,mixedcal=False))/24. for date in dates]
dateFmt = mpl.dates.DateFormatter('%Y-%m-%d-%H')
#daysLoc = mpl.dates.DayLocator(interval=1)
daysLoc = mpl.dates.HourLocator(interval=30*24)
hoursLoc = mpl.dates.HourLocator(interval=5*24)
daysLoc = mpl.dates.HourLocator(interval=5*24)
hoursLoc = mpl.dates.HourLocator(interval=1*24)
ax.xaxis.set_major_formatter(dateFmt)
ax.xaxis.set_major_locator(daysLoc)
ax.xaxis.set_minor_locator(hoursLoc)
print len(x20cr),(expt < x20cr).sum()
#plt.plot_date(times,20cr,'ro-',label='SILF (mean = %6.3f)' % 20cr.mean())
#plt.plot_date(times,expt,'bo-',label='SIL3 (mean = %6.3f)' % expt.mean())
plt.plot_date(times,x20cr,'k-',label='fixed localization (3000/3.5) (mean = %4.1f)' % x20cr.mean())
plt.plot_date(times,expt,'r-',label='fixed localization (2500/3.5) (mean = %4.1f)' % expt.mean())
plt.plot_date(times,expt2,'b-',label='fixed localization (2000/3.0) (mean = %4.1f)' % expt2.mean())
#plt.plot_date(times,expt2,'bo-',label='T126L64 psautco=0.0004 (mean = %4.1f)' %\
#        expt2.mean())
plt.legend(loc=4)
ax = plt.gca()
plt.setp(ax.get_xticklabels(), 'rotation', 90,
         'horizontalalignment', 'center', fontsize=10)
plt.title('Z500 RMS %s' % hem)
plt.ylabel('RMS difference with ERA-Interim (m)')
plt.xlabel('analysis time')
plt.ylim(0,35)
plt.grid(True)
#plt.savefig('z500_fixed3500vsexpt_200001_%s.png' % hem)
plt.savefig('z500_fixed3000vs2500vs2000_200001_%s.png' % hem)
plt.show()
