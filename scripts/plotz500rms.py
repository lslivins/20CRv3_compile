import matplotlib.pyplot as plt
import matplotlib as mpl
import numpy as np
from dateutils import datetohrs

name = 'Z500'
hem = 'NH'

filename1 = 'z500_4densvar_insitu_2000.out'
filename2 = 'z500_gfsenkf_insitu_2000_a.out'
#filename3 = 'z500_20cr_jfm2000.out'

fig = plt.figure(figsize=(10,4))
fig.subplots_adjust(bottom=0.2)
ax = fig.add_subplot(1,1,1)
if hem == 'NH':
    indx = 1 # nh
elif hem == 'TR':
    indx = 3 # trop
elif hem == 'SH':
    indx = 5 # sh
dates = []; ctl = []; expt = []; x20cr = []
for nline,line in enumerate(open(filename1)):
    if line.startswith('#'): continue
    linesplit = line.split()
    dates.append(linesplit[0])
    ctl.append(float(linesplit[indx]))
for nline,line in enumerate(open(filename2)):
    if line.startswith('#'): continue
    linesplit = line.split()
    expt.append(float(linesplit[indx]))
#for nline,line in enumerate(open(filename3)):
#    if line.startswith('#'): continue
#    linesplit = line.split()
#    x20cr.append(float(linesplit[indx]))
expt = np.array(expt)
ctl = np.array(ctl)
#x20cr = np.array(x20cr)
times = [float(datetohrs(date,mixedcal=False))/24. for date in dates]
dateFmt = mpl.dates.DateFormatter('%m-%d-%H')
#daysLoc = mpl.dates.DayLocator(interval=1)
daysLoc = mpl.dates.HourLocator(interval=30*24)
hoursLoc = mpl.dates.HourLocator(interval=5*24)
daysLoc = mpl.dates.HourLocator(interval=5*24)
hoursLoc = mpl.dates.HourLocator(interval=1*24)
ax.xaxis.set_major_formatter(dateFmt)
ax.xaxis.set_major_locator(daysLoc)
ax.xaxis.set_minor_locator(hoursLoc)
print len(ctl),(expt < ctl).sum()
#plt.plot_date(times,x20cr,'k-',label='20CRV2 (mean = %4.1f)' % x20cr.mean())
#plt.plot_date(times,ctl,'b-',label='fixed localization (3500/3.5) (mean = %4.1f)' % ctl.mean())
#plt.plot_date(times,expt,'r-',label='variable localization (mean = %4.1f)' %\
#        expt.mean())
plt.plot_date(times,ctl,'b-',label='4denvar hybrid (mean = %4.1f)' % ctl.mean())
plt.plot_date(times,expt,'r-',label='pure enkf (mean = %4.1f)' %\
        expt.mean())
plt.legend(loc=4)
ax = plt.gca()
plt.setp(ax.get_xticklabels(), 'rotation', 90,
         'horizontalalignment', 'center', fontsize=8)
plt.title('Z500 RMS %s' % hem)
plt.ylabel('RMS diff with ERA-Interim (m)')
plt.xlabel('analysis time')
#plt.ylim(5,45)
plt.ylim(0,12)
plt.grid(True)
plt.savefig('z500_4denvar_gfsenkf_%s.png' % hem)
#plt.savefig('z500_expt1vsexpt2_200001_%s.png' % hem)
plt.show()
