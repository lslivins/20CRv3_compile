import sys
from dateutils import *
import random
date1 = sys.argv[1]
date2 = sys.argv[2]
nanals = int(sys.argv[3])
datesall = daterange(date1,date2,6)
datesall = ['2011'+date[4:] for date in datesall]
dates = random.sample(datesall,nanals)
for date in dates:
    print '%s' % date
