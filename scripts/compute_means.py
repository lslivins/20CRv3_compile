import numpy as np
import sys
filename = sys.argv[1]
n1 = int(sys.argv[2])
n2 = int(sys.argv[3])
data = np.loadtxt(filename)
print '%s-%s' % (int(data[n1-1,0]),int(data[n2,0]))
print data[n1-1:n2,1].mean(),data[n1-1:n2,2].mean(),data[n1-1:n2,3].mean(),data[n1-1:n2,4].mean()
