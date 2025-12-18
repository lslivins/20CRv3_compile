import numpy as np
import matplotlib.pyplot as plt
import sys

paoverpb = np.linspace(0,1.,101)
#covl_efold = 0.2
covl_efold = float(sys.argv[1])
covl_minfact = 0.1
covl_fact = 1. - np.exp( -((1.-paoverpb)/covl_efold) )
covl_fact = np.where(covl_fact < covl_minfact, covl_minfact, covl_fact)
plt.plot(paoverpb,covl_fact,color='r')
plt.axhline(0.5,color='k')
plt.axvline(0.95,color='k')
plt.ylim(0.0,1.0)
plt.yticks(np.linspace(0,1,11))
plt.title('covl_efold %s' % covl_efold)
plt.show()
