import sys, os

def MakeNamelist(dtg,atcfname,stswitch,itmphrs):

    yy=int(dtg[2:4])
    mm=int(dtg[4:6])
    dd=int(dtg[6:8])
    hh=int(dtg[8:10])
    
    namelist="""&datein 
  inp%%byy=%02d,
  inp%%bmm=%02d,
  inp%%bdd=%02d,
  inp%%bhh=%02d, 
  inp%%model=1
/
&stormlist
  %s
/
&fhlist 
  %s
/
&atcfinfo 
  atcfnum=81,
  atcfname='%s',
  atcfymdh=%s
/
&phaseinfo 
  phaseflag='n',
  phasescheme='cps'
/
&structinfo
  structflag='n',
  ikeflag='n'
/
"""%(yy,mm,dd,hh,stswitch,itmphrs,atcfname,dtg)

    return(namelist)

dtg = sys.argv[1]
atcfname = sys.argv[2]

# make itmphrs card in gettrk namelist
#    
itmphrs='itmphrs = '

maxtaus=65
taus = range(0,121,6)
for n in range(0,maxtaus):
    try:
        tau=taus[n]
        ctau="%04d"%(tau)
    except:
        ctau='99'
    
    itmphrs="%s %s"%(itmphrs,ctau)

# make stswitch card in gettrk namelist -- storms to track
#

tcvitdir='/lfs1/projects/rtfim/tcvitals'
itcvitals="%s/tcvitals.%s.txt"%(tcvitdir,dtg)

maxstms=15
cmd="cat %s | wc -l "%(itcvitals)
cards=os.popen(cmd).readlines()

nstms=int(cards[0].strip())

stswitch='stswitch = '

for n in range(0,maxstms):
    if(n < nstms):
        stswitch="%s 1"%(stswitch)
    else:
        stswitch="%s 3"%(stswitch)
        
# change to tracker dir, make namelist and run app
#

namelist=MakeNamelist(dtg,atcfname,stswitch,itmphrs)

print namelist
