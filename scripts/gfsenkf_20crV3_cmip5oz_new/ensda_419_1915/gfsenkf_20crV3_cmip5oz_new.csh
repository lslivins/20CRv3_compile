#!/bin/csh
#PBS -q batch
#PBS -A cpo_ngrr_e
#PBS -l partition=c3
#PBS -l size=80 
##PBS -q urgent
##PBS -q debug
#PBS -l walltime=00:49:15
#PBS -N gfsenkf_415_1915
#PBS -e gfsenkf.err
#PBS -o gfsenkf.out
#PBS -S /bin/csh
setenv nhosts 2560  # this must make "-l size" above!

# if I don't do this, I get 'illegal instruction' error
# on c3 when numpy imported. 
module unload python_numpy
module unload python_matplotlib
module unload python_netcdf4
module unload python
# GFS does not compile with intel 15. (where did this come from? G. Compo 8 Feb 2017)
module switch intel intel/16.0.3.210

# Gil changed to 
# because needed to add this to her path for incdate:
set path = ( $path /lustre/f1/unswept/Gilbert.P.Compo/20CRV3/bin )

# go to dir where job was submitted
cd $PBS_O_WORKDIR

limit stacksize unlimited
setenv OMP_NUM_THREADS 1

setenv corespernode 32
setenv corespernumanode 16
set ncycles=4 # number of DA cycles to run
setenv nanals 80                                                    
setenv mpitasks `expr $nhosts \/ $nanals`
setenv fg_proc $mpitasks
setenv mpitaskspernode `expr $corespernode \/ $OMP_NUM_THREADS`
# this must be an integer (threads must be 1,2,3 or 6)
# taskspernumanode not used.
setenv taskspernumanode `expr $corespernumanode \/ $OMP_NUM_THREADS`
#setenv enkf_taskspernode `expr $corespernode \/ 2`
setenv enkf_taskspernode $corespernode
#setenv enkf_taskspernode 4

# gil is using 20CRV3 but only the "new" kind
setenv basedir /lustre/f1/unswept/${USER}/20CRV3

setenv MPICH_NO_BUFFER_ALIAS_CHECK 1
setenv MPICH_UNEX_BUFFER_SIZE 120M
setenv MPICH_MAX_SHORT_MSG_SIZE 8000
#setenv MPICH_PTL_UNEX_EVENTS 400000
#setenv MPICH_PTL_OTHER_EVENTS 100000
#setenv MPICH_UNEX_BUFFER_SIZE 640000000 
setenv MPICH_PTL_MEMD_LIMIT 20480
setenv MPICH_PTL_OTHER_EVENTS 20480
setenv MPICH_FAST_MEMCPY TRUE
#setenv APRUN_XFER_LIMITS 1
setenv PSC_OMP_AFFINITY FALSE
# setenv MPICH_PTL_MATCH_OFF 1

set ensda = "run_enkf_ps_h5_biascor_iau"
set fg_gfs = "run_fg_gfs_ens_iau"
set cleanup_obs = 'true' # remove existing obs files
set cleanup_anal = 'true' # remove existing anal files
set cleanup_fg = 'true' # remove existing first guess files
set cleanup_ensmean = 'false' # remove existing ensmean files
set do_cleanup = 'true' # if true, create tar files, delete *mem* files.

set datadir=/lustre/f1/unswept/${USER}/

# where the data will be created
setenv expname `basename $PWD`
#setenv datapath "${datadir}/${expname}/"
setenv datapath "${datadir}/gfsenkf_20crV3_cmip5oz/${expname}/" # backslash at end of path necessary

# Data reside in obs directory set dynamically in loop below ${obsdir}

# log directory
setenv logdir "${datadir}/logs/${expname}"

# some scripts reside here
# also need to make this dependent on user or a group writeable area -compo

setenv enkfscripts $PWD

# name of enkf executable.
setenv enkfbin "${basedir}/enkf/trunk/src/global_enkf_new"

setenv incdate "${enkfscripts}/incdate"

setenv homedir $PWD
setenv qcomp ecomp

##########################################################################
# enkf parameters.
setenv corrlengthnh 4000
setenv corrlengthtr 4000
setenv corrlengthsh 4000
# min allowed covl reduction (make 1.0 to turn off covl reduction with increasing paoverpb)
setenv covl_minfact 0.05
# covl_efold smaller means less reduction of covl as paoverpb -> 1
setenv covl_efold 0.2
setenv lnsigcutoffnh 4.0
setenv lnsigcutofftr 4.0
setenv lnsigcutoffsh 4.0
setenv lnsigcutoffpsnh 4.0
setenv lnsigcutoffpstr 4.0
setenv lnsigcutoffpssh 4.0
setenv lnsigcutoffsatnh 4.0  
setenv lnsigcutoffsattr 4.0  
setenv lnsigcutoffsatsh 4.0  
setenv use_height .true.
setenv obtimelnh 1.e30       
setenv obtimeltr 1.e30       
setenv obtimelsh 1.e30         
setenv reducedgrid .false.

# Assimilation parameters
setenv JCAP 254  
setenv LEVS 64
setenv LONB 512  
setenv LATB 256  
setenv LONA $LONB   
setenv LATA $LATB   
setenv SMOOTHINF 35
setenv npts `expr \( $LONA \) \* \( $LATA \)`
setenv LSOIL 4
setenv RUN "gfs"
#setenv obsdirh5 /lustre/f1/unswept/${USER}/HDF5/v321/
setenv obsdirh5 /lustre/f1/unswept/Chesley.Mccoll/HDF5/V4.1/ 
# HadISST ssts
#setenv sstpath /lustre/f1/unswept/Jeffrey.S.Whitaker/bound_cond_HadISST2.1
#setenv sstpath /lustre/f1/unswept/Gilbert.P.Compo/bound_cond_HadISST2.1
#setenv sstpath /lustre/f1/unswept/Gilbert.P.Compo/bound_cond_cobe2
# soda ssts and COBE ice
#setenv sstpath /lustre/f1/unswept/Gilbert.P.Compo/bound_cond_sodasi.2c
setenv sstpath /lustre/f1/unswept/Laura.Slivinski/bound_cond_sodasi.3c/
# soda ssts and hadisst ice
#setenv sstpath /lustre/f1/unswept/Laura.Slivinski/bound_cond_HadISST2.1_sodasi.3c/

# climo ice of HadISST ice? NOTE HadISST is a climatology from that dataset
setenv use_climoice "false" # use HadISST ice
setenv lastndays 60
setenv NTRAC 3
setenv nvars 4 #nvars=4 u,v,T,qvapor but not ozone updated
setenv LANDICE_OPT 2

setenv iassim_order 2

setenv covinflatemax 1.e2
setenv covinflatemin 1.00                                            
setenv analpertwtnh 0.9
setenv analpertwttr 0.9
setenv analpertwtsh 0.9
setenv covinflatenh 0.0
setenv covinflatetr 0.0
setenv covinflatesh 0.0
setenv lnsigcovinfcutoff 1.e30
setenv pseudo_rh .false.
setenv massbal_adjust .true.
                                                                    
#setenv sprd_tol 10.0       
setenv sprd_tol 3.2        
setenv varqc .true.
setenv zhuberleft 1.1
setenv zhuberright 1.1
setenv numiter 7

setenv paoverpb_thresh 1.0                                         
setenv saterrfact 1.0
setenv deterministic .true.
setenv sortinc .true.

setenv nlevt2 19 # levels used to compute lapse rate
setenv nlevt1 16 # the temp at this level is used in forward operator
setenv rlapse 0.001 # min lapse rate allowed in forward operator
setenv smoothparm 63 # lapse rate smoothing parameter
setenv zthresh 1000  # don't assim if station - model elev more than t his
setenv errfact 1.0 # inflate ob errors
setenv delz_const 0.001 # add this much to ob err every meter of diff in station and model elev
                                                                    
setenv nitermax 2

##########################################################################
# Some binaries and scripts reside here
#

setenv HOMEGLOBAL ${basedir}
setenv FIXGLOBAL ${basedir}/gfs/fix_am
setenv EXECGLOBAL ${HOMEGLOBAL}/bin
setenv SIGLEVEL ${FIXGLOBAL}/global_hyblev.l64.txt
#setenv FCSTEXEC /lustre/f1/unswept/${USER}/gfs/global_fcst.fd/global_fcst
#setenv FCSTEXEC ${basedir}/gfs/EXP-4densvarupdates/global_fcst_cmip5
setenv FCSTEXEC ${basedir}/gfs/global_fcst.fd.v14.0.1/global_fcst_cmip5
setenv USHGLOBAL $EXECGLOBAL
setenv CYCLESH ${enkfscripts}/global_cyclep.sh
setenv CYCLEXEC ${HOMEGLOBAL}/gfs/global_cycle.fd/global_cyclep
setenv POSTGPSH ${enkfscripts}/global_postgpp.sh 
setenv POSTGPLIST ${FIXGLOBAL}/global_kplist.reanl.txt
# make sure computations for post-processing done on gaussian grid, smoothing off
setenv POSTGPVARS "IDRT=0,IDRTC=4,IOC=$LONB,JOC=$LATB,MOO=255,MOOSLP=0"
setenv POSTPROC "YES" # if yes, compute pgrb files for 6-h forecast for every member.
setenv IO 240
setenv JO 121  
setenv IGEN 425

setenv nbackground_max 24 # should be <= total number of nodes

# 6-h cycle
setenv FHMAX 9
setenv FHMIN 3
setenv FHDFI 0
setenv FHOUT 3
setenv FHLWR 3600
setenv FHSWR 3600
setenv DELTIM 1200
setenv dtphys 600

setenv ANALINC 6
setenv DELTSFC $ANALINC
setenv iau .true.
setenv iau_delthrs 6
setenv iaufhrs "3,6,9"
setenv iau_save $iau

# Variables for High Frequency Output
setenv FHOUT_HF 1      # High Frequency Forecast Output Interval
setenv FHMAX_HF 0      # High Frequency Forecast Length (Hours)
# Variables for input to the Namelist
setenv IEMS 1          # 0-blackbody ground emission; 1-climatology on one-deg map
setenv ISOL 1          # 0--fixed solar constant; 1--changing solar constant
setenv IAER 111        # 111--with stratospheric aerosol, tropospheric aerosol LW, troposphere aerosol SW.
setenv ICO2 2          # 0--fixed CO2 constant; 1--time varying global mean CO2; 2--changing CO2
setenv IALB 0          # 0: climatology sw albedo based on surface veg types;
#                      # 1: MODIS based land surface albedo
setenv IOVR_SW 1       # 0--random cloud overlap for SW; 1--maximum-random cloud overlap for SW
setenv IOVR_LW 1       # 0--random cloud overlap for LW; 1--maximum-random cloud overlap for SW
setenv ISUBC_LW 2
setenv ISUBC_SW 2
setenv ICTM 1
setenv slrd0 0.002 # begin linear damping at this sigma level (default 0.002 results in CFL violations)

# stochastic physics settings
#setenv SPPT 1.0
#setenv SPPT_TAU 21600
#setenv SPPT_LSCALE 500000
#
#setenv SHUM 0.0024
#setenv SHUM_TAU 21600
#setenv SHUM_LSCALE 500000
#
#setenv SKEB 15000
#setenv SKEB_TAU 21600
#setenv SKEB_LSCALE 500000
#setenv SKEB_VARSPECT_OPT 0
#setenv SKEB_VFILT 30
#
#setenv VC 0.0
#setenv VCAMP 0.0
#setenv VC_TAU 21600
#setenv VC_LSCALE 1000000.

setenv SPPT 0.8
setenv SPPT_TAU 21600
setenv SPPT_LSCALE 500000

setenv SHUM 0.005
setenv SHUM_TAU 21600
setenv SHUM_LSCALE 500000

setenv SKEB 0.0
setenv SKEB_TAU 21600
setenv SKEB_LSCALE 250000
setenv SKEB_VARSPECT_OPT 0
setenv SKEB_VFILT 20

setenv VC 0.0
setenv VCAMP 0  
setenv VC_TAU 21600
setenv VC_LSCALE 1000000.

setenv hdif_fac2 1.0 # reduced diffusion

# no stochastic physics
#setenv SKEB 0
#setenv SHUM 0
#setenv SPPT 0
#setenv VC 0
#setenv VCAMP 0

# same as PRHW14 except psautco is lower, and cdmbgwd=0.125,3 instead of cdmbgwd=0.25,2
# reduce horiz diffusion by makeing hdif_fac2 < 1?

#added dspheat=.false. based on jeff's suggestion [6 Feb 2017 G. Compo]
setenv FCSTVARS "IEMS=$IEMS,ICO2=$ICO2,ISOL=$ISOL,IALB=$IALB,IAER=$IAER,ICTM=$ICTM,IOVR_LW=$IOVR_LW,IOVR_SW=$IOVR_SW,slrd0=$slrd0,psautco=2.0e-4,2.0e-4,SPPT=$SPPT,SPPT_TAU=$SPPT_TAU,SPPT_LSCALE=$SPPT_LSCALE,SHUM=$SHUM,SHUM_TAU=$SHUM_TAU,SHUM_LSCALE=$SHUM_LSCALE,SKEB=$SKEB,SKEB_TAU=$SKEB_TAU,SKEB_LSCALE=$SKEB_LSCALE,SKEB_VFILT=$SKEB_VFILT,VC=$VC,VCAMP=$VCAMP,VC_TAU=$VC_TAU,VC_LSCALE=$VC_LSCALE,HYBEDMF=.true.,DSPHEAT=.true.,fixtrc=.false.,.false.,.false.,use_ufo=.true.,sppt_sfclimit=.true.,semilag=.true.,herm_x=.true.,herm_y=.true.,herm_z=.true.,lin_xyz=.false.,wgt_cub_lin_xyz=.false.,wgt_cub_lin_xyz_trc=.false.,settls_dep3ds=.true.,settls_dep3dg=.true.,quamon=.false.,dtphys=$dtphys,lingg_a=.true.,lingg_b=.true.,ref_temp=350.0,sl_epsln=0.05,zflxtvd=.false.,bkgd_vdif_m=1.0,bkgd_vdif_h=1.0,bkgd_vdif_s=1.0,redrag=.true.,hdif_fac2=$hdif_fac2,redgg_a=.true.,levwgt=24,30,random_clds=.false.,crtrh=0.90,0.90,0.90,cdmbgwd=0.125,3.0,ncw=20,120,flgmin=0.18,0.22,cnvgwd=.true.,cgwf=0.5,0.05,ISUBC_LW=$ISUBC_LW,ISUBC_SW=$ISUBC_SW,cal_pre=.true.,dspheat=.false."

setenv startupenv "${datapath}/analdate.csh"
source $startupenv

#------------------------------------------------------------------------
mkdir -p $datapath
mkdir -p $logdir

echo "BaseDir: ${basedir}"
echo "EnKFBin: ${enkfbin}"
echo "DataPath: ${datapath}"
echo "LogDir: ${logdir}"

############################################################################
# Main Program
# Please do not edit the code below; it is not recommended except lines relevant to getsfcensmean.csh.

env

set ncycle=1
while ($ncycle < = $ncycles)

source $datapath/fg_only.csh # define fg_only variable.
# fg_only is true for cold start, false otherwise.
if ($fg_only == "true") then
  setenv iau ".false."
else
  setenv iau $iau_save
endif
# fg_only is true for cold start, false otherwise.

echo "starting the cycle $ncycle out of $ncycles"

# substringing to get yr, mon, day, hr info
setenv yr `echo $analdate | cut -c1-4`
setenv mon `echo $analdate | cut -c5-6`
setenv day `echo $analdate | cut -c7-8`
setenv hr `echo $analdate | cut -c9-10`
setenv ANALHR $hr
# set environment analdate
setenv datapath2 "${datapath}/${analdate}/"

# current analysis time.
setenv analdate $analdate
# previous analysis time.
setenv analdatem1 `${incdate} $analdate -$ANALINC`
# next analysis time.
setenv analdatep1 `${incdate} $analdate $ANALINC`
setenv hrp1 `echo $analdatep1 | cut -c9-10`
setenv hrm1 `echo $analdatem1 | cut -c9-10`
setenv datapathp1 "${datapath}/${analdatep1}/"
setenv datapathm1 "${datapath}/${analdatem1}/"
mkdir -p $datapathp1

# log walltime used by last job
if ($ncycle == 1) then
set walltime=`grep Walltime gfsenkf.err | tail -1 | cut -f4 -d" "`
echo "`date +%Y%m%d%H%M` ncycles=$ncycles $analdatem1 $walltime" >> ${enkfscripts}/walltime.out
endif

date
echo "analdate minus 1: $analdatem1"
echo "analdate: $analdate"
echo "analdate plus 1: $analdatep1"

setenv PREINP "${RUN}.t${hr}z."
setenv PREINP1 "${RUN}.t${hrp1}z."
setenv PREINPm1 "${RUN}.t${hrm1}z."

# make log dir for analdate
setenv current_logdir "${logdir}/ensda_out_${analdate}"
echo "Current LogDir: ${current_logdir}"
mkdir -p ${current_logdir}

if ($fg_only == "false") then

echo "starting ens mean computation `date`"

# extract hdf5 to text file, reformat
/bin/rm -rf ${datapath2}/psobfile
/bin/rm -rf ${datapath2}/psobs.txt
csh ${enkfscripts}/extracth5ps_v4.csh $analdate ${datapath2}/psobfile
ls -l ${datapath2}/psobfile
echo "compute bias correction..."
if ($analdate >= '1999090112') then
 time python ${enkfscripts}/get_biascorr.py ${analdate} ${datapath2}/psobs.txt   &
else
 # to skip bias correction, use this...
 time python ${enkfscripts}/rewrite2.py ${datapath2}/psobfile ${datapath2}/psobs.txt $analdate  &
endif

set fh=$FHMIN
while ($fh <= $FHMIN)
  set charfhr="fhr`printf %02i $fh`"
  if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && ! -s ${datapath}/${analdate}/bfg_${analdate}_${charfhr}_ensmean)) then
     echo "compute ens mean sfc file for $charfhr"
     time aprun -n $nanals -N $corespernode ${EXECGLOBAL}/getsfcensmeanp.x ${datapath}/${analdate}/ bfg_${analdate}_${charfhr}_ensmean bfg_${analdate}_${charfhr} ${nanals}  &
  endif
  @ fh = $fh + $FHOUT
end

set fh=$FHMIN
while ($fh <= $FHMAX)
  set charfhr="fhr`printf %02i $fh`"
  if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && ! -s ${datapath}/${analdate}/sfg_${analdate}_${charfhr}_ensmean)) then
   echo "compute ens mean sig file for $charfhr"
   time aprun -n $nanals -N $corespernode ${EXECGLOBAL}/getsigensmeanp.x ${datapath}/${analdate}/ sfg_${analdate}_${charfhr}_ensmean sfg_${analdate}_${charfhr} ${nanals} & 
  endif
  #@ fh = $fh + $FHOUT
  @ fh = $fh + $FHOUT
end

# compute mean and spread grib files.
set analdate_p3 = `${incdate} $analdatem1 3`
if ($POSTPROC == "YES") then
  echo "compute ens mean panl file"
  time aprun -n $nanals -N $corespernode ${EXECGLOBAL}/gribmeanp.x $datapathm1 $IO $JO $analdatem1 p anl  &
  echo "compute ens mean panl+3 file"
  time aprun -n $nanals -N $corespernode ${EXECGLOBAL}/gribmeanp.x $datapathm1 $IO $JO $analdate_p3 p anl  &
  echo "compute ens mean pfg file"
  time aprun -n $nanals -N $corespernode ${EXECGLOBAL}/gribmeanp.x $datapath2 $IO $JO $analdate p fg 06 &
  echo "compute ens mean sflxf03 file"
  time aprun -n $nanals -N $corespernode ${EXECGLOBAL}/gribmeanp.x $datapath2 $LONB $LATB $analdate sflx fg 03 & 
  echo "compute ens mean sflxf06 file"
  time aprun -n $nanals -N $corespernode ${EXECGLOBAL}/gribmeanp.x $datapath2 $LONB $LATB $analdate sflx fg 06 & 
endif
wait
ls -l ${datapath2}/ps*
echo "done ens mean computation `date`"

# compute rms of ps tend (a measure of imbalance).
if ($FHOUT == 1) then
echo "imbalance diagnostic for member 1"
time aprun -n 1 ${EXECGLOBAL}/getpstend.x  ${datapath}/ ${analdate} mem001
echo "imbalance diagnostic for ensemble mean"
time aprun -n 1 ${EXECGLOBAL}/getpstend.x  ${datapath}/ ${analdate} ensmean
endif

# remove obsfiles from the previous cycle
if ($cleanup_obs == 'true') then
   /bin/rm -f ${datapathp1}/*abias
   /bin/rm -f ${datapathp1}/*satang
   /bin/rm -f ${datapath2}/diag*ensmean
   set nanal=1
   while ($nanal <= $nanals)
     set charnanal="mem`printf %03i $nanal`"
     /bin/rm -f  ${datapath2}/diag*${charnanal}
     @ nanal = $nanal + 1
   end
   /bin/rm -f ${datapath2}/bfg2*
   /bin/rm -f ${datapath2}/hxprime*
endif

# remove previous analyses
if ($cleanup_anal == 'true') then
   /bin/rm -f ${datapath2}/sanl*mem*
endif

set niter=1
set alldone='no'
echo "${analdate} compute analysis increment `date`"
# need symlinks for satbias_angle, satbias_in, satinfo
ln -fs ${current_logdir}/satinfo.out ${datapath2}/fort.207
ln -fs ${current_logdir}/ozinfo.out ${datapath2}/fort.206
ln -fs ${current_logdir}/convinfo.out ${datapath2}/fort.205
setenv ABIAS ${datapathp1}/${PREINP1}abias
while ($alldone == 'no' && $niter <= $nitermax)
    if ($niter == 1) then
     /bin/cp -f ${current_logdir}/ensda.out ${current_logdir}/ensda.out.save$$
     /bin/cp -f ${current_logdir}/ensda_6.out ${current_logdir}/ensda_6.out.save$$
     /bin/cp -f ${current_logdir}/ensda_3.out ${current_logdir}/ensda_3.out.save$$
     csh ${enkfscripts}/${ensda} >&! ${current_logdir}/ensda.out
     set exitstat=$status
    else
     csh ${enkfscripts}/${ensda} >>& ${current_logdir}/ensda.out
     set exitstat=$status
    endif
    if ($exitstat == 0) then
       set alldone='yes'
    else
       echo "some files missing, try again .."
       @ niter = $niter + 1
    endif
end
if($alldone == 'no') then
    echo "Tried ${nitermax} times to run ensda and failed: ${analdate}"
    exit 1
endif
echo "${analdate} done computing analysis increment `date`"
 
endif # skip to here if fg_only = true

# run ensemble first guess.
# first, clean up old first guesses.
if ($cleanup_fg == 'true') then
set fhr=$FHMIN
while ( $fhr <= $FHMAX)
    set charfhr="fhr`printf %02i $fhr`"
    /bin/rm -f ${datapath}${analdatep1}/sfg_${analdatep1}_${charfhr}*_*mem* 
    /bin/rm -f ${datapath}${analdatep1}/bfg_${analdatep1}_${charfhr}*_*mem* 
    @ fhr = $fhr + $FHOUT
end
endif
mkdir -p ${datapath}${analdatep1}

set niter=1
set alldone='no'
echo "${analdate} compute first guesses `date`"
while ($alldone == 'no' && $niter <= $nitermax)
    if ($niter == 1) then
    csh ${enkfscripts}/${fg_gfs} >&! ${current_logdir}/run_fg.out
    set exitstat=$status
    else
    setenv DELTIM `expr $DELTIM / 2` # reduce time step
    csh ${enkfscripts}/${fg_gfs} >>& ${current_logdir}/run_fg.out
    set exitstat=$status
    endif
    if ($exitstat == 0) then
       set alldone='yes'
    else
       echo "some files missing, try again .."
       @ niter = $niter + 1
    endif
end
if($alldone == 'no') then
    echo "Tried ${nitermax} times to run run_fg and failed: ${analdate}"
    exit 1
endif
echo "${analdate} done computing first guesses `date`"

# tar up directory to HPSS.
#if ($day == '01' && $hr == '00') then
#if (($day == '01' || $day == '10' || $day == '20') && $hr == '00') then
#if ($hr == '00') then
#   cd $datapath
#   hsi mkdir ${expname}
#   htar -cvf ${expname}/${analdate}.tar ${analdate}
#   #if ($status != 0) then
#   #  echo "htar of ${analdate} dir failed, not advancing .."
#   #  exit 1
#   #endif
#endif

if ($do_cleanup == "true") then
echo "do cleanup"
if (($day == '01' || $day == '10' || $day == '20') && $hr == '00') then
 echo "no cleanup; tar"
 setenv hpss_date ${analdate}
 echo "setenv hpss_date ${analdate}" >! hpss_date.csh
 echo "would be submitting an hpss job, but it needs to be set up for $USER"
 ###msub hpss.csh
else
# cleanup
/bin/rm -rf ${datapath2}/*tmp*
/bin/rm -f ${datapath2}/satbias_in
/bin/rm -f ${datapath2}/satbias_angle
/bin/rm -f ${datapath2}/satinfo
/bin/rm -f ${datapath2}/convinfo
/bin/rm -f ${datapath2}/ozinfo
/bin/rm -f ${datapath2}/covinflate.dat
/bin/rm -f ${datapath2}/lapserate.dat
/bin/rm -f ${datapath2}/fort.*
/bin/rm -f ${datapath2}/*nml
/bin/rm -f ${datapath2}/*log
set fh=0
while ($fh <= $FHMAX)
 /bin/rm -f ${datapath2}/bfg*fhr0${fh}*mem*
 #if ($fh != $ANALINC) then
   /bin/rm -f ${datapath2}/sfg*fhr0${fh}*mem*
 #endif
 if ($fh != 0 && $fh != $ANALINC) then
   /bin/rm -f ${datapath2}/sflx*fhr0${fh}*mem*
 endif
 @ fh = $fh + $FHOUT
end

# just keep a small subset on dist (ensmean means, spreads)
/bin/rm -f ${datapath2}/sflx*mem*
/bin/rm -f ${datapathm1}/pgrbanl*mem*
/bin/rm -f ${datapath2}/pgrbfg*mem*
/bin/rm -f ${datapath2}/sanl*mem*
/bin/rm -f ${datapath2}/sfcanl*mem*
/bin/rm -f ${datapath2}/diag*mem*
echo "done cleanup"
endif
else
echo "no cleanup"
endif
echo "next analysis time"

setenv analdate `${incdate} $analdate $ANALINC`
echo "setenv analdate ${analdate}" >! $startupenv
setenv fg_only false
echo "setenv fg_only false" >! $datapath/fg_only.csh
@ ncycle = $ncycle + 1
end
cd ${enkfscripts}
msub gfsenkf_20crV3_cmip5oz_new.csh >&! current_jobid

exit 0
