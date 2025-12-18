#!/bin/csh

# standalone enkf script

#$ -j y
#$ -cwd
#$ -l h_rt=06:00:00
#$ -A gfsenkf
##$ -pe thfip 3840  
#$ -pe thfip 3852
#$ -N gfsenkf
#$ -o gfsenkf.out

setenv ensda "run_enkf_gfs"
setenv runobs 'runobs_enkf.csh'
setenv rungsi 'run_gsi_hybrid'
setenv rungfs 'run_gfs'
setenv fg_gfs "run_fg_gfs.csh"
setenv drive_ensmean "drive_gfs"
setenv adjust_vmassdiv 'true' # mass-div post-analysis adjustment
setenv FHDFI 2 # digital filter
setenv enkf_threads 2 # enkf threads
setenv fg_proc 96 # number of cores per enkf fg ens member. 
setenv fg_threads 2 # ens fcst threads
setenv hx_proc 96 # number of cores per instance of gsi for enkf forward operator.
setenv hx_threads 3 # enkf forward operator threads
setenv cleanup_obs 'true' # remove existing obs files
setenv cleanup_anal 'true' # remove existing anal files
setenv cleanup_fg 'true' # remove existing first guess files
setenv cleanup_ensmean 'false' # remove existing ensmean files
setenv do_cleanup 'true' # if true, create tar files, delete *mem* files.
setenv control_fcst 'true'
setenv longer_ens 'true'
setenv do_hybrid 'false'
setenv nproc_gfs_control 960  # number of procs devoted to gfs control fg forecast.
#setenv nproc_gfs_control 1128 # number of procs devoted to gfs control fg forecast.
setenv nproc_gfs_ens 2880  # number of procs devoted to gfs ens fcst.
setenv gfs_control_threads 2 # gfs control fg forecast threads.

setenv basedir /lfs1/projects/fim/whitaker

set datadir=$basedir

# where the data will be created
setenv datapath "${datadir}/gfsenkf_t574/"

source $datapath/fg_only.csh # define fg_only variable.
source $datapath/skip_gfs.csh # define skip_gfs variable.
# if skip_gfs false, try to do gfs analysis
# if skip_gfs true, gfs analysis already done
# if skip_gfs true2, cycling with gdas cutoff only
# fg_only is true for cold start, false otherwise.
if ($fg_only == 'true') set longer_ens='false'

# create hostfile with all allocated hosts
set hosts = `cat $TMPDIR/machines`
echo $hosts
set nhosts = $#hosts
/bin/rm -f ${datapath}/hostfileall
set node=1
set corespernode=0
set hostsave=''
while ($node <= $NSLOTS) 
   set host1 = `echo $hosts[$node]`
   if ($corespernode == 0 && $node > 1 && $host1 != $hostsave) set corespernode=$node
   echo "${host1}" >> ${datapath}/hostfileall
   @ node = $node + 1
   set hostsave=$host1
end
@ corespernode = $corespernode - 1
echo "cores per node = $corespernode"
setenv corespernode $corespernode

# don't include first node (so job will not hang if it runs out of memory)
@ nodesuse = $NSLOTS - $corespernode
tail -$nodesuse ${datapath}/hostfileall > $TMPDIR/hostfile_tmp
/bin/mv -f $TMPDIR/hostfile_tmp ${datapath}/hostfileall
set hosts=`cat ${datapath}/hostfileall`
echo $hosts
set nhosts=$#hosts

# Data reside in obs directory set dynamically in loop below ${obsdir}

# log directory
setenv logdir "${datadir}/logs/gfsenkf_t574/"

# some scripts reside here
# also need to make this dependent on user or a group writeable area -compo

setenv enkfscripts "${basedir}/gfsenkf/scripts/realtime2011"
setenv enkfexec "${basedir}/ncepsvn/enkf/src_trunk"

# name of enkf executable.
setenv enkfbin "${enkfexec}/global_enkf_gfs"

setenv incdate "${enkfscripts}/incdate"

setenv homedir $PWD
setenv qcomp ecomp

##########################################################################
# enkf parameters.
setenv corrlengthnh 2000
setenv corrlengthtr 2000
setenv corrlengthsh 2000
setenv lnsigcutoffnh 1.5
setenv lnsigcutofftr 1.5
setenv lnsigcutoffsh 1.5
setenv lnsigcutoffpsnh 1.5
setenv lnsigcutoffpstr 1.5
setenv lnsigcutoffpssh 1.5
setenv lnsigcutoffsatnh 1.5  
setenv lnsigcutoffsattr 1.5  
setenv lnsigcutoffsatsh 1.5  
setenv obtimelnh 1.e30       
setenv obtimeltr 1.e30       
setenv obtimelsh 1.e30       
setenv readin_localization .true.
setenv localization_factor 125

# Assimilation parameters
setenv JCAP 574
setenv USE_THREADS "YES"
setenv oldpost "true"
setenv LEVS 64
setenv LONB 1760 
setenv LATB 880
setenv DELTIM 120
setenv LONA 1152  
setenv LATA 576  
setenv JCAP_ens $JCAP
setenv LEVS_ens $LEVS
setenv LONB_ens $LONB    
setenv LATB_ens $LATB
setenv DELTIM_ens $DELTIM
setenv JCAP_CNTL $JCAP
setenv LATB_CNTL $LATB
setenv LONB_CNTL $LONB
setenv LEVS_CNTL $LEVS
setenv DELTIM_CNTL $DELTIM
setenv SMOOTHINF 35
setenv npts `expr \( $LONA \) \* \( $LATA \)`
setenv LSOIL 4
setenv obs_datapath_gdas "/lfs1/projects/fim/whitaker/bufr/gdas1"
setenv obs_datapath_gfs "/lfs1/projects/fim/whitaker/bufr"
setenv NTRAC 3
setenv nvars 5
setenv reducedgrid .true.
setenv LANDICE_OPT 2
# parameters for additive inflation
setenv scalefact 35 
setenv addpertpath "/lfs1/projects/fim/whitaker/adderr/t574l64/"
setenv lonscramble 0
setenv backuphrs 0

setenv iassim_order 0

setenv covinflatemax 1.e2
setenv covinflatemin 1.0                                            
setenv analpertwtnh 0.9 
setenv analpertwtsh 0.9
setenv analpertwttr 0.9
setenv pseudo_rh .true.
                                                                    
setenv sprd_tol 1.e30
setenv varqc .false.
setenv huber .false.
setenv zhuberleft 1.e10
setenv zhuberright 1.e10
setenv numiter 3

#setenv sprd_tol 10.
#setenv varqc .true.
#setenv huber .true.
#setenv zhuberleft 1.1
#setenv zhuberright 1.1
#setenv numiter 10
                                                                    
setenv nanals 80                                                    
                                                                    
setenv paoverpb_thresh 0.99                                         
setenv saterrfact 1.0
setenv deterministic .true.
setenv sortinc .true.
setenv lupd_satbiasc .true.
                                                                    
setenv nitermax 3

##########################################################################
# Some binaries and scripts reside here
#

setenv HOMEGLOBAL ${basedir}/gfsenkf
setenv FIXGLOBAL /lfs1/projects/fim/whitaker/fix_200912
setenv gsipath /lfs1/projects/fim/whitaker/ncepsvn/EXP-hybens
setenv gsiexec ${gsipath}/src/global_gsi
setenv EXECGLOBAL ${HOMEGLOBAL}/bin
setenv SIGLEVEL ${FIXGLOBAL}/global_hyblev.l64.txt
setenv FCSTEXEC ${EXECGLOBAL}/global_fcst_rfcq3fy10_update
setenv USHGLOBAL $EXECGLOBAL
setenv CHGRESSH ${enkfscripts}/global_chgresp.sh
setenv CYCLESH ${enkfscripts}/global_cyclep.sh
setenv POSTPROC NO # compute pgb files for each ensemble member (analysis and 6-h forecast)
setenv POSTGPSH ${enkfscripts}/global_postgpp.sh 
setenv POSTGPLIST ${FIXGLOBAL}/global_kplist.private.txt
setenv POSTGPEXEC $EXECGLOBAL/global_postgp
# make sure computations for post-processing done on gaussian grid, smoothing off
#setenv POSTGPVARS "IDRT=4,IDRTC=4,IOC=$LONB,JOC=$LATB,MOO=255,MOOSLP=0"
#setenv IO $LONB   
#setenv JO $LATB
setenv POSTGPVARS "IDRT=0,IDRTC=4,IOC=$LONB_CNTL,JOC=$LATB_CNTL,MOO=255,MOOSLP=0"
setenv IO 720   
setenv JO 361   
#setenv ENS_SPS true
#setenv FH_INC 6

# parameters for tc_ps observations for ensemble
setenv tcp_obberr_ens 1.25
setenv tcp_innmax_ens 30
setenv tcp_oedelt_ens 4.0

# 6-h cycle
setenv FHMAX 9
setenv FHMIN 3
setenv FHOUT 1
setenv FHLWR 1
setenv FHMAX_LONG 168
setenv FHOUT_LONG 6  
setenv FHDFI_LONG $FHDFI

setenv ANALINC 6
setenv DELTSFC $ANALINC
# Variables for High Frequency Output
setenv FHOUT_HF 1      # High Frequency Forecast Output Interval
setenv FHMAX_HF 0      # High Frequency Forecast Length (Hours)
#
# # 201006 values
# # Variables for input to the Namelist
 setenv IEMS 0          # 0-blackbody ground emission; 1-climatology on one-deg map
 setenv ISOL 0          # 0--fixed solar constant; 1--changing solar constant
 setenv IAER 111        # 111--with stratospheric aerosol, tropospheric aerosol LW, troposphere aerosol SW.
 setenv ICO2 1          # 0--fixed CO2 constant; 1--time varying global mean CO2; 2--changing CO2
 setenv ialb 0          # 0: climatology sw albedo based on surface veg types;
#                        # 1: MODIS based land surface albedo
 setenv IOVR_SW 1       # 0--random cloud overlap for SW; 1--maximum-random cloud overlap for SW
#  Other parameters input to the model
 setenv FCSTVARS "ras=.false.,nsout=0,lsm=1,tfiltc=0.85,liope=.true.,zhao_mic=.true.,ncw=50,150,crtrh=0.85,0.85,0.85,flgmin=0.220 ,IALB=$ialb,ccnorm=.false.,OUT_VIRTTEMP=.true.,LDIAG3D=.false.,mstrat=.false.,ctei_rm=0.50,MOM4ICE=.false.,cnvgwd=.false.,RUN_ENTHALPY=.false.,IOVR_SW=$IOVR_SW,zflxtvd=.true.,sashal=.true.,old_monin=.false.,newsas=.true.,"
 #export FCSTVARS="ras=.false.,nsout=0,lsm=1,tfiltc=0.85,liope=.true.,zhao_mic=.true.,old_monin=.true.,ncw=50,150,crtrh=0.85,0.85,0.85,flgmin=0.220,IALB=$ialb,ccnorm=.false.,OUT_VIRTTEMP=.false.,LDIAG3D=.false.,mstrat=.false.,ctei_rm=0.50,MOM4ICE=.false.,cnvgwd=.false.,RUN_ENTHALPY=.false.,IOVR_SW=$IOVR_SW,"
#
# 200912 oper values
# Variables for input to the Namelist
#setenv IAER 001        # 111--with stratospheric aerosol, tropospheric aerosol LW, troposphere aerosol SW.
#setenv IOVR_SW 0       # 0--random cloud overlap for SW; 1--maximum-random cloud overlap for SW
#  Other parameters input to the model
#setenv FCSTVARS "ras=.false.,nsout=0,lsm=1,tfiltc=0.85,liope=.true.,zhao_mic=.true.,old_monin=.true.,ncw=50,150,crtrh=0.85,0.85,0.85,flgmin=0.220,IALB=$ialb,ccnorm=.false.,OUT_VIRTTEMP=.true.,LDIAG3D=.false.,mstrat=.false.,ctei_rm=0.50,MOM4ICE=.false.,cnvgwd=.false.,RUN_ENTHALPY=.false.,IOVR_SW=$IOVR_SW,"

set lastbufrdate = `cat ${obs_datapath_gfs}/last_bufr_date`
setenv startupenv "${datapath}/analdate.csh"
source $startupenv
if ($analdate > $lastbufrdate) then
 echo "last GFS bufr data is for $lastbufrdate"
 echo "trying to do GFS analysis for $analdate"
 echo "GFS bufr data not yet in for this analysis date"
 sleep 60
 qsub gfsenkf.csh
 exit 1
endif

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
echo "starting the cycle"

# substringing to get yr, mon, day, hr info
setenv yr `echo $analdate | cut -c1-4`
setenv mon `echo $analdate | cut -c5-6`
setenv day `echo $analdate | cut -c7-8`
setenv hr `echo $analdate | cut -c9-10`
setenv ANALHR $hr
# set environment analdate
setenv datapath2 "${datapath}/${analdate}/"
setenv hostfilein ${datapath}/hostfileall

# current analysis time.
setenv analdate $analdate
# previous analysis time.
setenv analdatem1 `${incdate} $analdate -$ANALINC`
# next analysis time.
setenv analdatep1 `${incdate} $analdate $ANALINC`
setenv analdate_adderr `${incdate} $analdate -$backuphrs`
setenv hrp1 `echo $analdatep1 | cut -c9-10`
setenv hrm1 `echo $analdatem1 | cut -c9-10`
setenv datapathp1 "${datapath}/${analdatep1}/"
setenv datapathm1 "${datapath}/${analdatem1}/"
mkdir -p $datapathp1

date
echo "analdate minus 1: $analdatem1"
echo "analdate: $analdate"
echo "analdate plus 1: $analdatep1"

# make log dir for analdate
setenv current_logdir "${logdir}/ensda_out_${analdate}"
echo "Current LogDir: ${current_logdir}"
mkdir -p ${current_logdir}

# estimate covariance length scales from first guess ensemble.
# (creates hybens_locinfo file in datapath2)
if ($fg_only == 'false' && $readin_localization == ".true.") then
#cd $datapath2
#time mpirun -hostfile $TMPDIR/machines -np $nanals ${enkfexec}/calc_corrlength.x $nanals ${analdate} $LONA $LATA $localization_factor
/bin/rm -f $datapath2/hybens_locinfo
/bin/rm -f $datapath2/hybens_smoothinfo
/bin/cp -f ${gsipath}/fix/hybens_locinfo $datapath2
#/bin/cp -f ${gsipath}/fix/hybens_smoothinfo $datapath2/hybens_smoothinfo
endif

if ($fg_only ==  'false') then
echo "starting ens mean computation `date`"
csh ${enkfscripts}/compute_ensmeans_smooth.csh >&!  ${current_logdir}/compute_ensmeans.out
echo "done ens mean computation `date`"
endif

# in background, compute hybrid analysis (using first half of allocated core set).

if ($skip_gfs == 'false' && $fg_only == 'false') then
setenv obs_datapath $obs_datapath_gfs
setenv RUN gfs
setenv DOSFCANL "YES"
setenv PREINP "${RUN}.t${hr}z."
setenv PREINP1 "${RUN}.t${hrp1}z."
setenv PREINPm1 "${RUN}.t${hrm1}z."

# compute enkf analyses.

echo "run $RUN enkf `date`"
csh ${enkfscripts}/runenkf.csh  >&! ${current_logdir}/run_enkf.out  

set enkf_done=`cat ${current_logdir}/run_enkf.log`
if ($enkf_done == 'yes') then
  echo "$RUN enkf analysis completed successfully `date`"
else
  echo "$RUN enkf analysis did not complete successfully, exiting `date`"
  exit 1
endif
/bin/mv -f ${datapath2}/gsistats.${analdate}_ensmean ${datapath2}/gsistats.${analdate}_ensmean.gfs
/bin/mv -f ${datapath2}/diag_conv_ges.${analdate}_ensmean ${datapath2}/diag_conv_ges.${analdate}_ensmean.gfs

# make symlinks for "control" analysis.
ln -fs ${datapath2}/sanl${RUN}_${analdate}_ensmean ${datapath2}/sanl${RUN}_${analdate}_control
ln -fs ${datapath2}/sfcanl${RUN}_${analdate}_ensmean ${datapath2}/sfcanl${RUN}_${analdate}_control
ln -fs ${datapath2}/sanl${RUN}_${analdate}_ensmean ${datapath2}/sanl_${analdate}_control
ln -fs ${datapath2}/sfcanl${RUN}_${analdate}_ensmean ${datapath2}/sfcanl_${analdate}_control

# optionally, adjust vertically integrated mass div for EnKF analysis ensemble.
if ($adjust_vmassdiv == 'true') then
   echo "adjust vertically integrated mass div in ${RUN} EnKF ensemble"
   csh ${enkfscripts}/adjustmassdiv.csh >&! ${current_logdir}/adjustmassdiv_${RUN}.out
endif

# run control forecast inline.
#if ($control_fcst == 'true') then
#   echo "running control forecast from ${RUN} ens. mean analysis `date` ..."
#   setenv charnanal "ensmean"
#   csh ${enkfscripts}/run_control_fcst.csh >&! ${current_logdir}/run_control_fcst.out
#   echo "done running control forecast from ${RUN} ens. mean analysis `date` ..."
#endif

# run control forecast and ensemble concurrently inline on separate sets of procs.
echo "run longer forecasts `date`"
head -$nproc_gfs_control ${datapath}/hostfileall >! ${datapath}/hostfile1
if ($control_fcst == 'true') then
   echo "running control forecast from ${RUN} ens. mean analysis `date` ..."
   setenv charnanal "ensmean"
   setenv hostfilein ${datapath}/hostfile1
   csh ${enkfscripts}/run_control_fcst.csh >&! ${current_logdir}/run_control_fcst.out &
endif
tail -$nproc_gfs_ens ${datapath}/hostfileall >! ${datapath}/hostfile2
if ($longer_ens == "true") then
   # this ensemble used for regional EnKF ICs and BCs
   setenv hostfilein ${datapath}/hostfile2
   setenv nanals2 30 # only a subset, ensemble will be recentered.
   setenv runtracker "false"
   set FHMAX_save=$FHMAX_LONG
   set FHOUT_save=$FHOUT_LONG
   set FHDFI_save=$FHDFI_LONG
   setenv FHMAX_LONG 12
   setenv FHOUT_LONG 1
   setenv FHDFI_LONG 1
   csh run_ens_fcst.csh >&! ${current_logdir}/run_ens_fcst.out  &
   setenv FHMAX_LONG $FHMAX_save
   setenv FHOUT_LONG $FHOUT_save
   setenv FHDFI_LONG $FHDFI_save
endif
wait
echo "done running longer forecasts `date`"
setenv hostfilein ${datapath}/hostfileall
 
# end of analysis, forecasts for GFS cutoff
endif # skip to here if fg_only = true or fg_only == true

# now for GDAS cutoff...
# in background, compute hybrid analysis (using first half of allocated core set).


if ($skip_gfs != 'true') echo "setenv skip_gfs true" >! $datapath/skip_gfs.csh
set lastbufrdate = `cat ${obs_datapath_gdas}/last_bufr_date`
setenv startupenv "${datapath}/analdate.csh"
source $startupenv
if ($analdate > $lastbufrdate) then
 echo "last GDAS bufr data is for $lastbufrdate"
 echo "trying to do GDAS analysis for $analdate"
 echo "GDAS bufr data not yet in for this analysis date"
 sleep 60
 qsub gfsenkf.csh
 exit 1
endif

setenv obs_datapath $obs_datapath_gdas
setenv RUN gdas1
# if gfs cutoff run, sfcanl already created.
if ($skip_gfs == 'true2') then
   setenv DOSFCANL "YES"
else
   setenv DOSFCANL "NO"
endif
setenv PREINP "${RUN}.t${hr}z."
setenv PREINP1 "${RUN}.t${hrp1}z."
setenv PREINPm1 "${RUN}.t${hrm1}z."

if ($fg_only == "false") then

# compute enkf analyses.

echo "run $RUN enkf `date`"
csh ${enkfscripts}/runenkf.csh  >&! ${current_logdir}/run_enkf.out  

set enkf_done=`cat ${current_logdir}/run_enkf.log`
if ($enkf_done == 'yes') then
  echo "$RUN enkf analysis completed successfully `date`"
else
  echo "$RUN enkf analysis did not complete successfully, exiting `date`"
  exit 1
endif

# make symlinks for "control" analysis.
ln -fs ${datapath2}/sanl${RUN}_${analdate}_ensmean ${datapath2}/sanl${RUN}_${analdate}_control
ln -fs ${datapath2}/sfcanl${RUN}_${analdate}_ensmean ${datapath2}/sfcanl${RUN}_${analdate}_control
if ($skip_gfs == 'true2') then
   ln -fs ${datapath2}/sanl${RUN}_${analdate}_ensmean ${datapath2}/sanl_${analdate}_control
   ln -fs ${datapath2}/sfcanl${RUN}_${analdate}_ensmean ${datapath2}/sfcanl_${analdate}_control
endif

# optionally, adjust vertically integrated mass div for EnKF analysis ensemble.
if ($adjust_vmassdiv == 'true') then
   echo "adjust vertically integrated mass div in ${RUN} EnKF ensemble"
   csh ${enkfscripts}/adjustmassdiv.csh >&! ${current_logdir}/adjustmassdiv_${RUN}.out
endif

# post-process analysis and first guess mean.
cd $enkfscripts
setenv charnanal "ensmean"
qsub -V postproc.csh # postprocess GDAS control analysis

# run control forecast inline (if gfs cutoff skipped).
if ($skip_gfs == 'true2' && $control_fcst == 'true') then
   echo "running control forecast from ${RUN} ens. mean analysis `date` ..."
   setenv charnanal "ensmean"
   csh ${enkfscripts}/run_control_fcst.csh >&! ${current_logdir}/run_control_fcst.out
   echo "done running control forecast from ${RUN} ens. mean analysis `date` ..."
endif

# end of analysis for GDAS cutoff
endif # skip to here if fg_only = true

# advance ensemble to next analysis time.
echo "run enkf ens first guess `date`"
csh ${enkfscripts}/run_fg_ens.csh  >&! ${current_logdir}/run_fg_ens.out  

set enkf_done=`cat ${current_logdir}/run_fg_ens.log`
if ($enkf_done == 'yes') then
  echo "enkf first-guess completed successfully `date`"
else
  echo "enkf first-guess did not complete successfully, exiting `date`"
  exit 1
endif

if ($fg_only == 'false') then

# compute mean and spread grib files.
# pressure level grib files.
set nhost=1
if ($POSTPROC == "YES") then
  setenv HOSTFILE ${datapath2}/hostfilepanl
  set host = `echo $hosts[$nhost]`
  echo "${host}" >! ${HOSTFILE}
  echo "HOSTFILE for node $nhost"
  cat $HOSTFILE
  @ nhost = $nhost + $corespernode
  mpirun -hostfile $HOSTFILE -np 1 "IO=$IO" "JO=$JO" ${enkfexec}/gribmean.x $datapath2 $nanals $analdate p anl &
  #time ${enkfexec}/gribmean.x $datapath2 $nanals $analdate p anl
  setenv HOSTFILE ${datapath2}/hostfilepfg
  set host = `echo $hosts[$nhost]`
  echo "${host}" >! ${HOSTFILE}
  echo "HOSTFILE for core $nhost"
  cat $HOSTFILE
  @ nhost = $nhost + $corespernode
  mpirun -hostfile $HOSTFILE -np 1 "IO=$IO" "JO=$JO" ${enkfexec}/gribmean.x $datapathp1 $nanals $analdatep1 p fg 06 &
  #time ${enkfexec}/gribmean.x $datapathp1 $nanals $analdatep1 p fg $fhr
endif
# surface flux grib files.
set fh=0
while ($fh <= $FHMAX)
   set fhr=`printf %02i $fh`
   setenv HOSTFILE ${datapath2}/hostfilesflx${fh}
   set host = `echo $hosts[$nhost]`
   echo "${host}" >! ${HOSTFILE}
   echo "HOSTFILE for core $nhost"
   cat $HOSTFILE
   @ nhost = $nhost + $corespernode
   mpirun -hostfile $HOSTFILE -np 1 "LONB=$LONB" "LATB=$LATB" ${enkfexec}/gribmean.x $datapathp1 $nanals $analdatep1 sflx fg $fhr &
   #time ${enkfexec}/gribmean.x $datapathp1 $nanals $analdatep1 sflx fg $fhr
   @ fh = $fh + $FHOUT
end
wait

# cleanup
if ($do_cleanup == 'true') then
echo "clean up files `date`"
cd $datapath2

# move every member files to a temp dir.
mkdir memtmp
mv sfcanl_*${analdate}*mem*  memtmp
mv sanlgdas*${analdate}*mem*  memtmp
#mv sanlgfs*${analdate}*mem*  memtmp
mv pgrbanl*${analdate}*mem* memtmp
mv pgrbfg*${analdate}*fhr0${ANALINC}*mem* memtmp
mv sflxgrb*${analdate}*fhr00*mem* memtmp
#mv sflxgrb*${analdate}*fhr0${ANALINC}**mem* memtmp
mv diag_conv*mem* memtmp

set sattypes = `ls $datapath2/diag*mem001 | cut -d "_" -f3,4 | uniq`
foreach sattype ($sattypes)
 /bin/rm -f diag*${sattype}*mem*
 # remove zero size ensmean diag files
 if ( ! -s diag_${sattype}_ges.ensmean ) then
    /bin/rm -f diag_${sattype}_ges*ensmean
 endif
end

# remove unwanted files.
/bin/rm -f diag*mem*
/bin/rm -f *.sigr1 *.sigr2 *.sfcr *.log* 
/bin/rm -rf hostfile
/bin/rm -rf gsitmp*
/bin/rm -rf gfstmp*
/bin/rm -rf chgrestmp.*
/bin/rm -f *.nml
/bin/rm -f sfcanl*mem*
/bin/rm -f fort*
/bin/rm -f dates.dat
set fh=0
while ($fh <= $FHMAX)
 set fhr=`printf %02i $fh`
 /bin/rm -f bfg*fhr$fhr*mem*
 /bin/rm -f sfg*fhr$fhr*mem*
 /bin/rm -f sflxgrb*fhr$fhr*mem*
 /bin/rm -f pgrb*fhr$fhr*mem*
 @ fh = $fh + $FHOUT
end
/bin/rm -f *mem*

/bin/rm -f hostfile*
/bin/rm -rf postgptmp*
/bin/rm -f covinflate.dat
/bin/rm -f sanlp*
/bin/rm -f log*
/bin/rm -f ozinfo convinfo satinfo scaninfo

# move every member files back
mv memtmp/sfcanl*${analdate}*mem*  .
mv memtmp/sanl*${analdate}*mem*  .
mv memtmp/pgrbanl*${analdate}*mem* .
mv memtmp/pgrbfg*${analdate}*fhr0${ANALINC}*mem* .
mv memtmp/sflx*fhr00*mem* .
#mv memtmp/sflx*fhr0${ANALINC}*mem* .
mv memtmp/diag_conv*mem* .
/bin/rm -rf memtmp
endif
cd $enkfscripts

wait # wait for backgrounded processes to finish
echo "${analdate} done cleanup `date`"

endif # skip to here if fg_only = true

# next analdate: increment by $ANALINC
set analdate_save=$analdate
setenv analdate `${incdate} $analdate $ANALINC`

echo "setenv analdate ${analdate}" >! $startupenv
echo "setenv fg_only false" >! $datapath/fg_only.csh
if ($skip_gfs != 'true2') echo "setenv skip_gfs false" >! $datapath/skip_gfs.csh

# resubmit this script.
qsub gfsenkf.csh

exit 0
