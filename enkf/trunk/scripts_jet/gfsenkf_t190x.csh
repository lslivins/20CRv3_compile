#!/bin/csh

#$ -j y
#$ -cwd
#$ -l h_rt=04:00:00
#$ -A fim
#$ -pe ncomp 320
#$ -N gfsenkf_t190x
#$ -o gfsenkf_t190x.out

set ensda = "run_enkf_gfs"
set runobs = 'runobs_enkf.csh'
setenv rungsi 'run_gsi_hybrid'
set fg_gfs = "run_fg_gfs"
set drive_ensmean = "drive_gfs"
set cleanup_obs = 'true' # remove existing obs files
set cleanup_anal = 'true' # remove existing anal files
set cleanup_fg = 'true' # remove existing first guess files
set cleanup_ensmean = 'true' # remove existing ensmean files
set do_cleanup = 'true' # if true, create tar files, delete *mem* files.
set control_fcst = 'false'
setenv longer_ens 'true'


setenv basedir /lfs1/projects/fim/whitaker

set datadir=$basedir

# where the data will be created
setenv datapath "${datadir}/gfsenkf_t190x/"

source $datapath/fg_only.csh # define fg_only variable.
# fg_only is true for cold start, false otherwise.
if ($fg_only == 'true') set longer_ens='false'

# create hostfile with all allocated hosts
set hosts = `cat $TMPDIR/machines`
echo $hosts
set nhosts = $#hosts
/bin/rm -f ${datapath}/hostfileall
set node=1
while ($node <= $NSLOTS) 
   set host1 = `echo $hosts[$node]`
   echo "${host1}" >> ${datapath}/hostfileall
   @ node = $node + 1
end

# Data reside in obs directory set dynamically in loop below ${obsdir}

# log directory
setenv logdir "${datadir}/logs/gfsenkf_t190x"

# some scripts reside here
# also need to make this dependent on user or a group writeable area -compo

setenv enkfscripts "${basedir}/gfsenkf/scripts"
setenv enkfexec "${basedir}/gfsenkf/enkf_gfs"

# name of enkf executable.
setenv enkfbin "${enkfexec}/global_enkf_gfs"

setenv incdate "${enkfscripts}/incdate"

setenv homedir $PWD
setenv qcomp ecomp

##########################################################################
# enkf parameters.
setenv corrlengthnh 1600
setenv corrlengthtr 1600
setenv corrlengthsh 1600
setenv lnsigcutoffnh 1.1
setenv lnsigcutofftr 1.1
setenv lnsigcutoffsh 1.1
setenv lnsigcutoffpsnh 2.2
setenv lnsigcutoffpstr 2.2
setenv lnsigcutoffpssh 2.2
setenv lnsigcutoffsatnh 3.3  
setenv lnsigcutoffsattr 3.3  
setenv lnsigcutoffsatsh 3.3  
setenv obtimelnh 16.       
setenv obtimeltr 16.       
setenv obtimelsh 16.       

# Assimilation parameters
setenv JCAP 190
setenv JCAP_HIGH 382
setenv LEVS 64
setenv LONB 576  
setenv LATB 288 
setenv LONA $LONB   
setenv LATA $LATB   
setenv JCAP_ens $JCAP
setenv LEVS_ens 28
setenv LONB_ens $LONB  
setenv LATB_ens $LATB
setenv LONA $LONB   
setenv LATA $LATB   
setenv SMOOTHINF 24 
setenv LONB_HIGH 1152
setenv LATB_HIGH 576
setenv npts `expr \( $LONA \) \* \( $LATA \)`
setenv LSOIL 4
setenv RUN "gfs"
setenv obs_datapath "/lfs1/projects/fim/whitaker/bufr"
setenv NTRAC 3
setenv nvars 3
setenv ntrac_update 2
setenv LANDICE_OPT 2
# parameters for additive inflation
setenv scalefact 40 
setenv addpertpath "/lfs1/projects/fim/whitaker/adderr/t${JCAP}l${LEVS}/"
setenv addpertinc 12
setenv lonscramble 1
setenv runprefix "gdas1"
setenv backuphrs 0

setenv simple_partition .false.
setenv random_partition .false.
setenv iassim_order 0
setenv use_height .false.

setenv covinflatemax 1.e2
setenv covinflatemin 1.0                                            
setenv analpertwtnh 0.9 
setenv analpertwtsh 0.9
setenv analpertwttr 0.9
setenv covinflatenh 0.0
setenv covinflatetr 0.0
setenv covinflatesh 0.0
setenv lnsigcovinfcutoff 1.e30
setenv pseudo_rh .true.
                                                                    
setenv sprd_tol 1.e30                                               
setenv probgrosserr -0.001 # turn off varqc by setting to zero.
setenv grosserrwidth 1.e30
setenv varqc .false.
                                                                    
setenv nanals 80                                                    
setenv nanals2 20 # longer ens.
                                                                    
setenv paoverpb_thresh 1.0                                          
setenv saterrfact 1.0
setenv deterministic .true.
setenv sortinc .true.
setenv numiter 3
                                                                    
setenv nitermax 3

##########################################################################
# Some binaries and scripts reside here
#

setenv HOMEGLOBAL ${basedir}/gfsenkf
setenv FIXGLOBAL /lfs1/projects/fim/whitaker/fix_200912
setenv gsipath /lfs1/projects/fim/whitaker/ncepsvn/gsi_6890
setenv EXECGLOBAL ${HOMEGLOBAL}/bin
setenv SIGLEVEL ${FIXGLOBAL}/global_hyblev.l64.txt
setenv GSIEXEC ${EXECGLOBAL}/global_gsi
setenv FCSTEXEC ${EXECGLOBAL}/global_fcst_q409
setenv USHGLOBAL $EXECGLOBAL
setenv CHGRESSH ${enkfscripts}/global_chgresp.sh
setenv CYCLESH ${enkfscripts}/global_cyclep.sh
setenv POSTGPSH ${enkfscripts}/global_postgpp.sh 
setenv POSTGPLIST ${FIXGLOBAL}/global_kplist.1d.txt
#setenv POSTGPEXEC $EXECGLOBAL/global_postgs
setenv POSTGPEXEC /lfs0/projects/reanl/whitaker/gfsenkf/bin/global_postgs
# make sure computations for post-processing done on gaussian grid, smoothing off
setenv POSTGPVARS "IDRT=0,IDRTC=4,IOC=$LONB,JOC=$LATB,MOO=255,MOOSLP=0"
setenv POSTPROC "YES" # if yes, compute pgrb files for 6-h forecast for every member.
setenv IO 144
setenv JO 73   
setenv IO_HIGH 720
setenv JO_HIGH 361
setenv ENS_SPS true
setenv FH_INC 6


#if ($NSLOTS > $JCAP_HIGH) then
#  setenv ensmeanfcst_nprocs $JCAP_HIGH
#else
#  setenv ensmeanfcst_nprocs $NSLOTS
#endif
setenv ensmeanfcst_nprocs 256
setenv fg_proc 8  
setenv hx_proc 16

setenv nbackground_max 24 # should be <= total number of nodes


# 6-h cycle
setenv FHMAX 9
setenv FHMIN 3
setenv FHDFI 2
setenv FHOUT 1
setenv FHLWR 1

setenv ANALINC 6
setenv DELTSFC $ANALINC

set lastbufrdate = `cat ${obs_datapath}/last_bufr_date`
setenv startupenv "${datapath}/analdate.csh"
source $startupenv
if ($analdate > $lastbufrdate) then
 echo "last bufr data is for $lastbufrdate"
 echo "trying to do analysis for $analdate"
 echo "bufr data not yet in for this analysis date"
 sleep 60
 qsub gfsenkf_t190x.csh
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
# copy hostfileall to working dir.
/bin/cp -f ${datapath}/hostfileall ${datapath2}

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

setenv PREINP "${RUN}.t${hr}z."
setenv PREINP1 "${RUN}.t${hrp1}z."
setenv PREINPm1 "${RUN}.t${hrm1}z."

# make log dir for analdate
setenv current_logdir "${logdir}/ensda_out_${analdate}"
echo "Current LogDir: ${current_logdir}"
mkdir -p ${current_logdir}

if ($fg_only == "false") then

echo "starting ens mean computation `date`"
set fh=${FHMIN}
while ($fh <= $FHMAX)
  set charfhr="fhr`printf %02i $fh`"
  if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && ! -s ${datapath}/${analdate}/bfg_${analdate}_${charfhr}_ensmean)) then
  ${enkfexec}/getsfcensmean.x ${datapath}/${analdate}/ bfg_${analdate}_${charfhr}_ensmean bfg_${analdate}_${charfhr} ${nanals} &
  endif
  @ fh = $fh + $FHOUT
end
wait
set fh=${FHMIN}
#set fh=$FHOUT # use this if getpstend is run
while ($fh <= $FHMAX)
  set charfhr="fhr`printf %02i $fh`"
  if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && ! -s ${datapath}/${analdate}/sfg_${analdate}_${charfhr}_ensmean)) then
  ${enkfexec}/getsigensmean.x ${datapath}/${analdate}/ sfg_${analdate}_${charfhr}_ensmean sfg_${analdate}_${charfhr} ${nanals} ${charfhr}&
  endif
  @ fh = $fh + $FHOUT
end
wait
echo "done ens mean computation `date`"

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

set niter=1
set alldone='no'
echo "${analdate} compute forward operator `date`"
while ($alldone == 'no' && $niter <= $nitermax)
    if ($niter == 1) then
     csh ${enkfscripts}/${runobs} >&! ${current_logdir}/run_obs.out
     set exitstat=$status
    else
     csh ${enkfscripts}/${runobs} >>& ${current_logdir}/run_obs.out
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
    echo "Tried ${nitermax} times and to create obs files and failed: ${analdate}"
    exit 1
endif
echo "${analdate} done computing forward operator `date`"

# remove previous analyses
if ($cleanup_anal == 'true') then
   /bin/rm -f ${datapath2}/sanl*mem*
endif

set niter=1
set alldone='no'
echo "${analdate} compute analysis increment `date`"
# need symlinks for satbias_angle, satbias_in, satinfo
setenv GBIAS   ${datapathm1}/${PREINPm1}abias 
setenv GSATANG ${datapathm1}/${PREINPm1}satang 
ln -fs $GBIAS   ${datapath2}/satbias_in
ln -fs $GSATANG ${datapath2}/satbias_angle
ln -fs ${gsipath}/fix/global_satinfo.txt ${datapath2}/satinfo
ln -fs ${gsipath}/fix/global_convinfo.txt ${datapath2}/convinfo
ln -fs ${gsipath}/fix/global_ozinfo.txt ${datapath2}/ozinfo
ln -fs ${current_logdir}/satinfo.out ${datapath2}/fort.207
ln -fs ${current_logdir}/ozinfo.out ${datapath2}/fort.206
ln -fs ${current_logdir}/convinfo.out ${datapath2}/fort.205
setenv ABIAS ${datapathp1}/${PREINP1}abias
while ($alldone == 'no' && $niter <= $nitermax)
    if ($niter == 1) then
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
    /bin/rm -f ${datapath}${analdatep1}/sfg_${analdatep1}_${charfhr}*_* 
    /bin/rm -f ${datapath}${analdatep1}/bfg_${analdatep1}_${charfhr}*_* 
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

if ($fg_only == 'false') then

# compute mean and spread grib files.
if ($POSTPROC == "YES") then
  time ${enkfexec}/gribmean.x $datapath2 $nanals $analdate p anl
  time ${enkfexec}/gribmean.x $datapathp1 $nanals $analdatep1 p fg 06
endif

endif

if ($longer_ens == 'true') then
# set datapathin, datapathout, fgprefix, FHMAX, FHOUT
#set POSTGPLIST_save=$POSTGPLIST
#setenv POSTGPLIST ${FIXGLOBAL}/global_kplist.tracker.txt
setenv datapathin ${datapath2}/ens${nanals2}/
setenv datapathout $datapathin
setenv fgprefix sf
setenv fgprefix2 bf
@ nanals2p1 = $nanals2 + 1
setenv ENS_NUM $nanals2p1
setenv LIOPE .false.
set FHMAX_save=$FHMAX
set FHDFI_save=$FHDFI
set FHOUT_save=$FHOUT
set FHMIN_save=$FHMIN
set JCAP_save=$JCAP
set LEVS_save=$LEVS
set LONB_save=$LONB
set LATB_save=$LATB
setenv JCAP $JCAP_ens
setenv LEVS $LEVS_ens
setenv LONB $LONB_ens
setenv LATB $LATB_ens
setenv FHMAX 240
setenv FHOUT 12
setenv FHMIN $FHOUT
setenv FHDFI 3
setenv FHZER $FHOUT
setenv SIGI ${datapathin}/sanl_${analdate}
ln -fs ${SIGI}_ensmean ${SIGI}
setenv SFCI ${datapathin}/sfcanl_${analdate}
ln -fs ${SFCI}_ensmean ${SFCI}
set mem_names="_01"
set nmem=1
while ($nmem <= $ENS_NUM)
 set mn=`printf %02i $nmem`
 if ($mn == $ENS_NUM) then # ensemble mean
     ln -fs ${SIGI}_ensmean ${SIGI}_${mn}
     ln -fs ${SFCI}_ensmean ${SFCI}_${mn}
 else
     ln -fs ${SIGI}_mem0${mn} ${SIGI}_${mn}
     ln -fs ${SFCI}_mem0${mn} ${SFCI}_${mn}
 endif
 @ nmem = $nmem + 1
 set mem_names="${mem_names} _${mn}"
end
setenv MEMBER_NAMES "$mem_names"
setenv DATOUT ${datapathout}
setenv DATA ${datapath2}/gefstmp$$
setenv SIGO $DATOUT/${fgprefix}_${analdate}_fhr'${FH}${MN}'
setenv SFCO $DATOUT/${fgprefix2}_${analdate}_fhr'${FH}${MN}'
setenv FLXO $DATOUT/sflxgrb_${analdate}_fhr'${FH}${MN}'
setenv LOGO /dev/null
setenv D3DO /dev/null
setenv G3DO /dev/null
setenv NSSTO /dev/null
setenv nprocs `expr $fg_proc \* $nanals2p1`
setenv HOSTFILE $TMPDIR/machines
echo "${analdate} run longer ensemble `date`"
sh ${enkfscripts}/run_gefs >&! ${current_logdir}/run_ens.out
echo "${analdate} done computing longer ensemble `date`"
/bin/rm -rf $DATA
#set nanal=1
#while ($nanal <= $nanals2p1)
#  set charnanal="mem`printf %03i $nanal`"
#  set mem=`printf %02i $nanal`
#  set fh=$FHMIN
#  while ($fh <= $FHMAX)
#    if ($fh < 10) then
#       set fhr=`printf %02i $fh`
#    else
#       set fhr=$fh
#    endif
#    /bin/mv -f $DATOUT/${fgprefix}_${analdate}_fhr${fhr}_${mem} $DATOUT/${fgprefix}_${analdate}_fhr${fhr}_${charnanal}
#    /bin/mv -f $DATOUT/sflxgrb_${analdate}_fhr${fhr}_${mem} $DATOUT/sflxgrb_${analdate}_fhr${fhr}_${charnanal}
#    /bin/rm -f $DATOUT/${fgprefix2}_${analdate}_fhr${fhr}_${mem} 
#    @ fh = $fh + $FHOUT
#  end
#  @ nanal = $nanal + 1
#end

# compute mean, spread grib files
set fh=$FHMIN
set nbackground = 1
while ($fh <= $FHMAX)
  if ($fh < 10) then
     set fhr="0${fh}"
  else
     set fhr=$fh
  endif
  time ${enkfexec}/gribmean.x $datapathout $nanals2p1 $analdate p fg $fhr  &
  if ($nbackground == $nbackground_max) then
     echo "waiting at forecast hour $fhr"
     wait
     set nbackground = 1
  else
      @ nbackground = $nbackground + 1
  endif
  @ fh = $fh + $FHOUT
end
wait
csh ${enkfscripts}/makeenspost.csh
/bin/rm -f $datapathout/sf_*
/bin/rm -f $datapathout/bf_*
/bin/rm -f $datapathout/sflxgrb_*
/bin/rm -f $datapathout/pgrbfg_*mem*
#setenv POSTGPLIST $POSTGPLIST_save
setenv FHMAX $FHMAX_save
setenv FHOUT $FHOUT_save
setenv FHMIN $FHMIN_save
setenv FHDFI $FHDFI_save
setenv JCAP $JCAP_save
setenv LEVS $LEVS_save
setenv LONB $LONB_save
setenv LATB $LATB_save
unsetenv ENS_NUM
unsetenv MEMBER_NAMES
unsetenv LIOPE
unsetenv FHZER
endif

# create high res control analysis file.
#if ($hr == '00' || $hr == '12') then
echo "create high-res control analysis"
setenv HOSTFILE ${datapath2}/hostfileall
time $CHGRESSH ${datapath2}/sanl_${analdate}_ensmean ${datapath2}/sfcanl_${analdate}_ensmean ${datapath2}/sanl_${analdate}_control ${datapath2}/sfcanl_${analdate}_control $JCAP_HIGH $LEVS $LONB_HIGH $LATB_HIGH > /dev/null
#endif

# run high res forecast.
# 5 day forecasts at 00 and 12z.
if ($control_fcst == 'true' && ($hr == '00' || $hr == '12')) then
set FHMAX_save=$FHMAX
set FHDFI_save=$FHDFI
set RUN_save=$RUN
set POSTPROC_save=$POSTPROC
set JCAP_save=$JCAP
set FHOUT_save=$FHOUT
set LONB_save=$LONB
set LATB_save=$LATB
set IO_save=$IO
set JO_save=$JO
setenv FHMAX 123
setenv FHOUT 3
setenv FHDFI 2
setenv RUN enkf
setenv POSTPROC NO
setenv DATOUT ${datapath2}
setenv JCAP $JCAP_HIGH
setenv LONB $LONB_HIGH
setenv LATB $LATB_HIGH
setenv POSTGPVARS "IDRT=0,IDRTC=4,IOC=$LONB,JOC=$LATB,MOO=255,MOOSLP=0"
setenv IO $IO_HIGH
setenv JO $JO_HIGH
setenv DATA ${datapath2}/chgrestmp.$$
setenv HOSTFILE ${datapath2}/hostfileall
setenv nprocs $ensmeanfcst_nprocs
setenv SIGO ${DATOUT}/${RUN}.t${hr}z.sf'${FH}'
setenv SFCO ${DATOUT}/${RUN}.t${hr}z.bf'${FH}'
setenv FLXO ${DATOUT}/${RUN}.t${hr}z.sfluxgrbf'${FH}'
setenv SIGI ${datapath}${analdate}/sanl_${analdate}_control
setenv SFCI ${datapath}${analdate}/sfcanl_${analdate}_control
setenv RERUN YES
setenv nprocs $ensmeanfcst_nprocs
echo "${analdate} run ${FHMAX} hour single forecast from eda solution `date`"
sh ${enkfscripts}/${drive_ensmean} >&! ${current_logdir}/gfs_fcst.out  
unsetenv SIGO
unsetenv SFCO
unsetenv FLXO
unsetenv nprocs
unsetenv $HOSTFILE
echo "${analdate} done running ${FHMAX} hour single forecast `date`"

# do postprocessing
set nhost=1
set hostsave = `echo $hosts[$nhost]`
set host=$hostsave
setenv HOSTFILE ${datapath2}/hostfile00
# run each instance on a different node (skipping the first one).
# when there are nbackground_max running the background, wait.
while ($host == $hostsave)
  set host = `echo $hosts[$nhost]`
  @ nhost = $nhost + 1
end
echo "${host}" >! ${HOSTFILE}
set hostsave=$host
set nbackground = 0
setenv DATA ${datapath2}/postgptmp_00.$$
sh ${enkfscripts}/global_postgpp.sh ${datapath2}/sanl_${analdate}_control /dev/null /dev/null ${datapath2}/${RUN}.t${hr}z.pgrbanl /dev/null $IO $JO >&! ${current_logdir}/gfs_fcst_pgrbanl.out &
@ nbackground = $nbackground + 1
set fh=$FHOUT
while ($fh <= $FHMAX)
  set fhr=$fh
  if ($fh < 10) then
     set fhr=`printf %02i $fh`
  endif
  setenv HOSTFILE ${datapath2}/hostfile${fhr}
  # run each instance on a different node (skipping the first one).
  while ($host == $hostsave)
    set host = `echo $hosts[$nhost]`
    @ nhost = $nhost + 1
  end
  echo "${host}" >! ${HOSTFILE}
  set hostsave=$host
  setenv DATA ${datapath2}/postgptmp_${fhr}.$$
  sh ${enkfscripts}/global_postgpp.sh ${datapath2}/${RUN}.t${hr}z.sf${fhr} ${datapath2}/${RUN}.t${hr}z.sfluxgrbf${fhr} /dev/null ${datapath2}/${RUN}.t${hr}z.pgrbf${fhr} /dev/null $IO $JO >&! ${current_logdir}/gfs_fcst_pgrbf${fhr}.out &
  echo "nbackground = $nbackground fh = $fhr"
  @ nbackground = $nbackground + 1
  echo $nbackground
  if ($nbackground == $nbackground_max) then
     echo "waiting at forecast hour $fhr"
     wait
     set nbackground = 1
     set nhost=1
  endif
  @ fh = $fh + $FHOUT
  echo $fh
end
wait
/bin/rm -f ${datapath2}/postgptmp_*

# reset FHMAX, RUN to original value.
setenv FHMAX $FHMAX_save
setenv FHDFI $FHDFI_save
setenv RUN $RUN_save
setenv POSTPROC $POSTPROC_save
setenv JCAP $JCAP_save
setenv FHOUT $FHOUT_save
setenv LONB $LONB_save
setenv LATB $LATB_save
setenv IO $IO_save
setenv JO $JO_save
setenv POSTGPVARS "IDRT=0,IDRTC=4,IOC=$LONB,JOC=$LATB,MOO=255,MOOSLP=0"
endif

if ($fg_only == "false") then

# cleanup
if ($do_cleanup == 'true') then
echo "make tar files, then clean up files `date`"
cd $datapath2
tar -cvf sanl_ens_${analdate}.tar sanl*${analdate}*mem* 
tar -cvzf sfcanl_ens_${analdate}.tar.gz sfcanl*${analdate}*mem* 
tar -cvzf pgrbfg_ens_${analdate}.tar.gz pgrbfg*${analdate}*fhr06*mem*
tar -cvzf pgrbanl_ens_${analdate}.tar.gz pgrbanl*${analdate}*mem*
tar -cvzf diag_conv_ens_${analdate}.tar.gz diag_conv*${analdate}*mem*
tar -cvzf diag_amsua_n15_${analdate}.tar.gz diag_amsua_n15*${analdate}*mem*
if ($longer_ens == "true" && $nanals2 > 0) then
cd ens${nanals2}
tar -cvf sanl_ens_${analdate}.tar sanl_${analdate}*
tar -cvzf sfcanl_ens_${analdate}.tar.gz sfcanl_${analdate}*
cd ..
endif
#tar -cvf enkffcst_${analdate}.tar enkf.t*
set sattypes = `ls $datapath2/diag*mem001 | cut -d "_" -f3,4 | uniq`
foreach sattype ($sattypes)
 /bin/rm -f diag*${sattype}*${analdate}*mem*
 # remove zero size files
 if ( ! -s diag_${sattype}_ges.${analdate}_ensmean ) then
    /bin/rm -f diag_${sattype}_ges.${analdate}_ensmean
 endif
end
/bin/rm -f diag_conv*mem*
#/bin/rm -f enkf.t*
/bin/rm -f *${yr}${mon}${day}_mem*.t${hr}z*
/bin/rm -f *.sigr1 *.sigr2 *.sfcr *.log* 
/bin/rm -rf hostfile
/bin/rm -rf gsitmp*
/bin/rm -rf gfstmp.*
/bin/rm -rf chgrestmp.*
/bin/rm -f *.nml
/bin/rm -f sfcanl*mem*
/bin/rm -f pgrbanl*mem*
/bin/rm -f bfg2*ensmean*
/bin/rm -f fort*
/bin/rm -f dates.dat
if ($longer_ens == "true" && $nanals2 > 0) then
/bin/rm -f ens${nanals2}/sanl*mem*
/bin/rm -f ens${nanals2}/sfcanl*mem*
/bin/rm -f ens${nanals2}/sanl${nanals}*
/bin/rm -f ens${nanals2}/sfcanl${nanals}*
/bin/rm -f ens${nanals2}/sanl${nanals2}*
endif
set fh=0
while ($fh <= $FHMAX)
 set fhr=`printf %02i $fh`
 /bin/rm -f bfg2*fhr$fhr*mem*
 /bin/rm -f bfg*fhr$fhr*mem*
 if ($fh == 6) then
    tar -cvf sfg_fhr06_ens_${analdate}.tar sfg*fhr$fhr*mem*
 endif
 /bin/rm -f sfg*fhr$fhr*mem*
 /bin/rm -f gfg*fhr$fhr*mem*
 /bin/rm -f sflxgrb*fhr$fhr*mem*
 /bin/rm -f pgrb*fhr$fhr*mem*
 @ fh = $fh + $FHOUT
end
/bin/rm -f sanl*mem*
/bin/rm -f hostfile*
/bin/rm -f hxprime*
/bin/rm -rf postgptmp*
/bin/rm -f sanlp*
/bin/rm -f sat*
#/bin/rm -f covinflate.dat
cd $current_logdir
tar -cvzf run_fg_logs.tgz run_fg_mem*
tar -cvzf run_obs_logs.tgz run_obs_mem*
tar -cvzf run_cycle_logs.tgz run_cycle_mem*
tar -cvzf chgres_logs.tgz chgres_mem*
/bin/rm -f *mem*
cd $enkfscripts
endif

wait # wait for backgrounded processes to finish
echo "${analdate} done cleanup `date`"

endif # skip to here if fg_only = true

if ($fg_only == 'true') set fg_only='false'

# next analdate: increment by $ANALINC
set analdate_save=$analdate
setenv analdate `${incdate} $analdate $ANALINC`

echo "setenv analdate ${analdate}" >! $startupenv
echo "setenv fg_only false" >! $datapath/fg_only.csh

# resubmit this script.
qsub gfsenkf_t190x.csh

# archive the data.
#ssh wfe7 "cd $datapath; tar -cvf /mss/jet/projects/fim/whitaker/gfsenkf_t190x/${analdate_save}.tar $analdate_save" 

exit 0
