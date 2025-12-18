#!/bin/csh
setenv OMP_NUM_THREADS $gfs_control_threads
set nproctot=`wc -l $hostfilein | cut -f1 -d" "`
setenv nprocs `expr $nproctot \/ $OMP_NUM_THREADS`
setenv HOSTFILE $TMPDIR/hostfile_gfs
if ($OMP_NUM_THREADS > 1) then
awk "NR%${OMP_NUM_THREADS} == 1" $hostfilein >&! $HOSTFILE
else
setenv HOSTFILE $hostfilein
endif
# run first-guess for hybrid.
setenv SIGI ${datapath2}/sanl_${analdate}_control
setenv SFCI ${datapath2}/sfcanl_${analdate}_control
setenv DATOUT ${datapathp1}
setenv COMOUT $DATOUT
mkdir -p $DATOUT
setenv SIGO ${DATOUT}/sfg_${analdatep1}_fhr'${FH}'_control
setenv SFCO ${DATOUT}/bfg_${analdatep1}_fhr'${FH}'_control
setenv FLXO ${DATOUT}/sflxgrbfg_${analdatep1}_fhr'${FH}'_control
setenv DATA $datapath2/gfstmp$$
setenv JCAP $JCAP_CNTL
setenv LONB $LONB_CNTL
setenv LATB $LATB_CNTL
setenv POSTPROC "NO"
setenv DELTIM $DELTIM_CNTL


if ($cleanup_controlfg == 'true') then
  /bin/rm -f  ${DATOUT}/sfg_${analdatep1}_fhr0${FHMAX}_control
endif

set niter=1
set alldone='no'
if ( -s ${DATOUT}/sfg_${analdatep1}_fhr0${FHMAX}_control ) set alldone='yes'
while ($alldone == 'no' && $niter <= $nitermax)

time sh ${rungfs} >&! ${current_logdir}/run_gfs_hybrid.out 
/bin/rm -rf $DATA

if ( ! -s ${DATOUT}/sfg_${analdatep1}_fhr0${FHMAX}_control ) then
  echo "gsi hybrid high-res first-guess forecast did not complete sucessfully"
  set exitstat=1
else
  echo "gsi hybrid high-res first-guess forecast completed sucessfully"
  set exitstat=0
endif

if ($exitstat == 0) then
   set alldone='yes'
else
   echo "some files missing, try again .."
   @ niter = $niter + 1
endif

end

if($alldone == 'no') then
    echo "Tried ${nitermax} times to run high-res first guess  and failed: ${analdate}"
    echo "no" >&! ${current_logdir}/run_gfs_control.log
else
    echo "yes" >&! ${current_logdir}/run_gfs_control.log
endif
