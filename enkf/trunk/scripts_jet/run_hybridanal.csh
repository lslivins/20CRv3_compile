#!/bin/csh
# do hybrid analysis.

setenv charnanal "control"
setenv SIGANL ${datapath2}/sanl${RUN}_${analdate}_${charnanal}
setenv SFCANL ${datapath2}/sfcanl_${analdate}_${charnanal}
setenv BIASO ${datapath2}/${PREINP}abias
setenv SATANGO ${datapath2}/${PREINP}satang

if ($cleanup_controlanl == 'true') then
   /bin/rm -f ${SIGANL}
endif

set niter=1
set alldone='no'
if ( -s $SIGANL && -s $SFCANL && -s $BIASO && -s $SATANGO) set alldone='yes'
while ($alldone == 'no' && $niter <= $nitermax)

setenv JCAP $JCAP_CNTL
setenv LONB $LONB_CNTL
setenv LATB $LATB_CNTL
setenv LONA $LONA_CNTL
setenv LATA $LATA_CNTL
setenv POSTPROC "NO"
setenv HXONLY 'NO'
setenv VERBOSE YES
setenv OMP_NUM_THREADS 2
set np=`wc -l $hostfilein | cut -f1 -d" "`
setenv nprocs `expr $np \/ $OMP_NUM_THREADS`
setenv HOSTFILE $TMPDIR/hostfile_hybridanal
if ($OMP_NUM_THREADS > 1) then
awk "NR%${OMP_NUM_THREADS} == 1" ${hostfilein} >&! $HOSTFILE
else
setenv HOSTFILE ${hostfilein}
endif

if ( -s ${datapathm1}/gdas1.t${hrm1}z.abias) then
setenv GBIAS ${datapathm1}/gdas1.t${hrm1}z.abias
else
setenv GBIAS ${datapathm1}/${PREINPm1}abias
endif
if ( -s ${datapathm1}/gdas1.t${hrm1}z.satang) then
setenv GSATANG ${datapathm1}/gdas1.t${hrm1}z.satang
else
setenv GSATANG ${datapathm1}/${PREINPm1}satang
endif

echo "${analdate} compute gsi hybrid high-res analysis increment `date`"
setenv lread_obs_save ".false."
setenv lread_obs_skip ".false."
setenv tmpdir $datapath2/hybridtmp$$
/bin/rm -rf $tmpdir
mkdir -p $tmpdir
/bin/cp -f $datapath2/hybens_locinfo $tmpdir
time sh ${enkfscripts}/${rungsi}
/bin/rm -rf $tmpdir
if ( ! -s $SIGANL ) then
  echo "$RUN gsi hybrid high-res analysis did not complete sucessfully"
  set exitstat=1
else
  echo "$RUN gsi hybrid high-res completed sucessfully"
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
    echo "Tried ${nitermax} times and to do $RUN gsi high-res hybrid analysis and failed"
    echo "no" >&! ${current_logdir}/run_gsi_hybrid.log
else
    ln -fs $SIGANL ${datapath2}/sanl_${analdate}_${charnanal}
    echo "yes" >&! ${current_logdir}/run_gsi_hybrid.log
endif
