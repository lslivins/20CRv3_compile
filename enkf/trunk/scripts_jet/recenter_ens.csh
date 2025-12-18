#!/bin/csh

set charnanal="control"
# reduce resolution of gfs hybrid analysis and recenter enkf ensemble around it.
setenv HOSTFILE ${datapath2}/hostfileall
set SIGI=${datapath2}/sanl${RUN}_${analdate}_${charnanal}
set SFCI=${datapath2}/sfcanl${RUN}_${analdate}_${charnanal}
set SIGO=${datapath2}/sanl${RUN}_${analdate}_${charnanal}_lores
set SFCO=${datapath2}/sfcanl${RUN}_${analdate}_${charnanal}_lores
/bin/rm -f $SIGO
/bin/rm -f $SFCO
setenv DATA $datapath2/chgrestmp$$
mkdir -p $DATA
time $CHGRESSH $SIGI $SFCI $SIGO $SFCO $JCAP_ens $LEVS_ens $LONB_ens $LATB_ens
/bin/rm -rf $DATA
if ( -s $SIGO ) then
echo "recenter ensemble perturbations about low resolution hybrid analysis"
set filename_meanin=${datapath2}/sanl${RUN}_${analdate}_ensmean
set filename_meanout=${datapath2}/sanl${RUN}_${analdate}_${charnanal}_lores
set filenamein="${datapath2}/sanl${RUN}_${analdate}"
set filenameout="${datapath2}/sanl${RUN}_${analdate}"
time mpirun -hostfile ${HOSTFILE} -np $nanals ${enkfexec}/recentersigp.x $filenamein $filename_meanin $filename_meanout $filenameout $nanals
if ($status == 0) then
  /bin/cp -f $filename_meanin  ${filename_meanin}.orig
  /bin/mv -f $filename_meanout $filename_meanin
  if ($status == 0) then
     echo "yes" >! ${current_logdir}/recenter_ens_${RUN}.log
  else
     echo "no" >! ${current_logdir}/recenter_ens_${RUN}.log
  endif
else
  echo "no" >! ${current_logdir}/recenter_ens_${RUN}.log
endif
else
  echo "no" >! ${current_logdir}/recenter_ens_${RUN}.log
endif
