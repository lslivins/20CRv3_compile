#!/bin/csh

set hosts = `cat ${hostfilein}`
set nhosts = $#hosts

echo "starting ens mean computation `date`"
#set fh=${FHMIN}
set fh=0
set nhost=1
while ($fh <= $FHMAX)
  set charfhr="fhr`printf %02i $fh`"
  if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && ! -s ${datapath}/${analdate}/bfg_${analdate}_${charfhr}_ensmean)) then
  setenv HOSTFILE ${datapath2}/hostfile${fh}
  set host = `echo $hosts[$nhost]`
  echo "${host}" >! ${HOSTFILE}
  echo "HOSTFILE for node $nhost"
  cat $HOSTFILE
  @ nhost = $nhost + $corespernode
  time mpirun -hostfile $HOSTFILE -np 1 ${enkfexec}/getsfcensmean.x ${datapath}/${analdate}/ bfg_${analdate}_${charfhr}_ensmean bfg_${analdate}_${charfhr} ${nanals} &
  endif
  @ fh = $fh + $FHOUT
end
#wait
#set nhost=1
#set fh=${FHMIN}
set fh=0
#set fh=$FHOUT # use this if getpstend is run
while ($fh <= $FHMAX)
  set charfhr="fhr`printf %02i $fh`"
  if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && ! -s ${datapath}/${analdate}/sfg_${analdate}_${charfhr}_ensmean)) then
  setenv HOSTFILE ${datapath2}/hostfile${fh}
  set host = `echo $hosts[$nhost]`
  echo "${host}" >! ${HOSTFILE}
  echo "HOSTFILE for node $nhost"
  cat $HOSTFILE
  @ nhost = $nhost + $corespernode
  time mpirun -hostfile $HOSTFILE -np 1  ${enkfexec}/getsigensmean.x ${datapath}/${analdate}/ sfg_${analdate}_${charfhr}_ensmean sfg_${analdate}_${charfhr} ${nanals} ${charfhr}&
  endif
  @ fh = $fh + $FHOUT
end
wait
echo "done ens mean computation `date`"
# compute rms of ps tend (a measure of imbalance).
setenv HOSTFILE $hostfilein
echo "imbalance diagnostic for member 1"
time mpirun -hostfile $HOSTFILE -np 1 ${enkfexec}/getpstend.x  ${datapath}/ ${analdate} ${analdatem1} mem001
echo "imbalance diagnostic for ensemble mean"
time mpirun -hostfile $HOSTFILE -np 1 ${enkfexec}/getpstend.x  ${datapath}/ ${analdate} ${analdatem1} ensmean
