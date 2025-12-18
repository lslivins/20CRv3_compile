#!/bin/csh

#$ -j y
#$ -cwd
#$ -l h_rt=04:00:00
#$ -A fim-njet
#$ -pe thfip 756  
#$ -N run_longer_ens
#$ -o run_longer_ens.out

if ( ! $?hostfilein ) then
 setenv hostfilein $TMPDIR/machines
 setenv nproc_gfs_ens $NSLOTS
 setenv fg_threads 2
endif
if ( ! $?runtracker ) setenv runtracker "true"

setenv OMP_NUM_THREADS $fg_threads
setenv nprocs `expr $nproc_gfs_ens \/ $OMP_NUM_THREADS`
setenv HOSTFILE $TMPDIR/hostfile_ens
if ($OMP_NUM_THREADS > 1) then
awk "NR%${OMP_NUM_THREADS} == 1" $hostfilein >&! $HOSTFILE
else
setenv HOSTFILE $hostfilein
endif

# recenter $nanals2 member ensemble around $nanals ensemble mean.
echo "recentering $nanals2 member ensemble around $nanals member mean"
mkdir $datapath2/ens${nanals2}
time mpirun -hostfile $hostfilein -np $nanals2 ${enkfexec}/getsigensmeanp.x ${datapath2}/ ens${nanals2}/sanl${nanals2}_${analdate}_ensmean sanl${RUN}_${analdate} $nanals2
set filename_meanin=${datapath2}/ens${nanals2}/sanl${nanals2}_${analdate}_ensmean
set filename_meanout=${datapath2}/sanl${RUN}_${analdate}_ensmean
/bin/cp -f $filename_meanout ${datapath2}/ens${nanals2}/sanl${nanals}_${analdate}_ensmean
/bin/cp -f ${datapath2}/sfcanl_${analdate}_ensmean ${datapath2}/ens${nanals2}/sfcanl${nanals}_${analdate}_ensmean
set filenamein="${datapath2}/sanl${RUN}_${analdate}"
set filenameout="${datapath2}/ens${nanals2}/sanl_${analdate}"
time mpirun -hostfile $HOSTFILE -np $nanals2 ${enkfexec}/recentersigp.x $filenamein $filename_meanin $filename_meanout $filenameout $nanals2
# just copy 1st $nanals2 surface files (no recentering)
set nanal=1
while ($nanal <= $nanals2) 
   set charnanal="mem`printf %03i $nanal`"
   /bin/cp -f ${datapath2}/sfcanl_${analdate}_${charnanal} ${datapath2}/ens${nanals2}/sfcanl_${analdate}_${charnanal}
   @ nanal = $nanal + 1
end
/bin/mv -f ${datapath2}/ens${nanals2}/sfcanl${nanals}_${analdate}_ensmean ${datapath2}/ens${nanals2}/sfcanl_${analdate}_ensmean
/bin/mv -f ${datapath2}/ens${nanals2}/sanl${nanals}_${analdate}_ensmean ${datapath2}/ens${nanals2}/sanl_${analdate}_ensmean
/bin/rm -f $${datapath2}/ens${nanals2}/*anl${nanals2}*ensmean
/bin/rm -f $${datapath2}/ens${nanals2}/*anl${nanals}*ensmean

echo "${analdate} start running longer ensemble `date`"
setenv POSTPROC YES
setenv datapathin ${datapath2}/ens${nanals2}/
setenv datapathout $datapathin
setenv fgprefix sf
setenv ENS_NUM $nanals2
setenv LIOPE .true.
setenv JCAP $JCAP_ens
setenv LEVS $LEVS_ens
setenv LONB $LONB_ens
setenv LATB $LATB_ens
setenv DELTIM $DELTIM_ens
setenv VERBOSE YES
setenv FHMAX $FHMAX_LONG
setenv FHOUT $FHOUT_LONG
setenv FHMIN $FHOUT
setenv FHDFI $FHDFI_LONG
setenv FHZER $FHOUT
setenv SIGI ${datapathin}/sanl_${analdate}
ln -fs ${SIGI}_mem001 ${SIGI}
setenv SFCI ${datapathin}/sfcanl_${analdate}
ln -fs ${SFCI}_mem001 ${SFCI}
set mem_names="_01"
set nmem=1
while ($nmem <= $ENS_NUM)
 set mn=`printf %02i $nmem`
 ln -fs ${SIGI}_mem0${mn} ${SIGI}_${mn}
 ln -fs ${SFCI}_mem0${mn} ${SFCI}_${mn}
 @ nmem = $nmem + 1
 set mem_names="${mem_names} _${mn}"
end
setenv MEMBER_NAMES "$mem_names"
setenv DATOUT ${datapathout}
setenv DATA ${datapath2}/gefstmp$$
setenv SIGO $DATOUT/${fgprefix}_${analdate}_fhr'${FH}${MN}'
#setenv SFCO $DATOUT/bf_${analdate}_fhr'${FH}${MN}'
setenv SFCO /dev/null
setenv FLXO $DATOUT/sflxgrb_${analdate}_fhr'${FH}${MN}'
setenv LOGO /dev/null
setenv D3DO /dev/null
setenv G3DO /dev/null
setenv NSSTO /dev/null

sh ${enkfscripts}/run_gefs 
echo "${analdate} done running longer ensemble `date`"

/bin/rm -rf $DATA 
if ($runtracker == 'true') then
  cd ${enkfscripts}
  qsub -V runtracker_ens.csh
endif
