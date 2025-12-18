#!/bin/csh

#$ -j y
#$ -cwd
#$ -l h_rt=04:00:00
#$ -A fim-njet
#$ -pe thfip 960  
#$ -N run_control_fcst
#$ -o run_control_fcst.out

#set set_env='false' # if false, inherit from environment.
#if ($set_env == 'true') then # set vars here.
#endif # end set_env == true

if ( ! $?charnanal ) setenv charnanal "control"
if ( ! $?hostfilein ) then
 setenv hostfilein $TMPDIR/machines
 setenv nproc_gfs_control $NSLOTS
 setenv gfs_control_threads 2
endif


echo "${analdate} start running control fcst `date`"
setenv FHMAX $FHMAX_LONG
setenv FHOUT $FHOUT_LONG
setenv FHDFI $FHDFI_LONG
setenv FHMIN 0
setenv FHZER $FHOUT

setenv JCAP $JCAP_CNTL
setenv LONB $LONB_CNTL
setenv LATB $LATB_CNTL
setenv DELTIM $DELTIM_CNTL
setenv POSTPROC_SAVE $POSTPROC
setenv POSTPROC "NO"
setenv OMP_NUM_THREADS $gfs_control_threads
setenv nprocs `expr $nproc_gfs_control \/ $OMP_NUM_THREADS`
setenv HOSTFILE $TMPDIR/hostfile_cntlfcst
if ($OMP_NUM_THREADS > 1) then
awk "NR%${OMP_NUM_THREADS} == 1" $hostfilein >&! $HOSTFILE
else
setenv HOSTFILE $hostfilein
endif
setenv SIGI ${datapath2}/sanl_${analdate}_${charnanal}
setenv SFCI ${datapath2}/sfcanl_${analdate}_${charnanal}
setenv DATOUT ${datapath2}/control
setenv COMOUT $DATOUT
mkdir -p $DATOUT
setenv SIGO ${DATOUT}/sf_gfscntl_${analdate}_fhr'${FH}'
#setenv SFCO ${DATOUT}/bf_gfscntl_${analdate}_fhr'${FH}'
setenv SFCO /dev/null
setenv FLXO ${DATOUT}/sflxgrb_gfscntl_${analdate}_fhr'${FH}'
setenv DATA $DATOUT/gfstmp$$
cd ${enkfscripts}
time sh ${rungfs} 

/bin/rm -rf $DATA
/bin/rm -f $DATOUT/log*

echo "${analdate} done running high res control `date`"

setenv POSTPROC $POSTPROC_SAVE
#if ($POSTPROC == 'YES') then
setenv POSTGPSH ${enkfscripts}/global_postgpp.sh 
setenv POSTGPLIST ${FIXGLOBAL}/global_kplist.private.txt
setenv POSTGPEXEC $EXECGLOBAL/global_postgp
setenv POSTGPVARS "IDRT=0,IDRTC=4,IOC=$LONB_CNTL,JOC=$LATB_CNTL,MOO=255,MOOSLP=0"
setenv IO 720   
setenv JO 361   
set nodefile=$TMPDIR/nodefile
cat $hostfilein | uniq > $nodefile
set FH=0
set nhr=1
setenv nprocs $corespernode
while ($FH <= $FHMAX)
   set charfhr="fhr`printf %03i $FH`"
   if ($FH < 10) then
      set FH2="0${FH}"
   else
      set FH2=$FH
   endif
   setenv HOSTFILE ${DATOUT}/hostfile_fhr${FH2}
   set host=`head -$nhr $nodefile | tail -1`
   set core=1
   while ($core <= $nprocs)
     echo $host > $HOSTFILE
     @ core = $core + 1
   end
   echo "HOSTFILE:"
   cat $HOSTFILE
   setenv DATA ${DATOUT}/postgp_fhr${FH2}
   mkdir $DATA
   set PGBOUT=${DATOUT}/pgb_gfscntl_${analdate}_fhr${FH2}
   /bin/rm -rf $PGBOUT
   echo "running post for fhr=${FH2} ..."
   echo "$POSTGPSH ${DATOUT}/sf_gfscntl_${analdate}_fhr${FH2} ${DATOUT}/sflxgrb_gfscntl_${analdate}_fhr${FH2} /dev/null $PGBOUT /dev/null $IO $JO"
   time sh $POSTGPSH ${DATOUT}/sf_gfscntl_${analdate}_fhr${FH2} ${DATOUT}/sflxgrb_gfscntl_${analdate}_fhr${FH2} /dev/null $PGBOUT /dev/null $IO $JO &
   @ nhr = $nhr + 1
   @ FH = $FH + $FHOUT
end
wait
/bin/rm -f ${DATOUT}/hostfile_fhr*
/bin/rm -rf ${DATOUT}/postgp_*
echo "${analdate} done postprocessing running high res control `date`"
cd ${enkfscripts}
qsub -V runtracker_control.csh
#endif
