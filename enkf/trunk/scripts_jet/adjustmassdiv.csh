#!/bin/csh
setenv HOSTFILE $TMPDIR/hostfiletmp
cat $TMPDIR/machines | uniq > $HOSTFILE
cat $HOSTFILE
setenv OMP_STACKSIZE 256M
setenv OMP_NUM_THREADS $corespernode
limit
cd $datapath2
if ($FHDFI != 0) then
   time mpirun -hostfile $HOSTFILE -np $nanals  OMP_NUM_THREADS="$OMP_NUM_THREADS" OMP_STACKSIZE="$OMP_STACKSIZE" ${enkfexec}/externalmodebal.x $nanals $analdate $ANALINC $ANALINC 
else
   time mpirun -hostfile $HOSTFILE -np $nanals  OMP_NUM_THREADS="$OMP_NUM_THREADS" OMP_STACKSIZE="$OMP_STACKSIZE" ${enkfexec}/externalmodebal.x $nanals $analdate $FHMIN $FHMAX
endif
# compute new ensemble mean analysis file
# NOTE: sanl_${analdate}_* should be a symlink to sanl${RUN}_${analdate}_* (where RUN is gfs or gdas)
time mpirun -hostfile $HOSTFILE -np $nanals ${enkfexec}/getsigensmeanp.x ${datapath2}/ sanl_${analdate}_ensmean sanl_${analdate} ${nanals}
