#!/bin/csh
#$ -j y
#$ -cwd
#$ -l h_rt=00:30:00
#$ -A fim-njet
#$ -pe thfip 12
#$ -N postgp
#$ -o postgp.out
if ( ! $?charnanal ) setenv charnanal "control"
if ( ! $?RUN ) setenv RUN "gdas1"
setenv HOSTFILE $TMPDIR/machines
setenv OMP_NUM_THREADS 1
setenv nprocs $NSLOTS
setenv DATA $datapath/postgpcntl$$
setenv POSTGPEXEC $EXECGLOBAL/global_postgp
mkdir -p $DATA
if ($charnanal == 'ensmean') then
time sh ${enkfscripts}/global_postgpp.sh ${datapath2}/sanl${RUN}_${analdate}_${charnanal} ${datapath2}/sflxgrbensmeanfg_${analdate}_fhr00 /dev/null  ${datapath2}/pgrbensmeananl_${analdate} /dev/null $IO $JO 
else
time sh ${enkfscripts}/global_postgpp.sh ${datapath2}/sanl${RUN}_${analdate}_${charnanal} ${datapath2}/sflxgrbfg_${analdate}_fhr00_${charnanal} /dev/null  ${datapath2}/pgrbanl_${analdate}_${charnanal} /dev/null $IO $JO 
endif
/bin/rm -rf $DATA
mkdir -p $DATA
if ($charnanal == 'ensmean') then
time sh ${enkfscripts}/global_postgpp.sh ${datapath2}/sfg_${analdate}_fhr06_${charnanal} ${datapath2}/sflxgrbensmeanfg_${analdate}_fhr06 /dev/null  ${datapath2}/pgrbensmeanfg_${analdate} /dev/null $IO $JO 
else
time sh ${enkfscripts}/global_postgpp.sh ${datapath2}/sfg_${analdate}_fhr06_${charnanal} ${datapath2}/sflxgrbfg_${analdate}_fhr06_${charnanal} /dev/null  ${datapath2}/pgrbfg_${analdate}_fhr06_${charnanal} /dev/null $IO $JO 
endif
/bin/rm -rf $DATA
