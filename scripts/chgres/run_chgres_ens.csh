#!/bin/csh
#PBS -q premium
#PBS -A m958
#PBS -l mppwidth=24  
#PBS -l walltime=02:00:00
#PBS -N chgres  
#PBS -e chgres.err
#PBS -o chgres.out
#PBS -S /bin/csh

limit stacksize unlimited
setenv VERBOSE YES
setenv OMP_NUM_THREADS 4
setenv OMP_STACKSIZE 256M

setenv basedir /global/project/projectdirs/incite11/whitaker/20CRV3/
setenv enkfscripts /global/project/projectdirs/incite11/whitaker/20CRV3/scripts/chgres
setenv HOMEGLOBAL ${basedir}
setenv EXECGLOBAL ${HOMEGLOBAL}/bin
setenv FIXGLOBAL $HOMEGLOBAL/gfs/fix_am
setenv CHGRESSH ${enkfscripts}/global_chgresp.sh
setenv CHGRESEXEC ${basedir}/gfs/global_chgres.fd/global_chgres

setenv JCAP 254  
setenv LONB 512  
setenv LATB 256 
setenv LEVS 64
setenv SIGLEVEL $FIXGLOBAL/global_hyblev.l${LEVS}.txt
setenv OROGRAPHY ${FIXGLOBAL}/global_orography.t${JCAP}.${LONB}.${LATB}.grb
setenv OROGRAPHY_UF ${FIXGLOBAL}/global_orography_uf.t${JCAP}.${LONB}.${LATB}.grb
setenv SLMASK ${FIXGLOBAL}/global_slmask.t${JCAP}.${LONB}.${LATB}.grb
setenv IDVC 2
setenv IDVM 1
setenv NVCOORD 2
setenv IDVT 0

setenv analdate 1945010100
set datapath=/scratch2/scratchdirs/whitaker/${analdate}
set datapatho=/scratch2/scratchdirs/whitaker/gfsenkf_20crV3_cmip5oz/${analdate}
/bin/rm -f ${datapatho}/*mem*
mkdir -p $datapatho

set nanal=1
set nanals=80
while ($nanal <= $nanals)

set charnanal=`printf %02i $nanal`
set charnanal3=mem`printf %03i $nanal`

setenv SIGI $datapath/sanl_${analdate}_${charnanal3}
setenv SFCI $datapath/sfcanl_${analdate}_${charnanal3}
setenv SIGO $datapatho/sanl_${analdate}_${charnanal3}
setenv SFCO $datapatho/sfcanl_${analdate}_${charnanal3}
if ( ! -s $SFCO || ! -s $SIGO) then
   setenv DATA ${datapatho}/chgrestmp_${charnanal}
   mkdir -p $DATA
   echo "sh $CHGRESSH $SIGI $SFCI $SIGO $SFCO $JCAP $LEVS $LONB $LATB"
   setenv VERBOSE YES
   time sh $CHGRESSH $SIGI $SFCI $SIGO $SFCO $JCAP $LEVS $LONB $LATB   
   ls -l $SIGO
endif
@ nanal = $nanal + 1

end

wait
/bin/rm -rf ${datapatho}/chgrestmp*
