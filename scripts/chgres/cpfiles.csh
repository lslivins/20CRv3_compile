set nanal=1
set analdate=1945010100
set analdate2=`incdate $analdate 24`
set datapath=/scratch2/scratchdirs/${USER}/ensda_v401/ensda_1945
set outdir=/scratch2/scratchdirs/${USER}/${analdate}
/bin/rm -rf $outdir
mkdir -p $outdir
set basedir=/project/projectdirs/incite11/${USER}/20CRV3
set enkfexec="${basedir}/bin"
while ($nanal <= 56)
 set charnanal2=`printf %02i $nanal`
 set charnanal3="mem"`printf %03i $nanal`
 /bin/cp -f ${datapath}/${analdate}/sanl_${analdate}_${charnanal2} ${outdir}/sanl_${analdate}_${charnanal3}
 /bin/cp -f ${datapath}/${analdate}/sfcanl_${analdate}_${charnanal2} ${outdir}/sfcanl_${analdate}_${charnanal3}
 @ nanal2 = $nanal + 56
 set charnanal3="mem"`printf %03i $nanal2`
 $enkfexec/chgdatesig.x  ${datapath}/${analdate2}/sanl_${analdate2}_${charnanal2} ${analdate} 0 ${outdir}/sanl_${analdate}_${charnanal3}
 $enkfexec/chgdatesfc.x  ${datapath}/${analdate2}/sfcanl_${analdate2}_${charnanal2} ${analdate} 0 ${outdir}/sfcanl_${analdate}_${charnanal3}
 @ nanal = $nanal + 1
end
#tar -cvf ${analdate}_20crV2c.tar ${outdir}
