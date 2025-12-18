#!/bin/csh
#$ -j y
#$ -cwd
#$ -l h_rt=04:00:00
#$ -A fim-njet
#$ -pe thfip 12
#$ -N runtracker_ens
#$ -o runtracker_ens.out

if (${?analdate} == 0) then
   set analdate=2010092000
   set datapath2=/lfs1/projects/fim/whitaker/gfsenkf_hybrid/${analdate}
   set nanals2=20
   set enkfscripts=/lfs1/projects/fim/whitaker/gfsenkf/scripts/realtime2011
endif

setenv TCVITDIR /lfs1/projects/rtfim/tcvitals
#set trackerbin=/lfs1/projects/fim/whitaker/gfsenkf/bin/gettrk.gfs.x
set trackerbin=/lfs1/projects/rtfim/tracker/bin/gettrk.gfs.x
set grbindexbin=/lfs1/projects/rtfim/tracker/bin/grbindex
set tcvitals=${TCVITDIR}/tcvitals.${analdate}.txt
if (! -s $tcvitals) then
   echo "error: no tcvitals file found for ${analdate}!"
   exit 1
endif

set nanal=1
@ nanalsmax = $nanals2 + 1
while ($nanal <= $nanalsmax)
   set charnanal="mem`printf %03i $nanal`"
   set charnanal2="`printf %02i $nanal`"
   set tmpdir=${datapath2}/ens${nanals2}/tracker.${charnanal}
   /bin/rm -rf $tmpdir
   mkdir $tmpdir
   pushd $tmpdir
   /bin/cp -f ${enkfscripts}/namelist.template $tmpdir
   python ${enkfscripts}/makenamelist.py $analdate GE${charnanal2} >! namelist.${charnanal}
   cat ${datapath2}/ens${nanals2}/pgrbfg_${analdate}*${charnanal}* > ${charnanal}.tracker.${analdate}.grib1
   ${grbindexbin} ${charnanal}.tracker.${analdate}.grib1 ${charnanal}.tracker.${analdate}.grib1.ix
   /bin/cp -f $tcvitals tcvitals.txt
   ln -fs ${charnanal}.tracker.${analdate}.grib1 fort.11
   ln -fs ${charnanal}.tracker.${analdate}.grib1.ix fort.31
   ln -fs tcvitals.txt fort.12
   ln -fs ${datapath2}/ens${nanals2}/track.${analdate}.GE${charnanal2} fort.64
   ${trackerbin} < namelist.${charnanal} 
   popd
   /bin/rm -rf $tmpdir
   @ nanal = $nanal + 1
end

wait
exit 0
