#!/bin/csh

echo "FHMAX = $FHMAX"
date
# run model
setenv DATOUT ${datapath}${analdatep1}

setenv OMP_NUM_THREADS $fg_threads
setenv nprocs `expr $fg_proc \/ $OMP_NUM_THREADS`
setenv HOSTFILE $TMPDIR/hostfile_ens
if ($OMP_NUM_THREADS > 1) then
awk "NR%${OMP_NUM_THREADS} == 1" ${hostfilein} >&! $HOSTFILE
else
setenv HOSTFILE ${hostfilein}
endif

set hosts = `cat $HOSTFILE`
echo $hosts
set nhosts = $#hosts
echo "nhosts = $nhosts"

set nhost1=1
set nhost=$nhost1

set nanal=1

while ($nanal <= $nanals)
 setenv charnanal "mem`printf %03i $nanal`"
 setenv SFCI ${datapath}/${analdate}/sfcanl_${analdate}_${charnanal}
 setenv SIGI ${datapath}/${analdate}/sanl${RUN}_${analdate}_${charnanal}

# check to see if output files already created.
 set fhr=$FHMIN
 set outfiles=""
 while ($fhr <= 9)
    set charhr="fhr`printf %02i $fhr`"
    set outfiles = "${outfiles} ${datapath}${analdatep1}/sfg_${analdatep1}_${charhr}_${charnanal} ${datapath}${analdatep1}/bfg_${analdatep1}_${charhr}_${charnanal}"
    @ fhr = $fhr + $FHOUT
 end
 set filemissing='no'
 foreach outfile ($outfiles) 
   if ( ! -s $outfile) then
     echo "${outfile} is missing"
     set filemissing='yes'
   else
     echo "${outfile} is OK"
   endif
 end

 set node=$nhost
 set node_end=$node
 @ node_end = $node_end + $nprocs - 1
 if ($filemissing == 'yes') then
   #if ($node_end > $nhosts) set node_end=$nhosts
   echo "nanal = ${nanal}, nhost = ${nhost}, node = ${node}, node_end = ${node_end}"
   setenv HOSTFILE ${datapath2}/hostfile${node}
   /bin/rm -f $HOSTFILE
   while ($node <= $node_end) 
      #set host1=`echo $hosts[$node] | awk -F- '{print $2}'`
      set host1 = `echo $hosts[$node]`
      #echo "$node $host1"
      #echo ${host1}-ib0 >> ${HOSTFILE}
      echo ${host1} >> ${HOSTFILE}
      @ node = $node + 1
   end
   echo "HOSTFILE = $HOSTFILE"
   cat $HOSTFILE
   #if ($nanal == $nanals && $nhost == $nhost1) then
   #  setenv HOSTFILE ${datapath2}/hostfileall
   #  setenv nprocs $NSLOTS
   #endif 
   #if ($nanal == 1 || $nanal == 2) then
    time sh ${enkfscripts}/drive_gfs >&! ${current_logdir}/run_fg_${charnanal}.out &
   #else
   # sh ${enkfscripts}/drive_gfs >&! /dev/null &
   #endif
   @ nhost = $nhost + $nprocs
 else
   echo "skipping nanal = ${nanal}, output files already created"
 endif

 @ node_end_next = $node_end + $nprocs - 1
 if ($node_end > $nhosts || $node_end_next > $nhosts) then
  echo "$node_end $node_end_next $nhosts"
  echo "waiting at nanal = ${nanal} `date`"
  wait
  set nhost=$nhost1
 endif

@ nanal = $nanal + 1
end
echo "waiting at nanal = ${nanal} `date`"
wait
echo "all done `date`"

# check to see all files created
echo "checking output files .."`date`

set nanal=1
set anyfilemissing='no'
while ($nanal <= $nanals)
    setenv charnanal  "mem`printf %03i $nanal`"
    set fhr=$FHMIN
    set outfiles=""
    while ($fhr <= 9)
       set charhr="fhr`printf %02i $fhr`"
       set outfiles = "${outfiles} ${datapath}${analdatep1}/sfg_${analdatep1}_${charhr}_${charnanal} ${datapath}${analdatep1}/bfg_${analdatep1}_${charhr}_${charnanal}"
       @ fhr = $fhr + $FHOUT
    end
    set filemissing='no'
    foreach outfile ($outfiles) 
      ls -l $outfile
      if ( ! -s $outfile) then 
        echo "${outfile} is missing"
        set filemissing='yes'
        set anyfilemissing='yes'
      else
        echo "${outfile} is OK"
      endif
    end
    @ nanal = $nanal + 1
end

if ($anyfilemissing == 'yes') then
    echo "there are output files missing!"
    exit 1
else
    echo "all output files seem OK"
    #if ($FHDFI > 0) then
    ## sfg_* is before digital filter finalization, sfg2 is after.
    ## rename files so sfg_ is filtered, sfgpredff_ is unfiltered.
    #set nanal=1
    #while ($nanal <= $nanals)
    #    setenv charnanal  "mem`printf %03i $nanal`"
    #    set fhr=$FHDFI
    #    set charhr="fhr`printf %02i $fhr`"
    #    #/bin/mv -f   ${datapath}${analdatep1}/sfg_${analdatep1}_${charhr}_${charnanal} ${datapath}${analdatep1}/sfgpredff_${analdatep1}_${charhr}_${charnanal}
    #    /bin/mv -f   ${datapath}${analdatep1}/sfg2_${analdatep1}_${charhr}_${charnanal} ${datapath}${analdatep1}/sfg_${analdatep1}_${charhr}_${charnanal}
    #    @ nanal = $nanal + 1
    #end
    #endif
    date
    exit 0
endif
