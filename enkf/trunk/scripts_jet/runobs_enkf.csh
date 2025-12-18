#!/bin/csh

date
setenv VERBOSE YES

setenv nprocs $hx_proc
setenv HXONLY 'YES'

set hosts = `cat $TMPDIR/machines`
echo $hosts
set nhosts = $#hosts
echo "nhosts = $nhosts"

set nhost1=1
set nhost=$nhost1

echo "nprocs = $nprocs"

set nanal=0 # set to 0 if you want to do ensemble mean, otherwise 1.

while ($nanal <= $nanals)

if ($nanal == 0) then
setenv charnanal "ensmean"
setenv SKIP_ANGUPDATE "NO"
else
setenv charnanal "mem"`printf %03i $nanal`
setenv SKIP_ANGUPDATE "YES"
endif
setenv string $charnanal
setenv SFCANL $datapath2/sfcanl_${analdate}_${charnanal}

# check to see if output files already created.
set obsfiles = "${datapath2}/diag_conv_ges.${analdate}_${charnanal} ${datapath2}/sfcanl_${analdate}_${charnanal}"
set filemissing='no'
echo $obsfiles
foreach obsfile ($obsfiles) 
  if ( ! -s $obsfile) set filemissing='yes'
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
      set host1 = `echo $hosts[$node]`
      echo "${host1}" >> ${HOSTFILE}
      @ node = $node + 1
   end
   cat $HOSTFILE
   # run gsi
   setenv tmpdir $datapath2/gsitmp_${charnanal}
   /bin/rm -rf $tmpdir
   if ($nanal == 0) then
      # do ensemble mean first, use thinning decisions for other members.
      setenv CLEAN "NO"
      setenv lread_obs_save .true.
      setenv lread_obs_skip .false.
      mkdir -p $tmpdir
      time sh ${enkfscripts}/${rungsi} >&! ${current_logdir}/run_obs_${charnanal}.out  
      /bin/rm -f $tmpdir/pe* $tmpdir/obsdiags*
      unsetenv lread_obs_save
      unsetenv lread_obs_skip
   else
      /bin/cp -r $datapath2/gsitmp_ensmean $tmpdir
      setenv CLEAN "YES"
      time sh ${enkfscripts}/${rungsi} >&! ${current_logdir}/run_obs_${charnanal}.out  &
      @ nhost = $nhost + $nprocs
   endif
else
   echo "skipping nanal = ${nanal}, output files already created"
endif

@ node_end_next = $node_end + $nprocs - 1
if ($node_end > $nhosts || $node_end_next > $nhosts) then
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
    if ($nanal == '0') then
       setenv charnanal  "ensmean"
    else
       setenv charnanal  "mem"`printf %03i $nanal`
    endif
    echo $nanal
    echo $charnanal
    set obsfiles = "${datapath2}/diag_conv_ges.${analdate}_${charnanal} ${datapath2}/sfcanl_${analdate}_${charnanal}"
    set filemissing='no'
    foreach obsfile ($obsfiles) 
      ls -l $obsfile
      if ( ! -s $obsfile) then 
        set filemissing='yes'
        set anyfilemissing='yes'
      endif
    end
    if ($filemissing == 'yes') then
      echo "file missing .."
    else
      echo "files ok .."
    endif
    @ nanal = $nanal + 1
end

if ($anyfilemissing == 'yes') then
    echo "there are output files missing!"
    exit 1
else
    echo "all output files seem OK"
    date
    exit 0
endif
