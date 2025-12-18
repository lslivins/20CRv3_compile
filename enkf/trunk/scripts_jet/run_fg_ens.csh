#!/bin/csh

# run ensemble first guess.
# first, clean up old first guesses.
if ($cleanup_fg == 'true') then
set fhr=$FHMIN
while ( $fhr <= $FHMAX)
    set charfhr="fhr`printf %02i $fhr`"
    /bin/rm -f ${datapath}${analdatep1}/sfg_${analdatep1}_${charfhr}*_*mem* 
    /bin/rm -f ${datapath}${analdatep1}/bfg_${analdatep1}_${charfhr}*_*mem* 
    @ fhr = $fhr + $FHOUT
end
endif
mkdir -p ${datapath}${analdatep1}

set niter=1
set alldone='no'
echo "${analdate} compute first guesses `date`"
while ($alldone == 'no' && $niter <= $nitermax)
    if ($niter == 1) then
    csh ${enkfscripts}/${fg_gfs} >&! ${current_logdir}/run_fg.out
    set exitstat=$status
    else
    csh ${enkfscripts}/${fg_gfs} >>& ${current_logdir}/run_fg.out
    set exitstat=$status
    endif
    if ($exitstat == 0) then
       set alldone='yes'
    else
       echo "some files missing, try again .."
       @ niter = $niter + 1
    endif
end

if($alldone == 'no') then
    echo "Tried ${nitermax} times to run ens first-guesses and failed: ${analdate}"
    echo "no" >&! ${current_logdir}/run_fg_ens.log
else
    echo "yes" >&! ${current_logdir}/run_fg_ens.log
endif
