#!/bin/csh 

# remove obsfiles from the previous cycle
if ($cleanup_obs == 'true') then
   /bin/rm -f ${datapath2}/*abias_enkf
   /bin/rm -f ${datapath2}/*satang_enkf
   /bin/rm -f ${datapath2}/diag*ensmean
   /bin/rm -rf ${datapath2}/gsitmp*
   /bin/rm -f ${datapath2}/obs_input.tar
   set nanal=1
   while ($nanal <= $nanals)
     set charnanal="mem`printf %03i $nanal`"
     /bin/rm -f  ${datapath2}/diag*${charnanal}
     @ nanal = $nanal + 1
   end
endif

# need symlinks for satbias_angle, satbias_in, satinfo
if ( -s ${datapathm1}/gdas1.t${hrm1}z.abias) then
setenv GBIAS ${datapathm1}/gdas1.t${hrm1}z.abias
else
setenv GBIAS ${datapathm1}/${PREINPm1}abias
endif
if ( -s ${datapathm1}/gdas1.t${hrm1}z.satang) then
setenv GSATANG ${datapathm1}/gdas1.t${hrm1}z.satang
else
setenv GSATANG ${datapathm1}/${PREINPm1}satang
endif
ln -fs $GBIAS   ${datapath2}/satbias_in
ln -fs $GSATANG ${datapath2}/satbias_angle
ln -fs ${gsipath}/fix/global_satinfo.txt ${datapath2}/satinfo
ls -l ${gsipath}/fix/global_satinfo.txt
wc -l ${gsipath}/fix/global_satinfo.txt
ls -l ${datapath2}/satinfo
ln -fs ${gsipath}/fix/global_convinfo.txt ${datapath2}/convinfo
ln -fs ${gsipath}/fix/global_ozinfo.txt ${datapath2}/ozinfo
ln -fs ${gsipath}/fix/global_scaninfo.txt ${datapath2}/scaninfo
ln -fs ${current_logdir}/satinfo.out ${datapath2}/fort.207
ln -fs ${current_logdir}/ozinfo.out ${datapath2}/fort.206
ln -fs ${current_logdir}/convinfo.out ${datapath2}/fort.205
if ($do_hybrid == 'true') then
setenv ABIAS ${datapath2}/${PREINP}abias_enkf
else
setenv ABIAS ${datapath2}/${PREINP}abias
endif

set niter=1
set alldone='no'
echo "${analdate} compute $RUN enkf forward operator `date`"
if ($do_hybrid == 'true') then
setenv SATANGO ${datapath2}/${PREINP}satang_enkf
else
setenv SATANGO ${datapath2}/${PREINP}satang
endif
while ($alldone == 'no' && $niter <= $nitermax)
    if ($niter == 1) then
     csh ${enkfscripts}/${runobs} >&! ${current_logdir}/run_obs.out
     set exitstat=$status
    else
     csh ${enkfscripts}/${runobs} >>& ${current_logdir}/run_obs.out
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
    echo "Tried ${nitermax} times and to create obs files and failed: ${analdate}"
    echo "no" >&! ${current_logdir}/run_enkf.log
    exit 1
endif
echo "${analdate} done computing $RUN enkf forward operator `date`"

# remove previous analyses
if ($cleanup_anal == 'true') then
   /bin/rm -f ${datapath2}/sanl${RUN}_*mem*
   /bin/rm -f ${datapath2}/sanl_*mem*
endif

set niter=1
set alldone='no'
echo "${analdate} compute enkf analysis increment `date`"
while ($alldone == 'no' && $niter <= $nitermax)
    if ($niter == 1) then
     csh ${enkfscripts}/${ensda} >&! ${current_logdir}/ensda_${RUN}.out
     set exitstat=$status
    else
     csh ${enkfscripts}/${ensda} >>& ${current_logdir}/ensda_${RUN}.out
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
    echo "Tried ${nitermax} times to run $RUN ensda and failed: ${analdate}"
    echo "no" >&! ${current_logdir}/run_enkf.log
else
    echo "yes" >&! ${current_logdir}/run_enkf.log
endif
echo "${analdate} done computing $RUN enkf analysis increment `date`"
