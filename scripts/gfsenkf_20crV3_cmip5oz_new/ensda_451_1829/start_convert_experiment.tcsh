#!/bin/tcsh
 
#SBATCH -J 1829_cnvgrib2
#SBATCH -A m958
#SBATCH -e cnvgrib.err
#SBATCH -o cnvgrib.out
#SBATCH -q regular
##SBATCH -q scavenger
###SBATCH -q premium
#SBATCH -C knl,cache,quad
#SBATCH -N 1 
#SBATCH -t 04:49:14
###SBATCH --time-min=2:00:00

#SBATCH -L cscratch1, project


set datadir=/global/cscratch1/sd/$USER/
setenv wdir $PWD
# setup the enivorment variables 
# where the data will be converted
setenv expname `basename $PWD`
setenv experiment `echo $expname |cut -c7-9`
setenv spinyear `echo $expname |cut -c11-14`

setenv datapath "${datadir}/gfsenkf_20crV3_cmip5oz_CoriII/${expname}"

setenv grib2cnvenv "${datapath}/cnvgrib_date.csh"
source $grib2cnvenv

setenv cnv_year `echo $cnvgrib_date |cut -c1-4`
setenv cnv_month `echo $cnvgrib_date |cut -c5-6`


# need to check that the conversion should run
# compare the dates of next_archive_date and cnvgrib_date

# next archive time.
source $datapath/next_quick_archive_date.tcsh
setenv archive_year `echo $next_quick_archive_date |cut -c1-4`
setenv archive_month `echo $next_quick_archive_date |cut -c5-6`

if ($archive_year > $cnv_year) then 
  @ archive_month =  $archive_month + 12
endif

  @ delta_month = $archive_month - $cnv_month

#convert the month before last archive month
if ( $delta_month >= 1) then

#calculate when the next conversion date should be
  @ monthp1 = $cnv_month + 1
  if ( $monthp1 > 12 ) then   
#Its a new year! increase year and start at month 1
    @ nextyear = $cnv_year + 1
    @ nextmonth =  $monthp1 - 12
    setenv nmonth `printf '%02i' ${nextmonth}`
    setenv next_cnvdate  ${nextyear}${nmonth}
  else
   setenv nmonth `printf '%02i' ${monthp1}`
   setenv next_cnvdate ${cnv_year}${nmonth}
  endif

echo "Grib convert date is $cnv_year"
#there are 32 processors so split the work into 32 tasks at a time
# first 7 days every 4 hours background until done
  ls -d $datapath/${cnv_year}${cnv_month}0[1234567]* | cut -d/ -f9 > cnv_dirs_${cnv_year}${cnv_month}a

  foreach cdate (`cat ./cnv_dirs_${cnv_year}${cnv_month}a`)

    /global/u2/c/cmccoll/coriII_home/20CRv3_scripts/utilities/convert_grib2.tcsh ${experiment} ${spinyear} $cdate &

  end
  wait

# next 8 days every 4 hours background until done
  ls -d $datapath/${cnv_year}${cnv_month}0[89]* | cut -d/ -f9 > cnv_dirs_${cnv_year}${cnv_month}b
  ls -d $datapath/${cnv_year}${cnv_month}1[012345]* | cut -d/ -f9 >> cnv_dirs_${cnv_year}${cnv_month}b

  foreach cdate (`cat ./cnv_dirs_${cnv_year}${cnv_month}b`)

    /global/u2/c/cmccoll/coriII_home/20CRv3_scripts/utilities/convert_grib2.tcsh ${experiment} ${spinyear} $cdate &

  end
  wait

# next 8 days every 4 hours background until done
  ls -d $datapath/${cnv_year}${cnv_month}1[6789]* | cut -d/ -f9 > cnv_dirs_${cnv_year}${cnv_month}c
  ls -d $datapath/${cnv_year}${cnv_month}2[0123]* | cut -d/ -f9 >> cnv_dirs_${cnv_year}${cnv_month}c

  foreach cdate (`cat ./cnv_dirs_${cnv_year}${cnv_month}c`)

    /global/u2/c/cmccoll/coriII_home/20CRv3_scripts/utilities/convert_grib2.tcsh ${experiment} ${spinyear} $cdate &

  end
  wait

# next 8 days every 4 hours background until done
  ls -d $datapath/${cnv_year}${cnv_month}2[456789]* | cut -d/ -f9 > cnv_dirs_${cnv_year}${cnv_month}d
  ls -d $datapath/${cnv_year}${cnv_month}3[01]* | cut -d/ -f9 >> cnv_dirs_${cnv_year}${cnv_month}d

  foreach cdate (`cat ./cnv_dirs_${cnv_year}${cnv_month}d`)

    /global/u2/c/cmccoll/coriII_home/20CRv3_scripts/utilities/convert_grib2.tcsh ${experiment} ${spinyear} $cdate &

  end
  wait
#copy the files needed to the projects area
  cd $datapath
  rsync -avu --include='pgrbens*' --include='psob*' --include="*/" --exclude='*' ${cnv_year}${cnv_month}* /global/project/projectdirs/incite11/ensda_v${experiment}/ensda_${spinyear}
  cd $wdir
#cleanup temp files
 rm cnv_dirs_${cnv_year}${cnv_month}a
 rm cnv_dirs_${cnv_year}${cnv_month}b
 rm cnv_dirs_${cnv_year}${cnv_month}c
 rm cnv_dirs_${cnv_year}${cnv_month}d
echo "setenv cnvgrib_date ${next_cnvdate}" > ${datapath}/cnvgrib_date.csh
 if ($NERSC_HOST == 'edison') then
     ssh -x -f cori "cd $wdir;sbatch start_convert_experiment.tcsh"
   else
     sbatch start_convert_experiment.tcsh >&! convert_jobid
   endif
endif
exit 0

