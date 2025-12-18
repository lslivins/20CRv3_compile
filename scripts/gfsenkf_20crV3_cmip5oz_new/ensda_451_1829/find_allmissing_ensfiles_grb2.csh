#!/bin/tcsh

# a script to loop through a stream month and count and find which 
# sflx and pgrb ens
#statistics grb2 files are missing and need to be fixed in grib1
# using the steps in, e.g., 
# /global/u2/c/compo/coriII_home/20CRv3_scripts/ensda_451_1889/fix_single_gribmeanp.csh

#usage ./find_allmissing_ensfiles_grb2.csh 1840 06 


# gets the experiment and stream year from the working directory

set yyyy=$argv[1]
set mm=$argv[2]

echo $yyyy
echo $mm

set datadir=/global/cscratch1/sd/$USER/
setenv expname `basename $PWD`
setenv spinyear `echo $expname | cut -c11-14`
setenv expname `basename $PWD`
set datadir=/global/cscratch1/sd/$USER/
setenv datapath "${datadir}/gfsenkf_20crV3_cmip5oz_CoriII/${expname}/"

echo $datadir
echo $datapath

cd $datapath

ls -d ${yyyy}${mm}* >&! dir_names1

foreach yyyymmddhh (`cat dir_names1`)
	echo $yyyymmddhh
	cd $datapath/$yyyymmddhh
	#ls *ens*
	#exit
	set count=`ls pgrbensmeananl*grb2 | wc -l`
	#echo $count
	if ($count < 2) then
		echo "problem with pgrbensmeananl (0 or 3) $yyyymmddhh"
		@ datep3 = $yyyymmddhh + 3
		set count=`ls pgrbensmeananl_$datep3 | wc -l`
		if ($count < 1) then
			echo "  problem with pgrbensmeananl $datep3"
		endif
	endif
	
	set count=`ls pgrbensmeanfg*grb2 | wc -l`
	if ($count < 1) then
	echo "problem with pgrbensmeanfg $yyyymmddhh"
		
	endif
	
	set count=`ls pgrbenssprdanl* | wc -l`
	#echo $count
	if ($count < 2) then
		echo "problem with pgrbenssprdanl(0 or 3)  $yyyymmddhh"
		@ datep3 = $yyyymmddhh + 3
		set count=`ls pgrbenssprdanl_$datep3 | wc -l`
		if ($count < 1) then
			echo "  problem with pgrbenssprdanl $datep3"
		endif
		
	endif
	
	set count=`ls pgrbenssprdfg* | wc -l`
	if ($count < 1) then
		echo "problem with pgrbenssprdfg $yyyymmddhh"
		
	endif
	
	set count=`ls sflxgrbensmeanfg*fhr03.grb2 | wc -l`
	#echo $count
	if ($count < 1) then
		echo "problem with sflxgrbensmeanfg fhr03 $yyyymmddhh"	
		
	endif
	set count=`ls sflxgrbensmeanfg*fhr06.grb2 | wc -l`
	#echo $count
	if ($count < 1) then
		echo "problem with sflxgrbensmeanfg fhr06 $yyyymmddhh"	
		
	endif
	
	
	set count=`ls sflxgrbenssprdfg*fhr03 | wc -l`
	#echo $count
	if ($count < 1) then
		echo "problem with sflxgrbenssprdfg fhr03 $yyyymmddhh"	
		
	endif
	set count=`ls sflxgrbenssprdfg*fhr06 | wc -l`
	#echo $count
	if ($count < 1) then
		echo "problem with sflxgrbenssprdfg fhr06 $yyyymmddhh"	
		
	endif

end

exit 0
