#!/bin/tcsh

# a script to loop through a stream month and count and find which ens
#statistics files are missing


#usage ./find_missing_ensfiles.csh 1840 06 


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
	set count=`ls pgrbensmeananl_* | wc -l`
	#echo $count
	if ($count < 2) then
		echo "problem with pgrbensmeananl (0 or 3) $yyyymmddhh"
		@ datep3=$yyyymmddhh+3
		set count=`ls pgrbensmeananl_$datep3 | wc -l`
		if ($count < 1) then
			echo "  problem with pgrbensmeananl $datep3"
		endif
		
	endif
	
	set count=`ls pgrbensmeanfg_* | wc -l`
	if ($count < 1) then
	echo "problem with pgrbensmeanfg $yyyymmddhh"
		
	endif
	
	set count=`ls pgrbenssprdanl* | wc -l`
	#echo $count
	if ($count < 2) then
		echo "problem with pgrbenssprdanl $yyyymmddhh"
		
	endif
	
	set count=`ls pgrbenssprdfg* | wc -l`
	if ($count < 1) then
		echo "problem with pgrbenssprdfg $yyyymmddhh"
		
	endif
	
	

end

exit 0
