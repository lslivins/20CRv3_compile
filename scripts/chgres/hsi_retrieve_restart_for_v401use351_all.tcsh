#!/bin/tcsh


# script to retrieve the specified streams for a particular day
# and untar
# to the experimental v40x set

# experiment 401 starts from Sept of stream_year+5 of 351

# NOTE CHANGE ALL TO 354 after retrieving the first one 
# make the input and output streams the same 

setenv idir "/home/projects/incite11/ensda_v351/ensda_"
setenv odir "/scratch2/scratchdirs/${USER}/ensda_v401/ensda_"


#1845
#foreach ryear (18{5,6,7,8}{0,5} 1890)# ,7,8}{0,5} 1890)#189{5} 19{0,1,2,3,4}{0,5})#,5,6,7,8,9}{0,5})
# 1830 ,6,7,8,9 19{0,1,2,3,4}0,5}
#foreach ryear (18{3,4,5,6,7,8,9}{0,5} 19{0,1,2,3,4,5,6,7,8,9}{0,5} 200{0}) #,5,6,7,8,9}{0,5})


# for 1999 files, set ryear to 1995 and add 4
foreach ryear (1940) # this is the stream year
	set tidir = $idir$ryear
	
	@ uryear = $ryear + 5 # will put it in the 2000 stream
	set todir = $odir$uryear
	mkdir -p $todir
	
	echo $tidir
	echo $todir
	
	#@ year = $ryear + 4 
	@ year = $ryear + 4 + 1
	
#foreach mmdd (0901 0902) # month and day list to pull off goes here
foreach mmdd (0101 0102) # month and day list to pull off goes here


	
	set yyyymmdd = ${year}${mmdd}
	echo $yyyymmdd
	cd $todir
	hsi "get $tidir/${yyyymmdd}.tar"
	
	
	if (-e ${yyyymmdd}.tar) then

	   tar -xvf ${yyyymmdd}.tar
	   echo "${yyyymmdd}00" >&! archivedate_file_$ryear
	endif
	echo "setenv analdate_start ${yyyymmdd}00" >&! analdate.csh
	
end

end
