#!/bin/tcsh


foreach fyear ({17,18,19}{0,1,2,3,4,5,6,7,8,9}0 20{0,1,2}0) 
	
	@ syear = $fyear + 9
	set per="${fyear}-${syear}"
	echo $per

	set infile=crowley+unterman.global_volcanic_aerosols_$per.txt
	set outfile=global_volcanic_aerosols_$per.txt
	echo $infile
	echo $outfile
	cp $infile $outfile	

end
