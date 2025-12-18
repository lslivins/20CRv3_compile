#!/bin/tcsh

# uncomment the first sbatch of a debug 1 cycle job to get started if no other jobs have been submitted
sbatch gfsenkf_20crV3_cmip5oz_new_haswell_short.csh >&! current_jobid
# presume that already have been submitting, so
# only want to add on more held jobs


foreach inum (1)# 2 3 4 5 6 7 8 9 {1,2,3}{0,1,2,3,4,5,6,7,8,9})

	setenv UJOBID `cat current_jobid | cut -f4 -d" "`
	echo $UJOBID
	sbatch --dependency=afterok:$UJOBID gfsenkf_20crV3_cmip5oz_new_haswell.csh >&! current_jobid
	#sleep 8
	

end

