#!/bin/csh
# Testing whether sending process to background
# works on KNL and Haswell

# Finding: it does not under NERSC Slurm
# works under Moab/Torque PBS

# (have not tested) try to run interactively on KNL 
# salloc -N 3 -C knl,quad,cache ./test_process_to_background_withexclusive.csh
# or Haswell (tested and worked)
# salloc -N 3 -C haswell ./test_process_to_background_withexclusive.csh
# updated 16 Feb 2017 3:47pm PST


echo "inside test script with exclusive retry"
uname -a
echo " "

date

echo "starting sleeps `date`"
# try just 3
foreach icount (1 2 3)# 4 5 6 7 8 9 10)
	echo "$icount `date`"
	# does not work if salloc is not -N 3 for icount up to 3
	srun -n1 sleep 60 & # was sequential
	# try with -N1
	#srun -N1 -n1 --exclusive sleep 60 &
	# worked: srun -N1 -n1 --exclusive sleep 60 &
end
sleep 1
squeue -u compo
squeue -s -u compo
wait
echo "finished with all sleeps `date`"

exit 0


