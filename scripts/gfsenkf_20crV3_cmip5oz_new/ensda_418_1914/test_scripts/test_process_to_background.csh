#!/bin/csh
# Testing whether sending process to background
# works on KNL and Haswell

# Finding: it does not under NERSC Slurm
# works under Moab/Torque PBS

# try to run interactively on KNL
# salloc -p debug -t 20:00 -C knl,quad,cache
# or Haswell
# salloc -p debug -t 20:00 -C haswell
# then
# ./test_process_to_background.csh

echo "inside test script"
uname -a
echo " "

date

echo "starting sleeps `date`"
foreach icount (1 2 3 4 5 6 7 8 9 10)
	echo "$icount `date`"
	srun -N1 -n1 sleep 60 &
end

wait
echo "finished with all sleeps `date`"

exit 0


