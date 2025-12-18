#!/bin/csh
# Testing whether sending process to background
# works on KNL and Haswell

# Finding: it does not under NERSC Slurm
# works under Moab/Torque PBS

# try to run interactively on KNL
# salloc -p debug -t 20:00 -C knl,quad,cache
# or Haswell
# salloc -p debug -t 20:00 -C haswell ./test_process_to_background_retry.csh


echo "inside test script"
uname -a
echo " "

date

echo "starting non-exclusive sleeps `date`"
foreach icount (1 2 3)# 4 5 6 7 8 9 10)
	echo "$icount `date`"
	srun -n1 sleep 60 &
end
sleep 1
squeue -u compo
squeue -s -u compo
wait
echo "finished with all non-exclusive sleeps `date`"


echo "starting exclusive sleeps `date`"
foreach icount (1 2 3)# 4 5 6 7 8 9 10)
	echo "$icount `date`"
	srun -n1 --exclusive sleep 60 &
end
sleep 1
squeue -u compo
squeue -s -u compo
wait
echo "finished with all exclusive sleeps `date`"


exit 0


