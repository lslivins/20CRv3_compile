#!/bin/csh

#launch with 
#salloc -N4 -C haswell test_exclusive_nolabel.csh

srun -N1 -n1 --exclusive sleep 60 &
srun -N1 -n1 --exclusive sleep 60 &
srun -N1 -n1 --exclusive sleep 60 &
srun -N1 -n1 --exclusive sleep 60 &
sleep 1
squeue -u compo
squeue -s -u compo
wait
