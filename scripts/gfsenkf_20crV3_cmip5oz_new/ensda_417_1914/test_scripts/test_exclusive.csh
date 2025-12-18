#!/bin/csh

#launch with 
#salloc -N4 -C haswell test_exclusive.csh

srun -lN1 -n1 --exclusive sleep 60 &
srun -lN1 -n1 --exclusive sleep 60 &
srun -lN1 -n1 --exclusive sleep 60 &
srun -lN1 -n1 --exclusive sleep 60 &
sleep 1
squeue -u compo
squeue -s -u compo
wait
