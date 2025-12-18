#!/bin/bash

#launch with 
#salloc -N4 -C haswell test_exclusive.sh

srun -lN2 -n4 --exclusive sleep 60 &
srun -lN2 -n1 --exclusive sleep 60 &
sleep 1
squeue -u compo
squeue -s -u compo
wait
