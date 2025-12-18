#!/bin/bash

#launch with 
#salloc -N4 -C haswell test.sh

srun -lN2 -n4 -r 2 sleep 60 &
srun -lN2 -r 0 sleep 60 &
sleep 1
squeue -u compo
squeue -s -u compo
wait
