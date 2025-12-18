#!/bin/tcsh

cp -f global_kplist.reanl.txt.long global_kplist.reanl.txt 

grep -v "[87654321][27]5" global_kplist.reanl.txt >&! global_kplist.reanl.txt.fewer
grep -v "   116," global_kplist.reanl.txt.fewer >&! global_kplist.reanl.txt.evenfewer
