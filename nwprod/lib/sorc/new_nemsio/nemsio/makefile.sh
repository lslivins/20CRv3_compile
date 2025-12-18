#!/bin/sh
set -x
export machine=".Zeus.intel"
export LIBDIR=/scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/lib
make -f Makefile clean
make -f Makefile
