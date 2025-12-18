# If you run make -k with no other arguments, then it will try to make the
# first target, which I call all.  This will force both executables
#all:   make_tables_h5_c  copy_tables_h5_c  append_marine_h5_c h5totxt h5ftotxt feed_back_v320_h5_c append_hurricanes_ibtracs_h5_c append_land_h5_c
#all:	h5fmtotxt h5ftotxt

#module load cray-hdf5

all:	h5totxt_v4 

#DEBUG_FLAG= -g
#OPT_FLAG= -O2
#OPT_FLAG= -mtune=970
#OPT_FLAG= -force_cpusubtype_ALL
#OPT_FLAG= -mcpu=970
#CC = /opt/cray/craype/2.3.1/bin/cc
CC = cc
#CC =/home/jwhitaker/bin/h5cc
#CC = /opt/local/bin/h5cc



nmtools.o: nmtools.h nmtools.c
	${CC} ${DEBUG_FLAG} ${OPT_FLAG} -c nmtools.c
ispdb2.o: ispdb_data.h ispdb2.h ispdb2.c
	${CC} ${DEBUG_FLAG} ${OPT_FLAG} -c ispdb2.c
h5totxt_v4: ispdb2.o nmtools.o h5totxt_v4.o 
	${CC} ${DEBUG_FLAG} ${OPT_FLAG}  ispdb2.o nmtools.o h5totxt_v4.o -lm -o h5totxt_v4

# Running make -k clean will remove all the files that this makefile
# is capable of making.  It uses the rm command for both Unix and
# Windows, assuming that the cs1300 software is installed (because
# that software includes the rm command).  The -f option says to
# force removal of a file, even if it is read-only.
clean:
	rm -f append_hurricanes_h5_c append_marine_h5_c append_land_h5_c make_tables_h5_c append_marine_h5_c append_hurricanes_gtcw_h5_c feed_back_h5_c feed_back_v320_h5_c feed_back_v320_latlon_h5_c delete_landsrc_h5_c h5totxt_v4 h5ftotxt h5fmtotxt copy_tables_v321_h5_c feed_back_v320_tc_h5_c copy_tables_ispd2.1_h5_c


	rm -f append_hurricanes_ibtracs_h5_c.o  nmtools.o ispdb2.o make_tables_h5_c.o append_land_h5_c.o append_marine_h5_c.o feed_back_h5_c.o feed_back_v320_h5_c.o h5totxt_v4.o h5ftotxt.o h5fmtotxt.o copy_tables_v321_h5_c.o feed_back_v320_tc_h5_c.o copy_tables_ispd2.1_h5_c.o
