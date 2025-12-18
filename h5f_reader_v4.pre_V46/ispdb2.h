// ispdb.h
// header file for ispdb.c
//
// March 2, 2006
//
// written by Nobuki Matsui
// CIRES/PSD/ESRL/NOAA
// 
// Functions below create ISPDB h5 tables
// structure type data is defined in ispdb_data.h and
// is used as input to the functions.
//
// INVARIANTS:
// hid_t loc_id, fed to all the functions is assumed to be at the root level.
//
// One needs to have HDF5 1.8.x to compile the code
//
// C can't tolerate fancy dynamic memory allocation like C++.
// Please allocate memory before calling certain APIs to get data from tables
//
//

#ifndef ISPDB_H
#define ISPDB_H

#include "ispdb_data.h"
//#include "H5TA.h"
//#include "H5LT.h"

// -------------------------------
// Functions to open files
// -------------------------------
// function to read open file
hid_t ropen_h5file(char* filename);
// function to write open file
hid_t wopen_h5file(char* filename);

// function to create a h5 file
hid_t create_h5file(char* filename);

// functions not used anymore
// The original FeedBack table was inadequate and the functions were used to upgrade the old
// tables.
void renew_feedback_table (herr_t loc_id);
void fill_newfeedback(NewFeedBack newfeedback[], const int n);
herr_t process_dataindex(hid_t h5file);

// -------------------------------
// Functions to get information about the table
// -------------------------------
// nrecords_out : number of columns
// total          number of rows
void info_observations (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime);
void info_origobservations (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime);
void info_obstype (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime);
void info_origmetadata (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime);
void info_metadata (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime);
void info_source (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime);
void info_tracking (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime);
void info_corrections (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime);
void info_presinst (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime,const int etime);
void info_miscmarine (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime);
void info_srcmarine (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime);
void info_newfeedback (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime);
void info_feedback (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime);

// -------------------------------
// Functions to get data between specific time period (stime <-> etime)
// They haven't been used and they need to be re-worked.
// -------------------------------
void slice_observations (hid_t loc_id, const hsize_t total, const size_t n, const int stime, const int etime, Observations* dat);
void slice_origobservations (hid_t loc_id, const hsize_t total,  const size_t  n,const int stime, const int etime, OrigObservations* dat);
void slice_obstype (hid_t loc_id, const hsize_t total,  const  size_t,const int stime, const int etime, ObsType* dat);
void slice_origmetadata (hid_t loc_id, const hsize_t total,  const  size_t n,const int stime, const int etime, OrigMetaData* dat);
void slice_metadata (hid_t loc_id, const hsize_t total,  const  size_t n,const int stime, const int etime, MetaData* dat);
void slice_source (hid_t loc_id, const hsize_t total,  const  size_t n,const int stime, const int etime, Source* dat);
void slice_tracking (hid_t loc_id, const hsize_t total,  const  size_t n,const int stime, const int etime, Tracking* dat);
void slice_corrections (hid_t loc_id, const hsize_t total,  const  size_t n,const int stime, const int etime, Corrections* dat);
void slice_presinst (hid_t loc_id, const hsize_t total,  const  size_t n,const int stime,const int etime, PresInst* dat);
void slice_miscmarine (hid_t loc_id, const hsize_t total,  const  size_t n,const int stime, const int etime, MiscMarine* dat);
void slice_srcmarine (hid_t loc_id, const hsize_t total,  const  size_t n,const int stime, const int etime, SrcMarine* dat);
void slice_newfeedback (hid_t loc_id, const hsize_t total,  const  size_t n,const int stime, const int etime, NewFeedBack* dat);
void slice_feedback (hid_t loc_id, const hsize_t total,  const  size_t n,const int stime, const int etime, FeedBack* dat);

// -------------------------------
// Functions to get data 
// -------------------------------
void get_newfeedback(hid_t loc_id, NewFeedBack* dat);
void get_feedback(hid_t loc_id, FeedBack* dat);
void get_tracking (hid_t loc_id, Tracking* dat);
void get_obstype (hid_t loc_id, ObsType* dat);
void get_observations (hid_t loc_id, Observations* dat);
void get_metadata (hid_t loc_id, MetaData* dat);
void get_origmetadata (hid_t loc_id, OrigMetaData* dat);
void get_source (hid_t loc_id, Source* dat);
void get_corrections (hid_t loc_id, Corrections* dat);
void get_presinst (hid_t loc_id, PresInst* dat);
void get_origobservations (hid_t loc_id, OrigObservations* dat);
void get_srcmarine (hid_t loc_id, SrcMarine* dat);
void get_miscmarine (hid_t loc_id, MiscMarine* dat);
void get_hurricanes (hid_t loc_id, Hurricanes* dat);
void get_hurricanevariances (hid_t loc_id, HurricaneVariances* dat);
void get_hurricaneid (hid_t loc_id, HurricaneID* dat);
void get_hurricaneqc (hid_t loc_id, HurricaneQC* dat);


// -------------------------------
// Append Table Series
// herr_t append_obs(hid_t loc_id, Observations* dat); 
// This function appends one data
// PRECONDITION: dat must be well formed.
// POSTCONDITION: one data has been appended to dat
// -------------------------------
herr_t append_obs(hid_t loc_id, Observations* dat); 
// the rest are the same
herr_t append_origobs (hid_t loc_id, OrigObservations* dat);
herr_t append_srcmar(hid_t loc_id, SrcMarine* dat);
herr_t append_miscmar(hid_t loc_id, MiscMarine* dat );
herr_t append_corrections(hid_t loc_id, Corrections* dat);
herr_t append_newfeedback(hid_t loc_id, NewFeedBack* dat);
herr_t append_feedback(hid_t loc_id, FeedBack* dat);
herr_t append_metadata(hid_t loc_id, MetaData* dat);
herr_t append_obstype(hid_t loc_id, ObsType* dat);
herr_t append_src(hid_t loc_id, Source* dat);
herr_t append_tracking(hid_t loc_id,Tracking* dat);
herr_t append_presinst(hid_t loc_id, PresInst* dat);
herr_t append_origmetadata(hid_t loc_id, OrigMetaData* dat);
herr_t append_hurricanes(hid_t loc_id, Hurricanes* dat);
herr_t append_hurricanevariances(hid_t loc_id, HurricaneVariances* dat);
herr_t append_hurricaneid(hid_t loc_id, HurricaneID* dati);
herr_t append_hurricaneqc(hid_t loc_id, HurricaneQC* dat);

// -------------------------------
// Insert Table Series
// herr_t insert_obs(hid_t loc_id, Observations* dat); 
// This function inserts one record at the top of the table
// PRECONDITION: dat must be well formed.
// POSTCONDITION: one data has been inserted at the beginning of dat
// -------------------------------
herr_t insert_obs(hid_t loc_id, Observations* dat); 
// the rest are the same
herr_t insert_origobs (hid_t loc_id, OrigObservations* dat);
herr_t insert_srcmar(hid_t loc_id, SrcMarine* dat);
herr_t insert_miscmar(hid_t loc_id, MiscMarine* dat );
herr_t insert_corrections(hid_t loc_id, Corrections* dat);
herr_t insert_newfeedback(hid_t loc_id, NewFeedBack* dat);
herr_t insert_feedback(hid_t loc_id, FeedBack* dat);
herr_t insert_metadata(hid_t loc_id, MetaData* dat);
herr_t insert_obstype(hid_t loc_id, ObsType* dat);
herr_t insert_src(hid_t loc_id, Source* dat);
herr_t insert_tracking(hid_t loc_id,Tracking* dat);
herr_t insert_presinst(hid_t loc_id, PresInst* dat);
herr_t insert_origmetadata(hid_t loc_id, OrigMetaData* dat);


// -------------------------------
// Make Table Series
// PRECONDITIONS of the functions
// 1. loc_id contains hit_t data type int which is given by opening up
// h5 groups.
// 2. nrecs contains the number of data 
// 3. The third argument is a structure type and is filled with data.
// the number of elements inside this data type must match nrecs
//
// POSTCONDITIONS
// 1. tables have been created.
// 2. status will be returned.
// -------------------------------

// herr_t make_obs_table(hid_t loc_id, hsize_t nrecs, Observations dat[]); 
// This function creates observations table.
herr_t make_obs_table(hid_t loc_id, hsize_t nrecs, Observations dat[]); 
 
// herr_t make_corrections_table(hid_t loc_id, hsize_t nrecs, Corrections dat[]);
// This function creates corrections table.
herr_t make_corrections_table(hid_t loc_id, hsize_t nrecs, Corrections dat[]);

// herr_t make_datasetindex (hid_t loc_id);
// This function creates datasetindex table.  This table is a list of contacts
// who provided us data. Only loc_id is a necessary input.
herr_t make_datasetindex (hid_t loc_id);

// herr_t make_feedback_table(hid_t loc_id, hsize_t nrecs, FeedBack dat[]);
// This function creates feedback table.
herr_t make_newfeedback_table(hid_t loc_id, hsize_t nrecs, NewFeedBack dat[]);
herr_t make_feedback_table(hid_t loc_id, hsize_t nrecs, FeedBack dat[]);

// herr_t make_metadata_table(hid_t loc_id, hsize_t nrecs, MetaData dat[]);
// This function creates metadata table.
herr_t make_metadata_table(hid_t loc_id, hsize_t nrecs, MetaData dat[]);

// herr_t make_obstype_table(hid_t loc_id, hsize_t nrecs, ObsType dat[]);
// This function creates obstype table.
herr_t make_obstype_table(hid_t loc_id, hsize_t nrecs, ObsType dat[]);

// herr_t make_obs_original_table(hid_t loc_id, hsize_t nrecs, OrigObservations dat[]);
// This function creates original observations table.
herr_t make_obs_original_table(hid_t loc_id, hsize_t nrecs, OrigObservations dat[]);

// herr_t make_src_table(hid_t loc_id, hsize_t nrecs, Source dat[]);
// This function creates source table.
herr_t make_src_table(hid_t loc_id, hsize_t nrecs, Source dat[]);


//herr_t make_srcmar_table(hid_t loc_id, hsize_t nrecs, SrcMarine dat[]);
// This function creates srcmar table.
herr_t make_srcmar_table(hid_t loc_id, hsize_t nrecs, SrcMarine dat[]);

// herr_t make_tracking_table(hid_t loc_id, hsize_t nrecs, Tracking dat[]);
// This function creates tracking table.
herr_t make_tracking_table(hid_t loc_id, hsize_t nrecs, Tracking dat[]);

// herr_t make_presinst_table(hid_t loc_id, hsize_t nrecs, PresInst dat[]);
// This function creates presinst table.
herr_t make_presinst_table(hid_t loc_id, hsize_t nrecs, PresInst dat[]);

// herr_t make_origmetadata_table(hid_t loc_id, hsize_t nrecs, OrigMetaData dat[]);
// This function creates original metadata table.
herr_t make_origmetadata_table(hid_t loc_id, hsize_t nrecs, OrigMetaData dat[]);


//herr_t make_miscmar_table(hid_t loc_id, hsize_t nrecs, MiscMarine dat[] ); 
// This function creates miscmarine table
herr_t make_miscmar_table(hid_t loc_id, hsize_t nrecs, MiscMarine dat[] );

// herr_t make_hurricanes_table(hid_t loc_id, hsize_t nrecs, Hurricanes dat[] ); 
// This function creates hurricanes table
herr_t make_hurricanes_table(hid_t loc_id, hsize_t nrecs, Hurricanes dat[] ); 

herr_t make_hurricaneqc_table(hid_t loc_id, hsize_t nrecs, HurricaneQC dat[] );

herr_t make_hurricaneid_table(hid_t loc_id, hsize_t nrecs, HurricaneID dat[] );

herr_t make_hurricanevariances_table(hid_t loc_id, hsize_t nrecs, HurricaneVariances dat[] );

// add hurricanes group and table
void add_hurricanes_group (hid_t* file);
// make empty tables
void make_tables(hid_t* file);

// -------------------------------
// useful functions ??  
// -------------------------------
void construct_filenames (const char* outdir, const char* stime, const char* etime, filename files[]);

void sleeping();

// -------------------------------
// Functions below were used for particular applications.
// -------------------------------
void rw_threedim_h5 (const size_t stime, const size_t etime, char* input_filename, hid_t h5files[]);

// read in land data helper function for read_landdata
//void process_files (const char* dataset, const char* syear, const char* indir, hid_t h5files[], const int istime, const int ietime);

// read in marine data
void process_marinefiles (const char* input_filename, const size_t stime, const size_t etime, hid_t h5files[][24] );

// read in land data
// dataset: australia, brazil, ish, russian, td13, sao_1001, etc
// append data to each table 
void write_threedim_h5 (hid_t file, Observations* observations, OrigObservations* origobservations,FeedBack* feedback, ObsType* obstype,OrigMetaData* origmetadata, MetaData* metadata, Source* source, Tracking* tracking, Corrections* corrections, PresInst* presinst ) ;

void append_missing_mar_track ( const size_t stime,  const size_t etime, hid_t h5files[][24]);
#endif
