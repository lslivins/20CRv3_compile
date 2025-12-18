// Program ispdb2.cpp
// Impelmentations of ispdb2.h functions.
//
// March 2, 2006
//
// Written by Nobuki Matsui
// CIRES/PSD/ESRL/NOAA
//
// ispdb.h contains descriptions of functions below.
//
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "hdf5.h"
//#include "H5LT.h"
//#include "H5TA.h"
#include "hdf5_hl.h"
#include "ispdb2.h"
#include "ispdb_data.h"
#include "nmtools.h"
#include <string.h>

hid_t create_h5file(char* filename){
    unsigned flags=H5P_DEFAULT;
    hid_t file_id, access_plist;
    herr_t status;

    access_plist = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fclose_degree(access_plist, H5F_CLOSE_STRONG);
    if(!(file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, access_plist))){
    //if(!(file_id = H5Fopen(filename,flags, access_plist))){
        fprintf (stderr, "Can't open %s\n", filename);
        exit(1);
    }
    else {
        //printf ("Created %s\n", filename);
	;
    }

    return file_id;
}

hid_t wopen_h5file(char* filename){
    unsigned flags=H5F_ACC_RDWR;
    hid_t file_id, access_plist;
    herr_t status;
    
    access_plist = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fclose_degree(access_plist, H5F_CLOSE_STRONG);

    if(!(file_id = H5Fopen(filename,H5F_ACC_RDWR, access_plist))){
        fprintf (stderr, "Can't open %s\n", filename);
        exit(1);
    }
    //else {
   //     printf ("Opened %s\n", filename);
   // }

    return file_id;
}

hid_t ropen_h5file(char* filename){
    unsigned flags=H5F_ACC_RDONLY;
    hid_t file_id, access_plist;
    herr_t status;

    access_plist = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fclose_degree(access_plist, H5F_CLOSE_STRONG);

    if(!(file_id = H5Fopen(filename,flags, access_plist))){
        fprintf (stderr, "Can't open %s\n", filename);
        exit(1);
    }
    //else {
    //    printf ("Opened %s\n", filename);
   // }

    return file_id;
}

void renew_feedback_table (herr_t loc_id){
    herr_t status;
    hsize_t nfields_out, total;
    hid_t  grpFbk;

    //status=H5TBdelete_record( loc_id, "/Data/AssimilationFeedback/AssimilationFeedBack", 779, 1560);
    //exit(1);

    status=H5TBget_table_info (loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack", &nfields_out, &total);
    printf("total %u\n",total);
    //if( (int) nfields_out == 15) {
        FeedBack *feedback;
        NewFeedBack *newfeedback;
    //ckm    hid_t  grpFbk;
	hid_t plist;
	size_t i;
        total=0;
	feedback = malloc(sizeof(FeedBack)* (int) total);
	newfeedback = malloc(sizeof(NewFeedBack)* (int) total);
    //ckm    get_feedback(loc_id, feedback);
    //ckm	for(i = 0; i < total; i++){
    //ckm        strcpy(newfeedback[i].unoc, feedback[i].unoc);
        //ckm    strcpy(newfeedback[i].timestamp,feedback[i].timestamp);
	//ckm}
        //5112011printf("total %u\n",total); 
        grpFbk = H5Gopen(loc_id,"/Data/AssimilationFeedback",H5P_DEFAULT);
        status = H5Ldelete(grpFbk,"AssimilationFeedBack", H5P_DEFAULT);
        status =  make_newfeedback_table(grpFbk, total, newfeedback);

       H5Gclose(grpFbk); 
        free(feedback);
        free(newfeedback);
   // else 
   //     printf ("Feedback table does not need updating\n");

}

void fill_newfeedback(NewFeedBack newfeedback[], const int n){

    size_t i;

    for(i=0; i < n; i++){
        newfeedback[i].mdpavims = 0.0;
        newfeedback[i].epvims = 0.0;
        newfeedback[i].bias = 0;
        newfeedback[i].sfsfp = 0;
        newfeedback[i].ai = 0;
        newfeedback[i].uc = 0;
        newfeedback[i].bcf = 0;
        newfeedback[i].bf = 0;
        newfeedback[i].qc = 0;
        newfeedback[i].emfg = 0.0;
        newfeedback[i].sdeg = 0.0;
        newfeedback[i].mpmemfg = 0.0;
        newfeedback[i].emap = 0.0;
        newfeedback[i].sdeap = 0.0;
        newfeedback[i].mpmema = 0.0;
        newfeedback[i].melv = 0;
    }
}

void info_observations (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    Observations *tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[8] = {
         HOFFSET(Observations, timestamp),
         HOFFSET(Observations, unoc),
         HOFFSET(Observations, slp),
         HOFFSET(Observations, slpe),
         HOFFSET(Observations, slpqc),
         HOFFSET(Observations, sfp),
         HOFFSET(Observations, sfpe),
         HOFFSET(Observations, sfpqc)
    };

    size_t dset_sizes[8] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(float),  
                            sizeof(float),  
                            sizeof(int),  
                            sizeof(float),  
                            sizeof(float),  
                            sizeof(int)
    };
          
    size_t dset_size = sizeof(Observations);

    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/Data/Observations/Observations", &nfields_out, total);

    tmp = malloc(sizeof(Observations)*(*total));

    status=H5TBread_table(loc_id,"/Data/Observations/Observations",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth
   
    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }
    free(tmp);
}

void info_origobservations (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    OrigObservations *tmp;
    herr_t status;
    hsize_t nfields_out;
    size_t cutoff;

    size_t field_offsets[6] = {
         HOFFSET(OrigObservations, timestamp),
         HOFFSET(OrigObservations, unoc),
         HOFFSET(OrigObservations, oslp),
         HOFFSET(OrigObservations, uoslp),
         HOFFSET(OrigObservations, osfp),
         HOFFSET(OrigObservations, uosfp)
    };

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*10,
                            sizeof(char)*9,
                            sizeof(char)*10,
                            sizeof(char)*9
    };
          
    size_t dset_size = sizeof(OrigObservations);

    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/Data/Observations/OriginalObservations/OriginalObservations", &nfields_out, total);

    tmp = malloc(sizeof(OrigObservations)*(*total));

    status=H5TBread_table(loc_id,"/Data/Observations/OriginalObservations/OriginalObservations",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }
    free(tmp);
}

void info_newfeedback (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    NewFeedBack *tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[18] = {
         HOFFSET(NewFeedBack, timestamp),
         HOFFSET(NewFeedBack, unoc),
         HOFFSET(NewFeedBack, mdpavims),
         HOFFSET(NewFeedBack, epvims),
         HOFFSET(NewFeedBack, bias),
         HOFFSET(NewFeedBack, sfsfp),
         HOFFSET(NewFeedBack, ai),
         HOFFSET(NewFeedBack, uc),
         HOFFSET(NewFeedBack, bcf),
         HOFFSET(NewFeedBack, bf),
         HOFFSET(NewFeedBack, qc),
         HOFFSET(NewFeedBack, emfg),
         HOFFSET(NewFeedBack, sdeg),
         HOFFSET(NewFeedBack, mpmemfg),
         HOFFSET(NewFeedBack, emap),
         HOFFSET(NewFeedBack, sdeap),
         HOFFSET(NewFeedBack, mpmema),
         HOFFSET(NewFeedBack, melv)
    };

    size_t dset_sizes[18] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(double),
                            sizeof(float),
                            sizeof(double),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(int)
    };
          
    size_t dset_size = sizeof(NewFeedBack);

    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack", &nfields_out, total);

    tmp = malloc(sizeof(NewFeedBack)*(*total));

    status=H5TBread_table(loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }
    free(tmp);
}

void info_feedback (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    FeedBack *tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[15] = {
         HOFFSET(FeedBack, timestamp),
         HOFFSET(FeedBack, unoc),
         HOFFSET(FeedBack, mdpavims),
         HOFFSET(FeedBack, epvims),
         HOFFSET(FeedBack, sfsfp),
         HOFFSET(FeedBack, dc),
         HOFFSET(FeedBack, ai),
         HOFFSET(FeedBack, bcf),
         HOFFSET(FeedBack, bf),
         HOFFSET(FeedBack, emfg),
         HOFFSET(FeedBack, sdeg),
         HOFFSET(FeedBack, mpmemfg),
         HOFFSET(FeedBack, emap),
         HOFFSET(FeedBack, sdeap),
         HOFFSET(FeedBack, mpmema)
    };

    size_t dset_sizes[15] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(double),
                            sizeof(float),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double)
    };
          
    size_t dset_size = sizeof(FeedBack);

    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack", &nfields_out, total);

    tmp = malloc(sizeof(FeedBack)*(*total));

    status=H5TBread_table(loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }
    free(tmp);
}

void info_obstype (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    ObsType* tmp;
    herr_t status;
    hsize_t nfields_out;
    int  cutoff;

    size_t field_offsets[5] = {
         HOFFSET(ObsType, timestamp),
         HOFFSET(ObsType, unoc),
         HOFFSET(ObsType, id_type),
         HOFFSET(ObsType, ncep_type),
         HOFFSET(ObsType, ispdbcid)
    };

    size_t dset_sizes[5] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),  
                            sizeof(int),
                            sizeof(char)*7
    };
          
    size_t dset_size = sizeof(ObsType);

    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/Data/Observations/ObservationTypes", &nfields_out, total);

    tmp = malloc(sizeof(ObsType)*(*total));

    status=H5TBread_table(loc_id,"/Data/Observations/ObservationTypes",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }

    free(tmp);
}

void info_origmetadata (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    OrigMetaData *tmp;
    herr_t status;
    hsize_t nfields_out;
    int  cutoff;

    size_t field_offsets[6] = {
         HOFFSET(OrigMetaData, timestamp),
         HOFFSET(OrigMetaData, unoc),
         HOFFSET(OrigMetaData, olat),
         HOFFSET(OrigMetaData, olon),
         HOFFSET(OrigMetaData, oelev),
         HOFFSET(OrigMetaData, uoelev)
    };

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*9,
                            sizeof(char)*9,
                            sizeof(char)*7,
                            sizeof(char)*9
    };
          
    size_t dset_size = sizeof(OrigMetaData);

    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/Data/SpatialTemporalLocation/OriginalSpatialTemporalLocation/OriginalSpatialTemporalLocation", &nfields_out, total);

    tmp = malloc(sizeof(OrigMetaData)* (*total));
    
    status=H5TBread_table(loc_id,"/Data/SpatialTemporalLocation/OriginalSpatialTemporalLocation/OriginalSpatialTemporalLocation",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }

    free(tmp);
}

void info_metadata (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    MetaData *tmp; 
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[16] = {
         HOFFSET(MetaData, timestamp),
         HOFFSET(MetaData, unoc),
         HOFFSET(MetaData, id),
         HOFFSET(MetaData, year),
         HOFFSET(MetaData, month),
         HOFFSET(MetaData, day),
         HOFFSET(MetaData, hour),
         HOFFSET(MetaData, minute),
         HOFFSET(MetaData, second),
         HOFFSET(MetaData, time_in_sec),
         HOFFSET(MetaData, time_code),
         HOFFSET(MetaData, n_obs),
         HOFFSET(MetaData, h_gmt),
         HOFFSET(MetaData, lat),
         HOFFSET(MetaData, lon),
         HOFFSET(MetaData, elv)
    };

    size_t dset_sizes[16] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*14,
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(char)*4,
                            sizeof(int),
                            sizeof(char)*200,
                            sizeof(float),
                            sizeof(float),
                            sizeof(int)
    };
          
    size_t dset_size = sizeof(MetaData);

    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/Data/SpatialTemporalLocation/SpatialTemporalLocation", &nfields_out, total);

    tmp = malloc(sizeof(MetaData)*(*total));

    status=H5TBread_table(loc_id,"/Data/SpatialTemporalLocation/SpatialTemporalLocation",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }

    free(tmp);
}

void info_source (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    Source *tmp;
    herr_t status;
    hsize_t nfields_out;
    int  cutoff;

    size_t field_offsets[6] = {
         HOFFSET(Source, timestamp),
         HOFFSET(Source, unoc),
         HOFFSET(Source, sflsd),
         HOFFSET(Source, rtc),
         HOFFSET(Source, qcislp),
         HOFFSET(Source, qcisfp)
    };

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*2,
                            sizeof(char)*6,
                            sizeof(char)*6,
                            sizeof(char)*6
    };
          
    size_t dset_size = sizeof(Source);
 
    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/SupplementalData/Tracking/Land/TrackingLand", &nfields_out, total);

    tmp = malloc(sizeof(Source)*(*total));

    status=H5TBread_table(loc_id,"/SupplementalData/Tracking/Land/TrackingLand",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }
    free(tmp);
}

void info_tracking (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    Tracking* tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[4] = {
         HOFFSET(Tracking, timestamp),
         HOFFSET(Tracking, unoc),
         HOFFSET(Tracking, sname),
         HOFFSET(Tracking, slib)
    };

    size_t dset_sizes[4] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*31,
                            sizeof(char)*4
    };
          
    size_t dset_size = sizeof(Tracking);

    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/SupplementalData/Tracking/Tracking", &nfields_out, total);

    tmp = malloc(sizeof(Tracking)*(*total));

    status=H5TBread_table(loc_id,"/SupplementalData/Tracking/Tracking",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }
    free(tmp);
}

void info_corrections (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    Corrections *tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[11] = {
         HOFFSET(Corrections, timestamp),
         HOFFSET(Corrections, unoc),
         HOFFSET(Corrections, gcs),
         HOFFSET(Corrections, gcispd),
         HOFFSET(Corrections, tk),
         HOFFSET(Corrections, tat),
         HOFFSET(Corrections, utat),
         HOFFSET(Corrections, tcs),
         HOFFSET(Corrections, tcispd),
         HOFFSET(Corrections, hcs),
         HOFFSET(Corrections, hcispd)
    };

    size_t dset_sizes[11] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(int),
                            sizeof(char)*10,
                            sizeof(char)*9,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(char)*31
    };
          
    size_t dset_size = sizeof(Corrections);

    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/SupplementalData/Corrections/Corrections", &nfields_out, total);

    tmp = malloc(sizeof(Corrections)* (*total));

    status=H5TBread_table(loc_id,"/SupplementalData/Corrections/Corrections",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }
    free(tmp);
}

void info_presinst (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    PresInst* tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[3] = {
         HOFFSET(PresInst, timestamp),
         HOFFSET(PresInst, unoc),
         HOFFSET(PresInst, PressureInst)
    };

    size_t dset_sizes[3] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(unsigned int)
    };
          
    size_t dset_size = sizeof(PresInst);

    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/SupplementalData/Corrections/PressureInstrument", &nfields_out, total);

    tmp = malloc(sizeof(PresInst)*(*total));

    status=H5TBread_table(loc_id,"/SupplementalData/Corrections/PressureInstrument",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }

    free(tmp);
}

void info_miscmarine (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    MiscMarine *tmp;
    herr_t status;
    hsize_t nfields_out;
    int  cutoff;

    size_t field_offsets[3] = {
         HOFFSET(MiscMarine, timestamp),
         HOFFSET(MiscMarine, unoc),
         HOFFSET(MiscMarine, PressureBias)
    };

    size_t dset_sizes[3] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(unsigned int)
    };
          
    size_t dset_size = sizeof(MiscMarine);

    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/SupplementalData/Misc/ICOADS/PressureBias", &nfields_out, total);

    tmp = malloc(sizeof(MiscMarine)* (*total));

    status=H5TBread_table(loc_id,"/SupplementalData/Misc/ICOADS/PressureBias",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }
    free(tmp);
}

void info_srcmarine (hid_t loc_id, size_t* nrecords_out, hsize_t* total, const int stime, const int etime){
    SrcMarine *tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[12] = {
         HOFFSET(SrcMarine, timestamp),
         HOFFSET(SrcMarine, unoc),
         HOFFSET(SrcMarine, sid),
         HOFFSET(SrcMarine, dck),
         HOFFSET(SrcMarine, pt),
         HOFFSET(SrcMarine, orig_hr),
         HOFFSET(SrcMarine, uid),
         HOFFSET(SrcMarine, rn1),
         HOFFSET(SrcMarine, rn2),
         HOFFSET(SrcMarine, rn3),
         HOFFSET(SrcMarine, rsa),
         HOFFSET(SrcMarine, irf)
    };

    size_t dset_sizes[12] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(char)*4,
                            sizeof(char)*6,
                            sizeof(char),
                            sizeof(char),
                            sizeof(char),
                            sizeof(int),
                            sizeof(int)
    };
          
    size_t dset_size = sizeof(SrcMarine);

    /* Get table info  */
    status=H5TBget_table_info (loc_id,"/SupplementalData/Tracking/ICOADS/TrackingICOADS", &nfields_out, total);

    tmp = malloc(sizeof(SrcMarine)*(*total));

    status=H5TBread_table(loc_id,"/SupplementalData/Tracking/ICOADS/TrackingICOADS",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i=0; 
    for(i=0; i < (int) *total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
           *nrecords_out = *nrecords_out + 1;
        }
    }
    free(tmp);
}

//-----------------------------------------------------------------------------------------------------------------------------

void slice_observations (hid_t loc_id, const hsize_t total, const size_t n, const int stime, const int etime, Observations* dat){
    Observations *tmp;
    herr_t status;
    int cutoff;

    size_t field_offsets[8] = {
         HOFFSET(Observations, timestamp),
         HOFFSET(Observations, unoc),
         HOFFSET(Observations, slp),
         HOFFSET(Observations, slpe),
         HOFFSET(Observations, slpqc),
         HOFFSET(Observations, sfp),
         HOFFSET(Observations, sfpe),
         HOFFSET(Observations, sfpqc)
    };

    size_t dset_sizes[8] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(float),  
                            sizeof(float),  
                            sizeof(int),  
                            sizeof(float),  
                            sizeof(float),  
                            sizeof(int)
    };
          
    size_t dset_size = sizeof(Observations);

    tmp = malloc(sizeof(Observations)*total);

    status=H5TBread_table(loc_id,"/Data/Observations/Observations",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth
    size_t j = 0;

    size_t i=0; 
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }

    if(n != j) printf ("observations total not matching pre-supplied info\n");
    free(tmp);
}

void slice_origobservations (hid_t loc_id, const hsize_t total, const size_t n, const int stime, const int etime, OrigObservations* dat){
    OrigObservations *tmp;
    herr_t status;
    size_t cutoff;

    size_t field_offsets[6] = {
         HOFFSET(OrigObservations, timestamp),
         HOFFSET(OrigObservations, unoc),
         HOFFSET(OrigObservations, oslp),
         HOFFSET(OrigObservations, uoslp),
         HOFFSET(OrigObservations, osfp),
         HOFFSET(OrigObservations, uosfp)
    };

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*10,
                            sizeof(char)*9,
                            sizeof(char)*10,
                            sizeof(char)*9
    };
          
    size_t dset_size = sizeof(OrigObservations);


    tmp = malloc(sizeof(OrigObservations)*total);

    status=H5TBread_table(loc_id,"/Data/Observations/OriginalObservations/OriginalObservations",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t i = 0;
    size_t j = 0;
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }
    if(n != j) printf ("origobservations total not matching pre-supplied info\n");
    free(tmp);
}

void slice_newfeedback (hid_t loc_id, const hsize_t total, const size_t n, const int stime, const int etime, NewFeedBack* dat){
    NewFeedBack *tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[18] = {
         HOFFSET(NewFeedBack, timestamp),
         HOFFSET(NewFeedBack, unoc),
         HOFFSET(NewFeedBack, mdpavims),
         HOFFSET(NewFeedBack, epvims),
         HOFFSET(NewFeedBack, bias),
         HOFFSET(NewFeedBack, sfsfp),
         HOFFSET(NewFeedBack, ai),
         HOFFSET(NewFeedBack, uc),
         HOFFSET(NewFeedBack, bcf),
         HOFFSET(NewFeedBack, bf),
         HOFFSET(NewFeedBack, qc),
         HOFFSET(NewFeedBack, emfg),
         HOFFSET(NewFeedBack, sdeg),
         HOFFSET(NewFeedBack, mpmemfg),
         HOFFSET(NewFeedBack, emap),
         HOFFSET(NewFeedBack, sdeap),
         HOFFSET(NewFeedBack, mpmema),
         HOFFSET(NewFeedBack, melv)
    };

    size_t dset_sizes[18] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(double),
                            sizeof(float),
                            sizeof(double),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(int)
    };
          
    size_t dset_size = sizeof(NewFeedBack);

    tmp = malloc(sizeof(NewFeedBack)*total);

    status=H5TBread_table(loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t j = 0;
    size_t i = 0;
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }
    if(n != j) printf ("feedback total not matching pre-supplied info\n");
    free(tmp);
}

void slice_feedback (hid_t loc_id, const hsize_t total, const size_t n, const int stime, const int etime, FeedBack* dat){
    FeedBack *tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[15] = {
         HOFFSET(FeedBack, timestamp),
         HOFFSET(FeedBack, unoc),
         HOFFSET(FeedBack, mdpavims),
         HOFFSET(FeedBack, epvims),
         HOFFSET(FeedBack, sfsfp),
         HOFFSET(FeedBack, dc),
         HOFFSET(FeedBack, ai),
         HOFFSET(FeedBack, bcf),
         HOFFSET(FeedBack, bf),
         HOFFSET(FeedBack, emfg),
         HOFFSET(FeedBack, sdeg),
         HOFFSET(FeedBack, mpmemfg),
         HOFFSET(FeedBack, emap),
         HOFFSET(FeedBack, sdeap),
         HOFFSET(FeedBack, mpmema)
    };

    size_t dset_sizes[15] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(double),
                            sizeof(float),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double)
    };
          
    size_t dset_size = sizeof(FeedBack);

    tmp = malloc(sizeof(FeedBack)*total);

    status=H5TBread_table(loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t j = 0;
    size_t i = 0;
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }
    if(n != j) printf ("feedback total not matching pre-supplied info\n");
    free(tmp);
}

void slice_obstype (hid_t loc_id, const hsize_t total, const size_t n, const int stime, const int etime, ObsType* dat){
    ObsType* tmp;
    herr_t status;
    hsize_t nfields_out;
    int  cutoff;

    size_t field_offsets[5] = {
         HOFFSET(ObsType, timestamp),
         HOFFSET(ObsType, unoc),
         HOFFSET(ObsType, id_type),
         HOFFSET(ObsType, ncep_type),
         HOFFSET(ObsType, ispdbcid)
    };

    size_t dset_sizes[5] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),  
                            sizeof(int),
                            sizeof(char)*7
    };
          
    size_t dset_size = sizeof(ObsType);

    tmp = malloc(sizeof(ObsType)*total);

    status=H5TBread_table(loc_id,"/Data/Observations/ObservationTypes",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t j = 0;
    size_t i = 0;
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }
    if(n != j) printf ("observationtypes total not matching pre-supplied info\n");
    free(tmp);
}

void slice_origmetadata (hid_t loc_id, const hsize_t total, const size_t n, int stime, const int etime, OrigMetaData* dat){
    OrigMetaData *tmp;
    herr_t status;
    hsize_t nfields_out;
    int  cutoff;

    size_t field_offsets[6] = {
         HOFFSET(OrigMetaData, timestamp),
         HOFFSET(OrigMetaData, unoc),
         HOFFSET(OrigMetaData, olat),
         HOFFSET(OrigMetaData, olon),
         HOFFSET(OrigMetaData, oelev),
         HOFFSET(OrigMetaData, uoelev)
    };

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*9,
                            sizeof(char)*9,
                            sizeof(char)*7,
                            sizeof(char)*9
    };
          
    size_t dset_size = sizeof(OrigMetaData);

    tmp = malloc(sizeof(OrigMetaData)*total);
    
    status=H5TBread_table(loc_id,"/Data/SpatialTemporalLocation/OriginalSpatialTemporalLocation/OriginalSpatialTemporalLocation",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t j = 0;
    size_t i = 0;
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }
    if(n != j) printf ("orig metadata total not matching pre-supplied info\n");
    free(tmp);
}

void slice_metadata (hid_t loc_id, const hsize_t total, const size_t n, const int stime, const int etime, MetaData* dat){
    MetaData *tmp; 
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[16] = {
         HOFFSET(MetaData, timestamp),
         HOFFSET(MetaData, unoc),
         HOFFSET(MetaData, id),
         HOFFSET(MetaData, year),
         HOFFSET(MetaData, month),
         HOFFSET(MetaData, day),
         HOFFSET(MetaData, hour),
         HOFFSET(MetaData, minute),
         HOFFSET(MetaData, second),
         HOFFSET(MetaData, time_in_sec),
         HOFFSET(MetaData, time_code),
         HOFFSET(MetaData, n_obs),
         HOFFSET(MetaData, h_gmt),
         HOFFSET(MetaData, lat),
         HOFFSET(MetaData, lon),
         HOFFSET(MetaData, elv)
    };

    size_t dset_sizes[16] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*14,
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(char)*4,
                            sizeof(int),
                            sizeof(char)*200,
                            sizeof(float),
                            sizeof(float),
                            sizeof(int)
    };
          
    size_t dset_size = sizeof(MetaData);

    tmp = malloc(sizeof(MetaData)*total);

    status=H5TBread_table(loc_id,"/Data/SpatialTemporalLocation/SpatialTemporalLocation",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t j = 0;
    size_t i = 0;
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }
    if(n  != j) printf  ("metadata total not matching pre-supplied info\n");
    free(tmp);
}

void slice_source (hid_t loc_id, const hsize_t total, const size_t n, const int stime, const int etime, Source* dat){
    Source *tmp;
    herr_t status;
    hsize_t nfields_out;
    int  cutoff;

    size_t field_offsets[6] = {
         HOFFSET(Source, timestamp),
         HOFFSET(Source, unoc),
         HOFFSET(Source, sflsd),
         HOFFSET(Source, rtc),
         HOFFSET(Source, qcislp),
         HOFFSET(Source, qcisfp)
    };

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*2,
                            sizeof(char)*6,
                            sizeof(char)*6,
                            sizeof(char)*6
    };
          
    size_t dset_size = sizeof(Source);
 
    tmp = malloc(sizeof(Source)*total);

    status=H5TBread_table(loc_id,"/SupplementalData/Tracking/Land/TrackingLand",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t j = 0;
    size_t i = 0;
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }
    if(n != j) printf  ("source total not matching pre-supplied info\n");
    free(tmp);
}

void slice_tracking (hid_t loc_id, const hsize_t total, const size_t n, const int stime, const int etime, Tracking* dat){
    Tracking* tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[4] = {
         HOFFSET(Tracking, timestamp),
         HOFFSET(Tracking, unoc),
         HOFFSET(Tracking, sname),
         HOFFSET(Tracking, slib)
    };

    size_t dset_sizes[4] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*31,
                            sizeof(char)*4
    };
          
    size_t dset_size = sizeof(Tracking);

    tmp = malloc(sizeof(Tracking)*total);

    status=H5TBread_table(loc_id,"/SupplementalData/Tracking/Tracking",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t j = 0;
    size_t i = 0;
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }
    if(n != j) printf ("tracking total not matching pre-supplied info\n");
    free(tmp);
}

void slice_corrections (hid_t loc_id, const hsize_t total, const size_t n, const int stime, const int etime, Corrections* dat){
    Corrections *tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[11] = {
         HOFFSET(Corrections, timestamp),
         HOFFSET(Corrections, unoc),
         HOFFSET(Corrections, gcs),
         HOFFSET(Corrections, gcispd),
         HOFFSET(Corrections, tk),
         HOFFSET(Corrections, tat),
         HOFFSET(Corrections, utat),
         HOFFSET(Corrections, tcs),
         HOFFSET(Corrections, tcispd),
         HOFFSET(Corrections, hcs),
         HOFFSET(Corrections, hcispd)
    };

    size_t dset_sizes[11] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(int),
                            sizeof(char)*10,
                            sizeof(char)*9,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(char)*31
    };
          
    size_t dset_size = sizeof(Corrections);

    tmp = malloc(sizeof(Corrections)*total);

    status=H5TBread_table(loc_id,"/SupplementalData/Corrections/Corrections",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t j = 0;
    size_t i = 0;
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }

    if(n != j) printf ( "corrections total not matching pre-supplied info\n");
    free(tmp);
}

void slice_presinst (hid_t loc_id, const hsize_t total,  const size_t n, const int stime, const int etime, PresInst* dat){
    PresInst* tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[3] = {
         HOFFSET(PresInst, timestamp),
         HOFFSET(PresInst, unoc),
         HOFFSET(PresInst, PressureInst)
    };

    size_t dset_sizes[3] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(unsigned int)
    };
          
    size_t dset_size = sizeof(PresInst);

    tmp = malloc(sizeof(PresInst)*total);

    status=H5TBread_table(loc_id,"/SupplementalData/Corrections/PressureInstrument",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t j = 0;
    size_t i = 0;
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }
    if(n != j) printf ( "presinst total not matching pre-supplied info\n");
    free(tmp);
}

void slice_miscmarine (hid_t loc_id, const hsize_t total, const size_t n, const int stime, const int etime, MiscMarine* dat){
    MiscMarine *tmp;
    herr_t status;
    hsize_t nfields_out;
    int  cutoff;

    size_t field_offsets[3] = {
         HOFFSET(MiscMarine, timestamp),
         HOFFSET(MiscMarine, unoc),
         HOFFSET(MiscMarine, PressureBias)
    };

    size_t dset_sizes[3] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(unsigned int)
    };
          
    size_t dset_size = sizeof(MiscMarine);

    tmp = malloc(sizeof(MiscMarine)*total);

    status=H5TBread_table(loc_id,"/SupplementalData/Misc/ICOADS/PressureBias",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t j = 0;
    size_t i = 0;
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }
    if(n != j) printf ( "misc marine total not matching pre-supplied info\n");
    free(tmp);
}

void slice_srcmarine (hid_t loc_id, const hsize_t total, const size_t n, const int stime, const int etime, SrcMarine* dat){
    SrcMarine *tmp;
    herr_t status;
    hsize_t nfields_out;
    int cutoff;

    size_t field_offsets[12] = {
         HOFFSET(SrcMarine, timestamp),
         HOFFSET(SrcMarine, unoc),
         HOFFSET(SrcMarine, sid),
         HOFFSET(SrcMarine, dck),
         HOFFSET(SrcMarine, pt),
         HOFFSET(SrcMarine, orig_hr),
         HOFFSET(SrcMarine, uid),
         HOFFSET(SrcMarine, rn1),
         HOFFSET(SrcMarine, rn2),
         HOFFSET(SrcMarine, rn3),
         HOFFSET(SrcMarine, rsa),
         HOFFSET(SrcMarine, irf)
    };

    size_t dset_sizes[12] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(char)*4,
                            sizeof(char)*6,
                            sizeof(char),
                            sizeof(char),
                            sizeof(char),
                            sizeof(int),
                            sizeof(int)
    };
          
    size_t dset_size = sizeof(SrcMarine);

    tmp = malloc(sizeof(SrcMarine)*total);

    status=H5TBread_table(loc_id,"/SupplementalData/Tracking/ICOADS/TrackingICOADS",dset_size,field_offsets,dset_sizes,tmp);

    cutoff = 8;   // we caompare 8 chars   day worth

    size_t j = 0;
    size_t i = 0;
    for(i=0; i < (int) total;i++){
        int tmptime = get_intvalue(tmp[i].timestamp,0,cutoff);
        if(tmptime >= stime &&  tmptime <= etime){
            dat[j] = tmp[i];
            j++; 
        }
    }
    if(n != j) printf ( "src marine total not matching pre-supplied info\n");

    free(tmp);
}

void get_newfeedback(hid_t loc_id, NewFeedBack* dat){
    herr_t status;

    size_t field_offsets[18] = {
         HOFFSET(NewFeedBack, timestamp),
         HOFFSET(NewFeedBack, unoc),
         HOFFSET(NewFeedBack, mdpavims),
         HOFFSET(NewFeedBack, epvims),
         HOFFSET(NewFeedBack, bias),
         HOFFSET(NewFeedBack, sfsfp),
         HOFFSET(NewFeedBack, ai),
         HOFFSET(NewFeedBack, uc),
         HOFFSET(NewFeedBack, bcf),
         HOFFSET(NewFeedBack, bf),
         HOFFSET(NewFeedBack, qc),
         HOFFSET(NewFeedBack, emfg),
         HOFFSET(NewFeedBack, sdeg),
         HOFFSET(NewFeedBack, mpmemfg),
         HOFFSET(NewFeedBack, emap),
         HOFFSET(NewFeedBack, sdeap),
         HOFFSET(NewFeedBack, mpmema),
         HOFFSET(NewFeedBack, melv)
    };

    size_t dset_sizes[18] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(double),
                            sizeof(float),
                            sizeof(double),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(int)
    };

    size_t dset_size = sizeof(NewFeedBack);

    status=H5TBread_table(loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack",dset_size,field_offsets,dset_sizes,dat);
}

void get_feedback(hid_t loc_id, FeedBack* dat){
    herr_t status;

    size_t field_offsets[15] = {
         HOFFSET(FeedBack, timestamp),
         HOFFSET(FeedBack, unoc),
         HOFFSET(FeedBack, mdpavims),
         HOFFSET(FeedBack, epvims),
         HOFFSET(FeedBack, sfsfp),
         HOFFSET(FeedBack, dc),
         HOFFSET(FeedBack, ai),
         HOFFSET(FeedBack, bcf),
         HOFFSET(FeedBack, bf),
         HOFFSET(FeedBack, emfg),
         HOFFSET(FeedBack, sdeg),
         HOFFSET(FeedBack, mpmemfg),
         HOFFSET(FeedBack, emap),
         HOFFSET(FeedBack, sdeap),
         HOFFSET(FeedBack, mpmema)
    };

    size_t dset_sizes[15] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(double),
                            sizeof(float),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double)
    };

    size_t dset_size = sizeof(FeedBack);

    status=H5TBread_table(loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack",dset_size,field_offsets,dset_sizes,dat);
}

void get_metadata (hid_t loc_id, MetaData* dat){
    herr_t status;
    hsize_t nfields_out;

    size_t field_offsets[16] = {
         HOFFSET(MetaData, timestamp),
         HOFFSET(MetaData, unoc),
         HOFFSET(MetaData, id),
         HOFFSET(MetaData, year),
         HOFFSET(MetaData, month),
         HOFFSET(MetaData, day),
         HOFFSET(MetaData, hour),
         HOFFSET(MetaData, minute),
         HOFFSET(MetaData, second),
         HOFFSET(MetaData, time_in_sec),
         HOFFSET(MetaData, time_code),
         HOFFSET(MetaData, n_obs),
         HOFFSET(MetaData, h_gmt),
         HOFFSET(MetaData, lat),
         HOFFSET(MetaData, lon),
         HOFFSET(MetaData, elv)
    };

    size_t dset_sizes[16] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*14,
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(char)*4,
                            sizeof(int),
                            sizeof(char)*200,
                            sizeof(float),
                            sizeof(float),
                            sizeof(int)
    };

    size_t dset_size = sizeof(MetaData);

    status=H5TBread_table(loc_id,"/Data/SpatialTemporalLocation/SpatialTemporalLocation",dset_size,field_offsets,dset_sizes,dat);
}


void get_observations (hid_t loc_id, Observations* dat){
    herr_t status;

    size_t field_offsets[8] = {
         HOFFSET(Observations, timestamp),
         HOFFSET(Observations, unoc),
         HOFFSET(Observations, slp),
         HOFFSET(Observations, slpe),
         HOFFSET(Observations, slpqc),
         HOFFSET(Observations, sfp),
         HOFFSET(Observations, sfpe),
         HOFFSET(Observations, sfpqc)
    };

    size_t dset_sizes[8] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(float),
                            sizeof(float),
                            sizeof(int),
                            sizeof(float),
                            sizeof(float),
                            sizeof(int)
    };

    size_t dset_size = sizeof(Observations);


    status=H5TBread_table(loc_id,"/Data/Observations/Observations",dset_size,field_offsets,dset_sizes,dat);
}

void get_tracking (hid_t loc_id, Tracking* dat){
    herr_t status;          

    size_t field_offsets[4] = {
         HOFFSET(Tracking, timestamp),
         HOFFSET(Tracking, unoc),
         HOFFSET(Tracking, sname),
         HOFFSET(Tracking, slib)
    };
    
    size_t dset_sizes[4] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*31,
                            sizeof(char)*4
    };  
     
    size_t dset_size = sizeof(Tracking);
   
    
    status=H5TBread_table(loc_id,"/SupplementalData/Tracking/Tracking",dset_size,field_offsets,dset_sizes,dat);
}

void get_obstype (hid_t loc_id, ObsType* dat){
    herr_t status;

    size_t field_offsets[5] = {
         HOFFSET(ObsType, timestamp),
         HOFFSET(ObsType, unoc),
         HOFFSET(ObsType, id_type),
         HOFFSET(ObsType, ncep_type),
         HOFFSET(ObsType, ispdbcid)
    };
    
    size_t dset_sizes[5] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),  
                            sizeof(int),
                            sizeof(char)*7
    };
          
    size_t dset_size = sizeof(ObsType);
    
    status=H5TBread_table(loc_id,"/Data/Observations/ObservationTypes",dset_size,field_offsets,dset_sizes,dat);
}

void get_origmetadata (hid_t loc_id, OrigMetaData* dat){
    herr_t status;

    size_t field_offsets[6] = {
         HOFFSET(OrigMetaData, timestamp),
         HOFFSET(OrigMetaData, unoc),
         HOFFSET(OrigMetaData, olat),
         HOFFSET(OrigMetaData, olon),
         HOFFSET(OrigMetaData, oelev),
         HOFFSET(OrigMetaData, uoelev)
    };

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*9,
                            sizeof(char)*9,
                            sizeof(char)*7,
                            sizeof(char)*9
    };

    size_t dset_size = sizeof(OrigMetaData);

    status=H5TBread_table(loc_id,"/Data/SpatialTemporalLocation/OriginalSpatialTemporalLocation/OriginalSpatialTemporalLocation",dset_size,field_offsets,dset_sizes,dat);
}
void get_source (hid_t loc_id, Source* dat){
    herr_t status;

    size_t field_offsets[6] = {
         HOFFSET(Source, timestamp),
         HOFFSET(Source, unoc),
         HOFFSET(Source, sflsd),
         HOFFSET(Source, rtc),
         HOFFSET(Source, qcislp),
         HOFFSET(Source, qcisfp)
    };

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*2,
                            sizeof(char)*6,
                            sizeof(char)*6,
                            sizeof(char)*6
    };

    size_t dset_size = sizeof(Source);

    status=H5TBread_table(loc_id,"/SupplementalData/Tracking/Land/TrackingLand",dset_size,field_offsets,dset_sizes,dat);

}
void get_corrections (hid_t loc_id, Corrections* dat){
    herr_t status;

    size_t field_offsets[11] = {
         HOFFSET(Corrections, timestamp),
         HOFFSET(Corrections, unoc),
         HOFFSET(Corrections, gcs),
         HOFFSET(Corrections, gcispd),
         HOFFSET(Corrections, tk),
         HOFFSET(Corrections, tat),
         HOFFSET(Corrections, utat),
         HOFFSET(Corrections, tcs),
         HOFFSET(Corrections, tcispd),
         HOFFSET(Corrections, hcs),
         HOFFSET(Corrections, hcispd)
    };

    size_t dset_sizes[11] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(int),
                            sizeof(char)*10,
                            sizeof(char)*9,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(char)*31
    };

    size_t dset_size = sizeof(Corrections);

    status=H5TBread_table(loc_id,"/SupplementalData/Corrections/Corrections",dset_size,field_offsets,dset_sizes,dat);

}
void get_presinst (hid_t loc_id, PresInst* dat){
    herr_t status;

    size_t field_offsets[3] = {
         HOFFSET(PresInst, timestamp),
         HOFFSET(PresInst, unoc),
         HOFFSET(PresInst, PressureInst)
    };

    size_t dset_sizes[3] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(unsigned int)
    };

    size_t dset_size = sizeof(PresInst);

    status=H5TBread_table(loc_id,"/SupplementalData/Corrections/PressureInstrument",dset_size,field_offsets,dset_sizes,dat);

}
void get_origobservations (hid_t loc_id, OrigObservations* dat){
    herr_t status;

    size_t field_offsets[6] = {
         HOFFSET(OrigObservations, timestamp),
         HOFFSET(OrigObservations, unoc),
         HOFFSET(OrigObservations, oslp),
         HOFFSET(OrigObservations, uoslp),
         HOFFSET(OrigObservations, osfp),
         HOFFSET(OrigObservations, uosfp)
    };

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*10,
                            sizeof(char)*9,
                            sizeof(char)*10,
                            sizeof(char)*9
    };

    size_t dset_size = sizeof(OrigObservations);

    status=H5TBread_table(loc_id,"/Data/Observations/OriginalObservations/OriginalObservations",dset_size,field_offsets,dset_sizes,dat);
}

void get_srcmarine (hid_t loc_id, SrcMarine* dat){
    herr_t status;

    size_t field_offsets[12] = {
         HOFFSET(SrcMarine, timestamp),
         HOFFSET(SrcMarine, unoc),
         HOFFSET(SrcMarine, sid),
         HOFFSET(SrcMarine, dck),
         HOFFSET(SrcMarine, pt),
         HOFFSET(SrcMarine, orig_hr),
         HOFFSET(SrcMarine, uid),
         HOFFSET(SrcMarine, rn1),
         HOFFSET(SrcMarine, rn2),
         HOFFSET(SrcMarine, rn3),
         HOFFSET(SrcMarine, rsa),
         HOFFSET(SrcMarine, irf)
    };

    size_t dset_sizes[12] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(char)*4,
                            sizeof(char)*6,
                            sizeof(char),
                            sizeof(char),
                            sizeof(char),
                            sizeof(int),
                            sizeof(int)
    };

    size_t dset_size = sizeof(SrcMarine);

    status=H5TBread_table(loc_id,"/SupplementalData/Tracking/ICOADS/TrackingICOADS",dset_size,field_offsets,dset_sizes,dat);
}
void get_miscmarine (hid_t loc_id, MiscMarine* dat){
    herr_t status;

    size_t field_offsets[3] = {
         HOFFSET(MiscMarine, timestamp),
         HOFFSET(MiscMarine, unoc),
         HOFFSET(MiscMarine, PressureBias)
    };

    size_t dset_sizes[3] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(unsigned int)
    };

    size_t dset_size = sizeof(MiscMarine);

    status=H5TBread_table(loc_id,"/SupplementalData/Misc/ICOADS/PressureBias",dset_size,field_offsets,dset_sizes,dat);
}


void get_hurricanes (hid_t loc_id, Hurricanes* dat){
    herr_t status;
    size_t field_offsets[NFIELDS11] = { HOFFSET( Hurricanes, timestamp ),
                                   HOFFSET( Hurricanes, unoc ),
                                   HOFFSET( Hurricanes, dir ),
                                   HOFFSET( Hurricanes, trm ),
                                   HOFFSET( Hurricanes, trmm ),
                                   HOFFSET( Hurricanes, trk ),
                                   HOFFSET( Hurricanes, wm ),
                                   HOFFSET( Hurricanes, wmm ),
                                   HOFFSET( Hurricanes, wk ),
				   HOFFSET( Hurricanes, d2l ),
                                   HOFFSET( Hurricanes, lnd )};
    size_t dset_sizes[11] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),
                            sizeof(int),
                            sizeof(float),
                            sizeof(int),
                            sizeof(int),
                            sizeof(float),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int)
    };

    size_t dset_size = sizeof(Hurricanes);

    status=H5TBread_table(loc_id,"/SupplementalData/Misc/TropicalStorms/TropicalStorms",dset_size,field_offsets,dset_sizes,dat);
}

void get_hurricanevariances (hid_t loc_id, HurricaneVariances* dat){
    herr_t status;
    size_t dset_size =  sizeof( HurricaneVariances );
    size_t field_offsets[NFIELDS5] = { HOFFSET( HurricaneVariances, timestamp ),
                                   HOFFSET( HurricaneVariances, unoc ),
                                   HOFFSET( HurricaneVariances, pe ),
                                   HOFFSET( HurricaneVariances, we ),
                                   HOFFSET( HurricaneVariances, pre )};

    size_t dset_sizes[5] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),
                            sizeof(int),
                            sizeof(int)
    };

    status=H5TBread_table(loc_id,"/SupplementalData/Misc/TropicalStorms/Variances",dset_size,field_offsets,dset_sizes,dat);
}

void get_hurricaneid (hid_t loc_id, HurricaneID* dat){
    herr_t status;
    size_t dset_size =  sizeof( HurricaneID );
    size_t field_offsets[NFIELDS3] = { HOFFSET( HurricaneID, timestamp ),
                                   HOFFSET( HurricaneID, unoc ),
                                   HOFFSET( HurricaneID, storm_id )};

    size_t dset_sizes[3] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*31
    };

    status=H5TBread_table(loc_id,"/SupplementalData/Misc/TropicalStorms/StormID",dset_size,field_offsets,dset_sizes,dat);
}

void get_hurricaneqc (hid_t loc_id, HurricaneQC* dat){
    herr_t status;
    size_t dset_size =  sizeof( HurricaneQC );
    size_t field_offsets[NFIELDS4] = { HOFFSET( HurricaneQC, timestamp ),
                                   HOFFSET( HurricaneQC, unoc ),
                                   HOFFSET( HurricaneQC, wq ),
                                   HOFFSET( HurricaneQC, pq )};

    size_t dset_sizes[4] = { sizeof(char)*13,
                            sizeof(char)*8,
			    sizeof(int),
			    sizeof(int)
    };

    status=H5TBread_table(loc_id,"/SupplementalData/Misc/TropicalStorms/QualityControl",dset_size,field_offsets,dset_sizes,dat);
}


// -----------------------------------------------------------------------------------------------------
//

// append_ series appends data to each table
herr_t append_miscmar(hid_t loc_id, MiscMarine* dat ) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( MiscMarine );
    size_t dst_offset[NFIELDS3] = { HOFFSET( MiscMarine, timestamp ),
                                   HOFFSET( MiscMarine, unoc ),
                                   HOFFSET( MiscMarine, PressureBias )};

    size_t dset_sizes[3] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(unsigned int)
    };
    herr_t     status;
 //   size_t nfields_out, total;
 //   nfields_out = total = 0;

    //status=H5TBget_table_info (loc_id,"/SupplementalData/Misc/ICOADS/PressureBias", &nfields_out, &total);
//    status=H5TBget_table_info (loc_id,"/SupplementalData/Misc/ICOADS/PressureBias", &nfields_out, &total);

    status = H5TBappend_records(loc_id,"/SupplementalData/Misc/ICOADS/PressureBias",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);

    return status;
}

herr_t append_srcmar(hid_t loc_id, SrcMarine* dat) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( SrcMarine );
    size_t dst_offset[NFIELDS12] = { HOFFSET( SrcMarine, timestamp ),
	                           HOFFSET( SrcMarine, unoc ), 
	                           HOFFSET( SrcMarine, sid ), 
	                           HOFFSET( SrcMarine, dck ), 
                                   HOFFSET( SrcMarine, pt ),
				   HOFFSET( SrcMarine, orig_hr),
         			   HOFFSET( SrcMarine, uid),
         			   HOFFSET( SrcMarine, rn1),
         			   HOFFSET( SrcMarine, rn2),
         			   HOFFSET( SrcMarine, rn3),
         			   HOFFSET( SrcMarine, rsa),
         			   HOFFSET( SrcMarine, irf)};

    size_t dset_sizes[12] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(char)*4,
                            sizeof(char)*6,
                            sizeof(char),
                            sizeof(char),
                            sizeof(char),
                            sizeof(int),
                            sizeof(int)
    };
    herr_t status;

    status = H5TBappend_records(loc_id,"/SupplementalData/Tracking/ICOADS/TrackingICOADS",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);

    return status;
}

herr_t append_origobs (hid_t loc_id, OrigObservations* dat){
    herr_t status;
    //hsize_t nfields_out, total;

    size_t dst_size = sizeof(OrigObservations);
    size_t dst_offsets[NFIELDS6] = {
         HOFFSET(OrigObservations, timestamp),
         HOFFSET(OrigObservations, unoc),
         HOFFSET(OrigObservations, oslp),
         HOFFSET(OrigObservations, uoslp),
         HOFFSET(OrigObservations, osfp),
         HOFFSET(OrigObservations, uosfp)
    };

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*10,
                            sizeof(char)*9,
                            sizeof(char)*10,
                            sizeof(char)*9
    };


    status = H5TBappend_records(loc_id,"/Data/Observations/OriginalObservations/OriginalObservations",(hsize_t)1,dst_size,dst_offsets,dset_sizes,dat);

    return status;
}

herr_t append_obs(hid_t loc_id, Observations* dat) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( Observations );
    size_t dst_offset[NFIELDS8] = { HOFFSET( Observations,timestamp ),
                                   HOFFSET( Observations, unoc  ),
                                   HOFFSET( Observations, slp ),
                                   HOFFSET( Observations, slpe ),
                                   HOFFSET( Observations, slpqc ),
                                   HOFFSET( Observations, sfp ),
                                   HOFFSET( Observations, sfpe ),
                                   HOFFSET( Observations, sfpqc )};
    size_t dset_sizes[8] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(float),  
                            sizeof(float),  
                            sizeof(int),  
                            sizeof(float),  
                            sizeof(float),  
                            sizeof(int)
    };
    herr_t     status;

    status = H5TBappend_records(loc_id,"/Data/Observations/Observations",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);

    return status;
}

herr_t append_corrections(hid_t loc_id, Corrections* dat){
    herr_t status;
    size_t dst_size =  sizeof( Corrections );
    size_t dst_offset[NFIELDS11] = { HOFFSET( Corrections, timestamp ),
                                   HOFFSET( Corrections, unoc ),
                                   HOFFSET( Corrections, gcs ),
                                   HOFFSET( Corrections, gcispd ),
                                   HOFFSET( Corrections, tk ),
                                   HOFFSET( Corrections, tat ),
                                   HOFFSET( Corrections, utat ),
                                   HOFFSET( Corrections, tcs ),
                                   HOFFSET( Corrections, tcispd ),
                                   HOFFSET( Corrections, hcs ),
                                   HOFFSET( Corrections, hcispd )};
    size_t dset_sizes[11] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(int),
                            sizeof(char)*10,
                            sizeof(char)*9,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(char)*31
    };
    //status=H5TBget_table_info (loc_id,"/SupplementalData/Corrections/Corrections", &nfields_out, &total);
    status = H5TBappend_records(loc_id,"/SupplementalData/Corrections/Corrections",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);

    return status;
}

herr_t append_newfeedback(hid_t loc_id, NewFeedBack* dat){
    herr_t status;
    size_t dst_size =  sizeof( NewFeedBack );
    size_t dst_offset[NFIELDS18] = { HOFFSET( NewFeedBack, timestamp ),
                                   HOFFSET( NewFeedBack, unoc ),
                                   HOFFSET( NewFeedBack, mdpavims ),
                                   HOFFSET( NewFeedBack, epvims ),
                                   HOFFSET( NewFeedBack, bias ),
                                   HOFFSET( NewFeedBack, sfsfp ),
                                   HOFFSET( NewFeedBack, ai ),
                                   HOFFSET( NewFeedBack, uc ),
                                   HOFFSET( NewFeedBack, bcf ),
                                   HOFFSET( NewFeedBack, bf ),
                                   HOFFSET( NewFeedBack, qc ),
                                   HOFFSET( NewFeedBack, emfg ),
                                   HOFFSET( NewFeedBack, sdeg ),
                                   HOFFSET( NewFeedBack, mpmemfg ),
                                   HOFFSET( NewFeedBack, emap ),
                                   HOFFSET( NewFeedBack, sdeap ),
                                   HOFFSET( NewFeedBack, mpmema ),
                                   HOFFSET( NewFeedBack, melv ) };
    size_t dset_sizes[18] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(double),
                            sizeof(float),
                            sizeof(double),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(int) };
    //status=H5TBget_table_info (loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack", &nfields_out, &total);

    status = H5TBappend_records(loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);
    return status;
}

herr_t append_feedback(hid_t loc_id, FeedBack* dat){
    herr_t status;
    size_t dst_size =  sizeof( FeedBack );
    size_t dst_offset[NFIELDS15] = { HOFFSET( FeedBack, timestamp ),
                                   HOFFSET( FeedBack, unoc ),
                                   HOFFSET( FeedBack, mdpavims ),
                                   HOFFSET( FeedBack, epvims ),
                                   HOFFSET( FeedBack, sfsfp ),
                                   HOFFSET( FeedBack, dc ),
                                   HOFFSET( FeedBack, ai ),
                                   HOFFSET( FeedBack, bcf ),
                                   HOFFSET( FeedBack, bf ),
                                   HOFFSET( FeedBack, emfg ),
                                   HOFFSET( FeedBack, sdeg ),
                                   HOFFSET( FeedBack, mpmemfg ),
                                   HOFFSET( FeedBack, emap ),
                                   HOFFSET( FeedBack, sdeap ),
                                   HOFFSET( FeedBack, mpmema ) };
    size_t dset_sizes[15] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(double),
                            sizeof(float),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double) };
    //status=H5TBget_table_info (loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack", &nfields_out, &total);

    status = H5TBappend_records(loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);
    return status;
}

herr_t append_metadata(hid_t loc_id, MetaData* dat){
    herr_t status;
    size_t dst_size =  sizeof( MetaData );
    size_t dst_offset[NFIELDS16] = { HOFFSET( MetaData, timestamp ),
	                           HOFFSET( MetaData, unoc ),
                                   HOFFSET( MetaData, id ),
                                   HOFFSET( MetaData, year ),
                                   HOFFSET( MetaData, month ),
                                   HOFFSET( MetaData, day ),
                                   HOFFSET( MetaData, hour ),
                                   HOFFSET( MetaData, minute ),
                                   HOFFSET( MetaData, second ),
                                   HOFFSET( MetaData, time_in_sec ),
                                   HOFFSET( MetaData, time_code ),
                                   HOFFSET( MetaData, n_obs ),
                                   HOFFSET( MetaData, h_gmt ),
                                   HOFFSET( MetaData, lat ),
                                   HOFFSET( MetaData, lon ),
                                   HOFFSET( MetaData, elv )};

    size_t dset_sizes[16] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*14,
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(char)*4,
                            sizeof(int),
                            sizeof(char)*200,
                            sizeof(float),
                            sizeof(float),
                            sizeof(int)
    };
    // status=H5TBget_table_info (loc_id,"/Data/SpatialTemporalLocation/SpatialTemporalLocation", &nfields_out, &total);
          
    status = H5TBappend_records(loc_id,"/Data/SpatialTemporalLocation/SpatialTemporalLocation",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);

    return status;

}
herr_t append_obstype(hid_t loc_id, ObsType* dat){
    herr_t status;
    size_t dst_size =  sizeof( ObsType );
    size_t dst_offset[NFIELDS5] = { HOFFSET( ObsType, timestamp ),
                                   HOFFSET( ObsType, unoc ),
                                   HOFFSET( ObsType, id_type ),
                                   HOFFSET( ObsType, ncep_type ),
                                   HOFFSET( ObsType, ispdbcid )};
    size_t dset_sizes[5] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),  
                            sizeof(int),
                            sizeof(char)*7
    };
    //status=H5TBget_table_info (loc_id,"/Data/Observations/ObservationTypes", &nfields_out, &total);
    status = H5TBappend_records(loc_id,"/Data/Observations/ObservationTypes",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);

    return status;

}
herr_t append_src(hid_t loc_id, Source* dat){
    herr_t status;
    size_t dst_size =  sizeof( Source );
    size_t dst_offset[NFIELDS6] = { HOFFSET( Source, timestamp ),
                                   HOFFSET( Source, unoc ),
                                   HOFFSET( Source, sflsd ),
                                   HOFFSET( Source, rtc ),
                                   HOFFSET( Source, qcislp ),
                                   HOFFSET( Source, qcisfp )};

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*2,
                            sizeof(char)*6,
                            sizeof(char)*6,
                            sizeof(char)*6
    };
    //status=H5TBget_table_info (loc_id,"/SupplementalData/Tracking/Land/TrackingLand", &nfields_out, &total);
          
    status = H5TBappend_records(loc_id,"/SupplementalData/Tracking/Land/TrackingLand",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);

    return status;

}
herr_t append_tracking(hid_t loc_id, Tracking* dat){
    herr_t status;
    size_t dst_size =  sizeof( Tracking );
    size_t dst_offset[NFIELDS4] = { HOFFSET( Tracking, timestamp ),
                                   HOFFSET( Tracking, unoc ),
                                   HOFFSET( Tracking, sname ),
                                   HOFFSET( Tracking, slib )};
    size_t dset_sizes[4] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*31,
                            sizeof(char)*4
    };
    //status=H5TBget_table_info (loc_id,"/SupplementalData/Tracking/Tracking", &nfields_out, &total);

    status = H5TBappend_records(loc_id,"/SupplementalData/Tracking/Tracking",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);

    return status;

}
herr_t append_presinst(hid_t loc_id, PresInst* dat){
    herr_t status;
    size_t dst_size =  sizeof( PresInst );
    size_t dst_offset[NFIELDS3] = { HOFFSET( PresInst, timestamp ),
                                   HOFFSET( PresInst, unoc ),
                                   HOFFSET( PresInst, PressureInst )};
    size_t dset_sizes[3] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(unsigned int)
    };
    //status=H5TBget_table_info (loc_id,"/SupplementalData/Corrections/PressureInstrument", &nfields_out, &total);

    status = H5TBappend_records(loc_id,"/SupplementalData/Corrections/PressureInstrument",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);
    return status;
}
herr_t append_origmetadata(hid_t loc_id, OrigMetaData* dat){
    herr_t status;
    size_t dst_size =  sizeof( OrigMetaData );
    size_t dst_offset[NFIELDS6] = { HOFFSET( OrigMetaData, timestamp ),
	                           HOFFSET( OrigMetaData, unoc ),
	                           HOFFSET( OrigMetaData, olat ),
	                           HOFFSET( OrigMetaData, olon ),
	                           HOFFSET( OrigMetaData, oelev ),
	                           HOFFSET( OrigMetaData, uoelev )};
    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*9,
                            sizeof(char)*9,
                            sizeof(char)*7,
                            sizeof(char)*9
    };

    //status=H5TBget_table_info (loc_id,"/Data/SpatialTemporalLocation/OriginalSpatialTemporalLocation/OriginalSpatialTemporalLocation", &nfields_out, &total);
          
    status = H5TBappend_records(loc_id,"/Data/SpatialTemporalLocation/OriginalSpatialTemporalLocation/OriginalSpatialTemporalLocation",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);

    return status;
}

herr_t append_hurricanes(hid_t loc_id, Hurricanes* dat){
    herr_t status;
    size_t dst_size =  sizeof( Hurricanes );
    size_t dst_offset[NFIELDS11] = { HOFFSET( Hurricanes, timestamp ),
                                   HOFFSET( Hurricanes, unoc ),
                                   HOFFSET( Hurricanes, dir ),
                                   HOFFSET( Hurricanes, trm ),
                                   HOFFSET( Hurricanes, trmm ),
                                   HOFFSET( Hurricanes, trk ),
                                   HOFFSET( Hurricanes, wm ),
                                   HOFFSET( Hurricanes, wmm ),
                                   HOFFSET( Hurricanes, wk ),
                                   HOFFSET( Hurricanes, d2l ),
                                   HOFFSET( Hurricanes, lnd )};
    size_t dset_sizes[11] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),
                            sizeof(int),
                            sizeof(float),
                            sizeof(int),
                            sizeof(int),
                            sizeof(float),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int)
    };

    status = H5TBappend_records(loc_id,"/SupplementalData/Misc/TropicalStorms/TropicalStorms",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);
    return status;
}

herr_t append_hurricaneqc(hid_t loc_id, HurricaneQC* dat){
    herr_t status;
    size_t dst_size =  sizeof( HurricaneQC );
    size_t dst_offset[NFIELDS4] = { HOFFSET( HurricaneQC, timestamp ),
                                   HOFFSET( HurricaneQC, unoc ),
                                   HOFFSET( HurricaneQC, wq ),
                                   HOFFSET( HurricaneQC, pq )};
    size_t dset_sizes[4] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),
                            sizeof(int)
    };

    status = H5TBappend_records(loc_id,"/SupplementalData/Misc/TropicalStorms/QualityControl",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);
    return status;
}
herr_t append_hurricaneid(hid_t loc_id, HurricaneID* dat){
    herr_t status;
    size_t dst_size =  sizeof( HurricaneID );
    size_t dst_offset[NFIELDS3] = { HOFFSET( HurricaneID, timestamp ),
                                   HOFFSET( HurricaneID, unoc ),
                                   HOFFSET( HurricaneID, storm_id )};
    size_t dset_sizes[3] = { sizeof(char)*13,
                            sizeof(char)*8,
			    sizeof(char)*31
    };

    status = H5TBappend_records(loc_id,"/SupplementalData/Misc/TropicalStorms/StormID",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);
    return status;
}

herr_t append_hurricanevariances(hid_t loc_id, HurricaneVariances* dat){
    herr_t status;
    size_t dst_size =  sizeof( HurricaneVariances );
    size_t dst_offset[NFIELDS5] = { HOFFSET( HurricaneVariances, timestamp ),
                                   HOFFSET( HurricaneVariances, unoc ),
                                   HOFFSET( HurricaneVariances, pe ),
                                   HOFFSET( HurricaneVariances, we ),
                                   HOFFSET( HurricaneVariances, pre )};
    size_t dset_sizes[5] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),
                            sizeof(int),
                            sizeof(int)
    };

    status = H5TBappend_records(loc_id,"/SupplementalData/Misc/TropicalStorms/Variances",(hsize_t)1,dst_size,dst_offset,dset_sizes,dat);
    return status;
}

// insert_series add data at the begining of each table
herr_t insert_miscmar(hid_t loc_id, MiscMarine* dat ) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( MiscMarine );
    size_t dst_offset[NFIELDS3] = { HOFFSET( MiscMarine, timestamp ),
                                   HOFFSET( MiscMarine, unoc ),
                                   HOFFSET( MiscMarine, PressureBias )};

    size_t dset_sizes[3] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(unsigned int)
    };
    herr_t     status;
    //size_t nfields_out, total;
    //nfields_out = total = 0;

    //status=H5TBget_table_info (loc_id,"/SupplementalData/Misc/ICOADS/PressureBias", &nfields_out, &total);
    //status=H5TBget_table_info (loc_id,"/SupplementalData/Misc/ICOADS/PressureBias", &nfields_out, &total);
    status = H5TBinsert_record(loc_id,"/SupplementalData/Misc/ICOADS/PressureBias",0,1,dst_size,dst_offset,dset_sizes,dat);

    return status;
}

herr_t insert_srcmar(hid_t loc_id, SrcMarine* dat) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( SrcMarine );
    size_t dst_offset[NFIELDS12] = { HOFFSET( SrcMarine, timestamp ),
	                           HOFFSET( SrcMarine, unoc ), 
	                           HOFFSET( SrcMarine, sid ), 
	                           HOFFSET( SrcMarine, dck ), 
                                   HOFFSET( SrcMarine, pt ),
                                   HOFFSET( SrcMarine, orig_hr),
                                   HOFFSET( SrcMarine, uid),
                                   HOFFSET( SrcMarine, rn1),
                                   HOFFSET( SrcMarine, rn2),
                                   HOFFSET( SrcMarine, rn3),
                                   HOFFSET( SrcMarine, rsa),
                                   HOFFSET( SrcMarine, irf)};

    size_t dset_sizes[12] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(char)*4,
                            sizeof(char)*6,
                            sizeof(char),
                            sizeof(char),
                            sizeof(char),
                            sizeof(int),
                            sizeof(int)
    };
    herr_t status;

    status = H5TBinsert_record(loc_id,"/SupplementalData/Tracking/ICOADS/TrackingICOADS",0,1,dst_size,dst_offset,dset_sizes,dat);

    return status;
}

herr_t insert_origobs (hid_t loc_id, OrigObservations* dat){
    herr_t status;
    //hsize_t nfields_out, total;

    size_t dst_offsets[6] = {
         HOFFSET(OrigObservations, timestamp),
         HOFFSET(OrigObservations, unoc),
         HOFFSET(OrigObservations, oslp),
         HOFFSET(OrigObservations, uoslp),
         HOFFSET(OrigObservations, osfp),
         HOFFSET(OrigObservations, uosfp)
    };

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*10,
                            sizeof(char)*9,
                            sizeof(char)*10,
                            sizeof(char)*9
    };
          
    size_t dst_size = sizeof(OrigObservations);

    /* Get table info  */
//    status=H5TBget_table_info (loc_id,"/Data/Observations/OriginalObservations/OriginalObservations", &nfields_out, &total);


    status = H5TBinsert_record(loc_id,"/Data/Observations/OriginalObservations/OriginalObservations",0,1,dst_size,dst_offsets,dset_sizes,dat);

    return status;
}

herr_t insert_obs(hid_t loc_id, Observations* dat) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( Observations );
    size_t dst_offset[NFIELDS8] = { HOFFSET( Observations,timestamp ),
                                   HOFFSET( Observations, unoc  ),
                                   HOFFSET( Observations, slp ),
                                   HOFFSET( Observations, slpe ),
                                   HOFFSET( Observations, slpqc ),
                                   HOFFSET( Observations, sfp ),
                                   HOFFSET( Observations, sfpe ),
                                   HOFFSET( Observations, sfpqc )};
    size_t dset_sizes[8] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(float),  
                            sizeof(float),  
                            sizeof(int),  
                            sizeof(float),  
                            sizeof(float),  
                            sizeof(int)
    };
    herr_t     status;

    status = H5TBinsert_record(loc_id,"/Data/Observations/Observations",0,1,dst_size,dst_offset,dset_sizes,dat);

    return status;
}

herr_t insert_corrections(hid_t loc_id, Corrections* dat){
    herr_t status;
    size_t dst_size =  sizeof( Corrections );
    size_t dst_offset[NFIELDS11] = { HOFFSET( Corrections, timestamp ),
                                   HOFFSET( Corrections, unoc ),
                                   HOFFSET( Corrections, gcs ),
                                   HOFFSET( Corrections, gcispd ),
                                   HOFFSET( Corrections, tk ),
                                   HOFFSET( Corrections, tat ),
                                   HOFFSET( Corrections, utat ),
                                   HOFFSET( Corrections, tcs ),
                                   HOFFSET( Corrections, tcispd ),
                                   HOFFSET( Corrections, hcs ),
                                   HOFFSET( Corrections, hcispd )};
    size_t dset_sizes[11] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(int),
                            sizeof(char)*10,
                            sizeof(char)*9,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(char)*31,
                            sizeof(char)*31
    };
    //status=H5TBget_table_info (loc_id,"/SupplementalData/Corrections/Corrections", &nfields_out, &total);
    status = H5TBinsert_record(loc_id,"/SupplementalData/Corrections/Corrections",0,1,dst_size,dst_offset,dset_sizes,dat);

    return status;
}

herr_t insert_newfeedback(hid_t loc_id, NewFeedBack* dat){
    herr_t status;
    size_t dst_size =  sizeof( NewFeedBack );
    size_t dst_offset[NFIELDS18] = { HOFFSET( NewFeedBack, timestamp ),
                                   HOFFSET( NewFeedBack, unoc ),
                                   HOFFSET( NewFeedBack, mdpavims ),
                                   HOFFSET( NewFeedBack, epvims ),
                                   HOFFSET( NewFeedBack, bias ),
                                   HOFFSET( NewFeedBack, sfsfp ),
                                   HOFFSET( NewFeedBack, ai ),
                                   HOFFSET( NewFeedBack, uc ),
                                   HOFFSET( NewFeedBack, bcf ),
                                   HOFFSET( NewFeedBack, bf ),
                                   HOFFSET( NewFeedBack, qc ),
                                   HOFFSET( NewFeedBack, emfg ),
                                   HOFFSET( NewFeedBack, sdeg ),
                                   HOFFSET( NewFeedBack, mpmemfg ),
                                   HOFFSET( NewFeedBack, emap ),
                                   HOFFSET( NewFeedBack, sdeap ),
                                   HOFFSET( NewFeedBack, mpmema ),
                                   HOFFSET( NewFeedBack, melv ) };
    size_t dset_sizes[18] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(double),
                            sizeof(float),
                            sizeof(double),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(int) };
    //status=H5TBget_table_info (loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack", &nfields_out, &total);

    status = H5TBinsert_record(loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack",0,1,dst_size,dst_offset,dset_sizes,dat);
    return status;
}

herr_t insert_feedback(hid_t loc_id, FeedBack* dat){
    herr_t status;
    size_t dst_size =  sizeof( FeedBack );
    size_t dst_offset[NFIELDS15] = { HOFFSET( FeedBack, timestamp ),
                                   HOFFSET( FeedBack, unoc ),
                                   HOFFSET( FeedBack, mdpavims ),
                                   HOFFSET( FeedBack, epvims ),
                                   HOFFSET( FeedBack, sfsfp ),
                                   HOFFSET( FeedBack, dc ),
                                   HOFFSET( FeedBack, ai ),
                                   HOFFSET( FeedBack, bcf ),
                                   HOFFSET( FeedBack, bf ),
                                   HOFFSET( FeedBack, emfg ),
                                   HOFFSET( FeedBack, sdeg ),
                                   HOFFSET( FeedBack, mpmemfg ),
                                   HOFFSET( FeedBack, emap ),
                                   HOFFSET( FeedBack, sdeap ),
                                   HOFFSET( FeedBack, mpmema ) };
    size_t dset_sizes[15] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(double),
                            sizeof(float),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(unsigned int),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double),
                            sizeof(double) };
    //status=H5TBget_table_info (loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack", &nfields_out, &total);

    status = H5TBinsert_record(loc_id,"/Data/AssimilationFeedback/AssimilationFeedBack",0,1,dst_size,dst_offset,dset_sizes,dat);
    return status;
}

herr_t insert_metadata(hid_t loc_id, MetaData* dat){
    herr_t status;
    size_t dst_size =  sizeof( MetaData );
    size_t dst_offset[NFIELDS16] = { HOFFSET( MetaData, timestamp ),
	                           HOFFSET( MetaData, unoc ),
                                   HOFFSET( MetaData, id ),
                                   HOFFSET( MetaData, year ),
                                   HOFFSET( MetaData, month ),
                                   HOFFSET( MetaData, day ),
                                   HOFFSET( MetaData, hour ),
                                   HOFFSET( MetaData, minute ),
                                   HOFFSET( MetaData, second ),
                                   HOFFSET( MetaData, time_in_sec ),
                                   HOFFSET( MetaData, time_code ),
                                   HOFFSET( MetaData, n_obs ),
                                   HOFFSET( MetaData, h_gmt ),
                                   HOFFSET( MetaData, lat ),
                                   HOFFSET( MetaData, lon ),
                                   HOFFSET( MetaData, elv )};

    size_t dset_sizes[16] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*14,
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(int),
                            sizeof(char)*4,
                            sizeof(int),
                            sizeof(char)*200,
                            sizeof(float),
                            sizeof(float),
                            sizeof(int)
    };
    // status=H5TBget_table_info (loc_id,"/Data/SpatialTemporalLocation/SpatialTemporalLocation", &nfields_out, &total);
          
    status = H5TBinsert_record(loc_id,"/Data/SpatialTemporalLocation/SpatialTemporalLocation",0,1,dst_size,dst_offset,dset_sizes,dat);

    return status;

}
herr_t insert_obstype(hid_t loc_id, ObsType* dat){
    herr_t status;
    size_t dst_size =  sizeof( ObsType );
    size_t dst_offset[NFIELDS5] = { HOFFSET( ObsType, timestamp ),
                                   HOFFSET( ObsType, unoc ),
                                   HOFFSET( ObsType, id_type ),
                                   HOFFSET( ObsType, ncep_type ),
                                   HOFFSET( ObsType, ispdbcid )};
    size_t dset_sizes[5] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(int),  
                            sizeof(int),
                            sizeof(char)*7
    };
    //status=H5TBget_table_info (loc_id,"/Data/Observations/ObservationTypes", &nfields_out, &total);
    status = H5TBinsert_record(loc_id,"/Data/Observations/ObservationTypes",0,1,dst_size,dst_offset,dset_sizes,dat);

    return status;

}
herr_t insert_src(hid_t loc_id, Source* dat){
    herr_t status;
    size_t dst_size =  sizeof( Source );
    size_t dst_offset[NFIELDS6] = { HOFFSET( Source, timestamp ),
                                   HOFFSET( Source, unoc ),
                                   HOFFSET( Source, sflsd ),
                                   HOFFSET( Source, rtc ),
                                   HOFFSET( Source, qcislp ),
                                   HOFFSET( Source, qcisfp )};

    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*2,
                            sizeof(char)*6,
                            sizeof(char)*6,
                            sizeof(char)*6
    };
    //status=H5TBget_table_info (loc_id,"/SupplementalData/Tracking/Land/TrackingLand", &nfields_out, &total);
          
    status = H5TBinsert_record(loc_id,"/SupplementalData/Tracking/Land/TrackingLand",0,1,dst_size,dst_offset,dset_sizes,dat);

    return status;

}
herr_t insert_tracking(hid_t loc_id, Tracking* dat){
    herr_t status;
    size_t dst_size =  sizeof( Tracking );
    size_t dst_offset[NFIELDS4] = { HOFFSET( Tracking, timestamp ),
                                   HOFFSET( Tracking, unoc ),
                                   HOFFSET( Tracking, sname ),
                                   HOFFSET( Tracking, slib )};
    size_t dset_sizes[4] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*31,
                            sizeof(char)*4
    };
    //status=H5TBget_table_info (loc_id,"/SupplementalData/Tracking/Tracking", &nfields_out, &total);

    status = H5TBinsert_record(loc_id,"/SupplementalData/Tracking/Tracking",0,1,dst_size,dst_offset,dset_sizes,dat);

    return status;

}
herr_t insert_presinst(hid_t loc_id, PresInst* dat){
    herr_t status;
    size_t dst_size =  sizeof( PresInst );
    size_t dst_offset[NFIELDS3] = { HOFFSET( PresInst, timestamp ),
                                   HOFFSET( PresInst, unoc ),
                                   HOFFSET( PresInst, PressureInst )};
    size_t dset_sizes[3] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(unsigned int)
    };
    //status=H5TBget_table_info (loc_id,"/SupplementalData/Corrections/PressureInstrument", &nfields_out, &total);

    status = H5TBinsert_record(loc_id,"/SupplementalData/Corrections/PressureInstrument",0,1,dst_size,dst_offset,dset_sizes,dat);
    return status;
}
herr_t insert_origmetadata(hid_t loc_id, OrigMetaData* dat){
    herr_t status;
    size_t dst_size =  sizeof( OrigMetaData );
    size_t dst_offset[NFIELDS6] = { HOFFSET( OrigMetaData, timestamp ),
	                           HOFFSET( OrigMetaData, unoc ),
	                           HOFFSET( OrigMetaData, olat ),
	                           HOFFSET( OrigMetaData, olon ),
	                           HOFFSET( OrigMetaData, oelev ),
	                           HOFFSET( OrigMetaData, uoelev )};
    size_t dset_sizes[6] = { sizeof(char)*13,
                            sizeof(char)*8,
                            sizeof(char)*9,
                            sizeof(char)*9,
                            sizeof(char)*7,
                            sizeof(char)*9
    };

    //status=H5TBget_table_info (loc_id,"/Data/SpatialTemporalLocation/OriginalSpatialTemporalLocation/OriginalSpatialTemporalLocation", &nfields_out, &total);
          
    status = H5TBinsert_record(loc_id,"/Data/SpatialTemporalLocation/OriginalSpatialTemporalLocation/OriginalSpatialTemporalLocation",0,1,dst_size,dst_offset,dset_sizes,dat);

    return status;
}

// make_ _table series creates each table with data
herr_t make_obs_table(hid_t loc_id, hsize_t nrecs, Observations dat[]) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( Observations );
    size_t dst_offset[NFIELDS8] = { HOFFSET( Observations,timestamp ),
                                   HOFFSET( Observations, unoc  ),
                                   HOFFSET( Observations, slp ),
                                   HOFFSET( Observations, slpe ),
                                   HOFFSET( Observations, slpqc ),
                                   HOFFSET( Observations, sfp ),
                                   HOFFSET( Observations, sfpe ),
                                   HOFFSET( Observations, sfpqc )};

    /* Define field information */
    const char *field_names[NFIELDS8]  =
        { "Timestamp", "Unique Observation Number","Observed Sea Level Pressure","Observation Error in the Observed Sea Level Pressure","QC flag for the Observed Sea Level Pressure","Observed Surface Pressure","Observation Error in the Observed Surface Pressure","QC flag for the Observed Surface Pressure" };

    hid_t      field_type[NFIELDS8];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status;
    hid_t      string_type8, string_type13;

    //EXAMPLE("make a table Observations\n");

    /* Initialize the field field_type */
    string_type8 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type8, 8 );
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type13, 13 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = H5T_NATIVE_FLOAT;
    field_type[3] = H5T_NATIVE_FLOAT;
    field_type[4] = H5T_NATIVE_INT;
    field_type[5] = H5T_NATIVE_FLOAT;
    field_type[6] = H5T_NATIVE_FLOAT;
    field_type[7] = H5T_NATIVE_INT;

    status=H5TBmake_table( "Observations", loc_id, "Observations",(hsize_t) NFIELDS8, (hsize_t)nrecs, dst_size,
                            field_names, dst_offset, field_type,
                            chunk_size, fill_data, compress, dat  );
    status = H5LTset_attribute_string(loc_id,"Observations","SLP Unit","hPa");
    status = H5LTset_attribute_string(loc_id,"Observations","STP Unit","hPa");

    return status;
}
 
herr_t make_corrections_table(hid_t loc_id, hsize_t nrecs, Corrections dat[]) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( Corrections );
    size_t dst_offset[NFIELDS11] = { HOFFSET( Corrections, timestamp ),
                                   HOFFSET( Corrections, unoc ),
                                   HOFFSET( Corrections, gcs ),
                                   HOFFSET( Corrections, gcispd ),
                                   HOFFSET( Corrections, tk ),
                                   HOFFSET( Corrections, tat ),
                                   HOFFSET( Corrections, utat ),
                                   HOFFSET( Corrections, tcs ),
                                   HOFFSET( Corrections, tcispd ),
                                   HOFFSET( Corrections, hcs ),
                                   HOFFSET( Corrections, hcispd )};

    const char *field_names[NFIELDS11]  = 
        { "Timestamp", "Unique Observation Number", "Description of Gravity Correction made by source","Description of Gravity correction made by ISPD","Observed Temperature of the thermometer in K","Original Temperature of the attached thermometer","Units of the Original Temperature of the attached thermometer","Temperature correction made by source","Temperature correction made by ISPD","Homogenization correction made by source","Homogenization correction made by ISPD" };

    hid_t      field_type[NFIELDS11];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status; 
    hid_t      string_type9, string_type31;
    hid_t      string_type10,string_type8,string_type13;

    //EXAMPLE("make a table Corrections\n");

    /* Initialize the field field_type */
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type13, 13 );
    string_type8 = H5Tcopy( H5T_C_S1 );
    string_type9 = H5Tcopy( H5T_C_S1 );
    string_type10 = H5Tcopy( H5T_C_S1 );
    string_type31 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type9, 9 );
    H5Tset_size( string_type10, 10 );
    H5Tset_size( string_type31, 31 );
    H5Tset_size( string_type8, 8 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = string_type31;
    field_type[3] = string_type31;
    field_type[4] = H5T_NATIVE_INT;
    field_type[5] = string_type10;
    field_type[6] = string_type9;
    field_type[7] = string_type31;
    field_type[8] = string_type31;
    field_type[9] = string_type31;
    field_type[10] = string_type31;
					        
    status=H5TBmake_table( "Corrections", loc_id, "Corrections",(hsize_t) NFIELDS11, (hsize_t)nrecs, dst_size, 
                            field_names, dst_offset, field_type, 
                            chunk_size, fill_data, compress, dat  );

    return status;
}

herr_t make_newfeedback_table(hid_t loc_id, hsize_t nrecs, NewFeedBack dat[]) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( NewFeedBack );
    size_t dst_offset[NFIELDS18] = { HOFFSET( NewFeedBack, timestamp ),
                                   HOFFSET( NewFeedBack, unoc ),
                                   HOFFSET( NewFeedBack, mdpavims ),
                                   HOFFSET( NewFeedBack, epvims ),
                                   HOFFSET( NewFeedBack, bias ),
                                   HOFFSET( NewFeedBack, sfsfp ),
                                   HOFFSET( NewFeedBack, ai ),
                                   HOFFSET( NewFeedBack, uc ),
                                   HOFFSET( NewFeedBack, bcf ),
                                   HOFFSET( NewFeedBack, bf ),
                                   HOFFSET( NewFeedBack, qc ),
                                   HOFFSET( NewFeedBack, emfg ),
                                   HOFFSET( NewFeedBack, sdeg ),
                                   HOFFSET( NewFeedBack, mpmemfg ),
                                   HOFFSET( NewFeedBack, emap ),
                                   HOFFSET( NewFeedBack, sdeap ),
                                   HOFFSET( NewFeedBack, mpmema ),
                                   HOFFSET( NewFeedBack, melv ) };

    /* Define field information */
    const char *field_names[NFIELDS18]  = 
        { "Timestamp","Unique Observation Number","Modified Observed Pressure after Vertically Interpolating to Model Surface","Error in Observed Pressure Vertically Interpolated to Model Surface","Bias", "Status Flag for Observed Surface Pressure", "Assimilation indicator", "Usability Check for Reanalysis", "QC Background check flag indicator", "Buddy flag indicator", "Quality Control Indicator","Ensemble Mean First Guess Pressure","Standard Deviation of Ensemble Guess Pressure","Modified Observation Pressure minus Ensemble Mean First Guess Pressure","Ensemble Mean Analysis Pressure","Standard Deviation of Ensemble Analysis Pressure","Modified Observation Pressure minus Ensemble Mean Analysis Pressure", "Modified Elevation Vertically Interpolated to Model Surface" };

    hid_t      field_type[NFIELDS18];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status; 
    hid_t      string_type8,string_type13;

    //EXAMPLE("make a table AssimilationFeedback\n");

    /* Initialize the field field_type */
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type13, 13 );
    string_type8 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type8, 8 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = H5T_NATIVE_DOUBLE;
    field_type[3] = H5T_NATIVE_FLOAT;
    field_type[4] = H5T_NATIVE_DOUBLE;
    field_type[5] = H5T_NATIVE_INT;
    field_type[6] = H5T_NATIVE_INT;
    field_type[7] = H5T_NATIVE_INT;
    field_type[8] = H5T_NATIVE_INT;
    field_type[9] = H5T_NATIVE_INT;
    field_type[10] = H5T_NATIVE_INT;
    field_type[11] = H5T_NATIVE_DOUBLE;
    field_type[12] = H5T_NATIVE_DOUBLE;
    field_type[13] = H5T_NATIVE_DOUBLE;
    field_type[14] = H5T_NATIVE_DOUBLE;
    field_type[15] = H5T_NATIVE_DOUBLE;
    field_type[16] = H5T_NATIVE_DOUBLE;
    field_type[17] = H5T_NATIVE_INT;
					        
    status=H5TBmake_table( "AssimilationFeedBack", loc_id, "AssimilationFeedBack",(hsize_t) NFIELDS18, (hsize_t)nrecs, dst_size, 
                            field_names, dst_offset, field_type, 
                            chunk_size, fill_data, compress, dat );
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of timestamp","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Unique Observation Number","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Modified Observed Pressure after Vertically Interpolating to Model Surface","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Error in Observed Pressure Vertically Interpolated to Model Surface","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Bias","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Status Flag for Observed Surface Pressure","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Assimilation Indicator","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Usability Indicator","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of QC Background check flag indicator","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Buddry flag indicator","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Quality Control indicator","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Ensemble Mean First Guess Pressure","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Standard Deviation of Ensemble Guess Pressure","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Modified Observation Pressure minus Ensemble Mean First Guess Pressure","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Ensemble Mean Analysis Pressure","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Standard Deviation of Ensemble Analysis Pressure","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Modified Observation Pressure minus Ensemble Mean Analysis Pressure","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Modified Elevation Vertically Interpolated to  Model Surface","m");

    return status;
}

herr_t make_feedback_table(hid_t loc_id, hsize_t nrecs, FeedBack dat[]) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( FeedBack );
    size_t dst_offset[NFIELDS15] = { HOFFSET( FeedBack, timestamp ),
                                   HOFFSET( FeedBack, unoc ),
                                   HOFFSET( FeedBack, mdpavims ),
                                   HOFFSET( FeedBack, epvims ),
                                   HOFFSET( FeedBack, sfsfp ),
                                   HOFFSET( FeedBack, dc ),
                                   HOFFSET( FeedBack, ai ),
                                   HOFFSET( FeedBack, bcf ),
                                   HOFFSET( FeedBack, bf ),
                                   HOFFSET( FeedBack, emfg ),
                                   HOFFSET( FeedBack, sdeg ),
                                   HOFFSET( FeedBack, mpmemfg ),
                                   HOFFSET( FeedBack, emap ),
                                   HOFFSET( FeedBack, sdeap ),
                                   HOFFSET( FeedBack, mpmema ) };

    /* Define field information */
    const char *field_names[NFIELDS15]  = 
        { "Timestamp", "Unique Observation Number", "Modified Observed Pressure after Vertically Interpolating to Model Surface","Error in Observed Pressure vertically Interpolated to Model Surface","Status Flag for Observed Surface Pressure","Duplicate Check for Reanalysis","Assimilation indicator","QC Background check flag indicator","Buddy flag indicator","Ensemble Mean First Guess Pressure","Standard Deviation of Ensemble Guess Pressure","Modified Observation Pressure minus Ensemble Mean First Guess Pressure","Ensemble Mean Analysis Pressure","Standard Deviation of Ensemble Analysis Pressure","Modified Observation Pressure minus Ensemble Mean Analysis Pressure" };

    hid_t      field_type[NFIELDS15];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status; 
    hid_t      string_type8,string_type13;

    //EXAMPLE("make a table AssimilationFeedback\n");

    /* Initialize the field field_type */
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type13, 13 );
    string_type8 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type8, 8 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = H5T_NATIVE_DOUBLE;
    field_type[3] = H5T_NATIVE_FLOAT;
    field_type[4] = H5T_NATIVE_INT;
    field_type[5] = H5T_NATIVE_INT;
    field_type[6] = H5T_NATIVE_INT;
    field_type[7] = H5T_NATIVE_INT;
    field_type[8] = H5T_NATIVE_INT;
    field_type[9] = H5T_NATIVE_DOUBLE;
    field_type[10] = H5T_NATIVE_DOUBLE;
    field_type[11] = H5T_NATIVE_DOUBLE;
    field_type[12] = H5T_NATIVE_DOUBLE;
    field_type[13] = H5T_NATIVE_DOUBLE;
    field_type[14] = H5T_NATIVE_DOUBLE;
					        
    status=H5TBmake_table( "AssimilationFeedBack", loc_id, "AssimilationFeedBack",(hsize_t) NFIELDS15, (hsize_t)nrecs, dst_size, 
                            field_names, dst_offset, field_type, 
                            chunk_size, fill_data, compress, dat );
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of timestamp","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Unique Observation Number","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Modified Observed Pressure after Vertically Interpolating to Model Surface","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Error in Observed Pressure vertically Interpolated to Model Surface","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Status Flag for Observed Surface Pressure","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Duplicate Check for Reanalysis","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Assimilation Indicator","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of QC Background check flag indicator","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Buddry flag indicator","");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Ensemble Mean First Guess Pressure","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Standard Deviation of Ensemble Guess Pressure","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Modified Observation Pressure minus Ensemble Mean First Guess Pressure","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Ensemble Mean Analysis Pressure","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Standard Deviation of Ensemble Analysis Pressure","hPa");
    status = H5LTset_attribute_string(loc_id,"AssimilationFeedBack","Unit of Modified Observation Pressure minus Ensemble Mean Analysis Pressure","hPa");

    return status;
}

herr_t make_metadata_table(hid_t loc_id, hsize_t nrecs, MetaData dat[]){
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( MetaData );
    size_t dst_offset[NFIELDS16] = { HOFFSET( MetaData, timestamp ),
	                           HOFFSET( MetaData, unoc ),
                                   HOFFSET( MetaData, id ),
                                   HOFFSET( MetaData, year ),
                                   HOFFSET( MetaData, month ),
                                   HOFFSET( MetaData, day ),
                                   HOFFSET( MetaData, hour ),
                                   HOFFSET( MetaData, minute ),
                                   HOFFSET( MetaData, second ),
                                   HOFFSET( MetaData, time_in_sec ),
                                   HOFFSET( MetaData, time_code ),
                                   HOFFSET( MetaData, n_obs ),
                                   HOFFSET( MetaData, h_gmt ),
                                   HOFFSET( MetaData, lat ),
                                   HOFFSET( MetaData, lon ),
                                   HOFFSET( MetaData, elv )};

    /* Define field information */
    const char *field_names[NFIELDS16]  = 
        { "Timestamp", "Unique Observation Number","Observation ID", "Year","Month","Day","Hour","Minute","Second", "Time in Second after 1600", "Time Code","Number of Observations used per day if observation is average","Hours of the day in GMT of the observations","Latitude","Longitude","Elevation" };

    hid_t      field_type[NFIELDS16];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status; 
    hid_t      string_type4,string_type200;
    hid_t      string_type8,string_type13;

    //EXAMPLE("make a table SpatialTemporalLocation\n");

    /* Initialize the field field_type */
    string_type13 = H5Tcopy( H5T_C_S1 );
    string_type8 = H5Tcopy( H5T_C_S1 );
    string_type4 = H5Tcopy( H5T_C_S1 );
    string_type200 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type8, 8);
    H5Tset_size( string_type13,13 );
    H5Tset_size( string_type4, 4 );
    H5Tset_size( string_type200, 200 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = string_type13;
    field_type[3] = H5T_NATIVE_INT;
    field_type[4] = H5T_NATIVE_INT;
    field_type[5] = H5T_NATIVE_INT;
    field_type[6] = H5T_NATIVE_INT;
    field_type[7] = H5T_NATIVE_INT;
    field_type[8] = H5T_NATIVE_INT;
    field_type[9] = H5T_NATIVE_INT;
    field_type[10] = string_type4;
    field_type[11] = H5T_NATIVE_INT;
    field_type[12] = string_type200;
    field_type[13] = H5T_NATIVE_FLOAT;
    field_type[14] = H5T_NATIVE_FLOAT;
    field_type[15] = H5T_NATIVE_INT;
					        
    status=H5TBmake_table( "SpatialTemporalLocation", loc_id, "SpatialTemporalLocation",(hsize_t) NFIELDS16, (hsize_t)nrecs,     dst_size,field_names, dst_offset, field_type, chunk_size, fill_data, compress, dat);
    status = H5LTset_attribute_string(loc_id,"SpatialTemporalLocation","Lat Unit","Degrees_North");
    status = H5LTset_attribute_string(loc_id,"SpatialTemporalLocation","Lon Unit","Degrees_East");
    status = H5LTset_attribute_string(loc_id,"SpatialTemporalLocation","Elevation Unit","meter");
    return status;
}

herr_t make_obstype_table(hid_t loc_id, hsize_t nrecs, ObsType dat[]){
    size_t dst_size =  sizeof( ObsType );
    size_t dst_offset[NFIELDS5] = { HOFFSET( ObsType, timestamp ),
                                   HOFFSET( ObsType, unoc ),
                                   HOFFSET( ObsType, id_type ),
                                   HOFFSET( ObsType, ncep_type ),
                                   HOFFSET( ObsType, ispdbcid )};

    /* Define field information */
    const char *field_names[NFIELDS5]  = 
        { "Timestamp", "Unique Observation Number", "Observation ID Type","NCEP Observation Type Code", "International Surface Pressure Data Bank Collection ID" };

    hid_t      field_type[NFIELDS5];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status; 
    hid_t      string_type7,string_type8,string_type13;

    //EXAMPLE("make a table ObservationTypes\n");
    string_type7 = H5Tcopy( H5T_C_S1 );
    string_type8 = H5Tcopy( H5T_C_S1 );
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type7, 7);
    H5Tset_size( string_type8, 8);
    H5Tset_size( string_type13, 13);
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = H5T_NATIVE_INT;
    field_type[3] = H5T_NATIVE_INT;
    field_type[4] = string_type7;

    status=H5TBmake_table( "ObservationTypes", loc_id, "ObservationTypes",(hsize_t) NFIELDS5, (hsize_t)nrecs, dst_size, 
                            field_names, dst_offset, field_type, 
                            chunk_size, fill_data, compress, dat );
    return status;
}

herr_t make_obs_original_table(hid_t loc_id, hsize_t nrecs, OrigObservations dat[]) {
    size_t dst_size =  sizeof( OrigObservations );
    size_t dst_offset[NFIELDS6] = { HOFFSET( OrigObservations, timestamp ),
                                   HOFFSET( OrigObservations, unoc ),
                                   HOFFSET( OrigObservations, oslp ),
                                   HOFFSET( OrigObservations, uoslp ),
                                   HOFFSET( OrigObservations, osfp ),
                                   HOFFSET( OrigObservations, uosfp )};
    /* Define an array of Src */
    const char *field_names[NFIELDS6]  = 
        { "Timestamp", "Unique Observation Number", "Original Observed Sea Level Pressure","Units of Original Observed Sea Level Pressure","Original Observed Surface Pressure","Units of Original Observed Surface Pressure" };

    hid_t      field_type[NFIELDS6];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status; 
    hid_t      string_type8,string_type13;
    hid_t      string_type9, string_type10;

    //EXAMPLE("make a table OriginalObservations \n");

    /* Initialize the field field_type */
    string_type8 = H5Tcopy( H5T_C_S1 );
    string_type9 = H5Tcopy( H5T_C_S1 );
    string_type10 = H5Tcopy( H5T_C_S1 );
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type10, 10 );
    H5Tset_size( string_type9, 9 );
    H5Tset_size( string_type8, 8 );
    H5Tset_size( string_type13, 13 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = string_type10;
    field_type[3] = string_type9;
    field_type[4] = string_type10;
    field_type[5] = string_type9;
					        
    status=H5TBmake_table( "OriginalObservations", loc_id, "OriginalObservations",(hsize_t) NFIELDS6, (hsize_t)nrecs, dst_size, 
                            field_names, dst_offset, field_type, 
                            chunk_size, fill_data, compress, dat  );
    return status;
}

herr_t make_src_table(hid_t loc_id, hsize_t nrecs, Source dat[]) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( Source );
    size_t dst_offset[NFIELDS6] = { HOFFSET( Source, timestamp ),
                                   HOFFSET( Source, unoc ),
                                   HOFFSET( Source, sflsd ),
                                   HOFFSET( Source, rtc ),
                                   HOFFSET( Source, qcislp ),
                                   HOFFSET( Source, qcisfp )};

    /* Define field information */
    const char *field_names[NFIELDS6]  = 
        { "Timestamp", "Unique Observation Number", "Source flag for Land Station Data","Report type code","Quality Control Indicators for sea level pressure value from source","Quality Control Indicators for surface pressure value from source" };

    hid_t      field_type[NFIELDS6];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status; 
    hid_t      string_type2, string_type6;
    hid_t      string_type8, string_type13;

    //EXAMPLE("make a table TrackingLand \n");

    /* Initialize the field field_type */
    string_type2 = H5Tcopy( H5T_C_S1 );
    string_type6 = H5Tcopy( H5T_C_S1 );
    string_type8 = H5Tcopy( H5T_C_S1 );
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type13, 13 );
    H5Tset_size( string_type2, 2 );
    H5Tset_size( string_type6, 6 );
    H5Tset_size( string_type8, 8 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = string_type2;
    field_type[3] = string_type6;
    field_type[4] = string_type6;
    field_type[5] = string_type6;
					        
    status=H5TBmake_table( "TrackingLand", loc_id, "TrackingLand",(hsize_t) NFIELDS6, (hsize_t)nrecs, dst_size, 
                            field_names, dst_offset, field_type, 
                            chunk_size, fill_data, compress, dat  );
    return status;
}

herr_t make_srcmar_table(hid_t loc_id, hsize_t nrecs, SrcMarine dat[]) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( SrcMarine );
    size_t dst_offset[NFIELDS12] = { HOFFSET( SrcMarine, timestamp ),
	                           HOFFSET( SrcMarine, unoc ), 
	                           HOFFSET( SrcMarine, sid ), 
	                           HOFFSET( SrcMarine, dck ), 
                                   HOFFSET( SrcMarine, pt ),
                                   HOFFSET( SrcMarine, orig_hr),
                                   HOFFSET( SrcMarine, uid),
                                   HOFFSET( SrcMarine, rn1),
                                   HOFFSET( SrcMarine, rn2),
                                   HOFFSET( SrcMarine, rn3),
                                   HOFFSET( SrcMarine, rsa),
                                   HOFFSET( SrcMarine, irf)};
    /* Define field information */
    const char *field_names[NFIELDS12]  = 
        { "Timestamp", "Unique Observation Number", "Source ID", "Deck ID", "Platform Type" , "Original HR", "Unique report ID" , "Release no.: primary" , "Release no.: secondary" , "Release no.: tertiary" , "Release status indicator" , "Intermediate reject flag" };

    hid_t      field_type[NFIELDS12];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status; 
    hid_t      string_type1,string_type4,string_type6,string_type8,string_type13;

    //EXAMPLE("make a table TrackingICOADS\n");

    /* Initialize the field field_type */
    string_type1 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type1, 1 );
    string_type4 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type4, 4 );
    string_type6 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type6, 6 );
    string_type8 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type8, 8 );
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type13, 13 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = H5T_NATIVE_INT;
    field_type[3] = H5T_NATIVE_INT;
    field_type[4] = H5T_NATIVE_INT;
    field_type[5] = string_type4;
    field_type[6] = string_type6;
    field_type[7] = string_type1;
    field_type[8] = string_type1;
    field_type[9] = string_type1;
    field_type[10] = H5T_NATIVE_INT;
    field_type[11] = H5T_NATIVE_INT;
					        
    status=H5TBmake_table( "TrackingICOADS", loc_id, "TrackingICOADS",(hsize_t) NFIELDS12, (hsize_t)nrecs, dst_size, 
                            field_names, dst_offset, field_type, 
                            chunk_size, fill_data, compress, dat );
    return status;
}

herr_t make_tracking_table(hid_t loc_id, hsize_t nrecs, Tracking dat[]) {
    size_t dst_size =  sizeof( Tracking );
    size_t dst_offset[NFIELDS4] = { HOFFSET( Tracking, timestamp ),
                                   HOFFSET( Tracking, unoc ),
                                   HOFFSET( Tracking, sname ),
                                   HOFFSET( Tracking, slib )};

    const char *field_names[NFIELDS4]  = 
        { "Timestamp", "Unique Observation Number", "Name of ships or stations", "Name of station library" };

    hid_t      field_type[NFIELDS4];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status; 
    hid_t      string_type4, string_type31;
    hid_t      string_type8, string_type13;

    //EXAMPLE("make a table Tracking\n");

    /* Initialize the field field_type */
    string_type4 = H5Tcopy( H5T_C_S1 );
    string_type13 = H5Tcopy( H5T_C_S1 );
    string_type8 = H5Tcopy( H5T_C_S1 );
    string_type31 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type13, 13 );
    H5Tset_size( string_type4, 4 );
    H5Tset_size( string_type8, 8 );
    H5Tset_size( string_type31, 31 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = string_type31;
    field_type[3] = string_type4;
					        
    status=H5TBmake_table( "Tracking", loc_id, "Tracking",(hsize_t) NFIELDS4, (hsize_t)nrecs, dst_size, 
                            field_names, dst_offset, field_type, 
                            chunk_size, fill_data, compress, dat  );
    return status;
}

herr_t make_presinst_table(hid_t loc_id, hsize_t nrecs, PresInst dat[]) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( PresInst );
    size_t dst_offset[NFIELDS3] = { HOFFSET( PresInst, timestamp ),
                                   HOFFSET( PresInst, unoc ),
                                   HOFFSET( PresInst, PressureInst )};
    /* Define field information */
    const char *field_names[NFIELDS3]  =
        { "Timestamp", "Unique Observation Number", "Pressure Instrument Identifier" };

    hid_t      field_type[NFIELDS3];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status;
    hid_t      string_type8, string_type13;

    //EXAMPLE("make a table PresInst\n");

    /* Initialize the field field_type */
    string_type13 = H5Tcopy( H5T_C_S1 );
    string_type8 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type8, 8 );
    H5Tset_size( string_type13, 13 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = H5T_NATIVE_INT;

    status=H5TBmake_table( "PressureInstrument", loc_id, "PressureInstrument",(hsize_t) NFIELDS3, (hsize_t)nrecs, dst_size,
                            field_names, dst_offset, field_type,
                            chunk_size, fill_data, compress, dat  );
    return status;
}

herr_t make_origmetadata_table(hid_t loc_id, hsize_t nrecs, OrigMetaData dat[]){
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( OrigMetaData );
    size_t dst_offset[NFIELDS6] = { HOFFSET( OrigMetaData, timestamp ),
	                           HOFFSET( OrigMetaData, unoc ),
	                           HOFFSET( OrigMetaData, olat ),
	                           HOFFSET( OrigMetaData, olon ),
	                           HOFFSET( OrigMetaData, oelev ),
	                           HOFFSET( OrigMetaData, uoelev )};

    /* Define field information */
    const char *field_names[NFIELDS6]  = 
        { "Timestamp", "Unique Observation Number","Original Latitude", "Original Longitude","Original Elevation","Unit of Original Elevation" };

    hid_t      field_type[NFIELDS6];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status; 
    hid_t      string_type9,string_type7;
    hid_t      string_type8, string_type13;

    //EXAMPLE("make a table original spatial temporal location\n");

    /* Initialize the field field_type */
    string_type13 = H5Tcopy( H5T_C_S1 );
    string_type8 = H5Tcopy( H5T_C_S1 );
    string_type9 = H5Tcopy( H5T_C_S1 );
    string_type7 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type13, 13);
    H5Tset_size( string_type8, 8);
    H5Tset_size( string_type9, 9);
    H5Tset_size( string_type7, 7);
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = string_type9;
    field_type[3] = string_type9;
    field_type[4] = string_type7;
    field_type[5] = string_type9;
					        
    status=H5TBmake_table( "OriginalSpatialTemporalLocation", loc_id, "OriginalSpatialTemporalLocation",(hsize_t) NFIELDS6, (hsize_t)nrecs,     dst_size,field_names, dst_offset, field_type, chunk_size, fill_data, compress, dat);
    return status;
}

herr_t make_miscmar_table(hid_t loc_id, hsize_t nrecs, MiscMarine dat[] ) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( MiscMarine );
    size_t dst_offset[NFIELDS3] = { HOFFSET( MiscMarine, timestamp ),
                                   HOFFSET( MiscMarine, unoc ),
                                   HOFFSET( MiscMarine, PressureBias )};

    /* Define field information */
    const char *field_names[NFIELDS3]  =
        { "Timestamp", "Unique Observation Number", "Pressure Bias" };

    hid_t      field_type[NFIELDS3];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status;
    hid_t      string_type8,string_type13;

    //EXAMPLE("make a table PressureBias in miscmar\n");

    /* Initialize the field field_type */
    string_type8 = H5Tcopy( H5T_C_S1 );
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type8, 8 );
    H5Tset_size( string_type13, 13 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = H5T_NATIVE_INT;

    status=H5TBmake_table( "PressureBias", loc_id, "PressureBias",(hsize_t) NFIELDS3, (hsize_t)nrecs, dst_size,
                            field_names, dst_offset, field_type,
                            chunk_size, fill_data, compress, dat  );
    return status;
}

herr_t make_hurricanes_table(hid_t loc_id, hsize_t nrecs, Hurricanes dat[] ) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( Hurricanes );
    size_t dst_offset[NFIELDS11] = { HOFFSET( Hurricanes, timestamp ),
                                   HOFFSET( Hurricanes, unoc ),
                                   HOFFSET( Hurricanes, dir ),
                                   HOFFSET( Hurricanes, trm ),
                                   HOFFSET( Hurricanes, trmm ),
                                   HOFFSET( Hurricanes, trk ),
                                   HOFFSET( Hurricanes, wm ),
                                   HOFFSET( Hurricanes, wmm ),
                                   HOFFSET( Hurricanes, wk ),
                                   HOFFSET( Hurricanes, d2l ),
                                   HOFFSET( Hurricanes, lnd )};

    /* Define field information */
    const char *field_names[NFIELDS11]  =
        { "Timestamp", "Unique Observation Number", "Maximum Sustained Wind Direction", "Translation Speed in Miles/hour", "Translation Speed in m/s", "Translation Speed in Knots", "Wind Speed in Miles/hour", "Wind Speed in m/s", "Wind Speed in knots","Distance to Land (km)","Minimum distance to land between current location and next km" };

    hid_t      field_type[NFIELDS11];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status;
    hid_t      string_type8,string_type13;

    //EXAMPLE("make a table PressureBias in miscmar\n");

    /* Initialize the field field_type */
    string_type8 = H5Tcopy( H5T_C_S1 );
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type8, 8 );
    H5Tset_size( string_type13, 13 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = H5T_NATIVE_INT;
    field_type[3] = H5T_NATIVE_INT;
    field_type[4] = H5T_NATIVE_FLOAT;
    field_type[5] = H5T_NATIVE_INT;
    field_type[6] = H5T_NATIVE_INT;
    field_type[7] = H5T_NATIVE_FLOAT;
    field_type[8] = H5T_NATIVE_INT;
    field_type[9] = H5T_NATIVE_INT;
    field_type[10] = H5T_NATIVE_INT;

     status=H5TBmake_table( "TropicalStorms", loc_id, "TropicalStorms",(hsize_t) NFIELDS11, (hsize_t)nrecs, dst_size,
                            field_names, dst_offset, field_type,
                            chunk_size, fill_data, compress, dat  );
    return status;
}

herr_t make_hurricaneid_table(hid_t loc_id, hsize_t nrecs, HurricaneID dat[] ) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( HurricaneID );
    size_t dst_offset[NFIELDS3] = { HOFFSET( HurricaneID, timestamp ),
                                   HOFFSET( HurricaneID, unoc ),
                                   HOFFSET( HurricaneID, storm_id )};

    /* Define field information */
    const char *field_names[NFIELDS3]  =
        { "Timestamp", "Unique Observation Number", "Storm ID" };

    hid_t      field_type[NFIELDS3];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status;
    hid_t      string_type8,string_type13,string_type31;


    /* Initialize the field field_type */
    string_type8 = H5Tcopy( H5T_C_S1 );
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type8, 8 );
    H5Tset_size( string_type13, 13 );
    H5Tset_size( string_type31, 31 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = string_type31;

     status=H5TBmake_table( "StormID", loc_id, "StormID",(hsize_t) NFIELDS3, (hsize_t)nrecs, dst_size,
                            field_names, dst_offset, field_type,
                            chunk_size, fill_data, compress, dat  );
    return status;
}

herr_t make_hurricaneqc_table(hid_t loc_id, hsize_t nrecs, HurricaneQC dat[] ) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( HurricaneVariances );
    size_t dst_offset[NFIELDS4] = { HOFFSET( HurricaneQC, timestamp ),
                                   HOFFSET( HurricaneQC, unoc ),
                                   HOFFSET( HurricaneQC, wq ),
                                   HOFFSET( HurricaneQC, pq )};

    /* Define field information */
    const char *field_names[NFIELDS4]  =
        { "Timestamp", "Unique Observation Number", "Wind Quality", "Pressure Quality" };

    hid_t      field_type[NFIELDS4];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status;
    hid_t      string_type8,string_type13;


    /* Initialize the field field_type */
    string_type8 = H5Tcopy( H5T_C_S1 );
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type8, 8 );
    H5Tset_size( string_type13, 13 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = H5T_NATIVE_INT;
    field_type[3] = H5T_NATIVE_INT;

     status=H5TBmake_table( "QualityControl", loc_id, "QualityControl",(hsize_t) NFIELDS4, (hsize_t)nrecs, dst_size,
                            field_names, dst_offset, field_type,
                            chunk_size, fill_data, compress, dat  );
    return status;
}

herr_t make_hurricanevariances_table(hid_t loc_id, hsize_t nrecs, HurricaneVariances dat[] ) {
    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size =  sizeof( HurricaneVariances );
    size_t dst_offset[NFIELDS5] = { HOFFSET( HurricaneVariances, timestamp ),
                                   HOFFSET( HurricaneVariances, unoc ),
                                   HOFFSET( HurricaneVariances, pe ),
                                   HOFFSET( HurricaneVariances, we ),
                                   HOFFSET( HurricaneVariances, pre )};

    /* Define field information */
    const char *field_names[NFIELDS5]  =
        { "Timestamp", "Unique Observation Number", "Variance in the position amongst the Tropical Cyclone centers", "Variance in the wind wind amongst the Tropical Cyclone centers", "Variance in the pressure amongst the Tropical Cyclone centers" };

    hid_t      field_type[NFIELDS5];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status;
    hid_t      string_type8,string_type13;


    /* Initialize the field field_type */
    string_type8 = H5Tcopy( H5T_C_S1 );
    string_type13 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type8, 8 );
    H5Tset_size( string_type13, 13 );
    field_type[0] = string_type13;
    field_type[1] = string_type8;
    field_type[2] = H5T_NATIVE_INT;
    field_type[3] = H5T_NATIVE_INT;
    field_type[4] = H5T_NATIVE_INT;

     status=H5TBmake_table( "Variances", loc_id, "Variances",(hsize_t) NFIELDS5, (hsize_t)nrecs, dst_size,
                            field_names, dst_offset, field_type,
                            chunk_size, fill_data, compress, dat  );
    return status;
}
// -------------------------------------------------------------------------------
// -------------------------------------------------------------------------------

void add_hurricanes_group (hid_t* file){
    hid_t grpHur, grpMisc;
    Hurricanes* hurricanes;
    HurricaneVariances* hurricanevariances;
    HurricaneID* hurricaneid;
    HurricaneQC* hurricaneqc;
    herr_t status;

    hurricanes = malloc(sizeof(Hurricanes));
    grpMisc = H5Gopen(*file, "/SupplementalData/Misc", H5P_DEFAULT);
    grpHur = H5Gcreate(grpMisc,"TropicalStorms",0,H5P_DEFAULT,H5P_DEFAULT);
    status= make_hurricanes_table(grpHur,0,hurricanes);
    status= make_hurricaneid_table(grpHur,0,hurricaneid);
    status= make_hurricaneqc_table(grpHur,0,hurricaneqc);
    status= make_hurricanevariances_table(grpHur,0,hurricanevariances);
    status = H5Gclose(grpHur);
    status = H5Gclose(grpMisc);
    free(hurricanes);


}
// make_tables function creates all the ISPDB tables without data
void make_tables (hid_t* file){

    // local vars
    size_t n_fstr, n_ostr;
    hid_t  grpDat,grpOrgGeo,grpGeo,grpTrc,grpCor,grpMiscData,grpMisc,grpObs,grpOrgObs,grpFbk,grpSrcMar,grpMiscMar,grpLnd,grpText, grpHur;
    herr_t   status;
    char* str_ptr;
    ObsType* obstype;
    OrigObservations* origobservations;
    Observations* observations;
    MetaData* metadata;
    OrigMetaData* origmetadata;
    Tracking* tracking;
    Source* source;
    Corrections* corrections;
    PresInst* presinst;
    NewFeedBack* feedback;
    MiscMarine* miscmar;
    SrcMarine* srcmar;
    Hurricanes* hurricanes;
    HurricaneID* hurricaneid;
    HurricaneQC* hurricaneqc;
    HurricaneVariances* hurricanevariances;

    // creating dummy data
    obstype = malloc(sizeof( ObsType));
    origobservations = malloc(sizeof( OrigObservations));
    observations = malloc(sizeof(Observations));
    metadata = malloc(sizeof(MetaData));
    origmetadata = malloc(sizeof(OrigMetaData));
    tracking =  malloc(sizeof(Tracking));
    source =  malloc(sizeof(Source));
    corrections =  malloc(sizeof(Corrections));
    presinst = malloc(sizeof(PresInst));
    feedback =  malloc(sizeof(NewFeedBack));
    miscmar =  malloc(sizeof(MiscMarine));
    srcmar = malloc(sizeof( SrcMarine));
    hurricanes = malloc(sizeof(Hurricanes));
    hurricaneid = malloc(sizeof(HurricaneID));
    hurricaneqc = malloc(sizeof(HurricaneQC));
    hurricanevariances = malloc(sizeof(HurricaneVariances));

    // ISPD Version
    status = H5LTmake_dataset_string (*file, "ISPD_Format_Version", "11.0");
    // ISPD 2.2
    status = H5LTmake_dataset_string (*file, "ISPD_Version", "4.0");
    // Create groups in the file.
    // then create tables
    grpDat = H5Gcreate(*file, "Data",0,H5P_DEFAULT,H5P_DEFAULT);
        grpGeo = H5Gcreate(grpDat, "SpatialTemporalLocation",0,H5P_DEFAULT,H5P_DEFAULT);
        status = make_metadata_table(grpGeo,0,metadata);
            grpOrgGeo = H5Gcreate(grpGeo, "OriginalSpatialTemporalLocation",0,H5P_DEFAULT,H5P_DEFAULT);
            status = make_origmetadata_table(grpOrgGeo,0,origmetadata);
            status = H5Gclose(grpOrgGeo);
        status = H5Gclose(grpGeo);

        grpObs = H5Gcreate(grpDat,"Observations",0,H5P_DEFAULT,H5P_DEFAULT);
        status = make_obs_table(grpObs,0,observations);
        status = make_obstype_table(grpObs,0,obstype);
            grpOrgObs = H5Gcreate(grpObs, "OriginalObservations",0,H5P_DEFAULT,H5P_DEFAULT);
            status = make_obs_original_table(grpOrgObs,0,origobservations);
            status = H5Gclose(grpOrgObs);
        status = H5Gclose(grpObs);

        grpFbk = H5Gcreate(grpDat,"AssimilationFeedback",0,H5P_DEFAULT,H5P_DEFAULT);
        status = make_newfeedback_table(grpFbk,0,feedback);
        status = H5Gclose(grpFbk);
    status = H5Gclose(grpDat);


    grpMiscData = H5Gcreate(*file,"SupplementalData",0,H5P_DEFAULT,H5P_DEFAULT);
        grpCor = H5Gcreate(grpMiscData, "Corrections",0,H5P_DEFAULT,H5P_DEFAULT);
        status = make_corrections_table(grpCor,0,corrections);
        status = make_presinst_table(grpCor,0,presinst);
        status = H5Gclose(grpCor);

        grpMisc = H5Gcreate(grpMiscData, "Misc",0,H5P_DEFAULT,H5P_DEFAULT);
            grpMiscMar = H5Gcreate(grpMisc,"ICOADS",0,H5P_DEFAULT,H5P_DEFAULT);
            status= make_miscmar_table(grpMiscMar,0,miscmar);
            status = H5Gclose(grpMiscMar);

            grpHur = H5Gcreate(grpMisc,"TropicalStorms",0,H5P_DEFAULT,H5P_DEFAULT);
            status= make_hurricanes_table(grpHur,0,hurricanes);
            status= make_hurricaneid_table(grpHur,0,hurricaneid);
            status= make_hurricaneqc_table(grpHur,0,hurricaneqc);
            status= make_hurricanevariances_table(grpHur,0,hurricanevariances);
            status = H5Gclose(grpHur);
        status = H5Gclose(grpMisc);

        grpTrc = H5Gcreate(grpMiscData, "Tracking",0,H5P_DEFAULT,H5P_DEFAULT);
         status = make_tracking_table(grpTrc,0,tracking);

            grpLnd = H5Gcreate(grpTrc, "Land",0,H5P_DEFAULT,H5P_DEFAULT);
            status = make_src_table(grpLnd,0,source);
            status = H5Gclose(grpLnd);

            grpSrcMar = H5Gcreate(grpTrc,"ICOADS",0,H5P_DEFAULT,H5P_DEFAULT);
            status= make_srcmar_table(grpSrcMar,0,srcmar);
            status = H5Gclose(grpSrcMar);

        status = H5Gclose(grpTrc);


    status = H5Gclose(grpMiscData);

    grpText = H5Gcreate(*file,"TableOfOriginalSources",0,H5P_DEFAULT,H5P_DEFAULT);
    status = make_datasetindex(grpText);
    status = H5Gclose(grpText);

    free(observations);
    free(obstype);
    free(origobservations);
    free(metadata);
    free(origmetadata);
    free(tracking);
    free(source);
    free(corrections);
    free(presinst);
    free(feedback);
    free(miscmar);
    free(srcmar);
    free(hurricanes);
    free(hurricaneid);
    free(hurricaneqc);
    free(hurricanevariances);
}

void rw_threedim_h5 (const size_t stime, const size_t etime, char* input_filename, hid_t h5files[] ){
// stime -i.e. 19740103
// etime -i.e. 19740106
    
    hid_t file_id;
    herr_t status;          
    ObsType* tmp_obstype;       
    OrigObservations* tmp_origobservations;
    Observations* tmp_observations;
    MetaData* tmp_metadata;     
    OrigMetaData* tmp_origmetadata;
    Tracking* tmp_tracking;     
    Source* tmp_source;
    Corrections* tmp_corrections;
    PresInst* tmp_presinst;
    FeedBack* tmp_feedback;
    int sd = (int)stime % 100;
    int ed = (int)etime % 100;
    
    
    size_t  nobstype,norigobs,nmeta,norigmeta,ntrack,nsrc,ncorr,npres,nfeed,nobs;
    nobstype=norigobs=nmeta=norigmeta=ntrack=nsrc=ncorr=npres=nfeed=nobs=0;
    hsize_t  tot_nobstype,tot_norigobs,tot_nmeta,tot_norigmeta,tot_ntrack,tot_nsrc,tot_ncorr,tot_npres,tot_nfeed,tot_nobs;
    tot_nobstype=tot_norigobs=tot_nmeta=tot_norigmeta=tot_ntrack=tot_nsrc=tot_ncorr=tot_npres=tot_nfeed=tot_nobs=0;

    file_id = ropen_h5file(input_filename);
    
    printf ( "getting info of data\n");
    info_observations (file_id, &nobs, &tot_nobs,(int)stime, (int)etime);
    info_origobservations (file_id, &norigobs, &tot_norigobs,(int)stime, (int)etime);
    info_obstype (file_id, &nobstype, &tot_nobstype,(int)stime, (int)etime);
    info_origmetadata (file_id, &norigmeta, &tot_norigmeta,(int)stime, (int)etime);
    info_metadata (file_id, &nmeta, &tot_nmeta,(int)stime, (int)etime);
    info_source (file_id, &nsrc, &tot_nsrc,(int)stime, (int)etime);
    info_tracking (file_id, &ntrack, &tot_ntrack,(int)stime, (int)etime);
    info_corrections (file_id, &ncorr, &tot_ncorr,(int)stime, (int)etime);
    info_presinst (file_id, &npres, &tot_npres,(int)stime, (int)etime);
    info_feedback (file_id, &nfeed, &tot_nfeed,(int)stime, (int)etime);

    if(nobs != nobstype || nobs != norigobs || nobs != nmeta || nobs != norigmeta || nobs != ntrack || nobs != nsrc || nobs != ncorr || nobs != npres || nobs != nfeed){
        fprintf (stderr, "Numbers of obs do not match for %s\n", input_filename);
        //fprintf (stderr, "nobs %i nobstype %i norigobs %i nmeta %i norigmeta %i ntrack %i nsrc %i ncorr %i npres %i nfeed %i\n",nobs,nobstype,norigobs,nmeta,norigmeta,ntrack,nsrc,ncorr,npres,nfeed);
        fprintf (stderr, "nobs %zd nobstype %zd norigobs %zd nmeta %zd norigmeta %zd ntrack %zd nsrc %zd ncorr %zd npres %zd nfeed %zd\n",nobs,nobstype,norigobs,nmeta,norigmeta,ntrack,nsrc,ncorr,npres,nfeed);
        H5Fclose( file_id );
        printf ("Closed  %s\n", input_filename);
        exit(1);
    }

    if(tot_nobs != tot_nobstype || tot_nobs != tot_norigobs || tot_nobs != tot_nmeta || tot_nobs != tot_norigmeta || tot_nobs != tot_ntrack || tot_nobs != tot_nsrc || tot_nobs != tot_ncorr || tot_nobs != tot_npres || tot_nobs != tot_nfeed){
        fprintf (stderr, "Numbers of total obs do not match for %s\n", input_filename);
        fprintf (stderr, "nobs %i nobstype %i norigobs %i nmeta %i norigmeta %i ntrack %i nsrc %i ncorr %i npres %i nfeed %i\n",(int)tot_nobs,(int)tot_nobstype,(int)tot_norigobs,(int)tot_nmeta,(int)tot_norigmeta,(int)tot_ntrack,(int)tot_nsrc,(int)tot_ncorr,(int)tot_npres,(int)tot_nfeed);
        H5Fclose( file_id );
        printf ("Closed  %s\n", input_filename);
        exit(1);
    }

    //fprintf (stderr, "nobs %i nobstype %i norigobs %i nmeta %i norigmeta %i ntrack %i nsrc %i ncorr %i npres %i nfeed %i\n",nobs,nobstype,norigobs,nmeta,norigmeta,ntrack,nsrc,ncorr,npres,nfeed);

    if(nmeta > 0) {
        // cout << "allocating mem since we found " << nmeta << " obs ... we'll write\n";

        tmp_metadata = malloc(sizeof(MetaData)*nmeta);
        tmp_observations =  malloc(sizeof(Observations)*nmeta);
        tmp_origobservations =  malloc(sizeof(OrigObservations)*nmeta);
        tmp_feedback =  malloc(sizeof(FeedBack)*nmeta);
        tmp_obstype = malloc(sizeof(ObsType)*nmeta);
        tmp_origmetadata =  malloc(sizeof(OrigMetaData)*nmeta);
        tmp_tracking =  malloc(sizeof(Tracking)*nmeta);
        tmp_corrections =  malloc(sizeof(Corrections)*nmeta);
        tmp_presinst =  malloc(sizeof(PresInst)*nmeta);
        tmp_source =  malloc(sizeof(Source)*nmeta);


        slice_metadata (file_id, tot_nmeta,nmeta,(int)stime, (int)etime,tmp_metadata);
        slice_observations (file_id, tot_nobs,nobs,(int)stime, (int)etime,tmp_observations);
        slice_origobservations (file_id, tot_norigobs,norigobs,(int)stime, (int)etime,tmp_origobservations);
        slice_feedback (file_id, tot_nfeed,nfeed,(int)stime, (int)etime,tmp_feedback);
        slice_obstype (file_id, tot_nobstype,nobstype,(int)stime, (int)etime,tmp_obstype);
        slice_origmetadata (file_id, tot_norigmeta,norigmeta,(int)stime, (int)etime,tmp_origmetadata);
        slice_source (file_id, tot_nsrc,nsrc,(int)stime, (int)etime,tmp_source);
        slice_tracking (file_id, tot_ntrack,ntrack,(int)stime, (int)etime,tmp_tracking);
        slice_presinst (file_id, tot_npres,npres,(int)stime, (int)etime,tmp_presinst);
        slice_corrections (file_id, tot_ncorr,ncorr,(int)stime, (int)etime,tmp_corrections);

        H5Fclose( file_id );
        printf ("Closed  %s\n", input_filename);

        size_t i = 0;
        for(i=0; i < nmeta; i++){
            int hour = tmp_metadata[i].hour;
       //     info_observations (h5files[hour], &nobs, &tot_nobs,(int)stime, (int)etime);
            status = append_metadata(h5files[hour],&(tmp_metadata[i]));
        } 
        if(status < 0) fprintf (stderr, "error: appending metadata\n");

        for(i=0; i < nmeta; i++){
            int hour = tmp_metadata[i].hour;
            status = append_obs (h5files[hour], &(tmp_observations[i]));
        }

        if(status < 0) fprintf (stderr, "error: appending origobservations\n");

        for(i=0; i < nmeta; i++){
            int hour = tmp_metadata[i].hour;
            status = append_feedback(h5files[hour],&(tmp_feedback[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending feedback");

        for(i=0; i < nmeta; i++){
            int hour = tmp_metadata[i].hour;
            status = append_obstype(h5files[hour],&(tmp_obstype[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending obstype\n");

        for(i=0; i < nmeta; i++){
            int hour = tmp_metadata[i].hour;
            status = append_origmetadata(h5files[hour],&(tmp_origmetadata[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending origmeatadata\n");

        for(i=0; i < nmeta; i++){
            int hour = tmp_metadata[i].hour;
            status = append_src(h5files[hour],&(tmp_source[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending source\n");

        for(i=0; i < nmeta; i++){
            int hour = tmp_metadata[i].hour;
            status = append_tracking(h5files[hour],&(tmp_tracking[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending tracking\n");

        for(i=0; i < nmeta; i++){
            int hour = tmp_metadata[i].hour;
            status = append_corrections(h5files[hour], &(tmp_corrections[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending corrections\n");

        for(i=0; i < nmeta; i++){
            int hour = tmp_metadata[i].hour;
            status = append_presinst(h5files[hour],&(tmp_presinst[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending presinst\n");

        for(i=0; i < nmeta; i++){
            int hour = tmp_metadata[i].hour;
            if(status >= 0 )status = append_origobs (h5files[hour], &(tmp_origobservations[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending origobservations\n");

        free(tmp_corrections);
        free(tmp_tracking);
        free(tmp_source);
        free(tmp_observations);
        free(tmp_origmetadata);
        free(tmp_presinst);
        free(tmp_metadata);
        free(tmp_obstype);
        free(tmp_feedback);
    }
    else
        printf (" no data found\n");
}

        
void construct_filenames (const char* outdir, const char* stime, const char* etime, filename files[]){
    size_t  i;
	               
    for(i=0; i < 24; i++){
        char text[3]; 
        sprintf(text, "%02i", i);
        strcpy(files[i].file,outdir);
        strcat(files[i].file, stime);
        strcat(files[i].file, text);
        strcat(files[i].file, ".h5");
    }

}

/*
void process_files (const char* dataset, const char* syear, const char* indir, hid_t h5files[], const int istime, const int ietime){
    int n = 0;
    size_t j = 0;
    fstream inFile;
    char field[80];
    char filenm[100];
    filename* files;

    strcpy(filenm, syear);
    strcat(filenm, "_");
    strcat(filenm, dataset);
    strcat(filenm, ".txt");
  
    inFile.open(filenm, ios::in);

    cout << filenm << endl;
    if (!inFile) {
        cerr << "Unable to open station files" << endl;
        exit(1);   // call system to stop
    }

    // let's store the values read off from the file
    while (inFile >> field) {
        //cout << field << endl;
        n++;
    }

    inFile.clear();
    inFile.seekg(ios::beg);

    files = new filename[n];

    while (inFile >>  field ) {
        files[j].file[0]='\0';
        strcat(files[j].file,indir);
        strcat(files[j].file, dataset);
        strcat(files[j].file, "/");
        strcat(files[j].file, field);
        j++;
    }

    for (size_t i=0; i < n; i++) {
        if(strlen(files[i].file) != 0){
            cout << files[i].file << endl; 
            rw_threedim_h5 ((size_t)istime, (size_t)ietime, files[i].file, h5files);
        }
    }

    delete [] files;
}
*/

void process_marinefiles (const char* input_filename, const size_t stime,  const size_t etime, hid_t h5files[][24]){
    unsigned flags=H5F_ACC_RDWR;
    herr_t status;
    hid_t file_id, access_plist;
    ObsType* tmp_obstype;
    OrigObservations* tmp_origobservations;
    Observations* tmp_observations;
    MetaData* tmp_metadata;
    OrigMetaData* tmp_origmetadata;
    Tracking* tmp_tracking;     
    Corrections* tmp_corrections;
    PresInst* tmp_presinst;
    NewFeedBack* tmp_feedback;
    FeedBack *feedback;
    SrcMarine* tmp_srcmarine;
    MiscMarine* tmp_miscmarine;
    size_t i;

    int sd = (int)stime % 100;
    int ed = (int)etime % 100;

    size_t nobs, nobstype,norigobs,nmeta,norigmeta,ncorr,npres,nfeed,nsrcmr,nmiscmr, ntrack;
    nobs=nobstype=norigobs=nmeta=norigmeta=ncorr=npres=nfeed=nsrcmr=nmiscmr=ntrack=0;
    hsize_t  tot_nobstype,tot_norigobs,tot_nmeta,tot_norigmeta,tot_nsrcmr,tot_nmiscmr,tot_ncorr,tot_npres,tot_nfeed,tot_nobs,tot_ntrack;    
    tot_nobstype=tot_norigobs=tot_nmeta=tot_norigmeta=tot_nsrcmr=tot_nmiscmr=tot_ncorr=tot_npres=tot_nfeed=tot_nobs=tot_ntrack=0;

    access_plist = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fclose_degree(access_plist, H5F_CLOSE_STRONG);

    printf ("opening up %s\n", input_filename);
    if(!(file_id = H5Fopen(input_filename,flags, access_plist))){
        fprintf (stderr, "Can't open %s\n", input_filename);
        exit(1);
    }

    printf ("getting info of data\n");
    info_observations (file_id, &nobs, &tot_nobs,(int)stime, (int)etime);    
    printf ("observations info done\n");
    info_origobservations (file_id, &norigobs, &tot_norigobs,(int)stime, (int)etime);
    printf ("original observations info done\n");
    info_obstype (file_id, &nobstype, &tot_nobstype,(int)stime, (int)etime);
    printf ("obstype info done\n");
    info_origmetadata (file_id, &norigmeta, &tot_norigmeta,(int)stime, (int)etime);    
    printf ("original metadata info done\n");
    info_metadata (file_id, &nmeta, &tot_nmeta,(int)stime, (int)etime);
    printf ("metadata info done\n");
    info_srcmarine (file_id, &nsrcmr, &tot_nsrcmr,(int)stime, (int)etime);
    printf ("src marine info done\n");
    info_miscmarine (file_id, &nmiscmr, &tot_nmiscmr,(int)stime, (int)etime);
    printf ("misc marine info done\n");
    info_tracking (file_id, &ntrack, &tot_ntrack,(int)stime, (int)etime);
    printf ("tracking info done\n");
    info_corrections (file_id, &ncorr, &tot_ncorr,(int)stime, (int)etime);
    printf ("corrections info done\n");
    info_presinst (file_id, &npres, &tot_npres,(int)stime, (int)etime);
    printf ("presinst info  done\n");

    info_newfeedback (file_id, &nfeed, &tot_nfeed,(int)stime, (int)etime);
    printf ("feedback info  done\n");

    if(nobs != nobstype || nobs != norigobs || nobs != nmeta || nobs != norigmeta ||  nobs != ncorr || nobs != npres || nobs != nfeed || nsrcmr != nmiscmr){
        fprintf (stderr, "Numbers of obs do not match\n");
        //fprintf (stderr, "nobs %i nobstype %i norigobs %i nmeta %i norigmeta %i ncorr %i npres %i nfeed %i",nobs,nobstype,norigobs,nmeta,norigmeta,ncorr,npres,nfeed);
        fprintf (stderr, "nobs %zd nobstype %zd norigobs %zd nmeta %zd norigmeta %zd ncorr %zd npres %zd nfeed %zd",nobs,nobstype,norigobs,nmeta,norigmeta,ncorr,npres,nfeed);
        //fprintf (stderr, "nsrcmr %i nmiscmr %i\n", nsrcmr,nmiscmr);
        fprintf (stderr, "nsrcmr %zd nmiscmr %zd\n", nsrcmr,nmiscmr);
        exit(1);
    }

   if(tot_nobs != tot_nobstype || tot_nobs != tot_norigobs || tot_nobs != tot_nmeta || tot_nobs != tot_norigmeta ||  tot_nobs != tot_nmiscmr || tot_nobs != tot_nmiscmr || tot_nobs != tot_ncorr || tot_nobs != tot_npres || tot_nobs != tot_nfeed){
        fprintf (stderr, "Numbers of total obs do not match for %s\n", input_filename);
        fprintf (stderr, "nobs %i nobstype %i norigobs %i nmeta %i norigmeta %i nsrcmr %i nmiscmr %i ncorr %i npres %i nfeed %i\n",(int)tot_nobs,(int)tot_nobstype,(int)tot_norigobs,(int)tot_nmeta,(int)tot_norigmeta,(int)tot_nsrcmr, (int)tot_nmiscmr,(int)tot_ncorr,(int)tot_npres,(int)tot_nfeed);
        H5Fclose( file_id );
        printf ("Closed  %s\n", input_filename);
        exit(1);
    }

    //printf ("%i obs found.  start writing\n", nmeta);
    printf ("%zd obs found.  start writing\n", nmeta);
    if(nmeta > 0) {
        tmp_metadata = malloc(sizeof(MetaData)*nmeta);
        tmp_observations =  malloc(sizeof(Observations)*nmeta);
        tmp_origobservations =  malloc(sizeof(OrigObservations)*nmeta);        
        tmp_feedback =  malloc(sizeof(NewFeedBack)*nmeta);
        tmp_obstype = malloc(sizeof(ObsType)*nmeta);        
        tmp_origmetadata =  malloc(sizeof(OrigMetaData)*nmeta);        
        tmp_tracking =  malloc(sizeof(Tracking)*nmeta);
        tmp_corrections =  malloc(sizeof(Corrections)*nmeta);
        tmp_presinst =  malloc(sizeof(PresInst)*nmeta);
        tmp_srcmarine =  malloc(sizeof(SrcMarine)*nmeta);
        tmp_miscmarine =  malloc(sizeof(MiscMarine)*nmeta);

        slice_metadata (file_id, tot_nmeta,nmeta,(int)stime, (int)etime,tmp_metadata);
        slice_observations (file_id, tot_nobs,nobs,(int)stime, (int)etime,tmp_observations);
        slice_origobservations (file_id, tot_norigobs,norigobs,(int)stime, (int)etime,tmp_origobservations);        
        slice_newfeedback (file_id, tot_nfeed,nfeed,(int)stime, (int)etime,tmp_feedback);      
	fill_newfeedback(tmp_feedback,nfeed);  
	slice_obstype (file_id, tot_nobstype,nobstype,(int)stime, (int)etime,tmp_obstype);
        slice_origmetadata (file_id, tot_norigmeta,norigmeta,(int)stime, (int)etime,tmp_origmetadata);
        slice_tracking (file_id, tot_ntrack,ntrack,(int)stime, (int)etime,tmp_tracking);
        slice_presinst (file_id, tot_npres,npres,(int)stime, (int)etime,tmp_presinst);
        slice_corrections (file_id, tot_ncorr,ncorr,(int)stime, (int)etime,tmp_corrections);
        slice_miscmarine (file_id, tot_nmiscmr,nmiscmr,(int)stime, (int)etime,tmp_miscmarine);
        slice_srcmarine (file_id, tot_nsrcmr,nsrcmr,(int)stime, (int)etime,tmp_srcmarine);

       size_t i = 0;
        for(i=0; i < nmeta; i++){
            int day = tmp_metadata[i].day;
            int hour = tmp_metadata[i].hour;
            status = append_metadata(h5files[day][hour],&(tmp_metadata[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending metadata\n");

        for(i=0; i < nmeta; i++){
            int day = tmp_metadata[i].day;
            int hour = tmp_metadata[i].hour;
            status = append_obs (h5files[day][hour], &(tmp_observations[i]));
        }

        for(i=0; i < nmeta; i++){
            int day = tmp_metadata[i].day;
            int hour = tmp_metadata[i].hour;
            if(status >= 0 )status = append_origobs (h5files[day][hour], &(tmp_origobservations[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending origobservations\n");

        for(i=0; i < nmeta; i++){
            int day = tmp_metadata[i].day;
            int hour = tmp_metadata[i].hour;
            status = append_newfeedback(h5files[day][hour],&(tmp_feedback[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending feedback");

        for(i=0; i < nmeta; i++){
            int day = tmp_metadata[i].day;
            int hour = tmp_metadata[i].hour;
            status = append_obstype(h5files[day][hour],&(tmp_obstype[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending obstype\n");

        for(i=0; i < nmeta; i++){
            int day = tmp_metadata[i].day;
            int hour = tmp_metadata[i].hour;
            status = append_origmetadata(h5files[day][hour],&(tmp_origmetadata[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending origmeatadata\n");

        for(i=0; i < nmeta; i++){
            int day = tmp_metadata[i].day;
            int hour = tmp_metadata[i].hour;
            status = append_miscmar(h5files[day][hour],&(tmp_miscmarine[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending miscmarine\n");

        for(i=0; i < nmeta; i++){
            int day = tmp_metadata[i].day;
            int hour = tmp_metadata[i].hour;
            status = append_srcmar(h5files[day][hour],&(tmp_srcmarine[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending miscmarine\n");

        for(i=0; i < nmeta; i++){
            int day = tmp_metadata[i].day;
            int hour = tmp_metadata[i].hour;
            status = append_tracking(h5files[day][hour],&(tmp_tracking[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending tracking\n");

        for(i=0; i < nmeta; i++){
            int day = tmp_metadata[i].day;
            int hour = tmp_metadata[i].hour;
            status = append_corrections(h5files[day][hour], &(tmp_corrections[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending corrections\n");


        for(i=0; i < nmeta; i++){
            int day = tmp_metadata[i].day;
            int hour = tmp_metadata[i].hour;
            status = append_presinst(h5files[day][hour],&(tmp_presinst[i]));
        }
        if(status < 0) fprintf (stderr, "error: appending presinst\n");

        free(tmp_corrections);
        free(tmp_tracking);
        free(tmp_miscmarine);
        free(tmp_srcmarine);
        free(tmp_observations);
        free(tmp_origobservations);
        free(tmp_origmetadata);
        free(tmp_presinst);
        free(tmp_metadata);
        free(tmp_obstype);
        free(tmp_feedback);
    }
    else
        printf (" no data found\n");

    /* Close the input file. */
    H5Fclose( file_id );
    printf ("Closed  %s\n", input_filename);
}

void write_threedim_h5 (hid_t file, Observations* observations, OrigObservations* origobservations,FeedBack* feedback, ObsType* obstype,OrigMetaData* origmetadata, MetaData* metadata, Source* source, Tracking* tracking, Corrections* corrections, PresInst* presinst) {
    herr_t status = 0;

    printf ( "appending data\n");
    status = append_obs (file, observations);

    if(status < 0) fprintf (stderr, "error: appending observations\n");
    if(status >= 0 )status = append_origobs (file, origobservations);
    if(status < 0) fprintf (stderr, "error: appending origobservations\n");
    if(status >= 0) status = append_corrections(file, corrections);
    if(status < 0) fprintf (stderr, "error: appending corrections\n");
    if(status >= 0) status = append_feedback(file,feedback);
    if(status < 0) fprintf (stderr, "error: appending feedback\n");
    if(status >= 0) status = append_metadata(file,metadata);
    if(status < 0) fprintf (stderr, "error: appending metadata\n");
    if(status >= 0) status = append_obstype(file,obstype);
    if(status < 0) fprintf (stderr, "error: appending obstype\n");
    if(status >= 0) status = append_src(file,source);
    if(status < 0) fprintf (stderr, "error: appending source\n");
    if(status >= 0) status = append_tracking(file,tracking);
    if(status < 0) fprintf (stderr, "error: appending tracking\n");
    if(status >= 0) status = append_presinst(file,presinst);
    if(status < 0) fprintf (stderr, "error: appending presinst\n");
    if(status >= 0) status = append_origmetadata(file,origmetadata);
    if(status < 0) fprintf (stderr, "error: appending origmeatadata\n");
}

void sleeping () {
   size_t i;
    for(i=0; i < 100000; i++)
        ;
}

void append_missing_mar_track (const size_t stime,  const size_t etime, hid_t h5files[][24]){
    unsigned flags=H5F_ACC_RDWR;
    herr_t status;
    hid_t file_id, access_plist;
    size_t d = 0;
    size_t h = 0;

    for(d = stime; d <= etime; d++) { 
        for(h=0; h< 24; h++) {
            MetaData* tmp_meta;
            ObsType* tmp_obstype;

            int day = (int)d % 100;

            size_t ntrack, nmeta, nobstype;
            ntrack=nmeta=nobstype=0;
            hsize_t  tot_ntrack, tot_nmeta, tot_nobstype;    
            tot_ntrack=tot_nmeta=0;

            info_tracking (h5files[day][h], &ntrack, &tot_ntrack,(int)stime, (int)etime);
            //printf ("tracking info done\n");
            info_metadata (h5files[day][h], &nmeta, &tot_nmeta,(int)stime, (int)etime);
            //printf ("metadata info done\n");
            info_obstype (h5files[day][h], &nobstype, &tot_nobstype,(int)stime, (int)etime);
            //printf ("obstypes info done\n");

            if(nobstype != nmeta){
                //fprintf(stderr, "Nobstype %i Nmeta %i do not match\n", nobstype, nmeta);
                fprintf(stderr, "Nobstype %zd Nmeta %zd do not match\n", nobstype, nmeta);
                continue;
            }
            if(nmeta != ntrack) {
                //fprintf (stderr, "Numbers of obs do not match nmeta %i ntrack %i\n", nmeta, ntrack);
                fprintf (stderr, "Numbers of obs do not match nmeta %zd ntrack %zd\n", nmeta, ntrack);
                tmp_meta = malloc(sizeof(MetaData)*nmeta);
                tmp_obstype = malloc(sizeof(ObsType)*nobstype);
                slice_metadata (h5files[day][h], tot_nmeta,nmeta,(int)stime, (int)etime,tmp_meta);
                slice_obstype (h5files[day][h], tot_nobstype,nobstype,(int)stime, (int)etime,tmp_obstype);
                size_t i, j;
                j=0;
                for(i=0; i < nmeta; i++){
                    int day = tmp_meta[i].day;
                    int hour = tmp_meta[i].hour;
                    if(tmp_obstype[i].ncep_type == 180) {
                        j++; 
                    }
                }
                if(j != (nmeta-ntrack)){
                    //fprintf (stderr, "Numbers of obs do not match found %i nmeta-ntrack %i\n", j, nmeta-ntrack);
                    fprintf (stderr, "Numbers of obs do not match found %zd nmeta-ntrack %zd\n", j, nmeta-ntrack);
                    continue;
                }
       
                for(i=0; i < nmeta; i++){
                    int day = tmp_meta[i].day;
                    int hour = tmp_meta[i].hour;
                    if(tmp_obstype[i].ncep_type == 180) {
                        Tracking track;
                        strcpy(track.sname, "999999999999999999999999999999");
                        strcpy(track.slib, "000");
                        strcpy(track.timestamp,tmp_meta[i].timestamp);
                        strcpy(track.unoc,tmp_meta[i].unoc);
                        //printf("%s %s\n", track.timestamp, track.unoc);
                        status = append_tracking(h5files[day][hour], &track);
                        //if(status < 0) fprintf (stderr, "error: appending tracking: day %i hour%i\n", day, hour);
                    }
                }
                free(tmp_meta);
                free(tmp_obstype);
            }

        }
    }
}


// This verssion overwrites on pre-existing ones
herr_t process_dataindex(hid_t h5file){
    herr_t status;
    hsize_t nfields_out, total;
    status=H5TBget_table_info (h5file,"/TableOfOriginalSources/DatasetIndex", &nfields_out, &total);

    hsize_t n_add = 40 - total;

    DatasetIndex* add_index;
    add_index = malloc(sizeof(DatasetIndex) * (int) n_add);


    size_t dst_size =  sizeof( DatasetIndex );
    size_t dst_offset[NFIELDS8] = { HOFFSET( DatasetIndex, cdcid ), // 10
                                    HOFFSET( DatasetIndex, name ),     // 100
                                    HOFFSET( DatasetIndex, description ), // 100
                                    HOFFSET( DatasetIndex, period ), // 20
                                    HOFFSET( DatasetIndex, ncdcref ), // 20
                                    HOFFSET( DatasetIndex, ncarref ), // 20
                                    HOFFSET( DatasetIndex, contact ),   // 100
                                    HOFFSET( DatasetIndex, url )};   // 200

    size_t dst_sizes[NFIELDS8] = { sizeof(char)*10,
                               sizeof(char)*100,
                               sizeof(char)*100,
                               sizeof(char)*20,
                               sizeof(char)*20,
                               sizeof(char)*20,
                               sizeof(char)*100,
                               sizeof(char)*200};

    if((int) n_add > 0)  // add empty records
        status = H5TBappend_records(h5file, "/TableOfOriginalSources/DatasetIndex", n_add, dst_size, dst_offset, dst_sizes, add_index);

    DatasetIndex p_data[NRECORDS_DSETINDEX] = {{"0106","ICOADS Release 2.5.1i","Global Marine Surface Observations","1662-2014","","DS540.0","Scott.Woodruff@noaa.gov","" },
        {"0800","ICOADS Auxiliary German Maury","Without IGADS temperature correction","1845-1868","","","Sandra.Lubker@noaa.gov","" },
        {"0900","ACRE Marine Data","Early Expeditions","1699-1910","","","Philip.Brohan@metoffice.gov.uk","" },
        {"0901","ACRE Marine Data","English East India Company","1789-1834","","","Philip.Brohan@metoffice.gov.uk","" },
        {"0902","OLD WEATHER Data","Old Weather.org","1913-1941","","","Philip.Brohan@metoffice.gov.uk","" },
        {"0903","WEATHER Detectives","Weather Detectives","1889-1899","","","Philip.Brohan@metoffice.gov.uk","" },
        {"1000","Federal Climate Complex Integrated Surface Hourly","Global Land Surface Observations","1860-2014","3505","DS463.3", "Neal.Lott@noaa.gov",""},
        {"8001","IBTrACS","International Best Track Archive for Climate and Stewardship assembled by NOAA NCDC", "1848-2013","","", "michael.kruk@noaa.gov",""}};

    status=H5TBwrite_records(h5file, "/TableOfOriginalSources/DatasetIndex", (hsize_t)0, (hsize_t)NRECORDS_DSETINDEX, dst_size, dst_offset,
                dst_sizes, p_data);

}


herr_t make_datasetindex (hid_t loc_id) {
    size_t dst_size =  sizeof( DatasetIndex );
    size_t dst_offset[NFIELDS8] = { HOFFSET( DatasetIndex, cdcid ), // 10
                                    HOFFSET( DatasetIndex, name ),     // 100
                                    HOFFSET( DatasetIndex, description ), // 100
                                    HOFFSET( DatasetIndex, period ), // 20
                                    HOFFSET( DatasetIndex, ncdcref ), // 20
                                    HOFFSET( DatasetIndex, ncarref ), // 20
                                    HOFFSET( DatasetIndex, contact ),   // 100
                                    HOFFSET( DatasetIndex, url )};   // 200

    DatasetIndex p_data[NRECORDS_DSETINDEX] = {
        {"0106","ICOADS Release 2.5.1i","Global Marine Surface Observations","1662-2014","","DS540.0","Scott.Woodruff@noaa.gov","" },
        {"0800","ICOADS Auxiliary German Maury","Without IGADS temperature correction","1845-1868","","","Sandra.Lubker@noaa.gov","" },
        {"0900","ACRE Marine Data","Beagle Vincennes Astrolabe","1831-1842","","","Philip.Brohan@metoffice.gov.uk","" },
        {"0900","ACRE Marine Data","English East India Compancy","1829-1834","","","Philip.Brohan@metoffice.gov.uk","" },
        {"1000","Federal Climate Complex Integrated Surface Hourly","Global Land Surface Observations","1901-2005","3505","DS463.3", "Neal.Lott@noaa.gov",""},
        {"8001","IBTrACS","International Best Track Archive for Climate and Stewardship assembled by NOAA NCDC", "1848-2007","","", "michael.kruk@noaa.gov",""}};
    

    /* Define field information */
    const char *field_names[NFIELDS8]  =  {"CDC ID","Name","Description","PERIOD","NCDC Ref", "NCAR Ref", "Contact","URL"};
    hid_t      field_type[NFIELDS8];
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 1;
    herr_t     status;
    hid_t      string_type11,string_type21, string_type100, string_type200;

    //EXAMPLE("make a table DatasetIndex\n");
    string_type11 = H5Tcopy( H5T_C_S1 );
    string_type21 = H5Tcopy( H5T_C_S1 );
    string_type100 = H5Tcopy( H5T_C_S1 );
    string_type200 = H5Tcopy( H5T_C_S1 );
    H5Tset_size( string_type11, 11 );
    H5Tset_size( string_type21, 21 );
    H5Tset_size( string_type100, 100 );
    H5Tset_size( string_type200, 200 );

    field_type[0] = string_type11;
    field_type[1] = string_type100;
    field_type[2] = string_type100;
    field_type[3] = string_type21;
    field_type[4] = string_type21;
    field_type[5] = string_type21;
    field_type[6] = string_type100;
    field_type[7] = string_type200;

    status=H5TBmake_table( "DatasetIndex", loc_id, "DatasetIndex",(hsize_t) NFIELDS8, (hsize_t)NRECORDS_DSETINDEX, dst_size,
                            field_names, dst_offset, field_type,
                            chunk_size, fill_data, compress, p_data  );
    return status;
}
                        
