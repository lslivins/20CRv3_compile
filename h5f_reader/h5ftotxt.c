/*
ispdb_data.h defines ispdb h5 data types
ispdb2.h is a header file for PSD h5 API's
nmtools.h is a header file from Nobuki Matsui's personal toolbox
struct tm and time_t are defined in time.h c header
stdlib.h provides c string functions
stdio.h provides basic c IO routines

Time Arithmetic tool
StrfTime.h and def.h are from http://www.gdargaud.net/Hack/SourceCode.html 
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include "nmtools.h"    // Nobuki's tool box$$
#include "ispdb2.h"     // ISPDB tools
#include "ispdb_data.h" // ISPDB data defined
#include "hdf5.h"
#include "hdf5_hl.h"
#include <string.h>


// Constants
#define Pi              3.1415926535897932384626433832795028841971

// function prototype declaration
static void cleanup (char* file);
void create_h5_filename(char* analtime, int diff, char* path); 
void int_char (int n, char* buf );
void int_char_nozero (int n, char* buf ); 
void split_date(const int timestamp, int* yr, int* mon, int* dy, int* hr);
void split_longdate(const long  timestamp, int* yr, int* mon, int* dy, int* hr, int* min);
void call_system(char* commandline);
int make_time(char* yyyymmddhh, int h); 
void make_tm(long int* Time, const int yr, const int mon, const int dy, const int hr, const int min); 
int return_itime(const int yr, const int mon, const int dy, const int hr);
long int return_ifulltime(const int yr, const int mon, const int dy, const int hr, const int min);
long diff_in_sec(const long int time1, const long int time2);
void print_dat (char* filename, int analtime); 
void chk_bigfloat (float* a, float b);
void chk_bigdouble (double* a, double b);

// MAIN PROGRM
// envp is for getenv
int main (int argc, char *argv[], char *envp[]) {
    //char datapath[200];
    char* datapath;
    char filemthree[100];
    char filemtwo[100];
    char filemone[100];
    char fileanal[100];
    char filepone[100];
    char fileptwo[100];
    char analdate[11];
    int ianaltime;

    if (argc < 2){
        fprintf (stderr,"Usage: h5totxt  <analtime>\n");
        exit(1);
    }

    if(!(datapath = getenv("obsdirh5"))){
        fprintf (stderr,"env value obsdirh5 not found\n");
        exit(1);
    }


    strcpy(filemthree, datapath); 
    strcpy(filemtwo, datapath); 
    strcpy(filemone, datapath); 
    strcpy(fileanal, datapath); 
    strcpy(filepone, datapath); 
    strcpy(fileptwo, datapath); 


    // get analysis date (command line parameter YYYYMMDDHH)
    strcpy(analdate,argv[1]);
    create_h5_filename(analdate, 0, fileanal); 
    create_h5_filename(analdate, -3, filemthree); 
    create_h5_filename(analdate, -2, filemtwo); 
    create_h5_filename(analdate, -1, filemone); 
    create_h5_filename(analdate, 1, filepone); 
    create_h5_filename(analdate, 2, fileptwo);


    ianaltime = make_time(analdate,0);

    //print_dat(filemthree, ianaltime);
    //print_dat(filemtwo, ianaltime);
    //print_dat(filemone, ianaltime);
    print_dat(fileanal, ianaltime);
    //print_dat(filepone, ianaltime);
    //print_dat(fileptwo, ianaltime);

    return 0;
}

// FUNCTIONS
static void cleanup (char* file){
    if ( file != 0 ) {
        //printf("cleaning up temp. file %s\n", file);
        unlink(file);
        //free(file);
    }
}

void create_h5_filename(char* analtime, int diff, char* path) {
    int ianaltime = make_time(analtime, diff);
    char buf[11];
    char yr_buf[5];
    char mon_buf[3];

    int yr, mon, dy, hr;
    split_date(ianaltime, &yr,&mon,&dy,&hr);
    int_char(yr,yr_buf);
    int_char(mon,mon_buf);
    int_char(ianaltime, buf); 
    strcat(path, "/");
    strcat(path, yr_buf);
    strcat(path, "/");
    strcat(path, mon_buf);
    strcat(path, "/");
    strcat(path, buf);
    strcat(path, ".h5");
}

void int_char (int n, char* buf ) {
    sprintf(buf, "%02i\0", n);
}

void int_char_nozero (int n, char* buf ) {
    sprintf(buf, "%i", n);
}

void split_date(const int timestamp, int* yr, int* mon, int* dy, int* hr){
    *hr = timestamp % 100;
    *dy = ((timestamp - (*hr)) %10000 )/ 100;
    *mon = ((timestamp - (*hr) - (*dy)*100)/10000) %100;
    *yr = ((timestamp - (*hr) - (*dy)*100 - (*mon)*10000) /1000000) % 1000000;
}

void split_longdate(const long  timestamp, int* yr, int* mon, int* dy, int* hr, int* min){
    *min = timestamp % 100;
    *hr = ((timestamp - (*min))%10000)/100;
    *dy = ((timestamp - (*min) - (*hr)*100)/10000) % 100;
    *mon = ((timestamp - (*min) - (*hr)*100 - (*dy)*10000)/1000000) % 100;
    *yr = ((timestamp - (*min) - (*hr)*100 - (*dy)*10000 - (*mon)*100000) /100000000);
}

void call_system(char* commandline){
    int rc;

    errno = 0;

    rc = system(commandline);
 
    if( rc == 127 && errno != 0 ){
        // command line failed to start
        fprintf(stderr, "%s starting system(%s)\n", strerror(errno), commandline);
    }
    else if ( rc == -1 ) {  
        // other error occured
        fprintf (stderr, "%s: system(%s)\n", strerror(errno), commandline);
    } 
}

int make_time(char* yyyymmddhh, int h) {
    #define STRING_LENGTH_TS 11
    char ST[11];
    char NEWST[11];
    char str_h[3];
    char commandline[120];
    char cm[100];
    int newtimestamp;
    int rc;
    int timestamp = string2int(yyyymmddhh);
    FILE* file;
    int flag = 0;

    char sfn[50];
    char* user;
    int fd = -1;

    char* datapath;


    strcpy(sfn, "/tmp/");
    strcat(sfn, "/reanl.XXXXXX");

    if ((fd = mkstemp(sfn)) == -1){
        fprintf(stderr, "%s: generating a temp file name.\n", strerror(errno));
        abort();
    }

    strcpy(cm, "touch ");
    strcat(cm, sfn);
    call_system(cm);

    strcpy(commandline, "incdate ");
    int_char(h, str_h);
    strcat(commandline, yyyymmddhh);
    strcat(commandline, " ");
    strcat(commandline, str_h);
    strcat(commandline, " > ");
    strcat(commandline, sfn);

    call_system(commandline);

    if((file = fopen(sfn, "r")) == NULL){
        fprintf (stderr, "error opening r file %s\n", sfn);
        exit(1);
    }

    while ( ! feof(file) && ! ferror(file))
        while( fgets(ST, STRING_LENGTH_TS, file) != NULL && flag == 0){
            flag = 1;
            strcpy(NEWST, ST);
        }


    fclose(file);

    cleanup(sfn);

    return string2int(NEWST);
}



void make_tm (long int* Time, const int yr, const int mon, const int dy, const int hr, const int min){
    int nyr = yr;
    int nmin = min;
        
    if(nyr >= 2000) {
        nyr = 100 + nyr % 100;
    }   
    else  {
        nyr =  nyr % 100;
    }   

    if(nmin > 60) 
        nmin = 0;

    *Time = return_ifulltime(yr,mon,dy,hr,min);
}

int return_itime(const int yr, const int mon, const int dy, const int hr){
    return yr * 1000000 + mon * 10000 + dy * 100 + hr; 
}

long int return_ifulltime(const int yr, const int mon, const int dy, int hr, int min){
    return  (long int) yr * 100000000 + (long int) mon * 1000000 + (long int) dy * 10000 + (long int) hr * 100 + (long int) min; 
}

// return difference in second between time1 and time2
// It might be inefficient to splitting back long int here again
// but for the sake of consistency....
long diff_in_sec(const long int time1, const long int time2){
    #define DIM 500
    int status, dutc1, tai1, dutc2, tai2;
    double secs1, secs2;
    char str1[18], str2[18];
    char pref[4];
    int yr1, yr2, mon1, mon2, dy1, dy2, hr1, hr2, min1, min2;
    char syr1[5],syr2[5],smon1[3],smon2[3],sdy1[3],sdy2[3],shr1[3],shr2[3],smin1[3],smin2[3];

    split_longdate(time1, &yr1,&mon1,&dy1,&hr1,&min1);
    split_longdate(time2, &yr2,&mon2,&dy2,&hr2,&min2);

    return fulljulday(mon1,dy1,yr1,hr1,min1) - fulljulday(mon2,dy2,yr2,hr2,min2); 
}

void print_dat (char* filename, int analtime) {
    #define DIM 500
    char ST[DIM];
    herr_t status;
    hsize_t nfields_out, nobs, nobstype, ntrack, nmeta, nfeed;
    Observations* observations;
    ObsType*  obstype;
    NewFeedBack* feedback;
    Tracking* track;
    MetaData* meta;
    float ob;
    char obtype = 'P';
    int elevation;
    hid_t h5file;
    size_t i;
    long int obtime;
    int yr, mon, dy, hr; 
    long int tanaltime;
    float analtime_offset;
    char sname[31];
    int j;

    nfields_out = nobs = nobstype = ntrack = nmeta = nfeed = 0;

    // PSD H5 API call
    h5file = ropen_h5file(filename);

    // get n of data for each table we ignore nfields_out 
    status=H5TBget_table_info (h5file,"/Data/Observations/Observations", &nfields_out, &nobs);
    status=H5TBget_table_info (h5file,"/SupplementalData/Tracking/Tracking", &nfields_out, &ntrack);
    status=H5TBget_table_info (h5file,"/Data/Observations/ObservationTypes", &nfields_out, &nobstype);
    status=H5TBget_table_info (h5file,"/Data/SpatialTemporalLocation/OriginalSpatialTemporalLocation/OriginalSpatialTemporalLocation", &nfields_out, &nmeta);
    status=H5TBget_table_info (h5file,"/Data/AssimilationFeedback/AssimilationFeedBack", &nfields_out, &nfeed);

    // sanity check
    if(nobs != nobstype || nobs != nmeta){
        fprintf (stderr,"%i nobs do not match obs: %i track: %i obstypes: %i location: %i\n", analtime, nobs, ntrack, nobstype, nmeta);
        exit(1);
    }
    
    // allocate memory for each table     
    observations = malloc(sizeof(Observations)*nobs);
    track = malloc(sizeof(Tracking)*ntrack);
    obstype = malloc(sizeof(ObsType)*nobstype);
    meta = malloc(sizeof(MetaData)*nobstype);


    // PSD H5 API call let's grab data
    get_observations(h5file, observations);
    get_metadata(h5file, meta);
    get_obstype(h5file,obstype);
    get_tracking(h5file,track);

    // want to fill with missing values
    // then overwrite with found values
    //if(nfeed == 0) {
        feedback = malloc(sizeof(NewFeedBack)*nobs);
        for(i=0; i < nobs; i++) {
            strcpy(feedback[i].unoc, obstype[i].unoc);
	    strcmp(feedback[i].timestamp, obstype[i].timestamp);
	    feedback[i].sfsfp = 9;
	    feedback[i].ai = 9;
	    feedback[i].uc = 9;
	    feedback[i].bcf = 9;
	    feedback[i].bf = 9;
	    feedback[i].qc = 9;

	    feedback[i].mdpavims = 9999.99;
	    feedback[i].epvims = -9.99;
	    feedback[i].emfg = 9999.99;
	    feedback[i].sdeg = -9.99;
	    feedback[i].mpmemfg = 999.99;
	    feedback[i].emap = 9999.99;
	    feedback[i].sdeap = -9.99;
	    feedback[i].mpmema = 999.99;
	    feedback[i].melv = 9999;
	    feedback[i].bias = 0.00;
        }

    //}
    //else {
    //    feedback = malloc(sizeof(NewFeedBack)*nfeed);
        get_newfeedback(h5file,feedback);
    //}


    // for each data
    for(i=0; i < nobs; i++) {
        char obid[20];
	int sid, dck, pt; 
        int track_match = 0;


        // sanity check, compare unique observation ID
        if((strcmp(observations[i].unoc, obstype[i].unoc) != 0) || (strcmp(observations[i].unoc, meta[i].unoc))) {
            fprintf (stderr,"Unique ID do not match obs: %s track: %s obstypes: %s location: %s\n", observations[i].unoc, track[i].unoc, obstype[i].unoc, meta[i].unoc);
            exit(1);
        }

        elevation = meta[i].elv;

        // determine whch pressure and err to print
        // and take care of elevation for slp

        if (obstype[i].ncep_type == 180){
            ob = observations[i].slp;
            elevation = 0;  
        }
        else if (obstype[i].ncep_type == 183) {
            ob = observations[i].slp;
            elevation = 0;
        }
        else if (obstype[i].ncep_type >= 300) {
            ob = observations[i].slp;
            elevation = 0;
        }
        else{
            ob = observations[i].sfp;
        }

        // sanity check for ps ob.        
        if (ob <= 400 ||  ob >= 1200 )  { 
            //printf("ERROR %s %s %s %f %f abnormal ob %f\n", observations[i].unoc, meta[i].timestamp, track[j].sname, meta[i].lat, meta[i].lon, ob);
            continue;
        }
        // check for missing orography    
        if (elevation > 9998) {
            //printf("ERROR %s %s abnormal ob %i\n", observations[i].unoc, meta[i].timestamp, elevation);
            continue;
        }

        // check for non physical coordinates
        if(meta[i].lat < -90.0 || meta[i].lat > 90.0){
            //printf("ERROR %s %s abnormal lat %f\n", observations[i].unoc, meta[i].timestamp, meta[i].lat);
            continue;
        }
        if(meta[i].lon < 0.0 || meta[i].lon > 360.0){
            //printf("ERROR %s %s abnormal lon %f\n", observations[i].unoc, meta[i].timestamp, meta[i].lon);
            continue;
        }


        // exclude non atlantic centerfix excel cooridnates
        if(obstype[i].ncep_type >= 430 && obstype[i].ncep_type < 440)
            if(meta[i].lon < 240.0){
              //  printf("ERROR %s %s abnormal atlantic lon %f\n", observations[i].unoc, meta[i].timestamp, meta[i].lon);
                continue;
            }


        if(meta[i].minute == 99)
             make_tm(&obtime, meta[i].year, meta[i].month, meta[i].day, meta[i].hour, 0);
        else
             make_tm(&obtime, meta[i].year, meta[i].month, meta[i].day, meta[i].hour, meta[i].minute);


        strcpy(obid, meta[i].timestamp);
   
        split_date(analtime, &yr,&mon,&dy,&hr);
        // analtime yyyymmddhh
        make_tm(&tanaltime, yr,mon,dy,hr,0);
        // tanaltime yyyymmddhhmm

        // difftime returns in seconds
       
        analtime_offset =  (float) diff_in_sec(obtime,tanaltime)/3600;

        // create ID
        strcat(obid, meta[i].unoc);

        //because there are some non-unique ids just do a sanity check for the ob, don't search again for a match
        //for(j=0; j < ntrack; j++) {
        //    if((strcmp(track[j].unoc, observations[i].unoc) == 0 )&& (strcmp(track[j].timestamp,observations[i].timestamp)== 0)) {

            if((strcmp(track[i].unoc, observations[i].unoc) == 0 )&& (strcmp(track[i].timestamp,observations[i].timestamp)== 0)) {
                 track_match = 1;
        //        strcpy(sname, track[j].sname);
                 strcpy(sname, track[i].sname);
        //        break;
             }
        //}
        if(track_match == 0) 
            strcpy(sname, "999999999999999999999999999999");

       
        /*
            double            mdpavims;  // Modified Observed Pressure after Vertically Interpolating to Model Surface
            float             epvims;    // Error in Observed Pressure vertically Interpolated to Model Surface
            double      bias;      // difference between observation and analysis averaged over past sixty days
            unsigned int      sfsfp;     // Status Flag for Observed Surface Pressure
            unsigned int      ai;        // Assimilation indicator
            unsigned int      uc;        // Usability Check for Reanalysis
            unsigned int      bcf;       // QC Background Check Flag Indicator
            unsigned int      bf;        // Buddy Flag Indicator
            unsigned int      qc;        // QC Control Indicator
            double            emfg;      // Ensemble Mean First Guess Pressure
            double            sdeg;      // Standard Deviation of Ensemble Guess Pressure
            double            mpmemfg;   // Modified Observatio Pressure minus Ensemble Mean First Guess Pressure
            double            emap;      // Ensemble Mean Analysis Pressure
            double            sdeap;     // Standard Deviation of Ensemble Analysis Pressure
            double            mpmema;    // Modified Observation Pressure minus Ensemble Mean Analysis Pressure

        */
        chk_bigdouble(&feedback[i].mdpavims, 9999.99);
        chk_bigfloat(&feedback[i].epvims, -9.99);
        chk_bigdouble(&feedback[i].emfg, 9999.99);
        chk_bigdouble(&feedback[i].sdeg, -9.99);
        chk_bigdouble(&feedback[i].mpmemfg, 999.99);
        chk_bigdouble(&feedback[i].emap, 9999.99);
        chk_bigdouble(&feedback[i].sdeap, -9.99);
        chk_bigdouble(&feedback[i].mpmema, 999.99);
      
        if(feedback[i].bias > 10000)
	    feedback[i].bias = 0.00;
        printf ("%-19s %3i %01c %7.2f %6.2f %5i %6.2f %8.2f ",obid,obstype[i].ncep_type,obtype,meta[i].lon,meta[i].lat,elevation,analtime_offset,ob);
        printf ("%7.2f %5.2f %7.2f %1d %1d %1d %1d %1d %1d ", feedback[i].mdpavims, feedback[i].epvims, feedback[i].bias, feedback[i].sfsfp,feedback[i].ai,feedback[i].uc,feedback[i].bcf,feedback[i].bf,feedback[i].qc);
        printf ("%7.2f %5.2f %6.2f %7.2f %5.2f %6.2f %4d ", feedback[i].emfg,feedback[i].sdeg,feedback[i].mpmemfg,feedback[i].emap,feedback[i].sdeap, feedback[i].mpmema, feedback[i].melv);
        printf ("%30s %13s ",sname,meta[i].id);
        printf ("\n");

	/*
	    int               sid;           // Source ID
	    int               dck;           // Deck ID
	    int               pt;            // Platform
	*/

    }

    // releasing resources
    free(observations);
    free(track);
    free(obstype);
    free(meta);
    free(feedback);
    H5Fclose(h5file);
}

void chk_bigfloat (float* a, float b ){
    if(*a > 1100)
        *a = b; 
}

void chk_bigdouble (double* a , double b){
    if(*a > 1100)
        *a = b; 
}


