// Header file ispdb_data.h
//
// This header file contains datatypes necessary to create
// ISPDB tables in h5.
//
// March 2, 2006 
//
// Written by Nobuki Matsui
// CIRES/PSD/ESRL/NOAA
//
#ifndef ISPDB_DATA_H
#define ISPDB_DATA_H

#include <hdf5.h>
// #include <H5LT.h>
// #include <H5TA.h>


#define NRECORDS_DSETINDEX 46
#define NRECORDS 1
#define NFIELDS 1
#define NFIELDS2 2
#define NFIELDS3 3
#define NFIELDS4 4
#define NFIELDS5 5
#define NFIELDS6 6
#define NFIELDS7 7
#define NFIELDS8 8
#define NFIELDS9 9
#define NFIELDS10 10
#define NFIELDS11 11
#define NFIELDS12 12
#define NFIELDS13 13
#define NFIELDS14 14
#define NFIELDS15 15
#define NFIELDS16 16
#define NFIELDS17 17
#define NFIELDS18 18
#define COLS 1
#define DAYS 1

typedef struct NewFeedBack {
   char      timestamp[13]; 
   char              unoc[8];   // Unique Observation Code
   double            mdpavims;  // Modified Observed Pressure after Vertically Interpolating to Model Surface
   float             epvims;    // Error in Observed Pressure vertically Interpolated to Model Surface  
   double      bias;      // difference between observation and analysis averaged over past sixty days
   unsigned int      sfsfp;     // Status Flag for Observed Surface Pressure
   unsigned int      ai;        // Assimilation indicator
   unsigned int      uc;        // Usability Check for Reanalysis
   unsigned int      bcf;       // QC Background Check Flag Indicator
   unsigned int      bf;        // Buddy Flag Indicator
   unsigned int      qc;        // QC Control Indicator
   double            emfg;      // Ensemble Mean Guess Pressure
   double            sdeg;      // Standard Deviation of Ensemble First Guess Pressure
   double            mpmemfg;   // Modified Observatio Pressure minus Ensemble Mean First Guess Pressure
   double            emap;      // Ensemble Mean Analysis Pressure
   double            sdeap;     // Standard Deviation of Ensemble Analysis Pressure
   double            mpmema;    // Modified Observation Pressure minus Ensemble Mean Analysis Pressure
   int               melv;      // Modified Elevation
} NewFeedBack;

typedef struct FeedBack {
   char      timestamp[13]; 
   char              unoc[8];   // Unique Observation Code
   double            mdpavims;  // Modified Observed Pressure after Vertically Interpolating to Model Surface
   float             epvims;    // Error in Observed Pressure vertically Interpolated to Model Surface  
   unsigned int      sfsfp;     // Status Flag for Observed Surface Pressure
   unsigned int      dc;        // Duplicate Check for Reanalysis
   unsigned int      ai;        // Assimilation indicator
   unsigned int      bcf;       // QC Background Check Flag Indicator
   unsigned int      bf;        // Buddy Flag Indicator
   double            emfg;      // Ensemble Mean First Guess Pressure
   double            sdeg;      // Standard Deviation of Ensemble Guess Pressure
   double            mpmemfg;   // Modified Observatio Pressure minus Ensemble Mean First Guess Pressure
   double            emap;      // Ensemble Mean Analysis Pressure
   double            sdeap;     // Standard Deviation of Ensemble Analysis Pressure
   double            mpmema;    // Modified Observation Pressure minus Ensemble Mean Analysis Pressure
} FeedBack;

typedef struct ObsType {
   char      timestamp[13]; 
   char              unoc[8];
   int               id_type;     // observation ID Type
   int               ncep_type;   // NCEP Observation Type Code
   char              ispdbcid[7]; // ISPDB Collection ID
} ObsType;

// UNOC,ID,ID_TYPE,NCEP_TYPE,YEAR,MONTH,DAY,MINUTE,TIME_CODE,N_OBS,H_GMT,LAT,LON,ELV
typedef struct MetaData {
   char      timestamp[13]; 
   char              unoc[8];
   char              id[14];     // Observation ID
   int               year;
   int               month;
   int               day;
   int               hour;
   int               minute;
   int               second;
   int               time_in_sec;
   char              time_code[4];
   int               n_obs;          // Number of observations
   char              h_gmt[200];     // hours in GMT
   float             lat;            // lat
   float             lon;            // lon
   int               elv;            // elevation
} MetaData;

typedef struct OrigMetaData {
   char      timestamp[13]; 
   char              unoc[8];
   char              olat[9];      // original lat
   char              olon[9];      // original lon
   char              oelev[7];     // original elevation
   char              uoelev[9];    // unit of original elevation
} OrigMetaData;

// *UNOC,SLP,SLPE,SLPQC,SFP,SFPE,SFPQC,OSLP,UOSLP,OSFP,UOSFP
typedef struct Observations {
   char      timestamp[13]; 
   char              unoc[8];
   float             slp;          // observed sea level pressure
   float             slpe;         // sea level pressure error  
   int               slpqc;        // sea level pressure flag
   float             sfp;          // surface level pressure
   float             sfpe;         // surface level pressure error
   int               sfpqc;        // surface level pressure flag
} Observations;

typedef struct OrigObservations {
   char      timestamp[13]; 
   char              unoc[8];
   char              oslp[10];     // original sea level pressure
   char              uoslp[9];    // original unit of sea level pressure
   char              osfp[10];     // original surface level pressure
   char              uosfp[9];    // original unit of surface level pressure
} OrigObservations;

// *UNOC,SFLSD,RTC,QCISLP,QCISFP,NAME,LIB
//
typedef struct Source {
   char      timestamp[13]; 
   char              unoc[8];
   char              sflsd[2];   // Source flag for Land Station Data
   char              rtc[6];     // Report Type Code 
   char              qcislp[6];  // Quality Control Indicators for Sea Level Pressure Value from Source
   char              qcisfp[6];  // Quality Control Indicators for Surface Level Pressure Value from Source
} Source;

typedef struct Tracking {
   char      timestamp[13]; 
   char              unoc[8];
   char              sname[31];  // name of stations or ships   
   char              slib[4];    // Name of station library
} Tracking;

// *UNOC,GCS,GCISPD,TK,TAT,UTAT,TCS,TCISPD,HCS,HCISPD
//
typedef struct Corrections {
   char      timestamp[13]; 
   char              unoc[8];
   char              gcs[31];     // Gravity correction made by source
   char              gcispd[31];  // Gravity correction made by ISPD
   int               tk;          // Observed Temperature of the attached thermometer in K
   char              tat[10];      // original temperature of the attached thermometer
   char              utat[9];     // Units of the Original Temperature of the attached thermoemter
   char              tcs[31];     // Temperature correction made by source
   char              tcispd[31];  // Temperature correction made by ISPD 
   char              hcs[31];     // Homogenization correctio made by source
   char              hcispd[31];  // Homogenization correction made by ISPD
} Corrections;

typedef struct PresInst {
   char      timestamp[13]; 
   char              unoc[8];
   unsigned int      PressureInst;  // Pressure Instrument
} PresInst;

typedef struct MiscMarine {
   char      timestamp[13]; 
   char              unoc[8];
   unsigned int      PressureBias;  // Pressure Bias
} MiscMarine;

typedef struct SrcMarine {
   char      timestamp[13]; 
   char              unoc[8];
   int               sid;           // Source ID
   int               dck;           // Deck ID 
   int               pt;            // Platform
} SrcMarine;

typedef struct Hurricanes {
   char      timestamp[13]; 
   char              unoc[8];
   int               dir;           // direction
   int               trm;           // translation speed in miles/h
   float             trmm;          // translation speed in m/h
   int               trk;           // translation speed in knots
   int               wm;            // wind speed in miles/h
   float             wmm;           // wind speed in m/h
   int               wk;            // wind speed in knots
} Hurricanes;

typedef struct HurricaneVariances {
   char      timestamp[13]; 
   char              unoc[8];
   int               pe;            // position stdv
   int               we;            // wind speed stdv
   int               pre;           // pressure stdv
} HurricaneVariances;

typedef struct HurricaneQC {
   char      timestamp[13]; 
   char              unoc[8];
   int               wq;           // wind quality
   int               pq;            // pressure quality
} HurricaneQC;

typedef struct HurricaneID {
   char      timestamp[13]; 
   char              unoc[8];
   char        storm_id[31];       // storm id
} HurricaneID;

// a list of contacts
typedef struct DatasetIndex {
   char    cdcid[11];               
   char    name[100];               
   char    description[100];        
   char    period[21];
   char    ncdcref[21];
   char    ncarref[21];
   char    contact[100];
   char    url[200];
} DatasetIndex;


typedef struct filename {
    char file[200];
} filename;


#endif
