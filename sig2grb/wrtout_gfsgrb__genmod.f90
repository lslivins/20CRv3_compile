        !COMPILER-GENERATED INTERFACE MODULE: Wed Mar  6 02:36:42 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WRTOUT_GFSGRB__genmod
          INTERFACE 
            SUBROUTINE WRTOUT_GFSGRB(SIGHEAD,NLONS,NLATS,NLEVS,NTRAC,   &
     &NTRUNC,PSG,DLNPSDX,DLNPSDY,TOPOG,UG,VG,DIVG,VIRTEMPG,TRACERG,     &
     &FILENAME)
              USE SIGIO_MODULE, ONLY :                                  &
     &          SIGIO_HEAD
              INTEGER(KIND=4), INTENT(IN) :: NTRAC
              INTEGER(KIND=4), INTENT(IN) :: NLEVS
              INTEGER(KIND=4), INTENT(IN) :: NLATS
              INTEGER(KIND=4), INTENT(IN) :: NLONS
              TYPE (SIGIO_HEAD), INTENT(IN) :: SIGHEAD
              INTEGER(KIND=4), INTENT(IN) :: NTRUNC
              REAL(KIND=4), INTENT(IN) :: PSG(NLONS,NLATS)
              REAL(KIND=4), INTENT(IN) :: DLNPSDX(NLONS,NLATS)
              REAL(KIND=4), INTENT(IN) :: DLNPSDY(NLONS,NLATS)
              REAL(KIND=4), INTENT(IN) :: TOPOG(NLONS,NLATS)
              REAL(KIND=4), INTENT(IN) :: UG(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: VG(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: DIVG(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: VIRTEMPG(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: TRACERG(NLONS,NLATS,NLEVS,    &
     &NTRAC)
              CHARACTER(LEN=500), INTENT(IN) :: FILENAME
            END SUBROUTINE WRTOUT_GFSGRB
          END INTERFACE 
        END MODULE WRTOUT_GFSGRB__genmod
