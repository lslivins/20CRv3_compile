        !COMPILER-GENERATED INTERFACE MODULE: Wed Aug 21 01:04:22 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GETSIGDATA__genmod
          INTERFACE 
            SUBROUTINE GETSIGDATA(SIGHEAD,SIGDATA,GLATS,TEMPG,PSG,PSLG, &
     &PSIG,ZSG,NLONS,NLATS,NLEVS,NTRUNC)
              USE SIGIO_MODULE
              INTEGER(KIND=4), INTENT(IN) :: NLEVS
              INTEGER(KIND=4), INTENT(IN) :: NLATS
              INTEGER(KIND=4), INTENT(IN) :: NLONS
              TYPE (SIGIO_HEAD), INTENT(IN) :: SIGHEAD
              TYPE (SIGIO_DATA), INTENT(INOUT) :: SIGDATA
              REAL(KIND=4), INTENT(OUT) :: GLATS(NLATS)
              REAL(KIND=4), INTENT(OUT) :: TEMPG(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(OUT) :: PSG(NLONS,NLATS)
              REAL(KIND=4), INTENT(OUT) :: PSLG(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(OUT) :: PSIG(NLONS,NLATS,NLEVS+1)
              REAL(KIND=4), INTENT(OUT) :: ZSG(NLONS,NLATS)
              INTEGER(KIND=4), INTENT(IN) :: NTRUNC
            END SUBROUTINE GETSIGDATA
          END INTERFACE 
        END MODULE GETSIGDATA__genmod
