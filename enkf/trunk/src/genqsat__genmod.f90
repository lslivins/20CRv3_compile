        !COMPILER-GENERATED INTERFACE MODULE: Thu Aug 22 10:21:18 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GENQSAT__genmod
          INTERFACE 
            SUBROUTINE GENQSAT(SPH,QSAT,GES_PRSL,GES_TV,ICE,NPTS,NLEVS)
              INTEGER(KIND=4), INTENT(IN) :: NLEVS
              INTEGER(KIND=4), INTENT(IN) :: NPTS
              REAL(KIND=4), INTENT(IN) :: SPH(NPTS,NLEVS)
              REAL(KIND=8), INTENT(OUT) :: QSAT(NPTS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: GES_PRSL(NPTS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: GES_TV(NPTS,NLEVS)
              LOGICAL(KIND=4), INTENT(IN) :: ICE
            END SUBROUTINE GENQSAT
          END INTERFACE 
        END MODULE GENQSAT__genmod
