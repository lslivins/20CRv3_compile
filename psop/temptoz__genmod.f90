        !COMPILER-GENERATED INTERFACE MODULE: Wed Aug 21 01:04:22 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TEMPTOZ__genmod
          INTERFACE 
            SUBROUTINE TEMPTOZ(NLONS,NLATS,NLEVS,RGAS,CP,GRAV,PINT,PL,ZS&
     &,TV,Z)
              INTEGER(KIND=4), INTENT(IN) :: NLEVS
              INTEGER(KIND=4), INTENT(IN) :: NLATS
              INTEGER(KIND=4), INTENT(IN) :: NLONS
              REAL(KIND=4), INTENT(IN) :: RGAS
              REAL(KIND=4), INTENT(IN) :: CP
              REAL(KIND=4), INTENT(IN) :: GRAV
              REAL(KIND=4), INTENT(IN) :: PINT(NLONS,NLATS,NLEVS+1)
              REAL(KIND=4), INTENT(IN) :: PL(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: ZS(NLONS,NLATS)
              REAL(KIND=4), INTENT(IN) :: TV(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(OUT) :: Z(NLONS,NLATS,NLEVS)
            END SUBROUTINE TEMPTOZ
          END INTERFACE 
        END MODULE TEMPTOZ__genmod
