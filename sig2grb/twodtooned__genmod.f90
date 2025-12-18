        !COMPILER-GENERATED INTERFACE MODULE: Wed Mar  6 02:36:42 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TWODTOONED__genmod
          INTERFACE 
            SUBROUTINE TWODTOONED(DATA2,DATA1,NLONS,NLATS)
              INTEGER(KIND=4), INTENT(IN) :: NLATS
              INTEGER(KIND=4), INTENT(IN) :: NLONS
              REAL(KIND=4), INTENT(IN) :: DATA2(NLONS,NLATS)
              REAL(KIND=4), INTENT(OUT) :: DATA1(NLONS*NLATS)
            END SUBROUTINE TWODTOONED
          END INTERFACE 
        END MODULE TWODTOONED__genmod
