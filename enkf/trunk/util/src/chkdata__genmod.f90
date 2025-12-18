        !COMPILER-GENERATED INTERFACE MODULE: Wed Mar  6 02:35:03 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CHKDATA__genmod
          INTERFACE 
            SUBROUTINE CHKDATA(DATA,NLONS,NLATS)
              INTEGER(KIND=4) :: NLATS
              INTEGER(KIND=4) :: NLONS
              REAL(KIND=4) :: DATA(NLONS,NLATS)
            END SUBROUTINE CHKDATA
          END INTERFACE 
        END MODULE CHKDATA__genmod
