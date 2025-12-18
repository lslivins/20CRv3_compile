        !COMPILER-GENERATED INTERFACE MODULE: Thu Aug 22 10:21:18 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ISRCHF__genmod
          INTERFACE 
            FUNCTION ISRCHF(NX1,X,Y,FLG)
              INTEGER(KIND=4), INTENT(IN) :: NX1
              REAL(KIND=4), INTENT(IN) :: X(NX1)
              REAL(KIND=4), INTENT(IN) :: Y
              INTEGER(KIND=4), INTENT(IN) :: FLG
              INTEGER(KIND=4) :: ISRCHF
            END FUNCTION ISRCHF
          END INTERFACE 
        END MODULE ISRCHF__genmod
