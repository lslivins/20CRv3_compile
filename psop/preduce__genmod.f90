        !COMPILER-GENERATED INTERFACE MODULE: Wed Aug 21 01:04:22 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PREDUCE__genmod
          INTERFACE 
            FUNCTION PREDUCE(PS,TPRESS,T,ZMODEL,ZOB,RLAPSE)
              REAL(KIND=4), INTENT(IN) :: PS
              REAL(KIND=4), INTENT(IN) :: TPRESS
              REAL(KIND=4), INTENT(IN) :: T
              REAL(KIND=4), INTENT(IN) :: ZMODEL
              REAL(KIND=4), INTENT(IN) :: ZOB
              REAL(KIND=4), INTENT(IN) :: RLAPSE
              REAL(KIND=4) :: PREDUCE
            END FUNCTION PREDUCE
          END INTERFACE 
        END MODULE PREDUCE__genmod
