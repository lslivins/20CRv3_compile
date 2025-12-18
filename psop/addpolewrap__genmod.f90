        !COMPILER-GENERATED INTERFACE MODULE: Wed Aug 21 01:04:22 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ADDPOLEWRAP__genmod
          INTERFACE 
            SUBROUTINE ADDPOLEWRAP(FIN,FOUT,NX,NY)
              INTEGER(KIND=4) :: NY
              INTEGER(KIND=4) :: NX
              REAL(KIND=4) :: FIN(NX,NY)
              REAL(KIND=4) :: FOUT(NX+1,NY+2)
            END SUBROUTINE ADDPOLEWRAP
          END INTERFACE 
        END MODULE ADDPOLEWRAP__genmod
