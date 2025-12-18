        !COMPILER-GENERATED INTERFACE MODULE: Wed Mar  6 02:36:40 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LUDCMP__genmod
          INTERFACE 
            SUBROUTINE LUDCMP(A,N,NP,INDX)
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(NP,NP)
              INTEGER(KIND=4) :: INDX(N)
            END SUBROUTINE LUDCMP
          END INTERFACE 
        END MODULE LUDCMP__genmod
