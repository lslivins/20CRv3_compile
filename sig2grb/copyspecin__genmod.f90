        !COMPILER-GENERATED INTERFACE MODULE: Wed Mar  6 02:36:42 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE COPYSPECIN__genmod
          INTERFACE 
            SUBROUTINE COPYSPECIN(RSPECDATA,CSPECDATA,NDIMSPEC)
              INTEGER(KIND=4), INTENT(IN) :: NDIMSPEC
              REAL(KIND=4), INTENT(IN) :: RSPECDATA(2*NDIMSPEC)
              COMPLEX(KIND=4), INTENT(OUT) :: CSPECDATA(NDIMSPEC)
            END SUBROUTINE COPYSPECIN
          END INTERFACE 
        END MODULE COPYSPECIN__genmod
