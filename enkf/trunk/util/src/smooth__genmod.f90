        !COMPILER-GENERATED INTERFACE MODULE: Wed Mar  6 02:34:49 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SMOOTH__genmod
          INTERFACE 
            SUBROUTINE SMOOTH(SPECDAT,NTRUNC,NLEVS,SMOOTHPARM,WINDOW)
              INTEGER(KIND=4), INTENT(IN) :: NLEVS
              INTEGER(KIND=4), INTENT(IN) :: NTRUNC
              REAL(KIND=4), INTENT(INOUT) :: SPECDAT((NTRUNC+1)*(NTRUNC+&
     &2),NLEVS)
              INTEGER(KIND=4), INTENT(IN) :: SMOOTHPARM(NLEVS)
              INTEGER(KIND=4), INTENT(IN) :: WINDOW
            END SUBROUTINE SMOOTH
          END INTERFACE 
        END MODULE SMOOTH__genmod
