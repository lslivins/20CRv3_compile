        !COMPILER-GENERATED INTERFACE MODULE: Thu Aug 22 10:21:18 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GRDCRD__genmod
          INTERFACE 
            SUBROUTINE GRDCRD(D,ND,X,NX,FLG)
              INTEGER(KIND=4), INTENT(IN) :: NX
              INTEGER(KIND=4), INTENT(IN) :: ND
              REAL(KIND=4), INTENT(INOUT) :: D(ND)
              REAL(KIND=4), INTENT(IN) :: X(NX)
              INTEGER(KIND=4), INTENT(IN) :: FLG
            END SUBROUTINE GRDCRD
          END INTERFACE 
        END MODULE GRDCRD__genmod
