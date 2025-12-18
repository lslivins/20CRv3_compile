        !COMPILER-GENERATED INTERFACE MODULE: Wed Aug 21 01:04:22 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LINTRP2__genmod
          INTERFACE 
            SUBROUTINE LINTRP2(F,G,DX,DY,NX,NY)
              INTEGER(KIND=4), INTENT(IN) :: NY
              INTEGER(KIND=4), INTENT(IN) :: NX
              REAL(KIND=4), INTENT(IN) :: F(NX,NY)
              REAL(KIND=4), INTENT(OUT) :: G
              REAL(KIND=4), INTENT(IN) :: DX
              REAL(KIND=4), INTENT(IN) :: DY
            END SUBROUTINE LINTRP2
          END INTERFACE 
        END MODULE LINTRP2__genmod
