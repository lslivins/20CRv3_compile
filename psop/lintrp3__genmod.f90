        !COMPILER-GENERATED INTERFACE MODULE: Wed Aug 21 01:04:22 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LINTRP3__genmod
          INTERFACE 
            SUBROUTINE LINTRP3(F,G,DX,DY,DZ,NX,NY,NZ)
              INTEGER(KIND=4), INTENT(IN) :: NZ
              INTEGER(KIND=4), INTENT(IN) :: NY
              INTEGER(KIND=4), INTENT(IN) :: NX
              REAL(KIND=4), INTENT(IN) :: F(NX,NY,NZ)
              REAL(KIND=4), INTENT(OUT) :: G
              REAL(KIND=4), INTENT(IN) :: DX
              REAL(KIND=4), INTENT(IN) :: DY
              REAL(KIND=4), INTENT(IN) :: DZ
            END SUBROUTINE LINTRP3
          END INTERFACE 
        END MODULE LINTRP3__genmod
