        !COMPILER-GENERATED INTERFACE MODULE: Wed Mar  6 02:34:49 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE COPY_SIGDATA__genmod
          INTERFACE 
            SUBROUTINE COPY_SIGDATA(HEAD,DATA1,DATA2)
              USE SIGIO_MODULE
              TYPE (SIGIO_HEAD), INTENT(IN) :: HEAD
              TYPE (SIGIO_DATA), INTENT(IN) :: DATA1
              TYPE (SIGIO_DATA), INTENT(INOUT) :: DATA2
            END SUBROUTINE COPY_SIGDATA
          END INTERFACE 
        END MODULE COPY_SIGDATA__genmod
