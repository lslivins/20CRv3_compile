        !COMPILER-GENERATED INTERFACE MODULE: Wed Mar  6 02:34:49 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INIT_SIGDATA__genmod
          INTERFACE 
            SUBROUTINE INIT_SIGDATA(HEAD,DATA)
              USE SIGIO_MODULE
              TYPE (SIGIO_HEAD), INTENT(IN) :: HEAD
              TYPE (SIGIO_DATA), INTENT(INOUT) :: DATA
            END SUBROUTINE INIT_SIGDATA
          END INTERFACE 
        END MODULE INIT_SIGDATA__genmod
