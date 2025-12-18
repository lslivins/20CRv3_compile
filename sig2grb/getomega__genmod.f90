        !COMPILER-GENERATED INTERFACE MODULE: Wed Mar  6 02:36:42 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GETOMEGA__genmod
          INTERFACE 
            SUBROUTINE GETOMEGA(NLONS,NLATS,NLEVS,UG,VG,DIVG,BK,CK,DBK, &
     &PK,DPK,PSG,DLNPSDX,DLNPSDY,DLNPSDT,DLNPDTG,ETADOT)
              INTEGER(KIND=4), INTENT(IN) :: NLEVS
              INTEGER(KIND=4), INTENT(IN) :: NLATS
              INTEGER(KIND=4), INTENT(IN) :: NLONS
              REAL(KIND=4), INTENT(IN) :: UG(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: VG(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: DIVG(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: BK(NLEVS+1)
              REAL(KIND=4), INTENT(IN) :: CK(NLEVS)
              REAL(KIND=4), INTENT(IN) :: DBK(NLEVS)
              REAL(KIND=4), INTENT(IN) :: PK(NLONS,NLATS,NLEVS+1)
              REAL(KIND=4), INTENT(IN) :: DPK(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: PSG(NLONS,NLATS)
              REAL(KIND=4), INTENT(IN) :: DLNPSDX(NLONS,NLATS)
              REAL(KIND=4), INTENT(IN) :: DLNPSDY(NLONS,NLATS)
              REAL(KIND=4), INTENT(INOUT) :: DLNPSDT(NLONS,NLATS)
              REAL(KIND=4), INTENT(OUT) :: DLNPDTG(NLONS,NLATS,NLEVS)
              REAL(KIND=4), INTENT(OUT) :: ETADOT(NLONS,NLATS,NLEVS+1)
            END SUBROUTINE GETOMEGA
          END INTERFACE 
        END MODULE GETOMEGA__genmod
