program getsfcensmean
!$$$  main program documentation block
!
! program:  getsfcensmean              compute ensemble mean
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  compute ensemble mean surface file
!
! program history log:
!   2009-02-23  Initial version.
!
! usage:
!   input files:
!
!   output files:
!
! attributes:
!   language: f95
!
!$$$

! create ensemble mean NCEP GFS surface file.
  USE SFCIO_MODULE
  implicit none
  TYPE(SFCIO_HEAD) :: SFCHEADI,SFCHEADO
  TYPE(SFCIO_DATA) :: SFCDATAI,SFCDATAO
  character*500 filenamesfcin,filenamesfcout,datapath,fileprefix
  character*3 charnanal
  integer nsfci,nsfco,iret,nanals,nanal
  real(8) rnanals

  call w3tagb('GETSFCENSMEAN',2011,0319,0055,'NP25')

  NSFCI=21
  NSFCO=61
  call getarg(1,datapath)
  call getarg(2,filenamesfcout)
  call getarg(3,fileprefix)
  call getarg(4,charnanal)
  read(charnanal,'(i2)') nanals
  filenamesfcout = trim(adjustl(datapath))//filenamesfcout
  rnanals=nanals
  rnanals=1.0_8/rnanals

  write(6,*)'datapath      = ',trim(datapath)
  write(6,*)'filenamesfcout= ',trim(filenamesfcout)
  write(6,*)'fileprefix    = ',trim(fileprefix)
  write(6,*)'nanals,rnanals= ',nanals,rnanals

  do nanal=1,nanals
     write(charnanal,'(i3.3)') nanal
     filenamesfcin = trim(adjustl(datapath))// &
          trim(adjustl(fileprefix))//'_mem'//charnanal
     call sfcio_srohdc(nsfci,filenamesfcin,sfcheadi,sfcdatai,iret)
     if (nanal .eq. 1) then

!   sfcio_data        Surface file data fields
!     tsea              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       surface temperature in K
!     smc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil volumetric water content in fraction
!     sheleg            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       snow depth in m
!     stc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil temperature in K
!     tg3               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       deep soil temperature in K
!     zorl              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       roughness in cm
!     cv                Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud cover in fraction
!     cvb               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud bottom in kpa
!     cvt               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud top in kpa
!     alvsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible scattered in fraction
!     alvwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible beam in fraction
!     alnsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-IR scattered in fraction
!     alnwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-IR beam in fraction
!     slmsk             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       sea-land-ice mask (0-sea, 1-land, 2-ice)
!     vfrac             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation fraction in fraction
!     canopy            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       canopy water in m
!     f10m              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       10-meter wind speed over lowest model wind speed
!     t2m               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter temperature in K
!     q2m               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter specific humidity in kg/kg
!     vtype             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation type in integer 1-13
!     stype             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       soil type in integer 1-9
!     facsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
!     facwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
!     uustar            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     ffmm              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     ffhh              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     hice              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     fice              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     tisfc             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     tprcp             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     srflag            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     snwdph            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     slc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       xxx in xxx
!     shdmin            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     shdmax            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     slope             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       slope type
!     snoalb            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     orog              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       orography in m

        call sfcio_aldata(sfcheadi,sfcdatao,iret)
        sfcheado = sfcheadi
        sfcdatao%tsea	=sfcdatai%tsea	*rnanals
        sfcdatao%smc	=sfcdatai%smc	*rnanals
        sfcdatao%sheleg	=sfcdatai%sheleg*rnanals
        sfcdatao%stc	=sfcdatai%stc	*rnanals
        sfcdatao%tg3 	=sfcdatai%tg3	*rnanals
        sfcdatao%zorl	=sfcdatai%zorl  *rnanals
        sfcdatao%cv		=sfcdatai%cv	*rnanals
        sfcdatao%cvb	=sfcdatai%cvb	*rnanals
        sfcdatao%cvt	=sfcdatai%cvt	*rnanals
        sfcdatao%alvsf	=sfcdatai%alvsf *rnanals
        sfcdatao%alvwf	=sfcdatai%alvwf *rnanals
        sfcdatao%alnsf	=sfcdatai%alnsf *rnanals
        sfcdatao%alnwf	=sfcdatai%alnwf *rnanals
        sfcdatao%slmsk	=sfcdatai%slmsk
        sfcdatao%vfrac	=sfcdatai%vfrac *rnanals
        sfcdatao%canopy	=sfcdatai%canopy*rnanals
        sfcdatao%f10m	=sfcdatai%f10m  *rnanals
        sfcdatao%t2m	=sfcdatai%t2m  *rnanals
        sfcdatao%q2m	=sfcdatai%q2m  *rnanals
        sfcdatao%vtype	=sfcdatai%vtype 
        sfcdatao%stype	=sfcdatai%stype 
        sfcdatao%facsf	=sfcdatai%facsf *rnanals
        sfcdatao%facwf	=sfcdatai%facwf *rnanals
        sfcdatao%uustar	=sfcdatai%uustar*rnanals
        sfcdatao%ffmm	=sfcdatai%ffmm  *rnanals
        sfcdatao%ffhh	=sfcdatai%ffhh  *rnanals
        sfcdatao%hice	=sfcdatai%hice  *rnanals
        sfcdatao%fice	=sfcdatai%fice  *rnanals
        sfcdatao%tisfc	=sfcdatai%tisfc  *rnanals
        sfcdatao%tprcp      =sfcdatai%tprcp  *rnanals
        sfcdatao%srflag	=sfcdatai%srflag  *rnanals
        sfcdatao%snwdph	=sfcdatai%snwdph  *rnanals
        sfcdatao%slc	=sfcdatai%slc  *rnanals
        sfcdatao%shdmin	=sfcdatai%shdmin  *rnanals
        sfcdatao%shdmax	=sfcdatai%shdmax  *rnanals
        sfcdatao%slope	=sfcdatai%slope  
        sfcdatao%snoalb	=sfcdatai%snoalb  *rnanals
        sfcdatao%orog	=sfcdatai%orog  *rnanals
     else
        sfcdatao%tsea	=sfcdatao%tsea  +sfcdatai%tsea *rnanals
        sfcdatao%smc	=sfcdatao%smc	+sfcdatai%smc  *rnanals
        sfcdatao%sheleg	=sfcdatao%sheleg+sfcdatai%sheleg*rnanals
        sfcdatao%stc	=sfcdatao%stc	+sfcdatai%stc  *rnanals
        sfcdatao%tg3 	=sfcdatao%tg3	+sfcdatai%tg3  *rnanals
        sfcdatao%zorl	=sfcdatao%zorl  +sfcdatai%zorl  *rnanals
        sfcdatao%cv		=sfcdatao%cv	+sfcdatai%cv   *rnanals
        sfcdatao%cvb	=sfcdatao%cvb	+sfcdatai%cvb  *rnanals
        sfcdatao%cvt	=sfcdatao%cvt	+sfcdatai%cvt  *rnanals
        sfcdatao%alvsf	=sfcdatao%alvsf +sfcdatai%alvsf *rnanals
        sfcdatao%alvwf	=sfcdatao%alvwf +sfcdatai%alvwf *rnanals
        sfcdatao%alnsf	=sfcdatao%alnsf +sfcdatai%alnsf *rnanals
        sfcdatao%alnwf	=sfcdatao%alnwf +sfcdatai%alnwf *rnanals
        sfcdatao%vfrac	=sfcdatao%vfrac +sfcdatai%vfrac *rnanals
        sfcdatao%canopy	=sfcdatao%canopy+sfcdatai%canopy*rnanals
        sfcdatao%f10m	=sfcdatao%f10m  +sfcdatai%f10m  *rnanals
        sfcdatao%t2m	=sfcdatao%t2m  +sfcdatai%t2m  *rnanals
        sfcdatao%q2m	=sfcdatao%q2m  +sfcdatai%q2m  *rnanals
        sfcdatao%facsf	=sfcdatao%facsf +sfcdatai%facsf *rnanals
        sfcdatao%facwf	=sfcdatao%facwf +sfcdatai%facwf *rnanals
        sfcdatao%uustar	=sfcdatao%uustar+sfcdatai%uustar*rnanals
        sfcdatao%ffmm	=sfcdatao%ffmm  +sfcdatai%ffmm  *rnanals
        sfcdatao%ffhh	=sfcdatao%ffhh  +sfcdatai%ffhh  *rnanals
        sfcdatao%hice	=sfcdatao%hice + sfcdatai%hice  *rnanals
        sfcdatao%fice	=sfcdatao%fice + sfcdatai%fice  *rnanals
        sfcdatao%tisfc	=sfcdatao%tisfc + sfcdatai%tisfc  *rnanals
        sfcdatao%tprcp      =sfcdatao%tprcp + sfcdatai%tprcp  *rnanals
        sfcdatao%srflag	=sfcdatao%srflag + sfcdatai%srflag  *rnanals
        sfcdatao%snwdph	=sfcdatao%snwdph + sfcdatai%snwdph  *rnanals
        sfcdatao%slc	=sfcdatao%slc + sfcdatai%slc  *rnanals
        sfcdatao%shdmin	=sfcdatao%shdmin + sfcdatai%shdmin  *rnanals
        sfcdatao%shdmax	=sfcdatao%shdmax + sfcdatai%shdmax  *rnanals
        sfcdatao%snoalb	=sfcdatao%snoalb + sfcdatai%snoalb  *rnanals
        sfcdatao%orog	=sfcdatao%orog + sfcdatai%orog  *rnanals
     end if
     call sfcio_axdata(sfcdatai,iret)
  enddo
  call sfcio_swohdc(nsfco,filenamesfcout,sfcheado,sfcdatao,iret)

  call w3tage('GETSFCENSMEAN')

END program getsfcensmean
