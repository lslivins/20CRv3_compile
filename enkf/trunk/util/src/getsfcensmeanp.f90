      program getsfcensmeanp
! create ensemble mean NCEP GFS surface file.
      USE SFCIO_MODULE
      implicit none
      TYPE(SFCIO_HEAD) :: SFCHEADI,SFCHEADO
      TYPE(SFCIO_DATA) :: SFCDATAI,SFCDATAO
      character*500 filenamesfcin,filenamesfcout,datapath,fileprefix
      character*3 charnanal
      integer nsfci,nsfco,iret,nanals,nanal,nproc,numproc,rmin,rmax
      ! mpi definitions.
      include 'mpif.h'
     
      call MPI_Init(iret)
      ! nproc is process number, numproc is total number of processes.
      call MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
      call MPI_Comm_size(MPI_COMM_WORLD,numproc,iret)
      NSFCI=21
      NSFCO=61
      call getarg(1,datapath)
      call getarg(2,filenamesfcout)
      call getarg(3,fileprefix)
      call getarg(4,charnanal)
      read(charnanal,'(i2)') nanals
      filenamesfcout = trim(adjustl(datapath))//filenamesfcout
      if (nproc .eq. 0) print *,filenamesfcout
      nanal = nproc + 1
      write(charnanal,'(i3.3)') nanal
      filenamesfcin = trim(adjustl(datapath))// &
      trim(adjustl(fileprefix))//'_mem'//charnanal
      !print *,filenamesfcin
      call sfcio_srohdc(nsfci,filenamesfcin,sfcheadi,sfcdatai,iret)
      if (iret .ne. 0) then
        call mpi_abort(MPI_COMM_WORLD,1,iret)
      endif
      !print *,trim(filenamesfcin),iret
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
      ! check data in 1st and last records.
      rmin = minval(sfcdatai%tsea)
      rmax = maxval(sfcdatai%tsea)
      if (rmin .eq. rmax .or. rmin < -1.e10 .or. rmax .gt. 1.e10) then
        call mpi_abort(MPI_COMM_WORLD,1,iret)
      endif
      rmin = minval(sfcdatai%orog)
      rmax = maxval(sfcdatai%orog)
      if (rmin .eq. rmax .or. rmin < -1.e10 .or. rmax .gt. 1.e10) then
        call mpi_abort(MPI_COMM_WORLD,1,iret)
      endif
      call sfcio_aldata(sfcheadi,sfcdatao,iret)
      !print *,'aldata',iret
      sfcheado = sfcheadi
      ! these fields are fixed, don't take mean.
      sfcdatao%slmsk	=sfcdatai%slmsk
      sfcdatao%vtype	=sfcdatai%vtype 
      sfcdatao%stype	=sfcdatai%stype 
      sfcdatao%slope	=sfcdatai%slope  
      sfcdatao%orog = sfcdatai%orog
      ! compute means of these
      call mpi_allreduce(sfcdatai%tsea,sfcdatao%tsea,size(sfcdatai%tsea),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%tsea	=sfcdatao%tsea /float(nanals)
      call mpi_allreduce(sfcdatai%smc,sfcdatao%smc,size(sfcdatai%smc),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%smc	=sfcdatao%smc  /float(nanals)
      call mpi_allreduce(sfcdatai%sheleg,sfcdatao%sheleg,size(sfcdatai%sheleg),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%sheleg	=sfcdatao%sheleg/float(nanals)
      call mpi_allreduce(sfcdatai%stc,sfcdatao%stc,size(sfcdatai%stc),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%stc	=sfcdatao%stc  /float(nanals)
      call mpi_allreduce(sfcdatai%tg3,sfcdatao%tg3,size(sfcdatai%tg3),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%tg3 	=sfcdatao%tg3  /float(nanals)
      call mpi_allreduce(sfcdatai%zorl,sfcdatao%zorl,size(sfcdatai%zorl),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%zorl	=sfcdatao%zorl  /float(nanals)
      call mpi_allreduce(sfcdatai%cv,sfcdatao%cv,size(sfcdatai%cv),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%cv	=sfcdatao%cv   /float(nanals)
      call mpi_allreduce(sfcdatai%cvb,sfcdatao%cvb,size(sfcdatai%cvb),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%cvb	=sfcdatao%cvb  /float(nanals)
      call mpi_allreduce(sfcdatai%cvt,sfcdatao%cvt,size(sfcdatai%cvt),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%cvt	=sfcdatao%cvt  /float(nanals)
      call mpi_allreduce(sfcdatai%alvsf,sfcdatao%alvsf,size(sfcdatai%alvsf),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%alvsf	=sfcdatao%alvsf /float(nanals)
      call mpi_allreduce(sfcdatai%alvwf,sfcdatao%alvwf,size(sfcdatai%alvwf),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%alvwf	=sfcdatao%alvwf /float(nanals)
      call mpi_allreduce(sfcdatai%alnsf,sfcdatao%alnsf,size(sfcdatai%alnsf),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%alnsf	=sfcdatao%alnsf /float(nanals)
      call mpi_allreduce(sfcdatai%alnwf,sfcdatao%alnwf,size(sfcdatai%alnwf),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%alnwf	=sfcdatao%alnwf /float(nanals)
      call mpi_allreduce(sfcdatai%vfrac,sfcdatao%vfrac,size(sfcdatai%vfrac),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%vfrac	=sfcdatao%vfrac /float(nanals)
      call mpi_allreduce(sfcdatai%canopy,sfcdatao%canopy,size(sfcdatai%canopy),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%canopy	=sfcdatao%canopy/float(nanals)
      call mpi_allreduce(sfcdatai%f10m,sfcdatao%f10m,size(sfcdatai%f10m),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%f10m	=sfcdatao%f10m  /float(nanals)
      call mpi_allreduce(sfcdatai%t2m,sfcdatao%t2m,size(sfcdatai%t2m),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%t2m	=sfcdatao%t2m  /float(nanals)
      call mpi_allreduce(sfcdatai%q2m,sfcdatao%q2m,size(sfcdatai%q2m),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%q2m	=sfcdatao%q2m  /float(nanals)
      call mpi_allreduce(sfcdatai%facsf,sfcdatao%facsf,size(sfcdatai%facsf),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%facsf	=sfcdatao%facsf /float(nanals)
      call mpi_allreduce(sfcdatai%facwf,sfcdatao%facwf,size(sfcdatai%facwf),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%facwf	=sfcdatao%facwf /float(nanals)
      call mpi_allreduce(sfcdatai%uustar,sfcdatao%uustar,size(sfcdatai%uustar),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%uustar	=sfcdatao%uustar/float(nanals)
      call mpi_allreduce(sfcdatai%ffmm,sfcdatao%ffmm,size(sfcdatai%ffmm),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%ffmm	=sfcdatao%ffmm  /float(nanals)
      call mpi_allreduce(sfcdatai%ffhh,sfcdatao%ffhh,size(sfcdatai%ffhh),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%ffhh	=sfcdatao%ffhh  /float(nanals)
      call mpi_allreduce(sfcdatai%hice,sfcdatao%hice,size(sfcdatai%hice),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%hice	=sfcdatao%hice  /float(nanals)
      call mpi_allreduce(sfcdatai%fice,sfcdatao%fice,size(sfcdatai%fice),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%fice	=sfcdatao%fice  /float(nanals)
      call mpi_allreduce(sfcdatai%tisfc,sfcdatao%tisfc,size(sfcdatai%tisfc),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%tisfc	=sfcdatao%tisfc  /float(nanals)
      call mpi_allreduce(sfcdatai%tprcp,sfcdatao%tprcp,size(sfcdatai%tprcp),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%tprcp    =sfcdatao%tprcp  /float(nanals)
      call mpi_allreduce(sfcdatai%srflag,sfcdatao%srflag,size(sfcdatai%srflag),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%srflag	=sfcdatao%srflag  /float(nanals)
      call mpi_allreduce(sfcdatai%snwdph,sfcdatao%snwdph,size(sfcdatai%snwdph),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%snwdph	=sfcdatao%snwdph  /float(nanals)
      call mpi_allreduce(sfcdatai%slc,sfcdatao%slc,size(sfcdatai%slc),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%slc	=sfcdatao%slc  /float(nanals)
      call mpi_allreduce(sfcdatai%shdmin,sfcdatao%shdmin,size(sfcdatai%shdmin),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%shdmin	=sfcdatao%shdmin  /float(nanals)
      call mpi_allreduce(sfcdatai%shdmax,sfcdatao%shdmax,size(sfcdatai%shdmax),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%shdmax	=sfcdatao%shdmax  /float(nanals)
      call mpi_allreduce(sfcdatai%snoalb,sfcdatao%snoalb,size(sfcdatai%snoalb),mpi_real,mpi_sum,mpi_comm_world,iret)
      sfcdatao%snoalb	=sfcdatao%snoalb  /float(nanals)
      call sfcio_axdata(sfcdatai,iret)
      if (nproc .eq. 0) call sfcio_swohdc(nsfco,filenamesfcout,sfcheado,sfcdatao,iret)
      !print *,trim(filenamesfcout),iret
      call MPI_Barrier(MPI_COMM_WORLD,iret)
      if (nproc .eq. 0) write(6,*) 'all done!'
      call MPI_Finalize(iret)
      if (nproc .eq. 0 .and. iret .ne. 0) then
       print *, 'MPI_Finalize error status = ',iret
      end if
      end program getsfcensmeanp
