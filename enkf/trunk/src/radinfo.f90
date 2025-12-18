! this module was extracted from the GSI version operational
! at NCEP in Dec. 2007.
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  radinfo --- Provide information on satellite radiance
!
! !INTERFACE:
!
module radinfo 

! !USES:

  use kinds, only:r_kind,i_kind
  implicit none
  
! set default to private
  private

! set subroutines to public
  public :: init_rad
  public :: init_rad_vars
  public :: radinfo_read
  public :: radinfo_write

! set passed variables to public
  public :: adp_anglebc
  public :: angord
  public :: nuchan
  public :: npred
  public :: npreds
  public :: jpch_rad
  public :: iuse_rad
  public :: nusis
  public :: predx
  public :: pg_rad


! !DESCRIPTION:  This module contains variables and routines related
!            to information for the use of satellite radiance 
!            data.
!
! !REVISION HISTORY:
!   1995-07-06  derber
!   2004-05-13  kleist, documentation
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2004-11-23  treadon - change 110 format statement
!   2004-11-30  xu li   - add array fbias for AVHRR bias correction
!   2004-12-08  xu li   - add logical flag retrieval to module
!   2004-12-22  treadon - rename logical "idiag_rad" as "diag_rad"
!   2005-02-03  xu li   - add SST analysis read and move sub intgrid2 from sst_retrieval to this module
!   2005-03-25  xu li   - modify sub rdgrbsst and remove intgrid2
!   2005-04-18  treadon - make rdgrbsst a separate subroutine
!   2005-09-28  derber  - change radinfo input file add ermax_rad,b_rad and pg_rad
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-04-27  derber  - remove jppf
!
! !CALLING SEQUENCE:
!   sub init_rad       - set satellite related variables to defaults
!   sub init_rad_vars  - initialize satellite related variables
!   sub radinfo_read   - read in sat info and biases, including read sst_an and avhrr bias correction
!   sub radinfo_write  - write out satellite biases
!
!
! !REMARKS:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   derber           org: np23                date: 1995-07-06
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind),parameter:: numt = 33   ! size of AVHRR bias correction file

  logical diag_rad   ! logical to turn off or on the diagnostic radiance file (true=on)
  logical retrieval  ! logical to turn off or on the SST retrieval with AVHRR data
  logical adp_anglebc ! logical to turn off or on the variational radiance angle bias correction
  logical passive_bc  ! logical to turn off or on radiance bias correction for monitored channels
  logical use_edges   ! logical to use data on scan edges (.true.=to use)

  integer(i_kind) jpch_rad   ! number of channels*sat
  integer(i_kind) npred      ! number of radiance bias predictors
  integer(i_kind) npreds     ! number of radiance bias predictors (included the non-adaptive scan angle correction)

  integer(i_kind) mype_rad   ! task id for writing out radiance diagnostics
  integer(i_kind) npred1     ! number of radiance biases predictors minus one
  integer(i_kind) n_sensors  ! number of unique satellite/sensor entries in satinfo file
  integer(i_kind) angord        ! order of polynomial for angle bias correction

  real(r_kind),allocatable,dimension(:):: varch       ! variance for each satellite channel
  real(r_kind),allocatable,dimension(:):: varch_cld   ! variance for cloudy radiance
  real(r_kind),allocatable,dimension(:):: ermax_rad   ! error maximum (qc)
  real(r_kind),allocatable,dimension(:):: b_rad       ! variational b value
  real(r_kind),allocatable,dimension(:):: pg_rad      ! variational pg value
  real(r_kind),allocatable,dimension(:):: tlapmean    ! mean lapse rate (fixed from input file)
  real(r_kind),allocatable,dimension(:):: tsum_tlapmean  ! err sum of mean lapse rate
  real(r_kind),allocatable,dimension(:,:):: fbias     ! bias for AVHRR siumulated radiance
  real(r_kind),allocatable,dimension(:,:):: cbias     ! angle dependent bias for satellite channels
  real(r_kind),allocatable,dimension(:,:):: predx     ! coefficients for predictor part of bias correction

  real(r_kind),allocatable,dimension(:):: radstart    ! starting scan angle
  real(r_kind),allocatable,dimension(:):: radstep     ! step of scan angle
  real(r_kind),allocatable,dimension(:):: radnstep    ! nstep of scan angle

  integer(i_kind),allocatable,dimension(:):: radedge1    ! cut-off of edge removal
  integer(i_kind),allocatable,dimension(:):: radedge2    ! cut-off of edge removal

  integer(i_kind),allocatable,dimension(:):: count_tlapmean ! the count of tlapmean update

  integer(i_kind),allocatable,dimension(:):: nuchan    ! satellite channel
  integer(i_kind),allocatable,dimension(:):: iuse_rad  ! use to turn off satellite radiance data
  integer(i_kind),allocatable,dimension(:):: ifactq    ! scaling parameter for d(Tb)/dq sensitivity

  logical,allocatable,dimension(:):: update_tlapmean ! indicator if tlapmean update is needed

  character(len=20),allocatable,dimension(:):: nusis   ! sensor/instrument/satellite indicator
  character(len=20),allocatable,dimension(:):: sensorlist ! CRTM satellite/sensor list 


contains


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_rad --- Initialize parameters for radiance data
!
! !INTERFACE:
!
  subroutine init_rad

! !USES:

! !DESCRIPTION:  This routine sets default values for variables used in
!            the radiance processing routines
!
! !REVISION HISTORY:
!   1995-07-06  derber
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:    
!   derber      org: np23                date: 1995-07-06
!
!EOP
!-------------------------------------------------------------------------
    implicit none

    jpch_rad = 0         ! total number of channels over all instruments & satellites
    retrieval = .false.  ! .true. = apply physical SST retrieval with AVHRR data
    diag_rad = .true.    ! .true.=generate radiance diagnostic file
    mype_rad = 0         ! mpi task to collect and print radiance use information on/from
    npred=5              ! number of bias correction predictors
    npreds=npred+1       ! number of bias correction predictors (included the non-adaptive scan angle correction)
    n_sensors = 0        ! number of unique satellite/sensors in satinfo file

    passive_bc = .false.  ! .true.=turn on bias correction for monitored channels
    adp_anglebc = .false. ! .true.=turn on angle bias correction
    angord = 0            ! order of polynomial for angle bias correction
    use_edges = .true.    ! .true.=to use data on scan edges

  end subroutine init_rad


  subroutine init_rad_vars
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_rad_vars
!
!   prgrmmr:     derber      org: np23                date: 1995-07-06
!
! abstract:  This routine sets parameters used in the radiance
!            assimilation.  The parameters below depend on values
!            which may be altered by the SETUP namelist.
!
! program history log:
!   1995-07-06  derber
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2008-04-23  safford -- add standard subprogram doc block
!   2010-05-06  zhu     - add option adp_anglebc
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    implicit none
    
!   safeguard angord value for option adp_anglebc
    if (adp_anglebc) then
       if (angord==0) then
          write(6,*)'INIT_RAD_VARS:  ***ERROR*** error value for angord, reset angord to be 4'
          angord=4
       end if
    else
       if (angord/=0) angord=0
    end if

    if (adp_anglebc) npred=npred+angord
    npreds=npred+1

    return
  end subroutine init_rad_vars


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: radinfo_read --- Read satinfo,satang,satbias
!
! !INTERFACE:
!
  subroutine radinfo_read(mype,iout_rad)

! !USES:

    use kinds, only: r_kind,i_kind
    !use obsmod, only: iout_rad
    use constants, only: zero,one
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind), intent(in)::  mype,iout_rad   ! mpi task id

! !DESCRIPTION:  This routine reads the satinfo, satbias\_angle, and
!            satbias files.  
!
!            The satinfo file contains information about the channels,
!            sensors, and satellites.  It specifies observation error
!            for the channels, how to use the channels (assimilate,
!            monitor, etc), the type of channel (ir or microwave),
!            and other useful information.  
!
!            The satbias\_angle file contains the angle dependent part
!            of the brightness temperature bias for each channel/
!            instrument/satellite.  Also included in this file is 
!            the mean temperature lapse rate for each channels 
!            weighted by the weighting function for the given channel/
!            instrument.
!
!            The satbias\_in file contains the coefficients for the
!            predictive part of the bias correction.
!
!
! !REVISION HISTORY:
!   1998-05-15  yang, weiyu
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2004-11-30  xu li- read SST dependent bias for AVHRR radiance (NOAA-16 & NOAA-17) 
!                      and SST analysis when retrieval = .true.
!   2005-02-08  xu li- read SST analysis when retrieval = .true.
!   2005-10-11  treadon - change satinfo read to free format
!   2005-11-30  li - fix a bug in the format to read avhrr SST dependent BC
!   2007-03-13  derber  - modify to allow input bias correction files of different lengths and orders
!   2007-06-29  treadon - determine/build n_sensors and sensorlist from satinfo file
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:    
!   yang        org: np20                date: 1998-05-15
!
!EOP
!-------------------------------------------------------------------------
    
    integer(i_kind) i,j,k,ich,lunin,lunout,nlines
    integer(i_kind) ip,istat,n,ichan,nstep,edge1,edge2,ntlapupdate
    real(r_kind),dimension(npred):: predr
    real(r_kind) tlapm
    real(r_kind) tsum
    real(r_kind) start,step
    real(r_kind),dimension(90)::cbiasx
    character(len=1):: cflg
    character(len=120) crecord
    character(len=20) :: isis 
    character(len=20) :: satscan_sis
    character(len=20),allocatable,dimension(:):: satsenlist
    real(r_kind),dimension(numt):: fbiasx     ! contains SST dependent bias  for SST retrieval
    logical,allocatable,dimension(:):: nfound
    logical cfound,pcexist

    data lunin / 49 /
    data lunout / 51 /

!============================================================================

!   Determine number of entries in satellite information file
    open(lunin,file='satinfo',form='formatted')
    j=0
    nlines=0
    read1:  do
       read(lunin,100,iostat=istat) cflg,crecord
       if (istat /= 0) exit
       nlines=nlines+1
       if (cflg == '!') cycle
       j=j+1
    end do read1
    if (istat>0) then
       close(lunin)
       write(6,*)'RADINFO_READ:  ***ERROR*** error reading radinfo, istat=',istat
       write(6,*)'RADINFO_READ:  stop program execution'
       call stop2(79)
    endif
    jpch_rad = j


!   Allocate arrays to hold radiance information
!     nuchan    - channel number
!     nusis     - sensor/instrument/satellite
!     iuse_rad  - use parameter
!     ifactq    - scaling parameter for d(Tb)/dq sensitivity
!     varch     - variance for each channel
!     varch_cld - variance for cloudy radiance for each channel

    allocate(nuchan(jpch_rad),nusis(jpch_rad),&
         iuse_rad(0:jpch_rad),ifactq(jpch_rad),varch(jpch_rad),&
         varch_cld(jpch_rad),&
         ermax_rad(jpch_rad),b_rad(jpch_rad),pg_rad(jpch_rad))
    allocate(satsenlist(jpch_rad),nfound(jpch_rad))
    iuse_rad(0)=-999
    ifactq=0


!   All mpi tasks open and read radiance information file.
!   Task mype_rad writes information to radiance runtime file

    if (mype==mype_rad) then
       open(iout_rad)
       write(iout_rad,*)'RADINFO_READ:  jpch_rad=',jpch_rad
    endif
    rewind(lunin)
    j=0
    do k=1,nlines
       read(lunin,100) cflg,crecord
       if (cflg == '!') cycle
       j=j+1
       read(crecord,*,err=101) nusis(j),nuchan(j),iuse_rad(j),&
            varch(j),ermax_rad(j),b_rad(j),pg_rad(j)
       if (mype==mype_rad) write(iout_rad,110) j,nusis(j), &
            nuchan(j),varch(j),iuse_rad(j),ermax_rad(j), &
            b_rad(j),pg_rad(j)
       go to 102
 101   continue
       read(crecord,*) nusis(j),nuchan(j),iuse_rad(j),&
            varch(j),varch_cld(j),ermax_rad(j),b_rad(j),pg_rad(j)
       if (mype==mype_rad) write(iout_rad,111) j,nusis(j), &
            nuchan(j),varch(j),varch_cld(j),iuse_rad(j),ermax_rad(j), &
            b_rad(j),pg_rad(j)
 102   continue
    end do
    close(lunin)
100 format(a1,a120)
110   format(i4,1x,a20,' chan= ',i4,  &
            ' var= ',f7.3,' use= ',i2,' ermax= ',F7.3, &
            ' b_rad= ',F7.2,' pg_rad=',F7.2)
111 format(i4,1x,a20,' chan= ',i4,  &
          ' var= ',f7.3,' varch_cld=',f7.3,' use= ',i2,' ermax= ',F7.3, &
          ' b_rad= ',F7.2,' pg_rad=',F7.2)


!   Read in start,step information and cutoff values for scan edges
    allocate(radstart(jpch_rad),radstep(jpch_rad),radnstep(jpch_rad))
    allocate(radedge1(jpch_rad),radedge2(jpch_rad))
    radstart=zero
    radstep =one
    radnstep=90
    radedge1=-1
    radedge2=-1

    inquire(file='scaninfo',exist=pcexist)
    if (pcexist) then
       open(lunin,file='scaninfo',form='formatted')
       do
          read(lunin,1000,IOSTAT=istat) cflg,satscan_sis,start,step,nstep,edge1,edge2
          if (istat /= 0) exit
          if (cflg == '!') cycle

          do j =1,jpch_rad
             if(trim(satscan_sis) == trim(nusis(j)))then
                radstart(j)=start
                radstep(j)=step
                radnstep(j)=nstep
                radedge1(j)=edge1
                radedge2(j)=edge2
             end if
          end do
       end do
1000   format(a1,a20,2f11.3,i10,2i6)
       close(lunin)
    else
       if(mype == 0) write(6,*) '***WARNING file scaninfo not found, use default'

       do j =1,jpch_rad
          call satstep(nusis(j),start,step,nstep,edge1,edge2)
          radstart(j)=start
          radstep(j)=step
          radnstep(j)=nstep
          radedge1(j)=edge1
          radedge2(j)=edge2
       end do
    end if  ! if pcexist

!   Build sensor list based on entries in satinfo file
    n_sensors=1
    satsenlist(n_sensors)=nusis(1)
    do j=2,jpch_rad
       cfound=.false.
       search: do i=1,n_sensors
          if (nusis(j)==satsenlist(i)) then
             cfound=.true.
             exit search
          endif
       end do search
       if (.not.cfound) then
          n_sensors=n_sensors+1
          satsenlist(n_sensors)=nusis(j)
       endif
    end do

    allocate(sensorlist(n_sensors))
    do j=1,n_sensors
       sensorlist(j)=satsenlist(j)
    end do

    if (mype==mype_rad) then
       write(iout_rad,*)'RADINFO_READ:  n_sensors=',n_sensors
       do j=1,n_sensors
          write(iout_rad,120) j,sensorlist(j)
       end do
120    format(3x,'j,sensorlist=',i4,2x,a20)
    endif


!   Allocate arrays to receive angle dependent bias information.
!   Open file to bias file (satang=satbias_angle).  Read data.

    allocate(cbias(90,jpch_rad),tlapmean(jpch_rad))
    cbias=zero
    tlapmean=zero
    if (adp_anglebc) then
       allocate(count_tlapmean(jpch_rad),update_tlapmean(jpch_rad),tsum_tlapmean(jpch_rad))
       count_tlapmean=0
       tsum_tlapmean=zero
       update_tlapmean=.true.
    end if

    if (.not. adp_anglebc) then
       open(lunin,file='satbias_angle',form='formatted')
       nfound = .false.
       read2: do
          read(lunin,'(I5,1x,A20,2x,I4,e15.6/9(4x,10f7.3/))',iostat=istat) &
               ich,isis,ichan,tlapm,(cbiasx(ip),ip=1,90)
          if (istat /= 0) exit
          cfound = .false.
          do j =1,jpch_rad
             if(trim(isis) == trim(nusis(j)) .and. ichan == nuchan(j))then
                cfound = .true.
                nfound(j) = .true.
                do i=1,90
                   cbias(i,j)=cbiasx(i)
                end do
                tlapmean(j)=tlapm
             end if
          end do
          if(.not. cfound .and. mype == 0) &
               write(6,*) '***WARNING instrument/channel ',isis,ichan, &
               'found in satbias_angle file but not found in satinfo'
       end do read2
       close(lunin)
       if (istat>0) then
          write(6,*)'RADINFO_READ:  ***ERROR*** error reading satbias_angle, istat=',istat
          write(6,*)'RADINFO_READ:  stop program execution'
          call stop2(79)
       endif

       if (mype==mype_rad) then
          write(iout_rad,*)'RADINFO_READ:  read satbias_angle file'
          do j=1,jpch_rad
             if(.not. nfound(j))write(iout_rad,*) 'RADINFO_READ: ***WARNING instrument/channel ',&
                  nusis(j),nuchan(j),' not found in satbias_angle file - set to zero '
          end do
       end if
    end if ! end of .not.adp_anglebc


    if ( .not. retrieval ) then

!   Allocate array to hold coefficients for predictive (air mass) part of 
!   bias correction.  Open unit to input file.  Read data.
    allocate(predx(npred,jpch_rad))
    do j=1,jpch_rad
       do i=1,npred
          predx(i,j)=zero
       end do
    end do

    open(lunin,file='satbias_in' ,form='formatted')
    nfound = .false.
    read4: do
       if (.not. adp_anglebc) then
          read(lunin,'(I5,1x,A20,1x,I5,10f12.6)',iostat=istat) ich,isis,&
               ichan,(predr(ip),ip=1,npred)
       else
          read(lunin,'(I5,1x,A20,1x,I5,2e15.6,1x,I5/2(4x,10f12.6/))',iostat=istat) ich,isis,&
               ichan,tlapm,tsum,ntlapupdate,(predr(ip),ip=1,npred)
       endif
       if (istat /= 0) exit
       cfound = .false.
       do j =1,jpch_rad
          if(trim(isis) == trim(nusis(j)) .and. ichan == nuchan(j))then
             cfound = .true.
             nfound(j) = .true.
             do i=1,npred
                predx(i,j)=predr(i)
             end do
          end if
       end do
       if(mype == 0 .and. .not. cfound) &
            write(6,*) '***WARNING instrument/channel ',isis,ichan, &
            'found in satbias_in file but not found in satinfo'
    end do read4
    close(lunin)
    if (istat>0) then
       write(6,*)'RADINFO_READ:  ***ERROR*** error reading satbias_in, istat=',istat
       write(6,*)'RADINFO_READ:  stop program execution'
       call stop2(79)
    endif

    if (mype==mype_rad) then
       write(iout_rad,*)'RADINFO_READ:  guess air mass bias correction coefficients below'
       do j=1,jpch_rad
          if (nfound(j)) then
             write(iout_rad,140) j,trim(nusis(j)),(predx(n,j),n=1,npred)
          else
             write(iout_rad,*) '***WARNING instrument/channel ',&
             nusis(j),nuchan(j),' not found in satbias_in file - set to zero '
          endif
      end do
140    format(i4,1x,a20,10f12.6)
    endif

!      Initialize predx if inew_rad and compute angle bias correction and tlapmean
       if (adp_anglebc) then
!         call init_predx(newpc4pred)    ! init_predx code not yet in enkf radinfo
          do j=1,jpch_rad
             call angle_cbias(trim(nusis(j)),j,cbias(1,j))
          end do

          if (mype==mype_rad) then
             open(lunout,file='satbias_ang.out',form='formatted')
             do j=1,jpch_rad
                write(lunout,'(I5,1x,A20,2x,I4,e15.6/9(4x,10f7.3/))') &
                     j,nusis(j),nuchan(j),tlapmean(j),(cbias(i,j),i=1,90)
             end do
             close(lunout)
          end if
       end if

    endif

! Read SST dependent radiance bias correction lookup table
   if (retrieval) then
      
       allocate(fbias(numt,jpch_rad))
       fbias=zero
       
       if(mype==mype_rad) write(iout_rad,*) &
            'RADINFO_READ:  read SST & D/N dependent bias correction from ',lunin
       open(lunin,file='satbias_sst',form='formatted')
       rewind (lunin)

!      Loop over satellites sensors & channels
       read5: do
          read(lunin,'(I5,1x,a20,1x,I5/3(4x,11f10.3/) )',iostat=istat) ich,isis,ichan,(fbiasx(i),i=1,numt)
          if (istat /= 0) exit
          cfound = .false.
          do j=1,jpch_rad
             if(trim(isis) == trim(nusis(j)) .and. ichan == nuchan(j))then
                cfound = .true.
                do i=1,numt
                   fbias(i,j)=fbiasx(i)
                end do
                tlapmean(j)=tlapm
             end if
          end do
          if(.not. cfound)write(6,*) ' WARNING instrument/channel ',isis,ichan, &
               'found in satbias_sst file and not found in satinfo'
       end do read5
       close(lunin)
    endif           ! endif for if (retrieval) then


!   Close unit for runtime output.  Return to calling routine
    if(mype==mype_rad)close(iout_rad)
    return

  end subroutine radinfo_read


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: radinfo_write --- Write satbias_out file
!
! !INTERFACE:
!
  subroutine radinfo_write()

! !USES:

    !use obsmod, only: iout_rad
    implicit none

! !DESCRIPTION:  This routine writes an updated version of the predictive
!            part of the bias correction.
!
! !REVISION HISTORY:
!   1998-05-15  yang, weiyu
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:    
!   yang        org: np20                date: 1998-05-15
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) lunout,jch,ip
    data lunout / 51 /


!   Open unit to output file.  Write updated coefficients.  Close unit.
    open(lunout,file='satbias_out',form='formatted')
    rewind lunout
    if (.not.adp_anglebc) then
       do jch=1,jpch_rad
          write(lunout,'(I5,1x,a20,1x,i5,10f12.6)') jch,nusis(jch),nuchan(jch),&
               (predx(ip,jch),ip=1,npred)
       end do
    else
       do jch=1,jpch_rad
          write(lunout,'(I5,1x,a20,1x,i5,2e15.6,1x,I5/2(4x,10f12.6/))')jch,nusis(jch),nuchan(jch),&
               tlapmean(jch),tsum_tlapmean(jch),count_tlapmean(jch),(predx(ip,jch),ip=1,npred)
       end do
    endif
    close(lunout)

!   Deallocate data arrays for bias correction and those which hold
!   information from satinfo file.
    deallocate (predx,cbias,tlapmean,nuchan,nusis,iuse_rad, &
                ifactq,varch,varch_cld,sensorlist)
    return
  end subroutine radinfo_write

  integer(i_kind) function newchn(sis,ichan)   ! "satinfo-relative" index of 
                                               ! (sis,ichan) combination
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    function newchn
!
!   prgrmmr:     derber      org: np23                date: 1997-08-13
!
! abstract:  For a given satellite and channel produce a combined 
!            channel number based on input from the satinfo file.
!            If the requested channel/satellite combination is
!            not found, the function returns a zero value.
!
! program history log:
!   1997-08-13  derber
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2008-04-23  safford - add standard subprogram doc block, rm unused uses
!
!   input argument list:
!     sis     - satellite to search for
!     ichan   - channel number to search for
!
!   return:
!             - combined channel number
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

    implicit none

! !INPUT PARAMETERS:

    character(len=20), intent(in   ) :: sis   ! satellite to search for
    integer(i_kind)  , intent(in   ) :: ichan ! channel number to search for

    integer(i_kind) j

    do j=1,jpch_rad
       if ( nuchan(j)==ichan .and. nusis(j)==sis) then
          newchn=j
          return
       end if
    end do
    write(6,*) 'NEWCHN:  channel=',ichan,' sensor/instrument/satellite=',sis, &
         ' not present in satinfo file'
    newchn=0
    return
  end function newchn

   real(r_kind) function rnad_pos(isis,iscan,jch)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    function rnad_pos
!
!   prgrmmr:     zhu      org: np23                date: 2010-05-06
!
! abstract:  For a given satellite/sensor produce the scan angle
!
! program history log:
!   2010-05-06  zhu
!
!   return:
!             - scan angle
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:
   implicit none
   character(len=20),intent(in):: isis
   integer(i_kind),intent(in):: iscan,jch

   integer(i_kind) ifov
   real(r_kind) piece

   if (index(isis,'iasi')/=0) then

      piece=-0.625_r_kind
      if (mod(iscan,2) == 1) piece = 0.625_r_kind
      rnad_pos=radstart(jch)+radstep(jch)*float((iscan-1)/2)+piece

   else

      if (index(isis,'hirs')/=0 .and. (index(isis,'n16')/=0 .or. &
                                       index(isis,'n17')/=0)) then
         ifov=iscan+1
      else
         ifov=iscan
      end if
      rnad_pos=radstart(jch)+radstep(jch)*float(ifov-1)

   end if

   return
   end function rnad_pos

   subroutine angle_cbias(isis,j,cbiasj)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    angle_cbias
!
!   prgrmmr:     zhu      org: np23                date: 2010-05-06
!
! abstract:  For a given satellite/sensor produce angle bias correction
!
! program history log:
!   2010-05-06  zhu
!   2010-12-16  treadon - recode cbiasj to be consistent with setuprad
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

     use constants, only: zero,deg2rad
     implicit none
     
     character(len=20),intent(in):: isis
     integer(i_kind),intent(in):: j
     real(r_kind),dimension(90),intent(inout):: cbiasj
     
     integer(i_kind) i,k
     real(r_kind),dimension(npred):: pred

     pred=zero
     do i=1,radnstep(j)
        pred(npred)=rnad_pos(isis,i,j)*deg2rad
        do k=2,angord
           pred(npred-k+1)=pred(npred)**k
        end do
        cbiasj(i)=zero
        do k=1,angord
           cbiasj(i) = cbiasj(i)+ predx(npred-k+1,j)*pred(npred-k+1)
        end do
        
     end do
     return
   end subroutine angle_cbias

   subroutine satstep(isis,start,step,nstep,edge1,edge2)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    satstep
!
!   prgrmmr:     zhu      org: np23                date: 2010-05-06
!
! abstract:  This routine sets step, start, and nstart
!
! program history log:
!   2010-05-06  zhu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

   use constants, only: zero,one,three
   implicit none

   character(len=20),intent(in) :: isis
   integer(i_kind),intent(out)  :: nstep,edge1,edge2
   real(r_kind),intent(out)     :: start,step

   start=zero
   step =one
   nstep=90
   edge1=-1
   edge2=-1

   if (index(isis,'hirs')/=0) then
      step  = 1.80_r_kind
      start = -49.5_r_kind
      nstep = 56
      edge1 = 7
      edge2 = 50
   else if (index(isis,'msu')/=0) then
      if (index(isis,'amsua')/=0) then
         step  = three + one/three
         start = -48._r_kind - one/three
         nstep = 30
         edge1 = 4
         edge2 = 27
      else if (index(isis,'amsub')/=0) then
         step  = 1.1_r_kind
         start = -48.95_r_kind
         nstep = 90
         edge1 = 10
         edge2 = 81
      else
         step  = 9.474_r_kind
         start = -47.37_r_kind
         nstep = 90
         edge1 = 2
         edge2 = 10
      end if
   else if (index(isis,'mhs')/=0) then
      step  = 10.0_r_kind/9.0_r_kind
      start = -445.0_r_kind/9.0_r_kind
      nstep = 90
      edge1 = 10
      edge2 = 81
   else if (index(isis,'ssu')/=0) then
      step  = 10.0_r_kind
      start = -35.00_r_kind
      nstep = 90
      edge1 = 2
      edge2 = 7
   else if (index(isis,'airs')/=0) then
      step  = 1.1_r_kind
      start = -48.9_r_kind
      nstep = 90
      edge1 = 10
      edge2 = 81
   else if (index(isis,'hsb')/=0) then
      step  = 1.1_r_kind
      start = -48.95_r_kind
      nstep = 90
      edge1 = 10
      edge2 = 81
   else if (index(isis,'iasi')/=0) then
      step  = 3.334_r_kind
      start = -48.33_r_kind
      nstep = 60
      edge1 = 5
      edge2 = 56
   end if

   return
   end subroutine satstep


end module radinfo
