program adderrspec_nmcmeth
! addsamples of 48-24 forecast differences with a
! a specified amplitude and zero mean to analysis ensemle. Initial dates for
! forecasts are read in from dates.dat (this file must be
! created beforehand). Ensemble perts are smoothed, ens mean written out.
 use random_normal, only : rnorm, set_random_seed, iran
 USE SIGIO_MODULE
 use specmod
 use constants, only: rd, grav, cp, rearth, init_constants, init_constants_derived
 implicit none
 TYPE(SIGIO_HEAD) :: sigheadi,SIGHEADO,sighead
 TYPE(SIGIO_DATA) :: sigdatai,SIGDATAO,sigdata
 character(len=120) filenamein,filenameout,filenamepert, &
 datapath,filenameoutmean,fname
 character(len=10) datestring, datestringpert
 integer iret,nlats,nlons,nlevs,ntrac,ntrunc,k,ierr,nanals,&
              nanal,numproc,nproc,iunit,idatein(4),idateout(4),&
              ilonscramble,iargc,nt,&
              iunit2,iunitsf,iunitsf2,iscalefact,na
 logical lonscramble,meanonly,lexist
 real(8) t1,t2
 real scalefactnh,scalefacttr,scalefactsh
 character(len=4) charnlons,charnlats
 character(len=3) charnanal
 integer, allocatable, dimension(:) :: ishift
 integer, dimension(:),allocatable :: smoothparm
 real, dimension(:,:), allocatable :: psg,zsg,&
 psgpert,psgmean,psgmean2
 real, dimension(:,:,:), allocatable :: ug,vg,tempg,diff,&
 psgpert1,ugpert,vgpert,tempgpert,ugmean,vgmean,tempgmean
 real, dimension(:,:,:,:), allocatable :: qg,qgpert,qgmean,qgmean2
! mpi definitions.
 include 'mpif.h'

 call MPI_Init(ierr)
 ! nproc is process number, numproc is total number of processes.
 call MPI_Comm_rank(MPI_COMM_WORLD,nproc,ierr)
 call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)

 ! get nanals,datestring,nlons,nlats,scalefact,lonscramble
 ! from command line.
 call getarg(1,charnlons)
 read(charnlons,'(i4)') nanals
 call getarg(2,datestring)
 call getarg(3,charnlons)
 call getarg(4,charnlats)
 read(charnlons,'(i4)') nlons
 read(charnlats,'(i4)') nlats
 call getarg(5,charnlons)
 ! scalefact is scaling factor for 48-24 forecast differences.
 read(charnlons,'(i4)') iscalefact
 scalefactnh = iscalefact/100.
 call getarg(6,charnlons)
 ! scalefact is scaling factor for 48-24 forecast differences.
 read(charnlons,'(i4)') iscalefact
 scalefacttr = iscalefact/100.
 call getarg(7,charnlons)
 ! scalefact is scaling factor for 48-24 forecast differences.
 read(charnlons,'(i4)') iscalefact
 scalefactsh = iscalefact/100.
 call getarg(8,datapath)
 if (nproc .eq. 0) print *,'scalefact = ',scalefactnh,scalefacttr,scalefactsh
 call getarg(9,charnlons)
 ! if ilonscramble=1, the longitudes of the perturbations are randomly
 ! scrambled - equivalent to imposing a zonally homogenous covariance 
 ! structure.
 read(charnlons,'(i4)') ilonscramble
 if (ilonscramble == 0) then
   lonscramble = .false.
 else
   lonscramble = .true.
 end if
 if (iargc() .gt. 9) then
     meanonly=.true.
 else
     meanonly=.false.
 endif
 if (nproc .eq. 0) print *,iargc(),' meanonly ',meanonly

 if (numproc .lt. nanals) then
    print *,numproc,nanals
    print *,'warning, numproc too small!'
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
 end if


 iunitsf=22
 iunitsf2 = 23
 iunit=7
 iunit2=8
 call init_constants(.false.)
 call init_constants_derived()

 filenamein = "sfg_"//datestring//"_fhr06_ensmean"
 ! only need header here.
 call sigio_sropen(iunit,trim(filenamein),ierr)
 if (ierr .ne. 0) then
    print *,'cannot read file ',filenamein,ierr
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
 end if
 call sigio_srhead(iunit,sigheado,ierr)
 call sigio_sclose(iunit,ierr)

 ntrunc = sigheado%jcap
 ntrac = sigheado%ntrac
 ! if nlons, nlats not given by env vars, use value in sigma file.
 if (nlons <= 0 .or. nlats <= 0) then
    nlats = sigheado%latf
    nlons = sigheado%lonf
 endif
 nlevs = sigheado%levs
 if (nproc .eq. 0) then
    print *,filenamein
    print *,'nlons,nlats,nlevs,ntrunc,ntrac=',nlons,nlats,nlevs,ntrunc,ntrac
 endif
 call init_spec_vars(nlons,nlats,ntrunc,4)

 nanal = nproc + 1
 write(charnanal,'(i3.3)') nanal
 if (meanonly) then
    filenamein = "sanl_"//datestring//"_ensmean"
 else
    filenamein = "sanl_"//datestring//"_mem"//charnanal
 endif
 print *,nproc,nanal,trim(filenamein)
 filenameout = "sanl_"//datestring//"_mem"//charnanal
 filenameoutmean = "sanlensmean_"//datestring//"_mem"//charnanal

 allocate(psg(nlons,nlats))
 allocate(psgpert(nlons,nlats))
 allocate(psgpert1(nlons,nlats,1))
 allocate(psgmean(nlons,nlats))
 allocate(zsg(nlons,nlats))
 allocate(ug(nlons,nlats,nlevs))
 allocate(ugpert(nlons,nlats,nlevs))
 allocate(ugmean(nlons,nlats,nlevs))
 allocate(vg(nlons,nlats,nlevs))
 allocate(vgpert(nlons,nlats,nlevs))
 allocate(vgmean(nlons,nlats,nlevs))
 allocate(tempg(nlons,nlats,nlevs))
 allocate(tempgpert(nlons,nlats,nlevs))
 allocate(tempgmean(nlons,nlats,nlevs))
 allocate(qg(nlons,nlats,nlevs,ntrac))
 allocate(qgpert(nlons,nlats,nlevs,ntrac))
 allocate(qgmean(nlons,nlats,nlevs,ntrac))
 allocate(ishift(nanals))
 ishift = 0
 allocate(smoothparm(nlevs))
 smoothparm = -1

 fname='hybens_smoothinfo'
 inquire(file=trim(fname),exist=lexist)
 if ( lexist ) then
    open(9,form='formatted',file=fname)
    if (nproc .eq. 0) print *,'smoothing parameter by level:'
    do k=1,nlevs
       read(9,'(i3)') smoothparm(k)
       if (nproc .eq. 0) print *,k,smoothparm(k)
    enddo
    close(9)
 else
    if (nproc .eq. 0) print *,'hybens_smoothinfo not found - no smoothing'
 endif


 idatein = sigheado%idate
 read(datestring(1:4),'(i4)') idateout(4)
 read(datestring(5:6),'(i2)') idateout(2)
 read(datestring(7:8),'(i2)') idateout(3)
 read(datestring(9:10),'(i2)') idateout(1)
 !print *,' idatein = ',idatein
 !print *,' idateout = ',idateout
 ! write out to GFS sigma file.
 sigheado%idate = idateout
 sigheado%fhour = 0.
 ! ensemble info
 ! http://www.emc.ncep.noaa.gov/gmb/ens/info/ens_grib.html#gribex
 sigheado%iens(1) = 3 ! pos pert
 sigheado%iens(2) = nanal ! ensemble member number
 sigheado%icen2 = 2 ! sub-center, must be 2 or ens info not used
 !print *,'nlons,nlats,nanals,ntrunc,ntrac',nlons,nlats,nanals,ntrunc,ntrac


! read each ensemble member analysis.

 call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
 call getsigdata(sigdatai,ug,vg,tempg,qg,psg,zsg,nlons,nlats,nlevs,ntrac,ntrunc)
 call sigio_axdata(sigdatai,ierr)
 call sigio_sclose(iunit,ierr)
 call mpi_allreduce(ug,ugmean,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 call mpi_allreduce(vg,vgmean,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 call mpi_allreduce(tempg,tempgmean,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
 call mpi_allreduce(qg,qgmean,nlons*nlats*nlevs*ntrac,mpi_real,mpi_sum,mpi_comm_world,ierr)
 call mpi_allreduce(psg,psgmean,nlons*nlats,mpi_real,mpi_sum,mpi_comm_world,ierr)
 ugmean = ugmean/float(nanals)
 vgmean = vgmean/float(nanals)
 tempgmean = tempgmean/float(nanals)
 qgmean = qgmean/float(nanals)
 psgmean = psgmean/float(nanals)
 call MPI_Barrier(MPI_COMM_WORLD,ierr)

 if (scalefactnh .gt. 0.0 .and. scalefacttr .gt. 0.0 .and. scalefactsh .gt. 0.0) then
    ! compute random longitude shifts on root, brodcast to other tasks.
    if (lonscramble) then
       if (nproc .eq. 0) then
           call set_random_seed(0, nproc)
           call iran(nlons,nanals,ishift)
       end if
       call MPI_Bcast(ishift,nanals,MPI_INTEGER,0, &
                      MPI_COMM_WORLD,ierr)
    end if
    open(9,form='formatted',file='dates.dat')
    do na=1,nanal
       read(9,'(a10,1x,a10)') datestringpert
    enddo
    close(9)
    !print *,nanal,datestringpert,ishift(nanal)
    filenamepert = trim(datapath)//'gfsdiff48-24_'//trim(datestringpert)

    call sigio_srohdc(iunitsf,trim(filenamepert), &
                     sighead,sigdata,iret)
    if (iret .ne. 0) then
       print *,'error opening ',trim(filenamepert)
       flush(6)
       flush(0)
       call MPI_Abort(MPI_COMM_WORLD,101,ierr)
       stop
    end if
    if (iret .ne. 0) then
       print *,'error opening ',trim(filenamepert)
       flush(6)
       flush(0)
       call MPI_Abort(MPI_COMM_WORLD,101,ierr)
       stop
    end if
    allocate(diff(nlons,nlats,nlevs))
    call getsigdata2(sighead,sigdata,ugpert,vgpert,tempgpert,qgpert,psgpert,nlons,nlats,nlevs,ntrac,ntrunc)
    diff = ugpert
    call shiftlon(diff,nlons,nlats,nlevs,ishift(nanal))
    call scaleperts(diff,scalefactnh,scalefacttr,scalefactsh,nlons,nlats,nlevs,gaulats,ugpert)
    diff = vgpert
    call shiftlon(diff,nlons,nlats,nlevs,ishift(nanal))
    call scaleperts(diff,scalefactnh,scalefacttr,scalefactsh,nlons,nlats,nlevs,gaulats,vgpert)
    diff = tempgpert
    call shiftlon(diff,nlons,nlats,nlevs,ishift(nanal))
    call scaleperts(diff,scalefactnh,scalefacttr,scalefactsh,nlons,nlats,nlevs,gaulats,tempgpert)
    do nt=1,ntrac
       diff = qgpert(:,:,:,nt)
       call shiftlon(diff,nlons,nlats,nlevs,ishift(nanal))
       call scaleperts(diff,scalefactnh,scalefacttr,scalefactsh,nlons,nlats,nlevs,gaulats,qgpert(:,:,:,nt))
    enddo
    deallocate(diff)
    allocate(diff(nlons,nlats,1))
    diff(:,:,1) = psgpert(:,:)
    call shiftlon(diff,nlons,nlats,1,ishift(nanal))
    call scaleperts(diff,scalefactnh,scalefacttr,scalefactsh,nlons,nlats,1,gaulats,psgpert1)
    psgpert = psgpert1(:,:,1)
    deallocate(diff)
    allocate(diff(nlons,nlats,nlevs))
    call sigio_axdata(sigdata,ierr)
    ! make sure mean of perts is zero.
    call mpi_allreduce(ugpert,diff,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
    diff = diff/float(nanals)
    ugpert = ugpert - diff
    call mpi_allreduce(vgpert,diff,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
    diff = diff/float(nanals)
    vgpert = vgpert - diff
    call mpi_allreduce(tempgpert,diff,nlons*nlats*nlevs,mpi_real,mpi_sum,mpi_comm_world,ierr)
    diff = diff/float(nanals)
    tempgpert = tempgpert - diff
    deallocate(diff,psgpert1)
    allocate(qgmean2(nlons,nlats,nlevs,ntrac))
    call mpi_allreduce(qgpert,qgmean2,nlons*nlats*nlevs*ntrac,mpi_real,mpi_sum,mpi_comm_world,ierr)
    qgmean2 = qgmean2/float(nanals)
    qgpert = qgpert - qgmean2
    deallocate(qgmean2)
    allocate(psgmean2(nlons,nlats))
    call mpi_allreduce(psgpert,psgmean2,nlons*nlats,mpi_real,mpi_sum,mpi_comm_world,ierr)
    psgmean2 = psgmean2/float(nanals)
    psgpert = psgpert - psgmean2
    deallocate(psgmean2)
 else ! if scalefact = 0, set perts to zero.
    ugpert = 0.
    vgpert = 0.
    tempgpert = 0.
    qgpert = 0.
    psgpert = 0.
 endif ! scalefact > 0.
 ! compute total perturbations.
 ugpert = ug - ugmean + ugpert
 vgpert = vg - vgmean + vgpert
 tempgpert = tempg - tempgmean + tempgpert
 qgpert = qg - qgmean + qgpert
 psgpert = psg - psgmean + psgpert
 ! smooth perturbations
 if (maxval(smoothparm) .gt. 0) then
    call smooth(ugpert,nlons,nlats,nlevs,smoothparm)
    call smooth(vgpert,nlons,nlats,nlevs,smoothparm)
    call smooth(tempgpert,nlons,nlats,nlevs,smoothparm)
    allocate(diff(nlons,nlats,1))
    diff(:,:,1) = psgpert
    call smooth(diff,nlons,nlats,1,smoothparm)
    psgpert = diff(:,:,1)
    deallocate(diff)
    do nt=1,ntrac
      call smooth(qgpert(:,:,:,nt),nlons,nlats,nlevs,smoothparm)
    enddo
 endif
 !call MPI_Barrier(MPI_COMM_WORLD,ierr)
 if (nanal .eq. 1) print *,'perturbations for scalefact =', &
 scalefactnh,scalefacttr,scalefactsh
 print *,nanal,ishift(nanal),datestringpert,' ugpert',minval(ugpert),maxval(ugpert)
 print *,nanal,ishift(nanal),datestringpert,' vgpert',minval(vgpert),maxval(vgpert)
 print *,nanal,ishift(nanal),datestringpert,' tempgpert',minval(tempgpert),maxval(tempgpert)
 print *,nanal,ishift(nanal),datestringpert,' psgpert',minval(psgpert),maxval(psgpert)
 if (nanal .eq. 1) then
    print *,'perts by level for nanal = ',nanal
    print *,'min/max pert psg',minval(psgpert),maxval(psgpert)
    do k=1,nlevs
    print *,k,'min/max pert tempg',minval(tempgpert(:,:,k)),maxval(tempgpert(:,:,k))
    print *,k,'min/max pert ug',minval(ugpert(:,:,k)),maxval(ugpert(:,:,k))
    print *,k,'min/max pert vg',minval(vgpert(:,:,k)),maxval(vgpert(:,:,k))
    end do
 end if

 t1 = MPI_Wtime()
 ! add mean back in
 ug = ugmean + ugpert
 vg = vgmean + vgpert
 tempg = tempgmean + tempgpert
 psg = psgmean + psgpert
 qg = qgmean + qgpert
 deallocate(ugpert,vgpert,tempgpert,qgpert,psgpert)
 call sigio_aldata(sigheado,sigdatao,ierr)
 call putsigdata(sigdatao,ug,vg,tempg,qg,psg,zsg,nlons,nlats,nlevs,ntrac,ntrunc)
 t2 = MPI_Wtime()
 !print *,'time to transform grid data to spectral',t2-t1

 t1 = MPI_Wtime()
 call sigio_swohdc(iunitsf2,filenameout,sigheado,sigdatao,ierr)
 t2 = MPI_Wtime()
 !print *,'time to write out spectral data',t2-t1
 ! write out ens mean also (on root)
 if (nproc .eq. 0) then
    call putsigdata(sigdatao,ugmean,vgmean,tempgmean,qgmean,psgmean,zsg,nlons,nlats,nlevs,ntrac,ntrunc)
    sigheado%iens(1) = 1 ! unperturbed control
    sigheado%iens(2) = 2 ! low res control
    sigheado%icen2 = 2 ! sub-center, must be 2 or ens info not used
    call sigio_swohdc(iunitsf2,filenameoutmean,sigheado,sigdatao,ierr)
    call sigio_axdata(sigdatao,ierr)
 end if
 call sigio_axdata(sigdatao,ierr)
 deallocate(ugmean,vgmean,tempgmean,qgmean,psgmean)
 deallocate(ishift,smoothparm,zsg)

 call MPI_Barrier(MPI_COMM_WORLD,ierr)
 if (nproc .eq. 0) write(6,*) 'all done!'
 call MPI_Finalize(ierr)
 if (nproc .eq. 0 .and. ierr .ne. 0) then
  print *, 'MPI_Finalize error status = ',ierr
 end if
! create log file that can be checked for normal completion
 if (nproc .eq. 0) then
    open(91,form='formatted',file='adderrspec.log')
    write(91,*) datestring
    close(91)
 endif

end program adderrspec_nmcmeth

 subroutine putsigdata(sigdata,ug,vg,tempg,qg,psg,zsg,nlons,nlats,nlevs,ntrac,ntrunc)
  ! put data into sigma file structure, given grids of wind, temp, tracers,
  ! surface pressure (in mb), topography.
  use sigio_module
  use specmod
  implicit none
  type (sigio_data), intent(in out) :: sigdata
  real, dimension(nlons,nlats,nlevs), intent(in out) :: ug,vg,tempg
  real, dimension(nlons,nlats,nlevs,ntrac), intent(in out) :: qg
  real, dimension(nlons,nlats), intent(in out) :: psg,zsg
  integer, intent(in) :: ntrunc,nlevs,ntrac,nlons,nlats
  integer i,j,k,nt
  call init_spec_vars(nlons,nlats,ntrunc,4)
  ! convert to spectral coefficients.
!$omp parallel do private(k,nt)
  do k=1,nlevs
     call sptezv_s(sigdata%d(:,k),sigdata%z(:,k),ug(:,:,k),vg(:,:,k),-1)
     call sptez_s(sigdata%t(:,k),tempg(:,:,k),-1)
     do nt=1,ntrac
        ! clip tracer mixing ratios so they are pos. definite. 
        do j=1,nlats
        do i=1,nlons
           if (qg(i,j,k,nt) .lt. tiny(qg(i,j,k,nt))) qg(i,j,k,nt) = tiny(qg(i,j,k,nt))
        enddo
        enddo
        call sptez_s(sigdata%q(:,k,nt),qg(:,:,k,nt),-1)
     enddo
  enddo
  ! convert psg back to ln(centibars).
  psg = log(psg/10.)
  call sptez_s(sigdata%ps,psg,-1)
  call sptez_s(sigdata%hs,zsg,-1)
 end subroutine putsigdata

 subroutine getsigdata(sigdata,ug,vg,tempg,qg,psg,zsg,nlons,nlats,nlevs,ntrac,ntrunc)
  use sigio_module
  use specmod
  use constants, only: rd, grav, cp, rearth, init_constants, init_constants_derived
  implicit none
  type (sigio_data), intent(in out) :: sigdata
  real, dimension(nlons,nlats,nlevs), intent(out) :: ug,vg,tempg
  real, dimension(nlons,nlats,nlevs,ntrac), intent(out) :: qg
  real, dimension(nlons,nlats), intent(out) :: psg,zsg
  integer, intent(in) :: ntrunc,nlevs,ntrac,nlons,nlats
  integer i,j,k,nt
  call init_spec_vars(nlons,nlats,ntrunc,4)
  !==> get U,V,temp,z,q,ps on gaussian grid.
!$omp parallel do private(k,nt)
  do k=1,nlevs
     call sptezv_s(sigdata%d(:,k),sigdata%z(:,k),ug(:,:,k),vg(:,:,k),1)
     do nt=1,ntrac
        call sptez_s(sigdata%q(:,k,nt),qg(:,:,k,nt),1)
        do j=1,nlats
        do i=1,nlons
           if (qg(i,j,k,nt) .lt. tiny(qg(i,j,k,nt))) qg(i,j,k,nt) = tiny(qg(i,j,k,nt))
        enddo
        enddo
     enddo
     call sptez_s(sigdata%t(:,k),tempg(:,:,k),1)
  enddo
  call sptez_s(sigdata%ps,psg,1)
  call sptez_s(sigdata%hs,zsg,1)
  !==> input psg is ln(ps) in centibars - convert to ps in millibars.
  psg = 10.*exp(psg)

end subroutine getsigdata

 subroutine getsigdata2(sighead,sigdata,ug,vg,tempg,qg,psg,nlons,nlats,nlevs,ntrac,ntrunc)
  use sigio_module
  use specmod
  implicit none
  type (sigio_data), intent(in out) :: sigdata
  type (sigio_head), intent(in) :: sighead 
  real, dimension(nlons,nlats,nlevs), intent(out) :: ug,vg,tempg
  real, dimension(nlons,nlats,nlevs,ntrac), intent(out) :: qg
  real, dimension(nlons,nlats), intent(out) :: psg
  real specdat1((ntrunc+1)*(ntrunc+2)),specdat2((ntrunc+1)*(ntrunc+2))
  integer, intent(in) :: ntrunc,nlevs,ntrac,nlons,nlats
  integer k,nt
  call init_spec_vars(nlons,nlats,ntrunc,4)
  !==> get U,V,temp,z,q,ps on gaussian grid.
!$omp parallel do private(k,nt,specdat1,specdat2)
  do k=1,nlevs
     call spectrunc(sigdata%d(:,k),specdat1,sighead%jcap,ntrunc)
     call spectrunc(sigdata%z(:,k),specdat2,sighead%jcap,ntrunc)
     call sptezv_s(specdat1,specdat2,ug(:,:,k),vg(:,:,k),1)
     do nt=1,ntrac
        call spectrunc(sigdata%q(:,k,nt),specdat1,sighead%jcap,ntrunc)
        call sptez_s(specdat1,qg(:,:,k,nt),1)
     enddo
     call spectrunc(sigdata%t(:,k),specdat1,sighead%jcap,ntrunc)
     call sptez_s(specdat1,tempg(:,:,k),1)
  enddo
  call spectrunc(sigdata%ps,specdat1,sighead%jcap,ntrunc)
  call sptez_s(sigdata%ps,psg,1)

end subroutine getsigdata2

subroutine shiftlon(data,nlons,nlats,nlevs,ishift)
 
 integer, intent(in) :: ishift,nlons,nlats,nlevs
 real, intent(inout), dimension(nlons,nlats,nlevs) :: data
 real, dimension(nlons,nlats,nlevs) :: datasave
 integer ii,i,j,k
 
 datasave = data
 do k=1,nlevs
 do j=1,nlats
 do i=1,nlons
    ii = i + ishift
    if (ii .gt. nlons) ii = ii - nlons
    datasave(i,j,k) = data(ii,j,k)
 enddo
 enddo
 enddo
 data = datasave

end subroutine shiftlon

subroutine spectrunc(specin,specout,ntruncin,ntruncout)
 integer, intent(in) :: ntruncin,ntruncout
 real, intent(in) :: specin((ntruncin+1)*(ntruncin+2))
 real, intent(out) :: specout((ntruncout+1)*(ntruncout+2))
 integer m,n,nm,nmout
 nm = 1
 nmout = 1
 do m=0,ntruncin
    do n=m,ntruncin
       if (n .le. ntruncout) then
          specout(nmout) = specin(nm)
          specout(nmout+1) = specin(nm+1)
          nmout = nmout + 2
       end if
       nm = nm + 2
    enddo
 enddo
end subroutine spectrunc

subroutine smooth(grid,nlons,nlats,nlevs,smoothparm)
 use specmod, only: sptez_s, init_spec_vars
 implicit none
 integer :: m,nm,n,k
 real, intent(inout) :: grid(nlons,nlats,nlevs)
 integer, intent(in) ::  smoothparm(nlevs) ! smoothing parameter.
 integer, intent(in) :: nlons,nlats,nlevs
 real specdat(nlats*(nlats+1),nlevs),smoothfact,pi
 pi = 4.*atan(1.0)
 call init_spec_vars(nlons,nlats,nlats-1,4)
 !$omp parallel do private(k)
 do k=1,nlevs
 if (smoothparm(k) .gt. 0.) call sptez_s(specdat(:,k),grid(:,:,k),-1)
 enddo
 do k=1,nlevs
    if (smoothparm(k) .gt. 0.) then
    nm = 1
    do m=0,nlats-1
       do n=m,nlats-1
          ! gaussian window.
          !smoothfact = exp(-(real(n)/real(smoothparm(k)))**2)
          ! Hann window (cosine bell)
          if (n <= smoothparm(k)) then
             smoothfact = 0.5*(1.0 + cos(pi*real(n)/smoothparm(k)))
          else
             smoothfact = 0.
          endif
          ! rectangular window (straight truncation)
          !if (n <= smoothparm(k)) then
          !   smoothfact = 1.
          !else
          !   smoothfact = 0.
          !endif
          specdat(nm,k) = smoothfact*specdat(nm,k)
          specdat(nm+1,k) = smoothfact*specdat(nm+1,k)
          nm = nm + 2
       enddo
    enddo
    endif
 enddo
 !$omp parallel do private(k)
 do k=1,nlevs
 if (smoothparm(k) .gt. 0.) call sptez_s(specdat(:,k),grid(:,:,k),1)
 enddo
end subroutine smooth

subroutine scaleperts(diff,scalefactnh,scalefacttr,scalefactsh,nlons,nlats,nlevs,gaulats,ugpert)
  integer, intent(in) :: nlons,nlats,nlevs
  real, intent(in) :: scalefactnh,scalefacttr,scalefactsh
  real, intent(in), dimension(nlons,nlats,nlevs) :: diff
  real, intent(out), dimension(nlons,nlats,nlevs) :: ugpert
  real, intent(in), dimension(nlats) :: gaulats
  integer j,k
  real scalefact,rtod,deglat,latbound,latboundpp,latboundpm,latboundmp,latboundmm,delat,delatinv
  latbound = 25.
  delat = 10.
  delatinv = 1./10.
  latboundpp=latbound+0.5*delat
  latboundpm=latbound-0.5*delat
  latboundmp=-latbound+0.5*delat
  latboundmm=-latbound-0.5*delat
  rtod = 4.*atan(1.0)/180.
  do k=1,nlevs
  do j=1,nlats
     deglat = rtod*asin(gaulats(j))
     if (deglat > latboundpp) then
        scalefact = scalefactnh
     else if (deglat >= latboundpm) then
        scalefact = ((latboundpp-deglat)*scalefacttr + (deglat-latboundpm)*scalefactnh)*delatinv
     else if (deglat > latboundmp) then
        scalefact = scalefacttr
     else if (deglat >= latboundmm) then
        scalefact = ((latboundpp+deglat)*scalefacttr + (-deglat+latboundmp)*scalefactsh)*delatinv
     else
        scalefact = scalefactsh
     end if
     ugpert(:,j,k) = scalefact*diff(:,j,k)
  enddo
  enddo
end subroutine scaleperts
