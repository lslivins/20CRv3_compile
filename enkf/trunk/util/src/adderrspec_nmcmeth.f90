program adderrspec_nmcmeth
! convert ganl*mem* files output from EnKF to GFS spectral format,
! adding samples of 48-24 forecast differences with a
! a specified amplitude and zero mean. Initial dates for
! forecasts are read in from dates.dat (this file must be
! created beforehand).
 use random_normal, only : rnorm, set_random_seed, iran
 USE SIGIO_MODULE
 ! module used to generate additive inflation perturbations.
 ! (from random samples of 48-24 forecast differences).
 use constants, only: rd, grav, cp, rearth, init_constants, init_constants_derived
 implicit none
 TYPE(SIGIO_HEAD) :: sigheadi,SIGHEADO,sighead
 TYPE(SIGIO_DATA) :: sigdatai,SIGDATAO,sigdata
 character(len=500) filenamein,filenameout,filenamepert, datapath
 character(len=10) datestring, datestringpert
 integer iret,nlats,nlons,nlevs,ntrac,ntrunc,k,ierr,nanals,&
              nanal,numproc,nproc,iunit,idatein(4),idateout(4),&
              ilonscramble,iargc,nt,&
              iunit2,iunitsf,iunitsf2,iscalefact,na
 logical lonscramble,meanonly
 real(8) t1,t2
 real scalefact
 character(len=4) charnlons,charnlats
 character(len=3) charnanal
 integer, allocatable, dimension(:) :: ishift
 real, dimension(:,:), allocatable :: psg,zsg,&
 psgpert
 real, dimension(:,:,:), allocatable :: ug,vg,tempg,diff,&
 ugpert,vgpert,tempgpert
 real, dimension(:,:,:,:), allocatable :: qg,qgpert
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
 scalefact = iscalefact/100.
 call getarg(6,datapath)
 if (nproc .eq. 0) print *,'scalefact = ',scalefact
 call getarg(7,charnlons)
 ! if ilonscramble=1, the longitudes of the perturbations are randomly
 ! scrambled - equivalent to imposing a zonally homogenous covariance 
 ! structure.
 read(charnlons,'(i4)') ilonscramble
 if (ilonscramble == 0) then
   lonscramble = .false.
 else
   lonscramble = .true.
 end if
 if (iargc() .gt. 7) then
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

 nanal = nproc + 1
 write(charnanal,'(i3.3)') nanal
 if (meanonly) then
    filenamein = "sanl_"//datestring//"_ensmean"
 else
    filenamein = "sanl_"//datestring//"_mem"//charnanal
 endif
 !print *,nproc,nanal,trim(filenamein)
 filenameout = "sanl_"//datestring//"_mem"//charnanal

 allocate(psg(nlons,nlats))
 allocate(psgpert(nlons,nlats))
 allocate(zsg(nlons,nlats))
 allocate(ug(nlons,nlats,nlevs))
 allocate(ugpert(nlons,nlats,nlevs))
 allocate(vg(nlons,nlats,nlevs))
 allocate(vgpert(nlons,nlats,nlevs))
 allocate(tempg(nlons,nlats,nlevs))
 allocate(tempgpert(nlons,nlats,nlevs))
 allocate(diff(nlons,nlats,nlevs))
 allocate(qg(nlons,nlats,nlevs,ntrac))
 allocate(qgpert(nlons,nlats,nlevs,ntrac))
 allocate(ishift(nanals))

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
 call getsigdata(sigdatai,ug,vg,tempg,qg,psg,zsg,nlons,nlats,nlevs,ntrac,ntrunc,mpi_comm_world)
 call sigio_axdata(sigdatai,ierr)
 call sigio_sclose(iunit,ierr)
 call MPI_Barrier(MPI_COMM_WORLD,ierr)

 if (scalefact .gt. 1.e-5) then
 ! compute random longitude shifts on root, brodcast to other tasks.
 if (nproc .eq. 0) then
     call set_random_seed(0, nproc)
     if (lonscramble) then
         call iran(nlons,nanals,ishift)
     else
         ishift=0 ! no longitude scrambling
     end if
 end if
 call MPI_Bcast(ishift,nanals,MPI_INTEGER,0, &
                MPI_COMM_WORLD,ierr)
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
 call getsigdata2(sighead,sigdata,ugpert,vgpert,tempgpert,qgpert,psgpert,nlons,nlats,nlevs,ntrac,ntrunc,mpi_comm_world)
 diff = ugpert
 call shiftlon(diff,nlons,nlats,nlevs,ishift(nanal))
 ugpert = scalefact*diff
 diff = vgpert
 call shiftlon(diff,nlons,nlats,nlevs,ishift(nanal))
 vgpert = scalefact*diff
 diff = tempgpert
 call shiftlon(diff,nlons,nlats,nlevs,ishift(nanal))
 tempgpert = scalefact*diff
 do nt=1,ntrac
    diff = qgpert(:,:,:,nt)
    call shiftlon(diff,nlons,nlats,nlevs,ishift(nanal))
    qgpert(:,:,:,nt) = scalefact*diff
 enddo
 diff(:,:,1) = psgpert
 call shiftlon(diff(:,:,1),nlons,nlats,1,ishift(nanal))
 psgpert = scalefact*diff(:,:,1)
 call sigio_axdata(sigdata,ierr)
 ug = ug + ugpert
 vg = vg + vgpert
 tempg = tempg + tempgpert
 ! noise only added to spec hum (not other tracers)
 qg = qg + qgpert
 psg = psg + psgpert
 !call MPI_Barrier(MPI_COMM_WORLD,ierr)
 if (nanal .eq. 1) print *,'additive perturbations for scalefact = ',scalefact
 print *,nanal,ishift(nanal),datestringpert,' ugpert',minval(ugpert),maxval(ugpert)
 print *,nanal,ishift(nanal),datestringpert,' vgpert',minval(vgpert),maxval(vgpert)
 print *,nanal,ishift(nanal),datestringpert,' tempgpert',minval(tempgpert),maxval(tempgpert)
 print *,nanal,ishift(nanal),datestringpert,' psgpert',minval(psgpert),maxval(psgpert)
 if (nanal .eq. 1) then
    print *,'for nanal = ',nanal
    print *,'min/max pert psg',minval(psgpert),maxval(psgpert)
    do k=1,nlevs
    print *,k,'min/max pert tempg',minval(tempgpert(:,:,k)),maxval(tempgpert(:,:,k))
    print *,k,'min/max pert ug',minval(ugpert(:,:,k)),maxval(ugpert(:,:,k))
    print *,k,'min/max pert vg',minval(vgpert(:,:,k)),maxval(vgpert(:,:,k))
    end do
 end if
 end if

 t1 = MPI_Wtime()
 call sigio_aldata(sigheado,sigdatao,ierr)
 call putsigdata(sigdatao,ug,vg,tempg,qg,psg,zsg,nlons,nlats,nlevs,ntrac,ntrunc)
 t2 = MPI_Wtime()
 !print *,'time to transform grid data to spectral',t2-t1

 t1 = MPI_Wtime()
 call sigio_swohdc(iunitsf2,filenameout,sigheado,sigdatao,ierr)
 call sigio_axdata(sigdatao,ierr)
 t2 = MPI_Wtime()
 !print *,'time to write out spectral data',t2-t1

 call MPI_Barrier(MPI_COMM_WORLD,ierr)
 if (nproc .eq. 0) write(6,*) 'all done!'
 call MPI_Finalize(ierr)
 if (nproc .eq. 0 .and. ierr .ne. 0) then
  print *, 'MPI_Finalize error status = ',ierr
 end if
! create log file that can be checked for normal completion
 open(91,form='formatted',file='adderrspec.log')
 write(91,*) datestring
 close(91)

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

 subroutine getsigdata(sigdata,ug,vg,tempg,qg,psg,zsg,nlons,nlats,nlevs,ntrac,ntrunc,mpi_comm_world)
  use sigio_module
  use specmod
  use constants, only: rd, grav, cp, rearth, init_constants, init_constants_derived
  implicit none
  type (sigio_data), intent(in out) :: sigdata
  real, dimension(nlons,nlats,nlevs), intent(out) :: ug,vg,tempg
  real, dimension(nlons,nlats,nlevs,ntrac), intent(out) :: qg
  real, dimension(nlons,nlats), intent(out) :: psg,zsg
  integer, intent(in) :: ntrunc,nlevs,ntrac,nlons,nlats,mpi_comm_world
  integer i,j,k,ierr,nt
  if (ntrunc .lt. 0) then
    print *,'illegal ntrunc = ',ntrunc
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
  endif
  call init_spec_vars(nlons,nlats,ntrunc,4)
  !==> get U,V,temp,z,q,ps on gaussian grid.
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

 subroutine getsigdata2(sighead,sigdata,ug,vg,tempg,qg,psg,nlons,nlats,nlevs,ntrac,ntrunc,mpi_comm_world)
  use sigio_module
  use specmod
  implicit none
  type (sigio_data), intent(in out) :: sigdata
  type (sigio_head), intent(in) :: sighead 
  real, dimension(nlons,nlats,nlevs), intent(out) :: ug,vg,tempg
  real, dimension(nlons,nlats,nlevs,ntrac), intent(out) :: qg
  real, dimension(nlons,nlats), intent(out) :: psg
  real specdat1((ntrunc+1)*(ntrunc+2)),specdat2((ntrunc+1)*(ntrunc+2))
  integer, intent(in) :: ntrunc,nlevs,ntrac,nlons,nlats,mpi_comm_world
  integer k,ierr,nt
  if (ntrunc .lt. 0) then
    print *,'illegal ntrunc = ',ntrunc
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
  endif
  call init_spec_vars(nlons,nlats,ntrunc,4)
  !==> get U,V,temp,z,q,ps on gaussian grid.
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
