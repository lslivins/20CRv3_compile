program grdtospec_ncep
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
 TYPE(SIGIO_HEAD) :: sigheadi,SIGHEADO,sighead24,sighead48
 TYPE(SIGIO_DATA) :: sigdatai,SIGDATAO,sigdata24,sigdata48
 character(len=500) filenamein,filenameout,filename24,filename48, datapath
 character(len=10) datestring,datestring24,datestring48
 integer iret,nlats,nlons,nlevs,ntrac,ntrunc,k,ierr,nanals,&
              nanal,numproc,nproc,iunit,idatein(4),idateout(4),&
              iunitsf24,iunitsf48,ilonscramble,iargc,ipair,&
              iunit2,iunitsf,iunitsf2,iscalefact,na
 integer:: krsize,npert,i
 integer,dimension(4):: iadate
 integer,allocatable,dimension(:):: nrnd,iwork
 real(8):: rseed
 real,allocatable,dimension(:):: rwork

 logical lonscramble,meanonly
 real(8) t1,t2
 real scalefact
 character(len=4) charnlons,charnlats
 character(len=3) charnanal,ipairname
 integer, allocatable, dimension(:) :: ishift
 real, dimension(:), allocatable  :: glats
 real, dimension(:,:), allocatable :: psg,zsg,&
 psgpert,psg24,psg48
 real, dimension(:,:,:), allocatable :: ug,vg,tempg,psig,diff,&
 ugpert,vgpert,tempgpert,qgpert,pg,&
 pslg,ug24,vg24,tempg24,psig24,pslg24,ug48,tempg48,vg48,psig48,pslg48
 real, dimension(:,:,:,:), allocatable :: qg,qg24,qg48
! mpi definitions.
 include 'mpif.h'

 call MPI_Init(ierr)
 ! nproc is process number, numproc is total number of processes.
 call MPI_Comm_rank(MPI_COMM_WORLD,nproc,ierr)
 call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)

 ! get nanals,datestring,nlons,nlats,scalefact,lonscramble,npert
 ! from command line.
 call getarg(1,charnlons)
 read(charnlons,'(i4)') nanals
 call getarg(2,datestring)
 read(datestring,'(i4,i2,i2,i2)') iadate(1),iadate(2),iadate(3),iadate(4)
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
 call getarg(8,charnlons)
 read(charnlons,'(i4)') npert
 if (ilonscramble == 0) then
   lonscramble = .false.
 else
   lonscramble = .true.
 end if
 if (iargc() .gt. 8) then
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

! Generate random numbers for pairs
 call random_seed(size=krsize)
 allocate(nrnd(krsize))
 rseed = 1.0e6_8*iadate(1) + 1.0e4_8*iadate(2) + 1.0e2_8*iadate(3) + iadate(4)
 do i=1,krsize
    nrnd(i) = rseed
 end do
 call random_seed(put=nrnd)
 deallocate(nrnd)
 allocate(rwork(nanals),iwork(nanals))
 call random_number(rwork)
 do i=1,nanals
    iwork(i)=rwork(i)*npert+1
    iwork(i)=min(max(1,iwork(i)),npert)
 end do
 if (nproc==0) then
    open(9,form='formatted',file='sigpairs.dat')
    do i=1,nanals
       write(9,'(i3)') iwork(i)
    end do
    close(9)
    write(6,*)'rseed=',rseed,' nanals=',nanals,' npert=',npert
    write(6,*)'random pair=',(iwork(i),i=1,nanals)
 endif
 call mpi_barrier(mpi_comm_world,ierr)
 deallocate(rwork,iwork)

 iunitsf=22
 iunitsf2 = 23
 iunitsf24 = 24
 iunitsf48 = 48
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

 if (nanal.le.nanals) then
   write(charnanal,'(i3.3)') nanal

   if (meanonly) then
      filenamein = "sanl_"//datestring//"_ensmean"
   else
      filenamein = "sanl_"//datestring//"_mem"//charnanal
   endif

!  Do not overwrite input file.  Write output to uniquely named file
!! filenameout = "sanl_"//datestring//"_mem"//charnanal
   filenameout = "sanlp_"//datestring//"_mem"//charnanal

   write(6,*) 'nproc, myfilein = ',nanal,filenamein
   write(6,*) 'nproc, myfileout = ',nanal,filenameout
 else
   write(6,*) 'no files to process for mpi task = ',nproc
 end if

 allocate(psg(nlons,nlats))
 allocate(psg24(nlons,nlats))
 allocate(psg48(nlons,nlats))
 allocate(zsg(nlons,nlats))
 allocate(ug(nlons,nlats,nlevs))
 allocate(ug24(nlons,nlats,nlevs))
 allocate(ug48(nlons,nlats,nlevs))
 allocate(vg(nlons,nlats,nlevs))
 allocate(vg24(nlons,nlats,nlevs))
 allocate(vg48(nlons,nlats,nlevs))
 allocate(tempg(nlons,nlats,nlevs))
 allocate(tempg24(nlons,nlats,nlevs))
 allocate(tempg48(nlons,nlats,nlevs))
 allocate(psig(nlons,nlats,nlevs+1))
 allocate(psig24(nlons,nlats,nlevs+1))
 allocate(psig48(nlons,nlats,nlevs+1))
 allocate(pslg(nlons,nlats,nlevs))
 allocate(pslg24(nlons,nlats,nlevs))
 allocate(pslg48(nlons,nlats,nlevs))
 allocate(diff(nlons,nlats,nlevs))
 allocate(qg(nlons,nlats,nlevs,ntrac))
 allocate(qg24(nlons,nlats,nlevs,ntrac))
 allocate(qg48(nlons,nlats,nlevs,ntrac))
 allocate(ishift(nanals))
 allocate(glats(nlats))
 allocate(ugpert(nlons,nlats,nlevs))
 allocate(vgpert(nlons,nlats,nlevs))
 allocate(tempgpert(nlons,nlats,nlevs))
 allocate(psgpert(nlons,nlats))
 allocate(qgpert(nlons,nlats,nlevs))
 allocate(pg(nlons,nlats,nlevs))

 IF (nanal.le.nanals) then

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
 call getsigdata(sigheadi,sigdatai,glats,ug,vg,tempg,qg,psg,pslg,psig,zsg,nlons,nlats,nlevs,ntrac,sigheadi%jcap,mpi_comm_world)
 call sigio_axdata(sigdatai,ierr)
 call sigio_sclose(iunit,ierr)
 end if  !END IF MYPE CHECK

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

 if (nanal.le.nanals) then

 open(9,form='formatted',file='sigpairs.dat')
 do na=1,nanal
    read(9,'(i3)') ipair
 enddo
 write(ipairname,'(i3.3)') ipair

 close(9)
 write(6,*) 'NANAL,ISHIFT = ',nanal,ishift(nanal)

! Now have T254L64 pairs.  Rename files.
!! filename24 = trim(datapath)//'/sigf24.gfs190.pair'//trim(ipairname)
!! filename48 = trim(datapath)//'/sigf48.gfs190.pair'//trim(ipairname)
 filename24 = trim(datapath)//'/sigf24.gfs254.pair'//trim(ipairname)
 filename48 = trim(datapath)//'/sigf48.gfs254.pair'//trim(ipairname)

 print *,trim(filename24)
 print *,trim(filename48)

 call sigio_srohdc(iunitsf24,trim(filename24), &
                  sighead24,sigdata24,iret)
 if (iret .ne. 0) then
    print *,'error opening ',trim(filename24)
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
 end if
 call sigio_srohdc(iunitsf48,trim(filename48), &
                  sighead48,sigdata48,iret)
 if (iret .ne. 0) then
    print *,'error opening ',trim(filename48)
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
 end if
 call getsigdata(sighead24,sigdata24,glats,ug24,vg24,tempg24,qg24,psg24,pslg24,psig24,zsg,nlons,nlats,nlevs,ntrac,sighead24%jcap,mpi_comm_world)
 call getsigdata(sighead48,sigdata48,glats,ug48,vg48,tempg48,qg48,psg48,pslg48,psig48,zsg,nlons,nlats,nlevs,ntrac,sighead48%jcap,mpi_comm_world)
 diff = ug48-ug24
 call shiftlon(diff,nlons,nlats,nlevs,ishift(nanal))
 ugpert = scalefact*diff
 diff = vg48-vg24
 call shiftlon(diff,nlons,nlats,nlevs,ishift(nanal))
 vgpert = scalefact*diff
 diff = tempg48-tempg24
 call shiftlon(diff,nlons,nlats,nlevs,ishift(nanal))
 tempgpert = scalefact*diff
 diff = qg48(:,:,:,1)-qg24(:,:,:,1)
 call shiftlon(diff,nlons,nlats,nlevs,ishift(nanal))
 qgpert = scalefact*diff
 diff(:,:,1) = psg48-psg24
 call shiftlon(diff(:,:,1),nlons,nlats,1,ishift(nanal))
 psgpert = scalefact*diff(:,:,1)
 call sigio_axdata(sigdata24,ierr)
 call sigio_axdata(sigdata48,ierr)
 ug = ug + ugpert
 vg = vg + vgpert
 tempg = tempg + tempgpert
 ! noise only added to spec hum (not other tracers)
 qg(:,:,:,1) = qg(:,:,:,1) + qgpert
 psg = psg + psgpert
 !call MPI_Barrier(MPI_COMM_WORLD,ierr)
 if (nanal .eq. 1) print *,'additive perturbations for scalefact = ',scalefact
 print *,nanal,minval(ugpert),maxval(ugpert)
 print *,nanal,minval(vgpert),maxval(vgpert)
 print *,nanal,minval(tempgpert),maxval(tempgpert)
 print *,nanal,minval(psgpert),maxval(psgpert)
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

 !deallocate(ug,vg,tempg,psg,zsg,qg,grid_tmp,psig)

  end if ! end if mype


 call MPI_Barrier(MPI_COMM_WORLD,ierr)
 if (nproc .eq. 0) write(6,*) 'all done!'
 call MPI_Finalize(ierr)
 if (nproc .eq. 0 .and. ierr .ne. 0) then
  print *, 'MPI_Finalize error status = ',ierr
 end if

end program grdtospec_ncep

 subroutine putsigdata(sigdata,ug,vg,tempg,qg,psg,zsg,nlons,nlats,nlevs,ntrac,ntrunc)
  ! put data into sigma file structure, given grids of wind, temp, tracers,
  ! surface pressure (in mb), topography.
  use sigio_module
  use specmod
  use constants, only: rd, grav, cp, rearth, init_constants, init_constants_derived
  implicit none
  type (sigio_data), intent(in out) :: sigdata
  real, dimension(nlons,nlats,nlevs), intent(in out) :: ug,vg,tempg
  real, dimension(nlons,nlats,nlevs,ntrac), intent(in out) :: qg
  real, dimension(nlons,nlats), intent(in out) :: psg,zsg
  integer, intent(in) :: ntrunc,nlevs,ntrac,nlons,nlats
  integer i,j,k,nt
  call init_spec_vars(nlons,nlats,ntrunc,4)
  call init_constants(.false.)
  call init_constants_derived()
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

 subroutine getsigdata(sighead,sigdata,glats,ug,vg,tempg,qg,psg,pslg,psig,zsg,nlons,nlats,nlevs,ntrac,ntrunc,mpi_comm_world)
  use sigio_module
  use specmod
  use constants, only: rd, grav, cp, rearth, init_constants, init_constants_derived
  implicit none
  type (sigio_data), intent(in out) :: sigdata
  type (sigio_head), intent(in) :: sighead 
  real ak(nlevs+1),bk(nlevs+1),kap1,kapr
  real, dimension(nlons,nlats,nlevs), intent(out) :: ug,vg,tempg,pslg
  real, dimension(nlons,nlats,nlevs+1), intent(out) :: psig
  real, dimension(nlons,nlats,nlevs,ntrac), intent(out) :: qg
  real, dimension(nlons,nlats), intent(out) :: psg,zsg
  real, dimension(nlats), intent(out) :: glats
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
  call init_constants(.false.)
  call init_constants_derived()
  kap1 = (rd/cp)+1.0
  kapr = (cp/rd)
  do j=1,nlats
     glats(j) = asin(gaulats(j))
  enddo
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
  if (sighead%idvc .eq. 0) then ! sigma coordinate, old file format.
      ak = 0.
      bk = sighead%si(1:nlevs+1)
  else if (sighead%idvc .eq. 1) then ! sigma coordinate
      ak = 0.
      bk = sighead%vcoord(1:nlevs+1,2)
  else if (sighead%idvc .eq. 2 .or. sighead%idvc .eq. 3) then ! hybrid coordinate
      bk = sighead%vcoord(1:nlevs+1,2) 
      ak = 0.01*sighead%vcoord(1:nlevs+1,1)  ! convert to mb
  else
      print *,'unknown vertical coordinate type',sighead%idvc
      flush(6)
      flush(0)
      call MPI_Abort(MPI_COMM_WORLD,101,ierr)
      stop
  end if
  !==> pressure at layers and interfaces.
  do k=1,nlevs+1
   psig(:,:,k)=ak(k)+bk(k)*psg(:,:) 
  enddo
  do k=1,nlevs
   ! gsi formula ("phillips vertical interpolation")
   pslg(:,:,k)=((psig(:,:,k)**kap1-psig(:,:,k+1)**kap1)/&
                (kap1*(psig(:,:,k)-psig(:,:,k+1))))**kapr
   ! average of interface exner
   !pslg(:,:,k) = (0.5*(psig(:,:,k)**kapr + psig(:,:.k+1)**kapr))**(1./kapr)
   ! average of log(p) - consistent with linear interp in log(p) used
   ! in forward operator.
   !pslg(:,:,k) = exp( 0.5*( log(psig(:,:,k)) + log(psig(:,:,k+1)) ) )
   ! simple average of p.
   !pslg(:,:,k) = 0.5*(psig(:,:,k)+psig(:,k+1))
  end do

end subroutine getsigdata

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
