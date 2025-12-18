program adderrspec_nmcmeth
! addsamples of 48-24 forecast differences with a
! a specified amplitude and zero mean to analysis ensemle. Initial dates for
! forecasts are read in from dates.dat (this file must be
! created beforehand). Mean of ensemble perts is set to zero, ens mean written out.
 USE SIGIO_MODULE
 implicit none
 TYPE(SIGIO_HEAD) :: sigheadi,SIGHEADO,sighead_hires
 TYPE(SIGIO_DATA) :: sigdatai,SIGDATAO,sigdata,sigdata_hires
 character(len=120) filenamein,filenameout,filenamepert, &
 datapath,filenameoutmean
 character(len=10) datestring, datestringpert
 integer iret,nlevs,ntrac,ntrunc,ierr,nanals,nt,k,&
         nanal,numproc,nproc,iunit,idatein(4),idateout(4),&
         iargc,iunit2,iunitsf,iunitsf2,iscalefact,nlons,nlats
 logical meanonly,lexist
 real, dimension(:,:), allocatable :: psg,psgmean,psgpertmean,psgi
 real scalefact
 character(len=4) char4
 character(len=3) charnanal
! mpi definitions.
 include 'mpif.h'

 call MPI_Init(ierr)
 ! nproc is process number, numproc is total number of processes.
 call MPI_Comm_rank(MPI_COMM_WORLD,nproc,ierr)
 call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)

 ! get nanals,datestring,scalefact,(meanonly)
 ! from command line.
 call getarg(1,char4)
 read(char4,'(i4)') nanals
 if (nproc .le. nanals-1) then ! don't do anything on task > nanals-1
 call getarg(2,datestring)
 call getarg(3,char4)
 ! scalefact is scaling factor for 48-24 forecast differences.
 read(char4,'(i4)') iscalefact
 scalefact = iscalefact/100.
 call getarg(4,datapath)
 if (nproc .eq. 0) print *,'scalefact = ',scalefact
 if (iargc() .gt. 4) then
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

 ! read first guess mean to get header.
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
 nlevs = sigheado%levs
 nlats = sigheado%latb
 nlons = sigheado%lonb
 allocate(psg(nlons,nlats),psgmean(nlons,nlats),psgpertmean(nlons,nlats),psgi(nlons,nlats))
 if (nproc .eq. 0) then
    print *,filenamein
    print *,'nlevs,ntrunc,ntrac=',nlevs,ntrunc,ntrac
 endif

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

 ! reset date in header
 idatein = sigheado%idate
 read(datestring(1:4),'(i4)') idateout(4)
 read(datestring(5:6),'(i2)') idateout(2)
 read(datestring(7:8),'(i2)') idateout(3)
 read(datestring(9:10),'(i2)') idateout(1)
 !print *,' idatein = ',idatein
 !print *,' idateout = ',idateout
 sigheado%idate = idateout
 sigheado%fhour = 0.
 call sigio_aldata(sigheado,sigdatao,ierr)
 call sigio_aldata(sigheado,sigdata,ierr)

! read each ensemble member analysis.
 call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
 if (iret .ne. 0) then
    print *,'error opening ',trim(filenamein)
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
 end if

! compute ensemble sums.
 call mpi_allreduce(sigdatai%z,sigdatao%z,(ntrunc+1)*(ntrunc+2)*nlevs,mpi_real,mpi_sum,mpi_comm_world,iret)
 call mpi_allreduce(sigdatai%d,sigdatao%d,(ntrunc+1)*(ntrunc+2)*nlevs,mpi_real,mpi_sum,mpi_comm_world,iret)
 call mpi_allreduce(sigdatai%t,sigdatao%t,(ntrunc+1)*(ntrunc+2)*nlevs,mpi_real,mpi_sum,mpi_comm_world,iret)
 call mpi_allreduce(sigdatai%q,sigdatao%q,(ntrunc+1)*(ntrunc+2)*nlevs*ntrac,mpi_real,mpi_sum,mpi_comm_world,iret)
 ! convert sums to means. 
 sigdatao%z = sigdatao%z/float(nanals)
 sigdatao%d = sigdatao%d/float(nanals)
 sigdatao%t = sigdatao%t/float(nanals)
 sigdatao%q = sigdatao%q/float(nanals)
 sigdatao%hs = sigdatai%hs
 ! get ps, ps mean on grid.
 call sptez(0,ntrunc,4,nlons,nlats,sigdatai%ps,psgi,1)
 psgi = 10.*exp(psgi)
 ! mean of lnps in spectral space
 !call mpi_allreduce(sigdatai%ps,sigdatao%ps,(ntrunc+1)*(ntrunc+2),mpi_real,mpi_sum,mpi_comm_world,iret)
 !sigdatao%ps = sigdatao%ps/float(nanals)
 ! convert to grid.
 !call sptez(0,ntrunc,4,nlons,nlats,sigdatao%ps,psgmean,1)
 !psgmean = 10.*exp(psgmean)
 ! mean of ps in grid space
 call mpi_allreduce(psgi,psgmean,nlons*nlats,mpi_real,mpi_sum,mpi_comm_world,iret)
 psgmean = psgmean/float(nanals)
 ! convert to spectral
 call sptez(0,ntrunc,4,nlons,nlats,sigdatao%ps,log(psgmean/10.),-1)
 if (nproc .eq. 0) print *,'min/max psgmean =',minval(psgmean),maxval(psgmean)
! write out ens. mean on root.
 if (nproc .eq. 0) then
    ! write out.
    sigheado%iens(1) = 1 ! unperturbed control
    sigheado%iens(2) = 2 ! low res control
    sigheado%icen2 = 2 ! sub-center, must be 2 or ens info not used
    call sigio_swohdc(iunit,filenameoutmean,sigheado,sigdatao,iret)
 end if 

 ! read a perturbation.
 ! get date from dates.dat text file
 inquire(file='dates.dat',exist=lexist)
 if (lexist)  then
    open(9,form='formatted',file='dates.dat')
    do k=1,nanal
       read(9,'(a10,1x,a10)') datestringpert
    enddo
    close(9)
 else
    print *,'error opening dates.dat'
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
 end if
 filenamepert = trim(datapath)//'gfsdiff48-24_'//trim(datestringpert)
 call sigio_srohdc(iunitsf,trim(filenamepert),sighead_hires,sigdata_hires,iret)
 if (iret .ne. 0) then
    print *,'error opening ',trim(filenamepert)
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,ierr)
    stop
 end if
 ! truncate.
 call spectrunc(sigdata_hires%ps,sigdata%ps,sighead_hires%jcap,ntrunc)
 do k=1,nlevs
    call spectrunc(sigdata_hires%d(:,k),sigdata%d(:,k),sighead_hires%jcap,ntrunc)
    call spectrunc(sigdata_hires%z(:,k),sigdata%z(:,k),sighead_hires%jcap,ntrunc)
    call spectrunc(sigdata_hires%t(:,k),sigdata%t(:,k),sighead_hires%jcap,ntrunc)
    do nt=1,ntrac
       call spectrunc(sigdata_hires%q(:,k,nt),sigdata%q(:,k,nt),sighead_hires%jcap,ntrunc)
    enddo
 enddo
 call sigio_axdata(sigdata_hires,iret)

! make sure mean of perts is zero.
 call mpi_allreduce(sigdata%z,sigdatao%z,(ntrunc+1)*(ntrunc+2)*nlevs,mpi_real,mpi_sum,mpi_comm_world,iret)
 call mpi_allreduce(sigdata%d,sigdatao%d,(ntrunc+1)*(ntrunc+2)*nlevs,mpi_real,mpi_sum,mpi_comm_world,iret)
 call mpi_allreduce(sigdata%t,sigdatao%t,(ntrunc+1)*(ntrunc+2)*nlevs,mpi_real,mpi_sum,mpi_comm_world,iret)
 call mpi_allreduce(sigdata%q,sigdatao%q,(ntrunc+1)*(ntrunc+2)*nlevs*ntrac,mpi_real,mpi_sum,mpi_comm_world,iret)
! pert ps in mb, *not* logps in cb.
 call sptez(0,ntrunc,4,nlons,nlats,sigdata%ps,psg,1)
 print *,nanal,minval(psg),maxval(psg)
 call mpi_allreduce(psg,psgpertmean,nlons*nlats,mpi_real,mpi_sum,mpi_comm_world,iret)
 sigdata%z = sigdata%z - sigdatao%z/float(nanals)
 sigdata%d = sigdata%d - sigdatao%d/float(nanals)
 sigdata%t = sigdata%t - sigdatao%t/float(nanals)
 sigdata%q = sigdata%q - sigdatao%q/float(nanals)
 psgpertmean = psgpertmean/float(nanals)
 if (nproc .eq. 0) print *,'psgpert mean removed',&
 minval(psgpertmean),maxval(psgpertmean)
 psg = psg - psgpertmean
 !call mpi_allreduce(psg,psgpertmean,nlons*nlats,mpi_real,mpi_sum,mpi_comm_world,iret)
 !psgpertmean = psgpertmean/float(nanals)
 !if (nproc .eq. 0) print *,'new psgpert mean (should be zero)',&
 !minval(psgpertmean),maxval(psgpertmean)

 ! add scaled perturbation.
 sigdatao%z = sigdatai%z + scalefact*sigdata%z
 sigdatao%d = sigdatai%d + scalefact*sigdata%d
 sigdatao%t = sigdatai%t + scalefact*sigdata%t
 sigdatao%q = sigdatai%q + scalefact*sigdata%q
 psgi = psgi + scalefact*psg
 ! mean should be unchanged
 !call mpi_allreduce(psgi,psgpertmean,nlons*nlats,mpi_real,mpi_sum,mpi_comm_world,iret)
 !psgpertmean = psgpertmean/float(nanals)
 !if (nproc .eq. 0) print *,'min/max psg mean diff (should be zero) = ',&
 !minval(psgmean-psgpertmean),maxval(psgmean-psgpertmean)
 psgi = alog(psgi/10.)
 call sptez(0,ntrunc,4,nlons,nlats,sigdatao%ps,psgi,-1)
 sigdatao%hs = sigdatai%hs

 ! print out stats for ps
 call sptez(0,ntrunc,4,nlons,nlats,sigdatao%ps,psg,1)
 psg = 10.*exp(psg)
 call mpi_allreduce(psg,psgpertmean,nlons*nlats,mpi_real,mpi_sum,mpi_comm_world,iret)
 psgpertmean = psgpertmean/float(nanals)
 print *,'min/max psgpert for nanal',nanal,'=',minval(psg-psgpertmean),maxval(psg-psgpertmean)
 !if (nproc .eq. 0) then
 !   open(7,file='psg.dat',form='unformatted',access='direct',recl=nlons*nlats*4)
 !   write(7,rec=1) psgmean
 !   write(7,rec=2) psgpertmean
 !   write(7,rec=3) psg
 !   close(7)
 !end if

 ! set ensemble info in header.
 ! http://www.emc.ncep.noaa.gov/gmb/ens/info/ens_grib.html#gribex
 sigheado%iens(1) = 3 ! pos pert
 sigheado%iens(2) = nanal ! ensemble member number
 sigheado%icen2 = 2 ! sub-center, must be 2 or ens info not used

 call sigio_swohdc(iunitsf2,filenameout,sigheado,sigdatao,ierr)
 call sigio_axdata(sigdatao,ierr)
 call sigio_axdata(sigdata,ierr)
 call sigio_axdata(sigdatai,ierr)
 deallocate(psg,psgmean,psgpertmean,psgi)

 end if ! no work to be done on this processor.

 call MPI_Barrier(MPI_COMM_WORLD,ierr)
! create log file that can be checked for normal completion
 if (nproc .eq. 0) then
    open(91,form='formatted',file='adderrspec.log')
    write(91,*) datestring
    close(91)
 endif
 if (nproc .eq. 0) write(6,*) 'all done!'
 call MPI_Finalize(ierr)
 if (nproc .eq. 0 .and. ierr .ne. 0) then
  print *, 'MPI_Finalize error status = ',ierr
 end if

end program adderrspec_nmcmeth

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
