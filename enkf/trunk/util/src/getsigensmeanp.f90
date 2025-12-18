program getsigensmeanp

 USE SIGIO_MODULE
 implicit none
 TYPE(SIGIO_HEAD) :: sigheadi,sigheado
 TYPE(SIGIO_DATA) :: sigdatai,sigdatao
 character(len=500) filenamein,filenameout,datapath,fileprefix
 integer iret,nlats,nlons,nlevs,ntrac,ntrunc,nanals,&
         i,j,k,nanal,numproc,nproc,iunit,iunit2
 character(len=3) charnanal
 real latmin,sumvar,sumcoslat,rlat,pi,coslat,rmin,rmax
 real, dimension(:,:), allocatable :: psg,psgmean,psgvar
 real, dimension(:), allocatable :: gaulats,gauwts

! mpi definitions.
 include 'mpif.h'

 call MPI_Init(iret)
 ! nproc is process number, numproc is total number of processes.
 call MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
 call MPI_Comm_size(MPI_COMM_WORLD,numproc,iret)

 call getarg(1,datapath)
 call getarg(2,filenameout)
 call getarg(3,fileprefix)
 call getarg(4,charnanal)
 read(charnanal,'(i3)') nanals
 filenameout = trim(adjustl(datapath))//filenameout

 latmin = 20.
 pi = 4.*atan(1.0)

 if (numproc .lt. nanals) then
    print *,numproc,nanals
    print *,'warning, numproc too small!'
    call MPI_Abort(MPI_COMM_WORLD,101,iret)
    stop
 end if

 nanal = nproc+1


 write(charnanal,'(i3.3)') nanal

 filenamein = trim(adjustl(datapath))// &
 trim(adjustl(fileprefix))//'_mem'//charnanal
 write(6,*)'process nanal=',nanal,' filenamein=',trim(filenamein)

! read each ensemble member FHDFI forecast.

 iunit = 21; iunit2 = 61
 call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)

 ! check data
 if (iret .ne. 0) then
   print *,'non-zero return code from sigio_srohdc for nanal=',nanal
   call mpi_abort(MPI_COMM_WORLD,1,iret)
 endif
 do k=1,sigheadi%levs
    rmin = minval(sigdatai%t(:,k))
    rmax = maxval(sigdatai%t(:,k))
    if (rmin .eq. rmax .or. rmin < -1000 .or. rmax .gt. 1000) then
       print *,'bad data for nanal=',nanal
       call mpi_abort(MPI_COMM_WORLD,1,iret)
    endif
 enddo

 ntrunc = sigheadi%jcap
 ntrac = sigheadi%ntrac
 nlats = sigheadi%latf
 nlons = sigheadi%lonf
 nlevs = sigheadi%levs
 if (nproc .eq. 0) then
    print *,filenamein
    print *,'nlons,nlats,nlevs,ntrunc,ntrac=',nlons,nlats,nlevs,ntrunc,ntrac
 end if
 allocate(psg(nlons,nlats))
 allocate(gaulats(nlats))
 allocate(gauwts(nlats))
 allocate(psgmean(nlons,nlats))
 allocate(psgvar(nlons,nlats))
 call splat(4,nlats,gaulats,gauwts)

 call sigio_aldata(sigheadi,sigdatao,iret)
 sigheado = sigheadi

! compute ensemble means.
 call mpi_allreduce(sigdatai%z,sigdatao%z,(ntrunc+1)*(ntrunc+2)*nlevs,mpi_real,mpi_sum,mpi_comm_world,iret)
 if (iret .ne. 0) then
   print *,'non-zero return code from mpi_allreduce'
   call mpi_abort(MPI_COMM_WORLD,1,iret)
 endif
 call mpi_allreduce(sigdatai%d,sigdatao%d,(ntrunc+1)*(ntrunc+2)*nlevs,mpi_real,mpi_sum,mpi_comm_world,iret)
 if (iret .ne. 0) then
   print *,'non-zero return code from mpi_allreduce'
   call mpi_abort(MPI_COMM_WORLD,1,iret)
 endif
 call mpi_allreduce(sigdatai%t,sigdatao%t,(ntrunc+1)*(ntrunc+2)*nlevs,mpi_real,mpi_sum,mpi_comm_world,iret)
 if (iret .ne. 0) then
   print *,'non-zero return code from mpi_allreduce'
   call mpi_abort(MPI_COMM_WORLD,1,iret)
 endif
 call mpi_allreduce(sigdatai%q,sigdatao%q,(ntrunc+1)*(ntrunc+2)*nlevs*ntrac,mpi_real,mpi_sum,mpi_comm_world,iret)
 if (iret .ne. 0) then
   print *,'non-zero return code from mpi_allreduce'
   call mpi_abort(MPI_COMM_WORLD,1,iret)
 endif
 call mpi_allreduce(sigdatai%ps,sigdatao%ps,(ntrunc+1)*(ntrunc+2),mpi_real,mpi_sum,mpi_comm_world,iret)
 if (iret .ne. 0) then
   print *,'non-zero return code from mpi_allreduce'
   call mpi_abort(MPI_COMM_WORLD,1,iret)
 endif
 sigdatao%z = sigdatao%z/float(nanals)
 sigdatao%d = sigdatao%d/float(nanals)
 sigdatao%t = sigdatao%t/float(nanals)
 sigdatao%q = sigdatao%q/float(nanals)
 sigdatao%hs = sigdatai%hs
 sigdatao%ps = sigdatao%ps/float(nanals)

 ! calculate ps stdev for NH, print out.
 call sptez(0,ntrunc,4,nlons,nlats,sigdatai%ps,psg,1)
 psg = 10.*exp(psg)  
 call mpi_allreduce(psg,psgmean,nlons*nlats,mpi_real,mpi_sum,mpi_comm_world,iret)
 psgmean = psgmean/float(nanals)
 psg = psg - psgmean
 print *,nanal,minval(psg),maxval(psg)
 call mpi_allreduce(psg**2,psgvar,nlons*nlats,mpi_real,mpi_sum,mpi_comm_world,iret)
 psgvar = psgvar/float(nanals-1)

 if (nproc .eq. 0) then

    sumvar = 0.
    sumcoslat = 0.
    do j=1,nlats
    coslat = sqrt(1.-gaulats(j)**2)
    rlat = (180./pi)*asin(gaulats(j))
    do i=1,nlons
       if (rlat .ge. latmin) then
          sumvar = sumvar + psgvar(i,j)*coslat
          sumcoslat = sumcoslat + coslat
       end if
    enddo
    enddo
    print *,'area averaged NH ps standard deviation ',sqrt(sumvar/sumcoslat)

    ! write out.
    sigheado%iens(1) = 1 ! unperturbed control
    sigheado%iens(2) = 2 ! low res control
    sigheado%icen2 = 2 ! sub-center, must be 2 or ens info not used
    call sigio_swohdc(iunit2,filenameout,sigheado,sigdatao,iret)
 end if 

 call sigio_axdata(sigdatai,iret)
 call sigio_sclose(iunit,iret)
 call sigio_axdata(sigdatao,iret)
 call sigio_sclose(iunit2,iret)
 deallocate(psg,psgmean,psgvar,gaulats,gauwts)

 call mpi_barrier(mpi_comm_world,iret)
 if (nproc .eq. 0) write(6,*) 'all done!'

 call mpi_finalize(iret)
 if (nproc .eq. 0 .and. iret .ne. 0) then
  print *, 'mpi_finalize error status = ',iret
 end if

end program getsigensmeanp
