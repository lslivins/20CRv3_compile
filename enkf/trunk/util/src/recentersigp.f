      program recentersigp
! read NCEP GFS spectral sigma file from a file, remove mean specified from another
! file, add a new mean specified from a third file, and write out
! result to a fourth file.
      use specmod
      USE SIGIO_MODULE
      implicit none
      TYPE(SIGIO_HEAD) :: SIGHEADI,SIGHEADO,SIGHEADMI,SIGHEADMO
      TYPE(SIGIO_DATA) :: SIGDATAI,SIGDATAO,SIGDATAMI,SIGDATAMO
      character*500 filename_meani,filename_meano, &
         filenamein,filenameout
      character*3 charnanal
      character(len=4) charnin
      integer nsigi,nsigo,iret,nlats,nlons,ntrunc,nproc,numproc, &
         nanal,nanals,ierr
      real,allocatable,dimension(:,:) :: psg,psgmeano,psgmeani
! mpi definitions.
      include 'mpif.h'

      call MPI_Init(ierr)
      ! nproc is process number, numproc is total number of processes.
      call MPI_Comm_rank(MPI_COMM_WORLD,nproc,ierr)
      call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)

      NSIGI=21
      NSIGO=61
! read data from this file
      call getarg(1,filenamein)
! subtract this mean
      call getarg(2,filename_meani)
! then add to this mean
      call getarg(3,filename_meano)
! and put in this file.
      call getarg(4,filenameout)
! how many ensemble members to process
      call getarg(5,charnin)
      read(charnin,'(i4)') nanals

      if (nproc==0) write(6,*) 'RECENTERSIGP:  PROCESS ',nanals,' ENSEMBLE MEMBERS'
      nanal = nproc+1

      if (nanal.gt.nanals) then
        write (6,*) 'no files to process for mpi task = ',nproc
      else
        write(charnanal,'(i3.3)') nanal
        write (6,*) 'nproc, myfilein = ',nanal


      call sigio_srohdc(nsigi,trim(filename_meani),  &
                       sigheadmi,sigdatami,iret)
      print *,trim(filename_meani)
      print *,iret
      call sigio_srohdc(nsigi,trim(filename_meano),  &
                       sigheadmo,sigdatamo,iret)
      print *,trim(filename_meano)
      print *,iret
      call sigio_srohdc(nsigi,trim(filenamein)//"_mem"//charnanal,  &
                       sigheadi,sigdatai,iret)
      print *,trim(filenamein)
      print *,iret

      sigheado = sigheadmo 
      call sigio_aldata(sigheado,sigdatao,iret)
      ntrunc = sigheado%jcap
      nlats = sigheado%latf
      nlons = sigheado%lonf
      allocate(psg(nlons,nlats))
      allocate(psgmeano(nlons,nlats))
      allocate(psgmeani(nlons,nlats))
      print *,'nlons,nlats,jcap = ',nlons,nlats,ntrunc
      call init_spec_vars(nlons,nlats,ntrunc,4)

! mean surface pressure for output time.
      call sptez_s(sigdatamo%ps,psgmeano,1)
      psgmeano = 10.*exp(psgmeano)

! mean surface pressure for input time.
      call sptez_s(sigdatami%ps,psgmeani,1)
      psgmeani = 10.*exp(psgmeani)

! input surface pressure.
      call sptez_s(sigdatai%ps,psg,1)
      psg = 10.*exp(psg)

      sigdatao%hs = sigdatai%hs
      sigdatao%t = sigdatai%t - sigdatami%t + sigdatamo%t
      sigdatao%z = sigdatai%z - sigdatami%z + sigdatamo%z
      sigdatao%d = sigdatai%d - sigdatami%d + sigdatamo%d
      sigdatao%q = sigdatai%q - sigdatami%q + sigdatamo%q
      print *,minval(psg-psgmeani),maxval(psg-psgmeani)
      psg = psg - psgmeani + psgmeano

      !print *,minval(psg),maxval(psg)
      psg = alog(psg/10.)
      call sptez_s(sigdatao%ps,psg,-1)
      call sigio_swohdc(nsigo,trim(filenameout)//"_mem"//charnanal,sigheado,sigdatao,iret)
      print *,trim(filenameout)
      print *,iret

   end if  ! end if mype

      call MPI_Barrier(MPI_COMM_WORLD,ierr)
      if (nproc .eq. 0) write(6,*) 'all done!'
      call MPI_Finalize(ierr)
      if (nproc .eq. 0 .and. ierr .ne. 0) then
       print *, 'MPI_Finalize error status = ',ierr
      end if

      STOP
      END
