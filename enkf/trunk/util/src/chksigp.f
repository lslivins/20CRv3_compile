      program chksig
      USE SIGIO_MODULE
      implicit none
      TYPE(SIGIO_HEAD) :: SIGHEAD
      TYPE(SIGIO_DATA) :: SIGDATA
      character*500 filenamein, datapath, fileprefix
      character*3 charnanal
      integer nsigi,iret,k,nproc,numproc
      real rmin, rmax
      ! mpi definitions.
      include 'mpif.h'
      call MPI_Init(iret)
      ! nproc is process number, numproc is total number of processes.
      call MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
      call MPI_Comm_size(MPI_COMM_WORLD,numproc,iret)
      call getarg(1,datapath)
      call getarg(2,fileprefix)
      write(charnanal,'(i3.3)') nproc+1
      filenamein = trim(adjustl(datapath))// 
     * trim(adjustl(fileprefix))//'_mem'//charnanal
      ! read all data from file
      print *,trim(filenamein)
      NSIGI=21
      call sigio_srohdc(nsigi,trim(filenamein),sighead,sigdata,iret)
      if (iret .ne. 0) then
        print *,
     *  'non-zero return code from sigio_srohdc for nanal=',nproc+1
        call mpi_abort(MPI_COMM_WORLD,1,iret)
      endif
      ! check temp data.
      do k=1,sighead%levs
         rmin = minval(sigdata%t(:,k))
         rmax = maxval(sigdata%t(:,k))
         if (rmin .eq. rmax .or. rmin < -1000 .or.
     *       rmax .gt. 1000) then
            print *,'bad data for nanal=',nproc+1
            call mpi_abort(MPI_COMM_WORLD,1,iret)
         endif
      enddo
      print *,'all OK'
      call sigio_axdata(sigdata,iret)
      call sigio_sclose(nsigi,iret)
      call mpi_barrier(mpi_comm_world,iret)
      call mpi_finalize(iret)
      !call mpi_abort(MPI_COMM_WORLD,0,iret)
      stop(0)
      END
