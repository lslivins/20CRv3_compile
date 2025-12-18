      program chksfc
      USE SFCIO_MODULE
      implicit none
      TYPE(SFCIO_HEAD) :: SFCHEAD
      TYPE(SFCIO_DATA) :: SFCDATA
      character*500 filenamein,fileprefix,datapath
      character*3 charnanal
      integer nsfci,iret,k,nproc,numproc,nlons,nlats
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
      call sfcio_srohdc(nsfci,filenamein,sfchead,sfcdata,iret)
      if (iret .ne. 0) then
        call mpi_abort(MPI_COMM_WORLD,1,iret)
      endif
      nlons = sfchead%lonb
      nlats = sfchead%latb
      call chkdata(sfcdata%tsea,nlons,nlats,MPI_COMM_WORLD)
      do k=1,sfchead%lsoil
        call chkdata(sfcdata%smc(:,:,k),nlons,nlats,MPI_COMM_WORLD)
        call chkdata(sfcdata%stc(:,:,k),nlons,nlats,MPI_COMM_WORLD)
        call chkdata(sfcdata%slc(:,:,k),nlons,nlats,MPI_COMM_WORLD)
      enddo
      call chkdata(sfcdata%sheleg,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%tg3,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%zorl,nlons,nlats,MPI_COMM_WORLD)
      !call chkdata(sfcdata%cv,nlons,nlats,MPI_COMM_WORLD)
      !call chkdata(sfcdata%cvb,nlons,nlats,MPI_COMM_WORLD)
      !call chkdata(sfcdata%cvt,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%alvsf,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%alvwf,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%alnsf,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%alnwf,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%slmsk,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%vfrac,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%canopy,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%f10m,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%t2m,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%q2m,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%vtype,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%stype,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%facsf,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%facwf,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%uustar,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%ffmm,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%ffhh,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%hice,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%fice,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%tisfc,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%tprcp,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%srflag,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%snwdph,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%shdmin,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%shdmax,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%slope,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%snoalb,nlons,nlats,MPI_COMM_WORLD)
      call chkdata(sfcdata%orog,nlons,nlats,MPI_COMM_WORLD)
      call mpi_barrier(mpi_comm_world,iret)
      call mpi_finalize(iret)
      !call mpi_abort(MPI_COMM_WORLD,0,iret)
      stop(0)
      END
      subroutine chkdata(data,nlons,nlats,mpi_comm_world)
      integer nlons,nlats,iret,mpi_comm_world
      real data(nlons,nlats),rmin,rmax
      rmin = minval(data)
      rmax = maxval(data)
      if (rmin .eq. rmax .or. rmin < -1.e10.or.
     *    rmax .gt. 1.e10 ) then
        call mpi_abort(MPI_COMM_WORLD,1,iret)
      endif
      return
      end
