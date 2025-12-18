      program chksig
      USE SIGIO_MODULE
      implicit none
      TYPE(SIGIO_HEAD) :: SIGHEAD
      TYPE(SIGIO_DATA) :: SIGDATA
      character*120 filenamein
      integer nsigi,iret,k
      real rmin, rmax
      NSIGI=21
      call getarg(1,filenamein)
      call sigio_srohdc(nsigi,trim(filenamein),sighead,sigdata,iret)
      if (iret .ne. 0) then
        stop(1)
      endif
      do k=1,sighead%levs
         rmin = minval(sigdata%t(:,k))
         rmax = maxval(sigdata%t(:,k))
         if (rmin .eq. rmax .or. rmin < -1000 .or.
     *       rmax .gt. 1000) then
            stop(1)
         endif
      enddo
      STOP(0)
      END
