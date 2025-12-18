      program chksig
      USE SFCIO_MODULE
      implicit none
      TYPE(SFCIO_HEAD) :: SFCHEAD
      TYPE(SFCIO_DATA) :: SFCDATA
      character*120 filenamein
      integer nsfci,iret, k,nlons,nlats
      NSFCI=21
      call getarg(1,filenamein)
      call sfcio_srohdc(nsfci,filenamein,sfchead,sfcdata,iret)
      if (iret .ne. 0) then
        stop(1)
      endif
      nlons = sfchead%lonb
      nlats = sfchead%latb
      call chkdata(sfcdata%tsea,nlons,nlats)
      do k=1,sfchead%lsoil
        call chkdata(sfcdata%smc(:,:,k),nlons,nlats)
        call chkdata(sfcdata%stc(:,:,k),nlons,nlats)
        call chkdata(sfcdata%slc(:,:,k),nlons,nlats)
      enddo
      call chkdata(sfcdata%sheleg,nlons,nlats)
      call chkdata(sfcdata%tg3,nlons,nlats)
      call chkdata(sfcdata%zorl,nlons,nlats)
      !call chkdata(sfcdata%cv,nlons,nlats)
      !call chkdata(sfcdata%cvb,nlons,nlats)
      !call chkdata(sfcdata%cvt,nlons,nlats)
      call chkdata(sfcdata%alvsf,nlons,nlats)
      call chkdata(sfcdata%alvwf,nlons,nlats)
      call chkdata(sfcdata%alnsf,nlons,nlats)
      call chkdata(sfcdata%alnwf,nlons,nlats)
      call chkdata(sfcdata%slmsk,nlons,nlats)
      call chkdata(sfcdata%vfrac,nlons,nlats)
      call chkdata(sfcdata%canopy,nlons,nlats)
      call chkdata(sfcdata%f10m,nlons,nlats)
      call chkdata(sfcdata%t2m,nlons,nlats)
      call chkdata(sfcdata%q2m,nlons,nlats)
      call chkdata(sfcdata%vtype,nlons,nlats)
      call chkdata(sfcdata%stype,nlons,nlats)
      call chkdata(sfcdata%facsf,nlons,nlats)
      call chkdata(sfcdata%facwf,nlons,nlats)
      call chkdata(sfcdata%uustar,nlons,nlats)
      call chkdata(sfcdata%ffmm,nlons,nlats)
      call chkdata(sfcdata%ffhh,nlons,nlats)
      call chkdata(sfcdata%hice,nlons,nlats)
      call chkdata(sfcdata%fice,nlons,nlats)
      call chkdata(sfcdata%tisfc,nlons,nlats)
      call chkdata(sfcdata%tprcp,nlons,nlats)
      call chkdata(sfcdata%srflag,nlons,nlats)
      call chkdata(sfcdata%snwdph,nlons,nlats)
      call chkdata(sfcdata%shdmin,nlons,nlats)
      call chkdata(sfcdata%shdmax,nlons,nlats)
      call chkdata(sfcdata%slope,nlons,nlats)
      call chkdata(sfcdata%snoalb,nlons,nlats)
      call chkdata(sfcdata%orog,nlons,nlats)
      STOP(0)
      END
      subroutine chkdata(data,nlons,nlats)
      integer nlons,nlats
      real data(nlons,nlats),rmin,rmax
      rmin = minval(data)
      rmax = maxval(data)
      if (rmin .eq. rmax .or. rmin < -1.e10.or.
     *    rmax .gt. 1.e10 ) then
           stop(1)
      endif
      return
      end
