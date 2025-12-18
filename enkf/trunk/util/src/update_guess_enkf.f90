subroutine update_guess(sval,sbias)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_guess          add analysis increment to guess
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract:  This routine adds the analysis increment from the inner 
!            loop to the guess.  For certain variables, a change in
!            in units or represenation is made.
!
!            For ozone, a change of units is made from the units used
!            in the minimization to those used in the guess.  Stream 
!            function and velocity potential are converted into 
!            vorticity and divergence, the guess variables.
!
! program history log:
!   2012-02-06  whitaker - hack to perform incremental balance on enkf
!                          increments.
!
!   input argument list:
!    sval
!    sbias
!
!   output argument list:
!    sval
!    sbias
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use mpimod, only: mype
  use constants, only: zero,one,fv,max_varname_length,qmin
  use jfunc, only: iout_iter,biascor,tsensible
  use gridmod, only: lat2,lon2,nsig,&
       regional,twodvar_regional,regional_ozone
  use guess_grids, only: ges_div,ges_vor,ges_ps,ges_tv,ges_q,&
       ges_tsen,ges_oz,ges_u,ges_v,ges_gust,ges_vis,ges_pblh,&
       nfldsig,hrdifsig,hrdifsfc,nfldsfc,dsfct
  use xhat_vordivmod, only: xhat_vor,xhat_div,xhat_vordiv_calc2
  use gsi_4dvar, only: nobs_bins, hr_obsbin
  use gsi_bundlemod, only: gsi_bundle, gsi_bundlegetpointer
  use bias_predictors, only: predictors
  use balmod, only: strong_bk

  implicit none

! Declare passed variables
  type(gsi_bundle), intent(inout) :: sval(nobs_bins)
  type(predictors), intent(inout) :: sbias

! Declare local variables
  character(80) filename
  real(r_single),dimension(lat2,lon2):: scr2
  real(r_single),dimension(lat2,lon2,nsig):: scr3
  integer(i_kind) i,j,k,it,ij,ii,ic,id,nguess,istatus
  integer(i_kind) is_u,is_v,is_t,is_q,is_oz,is_cw,is_ps,is_sst
  integer(i_kind) is_gust,is_vis,is_pblh
  integer(i_kind) ipinc,ipges,iret
  integer(i_kind) :: luninc=11
  real(r_kind) :: zt
  real(r_kind),allocatable,dimension(:,:  ) :: p_ps
  real(r_kind),allocatable,dimension(:,:,:) :: p_u     
  real(r_kind),allocatable,dimension(:,:,:) :: p_v     
  real(r_kind),allocatable,dimension(:,:,:) :: p_vor    
  real(r_kind),allocatable,dimension(:,:,:) :: p_div   
  real(r_kind),allocatable,dimension(:,:,:) :: p_q     
  real(r_kind),allocatable,dimension(:,:,:) :: p_tv   
  real(r_kind),allocatable,dimension(:,:,:) :: p_oz    

!*******************************************************************************
! In 3dvar, nobs_bins=1 is smaller than nfldsig. This subroutine is
! written in a way that is more efficient in that case but might not
! be the best in 4dvar.

! Get required pointers and abort if not found (RTod: needs revision)
  call gsi_bundlegetpointer(sval(1),'u',  is_u,  istatus)
  call gsi_bundlegetpointer(sval(1),'v',  is_v,  istatus)
  call gsi_bundlegetpointer(sval(1),'tv', is_t,  istatus)
  call gsi_bundlegetpointer(sval(1),'q',  is_q,  istatus)
  call gsi_bundlegetpointer(sval(1),'oz', is_oz, istatus)
  call gsi_bundlegetpointer(sval(1),'ps', is_ps, istatus)

! Add increment to background
  do it=1,nfldsig
     if (nobs_bins>1) then
        zt = hrdifsig(it)
        ii = NINT(zt/hr_obsbin)+1
     else
        ii = 1
     endif

     ! read pre-processed enkf increment (one file for each bin that has the
     ! increment for a subdomain)
     write(filename,103) mype
 103 format('enkfinc.pe',i4.4)
     if (mype==0) write(6,*) 'READ PRE-PROCESSED ENKF INCREMENT FILE: ',trim(filename)
     if (mype==0) write(6,*) 'lat2,lon2,nsig',lat2,lon2,nsig
     open(luninc,file=filename,form='unformatted',iostat=iret)
     if (iret /= 0) go to 104
     allocate(p_ps(lat2,lon2))
     allocate(p_u(lat2,lon2,nsig),p_v(lat2,lon2,nsig),p_tv(lat2,lon2,nsig))
     allocate(p_q(lat2,lon2,nsig),p_oz(lat2,lon2,nsig))
     allocate(p_vor(lat2,lon2,nsig),p_div(lat2,lon2,nsig))
     read(luninc,err=104) scr2; p_ps = scr2
     read(luninc,err=104) scr3; p_u = scr3
     read(luninc,err=104) scr3; p_v = scr3
     read(luninc,err=104) scr3; p_tv = scr3
     read(luninc,err=104) scr3; p_q = scr3
     read(luninc,err=104) scr3; p_oz = scr3
     close(luninc)
     go to 105
 104 continue
     write(6,*) 'error reading enkf increment, aborting ...'
     call stop2(999)
 105 continue

     if (mype == 0) then
         print *,'before strong_bk'
         print *,'min/max ps increment on root:',minval(10.*p_ps),maxval(10.*p_ps)
         print *,'min/max u increment on root:',minval(p_u),maxval(p_u)
         print *,'min/max v increment on root:',minval(p_v),maxval(p_v)
         print *,'min/max tv increment on root:',minval(p_tv),maxval(p_tv)
         print *,'min/max q increment on root:',minval(p_q),maxval(p_q)
         print *,'min/max oz increment on root:',minval(p_oz),maxval(p_oz)
      endif

     ! balance increments.
     call strong_bk(p_u,p_v,p_ps,p_tv,.true.)

     ! convert increments of u,v to vort,div
     call xhat_vordiv_calc2(p_u,p_v,p_vor,p_div)

     if (mype == 0) then
         print *,'after strong_bk'
         print *,'min/max ps increment on root:',minval(10.*p_ps),maxval(10.*p_ps)
         print *,'min/max u increment on root:',minval(p_u),maxval(p_u)
         print *,'min/max v increment on root:',minval(p_v),maxval(p_v)
         print *,'min/max vor increment on root:',minval(p_vor),maxval(p_vor)
         print *,'min/max div increment on root:',minval(p_div),maxval(p_div)
         print *,'min/max tv increment on root:',minval(p_tv),maxval(p_tv)
      endif

     ! add increments to guess.
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              if(is_u>0) ges_u(i,j,k,it) =     ges_u(i,j,k,it)    + p_u(i,j,k)
              if(is_v>0) ges_v(i,j,k,it) =     ges_v(i,j,k,it)    + p_v(i,j,k)
              if(is_q>0) ges_q(i,j,k,it) = max(ges_q(i,j,k,it)    + p_q(i,j,k),qmin) 
              if(is_t > 0) then
                 if (.not.twodvar_regional .or. .not.tsensible) then
                    ges_tv(i,j,k,it)   = ges_tv(i,j,k,it)   + p_tv(i,j,k)
!  produce sensible temperature
                    ges_tsen(i,j,k,it) = ges_tv(i,j,k,it)/(one+fv*ges_q(i,j,k,it))
                 else
                    ges_tsen(i,j,k,it) = ges_tsen(i,j,k,it) + p_tv(i,j,k)
!  produce virtual temperature
                    ges_tv(i,j,k,it)   = ges_tsen(i,j,k,it)*(one+fv*ges_q(i,j,k,it))
                 endif
              endif
!             Note:  Below variables only used in NCEP GFS model
              if(is_oz>0) ges_oz(i,j,k,it)  = ges_oz(i,j,k,it)   + p_oz(i,j,k)
                          ges_div(i,j,k,it) = ges_div(i,j,k,it)  + p_div(i,j,k)
                          ges_vor(i,j,k,it) = ges_vor(i,j,k,it)  + p_vor(i,j,k)
           end do
        end do
     end do
     if(is_ps>0) then
        do j=1,lon2
           do i=1,lat2
              ges_ps(i,j,it) = ges_ps(i,j,it) + p_ps(i,j)
           end do
        end do
     endif

  end do ! loop over it=1,nfldsig

  deallocate(p_ps,p_u,p_v,p_vor,p_div,p_tv,p_q,p_oz)

  return
end subroutine update_guess
