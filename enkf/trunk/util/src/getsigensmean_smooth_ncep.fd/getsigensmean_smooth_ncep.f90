program getsigensmean_smooth
!$$$  main program documentation block
!
! program:  getsigensmean              compute ensemble mean
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  create ensemble mean NCEP GFS spectral sigma file.
!
! program history log:
!   2009-02-23  Initial version.
!
! usage:
!   input files:
!
!   output files:
!
! attributes:
!   language: f95
!
!$$$
  
  USE SIGIO_MODULE
  implicit none
  logical lexist,dosmooth
  character(len=3) charnanal
  character(len=500) filenamein,filenameout,filenameouts,datapath,fileprefix,fname
  integer iret,nlevs,ntrac,ntrunc,nanals,ngrd,k,nanal,iunit,window
  integer, dimension(:), allocatable :: smoothparm
  real(8) rnanals
  real(8),dimension(:,:,:), allocatable :: smoothfact
  TYPE(SIGIO_HEAD) :: sigheadi,sigheadm
  TYPE(SIGIO_DATA) :: sigdatai,sigdatam,sigdatapert

  call w3tagb('GETSIGENSMEAN_SMOOTH',2011,0319,0055,'NP25')

! Get user input from command line
  call getarg(1,datapath)
  call getarg(2,filenameout)
  call getarg(3,fileprefix)
  call getarg(4,charnanal)
  read(charnanal,'(i3)') nanals
  rnanals=nanals
  rnanals=1.0_8/rnanals

  filenameout = trim(adjustl(datapath))//filenameout
  
  write(6,*)'datapath      = ',trim(datapath)
  write(6,*)'filenameout   = ',trim(filenameout)
  write(6,*)'fileprefix    = ',trim(fileprefix)
  write(6,*)'nanals,rnanals= ',nanals,rnanals
  
  iunit = 21
  window = 1 ! cosine bell window for smoothing

! Loop over analysis files
  do nanal=1,nanals

     write(charnanal,'(i3.3)') nanal
     filenamein = trim(adjustl(datapath))// &
          trim(adjustl(fileprefix))//'_mem'//charnanal
     
!    Read each ensemble member FHDFI forecast.
     call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
     
!    Operations done only on first loop     
     if (nanal==1) then
        call sigio_aldata(sigheadi,sigdatam,iret)
        sigheadm    = sigheadi
        sigdatam%hs = sigdatai%hs
        sigdatam%ps = sigdatai%ps*rnanals
        sigdatam%z  = sigdatai%z*rnanals
        sigdatam%d  = sigdatai%d*rnanals
        sigdatam%t  = sigdatai%t*rnanals
        sigdatam%q  = sigdatai%q*rnanals

        ngrd = sigheadi%nxgr
        if (ngrd>0) sigdatam%xgr = sigdatai%xgr*rnanals
     else
        sigdatam%ps = sigdatam%ps + rnanals*sigdatai%ps
        sigdatam%z  = sigdatam%z  + rnanals*sigdatai%z
        sigdatam%d  = sigdatam%d  + rnanals*sigdatai%d
        sigdatam%t  = sigdatam%t  + rnanals*sigdatai%t
        sigdatam%q  = sigdatam%q  + rnanals*sigdatai%q
        if (ngrd>0) sigdatam%xgr = sigdatam%xgr + sigdatai%xgr
     endif
     call sigio_axdata(sigdatai,iret)
  end do

! Save truncation and number of levels
  ntrunc= sigheadm%jcap
  nlevs = sigheadm%levs
  ntrac = sigheadm%ntrac

! Write out ensemble mean
  sigheadm%iens(1) = 1 ! unperturbed control
  sigheadm%iens(2) = 2 ! low res control
  sigheadm%icen2 = 2 ! sub-center, must be 2 or ens info not used
  call sigio_swohdc(iunit,filenameout,sigheadm,sigdatam,iret)
  call sigio_sclose(iunit,iret)

! Read smoothing parameters, if available
  fname='hybens_smoothinfo'
  inquire(file=trim(fname),exist=lexist)
  if ( lexist ) then
     allocate(smoothparm(nlevs))
     smoothparm = -1
     open(9,form='formatted',file=fname)
     do k=1,nlevs
        read(9,'(i3)') smoothparm(k)
     enddo
     close(9)
     dosmooth = maxval(smoothparm)>0
  else
     write(6,*)'***WARNING***  hybens_smoothinfo not found - no smoothing'
     dosmooth = .false.
  endif
  write(6,*)'dosmooth=',dosmooth

! If smoothing requested, loop over and smooth analysis files
  if (dosmooth) then

!    Set up smoother
     allocate(smoothfact(0:ntrunc,0:ntrunc,nlevs))
     smoothfact=1.0_8
     call setup_smooth(ntrunc,nlevs,smoothparm,window,smoothfact)

!    Loop over files and smooth each
     do nanal=1,nanals

        write(charnanal,'(i3.3)') nanal
        filenamein = trim(adjustl(datapath))// &
             trim(adjustl(fileprefix))//'_mem'//charnanal
        
        filenameouts= trim(adjustl(datapath))// &
             trim(adjustl(fileprefix)) // 's' //'_mem'//charnanal

!       Read each ensemble member FHDFI forecast.
        call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
!!      write(6,*)'read ',trim(filenamein),' iret=',iret

        if (maxval(smoothparm) > 0) then
           call sigio_aldata(sigheadi,sigdatapert,iret)
           
           sigdatapert%z  = sigdatai%z  - sigdatam%z
           call smooth(sigdatapert%z,ntrunc,nlevs,smoothparm,smoothfact)
           sigdatai%z = sigdatam%z + sigdatapert%z
           
           sigdatapert%d  = sigdatai%d  - sigdatam%d
           call smooth(sigdatapert%d,ntrunc,nlevs,smoothparm,smoothfact)
           sigdatai%d = sigdatam%d + sigdatapert%d
           
           sigdatapert%t  = sigdatai%t  - sigdatam%t
           call smooth(sigdatapert%t,ntrunc,nlevs,smoothparm,smoothfact)
           sigdatai%t = sigdatam%t + sigdatapert%t
           
           do k=1,ntrac
              sigdatapert%q(:,:,k) = sigdatai%q(:,:,k)  - sigdatam%q(:,:,k)
              call smooth(sigdatapert%q(:,:,k),ntrunc,nlevs,smoothparm,smoothfact)
              sigdatai%q(:,:,k) = sigdatam%q(:,:,k) + sigdatapert%q(:,:,k)
           end do
           
           sigdatapert%ps = sigdatai%ps - sigdatam%ps
           call smooth(sigdatapert%ps,ntrunc,1,smoothparm,smoothfact)
           sigdatai%ps = sigdatam%ps + sigdatapert%ps
           
           call sigio_axdata(sigdatapert,iret)
        endif
        
!       Write out
        call sigio_swohdc(iunit,trim(filenameouts),sigheadi,sigdatai,iret)
        call sigio_axdata(sigdatai,iret)

!    End of loop over files
     end do

!    Deallocate smoothing factors
     deallocate(smoothfact)

! End of smoothing block
  endif

! Deallocate smoothing parameter array
  if (lexist) deallocate(smoothparm)

  write(6,*) 'all done!'
  call w3tage('GETSIGENSMEAN_SMOOTH')

end program getsigensmean_smooth

subroutine smooth(specdat,ntrunc,nlevs,smoothparm,smoothfact)
  implicit none
  integer :: m,nm,n,k
  integer, intent(in) :: ntrunc,nlevs
  integer,intent(in)  :: smoothparm(nlevs) ! smoothing parameter
  real(8),intent(in)  ::  smoothfact(0:ntrunc,0:ntrunc,nlevs) ! smoothing factor
  real, intent(inout) :: specdat((ntrunc+1)*(ntrunc+2),nlevs)
  k_loop: do k=1,nlevs
     if (smoothparm(k) .le. 0.) cycle k_loop
     nm = 1
     m_loop: do m=0,ntrunc
        n_loop: do n=m,ntrunc
           specdat(nm,k)   = smoothfact(n,m,k)*specdat(nm,k)
           specdat(nm+1,k) = smoothfact(n,m,k)*specdat(nm+1,k)
           nm = nm + 2
        enddo n_loop
     enddo m_loop
  enddo k_loop
end subroutine smooth

subroutine setup_smooth(ntrunc,nlevs,smoothparm,window,smoothfact)
  implicit none
  integer :: m,n,k
  integer, intent(in) :: ntrunc,nlevs,window
  integer,dimension(nlevs),intent(in) ::  smoothparm ! smoothing parameter.
  real(8),dimension(0:ntrunc,0:ntrunc,nlevs),intent(out) :: smoothfact
  real(8) zero,half,one,pi,smoothval,rsmoothval,waven

  zero = 0.0_8
  half = 0.5_8
  one  = 1.0_8
  pi   = 4.0_8*atan(one)

  k_loop: do k=1,nlevs
     if (smoothparm(k) .le. 0.) cycle k_loop
     smoothval=smoothparm(k)
     rsmoothval=one/smoothval
     m_loop: do m=0,ntrunc
        n_loop: do n=m,ntrunc
           waven=real(n)
           if (window .eq. 1) then
              ! Hann window (cosine bell).
              if (n <= smoothparm(k)) then
                 smoothfact(n,m,k) = half*(one + cos(pi*waven*rsmoothval))
              else
                 smoothfact(n,m,k) = zero
              endif
           else if (window .eq. 2) then
              ! gaussian window.
              smoothfact(n,m,k) = exp(-(waven*rsmoothval)**2)
           else
              ! rectangular window (simple truncation)
              if (n <= smoothparm(k)) then
                 smoothfact(n,m,k) = one
              else
                 smoothfact(n,m,k) = zero
              endif
           endif
        enddo n_loop
     enddo m_loop
  enddo k_loop
end subroutine setup_smooth

