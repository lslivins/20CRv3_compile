program getsigensmean_smooth

  USE SIGIO_MODULE
  implicit none
  TYPE(SIGIO_HEAD) :: sigheadi,sigheadm
  TYPE(SIGIO_DATA) :: sigdatai,sigdatam,sigdatapert
  character(len=500) filenamein,filenameout,filenameouts,datapath,fileprefix,fname
  integer iret,nlevs,ntrac,ntrunc,nanals,ngrd,&
       k,nanal,iunit
  character(len=3) charnanal
  real latmin,pi,rnanals
  integer, dimension(:), allocatable :: smoothparm
  logical lexist,dosmooth
  integer window
  real(8):: t0,t1,t2,rtc

  t0=rtc()

! Get user input from command line
  call getarg(1,datapath)
  call getarg(2,filenameout)
  call getarg(3,fileprefix)
  call getarg(4,charnanal)
  read(charnanal,'(i3)') nanals
  rnanals = 1.0/float(nanals)

  filenameout = trim(adjustl(datapath))//filenameout
  print *,filenameout
  latmin = 20.
  pi = 4.*atan(1.0)
  window = 1 ! cosine bell window for smoothing
  iunit = 21

  t1=rtc()
! Loop over analysis files
  do nanal=1,nanals

     write(charnanal,'(i3.3)') nanal
     filenamein = trim(adjustl(datapath))// &
          trim(adjustl(fileprefix))//'_mem'//charnanal
     
!    Read each ensemble member FHDFI forecast.
     call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
!!   write(6,*)'read ',trim(filenamein),' iret=',iret
     
!    Operations done only on first loop     
     if (nanal==1) then
        call sigio_aldata(sigheadi,sigdatam,iret)
        sigdatam%hs = sigdatai%hs
        sigheadm = sigheadi
!!      write(6,*)'allocate sigdatam with iret=',iret

        ngrd = sigheadi%nxgr

        sigdatam%z  = 0.0
        sigdatam%d  = 0.0
        sigdatam%t  = 0.0
        sigdatam%q  = 0.0
        sigdatam%ps = 0.0
        if (ngrd==0) sigdatam%xgr = 0.0
     endif

!    Accumulate mean
     sigdatam%z  = sigdatam%z  + rnanals*sigdatai%z
     sigdatam%d  = sigdatam%d  + rnanals*sigdatai%d
     sigdatam%t  = sigdatam%t  + rnanals*sigdatai%t
     sigdatam%q  = sigdatam%q  + rnanals*sigdatai%q
     sigdatam%ps = sigdatam%ps + rnanals*sigdatai%ps
     if (ngrd==0) sigdatam%xgr = sigdatam%xgr + sigdatai%xgr

     call sigio_axdata(sigdatai,iret)

  end do

! Save number of levels
  nlevs = sigheadm%levs

! Write out ensemble mean
  sigheadm%iens(1) = 1 ! unperturbed control
  sigheadm%iens(2) = 2 ! low res control
  sigheadm%icen2 = 2 ! sub-center, must be 2 or ens info not used
  call sigio_swohdc(iunit,filenameout,sigheadm,sigdatam,iret)
  call sigio_sclose(iunit,iret)

  t2=rtc()
  write(6,*)'time for ensmean is ',t2-t1
  t1=rtc()

! Read smoothing parameters, if available
  allocate(smoothparm(nlevs))
  smoothparm = -1
  fname='hybens_smoothinfo'
  inquire(file=trim(fname),exist=lexist)
  if ( lexist ) then
     open(9,form='formatted',file=fname)
!!     print *,'smoothing parameter by level:'
     do k=1,nlevs
        read(9,'(i3)') smoothparm(k)
!!        print *,k,smoothparm(k)
     enddo
     close(9)
!!  else
!!     print *,'hybens_smoothinfo not found - no smoothing'
  endif
  dosmooth = maxval(smoothparm)>0
  write(6,*)'dosmooth=',dosmooth

! If smoothing requested, loop over and smooth analysis files
  if (dosmooth) then
     do nanal=1,nanals

        write(charnanal,'(i3.3)') nanal
        filenamein = trim(adjustl(datapath))// &
             trim(adjustl(fileprefix))//'_mem'//charnanal
        
        filenameouts= trim(adjustl(datapath))// &
             trim(adjustl(fileprefix)) // 's' //'_mem'//charnanal

!       Read each ensemble member FHDFI forecast.
        call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
!!        write(6,*)'read ',trim(filenamein),' iret=',iret

        if (maxval(smoothparm) > 0) then
           ntrunc = sigheadi%jcap
           ntrac = sigheadi%ntrac
           nlevs = sigheadi%levs

           call sigio_aldata(sigheadi,sigdatapert,iret)
           
           sigdatapert%z  = sigdatai%z  - sigdatam%z
           call smooth(sigdatapert%z,ntrunc,nlevs,smoothparm,window)
           sigdatai%z = sigdatam%z + sigdatapert%z
           
           sigdatapert%d  = sigdatai%d  - sigdatam%d
           call smooth(sigdatapert%d,ntrunc,nlevs,smoothparm,window)
           sigdatai%d = sigdatam%d + sigdatapert%d
           
           sigdatapert%t  = sigdatai%t  - sigdatam%t
           call smooth(sigdatapert%t,ntrunc,nlevs,smoothparm,window)
           sigdatai%t = sigdatam%t + sigdatapert%t
           
           do k=1,ntrac
              sigdatapert%q(:,:,k) = sigdatai%q(:,:,k)  - sigdatam%q(:,:,k)
              call smooth(sigdatapert%q(:,:,k),ntrunc,nlevs,smoothparm,window)
              sigdatai%q(:,:,k) = sigdatam%q(:,:,k) + sigdatapert%q(:,:,k)
           end do
           
           sigdatapert%ps = sigdatai%ps - sigdatam%ps
           call smooth(sigdatapert%ps,ntrunc,1,smoothparm(1),window)
           sigdatai%ps = sigdatam%ps + sigdatapert%ps
           
           call sigio_axdata(sigdatapert,iret)
        endif
        
!       Write out
        call sigio_swohdc(iunit,trim(filenameouts),sigheadi,sigdatai,iret)
        call sigio_axdata(sigdatai,iret)

!    End of loop over files
     end do

! End of smoothing block
  endif

  t2=rtc()
  write(6,*)'time for smooth is ',t2-t1
  write(6,*)'total time is ',t2-t0

  write(6,*) 'all done!'

end program getsigensmean_smooth

subroutine smooth(specdat,ntrunc,nlevs,smoothparm,window)
 implicit none
 integer :: m,nm,n,k
 integer, intent(in) :: ntrunc,nlevs,window
 real, intent(inout) :: specdat((ntrunc+1)*(ntrunc+2),nlevs)
 integer, intent(in) ::  smoothparm(nlevs) ! smoothing parameter.
 real pi, smoothfact
 pi = 4.*atan(1.0)
 do k=1,nlevs
    if (smoothparm(k) .gt. 0.) then
    nm = 1
    do m=0,ntrunc
       do n=m,ntrunc
          if (window .eq. 1) then
             ! Hann window (cosine bell).
             if (n <= smoothparm(k)) then
                smoothfact = 0.5*(1.0 + cos(pi*real(n)/smoothparm(k)))
             else
                smoothfact = 0.
             endif
          else if (window .eq. 2) then
             ! gaussian window.
             smoothfact = exp(-(real(n)/real(smoothparm(k)))**2)
          else
             ! rectangular window (simple truncation)
             if (n <= smoothparm(k)) then
                smoothfact = 1.
             else
                smoothfact = 0.
             endif
          endif
          specdat(nm,k) = smoothfact*specdat(nm,k)
          specdat(nm+1,k) = smoothfact*specdat(nm+1,k)
          nm = nm + 2
       enddo
    enddo
    endif
 enddo
end subroutine smooth
