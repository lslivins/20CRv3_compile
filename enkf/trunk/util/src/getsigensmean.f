      program getsigensmean
! create ensemble mean NCEP GFS spectral sigma file.
      use sigio_module
      implicit none
      TYPE(SIGIO_HEAD) :: SIGHEADI,SIGHEADO
      TYPE(SIGIO_DATA) :: SIGDATAI,SIGDATAO
      character*500 filenamesigin,filenamesigout,datapath
      character*80 fileprefix
      character*3 charnanal
      character*5 charfhr
      integer nsigi,nsigo,iret,nanals,nanal,nlats,nlons, &
             jcap,ngrd,n,k,i,j
      real*4 sumvar,sumcoslat,coslat,rlat,pi,latmin,rmin,rmax
      real*4,allocatable,dimension(:,:,:) :: psg
      real*4,allocatable,dimension(:,:) :: psgvar,psgmean
      real*4,allocatable,dimension(:) :: gaulats, gauwts
      NSIGI=21
      NSIGO=61
      pi = 4.*atan(1.0)
      latmin = 20.
      call getarg(1,datapath)
      call getarg(2,filenamesigout)
      call getarg(3,fileprefix)
      call getarg(4,charnanal)
      call getarg(5,charfhr)
      read(charnanal,'(i3)') nanals
      filenamesigout = trim(adjustl(datapath))//filenamesigout
      print *,filenamesigout
      do nanal=1,nanals
         write(charnanal,'(i3.3)') nanal
         filenamesigin = trim(adjustl(datapath))// &
        trim(adjustl(fileprefix))//'_mem'//charnanal
!        print *,filenamesigin
         call sigio_srohdc(nsigi,filenamesigin,sigheadi,sigdatai,iret)
         if (iret .ne. 0) then
           print *,'cannot read data from',trim(filenamesigin)
           stop(1)
         endif
         do k=1,sigheadi%levs
            rmin = minval(sigdatai%t(:,k))
            rmax = maxval(sigdatai%t(:,k))
            if (rmin .eq. rmax .or. rmin < -1000 .or. rmax .gt. 1000) then
               print *,'bad data',trim(filenamesigin)
               stop(1)
            endif
         enddo
         if (nanal .eq. 1) then
          !print *,filenamesigin
          jcap = sigheadi%jcap
          nlats = sigheadi%latf
          nlons = sigheadi%lonf
          ngrd = sigheadi%nxgr
!         print *,'ngrd = ',ngrd
!         print *,'jcap,nlons,nlats,nlevs = ',
!    *    jcap,nlons,nlats,sigheadi%levs
          allocate(psg(nlons,nlats,nanals))
	  allocate(gaulats(nlats))
	  allocate(gauwts(nlats))
          allocate(psgmean(nlons,nlats))
          allocate(psgvar(nlons,nlats))
	  call splat(4,nlats,gaulats,gauwts)
         end if
         call sptez(0,jcap,4,nlons,nlats,sigdatai%ps,psg(:,:,nanal),1)
         psg(:,:,nanal) = 10.*exp(psg(:,:,nanal))
         !print *,nanal,minval(psg(:,:,nanal)),maxval(psg(:,:,nanal))
         if (nanal .eq. 1) then
            call sigio_aldata(sigheadi,sigdatao,iret)
            sigheado = sigheadi
            psgmean = psg(:,:,nanal)/float(nanals)
            sigdatao%hs = sigdatai%hs
            sigdatao%ps=sigdatai%ps/float(nanals)
            sigdatao%t=sigdatai%t/float(nanals)
            sigdatao%z=sigdatai%z/float(nanals)
            sigdatao%d=sigdatai%d/float(nanals)
            sigdatao%q=sigdatai%q/float(nanals)
            if (ngrd .gt. 0) &
           sigdatao%xgr=sigdatai%xgr/float(nanals)
         else
            psgmean = psgmean + psg(:,:,nanal)/float(nanals)
            if (ngrd .gt. 0) &
            sigdatao%xgr=sigdatao%xgr+sigdatai%xgr/float(nanals)
            sigdatao%ps=sigdatao%ps+sigdatai%ps/float(nanals)
            sigdatao%t=sigdatao%t+sigdatai%t/float(nanals)
            sigdatao%z=sigdatao%z+sigdatai%z/float(nanals)
            sigdatao%d=sigdatao%d+sigdatai%d/float(nanals)
            sigdatao%q=sigdatao%q+sigdatai%q/float(nanals)
         end if
         call sigio_axdata(sigdatai,iret)
      enddo
!     print *,minval(psgmean),maxval(psgmean)
      psgvar = 0.
      do nanal=1,nanals
         psgvar = psgvar + (psg(:,:,nanal)-psgmean)**2/float(nanals-1)
      enddo
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
      !print *,trim(filenamesigout)
      print *,charfhr,' area averaged NH ps standard deviation ',sqrt(sumvar/sumcoslat)
      psgmean = alog(psgmean/10.)
!! keep spectral values
!!! !     call sptez(0,jcap,4,nlons,nlats,sigdatao%ps,psgmean,-1)
      do n=1,ngrd
         print *,k,n,minval(sigdatao%xgr(:,:,n)),maxval(sigdatao%xgr(:,:,n))
      enddo

      sigheado%iens(1) = 1 ! unperturbed control
      sigheado%iens(2) = 2 ! unpert low res cntl
      sigheado%icen2 = 2 ! sub-center, must be 2 or ens info not used
      call sigio_swohdc(nsigo,filenamesigout,sigheado,sigdatao,iret)

      stop(0)
      
      END PROGRAM getsigensmean
