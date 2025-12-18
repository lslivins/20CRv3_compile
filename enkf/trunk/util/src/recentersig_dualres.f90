program recentersig_dualres

! read high res NCEP GFS spectral sigma file from a file, add difference
! between two low res spectral files, write out another high res
! spectral file.

 USE SIGIO_MODULE
 implicit none
 TYPE(SIGIO_HEAD) :: SIGHEADHRFG,SIGHEADHRANL,SIGHEADLRFG,SIGHEADLRANL
 TYPE(SIGIO_DATA) :: SIGDATAHRFG,SIGDATAHRANL,SIGDATALRFG,SIGDATALRANL
 character*120 filenamelrfg,filenamelranl,filenamehrfg,filenamehranl
 integer nsigi,nsigo,iret,k,nt
 real,allocatable,dimension(:) :: specdat
 real,allocatable,dimension(:,:) :: psg,psgmeanfg,psgmeananl,psginc
 !real,allocatable,dimension(:,:,:) :: fg,anl

 NSIGI=21
 NSIGO=61

! recentersig_dualres.x ${datapath2}/sfg_${analdate}_fhr0${ANALINC}_control ${datapath2}/sfg_${analdate}_fhr0${ANALINC}_ensmean ${datapath2}/sanl_${analdate}_ensmean ${datapath2}/sanl_${analdate}_control
! read in high res first guess
 call getarg(1,filenamehrfg)
! subtract this (low res first guess)
 call getarg(2,filenamelrfg)
! add this (low res analysis)
 call getarg(3,filenamelranl)
! and put in this file (high res anal)
 call getarg(4,filenamehranl)

! low res analysis
 call sigio_srohdc(nsigi,trim(filenamelranl), &
                   sigheadlranl,sigdatalranl,iret)
 print *,'low res analysis',trim(filenamelranl)
 print *,iret
 ! low res first guess
 call sigio_srohdc(nsigi,trim(filenamelrfg),&
                   sigheadlrfg,sigdatalrfg,iret)
 print *,'low res first guess',trim(filenamelrfg)
 print *,iret
 ! high res first guess
 call sigio_srohdc(nsigi,trim(filenamehrfg),&
                   sigheadhrfg,sigdatahrfg,iret)
 print *,'high res first guess',trim(filenamehrfg)
 print *,iret

 allocate(specdat((sigheadhrfg%jcap+1)*(sigheadhrfg%jcap+2)))
 allocate(psg(sigheadhrfg%lonb,sigheadhrfg%latb))
 allocate(psginc(sigheadhrfg%lonb,sigheadhrfg%latb))
 allocate(psgmeanfg(sigheadlrfg%lonb,sigheadlrfg%latb))
 allocate(psgmeananl(sigheadlrfg%lonb,sigheadlrfg%latb))
 !allocate(fg(sigheadhrfg%lonb,sigheadhrfg%latb,sigheadhrfg%levs))
 !allocate(anl(sigheadhrfg%lonb,sigheadhrfg%latb,sigheadhrfg%levs))
 sigheadhranl = sigheadhrfg
 sigheadhranl%idate = sigheadlranl%idate
 sigheadhranl%fhour = sigheadlranl%fhour
 call sigio_aldata(sigheadhranl,sigdatahranl,iret)

 ! compute difference of ps in grid space.
 !call sptez(0,sigheadlrfg%jcap,4,sigheadlrfg%lonb,sigheadlrfg%latb,sigdatalrfg%ps,psgmeanfg,1)
 !psgmeanfg = 10.*exp(psgmeanfg)
 !call sptez(0,sigheadlranl%jcap,4,sigheadlranl%lonb,sigheadlranl%latb,sigdatalranl%ps,psgmeananl,1)
 !psgmeananl = 10.*exp(psgmeananl)
 !print *,'low res ps increment',&
 !minval(psgmeananl-psgmeanfg),maxval(psgmeananl-psgmeanfg)
 !call sptez(0,sigheadlrfg%jcap,4,sigheadhrfg%lonb,sigheadhrfg%latb,sigdatahrfg%ps,psg,1)
 !psg = 10.*exp(psg)
 !psgmeanfg = psgmeananl-psgmeanfg
 !!open(7,file='lowresinc.dat',form='unformatted',access='direct',recl=sigheadlrfg%lonb*sigheadlrfg%latb*4)
 !!write(7,rec=1) psgmeanfg
 !!close(7)
 !call sptez(0,sigheadlrfg%jcap,4,sigheadlrfg%lonb,sigheadlrfg%latb,sigdatalrfg%ps,psgmeanfg,-1)
 !call specpad(sigdatalrfg%ps,specdat,sigheadlrfg%jcap,sigheadhranl%jcap)
 !call sptez(0,sigheadhrfg%jcap,4,sigheadhrfg%lonb,sigheadhrfg%latb,specdat,psginc,1)
 !!open(7,file='highresinc.dat',form='unformatted',access='direct',recl=sigheadhrfg%lonb*sigheadhrfg%latb*4)
 !!write(7,rec=1) psginc
 !!close(7)
 !print *,'high res ps increment',minval(psginc),maxval(psginc)
 !psg = psg + psginc
 !psg = alog(psg/10.)
 !call sptez(0,sigheadhranl%jcap,4,sigheadhranl%lonb,sigheadhranl%latb,sigdatahranl%ps,psg,-1)

 ! compute difference of lnps in spectral space
 call specpad(sigdatalranl%ps-sigdatalrfg%ps,specdat,sigheadlrfg%jcap,sigheadhranl%jcap)
 sigdatahranl%ps = sigdatahrfg%ps + specdat
 call sptez(0,sigheadhrfg%jcap,4,sigheadhrfg%lonb,sigheadhrfg%latb,sigdatahrfg%ps,psg,1)
 psg = 10.*exp(psg)
 call sptez(0,sigheadhranl%jcap,4,sigheadhranl%lonb,sigheadhranl%latb,sigdatahranl%ps,psginc,1)
 psginc = 10.*exp(psginc)
 !open(7,file='highresinc.dat',form='unformatted',access='direct',recl=sigheadlrfg%lonb*sigheadlrfg%latb*4)
 !write(7,rec=1) psginc
 !close(7)
 psginc = psginc - psg
 print *,'high res ps increment',minval(psginc),maxval(psginc)

 do k=1,sigheadhranl%levs
    ! vorticity
    call specpad(sigdatalranl%z(:,k)-sigdatalrfg%z(:,k),specdat,sigheadlrfg%jcap,sigheadhranl%jcap)
    sigdatahranl%z(:,k) = sigdatahrfg%z(:,k) + specdat
    ! divergence
    call specpad(sigdatalranl%d(:,k)-sigdatalrfg%d(:,k),specdat,sigheadlrfg%jcap,sigheadhranl%jcap)
    sigdatahranl%d(:,k) = sigdatahrfg%d(:,k) + specdat
    ! temp
    call specpad(sigdatalranl%t(:,k)-sigdatalrfg%t(:,k),specdat,sigheadlrfg%jcap,sigheadhranl%jcap)
    sigdatahranl%t(:,k) = sigdatahrfg%t(:,k) + specdat
    !call sptez(0,sigheadhrfg%jcap,4,sigheadhrfg%lonb,sigheadhrfg%latb,sigdatahrfg%t(:,k),fg(:,:,k),1)
    !call sptez(0,sigheadhranl%jcap,4,sigheadhranl%lonb,sigheadhranl%latb,sigdatahranl%t(:,k),anl(:,:,k),1)
    ! tracers (including humidity)
    do nt=1,sigheadhranl%ntrac
       call specpad(sigdatalranl%q(:,k,nt)-sigdatalrfg%q(:,k,nt),specdat,sigheadlrfg%jcap,sigheadhranl%jcap)
       sigdatahranl%q(:,k,nt) = sigdatahrfg%q(:,k,nt) + specdat
    enddo
 enddo
 !print *,minval(anl),maxval(anl)
 !print *,minval(anl-fg),maxval(anl-fg)
 sigdatahranl%hs = sigdatahrfg%hs ! orography

 deallocate(specdat,psg,psgmeanfg,psgmeananl,psginc)

 call sigio_swohdc(nsigo,trim(filenamehranl),sigheadhranl,sigdatahranl,iret)
 print *,'high res analysis',trim(filenamehranl)
 print *,sigheadhranl%idate, sigheadhranl%fhour, iret

end program recentersig_dualres

subroutine specpad(specin,specout,ntruncin,ntruncout)
 ! put specin (truncation ntruncin) in specout (truncation ntruncout), padding with zeros.
 ! ntruncin should be <= ntruncout
 integer, intent(in) :: ntruncin,ntruncout
 real, intent(in) :: specin((ntruncin+1)*(ntruncin+2))
 real, intent(out) :: specout((ntruncout+1)*(ntruncout+2))
 integer indxmn(0:ntruncout,0:ntruncout)
 integer m,n,nm
 nm = 1
 do m=0,ntruncout
    do n=m,ntruncout
       indxmn(m,n) = nm
       nm = nm + 2
    enddo
 enddo
 nm = 1
 specout = 0.
 do m=0,ntruncin
    do n=m,ntruncin
       specout(indxmn(m,n)) = specin(nm)
       specout(indxmn(m,n)+1) = specin(nm+1)
       nm = nm + 2
    enddo
 enddo
end subroutine specpad
