program psop

!ftn -O3 -assume byterecl -warn all -implicitnone -C -traceback -FR -I /project/projectdirs/incite11/whitaker/edison/newstuff/nwprod/lib/incmod/sigio_4 -o psop_test.x psop_gfs_test1.f90 kinds.o specmods.o rnorm.o constants.o -L/project/projectdirs/incite11/whitaker/edison/lib -lshtns -lfftw3 -L/project/projectdirs/incite11/whitaker/edison/newstuff/nwprod/lib -lsigio_4
! ! EnKF surface pressure forward operator.

 USE SIGIO_MODULE
 use specmod, only: sptez_s, isinitialized
 use constants, only: pi, rd, grav, cp, rearth, deg2rad, rad2deg, init_constants, init_constants_derived
 implicit none
 TYPE(SIGIO_HEAD) :: SIGHEAD
 TYPE(SIGIO_DATA) :: SIGDATA
 character(len=120) filenamein,obsfile,filename,obsfileout
 character(len=10) datestring
 integer m,n,nm,iret,nlats,nlons,nlevs,ntrac,ntrunc,ierr,nanals,nfhr,nobstot,&
         nob,nanal,j,iunit,iunitsig,fhmin,fhmax,fhout,fhanal,ntimes,&
         nchar,nreal,ii,nn,nlevt1,nlevt2,ntime,np,nobsh,izob,iunit_nml,iunito,idate
 real dxob,dyob,dtob,zerr,anal_obt,anal_oblapse,anal_obp,smoothparm,smoothfact,&
      lapsemin,lapsemax,lapseminall,lapsemaxall,&
      wavenumsq,delz_const,ensmean_ob,slpob,bias,preduce,palt,zthresh,zdiff,altob,errfact
 character(len=2) charfhr
 character(len=3) charnanal
 real, dimension(:), allocatable :: glats, glatspluspoles, specdat
 real, dimension(:), allocatable :: oblocx,oblocy,ob,zob,obtime,stdev,&
                                    anal_obz,stdevorig,anal_ob,biasob
 real, dimension(:,:), allocatable :: psg,zsg,datout,grid_tmp,analzs,&
                                      anal_ob2,rdiagbuf
 real, dimension(:,:,:), allocatable :: tempg,psig,pslg,&
       analpress,analtemp,anallapse,analps,zg
 integer, allocatable, dimension(:) :: stattype,iuseob
 character(8),allocatable,dimension(:):: cdiagbuf
 character (len=1) :: obtype
 character(len=19), allocatable :: statid(:)
 character (len=30) :: statname
 character (len=13) :: obid
 namelist /nam_psop/nlevt1,nlevt2,zerr,fhmin,fhmax,fhout,fhanal,datestring,&
                    nanals,nlons,nlats,obsfile,zthresh,errfact,delz_const

 call init_constants(.false.) ! initialize constants.
 call init_constants_derived()

 nchar=1; nreal=19
 iunit = 7
 iunitsig = 22
 iunit_nml = 912
 iunito = 9

 ! read namelist from file on all processors.
 zthresh = 9.9e31
 delz_const = 0.0 ! factor for adjusting ob error based on diff between station and model height
 zerr = 0
 errfact = 1.0
 open(iunit_nml,file='psop.nml',form='formatted')
 read(iunit_nml,nam_psop)
 close(iunit_nml)
 write(6,nam_psop)


 ntimes = 1+((fhmax-fhmin)/fhout)

 nanal =  1

 read(datestring,'(i10)') idate

!==> read in obs data (on root process).
 
 
 print *,trim(adjustl(obsfile))
 open(149,form="formatted",file=trim(adjustl(obsfile)))
 print *, filename
 nobstot = 0
 do 
   read(149,9801,err=199,end=199) obtype
   nobstot = nobstot + 1
 enddo
 199 continue

!==> allocate some arrays for obs and obs metadata.
  
 allocate(statid(nobstot))
 allocate(anal_ob(nobstot))
 allocate(biasob(nobstot))
 allocate(anal_ob2(nanals+1,nobstot))
 allocate(anal_obz(nobstot))
 allocate(stattype(nobstot))
 allocate(iuseob(nobstot))
 allocate(oblocx(nobstot))
 allocate(oblocy(nobstot))
 allocate(ob(nobstot))
 allocate(zob(nobstot))
 allocate(obtime(nobstot))
 allocate(stdev(nobstot))
 allocate(stdevorig(nobstot))
 allocate(cdiagbuf(nobstot),rdiagbuf(nreal,nobstot))
 
 
 rewind(149)
 nobsh = 0
 biasob = 0.
 do nob=1,nobstot
      read(149,9801) statid(nob),stattype(nob),obtype,oblocx(nob),oblocy(nob),&
           izob,obtime(nob),ob(nob),slpob,bias,stdevorig(nob),statname,obid
      if (bias .lt. 1.e20) biasob(nob) = bias
      stdev(nob) = errfact*stdevorig(nob)
      zob(nob) = izob
 9801 format(a19,1x,i3,1x,a1,1x,f7.2,1x,f6.2,1x,i5,1x,f6.2,1x,f7.1,&
             1x,f7.1,1x,e10.3,1x,f5.2,1x,a30,1x,a13)
      if (oblocy(nob) .lt. 0.) nobsh = nobsh + 1
      if (oblocx(nob) .lt. 0.) oblocx(nob) = oblocx(nob) + 360.
 enddo
 print *, nobstot,' total obs'
 print *, nobsh,' total obs in SH'
 close(149)

!==> convert ob location to radians.
  
 oblocx = deg2rad*oblocx
 oblocy = deg2rad*oblocy

 write(charnanal,'(i3.3)') nanal
 if (nanal .eq. nanals+1) then
    obsfileout = "diag_conv_ges."//datestring//"_ensmean"
 else
    obsfileout = "diag_conv_ges."//datestring//"_mem"//charnanal
 end if
 open(iunito,form="unformatted",file=trim(adjustl(obsfileout)),status='replace',convert='native')
 
 ntime = 0
 do nfhr=fhmin,fhmax,fhout
 ntime = ntime + 1

 write(charfhr,'(i2.2)') nfhr
 if (nanal .eq. nanals+1) then
 filenamein = "sfg_"//datestring//"_fhr"//charfhr//"_ensmean"
 else
 filenamein = "sfg_"//datestring//"_fhr"//charfhr//"_mem"//charnanal
 end if

 call sigio_srohdc(iunitsig,trim(filenamein), &
                   sighead,sigdata,iret)
 if (iret .ne. 0) then
    print *,'error reading file ',iret,trim(filenamein)
    stop
 end if

 if (nfhr .eq. fhmin) then
 nlevs = sighead%levs
 ntrac = sighead%ntrac
 ntrunc = sighead%jcap
 allocate(psg(nlons,nlats))
 allocate(zsg(nlons,nlats))
 allocate(tempg(nlons,nlats,nlevs))
 allocate(zg(nlons,nlats,nlevs))
 allocate(pslg(nlons,nlats,nlevs))
 allocate(psig(nlons,nlats,nlevs+1))
 allocate(datout(nlons,nlats))
 allocate(grid_tmp(nlons,nlats))
 allocate(specdat((ntrunc+1)*(ntrunc+2)))
 allocate(glats(nlats))
 allocate(glatspluspoles(nlats+2))
 allocate(analps(nlons+1,nlats+2,ntimes))
 allocate(analtemp(nlons+1,nlats+2,ntimes))
 allocate(analpress(nlons+1,nlats+2,ntimes))
 allocate(anallapse(nlons+1,nlats+2,ntimes))
 allocate(analzs(nlons+1,nlats+2))
 end if

 call getsigdata(sighead,sigdata,glats,tempg,psg,pslg,psig,zsg,nlons,nlats,nlevs,ntrunc)
 call sigio_axdata(sigdata,ierr)

 ! integrate hydrostatic equation to get heights.
 call temptoz(nlats,nlons,nlevs,real(rd),real(cp),real(grav),psig,pslg,zsg,tempg,zg)

 ! add wraparound and pole points.
 call addpolewrap(psg,analps(:,:,ntime),nlons,nlats)
 call addpolewrap(zsg,analzs,nlons,nlats)
 call addpolewrap(tempg(:,:,nlevt2),analtemp(:,:,ntime),nlons,nlats)
 call addpolewrap(pslg(:,:,nlevt2),analpress(:,:,ntime),nlons,nlats)
 grid_tmp = (tempg(:,:,nlevt1)-tempg(:,:,nlevt2))/&
            (zg(:,:,nlevt2)-zg(:,:,nlevt1))
 lapsemin = minval(grid_tmp); lapsemax = maxval(grid_tmp)
 ! smooth lapse rate to remove neg values.
 call sptez_s(specdat,grid_tmp,-1)
 print *,'min/max/mean unsmoothed lapse rate',lapsemin,lapsemax,0.5*sqrt(2.)*specdat(1)
 smoothparm = real(ntrunc)/2.
 nm = 1
 do m=0,ntrunc
    do n=m,ntrunc
       wavenumsq = n*(n+1)
       smoothfact = exp(-(wavenumsq/smoothparm**2))
       !smoothfact = 1.-(real(n)/real(ntrunc))
       specdat(nm) = smoothfact*specdat(nm)
       specdat(nm+1) = smoothfact*specdat(nm+1)
       nm = nm + 2
    enddo
 enddo
 !specdat(3:(ntrunc+1)*(ntrunc+2)) = 0.
 call sptez_s(specdat,grid_tmp,1)
 lapsemin = minval(grid_tmp); lapsemax = maxval(grid_tmp)
 print *,'min/max/mean smoothed lapse rate',lapsemin,lapsemax,0.5*sqrt(2.)*specdat(1)
 if (lapsemin .lt. 0) then
      print *,'negative lapse rate'
      stop
 endif
 call addpolewrap(grid_tmp,anallapse(:,:,ntime),nlons,nlats)

 enddo ! nfhr

 !==> 0.5*pi-latitudes with poles included (used in bilinear interp routine).
 glatspluspoles(1) = 0.
 glatspluspoles(nlats+2) = pi
 do j=2,nlats+1
   glatspluspoles(j) = 0.5*pi - glats(j-1)
 enddo

 !==> perform Benjamin and Miller reduction for each ob, compute ob priors.
 !    also do gross qc checks.
 nn = 0
 do nob=1,nobstot
    ! make sure ob location > -90 degrees.
    if (oblocy(nob) .gt. 0.5*pi+1.e-6 .or. oblocy(nob) .lt. -0.5*pi-1.e-6) then
       print *,'WARNING: ob located outside domain',oblocy(nob)
       print *,'the ob latitude will be clipped to the nearest pole'
    end if
    if (oblocy(nob) .lt. -0.5*pi) oblocy(nob) = -0.5*pi
    if (oblocy(nob) .gt. 0.5*pi) oblocy(nob) = 0.5*pi
    ! longitudes are evenly spaced
    dxob = (0.5*float(nlons)*oblocx(nob)/pi)+1.
    ! gaussian latitudes are not.
    j=1
    dyob = 1.
    do 
       if (glatspluspoles(j) .ge. 0.5*pi-oblocy(nob)) exit
       j = j + 1
    enddo 
    if (j .gt. 1) dyob = float(j-1) + (0.5*pi-oblocy(nob)-glatspluspoles(j-1))/(glatspluspoles(j)-glatspluspoles(j-1))
    dtob = 1.+((real(fhmin)+obtime(nob))/real(fhout))
    !if (nproc .eq. 0) write(6,9001) rad2deg*oblocx(nob),rad2deg*oblocy(nob),dxob,dyob,rad2deg*(0.5*pi-glatspluspoles(int(dyob))),rad2deg*(0.5*pi-glatspluspoles(int(dyob)+1))
    !9001 format(1x,f7.2,1x,f6.2,1x,f7.1,1x,f7.1,1x,f6.2,1x,f6.2)
    !print *,'min/max analtemp',minval(analtemp),maxval(analtemp)
    call lintrp3(analtemp,anal_obt,&
                 dxob,dyob,dtob,nlons+1,nlats+2,ntimes)
    call lintrp3(anallapse,anal_oblapse,&
                 dxob,dyob,dtob,nlons+1,nlats+2,ntimes)
    call lintrp3(analpress,anal_obp,&
                 dxob,dyob,dtob,nlons+1,nlats+2,ntimes)
    call lintrp2(analzs,anal_obz(nob),&
                 dxob,dyob,nlons+1,nlats+2)
    !if (nproc .eq. 0) write(6,9001) rad2deg*oblocx(nob),rad2deg*oblocy(nob),int(zob(nob)),int(anal_obz(nob)),nint(analzs(int(dxob),int(dyob))),nint(analzs(int(dxob)+1,int(dyob))),nint(analzs(int(dxob),int(dyob)+1)),nint(analzs(int(dxob)+1,int(dyob)+1))
    !9001 format(1x,f7.2,1x,f6.2,1x,i5,1x,i5,4(1x,i5))
    
    ! this is ob prior.
    call lintrp3(analps,anal_ob(nob),&
                 dxob,dyob,dtob,nlons+1,nlats+2,ntimes)
    ! adjust Hx to (perturbed) station height
    anal_ob(nob) = &
    preduce(anal_ob(nob),anal_obp,anal_obt,zob(nob),anal_obz(nob),anal_oblapse)
    !if (anal_ob(nob) .ne. anal_ob(nob)) then
    !  print *,'anal_ob 2 NaN for nanal',nanal
    !endif

    zdiff = zob(nob)-anal_obz(nob)
    ! adjust ob error based on diff between station and model height.
    ! (GSI uses 0.005 for delz_const)
    stdev(nob) = stdev(nob) + delz_const*abs(zdiff)
    if ((obtime(nob) .ge. -3. .and. &
        obtime(nob) .le. 3.) .and. abs(zdiff) .lt. zthresh) then
        iuseob(nob) = 1
    else
        iuseob(nob) = 0
        nn = nn + 1
    end if
! gross error check.
    if (iuseob(nob) .eq. 1) then
       altob = palt(ob(nob),zob(nob))
       if (altob .lt. 850. .or. altob .gt. 1090.) then
          print *,'failed gross error check',rad2deg*oblocx(nob),rad2deg*oblocy(nob),obtime(nob),ob(nob),zob(nob),anal_obz(nob),altob
          iuseob(nob)=0
          nn = nn + 1
       end if
    end if   
 enddo
 if (nn .ne. 0) print *,nanal,nn,' failed gross qc check'

! distribute the ob error to all processors.
! first, gather back on last proc.
    do nob=1,nobstot
       ! replace hx computed from ens mean with ens mean hx.
       ensmean_ob = sum(anal_ob2(1:nanals,nob))/float(nanals)
       anal_ob2(nanals+1,nob) = ensmean_ob
       ! recenter hx ensemble about hx computed from ensemble mean.
       !do nanal=1,nanals
       !   anal_ob2(nanal,nob) = anal_ob2(nanal,nob) - ensmean_ob + anal_ob(nob)
       !enddo
       !anal_ob2(nanals+1,nob) = anal_ob(nob)
    enddo

! now push back out to all other procs.
 do ii=1,nobstot
    if (iuseob(ii) .eq. 0) stdev(ii) = 1.e4
    cdiagbuf(ii)    = trim(adjustl(statid(ii)))  ! station id

    rdiagbuf(1,ii)  = stattype(ii)       ! observation type
    rdiagbuf(2,ii)  = stattype(ii)       ! observation subtype
    rdiagbuf(3,ii)  = rad2deg*oblocy(ii) ! observation latitude (degrees)
    rdiagbuf(4,ii)  = rad2deg*oblocx(ii) ! observation longitude (degrees)
    rdiagbuf(5,ii)  = zob(ii)            ! station elevation (meters)
    rdiagbuf(6,ii)  = ob(ii)             ! observation pressure (hPa)
    rdiagbuf(7,ii)  = anal_obz(ii)       ! observation height (meters)
    rdiagbuf(8,ii)  = obtime(ii)         ! obs time (hours relative to analysis time)

    rdiagbuf(9,ii)  = 1.                 ! input prepbufr qc or event mark
    rdiagbuf(10,ii) = 1.e30              ! setup qc or event mark
    rdiagbuf(11,ii) = 1.                 ! read_prepbufr data usage flag
    !if(iuseob(ii) .eq. 1) then
    !   rdiagbuf(12,ii) = 1.              ! analysis usage flag (1=use, -1=not used)
    !else
    !   rdiagbuf(12,ii) = -1.                    
    !endif
    rdiagbuf(12,ii) = 1.                 ! analysis usage flag (1=use, -1=not used)
    rdiagbuf(13,ii) = 1.                 ! nonlinear qc relative weight
    rdiagbuf(14,ii) = 1./stdevorig(ii)   ! prepbufr inverse obs error (hPa**-1)
    rdiagbuf(15,ii) = 1./stdevorig(ii)   ! read_prepbufr inverse obs error (hPa**-1)
    rdiagbuf(16,ii) = 1./stdev(ii)       ! final inverse observation error (hPa**-1)
    rdiagbuf(17,ii) = ob(ii)  ! surface pressure observation (hPa)
    ! bias correction applied to Hx (guess in ob space).
    ! biasob is mean O-F over last 60 or so days.
    ! obs-ges used in analysis (coverted to hPa)
    rdiagbuf(18,ii) = ob(ii)-(anal_ob2(nanal,ii)+biasob(ii))
    ! obs-ges w/o bias correction.
    rdiagbuf(19,ii) = ob(ii)-anal_ob2(nanal,ii)
    if (anal_ob2(nanal,ii) .ne. anal_ob2(nanal,ii)) then
      print *,'NaN for nanal',nanal
    endif
 enddo
 write(iunito) idate
 write(iunito)' ps',nchar,nreal,nobstot,nanal
 write(iunito)cdiagbuf(1:nobstot),rdiagbuf(:,1:nobstot)
 if (nanal .eq. nanals+1) then
    open(9,form='formatted',file='psobs_prior.txt')
    do nob=1,nobstot
       if (stdev(nob) .gt. 99.99) stdev(nob) = 99.99
       write(9,9802) stattype(nob),rad2deg*oblocx(nob),rad2deg*oblocy(nob),&
               nint(zob(nob)),nint(anal_obz(nob)),obtime(nob),ob(nob),&
               anal_ob2(nanal,nob),stdevorig(nob),stdev(nob),iuseob(nob)
    enddo
    9802 format(i3,1x,f7.2,1x,f6.2,1x,i5,1x,i5,1x,f6.2,1x,f7.1,1x,&
                  f7.1,1x,f5.2,1x,f5.2,1x,i1)
    close(9)
 !else
 !   filename = 'psobs_mem'//charnanal//'.txt'
 !   print *,'write out ',trim(filename)
 !   open(9,form='formatted',file=filename)
 !   do nob=1,nobstot
 !      if (stdev(nob) .gt. 99.99) stdev(nob) = 99.99
 !      write(9,9802) stattype(nob),rad2deg*oblocx(nob),rad2deg*oblocy(nob),&
 !              nint(zob(nob)),nint(anal_obz(nob)),obtime(nob),ob(nob),&
 !              anal_ob2(nanal,nob),stdevorig(nob),stdev(nob),iuseob(nob)
 !   enddo
 !   close(9)
 end if
 close(iunito)

end program psop

subroutine addpolewrap(fin,fout,nx,ny)
! add pole and wrap-around points to lon,lat array.
 integer j,nx,ny
 real fin(nx,ny),fout(nx+1,ny+2)
 do j=2,ny+1
    fout(1:nx,j) = fin(:,j-1)
 enddo
 fout(:,1) = sum(fin(:,1))/float(nx)
 fout(:,ny+2) = sum(fin(:,ny))/float(nx)
 fout(nx+1,:) = fout(1,:)
end subroutine addpolewrap

real function preduce(ps,tpress,t,zmodel,zob,rlapse)
! compute MAPS pressure reduction from model to station elevation
! See Benjamin and Miller (1990, MWR, p. 2100)
! uses 'effective' surface temperature extrapolated
! from virtual temp (tv) at tpress mb
! using specified lapse rate.
! ps - surface pressure to reduce.
! t - virtual temp. at pressure tpress.
! zmodel - model orographic height.
! zob - station height
! rlapse - lapse rate (positive)
   use constants, only: grav,rd
   implicit none
   real, intent(in) :: t,tpress,zmodel,zob,ps,rlapse
   real t0,alpha
   real, parameter :: tx = 290.5
   real, parameter :: ty = 255.0
   alpha = rd*rlapse/grav
   ! t0 is eqn 5.21,5.22 from IFS docs
   ! chap 5 (http://www.ecmwf.int/research/ifsdocs/CY33r1/ASSIMILATION/IFSPart2.pdf)
   !t0 = t1 + alpha*t1*log(ps/p1)
   !t0 = 0.5*(t0 + max(ty,min(tx,t0)))
   ! from Benjamin and Miller (http://dx.doi.org/10.1175/1520-0493(1990)118<2099:AASLPR>2.0.CO;2) 
   t0 = t*(ps/tpress)**alpha ! eqn 4 from B&M
   preduce = ps*((t0 + rlapse*(zob-zmodel))/t0)**(1./alpha) ! eqn 1 from B&M
end function preduce

function palt(ps,zs)
! compute QNH altimeter setting (in mb) given ps (in mb), zs (in m).
! see WAF, Sept 1998, p 833-850 (Pauley) section 2c
   real, parameter :: rd =2.8705e+2
   real, parameter :: g = 9.8
   real, parameter :: rlapse = 0.0065
   real, parameter :: t0 = 288.15
   real, parameter :: p0 = 1013.25
   real, parameter :: alpha = rd*rlapse/g
   real palt,ps,zs
   palt = ps*(1.+(rlapse*zs/t0)*(p0/ps)**alpha)**(1./alpha)
end function palt

 subroutine getsigdata(sighead,sigdata,glats,tempg,psg,pslg,psig,zsg,nlons,nlats,nlevs,ntrunc)
  use sigio_module
  use specmod, only: gaulats, gauwts, init_spec_vars, sptez_s, isinitialized
  use constants, only: rd, grav, cp, rearth, init_constants, init_constants_derived
  implicit none
  real ak(nlevs+1),bk(nlevs+1),kap1,kapr
  type (sigio_data), intent(in out) :: sigdata
  type (sigio_head), intent(in) :: sighead 
  real, dimension(nlons,nlats,nlevs), intent(out) :: tempg,pslg
  real, intent(out), dimension(nlons,nlats,nlevs+1) :: psig
  real, dimension(nlons,nlats), intent(out) :: psg,zsg
  real, dimension(nlats), intent(out) :: glats
  integer, intent(in) :: ntrunc,nlevs,nlons,nlats 
  integer k,ierr
  if (.not. isinitialized) then
     call init_spec_vars(nlons,nlats,ntrunc,4)
     call init_constants(.false.)
     call init_constants_derived()
  endif
  kap1 = (rd/cp)+1.0
  kapr = (cp/rd)
  glats = asin(gaulats)
  !==> get U,V,temp,z,q,ps on gaussian grid.
  do k=1,nlevs
     call sptez_s(sigdata%t(:,k),tempg(:,:,k),1)
     print *,k,minval(tempg(:,:,k)),maxval(tempg(:,:,k))
  enddo
  print *,'min/max tempg',nlons,nlats,nlevs, minval(tempg),maxval(tempg)
  !print *,'min/max tempspec',minval(sigdata%t),maxval(sigdata%t)
  call sptez_s(sigdata%ps,psg,1)
  call sptez_s(sigdata%hs,zsg,1)
  !==> input psg is ln(ps) in centibars - convert to ps in millibars.
  psg = 10.*exp(psg)
  if (sighead%idvc .eq. 0) then ! sigma coordinate, old file format.
      ak = 0.
      bk = sighead%si(1:nlevs+1)
  else if (sighead%idvc .eq. 1) then ! sigma coordinate
      ak = 0.
      bk = sighead%vcoord(1:nlevs+1,2)
  else if (sighead%idvc .eq. 2 .or. sighead%idvc .eq. 3) then ! hybrid coordinate
      bk = sighead%vcoord(1:nlevs+1,2) 
      ak = 0.01*sighead%vcoord(1:nlevs+1,1)  ! convert to mb
  else
      print *,'unknown vertical coordinate type',sighead%idvc
      stop
  end if
  !==> pressure at layers and interfaces.
  do k=1,nlevs
   psig(:,:,k)=ak(k)+bk(k)*psg(:,:) 
  enddo
  psig(:,:,nlevs+1)=0.
  do k=1,nlevs
   ! gsi formula ("phillips vertical interpolation")
   pslg(:,:,k)=((psig(:,:,k)**kap1-psig(:,:,k+1)**kap1)/&
                (kap1*(psig(:,:,k)-psig(:,:,k+1))))**kapr
   ! average of interface exner
   !pslg(:,:,k) = (0.5*(psig(:,:,k)**kapr + psig(:,:.k+1)**kapr))**(1./kapr)
   ! average of log(p) - consistent with linear interp in log(p) used
   ! simple average of p.
   !pslg(:,:,k) = 0.5*(psig(:,:,k)+psig(:,k+1))
  end do

end subroutine getsigdata

subroutine lintrp2(f,g,dx,dy,nx,ny)
                                                                      
! subprogram:    lintrp2      linear interpolation in two dimensions.
!
!
!   input argument list:
!     f        - input interpolator
!     dx,dy    - input x,y -coords of interpolation point (grid units)
!     nx,ny    - x,y-dimensions of interpolator grid
!
!   output argument list:
!     g        - output interpolatee
 
 integer, intent(in) :: nx,ny
 real, intent(in) :: f(nx,ny),dx,dy
 real, intent(out) :: g
 integer ix,iy,ixp,iyp
 real delx,dely
  

 ix=dx
 ix=max(1,min(ix,nx))
 iy=dy
 iy=max(1,min(iy,ny))
 ixp=ix+1
 ixp=min(ixp,nx)
 iyp=iy+1
 iyp=min(iyp,ny)
 delx=dx-ix
 dely=dy-iy
 g=f(ix,iy)*(1.-delx)*(1.-dely) &
  +f(ixp,iy)*delx*(1.-dely) &
  +f(ix,iyp)*(1.-delx)*dely &
  +f(ixp,iyp)*delx*dely

end subroutine lintrp2

subroutine lintrp3(f,g,dx,dy,dz,nx,ny,nz)
!                .      .    .                                        
! subprogram:    lintrp3      linear interpolation in three dimensions.
!
!   input argument list:
!     f        - input interpolator
!     dx,dy,gz    - input x,y,z -coords of interpolation point (grid units)
!     nx,ny,nz    - x,y,z-dimensions of interpolator grid
!
!     x is longitude, y is latitude and z is ln(sigma).
!     x is assumed to be regularly spaced, y and z aren't.
!     if z outside range of z(1) to z(nz), g=-9.9e31 is returned.
!
!   output argument list:
!     g        - output interpolatee
!
 integer, intent(in) :: nx,ny,nz
 real, intent(in) :: f(nx,ny,nz),dx,dy,dz
 real, intent(out) :: g
 integer ix,iy,ixp,iyp,iz,izp
 real delx,dely,delz

 ix=dx
 ix=max(1,min(ix,nx))
 iy=dy
 iy=max(1,min(iy,ny))
 iz=dz
 iz=max(1,min(iz,nz))
 ixp=ix+1
 ixp=min(ixp,nx)
 iyp=iy+1
 iyp=min(iyp,ny)
 izp=iz+1
 izp=min(izp,nz)
 delx=dx-ix
 dely=dy-iy
 delz=dz-iz
 g=f(ix,iy,iz)*(1.-delx)*(1.-dely)*(1.-delz)+ &
   f(ixp,iy,iz)*delx*(1.-dely)*(1.-delz)+ &
   f(ix,iyp,iz)*(1.-delx)*dely*(1.-delz)+ &
   f(ixp,iyp,iz)*delx*dely*(1.-delz)+ &
   f(ix,iy,izp)*(1.-delx)*(1.-dely)*delz+ &
   f(ixp,iy,izp)*delx*(1.-dely)*delz+ &
   f(ix,iyp,izp)*(1.-delx)*dely*delz+ &
   f(ixp,iyp,izp)*delx*dely*delz

end subroutine lintrp3

 subroutine temptoz(nlons,nlats, nlevs,rgas,cp,grav,pint,pl,zs,tv,z)
! compute z (geopot height) on interfaces, given 
! pint (interface pressure in hPa),
! pl (pressure at mid-layers in hPa), tv (virtual temp at mid-layers) and
! zs (surface orog). rgas,cp,grav are gas constant, specific heat and gravity.
! z does not include surface height (k=1 is 1st level, k=nlevs is model top)
! uses hydrostatic eqn d(phi)/d(pi) = -thetav, where phi is geopot. height,
! pi is exner function and thetav is virtual potential temp.
  implicit none
  integer, intent(in) :: nlons,nlats,nlevs
  real, dimension(nlons,nlats, nlevs) :: thetav,pil
  real, dimension(nlons,nlats, nlevs+1) :: pii
  real, intent(in), dimension(nlons,nlats,nlevs) :: tv,pl
  real, intent(in), dimension(nlons,nlats,nlevs+1) :: pint
  real, intent(out), dimension(nlons,nlats,nlevs) :: z
  real, intent(in), dimension(nlons,nlats) :: zs
  real, intent(in) :: rgas,cp,grav
  integer i,j,k
  real dz
 
  pii = cp*(pint/1.e3)**(rgas/cp)
  pil = cp*(pl/1.e3)**(rgas/cp)
  thetav = cp*tv/pil
  do j=1,nlats
  do i=1,nlons
     dz = -thetav(i,j,1) * (pii(i,j,2)-pii(i,j,1))
     z(i,j,1) = grav*zs(i,j) + dz
     do k=3,nlevs+1
        dz = -thetav(i,j,k-1) * (pii(i,j,k)-pii(i,j,k-1))
        z(i,j,k-1) = z(i,j,k-2) + dz
     end do
  end do
  end do
  z = z/grav
 
 end subroutine temptoz
