program ensrs_gfs_psonly

! Assimilate surface pressure obs into the NCEP GFS serially using 
! a 4-D EnSRF (similar to a smoother).

! obs adjusted to model orography using
! Benjamin and Miller (1990, MWR, p. 2100) MAPS
! pressure reduction.
! uncertainity in station elevation, observation, temperature and 
! lapse rate accounted for in reduction.

! State vector updated by obs is (U*coslat,V*coslat,TV,Q, ..tracers.. ,PS)
! tracers (ozone, cloud liquid water) may not updated if nvars < 6

! uses spherical harmonic module (spharmt).

! horizontal and vertical covariance localization included.
! separate covariance localization for NH,TR,SH.

! separate covariance inflation factors for NH,TR,SH.
! inflation applied to prior AND posterior if postinflate = .true.

! ensemble quality control (using a background check and an f-test to see
! if fit to nearby obs harmed by assimilating each ob). Obs that fail
! background check can be 'redeemed' if the significantly improve fit
! to nearby obs.

! ob selection based upon significance of HPaHT/HPbHT (ob not used
! if f-test probability is less than a specified threshold).

! Prior at ob locations
! updated as well as model state vector - eliminates the need
! for mpi communication with obs loop at the cost of redundant
! computation updating all ob priors for each ob on every processo).
! More importantly, it simplifies computing Hx at the right time
! (linear interpolation in time and space used for Hx).

! domain decomposition is by vertical level.

! run-time parameters read from namelist (nam_ensrf)

! Jeff Whitaker <jeffrey.s.whitaker@noaa.gov> 20070312

! spherical harmonic module
use spharmt
 
implicit none 

! number of lons and lats on gaussian grid 
integer, parameter :: nlons = 192
integer, parameter :: nlats = 94  
! spectral truncation
integer, parameter :: ntrunc = 62  
! total number of sigmal levels
integer, parameter :: nlevs = 28 
! sigma level for tv used in MAPS pressure reduction.
! (typically first level above boundary layer).
integer, parameter :: nlevt2 = 9
integer, parameter :: nlevt1 = nlevt2-1
! number of ensemble members
integer, parameter :: nanals = 56   
! total number of complex spectral coeffs.
integer, parameter :: nmdim = (ntrunc+1)*(ntrunc+2)/2
! number of 'tracers' (specific humidity, ozone, cloud condensate)
integer, parameter :: ntrac = 3

! nvars can be 4, 5 or 6.
integer, parameter :: nvars = 4 ! neither ozone or cloud condensate updated. 
!integer, parameter :: nvars = 5 ! ozone updated, but cloud condensate not.
!integer, parameter :: nvars = 6 ! everything updated.

! number of time levels to compute hx.
integer, parameter :: ntimes = 3
! forecast hrs from fhrmin to fhrmin+fhrinc*(ntimes-1) used to compute hx.
real, parameter :: fhrmin = 3.
real, parameter :: fhrinc = 3.
! total number of horizontal grids in state vector.
integer, parameter :: ndim = nlevs*nvars+1

! structure for spherical harmonic transforms.
type (sphere) :: sphere_dat

real, dimension(44) :: headext
real, dimension(201) :: sisl
integer, allocatable, dimension(:) :: iuseob,stattype,iassim,ibadob,nindxob,ifailbg,ifailbud
integer idatein(4),idateout(4),nindx((nlons+1)*(nlats+2)),iunit,iunit_tmp,&
 iunit_nml,i,j,nproc,nobstot,nvar,nn,nt,k,nob,np,nanal,nx,ij,nobsps_tr,nobx,&
 niter,nobmin,nobsps_sh,nobsps_nh,ierr,n,numproc,izob,nobsh,nobsuse,&
 igen,nfail,buddy_iterations,&
 nobsclose,ndim_max,ntot,nnx,npt,nobskip,i1,i2,n1,n2,ninc,nob2,ncount_tot,nfhr
real fsg(nlevs),hsg(nlevs+1),covinflatenh,covinflatesh,lnsigcutoffnh,&
 covinflatetr,lnfsg(nlevs),lnhsg(nlevs),pi,dtor,corrlengthnh,&
 spreadpsthresh,latbound,delat,corrlengthsh,corrlengthtr,lnsigl,altob,&
 standlapse_err,lnsigcutoffsh,lnsigcutofftr,oberrfact,sprd_tol,&
 oberrnew,ob_mean,yminushx,sigtest_zscore,sigtest,delt,&
 corrlav,corrlmin,corrlmax,lnsiglav,lnsiglmin,lnsiglmax,fmin,&
 rtod,covinf,covinfmax,deglat,sumps_nh,biasps_nh,sumps_sh,biasps_sh,&
 ftest_thresh2,sumps_tr,biasps_tr,r,cf,pfht,height,gainfact,corrl,pfht_ob,&
 zdiff,zthresh,ftest_dist,ftest_thresh,corrl2,suma,sumb,sumao,sumbo,&
 zerr,theobfit_prior,theobfit_post,theobsprd_prior,theobsprd_post,rnorm,&
 sumps_spread_nh,sumps_spread_sh,sumps_spread_tr,sumps_oberr_nh,preduce,&
 sumps_oberr_sh,sumps_oberr_tr,rlat,rlon,fhour,corr_fn,dist,betai,f,df,prob,palt
character (len=1), allocatable, dimension(:) :: obtype
character (len=19), allocatable, dimension(:) :: statid
character (len=30), allocatable, dimension(:) :: statname
character (len=13), allocatable, dimension(:) :: obid
real, dimension(ndim) :: hgt
real, allocatable, dimension(:) :: oblocx,oblocy,oberrvar,&
                                   dxob,dyob,ensmean_ob,dtob,slpob,&
                                   ensmean_obt,rlapse_ensmean,&
                                   oborig,bias,covlocalob,&
                                   stdevorig,&
                                   obtime,stdev,ob,zob,zfgob,pob,&
                                   obfit_prior,obsprd_prior,&
                                   obfit_post,obsprd_post,pfhtob
real, allocatable, dimension(:,:) :: &
ensmean_chunk,fact,anal_ob,anal_obt,zobens,rlapse
real, allocatable, dimension(:,:,:) :: anal_chunk


real, dimension(ndim) :: vfact,covinf_vfact
real, dimension((nlons+1)*(nlats+2)) ::  hcovlocal,lons,lats,hdep,analps_stdev
real (kind=8) :: tbegin,tend,t1,t2,tav,tmax,tmin,tn
real, dimension((nlons+1)*(nlats+2),ndim) :: ensmean,anal
! analps,analtemp just used for computation of Hx
real, dimension((nlons+1)*(nlats+2),ntimes,nanals) :: analps,analtemp,anallapse
real, dimension((nlons+1)*(nlats+2),ntimes) :: ensmeanps, psmeancheck, &
                          ensmeantmp, ensmeantemp, ensmeanlapse
real glats(nlats+2),kfgain,anal_ob2(nanals),ensmean_ob2,ob_new(nanals)
real, dimension(nlons+1,nlats+2) :: zsg,psg
real, dimension((nlons+1)*(nlats+2),ndim) :: covinflate
real, dimension(nlons+1,nlats+2,nlevs) :: ug,vg,tempg,zg
real, dimension(nlons+1,nlats+2,nlevs,nvars-3) :: qg
complex, dimension(nmdim,nlevs) :: vrtnm,divnm,tempnm
complex, dimension(nmdim,nlevs,ntrac) :: qnm
! array to hold first guess ensemble mean tracers (that are not updated).
complex, allocatable, dimension(:,:,:) :: qnmfgmean
complex, dimension(nmdim) :: psnm,zsnm,scrnm
character (len=120) :: datapath,filename,obsfile,obsfileout,obsfileout_ens
character (len=10) :: datestring,fgprefix
character (len=3) :: charnanal,charnav
character (len=1) :: c1 
character (len=8), dimension(4) :: lab
logical useheight, adaptive_local, postinflate, use_standlapse, biascorrected

namelist /nam_ensrf/datestring,datapath,obsfile,corrlengthnh,&
                    corrlengthsh,corrlengthtr,covinflatenh,adaptive_local,&
                    covinflatesh,lnsigcutoffnh,useheight,obsfileout,&
                    obsfileout_ens,use_standlapse,sprd_tol,buddy_iterations,&
                    lnsigcutoffsh,lnsigcutofftr,zthresh,zerr,&
                    fgprefix,covinflatetr,oberrfact,postinflate,&
                    sigtest_zscore,ftest_dist,ftest_thresh,&
                    ftest_thresh2,standlapse_err,&
                    igen,delat,latbound,spreadpsthresh

! constants.
real, parameter :: re = 6.3712e6! radius of earth
real, parameter :: g = 9.80665  ! gravity
real, parameter :: rgas = 287.05! gas constant for dry air
real, parameter :: standlapse = 0.0065! gas constant for dry air
! mpi definitions.
include 'mpif.h'
integer MPI_Status(MPI_STATUS_SIZE)

call MPI_Init(ierr)
! nproc is process number, numproc is total number of processes.
call MPI_Comm_rank(MPI_COMM_WORLD,nproc,ierr)
call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)

if (nproc .eq. 0) print *,'running on ',numproc,' processors ..'

pi = 4.*atan(1.0)
dtor = pi/180.
rtod = 1./dtor
call set_random_seed(0) ! set random seed.

! defaults for namelist
! time (analysis time YYYYMMDDHH)
datestring = " " ! mandatory
! horizontal localization length scale (km).
corrlengthnh = 5000 ! default 5000 km for nh.
corrlengthsh = 5000 ! for sh.
corrlengthtr = 5000 ! for tropics.
! vertical localization length scale (in -ln(sigma))
! Default is no vertical localization.
lnsigcutoffnh = 9.9e31
lnsigcutofftr = 9.9e31
lnsigcutoffsh = 9.9e31
! covinflatenh (covariance inflation for nh)
covinflatenh = 1.0 ! default is no covariance inflation.
! covinflatesh (covariance inflation for sh)
covinflatesh = 1.0
! covinflatetr (covariance inflation for tropicsh)
covinflatesh = 1.0
! path to data directory (include trailing slash)
datapath = " " ! mandatory
! observation filename
obsfile = " " ! mandatory
! output observation filename (obs used, innovation stats).
obsfileout = " " ! if blank, don't write out.
obsfileout_ens = " " ! if blank, don't write out.
! tolerance for background check.
! if |y-Hx| < sprd_tol*sqrt(S+R), don't use that ob.
sprd_tol = 9.9e31
! analyze height instead of temp (no temp obs assimilated if true).
useheight = .false.
! don't use stations higher than this elevation
zthresh = 9.9e31
! uncertainty in station elevation.
zerr = 0.
! unit # for i/o
iunit = 7
iunit_tmp=66
iunit_nml=912
! first-guess file prefix (max 10 char)?
fgprefix = 'sfg'
! factor to inflate ob error standard deviations.
oberrfact = 1.0 ! default don't inflate.
! don't assimilate ob if the t-statistic for the increment is less than sigtest_zscore.
sigtest_zscore = 0.0 ! default use all obs.
! parameters for f-test (don't reject anything by default).
ftest_dist=0.0
ftest_thresh=1.0
ftest_thresh2=-1.0
! posterior inflation
postinflate = .false.
! adaptive localization
adaptive_local = .false.
! use standard atmosphere lapse rate?
use_standlapse = .false.
! error in standard atm. lapse rate
standlapse_err = 0.
! do buddy check only once by default
buddy_iterations = 1
! generating code (we use as version #)
igen = 100
! definition of tropics and mid-latitudes (for inflation and localization)
latbound = 30. ! this is where the tropics start
delat = 10.    ! width of transition zone.
! if ensemble standard deviation exceeds this in NH,SH or TR,
! set inflation factor in that region to 1.0.
spreadpsthresh = 17. 


! read namelist from file on all processors.
open(iunit_nml,file='ensrf.nml',form='formatted')
read(iunit_nml,nam_ensrf)
close(iunit_nml)

! buddy check threshold for restore same as for exclude by default.
if (ftest_thresh2 .lt. 0.) then
    ftest_thresh2=ftest_thresh
end if

if (nproc .eq. 0) then
if (obsfile .eq. " ") then
  print *,'need to specify obsfile in namelist!'
  stop
else if (datapath .eq. " ") then
  print *,'need to specify datapath in namelist!'
  stop
else if (datestring .eq. " ") then
  print *,'need to specify datestring in namelist!'
  stop
end if

! check for mandatory namelist parameters.
print *, datapath
print *, 'analysis time ',datestring
print *, nanals,' members'

if (useheight) print *,'analysis variable Z instead of Tv!'
if (postinflate) print *,'inflating both the prior AND the posterior!'

print *,'namelist parameters:'
write(6,nam_ensrf)
endif

!==> covariance filter length scale (increments go to zero at this distance)
!    convert from km to radians.

corrlengthnh = corrlengthnh * 1.e3/re
corrlengthsh = corrlengthsh * 1.e3/re
corrlengthtr = corrlengthtr * 1.e3/re

!==> initialize spherical harmonic stuff.
 
call spharmt_init(sphere_dat,nlons,nlats,ntrunc,re)
 
!==> latitudes with poles included (used in bilinear interp routine).
 
glats(1) = 0.
glats(nlats+2) = pi
do j=2,nlats+1
  glats(j) = 0.5*pi - asin(sphere_dat%gaulats(j-1))
enddo
 
!==> read in obs data (on root process).
 
if (nproc .eq. 0) then

print *,obsfile
open(149,form="formatted",file=trim(adjustl(obsfile)))
print *, filename
nobstot = 0
do 
  read(149,9801,err=199,end=199) c1
  nobstot = nobstot + 1
enddo
199 continue

do np=1,numproc-1
 call MPI_Send(nobstot,1,MPI_INTEGER,np, &
               1,MPI_COMM_WORLD,ierr)
enddo

else 

call MPI_Recv(nobstot,1,MPI_INTEGER,0, &
              1,MPI_COMM_WORLD,MPI_Status,ierr)
end if

!==> allocate some arrays for obs and obs metadata.
 
allocate(covlocalob(nobstot))
allocate(ensmean_ob(nobstot))
allocate(ensmean_obt(nobstot))
allocate(rlapse_ensmean(nobstot))
allocate(anal_ob(nobstot,nanals))
allocate(anal_obt(nobstot,nanals))
allocate(rlapse(nobstot,nanals))
allocate(zobens(nobstot,nanals))
allocate(iuseob(nobstot))
allocate(ibadob(nobstot))
allocate(nindxob(nobstot))
allocate(obtype(nobstot))
allocate(stattype(nobstot))
allocate(oblocx(nobstot))
allocate(dxob(nobstot))
allocate(dyob(nobstot))
allocate(dtob(nobstot))
allocate(oblocy(nobstot))
allocate(oberrvar(nobstot))
allocate(ob(nobstot))
allocate(oborig(nobstot))
allocate(slpob(nobstot))
allocate(bias(nobstot))
allocate(iassim(nobstot))
allocate(ifailbg(nobstot))
allocate(ifailbud(nobstot))
allocate(zob(nobstot))
allocate(zfgob(nobstot))
allocate(pob(nobstot))
allocate(obtime(nobstot))
allocate(stdev(nobstot))
allocate(stdevorig(nobstot))
allocate(obfit_prior(nobstot))
allocate(obsprd_prior(nobstot))
allocate(obfit_post(nobstot))
allocate(obsprd_post(nobstot))
allocate(statid(nobstot))
allocate(statname(nobstot))
allocate(obid(nobstot))
allocate(pfhtob(nobstot))
obfit_prior = 9.9e31
obsprd_prior = 9.9e31
obfit_post = 9.9e31
obsprd_post = 9.9e31

if (nproc .eq. 0) then

rewind(149)
nobsh = 0
do nob=1,nobstot
     read(149,9801) statid(nob),stattype(nob),obtype(nob),oblocx(nob),oblocy(nob),&
          izob,obtime(nob),ob(nob),slpob(nob),bias(nob),stdev(nob),statname(nob),obid(nob)
     oborig(nob) = ob(nob)
     stdevorig(nob) = stdev(nob)
     stdev(nob) = oberrfact*stdev(nob)
     zob(nob) = izob
9801 format(a19,1x,i3,1x,a1,1x,f7.2,1x,f6.2,1x,i5,1x,f6.2,1x,f7.1,&
            1x,f7.1,1x,e10.3,1x,f5.2,1x,a30,1x,a13)
     if (oblocy(nob) .lt. 0.) nobsh = nobsh + 1
     if (oblocx(nob) .lt. 0.) oblocx(nob) = oblocx(nob) + 360.
     iassim(nob) = 0
enddo
print *, nobstot,' total obs'
print *, nobsh,' total obs in SH'
close(149)

do np=1,numproc-1
 call MPI_Send(oblocx,nobstot,MPI_REAL,np, &
               1,MPI_COMM_WORLD,ierr)
 call MPI_Send(oblocy,nobstot,MPI_REAL,np, &
               2,MPI_COMM_WORLD,ierr)
 call MPI_Send(obtype,nobstot,MPI_CHARACTER,np, &
               3,MPI_COMM_WORLD,ierr)
 call MPI_Send(iassim,nobstot,MPI_INTEGER,np, &
               4,MPI_COMM_WORLD,ierr)
enddo

else

call MPI_Recv(oblocx,nobstot,MPI_REAL,0, &
              1,MPI_COMM_WORLD,MPI_Status,ierr)
call MPI_Recv(oblocy,nobstot,MPI_REAL,0, &
              2,MPI_COMM_WORLD,MPI_Status,ierr)
call MPI_Recv(obtype,nobstot,MPI_CHARACTER,0, &
              3,MPI_COMM_WORLD,MPI_Status,ierr)
call MPI_Recv(iassim,nobstot,MPI_INTEGER,0, &
              4,MPI_COMM_WORLD,MPI_Status,ierr)
end if

!==> convert ob location to radians, error to variance.
 
oblocx = dtor*oblocx
oblocy = dtor*oblocy

!==> setup parameters for domain decomp (by level).

ninc = (ndim-1)/(numproc-1)
if (ndim .eq. numproc) ninc=1
ndim_max = 0
if (nproc .eq. 0) print *,'nproc,n1,n2:'
do np=0,numproc-1
   n1 = np * ninc + 1
   n2 = (np+1) * ninc 
   if (np .eq. numproc-1) n2=ndim
   if (nproc .eq. 0) print *,np,n1,n2
   if (n2-n1+1 .gt. ndim_max) ndim_max=n2-n1+1
enddo
if (nproc .eq. 0) then
 print *,'ndim,ninc = ',ndim,ninc
 print *,'ndim_max = ',ndim_max
end if
!==> levels on this processor.
n1 = nproc * ninc + 1
n2 = (nproc+1) * ninc 
if (nproc .eq. numproc-1) n2=ndim

!==> allocate arrays to hold pieces of 6h forecast state vector on
!    each processor.

allocate(anal_chunk((nlons+1)*(nlats+2),ndim_max,nanals))
allocate(ensmean_chunk((nlons+1)*(nlats+2),ndim_max))
allocate(fact((nlons+1)*(nlats+2),ndim_max))

!==> read in first guess fields (root process only).
!    send pieces of 6h state vector to other processors.
 
if (nproc .eq. 0) then

ensmean = 0.0

if (nvars .lt. 6 .and. ntrac .gt. 1) then
   allocate(qnmfgmean(nmdim,nlevs,ntrac-(nvars-3)))
   qnmfgmean = 0.
end if

do nanal=1,nanals

 write(charnanal,'(i3.3)') nanal

! read just surface pressure on for ntimes times 
!  fhr = fhrmin to fhrmin+fhrinc*(ntimes-1), every fhrinc

 nt = 0
 do nfhr=int(fhrmin),int(fhrmin)+int(fhrinc)*(ntimes-1),int(fhrinc)
     nt = nt + 1
     write(c1,'(i1.1)') nfhr
     filename = trim(adjustl(datapath)) // trim(adjustl(datestring)) // "/" //trim(adjustl(fgprefix))//"_"//datestring//"_fhr"//c1//"_mem"//charnanal
     open(iunit,form="unformatted",file=filename)
     READ(iunit) LAB
     read(iunit) FHOUR,IDATEIN,SISL
     hsg(1:nlevs+1)=sisl(1:nlevs+1)
     fsg(1:nlevs)=sisl(nlevs+2:2*nlevs+1)
     READ(iunit) (ZSNM(I),I=1, NMDIM )
     READ(iunit) (PSNM(I),I=1, NMDIM )
     call spharm_pw(sphere_dat,psg,psnm,-1)
     call spharm_pw(sphere_dat,zsg,zsnm,-1)
     if (.not. use_standlapse) then
        DO K=1, NLEVS
           READ(iunit)(TEMPNM(I,K),I=1, NMDIM )
        ENDDO
        do k=1,nlevs
           call spharm_pw(sphere_dat,tempg(1,1,k),tempnm(1,k),-1)
        enddo
        call temptoz(nlons+1,nlats+2,nlevs,rgas/g,hsg,fsg,zsg,tempg,zg)
     else
        DO K=1, NLEVT2
           READ(iunit)(TEMPNM(I,K),I=1, NMDIM )
        ENDDO
        call spharm_pw(sphere_dat,tempg(1,1,nlevt2),tempnm(1,nlevt2),-1)
     end if
     close(iunit)
     psg = 10.*exp(psg)
     do j=1,nlats+2
     do i=1,nlons+1
        nx = (j-1)*(nlons+1) + i
        analps(nx,nt,nanal) = psg(i,j)
        analtemp(nx,nt,nanal) = tempg(i,j,nlevt2)
        if (.not. use_standlapse) then
           anallapse(nx,nt,nanal) = (tempg(i,j,nlevt1)-tempg(i,j,nlevt2))/&
                                    (zg(i,j,nlevt2)-zg(i,j,nlevt1))
        end if
     enddo
     enddo
 enddo

! read in everything for 6h forecast.
 filename = trim(adjustl(datapath)) // trim(adjustl(datestring)) // "/" // trim(adjustl(fgprefix))//"_"//datestring//"_fhr6_mem"//charnanal
 open(iunit,form="unformatted",file=filename)

 READ(iunit) LAB
 read(iunit) FHOUR,IDATEIN,SISL,HEADEXT
 hsg(1:nlevs+1)=sisl(1:nlevs+1)
 fsg(1:nlevs)=sisl(nlevs+2:2*nlevs+1)
 READ(iunit) (ZSNM(I),I=1, NMDIM )
 READ(iunit) (PSNM(I),I=1, NMDIM )
 DO K=1, NLEVS
 READ(iunit)(TEMPNM(I,K),I=1, NMDIM )
 ENDDO
 DO K=1, NLEVS
 READ(iunit)(DIVNM(I,K),I=1, NMDIM )
 READ(iunit)(VRTNM(I,K),I=1, NMDIM )
 ENDDO
 DO NT=1, NTRAC
 DO K=1, NLEVS
 READ(iunit)(QNM(I,K,NT),I=1, NMDIM )
 ENDDO
 ENDDO
 close(iunit)

 if (nvars .lt. 6 .and. ntrac .gt. 1) then
! save last two tracers if they are not updated.
 n = 0
 DO nt=nvars-3,ntrac-1
   n = n + 1
   qnmfgmean(:,:,n) = qnmfgmean(:,:,n) + qnm(:,:,nt+1)/float(nanals)
 enddo
 end if
 write(iunit_tmp) qnm

 
!==> get U,V,temp,z,q,ps on gaussian grid.
 
 do k=1,nlevs
    call getuv_pw(sphere_dat,vrtnm(:,k),divnm(:,k),ug(:,:,k),vg(:,:,k))
    call spharm_pw(sphere_dat,tempg(:,:,k),tempnm(:,k),-1)
    do nt=1,nvars-3
       call spharm_pw(sphere_dat,qg(:,:,k,nt),qnm(:,k,nt),-1)
    enddo
 enddo
 call spharm_pw(sphere_dat,psg,psnm,-1)
! convert temp to height.
 if (useheight) call temptoz(nlons+1,nlats+2,nlevs,rgas/g,hsg,fsg,zsg,tempg,zg)

!==> input psg is ln(ps) in centibars - convert to ps in millibars.

 psg = 10.*exp(psg)

 nn = 0
 do nvar=1,nvars
 do k=1,nlevs
 nn = nn + 1
 do j=1,nlats+2
 do i=1,nlons+1
    nx = (j-1)*(nlons+1) + i
    if (nvar .eq. 1) then
       anal(nx,nn) = ug(i,j,k)
    else if (nvar .eq. 2) then
       anal(nx,nn) = vg(i,j,k)
    else if (nvar .eq. 3) then
       if (useheight) then
       anal(nx,nn) = zg(i,j,k)
       else
       anal(nx,nn) = tempg(i,j,k)
       end if
    else
       if (qg(i,j,k,nvar-3) .lt. 1.e-8) qg(i,j,k,nvar-3)=1.e-8
       anal(nx,nn) = qg(i,j,k,nvar-3)
    end if
 enddo
 enddo
 enddo
 enddo
 nn = nn + 1
 do j=1,nlats+2
 do i=1,nlons+1
    nx = (j-1)*(nlons+1) + i
    anal(nx,nn) = psg(i,j)
 enddo
 enddo
 !print *, 'max/min values of member ',nanal,' first guess'
 !print *, 'U,V,T/Z,q,ps:'
 !do nvar=1,nvars
 !print *, minval(anal(:,(nvar-1)*nlevs+1:nvar*nlevs)),&
 !         maxval(anal(:,(nvar-1)*nlevs+1:nvar*nlevs))
 !enddo
 !print *, minval(anal(:,ndim)),&
 !         maxval(anal(:,ndim))
 ensmean = ensmean + (anal/float(nanals))

 do np=1,numproc-1
   i1 = np * ninc + 1
   i2 = (np+1) * ninc 
   if (np .eq. numproc-1) i2=ndim
   ensmean_chunk(:,1:i2-i1+1) = anal(:,i1:i2)
   call MPI_Send(ensmean_chunk,(nlons+1)*(nlats+2)*ndim_max,MPI_REAL,np, &
                 nanal,MPI_COMM_WORLD,ierr)
 enddo
 anal_chunk(:,1:ninc,nanal) = anal(:,1:ninc)

enddo
rewind(iunit_tmp)

do np=1,numproc-1
 call MPI_Send(fsg,nlevs,MPI_REAL,np, &
               nanals+1,MPI_COMM_WORLD,ierr)
 call MPI_Send(hsg,nlevs+1,MPI_REAL,np, &
               nanals+2,MPI_COMM_WORLD,ierr)
enddo

call cpu_time(t2)
print *,'time to read in first guess = ',t2-t1

else

do nanal=1,nanals
  call MPI_Recv(anal_chunk(:,:,nanal),(nlons+1)*(nlats+2)*ndim_max,MPI_REAL,0, &
                nanal,MPI_COMM_WORLD,MPI_Status,ierr)
enddo
call MPI_Recv(fsg,nlevs,MPI_REAL,0, &
              nanals+1,MPI_COMM_WORLD,MPI_Status,ierr)
call MPI_Recv(hsg,nlevs+1,MPI_REAL,0, &
              nanals+2,MPI_COMM_WORLD,MPI_Status,ierr)

end if

! compute ln(sigma levels and interfaces) on each node.
do k=1,nlevs
   lnfsg(k) = -alog(fsg(k))
enddo
do k=1,nlevs-1
   lnhsg(k) = -alog(hsg(k+1))
enddo
lnhsg(nlevs) = lnfsg(nlevs)

!==> output date.

if (nproc .eq. 0) then
   read(datestring(1:4),'(i4)') idateout(4)
   read(datestring(5:6),'(i2)') idateout(2)
   read(datestring(7:8),'(i2)') idateout(3)
   read(datestring(9:10),'(i2)') idateout(1)
   print *,' idatein = ',idatein
   print *,' idateout = ',idateout
end if
 
!==> compute ensemble of first guesses, remove mean from anal.
! full anal array only exists on root process.

do nn=1,n2-n1+1 
do nx=1,(nlons+1)*(nlats+2)
   ensmean_chunk(nx,nn) = sum(anal_chunk(nx,nn,:))/float(nanals)
enddo
enddo
do nanal=1,nanals
   anal_chunk(:,:,nanal) = anal_chunk(:,:,nanal)-ensmean_chunk(:,:)
enddo

if (nproc .eq. 0) then
do nt=1,ntimes
do nx=1,(nlons+1)*(nlats+2)
   ensmeanps(nx,nt) = sum(analps(nx,nt,:))/float(nanals)
   ensmeantemp(nx,nt) = sum(analtemp(nx,nt,:))/float(nanals)
enddo
enddo

print *, 'max/min values of ensemble mean first guess'
print *, 'U,V,T/Z,q,ps:'
do nvar=1,nvars
print *, minval(ensmean(:,(nvar-1)*nlevs+1:nvar*nlevs)),&
         maxval(ensmean(:,(nvar-1)*nlevs+1:nvar*nlevs))
enddo
print *, minval(ensmean(:,ndim)),&
         maxval(ensmean(:,ndim))

! just use standard atmospheric lapse rate, with some uncertainity.
if (use_standlapse) then
  do nanal=1,nanals
    anallapse(:,:,nanal) = standlapse + standlapse_err*rnorm()
  enddo
end if
! remove mean from ensemble.
do nanal=1,nanals
   analps(:,:,nanal) = analps(:,:,nanal)-ensmeanps(:,:)
   analtemp(:,:,nanal) = analtemp(:,:,nanal)-ensmeantemp(:,:)
   anallapse(:,:,nanal) = anallapse(:,:,nanal)-ensmeanlapse(:,:)
enddo
! check spread of surface pressure.
do nx=1,(nlons+1)*(nlats+2)
   analps_stdev(nx) = sqrt(sum(analps(nx,ntimes,:)**2)/float(nanals-1))
enddo
print *,'min/max of ps spread = ',minval(analps_stdev),maxval(analps_stdev)
! if spread of ps at nt=ntimes exceeds threshold in NH,SH or TR
! set inflation to 1.0 in that region and print a warning.
do j=1,nlats+2
deglat = rtod*(0.5*pi-glats(j))
do i=1,nlons+1
   ij = (j-1)*(nlons+1) + i
   if (deglat .lt. -latbound .and. analps_stdev(ij) .gt. spreadpsthresh) then
      if (covinflatesh .gt. 1.0) print *,'WARNING: spread in SH exceeds threshold of',spreadpsthresh,' hPa, setting covinflatesh to 1.0'
      covinflatesh = 1.0
   else if (deglat .gt. latbound .and. analps_stdev(ij) .gt. spreadpsthresh) then
      if (covinflatenh .gt. 1.0) print *,'WARNING: spread in NH exceeds threshold of',spreadpsthresh,' hPa, setting covinflatenh to 1.0'
      covinflatenh = 1.0
   else if (deglat .lt. latbound .and. deglat .gt. -latbound .and. analps_stdev(ij) .gt. spreadpsthresh) then
      if (covinflatetr .gt. 1.0) print *,'WARNING: spread in TR exceeds threshold of',spreadpsthresh,' hPa, setting covinflatetr to 1.0'
      covinflatetr = 1.0
   end if
enddo
enddo
! propagate the modified inflation factors to the other processors.
do np=1,numproc-1
 call MPI_Send(covinflatenh,1,MPI_REAL,np, &
               1,MPI_COMM_WORLD,ierr)
 call MPI_Send(covinflatetr,1,MPI_REAL,np, &
               2,MPI_COMM_WORLD,ierr)
 call MPI_Send(covinflatesh,1,MPI_REAL,np, &
               3,MPI_COMM_WORLD,ierr)
enddo
else
call MPI_Recv(covinflatenh,1,MPI_REAL,0, &
              1,MPI_COMM_WORLD,MPI_Status,ierr)
call MPI_Recv(covinflatetr,1,MPI_REAL,0, &
              2,MPI_COMM_WORLD,MPI_Status,ierr)
call MPI_Recv(covinflatesh,1,MPI_REAL,0, &
              3,MPI_COMM_WORLD,MPI_Status,ierr)
end if

!==> get horizontal ob locations in grid coordinates.
do nob=1,nobstot
! longitudes are evenly spaced
 dxob(nob) = (0.5*float(nlons)*oblocx(nob)/pi)+1.
! gaussian latitudes are not.
 j=1
 dyob(nob) = 1.
 do 
   if (glats(j) .ge. 0.5*pi-oblocy(nob)) exit
   j = j + 1
 enddo 
 if (j .gt. 1) dyob(nob) = float(j-1) + (0.5*pi-oblocy(nob)-glats(j-1))/(glats(j)-glats(j-1))
enddo
! get ob locations in time coordinates. 
do nob=1,nobstot
   dtob(nob) = 1.+((fhrmin+obtime(nob))/fhrinc)
enddo

!==> precompute lats/lons of analysis grids.
 
do j=1,nlats+2
rlat = 0.5*pi-glats(j)
do i=1,nlons+1
   ij = (j-1)*(nlons+1) + i
   rlon = 2.*pi*float(i-1)/float(nlons)
   lons(ij) = rlon
   lats(ij) = rlat
enddo
enddo

!==> prior inflation.
!    varies from covinflatesh in SH 
!    to covinflatetr in tropics to covinflatenh in NH.
!    vertical structure given by lnsigcutoffnh,sh,tr.

hgt = 0.
nn = 0
do nvar=1,nvars
do k=1,nlevs
  nn = nn + 1
  if (useheight .and. nvar .eq. 3) then
       hgt(nn) = lnhsg(k)
  else
       hgt(nn) = lnfsg(k)
  end if
enddo
enddo

do nn=1,ndim
do j=1,nlats+2
deglat = rtod*(0.5*pi-glats(j))
do i=1,nlons+1
   ij = (j-1)*(nlons+1) + i
   if (deglat .gt. latbound+0.5*delat) then
     covinf = covinflatenh
     lnsigl = lnsigcutoffnh
   else if (deglat .le. latbound+0.5*delat .and. deglat .ge. latbound-0.5*delat) then
     covinf = (((latbound+0.5*delat)-deglat)/delat)*covinflatetr + ((deglat-(latbound-0.5*delat))/delat)*covinflatenh
     lnsigl = (((latbound+0.5*delat)-deglat)/delat)*lnsigcutofftr + ((deglat-(latbound-0.5*delat))/delat)*lnsigcutoffnh
   else if (deglat .lt. latbound-0.5*delat .and. deglat .gt. -latbound+0.5*delat) then
     covinf = covinflatetr
     lnsigl = lnsigcutofftr
   else if (deglat .le. -latbound+0.5*delat .and. deglat .ge. -latbound-0.5*delat) then
     covinf = (((latbound+0.5*delat)+deglat)/delat)*covinflatetr + ((-deglat-(latbound-0.5*delat))/delat)*covinflatesh
     lnsigl = (((latbound+0.5*delat)+deglat)/delat)*lnsigcutofftr + ((-deglat-(latbound-0.5*delat))/delat)*lnsigcutoffsh
   else
     covinf = covinflatesh
     lnsigl = lnsigcutoffsh
   end if
   covinflate(ij,nn) = (1.+corr_fn(hgt(nn), lnsigl)*(covinf-1.))
   !if (nproc .eq. 0 .and. i .eq. 1) print *,j,nn,deglat,hgt(nn),covinf,covinflate(ij,nn)
enddo
enddo
enddo
do nanal=1,nanals
  anal_chunk(:,1:n2-n1+1,nanal) = covinflate(:,n1:n2)*anal_chunk(:,1:n2-n1+1,nanal)
enddo

if (nproc .eq. 0) then

! inflate ensemble of background ps and 'just above PBL' temp.
! (these are used in forward operator).
do nanal=1,nanals
do nt=1,ntimes
   analps(:,nt,nanal) = covinflate(:,ndim)*analps(:,nt,nanal)
   analtemp(:,nt,nanal) = covinflate(:,ndim)*analtemp(:,nt,nanal)
   if (.not. use_standlapse) then
      anallapse(:,nt,nanal) = covinflate(:,ndim)*anallapse(:,nt,nanal)
   end if
enddo
enddo

! make an ensemble of station elevations using assumed error (zerr).
do nob=1,nobstot
do nanal=1,nanals
   ! don't perturb station height for SLP obs (including ships).
   if (stattype(nob) .ne. 183 .and. stattype(nob) .ne. 180) then
      zobens(nob,nanal) = zob(nob) + zerr*rnorm() ! gaussian random deviate
   else
      zobens(nob,nanal) = zob(nob)
   end if
enddo
enddo
! make sure mean of ensemble centered on zob.
do nob=1,nobstot
   pfhtob(nob) = sum(zobens(nob,:))/float(nanals)
enddo
do nanal=1,nanals
   zobens(:,nanal) = zobens(:,nanal) - pfhtob + zob
enddo

!==> interpolate first guess to ob locations.
!==> calculate fit of first guess to obs.

do nob=1,nobstot
 do nanal=1,nanals
    ensmeantmp = analtemp(:,:,nanal) + ensmeantemp
    call lintrp3(ensmeantmp,anal_obt(nob,nanal),&
                 dxob(nob),dyob(nob),dtob(nob),nlons+1,nlats+2,ntimes)
    ensmeantmp = anallapse(:,:,nanal) + ensmeanlapse
    call lintrp3(ensmeantmp,rlapse(nob,nanal),&
                 dxob(nob),dyob(nob),dtob(nob),nlons+1,nlats+2,ntimes)
 enddo
enddo
do nob=1,nobstot
   ensmean_obt(nob) = sum(anal_obt(nob,:))/float(nanals)
   rlapse_ensmean(nob) = sum(rlapse(nob,:))/float(nanals)
enddo

call cpu_time(t1)
 
oberrvar = (stdev)**2

nobsps_nh = 0
nobsps_sh = 0
nobsps_tr = 0

iuseob = 0
ibadob = 0
ifailbg = 0
ifailbud = 0

nn = 0
do nob=1,nobstot

! surface pressure only, 
! within +/- 3h of analysis time.
! if this test fails, iuseob=0, but all the other qc flags will be zero.

 call lintrp2(zsg,zfgob(nob),&
              dxob(nob),dyob(nob),nlons+1,nlats+2)
 zdiff = zob(nob)-zfgob(nob)
 if (obtype(nob) .eq. 'P' .and. (obtime(nob) .ge. -3. .and. &
     obtime(nob) .le. 3.) .and. abs(zdiff) .lt. zthresh) then
     iuseob(nob) = 1
 else
     nn = nn + 1
 end if

enddo
if (nn .ne. 0) print *,nn,' obs outside specified time window'

! do gross check (see if altimeter setting is sane).
! if gross check fails, iuseob=0,ibadob=1 but ifailbg=0 and ifailbud=0
nn = 0
do nob=1,nobstot
! catch crazy values that might cause NaNs
! (and mess up formatting of output text file)
 if (iuseob(nob) .eq. 1 .and. ob(nob) .lt. 0.) then
   oborig(nob) = 9999.9
   ob(nob) = 9999.9
   slpob(nob) = 9999.9
   stdevorig(nob) = 99.99
   stdev(nob) = 99.99
   iuseob(nob)=0
   ibadob(nob)=1
   nn = nn + 1
 end if
 if (iuseob(nob) .eq. 1) then
    altob = palt(ob(nob),zob(nob))
    if (altob .lt. 880. .or. altob .gt. 1060.) then
      iuseob(nob)=0
      ibadob(nob)=1
      nn = nn + 1
    end if   
    !print *,ob(nob),altob,zob(nob),ibadob(nob)
 end if
enddo
print *,nn,' obs failed sanity check'

! find fit of prior to obs (obfit_prior)
! adjust obs to model orography, adjusting ob error.
do nob=1,nobstot
 ! perturb obs with ob error, making sure mean is unchanged.
 ob_mean = 0.
 do nanal=1,nanals
    ob_new(nanal) = ob(nob) + rnorm()*stdev(nob)
    ob_mean = ob_mean + ob_new(nanal)/float(nanals)
 enddo
 ob_new = ob_new - ob_mean + ob(nob)
 do nanal=1,nanals
    ensmeantmp = analps(:,:,nanal) + ensmeanps
    call lintrp3(ensmeantmp,anal_ob(nob,nanal),&
                 dxob(nob),dyob(nob),dtob(nob),nlons+1,nlats+2,ntimes)
! use a different station elevation, temperature, and lapse rate
! for each member when adjusting obs.
    ob_new(nanal) = &
    preduce(ob_new(nanal),anal_ob(nob,nanal)*fsg(nlevt2),anal_obt(nob,nanal),zfgob(nob),zobens(nob,nanal),rgas,g,rlapse(nob,nanal))
 enddo
 ensmean_ob(nob) = sum(anal_ob(nob,:))/float(nanals)
! adjust mean ob.
 ob(nob) = &
 preduce(ob(nob),ensmean_ob(nob)*fsg(nlevt2),ensmean_obt(nob),zfgob(nob),zob(nob),rgas,g,rlapse_ensmean(nob))
! spread of adjusted obs is new ob error.
 ob_mean = sum(ob_new)/float(nanals)
 oberrnew = sum((ob_new-ob_mean)**2)/float(nanals-1)
! make sure ob error not reduced.
 if (oberrnew .gt. oberrvar(nob)) then
   oberrvar(nob) = oberrnew
   stdev(nob) = sqrt(oberrvar(nob))
 end if
 !print *,ensmean_obt(nob),rlapse_ensmean(nob)
 !print *,trim(adjustl(statid(nob))),oborig(nob),ob(nob),zob(nob),zfgob(nob),stdevorig(nob),stdev(nob)
enddo
do nob=1,nobstot
   ensmean_obt(nob) = sum(anal_obt(nob,:))/float(nanals)
enddo
do nanal=1,nanals
   anal_ob(:,nanal) = anal_ob(:,nanal) - ensmean_ob
enddo
do nob=1,nobstot
   obfit_prior(nob) = ensmean_ob(nob)-ob(nob)
   obsprd_prior(nob) = sum(anal_ob(nob,:)**2)/float(nanals-1)
enddo

! do background QC check.
ifailbg=0
do nob=1,nobstot
 if (iuseob(nob) .eq. 1  .and. &
    abs(obfit_prior(nob)) .gt. &
    sprd_tol*sqrt(obsprd_prior(nob)+oberrvar(nob))) then
    iuseob(nob)=0
    ibadob(nob)=1
    ifailbg(nob)=1
 end if
enddo
print *,count(ifailbg .gt. 0),' obs failed background check'

! check to see whether how fit of mean to nearby obs is affected
! assimilating each ob in turn.
! this currently works independently of background check (i.e.
! it does not try to restore obs rejected by background check).

if (ftest_thresh .gt. 1.e-5) then
do niter=1,buddy_iterations ! iterate buddy check.
nfail = 0 ! counter for obs that fail buddy check,
nn = 0 ! counter for obs that failed bg check, but brought back by buddy check.
corrl = ftest_dist * 1.e3/re
psmeancheck = ensmeanps
do nob=1,nobstot

! just check those obs we've decided to keep (don't check
! those that have failed background check).
!if (iuseob(nob) .eq. 1) then
! just check those obs we've decided to keep, plus those that have
! failed background check (and haven't failed buddy check).
if (iuseob(nob) .eq. 1 .or. (ifailbg(nob) .eq. 1 .and. ifailbud(nob) .eq. 0)) then

 pfht_ob = sum(anal_ob(nob,:)**2)/float(nanals-1)

!==> find pts where corr_fn > 0 for this ob location,
!    compute covariance localization function.

 hcovlocal = 0.
 do ij=1,(nlons+1)*(nlats+2)
    r = dist(oblocx(nob),lons(ij),oblocy(nob),lats(ij))
    if (r .lt. corrl) hcovlocal(ij) = corr_fn(r,corrl)
 enddo

! find indices for 'close' analysis points.
 ntot = count(hcovlocal .gt. 0.)
 nindx = 0
 npt = 0
 do ij=1,(nlons+1)*(nlats+2)
    if (hcovlocal(ij) .gt. 0) then
       npt = npt + 1
       nindx(npt) = ij
    end if
 enddo


 hcovlocal = 0.
 deglat = oblocy(nob)*rtod
 if (deglat .gt. latbound+0.5*delat) then
   corrl2 = corrlengthnh
 else if (deglat .le. latbound+0.5*delat .and. deglat .ge. latbound-0.5*delat) then
   corrl2 = (((latbound+0.5*delat)-deglat)/delat)*corrlengthtr + ((deglat-(latbound-0.5*delat))/delat)*corrlengthnh
 else if (deglat .lt. latbound-0.5*delat .and. deglat .gt. -latbound+0.5*delat) then
   corrl2 = corrlengthtr
 else if (deglat .le. -latbound+0.5*delat .and. deglat .ge. -latbound-0.5*delat) then
   corrl2 = (((latbound+0.5*delat)+deglat)/delat)*corrlengthtr + ((-deglat-(latbound-0.5*delat))/delat)*corrlengthsh
 else
   corrl2 = corrlengthsh
 end if
 do ij=1,(nlons+1)*(nlats+2)
    r = dist(oblocx(nob),lons(ij),oblocy(nob),lats(ij))
    if (r .lt. corrl2) hcovlocal(ij) = corr_fn(r,corrl2)
 enddo

! factor to multiply gain for update of perturbations.
 gainfact = ((pfht_ob+oberrvar(nob))/pfht_ob)*&
            (1.-sqrt(oberrvar(nob)/(pfht_ob+oberrvar(nob))))
! loop over points that are closer than corrlength to ob.
 do nt=1,ntimes
 do npt=1,ntot
! compute gain.
    pfht = sum(analps(nindx(npt),nt,:)*anal_ob(nob,:))/float(nanals-1)
! 'fact' is covariance localization.
    kfgain = hcovlocal(nindx(npt))*pfht/(pfht_ob+oberrvar(nob))
! update mean
    psmeancheck(nindx(npt),nt) = &
    ensmeanps(nindx(npt),nt) + kfgain*(ob(nob)-ensmean_ob(nob))
 enddo
 enddo

! see how fit to other obs close to this ob is changed.
! Use F-Test to decide if this ob is doing more harm than good
! (variance of fits for surrounding obs increased).
! This is a 'buddy check' QC.

 ncount_tot = 0
 suma = 0.
 sumb = 0.
 sumao = 0.
 sumbo = 0.
 loopobs: do nob2=1,nobstot
! exclude all obs that we have decided not to use already
! (including those that have failed background check).
    if (iuseob(nob2) .eq. 0 .or. nob2 .eq. nob) cycle loopobs
! look at fits to obs that have failed background check.
!   if ((iuseob(nob2) .eq. 0 .and. ifailbg(nob2) .eq. 0) .or. nob2 .eq. nob) cycle loopobs
    r = dist(oblocx(nob),oblocx(nob2),oblocy(nob),oblocy(nob2))
    if (r .ge. corrl) cycle loopobs
    ncount_tot = ncount_tot + 1
    call lintrp3(psmeancheck,ensmean_ob2,&
                 dxob(nob2),dyob(nob2),dtob(nob2),nlons+1,nlats+2,ntimes)
    sumao = sumao + (ensmean_ob2-ob(nob2))**2
    sumbo = sumbo + (obfit_prior(nob2))**2
 end do loopobs
 if (ncount_tot .gt. 1) then
     df = ncount_tot-1
     ! can't have more degrees of freedom than ensemble members.
     if (df .gt. nanals) df = nanals 
     f = sumao/sumbo
     if (f .gt. 1.) then
         ! prob that f significantly greater than 1.
         ! smaller value means more likely that the
         ! fit to buddies will be degraded if ob used.
         prob = 2.*betai(0.5*df,0.5*df,1./(1.+f))
         if (prob .gt. 1.) prob=2.-prob
     else
         prob = 1.
     end if
     ! toss obs that significatly degrade fit to buddies.
     if (prob .le. ftest_thresh) then ! test fails
       iuseob(nob)=0
       ibadob(nob)=1
       ifailbud(nob)=1
       nfail = nfail + 1
     end if
     ! see if obs that failed background check can be 'redeemed'
     ! (if they significantly improve fit to buddies).
     ! only let land stations be redeemed if bias correction has been applied.
     biascorrected = bias(nob) .lt. 1.e20
     if (stattype(nob) .eq. 180) biascorrected = .true. ! ships can always be redeemed
     if (f .lt. 1. .and. (biascorrected .and. iuseob(nob) .eq. 0 .and. ifailbg(nob) .eq. 1)) then ! bring obs back that failed bg check
       ! prob that f significantly less than 1.
       ! smaller value means more likely that the
       ! fit to buddies will be improved if ob used.
       prob = 2.*betai(0.5*df,0.5*df,1./(1.+(1./f)))
       if (prob .gt. 1.) prob=2.-prob
       if (prob .le. ftest_thresh2) then
         iuseob(nob)=1
         ibadob(nob)=0
         !print *,stattype(nob),int(zob(nob)),rtod*oblocx(nob),rtod*oblocy(nob),ob(nob),ensmean_ob(nob)
         nn = nn + 1
       end if
     end if
 end if

 end if

enddo
print *,nn,' obs brought back by buddy check iteration',niter
print *,nfail,' obs failed buddy check iteration',niter
enddo ! end buddy check iteration
end if
print *,count(ifailbud .gt. 0),'total obs excluded by buddy check'
print *,count(ifailbud-ifailbg .eq. 1 .and. ibadob .eq. 1),'obs excluded because they failed buddy but passed original background check'
print *,count(ifailbg-ifailbud .eq. 1 .and. ibadob .eq. 1),'obs excluded because they failed original background and were not restored by buddy check'

print *, count(iuseob .gt. 0),' observations'
print *, count(ibadob .gt. 0),' obs failed QC checks'
print *,'innovation statistics for prior (using mean):'
! last argument = 1 means use median (0 for mean).
call innovstats(obfit_prior,obsprd_prior,oberrvar,oblocy,iuseob,nobstot,'ps',0)
print *,'innovation statistics for prior (using median):'
!call innovstats(obfit_prior,obsprd_prior,oberrvar,oblocy,iuseob,nobstot,'ps',1)
call innovstats2(obfit_prior,anal_ob,oberrvar,oblocy,iuseob,nanals,nobstot,'ps')
! write out Hx's for prior.
if (obsfileout_ens .ne. " ") then
print *,obsfileout_ens
open(159,form="formatted",file=trim(adjustl(obsfileout_ens)))
do nob=1,nobstot
   if (obtype(nob).eq.'P' .and. obtime(nob) .ge. -3. .and. &
       obtime(nob) .le. 3.) then
       write(159,9803) statid(nob),(anal_ob(nob,nanal),nanal=1,nanals)
    end if
enddo
! works up to an ensemble size of 100
9803  format(a19,100(1x,e10.3))
close(159)
end if

call cpu_time(t2)
print *,'time to compute hx and perform qc = ',t2-t1

do np=1,numproc-1
 call MPI_Send(iuseob,nobstot,MPI_INTEGER,np, &
               1,MPI_COMM_WORLD,ierr)
 call MPI_Send(ob,nobstot,MPI_REAL,np, &
               2,MPI_COMM_WORLD,ierr)
 call MPI_Send(oberrvar,nobstot,MPI_REAL,np, &
               3,MPI_COMM_WORLD,ierr)
 call MPI_Send(ensmean_ob,nobstot,MPI_REAL,np, &
               4,MPI_COMM_WORLD,ierr)
 call MPI_Send(anal_ob,nanals*nobstot,MPI_REAL,np, &
               5,MPI_COMM_WORLD,ierr)
enddo

else

call MPI_Recv(iuseob,nobstot,MPI_INTEGER,0, &
              1,MPI_COMM_WORLD,MPI_Status,ierr)
call MPI_Recv(ob,nobstot,MPI_REAL,0, &
              2,MPI_COMM_WORLD,MPI_Status,ierr)
call MPI_Recv(oberrvar,nobstot,MPI_REAL,0, &
              3,MPI_COMM_WORLD,MPI_Status,ierr)
call MPI_Recv(ensmean_ob,nobstot,MPI_REAL,0, &
              4,MPI_COMM_WORLD,MPI_Status,ierr)
call MPI_Recv(anal_ob,nanals*nobstot,MPI_REAL,0, &
              5,MPI_COMM_WORLD,MPI_Status,ierr)
end if

! pre-compute HPbHT before loop over observations.
! It will be updated in the loop over obs.
do nob=1,nobstot
    pfhtob(nob) = sum(anal_ob(nob,:)**2)/float(nanals-1)
enddo

!==> process obs one by one.

tbegin = MPI_Wtime()
nobsuse = 0
corrlav = 0.
lnsiglav = 0.
corrlmin = 9.9e31
lnsiglmin = 9.9e31
corrlmax = -9.9e31
lnsiglmax = -9.9e31

obsloop: do nob2=1,nobstot

       ! assimilate the obs such that they are in order of largest 
       ! posterior variance reduction.
       ! find ob that will produce largest variance reduction.
       fmin = 99.99
       nobmin = 0
       do nob=1,nobstot
        ! only process obs that are still candidates to be assimilated.
        ! iassim=1 means the ob was already assimilated
        ! iuseob=1 means ob passed the QC and the gross checks.
        if (iuseob(nob) .eq. 1 .and. iassim(nob) .eq. 0) then
          ! HPaHT/HPbHT
          f = oberrvar(nob)/(pfhtob(nob)+oberrvar(nob))
          if (f .lt. fmin) then
             fmin = f
             nobmin = nob
          end if
        end if
      enddo
      if (nobmin .eq. 0) exit obsloop ! done
      nob = nobmin ! ob with largest variance reduction that is a candidate

      ! HPaHT/HPbHT

      f = oberrvar(nob)/(pfhtob(nob)+oberrvar(nob))

      ! is HPaHT/HPbHT significant with nanals ensemble members? if not, skip it.
      ! (one sided f-test, since we know f = HPaHT/HPbHT < 1).
      ! sigtest_zscore is a probability.
      df = nanals-1
      prob = betai(0.5*df,0.5*df,1./(1.+f))
      if (prob .gt. 1.) prob=1.-prob
      if (prob .le. sigtest_zscore) cycle obsloop

      !==> find pts where corr_fn > 0 for this ob location,
      !    compute covariance localization function.

      hcovlocal = 0.
      deglat = oblocy(nob)*rtod
      lnsigl = 9.9e31
      if (deglat .gt. latbound+0.5*delat) then
         corrl = corrlengthnh
         if (lnsigcutoffnh .lt. 1.e3) lnsigl = lnsigcutoffnh
      else if (deglat .le. latbound+0.5*delat .and. deglat .ge. latbound-0.5*delat) then
         corrl = (((latbound+0.5*delat)-deglat)/delat)*corrlengthtr + ((deglat-(latbound-0.5*delat))/delat)*corrlengthnh
         if (lnsigcutoffnh .lt. 1.e3 .and. lnsigcutofftr .lt. 1.e3) &
            lnsigl = (((latbound+0.5*delat)-deglat)/delat)*lnsigcutofftr + ((deglat-(latbound-0.5*delat))/delat)*lnsigcutoffnh
      else if (deglat .lt. latbound-0.5*delat .and. deglat .gt. -latbound+0.5*delat) then
         corrl = corrlengthtr
         if (lnsigcutofftr .lt. 1.e3) lnsigl = lnsigcutofftr
      else if (deglat .le. -latbound+0.5*delat .and. deglat .ge. -latbound-0.5*delat) then
         corrl = (((latbound+0.5*delat)+deglat)/delat)*corrlengthtr + ((-deglat-(latbound-0.5*delat))/delat)*corrlengthsh
         if (lnsigcutoffsh .lt. 1.e3 .and. lnsigcutofftr .lt. 1.e3) &
             lnsigl = (((latbound+0.5*delat)+deglat)/delat)*lnsigcutofftr + ((-deglat-(latbound-0.5*delat))/delat)*lnsigcutoffsh
      else
         corrl = corrlengthsh
         if (lnsigcutoffsh .lt. 1.e3) lnsigl = lnsigcutoffsh
      end if

      ! make corrl proportional to f=HPaHT/HPbHT
      ! when f = 0, double it
      if (adaptive_local) then
      corrl = (2. - f)*corrl
      lnsigl = (2. - f)*lnsigl
      ! keep track of average values
      corrlav = corrlav + corrl
      if (corrl .lt. corrlmin) corrlmin = corrl
      if (corrl .gt. corrlmax) corrlmax = corrl
      lnsiglav = lnsiglav + lnsigl
      if (lnsigl .lt. lnsiglmin) lnsiglmin = lnsigl
      if (lnsigl .gt. lnsiglmax) lnsiglmax = lnsigl
      end if
      ! total number of obs assimilated.
      nobsuse = nobsuse+1

      do ij=1,(nlons+1)*(nlats+2)
         r = dist(oblocx(nob),lons(ij),oblocy(nob),lats(ij))
         if (r .lt. corrl) hcovlocal(ij) = corr_fn(r,corrl)
      enddo

      !==> vertical localization for this ob.
      vfact = 1.0
      if (lnsigl .lt. 1.e3) then
         do nn=n1,n2
            vfact(nn) = corr_fn(hgt(nn), lnsigl)
         enddo
      end if

      ! combine horizontal and vertical localization.
      nnx = 0
      do nn=n1,n2
         nnx = nnx + 1
         fact(:,nnx) = vfact(nn)*hcovlocal(:)
      enddo

      ! find indices for 'close' (within localization radius)
      ! analysis points. There are ntot such points.
      ntot = 0
      do ij=1,(nlons+1)*(nlats+2)
         if (hcovlocal(ij) .gt. 0) then
            ntot = ntot + 1
            nindx(ntot) = ij
         end if
      enddo

      !  factor to multiply gain for update of perturbations.
      gainfact = ((pfhtob(nob)+oberrvar(nob))/pfhtob(nob))*&
                 (1.-sqrt(oberrvar(nob)/(pfhtob(nob)+oberrvar(nob))))
      ! loop over points that are closer than corrlength to ob.
      do nn=1,n2-n1+1
         do npt=1,ntot
            ! compute gain.
            pfht = sum(anal_chunk(nindx(npt),nn,:)*anal_ob(nob,:))/float(nanals-1)
            ! 'fact' is covariance localization.
            kfgain = fact(nindx(npt),nn)*pfht/(pfhtob(nob)+oberrvar(nob))
            ! update mean
            ensmean_chunk(nindx(npt),nn) = &
            ensmean_chunk(nindx(npt),nn) + kfgain*(ob(nob)-ensmean_ob(nob))
            ! update perturbations.
            anal_chunk(nindx(npt),nn,:) = &
            anal_chunk(nindx(npt),nn,:) - kfgain*gainfact*anal_ob(nob,:)
         enddo
      enddo

      ! localization in observation space.
      covlocalob = 0.
      iassim(nob) = 1
      do nobx=1,nobstot
         ! if you've already assimilated the ob, don't need the prior
         ! at that ob location anymore.
         ! ignore ob if you've skipped it or you're not going to assimilate it.
         ! (saves CPU time).
         !if (iuseob(nobx) .eq. 1 .and. iassim(nobx) .eq. 1) then
         ! process all ob priors (at least those that you intend to assimilate)
         ! so that you end up with useful posterior statistics.
         if (iuseob(nobx) .eq. 1) then
            r = dist(oblocx(nobx),oblocx(nob),oblocy(nobx),oblocy(nob))
            if (r .lt. corrl) covlocalob(nobx)=corr_fn(r,corrl)
         end if
      enddo

      ! nindxob are indices of 'close' (within localization radius) obs.
      ! there are nobsclose total.

      nobsclose = 0
      do nobx=1,nobstot
      if (covlocalob(nobx) .gt. 0) then
         nobsclose = nobsclose + 1
         nindxob(nobsclose) = nobx
      end if
      enddo

      ! loop over obs that are 'close' to ob.
      ! update mean, pertubation at ob location.
      ! (only need to update observation priors for obs that
      !  have not yet been assimilated, since already assimilated ob 
      !  priors not used anymore)
      anal_ob2(:) = anal_ob(nob,:)
      ensmean_ob2 = ensmean_ob(nob)
      pfht_ob = pfhtob(nob)
      do nobx=1,nobsclose
         ! compute gain.
         pfht = sum(anal_ob(nindxob(nobx),:)*anal_ob2(:))/float(nanals-1)
         ! 'covlocalob' is covariance localization.
         kfgain = covlocalob(nindxob(nobx))*pfht/(pfht_ob+oberrvar(nob))
         ensmean_ob(nindxob(nobx)) = &
              ensmean_ob(nindxob(nobx)) + kfgain*(ob(nob)-ensmean_ob2)
         anal_ob(nindxob(nobx),:) = &
              anal_ob(nindxob(nobx),:) - kfgain*gainfact*anal_ob2(:)
         ! compute updated variance at ob location.
         pfhtob(nindxob(nobx)) = &
             sum(anal_ob(nindxob(nobx),:)**2)/float(nanals-1)
      enddo

   !==> process next ob.

end do obsloop

tend = MPI_Wtime()
!print *,'time to process all obs (process ',nproc,') = ',tend-tbegin,' secs'
if (nproc .eq. 0) then
 tav = (tend-tbegin)/float(numproc)
 tmin = tav
 tmax = tav
 do np=1,numproc-1
    call MPI_Recv(tn,1,MPI_REAL8,np, &
                  1,MPI_COMM_WORLD,MPI_Status,ierr)
    tav = tav + (tn/float(numproc))
    if (tn .gt. tmax) tmax=tn
    if (tn .lt. tmax) tmin=tn
 enddo
else
  call MPI_Send(tend-tbegin,1,MPI_REAL8,0, &
                 1,MPI_COMM_WORLD,ierr)
end if
if (nproc .eq. 0) then
   print *,count(iassim .eq. 0 .and. iuseob .eq. 1),' observations skipped because increment failed significance test'
   print *,'average/max/min time to process all obs = ',tav,tmax,tmin,' secs'
   print *,nobsuse,count(iassim .gt. 0),' observations used'
   if (adaptive_local) then
   print *,'average corrl, lnsigl = ',re*corrlav/(1.e3*float(nobsuse)),lnsiglav/float(nobsuse)
   print *,'min/max corrl = ',re*corrlmin/1.e3, re*corrlmax/1.e3
   print *,'min/max lnsigl = ',lnsiglmin,lnsiglmax
   end if
end if

! posterior inflation.
if (postinflate) then
   do nanal=1,nanals
     anal_chunk(:,1:n2-n1+1,nanal) = covinflate(:,n1:n2)*anal_chunk(:,1:n2-n1+1,nanal)
   enddo
end if

! collect updated ensemble mean on root processor.
if (nproc .eq. 0) then
  ensmean(:,1:ninc) = ensmean_chunk(:,1:ninc)
  do np=1,numproc-1
    i1 = np * ninc + 1
    i2 = (np+1) * ninc 
    if (np .eq. numproc-1) i2=ndim
    call MPI_Recv(ensmean_chunk,(nlons+1)*(nlats+2)*ndim_max,MPI_REAL,np, &
                  1,MPI_COMM_WORLD,MPI_Status,ierr)
    ensmean(:,i1:i2) = ensmean_chunk(:,1:i2-i1+1)
  enddo
else
  call MPI_Send(ensmean_chunk,(nlons+1)*(nlats+2)*ndim_max,MPI_REAL,0, &
                 1,MPI_COMM_WORLD,ierr)
end if

!==> write out ensemble mean analysis.
! all the rest of the code is executed only by the root process.
! send perturbations back to root process
if (nproc .ne. 0) then
  do nanal=1,nanals
      call MPI_Send(anal_chunk(:,:,nanal),(nlons+1)*(nlats+2)*ndim_max,MPI_REAL,0, &
                    nanal,MPI_COMM_WORLD,ierr)
  enddo
else

call cpu_time(t1)

nn = 0
do nvar=1,nvars
do k=1,nlevs
nn = nn + 1
do j=1,nlats+2
do i=1,nlons+1
   nx = (j-1)*(nlons+1) + i
   if (nvar .eq. 1) then
      ug(i,j,k) = ensmean(nx,nn)
   else if (nvar .eq. 2) then
      vg(i,j,k) = ensmean(nx,nn) 
   else if (nvar .eq. 3) then
      if (useheight) then
        zg(i,j,k) = ensmean(nx,nn)
      else
        tempg(i,j,k) = ensmean(nx,nn) 
      end if
   else
      qg(i,j,k,nvar-3) = ensmean(nx,nn) 
      if (qg(i,j,k,nvar-3) .lt. 1.e-8) qg(i,j,k,nvar-3)=1.e-8
   end if
enddo
enddo
enddo
enddo
nn = nn + 1
do j=1,nlats+2
do i=1,nlons+1
   nx = (j-1)*(nlons+1) + i
   !print *,i,j,nx,ensmean(nx,nn)
   psg(i,j) = alog(ensmean(nx,nn)/10.)
enddo
enddo
! convert height to temp.
if (useheight) call ztotemp(nlons+1,nlats+2,nlevs,rgas/g,hsg,fsg,zsg,tempg,zg)
do k=1,nlevs
   call getvrtdiv_pw(sphere_dat,vrtnm(:,k),divnm(:,k),ug(:,:,k),vg(:,:,k))
   call spharm_pw(sphere_dat,tempg(:,:,k),tempnm(:,k),1)
   do nt=1,nvars-3
      call spharm_pw(sphere_dat,qg(:,:,k,nt),qnm(:,k,nt),1)
   enddo
enddo
if (nvars .lt. 6 .and. ntrac .gt. 1) then
n = 0
do nt=nvars-3,ntrac-1
   n = n + 1
   qnm(:,:,nt+1) = qnmfgmean(:,:,n)
enddo
end if
call spharm_pw(sphere_dat,psg,psnm,1)

filename = trim(adjustl(datapath)) // trim(adjustl(datestring)) // "/" // "sanl"//datestring
CALL WRSIG(IUNIT,FILENAME,LAB,0.0,IDATEOUT,SISL,HEADEXT,ZSNM,PSNM,VRTNM,DIVNM,&
           TEMPNM,QNM,NMDIM,NLEVS,NTRAC,NLONS,NLATS,0,IGEN)

!==> calculate fit of ens. mean analysis to obs.

nobsps_nh = 0
nobsps_sh = 0
nobsps_tr = 0
 
do nob=1,nobstot


! just compute posterior statistics if iuseob=1
 if (iuseob(nob) .eq. 1) then

 obfit_post(nob) = ensmean_ob(nob)-ob(nob)
! take into account posterior inflation here.
 if (postinflate) then
   deglat = rtod*oblocy(nob)
   if (deglat .gt. latbound+0.5*delat) then
     covinf = covinflatenh
     lnsigl = lnsigcutoffnh
   else if (deglat .le. latbound+0.5*delat .and. deglat .ge. latbound-0.5*delat) then
     covinf = (((latbound+0.5*delat)-deglat)/delat)*covinflatetr + ((deglat-(latbound-0.5*delat))/delat)*covinflatenh
     lnsigl = (((latbound+0.5*delat)-deglat)/delat)*lnsigcutofftr + ((deglat-(latbound-0.5*delat))/delat)*lnsigcutoffnh
   else if (deglat .lt. latbound-0.5*delat .and. deglat .gt. -latbound+0.5*delat) then
     covinf = covinflatetr
     lnsigl = lnsigcutofftr
   else if (deglat .le. -latbound+0.5*delat .and. deglat .ge. -latbound-0.5*delat) then
     covinf = (((latbound+0.5*delat)+deglat)/delat)*covinflatetr + ((-deglat-(latbound-0.5*delat))/delat)*covinflatesh
     lnsigl = (((latbound+0.5*delat)+deglat)/delat)*lnsigcutofftr + ((-deglat-(latbound-0.5*delat))/delat)*lnsigcutoffsh
   else
     covinf = covinflatesh
     lnsigl = lnsigcutoffsh
   end if
   covinf = 1.+corr_fn(0, lnsigl)*(covinf-1.)
   anal_ob(nob,:) = covinf*anal_ob(nob,:)
 endif
 obsprd_post(nob) = sum(anal_ob(nob,:)**2)/float(nanals-1)

 end if
enddo

print *,'innovation statistics for posterior (using mean):'
call innovstats(obfit_post,obsprd_post,oberrvar,oblocy,iuseob,nobstot,'ps',0)

! write out obs stats.
if (obsfileout .ne. " ") then
print *,obsfileout
open(159,form="formatted",file=trim(adjustl(obsfileout)))
do nob=1,nobstot
   if (obtype(nob).eq.'P' .and. obtime(nob) .ge. -3. .and. &
       obtime(nob) .le. 3.) then
       theobfit_prior = obfit_prior(nob)
       theobsprd_prior = sqrt(obsprd_prior(nob))
       theobfit_post = obfit_post(nob)
       theobsprd_post = sqrt(obsprd_post(nob))
       write(159,9802) statid(nob),stattype(nob),obtype(nob),rtod*oblocx(nob),rtod*oblocy(nob),&
            nint(zob(nob)),nint(zfgob(nob)),obtime(nob),oborig(nob),ob(nob),slpob(nob),bias(nob),stdevorig(nob),stdev(nob),&
            iassim(nob),iuseob(nob),ibadob(nob),ifailbg(nob),ifailbud(nob),&
            theobfit_prior,theobsprd_prior,theobfit_post,theobsprd_post,statname(nob),obid(nob)
    end if
enddo
9802    format(a19,1x,i3,1x,a1,1x,f7.2,1x,f6.2,1x,i5,1x,i5,1x,f6.2,1x,f7.1,1x,f7.1,1x,&
             f7.1,1x,e10.3,1x,f5.2,1x,f5.2,5(1x,i1),4(1x,e10.3),1x,a30,1x,a13)
close(159)
end if

print *, 'max/min values of ensemble mean analysis'
print *, 'U,V,T/Z,q,ps:'
do nvar=1,nvars
print *, minval(ensmean(:,(nvar-1)*nlevs+1:nvar*nlevs)),&
         maxval(ensmean(:,(nvar-1)*nlevs+1:nvar*nlevs))
enddo
print *, minval(ensmean(:,ndim)),&
         maxval(ensmean(:,ndim))

!==> write out analyses.
 
do nanal=1,nanals

 ! gather data for perturbation nanal, add mean back in.
 anal(:,1:ninc) = anal_chunk(:,1:ninc,nanal) + ensmean(:,1:ninc)
 do np=1,numproc-1
   i1 = np * ninc + 1
   i2 = (np+1) * ninc 
   if (np .eq. numproc-1) i2=ndim
   call MPI_Recv(ensmean_chunk,(nlons+1)*(nlats+2)*ndim_max,MPI_REAL,np, &
                 nanal,MPI_COMM_WORLD,MPI_Status,ierr)
   anal(:,i1:i2) = ensmean_chunk(:,1:i2-i1+1) + ensmean(:,i1:i2)
 enddo

 write(charnanal,'(i3.3)') nanal

! read first guess tracers back again to get tracers that were not updated.
 read(iunit_tmp) qnm

 nn = 0
 do nvar=1,nvars
 do k=1,nlevs
 nn = nn + 1
 do j=1,nlats+2
 do i=1,nlons+1
    nx = (j-1)*(nlons+1) + i
    if (nvar .eq. 1) then
       ug(i,j,k) = anal(nx,nn)
    else if (nvar .eq. 2) then
       vg(i,j,k) = anal(nx,nn) 
    else if (nvar .eq. 3) then
       if (useheight) then
         zg(i,j,k) = anal(nx,nn) 
       else
         tempg(i,j,k) = anal(nx,nn) 
       end if
    else
       qg(i,j,k,nvar-3) = anal(nx,nn) 
       if (qg(i,j,k,nvar-3) .lt. 1.e-8) qg(i,j,k,nvar-3)=1.e-8
    end if
 enddo
 enddo
 enddo
 enddo
 nn = nn + 1
 do j=1,nlats+2
 do i=1,nlons+1
    nx = (j-1)*(nlons+1) + i
    psg(i,j) = alog(anal(nx,nn)/10.)
 enddo
 enddo
! convert height to temp.
 if (useheight) call ztotemp(nlons+1,nlats+2,nlevs,rgas/g,hsg,fsg,zsg,tempg,zg)
 do k=1,nlevs
    call getvrtdiv_pw(sphere_dat,vrtnm(:,k),divnm(:,k),ug(:,:,k),vg(:,:,k))
    call spharm_pw(sphere_dat,tempg(:,:,k),tempnm(:,k),1)
    do nt=1,nvars-3
       call spharm_pw(sphere_dat,qg(:,:,k,nt),qnm(:,k,nt),1)
    enddo
 enddo
 call spharm_pw(sphere_dat,psg,psnm,1)

 filename = trim(adjustl(datapath)) // trim(datestring) // "/" //"sanl_"//datestring//"_mem"//charnanal
 CALL WRSIG(IUNIT,FILENAME,LAB,0.0,IDATEOUT,SISL,HEADEXT,ZSNM,PSNM,VRTNM,DIVNM,&
            TEMPNM,QNM,NMDIM,NLEVS,NTRAC,NLONS,NLATS,nanal,IGEN)
enddo
close(iunit_tmp)

call cpu_time(t2)
print *,'time to write out analysis = ',t2-t1

end if

! all done.

call MPI_Barrier(MPI_COMM_WORLD,ierr)
if (nproc .eq. 0) write(6,*) 'all done!'
call MPI_Finalize(ierr)
if (ierr .ne. 0) then
 print *, 'MPI_Finalize error status = ',ierr
end if

end program ensrs_gfs_psonly

subroutine innovstats(obfit,obsprd,oberrvar,oblats,iuseob,nobstot,obtype,imedian)
! prints innovation statistics in nh,tropics and sh.
!
! input:
!
! obfit = (real) ensmean-ob.
! obsprd = (real) ensemble variance at ob locations.
! oberrvar = (real) ob error variances.
! oblats = (real) latitudes of ob locations.
! iuseob = (int) 1 if ob is to be used.
! nobstot = (int) # of obs (previous 5 inputs are vectors of length nobstot). 
! obtype = 2 char string describing ob type.
! imedian = (int) if 1, use median for expected value (otherwise use mean).


 implicit none
 integer, intent(in) :: nobstot,imedian
 real, intent(in), dimension(nobstot) :: oblats,obfit,obsprd,oberrvar
 integer, intent(in), dimension(nobstot) :: iuseob
 character(len=2), intent(in) ::  obtype
 real, dimension(:), allocatable :: s,r,fminuso
 real med,medianfit,mediansprd,medianoberr,mediansplusr,bias,pi,dtor
 logical nh,sh,tr
 character(len=5) :: hemlab
 integer ihemflag(nobstot),nhem,nn,nob

 pi = 4.*atan(1.0)
 dtor = pi/180.

 hemloop: do nhem=1,3  ! loop over each region 

 ihemflag = 0
 do nob=1,nobstot
    if (oblats(nob) .gt. 20.*dtor) then
       nh = .true.
       sh = .false.
       tr = .false.
    else if (oblats(nob) .lt. -20.*dtor) then
       nh = .false.
       sh = .true.
       tr  = .false.
    else
       nh = .false.
       sh = .false.
       tr  = .true.
    end if
    if (nh .and. nhem .eq. 1) ihemflag(nob)=1
    if (tr .and. nhem .eq. 2) ihemflag(nob)=1
    if (sh .and. nhem .eq. 3) ihemflag(nob)=1
 enddo
 nn = count(iuseob .eq. 1 .and. ihemflag .eq. 1)
 if (nn .eq. 0) cycle hemloop
 allocate(s(nn))
 allocate(r(nn))
 allocate(fminuso(nn))
 nn = 0
 do nob=1,nobstot
    if (iuseob(nob) .eq. 1 .and. ihemflag(nob) .eq. 1) then
      nn = nn + 1
      s(nn) = obsprd(nob)
      r(nn) = oberrvar(nob)
      fminuso(nn) = obfit(nob)
    end if
 enddo
 bias = sum(fminuso)/real(nn)
 fminuso = fminuso**2
 if (imedian .eq. 1) then
   call median(fminuso,nn,med)
 else
   med = sum(fminuso)/real(nn)
 end if
 medianfit = sqrt(med)
 if (imedian .eq. 1) then
   call median(s,nn,med)
 else
   med = sum(s)/real(nn)
 end if
 mediansprd = sqrt(med)
 if (imedian .eq. 1) then
   call median(r,nn,med)
 else
   med = sum(r)/real(nn)
 end if
 medianoberr = sqrt(med)
 mediansplusr = sqrt(mediansprd**2 + medianoberr**2)
 if (nhem .eq. 1) hemlab='nh'//' '//obtype
 if (nhem .eq. 2) hemlab='tr'//' '//obtype
 if (nhem .eq. 3) hemlab='sh'//' '//obtype
 print *, 'fit to ',nn,hemlab,' obs =  ',medianfit
 print *, 'bias for ',nn,hemlab,' obs =  ',bias
 print *, 'S+R,S,R at ',nn,hemlab,' ob locations =  ',mediansplusr,mediansprd,medianoberr

 deallocate(s,r,fminuso)

 end do hemloop ! end region loop

end subroutine innovstats

subroutine innovstats2(obfit,anal_ob,oberrvar,oblats,iuseob,nanals,nobstot,obtype)
! prints innovation statistics in nh,tropics and sh.
!
! input:
!
! obfit = (real) ensmean-ob.
! anal_ob = (real) ensemble perturbations at ob locations.
! oberrvar = (real) ob error variances.
! oblats = (real) latitudes of ob locations.
! iuseob = (int) 1 if ob is to be used.
! nobstot = (int) # of obs (previous 5 inputs are vectors of length nobstot). 
! nanals = (int) # of ensemble members
! obtype = 2 char string describing ob type.


 implicit none
 integer, intent(in) :: nobstot,nanals
 real, intent(in), dimension(nobstot) :: oblats,obfit,oberrvar
 real, intent(in), dimension(nobstot,nanals) :: anal_ob
 integer, intent(in), dimension(nobstot) :: iuseob
 character(len=2), intent(in) ::  obtype
 !real, dimension(:), allocatable :: fminuso,medianens
 real, dimension(:), allocatable :: fminuso
 real, dimension(:,:), allocatable :: s
 real med,medianfit,medianpert,pi,dtor,rnorm,medianensmed
 logical nh,sh,tr
 character(len=5) :: hemlab
 integer ihemflag(nobstot),nhem,nn,nob,nn2,nanal

 pi = 4.*atan(1.0)
 dtor = pi/180.

 hemloop: do nhem=1,3  ! loop over each region 

 ihemflag = 0
 do nob=1,nobstot
    if (oblats(nob) .gt. 20.*dtor) then
       nh = .true.
       sh = .false.
       tr = .false.
    else if (oblats(nob) .lt. -20.*dtor) then
       nh = .false.
       sh = .true.
       tr  = .false.
    else
       nh = .false.
       sh = .false.
       tr  = .true.
    end if
    if (nh .and. nhem .eq. 1) ihemflag(nob)=1
    if (tr .and. nhem .eq. 2) ihemflag(nob)=1
    if (sh .and. nhem .eq. 3) ihemflag(nob)=1
 enddo
 nn = count(iuseob .eq. 1 .and. ihemflag .eq. 1)
 if (nn .eq. 0) cycle hemloop
 allocate(s(nanals,nn))
 allocate(fminuso(nn))
 !allocate(medianens(nn))
 nn = 0
 do nob=1,nobstot
    if (iuseob(nob) .eq. 1 .and. ihemflag(nob) .eq. 1) then
      nn = nn + 1
      do nanal=1,nanals
         s(nanal,nn) = abs(anal_ob(nob,nanal) + sqrt(oberrvar(nob))*rnorm())
      enddo
      fminuso(nn) = abs(obfit(nob))
    end if
 enddo
 call median(fminuso,nn,medianfit)
 call median(s(1,1),nn*nanals,medianpert)
 !do nob=1,nn
 !   call median(s(1,nob),nanals,medianens(nob))
 !enddo
 !call median(medianens,nn,medianensmed)
 if (nhem .eq. 1) hemlab='nh'//' '//obtype
 if (nhem .eq. 2) hemlab='tr'//' '//obtype
 if (nhem .eq. 3) hemlab='sh'//' '//obtype
 print *, 'median abs innovation for ',nn,hemlab,' obs =  ',medianfit
 !print *, 'median predicted abs innovation for ',nn,hemlab,' obs =  ',medianpert,medianensmed
 print *, 'median predicted abs innovation for ',nn,hemlab,' obs =  ',medianpert

 !deallocate(s,fminuso,medianens)
 deallocate(s,fminuso)

 end do hemloop ! end region loop

end subroutine innovstats2

real function corr_fn(r,corrlength)
! Gaspari and Cohn correlation function.
const = 0.5*corrlength
if (r .le. const) then
corr_fn =&
   -(1./4.)*(r/const)**5+(1./2.)*(r/const)**4+&
    (5./8.)*(r/const)**3-(5./3.)*(r/const)**2+1.
else if (r .gt. const .and. r .le. corrlength) then
corr_fn =&
    (1./12.)*(r/const)**5-(1./2.)*(r/const)**4+&
    (5./8.)*(r/const)**3+(5./3.)*(r/const)**2-5.*(r/const)+4.&
   -(2./3.)*(const/r)
else
corr_fn = 0.
end if
end function corr_fn

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
 
 real f(nx,ny)

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
 real f(nx,ny,nz)

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

      SUBROUTINE ZTOTEMP(NLONS,NLATS,NLEVS,ROG,SI,SL,ZS,TV,Z)
!
! SUBPROGRAM:    TEMPTOZ       CALCULATE GEOPOTENTIAL HEIGHTS
!
! ABSTRACT: CALCULATES VIRTUAL TEMP FROM HEIGHTS ON INTERFACES
!   (PLUS TOP SIGMA LEVEL)
!   USES THE HYDROSTATIC EQUATION DZ=RD/G*TV*DLNP.
!
! USAGE:    CALL ZTOTEMP(NLONS,NLATS,NLEVS,SI,SL,ZS,TV,Z)
!
!   INPUT ARGUMENT LIST:
!     NLONS    - INTEGER NUMBER OF LONGITUDES
!     NLATS    - INTEGER NUMBER OF LEVELS
!     NLEVS    - INTEGER NUMBER OF LEVELS
!     ROG      - GAS CONSTANT/GRAVITY
!     SI       - REAL (NLEVS+1) SIGMA INTERFACE VALUES
!     SL       - REAL (NLEVS) SIGMA VALUES
!     ZS       - REAL (NLONS,NLATS) OROGRAPHY IN M
!     Z        - REAL Z(:,:,1:NLEVS-1) ARE HEIGHTS IN M ON SI(2:NLEVS), 
!                REAL Z(:,:,NLEVS) ARE HEIGHTS IN M ON SL(NLEVS).
!
!   OUTPUT ARGUMENT LIST:
!     TV       - REAL (NLONS,NLATS,NLEVS) VIRTUAL TEMPERATURE IN K
!
!   LOCAL VARIABLES:
!     ZI       - REAL ZI(:,:,1:NLEVS) HEIGHTS IN M ON THE SI(1:NLEVS).
!                ZI(:,:,NLEVS+1) IS HEIGHT ON SL(NLEVS).
!
      REAL SI(NLEVS+1),SL(NLEVS),ZS(NLONS,NLATS),TV(NLONS,NLATS,NLEVS)
      REAL Z(NLONS,NLATS,NLEVS),ZI(NLONS,NLATS,NLEVS+1),ROG,CA,CB
      INTEGER K

      ZI(:,:,2:NLEVS+1) = Z(:,:,1:NLEVS)
      ZI(:,:,1) = ZS(:,:)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      DO K=1,NLEVS
        CA=ROG*LOG(SI(K)/SL(K))
        IF (K .LT. NLEVS) THEN
           CB=ROG*LOG(SL(K)/SI(K+1))
        ELSE
           CB=0.0
        END IF
        TV(:,:,K) = (ZI(:,:,K+1)-ZI(:,:,K))/(CA+CB)
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE ZTOTEMP


      SUBROUTINE TEMPTOZ(NLONS,NLATS,NLEVS,ROG,SI,SL,ZS,TV,ZOUT)
!
! SUBPROGRAM:    TEMPTOZ       CALCULATE GEOPOTENTIAL HEIGHTS
!
! ABSTRACT: CALCULATES GEOPOTENTIAL HEIGHTS ON BOTH THE SIGMA INTERFACES
!   AND THE SIGMA FULL LEVELS AS A FUNCTION OF OROGRAPHY, VIRTUAL TEMPERATURE
!   THE HYDROSTATIC EQUATION DZ=RD/G*TV*DLNP IS INTEGRATED.
!
! USAGE:    CALL TEMPTOZ(NLONS,NLATS,NLEVS,SI,SL,ZS,TV,ZOUT)
!
!   INPUT ARGUMENT LIST:
!     NLONS    - INTEGER NUMBER OF LONGITUDES
!     NLATS    - INTEGER NUMBER OF LEVELS
!     NLEVS    - INTEGER NUMBER OF LEVELS
!     ROG      - GAS CONSTANT/GRAVITY
!     SI       - REAL (NLEVS+1) SIGMA INTERFACE VALUES
!     SL       - REAL (NLEVS) SIGMA VALUES
!     ZS       - REAL (NLONS,NLATS) OROGRAPHY IN M
!     TV       - REAL (NLONS,NLATS,NLEVS) VIRTUAL TEMPERATURE IN K
!
!   OUTPUT ARGUMENT LIST:
!     ZOUT     - REAL ZOUT(:,:,1:NLEVS-1) ARE HEIGHTS IN M ON SI(2:NLEVS), 
!                REAL ZOUT(:,:,NLEVS) ARE HEIGHTS IN M ON SL(NLEVS).
!
!   LOCAL VARIABLES:
!     Z        - REAL (NLONS,NLATS,NLEVS) HEIGHTS ON THE FULL LEVELS IN M
!     ZI       - REAL (NLONS,NLATS,NLEVS) HEIGHTS ON THE INTERFACES IN M
!
      REAL SI(NLEVS+1),SL(NLEVS),ZS(NLONS,NLATS),TV(NLONS,NLATS,NLEVS)
      REAL Z(NLONS,NLATS,NLEVS),ZI(NLONS,NLATS,NLEVS)
      REAL ZOUT(NLONS,NLATS,NLEVS),ROG,CA,CB
      INTEGER K
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ZI(:,:,1)=ZS(:,:)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      DO K=1,NLEVS-1
        CA=ROG*LOG(SI(K)/SL(K))
        CB=ROG*LOG(SL(K)/SI(K+1))
        Z(:,:,K)=ZI(:,:,K)+CA*TV(:,:,K)
        ZI(:,:,K+1)=Z(:,:,K)+CB*TV(:,:,K)
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CA=ROG*LOG(SI(NLEVS)/SL(NLEVS))
      Z(:,:,NLEVS)=ZI(:,:,NLEVS)+CA*TV(:,:,NLEVS)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ZOUT(:,:,1:NLEVS-1) = ZI(:,:,2:NLEVS)
      ZOUT(:,:,NLEVS) = Z(:,:,NLEVS)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE TEMPTOZ

 SUBROUTINE WRSIG(IUNIT,FILENAME,LAB,FHOUR,IDATE,SISL,HEADEXT,ZSNM,PSNM,VRTNM,&
                  DIVNM,TEMPNM,QNM,NMDIM,NLEVS,NTRAC,NLONS,NLATS,NANAL,IGEN)
 real, dimension(44) :: headext
 real, dimension(201) :: sisl
 integer idate(4),nanal,igen
 real fhour
 character (len=8), dimension(4) :: lab
 character (len=120) :: filename
 complex, dimension(nmdim,nlevs) :: vrtnm,divnm,tempnm
 complex, dimension(nmdim,nlevs,ntrac) :: qnm
 complex, dimension(nmdim) :: psnm,zsnm

 open(iunit,form="unformatted",file=filename)

 WRITE(iunit) LAB

! ensemble info
! http://www.emc.ncep.noaa.gov/gmb/ens/info/ens_grib.html#gribex
 if (nanal .eq. 0) then
   headext(15) = 5 ! whole ensemble (41st element of PDS)
   headext(16) = 0 ! (42nd element of PDS)
 else
   headext(15) = 3 ! pos pert
   headext(16) = nanal ! ensemble member number
 end if
 headext(14) = 2 ! sub-center, must be 2 or ens info not used
! model-generating id (we use as version number, starting with 100)
 headext(6) = igen

 WRITE(iunit) FHOUR,IDATE,SISL,HEADEXT
 WRITE(iunit) (ZSNM(I),I=1, NMDIM )
 WRITE(iunit) (PSNM(I),I=1, NMDIM )
 DO K=1, NLEVS
 WRITE(iunit)(TEMPNM(I,K),I=1, NMDIM )
 ENDDO
 DO K=1, NLEVS
 WRITE(iunit)(DIVNM(I,K),I=1, NMDIM )
 WRITE(iunit)(VRTNM(I,K),I=1, NMDIM )
 ENDDO
 DO NT=1, NTRAC
 DO K=1, NLEVS
 WRITE(iunit)(QNM(I,K,NT),I=1, NMDIM )
 ENDDO
 ENDDO

 close(iunit)

 END SUBROUTINE WRSIG

function dist(lon1,lon2,lat1,lat2)

! compute great circle distance in radians between (lon1,lat1) and (lon2,lat2)
! lon,lat pairs given in radians - returned distance is also in radians.
! uses Haversine formula
! (see http://www.census.gov/cgi-bin/geo/gisfaq?Q5.1)

 real, intent(in) ::  lon1,lat1,lon2,lat2
 real :: dist,dlon,dlat

 dlon = lon2 - lon1
 dlat = lat2 - lat1
 a = (sin(dlat/2))**2 + cos(lat1) * cos(lat2) * (sin(dlon/2))**2
! this can happen due to roundoff error, resulting in dist = NaN.
 if (a .lt. 0.) a = 0.
 if (a .gt. 1.) a = 1.
 dist = 2.0 * atan2( sqrt(a), sqrt(1-a) )

end function dist

      FUNCTION GAMMLN(XX)
      REAL*8 COF(6),STP,HALF,ONE,FPF,X,TMP,SER
      DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,&
          -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
      DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/
      X=XX-ONE
      TMP=X+FPF
      TMP=(X+HALF)*LOG(TMP)-TMP
      SER=ONE
      DO 11 J=1,6
        X=X+ONE
        SER=SER+COF(J)/X
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER)
      RETURN
      END
      FUNCTION BETACF(A,B,X)
      PARAMETER (ITMAX=100,EPS=3.E-7)
      AM=1.
      BM=1.
      AZ=1.
      QAB=A+B
      QAP=A+1.
      QAM=A-1.
      BZ=1.-QAB*X/QAP
      DO 11 M=1,ITMAX
        EM=M
        TEM=EM+EM
        D=EM*(B-M)*X/((QAM+TEM)*(A+TEM))
        AP=AZ+D*AM
        BP=BZ+D*BM
        D=-(A+EM)*(QAB+EM)*X/((A+TEM)*(QAP+TEM))
        APP=AP+D*AZ
        BPP=BP+D*BZ
        AOLD=AZ
        AM=AP/BPP
        BM=BP/BPP
        AZ=APP/BPP
        BZ=1.
        IF(ABS(AZ-AOLD).LT.EPS*ABS(AZ))GO TO 1
11    CONTINUE
      PAUSE 'A OR B TOO BIG, OR ITMAX TOO SMALL'
1     BETACF=AZ
      RETURN
      END
      FUNCTION BETAI(A,B,X)
      IF(X.LT.0..OR.X.GT.1.)PAUSE 'BAD ARGUMENT X IN BETAI'
      IF(X.EQ.0..OR.X.EQ.1.)THEN
        BT=0.
      ELSE  
        BT=EXP(GAMMLN(A+B)-GAMMLN(A)-GAMMLN(B)&
            +A*ALOG(X)+B*ALOG(1.-X))
      ENDIF
      IF(X.LT.(A+1.)/(A+B+2.))THEN
        BETAI=BT*BETACF(A,B,X)/A
        RETURN
      ELSE  
        BETAI=1.-BT*BETACF(B,A,1.-X)/B
        RETURN
      ENDIF
      END

function preduce(ps,tpress,t,zmodel,zob,rgas,g,rlapse)
! compute MAPS pressure reduction from station to model elevation
! See Benjamin and Miller (1990, MWR, p. 2100)
! uses plus 'effective' surface
! temperature extrapolated from virtual temp (tv) at sig2 (tpress mb)
! using lapse rate between sig2 and sig1.
! ps - surface pressure observation to reduce
! t - virtual temp. at sig2 (pressure tpress).
! zmodel - model orographic height
! zob - station height
! rgas,g - gas constant, gravity
! rlapse - lapse rate between sig2 and sig1 (positive for
! temp increasing from sig2 to sig1).
   real preduce,ps,t,tpress,zmodel,zob,sig,t0,alpha
   alpha = rgas*rlapse/g
   t0 = t*(ps/tpress)**alpha
   preduce = ps*((t0 + rlapse*(zob-zmodel))/t0)**(1./alpha)
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
