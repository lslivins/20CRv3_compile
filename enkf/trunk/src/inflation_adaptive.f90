module inflation
!$$$  module documentation block
!
! module: inflation           inflate posterior ensemble perturbations
!                             by an factor proportional to the amount
!                             the ensemble spread is reduced by the assimilation
!                             of observations.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: posterior ensemble multiplicative inflation. The amount of
!  inflation is given at each analysis grid point by:
!
!  r = analpertwt*((stdev_prior-stdev_posterior)/stdev_posterior) + 1 
!
!  where stdev_prior is the prior ensemble standard deviation,
!  stdev_posterior is posterior ensemble standard deviation
!  (before inflation), and r is inflation factor applied to each ensemble
!  member deviation from the ensemble mean.
!  if analpertwt=1, ensemble inflated so posterior standard deviation same as prior.
!  if analpertwt=0, there is no inflation.
!  analpertwt is a namelist parameter defined in module params.
!  The inflation factor can be different for each variable, level and horizontal
!  grid point and will in general be larger where observations are dense.
!
!  if the smoothing parameter (smoothparm) > 0, the estimated inflation
!  factor is smoothed using a Gaussian spectral filter with an efolding
!  scale of smoothparm.
!
!  The minimum and maximum values allowed can be controlled by the
!  namelist parameters covinflatemin and covinflatemax.
!
! Public Subroutines:
!  inflate_ens: apply inflation to the ensemble perturbations after
!   the EnKF analysis step.
!
! Public Variables: None
!   
! Modules Used: mpisetup, params, kinds, covlocal, statevec, gridinfo, loadal
!
! program history log:
!   2009-02-23  Initial version.
! attributes:
!   language: f95
!
!$$$

use mpisetup
use params, only: analpertwtnh,analpertwtsh,analpertwttr,ndim,nanals,nlevs,ndim,&
                  iassim_order, latbound, delat, datapath, covinflatemax, &
                  covinflatemin, nlons, nlats, adaptinf, inflation_var, &
                  update_inflation_var, inflation_damp, nvars, smoothparm
use obsmod, only: oberrvar, ob, ensmean_ob, obloc, oblnp, nobsgood, &
                  obfit_prior, obsprd_prior, obtime,&
                  corrlengthsq,lnsigl,obtimel
use kinds, only: r_single, i_long, r_double, r_kind, i_kind
use kdtree2_module, only: kdtree2_r_nearest, kdtree2_result
use enkf, only: indxassim
use constants, only: rad2deg, deg2rad, constants_initialized, zero, one, two, pi
use covlocal, only: taper, latval
use statevec, only: anal_chunk, anal_chunk_prior
use gridinfo, only: latsgrd, npts
use loadbal, only: numobsperproc, numptsperproc, indxproc_obs, iprocob, &
                   indxproc, lnp_chunk, kdtree_obs, kdtree_grid, &
                   ensmean_obchunk, indxob_chunk, oblnp_chunk, nobs_max, &
                   obtime_chunk, numobsmax1, numobsmax2, grdloc_chunk, obloc_chunk, &
                   npts_max, anal_obchunk_prior
use smooth_mod, only: smooth

implicit none

private
public :: inflate_ens

contains

subroutine inflate_ens()
! driver 
if (adaptinf) then
 if (nproc .eq. 0)  print *,'inflation damping = ',inflation_damp
 call inflate_ens_adaptive()
else
 if (nproc .eq. 0) print *,'relaxation to prior inflation ..'
 call inflate_ens_relax()
end if
end subroutine inflate_ens

subroutine inflate_ens_adaptive()
! Following Anderson (2009) Tellus 61A 72-83 (fixed inflation prior variance)
! includes inflation damping

integer(i_kind),parameter :: ndiag = 3
real(r_kind) sprdmin, sprdmax, sprdmaxall, sprdminall
real(r_kind),dimension(ndiag) :: sumcoslat,suma,suma2,sumi,sumf,sumitot,sumatot, &
     sumcoslattot,suma2tot,sumftot
character(len=120) filename
real(r_kind) dist_2,corrsqr,lnsig,obt,&
             corrlengthinv,lnsiglinv,obtimelinv,deglat,coslat,fnanalsml
real(r_kind) &
pfht,gamma,r_nanals,r_nanalsm1,new_cov_inflate,new_cov_inflate_sd,inf_sd_min
real(r_kind) r,r2,r3
real(r_kind) taper1,taper3
! kd-tree search results
type(kdtree2_result),dimension(:),allocatable :: sresults1,sresults2 
integer(i_kind) i,nn,iunit,np,nob,nobx,nf,ii,ierr,npt,npob
real(r_kind), allocatable, dimension(:,:) :: tmp_chunk, inflation_chunk,&
                  covinfglobal, covinfglobal2, fsprd, asprd
real(r_kind) anal_obtmp(nanals)

if (.not. constants_initialized) then
    print *,'constants not initialized (with init_constants, init_constants_derived)'
    call stop2(27)
end if

allocate(sresults1(numptsperproc(nproc+1)))
allocate(sresults2(numobsperproc(nproc+1)))
allocate(tmp_chunk(npts_max,ndim))
allocate(fsprd(npts_max,ndim))
allocate(asprd(npts_max,ndim))
allocate(inflation_chunk(npts_max,ndim))

! read in inflation.
if (nproc .eq. 0) then
  allocate(covinfglobal(npts,ndim))
  iunit = 88
  filename = trim(adjustl(datapath))//"covinflate_prior.dat"
  open(iunit,form='unformatted',file=filename,access='direct',recl=npts*ndim*4)
  read(iunit,rec=1) covinfglobal 
  close(iunit)
  print *,'min/max u prior inflation = ',minval(covinfglobal(:,1:nlevs)),maxval(covinfglobal(:,1:nlevs))
  print *,'min/max v prior inflation = ',minval(covinfglobal(:,nlevs+1:2*nlevs)),maxval(covinfglobal(:,nlevs+1:2*nlevs))
  print *,'min/max temp prior inflation = ',minval(covinfglobal(:,2*nlevs+1:3*nlevs)),maxval(covinfglobal(:,2*nlevs+1:3*nlevs))
  if (nvars .gt. 3) print *,'min/max spfh prior inflation = ',minval(covinfglobal(:,3*nlevs+1:4*nlevs)),maxval(covinfglobal(:,3*nlevs+1:4*nlevs))
  print *,'min/max ps prior inflation = ',minval(covinfglobal(:,ndim)),maxval(covinfglobal(:,ndim))
  ! send data to other processors.
  do np=1,numproc-1
     do i=1,numptsperproc(np+1)
        tmp_chunk(i,:) = covinfglobal(indxproc(np+1,i),:)
     end do
     call MPI_Send(tmp_chunk,npts_max*ndim,MPI_REAL,np, &
          1,MPI_COMM_WORLD,ierr)
  end do
  ! for root proc
  do i=1,numptsperproc(1)
     inflation_chunk(i,:) = covinfglobal(indxproc(1,i),:)
  end do
  deallocate(covinfglobal)
else
  call MPI_Recv(inflation_chunk,npts_max*ndim,MPI_REAL,0, &
        1,MPI_COMM_WORLD,MPI_Status,ierr)
endif

! define a few frequently used parameters
r_nanals=one/float(nanals)
r_nanalsm1=one/float(nanals-1)

! compute prior and posterior spread.
do nn=1,ndim
do i=1,numptsperproc(nproc+1)
   fsprd(i,nn) = sum(anal_chunk_prior(:,i,nn)**2)*r_nanalsm1
   asprd(i,nn) = sum(anal_chunk(:,i,nn)**2)*r_nanalsm1
   ! area mean surface pressure posterior and prior spread.
   ! (this diagnostic only makes sense for grids that are regular in longitude)
   if (nn == ndim) then 
      coslat=cos(latsgrd(indxproc(nproc+1,i)))
      if (fsprd(i,nn) > sprdmax) sprdmax = fsprd(i,nn)
      if (fsprd(i,nn) < sprdmin) sprdmin = fsprd(i,nn)
      if (deglat > latbound) then 
         suma(1) = suma(1) + asprd(i,nn)*coslat
         sumf(1) = sumf(1) + fsprd(i,nn)*coslat
         sumcoslat(1) = sumcoslat(1) + coslat
      else if (deglat < -latbound) then
         suma(2) = suma(2) + asprd(i,nn)*coslat
         sumf(2) = sumf(2) + fsprd(i,nn)*coslat
         sumcoslat(2) = sumcoslat(2) + coslat
      else
         suma(3) = suma(3) + asprd(i,nn)*coslat
         sumf(3) = sumf(3) + fsprd(i,nn)*coslat
         sumcoslat(3) = sumcoslat(3) + coslat
      end if
   end if
enddo
enddo
! clip prior variance (in state and ob space) to avoid NaNs when stdev calculated
where (fsprd < tiny(fsprd(1,1))) fsprd = tiny(fsprd(1,1))
where (obsprd_prior < tiny(obsprd_prior(1))) obsprd_prior = tiny(obsprd_prior(1))
! use tmp_chunk to hold inflation variance
tmp_chunk = inflation_var
inf_sd_min = 0.01 ! minimum allowed inflation stdev.

! loop over 'good' obs.
obsloop: do nobx=1,nobsgood
    nob = indxassim(nobx)
    ! determine localization length scales based on latitude of ob.
    corrlengthinv=one/corrlengthsq(nob)
    corrsqr = corrlengthsq(nob)**2
    lnsiglinv=one/lnsigl(nob)
    obtimelinv=one/obtimel(nob)
    obt = abs(obtime(nob))
    ! what processor is this ob on?
    npob = iprocob(nob)
    ! get ob priors, ob increment from that processor,
    ! send to other processors.
    if (nproc == npob) anal_obtmp=anal_obchunk_prior(:,indxob_chunk(nob))
    call MPI_Bcast(anal_obtmp,nanals,MPI_REAL,npob,MPI_COMM_WORLD,ierr)
    dist_2 = obfit_prior(nob)**2
    ! search analysis grid points for those within corrlength of 
    ! ob being assimilated (using a kd-tree for speed).
    if (associated(kdtree_grid)) then
       call kdtree2_r_nearest(tp=kdtree_grid,qv=obloc(:,nob),r2=corrsqr,&
            nfound=nf,nalloc=numptsperproc(nproc+1),results=sresults1)
    else
       ! use brute force search if number of grid points on this proc <= 3
       nf = 0
       do npt=1,numptsperproc(nproc+1)
          r=(obloc(1,nob)-grdloc_chunk(1,npt))**2+&
            (obloc(2,nob)-grdloc_chunk(2,npt))**2+&
            (obloc(3,nob)-grdloc_chunk(3,npt))**2
          if (r < corrsqr) then
              nf = nf + 1
              sresults1(nf)%idx = npt
              sresults1(nf)%dis = r
          end if     
       end do
    end if
    ! update inflation 
    if (nf > 0) then
       r3 = obt*obtimelinv
       taper3=taper(r3)
       do ii=1,nf
          taper1=taper(sqrt(sresults1(ii)%dis)*corrlengthinv)*taper3
          do nn=1,ndim
             i = sresults1(ii)%idx
             lnsig = abs(lnp_chunk(i,nn)-oblnp(nob))
             if (lnsig < lnsigl(nob)) then
                r2 = lnsig*lnsiglinv
                pfht = sum(anal_chunk_prior(:,i,nn)*anal_obtmp)*r_nanalsm1
                gamma = abs(taper1*taper(r2)*pfht/sqrt(obsprd_prior(nob)*fsprd(i,nn)))
                ! compute updated inflation.
                call update_inf_mean(dist_2,obsprd_prior(nob),oberrvar(nob),&
                inflation_chunk(i,nn),tmp_chunk(i,nn),gamma,new_cov_inflate)
                if (update_inflation_var) then
                  call update_inf_stdev(dist_2,obsprd_prior(nob),oberrvar(nob),inflation_chunk(i,nn),new_cov_inflate,&
                  sqrt(tmp_chunk(i,nn)),inf_sd_min,gamma,new_cov_inflate_sd)
                  tmp_chunk(i,nn) = new_cov_inflate_sd**2 ! inf. var updated
                endif
                inflation_chunk(i,nn) = new_cov_inflate
             end if
          end do
       end do
    end if ! if no close grid points
end do obsloop ! loop over obs to assimilate
! inflation damping.
inflation_chunk = one + (inflation_chunk-one)*inflation_damp

! if desired, smooth inflation.
allocate(covinfglobal(npts,ndim),covinfglobal2(npts,ndim))
covinfglobal2=zero
do i=1,numptsperproc(nproc+1)
   covinfglobal2(indxproc(nproc+1,i),:) = inflation_chunk(i,:)
end do
call mpi_allreduce(covinfglobal2,covinfglobal,npts*ndim,mpi_real,mpi_sum,mpi_comm_world,ierr)
deallocate(covinfglobal2)
if (smoothparm .gt. zero) then
   call smooth(covinfglobal)
   where (covinfglobal < covinflatemin) covinfglobal = covinflatemin
   where (covinfglobal > covinflatemax) covinfglobal = covinflatemax
   do i=1,numptsperproc(nproc+1)
      inflation_chunk(i,:) = covinfglobal(indxproc(nproc+1,i),:)
   end do
end if

if(nproc == 0)then
   print *,'min/max u inflation = ',minval(covinfglobal(:,1:nlevs)),maxval(covinfglobal(:,1:nlevs))
   print *,'min/max v inflation = ',minval(covinfglobal(:,nlevs+1:2*nlevs)),maxval(covinfglobal(:,nlevs+1:2*nlevs))
   print *,'min/max temp inflation = ',minval(covinfglobal(:,2*nlevs+1:3*nlevs)),maxval(covinfglobal(:,2*nlevs+1:3*nlevs))
   print *,'min/max spfh inflation = ',minval(covinfglobal(:,3*nlevs+1:4*nlevs)),maxval(covinfglobal(:,3*nlevs+1:4*nlevs))
   print *,'min/max ps inflation = ',minval(covinfglobal(:,ndim)),maxval(covinfglobal(:,ndim))
!  write it out.
   iunit = 88
   filename = trim(adjustl(datapath))//"covinflate.dat"
   open(iunit,form='unformatted',file=filename,access='direct',recl=npts*ndim*4)
   write(iunit,rec=1) covinfglobal 
   close(iunit)
end if

! apply inflation.
do nn=1,ndim
 do i=1,numptsperproc(nproc+1)

   ! inflate posterior perturbations.
   anal_chunk(:,i,nn) = inflation_chunk(i,nn)*anal_chunk(:,i,nn)

   ! area mean surface pressure posterior spread, inflation.
   ! (this diagnostic only makes sense for grids that are regular in longitude)
   if (nn == ndim) then 
      coslat=cos(latsgrd(indxproc(nproc+1,i)))
      deglat = rad2deg*latsgrd(indxproc(nproc+1,i))
      if (deglat > latbound) then 
         suma2(1) = suma2(1) + &
         sum(anal_chunk(:,i,nn)**2)*coslat*fnanalsml
         sumi(1) = sumi(1) + inflation_chunk(i,nn)*coslat
      else if (deglat < -latbound) then
         suma2(2) = suma2(2) + &
         sum(anal_chunk(:,i,nn)**2)*coslat*fnanalsml
         sumi(2) = sumi(2) + inflation_chunk(i,nn)*coslat
      else
         suma2(3) = suma2(3) + &
         sum(anal_chunk(:,i,nn)**2)*coslat*fnanalsml
         sumi(3) = sumi(3) + inflation_chunk(i,nn)*coslat
      end if
   end if

 end do
end do

deallocate(inflation_chunk,fsprd,asprd,tmp_chunk)
deallocate(anal_obchunk_prior) ! no longer need these
deallocate(sresults1,sresults2)
deallocate(indxassim)

! collect statistics of area mean inflation, posterior and prior standard deviation for ps.
call mpi_reduce(sprdmin,sprdminall,1,mpi_real,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(sprdmax,sprdmaxall,1,mpi_real,mpi_max,0,mpi_comm_world,ierr)
call mpi_reduce(sumf,sumftot,ndiag,mpi_real,mpi_sum,0,mpi_comm_world,ierr)
call mpi_reduce(sumi,sumitot,ndiag,mpi_real,mpi_sum,0,mpi_comm_world,ierr)
call mpi_reduce(suma,sumatot,ndiag,mpi_real,mpi_sum,0,mpi_comm_world,ierr)
call mpi_reduce(suma2,suma2tot,ndiag,mpi_real,mpi_sum,0,mpi_comm_world,ierr)
call mpi_reduce(sumcoslat,sumcoslattot,ndiag,mpi_real,mpi_sum,0,mpi_comm_world,ierr)
if (nproc == 0) then
   sumftot = sqrt(sumftot/sumcoslattot)
   sumatot = sqrt(sumatot/sumcoslattot)
   suma2tot = sqrt(suma2tot/sumcoslattot)
   sumitot = sumitot/sumcoslattot
   print *,'global ps prior std. dev min/max = ',sqrt(sprdminall),sqrt(sprdmaxall)
   print *,'NH mean ps prior standard deviation = ',sumftot(1)
! NH first.
   print *,'NH mean ps posterior standard deviation (before inflation)= ',sumatot(1)
   print *,'NH mean ps posterior standard deviation (after inflation) = ',suma2tot(1)
   print *,'NH mean ps inflation = ',sumitot(1)

! now SH.
   print *,'SH mean ps prior standard deviation = ',sumftot(2)
   print *,'SH mean ps posterior standard deviation (before inflation)= ',sumatot(2)
   print *,'SH mean ps posterior standard deviation (after inflation) = ',suma2tot(2)
   print *,'SH mean ps inflation = ',sumitot(2)
! now tropics.
   print *,'TR mean ps prior standard deviation = ',sumftot(3)
   print *,'TR mean ps posterior standard deviation (before inflation)= ',sumatot(3)
   print *,'TR mean ps posterior standard deviation (after inflation) = ',suma2tot(3)
   print *,'TR mean ps inflation = ',sumitot(3)
end if

end subroutine inflate_ens_adaptive

subroutine inflate_ens_relax()

integer(i_kind),parameter :: ndiag = 3
!  Currently 3 diagnostic areas (ndiag =3)
!  Area 1 northern hemisphere
!  Area 2 southern hemisphere
!  Area 3 tropics

real(r_kind) sprdmin, sprdmax, sprdmaxall, &
  sprdminall, deglat,analpertwt, fsprd, asprd
real(r_kind),dimension(ndiag) :: sumcoslat,suma,suma2,sumi,sumf,sumitot,sumatot, &
     sumcoslattot,suma2tot,sumftot
real(r_kind) fnanalsml,coslat
integer(i_kind) i,nn,iunit,ierr
character(len=120) filename
real(r_kind), allocatable, dimension(:,:) :: inflation_chunk,&
         covinfglobal, covinfglobal2

! if no inflation called for, do nothing.
if (abs(analpertwtnh) < 1.e-5_r_kind .and. &
    abs(analpertwttr) < 1.e-5_r_kind .and. &
    abs(analpertwtsh) < 1.e-5_r_kind) return

if (.not. constants_initialized) then
    print *,'constants not initialized (with init_constants, init_constants_derived)'
    call stop2(27)
end if
fnanalsml = one/(real(nanals-1,r_kind))

! if analpertwtnh<0 use 'relaxation-to-prior' ensemble inflation,
! as first described in:
! Zhang, F., C. Snyder, and J. Sun, 2004: Tests of an ensemble 
! Kalman Filter for convective-scale data assim-imilation:
! Impact of initial estimate and observations. 
! Mon. Wea. Rev., 132, 1238-1253. 
if (analpertwtnh < 0) then
   do nn=1,ndim
    do i=1,numptsperproc(nproc+1)
      deglat = rad2deg*latsgrd(indxproc(nproc+1,i))
      ! coefficent can be different in NH, TR, SH.
      analpertwt = &
        latval(deglat,abs(analpertwtnh),abs(analpertwttr),abs(analpertwtsh))
      anal_chunk(:,i,nn) = analpertwt*anal_chunk_prior(:,i,nn) +&
        (one-analpertwt)*anal_chunk(:,i,nn)
    end do
   end do
   return
end if

! adaptive posterior inflation based upon ratio of posterior to prior spread.
allocate(inflation_chunk(npts_max,ndim))

! compute inflation.
sumf = zero
suma = zero
sumcoslat = zero
sprdmax = -9.9e31_r_kind
sprdmin = 9.9e31_r_kind

do nn=1,ndim
 do i=1,numptsperproc(nproc+1)
   deglat = rad2deg*latsgrd(indxproc(nproc+1,i))

   ! compute stdev of prior and posterior.
   asprd = sum(anal_chunk(:,i,nn)**2)*fnanalsml  
   fsprd = sum(anal_chunk_prior(:,i,nn)**2)*fnanalsml

   ! inflation proportional to posterior stdev reduction
   ! if analpertwt=1, ensemble inflated so posterior stdev same as prior.
   ! if analpertwt=0, no inflation.
   ! coefficent can be different in NH, TR, SH.
   analpertwt = latval(deglat,analpertwtnh,analpertwttr,analpertwtsh)
   fsprd = max(fsprd,tiny(fsprd))
   asprd = max(asprd,tiny(asprd))

   ! area mean surface pressure posterior and prior spread.
   ! (this diagnostic only makes sense for grids that are regular in longitude)
   if (nn == ndim) then 
      coslat=cos(latsgrd(indxproc(nproc+1,i)))
      if (fsprd > sprdmax) sprdmax = fsprd
      if (fsprd < sprdmin) sprdmin = fsprd
      if (deglat > latbound) then 
         suma(1) = suma(1) + asprd*coslat
         sumf(1) = sumf(1) + fsprd*coslat
         sumcoslat(1) = sumcoslat(1) + coslat
      else if (deglat < -latbound) then
         suma(2) = suma(2) + asprd*coslat
         sumf(2) = sumf(2) + fsprd*coslat
         sumcoslat(2) = sumcoslat(2) + coslat
      else
         suma(3) = suma(3) + asprd*coslat
         sumf(3) = sumf(3) + fsprd*coslat
         sumcoslat(3) = sumcoslat(3) + coslat
      end if
   end if

   fsprd = sqrt(fsprd); asprd = sqrt(asprd)
   ! clip values to avoid NaNs.
   asprd = max(asprd,tiny(asprd))
   fsprd = max(fsprd,tiny(fsprd))
   inflation_chunk(i,nn) = analpertwt*((fsprd-asprd)/asprd) + 1.0

   ! min/max inflation set by covinflatemin/covinflatemax.
   inflation_chunk(i,nn) = max(covinflatemin,min(inflation_chunk(i,nn),covinflatemax))

 end do
end do

allocate(covinfglobal(npts,ndim),covinfglobal2(npts,ndim))
covinfglobal2=zero
do i=1,numptsperproc(nproc+1)
   covinfglobal2(indxproc(nproc+1,i),:) = inflation_chunk(i,:)
end do
call mpi_allreduce(covinfglobal2,covinfglobal,npts*ndim,mpi_real,mpi_sum,mpi_comm_world,ierr)
if (smoothparm .gt. zero) call smooth(covinfglobal)
where (covinfglobal < covinflatemin) covinfglobal = covinflatemin
where (covinfglobal > covinflatemax) covinfglobal = covinflatemax
deallocate(covinfglobal2)
if(nproc == 0)then
   print *,'min/max u inflation = ',minval(covinfglobal(:,1:nlevs)),maxval(covinfglobal(:,1:nlevs))
   print *,'min/max v inflation = ',minval(covinfglobal(:,nlevs+1:2*nlevs)),maxval(covinfglobal(:,nlevs+1:2*nlevs))
   print *,'min/max temp inflation = ',minval(covinfglobal(:,2*nlevs+1:3*nlevs)),maxval(covinfglobal(:,2*nlevs+1:3*nlevs))
   print *,'min/max spfh inflation = ',minval(covinfglobal(:,3*nlevs+1:4*nlevs)),maxval(covinfglobal(:,3*nlevs+1:4*nlevs))
   print *,'min/max ps inflation = ',minval(covinfglobal(:,ndim)),maxval(covinfglobal(:,ndim))
!  write it out.
   iunit = 88
   filename = trim(adjustl(datapath))//"covinflate.dat"
   open(iunit,form='unformatted',file=filename,access='direct',recl=npts*ndim*4)
   write(iunit,rec=1) covinfglobal 
   close(iunit)
end if

suma2 = zero
sumi = zero

! apply inflation.
do nn=1,ndim
 do i=1,numptsperproc(nproc+1)

   ! inflate posterior perturbations.
   anal_chunk(:,i,nn) = inflation_chunk(i,nn)*anal_chunk(:,i,nn)

   ! area mean surface pressure posterior spread, inflation.
   ! (this diagnostic only makes sense for grids that are regular in longitude)
   if (nn == ndim) then 
      coslat=cos(latsgrd(indxproc(nproc+1,i)))
      deglat = rad2deg*latsgrd(indxproc(nproc+1,i))
      if (deglat > latbound) then 
         suma2(1) = suma2(1) + &
         sum(anal_chunk(:,i,nn)**2)*coslat*fnanalsml
         sumi(1) = sumi(1) + inflation_chunk(i,nn)*coslat
      else if (deglat < -latbound) then
         suma2(2) = suma2(2) + &
         sum(anal_chunk(:,i,nn)**2)*coslat*fnanalsml
         sumi(2) = sumi(2) + inflation_chunk(i,nn)*coslat
      else
         suma2(3) = suma2(3) + &
         sum(anal_chunk(:,i,nn)**2)*coslat*fnanalsml
         sumi(3) = sumi(3) + inflation_chunk(i,nn)*coslat
      end if
   end if

 end do
end do

deallocate(inflation_chunk,covinfglobal)

! collect statistics of area mean inflation, posterior and prior standard deviation for ps.
call mpi_reduce(sprdmin,sprdminall,1,mpi_real,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(sprdmax,sprdmaxall,1,mpi_real,mpi_max,0,mpi_comm_world,ierr)
call mpi_reduce(sumf,sumftot,ndiag,mpi_real,mpi_sum,0,mpi_comm_world,ierr)
call mpi_reduce(sumi,sumitot,ndiag,mpi_real,mpi_sum,0,mpi_comm_world,ierr)
call mpi_reduce(suma,sumatot,ndiag,mpi_real,mpi_sum,0,mpi_comm_world,ierr)
call mpi_reduce(suma2,suma2tot,ndiag,mpi_real,mpi_sum,0,mpi_comm_world,ierr)
call mpi_reduce(sumcoslat,sumcoslattot,ndiag,mpi_real,mpi_sum,0,mpi_comm_world,ierr)
if (nproc == 0) then
   sumftot = sqrt(sumftot/sumcoslattot)
   sumatot = sqrt(sumatot/sumcoslattot)
   suma2tot = sqrt(suma2tot/sumcoslattot)
   sumitot = sumitot/sumcoslattot
   print *,'global ps prior std. dev min/max = ',sqrt(sprdminall),sqrt(sprdmaxall)
   print *,'NH mean ps prior standard deviation = ',sumftot(1)
! NH first.
   print *,'NH mean ps posterior standard deviation (before inflation)= ',sumatot(1)
   print *,'NH mean ps posterior standard deviation (after inflation) = ',suma2tot(1)
   print *,'NH mean ps inflation = ',sumitot(1)

! now SH.
   print *,'SH mean ps prior standard deviation = ',sumftot(2)
   print *,'SH mean ps posterior standard deviation (before inflation)= ',sumatot(2)
   print *,'SH mean ps posterior standard deviation (after inflation) = ',suma2tot(2)
   print *,'SH mean ps inflation = ',sumitot(2)
! now tropics.
   print *,'TR mean ps prior standard deviation = ',sumftot(3)
   print *,'TR mean ps posterior standard deviation (before inflation)= ',sumatot(3)
   print *,'TR mean ps posterior standard deviation (after inflation) = ',suma2tot(3)
   print *,'TR mean ps inflation = ',sumitot(3)
end if


end subroutine inflate_ens_relax

! routines from DART for Bayesian adaptive inflation.

subroutine update_inf_mean(dist_2, sigma_p_2, sigma_o_2, lambda_mean, lambda_sd_2, gamma, new_cov_inflate)

! from DART

! inputs:

! dist_2:  distance between prior ensemble mean and observed value = (Hx-y)**2
! sigma_p_2: variance of observation priors
! sigma_o_2: observation error variance
! lambda_mean: prior mean inflation factor.
! lambda_sd_2: prior inflation factor variance. Either a prescribed constant
! or determined by equation (14) in Anderson 2009 Tellus article.
! gamma: From equation (10), the product of localization times the correlation
! between the observation prior ensemble and the state ensemble being inflated.

real(r_single), intent(in)  :: dist_2, sigma_p_2, sigma_o_2, lambda_sd_2, gamma
real(r_single), intent(in) :: lambda_mean
real(r_single), intent(out) :: new_cov_inflate

real(r_double) :: theta_bar_2, u_bar, like_exp_bar, v_bar, like_bar, like_prime,&
a, b, c, plus_root, minus_root, dtheta_dlambda, theta_bar
   
! Compute value of theta at current lambda_mean
theta_bar_2 = (one + gamma * (lambda_mean - one))**2 * sigma_p_2 + sigma_o_2
theta_bar = sqrt(theta_bar_2)
! Compute constant coefficient for likelihood at lambda_bar
u_bar = one / (sqrt(two*pi) * theta_bar)
! Compute exponent of likelihood at lambda_bar
like_exp_bar = dist_2 / (-two * theta_bar_2)
! Compute exponential part of likelihood at lambda_bar
v_bar = exp(like_exp_bar)
! Compute value of likelihood at current lambda_bar value
like_bar = u_bar * v_bar
!
! If like_bar goes to 0, can't do anything, so just keep current values
if(like_bar < tiny(like_bar)) then
   new_cov_inflate = lambda_mean
   return
endif

! Next compute derivative of likelihood at this point

! First compute d/dlambda of theta evaluated at lambda_mean
! Verified correct by finite difference, 1 January, 2006
dtheta_dlambda = 0.5 * sigma_p_2 * gamma *(one - gamma + gamma*lambda_mean) / &
   (theta_bar * lambda_mean)
like_prime = (like_bar * dtheta_dlambda / theta_bar) * (dist_2 / theta_bar_2 - one)

! If like_prime goes to 0, can't do anything, so just keep current values
if(abs(like_prime) < tiny(like_prime)) then
   new_cov_inflate = lambda_mean
   return
endif
 
a = one
b = like_bar / like_prime - two * lambda_mean**2
c = lambda_mean**4 -lambda_sd_2 - like_bar * lambda_mean**2 / like_prime

! Use nice scaled quadratic solver to avoid precision issues
call solve_quadratic(a, b, c, plus_root, minus_root)

! Do a check to pick closest root
if(abs(minus_root - lambda_mean**2) < abs(plus_root - lambda_mean**2)) then
   new_cov_inflate = sqrt(minus_root)
else
   new_cov_inflate = sqrt(plus_root)
endif
! inflation should not be less than covinflatemin, or greater than covinflatemax.
new_cov_inflate = min(covinflatemax, new_cov_inflate)
new_cov_inflate = max(covinflatemin, new_cov_inflate)

end subroutine update_inf_mean

subroutine solve_quadratic(a, b, c, r1, r2)

real(r_double), intent(in)  :: a, b, c
real(r_double), intent(out) :: r1, r2

real(r_double) :: scaling, as, bs, cs, disc, root

! Scale the coefficients to get better round-off tolerance
scaling = max(abs(a), abs(b), abs(c))
as = a / scaling
bs = b / scaling
cs = c / scaling

! Get discriminant of scaled equation
root = bs**2 - 4.0d0 * as * cs
if (root < tiny(root)) then
   disc = zero
else
   disc = sqrt(root)
endif

if(bs > zero) then
   r1 = (-bs - disc) / (two * as)
else
   r1 = (-bs + disc) / (two * as)
endif

! Compute the second root given the larger one
r2 = (cs / as) / r1

end subroutine solve_quadratic

function compute_new_density(dist_2, sigma_p_2, sigma_o_2, lambda_mean, lambda_sd, gamma, lambda)

! Used to update density by taking approximate gaussian product

real(r_double)             :: compute_new_density
real, intent(in) :: dist_2, sigma_p_2, sigma_o_2, lambda_mean, lambda_sd, gamma, lambda

real(r_double) :: theta_2, theta
real(r_double) :: exponent_prior, exponent_likelihood

! Compute probability of this lambda being correct
exponent_prior = (lambda - lambda_mean)**2 / (-two * lambda_sd**2)

! Compute probability that observation would have been observed given this lambda
theta_2 = (one + gamma * (sqrt(lambda) - one))**2 * sigma_p_2 + sigma_o_2
theta = sqrt(theta_2)

exponent_likelihood = dist_2 / ( -two * theta_2)

! Compute the updated probability density for lambda
! Have 1 / sqrt(2 PI) twice, so product is 1 / (2 PI)
compute_new_density = exp(exponent_likelihood + exponent_prior) / &
   (two * pi * lambda_sd * theta)

end function compute_new_density

subroutine update_inf_stdev(dist_2,sigma_p_2,sigma_o_2,inf_mean_prior,inf_mean_post,inf_sd,inf_sd_min,gamma,new_cov_inflate_sd)
   real, intent(in) :: sigma_o_2,sigma_p_2,&
   inf_sd,inf_sd_min,inf_mean_prior,inf_mean_post,gamma,dist_2
   real, intent(out) :: new_cov_inflate_sd
   real(r_double) new_max,ratio,new_1_sd

   ! compute updated inflation standard deviation.
   ! Bail out to save cost when lower bound is reached on lambda standard deviation
   if(inf_sd <= inf_sd_min) then
        new_cov_inflate_sd = inf_sd
   else
   ! Compute by forcing a Gaussian fit at one positive SD
   ! First compute the new_max value for normalization purposes
        new_max = max(tiny(new_max),compute_new_density(dist_2, sigma_p_2, sigma_o_2, &
                  inf_mean_prior, inf_sd, gamma, inf_mean_post))
   ! Find value at a point one OLD sd above new mean value
        new_1_sd = compute_new_density(dist_2, sigma_p_2, sigma_o_2,&
                   inf_mean_prior,inf_sd, gamma, inf_mean_post+inf_sd)
        ratio = max( tiny(ratio), new_1_sd / new_max )
   ! Another error for numerical issues; if ratio is too close to 1, bail out
        if(ratio > one-tiny(ratio)) then
            new_cov_inflate_sd = inf_sd
   ! Can now compute the standard deviation consistent with this as
   ! sigma = sqrt(-x^2 / (2 ln(r))  where r is ratio and x is lambda_sd (distance from mean)
        else
            new_cov_inflate_sd = sqrt( -one * inf_sd**2 / (two * log(ratio)))
        end if
   end if
   ! Prevent an increase in the sd of lambda???
   ! For now, this is mostly countering numerical errors in this computation
   if(new_cov_inflate_sd > inf_sd) new_cov_inflate_sd = inf_sd
   ! bound below buy inf_sd_min
   new_cov_inflate_sd = max(inf_sd_min, new_cov_inflate_sd)

end subroutine update_inf_stdev

end module inflation
