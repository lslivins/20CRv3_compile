program gribmean
! compute ensemble mean and spread grib files in parallel.
! Jeff Whitaker <jeffrey.s.whitaker@noaa.gov> 20120426
implicit none
character(len=500) ingrib,outgrib,datapath
integer lunin, nmsg, iret, nlons, nlats, ndata, kskp, kpds(200), kgds(200), &
        nproc,numproc,kens(200),jpds(200),jgds(200),nanals,nrecs,nanal,lunout,iargc
real, allocatable, dimension(:,:) :: grid
integer, allocatable, dimension(:,:) :: kpdsa,kgdsa
! accumulators for one-pass mean and variance are double precision.
real(8), allocatable, dimension(:,:,:) :: data, datamean, dataspread
logical*1, allocatable, dimension(:,:) :: lbms
logical, allocatable, dimension(:,:,:) :: bitmaps, bitmapstot
character(len=10) filetype
character(len=10) gribdate
character(len=4) gribtype,char4
character(len=3) fhr
character(len=3) nanalstr

! mpi definitions.
include 'mpif.h'

call MPI_Init(iret)
! nproc is process number, numproc is total number of processes.
call MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
call MPI_Comm_size(MPI_COMM_WORLD,numproc,iret)

! unit number for input grib files.
lunin = 11
! unit number for output grib files.
lunout = 12
! get command line args (same as gribmean.py)
call getarg(1, datapath)
call getarg(2, char4)
read(char4,'(i4)') nlons
call getarg(3, char4)
read(char4,'(i4)') nlats
nanals = numproc
call getarg(4, gribdate)
call getarg(5, gribtype)
call getarg(6, filetype)
if (iargc() .lt. 6) then
  if (nproc .eq. 0) then
  print *, "gribmean.x datapath nlons nlats gribdate gribtype filetype fhr"
  print *,"gribtype is p or sflx"
  print *,"filetype is fg or anl"
  endif
  call mpi_abort(mpi_comm_world,101,iret)
endif
if (iargc() .gt. 6) then
    call getarg(7, fhr)
else
    fhr=''
endif
!if (trim(filetype) .ne. 'fg' .and. trim(filetype) .ne. 'anl') then
!   print *,"filetype (5th arg) must be 'fg' or 'anl' ",filetype
!   stop
!endif
if (nproc .eq. 0) print *,'nlons, nlats = ',nlons,nlats

! read the first member, just to find how many records there are.
allocate(grid(nlons,nlats))
allocate(lbms(nlons,nlats))
!if (nproc .eq. 0) then
!nanal = 1
nanal = nproc+1
write(nanalstr,'(i3.3)') nanal
if (fhr .ne. '') then
if (trim(gribtype) .eq. 'p') then
   ingrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grb'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))//'_fhr'//trim(fhr)//'_mem'//trim(adjustl(nanalstr))
else
   ingrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grb_'//trim(adjustl(gribdate))//'_fhr'//trim(fhr)//'_mem'//trim(adjustl(nanalstr))
endif
else
   ingrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grb'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))//'_mem'//trim(adjustl(nanalstr))
end if
call baopenr(lunin, trim(ingrib), iret)
print *,trim(adjustl(ingrib)),iret
!if (nproc .eq. 0) then
jgds=-1; jpds=-1
kskp = 0; nmsg = 0; iret = 0
do while (iret .eq. 0) 
 CALL GETGB(LUNIN,-kskp,nlons*nlats,nmsg,jpds,jgds,NDATA,KSKP,KPDS,KGDS,LBMS,GRID,IRET)
 if (iret .eq. 0) then
    print *,kskp,ndata,minval(grid),maxval(grid),count(.not. lbms),iret
    nmsg = nmsg + 1
 end if
enddo
call baclose(lunin, iret)
!end if
nrecs = nmsg
!endif
print *,'nproc,nrecs',nproc,nrecs
call mpi_bcast(nrecs,1,mpi_integer,0,mpi_comm_world,iret)

!print *,nproc,nrecs,'messages'
! allocate arrays for all pds,gds,data,bitmaps, mean and spread.
allocate(data(nlons,nlats,nrecs))
allocate(bitmaps(nlons,nlats,nrecs))
allocate(bitmapstot(nlons,nlats,nrecs))
allocate(datamean(nlons,nlats,nrecs))
allocate(dataspread(nlons,nlats,nrecs))
allocate(kpdsa(200,nrecs))
allocate(kgdsa(200,nrecs))
! loop over each ensemble member
! for the first time, accumulate quantities used to compute mean and variance.

nanal = nproc + 1
! open file
write(nanalstr,'(i3.3)') nanal
if (fhr .ne. '') then
   if (trim(gribtype) .eq. 'p') then
   ingrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grb'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))//'_fhr'//trim(fhr)//'_mem'//trim(adjustl(nanalstr))
   else
   ingrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grb_'//trim(adjustl(gribdate))//'_fhr'//trim(fhr)//'_mem'//trim(adjustl(nanalstr))
   endif
else
   ingrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grb'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))//'_mem'//trim(adjustl(nanalstr))
end if
call baopenr(lunin+nanal, trim(ingrib), iret)
print *,trim(adjustl(ingrib)),iret
! read all data, bitmaps, pds and gds
jgds=-1; jpds=-1
kskp = 0; nmsg = 0; iret = 0
do while (iret .eq. 0) 
   CALL GETGB(LUNIN+nanal,-kskp,nlons*nlats,nmsg,jpds,jgds,NDATA,KSKP,KPDS,KGDS,LBMS,GRID,IRET)
   if (iret .ne. 0 .and. nmsg+1 .lt. nrecs) then
      print *,'failure reading',trim(adjustl(ingrib)),nmsg+1
      stop
   end if
   if (iret .eq. 0) then
      nmsg = nmsg + 1
      print *,'nanal,nmsg,iret',nanal,nmsg,iret
      data(:,:,nmsg) = grid
      if (nmsg .eq. nrecs) print *,nanal,nmsg,minval(grid),maxval(grid)
      bitmaps(:,:,nmsg) = lbms
      kpdsa(:,nmsg) = kpds
      kgdsa(:,nmsg) = kgds
   end if
enddo
!print *,nmsg,nrecs
! close file.
call baclose(lunin+nanal, iret)
datamean = 0.; dataspread = 0.
call mpi_allreduce(data,datamean,nlons*nlats*nrecs,mpi_real8,mpi_sum,mpi_comm_world,iret)
datamean = datamean/float(nanals)
!if (nproc .eq. 0) then
!   do nmsg=1,nrecs
!   print *,nmsg,'min/max datamean',minval(datamean(:,:,nmsg)),maxval(datamean(:,:,nmsg))
!   enddo
!endif
call mpi_allreduce(bitmaps,bitmapstot,nlons*nlats*nrecs,mpi_logical,mpi_land,mpi_comm_world,iret)
data = (data - datamean)**2
call mpi_allreduce(data,dataspread,nlons*nlats*nrecs,mpi_real8,mpi_sum,mpi_comm_world,iret)
where (dataspread < 1.e-13) dataspread = 1.e-13
dataspread = sqrt(dataspread/float(nanals-1))
!if (nproc .eq. 0) then
!   do nmsg=1,nrecs
!   print *,nmsg,'min/max dataspread',minval(dataspread(:,:,nmsg)),maxval(dataspread(:,:,nmsg))
!   enddo
!endif

! write out on root
if (nproc .eq. 0) then

   !print *,'min/max mean = ',minval(datamean),maxval(datamean)
   !print *,'min/max spread = ',minval(dataspread),maxval(dataspread)
   
   if (fhr .ne. '') then
      outgrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grbensmean'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))//'_fhr'//trim(fhr)
   else
      outgrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grbensmean'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))
   end if
   call baopenwt(lunout, trim(outgrib), iret)
   kens(1) = 1; kens(2) = 5; kens(3) = 0; kens(5) = 255
   kens(4) = 1
   do nmsg=1,nrecs
      grid = datamean(:,:,nmsg)
      lbms = bitmapstot(:,:,nmsg)
      print *,nmsg,'mean',minval(grid),maxval(grid)
      CALL PUTGBE(lunout,nlons*nlats,KPDSA(:,nmsg),KGDSA(:,nmsg),KENS,lbms,grid,IRET)
   enddo
   call baclose(lunout, iret)
   print *,outgrib
   
   if (fhr .ne. '') then
      outgrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grbenssprd'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))//'_fhr'//trim(fhr)
   else
      outgrib = trim(adjustl(datapath))//'/'//trim(adjustl(gribtype))//'grbenssprd'//trim(adjustl(filetype))//'_'//trim(adjustl(gribdate))
   end if
   call baopenwt(lunout+nanal, trim(outgrib), iret)
   kens(4) = 11
   ! increase decimal scale factor by two powers of 10.
   kpdsa(22,:) = kpdsa(22,:)+2
   do nmsg=1,nrecs
      grid = dataspread(:,:,nmsg)
      lbms = bitmapstot(:,:,nmsg)
      print *,nmsg,'sprd',minval(grid),maxval(grid)
      CALL PUTGBE(lunout+nanal,nlons*nlats,KPDSA(:,nmsg),KGDSA(:,nmsg),KENS,lbms,grid,IRET)
   enddo
   call baclose(lunout+nanal, iret)
   print *,outgrib

end if

call mpi_barrier(mpi_comm_world,iret)
if (nproc .eq. 0) write(6,*) 'all done!'

call mpi_finalize(iret)
if (nproc .eq. 0 .and. iret .ne. 0) then
 print *, 'mpi_finalize error status = ',iret
end if

end program gribmean
