program chgdatesigp

 USE SIGIO_MODULE
 implicit none
 TYPE(SIGIO_HEAD) :: sighead
 TYPE(SIGIO_DATA) :: sigdata
 character(len=500) filenamein,filenameout,datapath,fileprefix,fileprefixout
 integer iret,nanals,&
         i,nanal,numproc,nproc,iunit,iunit2
 character(len=3) charnanal,charfhr
 character(len=10) datestring
 integer:: orig_group, new_group, new_comm,idateout(4)
 integer,dimension(:),allocatable:: members
 integer fhour

! mpi definitions.
 include 'mpif.h'

 call MPI_Init(iret)
 ! nproc is process number, numproc is total number of processes.
 call MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
 call MPI_Comm_size(MPI_COMM_WORLD,numproc,iret)

 call getarg(1,datapath)
 call getarg(2,fileprefix)
 call getarg(3,charnanal)
 read(charnanal,'(i3)') nanals
! use this date
 call getarg(4,datestring)
! and this forecast hour
 call getarg(5,charfhr)
 read(charfhr,'(i2)') fhour
 call getarg(6,fileprefixout)

 read(datestring(1:4),'(i4)') idateout(4)
 read(datestring(5:6),'(i2)') idateout(2)
 read(datestring(7:8),'(i2)') idateout(3)
 read(datestring(9:10),'(i2)') idateout(1)

 if (numproc .lt. nanals) then
    print *,numproc,nanals
    print *,'warning, numproc too small!'
    flush(6)
    flush(0)
    call MPI_Abort(MPI_COMM_WORLD,101,iret)
    stop
 end if

 nanal = nproc+1


! Create sub-communicator to handle number of cases (nanals)
 call mpi_comm_group(mpi_comm_world,orig_group,iret)

 allocate(members(nanals))
 do i=1,nanals
    members(i)=i-1
 end do
 if (nanal .le. nanals) then
    call mpi_group_incl(orig_group,nanals,members,new_group,iret)
 endif
 call mpi_comm_create(mpi_comm_world,new_group,new_comm,iret)
 if (iret.ne.0) then
    write(6,*)'***ERROR*** after mpi_comm_create with iret=',iret
    flush(6)
    flush(0)
    call mpi_abort(mpi_comm_world,101,iret)
 endif

 if (nanal.le.nanals) then

 write(charnanal,'(i3.3)') nanal

 filenamein = trim(adjustl(datapath))// &
 trim(adjustl(fileprefix))//'_mem'//charnanal
 filenameout = trim(adjustl(datapath))// &
 trim(adjustl(fileprefixout))//'_mem'//charnanal
 write(6,*)'process nanal=',nanal,&
 ' filenamein=',trim(adjustl(filenamein)),&
 ' filenameout=',trim(adjustl(filenameout))
 write(6,*) 'idateout,fhour',idateout,fhour

! read each ensemble member.

 iunit = 21; iunit2 = 61
 call sigio_srohdc(iunit,trim(filenamein),sighead,sigdata,iret)

! change date and time.

 sighead%idate = idateout
 sighead%fhour = real(fhour)

! write out

 call sigio_swohdc(iunit2,trim(filenameout),sighead,sigdata,iret)

! clean up
 call sigio_axdata(sigdata,iret)
 call sigio_sclose(iunit,iret)
 call sigio_sclose(iunit2,iret)

! Jump here if more mpi processors than files to process
 else
    write(6,*) 'no files to process for mpi task = ',nproc
 endif
   
 call mpi_barrier(mpi_comm_world,iret)
 if (nproc .eq. 0) write(6,*) 'all done!'

 deallocate(members)

 call mpi_finalize(iret)
 if (nproc .eq. 0 .and. iret .ne. 0) then
  print *, 'mpi_finalize error status = ',iret
 end if

end program chgdatesigp
