!-------------------------------------------------------------------------------
subroutine rsearch1(km1,z1,km2,z2,l2)
!$$$  subprogram documentation block
!
! subprogram:    rsearch1    search for a surrounding real interval
!   prgmmr: iredell    org: w/nmc23     date: 98-05-01
!
! abstract: this subprogram searches a monotonic sequences of real numbers
!   for intervals that surround a given search set of real numbers.
!   the sequences may be monotonic in either direction; the real numbers
!   may be single or double precision.
!
! program history log:
! 1999-01-05  mark iredell
!
! usage:    call rsearch1(km1,z1,km2,z2,l2)
!   input argument list:
!     km1    integer number of points in the sequence
!     z1     real (km1) sequence values to search
!            (z1 must be monotonic in either direction)
!     km2    integer number of points to search for
!     z2     real (km2) set of values to search for
!            (z2 need not be monotonic)
!     
!   output argument list:
!     l2     integer (km2) interval locations from 0 to km1
!            (z2 will be between z1(l2) and z1(l2+1))
!
! subprograms called:
!   sbsrch essl binary search
!   dbsrch essl binary search
!
! remarks:
!   returned values of 0 or km1 indicate that the given search value
!   is outside the range of the sequence.
!
!   if a search value is identical to one of the sequence values
!   then the location returned points to the identical value.
!   if the sequence is not strictly monotonic and a search value is
!   identical to more than one of the sequence values, then the
!   location returned may point to any of the identical values.
!
!   if l2(k)=0, then z2(k) is less than the start point z1(1)
!   for ascending sequences (or greater than for descending sequences).
!   if l2(k)=km1, then z2(k) is greater than or equal to the end point
!   z1(km1) for ascending sequences (or less than or equal to for
!   descending sequences).  otherwise z2(k) is between the values
!   z1(l2(k)) and z1(l2(k+1)) and may equal the former.
!
! attributes:
!   language: fortran
!
!$$$
  implicit none
  integer,intent(in):: km1,km2
  real,intent(in):: z1(km1),z2(km2)
  integer,intent(out):: l2(km2)
  integer k1,k2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  find the surrounding input interval for each output point.
  if(z1(1).le.z1(km1)) then
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  input coordinate is monotonically ascending.
    do k2=1,km2
      l2(k2)=km1
      do k1=1,km1
        if(z1(k1).gt.z2(k2)) then 
          l2(k2)=k1-1
          exit
        endif
      enddo
    enddo
  else
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  input coordinate is monotonically descending.
    do k2=1,km2
      l2(k2)=km1
      do k1=1,km1
        if(z1(k1).lt.z2(k2)) then 
          l2(k2)=k1-1
          exit
        endif
      enddo
    enddo
  endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
end subroutine
