module globaldata
  implicit none
  include 'mpif.h'
  integer :: N,L,mcs,numtasks,myrank,ierror,sub_s
  real(8) :: E,T
  integer, allocatable :: spin(:),stats(:),collect(:),seedarray(:)
  real(8), allocatable :: prob(:,:),buffdata(:),Edata(:),Tarray(:)
end module
