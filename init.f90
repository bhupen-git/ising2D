subroutine init
  use globaldata
  implicit none
  integer :: seed
  L=16
  N=L*L
  mcs=10**4
  allocate(spin(0:N-1))
  allocate(prob(-1:1,-4:4))
  call initmpi
  call initseed(seed)
  call initran(seed)
  call initspin
  call initE
  allocate(buffdata(0:numtasks-1))
  buffdata(:)=0.d0
  allocate(Edata(0:numtasks-1))
  Edata(:)=0.d0
  allocate(Tarray(0:numtasks-1))
end subroutine

subroutine initspin
  use globaldata
  implicit none
  integer :: i
  real(8), external :: rn
  do i=0,N-1
    spin(i) = 2*int(2.d0*rn()) - 1
  end do
end subroutine

subroutine initE
  use globaldata
  implicit none
  integer, external :: field
  integer :: i
  E=0.d0
  do i=0,N-1
    E=E - spin(i)*field(i)
  end do
end subroutine

subroutine initmpi
  use globaldata
  implicit none
  call mpi_init(ierror)
  call mpi_comm_rank(mpi_comm_world,myrank,ierror)
  call mpi_comm_size(mpi_comm_world,numtasks,ierror)
  allocate(stats(mpi_status_size))
  allocate(collect(0:numtasks-1))
  sub_s=myrank
end subroutine

subroutine inittemp
  use globaldata
  implicit none
  T = 2.d0 + 0.1*myrank
end subroutine
