program main
  use globaldata
  implicit none
  integer :: i,j,comm
  real(8) :: Eavg=0.d0
  call init
  call inittemp
  call initprob
  do i=1,10**4
  call mcstep
  end do
  do i=1,mcs
  call mcstep
  call swaprandom
  Eavg=Eavg+E
  end do
  write(*,*)T, Eavg/(N*mcs)
  call mpi_finalize(ierror)
end program
