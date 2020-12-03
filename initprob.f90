subroutine initprob
  use globaldata
  implicit none
  integer :: i
  do i=-4,4
  prob(-1,i)=exp(2.d0*i/T)
  prob(1,i)=exp(-2.d0*i/T)
  end do
end subroutine
