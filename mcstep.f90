subroutine mcstep
  use globaldata
  implicit none
  integer :: i,s
  integer, external :: field
  real(8), external :: rn
  real(8) :: delta_E
  do i=1, n
  s = int(rn()*n)
  delta_E = 2*spin(s)*field(s)
  if (rn() < prob(spin(s), field(s))) then
    spin(s) = -spin(s)
    E = E + delta_E
  end if
  end do
end subroutine mcstep

