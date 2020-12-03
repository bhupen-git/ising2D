integer function field(s)
  use globaldata
  implicit none
  integer, intent(in) :: s
  integer :: s1,s2,s3,s4
  s1 = spin((s/l)*l + modulo(s+1, l))
  s2 = spin(modulo(s+l, n))
  s3 = spin((s/l)*l + modulo(s-1, l))
  s4 = spin(modulo(s-l, n))
  field=s1+s2+s3+s4
end function


