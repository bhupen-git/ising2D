!---------64-bit congruential rng---------------------------------!
!    r[n+1] = r[n]*2862933555777941757 + 1013904243 mod(2**64)    !
!  modulo operation is performed by truncation of overflowing     !
!  variables.                                                     ! 
!                                                                 !
!-----------------------------------------------------------------!

real(8) function rn()
        implicit none
        real(8) :: dmu64
        integer(8) :: ran64, mul64, add64
        common/bran64/dmu64, ran64, mul64, add64

        ran64 = ran64*mul64 + add64
        rn = 0.5d0 + dmu64*dble(ran64)

end function

subroutine initran(seed)
        implicit none

        integer(8) :: irmax
        integer(4) :: w, nb, b
        integer, intent(in) :: seed
        real(8) :: dmu64
        integer(8) :: ran64, mul64, add64
        common/bran64/dmu64, ran64, mul64, add64
        ran64=seed
        irmax = 2_8**31
        irmax=2*(irmax**2 - 1) + 1
        mul64 = 2862933555777941757_8
        add64 = 1013904243
        dmu64 = 0.5d0 / dble(irmax)
end subroutine initran

subroutine initseed(seed)
  use globaldata
  integer :: seedsize
  integer, intent(out) :: seed
  call random_seed()
  call random_seed(size=seedsize)
  allocate(seedarray(seedsize))
  call random_seed(get=seedarray)
  seed=abs(seedarray(1))
  deallocate(seedarray)
end subroutine
