module xc2f_mod
implicit none
private
public :: factors
contains

pure function factors(n) result(out)
! return the positive divisors ("factors") of n in ascending order.
! - n must be positive
! - return value is the factors array (size(...) gives count; empty on error)
use, intrinsic :: iso_fortran_env, only: real64
integer, intent(in) :: n
integer, allocatable :: out(:)
integer :: cnt_small, cnt_total, i, j, k, r
if (n <= 0) then
   allocate(out(0))
   return
end if
cnt_small = 0
cnt_total = 0
k = 0
r = floor(sqrt(real(n, kind=real64)))
do i = 1, r
   if (mod(n, i) == 0) then
      cnt_small = cnt_small + 1
      ! count small divisors (<= sqrt(n)) and total divisors
      cnt_total = cnt_total + merge(1, 2, i == n / i)
   end if
end do
! perfect square
allocate(out(cnt_total))
do i = 1, r
   if (mod(n, i) == 0) then
      out(k+1) = i
      k = k + 1
   end if
end do
do i = r, 1, -1
   if (mod(n, i) == 0) then
      j = n / i
      ! fill ascending:
      ! 1) small divisors increasing
      ! 2) corresponding large divisors decreasing (so overall increasing)
      if (j /= i) then
         out(k+1) = j
         k = k + 1
      end if
   end if
end do
end function factors

end module xc2f_mod

program main
use xc2f_mod, only: factors
implicit none
integer :: n
do n = 1, 10
   write(*,"(i0,a,*(1x,i0))") n, ":", factors(n)
end do
end program main
