module kind_mod
use, intrinsic :: iso_fortran_env, only: real64
implicit none
integer, parameter :: dp = real64
end module kind_mod

module xc2f_mod
use kind_mod, only: dp
implicit none
private
public :: matmul_f
contains

pure subroutine matmul_f(m, n, k, A, B, C)
! Multiply C = A(m x k) * B(k x n), row-major storage.
integer, intent(in) :: m, n, k
real(kind=dp), intent(in) :: A(:), B(:)
real(kind=dp), intent(inout) :: C(:)
integer :: i, j
real(kind=dp) :: s
integer :: t
do i = 0, m-1
   do j = 0, n-1
      s = 0.0
      do t = 0, k-1
         s = s + A(i * k + t+1) * B(t * n + j+1)
      end do
      C(i * n + j+1) = s
   end do
end do
end subroutine matmul_f

end module xc2f_mod

program main
use xc2f_mod, only: matmul_f
use kind_mod, only: dp
implicit none
integer :: i
integer, parameter :: k = 3, m = 2, n = 2
real(kind=dp), allocatable :: A(:), B(:), C(:)
allocate(C(int(m * n)))
! A = [1 2 3; 4 5 6], B = [7 8; 9 10; 11 12]
a = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
b = [7.0, 8.0, 9.0, 10.0, 11.0, 12.0]
call matmul_f(m, n, k, A, B, C)
! unsupported printf
do i = 0, m * n-1
   write(*,"(a,i0)", advance="no") " ", int(C(i+1))
end do
write(*,*)
end program main
