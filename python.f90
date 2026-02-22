module python_mod
implicit none
private

public :: isqrt_int       !@pyapi kind=function ret=integer args=x:integer:intent(in) desc="integer square root: return floor(sqrt(x)) for x >= 0"
public :: print_int_list  !@pyapi kind=subroutine args=a:integer(:):intent(in),n:integer:intent(in) desc="print integer list a(1:n) in python-style [..] format"

contains

      integer function isqrt_int(x)
         ! integer square root: return floor(sqrt(x)) for x >= 0
         implicit none
         integer, intent(in) :: x  ! input integer (x >= 0 expected)
         integer :: r
         if (x <= 0) then
            isqrt_int = 0
            return
         end if
         r = int(sqrt(real(x)))
         do while ((r+1)*(r+1) <= x)
            r = r + 1
         end do
         do while (r*r > x)
            r = r - 1
         end do
         isqrt_int = r
      end function isqrt_int

      subroutine print_int_list(a, n)
         ! print integer list a(1:n) in python-style [..] format
         implicit none
         integer, intent(in) :: a(:)  ! array containing values to print
         integer, intent(in) :: n     ! number of elements from a to print
         integer :: j
         if (n <= 0) then
            write(*,'(a)') '[]'
            return
         end if
         write(*,'(a)', advance='no') '['
         do j = 1, n
            if (j > 1) write(*,'(a)', advance='no') ', '
            write(*,'(i0)', advance='no') a(j)
         end do
         write(*,'(a)') ']'
      end subroutine print_int_list

end module python_mod
