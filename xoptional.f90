module m
implicit none
contains
function max2(x1, x2) result(xmax)
real, intent(in) :: x1
real, intent(in), optional :: x2
real :: xmax
if (present(x2)) then
   xmax = max(x1, x2)
else
   xmax = x1
end if
end function max2

function max_(x1, x2, x3, x4, x5) result(xmax)
real, intent(in) :: x1, x2
real, optional, intent(in) :: x3, x4, x5
real :: xmax
xmax = max(x1, x2)
if (present(x3)) xmax = max(x3, xmax)
xmax = max2(xmax, x4) ! allowed since 2nd argument of max2 is OPTIONAL
xmax = max(xmax, x5)  ! unsafe since x5 may not be PRESENT
end function max_
end module m

use m
print*,max_(3.2, 2.6)
print*,max_(3.2, 2.6, 4.5)
end
