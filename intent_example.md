`python xintent.py m_stats.f90 --out m_stats_intent.f90 --suggest-intent-out --suggest-intent-inout`
transforms

```fortran
  module m_stats
    implicit none
  contains

    subroutine circle(calc, radius, area)
    character (len=*) :: calc
    real :: radius, area ! cannot set INTENT for these since they can be inputs or outputs
    real, parameter :: pi = 3.1416
    if (calc == "radius") then
       radius = sqrt(area/pi)
    else if (calc == "area") then
       area = pi*radius**2
    else
       radius = -1.0
       area = -1.0
    end if
    end subroutine circle

    subroutine smooth(alpha, x)
      real :: alpha
      real :: x(:)
      integer :: i
      do i=2,size(x)
         x(i) = alpha*x(i) + (1-alpha)*x(i-1) 
      end do
    end subroutine smooth

    subroutine center_scale(n, x, mu, sigma, y)
      integer :: n, i
      real    :: x(n), y(n), mu, sigma
      do i = 1, n
        y(i) = (x(i) - mu) / sigma
      end do
    end subroutine center_scale

    subroutine mean_std(n, a, mu, sigma)
      integer :: n, i
      real    :: a(n), mu, sigma
      real    :: s, ss

      s = 0.0
      do i = 1, n
        s = s + a(i)
      end do
      mu = s / n

      ss = 0.0
      do i = 1, n
        ss = ss + (a(i) - mu)**2
      end do
      sigma = sqrt(ss / n)
    end subroutine mean_std

    subroutine zscore_sum(n, x, zsum)
      integer :: n, i
      real    :: x(n), zsum
      real    :: mu, sigma, zi
      call mean_std(n, x, mu, sigma)
      zsum = 0.0
      do i = 1, n
        zi = (x(i) - mu) / sigma
        zsum = zsum + zi
      end do
    end subroutine zscore_sum

  end module m_stats
```
to
```fortran
  module m_stats
    implicit none
  contains

    subroutine circle(calc, radius, area)
    character (len=*), intent(in) :: calc
    real :: radius, area ! cannot set INTENT for these since they can be inputs or outputs
    real, parameter :: pi = 3.1416
    if (calc == "radius") then
       radius = sqrt(area/pi)
    else if (calc == "area") then
       area = pi*radius**2
    else
       radius = -1.0
       area = -1.0
    end if
    end subroutine circle

    subroutine smooth(alpha, x)
      real, intent(in) :: alpha
      real, intent(in out) :: x(:)
      integer :: i
      do i=2,size(x)
         x(i) = alpha*x(i) + (1-alpha)*x(i-1) 
      end do
    end subroutine smooth

    subroutine center_scale(n, x, mu, sigma, y)
      integer, intent(in) :: n
      real, intent(in) :: x(n)
      real, intent(in) :: mu
      real, intent(in) :: sigma
      real, intent(out) :: y(n)
      integer :: i
      do i = 1, n
        y(i) = (x(i) - mu) / sigma
      end do
    end subroutine center_scale

    subroutine mean_std(n, a, mu, sigma)
      integer, intent(in) :: n
      real, intent(in) :: a(n)
      real, intent(out) :: mu
      real, intent(out) :: sigma
      integer :: i
      real    :: s, ss

      s = 0.0
      do i = 1, n
        s = s + a(i)
      end do
      mu = s / n

      ss = 0.0
      do i = 1, n
        ss = ss + (a(i) - mu)**2
      end do
      sigma = sqrt(ss / n)
    end subroutine mean_std

    subroutine zscore_sum(n, x, zsum)
      integer :: n, i
      real :: x(n)
      real, intent(out) :: zsum
      real    :: mu, sigma, zi
      call mean_std(n, x, mu, sigma)
      zsum = 0.0
      do i = 1, n
        zi = (x(i) - mu) / sigma
        zsum = zsum + zi
      end do
    end subroutine zscore_sum

  end module m_stats
```
