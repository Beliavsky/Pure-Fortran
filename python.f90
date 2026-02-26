module python_mod
use, intrinsic :: iso_fortran_env, only: real64
implicit none
private
integer, parameter :: dp = real64

type :: strvec_t
   character(len=:), allocatable :: v(:)
end type strvec_t

public :: isqrt_int       !@pyapi kind=function ret=integer args=x:integer:intent(in) desc="integer square root: return floor(sqrt(x)) for x >= 0"
public :: print_int_list  !@pyapi kind=subroutine args=a:integer(:):intent(in),n:integer:intent(in) desc="print integer list a(1:n) in python-style [..] format"
public :: random_normal_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out) desc="fill x with N(0,1) variates using Box-Muller"
public :: random_choice2 !@pyapi kind=subroutine args=p:real(dp)(:):intent(in),n:integer:intent(in),z:integer(:):intent(out) desc="sample n labels in {0,1} with probabilities p(1:2)"
public :: random_choice_norep !@pyapi kind=subroutine args=npop:integer:intent(in),nsamp:integer:intent(in),z:integer(:):intent(out) desc="sample nsamp unique labels from 0..npop-1 without replacement"
public :: random_choice_prob !@pyapi kind=subroutine args=p:real(dp)(:):intent(in),n:integer:intent(in),z:integer(:):intent(out) desc="sample n labels in 0..size(p)-1 with probabilities p"
public :: sort_real_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(inout) desc="sort real vector x in ascending order"
public :: argsort_real !@pyapi kind=subroutine args=x:real(dp)(:):intent(in),idx:integer(:):intent(out) desc="argsort indices (0-based) of real vector"
public :: mean_1d !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in) desc="mean of 1D real vector"
public :: var_1d !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in),ddof:integer:intent(in):optional desc="variance of 1D real vector with optional ddof (numpy-style)"
public :: strvec_t !@pyapi kind=type desc="string vector helper type"
public :: to_lower !@pyapi kind=function ret=character args=s:character:intent(in) desc="lowercase string"
public :: to_upper !@pyapi kind=function ret=character args=s:character:intent(in) desc="uppercase string"
public :: str_strip !@pyapi kind=function ret=character args=s:character:intent(in),chars:character:intent(in):optional desc="strip leading/trailing characters"
public :: str_lstrip !@pyapi kind=function ret=character args=s:character:intent(in),chars:character:intent(in):optional desc="strip leading characters"
public :: str_rstrip !@pyapi kind=function ret=character args=s:character:intent(in),chars:character:intent(in):optional desc="strip trailing characters"
public :: starts_with !@pyapi kind=function ret=logical args=s:character:intent(in),prefix:character:intent(in) desc="prefix test"
public :: ends_with !@pyapi kind=function ret=logical args=s:character:intent(in),suffix:character:intent(in) desc="suffix test"
public :: str_find !@pyapi kind=function ret=integer args=s:character:intent(in),sub:character:intent(in) desc="0-based find index or -1"
public :: str_rfind !@pyapi kind=function ret=integer args=s:character:intent(in),sub:character:intent(in) desc="0-based reverse find index or -1"
public :: str_replace !@pyapi kind=function ret=character args=s:character:intent(in),old:character:intent(in),new:character:intent(in) desc="replace all occurrences"
public :: str_split !@pyapi kind=function ret=type(strvec_t) args=s:character:intent(in),sep:character:intent(in):optional desc="split into string vector"
public :: str_join !@pyapi kind=function ret=character args=sep:character:intent(in),items:type(strvec_t):intent(in) desc="join string vector with separator"
public :: str_count !@pyapi kind=function ret=integer args=s:character:intent(in),sub:character:intent(in) desc="count non-overlapping occurrences"
public :: str_isdigit !@pyapi kind=function ret=logical args=s:character:intent(in) desc="true when all chars are digits"
public :: str_isalpha !@pyapi kind=function ret=logical args=s:character:intent(in) desc="true when all chars are letters"
public :: str_isalnum !@pyapi kind=function ret=logical args=s:character:intent(in) desc="true when all chars are alnum"
public :: str_isspace !@pyapi kind=function ret=logical args=s:character:intent(in) desc="true when all chars are whitespace"

contains

      pure integer function isqrt_int(x)
         ! integer square root: return floor(sqrt(x)) for x >= 0
         implicit none
         integer, intent(in) :: x  ! input integer (x >= 0 expected)
         integer :: r
         if (x <= 0) then
            isqrt_int = 0
            return
         end if
         r = int(sqrt(real(x, kind=dp)))
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

      subroutine random_normal_vec(x)
         ! fill x with N(0,1) variates using Box-Muller
         implicit none
         real(kind=dp), intent(out) :: x(:)
         integer :: i, n
         real(kind=dp) :: u1, u2, rad, theta
         real(kind=dp), parameter :: two_pi = 2.0_dp * acos(-1.0_dp)
         n = size(x)
         i = 1
         do while (i <= n)
            call random_number(u1)
            call random_number(u2)
            if (u1 <= tiny(1.0_dp)) cycle
            rad = sqrt(-2.0_dp * log(u1))
            theta = two_pi * u2
            x(i) = rad * cos(theta)
            if (i + 1 <= n) x(i + 1) = rad * sin(theta)
            i = i + 2
         end do
      end subroutine random_normal_vec

      subroutine random_choice2(p, n, z)
         ! sample n labels in {0,1} with probabilities p(1:2)
         implicit none
         real(kind=dp), intent(in) :: p(:)
         integer, intent(in) :: n
         integer, intent(out) :: z(:)
         integer :: i
         real(kind=dp) :: u, p1, s
         if (size(z) < n) stop "random_choice2: output array too small"
         if (size(p) < 2) stop "random_choice2: p must have at least 2 elements"
         p1 = max(0.0_dp, p(1))
         s = max(0.0_dp, p(1)) + max(0.0_dp, p(2))
         if (s > tiny(1.0_dp)) then
            p1 = p1 / s
         else
            p1 = 0.5_dp
         end if
         do i = 1, n
            call random_number(u)
            if (u < p1) then
               z(i) = 0
            else
               z(i) = 1
            end if
         end do
      end subroutine random_choice2

      subroutine random_choice_norep(npop, nsamp, z)
         integer, intent(in) :: npop, nsamp
         integer, intent(out) :: z(:)
         integer :: i, j, tmp
         real(kind=dp) :: u
         integer, allocatable :: pool(:)
         if (npop <= 0 .or. nsamp < 0) stop "random_choice_norep: invalid sizes"
         if (nsamp > npop) stop "random_choice_norep: nsamp > npop"
         if (size(z) < nsamp) stop "random_choice_norep: output array too small"
         allocate(pool(1:npop))
         do i = 1, npop
            pool(i) = i - 1
         end do
         do i = 1, nsamp
            call random_number(u)
            j = i + int(u * real(npop - i + 1, kind=dp))
            if (j < i) j = i
            if (j > npop) j = npop
            tmp = pool(i)
            pool(i) = pool(j)
            pool(j) = tmp
            z(i) = pool(i)
         end do
         if (allocated(pool)) deallocate(pool)
      end subroutine random_choice_norep

      subroutine random_choice_prob(p, n, z)
         real(kind=dp), intent(in) :: p(:)
         integer, intent(in) :: n
         integer, intent(out) :: z(:)
         integer :: i, j, k
         real(kind=dp) :: u, s
         real(kind=dp), allocatable :: cdf(:)
         k = size(p)
         if (k <= 0) stop "random_choice_prob: empty probability vector"
         if (size(z) < n) stop "random_choice_prob: output array too small"
         allocate(cdf(1:k))
         s = 0.0_dp
         do j = 1, k
            s = s + max(0.0_dp, p(j))
            cdf(j) = s
         end do
         if (s <= tiny(1.0_dp)) then
            do j = 1, k
               cdf(j) = real(j, kind=dp) / real(k, kind=dp)
            end do
         else
            cdf = cdf / s
         end if
         do i = 1, n
            call random_number(u)
            z(i) = k - 1
            do j = 1, k
               if (u <= cdf(j)) then
                  z(i) = j - 1
                  exit
               end if
            end do
         end do
         if (allocated(cdf)) deallocate(cdf)
      end subroutine random_choice_prob

      pure subroutine sort_real_vec(x)
         ! sort real vector x in ascending order
         implicit none
         real(kind=dp), intent(inout) :: x(:)
         integer :: i, j, n
         real(kind=dp) :: key
         n = size(x)
         do i = 2, n
            key = x(i)
            j = i - 1
            do while (j >= 1)
               if (x(j) <= key) exit
               x(j+1) = x(j)
               j = j - 1
            end do
            x(j+1) = key
         end do
      end subroutine sort_real_vec

      subroutine argsort_real(x, idx)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(out) :: idx(:)
         integer :: i, j, n, key
         n = size(x)
         if (size(idx) < n) stop "argsort_real: output array too small"
         do i = 1, n
            idx(i) = i - 1
         end do
         do i = 2, n
            key = idx(i)
            j = i - 1
            do while (j >= 1)
               if (x(idx(j)+1) <= x(key+1)) exit
               idx(j+1) = idx(j)
               j = j - 1
            end do
            idx(j+1) = key
         end do
      end subroutine argsort_real

      pure real(kind=dp) function mean_1d(x)
         real(kind=dp), intent(in) :: x(:)
         if (size(x) <= 0) then
            mean_1d = 0.0_dp
         else
            mean_1d = sum(x) / real(size(x), kind=dp)
         end if
      end function mean_1d

      pure real(kind=dp) function var_1d(x, ddof)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in), optional :: ddof
         integer :: n, d
         real(kind=dp) :: mu
         n = size(x)
         if (present(ddof)) then
            d = ddof
         else
            d = 0
         end if
         if (n <= d .or. n <= 0) then
            var_1d = 0.0_dp
            return
         end if
         mu = mean_1d(x)
         var_1d = sum((x - mu)**2) / real(n - d, kind=dp)
      end function var_1d

      pure character(len=len(s)) function to_lower(s)
         character(len=*), intent(in) :: s
         integer :: i, k
         do i = 1, len(s)
            k = iachar(s(i:i))
            if (k >= iachar("A") .and. k <= iachar("Z")) then
               to_lower(i:i) = achar(k + 32)
            else
               to_lower(i:i) = s(i:i)
            end if
         end do
      end function to_lower

      pure character(len=len(s)) function to_upper(s)
         character(len=*), intent(in) :: s
         integer :: i, k
         do i = 1, len(s)
            k = iachar(s(i:i))
            if (k >= iachar("a") .and. k <= iachar("z")) then
               to_upper(i:i) = achar(k - 32)
            else
               to_upper(i:i) = s(i:i)
            end if
         end do
      end function to_upper

      pure logical function char_in_set(ch, set_chars)
         character(len=1), intent(in) :: ch
         character(len=*), intent(in) :: set_chars
         integer :: i
         char_in_set = .false.
         do i = 1, len(set_chars)
            if (ch == set_chars(i:i)) then
               char_in_set = .true.
               return
            end if
         end do
      end function char_in_set

      function str_lstrip(s, chars) result(out)
         character(len=*), intent(in) :: s
         character(len=*), intent(in), optional :: chars
         character(len=:), allocatable :: out
         character(len=:), allocatable :: set_chars
         integer :: i, n
         n = len(s)
         if (present(chars)) then
            set_chars = chars
         else
            set_chars = " "
         end if
         i = 1
         do while (i <= n .and. char_in_set(s(i:i), set_chars))
            i = i + 1
         end do
         if (i > n) then
            out = ""
         else
            out = s(i:n)
         end if
      end function str_lstrip

      function str_rstrip(s, chars) result(out)
         character(len=*), intent(in) :: s
         character(len=*), intent(in), optional :: chars
         character(len=:), allocatable :: out
         character(len=:), allocatable :: set_chars
         integer :: j
         if (present(chars)) then
            set_chars = chars
         else
            set_chars = " "
         end if
         j = len(s)
         do while (j >= 1 .and. char_in_set(s(j:j), set_chars))
            j = j - 1
         end do
         if (j < 1) then
            out = ""
         else
            out = s(1:j)
         end if
      end function str_rstrip

      function str_strip(s, chars) result(out)
         character(len=*), intent(in) :: s
         character(len=*), intent(in), optional :: chars
         character(len=:), allocatable :: out
         character(len=:), allocatable :: tmp
         if (present(chars)) then
            tmp = str_lstrip(s, chars)
            out = str_rstrip(tmp, chars)
         else
            tmp = str_lstrip(s)
            out = str_rstrip(tmp)
         end if
      end function str_strip

      pure logical function starts_with(s, prefix)
         character(len=*), intent(in) :: s, prefix
         integer :: n
         n = len(prefix)
         if (n == 0) then
            starts_with = .true.
         else if (len(s) < n) then
            starts_with = .false.
         else
            starts_with = (s(1:n) == prefix)
         end if
      end function starts_with

      pure logical function ends_with(s, suffix)
         character(len=*), intent(in) :: s, suffix
         integer :: n, ls
         n = len(suffix)
         ls = len(s)
         if (n == 0) then
            ends_with = .true.
         else if (ls < n) then
            ends_with = .false.
         else
            ends_with = (s(ls - n + 1:ls) == suffix)
         end if
      end function ends_with

      pure integer function str_find(s, sub)
         character(len=*), intent(in) :: s, sub
         integer :: p
         if (len(sub) == 0) then
            str_find = 0
            return
         end if
         p = index(s, sub)
         if (p <= 0) then
            str_find = -1
         else
            str_find = p - 1
         end if
      end function str_find

      pure integer function str_rfind(s, sub)
         character(len=*), intent(in) :: s, sub
         integer :: p
         if (len(sub) == 0) then
            str_rfind = len(s)
            return
         end if
         p = index(s, sub, back=.true.)
         if (p <= 0) then
            str_rfind = -1
         else
            str_rfind = p - 1
         end if
      end function str_rfind

      function str_replace(s, old, new) result(out)
         character(len=*), intent(in) :: s, old, new
         character(len=:), allocatable :: out
         character(len=:), allocatable :: acc
         integer :: p, pos, lo
         if (len(old) == 0) then
            out = s
            return
         end if
         acc = ""
         pos = 1
         lo = len(old)
         do
            p = index(s(pos:), old)
            if (p <= 0) exit
            acc = acc // s(pos:pos + p - 2) // new
            pos = pos + p - 1 + lo
            if (pos > len(s)) exit
         end do
         if (pos <= len(s)) then
            out = acc // s(pos:)
         else
            out = acc
         end if
      end function str_replace

      subroutine append_strvec(items, tok)
         type(strvec_t), intent(inout) :: items
         character(len=*), intent(in) :: tok
         character(len=:), allocatable :: tmp(:)
         integer :: n, lnew, i
         if (.not. allocated(items%v)) then
            allocate(character(len=len(tok)) :: items%v(1))
            items%v(1) = tok
            return
         end if
         n = size(items%v)
         lnew = len(tok)
         do i = 1, n
            lnew = max(lnew, len(items%v(i)))
         end do
         allocate(character(len=lnew) :: tmp(n + 1))
         do i = 1, n
            tmp(i) = items%v(i)
         end do
         tmp(n + 1) = tok
         call move_alloc(tmp, items%v)
      end subroutine append_strvec

      function str_split(s, sep) result(out)
         character(len=*), intent(in) :: s
         character(len=*), intent(in), optional :: sep
         type(strvec_t) :: out
         character(len=:), allocatable :: d
         integer :: p, pos, ld, i, j
         if (present(sep)) then
            d = sep
         else
            d = " "
         end if
         if (len(d) == 0) then
            call append_strvec(out, s)
            return
         end if
         if (.not. present(sep)) then
            i = 1
            do while (i <= len(s))
               do while (i <= len(s) .and. (s(i:i) == " " .or. s(i:i) == achar(9)))
                  i = i + 1
               end do
               if (i > len(s)) exit
               j = i
               do while (j <= len(s) .and. .not. (s(j:j) == " " .or. s(j:j) == achar(9)))
                  j = j + 1
               end do
               call append_strvec(out, s(i:j - 1))
               i = j + 1
            end do
            return
         end if
         pos = 1
         ld = len(d)
         do
            p = index(s(pos:), d)
            if (p <= 0) exit
            call append_strvec(out, s(pos:pos + p - 2))
            pos = pos + p - 1 + ld
            if (pos > len(s) + 1) exit
         end do
         if (pos <= len(s) + 1) call append_strvec(out, s(pos:))
      end function str_split

      function str_join(sep, items) result(out)
         character(len=*), intent(in) :: sep
         type(strvec_t), intent(in) :: items
         character(len=:), allocatable :: out
         integer :: i, n
         out = ""
         if (.not. allocated(items%v)) return
         n = size(items%v)
         if (n <= 0) return
         out = items%v(1)
         do i = 2, n
            out = out // sep // items%v(i)
         end do
      end function str_join

      pure integer function str_count(s, sub)
         character(len=*), intent(in) :: s, sub
         integer :: p, pos, ls
         if (len(sub) == 0) then
            str_count = len(s) + 1
            return
         end if
         str_count = 0
         pos = 1
         ls = len(sub)
         do
            p = index(s(pos:), sub)
            if (p <= 0) exit
            str_count = str_count + 1
            pos = pos + p - 1 + ls
            if (pos > len(s)) exit
         end do
      end function str_count

      pure logical function str_isdigit(s)
         character(len=*), intent(in) :: s
         integer :: i, k
         if (len(s) <= 0) then
            str_isdigit = .false.
            return
         end if
         do i = 1, len(s)
            k = iachar(s(i:i))
            if (k < iachar("0") .or. k > iachar("9")) then
               str_isdigit = .false.
               return
            end if
         end do
         str_isdigit = .true.
      end function str_isdigit

      pure logical function str_isalpha(s)
         character(len=*), intent(in) :: s
         integer :: i, k
         if (len(s) <= 0) then
            str_isalpha = .false.
            return
         end if
         do i = 1, len(s)
            k = iachar(s(i:i))
            if (.not. ((k >= iachar("A") .and. k <= iachar("Z")) .or. (k >= iachar("a") .and. k <= iachar("z")))) then
               str_isalpha = .false.
               return
            end if
         end do
         str_isalpha = .true.
      end function str_isalpha

      pure logical function str_isalnum(s)
         character(len=*), intent(in) :: s
         integer :: i, k
         if (len(s) <= 0) then
            str_isalnum = .false.
            return
         end if
         do i = 1, len(s)
            k = iachar(s(i:i))
            if (.not. ((k >= iachar("0") .and. k <= iachar("9")) .or. &
                       (k >= iachar("A") .and. k <= iachar("Z")) .or. &
                       (k >= iachar("a") .and. k <= iachar("z")))) then
               str_isalnum = .false.
               return
            end if
         end do
         str_isalnum = .true.
      end function str_isalnum

      pure logical function str_isspace(s)
         character(len=*), intent(in) :: s
         integer :: i, k
         if (len(s) <= 0) then
            str_isspace = .false.
            return
         end if
         do i = 1, len(s)
            k = iachar(s(i:i))
            if (.not. (k == 32 .or. k == 9 .or. k == 10 .or. k == 11 .or. k == 12 .or. k == 13)) then
               str_isspace = .false.
               return
            end if
         end do
         str_isspace = .true.
      end function str_isspace

end module python_mod
