module python_mod
use, intrinsic :: iso_fortran_env, only: real64
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
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
public :: sort_int_vec !@pyapi kind=subroutine args=x:integer(:):intent(inout) desc="sort integer vector x in ascending order"
public :: argsort_real !@pyapi kind=subroutine args=x:real(dp)(:):intent(in),idx:integer(:):intent(out) desc="argsort indices (0-based) of real vector"
public :: argsort_int !@pyapi kind=subroutine args=x:integer(:):intent(in),idx:integer(:):intent(out) desc="argsort indices (0-based) of integer vector"
public :: arange_int !@pyapi kind=function ret=integer(:) args=start:integer:intent(in),stop:integer:intent(in),step:integer:intent(in) desc="integer arange(start, stop, step)"
public :: logspace !@pyapi kind=function ret=real(dp)(:) args=start:real(dp):intent(in),stop:real(dp):intent(in),num:integer:intent(in):optional,endpoint:logical:intent(in):optional,base:real(dp):intent(in):optional desc="logspace(start, stop, num=50, endpoint=True, base=10)"
public :: geomspace !@pyapi kind=function ret=real(dp)(:) args=start:real(dp):intent(in),stop:real(dp):intent(in),num:integer:intent(in):optional,endpoint:logical:intent(in):optional desc="geomspace(start, stop, num=50, endpoint=True)"
public :: mean_1d !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in) desc="mean of 1D real vector"
public :: var_1d !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in),ddof:integer:intent(in):optional desc="variance of 1D real vector with optional ddof (numpy-style)"
public :: zeros_real !@pyapi kind=function ret=real(dp)(:) args=n:integer:intent(in) desc="allocate and return length-n real array initialized to 0"
public :: ones_real !@pyapi kind=function ret=real(dp)(:) args=n:integer:intent(in) desc="allocate and return length-n real array initialized to 1"
public :: zeros_int !@pyapi kind=function ret=integer(:) args=n:integer:intent(in) desc="allocate and return length-n integer array initialized to 0"
public :: ones_int !@pyapi kind=function ret=integer(:) args=n:integer:intent(in) desc="allocate and return length-n integer array initialized to 1"
public :: zeros_logical !@pyapi kind=function ret=logical(:) args=n:integer:intent(in) desc="allocate and return length-n logical array initialized to .false."
public :: ones_logical !@pyapi kind=function ret=logical(:) args=n:integer:intent(in) desc="allocate and return length-n logical array initialized to .true."
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

public :: cumsum_real !@pyapi kind=function ret=real(dp)(:) args=x:real(dp)(:):intent(in) desc="cumulative sum of real vector"
public :: unique_int !@pyapi kind=function ret=integer(:) args=x:integer(:):intent(in) desc="sorted unique values of integer vector"
public :: tile_int !@pyapi kind=function ret=integer(:) args=x:integer(:):intent(in),reps:integer:intent(in) desc="tile integer vector reps times"
public :: tile_int_2d !@pyapi kind=function ret=integer(:,:) args=x:integer(:,:):intent(in),reps0:integer:intent(in),reps1:integer:intent(in) desc="tile integer matrix reps0 x reps1 times"
public :: diag_from_vec_int !@pyapi kind=function ret=integer(:,:) args=v:integer(:):intent(in) desc="return diagonal matrix from integer vector"
public :: cumprod_int !@pyapi kind=function ret=integer(:) args=x:integer(:):intent(in) desc="cumulative product of integer vector"
public :: repeat_int !@pyapi kind=function ret=integer(:) args=x:integer(:):intent(in),reps:integer:intent(in) desc="repeat each integer element reps times"
public :: repeat_int_axis0_2d !@pyapi kind=function ret=integer(:,:) args=x:integer(:,:):intent(in),reps:integer:intent(in) desc="repeat rows of integer matrix reps times"
public :: repeat_int_axis1_2d !@pyapi kind=function ret=integer(:,:) args=x:integer(:,:):intent(in),reps:integer:intent(in) desc="repeat columns of integer matrix reps times"
public :: diag_from_mat_real !@pyapi kind=function ret=real(dp)(:) args=a:real(dp)(:,:):intent(in) desc="return main diagonal of real matrix"
public :: cumsum_int !@pyapi kind=function ret=integer(:) args=x:integer(:):intent(in) desc="cumulative sum of integer vector"
public :: diag_from_vec_real !@pyapi kind=function ret=real(dp)(:,:) args=v:real(dp)(:):intent(in) desc="return diagonal matrix from real vector"
public :: repeat_real !@pyapi kind=function ret=real(dp)(:) args=x:real(dp)(:):intent(in),reps:integer:intent(in) desc="repeat each real element reps times"
public :: repeat_real_axis0_2d !@pyapi kind=function ret=real(dp)(:,:) args=x:real(dp)(:,:):intent(in),reps:integer:intent(in) desc="repeat rows of real matrix reps times"
public :: repeat_real_axis1_2d !@pyapi kind=function ret=real(dp)(:,:) args=x:real(dp)(:,:):intent(in),reps:integer:intent(in) desc="repeat columns of real matrix reps times"
public :: repeat_logical !@pyapi kind=function ret=logical(:) args=x:logical(:):intent(in),reps:integer:intent(in) desc="repeat each logical element reps times"
public :: repeat_logical_axis0_2d !@pyapi kind=function ret=logical(:,:) args=x:logical(:,:):intent(in),reps:integer:intent(in) desc="repeat rows of logical matrix reps times"
public :: repeat_logical_axis1_2d !@pyapi kind=function ret=logical(:,:) args=x:logical(:,:):intent(in),reps:integer:intent(in) desc="repeat columns of logical matrix reps times"
public :: diag_from_mat_int !@pyapi kind=function ret=integer(:) args=a:integer(:,:):intent(in) desc="return main diagonal of integer matrix"
public :: tile_real !@pyapi kind=function ret=real(dp)(:) args=x:real(dp)(:):intent(in),reps:integer:intent(in) desc="tile real vector reps times"
public :: tile_real_2d !@pyapi kind=function ret=real(dp)(:,:) args=x:real(dp)(:,:):intent(in),reps0:integer:intent(in),reps1:integer:intent(in) desc="tile real matrix reps0 x reps1 times"
public :: eye_real !@pyapi kind=function ret=real(dp)(:,:) args=n:integer:intent(in),m:integer:intent(in):optional desc="return n x m identity-like matrix (default m=n)"
public :: unique_real !@pyapi kind=function ret=real(dp)(:) args=x:real(dp)(:):intent(in) desc="sorted unique values of real vector"
public :: bincount_int !@pyapi kind=function ret=integer(:) args=x:integer(:):intent(in),minlength:integer:intent(in):optional desc="count occurrences of nonnegative integers"
public :: searchsorted_left_int !@pyapi kind=function ret=integer(:) args=a:integer(:):intent(in),v:integer(:):intent(in) desc="searchsorted left indices for integer vectors"
public :: searchsorted_right_int !@pyapi kind=function ret=integer(:) args=a:integer(:):intent(in),v:integer(:):intent(in) desc="searchsorted right indices for integer vectors"
public :: searchsorted_left_int_scalar !@pyapi kind=function ret=integer args=a:integer(:):intent(in),v:integer:intent(in) desc="searchsorted left index for scalar integer query"
public :: searchsorted_right_int_scalar !@pyapi kind=function ret=integer args=a:integer(:):intent(in),v:integer:intent(in) desc="searchsorted right index for scalar integer query"
public :: setdiff1d_int !@pyapi kind=function ret=integer(:) args=a:integer(:):intent(in),b:integer(:):intent(in) desc="sorted unique values in a not in b"
public :: intersect1d_int !@pyapi kind=function ret=integer(:) args=a:integer(:):intent(in),b:integer(:):intent(in) desc="sorted unique intersection of a and b"
public :: unique_int_inv_counts !@pyapi kind=subroutine args=a:integer(:):intent(in),u:integer(:):intent(out),inv:integer(:):intent(out),cnt:integer(:):intent(out) desc="unique values with inverse and counts"
public :: lexsort2_int !@pyapi kind=function ret=integer(:) args=key0:integer(:):intent(in),key1:integer(:):intent(in) desc="lexsort((key0,key1)): sort by key1 then key0, return 0-based indices"
public :: ravel_multi_index_2d !@pyapi kind=function ret=integer args=rc:integer(:):intent(in),shape:integer(:):intent(in) desc="2D ravel_multi_index"
public :: unravel_index_2d !@pyapi kind=function ret=integer(:) args=i:integer:intent(in),shape:integer(:):intent(in) desc="2D unravel_index"
public :: kron_2d !@pyapi kind=function ret=integer(:,:) args=a:integer(:,:):intent(in),b:integer(:,:):intent(in) desc="2D Kronecker product for integer matrices"
public :: histogram_real_edges !@pyapi kind=subroutine args=x:real(dp)(:):intent(in),bins:real(dp)(:):intent(in),h:integer(:):intent(out),edges:real(dp)(:):intent(out) desc="1D histogram with explicit real bin edges"
public :: histogram_int_edges !@pyapi kind=subroutine args=x:integer(:):intent(in),bins:integer(:):intent(in),h:integer(:):intent(out),edges:integer(:):intent(out) desc="1D histogram with explicit integer bin edges"
public :: reduceat_add_real !@pyapi kind=function ret=real(dp)(:) args=x:real(dp)(:):intent(in),idx:integer(:):intent(in) desc="np.add.reduceat for real vector"
public :: reduceat_add_int !@pyapi kind=function ret=integer(:) args=x:integer(:):intent(in),idx:integer(:):intent(in) desc="np.add.reduceat for integer vector"
public :: reduceat_mul_real !@pyapi kind=function ret=real(dp)(:) args=x:real(dp)(:):intent(in),idx:integer(:):intent(in) desc="np.multiply.reduceat for real vector"
public :: reduceat_mul_int !@pyapi kind=function ret=integer(:) args=x:integer(:):intent(in),idx:integer(:):intent(in) desc="np.multiply.reduceat for integer vector"
public :: reduceat_min_real !@pyapi kind=function ret=real(dp)(:) args=x:real(dp)(:):intent(in),idx:integer(:):intent(in) desc="np.minimum.reduceat for real vector"
public :: reduceat_min_int !@pyapi kind=function ret=integer(:) args=x:integer(:):intent(in),idx:integer(:):intent(in) desc="np.minimum.reduceat for integer vector"
public :: reduceat_max_real !@pyapi kind=function ret=real(dp)(:) args=x:real(dp)(:):intent(in),idx:integer(:):intent(in) desc="np.maximum.reduceat for real vector"
public :: reduceat_max_int !@pyapi kind=function ret=integer(:) args=x:integer(:):intent(in),idx:integer(:):intent(in) desc="np.maximum.reduceat for integer vector"
public :: reduceat_logical_and !@pyapi kind=function ret=logical(:) args=x:logical(:):intent(in),idx:integer(:):intent(in) desc="np.logical_and.reduceat for logical vector"
public :: reduceat_logical_or !@pyapi kind=function ret=logical(:) args=x:logical(:):intent(in),idx:integer(:):intent(in) desc="np.logical_or.reduceat for logical vector"
public :: cumprod_real !@pyapi kind=function ret=real(dp)(:) args=x:real(dp)(:):intent(in) desc="cumulative product of real vector"
public :: gradient_1d !@pyapi kind=function ret=real(dp)(:) args=x:real(dp)(:):intent(in) desc="1D gradient with unit spacing (numpy-style edge handling)"
public :: linalg_solve !@pyapi kind=function ret=real(dp)(:) args=a:real(dp)(:,:):intent(in),b:real(dp)(:):intent(in) desc="solve linear system A x = b using LAPACK DGESV"
public :: linalg_det !@pyapi kind=function ret=real(dp) args=a:real(dp)(:,:):intent(in) desc="determinant of square matrix using LAPACK DGETRF"
public :: linalg_inv !@pyapi kind=function ret=real(dp)(:,:) args=a:real(dp)(:,:):intent(in) desc="matrix inverse using LAPACK DGETRF/DGETRI"
public :: linalg_eig !@pyapi kind=subroutine args=a:real(dp)(:,:):intent(in),w:real(dp)(:):intent(out),v:real(dp)(:,:):intent(out) desc="right eigenpairs of real square matrix using LAPACK DGEEV (real-spectrum only)"
public :: linalg_svd !@pyapi kind=subroutine args=a:real(dp)(:,:):intent(in),u:real(dp)(:,:):intent(out),s:real(dp)(:):intent(out),vt:real(dp)(:,:):intent(out) desc="full SVD using LAPACK DGESVD"

public :: var !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in),ddof:integer:intent(in):optional desc="variance of 1D real vector with optional ddof (numpy-style)"
public :: mean !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in) desc="mean of 1D real vector"
public :: std !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in),ddof:integer:intent(in):optional desc="standard deviation of 1D real vector with optional ddof (numpy-style)"
public :: log2 !@pyapi kind=function ret=real(dp) args=x:real(dp):intent(in) desc="base-2 logarithm"
public :: nansum !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in) desc="sum ignoring NaN values"
public :: nanmean !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in) desc="mean ignoring NaN values"
public :: nanvar !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in),ddof:integer:intent(in):optional desc="variance ignoring NaN values with optional ddof"
public :: nanstd !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in),ddof:integer:intent(in):optional desc="standard deviation ignoring NaN values with optional ddof"
public :: nanmin !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in) desc="minimum ignoring NaN values"
public :: nanmax !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in) desc="maximum ignoring NaN values"
public :: nanargmin !@pyapi kind=function ret=integer args=x:real(dp)(:):intent(in) desc="0-based argmin ignoring NaN values; -1 when all NaN"
public :: nanargmax !@pyapi kind=function ret=integer args=x:real(dp)(:):intent(in) desc="0-based argmax ignoring NaN values; -1 when all NaN"
public :: cumsum
public :: cumprod
public :: eye
public :: diag
public :: repeat
public :: tile
public :: unique
public :: sort_vec
public :: argsort
public :: histogram
public :: reduceat_add
public :: reduceat_mul
public :: reduceat_min
public :: reduceat_max

interface cumsum
   module procedure cumsum_real, cumsum_int
end interface cumsum

interface cumprod
   module procedure cumprod_real, cumprod_int
end interface cumprod

interface eye
   module procedure eye_real
end interface eye

interface diag
   module procedure diag_from_vec_real, diag_from_mat_real
   module procedure diag_from_vec_int, diag_from_mat_int
end interface diag

interface repeat
   module procedure repeat_real, repeat_int, repeat_logical
end interface repeat

interface tile
   module procedure tile_real, tile_int
   module procedure tile_real_2d, tile_int_2d
end interface tile

interface unique
   module procedure unique_real, unique_int
end interface unique

interface sort_vec
   module procedure sort_real_vec, sort_int_vec
end interface sort_vec

interface argsort
   module procedure argsort_real, argsort_int
end interface argsort

interface histogram
   module procedure histogram_real_edges, histogram_int_edges
end interface histogram

interface reduceat_add
   module procedure reduceat_add_real, reduceat_add_int
end interface reduceat_add

interface reduceat_mul
   module procedure reduceat_mul_real, reduceat_mul_int
end interface reduceat_mul

interface reduceat_min
   module procedure reduceat_min_real, reduceat_min_int
end interface reduceat_min

interface reduceat_max
   module procedure reduceat_max_real, reduceat_max_int
end interface reduceat_max

interface linalg_solve
   module procedure linalg_solve_vec, linalg_solve_mat
end interface linalg_solve

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

      pure subroutine sort_int_vec(x)
         ! sort integer vector x in ascending order
         implicit none
         integer, intent(inout) :: x(:)
         integer :: i, j, n, key
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
      end subroutine sort_int_vec

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

      subroutine argsort_int(x, idx)
         integer, intent(in) :: x(:)
         integer, intent(out) :: idx(:)
         integer :: i, j, n, key
         n = size(x)
         if (size(idx) < n) stop "argsort_int: output array too small"
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
      end subroutine argsort_int

      function arange_int(start, stop, step) result(x)
         integer, intent(in) :: start, stop, step
         integer, allocatable :: x(:)
         integer :: n, i, s
         s = step
         if (s == 0) error stop 'arange_int: step cannot be zero'
         if (s > 0) then
            if (stop <= start) then
               n = 0
            else
               n = (stop - start + s - 1) / s
            end if
         else
            if (stop >= start) then
               n = 0
            else
               n = (start - stop + (-s) - 1) / (-s)
            end if
         end if
         allocate(x(n))
         do i = 1, n
            x(i) = start + (i - 1) * s
         end do
      end function arange_int

      function logspace(start, stop, num, endpoint, base) result(x)
         real(kind=dp), intent(in) :: start, stop
         integer, intent(in), optional :: num
         logical, intent(in), optional :: endpoint
         real(kind=dp), intent(in), optional :: base
         real(kind=dp), allocatable :: x(:)
         integer :: n, i
         logical :: ep
         real(kind=dp) :: b, dx
         n = 50
         if (present(num)) n = max(0, num)
         ep = .true.
         if (present(endpoint)) ep = endpoint
         b = 10.0_dp
         if (present(base)) b = base
         allocate(x(n))
         if (n <= 0) return
         if (n == 1) then
            if (ep) then
               x(1) = b**stop
            else
               x(1) = b**start
            end if
            return
         end if
         if (ep) then
            dx = (stop - start) / real(n - 1, kind=dp)
         else
            dx = (stop - start) / real(n, kind=dp)
         end if
         do i = 1, n
            x(i) = b**(start + dx * real(i - 1, kind=dp))
         end do
      end function logspace

      function geomspace(start, stop, num, endpoint) result(x)
         real(kind=dp), intent(in) :: start, stop
         integer, intent(in), optional :: num
         logical, intent(in), optional :: endpoint
         real(kind=dp), allocatable :: x(:)
         integer :: n, i
         logical :: ep
         real(kind=dp) :: sgn, la, lb, dx
         n = 50
         if (present(num)) n = max(0, num)
         ep = .true.
         if (present(endpoint)) ep = endpoint
         if (start == 0.0_dp .or. stop == 0.0_dp) error stop 'geomspace: start/stop must be non-zero'
         if (start * stop < 0.0_dp) error stop 'geomspace: start/stop must have same sign'
         sgn = 1.0_dp
         if (start < 0.0_dp) sgn = -1.0_dp
         la = log(abs(start))
         lb = log(abs(stop))
         allocate(x(n))
         if (n <= 0) return
         if (n == 1) then
            if (ep) then
               x(1) = stop
            else
               x(1) = start
            end if
            return
         end if
         if (ep) then
            dx = (lb - la) / real(n - 1, kind=dp)
         else
            dx = (lb - la) / real(n, kind=dp)
         end if
         do i = 1, n
            x(i) = sgn * exp(la + dx * real(i - 1, kind=dp))
         end do
      end function geomspace

      function bincount_int(x, minlength) result(c)
         integer, intent(in) :: x(:)
         integer, intent(in), optional :: minlength
         integer, allocatable :: c(:)
         integer :: i, nmax, nout, m
         if (present(minlength)) then
            m = max(0, minlength)
         else
            m = 0
         end if
         if (size(x) <= 0) then
            nout = m
            allocate(c(1:nout), source=0)
            return
         end if
         if (any(x < 0)) error stop 'bincount_int: negative values are not supported'
         nmax = maxval(x)
         nout = max(nmax + 1, m)
         allocate(c(1:nout), source=0)
         do i = 1, size(x)
            c(x(i) + 1) = c(x(i) + 1) + 1
         end do
      end function bincount_int

      function searchsorted_left_int(a, v) result(idx)
         integer, intent(in) :: a(:), v(:)
         integer, allocatable :: idx(:)
         integer :: i, lo, hi, mid, n
         n = size(a)
         allocate(idx(1:size(v)))
         do i = 1, size(v)
            lo = 1
            hi = n + 1
            do while (lo < hi)
               mid = (lo + hi) / 2
               if (mid <= n .and. a(mid) < v(i)) then
                  lo = mid + 1
               else
                  hi = mid
               end if
            end do
            idx(i) = lo - 1
         end do
      end function searchsorted_left_int

      function searchsorted_right_int(a, v) result(idx)
         integer, intent(in) :: a(:), v(:)
         integer, allocatable :: idx(:)
         integer :: i, lo, hi, mid, n
         n = size(a)
         allocate(idx(1:size(v)))
         do i = 1, size(v)
            lo = 1
            hi = n + 1
            do while (lo < hi)
               mid = (lo + hi) / 2
               if (mid <= n .and. a(mid) <= v(i)) then
                  lo = mid + 1
               else
                  hi = mid
               end if
            end do
            idx(i) = lo - 1
         end do
      end function searchsorted_right_int

      function searchsorted_left_int_scalar(a, v) result(idx)
         integer, intent(in) :: a(:), v
         integer :: idx
         integer :: lo, hi, mid, n
         n = size(a)
         lo = 1
         hi = n + 1
         do while (lo < hi)
            mid = (lo + hi) / 2
            if (mid <= n .and. a(mid) < v) then
               lo = mid + 1
            else
               hi = mid
            end if
         end do
         idx = lo - 1
      end function searchsorted_left_int_scalar

      function searchsorted_right_int_scalar(a, v) result(idx)
         integer, intent(in) :: a(:), v
         integer :: idx
         integer :: lo, hi, mid, n
         n = size(a)
         lo = 1
         hi = n + 1
         do while (lo < hi)
            mid = (lo + hi) / 2
            if (mid <= n .and. a(mid) <= v) then
               lo = mid + 1
            else
               hi = mid
            end if
         end do
         idx = lo - 1
      end function searchsorted_right_int_scalar

      function setdiff1d_int(a, b) result(c)
         integer, intent(in) :: a(:), b(:)
         integer, allocatable :: c(:)
         integer, allocatable :: ua(:), ub(:), tmp(:)
         integer :: i, j, nout
         ua = unique_int(a)
         ub = unique_int(b)
         allocate(tmp(1:size(ua)), source=0)
         nout = 0
         do i = 1, size(ua)
            do j = 1, size(ub)
               if (ua(i) == ub(j)) exit
            end do
            if (j > size(ub)) then
               nout = nout + 1
               tmp(nout) = ua(i)
            end if
         end do
         allocate(c(1:nout))
         if (nout > 0) c = tmp(1:nout)
      end function setdiff1d_int

      function intersect1d_int(a, b) result(c)
         integer, intent(in) :: a(:), b(:)
         integer, allocatable :: c(:)
         integer, allocatable :: ua(:), ub(:), tmp(:)
         integer :: i, j, nout
         ua = unique_int(a)
         ub = unique_int(b)
         allocate(tmp(1:min(size(ua), size(ub))), source=0)
         nout = 0
         do i = 1, size(ua)
            do j = 1, size(ub)
               if (ua(i) == ub(j)) then
                  nout = nout + 1
                  tmp(nout) = ua(i)
                  exit
               end if
            end do
         end do
         allocate(c(1:nout))
         if (nout > 0) c = tmp(1:nout)
      end function intersect1d_int

      subroutine unique_int_inv_counts(a, u, inv, cnt)
         integer, intent(in) :: a(:)
         integer, allocatable, intent(out) :: u(:), inv(:), cnt(:)
         integer :: i, j, n
         u = unique_int(a)
         n = size(a)
         allocate(inv(1:n), source=0)
         allocate(cnt(1:size(u)), source=0)
         do i = 1, n
            do j = 1, size(u)
               if (a(i) == u(j)) then
                  inv(i) = j - 1
                  cnt(j) = cnt(j) + 1
                  exit
               end if
            end do
         end do
      end subroutine unique_int_inv_counts

      function lexsort2_int(key0, key1) result(idx)
         integer, intent(in) :: key0(:), key1(:)
         integer, allocatable :: idx(:)
         integer :: i, j, n, t
         n = size(key0)
         if (size(key1) /= n) error stop 'lexsort2_int: key size mismatch'
         allocate(idx(1:n))
         do i = 1, n
            idx(i) = i - 1
         end do
         do i = 2, n
            t = idx(i)
            j = i - 1
            do while (j >= 1)
               if (key1(idx(j)+1) < key1(t+1)) exit
               if (key1(idx(j)+1) == key1(t+1) .and. key0(idx(j)+1) <= key0(t+1)) exit
               idx(j+1) = idx(j)
               j = j - 1
            end do
            idx(j+1) = t
         end do
      end function lexsort2_int

      function ravel_multi_index_2d(rc, shape) result(i)
         integer, intent(in) :: rc(:), shape(:)
         integer :: i
         if (size(rc) < 2 .or. size(shape) < 2) error stop 'ravel_multi_index_2d: expected rank-2 inputs'
         i = rc(1) * shape(2) + rc(2)
      end function ravel_multi_index_2d

      function unravel_index_2d(i, shape) result(rc)
         integer, intent(in) :: i
         integer, intent(in) :: shape(:)
         integer, allocatable :: rc(:)
         if (size(shape) < 2) error stop 'unravel_index_2d: expected shape rank 2'
         allocate(rc(1:2))
         rc(1) = i / shape(2)
         rc(2) = mod(i, shape(2))
      end function unravel_index_2d

      function kron_2d(a, b) result(k)
         integer, intent(in) :: a(:,:), b(:,:)
         integer, allocatable :: k(:,:)
         integer :: i, j, p, q, ra, ca, rb, cb
         ra = size(a,1); ca = size(a,2)
         rb = size(b,1); cb = size(b,2)
         allocate(k(1:ra*rb, 1:ca*cb), source=0)
         do i = 1, ra
            do j = 1, ca
               do p = 1, rb
                  do q = 1, cb
                     k((i-1)*rb+p, (j-1)*cb+q) = a(i,j) * b(p,q)
                  end do
               end do
            end do
         end do
      end function kron_2d

      subroutine histogram_real_edges(x, bins, h, edges)
         real(kind=dp), intent(in) :: x(:), bins(:)
         integer, allocatable, intent(out) :: h(:)
         real(kind=dp), allocatable, intent(out) :: edges(:)
         integer :: i, j, nb
         logical :: placed
         nb = size(bins) - 1
         if (nb < 1) error stop 'histogram_real_edges: bins must have at least 2 entries'
         allocate(h(1:nb), source=0)
         allocate(edges(1:size(bins)), source=bins)
         do i = 1, size(x)
            if (x(i) < bins(1)) cycle
            if (x(i) > bins(nb + 1)) cycle
            placed = .false.
            do j = 1, nb - 1
               if (x(i) >= bins(j) .and. x(i) < bins(j + 1)) then
                  h(j) = h(j) + 1
                  placed = .true.
                  exit
               end if
            end do
            if (.not. placed) then
               if (x(i) >= bins(nb) .and. x(i) <= bins(nb + 1)) h(nb) = h(nb) + 1
            end if
         end do
      end subroutine histogram_real_edges

      subroutine histogram_int_edges(x, bins, h, edges)
         integer, intent(in) :: x(:), bins(:)
         integer, allocatable, intent(out) :: h(:), edges(:)
         integer :: i, j, nb
         logical :: placed
         nb = size(bins) - 1
         if (nb < 1) error stop 'histogram_int_edges: bins must have at least 2 entries'
         allocate(h(1:nb), source=0)
         allocate(edges(1:size(bins)), source=bins)
         do i = 1, size(x)
            if (x(i) < bins(1)) cycle
            if (x(i) > bins(nb + 1)) cycle
            placed = .false.
            do j = 1, nb - 1
               if (x(i) >= bins(j) .and. x(i) < bins(j + 1)) then
                  h(j) = h(j) + 1
                  placed = .true.
                  exit
               end if
            end do
            if (.not. placed) then
               if (x(i) >= bins(nb) .and. x(i) <= bins(nb + 1)) h(nb) = h(nb) + 1
            end if
         end do
      end subroutine histogram_int_edges

      function reduceat_add_real(x, idx) result(y)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in) :: idx(:)
         real(kind=dp), allocatable :: y(:)
         integer :: i, j, lo, hi, n, m
         n = size(x)
         m = size(idx)
         allocate(y(1:m))
         do i = 1, m
            lo = idx(i) + 1
            if (lo < 1 .or. lo > n) error stop 'reduceat_add_real: idx out of bounds'
            if (i < m) then
               hi = idx(i + 1)
            else
               hi = n
            end if
            if (hi < lo) hi = lo
            y(i) = x(lo)
            do j = lo + 1, hi
               y(i) = y(i) + x(j)
            end do
         end do
      end function reduceat_add_real

      function reduceat_add_int(x, idx) result(y)
         integer, intent(in) :: x(:)
         integer, intent(in) :: idx(:)
         integer, allocatable :: y(:)
         integer :: i, j, lo, hi, n, m
         n = size(x)
         m = size(idx)
         allocate(y(1:m))
         do i = 1, m
            lo = idx(i) + 1
            if (lo < 1 .or. lo > n) error stop 'reduceat_add_int: idx out of bounds'
            if (i < m) then
               hi = idx(i + 1)
            else
               hi = n
            end if
            if (hi < lo) hi = lo
            y(i) = x(lo)
            do j = lo + 1, hi
               y(i) = y(i) + x(j)
            end do
         end do
      end function reduceat_add_int

      function reduceat_mul_real(x, idx) result(y)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in) :: idx(:)
         real(kind=dp), allocatable :: y(:)
         integer :: i, j, lo, hi, n, m
         n = size(x)
         m = size(idx)
         allocate(y(1:m))
         do i = 1, m
            lo = idx(i) + 1
            if (lo < 1 .or. lo > n) error stop 'reduceat_mul_real: idx out of bounds'
            if (i < m) then
               hi = idx(i + 1)
            else
               hi = n
            end if
            if (hi < lo) hi = lo
            y(i) = x(lo)
            do j = lo + 1, hi
               y(i) = y(i) * x(j)
            end do
         end do
      end function reduceat_mul_real

      function reduceat_mul_int(x, idx) result(y)
         integer, intent(in) :: x(:)
         integer, intent(in) :: idx(:)
         integer, allocatable :: y(:)
         integer :: i, j, lo, hi, n, m
         n = size(x)
         m = size(idx)
         allocate(y(1:m))
         do i = 1, m
            lo = idx(i) + 1
            if (lo < 1 .or. lo > n) error stop 'reduceat_mul_int: idx out of bounds'
            if (i < m) then
               hi = idx(i + 1)
            else
               hi = n
            end if
            if (hi < lo) hi = lo
            y(i) = x(lo)
            do j = lo + 1, hi
               y(i) = y(i) * x(j)
            end do
         end do
      end function reduceat_mul_int

      function reduceat_min_real(x, idx) result(y)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in) :: idx(:)
         real(kind=dp), allocatable :: y(:)
         integer :: i, j, lo, hi, n, m
         n = size(x)
         m = size(idx)
         allocate(y(1:m))
         do i = 1, m
            lo = idx(i) + 1
            if (lo < 1 .or. lo > n) error stop 'reduceat_min_real: idx out of bounds'
            if (i < m) then
               hi = idx(i + 1)
            else
               hi = n
            end if
            if (hi < lo) hi = lo
            y(i) = x(lo)
            do j = lo + 1, hi
               y(i) = min(y(i), x(j))
            end do
         end do
      end function reduceat_min_real

      function reduceat_min_int(x, idx) result(y)
         integer, intent(in) :: x(:)
         integer, intent(in) :: idx(:)
         integer, allocatable :: y(:)
         integer :: i, j, lo, hi, n, m
         n = size(x)
         m = size(idx)
         allocate(y(1:m))
         do i = 1, m
            lo = idx(i) + 1
            if (lo < 1 .or. lo > n) error stop 'reduceat_min_int: idx out of bounds'
            if (i < m) then
               hi = idx(i + 1)
            else
               hi = n
            end if
            if (hi < lo) hi = lo
            y(i) = x(lo)
            do j = lo + 1, hi
               y(i) = min(y(i), x(j))
            end do
         end do
      end function reduceat_min_int

      function reduceat_max_real(x, idx) result(y)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in) :: idx(:)
         real(kind=dp), allocatable :: y(:)
         integer :: i, j, lo, hi, n, m
         n = size(x)
         m = size(idx)
         allocate(y(1:m))
         do i = 1, m
            lo = idx(i) + 1
            if (lo < 1 .or. lo > n) error stop 'reduceat_max_real: idx out of bounds'
            if (i < m) then
               hi = idx(i + 1)
            else
               hi = n
            end if
            if (hi < lo) hi = lo
            y(i) = x(lo)
            do j = lo + 1, hi
               y(i) = max(y(i), x(j))
            end do
         end do
      end function reduceat_max_real

      function reduceat_max_int(x, idx) result(y)
         integer, intent(in) :: x(:)
         integer, intent(in) :: idx(:)
         integer, allocatable :: y(:)
         integer :: i, j, lo, hi, n, m
         n = size(x)
         m = size(idx)
         allocate(y(1:m))
         do i = 1, m
            lo = idx(i) + 1
            if (lo < 1 .or. lo > n) error stop 'reduceat_max_int: idx out of bounds'
            if (i < m) then
               hi = idx(i + 1)
            else
               hi = n
            end if
            if (hi < lo) hi = lo
            y(i) = x(lo)
            do j = lo + 1, hi
               y(i) = max(y(i), x(j))
            end do
         end do
      end function reduceat_max_int

      function reduceat_logical_and(x, idx) result(y)
         logical, intent(in) :: x(:)
         integer, intent(in) :: idx(:)
         logical, allocatable :: y(:)
         integer :: i, j, lo, hi, n, m
         n = size(x)
         m = size(idx)
         allocate(y(1:m))
         do i = 1, m
            lo = idx(i) + 1
            if (lo < 1 .or. lo > n) error stop 'reduceat_logical_and: idx out of bounds'
            if (i < m) then
               hi = idx(i + 1)
            else
               hi = n
            end if
            if (hi < lo) hi = lo
            y(i) = x(lo)
            do j = lo + 1, hi
               y(i) = y(i) .and. x(j)
            end do
         end do
      end function reduceat_logical_and

      function reduceat_logical_or(x, idx) result(y)
         logical, intent(in) :: x(:)
         integer, intent(in) :: idx(:)
         logical, allocatable :: y(:)
         integer :: i, j, lo, hi, n, m
         n = size(x)
         m = size(idx)
         allocate(y(1:m))
         do i = 1, m
            lo = idx(i) + 1
            if (lo < 1 .or. lo > n) error stop 'reduceat_logical_or: idx out of bounds'
            if (i < m) then
               hi = idx(i + 1)
            else
               hi = n
            end if
            if (hi < lo) hi = lo
            y(i) = x(lo)
            do j = lo + 1, hi
               y(i) = y(i) .or. x(j)
            end do
         end do
      end function reduceat_logical_or

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

      function zeros_real(n) result(x)
         integer, intent(in) :: n
         real(kind=dp), allocatable :: x(:)
         if (n < 0) error stop 'zeros_real: n must be >= 0'
         allocate(x(n), source=0.0_dp)
      end function zeros_real

      function ones_real(n) result(x)
         integer, intent(in) :: n
         real(kind=dp), allocatable :: x(:)
         if (n < 0) error stop 'ones_real: n must be >= 0'
         allocate(x(n), source=1.0_dp)
      end function ones_real

      function zeros_int(n) result(x)
         integer, intent(in) :: n
         integer, allocatable :: x(:)
         if (n < 0) error stop 'zeros_int: n must be >= 0'
         allocate(x(n), source=0)
      end function zeros_int

      function ones_int(n) result(x)
         integer, intent(in) :: n
         integer, allocatable :: x(:)
         if (n < 0) error stop 'ones_int: n must be >= 0'
         allocate(x(n), source=1)
      end function ones_int

      function zeros_logical(n) result(x)
         integer, intent(in) :: n
         logical, allocatable :: x(:)
         if (n < 0) error stop 'zeros_logical: n must be >= 0'
         allocate(x(n), source=.false.)
      end function zeros_logical

      function ones_logical(n) result(x)
         integer, intent(in) :: n
         logical, allocatable :: x(:)
         if (n < 0) error stop 'ones_logical: n must be >= 0'
         allocate(x(n), source=.true.)
      end function ones_logical

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

      function cumsum_real(x) result(y)
         real(kind=dp), intent(in) :: x(:)
         real(kind=dp), allocatable :: y(:)
         integer :: i, n
         n = size(x)
         allocate(y(1:n))
         if (n >= 1) then
            y(1) = x(1)
            do i = 2, n
               y(i) = y(i-1) + x(i)
            end do
         end if
      end function cumsum_real

      function unique_int(x) result(y)
         integer, intent(in) :: x(:)
         integer, allocatable :: y(:)
         integer, allocatable :: tmp(:)
         integer :: i, j, n, m, key
         n = size(x)
         allocate(tmp(1:n))
         tmp = x
         do i = 2, n
            key = tmp(i)
            j = i - 1
            do while (j >= 1 .and. tmp(j) > key)
               tmp(j+1) = tmp(j)
               j = j - 1
            end do
            tmp(j+1) = key
         end do
         m = 0
         do i = 1, n
            if (i == 1 .or. tmp(i) /= tmp(i-1)) m = m + 1
         end do
         allocate(y(1:m))
         m = 0
         do i = 1, n
            if (i == 1 .or. tmp(i) /= tmp(i-1)) then
               m = m + 1
               y(m) = tmp(i)
            end if
         end do
         deallocate(tmp)
      end function unique_int

      function tile_int(x, reps) result(y)
         integer, intent(in) :: x(:)
         integer, intent(in) :: reps
         integer, allocatable :: y(:)
         integer :: n, r, k, i1, i2
         n = size(x)
         r = max(0, reps)
         allocate(y(1:n*r))
         do k = 1, r
            i1 = (k - 1) * n + 1
            i2 = k * n
            y(i1:i2) = x
         end do
      end function tile_int

      function tile_int_2d(x, reps0, reps1) result(y)
         integer, intent(in) :: x(:,:)
         integer, intent(in) :: reps0, reps1
         integer, allocatable :: y(:,:)
         integer :: m, n, r0, r1, i, j
         m = size(x,1)
         n = size(x,2)
         r0 = max(0, reps0)
         r1 = max(0, reps1)
         allocate(y(1:m*r0, 1:n*r1))
         do i = 1, r0
            do j = 1, r1
               y((i-1)*m+1:i*m, (j-1)*n+1:j*n) = x
            end do
         end do
      end function tile_int_2d

      function diag_from_vec_int(v) result(x)
         integer, intent(in) :: v(:)
         integer, allocatable :: x(:,:)
         integer :: i, n
         n = size(v)
         allocate(x(1:n,1:n), source=0)
         do i = 1, n
            x(i,i) = v(i)
         end do
      end function diag_from_vec_int

      function cumprod_int(x) result(y)
         integer, intent(in) :: x(:)
         integer, allocatable :: y(:)
         integer :: i, n
         n = size(x)
         allocate(y(1:n))
         if (n >= 1) then
            y(1) = x(1)
            do i = 2, n
               y(i) = y(i-1) * x(i)
            end do
         end if
      end function cumprod_int

      function repeat_int(x, reps) result(y)
         integer, intent(in) :: x(:)
         integer, intent(in) :: reps
         integer, allocatable :: y(:)
         integer :: i, j, n, r, k
         n = size(x)
         r = max(0, reps)
         allocate(y(1:n*r))
         k = 0
         do i = 1, n
            do j = 1, r
               k = k + 1
               y(k) = x(i)
            end do
         end do
      end function repeat_int

      function repeat_int_axis0_2d(x, reps) result(y)
         integer, intent(in) :: x(:,:)
         integer, intent(in) :: reps
         integer, allocatable :: y(:,:)
         integer :: i, k, m, n, r
         m = size(x,1)
         n = size(x,2)
         r = max(0, reps)
         allocate(y(1:m*r, 1:n))
         do i = 1, m
            do k = 1, r
               y((i-1)*r + k, :) = x(i, :)
            end do
         end do
      end function repeat_int_axis0_2d

      function repeat_int_axis1_2d(x, reps) result(y)
         integer, intent(in) :: x(:,:)
         integer, intent(in) :: reps
         integer, allocatable :: y(:,:)
         integer :: j, k, m, n, r
         m = size(x,1)
         n = size(x,2)
         r = max(0, reps)
         allocate(y(1:m, 1:n*r))
         do j = 1, n
            do k = 1, r
               y(:, (j-1)*r + k) = x(:, j)
            end do
         end do
      end function repeat_int_axis1_2d

      function diag_from_mat_real(a) result(v)
         real(kind=dp), intent(in) :: a(:,:)
         real(kind=dp), allocatable :: v(:)
         integer :: i, n
         n = min(size(a,1), size(a,2))
         allocate(v(1:n))
         do i = 1, n
            v(i) = a(i,i)
         end do
      end function diag_from_mat_real

      function cumsum_int(x) result(y)
         integer, intent(in) :: x(:)
         integer, allocatable :: y(:)
         integer :: i, n
         n = size(x)
         allocate(y(1:n))
         if (n >= 1) then
            y(1) = x(1)
            do i = 2, n
               y(i) = y(i-1) + x(i)
            end do
         end if
      end function cumsum_int

      function diag_from_vec_real(v) result(x)
         real(kind=dp), intent(in) :: v(:)
         real(kind=dp), allocatable :: x(:,:)
         integer :: i, n
         n = size(v)
         allocate(x(1:n,1:n), source=0.0_dp)
         do i = 1, n
            x(i,i) = v(i)
         end do
      end function diag_from_vec_real

      function repeat_real(x, reps) result(y)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in) :: reps
         real(kind=dp), allocatable :: y(:)
         integer :: i, j, n, r, k
         n = size(x)
         r = max(0, reps)
         allocate(y(1:n*r))
         k = 0
         do i = 1, n
            do j = 1, r
               k = k + 1
               y(k) = x(i)
            end do
         end do
      end function repeat_real

      function repeat_real_axis0_2d(x, reps) result(y)
         real(kind=dp), intent(in) :: x(:,:)
         integer, intent(in) :: reps
         real(kind=dp), allocatable :: y(:,:)
         integer :: i, k, m, n, r
         m = size(x,1)
         n = size(x,2)
         r = max(0, reps)
         allocate(y(1:m*r, 1:n))
         do i = 1, m
            do k = 1, r
               y((i-1)*r + k, :) = x(i, :)
            end do
         end do
      end function repeat_real_axis0_2d

      function repeat_real_axis1_2d(x, reps) result(y)
         real(kind=dp), intent(in) :: x(:,:)
         integer, intent(in) :: reps
         real(kind=dp), allocatable :: y(:,:)
         integer :: j, k, m, n, r
         m = size(x,1)
         n = size(x,2)
         r = max(0, reps)
         allocate(y(1:m, 1:n*r))
         do j = 1, n
            do k = 1, r
               y(:, (j-1)*r + k) = x(:, j)
            end do
         end do
      end function repeat_real_axis1_2d

      function repeat_logical(x, reps) result(y)
         logical, intent(in) :: x(:)
         integer, intent(in) :: reps
         logical, allocatable :: y(:)
         integer :: i, j, n, r, k
         n = size(x)
         r = max(0, reps)
         allocate(y(1:n*r))
         k = 0
         do i = 1, n
            do j = 1, r
               k = k + 1
               y(k) = x(i)
            end do
         end do
      end function repeat_logical

      function repeat_logical_axis0_2d(x, reps) result(y)
         logical, intent(in) :: x(:,:)
         integer, intent(in) :: reps
         logical, allocatable :: y(:,:)
         integer :: i, k, m, n, r
         m = size(x,1)
         n = size(x,2)
         r = max(0, reps)
         allocate(y(1:m*r, 1:n))
         do i = 1, m
            do k = 1, r
               y((i-1)*r + k, :) = x(i, :)
            end do
         end do
      end function repeat_logical_axis0_2d

      function repeat_logical_axis1_2d(x, reps) result(y)
         logical, intent(in) :: x(:,:)
         integer, intent(in) :: reps
         logical, allocatable :: y(:,:)
         integer :: j, k, m, n, r
         m = size(x,1)
         n = size(x,2)
         r = max(0, reps)
         allocate(y(1:m, 1:n*r))
         do j = 1, n
            do k = 1, r
               y(:, (j-1)*r + k) = x(:, j)
            end do
         end do
      end function repeat_logical_axis1_2d

      function diag_from_mat_int(a) result(v)
         integer, intent(in) :: a(:,:)
         integer, allocatable :: v(:)
         integer :: i, n
         n = min(size(a,1), size(a,2))
         allocate(v(1:n))
         do i = 1, n
            v(i) = a(i,i)
         end do
      end function diag_from_mat_int

      function tile_real(x, reps) result(y)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in) :: reps
         real(kind=dp), allocatable :: y(:)
         integer :: n, r, k, i1, i2
         n = size(x)
         r = max(0, reps)
         allocate(y(1:n*r))
         do k = 1, r
            i1 = (k - 1) * n + 1
            i2 = k * n
            y(i1:i2) = x
         end do
      end function tile_real

      function tile_real_2d(x, reps0, reps1) result(y)
         real(kind=dp), intent(in) :: x(:,:)
         integer, intent(in) :: reps0, reps1
         real(kind=dp), allocatable :: y(:,:)
         integer :: m, n, r0, r1, i, j
         m = size(x,1)
         n = size(x,2)
         r0 = max(0, reps0)
         r1 = max(0, reps1)
         allocate(y(1:m*r0, 1:n*r1))
         do i = 1, r0
            do j = 1, r1
               y((i-1)*m+1:i*m, (j-1)*n+1:j*n) = x
            end do
         end do
      end function tile_real_2d

      function eye_real(n, m) result(x)
         integer, intent(in) :: n
         integer, intent(in), optional :: m
         real(kind=dp), allocatable :: x(:,:)
         integer :: mm, i, k
         mm = n
         if (present(m)) mm = m
         if (n < 0 .or. mm < 0) error stop "eye_real: dimensions must be >= 0"
         allocate(x(1:n,1:mm), source=0.0_dp)
         k = min(n, mm)
         do i = 1, k
            x(i,i) = 1.0_dp
         end do
      end function eye_real

      function unique_real(x) result(y)
         real(kind=dp), intent(in) :: x(:)
         real(kind=dp), allocatable :: y(:)
         real(kind=dp), allocatable :: tmp(:)
         integer :: i, n, m
         n = size(x)
         allocate(tmp(1:n))
         tmp = x
         call sort_real_vec(tmp)
         m = 0
         do i = 1, n
            if (i == 1 .or. tmp(i) /= tmp(i-1)) m = m + 1
         end do
         allocate(y(1:m))
         m = 0
         do i = 1, n
            if (i == 1 .or. tmp(i) /= tmp(i-1)) then
               m = m + 1
               y(m) = tmp(i)
            end if
         end do
         deallocate(tmp)
      end function unique_real

      function cumprod_real(x) result(y)
         real(kind=dp), intent(in) :: x(:)
         real(kind=dp), allocatable :: y(:)
         integer :: i, n
         n = size(x)
         allocate(y(1:n))
         if (n >= 1) then
            y(1) = x(1)
            do i = 2, n
               y(i) = y(i-1) * x(i)
            end do
         end if
      end function cumprod_real

      function gradient_1d(x) result(g)
         real(kind=dp), intent(in) :: x(:)
         real(kind=dp), allocatable :: g(:)
         integer :: n
         n = size(x)
         allocate(g(1:n))
         if (n <= 0) return
         if (n == 1) then
            g(1) = 0.0_dp
            return
         end if
         if (n == 2) then
            g(1) = x(2) - x(1)
            g(2) = x(2) - x(1)
            return
         end if
         g(1) = x(2) - x(1)
         g(2:n-1) = 0.5_dp * (x(3:n) - x(1:n-2))
         g(n) = x(n) - x(n-1)
      end function gradient_1d

      function linalg_solve_vec(a, b) result(x)
         real(kind=dp), intent(in) :: a(:,:), b(:)
         real(kind=dp), allocatable :: x(:)
         real(kind=dp), allocatable :: ac(:,:), bc(:,:)
         integer, allocatable :: ipiv(:)
         integer :: n, info
         interface
            subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
               integer, intent(in) :: n, nrhs, lda, ldb
               integer, intent(out) :: ipiv(*), info
               double precision, intent(inout) :: a(lda,*), b(ldb,*)
            end subroutine dgesv
         end interface
         n = size(a, 1)
         if (size(a,2) /= n) stop "linalg_solve: matrix must be square"
         if (size(b) /= n) stop "linalg_solve: rhs size mismatch"
         allocate(ac(1:n,1:n), source=a)
         allocate(bc(1:n,1), source=0.0_dp)
         bc(:,1) = b
         allocate(ipiv(1:n))
         call dgesv(n, 1, ac, n, ipiv, bc, n, info)
         if (info /= 0) stop "linalg_solve: dgesv failed"
         allocate(x(1:n))
         x = bc(:,1)
      end function linalg_solve_vec

      function linalg_solve_mat(a, b) result(x)
         real(kind=dp), intent(in) :: a(:,:), b(:,:)
         real(kind=dp), allocatable :: x(:,:)
         real(kind=dp), allocatable :: ac(:,:), bc(:,:)
         integer, allocatable :: ipiv(:)
         integer :: n, nrhs, info
         interface
            subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
               integer, intent(in) :: n, nrhs, lda, ldb
               integer, intent(out) :: ipiv(*), info
               double precision, intent(inout) :: a(lda,*), b(ldb,*)
            end subroutine dgesv
         end interface
         n = size(a, 1)
         if (size(a,2) /= n) stop "linalg_solve: matrix must be square"
         if (size(b,1) /= n) stop "linalg_solve: rhs row mismatch"
         nrhs = size(b,2)
         allocate(ac(1:n,1:n), source=a)
         allocate(bc(1:n,1:nrhs), source=b)
         allocate(ipiv(1:n))
         call dgesv(n, nrhs, ac, n, ipiv, bc, n, info)
         if (info /= 0) stop "linalg_solve: dgesv failed"
         allocate(x(1:n,1:nrhs))
         x = bc
      end function linalg_solve_mat

      function linalg_det(a) result(detv)
         real(kind=dp), intent(in) :: a(:,:)
         real(kind=dp) :: detv
         real(kind=dp), allocatable :: ac(:,:)
         integer, allocatable :: ipiv(:)
         integer :: n, i, info, sgn
         interface
            subroutine dgetrf(m, n, a, lda, ipiv, info)
               integer, intent(in) :: m, n, lda
               integer, intent(out) :: ipiv(*), info
               double precision, intent(inout) :: a(lda,*)
            end subroutine dgetrf
         end interface
         n = size(a,1)
         if (size(a,2) /= n) stop "linalg_det: matrix must be square"
         allocate(ac(1:n,1:n), source=a)
         allocate(ipiv(1:n))
         call dgetrf(n, n, ac, n, ipiv, info)
         if (info < 0) stop "linalg_det: dgetrf argument error"
         if (info > 0) then
            detv = 0.0_dp
            return
         end if
         sgn = 1
         do i = 1, n
            if (ipiv(i) /= i) sgn = -sgn
         end do
         detv = real(sgn, kind=dp)
         do i = 1, n
            detv = detv * ac(i,i)
         end do
      end function linalg_det

      function linalg_inv(a) result(ai)
         real(kind=dp), intent(in) :: a(:,:)
         real(kind=dp), allocatable :: ai(:,:)
         real(kind=dp), allocatable :: ac(:,:), work(:)
         integer, allocatable :: ipiv(:)
         integer :: n, info, lwork
         interface
            subroutine dgetrf(m, n, a, lda, ipiv, info)
               integer, intent(in) :: m, n, lda
               integer, intent(out) :: ipiv(*), info
               double precision, intent(inout) :: a(lda,*)
            end subroutine dgetrf
            subroutine dgetri(n, a, lda, ipiv, work, lwork, info)
               integer, intent(in) :: n, lda, lwork
               integer, intent(in) :: ipiv(*)
               integer, intent(out) :: info
               double precision, intent(inout) :: a(lda,*), work(*)
            end subroutine dgetri
         end interface
         n = size(a,1)
         if (size(a,2) /= n) stop "linalg_inv: matrix must be square"
         allocate(ac(1:n,1:n), source=a)
         allocate(ipiv(1:n))
         call dgetrf(n, n, ac, n, ipiv, info)
         if (info /= 0) stop "linalg_inv: dgetrf failed"
         allocate(work(1))
         lwork = -1
         call dgetri(n, ac, n, ipiv, work, lwork, info)
         if (info /= 0) stop "linalg_inv: dgetri workspace query failed"
         lwork = max(1, int(work(1)))
         deallocate(work)
         allocate(work(1:lwork))
         call dgetri(n, ac, n, ipiv, work, lwork, info)
         if (info /= 0) stop "linalg_inv: dgetri failed"
         allocate(ai(1:n,1:n))
         ai = ac
      end function linalg_inv

      subroutine linalg_eig(a, w, v)
         real(kind=dp), intent(in) :: a(:,:)
         real(kind=dp), allocatable, intent(out) :: w(:), v(:,:)
         real(kind=dp), allocatable :: ac(:,:), wr(:), wi(:), vr(:,:), vl_dummy(:,:), work(:)
         integer :: n, info, lwork
         interface
            subroutine dgeev(jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr, ldvr, work, lwork, info)
               character(len=1), intent(in) :: jobvl, jobvr
               integer, intent(in) :: n, lda, ldvl, ldvr, lwork
               integer, intent(out) :: info
               double precision, intent(inout) :: a(lda,*)
               double precision, intent(out) :: wr(*), wi(*), vl(ldvl,*), vr(ldvr,*), work(*)
            end subroutine dgeev
         end interface
         n = size(a,1)
         if (size(a,2) /= n) stop "linalg_eig: matrix must be square"
         allocate(ac(1:n,1:n), source=a)
         allocate(wr(1:n), wi(1:n))
         allocate(vr(1:n,1:n))
         allocate(vl_dummy(1,1))
         allocate(work(1))
         lwork = -1
         call dgeev('N', 'V', n, ac, n, wr, wi, vl_dummy, 1, vr, n, work, lwork, info)
         if (info /= 0) stop "linalg_eig: dgeev workspace query failed"
         lwork = max(1, int(work(1)))
         deallocate(work)
         allocate(work(1:lwork))
         call dgeev('N', 'V', n, ac, n, wr, wi, vl_dummy, 1, vr, n, work, lwork, info)
         if (info /= 0) stop "linalg_eig: dgeev failed"
         if (maxval(abs(wi)) > 1.0e-12_dp) stop "linalg_eig: complex eigenvalues not supported in this transpiler path"
         allocate(w(1:n), source=wr)
         allocate(v(1:n,1:n), source=vr)
      end subroutine linalg_eig

      subroutine linalg_svd(a, u, s, vt)
         real(kind=dp), intent(in) :: a(:,:)
         real(kind=dp), allocatable, intent(out) :: u(:,:), s(:), vt(:,:)
         real(kind=dp), allocatable :: ac(:,:), work(:)
         integer :: m, n, k, info, lwork
         interface
            subroutine dgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)
               character(len=1), intent(in) :: jobu, jobvt
               integer, intent(in) :: m, n, lda, ldu, ldvt, lwork
               integer, intent(out) :: info
               double precision, intent(inout) :: a(lda,*)
               double precision, intent(out) :: s(*), u(ldu,*), vt(ldvt,*), work(*)
            end subroutine dgesvd
         end interface
         m = size(a,1)
         n = size(a,2)
         k = min(m,n)
         allocate(ac(1:m,1:n), source=a)
         allocate(u(1:m,1:m), source=0.0_dp)
         allocate(s(1:k), source=0.0_dp)
         allocate(vt(1:n,1:n), source=0.0_dp)
         allocate(work(1))
         lwork = -1
         call dgesvd('A', 'A', m, n, ac, m, s, u, m, vt, n, work, lwork, info)
         if (info /= 0) stop "linalg_svd: dgesvd workspace query failed"
         lwork = max(1, int(work(1)))
         deallocate(work)
         allocate(work(1:lwork))
         call dgesvd('A', 'A', m, n, ac, m, s, u, m, vt, n, work, lwork, info)
         if (info /= 0) stop "linalg_svd: dgesvd failed"
      end subroutine linalg_svd

      pure real(kind=dp) function var(x, ddof)
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
            var = 0.0_dp
            return
         end if
         mu = mean(x)
         var = sum((x - mu)**2) / real(n - d, kind=dp)
      end function var

      pure real(kind=dp) function mean(x)
         real(kind=dp), intent(in) :: x(:)
         if (size(x) <= 0) then
            mean = 0.0_dp
         else
            mean = sum(x) / real(size(x), kind=dp)
         end if
      end function mean

      pure real(kind=dp) function std(x, ddof)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in), optional :: ddof
         if (present(ddof)) then
            std = sqrt(var(x, ddof))
         else
            std = sqrt(var(x))
         end if
      end function std

      pure elemental real(kind=dp) function log2(x)
         real(kind=dp), intent(in) :: x
         real(kind=dp), parameter :: log2_base = log(2.0_dp)
         log2 = log(x) / log2_base
      end function log2

      pure real(kind=dp) function nansum(x)
         real(kind=dp), intent(in) :: x(:)
         integer :: i
         nansum = 0.0_dp
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) nansum = nansum + x(i)
         end do
      end function nansum

      pure real(kind=dp) function nanmean(x)
         real(kind=dp), intent(in) :: x(:)
         integer :: i, cnt
         nanmean = 0.0_dp
         cnt = 0
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) then
               nanmean = nanmean + x(i)
               cnt = cnt + 1
            end if
         end do
         if (cnt <= 0) then
            nanmean = ieee_value(0.0_dp, ieee_quiet_nan)
         else
            nanmean = nanmean / real(cnt, kind=dp)
         end if
      end function nanmean

      pure real(kind=dp) function nanvar(x, ddof)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in), optional :: ddof
         integer :: i, cnt, d
         real(kind=dp) :: mu, ss
         if (present(ddof)) then
            d = ddof
         else
            d = 0
         end if
         mu = nanmean(x)
         if (ieee_is_nan(mu)) then
            nanvar = ieee_value(0.0_dp, ieee_quiet_nan)
            return
         end if
         cnt = 0
         ss = 0.0_dp
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) then
               cnt = cnt + 1
               ss = ss + (x(i) - mu)**2
            end if
         end do
         if (cnt - d <= 0) then
            nanvar = ieee_value(0.0_dp, ieee_quiet_nan)
         else
            nanvar = ss / real(cnt - d, kind=dp)
         end if
      end function nanvar

      pure real(kind=dp) function nanstd(x, ddof)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in), optional :: ddof
         if (present(ddof)) then
            nanstd = sqrt(nanvar(x, ddof))
         else
            nanstd = sqrt(nanvar(x))
         end if
      end function nanstd

      pure real(kind=dp) function nanmin(x)
         real(kind=dp), intent(in) :: x(:)
         integer :: i
         logical :: found
         found = .false.
         nanmin = ieee_value(0.0_dp, ieee_quiet_nan)
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) then
               if (.not. found) then
                  nanmin = x(i)
                  found = .true.
               else
                  if (x(i) < nanmin) nanmin = x(i)
               end if
            end if
         end do
      end function nanmin

      pure real(kind=dp) function nanmax(x)
         real(kind=dp), intent(in) :: x(:)
         integer :: i
         logical :: found
         found = .false.
         nanmax = ieee_value(0.0_dp, ieee_quiet_nan)
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) then
               if (.not. found) then
                  nanmax = x(i)
                  found = .true.
               else
                  if (x(i) > nanmax) nanmax = x(i)
               end if
            end if
         end do
      end function nanmax

      pure integer function nanargmin(x)
         real(kind=dp), intent(in) :: x(:)
         integer :: i, best
         logical :: found
         found = .false.
         best = -1
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) then
               if (.not. found) then
                  best = i
                  found = .true.
               else
                  if (x(i) < x(best)) best = i
               end if
            end if
         end do
         if (found) then
            nanargmin = best - 1
         else
            nanargmin = -1
         end if
      end function nanargmin

      pure integer function nanargmax(x)
         real(kind=dp), intent(in) :: x(:)
         integer :: i, best
         logical :: found
         found = .false.
         best = -1
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) then
               if (.not. found) then
                  best = i
                  found = .true.
               else
                  if (x(i) > x(best)) best = i
               end if
            end if
         end do
         if (found) then
            nanargmax = best - 1
         else
            nanargmax = -1
         end if
      end function nanargmax

end module python_mod
