module r_mod
use, intrinsic :: iso_fortran_env, only: real64, int64
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
   & ieee_is_finite
implicit none
private
public :: runif1, runif_vec, rnorm1, rnorm_vec, random_choice2_prob, &
   & randint_range, sample_int, quantile, dnorm, tail, cbind2, cbind, numeric, &
   & pmax, sd, colMeans, count_ws_tokens, read_real_vector, read_table_real_matrix, &
   & write_table_real_matrix, lm_fit_t, lm_fit_general, lm_predict_general, &
   & print_lm_summary, print_lm_coef_rstyle, cov, cor, r_seq_int, r_seq_len, &
   & r_seq_int_by, r_seq_int_length, r_seq_real_by, r_seq_real_length, &
   & r_rep_real, r_rep_int, r_array_real, r_array_int, r_array_char, matrix, &
   & r_matmul, r_add, r_sub, r_mul, r_div, print_matrix, print_real_scalar, print_real_vector, set_print_int_like, &
   & set_print_int_like_tol, set_recycle_warn, set_recycle_stop
integer, parameter :: dp = real64
logical :: print_int_like_default = .true.
real(kind=dp) :: print_int_like_tol = 1000.0_dp * epsilon(1.0_dp)
logical :: recycle_warn_default = .false.
logical :: recycle_stop_default = .false.
type :: lm_fit_t
   real(kind=dp), allocatable :: coef(:), fitted(:), resid(:)
   real(kind=dp) :: sigma, r_squared, adj_r_squared
end type lm_fit_t

interface cov
   module procedure cov_vec
   module procedure cov_mat
end interface cov

interface cor
   module procedure cor_vec
   module procedure cor_mat
end interface cor

interface matrix
   module procedure matrix_real
   module procedure matrix_int
end interface matrix

interface r_matmul
   module procedure r_matmul_vv_real
   module procedure r_matmul_vv_int
   module procedure r_matmul_vv_real_int
   module procedure r_matmul_vv_int_real
   module procedure r_matmul_mv_real
   module procedure r_matmul_mv_int
   module procedure r_matmul_mv_real_int
   module procedure r_matmul_mv_int_real
   module procedure r_matmul_vm_real
   module procedure r_matmul_vm_int
   module procedure r_matmul_vm_real_int
   module procedure r_matmul_vm_int_real
   module procedure r_matmul_mm_real
   module procedure r_matmul_mm_int
   module procedure r_matmul_mm_real_int
   module procedure r_matmul_mm_int_real
end interface r_matmul

interface r_add
   module procedure r_add_vv
   module procedure r_add_vs
   module procedure r_add_sv
end interface r_add

interface r_sub
   module procedure r_sub_vv
   module procedure r_sub_vs
   module procedure r_sub_sv
end interface r_sub

interface r_mul
   module procedure r_mul_vv
   module procedure r_mul_vs
   module procedure r_mul_sv
end interface r_mul

interface r_div
   module procedure r_div_vv
   module procedure r_div_vs
   module procedure r_div_sv
end interface r_div

interface r_array
   module procedure r_array_real
   module procedure r_array_int
   module procedure r_array_char
end interface r_array

interface print_matrix
   module procedure print_matrix_real
   module procedure print_matrix_int
end interface print_matrix

contains

subroutine set_print_int_like(flag)
! Enable/disable integer-like rendering for real matrix printing.
logical, intent(in) :: flag
print_int_like_default = flag
end subroutine set_print_int_like

subroutine set_print_int_like_tol(tol)
! Set tolerance used for integer-like real rendering.
real(kind=dp), intent(in) :: tol
if (tol > 0.0_dp) print_int_like_tol = tol
end subroutine set_print_int_like_tol

subroutine set_recycle_warn(flag)
! Enable/disable warnings for non-multiple recycling lengths.
logical, intent(in) :: flag
recycle_warn_default = flag
end subroutine set_recycle_warn

subroutine set_recycle_stop(flag)
! Enable/disable error stop for non-multiple recycling lengths.
logical, intent(in) :: flag
recycle_stop_default = flag
end subroutine set_recycle_stop

subroutine maybe_warn_recycle(op, na, nb)
! Warn/stop when any vector recycling occurs (lengths differ).
character(len=*), intent(in) :: op
integer, intent(in) :: na, nb
integer :: nmax, nmin
if ((.not. recycle_warn_default) .and. (.not. recycle_stop_default)) return
nmax = max(na, nb)
nmin = min(na, nb)
if (nmin <= 0) return
if (nmax /= nmin) then
   if (recycle_stop_default) then
      error stop "recycle-stop: vector recycling occurred (lengths differ)"
   end if
   if (recycle_warn_default) then
      write(*,'(a)') "Warning message:"
      write(*,'(3a)') "In ", trim(op), " : vector recycling occurred (lengths differ)"
   end if
end if
end subroutine maybe_warn_recycle

subroutine print_real_scalar(x, int_like)
! Print one real value using integer format when integer-like.
real(kind=dp), intent(in) :: x
logical, intent(in), optional :: int_like
logical :: use_int_like, as_int
integer(kind=int64) :: k
real(kind=dp) :: tol
use_int_like = print_int_like_default
if (present(int_like)) use_int_like = int_like
as_int = .false.
if (use_int_like) then
   if (ieee_is_finite(x) .and. abs(x) <= real(huge(0_int64), kind=dp)) then
      k = nint(x, kind=int64)
      tol = print_int_like_tol * max(1.0_dp, abs(x))
      as_int = abs(x - real(k, kind=dp)) <= tol
   end if
end if
if (as_int) then
   write(*,"(i0)") k
else
   write(*,"(g0)") x
end if
end subroutine print_real_scalar

subroutine print_real_vector(x, int_like)
! Print one real vector; use integer format when all values are integer-like.
real(kind=dp), intent(in) :: x(:)
logical, intent(in), optional :: int_like
logical :: use_int_like, all_int
integer :: i
integer(kind=int64) :: k
real(kind=dp) :: r, tol
use_int_like = print_int_like_default
if (present(int_like)) use_int_like = int_like
all_int = .false.
if (use_int_like) then
   all_int = .true.
   do i = 1, size(x)
      r = x(i)
      if (.not. ieee_is_finite(r)) then
         all_int = .false.
         exit
      end if
      if (abs(r) > real(huge(0_int64), kind=dp)) then
         all_int = .false.
         exit
      end if
      k = nint(r, kind=int64)
      tol = print_int_like_tol * max(1.0_dp, abs(r))
      if (abs(r - real(k, kind=dp)) > tol) then
         all_int = .false.
         exit
      end if
   end do
end if
if (all_int) then
   do i = 1, size(x)
      k = nint(x(i), kind=int64)
      write(*,"(i0)", advance="no") k
      if (i < size(x)) write(*,"(a)", advance="no") " "
   end do
   write(*,*)
else
   write(*,"(*(g0,1x))") x
end if
end subroutine print_real_vector

pure function r_seq_int(a, b) result(out)
! Return integer sequence a, a+step, ..., b with step +/-1.
integer, intent(in) :: a, b
integer, allocatable :: out(:)
integer :: i, n, step
n = abs(b - a) + 1
allocate(out(n))
step = merge(1, -1, a <= b)
do i = 1, n
   out(i) = a + (i - 1) * step
end do
end function r_seq_int

pure function r_seq_len(n) result(out)
! Return integer sequence 1..n (empty for n<=0).
integer, intent(in) :: n
integer, allocatable :: out(:)
integer :: i
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(out(n))
do i = 1, n
   out(i) = i
end do
end function r_seq_len

pure function r_seq_int_by(a, b, by) result(out)
! Return integer sequence from a to b using explicit integer step by.
integer, intent(in) :: a, b, by
integer, allocatable :: out(:)
integer :: i, n
if (by == 0) then
   allocate(out(0))
   return
end if
if ((by > 0 .and. a > b) .or. (by < 0 .and. a < b)) then
   allocate(out(0))
   return
end if
n = (abs(b - a) / abs(by)) + 1
allocate(out(n))
do i = 1, n
   out(i) = a + (i - 1) * by
end do
end function r_seq_int_by

pure function r_seq_int_length(a, b, n) result(out)
! Return n integer values linearly spaced from a to b.
integer, intent(in) :: a, b, n
integer, allocatable :: out(:)
integer :: i
real(kind=dp) :: t
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(out(n))
if (n == 1) then
   out(1) = a
   return
end if
do i = 1, n
   t = real(i - 1, kind=dp) / real(n - 1, kind=dp)
   out(i) = nint((1.0_dp - t) * real(a, kind=dp) + t * real(b, kind=dp))
end do
end function r_seq_int_length

pure function r_seq_real_by(a, b, by) result(out)
! Return real sequence from a to b with real step by.
real(kind=dp), intent(in) :: a, b, by
real(kind=dp), allocatable :: out(:)
integer :: i, n
if (by == 0.0_dp) then
   allocate(out(0))
   return
end if
if ((by > 0.0_dp .and. a > b) .or. (by < 0.0_dp .and. a < b)) then
   allocate(out(0))
   return
end if
n = int(floor((b - a) / by + 1.0e-12_dp)) + 1
if (n < 0) n = 0
allocate(out(n))
do i = 1, n
   out(i) = a + real(i - 1, kind=dp) * by
end do
end function r_seq_real_by

pure function r_seq_real_length(a, b, n) result(out)
! Return n real values linearly spaced from a to b.
real(kind=dp), intent(in) :: a, b
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:)
integer :: i
real(kind=dp) :: t
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(out(n))
if (n == 1) then
   out(1) = a
   return
end if
do i = 1, n
   t = real(i - 1, kind=dp) / real(n - 1, kind=dp)
   out(i) = (1.0_dp - t) * a + t * b
end do
end function r_seq_real_length

pure function r_rep_real(x, times, each, len_out, times_vec) result(out)
! Repeat elements/blocks of a real vector (R-like rep subset).
real(kind=dp), intent(in) :: x(:)
integer, intent(in), optional :: times, each, len_out
integer, intent(in), optional :: times_vec(:)
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: y(:), z(:)
integer :: i, j, n, e, t, k, m, need, c
n = size(x)
if (n <= 0) then
   allocate(out(0))
   return
end if
if (present(each)) then
   e = each
else
   e = 1
end if
if (e < 1) e = 1
allocate(y(n * e))
k = 0
do i = 1, n
   do j = 1, e
      k = k + 1
      y(k) = x(i)
   end do
end do
if (present(times_vec)) then
   m = size(y)
   c = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      if (t > 0) c = c + t
   end do
   allocate(z(c))
   k = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      do j = 1, max(0, t)
         k = k + 1
         z(k) = y(i)
      end do
   end do
else
   if (present(times)) then
      t = times
   else
      t = 1
   end if
   if (t < 0) t = 0
   allocate(z(size(y) * t))
   k = 0
   do j = 1, t
      do i = 1, size(y)
         k = k + 1
         z(k) = y(i)
      end do
   end do
end if
if (present(len_out)) then
   need = max(0, len_out)
   if (need == 0) then
      allocate(out(0))
      return
   end if
   allocate(out(need))
   if (size(z) > 0) then
      do i = 1, need
         out(i) = z(mod(i - 1, size(z)) + 1)
      end do
   end if
else
   out = z
end if
end function r_rep_real

pure function r_rep_int(x, times, each, len_out, times_vec) result(out)
! Repeat elements/blocks of an integer vector (R-like rep subset).
integer, intent(in) :: x(:)
integer, intent(in), optional :: times, each, len_out
integer, intent(in), optional :: times_vec(:)
integer, allocatable :: out(:)
integer, allocatable :: y(:), z(:)
integer :: i, j, n, e, t, k, m, need, c
n = size(x)
if (n <= 0) then
   allocate(out(0))
   return
end if
if (present(each)) then
   e = each
else
   e = 1
end if
if (e < 1) e = 1
allocate(y(n * e))
k = 0
do i = 1, n
   do j = 1, e
      k = k + 1
      y(k) = x(i)
   end do
end do
if (present(times_vec)) then
   m = size(y)
   c = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      if (t > 0) c = c + t
   end do
   allocate(z(c))
   k = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      do j = 1, max(0, t)
         k = k + 1
         z(k) = y(i)
      end do
   end do
else
   if (present(times)) then
      t = times
   else
      t = 1
   end if
   if (t < 0) t = 0
   allocate(z(size(y) * t))
   k = 0
   do j = 1, t
      do i = 1, size(y)
         k = k + 1
         z(k) = y(i)
      end do
   end do
end if
if (present(len_out)) then
   need = max(0, len_out)
   if (need == 0) then
      allocate(out(0))
      return
   end if
   allocate(out(need))
   if (size(z) > 0) then
      do i = 1, need
         out(i) = z(mod(i - 1, size(z)) + 1)
      end do
   end if
else
   out = z
end if
end function r_rep_int

function runif1() result(u)
! Return one U(0,1) variate.
real(kind=dp) :: u
call random_number(u)
end function runif1

function runif_vec(n) result(x)
! Return a length-n vector of U(0,1) variates.
integer, intent(in) :: n
real(kind=dp), allocatable :: x(:)
allocate(x(n))
call random_number(x)
end function runif_vec

function rnorm1() result(x)
! Return one N(0,1) variate (Box-Muller).
real(kind=dp) :: x
real(kind=dp) :: u1, u2
do
   call random_number(u1)
   call random_number(u2)
   if (u1 > tiny(1.0_dp)) exit
end do
x = sqrt(-2.0_dp * log(u1)) * cos(2.0_dp * acos(-1.0_dp) * u2)
end function rnorm1

function rnorm_vec(n) result(x)
! Return a length-n vector of N(0,1) variates.
integer, intent(in) :: n
real(kind=dp), allocatable :: x(:)
integer :: i
real(kind=dp) :: u1, u2, r, t
allocate(x(n))
i = 1
do while (i <= n)
   call random_number(u1)
   call random_number(u2)
   if (u1 <= tiny(1.0_dp)) cycle
   r = sqrt(-2.0_dp * log(u1))
   t = 2.0_dp * acos(-1.0_dp) * u2
   x(i) = r * cos(t)
   if (i + 1 <= n) x(i + 1) = r * sin(t)
   i = i + 2
end do
end function rnorm_vec

function random_choice2_prob(n, p1) result(z)
! Sample n labels in {1,2} with P(label=1)=p1.
integer, intent(in) :: n
real(kind=dp), intent(in) :: p1
integer, allocatable :: z(:)
integer :: i
real(kind=dp) :: u
allocate(z(n))
do i = 1, n
   call random_number(u)
   z(i) = merge(1, 2, u < p1)
end do
end function random_choice2_prob

function randint_range(n, lo, hi) result(out)
! Sample n integers uniformly from [lo, hi].
integer, intent(in) :: n, lo, hi
integer, allocatable :: out(:)
integer :: i, span
real(kind=dp) :: u
if (hi < lo) then
   allocate(out(0))
   return
end if
span = hi - lo + 1
allocate(out(n))
do i = 1, n
   call random_number(u)
   out(i) = lo + int(u * span)
   if (out(i) > hi) out(i) = hi
end do
end function randint_range

function sample_int(n, size_, replace, prob) result(out)
! R-like sample.int with optional replacement and probabilities.
integer, intent(in) :: n
integer, intent(in), optional :: size_
logical, intent(in), optional :: replace
real(kind=dp), intent(in), optional :: prob(:)
integer, allocatable :: out(:), pool(:)
real(kind=dp), allocatable :: w(:), cdf(:)
integer :: m, i, j, pick, tmp
logical :: rep
real(kind=dp) :: u, s, acc

m = n
if (present(size_)) m = size_
rep = .false.
if (present(replace)) rep = replace

if (n < 1) then
   if (m == 0) then
      allocate(out(0))
      return
   end if
   error stop "sample_int: n must be >= 1"
end if
if (m < 0) error stop "sample_int: size must be >= 0"
if ((.not. rep) .and. (m > n)) error stop &
   & "sample_int: size > n without replacement"
if (present(prob)) then
   if (size(prob) /= n) error stop "sample_int: prob length mismatch"
   if (any(prob < 0.0_dp)) error stop "sample_int: prob must be nonnegative"
   if (sum(prob) <= 0.0_dp) error stop "sample_int: prob sum must be positive"
end if

allocate(out(m))
if (m == 0) return

if (present(prob)) then
   if (rep) then
      allocate(cdf(n))
      s = sum(prob)
      cdf(1) = prob(1) / s
      do i = 2, n
         cdf(i) = cdf(i - 1) + prob(i) / s
      end do
      cdf(n) = 1.0_dp
      do i = 1, m
         call random_number(u)
         pick = 1
         do while (pick < n .and. u > cdf(pick))
            pick = pick + 1
         end do
         out(i) = pick
      end do
   else
      allocate(w(n))
      w = prob
      do i = 1, m
         s = sum(w)
         if (s <= 0.0_dp) error stop "sample_int: depleted probability mass"
         call random_number(u)
         u = u * s
         acc = 0.0_dp
         pick = n
         do j = 1, n
            acc = acc + w(j)
            if (u <= acc) then
               pick = j
               exit
            end if
         end do
         out(i) = pick
         w(pick) = 0.0_dp
      end do
   end if
else
   if (rep) then
      do i = 1, m
         call random_number(u)
         out(i) = 1 + int(u * real(n, kind=dp))
         if (out(i) > n) out(i) = n
      end do
   else
      allocate(pool(n))
      pool = [(j, j=1,n)]
      do i = 1, m
         call random_number(u)
         pick = i + int(u * real(n - i + 1, kind=dp))
         if (pick > n) pick = n
         tmp = pool(i)
         pool(i) = pool(pick)
         pool(pick) = tmp
         out(i) = pool(i)
      end do
   end if
end if
end function sample_int

pure subroutine sort_increasing(x)
! Sort a real vector in increasing order (insertion sort).
real(kind=dp), intent(inout) :: x(:)
integer :: i, j
real(kind=dp) :: key
do i = 2, size(x)
   key = x(i)
   j = i - 1
   do while (j >= 1 .and. x(j) > key)
      x(j + 1) = x(j)
      j = j - 1
   end do
   x(j + 1) = key
end do
end subroutine sort_increasing

pure function quantile(x, probs, names, type) result(out)
! Compute Type-7 quantiles for a numeric vector.
real(kind=dp), intent(in) :: x(:), probs(:)
logical, intent(in), optional :: names
integer, intent(in), optional :: type
real(kind=dp), allocatable :: out(:), xs(:)
integer :: n, i, j
real(kind=dp) :: p, h, g
n = size(x)
allocate(out(size(probs)))
if (n <= 0) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
xs = x
call sort_increasing(xs)
do i = 1, size(probs)
   p = min(1.0_dp, max(0.0_dp, probs(i)))
   h = (n - 1) * p + 1.0_dp
   j = int(floor(h))
   g = h - j
   if (j < 1) then
      out(i) = xs(1)
   else if (j >= n) then
      out(i) = xs(n)
   else
      out(i) = (1.0_dp - g) * xs(j) + g * xs(j + 1)
   end if
end do
! names/type accepted for API compatibility in this subset.
if (present(names)) continue
if (present(type)) continue
end function quantile

pure function dnorm(x, mean, sd, log_) result(out)
! Evaluate normal density (or log-density) elementwise.
real(kind=dp), intent(in) :: x(:), mean, sd
logical, intent(in), optional :: log_
real(kind=dp), allocatable :: out(:), z(:)
logical :: l
l = .false.
if (present(log_)) l = log_
if (sd <= 0.0_dp) then
   allocate(out(size(x)))
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
z = (x - mean) / sd
out = -0.5_dp * log(2.0_dp * acos(-1.0_dp)) - log(sd) - 0.5_dp * z**2
if (.not. l) out = exp(out)
end function dnorm

pure function tail(x, n) result(out)
! Return the last n elements of a vector.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:)
integer :: m, n0
n0 = size(x)
m = max(0, min(n, n0))
allocate(out(m))
if (m > 0) out = x(n0 - m + 1:n0)
end function tail

pure function cbind2(a, b) result(out)
! Bind two vectors as columns of a 2D array.
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:,:)
integer :: n
n = min(size(a), size(b))
allocate(out(n, 2))
if (n > 0) then
   out(:,1) = a(1:n)
   out(:,2) = b(1:n)
end if
end function cbind2

pure function cbind(r1, r2, r3) result(out)
! Bind two or three vectors as columns of a 2D array.
real(kind=dp), intent(in) :: r1(:), r2(:)
real(kind=dp), intent(in), optional :: r3(:)
real(kind=dp), allocatable :: out(:,:)
integer :: n
if (.not. present(r3)) then
   out = cbind2(r1, r2)
   return
end if
n = min(size(r1), min(size(r2), size(r3)))
allocate(out(n, 3))
if (n > 0) then
   out(:,1) = r1(1:n)
   out(:,2) = r2(1:n)
   out(:,3) = r3(1:n)
end if
end function cbind

pure function matrix_real(x, nrow, ncol) result(out)
! Build matrix with R-like recycling in column-major order.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: nrow, ncol
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: buf(:)
integer :: i, need_n, nx
nx = size(x)
if (nrow < 0 .or. ncol < 0) then
   allocate(out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(out(nrow, ncol))
   out = 0.0_dp
   return
end if
allocate(buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
out = reshape(buf, [nrow, ncol])
end function matrix_real

pure function matrix_int(x, nrow, ncol) result(out)
! Integer variant of matrix() with R-like recycling.
integer, intent(in) :: x(:)
integer, intent(in) :: nrow, ncol
integer, allocatable :: out(:,:)
integer, allocatable :: buf(:)
integer :: i, need_n, nx
nx = size(x)
if (nrow < 0 .or. ncol < 0) then
   allocate(out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(out(nrow, ncol))
   out = 0
   return
end if
allocate(buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
out = reshape(buf, [nrow, ncol])
end function matrix_int

pure function r_matmul_vv_real(a, b) result(out)
! Matrix-product helper for real vectors: dot product.
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp) :: out
out = dot_product(a, b)
end function r_matmul_vv_real

pure function r_matmul_vv_int(a, b) result(out)
! Matrix-product helper for integer vectors: dot product (real result).
integer, intent(in) :: a(:), b(:)
real(kind=dp) :: out
out = dot_product(real(a, kind=dp), real(b, kind=dp))
end function r_matmul_vv_int

pure function r_matmul_vv_real_int(a, b) result(out)
! Matrix-product helper for mixed real/int vectors: dot product.
real(kind=dp), intent(in) :: a(:)
integer, intent(in) :: b(:)
real(kind=dp) :: out
out = dot_product(a, real(b, kind=dp))
end function r_matmul_vv_real_int

pure function r_matmul_vv_int_real(a, b) result(out)
! Matrix-product helper for mixed int/real vectors: dot product.
integer, intent(in) :: a(:)
real(kind=dp), intent(in) :: b(:)
real(kind=dp) :: out
out = dot_product(real(a, kind=dp), b)
end function r_matmul_vv_int_real

pure function r_matmul_mv_real(a, b) result(out)
! Matrix-product helper for real matrix-vector multiplication.
real(kind=dp), intent(in) :: a(:,:), b(:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(a, b)
end function r_matmul_mv_real

pure function r_matmul_mv_int(a, b) result(out)
! Matrix-product helper for integer matrix-vector multiplication.
integer, intent(in) :: a(:,:), b(:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(real(a, kind=dp), real(b, kind=dp))
end function r_matmul_mv_int

pure function r_matmul_mv_real_int(a, b) result(out)
! Matrix-product helper for mixed real/int matrix-vector multiplication.
real(kind=dp), intent(in) :: a(:,:)
integer, intent(in) :: b(:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(a, real(b, kind=dp))
end function r_matmul_mv_real_int

pure function r_matmul_mv_int_real(a, b) result(out)
! Matrix-product helper for mixed int/real matrix-vector multiplication.
integer, intent(in) :: a(:,:)
real(kind=dp), intent(in) :: b(:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(real(a, kind=dp), b)
end function r_matmul_mv_int_real

pure function r_matmul_vm_real(a, b) result(out)
! Matrix-product helper for real vector-matrix multiplication.
real(kind=dp), intent(in) :: a(:), b(:,:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(b, 2)))
out = matmul(a, b)
end function r_matmul_vm_real

pure function r_matmul_vm_int(a, b) result(out)
! Matrix-product helper for integer vector-matrix multiplication.
integer, intent(in) :: a(:), b(:,:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(b, 2)))
out = matmul(real(a, kind=dp), real(b, kind=dp))
end function r_matmul_vm_int

pure function r_matmul_vm_real_int(a, b) result(out)
! Matrix-product helper for mixed real/int vector-matrix multiplication.
real(kind=dp), intent(in) :: a(:)
integer, intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(b, 2)))
out = matmul(a, real(b, kind=dp))
end function r_matmul_vm_real_int

pure function r_matmul_vm_int_real(a, b) result(out)
! Matrix-product helper for mixed int/real vector-matrix multiplication.
integer, intent(in) :: a(:)
real(kind=dp), intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(b, 2)))
out = matmul(real(a, kind=dp), b)
end function r_matmul_vm_int_real

pure function r_matmul_mm_real(a, b) result(out)
! Matrix-product helper for real matrix-matrix multiplication.
real(kind=dp), intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(a, b)
end function r_matmul_mm_real

pure function r_matmul_mm_int(a, b) result(out)
! Matrix-product helper for integer matrix-matrix multiplication.
integer, intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(real(a, kind=dp), real(b, kind=dp))
end function r_matmul_mm_int

pure function r_matmul_mm_real_int(a, b) result(out)
! Matrix-product helper for mixed real/int matrix-matrix multiplication.
real(kind=dp), intent(in) :: a(:,:)
integer, intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(a, real(b, kind=dp))
end function r_matmul_mm_real_int

pure function r_matmul_mm_int_real(a, b) result(out)
! Matrix-product helper for mixed int/real matrix-matrix multiplication.
integer, intent(in) :: a(:,:)
real(kind=dp), intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(real(a, kind=dp), b)
end function r_matmul_mm_int_real

function r_add_vv(a, b) result(out)
! Recycle and add two vectors (R-style recycling).
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:)
integer :: i, n, na, nb
na = size(a)
nb = size(b)
n = max(na, nb)
allocate(out(n))
if (n <= 0) return
call maybe_warn_recycle("x + y", na, nb)
do i = 1, n
   out(i) = a(modulo(i - 1, na) + 1) + b(modulo(i - 1, nb) + 1)
end do
end function r_add_vv

pure function r_add_vs(a, b) result(out)
! Add scalar to vector.
real(kind=dp), intent(in) :: a(:), b
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(a)
allocate(out(n))
if (n > 0) out = a + b
end function r_add_vs

pure function r_add_sv(a, b) result(out)
! Add vector to scalar.
real(kind=dp), intent(in) :: a, b(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(b)
allocate(out(n))
if (n > 0) out = a + b
end function r_add_sv

function r_sub_vv(a, b) result(out)
! Recycle and subtract two vectors (a - b).
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:)
integer :: i, n, na, nb
na = size(a)
nb = size(b)
n = max(na, nb)
allocate(out(n))
if (n <= 0) return
call maybe_warn_recycle("x - y", na, nb)
do i = 1, n
   out(i) = a(modulo(i - 1, na) + 1) - b(modulo(i - 1, nb) + 1)
end do
end function r_sub_vv

pure function r_sub_vs(a, b) result(out)
! Subtract scalar from vector.
real(kind=dp), intent(in) :: a(:), b
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(a)
allocate(out(n))
if (n > 0) out = a - b
end function r_sub_vs

pure function r_sub_sv(a, b) result(out)
! Subtract vector from scalar.
real(kind=dp), intent(in) :: a, b(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(b)
allocate(out(n))
if (n > 0) out = a - b
end function r_sub_sv

function r_mul_vv(a, b) result(out)
! Recycle and multiply two vectors.
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:)
integer :: i, n, na, nb
na = size(a)
nb = size(b)
n = max(na, nb)
allocate(out(n))
if (n <= 0) return
call maybe_warn_recycle("x * y", na, nb)
do i = 1, n
   out(i) = a(modulo(i - 1, na) + 1) * b(modulo(i - 1, nb) + 1)
end do
end function r_mul_vv

pure function r_mul_vs(a, b) result(out)
! Multiply vector by scalar.
real(kind=dp), intent(in) :: a(:), b
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(a)
allocate(out(n))
if (n > 0) out = a * b
end function r_mul_vs

pure function r_mul_sv(a, b) result(out)
! Multiply scalar by vector.
real(kind=dp), intent(in) :: a, b(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(b)
allocate(out(n))
if (n > 0) out = a * b
end function r_mul_sv

function r_div_vv(a, b) result(out)
! Recycle and divide two vectors (a / b).
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:)
integer :: i, n, na, nb
na = size(a)
nb = size(b)
n = max(na, nb)
allocate(out(n))
if (n <= 0) return
call maybe_warn_recycle("x / y", na, nb)
do i = 1, n
   out(i) = a(modulo(i - 1, na) + 1) / b(modulo(i - 1, nb) + 1)
end do
end function r_div_vv

pure function r_div_vs(a, b) result(out)
! Divide vector by scalar.
real(kind=dp), intent(in) :: a(:), b
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(a)
allocate(out(n))
if (n > 0) out = a / b
end function r_div_vs

pure function r_div_sv(a, b) result(out)
! Divide scalar by vector.
real(kind=dp), intent(in) :: a, b(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(b)
allocate(out(n))
if (n > 0) out = a / b
end function r_div_sv

pure function r_array_real(x, dim) result(out)
! Build 2D real array with R-like recycling.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: dim(:)
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: buf(:)
integer :: i, need_n, nx, nrow, ncol
if (size(dim) < 2) then
   allocate(out(0, 0))
   return
end if
nrow = dim(1)
ncol = dim(2)
nx = size(x)
if (nrow < 0 .or. ncol < 0) then
   allocate(out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(out(nrow, ncol))
   out = 0.0_dp
   return
end if
allocate(buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
out = reshape(buf, [nrow, ncol])
end function r_array_real

pure function r_array_int(x, dim) result(out)
! Build 2D integer array with R-like recycling.
integer, intent(in) :: x(:)
integer, intent(in) :: dim(:)
integer, allocatable :: out(:,:)
integer, allocatable :: buf(:)
integer :: i, need_n, nx, nrow, ncol
if (size(dim) < 2) then
   allocate(out(0, 0))
   return
end if
nrow = dim(1)
ncol = dim(2)
nx = size(x)
if (nrow < 0 .or. ncol < 0) then
   allocate(out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(out(nrow, ncol))
   out = 0
   return
end if
allocate(buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
out = reshape(buf, [nrow, ncol])
end function r_array_int

function r_array_char(x, dim) result(out)
! Build 2D character array with R-like recycling.
character(len=*), intent(in) :: x(:)
integer, intent(in) :: dim(:)
character(len=:), allocatable :: out(:,:)
character(len=:), allocatable :: buf(:)
integer :: i, need_n, nx, nrow, ncol, lch
if (size(dim) < 2) then
   allocate(character(len=1) :: out(0, 0))
   return
end if
nrow = dim(1)
ncol = dim(2)
nx = size(x)
lch = max(1, len(x))
if (nrow < 0 .or. ncol < 0) then
   allocate(character(len=lch) :: out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(character(len=lch) :: out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(character(len=lch) :: out(nrow, ncol))
   out = ""
   return
end if
allocate(character(len=lch) :: buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
allocate(character(len=lch) :: out(nrow, ncol))
out = reshape(buf, [nrow, ncol])
end function r_array_char

pure function numeric(n) result(out)
! Allocate a length-n real vector initialized to zero.
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:)
allocate(out(max(0, n)))
if (n > 0) out = 0.0_dp
end function numeric

pure elemental function pmax(a, b) result(out)
! Elementwise maximum of two real scalars.
real(kind=dp), intent(in) :: a, b
real(kind=dp) :: out
out = max(a, b)
end function pmax

pure function sd(x) result(out)
! Sample standard deviation (n-1 denominator).
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: out, m
integer :: n
n = size(x)
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
m = sum(x) / real(n, kind=dp)
out = sqrt(sum((x - m)**2) / real(n - 1, kind=dp))
end function sd

pure function colMeans(x) result(out)
! Column means of a numeric matrix.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(x, 1)
if (size(x, 2) <= 0) then
   allocate(out(0))
   return
end if
if (n <= 0) then
   allocate(out(size(x, 2)))
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
out = sum(x, dim=1) / real(n, kind=dp)
end function colMeans

pure function cov_vec(x, y) result(out)
! Sample covariance of two vectors (n-1 denominator).
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp) :: out, mx, my
integer :: n
n = min(size(x), size(y))
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
mx = sum(x(1:n)) / real(n, kind=dp)
my = sum(y(1:n)) / real(n, kind=dp)
out = sum((x(1:n) - mx) * (y(1:n) - my)) / real(n - 1, kind=dp)
end function cov_vec

pure function cov_mat(x) result(out)
! Sample covariance matrix of columns of x (n-1 denominator).
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: xc(:,:), mu(:)
integer :: n, p
n = size(x, 1)
p = size(x, 2)
if (p <= 0) then
   allocate(out(0,0))
   return
end if
if (n <= 1) then
   allocate(out(p, p))
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
mu = sum(x, dim=1) / real(n, kind=dp)
xc = x - spread(mu, dim=1, ncopies=n)
out = matmul(transpose(xc), xc) / real(n - 1, kind=dp)
end function cov_mat

pure function cor_vec(x, y) result(out)
! Sample correlation of two vectors.
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp) :: out, sdx, sdy
integer :: n
n = min(size(x), size(y))
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
sdx = sd(x(1:n))
sdy = sd(y(1:n))
if (.not. ieee_is_finite(sdx) .or. .not. ieee_is_finite(sdy) .or. sdx <= 0.0_dp .or. sdy <= 0.0_dp) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
out = cov_vec(x(1:n), y(1:n)) / (sdx * sdy)
end function cor_vec

pure function cor_mat(x) result(out)
! Sample correlation matrix of columns of x.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:,:), s(:)
real(kind=dp), allocatable :: c(:,:)
integer :: i, j, p
c = cov_mat(x)
p = size(c, 1)
if (p <= 0) then
   allocate(out(0,0))
   return
end if
allocate(out(p, p), s(p))
do i = 1, p
   s(i) = sqrt(c(i, i))
end do
do i = 1, p
   do j = 1, p
      if (.not. ieee_is_finite(s(i)) .or. .not. ieee_is_finite(s(j)) .or. s(i) <= 0.0_dp .or. s(j) <= 0.0_dp) then
         out(i, j) = ieee_value(0.0_dp, ieee_quiet_nan)
      else
         out(i, j) = c(i, j) / (s(i) * s(j))
      end if
   end do
end do
end function cor_mat

pure integer function count_ws_tokens(line) result(n_tok)
! Count whitespace-separated tokens in one text line.
character(len=*), intent(in) :: line
integer :: i, n
logical :: in_tok
n = len_trim(line)
n_tok = 0
in_tok = .false.
do i = 1, n
   if (line(i:i) /= " " .and. line(i:i) /= char(9)) then
      if (.not. in_tok) then
         n_tok = n_tok + 1
         in_tok = .true.
      end if
   else
      in_tok = .false.
   end if
end do
end function count_ws_tokens

subroutine read_real_vector(file_path, x)
! Read one real value per line into a vector.
character(len=*), intent(in) :: file_path
real(kind=dp), allocatable, intent(out) :: x(:)
integer :: fp, ios, n, cap, new_cap
real(kind=dp) :: v
n = 0
cap = 0
open(newunit=fp, file=file_path, status="old", action="read")
do
   read(fp, *, iostat=ios) v
   if (ios /= 0) exit
   if (n == cap) then
      new_cap = merge(1024, 2 * cap, cap == 0)
      block
         real(kind=dp), allocatable :: tmp(:)
         allocate(tmp(new_cap))
         if (allocated(x) .and. n > 0) tmp(1:n) = x(1:n)
         call move_alloc(tmp, x)
      end block
      cap = new_cap
   end if
   n = n + 1
   x(n) = v
end do
close(fp)
if (n == 0) then
   allocate(x(0))
else if (n < size(x)) then
   x = x(1:n)
end if
end subroutine read_real_vector

subroutine read_table_real_matrix(file_path, x)
! Read a whitespace-delimited numeric table into a matrix.
character(len=*), intent(in) :: file_path
real(kind=dp), allocatable, intent(out) :: x(:,:)
integer :: fp, ios, nrow, ncol, i
character(len=4096) :: line
nrow = 0
ncol = 0
open(newunit=fp, file=file_path, status="old", action="read")
do
   read(fp, "(A)", iostat=ios) line
   if (ios /= 0) exit
   if (len_trim(line) == 0) cycle
   nrow = nrow + 1
   if (ncol == 0) ncol = count_ws_tokens(line)
end do
if (nrow <= 0 .or. ncol <= 0) then
   allocate(x(0,0))
   close(fp)
   return
end if
allocate(x(nrow, ncol))
rewind(fp)
i = 0
do
   read(fp, "(A)", iostat=ios) line
   if (ios /= 0) exit
   if (len_trim(line) == 0) cycle
   i = i + 1
   read(line, *) x(i, 1:ncol)
end do
close(fp)
end subroutine read_table_real_matrix

subroutine write_table_real_matrix(file_path, x)
! Write a numeric matrix as a whitespace-delimited table.
character(len=*), intent(in) :: file_path
real(kind=dp), intent(in) :: x(:,:)
integer :: fp, i
open(newunit=fp, file=file_path, status="replace", action="write")
do i = 1, size(x, 1)
   write(fp, *) x(i, 1:size(x, 2))
end do
close(fp)
end subroutine write_table_real_matrix

subroutine print_matrix_real(x, int_like)
! Print a real matrix row-by-row; use integer format when all values are integer-like.
real(kind=dp), intent(in) :: x(:,:)
logical, intent(in), optional :: int_like
logical :: use_int_like, all_int
integer :: i, j
integer(kind=int64) :: k
real(kind=dp) :: r, tol
use_int_like = print_int_like_default
if (present(int_like)) use_int_like = int_like
all_int = .false.
if (use_int_like) then
   all_int = .true.
   do i = 1, size(x, 1)
      do j = 1, size(x, 2)
         r = x(i, j)
         if (.not. ieee_is_finite(r)) then
            all_int = .false.
            exit
         end if
         if (abs(r) > real(huge(0_int64), kind=dp)) then
            all_int = .false.
            exit
         end if
         k = nint(r, kind=int64)
         tol = print_int_like_tol * max(1.0_dp, abs(r))
         if (abs(r - real(k, kind=dp)) > tol) then
            all_int = .false.
            exit
         end if
      end do
      if (.not. all_int) exit
   end do
end if
if (all_int) then
   do i = 1, size(x, 1)
      do j = 1, size(x, 2)
         k = nint(x(i, j), kind=int64)
         write(*,"(i0)", advance="no") k
         if (j < size(x, 2)) write(*,"(a)", advance="no") " "
      end do
      write(*,*)
   end do
else
   do i = 1, size(x, 1)
      write(*,"(*(g0,1x))") x(i, :)
   end do
end if
end subroutine print_matrix_real

subroutine print_matrix_int(x)
! Print an integer matrix row-by-row.
integer, intent(in) :: x(:,:)
integer :: i
do i = 1, size(x, 1)
   write(*,"(*(i0,1x))") x(i, :)
end do
end subroutine print_matrix_int

pure function lm_predict_general(fit, xpred) result(yhat)
! Predict responses for a fitted linear model.
type(lm_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: xpred(:,:)
real(kind=dp), allocatable :: yhat(:)
integer :: p
p = size(xpred, 2)
if (size(fit%coef) /= p + 1) error stop "error: predictor count mismatch"
allocate(yhat(size(xpred, 1)))
yhat = fit%coef(1) + matmul(xpred, fit%coef(2:p+1))
end function lm_predict_general

subroutine solve_linear(a, b, x, ok)
! Solve Ax=b by Gaussian elimination with partial pivoting.
real(kind=dp), intent(inout) :: a(:,:), b(:)
real(kind=dp), intent(out) :: x(:)
logical, intent(out) :: ok
integer :: i, j, k, p, n
real(kind=dp) :: piv, fac, t
ok = .true.
n = size(b)
if (size(a,1) /= n .or. size(a,2) /= n .or. size(x) /= n) then
   ok = .false.
   x = 0.0_dp
   return
end if
do k = 1, n
   p = k
   piv = abs(a(k,k))
   do i = k + 1, n
      if (abs(a(i,k)) > piv) then
         p = i
         piv = abs(a(i,k))
      end if
   end do
   if (piv <= tiny(1.0_dp)) then
      ok = .false.
      x = 0.0_dp
      return
   end if
   if (p /= k) then
      do j = k, n
         t = a(k,j)
         a(k,j) = a(p,j)
         a(p,j) = t
      end do
      t = b(k)
      b(k) = b(p)
      b(p) = t
   end if
   do i = k + 1, n
      fac = a(i,k) / a(k,k)
      a(i,k:n) = a(i,k:n) - fac * a(k,k:n)
      b(i) = b(i) - fac * b(k)
   end do
end do
x(n) = b(n) / a(n,n)
do i = n - 1, 1, -1
   if (i < n) then
      x(i) = (b(i) - sum(a(i,i+1:n) * x(i+1:n))) / a(i,i)
   else
      x(i) = b(i) / a(i,i)
   end if
end do
end subroutine solve_linear

function lm_fit_general(y, xpred) result(fit)
! Fit linear regression with intercept and arbitrary predictors.
real(kind=dp), intent(in) :: y(:), xpred(:,:)
type(lm_fit_t) :: fit
integer :: i, j, n, p, k, dof
real(kind=dp), allocatable :: a(:,:), b(:), beta(:)
real(kind=dp) :: ybar, sse, sst
logical :: ok
if (size(y) /= size(xpred,1)) then
   error stop "error: need size(y) == size(xpred,1)"
end if
n = size(y)
p = size(xpred,2)
k = p + 1
if (n < k) error stop "error: need n >= number of parameters"
allocate(a(k,k), b(k), beta(k))
a = 0.0_dp
b = 0.0_dp
a(1,1) = n
b(1) = sum(y)
do j = 1, p
   a(1,j+1) = sum(xpred(:,j))
   a(j+1,1) = a(1,j+1)
   b(j+1) = sum(xpred(:,j) * y)
end do
do i = 1, p
   do j = i, p
      a(i+1,j+1) = sum(xpred(:,i) * xpred(:,j))
      a(j+1,i+1) = a(i+1,j+1)
   end do
end do
call solve_linear(a, b, beta, ok)
if (.not. ok) error stop "error: singular normal equations"
fit%coef = beta
fit%fitted = beta(1) + matmul(xpred, beta(2:k))
fit%resid = y - fit%fitted
sse = sum(fit%resid**2)
ybar = sum(y) / n
sst = sum((y - ybar)**2)
if (sst > 0.0_dp) then
   fit%r_squared = 1.0_dp - sse / sst
else
   fit%r_squared = 0.0_dp
end if
dof = max(1, n - k)
fit%sigma = sqrt(sse / dof)
fit%adj_r_squared = 1.0_dp - (1.0_dp - fit%r_squared) * (n - 1) / dof
end function lm_fit_general

subroutine print_lm_summary(fit)
! Print a compact summary of fitted linear model diagnostics.
type(lm_fit_t), intent(in) :: fit
write(*,'(a)') "lm summary:"
write(*,'(a,*(1x,g0))') "coef:", fit%coef
write(*,'(a,g0)') "sigma:", fit%sigma
write(*,'(a,g0)') "r.squared:", fit%r_squared
write(*,'(a,g0)') "adj.r.squared:", fit%adj_r_squared
end subroutine print_lm_summary

subroutine print_lm_coef_rstyle(fit, term_names)
! Print coefficients with R-like aligned header/value rows.
type(lm_fit_t), intent(in) :: fit
character(len=*), intent(in), optional :: term_names(:)
integer :: j, p
character(len=32) :: lbl
p = max(0, size(fit%coef) - 1)
write(*,'(a14)', advance='no') "(Intercept)"
do j = 1, p
   if (present(term_names) .and. size(term_names) >= j) then
      write(*,'(a14)', advance='no') trim(term_names(j))
   else
      write(lbl,'(a,i0)') "x", j
      write(*,'(a14)', advance='no') trim(lbl)
   end if
end do
write(*,*)
if (size(fit%coef) > 0) then
   write(*,'(*(f14.7))') fit%coef
else
   write(*,*)
end if
end subroutine print_lm_coef_rstyle

end module r_mod
