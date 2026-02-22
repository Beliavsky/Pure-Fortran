module funcs_mod
implicit none

integer, parameter :: dp = kind(1.0d0)

interface zeros
   module procedure zeros_1d, zeros_2d
end interface

interface ones
   module procedure ones_1d, ones_2d
end interface

interface eye
   module procedure eye_n, eye_mn
end interface

interface diag
   module procedure diag_from_vec, diag_from_mat
end interface

interface size_oct
   module procedure size_oct_vec_1d, size_oct_vec_2d
   module procedure size_oct_dim_1d, size_oct_dim_2d
end interface

interface rows
   module procedure rows_1d, rows_2d
end interface

interface columns
   module procedure columns_1d, columns_2d
end interface

interface length
   module procedure length_1d, length_2d
end interface

interface numel
   module procedure numel_1d, numel_2d
end interface

interface ndims
   module procedure ndims_0d, ndims_1d, ndims_2d, ndims_3d
end interface

interface reshape_oct
   module procedure reshape_oct_1d_from_1d, reshape_oct_2d_from_1d
   module procedure reshape_oct_1d_from_2d, reshape_oct_2d_from_2d
end interface

interface repmat
   module procedure repmat_1d, repmat_2d
end interface

interface cat
   module procedure cat_1d, cat_2d
end interface

interface squeeze
   module procedure squeeze_1d, squeeze_2d, squeeze_3d_to2d
end interface

interface permute
   module procedure permute_2d, permute_3d
end interface

interface sort
   module procedure sort_1d, sort_2d
end interface

interface sortrows
   module procedure sortrows_2d
end interface

! reductions + simple stats
interface sum_oct
   module procedure sum_oct_1d, sum_oct_2d, sum_oct_2d_dim
end interface

interface prod_oct
   module procedure prod_oct_1d, prod_oct_2d, prod_oct_2d_dim
end interface

interface min_oct
   module procedure min_oct_1d, min_oct_2d, min_oct_2d_dim
end interface

interface max_oct
   module procedure max_oct_1d, max_oct_2d, max_oct_2d_dim
end interface

interface mean
   module procedure mean_1d, mean_2d, mean_2d_dim
end interface

interface median
   module procedure median_1d, median_2d, median_2d_dim
end interface

interface var
   module procedure var_1d, var_2d, var_2d_dim
end interface

interface std
   module procedure std_1d, std_2d, std_2d_dim
end interface

interface cumsum
   module procedure cumsum_1d, cumsum_2d, cumsum_2d_dim
end interface

interface cumprod
   module procedure cumprod_1d, cumprod_2d, cumprod_2d_dim
end interface

interface diff
   module procedure diff_1d, diff_2d, diff_2d_dim
end interface

contains

function linspace(a, b, n) result(x)
   !! n points from a to b (inclusive)
   real(kind=dp), intent(in) :: a, b
   integer, intent(in) :: n
   real(kind=dp), allocatable :: x(:)
   real(kind=dp) :: step
   integer :: i

   if (n < 0) error stop "linspace: n must be >= 0"
   allocate(x(n))

   if (n == 0) return
   if (n == 1) then
      x(1) = a
      return
   end if

   step = (b - a) / real(n - 1, kind=dp)
   do i = 1, n
      x(i) = a + real(i - 1, kind=dp) * step
   end do
end function linspace

function logspace(a, b, n) result(x)
   !! n points from 10^a to 10^b
   real(kind=dp), intent(in) :: a, b
   integer, intent(in) :: n
   real(kind=dp), allocatable :: x(:)
   real(kind=dp), allocatable :: t(:)
   integer :: i

   t = linspace(a, b, n)
   allocate(x(size(t)))

   do i = 1, size(t)
      x(i) = 10.0_dp ** t(i)
   end do
end function logspace

function zeros_1d(n) result(x)
   !! length-n vector of zeros
   integer, intent(in) :: n
   real(kind=dp), allocatable :: x(:)

   if (n < 0) error stop "zeros: n must be >= 0"
   allocate(x(n))
   x = 0.0_dp
end function zeros_1d

function zeros_2d(m, n) result(x)
   !! m-by-n matrix of zeros
   integer, intent(in) :: m, n
   real(kind=dp), allocatable :: x(:,:)

   if (m < 0 .or. n < 0) error stop "zeros: dims must be >= 0"
   allocate(x(m, n))
   x = 0.0_dp
end function zeros_2d

function ones_1d(n) result(x)
   !! length-n vector of ones
   integer, intent(in) :: n
   real(kind=dp), allocatable :: x(:)

   if (n < 0) error stop "ones: n must be >= 0"
   allocate(x(n))
   x = 1.0_dp
end function ones_1d

function ones_2d(m, n) result(x)
   !! m-by-n matrix of ones
   integer, intent(in) :: m, n
   real(kind=dp), allocatable :: x(:,:)

   if (m < 0 .or. n < 0) error stop "ones: dims must be >= 0"
   allocate(x(m, n))
   x = 1.0_dp
end function ones_2d

function eye_n(n) result(a)
   !! n-by-n identity matrix
   integer, intent(in) :: n
   real(kind=dp), allocatable :: a(:,:)
   integer :: i

   if (n < 0) error stop "eye: n must be >= 0"
   allocate(a(n, n))
   a = 0.0_dp
   do i = 1, n
      a(i, i) = 1.0_dp
   end do
end function eye_n

function eye_mn(m, n) result(a)
   !! m-by-n identity-like matrix
   integer, intent(in) :: m, n
   real(kind=dp), allocatable :: a(:,:)
   integer :: i, k

   if (m < 0 .or. n < 0) error stop "eye: dims must be >= 0"
   allocate(a(m, n))
   a = 0.0_dp
   k = min(m, n)
   do i = 1, k
      a(i, i) = 1.0_dp
   end do
end function eye_mn

function diag_from_vec(v) result(a)
   !! square matrix with v on the diagonal
   real(kind=dp), intent(in) :: v(:)
   real(kind=dp), allocatable :: a(:,:)
   integer :: i, n

   n = size(v)
   allocate(a(n, n))
   a = 0.0_dp
   do i = 1, n
      a(i, i) = v(i)
   end do
end function diag_from_vec

function diag_from_mat(a) result(v)
   !! vector equal to the diagonal of a
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: v(:)
   integer :: i, k

   k = min(size(a, 1), size(a, 2))
   allocate(v(k))
   do i = 1, k
      v(i) = a(i, i)
   end do
end function diag_from_mat

subroutine meshgrid(x, y, xx, yy)
   !! matlab/octave-style meshgrid
   real(kind=dp), intent(in) :: x(:), y(:)
   real(kind=dp), allocatable, intent(out) :: xx(:,:), yy(:,:)
   integer :: i, j, nx, ny

   nx = size(x)
   ny = size(y)

   allocate(xx(ny, nx))
   allocate(yy(ny, nx))

   do j = 1, nx
      do i = 1, ny
         xx(i, j) = x(j)
         yy(i, j) = y(i)
      end do
   end do
end subroutine meshgrid

subroutine print_vec(v)
real(kind=dp), intent(in) :: v(:)
integer :: i
if (size(v) == 0) then
   write(*,'(a)') '(empty)'
   return
end if
write(*,'(*(f10.4,1x))') v
end subroutine print_vec

subroutine print_mat(m)
real(kind=dp), intent(in) :: m(:,:)
integer :: i
if (size(m,1) == 0 .or. size(m,2) == 0) then
   write(*,'(a)') '(empty)'
   return
end if
do i = 1, size(m,1)
   write(*,'(*(f10.4,1x))') m(i,:)
end do
end subroutine print_mat

function size_oct_vec_1d(a) result(s)
   !! octave-style size(a) -> [n]
   real(kind=dp), intent(in) :: a(:)
   integer, allocatable :: s(:)
   allocate(s(1))
   s(1) = size(a)
end function size_oct_vec_1d

function size_oct_vec_2d(a) result(s)
   !! octave-style size(a) -> [m, n]
   real(kind=dp), intent(in) :: a(:,:)
   integer, allocatable :: s(:)
   allocate(s(2))
   s(1) = size(a, 1)
   s(2) = size(a, 2)
end function size_oct_vec_2d

integer function size_oct_dim_1d(a, dim) result(n)
   !! octave-style size(a, dim)
   real(kind=dp), intent(in) :: a(:)
   integer, intent(in) :: dim
   if (dim == 1) then
      n = size(a)
   else
      n = 1
   end if
end function size_oct_dim_1d

integer function size_oct_dim_2d(a, dim) result(n)
   !! octave-style size(a, dim)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: dim
   if (dim == 1) then
      n = size(a, 1)
   else if (dim == 2) then
      n = size(a, 2)
   else
      n = 1
   end if
end function size_oct_dim_2d

integer function rows_1d(a) result(r)
   !! for 1d, treat as n-by-1
   real(kind=dp), intent(in) :: a(:)
   r = size(a)
end function rows_1d

integer function rows_2d(a) result(r)
   real(kind=dp), intent(in) :: a(:,:)
   r = size(a, 1)
end function rows_2d

integer function columns_1d(a) result(c)
   !! for 1d, treat as n-by-1
   real(kind=dp), intent(in) :: a(:)
   c = 1
end function columns_1d

integer function columns_2d(a) result(c)
   real(kind=dp), intent(in) :: a(:,:)
   c = size(a, 2)
end function columns_2d

integer function length_1d(a) result(n)
   !! octave length(a): max(size(a))
   real(kind=dp), intent(in) :: a(:)
   n = size(a)
end function length_1d

integer function length_2d(a) result(n)
   !! octave length(a): max(size(a,1), size(a,2))
   real(kind=dp), intent(in) :: a(:,:)
   n = max(size(a, 1), size(a, 2))
end function length_2d

integer function numel_1d(a) result(n)
   real(kind=dp), intent(in) :: a(:)
   n = size(a)
end function numel_1d

integer function numel_2d(a) result(n)
   real(kind=dp), intent(in) :: a(:,:)
   n = size(a)
end function numel_2d

integer function ndims_0d(a) result(n)
   real(kind=dp), intent(in) :: a
   n = 0
end function ndims_0d

integer function ndims_1d(a) result(n)
   real(kind=dp), intent(in) :: a(:)
   n = 1
end function ndims_1d

integer function ndims_2d(a) result(n)
   real(kind=dp), intent(in) :: a(:,:)
   n = 2
end function ndims_2d

integer function ndims_3d(a) result(n)
   real(kind=dp), intent(in) :: a(:,:,:)
   n = 3
end function ndims_3d

function reshape_oct_1d_from_1d(x, n) result(y)
   !! reshape_oct(x, n) -> vector of length n
   real(kind=dp), intent(in) :: x(:)
   integer, intent(in) :: n
   real(kind=dp), allocatable :: y(:)
   if (n < 0) error stop "reshape_oct: n must be >= 0"
   if (size(x) /= n) error stop "reshape_oct: numel mismatch"
   allocate(y(n))
   y = x
end function reshape_oct_1d_from_1d

function reshape_oct_2d_from_1d(x, m, n) result(y)
   !! reshape_oct(x, m, n) -> m-by-n
   real(kind=dp), intent(in) :: x(:)
   integer, intent(in) :: m, n
   real(kind=dp), allocatable :: y(:,:)
   if (m < 0 .or. n < 0) error stop "reshape_oct: dims must be >= 0"
   if (size(x) /= m*n) error stop "reshape_oct: numel mismatch"
   allocate(y(m, n))
   y = reshape(x, [m, n])
end function reshape_oct_2d_from_1d

function reshape_oct_1d_from_2d(x, n) result(y)
   !! reshape_oct(x, n) from matrix -> vector
   real(kind=dp), intent(in) :: x(:,:)
   integer, intent(in) :: n
   real(kind=dp), allocatable :: y(:)
   if (n < 0) error stop "reshape_oct: n must be >= 0"
   if (size(x) /= n) error stop "reshape_oct: numel mismatch"
   allocate(y(n))
   y = reshape(x, [n])
end function reshape_oct_1d_from_2d

function reshape_oct_2d_from_2d(x, m, n) result(y)
   !! reshape_oct(x, m, n) from matrix -> matrix
   real(kind=dp), intent(in) :: x(:,:)
   integer, intent(in) :: m, n
   real(kind=dp), allocatable :: y(:,:)
   if (m < 0 .or. n < 0) error stop "reshape_oct: dims must be >= 0"
   if (size(x) /= m*n) error stop "reshape_oct: numel mismatch"
   allocate(y(m, n))
   y = reshape(x, [m, n])
end function reshape_oct_2d_from_2d

function repmat_1d(a, m, n) result(b)
   !! repmat(vector, m, n) treating vector as (len x 1)
   real(kind=dp), intent(in) :: a(:)
   integer, intent(in) :: m, n
   real(kind=dp), allocatable :: b(:,:)
   integer :: r, i, j
   if (m < 0 .or. n < 0) error stop "repmat: m,n must be >= 0"
   r = size(a)
   allocate(b(m*r, n))
   do j = 1, n
      do i = 1, m
         b((i-1)*r+1:i*r, j) = a
      end do
   end do
end function repmat_1d

function repmat_2d(a, m, n) result(b)
   !! repmat(matrix, m, n)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: m, n
   real(kind=dp), allocatable :: b(:,:)
   integer :: r, c, i, j
   if (m < 0 .or. n < 0) error stop "repmat: m,n must be >= 0"
   r = size(a, 1)
   c = size(a, 2)
   allocate(b(m*r, n*c))
   do j = 1, n
      do i = 1, m
         b((i-1)*r+1:i*r, (j-1)*c+1:j*c) = a
      end do
   end do
end function repmat_2d

function cat_1d(dim, a, b) result(c)
   !! cat(1, a, b) for vectors
   integer, intent(in) :: dim
   real(kind=dp), intent(in) :: a(:), b(:)
   real(kind=dp), allocatable :: c(:)
   integer :: n0, n1
   if (dim /= 1) error stop "cat: for 1d, dim must be 1"
   n0 = size(a)
   n1 = size(b)
   allocate(c(n0+n1))
   if (n0 > 0) c(1:n0) = a
   if (n1 > 0) c(n0+1:n0+n1) = b
end function cat_1d

function cat_2d(dim, a, b) result(c)
   !! cat(1, a, b) stacks rows; cat(2, a, b) stacks columns
   integer, intent(in) :: dim
   real(kind=dp), intent(in) :: a(:,:), b(:,:)
   real(kind=dp), allocatable :: c(:,:)
   integer :: r0, c0, r1, c1
   r0 = size(a, 1); c0 = size(a, 2)
   r1 = size(b, 1); c1 = size(b, 2)

   if (dim == 1) then
      if (c0 /= c1) error stop "cat: dim=1 requires same number of columns"
      allocate(c(r0+r1, c0))
      if (r0 > 0) c(1:r0, :) = a
      if (r1 > 0) c(r0+1:r0+r1, :) = b
   else if (dim == 2) then
      if (r0 /= r1) error stop "cat: dim=2 requires same number of rows"
      allocate(c(r0, c0+c1))
      if (c0 > 0) c(:, 1:c0) = a
      if (c1 > 0) c(:, c0+1:c0+c1) = b
   else
      error stop "cat: dim must be 1 or 2"
   end if
end function cat_2d

function squeeze_1d(a) result(b)
   real(kind=dp), intent(in) :: a(:)
   real(kind=dp), allocatable :: b(:)
   allocate(b(size(a)))
   b = a
end function squeeze_1d

function squeeze_2d(a) result(b)
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: b(:,:)
   allocate(b(size(a,1), size(a,2)))
   b = a
end function squeeze_2d

function squeeze_3d_to2d(a) result(b)
   !! squeeze for 3d only when one dimension is 1 -> returns 2d
   real(kind=dp), intent(in) :: a(:,:,:)
   real(kind=dp), allocatable :: b(:,:)
   integer :: n1, n2, n3, i, j
   n1 = size(a, 1); n2 = size(a, 2); n3 = size(a, 3)

   if (n1 == 1 .and. n2 >= 0 .and. n3 >= 0) then
      allocate(b(n2, n3))
      do j = 1, n3
         do i = 1, n2
            b(i, j) = a(1, i, j)
         end do
      end do
   else if (n2 == 1 .and. n1 >= 0 .and. n3 >= 0) then
      allocate(b(n1, n3))
      do j = 1, n3
         do i = 1, n1
            b(i, j) = a(i, 1, j)
         end do
      end do
   else if (n3 == 1 .and. n1 >= 0 .and. n2 >= 0) then
      allocate(b(n1, n2))
      b = a(:, :, 1)
   else
      error stop "squeeze: 3d input must have a singleton dimension"
   end if
end function squeeze_3d_to2d

function permute_2d(a, order) result(b)
   !! permute for 2d: order must be [1,2] or [2,1]
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: order(:)
   real(kind=dp), allocatable :: b(:,:)

   if (size(order) /= 2) error stop "permute: order must have length 2"
   if (order(1) == 1 .and. order(2) == 2) then
      allocate(b(size(a,1), size(a,2)))
      b = a
   else if (order(1) == 2 .and. order(2) == 1) then
      allocate(b(size(a,2), size(a,1)))
      b = transpose(a)
   else
      error stop "permute: invalid order for 2d"
   end if
end function permute_2d

function permute_3d(a, order) result(b)
   !! permute for 3d
   real(kind=dp), intent(in) :: a(:,:,:)
   integer, intent(in) :: order(:)
   real(kind=dp), allocatable :: b(:,:,:)
   integer :: s(3), t(3), i1, i2, i3, idx(3)

   if (size(order) /= 3) error stop "permute: order must have length 3"
   s = [size(a,1), size(a,2), size(a,3)]
   if (any(order < 1) .or. any(order > 3)) error stop "permute: order entries must be 1..3"
   if (order(1) == order(2) .or. order(1) == order(3) .or. order(2) == order(3)) &
      error stop "permute: order must be a permutation"

   t = [s(order(1)), s(order(2)), s(order(3))]
   allocate(b(t(1), t(2), t(3)))

   do i3 = 1, t(3)
      do i2 = 1, t(2)
         do i1 = 1, t(1)
            idx(order(1)) = i1
            idx(order(2)) = i2
            idx(order(3)) = i3
            b(i1, i2, i3) = a(idx(1), idx(2), idx(3))
         end do
      end do
   end do
end function permute_3d

function sort_1d(x, descending) result(y)
   !! sort(vector) ascending by default; set descending=.true. for descending
   real(kind=dp), intent(in) :: x(:)
   logical, intent(in), optional :: descending
   real(kind=dp), allocatable :: y(:)
   logical :: asc

   asc = .true.
   if (present(descending)) asc = .not. descending

   allocate(y(size(x)))
   y = x
   if (size(y) >= 2) call quicksort_real(y, 1, size(y), asc)
end function sort_1d

function sort_2d(a, descending) result(b)
   !! sort(matrix) sorts each column
   real(kind=dp), intent(in) :: a(:,:)
   logical, intent(in), optional :: descending
   real(kind=dp), allocatable :: b(:,:)
   logical :: asc
   integer :: j

   asc = .true.
   if (present(descending)) asc = .not. descending

   allocate(b(size(a,1), size(a,2)))
   b = a
   do j = 1, size(b,2)
      if (size(b,1) >= 2) call quicksort_real(b(:,j), 1, size(b,1), asc)
   end do
end function sort_2d

function sortrows_2d(a, col, descending) result(b)
   !! sortrows(a[, col]) sorts rows by column col (default 1)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in), optional :: col
   logical, intent(in), optional :: descending
   real(kind=dp), allocatable :: b(:,:)
   integer, allocatable :: idx(:)
   integer :: n, j, c
   logical :: asc

   asc = .true.
   if (present(descending)) asc = .not. descending

   c = 1
   if (present(col)) c = col
   if (c < 1 .or. c > size(a,2)) error stop "sortrows: invalid column"

   n = size(a,1)
   allocate(idx(n))
   do j = 1, n
      idx(j) = j
   end do

   if (n >= 2) call quicksort_idx(idx, a, c, 1, n, asc)

   allocate(b(size(a,1), size(a,2)))
   b = a(idx, :)
end function sortrows_2d

recursive subroutine quicksort_real(v, left, right, asc)
   real(kind=dp), intent(inout) :: v(:)
   integer, intent(in) :: left, right
   logical, intent(in) :: asc
   integer :: i, j, mid
   real(kind=dp) :: pivot, tmp

   i = left
   j = right
   mid = (left + right) / 2
   pivot = v(mid)

   do
      if (asc) then
         do while (v(i) < pivot); i = i + 1; end do
         do while (v(j) > pivot); j = j - 1; end do
      else
         do while (v(i) > pivot); i = i + 1; end do
         do while (v(j) < pivot); j = j - 1; end do
      end if

      if (i <= j) then
         tmp = v(i); v(i) = v(j); v(j) = tmp
         i = i + 1
         j = j - 1
      end if
      if (i > j) exit
   end do

   if (left < j) call quicksort_real(v, left, j, asc)
   if (i < right) call quicksort_real(v, i, right, asc)
end subroutine quicksort_real

recursive subroutine quicksort_idx(idx, a, col, left, right, asc)
   integer, intent(inout) :: idx(:)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: col, left, right
   logical, intent(in) :: asc
   integer :: i, j, mid, tmpi
   real(kind=dp) :: pivot

   i = left
   j = right
   mid = (left + right) / 2
   pivot = a(idx(mid), col)

   do
      if (asc) then
         do while (a(idx(i), col) < pivot); i = i + 1; end do
         do while (a(idx(j), col) > pivot); j = j - 1; end do
      else
         do while (a(idx(i), col) > pivot); i = i + 1; end do
         do while (a(idx(j), col) < pivot); j = j - 1; end do
      end if

      if (i <= j) then
         tmpi = idx(i); idx(i) = idx(j); idx(j) = tmpi
         i = i + 1
         j = j - 1
      end if
      if (i > j) exit
   end do

   if (left < j) call quicksort_idx(idx, a, col, left, j, asc)
   if (i < right) call quicksort_idx(idx, a, col, i, right, asc)
end subroutine quicksort_idx

! ----------------------------
! reductions + simple stats
! ----------------------------

real(kind=dp) function sum_oct_1d(x) result(s)
   real(kind=dp), intent(in) :: x(:)
   integer :: i
   s = 0.0_dp
   do i = 1, size(x)
      s = s + x(i)
   end do
end function sum_oct_1d

function sum_oct_2d(a) result(s)
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: s(:)
   s = sum_oct_2d_dim(a, 1)
end function sum_oct_2d

function sum_oct_2d_dim(a, dim) result(s)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: dim
   real(kind=dp), allocatable :: s(:)
   integer :: i, j

   if (dim == 1) then
      allocate(s(size(a,2)))
      s = 0.0_dp
      do j = 1, size(a,2)
         do i = 1, size(a,1)
            s(j) = s(j) + a(i,j)
         end do
      end do
   else if (dim == 2) then
      allocate(s(size(a,1)))
      s = 0.0_dp
      do i = 1, size(a,1)
         do j = 1, size(a,2)
            s(i) = s(i) + a(i,j)
         end do
      end do
   else
      error stop "sum_oct: dim must be 1 or 2"
   end if
end function sum_oct_2d_dim

real(kind=dp) function prod_oct_1d(x) result(p)
   real(kind=dp), intent(in) :: x(:)
   integer :: i
   p = 1.0_dp
   do i = 1, size(x)
      p = p * x(i)
   end do
end function prod_oct_1d

function prod_oct_2d(a) result(p)
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: p(:)
   p = prod_oct_2d_dim(a, 1)
end function prod_oct_2d

function prod_oct_2d_dim(a, dim) result(p)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: dim
   real(kind=dp), allocatable :: p(:)
   integer :: i, j

   if (dim == 1) then
      allocate(p(size(a,2)))
      p = 1.0_dp
      do j = 1, size(a,2)
         do i = 1, size(a,1)
            p(j) = p(j) * a(i,j)
         end do
      end do
   else if (dim == 2) then
      allocate(p(size(a,1)))
      p = 1.0_dp
      do i = 1, size(a,1)
         do j = 1, size(a,2)
            p(i) = p(i) * a(i,j)
         end do
      end do
   else
      error stop "prod_oct: dim must be 1 or 2"
   end if
end function prod_oct_2d_dim

real(kind=dp) function min_oct_1d(x) result(mn)
   real(kind=dp), intent(in) :: x(:)
   integer :: i
   if (size(x) == 0) error stop "min_oct: empty input"
   mn = x(1)
   do i = 2, size(x)
      if (x(i) < mn) mn = x(i)
   end do
end function min_oct_1d

function min_oct_2d(a) result(mn)
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: mn(:)
   mn = min_oct_2d_dim(a, 1)
end function min_oct_2d

function min_oct_2d_dim(a, dim) result(mn)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: dim
   real(kind=dp), allocatable :: mn(:)
   integer :: i, j

   if (dim == 1) then
      if (size(a,1) == 0) error stop "min_oct: empty along dim"
      allocate(mn(size(a,2)))
      do j = 1, size(a,2)
         mn(j) = a(1,j)
         do i = 2, size(a,1)
            if (a(i,j) < mn(j)) mn(j) = a(i,j)
         end do
      end do
   else if (dim == 2) then
      if (size(a,2) == 0) error stop "min_oct: empty along dim"
      allocate(mn(size(a,1)))
      do i = 1, size(a,1)
         mn(i) = a(i,1)
         do j = 2, size(a,2)
            if (a(i,j) < mn(i)) mn(i) = a(i,j)
         end do
      end do
   else
      error stop "min_oct: dim must be 1 or 2"
   end if
end function min_oct_2d_dim

real(kind=dp) function max_oct_1d(x) result(mx)
   real(kind=dp), intent(in) :: x(:)
   integer :: i
   if (size(x) == 0) error stop "max_oct: empty input"
   mx = x(1)
   do i = 2, size(x)
      if (x(i) > mx) mx = x(i)
   end do
end function max_oct_1d

function max_oct_2d(a) result(mx)
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: mx(:)
   mx = max_oct_2d_dim(a, 1)
end function max_oct_2d

function max_oct_2d_dim(a, dim) result(mx)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: dim
   real(kind=dp), allocatable :: mx(:)
   integer :: i, j

   if (dim == 1) then
      if (size(a,1) == 0) error stop "max_oct: empty along dim"
      allocate(mx(size(a,2)))
      do j = 1, size(a,2)
         mx(j) = a(1,j)
         do i = 2, size(a,1)
            if (a(i,j) > mx(j)) mx(j) = a(i,j)
         end do
      end do
   else if (dim == 2) then
      if (size(a,2) == 0) error stop "max_oct: empty along dim"
      allocate(mx(size(a,1)))
      do i = 1, size(a,1)
         mx(i) = a(i,1)
         do j = 2, size(a,2)
            if (a(i,j) > mx(i)) mx(i) = a(i,j)
         end do
      end do
   else
      error stop "max_oct: dim must be 1 or 2"
   end if
end function max_oct_2d_dim

real(kind=dp) function mean_1d(x) result(m)
   real(kind=dp), intent(in) :: x(:)
   if (size(x) == 0) error stop "mean: empty input"
   m = sum_oct_1d(x) / real(size(x), kind=dp)
end function mean_1d

function mean_2d(a) result(m)
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: m(:)
   m = mean_2d_dim(a, 1)
end function mean_2d

function mean_2d_dim(a, dim) result(m)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: dim
   real(kind=dp), allocatable :: m(:)
   integer :: n

   if (dim == 1) then
      n = size(a,1)
      if (n == 0) error stop "mean: empty along dim"
      m = sum_oct_2d_dim(a, 1) / real(n, kind=dp)
   else if (dim == 2) then
      n = size(a,2)
      if (n == 0) error stop "mean: empty along dim"
      m = sum_oct_2d_dim(a, 2) / real(n, kind=dp)
   else
      error stop "mean: dim must be 1 or 2"
   end if
end function mean_2d_dim

real(kind=dp) function median_1d(x) result(med)
   real(kind=dp), intent(in) :: x(:)
   real(kind=dp), allocatable :: y(:)
   integer :: n, mid

   n = size(x)
   if (n == 0) error stop "median: empty input"
   y = sort_1d(x)
   if (mod(n,2) == 1) then
      mid = (n+1)/2
      med = y(mid)
   else
      mid = n/2
      med = 0.5_dp*(y(mid) + y(mid+1))
   end if
end function median_1d

function median_2d(a) result(med)
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: med(:)
   med = median_2d_dim(a, 1)
end function median_2d

function median_2d_dim(a, dim) result(med)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: dim
   real(kind=dp), allocatable :: med(:)
   real(kind=dp), allocatable :: tmp(:)
   integer :: i, j

   if (dim == 1) then
      if (size(a,1) == 0) error stop "median: empty along dim"
      allocate(med(size(a,2)))
      allocate(tmp(size(a,1)))
      do j = 1, size(a,2)
         tmp = a(:,j)
         med(j) = median_1d(tmp)
      end do
   else if (dim == 2) then
      if (size(a,2) == 0) error stop "median: empty along dim"
      allocate(med(size(a,1)))
      allocate(tmp(size(a,2)))
      do i = 1, size(a,1)
         tmp = a(i,:)
         med(i) = median_1d(tmp)
      end do
   else
      error stop "median: dim must be 1 or 2"
   end if
end function median_2d_dim

real(kind=dp) function var_1d(x) result(v)
   !! sample variance (n-1); for n=1 returns 0
   real(kind=dp), intent(in) :: x(:)
   real(kind=dp) :: mu
   integer :: i, n
   n = size(x)
   if (n == 0) error stop "var: empty input"
   if (n == 1) then
      v = 0.0_dp
      return
   end if
   mu = mean_1d(x)
   v = 0.0_dp
   do i = 1, n
      v = v + (x(i) - mu)**2
   end do
   v = v / real(n-1, kind=dp)
end function var_1d

function var_2d(a) result(v)
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: v(:)
   v = var_2d_dim(a, 1)
end function var_2d

function var_2d_dim(a, dim) result(v)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: dim
   real(kind=dp), allocatable :: v(:)
   real(kind=dp), allocatable :: tmp(:)
   integer :: i, j

   if (dim == 1) then
      if (size(a,1) == 0) error stop "var: empty along dim"
      allocate(v(size(a,2)))
      allocate(tmp(size(a,1)))
      do j = 1, size(a,2)
         tmp = a(:,j)
         v(j) = var_1d(tmp)
      end do
   else if (dim == 2) then
      if (size(a,2) == 0) error stop "var: empty along dim"
      allocate(v(size(a,1)))
      allocate(tmp(size(a,2)))
      do i = 1, size(a,1)
         tmp = a(i,:)
         v(i) = var_1d(tmp)
      end do
   else
      error stop "var: dim must be 1 or 2"
   end if
end function var_2d_dim

real(kind=dp) function std_1d(x) result(s)
   real(kind=dp), intent(in) :: x(:)
   s = sqrt(var_1d(x))
end function std_1d

function std_2d(a) result(s)
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: s(:)
   s = std_2d_dim(a, 1)
end function std_2d

function std_2d_dim(a, dim) result(s)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: dim
   real(kind=dp), allocatable :: s(:)
   s = sqrt(var_2d_dim(a, dim))
end function std_2d_dim

function cumsum_1d(x) result(y)
   real(kind=dp), intent(in) :: x(:)
   real(kind=dp), allocatable :: y(:)
   integer :: i
   allocate(y(size(x)))
   if (size(x) == 0) return
   y(1) = x(1)
   do i = 2, size(x)
      y(i) = y(i-1) + x(i)
   end do
end function cumsum_1d

function cumsum_2d(a) result(y)
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: y(:,:)
   y = cumsum_2d_dim(a, 1)
end function cumsum_2d

function cumsum_2d_dim(a, dim) result(y)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: dim
   real(kind=dp), allocatable :: y(:,:)
   integer :: i, j

   allocate(y(size(a,1), size(a,2)))
   y = 0.0_dp

   if (dim == 1) then
      if (size(a,1) == 0) return
      y(1,:) = a(1,:)
      do i = 2, size(a,1)
         do j = 1, size(a,2)
            y(i,j) = y(i-1,j) + a(i,j)
         end do
      end do
   else if (dim == 2) then
      if (size(a,2) == 0) return
      y(:,1) = a(:,1)
      do j = 2, size(a,2)
         do i = 1, size(a,1)
            y(i,j) = y(i,j-1) + a(i,j)
         end do
      end do
   else
      error stop "cumsum: dim must be 1 or 2"
   end if
end function cumsum_2d_dim

function cumprod_1d(x) result(y)
   real(kind=dp), intent(in) :: x(:)
   real(kind=dp), allocatable :: y(:)
   integer :: i
   allocate(y(size(x)))
   if (size(x) == 0) return
   y(1) = x(1)
   do i = 2, size(x)
      y(i) = y(i-1) * x(i)
   end do
end function cumprod_1d

function cumprod_2d(a) result(y)
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: y(:,:)
   y = cumprod_2d_dim(a, 1)
end function cumprod_2d

function cumprod_2d_dim(a, dim) result(y)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: dim
   real(kind=dp), allocatable :: y(:,:)
   integer :: i, j

   allocate(y(size(a,1), size(a,2)))
   y = 0.0_dp

   if (dim == 1) then
      if (size(a,1) == 0) return
      y(1,:) = a(1,:)
      do i = 2, size(a,1)
         do j = 1, size(a,2)
            y(i,j) = y(i-1,j) * a(i,j)
         end do
      end do
   else if (dim == 2) then
      if (size(a,2) == 0) return
      y(:,1) = a(:,1)
      do j = 2, size(a,2)
         do i = 1, size(a,1)
            y(i,j) = y(i,j-1) * a(i,j)
         end do
      end do
   else
      error stop "cumprod: dim must be 1 or 2"
   end if
end function cumprod_2d_dim

function diff_1d(x) result(y)
   real(kind=dp), intent(in) :: x(:)
   real(kind=dp), allocatable :: y(:)
   integer :: i, n
   n = size(x)
   if (n <= 1) then
      allocate(y(0))
      return
   end if
   allocate(y(n-1))
   do i = 1, n-1
      y(i) = x(i+1) - x(i)
   end do
end function diff_1d

function diff_2d(a) result(y)
   real(kind=dp), intent(in) :: a(:,:)
   real(kind=dp), allocatable :: y(:,:)
   y = diff_2d_dim(a, 1)
end function diff_2d

function diff_2d_dim(a, dim) result(y)
   real(kind=dp), intent(in) :: a(:,:)
   integer, intent(in) :: dim
   real(kind=dp), allocatable :: y(:,:)
   integer :: i, j, m, n

   m = size(a,1)
   n = size(a,2)

   if (dim == 1) then
      if (m <= 1) then
         allocate(y(0, n))
         return
      end if
      allocate(y(m-1, n))
      do j = 1, n
         do i = 1, m-1
            y(i,j) = a(i+1,j) - a(i,j)
         end do
      end do
   else if (dim == 2) then
      if (n <= 1) then
         allocate(y(m, 0))
         return
      end if
      allocate(y(m, n-1))
      do i = 1, m
         do j = 1, n-1
            y(i,j) = a(i,j+1) - a(i,j)
         end do
      end do
   else
      error stop "diff: dim must be 1 or 2"
   end if
end function diff_2d_dim

end module funcs_mod
