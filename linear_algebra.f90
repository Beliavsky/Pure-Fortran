module linear_algebra_mod
implicit none

integer, parameter :: dp = kind(1.0d0)

! ----------------------------
! public generics (FUNCTIONS)
! ----------------------------
interface inv
   module procedure inv_mat
end interface

interface pinv
   module procedure pinv_mat
end interface

interface det
   module procedure det_mat
end interface

interface rank
   module procedure rank_mat
end interface

interface trace
   module procedure trace_mat
end interface

interface norm
   module procedure norm_mat
end interface

! eig(A) returns eigenvalues only (complex)
interface eig
   module procedure eig_vals
end interface

! svd(A) returns singular values only
interface svd
   module procedure svd_vals
end interface

! qr(A) returns R only
interface qr
   module procedure qr_r_only
end interface

! lu(A) returns packed LU only
interface lu
   module procedure lu_packed
end interface

! chol(A) returns upper-triangular R only
interface chol
   module procedure chol_upper
end interface

! ----------------------------
! additional subroutine APIs
! ----------------------------
interface eig_full
   module procedure eig_vals_vecs
end interface

interface svd_full
   module procedure svd_full_sub
end interface

interface qr_full
   module procedure qr_full_sub
end interface

interface lu_full
   module procedure lu_full_sub
end interface

interface chol_sub
   module procedure chol_upper_sub
end interface

! ----------------------------
! LAPACK interfaces
! ----------------------------
interface
   subroutine dgetrf(m, n, a, lda, ipiv, info)
      import :: dp
      integer, intent(in) :: m, n, lda
      real(kind=dp), intent(inout) :: a(lda, *)
      integer, intent(out) :: ipiv(*)
      integer, intent(out) :: info
   end subroutine dgetrf

   subroutine dgetri(n, a, lda, ipiv, work, lwork, info)
      import :: dp
      integer, intent(in) :: n, lda, lwork
      real(kind=dp), intent(inout) :: a(lda, *)
      integer, intent(in) :: ipiv(*)
      real(kind=dp), intent(inout) :: work(*)
      integer, intent(out) :: info
   end subroutine dgetri

   subroutine dgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)
      import :: dp
      character(len=1), intent(in) :: jobu, jobvt
      integer, intent(in) :: m, n, lda, ldu, ldvt, lwork
      real(kind=dp), intent(inout) :: a(lda, *)
      real(kind=dp), intent(out) :: s(*)
      real(kind=dp), intent(out) :: u(ldu, *)
      real(kind=dp), intent(out) :: vt(ldvt, *)
      real(kind=dp), intent(inout) :: work(*)
      integer, intent(out) :: info
   end subroutine dgesvd

   subroutine dgeev(jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr, ldvr, work, lwork, info)
      import :: dp
      character(len=1), intent(in) :: jobvl, jobvr
      integer, intent(in) :: n, lda, ldvl, ldvr, lwork
      real(kind=dp), intent(inout) :: a(lda, *)
      real(kind=dp), intent(out) :: wr(*), wi(*)
      real(kind=dp), intent(out) :: vl(ldvl, *), vr(ldvr, *)
      real(kind=dp), intent(inout) :: work(*)
      integer, intent(out) :: info
   end subroutine dgeev

   subroutine dgeqrf(m, n, a, lda, tau, work, lwork, info)
      import :: dp
      integer, intent(in) :: m, n, lda, lwork
      real(kind=dp), intent(inout) :: a(lda, *)
      real(kind=dp), intent(out) :: tau(*)
      real(kind=dp), intent(inout) :: work(*)
      integer, intent(out) :: info
   end subroutine dgeqrf

   subroutine dorgqr(m, n, k, a, lda, tau, work, lwork, info)
      import :: dp
      integer, intent(in) :: m, n, k, lda, lwork
      real(kind=dp), intent(inout) :: a(lda, *)
      real(kind=dp), intent(in) :: tau(*)
      real(kind=dp), intent(inout) :: work(*)
      integer, intent(out) :: info
   end subroutine dorgqr

   subroutine dpotrf(uplo, n, a, lda, info)
      import :: dp
      character(len=1), intent(in) :: uplo
      integer, intent(in) :: n, lda
      real(kind=dp), intent(inout) :: a(lda, *)
      integer, intent(out) :: info
   end subroutine dpotrf
end interface

contains

subroutine assert_square(a, name)
real(kind=dp), intent(in) :: a(:,:)
character(len=*), intent(in) :: name
if (size(a,1) /= size(a,2)) error stop name//": matrix must be square"
end subroutine assert_square

function tolower_str(s) result(t)
character(len=*), intent(in) :: s
character(len=len(s)) :: t
integer :: i, c
do i = 1, len(s)
   c = iachar(s(i:i))
   if (c >= iachar("A") .and. c <= iachar("Z")) then
      t(i:i) = achar(c + 32)
   else
      t(i:i) = s(i:i)
   end if
end do
end function tolower_str

! ----------------------------
! inv / pinv
! ----------------------------
function inv_mat(a) result(ainv)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable :: ainv(:,:)
real(kind=dp), allocatable :: w(:)
real(kind=dp) :: wq(1)
integer, allocatable :: ipiv(:)
integer :: n, info, lwork

call assert_square(a, "inv")
n = size(a,1)
allocate(ainv(n,n))
ainv = a

allocate(ipiv(n))
call dgetrf(n, n, ainv, n, ipiv, info)
if (info /= 0) error stop "inv: dgetrf failed"

lwork = -1
call dgetri(n, ainv, n, ipiv, wq, lwork, info)
if (info /= 0) error stop "inv: work query failed"
lwork = max(1, int(wq(1)))
allocate(w(lwork))

call dgetri(n, ainv, n, ipiv, w, lwork, info)
if (info /= 0) error stop "inv: singular matrix or dgetri failed"
end function inv_mat

function pinv_mat(a, tol) result(ap)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), intent(in), optional :: tol
real(kind=dp), allocatable :: ap(:,:)

real(kind=dp), allocatable :: u(:,:), s(:), vt(:,:)
real(kind=dp), allocatable :: vk(:,:), ut(:,:), w(:,:)
real(kind=dp) :: t, smax
integer :: m, n, k, i

call svd_full_sub(a, u, s, vt)

m = size(a,1)
n = size(a,2)
k = min(m,n)

smax = 0.0_dp
if (k > 0) smax = maxval(s)

if (present(tol)) then
   t = tol
else
   t = real(max(m,n), kind=dp) * epsilon(1.0_dp) * smax
end if

! v(:,1:k) from vt(1:k,:) (since vt is V^T)
allocate(vk(n,k))
vk = transpose(vt(1:k, :))

allocate(ut(k,m))
ut = transpose(u(:,1:k))

allocate(w(k,m))
w = ut
do i = 1, k
   if (s(i) > t) then
      w(i,:) = w(i,:) / s(i)
   else
      w(i,:) = 0.0_dp
   end if
end do

allocate(ap(n,m))
ap = matmul(vk, w)
end function pinv_mat

! ----------------------------
! det / rank / trace / norm
! ----------------------------
function det_mat(a) result(d)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp) :: d
real(kind=dp), allocatable :: lu(:,:)
integer, allocatable :: ipiv(:)
integer :: n, info, i, swaps, sgn

call assert_square(a, "det")
n = size(a,1)
allocate(lu(n,n), ipiv(n))
lu = a

call dgetrf(n, n, lu, n, ipiv, info)
if (info /= 0) error stop "det: dgetrf failed"

swaps = 0
do i = 1, n
   if (ipiv(i) /= i) swaps = swaps + 1
end do
sgn = 1
if (mod(swaps,2) == 1) sgn = -1

d = real(sgn, kind=dp)
do i = 1, n
   d = d * lu(i,i)
end do
end function det_mat

integer function rank_mat(a, tol) result(r)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), intent(in), optional :: tol
real(kind=dp), allocatable :: s(:)
real(kind=dp) :: t, smax
integer :: i

s = svd_vals(a)
if (size(s) == 0) then
   r = 0
   return
end if

smax = maxval(s)
if (present(tol)) then
   t = tol
else
   t = real(max(size(a,1), size(a,2)), kind=dp) * epsilon(1.0_dp) * smax
end if

r = 0
do i = 1, size(s)
   if (s(i) > t) r = r + 1
end do
end function rank_mat

real(kind=dp) function trace_mat(a) result(t)
real(kind=dp), intent(in) :: a(:,:)
integer :: i, k
k = min(size(a,1), size(a,2))
t = 0.0_dp
do i = 1, k
   t = t + a(i,i)
end do
end function trace_mat

real(kind=dp) function norm_mat(a, kind) result(nrm)
real(kind=dp), intent(in) :: a(:,:)
character(len=*), intent(in), optional :: kind
character(len=16) :: k
integer :: i, j
real(kind=dp) :: tmp
real(kind=dp), allocatable :: s(:)

k = "2"
if (present(kind)) k = tolower_str(adjustl(kind))

select case (trim(k))
case ("fro", "f")
   nrm = 0.0_dp
   do j = 1, size(a,2)
      do i = 1, size(a,1)
         nrm = nrm + a(i,j)*a(i,j)
      end do
   end do
   nrm = sqrt(nrm)
case ("1")
   nrm = 0.0_dp
   do j = 1, size(a,2)
      tmp = 0.0_dp
      do i = 1, size(a,1)
         tmp = tmp + abs(a(i,j))
      end do
      if (tmp > nrm) nrm = tmp
   end do
case ("inf")
   nrm = 0.0_dp
   do i = 1, size(a,1)
      tmp = 0.0_dp
      do j = 1, size(a,2)
         tmp = tmp + abs(a(i,j))
      end do
      if (tmp > nrm) nrm = tmp
   end do
case ("2")
   s = svd_vals(a)
   if (size(s) == 0) then
      nrm = 0.0_dp
   else
      nrm = maxval(s)
   end if
case default
   error stop "norm: kind must be ""2"", ""fro"", ""1"", or ""inf"""
end select
end function norm_mat

! ----------------------------
! eig
! ----------------------------
function eig_vals(a) result(w)
real(kind=dp), intent(in) :: a(:,:)
complex(kind=dp), allocatable :: w(:)
real(kind=dp), allocatable :: aw(:,:), wr(:), wi(:), work(:), vl(:,:), vr(:,:)
real(kind=dp) :: wq(1)
integer :: n, info, lwork

call assert_square(a, "eig")
n = size(a,1)
allocate(aw(n,n))
aw = a

allocate(wr(n), wi(n))
allocate(vl(1,1), vr(1,1))

lwork = -1
call dgeev("N", "N", n, aw, n, wr, wi, vl, 1, vr, 1, wq, lwork, info)
if (info /= 0) error stop "eig: work query failed"
lwork = max(1, int(wq(1)))
allocate(work(lwork))

call dgeev("N", "N", n, aw, n, wr, wi, vl, 1, vr, 1, work, lwork, info)
if (info /= 0) error stop "eig: dgeev failed"

allocate(w(n))
w = cmplx(wr, wi, kind=dp)
end function eig_vals

subroutine eig_vals_vecs(a, w, v)
real(kind=dp), intent(in) :: a(:,:)
complex(kind=dp), allocatable, intent(out) :: w(:)
complex(kind=dp), allocatable, intent(out) :: v(:,:)

real(kind=dp), allocatable :: aw(:,:), wr(:), wi(:), work(:), vl(:,:), vr(:,:)
real(kind=dp) :: wq(1)
integer :: n, info, lwork
integer :: i

call assert_square(a, "eig_full")
n = size(a,1)

allocate(aw(n,n))
aw = a

allocate(wr(n), wi(n))
allocate(vl(1,1), vr(n,n))

lwork = -1
call dgeev("N", "V", n, aw, n, wr, wi, vl, 1, vr, n, wq, lwork, info)
if (info /= 0) error stop "eig_full: work query failed"
lwork = max(1, int(wq(1)))
allocate(work(lwork))

call dgeev("N", "V", n, aw, n, wr, wi, vl, 1, vr, n, work, lwork, info)
if (info /= 0) error stop "eig_full: dgeev failed"

allocate(w(n))
w = cmplx(wr, wi, kind=dp)

allocate(v(n,n))
v = (0.0_dp, 0.0_dp)

i = 1
do while (i <= n)
   if (wi(i) == 0.0_dp) then
      v(:,i) = cmplx(vr(:,i), 0.0_dp, kind=dp)
      i = i + 1
   else if (wi(i) > 0.0_dp) then
      v(:,i)   = cmplx(vr(:,i),  vr(:,i+1), kind=dp)
      v(:,i+1) = cmplx(vr(:,i), -vr(:,i+1), kind=dp)
      i = i + 2
   else
      v(:,i) = cmplx(vr(:,i), 0.0_dp, kind=dp)
      i = i + 1
   end if
end do
end subroutine eig_vals_vecs

! ----------------------------
! svd
! ----------------------------
function svd_vals(a) result(s)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable :: s(:)
real(kind=dp), allocatable :: aw(:,:), work(:)
real(kind=dp) :: wq(1)
real(kind=dp), allocatable :: u_dummy(:,:), vt_dummy(:,:)
integer :: m, n, k, info, lwork

m = size(a,1)
n = size(a,2)
k = min(m,n)

allocate(aw(m,n))
aw = a
allocate(s(k))

allocate(u_dummy(1,1), vt_dummy(1,1))

lwork = -1
call dgesvd("N", "N", m, n, aw, m, s, u_dummy, 1, vt_dummy, 1, wq, lwork, info)
if (info /= 0) error stop "svd: work query failed"
lwork = max(1, int(wq(1)))
allocate(work(lwork))

call dgesvd("N", "N", m, n, aw, m, s, u_dummy, 1, vt_dummy, 1, work, lwork, info)
if (info /= 0) error stop "svd: dgesvd failed"
end function svd_vals

subroutine svd_full_sub(a, u, s, vt)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: u(:,:), s(:), vt(:,:)
real(kind=dp), allocatable :: aw(:,:), work(:)
real(kind=dp) :: wq(1)
integer :: m, n, k, info, lwork

m = size(a,1)
n = size(a,2)
k = min(m,n)

allocate(aw(m,n))
aw = a

allocate(u(m,m), vt(n,n), s(k))

lwork = -1
call dgesvd("A", "A", m, n, aw, m, s, u, m, vt, n, wq, lwork, info)
if (info /= 0) error stop "svd_full: work query failed"
lwork = max(1, int(wq(1)))
allocate(work(lwork))

call dgesvd("A", "A", m, n, aw, m, s, u, m, vt, n, work, lwork, info)
if (info /= 0) error stop "svd_full: dgesvd failed"
end subroutine svd_full_sub

! ----------------------------
! qr
! ----------------------------
subroutine qr_full_sub(a, q, r)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: q(:,:), r(:,:)

real(kind=dp), allocatable :: aw(:,:), tau(:), work(:)
real(kind=dp) :: wq(1)
integer :: m, n, k, info, lwork
integer :: i, j

m = size(a,1)
n = size(a,2)
k = min(m,n)

allocate(aw(m,n))
aw = a
allocate(tau(k))

lwork = -1
call dgeqrf(m, n, aw, m, tau, wq, lwork, info)
if (info /= 0) error stop "qr_full: work query failed"
lwork = max(1, int(wq(1)))
allocate(work(lwork))

call dgeqrf(m, n, aw, m, tau, work, lwork, info)
if (info /= 0) error stop "qr_full: dgeqrf failed"

allocate(r(m,n))
r = 0.0_dp
do i = 1, m
   do j = i, n
      r(i,j) = aw(i,j)
   end do
end do

allocate(q(m,m))
q = 0.0_dp
if (k > 0) q(:,1:k) = aw(:,1:k)

lwork = -1
call dorgqr(m, m, k, q, m, tau, wq, lwork, info)
if (info /= 0) error stop "qr_full: work query failed"
lwork = max(1, int(wq(1)))
deallocate(work)
allocate(work(lwork))

call dorgqr(m, m, k, q, m, tau, work, lwork, info)
if (info /= 0) error stop "qr_full: dorgqr failed"
end subroutine qr_full_sub

function qr_r_only(a) result(r)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable :: r(:,:)
real(kind=dp), allocatable :: q(:,:)
call qr_full_sub(a, q, r)
end function qr_r_only

! ----------------------------
! lu
! ----------------------------
function lu_packed(a) result(lu)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable :: lu(:,:)
integer, allocatable :: ipiv(:)
integer :: m, n, k, info

m = size(a,1)
n = size(a,2)
k = min(m,n)

allocate(lu(m,n))
lu = a
allocate(ipiv(k))

call dgetrf(m, n, lu, m, ipiv, info)
if (info /= 0) error stop "lu: dgetrf failed"
end function lu_packed

subroutine lu_full_sub(a, l, u, p)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: l(:,:), u(:,:), p(:,:)

real(kind=dp), allocatable :: lu(:,:)
integer, allocatable :: ipiv(:)
integer :: m, n, k, info
integer :: i, j, ip

m = size(a,1)
n = size(a,2)
k = min(m,n)

allocate(lu(m,n))
lu = a
allocate(ipiv(k))

call dgetrf(m, n, lu, m, ipiv, info)
if (info /= 0) error stop "lu_full: dgetrf failed"

allocate(l(m,k))
l = 0.0_dp
do j = 1, k
   do i = 1, m
      if (i > j) then
         l(i,j) = lu(i,j)
      else if (i == j) then
         l(i,j) = 1.0_dp
      end if
   end do
end do

allocate(u(k,n))
u = 0.0_dp
do i = 1, k
   do j = i, n
      u(i,j) = lu(i,j)
   end do
end do

allocate(p(m,m))
p = 0.0_dp
do i = 1, m
   p(i,i) = 1.0_dp
end do
do i = 1, k
   ip = ipiv(i)
   if (ip /= i) then
      call swap_rows(p, i, ip)
   end if
end do
end subroutine lu_full_sub

subroutine swap_rows(a, i, j)
real(kind=dp), intent(inout) :: a(:,:)
integer, intent(in) :: i, j
real(kind=dp) :: tmp
integer :: k
do k = 1, size(a,2)
   tmp = a(i,k)
   a(i,k) = a(j,k)
   a(j,k) = tmp
end do
end subroutine swap_rows

! ----------------------------
! chol
! ----------------------------
function chol_upper(a) result(r)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable :: r(:,:)
call chol_upper_sub(a, r)
end function chol_upper

subroutine chol_upper_sub(a, r)
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable, intent(out) :: r(:,:)
real(kind=dp), allocatable :: aw(:,:)
integer :: n, info, i, j

call assert_square(a, "chol")
n = size(a,1)
allocate(aw(n,n))
aw = a

call dpotrf("U", n, aw, n, info)
if (info /= 0) error stop "chol: matrix is not positive definite"

allocate(r(n,n))
r = 0.0_dp
do j = 1, n
   do i = 1, j
      r(i,j) = aw(i,j)
   end do
end do
end subroutine chol_upper_sub

end module linear_algebra_mod
