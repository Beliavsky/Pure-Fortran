# xarray.py Transform Examples

This file shows typical `xarray.py` rewrites with short before/after snippets.

## 1) Elementwise loop -> whole-array operations

Before:
```fortran
do i = 1, n
   call random_number(x(i))
   x(i) = x(i) - 0.5
end do
```

After:
```fortran
call random_number(x)
x = x - 0.5
```

## 2) Reduction loop -> intrinsic reduction

Before:
```fortran
sum_positive = 0.0
do i = 1, n
   if (x(i) > 0.0) sum_positive = sum_positive + x(i)
end do
```

After:
```fortran
sum_positive = sum(x, mask = x > 0.0)
```

## 3) Min/max update loop -> minval/maxval

Before:
```fortran
do i = 1, n
   if (i == 1) then
      xmin = x(i)
      xmax = x(i)
   else
      xmin = min(xmin, x(i))
      xmax = max(xmax, x(i))
   end if
end do
```

After:
```fortran
xmin = minval(x)
xmax = maxval(x)
```

## 4) Print loop -> implied-do I/O

Before:
```fortran
do i = 1, n
   print "(i2,f7.3)", i, x(i)
end do
```

After:
```fortran
print "(i2,f7.3)", (i, x(i), i=1,n)
```

## 5) Consecutive element assignments -> array constructor

Before:
```fortran
a(1) = 10
a(2) = 20
a(3) = 30
```

After:
```fortran
a(1:3) = [10, 20, 30]
```

## 6) Sparse index assignments -> vector-subscript constructor

Before:
```fortran
k(1) = 11
k(4) = 44
k(9) = 99
```

After:
```fortran
k([1, 4, 9]) = [11, 44, 99]
```

## 7) Safe same-array packing

Before:
```fortran
node(2) = node(1) - 1
node(3) = node(1) - 2
```

After:
```fortran
node(2:3) = [node(1) - 1, node(1) - 2]
```

## 8) Optional concurrent rewrite (`--concurrent`)

Before:
```fortran
do i = 1, ni
   do j = 1, nj
      do k = 1, nk
         a(i,j,k) = b(i,j,k) + c(i,j,k)
      end do
   end do
end do
```

After:
```fortran
do concurrent (i = 1:ni, j = 1:nj, k = 1:nk)
   a(i,j,k) = b(i,j,k) + c(i,j,k)
end do
```

## 9) Post-pass temporary inlining (`--inline`)

Before:
```fortran
sum_positive = 0.0
do i = 1, n
   if (x(i) > 0.0) sum_positive = sum_positive + x(i)
end do
print *, sum_positive
```

After:
```fortran
print *, sum(x, mask = x > 0.0)
```

## 10) Matrix-vector loop -> `matmul`

Before:
```fortran
b(1:m) = 0.0
do i = 1, m
   do j = 1, n
      b(i) = b(i) + a(i,j) * x(j)
   end do
end do
```

After:
```fortran
b = matmul(a(1:m,1:n), x(1:n))
```

## 11) Row/column broadcast loop -> `spread`

Before:
```fortran
do i = 1, nrow
   a(i,1:ncol) = key(i)
end do
```

After:
```fortran
a(1:nrow,1:ncol) = spread(key(1:nrow), dim=2, ncopies=ncol)
```
