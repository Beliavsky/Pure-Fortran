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

## 2) Full-section assignment -> whole-array assignment

Before:
```fortran
r(1:n) = b(1:n) - ap(1:n)
x(1:n) = x(1:n) + alpha * p(1:n)
```

After:
```fortran
r = b - ap
x = x + alpha * p
```

## 3) Reduction loop -> intrinsic reduction

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

## 4) Min/max update loop -> minval/maxval

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

## 5) Print loop -> implied-do I/O

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

## 6) Consecutive element assignments -> array constructor

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

## 7) Sparse index assignments -> vector-subscript constructor

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

## 8) Safe same-array packing

Before:
```fortran
node(2) = node(1) - 1
node(3) = node(1) - 2
```

After:
```fortran
node(2:3) = [node(1) - 1, node(1) - 2]
```

## 9) Optional concurrent rewrite (`--concurrent`)

Before:
```fortran
do i = 1, n
   y(i) = f(x(i))
end do
```

After:
```fortran
do concurrent (i = 1:n)
   y(i) = f(x(i))
end do
```

## 10) Post-pass temporary inlining (`--inline`)

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
