module python_mod
use, intrinsic :: iso_fortran_env, only: real64
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
implicit none
private
integer, parameter :: dp = real64
logical, parameter :: fft_use_fast = .true.

type :: strvec_t
   character(len=:), allocatable :: v(:)
end type strvec_t

public :: isqrt_int       !@pyapi kind=function ret=integer args=x:integer:intent(in) desc="integer square root: return floor(sqrt(x)) for x >= 0"
public :: print_int_list  !@pyapi kind=subroutine args=a:integer(:):intent(in),n:integer:intent(in) desc="print integer list a(1:n) in python-style [..] format"
public :: seed_rng !@pyapi kind=subroutine args=seed:integer:intent(in):optional desc="seed intrinsic RNG; deterministic stream when seed is provided"
public :: random_normal_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out) desc="fill x with N(0,1) variates using Box-Muller"
public :: runif !@pyapi kind=function ret=real(dp) args=n:integer:intent(in):optional,m:integer:intent(in):optional desc="uniform variates: scalar runif(), vector runif(n), matrix runif(n,m)"
public :: rnorm !@pyapi kind=function ret=real(dp) args=n:integer:intent(in):optional,m:integer:intent(in):optional desc="normal variates: scalar rnorm(), vector rnorm(n), matrix rnorm(n,m)"
public :: random_exponential !@pyapi kind=function ret=real(dp) args=scale:real(dp):intent(in):optional desc="sample scalar exponential with scale (default 1)"
public :: random_exponential_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),scale:real(dp):intent(in):optional desc="fill x with exponential samples with scale (default 1)"
public :: random_gamma !@pyapi kind=function ret=real(dp) args=shape:real(dp):intent(in),scale:real(dp):intent(in):optional desc="sample scalar gamma(shape, scale), shape>0"
public :: random_gamma_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),shape:real(dp):intent(in),scale:real(dp):intent(in):optional desc="fill x with gamma samples"
public :: random_student_t !@pyapi kind=function ret=real(dp) args=df:real(dp):intent(in) desc="sample scalar Student t(df)"
public :: random_student_t_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),df:real(dp):intent(in) desc="fill x with Student t(df) samples"
public :: random_f !@pyapi kind=function ret=real(dp) args=dfnum:real(dp):intent(in),dfden:real(dp):intent(in) desc="sample scalar F(dfnum,dfden)"
public :: random_f_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),dfnum:real(dp):intent(in),dfden:real(dp):intent(in) desc="fill x with F samples"
public :: random_poisson !@pyapi kind=function ret=integer args=mu:real(dp):intent(in) desc="sample scalar Poisson(mu)"
public :: random_poisson_vec !@pyapi kind=subroutine args=x:integer(:):intent(out),mu:real(dp):intent(in) desc="fill x with Poisson(mu) samples"
public :: random_geometric !@pyapi kind=function ret=integer args=p:real(dp):intent(in) desc="sample scalar Geometric(p), support on {1,2,...}"
public :: random_geometric_vec !@pyapi kind=subroutine args=x:integer(:):intent(out),p:real(dp):intent(in) desc="fill x with Geometric(p) samples"
public :: random_binomial !@pyapi kind=function ret=integer args=n:integer:intent(in),p:real(dp):intent(in) desc="sample scalar Binomial(n,p)"
public :: random_binomial_vec !@pyapi kind=subroutine args=x:integer(:):intent(out),n:integer:intent(in),p:real(dp):intent(in) desc="fill x with Binomial(n,p) samples"
public :: random_hypergeometric !@pyapi kind=function ret=integer args=ngood:integer:intent(in),nbad:integer:intent(in),nsample:integer:intent(in) desc="sample scalar Hypergeometric(ngood,nbad,nsample)"
public :: random_hypergeometric_vec !@pyapi kind=subroutine args=x:integer(:):intent(out),ngood:integer:intent(in),nbad:integer:intent(in),nsample:integer:intent(in) desc="fill x with hypergeometric samples"
public :: random_zipf !@pyapi kind=function ret=integer args=a:real(dp):intent(in) desc="sample scalar Zipf(a), a>1"
public :: random_zipf_vec !@pyapi kind=subroutine args=x:integer(:):intent(out),a:real(dp):intent(in) desc="fill x with Zipf(a) samples"
public :: random_weibull !@pyapi kind=function ret=real(dp) args=a:real(dp):intent(in) desc="sample scalar Weibull(a)"
public :: random_weibull_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),a:real(dp):intent(in) desc="fill x with Weibull(a) samples"
public :: random_neg_binomial !@pyapi kind=function ret=integer args=sk:real(dp):intent(in),p:real(dp):intent(in) desc="sample scalar negative binomial failures before sk successes"
public :: random_neg_binomial_vec !@pyapi kind=subroutine args=x:integer(:):intent(out),sk:real(dp):intent(in),p:real(dp):intent(in) desc="fill x with negative binomial samples"
public :: random_von_mises !@pyapi kind=function ret=real(dp) args=kappa:real(dp):intent(in) desc="sample scalar von Mises with concentration kappa"
public :: random_von_mises_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),kappa:real(dp):intent(in) desc="fill x with von Mises samples"
public :: random_pareto !@pyapi kind=function ret=real(dp) args=a:real(dp):intent(in) desc="sample scalar Pareto(a) in NumPy convention"
public :: random_pareto_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),a:real(dp):intent(in) desc="fill x with Pareto(a) samples"
public :: random_power !@pyapi kind=function ret=real(dp) args=a:real(dp):intent(in) desc="sample scalar Power(a) in NumPy convention"
public :: random_power_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),a:real(dp):intent(in) desc="fill x with Power(a) samples"
public :: random_rayleigh !@pyapi kind=function ret=real(dp) args=scale:real(dp):intent(in):optional desc="sample scalar Rayleigh(scale), default scale=1"
public :: random_rayleigh_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),scale:real(dp):intent(in):optional desc="fill x with Rayleigh samples"
public :: random_gumbel !@pyapi kind=function ret=real(dp) args=loc:real(dp):intent(in):optional,scale:real(dp):intent(in):optional desc="sample scalar Gumbel(loc,scale)"
public :: random_gumbel_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),loc:real(dp):intent(in):optional,scale:real(dp):intent(in):optional desc="fill x with Gumbel samples"
public :: random_wald !@pyapi kind=function ret=real(dp) args=mean:real(dp):intent(in),scale:real(dp):intent(in) desc="sample scalar Wald(mean,scale)"
public :: random_wald_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),mean:real(dp):intent(in),scale:real(dp):intent(in) desc="fill x with Wald samples"
public :: random_noncentral_chisquare !@pyapi kind=function ret=real(dp) args=df:real(dp):intent(in),nonc:real(dp):intent(in) desc="sample scalar noncentral chisquare(df,nonc)"
public :: random_noncentral_chisquare_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),df:real(dp):intent(in),nonc:real(dp):intent(in) desc="fill x with noncentral chisquare samples"
public :: random_noncentral_f !@pyapi kind=function ret=real(dp) args=dfnum:real(dp):intent(in),dfden:real(dp):intent(in),nonc:real(dp):intent(in) desc="sample scalar noncentral F(dfnum,dfden,nonc)"
public :: random_noncentral_f_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),dfnum:real(dp):intent(in),dfden:real(dp):intent(in),nonc:real(dp):intent(in) desc="fill x with noncentral F samples"
public :: random_triangular !@pyapi kind=function ret=real(dp) args=left:real(dp):intent(in),mode:real(dp):intent(in),right:real(dp):intent(in) desc="sample scalar triangular(left,mode,right)"
public :: random_triangular_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),left:real(dp):intent(in),mode:real(dp):intent(in),right:real(dp):intent(in) desc="fill x with triangular samples"
public :: random_logseries !@pyapi kind=function ret=integer args=p:real(dp):intent(in) desc="sample scalar logarithmic-series(p)"
public :: random_logseries_vec !@pyapi kind=subroutine args=x:integer(:):intent(out),p:real(dp):intent(in) desc="fill x with logarithmic-series samples"
public :: random_dirichlet !@pyapi kind=subroutine args=alpha:real(dp)(:):intent(in),x:real(dp)(:):intent(out) desc="sample one Dirichlet(alpha) vector"
public :: random_dirichlet_samples !@pyapi kind=subroutine args=alpha:real(dp)(:):intent(in),x:real(dp)(:,:):intent(out) desc="fill rows of x with Dirichlet(alpha) samples"
public :: random_multinomial !@pyapi kind=subroutine args=n:integer:intent(in),p:real(dp)(:):intent(in),x:integer(:):intent(out) desc="sample one Multinomial(n,p) count vector"
public :: random_multinomial_samples !@pyapi kind=subroutine args=n:integer:intent(in),p:real(dp)(:):intent(in),x:integer(:,:):intent(out) desc="fill rows of x with Multinomial(n,p) samples"
public :: random_multivariate_hypergeometric !@pyapi kind=subroutine args=ngood:integer(:):intent(in),nsample:integer:intent(in),x:integer(:):intent(out) desc="sample one multivariate hypergeometric count vector"
public :: random_multivariate_hypergeometric_samples !@pyapi kind=subroutine args=ngood:integer(:):intent(in),nsample:integer:intent(in),x:integer(:,:):intent(out) desc="fill rows of x with multivariate hypergeometric samples"
public :: random_beta !@pyapi kind=function ret=real(dp) args=a:real(dp):intent(in),b:real(dp):intent(in) desc="sample scalar beta(a,b), a,b>0"
public :: random_beta_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),a:real(dp):intent(in),b:real(dp):intent(in) desc="fill x with beta samples"
public :: random_lognormal !@pyapi kind=function ret=real(dp) args=meanlog:real(dp):intent(in):optional,sdlog:real(dp):intent(in):optional desc="sample scalar lognormal exp(N(meanlog,sdlog^2))"
public :: random_lognormal_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),meanlog:real(dp):intent(in):optional,sdlog:real(dp):intent(in):optional desc="fill x with lognormal samples"
public :: random_chisquare !@pyapi kind=function ret=real(dp) args=df:real(dp):intent(in) desc="sample scalar chi-square(df)"
public :: random_chisquare_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),df:real(dp):intent(in) desc="fill x with chi-square samples"
public :: random_laplace !@pyapi kind=function ret=real(dp) args=loc:real(dp):intent(in):optional,scale:real(dp):intent(in):optional desc="sample scalar Laplace(loc,scale)"
public :: random_laplace_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),loc:real(dp):intent(in):optional,scale:real(dp):intent(in):optional desc="fill x with Laplace samples"
public :: random_logistic !@pyapi kind=function ret=real(dp) args=loc:real(dp):intent(in):optional,scale:real(dp):intent(in):optional desc="sample scalar Logistic(loc,scale)"
public :: random_logistic_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),loc:real(dp):intent(in):optional,scale:real(dp):intent(in):optional desc="fill x with Logistic samples"
public :: random_cauchy !@pyapi kind=function ret=real(dp) args=loc:real(dp):intent(in):optional,scale:real(dp):intent(in):optional desc="sample scalar Cauchy(loc,scale)"
public :: random_cauchy_vec !@pyapi kind=subroutine args=x:real(dp)(:):intent(out),loc:real(dp):intent(in):optional,scale:real(dp):intent(in):optional desc="fill x with Cauchy samples"
public :: random_mvn_samples !@pyapi kind=subroutine args=mean:real(dp)(:):intent(in),cov:real(dp)(:,:):intent(in),x:real(dp)(:,:):intent(out) desc="fill x(nsamp,ndim) with multivariate-normal samples using Cholesky factorization"
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
public :: py_str !@pyapi kind=function ret=character args=x:any:intent(in) desc="python-like str() conversion for scalar int/real/logical/char"
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
public :: unique_char !@pyapi kind=function ret=character(:) args=x:character(:):intent(in) desc="unique values of character vector (first-occurrence order)"
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
public :: setdiff1d_char !@pyapi kind=function ret=character(:) args=a:character(:):intent(in),b:character(:):intent(in) desc="unique values in a not in b (character)"
public :: intersect1d_char !@pyapi kind=function ret=character(:) args=a:character(:):intent(in),b:character(:):intent(in) desc="unique intersection of a and b (character)"
public :: unique_int_inv_counts !@pyapi kind=subroutine args=a:integer(:):intent(in),u:integer(:):intent(out),inv:integer(:):intent(out),cnt:integer(:):intent(out) desc="unique values with inverse and counts"
public :: unique_int_counts !@pyapi kind=subroutine args=a:integer(:):intent(in),u:integer(:):intent(out),cnt:integer(:):intent(out) desc="unique values with counts"
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
public :: linalg_cholesky !@pyapi kind=function ret=real(dp)(:,:) args=a:real(dp)(:,:):intent(in) desc="lower-triangular Cholesky factor using LAPACK DPOTRF"
public :: linalg_det !@pyapi kind=function ret=real(dp) args=a:real(dp)(:,:):intent(in) desc="determinant of square matrix using LAPACK DGETRF"
public :: linalg_inv !@pyapi kind=function ret=real(dp)(:,:) args=a:real(dp)(:,:):intent(in) desc="matrix inverse using LAPACK DGETRF/DGETRI"
public :: linalg_eig !@pyapi kind=subroutine args=a:real(dp)(:,:):intent(in),w:real(dp)(:):intent(out),v:real(dp)(:,:):intent(out) desc="right eigenpairs of real square matrix using LAPACK DGEEV (real-spectrum only)"
public :: linalg_eigh !@pyapi kind=subroutine args=a:real(dp)(:,:):intent(in),w:real(dp)(:):intent(out),v:real(dp)(:,:):intent(out) desc="eigenpairs of real symmetric matrix using LAPACK DSYEV"
public :: linalg_qr_reduced !@pyapi kind=subroutine args=a:real(dp)(:,:):intent(in),q:real(dp)(:,:):intent(out),r:real(dp)(:,:):intent(out) desc="reduced QR factorization using LAPACK DGEQRF/DORGQR"
public :: linalg_svd !@pyapi kind=subroutine args=a:real(dp)(:,:):intent(in),u:real(dp)(:,:):intent(out),s:real(dp)(:):intent(out),vt:real(dp)(:,:):intent(out) desc="full SVD using LAPACK DGESVD"
public :: quantile_linear !@pyapi kind=function ret=real(dp) args=x:real(dp)(:):intent(in),q:real(dp):intent(in) desc="1D quantile with linear interpolation (NumPy-like default)"
public :: tri_int !@pyapi kind=function ret=integer(:,:) args=n:integer:intent(in),m:integer:intent(in),k:integer:intent(in):optional desc="lower-triangular ones matrix with diagonal offset k"
public :: tri_real !@pyapi kind=function ret=real(dp)(:,:) args=n:integer:intent(in),m:integer:intent(in),k:integer:intent(in):optional desc="lower-triangular ones matrix with diagonal offset k"
public :: moveaxis3_int !@pyapi kind=function ret=integer(:,:,:) args=a:integer(:,:,:):intent(in),src:integer:intent(in),dst:integer:intent(in) desc="move one axis for rank-3 integer arrays (NumPy-style indices)"
public :: moveaxis3_real !@pyapi kind=function ret=real(dp)(:,:,:) args=a:real(dp)(:,:,:):intent(in),src:integer:intent(in),dst:integer:intent(in) desc="move one axis for rank-3 real arrays (NumPy-style indices)"
public :: moveaxis3_logical !@pyapi kind=function ret=logical(:,:,:) args=a:logical(:,:,:):intent(in),src:integer:intent(in),dst:integer:intent(in) desc="move one axis for rank-3 logical arrays (NumPy-style indices)"
public :: pad2d_int !@pyapi kind=function ret=integer(:,:) args=a:integer(:,:):intent(in),pt:integer:intent(in),pb:integer:intent(in),pl:integer:intent(in),pr:integer:intent(in),c:integer:intent(in) desc="2D constant pad for integer matrix"
public :: pad2d_real !@pyapi kind=function ret=real(dp)(:,:) args=a:real(dp)(:,:):intent(in),pt:integer:intent(in),pb:integer:intent(in),pl:integer:intent(in),pr:integer:intent(in),c:real(dp):intent(in) desc="2D constant pad for real matrix"
public :: allclose !@pyapi kind=function ret=logical args=a:real(dp)(:):intent(in),b:real(dp)(:):intent(in),rtol:real(dp):intent(in):optional,atol:real(dp):intent(in):optional,equal_nan:logical:intent(in):optional desc="NumPy-like allclose on 1D arrays"
public :: allclose_integer !@pyapi kind=function ret=logical args=a:integer(:):intent(in),b:integer(:):intent(in),rtol:real(dp):intent(in):optional,atol:real(dp):intent(in):optional desc="NumPy-like allclose on 1D integer arrays"
public :: cov2_real !@pyapi kind=function ret=real(dp)(:,:) args=x:real(dp)(:):intent(in),y:real(dp)(:):intent(in),ddof:integer:intent(in):optional desc="2x2 covariance matrix for two real vectors"
public :: cov_matrix_rows_real !@pyapi kind=function ret=real(dp)(:,:) args=x:real(dp)(:,:):intent(in),ddof:integer:intent(in):optional desc="covariance matrix for observations in rows (numpy rowvar=False)"
public :: corrcoef2_real !@pyapi kind=function ret=real(dp)(:,:) args=x:real(dp)(:):intent(in),y:real(dp)(:):intent(in) desc="2x2 correlation matrix for two real vectors"
public :: convolve_real !@pyapi kind=function ret=real(dp)(:) args=x:real(dp)(:):intent(in),h:real(dp)(:):intent(in),mode:character:intent(in):optional desc="1D convolution with mode full/same/valid"
public :: convolve_int !@pyapi kind=function ret=integer(:) args=x:integer(:):intent(in),h:integer(:):intent(in),mode:character:intent(in):optional desc="1D integer convolution with mode full/same/valid"
public :: loadtxt_real_2d !@pyapi kind=function ret=real(dp)(:,:) args=path:character:intent(in),skiprows:integer:intent(in):optional desc="load whitespace-delimited real matrix text file"
public :: savetxt_real_2d !@pyapi kind=subroutine args=path:character:intent(in),x:real(dp)(:,:):intent(in) desc="write whitespace-delimited real matrix text file"
public :: print_matrix !@pyapi kind=subroutine args=a:real(dp)(:,:):intent(in),label:character:intent(in):optional desc="print 2D real matrix with aligned columns"
public :: polyval_real_scalar !@pyapi kind=function ret=real(dp) args=p:real(dp)(:):intent(in),x:real(dp):intent(in) desc="evaluate polynomial with descending coefficients at scalar x"
public :: polyval_real_vec !@pyapi kind=function ret=real(dp)(:) args=p:real(dp)(:):intent(in),x:real(dp)(:):intent(in) desc="evaluate polynomial with descending coefficients at vector x"
public :: polyder_real !@pyapi kind=function ret=real(dp)(:) args=p:real(dp)(:):intent(in),m:integer:intent(in):optional desc="m-th derivative coefficients for descending-order polynomial"
public :: fft_fft !@pyapi kind=function ret=complex(dp)(:) args=x:real(dp)(:):intent(in),n:integer:intent(in):optional desc="numpy-like fft for 1D real/complex input (naive DFT backend)"
public :: fft_ifft !@pyapi kind=function ret=complex(dp)(:) args=x:complex(dp)(:):intent(in),n:integer:intent(in):optional desc="numpy-like ifft for 1D complex input (naive DFT backend)"
public :: fft_rfft !@pyapi kind=function ret=complex(dp)(:) args=x:real(dp)(:):intent(in),n:integer:intent(in):optional desc="numpy-like rfft for 1D real input"
public :: fft_irfft !@pyapi kind=function ret=real(dp)(:) args=x:complex(dp)(:):intent(in),n:integer:intent(in):optional desc="numpy-like irfft for 1D Hermitian spectrum"
public :: fft_fftfreq !@pyapi kind=function ret=real(dp)(:) args=n:integer:intent(in),d:real(dp):intent(in):optional desc="numpy-like fftfreq(n,d)"
public :: fft_rfftfreq !@pyapi kind=function ret=real(dp)(:) args=n:integer:intent(in),d:real(dp):intent(in):optional desc="numpy-like rfftfreq(n,d)"

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
public :: optval !@pyapi kind=function ret=scalar args=x:scalar:intent(in):optional,default:scalar:intent(in) desc="return x when present, otherwise default"
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
public :: polyval
public :: polyder

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
   module procedure unique_real, unique_int, unique_char
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

interface polyval
   module procedure polyval_real_scalar, polyval_real_vec
end interface polyval

interface polyder
   module procedure polyder_real
end interface polyder

interface runif
   module procedure runif0, runif1, runif2
end interface runif

interface rnorm
   module procedure rnorm0, rnorm1, rnorm2
end interface rnorm

interface allclose
   module procedure allclose_real, allclose_integer
end interface allclose

interface py_str
   module procedure py_str_int, py_str_real, py_str_logical, py_str_char
   module procedure py_str_int_vec, py_str_real_vec, py_str_logical_vec, py_str_char_vec
end interface py_str

interface linalg_solve
   module procedure linalg_solve_vec, linalg_solve_mat
end interface linalg_solve

interface optval
   module procedure optval_int
   module procedure optval_real
   module procedure optval_logical
   module procedure optval_char
end interface optval

interface convolve
   module procedure convolve_real
   module procedure convolve_int
end interface convolve

interface print_matrix
   module procedure print_matrix_real_2d
   module procedure print_matrix_label_real_2d
end interface print_matrix

interface fft_fft
   module procedure fft_fft_real
   module procedure fft_fft_complex
end interface fft_fft

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

      subroutine print_matrix_real_2d(a)
         real(kind=dp), intent(in) :: a(:,:)
         integer :: i, j, pad
         integer, allocatable :: w(:)
         character(len=64) :: buf

         allocate(w(size(a,2)))
         w = 1
         do j = 1, size(a,2)
            do i = 1, size(a,1)
               write(buf, "(g0)") a(i, j)
               w(j) = max(w(j), len_trim(adjustl(buf)))
            end do
         end do

         do i = 1, size(a,1)
            write(*, "(a)", advance="no") "["
            do j = 1, size(a,2)
               if (j > 1) write(*, "(a)", advance="no") " "
               write(buf, "(g0)") a(i, j)
               buf = adjustl(buf)
               pad = w(j) - len_trim(buf)
               if (pad > 0) write(*, "(a)", advance="no") repeat(" ", pad)
               write(*, "(a)", advance="no") trim(buf)
            end do
            write(*, "(a)") "]"
         end do
      end subroutine print_matrix_real_2d

      subroutine print_matrix_label_real_2d(label, a)
         character(len=*), intent(in) :: label
         real(kind=dp), intent(in) :: a(:,:)
         write(*, "(a)") trim(label)
         call print_matrix_real_2d(a)
      end subroutine print_matrix_label_real_2d

      function py_str_int(x) result(s)
         integer, intent(in) :: x
         character(len=:), allocatable :: s
         character(len=64) :: buf
         write(buf, "(i0)") x
         s = trim(adjustl(buf))
      end function py_str_int

      function py_str_real(x) result(s)
         real(kind=dp), intent(in) :: x
         character(len=:), allocatable :: s
         character(len=128) :: buf
         write(buf, "(g0)") x
         s = trim(adjustl(buf))
      end function py_str_real

      function py_str_logical(x) result(s)
         logical, intent(in) :: x
         character(len=:), allocatable :: s
         if (x) then
            s = "True"
         else
            s = "False"
         end if
      end function py_str_logical

      function py_str_char(x) result(s)
         character(len=*), intent(in) :: x
         character(len=:), allocatable :: s
         s = x
      end function py_str_char

      function py_str_int_vec(x) result(s)
         integer, intent(in) :: x(:)
         character(len=:), allocatable :: s(:)
         integer :: i
         allocate(character(len=64) :: s(size(x)))
         do i = 1, size(x)
            s(i) = py_str_int(x(i))
         end do
      end function py_str_int_vec

      function py_str_real_vec(x) result(s)
         real(kind=dp), intent(in) :: x(:)
         character(len=:), allocatable :: s(:)
         integer :: i
         allocate(character(len=128) :: s(size(x)))
         do i = 1, size(x)
            s(i) = py_str_real(x(i))
         end do
      end function py_str_real_vec

      function py_str_logical_vec(x) result(s)
         logical, intent(in) :: x(:)
         character(len=:), allocatable :: s(:)
         integer :: i
         allocate(character(len=5) :: s(size(x)))
         do i = 1, size(x)
            s(i) = py_str_logical(x(i))
         end do
      end function py_str_logical_vec

      function py_str_char_vec(x) result(s)
         character(len=*), intent(in) :: x(:)
         character(len=:), allocatable :: s(:)
         allocate(character(len=len(x)) :: s(size(x)))
         s = x
      end function py_str_char_vec

      subroutine seed_rng(seed)
         integer, intent(in), optional :: seed
         integer :: nseed_rng, i_rng, s0
         integer, allocatable :: seed_buf(:)
         if (.not. present(seed)) then
            call random_seed()
            return
         end if
         s0 = seed
         call random_seed(size=nseed_rng)
         allocate(seed_buf(nseed_rng))
         do i_rng = 1, nseed_rng
            seed_buf(i_rng) = s0 + 104729 * (i_rng - 1)
         end do
         call random_seed(put=seed_buf)
         deallocate(seed_buf)
      end subroutine seed_rng

      function loadtxt_real_2d(path, skiprows) result(x)
         character(len=*), intent(in) :: path
         integer, intent(in), optional :: skiprows
         real(kind=dp), allocatable :: x(:,:)
         integer :: u, ios, nskip, nrow, ncol, irow, i
         character(len=4096) :: line

         nskip = optval(skiprows, 0)
         open(newunit=u, file=trim(path), status="old", action="read", iostat=ios)
         if (ios /= 0) then
            allocate(x(0,0))
            return
         end if
         do i = 1, nskip
            read(u, "(A)", iostat=ios) line
            if (ios /= 0) exit
         end do

         nrow = 0
         ncol = 0
         do
            read(u, "(A)", iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle
            nrow = nrow + 1
            if (ncol == 0) ncol = count_tokens(line)
         end do

         if (nrow <= 0 .or. ncol <= 0) then
            close(u)
            allocate(x(0,0))
            return
         end if

         rewind(u)
         do i = 1, nskip
            read(u, "(A)", iostat=ios) line
            if (ios /= 0) exit
         end do

         allocate(x(nrow, ncol))
         irow = 0
         do
            read(u, "(A)", iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle
            irow = irow + 1
            read(line, *, iostat=ios) x(irow, :)
            if (ios /= 0) x(irow, :) = 0.0_dp
         end do
         close(u)
      contains
         pure integer function count_tokens(s) result(n)
            character(len=*), intent(in) :: s
            integer :: i
            logical :: in_tok
            n = 0
            in_tok = .false.
            do i = 1, len_trim(s)
               if (s(i:i) /= " " .and. s(i:i) /= achar(9)) then
                  if (.not. in_tok) then
                     n = n + 1
                     in_tok = .true.
                  end if
               else
                  in_tok = .false.
               end if
            end do
         end function count_tokens
      end function loadtxt_real_2d

      subroutine savetxt_real_2d(path, x)
         character(len=*), intent(in) :: path
         real(kind=dp), intent(in) :: x(:,:)
         integer :: u, i, j

         open(newunit=u, file=trim(path), status="replace", action="write")
         do i = 1, size(x,1)
            do j = 1, size(x,2)
               if (j > 1) write(u, "(a)", advance="no") " "
               write(u, "(g0)", advance="no") x(i, j)
            end do
            write(u, *)
         end do
         close(u)
      end subroutine savetxt_real_2d

      pure integer function optval_int(x, default) result(v)
         integer, intent(in), optional :: x
         integer, intent(in) :: default
         if (present(x)) then
            v = x
         else
            v = default
         end if
      end function optval_int

      pure real(kind=dp) function optval_real(x, default) result(v)
         real(kind=dp), intent(in), optional :: x
         real(kind=dp), intent(in) :: default
         if (present(x)) then
            v = x
         else
            v = default
         end if
      end function optval_real

      pure logical function optval_logical(x, default) result(v)
         logical, intent(in), optional :: x
         logical, intent(in) :: default
         if (present(x)) then
            v = x
         else
            v = default
         end if
      end function optval_logical

      pure function optval_char(x, default) result(v)
         character(len=*), intent(in), optional :: x
         character(len=*), intent(in) :: default
         character(len=:), allocatable :: v
         if (present(x)) then
            v = x
         else
            v = default
         end if
      end function optval_char

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

      function runif0() result(x)
         real(kind=dp) :: x
         call random_number(x)
      end function runif0

      function runif1(n) result(x)
         integer, intent(in) :: n
         real(kind=dp), allocatable :: x(:)
         if (n < 0) stop "runif: n must be >= 0"
         allocate(x(n))
         if (n > 0) call random_number(x)
      end function runif1

      function runif2(n, m) result(x)
         integer, intent(in) :: n, m
         real(kind=dp), allocatable :: x(:,:)
         if (n < 0 .or. m < 0) stop "runif: n,m must be >= 0"
         allocate(x(n, m))
         if (n > 0 .and. m > 0) call random_number(x)
      end function runif2

      function rnorm0() result(x)
         real(kind=dp) :: x
         real(kind=dp) :: tmp(1)
         call random_normal_vec(tmp)
         x = tmp(1)
      end function rnorm0

      function rnorm1(n) result(x)
         integer, intent(in) :: n
         real(kind=dp), allocatable :: x(:)
         if (n < 0) stop "rnorm: n must be >= 0"
         allocate(x(n))
         if (n > 0) call random_normal_vec(x)
      end function rnorm1

      function rnorm2(n, m) result(x)
         integer, intent(in) :: n, m
         real(kind=dp), allocatable :: x(:,:)
         real(kind=dp), allocatable :: tmp(:)
         if (n < 0 .or. m < 0) stop "rnorm: n,m must be >= 0"
         allocate(x(n, m))
         if (n > 0 .and. m > 0) then
            allocate(tmp(n * m))
            call random_normal_vec(tmp)
            x = reshape(tmp, [n, m])
            deallocate(tmp)
         end if
      end function rnorm2

      real(kind=dp) function random_exponential(scale)
         real(kind=dp), intent(in), optional :: scale
         real(kind=dp) :: u, s
         s = 1.0_dp
         if (present(scale)) s = scale
         call random_number(u)
         if (u <= tiny(1.0_dp)) u = tiny(1.0_dp)
         random_exponential = -log(u) * s
      end function random_exponential

      subroutine random_exponential_vec(x, scale)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in), optional :: scale
         integer :: i
         do i = 1, size(x)
            if (present(scale)) then
               x(i) = random_exponential(scale)
            else
               x(i) = random_exponential()
            end if
         end do
      end subroutine random_exponential_vec

      recursive real(kind=dp) function random_gamma(shape, scale) result(g)
         real(kind=dp), intent(in) :: shape
         real(kind=dp), intent(in), optional :: scale
         real(kind=dp) :: sc, d, c, u, v, z
         real(kind=dp) :: tmpn(1)
         if (shape <= 0.0_dp) stop "random_gamma: shape must be > 0"
         sc = 1.0_dp
         if (present(scale)) sc = scale
         if (sc <= 0.0_dp) stop "random_gamma: scale must be > 0"
         if (shape < 1.0_dp) then
            call random_number(u)
            if (u <= tiny(1.0_dp)) u = tiny(1.0_dp)
            g = random_gamma(shape + 1.0_dp, sc) * u**(1.0_dp / shape)
            return
         end if
         d = shape - 1.0_dp / 3.0_dp
         c = 1.0_dp / sqrt(9.0_dp * d)
         do
            call random_normal_vec(tmpn)
            z = tmpn(1)
            v = (1.0_dp + c * z)**3
            if (v <= 0.0_dp) cycle
            call random_number(u)
            if (u <= tiny(1.0_dp)) u = tiny(1.0_dp)
            if (log(u) < 0.5_dp * z**2 + d - d * v + d * log(v)) then
               g = d * v * sc
               return
            end if
         end do
      end function random_gamma

      subroutine random_gamma_vec(x, shape, scale)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: shape
         real(kind=dp), intent(in), optional :: scale
         integer :: i
         do i = 1, size(x)
            if (present(scale)) then
               x(i) = random_gamma(shape, scale)
            else
               x(i) = random_gamma(shape)
            end if
         end do
      end subroutine random_gamma_vec

      real(kind=dp) function random_student_t(df)
         real(kind=dp), intent(in) :: df
         real(kind=dp) :: z(1), v
         if (df <= 0.0_dp) stop "random_student_t: df must be > 0"
         call random_normal_vec(z)
         v = random_gamma(df / 2.0_dp, 2.0_dp)
         random_student_t = z(1) / sqrt(v / df)
      end function random_student_t

      subroutine random_student_t_vec(x, df)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: df
         integer :: i
         do i = 1, size(x)
            x(i) = random_student_t(df)
         end do
      end subroutine random_student_t_vec

      real(kind=dp) function random_f(dfnum, dfden)
         real(kind=dp), intent(in) :: dfnum, dfden
         real(kind=dp) :: x1, x2
         if (dfnum <= 0.0_dp .or. dfden <= 0.0_dp) stop "random_f: dfs must be > 0"
         x1 = random_gamma(dfnum / 2.0_dp, 2.0_dp) / dfnum
         x2 = random_gamma(dfden / 2.0_dp, 2.0_dp) / dfden
         random_f = x1 / x2
      end function random_f

      subroutine random_f_vec(x, dfnum, dfden)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: dfnum, dfden
         integer :: i
         do i = 1, size(x)
            x(i) = random_f(dfnum, dfden)
         end do
      end subroutine random_f_vec

      integer function random_poisson(mu)
         real(kind=dp), intent(in) :: mu
         real(kind=dp) :: l, p, u
         integer :: k
         if (mu < 0.0_dp) stop "random_poisson: mu must be >= 0"
         l = exp(-mu)
         k = 0
         p = 1.0_dp
         do
            k = k + 1
            call random_number(u)
            p = p * u
            if (p <= l) exit
         end do
         random_poisson = k - 1
      end function random_poisson

      subroutine random_poisson_vec(x, mu)
         integer, intent(out) :: x(:)
         real(kind=dp), intent(in) :: mu
         integer :: i
         do i = 1, size(x)
            x(i) = random_poisson(mu)
         end do
      end subroutine random_poisson_vec

      integer function random_geometric(p)
         ! Source: John Burkardt, prob.f90, geometric_sample/geometric_cdf_inv.
         real(kind=dp), intent(in) :: p
         real(kind=dp) :: u
         if (p < 0.0_dp .or. p > 1.0_dp) stop "random_geometric: p must be in [0,1]"
         if (p == 1.0_dp) then
            random_geometric = 1
            return
         end if
         if (p == 0.0_dp) then
            random_geometric = huge(1)
            return
         end if
         call random_number(u)
         u = min(max(u, tiny(1.0_dp)), 1.0_dp - tiny(1.0_dp))
         random_geometric = 1 + int(log(1.0_dp - u) / log(1.0_dp - p))
      end function random_geometric

      subroutine random_geometric_vec(x, p)
         integer, intent(out) :: x(:)
         real(kind=dp), intent(in) :: p
         integer :: i
         do i = 1, size(x)
            x(i) = random_geometric(p)
         end do
      end subroutine random_geometric_vec

      integer function random_binomial(n, p)
         integer, intent(in) :: n
         real(kind=dp), intent(in) :: p
         integer :: i
         real(kind=dp) :: u
         if (n < 0) stop "random_binomial: n must be >= 0"
         if (p < 0.0_dp .or. p > 1.0_dp) stop "random_binomial: p must be in [0,1]"
         random_binomial = 0
         do i = 1, n
            call random_number(u)
            if (u < p) random_binomial = random_binomial + 1
         end do
      end function random_binomial

      subroutine random_binomial_vec(x, n, p)
         integer, intent(out) :: x(:)
         integer, intent(in) :: n
         real(kind=dp), intent(in) :: p
         integer :: i
         do i = 1, size(x)
            x(i) = random_binomial(n, p)
         end do
      end subroutine random_binomial_vec

      integer function random_hypergeometric(ngood, nbad, nsample)
         integer, intent(in) :: ngood, nbad, nsample
         integer :: gg, nn, i
         real(kind=dp) :: pr, u
         if (ngood < 0 .or. nbad < 0 .or. nsample < 0) stop "random_hypergeometric: args must be >= 0"
         if (nsample > ngood + nbad) stop "random_hypergeometric: nsample too large"
         gg = ngood
         nn = ngood + nbad
         random_hypergeometric = 0
         do i = 1, nsample
            if (gg <= 0) exit
            pr = real(gg, kind=dp) / real(nn, kind=dp)
            call random_number(u)
            if (u < pr) then
               random_hypergeometric = random_hypergeometric + 1
               gg = gg - 1
            end if
            nn = nn - 1
         end do
      end function random_hypergeometric

      subroutine random_hypergeometric_vec(x, ngood, nbad, nsample)
         integer, intent(out) :: x(:)
         integer, intent(in) :: ngood, nbad, nsample
         integer :: i
         do i = 1, size(x)
            x(i) = random_hypergeometric(ngood, nbad, nsample)
         end do
      end subroutine random_hypergeometric_vec

      integer function random_zipf(a)
         real(kind=dp), intent(in) :: a
         real(kind=dp) :: b, const, t, u, v
         real(kind=dp), parameter :: one = 1.0_dp
         if (a <= 1.0_dp) stop "random_zipf: a must be > 1"
         b = 2.0_dp**(a - one)
         const = -one / (a - one)
         do
            call random_number(u)
            call random_number(v)
            random_zipf = int(floor(u**const))
            if (random_zipf < 1) cycle
            t = (one + one / real(random_zipf, kind=dp))**(a - one)
            if (v * real(random_zipf, kind=dp) * (t - one) / (b - one) <= t / b) exit
         end do
      end function random_zipf

      subroutine random_zipf_vec(x, a)
         integer, intent(out) :: x(:)
         real(kind=dp), intent(in) :: a
         integer :: i
         do i = 1, size(x)
            x(i) = random_zipf(a)
         end do
      end subroutine random_zipf_vec

      real(kind=dp) function random_beta(a, b)
         real(kind=dp), intent(in) :: a, b
         real(kind=dp) :: x1, x2
         if (a <= 0.0_dp .or. b <= 0.0_dp) stop "random_beta: a,b must be > 0"
         x1 = random_gamma(a, 1.0_dp)
         x2 = random_gamma(b, 1.0_dp)
         random_beta = x1 / (x1 + x2)
      end function random_beta

      subroutine random_beta_vec(x, a, b)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: a, b
         integer :: i
         do i = 1, size(x)
            x(i) = random_beta(a, b)
         end do
      end subroutine random_beta_vec

      real(kind=dp) function random_lognormal(meanlog, sdlog)
         real(kind=dp), intent(in), optional :: meanlog, sdlog
         real(kind=dp) :: mu, sig, z(1)
         mu = 0.0_dp
         sig = 1.0_dp
         if (present(meanlog)) mu = meanlog
         if (present(sdlog)) sig = sdlog
         call random_normal_vec(z)
         random_lognormal = exp(mu + sig * z(1))
      end function random_lognormal

      subroutine random_lognormal_vec(x, meanlog, sdlog)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in), optional :: meanlog, sdlog
         integer :: i
         do i = 1, size(x)
            if (present(meanlog) .and. present(sdlog)) then
               x(i) = random_lognormal(meanlog, sdlog)
            elseif (present(meanlog)) then
               x(i) = random_lognormal(meanlog=meanlog)
            elseif (present(sdlog)) then
               x(i) = random_lognormal(sdlog=sdlog)
            else
               x(i) = random_lognormal()
            end if
         end do
      end subroutine random_lognormal_vec

      real(kind=dp) function random_chisquare(df)
         real(kind=dp), intent(in) :: df
         if (df <= 0.0_dp) stop "random_chisquare: df must be > 0"
         random_chisquare = random_gamma(df / 2.0_dp, 2.0_dp)
      end function random_chisquare

      subroutine random_chisquare_vec(x, df)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: df
         integer :: i
         do i = 1, size(x)
            x(i) = random_chisquare(df)
         end do
      end subroutine random_chisquare_vec

      real(kind=dp) function random_laplace(loc, scale)
         real(kind=dp), intent(in), optional :: loc, scale
         real(kind=dp) :: u, mu, b
         mu = 0.0_dp
         b = 1.0_dp
         if (present(loc)) mu = loc
         if (present(scale)) b = scale
         if (b <= 0.0_dp) stop "random_laplace: scale must be > 0"
         call random_number(u)
         if (u < 0.5_dp) then
            random_laplace = mu + b * log(max(2.0_dp * u, tiny(1.0_dp)))
         else
            random_laplace = mu - b * log(max(2.0_dp * (1.0_dp - u), tiny(1.0_dp)))
         end if
      end function random_laplace

      subroutine random_laplace_vec(x, loc, scale)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in), optional :: loc, scale
         integer :: i
         do i = 1, size(x)
            if (present(loc) .and. present(scale)) then
               x(i) = random_laplace(loc, scale)
            elseif (present(loc)) then
               x(i) = random_laplace(loc=loc)
            elseif (present(scale)) then
               x(i) = random_laplace(scale=scale)
            else
               x(i) = random_laplace()
            end if
         end do
      end subroutine random_laplace_vec

      real(kind=dp) function random_logistic(loc, scale)
         real(kind=dp), intent(in), optional :: loc, scale
         real(kind=dp) :: u, mu, s
         mu = 0.0_dp
         s = 1.0_dp
         if (present(loc)) mu = loc
         if (present(scale)) s = scale
         if (s <= 0.0_dp) stop "random_logistic: scale must be > 0"
         call random_number(u)
         u = min(max(u, tiny(1.0_dp)), 1.0_dp - tiny(1.0_dp))
         random_logistic = mu + s * log(u / (1.0_dp - u))
      end function random_logistic

      subroutine random_logistic_vec(x, loc, scale)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in), optional :: loc, scale
         integer :: i
         do i = 1, size(x)
            if (present(loc) .and. present(scale)) then
               x(i) = random_logistic(loc, scale)
            elseif (present(loc)) then
               x(i) = random_logistic(loc=loc)
            elseif (present(scale)) then
               x(i) = random_logistic(scale=scale)
            else
               x(i) = random_logistic()
            end if
         end do
      end subroutine random_logistic_vec

      real(kind=dp) function random_cauchy(loc, scale)
         real(kind=dp), intent(in), optional :: loc, scale
         real(kind=dp) :: u, mu, s
         mu = 0.0_dp
         s = 1.0_dp
         if (present(loc)) mu = loc
         if (present(scale)) s = scale
         if (s <= 0.0_dp) stop "random_cauchy: scale must be > 0"
         call random_number(u)
         random_cauchy = mu + s * tan(acos(-1.0_dp) * (u - 0.5_dp))
      end function random_cauchy

      subroutine random_cauchy_vec(x, loc, scale)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in), optional :: loc, scale
         integer :: i
         do i = 1, size(x)
            if (present(loc) .and. present(scale)) then
               x(i) = random_cauchy(loc, scale)
            elseif (present(loc)) then
               x(i) = random_cauchy(loc=loc)
            elseif (present(scale)) then
               x(i) = random_cauchy(scale=scale)
            else
               x(i) = random_cauchy()
            end if
         end do
      end subroutine random_cauchy_vec

      real(kind=dp) function random_weibull(a)
         ! Source: Alan Miller, amiller_mirror/random.f90 (Weibull sampler).
         real(kind=dp), intent(in) :: a
         if (a <= 0.0_dp) stop "random_weibull: a must be > 0"
         random_weibull = random_exponential()**(1.0_dp / a)
      end function random_weibull

      subroutine random_weibull_vec(x, a)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: a
         integer :: i
         do i = 1, size(x)
            x(i) = random_weibull(a)
         end do
      end subroutine random_weibull_vec

      integer function random_neg_binomial(sk, p)
         ! Source: Alan Miller, amiller_mirror/random.f90 (negative binomial sampler).
         real(kind=dp), intent(in) :: sk, p
         real(kind=dp), parameter :: h = 0.7_dp
         real(kind=dp) :: p_alg, q, x, st, uln, v, r, s, y, g
         integer :: k, i, n
         if (sk <= 0.0_dp .or. p <= 0.0_dp .or. p >= 1.0_dp) stop "random_neg_binomial: invalid params"
         ! NumPy parameterization: p is success probability.
         ! Alan Miller routine expects the opposite probability in its recurrence.
         p_alg = 1.0_dp - p
         q = 1.0_dp - p_alg
         x = 0.0_dp
         st = sk
         if (p_alg > h) then
            v = 1.0_dp / log(p_alg)
            k = int(st)
            do i = 1, k
               do
                  call random_number(r)
                  if (r > 0.0_dp) exit
               end do
               n = int(v * log(r))
               x = x + real(n, kind=dp)
            end do
            st = st - real(k, kind=dp)
         end if
         s = 0.0_dp
         uln = -log(tiny(1.0_dp))
         if (st > -uln / log(q)) stop "random_neg_binomial: p too large for sk"
         y = q**st
         g = st
         call random_number(r)
         do
            if (y > r) exit
            r = r - y
            s = s + 1.0_dp
            y = y * p_alg * g / s
            g = g + 1.0_dp
         end do
         random_neg_binomial = int(x + s + 0.5_dp)
      end function random_neg_binomial

      subroutine random_neg_binomial_vec(x, sk, p)
         integer, intent(out) :: x(:)
         real(kind=dp), intent(in) :: sk, p
         integer :: i
         do i = 1, size(x)
            x(i) = random_neg_binomial(sk, p)
         end do
      end subroutine random_neg_binomial_vec

      real(kind=dp) function random_von_mises(kappa)
         ! Source: Alan Miller, amiller_mirror/random.f90 (von Mises sampler).
         real(kind=dp), intent(in) :: kappa
         integer :: j, n, nk
         real(kind=dp) :: sump, r, th, lambda, rlast
         real(kind=dp), save :: p(20), theta(0:20)
         real(kind=dp), save :: last_kappa = -1.0_dp
         logical, save :: initialized = .false.
         if (kappa < 0.0_dp) stop "random_von_mises: kappa must be >= 0"
         if ((.not. initialized) .or. abs(kappa - last_kappa) > 0.0_dp) then
            nk = int(kappa + kappa + 1.0_dp)
            if (nk > 20) stop "random_von_mises: kappa too large for table"
            theta(0) = 0.0_dp
            if (kappa > 0.5_dp) then
               sump = 0.0_dp
               do j = 1, nk
                  if (j < nk) then
                     theta(j) = acos(1.0_dp - real(j, kind=dp) / kappa)
                  else
                     theta(nk) = acos(-1.0_dp)
                  end if
                  call vm_integral(theta(j-1), theta(j), p(j), kappa)
                  sump = sump + p(j)
               end do
               p(1:nk) = p(1:nk) / sump
            else
               p(1) = 1.0_dp
               theta(1) = acos(-1.0_dp)
               nk = 1
            end if
            last_kappa = kappa
            initialized = .true.
         else
            nk = int(kappa + kappa + 1.0_dp)
            if (kappa <= 0.5_dp) nk = 1
         end if
         call random_number(r)
         do j = 1, nk
            r = r - p(j)
            if (r < 0.0_dp) exit
         end do
         r = -r / p(j)
         do
            th = theta(j-1) + r * (theta(j) - theta(j-1))
            lambda = kappa - real(j, kind=dp) + 1.0_dp - kappa * cos(th)
            n = 1
            rlast = lambda
            do
               call random_number(r)
               if (r > rlast) exit
               n = n + 1
               rlast = r
            end do
            if (mod(n, 2) /= 0) exit
            call random_number(r)
         end do
         random_von_mises = sign(th, (r - rlast) / (1.0_dp - rlast) - 0.5_dp)
      end function random_von_mises

      subroutine random_von_mises_vec(x, kappa)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: kappa
         integer :: i
         do i = 1, size(x)
            x(i) = random_von_mises(kappa)
         end do
      end subroutine random_von_mises_vec

      real(kind=dp) function random_pareto(a)
         ! Source: John Burkardt, prob.f90, pareto_sample/pareto_cdf_inv.
         ! NumPy convention: support x >= 0 with shape a.
         real(kind=dp), intent(in) :: a
         real(kind=dp) :: u
         if (a <= 0.0_dp) stop "random_pareto: a must be > 0"
         call random_number(u)
         u = min(max(u, tiny(1.0_dp)), 1.0_dp - tiny(1.0_dp))
         random_pareto = (1.0_dp - u)**(-1.0_dp / a) - 1.0_dp
      end function random_pareto

      subroutine random_pareto_vec(x, a)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: a
         integer :: i
         do i = 1, size(x)
            x(i) = random_pareto(a)
         end do
      end subroutine random_pareto_vec

      real(kind=dp) function random_power(a)
         ! Source: John Burkardt, prob.f90, power_sample/power_cdf_inv.
         ! NumPy convention on [0,1], shape a.
         real(kind=dp), intent(in) :: a
         real(kind=dp) :: u
         if (a <= 0.0_dp) stop "random_power: a must be > 0"
         call random_number(u)
         u = min(max(u, tiny(1.0_dp)), 1.0_dp - tiny(1.0_dp))
         random_power = u**(1.0_dp / a)
      end function random_power

      subroutine random_power_vec(x, a)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: a
         integer :: i
         do i = 1, size(x)
            x(i) = random_power(a)
         end do
      end subroutine random_power_vec

      real(kind=dp) function random_rayleigh(scale)
         ! Source: John Burkardt, prob.f90, rayleigh_sample/rayleigh_cdf_inv.
         real(kind=dp), intent(in), optional :: scale
         real(kind=dp) :: s, u
         s = 1.0_dp
         if (present(scale)) s = scale
         if (s <= 0.0_dp) stop "random_rayleigh: scale must be > 0"
         call random_number(u)
         u = min(max(u, tiny(1.0_dp)), 1.0_dp - tiny(1.0_dp))
         random_rayleigh = s * sqrt(-2.0_dp * log(1.0_dp - u))
      end function random_rayleigh

      subroutine random_rayleigh_vec(x, scale)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in), optional :: scale
         integer :: i
         do i = 1, size(x)
            if (present(scale)) then
               x(i) = random_rayleigh(scale)
            else
               x(i) = random_rayleigh()
            end if
         end do
      end subroutine random_rayleigh_vec

      real(kind=dp) function random_gumbel(loc, scale)
         ! Source: John Burkardt, prob.f90, gumbel_sample/gumbel_cdf_inv.
         real(kind=dp), intent(in), optional :: loc, scale
         real(kind=dp) :: mu, b, u
         mu = 0.0_dp
         b = 1.0_dp
         if (present(loc)) mu = loc
         if (present(scale)) b = scale
         if (b <= 0.0_dp) stop "random_gumbel: scale must be > 0"
         call random_number(u)
         u = min(max(u, tiny(1.0_dp)), 1.0_dp - tiny(1.0_dp))
         random_gumbel = mu - b * log(-log(u))
      end function random_gumbel

      subroutine random_gumbel_vec(x, loc, scale)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in), optional :: loc, scale
         integer :: i
         do i = 1, size(x)
            if (present(loc) .and. present(scale)) then
               x(i) = random_gumbel(loc, scale)
            elseif (present(loc)) then
               x(i) = random_gumbel(loc=loc)
            elseif (present(scale)) then
               x(i) = random_gumbel(scale=scale)
            else
               x(i) = random_gumbel()
            end if
         end do
      end subroutine random_gumbel_vec

      real(kind=dp) function random_wald(mean, scale)
         ! Source: John Burkardt, prob.f90, inverse_gaussian_sample.
         real(kind=dp), intent(in) :: mean, scale
         real(kind=dp) :: phi, z(1), y, t, u
         if (mean <= 0.0_dp .or. scale <= 0.0_dp) stop "random_wald: mean,scale must be > 0"
         phi = scale / mean
         call random_normal_vec(z)
         y = z(1) * z(1)
         t = 1.0_dp + 0.5_dp * (y - sqrt(4.0_dp * phi * y + y * y)) / phi
         call random_number(u)
         if (u * (1.0_dp + t) <= 1.0_dp) then
            random_wald = mean * t
         else
            random_wald = mean / t
         end if
      end function random_wald

      subroutine random_wald_vec(x, mean, scale)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: mean, scale
         integer :: i
         do i = 1, size(x)
            x(i) = random_wald(mean, scale)
         end do
      end subroutine random_wald_vec

      real(kind=dp) function random_noncentral_chisquare(df, nonc)
         ! Source: John Burkardt, prob.f90, chi_square_noncentral_sample.
         real(kind=dp), intent(in) :: df, nonc
         real(kind=dp) :: x1, x2n(1), mu
         if (df < 1.0_dp .or. nonc < 0.0_dp) stop "random_noncentral_chisquare: invalid parameters"
         x1 = random_chisquare(df - 1.0_dp)
         call random_normal_vec(x2n)
         mu = sqrt(max(nonc, 0.0_dp))
         random_noncentral_chisquare = x1 + (mu + x2n(1))**2
      end function random_noncentral_chisquare

      subroutine random_noncentral_chisquare_vec(x, df, nonc)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: df, nonc
         integer :: i
         do i = 1, size(x)
            x(i) = random_noncentral_chisquare(df, nonc)
         end do
      end subroutine random_noncentral_chisquare_vec

      real(kind=dp) function random_noncentral_f(dfnum, dfden, nonc)
         real(kind=dp), intent(in) :: dfnum, dfden, nonc
         real(kind=dp) :: xn, xd
         if (dfnum <= 0.0_dp .or. dfden <= 0.0_dp .or. nonc < 0.0_dp) stop "random_noncentral_f: invalid parameters"
         xn = random_noncentral_chisquare(dfnum, nonc) / dfnum
         xd = random_chisquare(dfden) / dfden
         random_noncentral_f = xn / xd
      end function random_noncentral_f

      subroutine random_noncentral_f_vec(x, dfnum, dfden, nonc)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: dfnum, dfden, nonc
         integer :: i
         do i = 1, size(x)
            x(i) = random_noncentral_f(dfnum, dfden, nonc)
         end do
      end subroutine random_noncentral_f_vec

      real(kind=dp) function random_triangular(left, mode, right)
         ! Source: John Burkardt, prob.f90, triangle_sample/triangle_cdf_inv.
         real(kind=dp), intent(in) :: left, mode, right
         real(kind=dp) :: u, cut
         if (left > mode .or. mode > right .or. left == right) stop "random_triangular: require left<=mode<=right and left<right"
         call random_number(u)
         u = min(max(u, tiny(1.0_dp)), 1.0_dp - tiny(1.0_dp))
         cut = (mode - left) / (right - left)
         if (u <= cut) then
            random_triangular = left + sqrt(u * (right - left) * (mode - left))
         else
            random_triangular = right - sqrt((1.0_dp - u) * (right - left) * (right - mode))
         end if
      end function random_triangular

      subroutine random_triangular_vec(x, left, mode, right)
         real(kind=dp), intent(out) :: x(:)
         real(kind=dp), intent(in) :: left, mode, right
         integer :: i
         do i = 1, size(x)
            x(i) = random_triangular(left, mode, right)
         end do
      end subroutine random_triangular_vec

      integer function random_logseries(p)
         ! Source: John Burkardt, prob.f90, log_series_sample.
         real(kind=dp), intent(in) :: p
         real(kind=dp) :: u, v
         if (p <= 0.0_dp .or. p >= 1.0_dp) stop "random_logseries: p must be in (0,1)"
         call random_number(u)
         call random_number(v)
         u = min(max(u, tiny(1.0_dp)), 1.0_dp - tiny(1.0_dp))
         v = min(max(v, tiny(1.0_dp)), 1.0_dp - tiny(1.0_dp))
         random_logseries = int(1.0_dp + log(v) / log(1.0_dp - (1.0_dp - p)**u))
      end function random_logseries

      subroutine random_logseries_vec(x, p)
         integer, intent(out) :: x(:)
         real(kind=dp), intent(in) :: p
         integer :: i
         do i = 1, size(x)
            x(i) = random_logseries(p)
         end do
      end subroutine random_logseries_vec

      subroutine random_dirichlet(alpha, x)
         ! Source: John Burkardt, prob.f90, dirichlet_sample.
         real(kind=dp), intent(in) :: alpha(:)
         real(kind=dp), intent(out) :: x(:)
         integer :: i, n
         real(kind=dp) :: s
         n = size(alpha)
         if (size(x) /= n) stop "random_dirichlet: size mismatch"
         do i = 1, n
            if (alpha(i) <= 0.0_dp) stop "random_dirichlet: alpha entries must be > 0"
            x(i) = random_gamma(alpha(i), 1.0_dp)
         end do
         s = sum(x)
         if (s <= tiny(1.0_dp)) stop "random_dirichlet: invalid sample sum"
         x = x / s
      end subroutine random_dirichlet

      subroutine random_dirichlet_samples(alpha, x)
         real(kind=dp), intent(in) :: alpha(:)
         real(kind=dp), intent(out) :: x(:,:)
         integer :: i
         if (size(x, 2) /= size(alpha)) stop "random_dirichlet_samples: second dim must equal len(alpha)"
         do i = 1, size(x, 1)
            call random_dirichlet(alpha, x(i, :))
         end do
      end subroutine random_dirichlet_samples

      subroutine random_multinomial(n, p, x)
         ! Source: John Burkardt, prob.f90, multinomial_sample.
         integer, intent(in) :: n
         real(kind=dp), intent(in) :: p(:)
         integer, intent(out) :: x(:)
         integer :: b, ifactor, ntot
         real(kind=dp), allocatable :: q(:)
         real(kind=dp) :: sum2, prob
         b = size(p)
         if (size(x) /= b) stop "random_multinomial: size mismatch"
         if (n < 0) stop "random_multinomial: n must be >= 0"
         allocate(q(1:b))
         q = max(p, 0.0_dp)
         sum2 = sum(q)
         if (sum2 <= tiny(1.0_dp)) stop "random_multinomial: probabilities must sum to > 0"
         q = q / sum2
         x = 0
         ntot = n
         sum2 = 1.0_dp
         do ifactor = 1, b - 1
            if (ntot <= 0) exit
            prob = q(ifactor) / sum2
            prob = min(max(prob, 0.0_dp), 1.0_dp)
            x(ifactor) = random_binomial(ntot, prob)
            ntot = ntot - x(ifactor)
            sum2 = sum2 - q(ifactor)
            if (sum2 <= tiny(1.0_dp)) exit
         end do
         x(b) = ntot
       end subroutine random_multinomial

      subroutine random_multinomial_samples(n, p, x)
         integer, intent(in) :: n
         real(kind=dp), intent(in) :: p(:)
         integer, intent(out) :: x(:,:)
         integer :: i
         if (size(x, 2) /= size(p)) stop "random_multinomial_samples: second dim must equal len(p)"
         do i = 1, size(x, 1)
            call random_multinomial(n, p, x(i, :))
         end do
      end subroutine random_multinomial_samples

      subroutine random_multivariate_hypergeometric(ngood, nsample, x)
         ! Source: reduction to sequential univariate hypergeometric draws.
         integer, intent(in) :: ngood(:)
         integer, intent(in) :: nsample
         integer, intent(out) :: x(:)
         integer :: i, k, rem_draw, rem_total, rem_bad
         k = size(ngood)
         if (size(x) /= k) stop "random_multivariate_hypergeometric: size mismatch"
         if (k <= 0) stop "random_multivariate_hypergeometric: ngood must be nonempty"
         if (any(ngood < 0)) stop "random_multivariate_hypergeometric: ngood entries must be >= 0"
         rem_total = sum(ngood)
         if (nsample < 0 .or. nsample > rem_total) stop "random_multivariate_hypergeometric: invalid nsample"
         rem_draw = nsample
         x = 0
         do i = 1, k - 1
            rem_bad = rem_total - ngood(i)
            if (rem_draw <= 0) then
               x(i) = 0
            else
               x(i) = random_hypergeometric(ngood(i), rem_bad, rem_draw)
            end if
            rem_draw = rem_draw - x(i)
            rem_total = rem_total - ngood(i)
         end do
         x(k) = rem_draw
      end subroutine random_multivariate_hypergeometric

      subroutine random_multivariate_hypergeometric_samples(ngood, nsample, x)
         integer, intent(in) :: ngood(:)
         integer, intent(in) :: nsample
         integer, intent(out) :: x(:,:)
         integer :: i
         if (size(x,2) /= size(ngood)) stop "random_multivariate_hypergeometric_samples: second dim mismatch"
         do i = 1, size(x,1)
            call random_multivariate_hypergeometric(ngood, nsample, x(i,:))
         end do
      end subroutine random_multivariate_hypergeometric_samples

      subroutine vm_integral(a, b, result, dk)
         ! Source: Alan Miller, amiller_mirror/random.f90 (quadrature helper for von Mises).
         real(kind=dp), intent(in) :: a, b, dk
         real(kind=dp), intent(out) :: result
         real(kind=dp) :: xmid, range, x1, x2
         real(kind=dp), parameter :: x(3) = (/ &
            0.238619186083197_dp, 0.661209386466265_dp, 0.932469514203152_dp /)
         real(kind=dp), parameter :: w(3) = (/ &
            0.467913934572691_dp, 0.360761573048139_dp, 0.171324492379170_dp /)
         integer :: i
         xmid = 0.5_dp * (a + b)
         range = 0.5_dp * (b - a)
         result = 0.0_dp
         do i = 1, 3
            x1 = xmid + range * x(i)
            x2 = xmid - range * x(i)
            result = result + w(i) * (exp(dk * cos(x1)) + exp(dk * cos(x2)))
         end do
         result = result * range
      end subroutine vm_integral

      subroutine random_mvn_samples(mean, cov, x)
         real(kind=dp), intent(in) :: mean(:)
         real(kind=dp), intent(in) :: cov(:,:)
         real(kind=dp), intent(out) :: x(:,:)
         integer :: n, p, i, j, info
         real(kind=dp), allocatable :: l(:,:), z(:,:), y(:,:)
         interface
            subroutine dpotrf(uplo, n, a, lda, info)
               character(len=1), intent(in) :: uplo
               integer, intent(in) :: n, lda
               double precision, intent(inout) :: a(lda,*)
               integer, intent(out) :: info
            end subroutine dpotrf
         end interface
         n = size(x, 1)
         p = size(mean)
         if (size(x, 2) /= p) stop "random_mvn_samples: output shape mismatch"
         if (size(cov, 1) /= p .or. size(cov, 2) /= p) stop "random_mvn_samples: covariance shape mismatch"
         allocate(l(p, p))
         l = cov
         call dpotrf('L', p, l, p, info)
         if (info /= 0) stop "random_mvn_samples: covariance not positive definite"
         do i = 1, p
            do j = i + 1, p
               l(i, j) = 0.0_dp
            end do
         end do
         allocate(z(p, n), y(p, n))
         do i = 1, n
            call random_normal_vec(z(:, i))
         end do
         y = matmul(l, z)
         do i = 1, n
            x(i, :) = mean + y(:, i)
         end do
      end subroutine random_mvn_samples

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

      function setdiff1d_char(a, b) result(c)
         character(len=*), intent(in) :: a(:), b(:)
         character(len=:), allocatable :: c(:)
         character(len=:), allocatable :: ua(:), ub(:), tmp(:)
         integer :: i, j, nout, llen
         ua = unique_char(a)
         ub = unique_char(b)
         llen = max(len(a), len(b))
         if (llen < 1) llen = 1
         allocate(character(len=llen) :: tmp(size(ua)))
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
         allocate(character(len=llen) :: c(nout))
         if (nout > 0) c = tmp(1:nout)
      end function setdiff1d_char

      function intersect1d_char(a, b) result(c)
         character(len=*), intent(in) :: a(:), b(:)
         character(len=:), allocatable :: c(:)
         character(len=:), allocatable :: ua(:), ub(:), tmp(:)
         integer :: i, j, nout, llen
         ua = unique_char(a)
         ub = unique_char(b)
         llen = max(len(a), len(b))
         if (llen < 1) llen = 1
         allocate(character(len=llen) :: tmp(min(size(ua), size(ub))))
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
         allocate(character(len=llen) :: c(nout))
         if (nout > 0) c = tmp(1:nout)
      end function intersect1d_char

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

      function unique_char(x) result(y)
         character(len=*), intent(in) :: x(:)
         character(len=:), allocatable :: y(:)
         integer :: i, j, n, m, llen
         logical :: seen
         n = size(x)
         llen = len(x)
         if (llen < 1) llen = 1
         m = 0
         do i = 1, n
            seen = .false.
            do j = 1, i - 1
               if (x(i) == x(j)) then
                  seen = .true.
                  exit
               end if
            end do
            if (.not. seen) m = m + 1
         end do
         allocate(character(len=llen) :: y(m))
         m = 0
         do i = 1, n
            seen = .false.
            do j = 1, i - 1
               if (x(i) == x(j)) then
                  seen = .true.
                  exit
               end if
            end do
            if (.not. seen) then
               m = m + 1
               y(m) = x(i)
            end if
         end do
      end function unique_char

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

      function linalg_cholesky(a) result(l)
         real(kind=dp), intent(in) :: a(:,:)
         real(kind=dp), allocatable :: l(:,:)
         integer :: n, i, j, info
         interface
            subroutine dpotrf(uplo, n, a, lda, info)
               character(len=1), intent(in) :: uplo
               integer, intent(in) :: n, lda
               double precision, intent(inout) :: a(lda,*)
               integer, intent(out) :: info
            end subroutine dpotrf
         end interface
         n = size(a,1)
         if (size(a,2) /= n) stop "linalg_cholesky: matrix must be square"
         allocate(l(1:n,1:n), source=a)
         call dpotrf('L', n, l, n, info)
         if (info < 0) stop "linalg_cholesky: dpotrf argument error"
         if (info > 0) stop "linalg_cholesky: matrix is not positive definite"
         do i = 1, n
            do j = i + 1, n
               l(i,j) = 0.0_dp
            end do
         end do
      end function linalg_cholesky

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

      subroutine linalg_eigh(a, w, v)
         real(kind=dp), intent(in) :: a(:,:)
         real(kind=dp), allocatable, intent(out) :: w(:), v(:,:)
         real(kind=dp), allocatable :: ac(:,:), work(:)
         integer :: n, info, lwork
         interface
            subroutine dsyev(jobz, uplo, n, a, lda, w, work, lwork, info)
               character(len=1), intent(in) :: jobz, uplo
               integer, intent(in) :: n, lda, lwork
               integer, intent(out) :: info
               double precision, intent(inout) :: a(lda,*), work(*)
               double precision, intent(out) :: w(*)
            end subroutine dsyev
         end interface
         n = size(a,1)
         if (size(a,2) /= n) stop "linalg_eigh: matrix must be square"
         allocate(ac(1:n,1:n), source=a)
         allocate(w(1:n))
         allocate(work(1))
         lwork = -1
         call dsyev('V', 'U', n, ac, n, w, work, lwork, info)
         if (info /= 0) stop "linalg_eigh: dsyev workspace query failed"
         lwork = max(1, int(work(1)))
         deallocate(work)
         allocate(work(1:lwork))
         call dsyev('V', 'U', n, ac, n, w, work, lwork, info)
         if (info /= 0) stop "linalg_eigh: dsyev failed"
         allocate(v(1:n,1:n), source=ac)
      end subroutine linalg_eigh

      subroutine linalg_qr_reduced(a, q, r)
         real(kind=dp), intent(in) :: a(:,:)
         real(kind=dp), allocatable, intent(out) :: q(:,:), r(:,:)
         real(kind=dp), allocatable :: ac(:,:), tau(:), work(:)
         integer :: m, n, k, info, lwork, i_r, j_r
         interface
            subroutine dgeqrf(m, n, a, lda, tau, work, lwork, info)
               integer, intent(in) :: m, n, lda, lwork
               integer, intent(out) :: info
               double precision, intent(inout) :: a(lda,*), tau(*), work(*)
            end subroutine dgeqrf
            subroutine dorgqr(m, n, k, a, lda, tau, work, lwork, info)
               integer, intent(in) :: m, n, k, lda, lwork
               integer, intent(out) :: info
               double precision, intent(inout) :: a(lda,*), tau(*), work(*)
            end subroutine dorgqr
         end interface
         m = size(a,1)
         n = size(a,2)
         k = min(m, n)
         allocate(ac(1:m,1:n), source=a)
         allocate(tau(1:k))
         allocate(work(1))
         lwork = -1
         call dgeqrf(m, n, ac, m, tau, work, lwork, info)
         if (info /= 0) stop "linalg_qr_reduced: dgeqrf workspace query failed"
         lwork = max(1, int(work(1)))
         deallocate(work)
         allocate(work(1:lwork))
         call dgeqrf(m, n, ac, m, tau, work, lwork, info)
         if (info /= 0) stop "linalg_qr_reduced: dgeqrf failed"
         allocate(r(1:k,1:n), source=0.0_dp)
         do i_r = 1, k
            do j_r = i_r, n
               r(i_r,j_r) = ac(i_r,j_r)
            end do
         end do
         if (k < n) ac(:,k+1:n) = 0.0_dp
         deallocate(work)
         allocate(work(1))
         lwork = -1
         call dorgqr(m, k, k, ac, m, tau, work, lwork, info)
         if (info /= 0) stop "linalg_qr_reduced: dorgqr workspace query failed"
         lwork = max(1, int(work(1)))
         deallocate(work)
         allocate(work(1:lwork))
         call dorgqr(m, k, k, ac, m, tau, work, lwork, info)
         if (info /= 0) stop "linalg_qr_reduced: dorgqr failed"
         allocate(q(1:m,1:k), source=ac(:,1:k))
      end subroutine linalg_qr_reduced

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

      real(kind=dp) function quantile_linear(x, q)
         real(kind=dp), intent(in) :: x(:)
         real(kind=dp), intent(in) :: q
         real(kind=dp), allocatable :: xs(:)
         real(kind=dp) :: qq, pos, frac
         integer :: n, lo, hi
         n = size(x)
         if (n <= 0) then
            quantile_linear = 0.0_dp
            return
         end if
         allocate(xs(1:n), source=x)
         call sort_vec(xs)
         qq = max(0.0_dp, min(1.0_dp, q))
         if (n == 1) then
            quantile_linear = xs(1)
            return
         end if
         pos = qq * real(n - 1, kind=dp) + 1.0_dp
         lo = int(floor(pos))
         hi = min(n, lo + 1)
         frac = pos - real(lo, kind=dp)
         quantile_linear = (1.0_dp - frac) * xs(lo) + frac * xs(hi)
      end function quantile_linear

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

      subroutine unique_int_counts(a, u, cnt)
         integer, intent(in) :: a(:)
         integer, allocatable, intent(out) :: u(:), cnt(:)
         integer :: i, j
         u = unique_int(a)
         allocate(cnt(size(u)))
         cnt = 0
         do i = 1, size(a)
            do j = 1, size(u)
               if (a(i) == u(j)) then
                  cnt(j) = cnt(j) + 1
                  exit
               end if
            end do
         end do
      end subroutine unique_int_counts

      function tri_int(n, m, k) result(t)
         integer, intent(in) :: n, m
         integer, intent(in), optional :: k
         integer, allocatable :: t(:,:)
         integer :: i, j, kk
         kk = 0
         if (present(k)) kk = k
         allocate(t(n, m))
         t = 0
         do i = 1, n
            do j = 1, m
               if (j <= i + kk) t(i, j) = 1
            end do
         end do
      end function tri_int

      function tri_real(n, m, k) result(t)
         integer, intent(in) :: n, m
         integer, intent(in), optional :: k
         real(kind=dp), allocatable :: t(:,:)
         integer :: i, j, kk
         kk = 0
         if (present(k)) kk = k
         allocate(t(n, m))
         t = 0.0_dp
         do i = 1, n
            do j = 1, m
               if (j <= i + kk) t(i, j) = 1.0_dp
            end do
         end do
      end function tri_real

      function moveaxis3_int(a, src, dst) result(b)
         integer, intent(in) :: a(:,:,:)
         integer, intent(in) :: src, dst
         integer, allocatable :: b(:,:,:)
         integer :: s, d
         s = src; d = dst
         if (s < 0) s = s + 3
         if (d < 0) d = d + 3
         if (s == 0 .and. d == 2) then
            allocate(b(size(a,2), size(a,3), size(a,1)))
            b = reshape(a, [size(a,2), size(a,3), size(a,1)], order=[2,3,1])
         elseif (s == 2 .and. d == 0) then
            allocate(b(size(a,3), size(a,1), size(a,2)))
            b = reshape(a, [size(a,3), size(a,1), size(a,2)], order=[3,1,2])
         else
            allocate(b(size(a,1), size(a,2), size(a,3)))
            b = a
         end if
      end function moveaxis3_int

      function moveaxis3_real(a, src, dst) result(b)
         real(kind=dp), intent(in) :: a(:,:,:)
         integer, intent(in) :: src, dst
         real(kind=dp), allocatable :: b(:,:,:)
         integer :: s, d
         s = src; d = dst
         if (s < 0) s = s + 3
         if (d < 0) d = d + 3
         if (s == 0 .and. d == 2) then
            allocate(b(size(a,2), size(a,3), size(a,1)))
            b = reshape(a, [size(a,2), size(a,3), size(a,1)], order=[2,3,1])
         elseif (s == 2 .and. d == 0) then
            allocate(b(size(a,3), size(a,1), size(a,2)))
            b = reshape(a, [size(a,3), size(a,1), size(a,2)], order=[3,1,2])
         else
            allocate(b(size(a,1), size(a,2), size(a,3)))
            b = a
         end if
      end function moveaxis3_real

      function moveaxis3_logical(a, src, dst) result(b)
         logical, intent(in) :: a(:,:,:)
         integer, intent(in) :: src, dst
         logical, allocatable :: b(:,:,:)
         integer :: s, d
         s = src; d = dst
         if (s < 0) s = s + 3
         if (d < 0) d = d + 3
         if (s == 0 .and. d == 2) then
            allocate(b(size(a,2), size(a,3), size(a,1)))
            b = reshape(a, [size(a,2), size(a,3), size(a,1)], order=[2,3,1])
         elseif (s == 2 .and. d == 0) then
            allocate(b(size(a,3), size(a,1), size(a,2)))
            b = reshape(a, [size(a,3), size(a,1), size(a,2)], order=[3,1,2])
         else
            allocate(b(size(a,1), size(a,2), size(a,3)))
            b = a
         end if
      end function moveaxis3_logical

      function pad2d_int(a, pt, pb, pl, pr, c) result(out)
         integer, intent(in) :: a(:,:)
         integer, intent(in) :: pt, pb, pl, pr, c
         integer, allocatable :: out(:,:)
         integer :: n0, n1
         n0 = size(a,1); n1 = size(a,2)
         allocate(out(n0 + pt + pb, n1 + pl + pr))
         out = c
         out(pt+1:pt+n0, pl+1:pl+n1) = a
      end function pad2d_int

      function pad2d_real(a, pt, pb, pl, pr, c) result(out)
         real(kind=dp), intent(in) :: a(:,:)
         integer, intent(in) :: pt, pb, pl, pr
         real(kind=dp), intent(in) :: c
         real(kind=dp), allocatable :: out(:,:)
         integer :: n0, n1
         n0 = size(a,1); n1 = size(a,2)
         allocate(out(n0 + pt + pb, n1 + pl + pr))
         out = c
         out(pt+1:pt+n0, pl+1:pl+n1) = a
      end function pad2d_real

      logical function allclose_real(a, b, rtol, atol, equal_nan)
         real(kind=dp), intent(in) :: a(:), b(:)
         real(kind=dp), intent(in), optional :: rtol, atol
         logical, intent(in), optional :: equal_nan
         real(kind=dp) :: rtolv, atolv
         logical :: eqnan
         integer :: i
         rtolv = 1.0e-5_dp
         atolv = 1.0e-8_dp
         eqnan = .false.
         if (present(rtol)) rtolv = rtol
         if (present(atol)) atolv = atol
         if (present(equal_nan)) eqnan = equal_nan
         if (size(a) /= size(b)) then
            allclose_real = .false.
            return
         end if
         allclose_real = .true.
         do i = 1, size(a)
            if (ieee_is_nan(a(i)) .or. ieee_is_nan(b(i))) then
               if (.not. (eqnan .and. ieee_is_nan(a(i)) .and. ieee_is_nan(b(i)))) then
                  allclose_real = .false.
                  return
               end if
            else
               if (abs(a(i) - b(i)) > atolv + rtolv * abs(b(i))) then
                  allclose_real = .false.
                  return
               end if
            end if
         end do
      end function allclose_real

      logical function allclose_integer(a, b, rtol, atol)
         integer, intent(in) :: a(:), b(:)
         real(kind=dp), intent(in), optional :: rtol, atol
         real(kind=dp) :: rtolv, atolv
         integer :: i
         rtolv = 1.0e-5_dp
         atolv = 1.0e-8_dp
         if (present(rtol)) rtolv = rtol
         if (present(atol)) atolv = atol
         if (size(a) /= size(b)) then
            allclose_integer = .false.
            return
         end if
         allclose_integer = .true.
         do i = 1, size(a)
            if (abs(real(a(i), kind=dp) - real(b(i), kind=dp)) > atolv + rtolv * abs(real(b(i), kind=dp))) then
               allclose_integer = .false.
               return
            end if
         end do
      end function allclose_integer

      function cov2_real(x, y, ddof) result(c)
         real(kind=dp), intent(in) :: x(:), y(:)
         integer, intent(in), optional :: ddof
         real(kind=dp), allocatable :: c(:,:)
         real(kind=dp) :: mx, my, vxx, vyy, vxy, den
         integer :: n, d
         n = min(size(x), size(y))
         d = 1
         if (present(ddof)) d = ddof
         mx = sum(x(1:n)) / real(n, kind=dp)
         my = sum(y(1:n)) / real(n, kind=dp)
         den = real(max(1, n - d), kind=dp)
         vxx = sum((x(1:n) - mx)**2) / den
         vyy = sum((y(1:n) - my)**2) / den
         vxy = sum((x(1:n) - mx) * (y(1:n) - my)) / den
         allocate(c(2,2))
         c(1,1) = vxx; c(1,2) = vxy
         c(2,1) = vxy; c(2,2) = vyy
      end function cov2_real

      function cov_matrix_rows_real(x, ddof) result(c)
         real(kind=dp), intent(in) :: x(:,:)
         integer, intent(in), optional :: ddof
         real(kind=dp), allocatable :: c(:,:)
         real(kind=dp), allocatable :: xc(:,:), mu(:)
         integer :: n, p, d
         n = size(x, 1)
         p = size(x, 2)
         d = 1
         if (present(ddof)) d = ddof
         allocate(c(p, p))
         if (n <= 0 .or. p <= 0) then
            c = 0.0_dp
            return
         end if
         allocate(mu(p), xc(n, p))
         mu = sum(x, dim=1) / real(n, kind=dp)
         xc = x - spread(mu, dim=1, ncopies=n)
         c = matmul(transpose(xc), xc) / real(max(1, n - d), kind=dp)
      end function cov_matrix_rows_real

      function corrcoef2_real(x, y) result(r)
         real(kind=dp), intent(in) :: x(:), y(:)
         real(kind=dp), allocatable :: r(:,:)
         real(kind=dp), allocatable :: c(:,:)
         real(kind=dp) :: sx, sy
         c = cov2_real(x, y, 1)
         sx = sqrt(max(c(1,1), 0.0_dp))
         sy = sqrt(max(c(2,2), 0.0_dp))
         allocate(r(2,2))
         if (sx <= tiny(1.0_dp) .or. sy <= tiny(1.0_dp)) then
            r = 0.0_dp
            r(1,1) = 1.0_dp
            r(2,2) = 1.0_dp
         else
            r(1,1) = 1.0_dp
            r(2,2) = 1.0_dp
            r(1,2) = c(1,2) / (sx * sy)
            r(2,1) = r(1,2)
         end if
      end function corrcoef2_real

      function convolve_real(x, h, mode) result(y)
         real(kind=dp), intent(in) :: x(:), h(:)
         character(len=*), intent(in), optional :: mode
         real(kind=dp), allocatable :: y(:), full(:)
         integer :: i, j, nx, nh, ny, start, stop
         character(len=:), allocatable :: m
         nx = size(x)
         nh = size(h)
         if (nx <= 0 .or. nh <= 0) then
            allocate(y(0))
            return
         end if
         allocate(full(nx + nh - 1))
         full = 0.0_dp
         do i = 1, nx
            do j = 1, nh
               full(i + j - 1) = full(i + j - 1) + x(i) * h(j)
            end do
         end do
         m = "full"
         if (present(mode)) m = to_lower(trim(mode))
         if (m == "full") then
            y = full
            return
         end if
         if (m == "same") then
            ny = nx
            start = nh / 2 + 1
            stop = start + ny - 1
            y = full(start:stop)
            return
         end if
         if (m == "valid") then
            ny = max(nx, nh) - min(nx, nh) + 1
            if (ny <= 0) then
               allocate(y(0))
            else
               start = min(nx, nh)
               stop = start + ny - 1
               y = full(start:stop)
            end if
            return
         end if
         y = full
      end function convolve_real


      function convolve_int(x, h, mode) result(y)
         integer, intent(in) :: x(:), h(:)
         character(len=*), intent(in), optional :: mode
         integer, allocatable :: y(:), full(:)
         integer :: i, j, nx, nh, ny, start, stop
         character(len=:), allocatable :: m
         nx = size(x)
         nh = size(h)
         if (nx <= 0 .or. nh <= 0) then
            allocate(y(0))
            return
         end if
         allocate(full(nx + nh - 1))
         full = 0
         do i = 1, nx
            do j = 1, nh
               full(i + j - 1) = full(i + j - 1) + x(i) * h(j)
            end do
         end do
         m = "full"
         if (present(mode)) m = to_lower(trim(mode))
         if (m == "full") then
            y = full
            return
         end if
         if (m == "same") then
            ny = nx
            start = nh / 2 + 1
            stop = start + ny - 1
            y = full(start:stop)
            return
         end if
         if (m == "valid") then
            ny = max(nx, nh) - min(nx, nh) + 1
            if (ny <= 0) then
               allocate(y(0))
            else
               start = min(nx, nh)
               stop = start + ny - 1
               y = full(start:stop)
            end if
            return
         end if
         y = full
      end function convolve_int

      pure real(kind=dp) function polyval_real_scalar(p, x) result(y)
         real(kind=dp), intent(in) :: p(:)
         real(kind=dp), intent(in) :: x
         integer :: i
         if (size(p) <= 0) then
            y = 0.0_dp
            return
         end if
         y = p(1)
         do i = 2, size(p)
            y = y * x + p(i)
         end do
      end function polyval_real_scalar

      pure function polyval_real_vec(p, x) result(y)
         real(kind=dp), intent(in) :: p(:)
         real(kind=dp), intent(in) :: x(:)
         real(kind=dp), allocatable :: y(:)
         integer :: i
         allocate(y(size(x)))
         do i = 1, size(x)
            y(i) = polyval_real_scalar(p, x(i))
         end do
      end function polyval_real_vec

      pure function polyder_real(p, m) result(dpcoef)
         real(kind=dp), intent(in) :: p(:)
         integer, intent(in), optional :: m
         real(kind=dp), allocatable :: dpcoef(:)
         real(kind=dp), allocatable :: cur(:), nxt(:)
         integer :: i, k, order, ncur
         if (present(m)) then
            order = m
         else
            order = 1
         end if
         if (order <= 0) then
            allocate(dpcoef(size(p)))
            dpcoef = p
            return
         end if
         if (size(p) <= 1 .or. order >= size(p)) then
            allocate(dpcoef(1))
            dpcoef = 0.0_dp
            return
         end if
         allocate(cur(size(p)))
         cur = p
         do k = 1, order
            ncur = size(cur) - 1
            if (ncur <= 0) then
               allocate(dpcoef(1))
               dpcoef = 0.0_dp
               return
            end if
            allocate(nxt(ncur))
            do i = 1, ncur
               nxt(i) = cur(i) * real(ncur - i + 1, kind=dp)
            end do
            call move_alloc(nxt, cur)
         end do
         allocate(dpcoef(size(cur)))
         dpcoef = cur
      end function polyder_real

      pure logical function fft_is_power_of_two(n)
         integer, intent(in) :: n
         fft_is_power_of_two = n > 0 .and. iand(n, n - 1) == 0
      end function fft_is_power_of_two


      subroutine fft_dft_forward(x, y)
         complex(kind=dp), intent(in) :: x(:)
         complex(kind=dp), intent(out) :: y(:)
         integer :: j, k, nn
         real(kind=dp) :: ang, twopi
         complex(kind=dp) :: w

         nn = size(x)
         twopi = 2.0_dp * acos(-1.0_dp)
         do k = 1, nn
            y(k) = cmplx(0.0_dp, 0.0_dp, kind=dp)
            do j = 1, nn
               ang = -twopi * real((k - 1) * (j - 1), kind=dp) / real(nn, kind=dp)
               w = cmplx(cos(ang), sin(ang), kind=dp)
               y(k) = y(k) + x(j) * w
            end do
         end do
      end subroutine fft_dft_forward


      subroutine fft_dft_inverse(x, y)
         complex(kind=dp), intent(in) :: x(:)
         complex(kind=dp), intent(out) :: y(:)
         integer :: j, k, nn
         real(kind=dp) :: ang, twopi
         complex(kind=dp) :: w

         nn = size(x)
         twopi = 2.0_dp * acos(-1.0_dp)
         do k = 1, nn
            y(k) = cmplx(0.0_dp, 0.0_dp, kind=dp)
            do j = 1, nn
               ang = twopi * real((k - 1) * (j - 1), kind=dp) / real(nn, kind=dp)
               w = cmplx(cos(ang), sin(ang), kind=dp)
               y(k) = y(k) + x(j) * w
            end do
            y(k) = y(k) / real(nn, kind=dp)
         end do
      end subroutine fft_dft_inverse


      subroutine fft_radix2_inplace(a, inverse)
         complex(kind=dp), intent(inout) :: a(:)
         logical, intent(in) :: inverse
         integer :: i, j, m, mmax, istep, n
         real(kind=dp) :: pi, theta, wi, wpi, wpr, wr, wtemp
         complex(kind=dp) :: temp

         n = size(a)
         if (n <= 1) return

         j = 1
         do i = 1, n
            if (i < j) then
               temp = a(j)
               a(j) = a(i)
               a(i) = temp
            end if
            m = n / 2
            do while (m >= 1 .and. j > m)
               j = j - m
               m = m / 2
            end do
            j = j + m
         end do

         pi = acos(-1.0_dp)
         mmax = 1
         do while (n > mmax)
            istep = 2 * mmax
            if (inverse) then
               theta = pi / real(mmax, kind=dp)
            else
               theta = -pi / real(mmax, kind=dp)
            end if
            wtemp = sin(0.5_dp * theta)
            wpr = -2.0_dp * wtemp * wtemp
            wpi = sin(theta)
            wr = 1.0_dp
            wi = 0.0_dp
            do m = 1, mmax
               do i = m, n, istep
                  j = i + mmax
                  temp = cmplx(wr, wi, kind=dp) * a(j)
                  a(j) = a(i) - temp
                  a(i) = a(i) + temp
               end do
               wtemp = wr
               wr = wr * wpr - wi * wpi + wr
               wi = wi * wpr + wtemp * wpi + wi
            end do
            mmax = istep
         end do

         if (inverse) a = a / real(n, kind=dp)
      end subroutine fft_radix2_inplace


      function fft_fft_real(x, n) result(y)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in), optional :: n
         complex(kind=dp), allocatable :: y(:)
         complex(kind=dp), allocatable :: xc(:)
         integer :: i, nn

         if (present(n)) then
            nn = n
         else
            nn = size(x)
         end if
         if (nn < 0) nn = 0
         allocate(y(nn))
         if (nn <= 0) return
         allocate(xc(nn))
         xc = cmplx(0.0_dp, 0.0_dp, kind=dp)
         do i = 1, min(size(x), nn)
            xc(i) = cmplx(x(i), 0.0_dp, kind=dp)
         end do
         if (fft_use_fast .and. fft_is_power_of_two(nn)) then
            y = xc
            call fft_radix2_inplace(y, .false.)
         else
            call fft_dft_forward(xc, y)
         end if
      end function fft_fft_real


      function fft_fft_complex(x, n) result(y)
         complex(kind=dp), intent(in) :: x(:)
         integer, intent(in), optional :: n
         complex(kind=dp), allocatable :: y(:)
         complex(kind=dp), allocatable :: xc(:)
         integer :: i, nn

         if (present(n)) then
            nn = n
         else
            nn = size(x)
         end if
         if (nn < 0) nn = 0
         allocate(y(nn))
         if (nn <= 0) return
         allocate(xc(nn))
         xc = cmplx(0.0_dp, 0.0_dp, kind=dp)
         do i = 1, min(size(x), nn)
            xc(i) = x(i)
         end do
         if (fft_use_fast .and. fft_is_power_of_two(nn)) then
            y = xc
            call fft_radix2_inplace(y, .false.)
         else
            call fft_dft_forward(xc, y)
         end if
      end function fft_fft_complex


      function fft_ifft(x, n) result(y)
         complex(kind=dp), intent(in) :: x(:)
         integer, intent(in), optional :: n
         complex(kind=dp), allocatable :: y(:)
         complex(kind=dp), allocatable :: xc(:)
         integer :: i, nn

         if (present(n)) then
            nn = n
         else
            nn = size(x)
         end if
         if (nn < 0) nn = 0
         allocate(y(nn))
         if (nn <= 0) return
         allocate(xc(nn))
         xc = cmplx(0.0_dp, 0.0_dp, kind=dp)
         do i = 1, min(size(x), nn)
            xc(i) = x(i)
         end do
         if (fft_use_fast .and. fft_is_power_of_two(nn)) then
            y = xc
            call fft_radix2_inplace(y, .true.)
         else
            call fft_dft_inverse(xc, y)
         end if
      end function fft_ifft


      function fft_rfft(x, n) result(y)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in), optional :: n
         complex(kind=dp), allocatable :: y(:)
         complex(kind=dp), allocatable :: full(:)
         integer :: nn, m

         full = fft_fft_real(x, n)
         nn = size(full)
         m = nn / 2 + 1
         allocate(y(m))
         y = full(1:m)
      end function fft_rfft


      function fft_irfft(x, n) result(y)
         complex(kind=dp), intent(in) :: x(:)
         integer, intent(in), optional :: n
         real(kind=dp), allocatable :: y(:)
         complex(kind=dp), allocatable :: full(:), ctmp(:)
         integer :: k, m, nn, idx

         if (present(n)) then
            nn = n
         else
            nn = 2 * max(0, size(x) - 1)
         end if
         if (nn < 0) nn = 0
         allocate(full(nn))
         full = cmplx(0.0_dp, 0.0_dp, kind=dp)
         if (nn == 0) then
            allocate(y(0))
            return
         end if
         m = min(size(x), nn / 2 + 1)
         if (m > 0) full(1:m) = x(1:m)
         do k = 2, m - 1
            idx = nn - k + 2
            if (1 <= idx .and. idx <= nn) then
               full(idx) = conjg(x(k))
            end if
         end do
         ctmp = fft_ifft(full, nn)
         allocate(y(nn))
         y = real(ctmp, kind=dp)
      end function fft_irfft


      function fft_fftfreq(n, d) result(f)
         integer, intent(in) :: n
         real(kind=dp), intent(in), optional :: d
         real(kind=dp), allocatable :: f(:)
         integer :: i, k
         real(kind=dp) :: dopt, den

         dopt = 1.0_dp
         if (present(d)) dopt = d
         if (n < 0) then
            allocate(f(0))
            return
         end if
         allocate(f(n))
         if (n == 0) return
         den = real(n, kind=dp) * dopt
         do i = 1, n
            k = i - 1
            if (k <= (n - 1) / 2) then
               f(i) = real(k, kind=dp) / den
            else
               f(i) = real(k - n, kind=dp) / den
            end if
         end do
      end function fft_fftfreq


      function fft_rfftfreq(n, d) result(f)
         integer, intent(in) :: n
         real(kind=dp), intent(in), optional :: d
         real(kind=dp), allocatable :: f(:)
         integer :: i, m
         real(kind=dp) :: dopt, den

         dopt = 1.0_dp
         if (present(d)) dopt = d
         if (n < 0) then
            allocate(f(0))
            return
         end if
         m = n / 2 + 1
         allocate(f(m))
         if (m == 0) return
         den = real(max(1, n), kind=dp) * dopt
         do i = 1, m
            f(i) = real(i - 1, kind=dp) / den
         end do
      end function fft_rfftfreq

end module python_mod
