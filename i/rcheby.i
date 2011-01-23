/* rcheby.i
 * $Id: rcheby.i,v 1.4 2011-01-23 03:54:26 dhmunro Exp $
 * rational/polynomial minimax Chebyshev fits
 */
/* Copyright (c) 2010, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func rcheby_fit(f, x, m, k, nterp=)
/* DOCUMENT fit = rcheby_fit(f, interval, m, k)
 *       or fit = rcheby_fit(f, x, m, k)
 *   returns a rational Chebyshev fit (for use in rcheby_eval) of
 *   numerator degree M and denominator degree K, to the function
 *   F on the INTERVAL (a 2 element array [a,b]). In the second form,
 *   F and X are arrays; the function to be fit is the piecewise cubic
 *   function of xp spline(f,x,xp), and the interval of the fit is
 *   [min(x),max(x)].  You can pass an alternate interpolator using
 *   the nterp= keyword; it must have the same calling sequence as
 *   spline or interp.
 *
 *   The return value is the array [m,k, a,b, 2*n0,n1,...nM,d1,...,dK]
 *   where [a,b] is the interval over which the fit applies, the ni are
 *   the coefficients of the Chebyshev polynomials for the numerator
 *   (note that double the zeroth coefficient is stored), while the di
 *   are the denominator coefficients, with a zeroth coefficient of
 *   1.0 assumed.
 *
 *   The fitting algorithm returns very nearly the minimax error
 *   rational fit to F, that is, the rational function which minimizes
 *   the maximum absolute deviation from F for the given degrees.  This
 *   function (very nearly) has M+K+1 points of maximum deviation from
 *   F on the interval, which alternate in sign and have equal absolute
 *   value.  The algorithm is inspired by the discussion in section 5.13
 *   of Numerical Recipes, which itself is inspired by the well-known
 *   Remez (or Remes) algorithms.  Note that it may be used with K=0
 *   to obtain minimax polynomial fits.  (Compare with cheby_fit, the
 *   standard Chebyshev polynomial fitting algorithm.)
 *
 * Problem with this algorithm:
 * Nothing prevents numerator and denominator from having a factor
 * in common.  This will always happen if the function being fit really
 * IS a rational function of lower degree in both numerator and
 * denominator than (m,k); hopefully it is rare in other cases.  But
 * if you really care about a fit, you would be wise to check.
 *
 * SEE ALSO: rcheby_eval, cheby_fit, rcheby_build, rcheby_conv, rcheby_trunc
 */
{
  if (is_void(nterp)) nterp = spline;
  x = double(x);
  xmin = min(x);  xmax = max(x);   /* determine the fitting interval */

  /* First evaluate the function at the extrema of a Chebyshev function
   * with an order much larger than m+k+1 (a factor of rcheby_np).
   */
  np = rcheby_np * (m+k+1);   /* number of points np many times order */
  th = (pi/(np-1.0))*indgen(0:np-1);
  xn = cos(th);
  if (is_func(f)) f = f(interp([xmin,xmax],[-1.,1.], xn));
  else            f = nterp(f,x, interp([xmin,xmax],[-1.,1.], xn));

  /* Trick is to do least squares fit to f(x)+-e where e is the mean
   * absolute deviation and the sign is chosen to be the pointwise sign
   * of the deviation in the previous iteration.
   * To solve in least squares sense:
   *   sum(a_m*T_m) - sum(b_k*T_k*(f+-e)) = (f+-e)
   * where T_m are the Chebyshev polynomials, and a_m(b_m) are their
   * unknown coefficients for the numerator(denominator).
   */
  x = [0.5, xn];
  if (max(m,k) > 1) grow, x, cos(indgen(2:max(m,k))(-,)*th);
  if (m < k) x = grow(x(,1:m+1), x(,2:k+1));
  else if (k) grow, x, x(,2:k+1);
  m1 = m+1;           /* x(,1:m1) are numerator polynomials */
  xn = x(,1:m1);
  k0=m+2;  k1=m+k+1;  /* x(,k0:k1) are denominator polynomials */
  if (k) xd = x(,k0:k1);

  ee = e = 0.;   /* first pass doesn't bother with +-e */
  wt = 1.0;      /* ... or weights */
  dev = 1.e99;
  for (i=1 ; i<=rcheby_it ; ++i) { /* just do fixed number of iterations */
    y = f + sign(ee)*e;
    if (k) x(,k0:k1) = -xd*y;
    a = SVsolve(wt*x, wt*y);    /* same as a = regress(y, x, sigy=1/wt); */
    ee = a(+:1:m1)*xn(,+);
    if (k) ee /= 1.0 + a(+:k0:k1)*xd(,+);
    ee -= f;
    wt = abs(ee);          /* emphasize points near extrema (Remez-like) */
    d = max(wt);
    e = avg(wt);        /* estimate error as mean of absolute deviations */
    if (d < dev) {
      aa = a;
      dev = d;
    }
  }
  return grow([m,k,xmin,xmax], aa);
}
rcheby_np = 8;  /* number of points per extremum */
rcheby_it = 5;  /* number of iterations */

func rcheby_eval(fit, x)
/* DOCUMENT y = cheby_eval(fit, x)
 *   evaluates a rational Chebyshev FIT at points X.  The FIT is a 1D
 *   array of Chebyshev coefficients, as returned by rcheby_fit, namely:
 *     [m,k, a,b, 2*n0,n1,...nM,d1,...,dK]
 *   where ni are the coefficients of the Chebyshev polynomials in the
 *   numerator and di are the coefficients for the denominator (d0=1.0
 *   is implicit).
 *
 * SEE ALSO: rcheby_fit, cheby_fit, rcheby_build, rcheby_conv
 */
{
  m = long(fit(1)) + 5;
  k = long(fit(2));
  x = interp([-2.,2.],fit(3:4), x);
  a = b = 0.;
  for (i=m ; i>4 ; --i) {
    c = b;
    b = a;
    a = x*b - c + fit(i);
  }
  if (!k) return 0.5*(a-c);
  f = a - c;
  a = b = 0.;
  for (i=k+m ; i>m ; --i) {
    c = b;
    b = a;
    a = x*b - c + fit(i);
  }
  return f/(x*a-b+2.0 - b);
}

func rcheby_num(fit)
/* DOCUMENT fit = rcheby_num(rfit)
 *   extract numerator from rcheby_fit RFIT to a Chebyshev fit suitable
 *   for input to cheby_eval (not rcheby_eval).
 * SEE ALSO: cheby_fit, rcheby_fit, rcheby_den, rcheby_build
 */
{
  return fit(3:5+long(fit(1)));
}

func rcheby_den(fit)
/* DOCUMENT fit = rcheby_den(rfit)
 *   extract denominator from rcheby_fit RFIT to a Chebyshev fit suitable
 *   for input to cheby_eval (not rcheby_eval).
 * SEE ALSO: cheby_fit, rcheby_fit, rcheby_num, rcheby_build
 */
{
  m = long(fit(1));
  k = long(fit(2));
  f = grow(double(fit(3:4)), [2.0]);
  if (k) grow, f, fit(6+m:5+m+k);
  return f;
}

func rcheby_build(nfit, dfit)
/* DOCUMENT rfit = rcheby_build(nfit, dfit)
 *       or rfit = rcheby_build(nfit)
 *       or rfit = rcheby_build(,dfit)
 *   build fit suitable for input to rcheby_fit from separate numerator
 *   and denominator fits in the format returned by cheby_fit.
 *   You can use rcheby_build in conjunction with cheby_conv to build
 *   an rfit given the polynomial coefficients for the numerator and
 *   denominator: rcheby_build(cheby_conv(n), cheby_conv(d)).
 * SEE ALSO: rcheby_fit, rcheby_eval, rcheby_num, rcheby_den, cheby_conv
 */
{
  if (is_void(dfit)) dfit = [nfit(1), nfit(2), 2.0];
  if (is_void(nfit)) nfit = [dfit(1), dfit(2), 2.0];
  if (anyof(abs(nfit(1:2)-dfit(1:2))>1.e-10*abs(nfit(2)-nfit(1))))
    error, "numerator and denominator not defined on same interval";
  m = numberof(nfit) - 3;
  k = numberof(dfit) - 3;
  fit = array(0., 5+m+k);
  fit(1:2) = [m, k];
  fit(3:5+m) = nfit;
  nrm = 0.5*dfit(3);
  fit(5:5+m) *= nrm;
  if (k) fit(6+m:5+m+k) = dfit(4:0)/nrm;
  return fit;
}

func rcheby_trunc(fit, err, &en, &ed)
/* DOCUMENT tfit = rcheby_trunc(fit, err)
 *   truncate rcheby_fit FIT to relative error ERR by dropping
 *   trailing Chebyshev coefficients smaller than ERR.  (Note that
 *   this destroys the minimax property of rcheby_fit -- use with
 *   caution.)  If ERR is omitted, it defaults to 1.e-9.
 * SEE ALSO: rcheby_fit, cheby_trunc
 */
{
  n = cheby_trunc(cheby_num(fit), err, en);
  d = cheby_trunc(cheby_den(fit), err, ed);
  return cheby_build(n, d);
}
