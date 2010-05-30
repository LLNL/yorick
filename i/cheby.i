/*
 * $Id: cheby.i,v 1.3 2010-05-30 16:24:04 dhmunro Exp $
 * Chebyshev polynomial approximation routines
 * after Numerical Recipes (Press et. al.) section 5.6
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func cheby_fit(f, x, n, nterp=)
/* DOCUMENT fit = cheby_fit(f, interval, n)
 *       or fit = cheby_fit(f, x, n)
 *   returns the Chebyshev fit (for use in cheby_eval) of degree N
 *   to the function F on the INTERVAL (a 2 element array [a,b]).
 *   In the second form, F and X are arrays; the function to be
 *   fit is the piecewise linear function of xp interp(f,x,xp), and
 *   the interval of the fit is [min(x),max(x)].  You can use the
 *   nterp= keyword to set a different interpolator, which must have
 *   the same calling sequence as interp (e.g.- nterp=spline).
 *
 *   The return value is the array [a,b, c0,c1,c2,...cN] where [a,b]
 *   is the interval over which the fit applies, and the ci are the
 *   Chebyshev coefficients.  It may be useful to use a relatively
 *   large value of N in the call to cheby_fit, then to truncate the
 *   resulting fit to fit(1:3+m) before calling cheby_eval.
 *
 * SEE ALSO: cheby_eval, cheby_integ, cheby_deriv, cheby_poly, cheby_conv,
 *           cheby_trunc, rcheby_fit
 */
{
  a = double(min(x));
  b = max(x);
  ++n;
  p = (pi/n) * span(0.5,n-0.5,n);
  c = cos(p*indgen(0:n-1)(-,));
  p = a + 0.5*(b-a)*(c(,2)+1.);
  if (is_array(f)) p = interp(f,x, p);
  else for (i=1 ; i<=n ; ++i) p(i) = f(p(i));
  return grow([a,b], (2./n) * (p(+)*c(+,)));
}

func cheby_eval(fit, x)
/* DOCUMENT cheby_eval(fit, x)
 *   evaluates the Chebyshev fit (from cheby_fit) at points X.
 *   the return values have the same dimensions as X.
 *
 * SEE ALSO: cheby_fit
 */
{
  x = interp([-2.,2.],fit(1:2), x);
  a = b = 0.;
  for (i=numberof(fit) ; i>2 ; --i) {
    c = b;
    b = a;
    a = x*b - c + fit(i);
  }
  return 0.5*(a-c);
}

func cheby_integ(fit, x0)
/* DOCUMENT cheby_integ(fit)
 *       or cheby_integ(fit, x0)
 *   returns Chebyshev fit to the integral of the function of the
 *   input Chebyshev FIT.  If X0 is given, the returned integral will
 *   be zero at X0 (which should be inside the fit interval fit(1:2)),
 *   otherwise the integral will be zero at x=fit(1).
 *
 * SEE ALSO: cheby_fit, cheby_deriv
 */
{
  fit = double(fit);
  if (is_void(x0)) x0 = fit(1);
  c = 0.25*(fit(2)-fit(1));
  n = numberof(fit);
  f = grow(fit, [0.]);
  if (n>4) f(4:n-1) = c * (fit(3:n-2)-fit(5:n))/indgen(n-4);
  f(n:n+1) = c * fit(n-1:n)/indgen(n-3:n-2);
  f(3) = 0.;
  f(3) = -2.*cheby_eval(f, x0);
  return f;
}

func cheby_deriv(fit)
/* DOCUMENT cheby_deriv(fit)
 *   returns Chebyshev fit to the derivative of the function of the
 *   input Chebyshev FIT.
 *
 * SEE ALSO: cheby_fit, cheby_integ
 */
{
  fit = double(fit);
  n = numberof(fit) - 2;
  if (n<2) return [fit(1),fit(2),0.];
  f = fit(1:-1);
  f(0) = 2.*(n-1)*fit(0);
  if (n>2) f(-1) = 2.*(n-2)*fit(-1);
  for (i=-2 ; i>1-n ; --i) f(i) = f(i+2) + 2.*(i+n-1)*fit(i);
  f(3:0) *= (2./(fit(2)-fit(1)));
  return f;
}

func cheby_poly(fit)
/* DOCUMENT cheby_poly(fit)
  *   returns coefficients An of x^n as [A0, A1, A2, ..., An] for
  *   the given FIT returned by cheby_fit.  You should only consider
  *   actually using these for very low degree polynomials; cheby_eval
  *   is nearly always a superior way to evaluate the polynomial.
  *
  * SEE ALSO: cheby_fit, cheby_conv
  */
{
   c = fit(3:0);
   n = numberof(c);

   /* first hunk finds coefficients of polynomial on [-1,1] */
   d = dd = 0.*c;
   d(1) = c(0);
   for (j=n-1 ; j>=2 ; j--) {
     for (k=n-j+2 ; k>=2 ; k--) {
       t = d(k);
       d(k) = 2.*d(k-1) - dd(k);
       dd(k) = t;
     }
     t = d(1);
     d(1) = -dd(1) + c(j);
     dd(1) = t;
   }
   for (j=n ; j>=2 ; j--) d(j) = d(j-1) - dd(j);
   d(1) = -dd(1) + 0.5*c(1);

   /* second hunk rescales and shifts coefficients to [a,b] */
   a = fit(1);
   b = fit(2);
   d *= (2./(b-a)) ^ indgen(0:n-1);
   c = 0.5*(a+b);
   for (j=1 ; j<n ; j++) for (k=n-1 ; k>=j ; k--) d(k) -= c*d(k+1);
   return d;
}

func cheby_conv(_p_, x)
/* DOCUMENT fit = cheby_conv(poly, interval)
 *   convert polynomial coefficients POLY = [a0, a1, ..., aN] to a
 *   Chebyshev fit suitable for input to cheby_eval.  The INTERVAL
 *   = [xmin,xmax] is the most stable region for evaluation.  Omitting
 *   INTERVAL gives the natural interval [-1,1] for Chebyshev polynomials.
 *   You may also pass another Chebyshev fit (from cheby_fit) as the
 *   INTERVAL to use the same interval as that fit.
 * SEE ALSO: cheby_fit, cheby_eval, cheby_poly
 */
{
  if (is_void(x)) x = [-1.0, 1.0];
  if (numberof(_p_) == 1) return grow(double(x(1:2)), 2.*_p_(1));
  return cheby_fit(_cheby_conv, x(1:2), numberof(_p_)-1);
}
func _cheby_conv(x)
{
  y = _p_(0);
  for (i=numberof(_p_)-1 ; i>=1 ; --i) y = y*x + _p_(i);
  return y;
}

func cheby_trunc(fit, err, &e)
/* DOCUMENT tfit = cheby_trunc(fit, err)
 *       or tfit = cheby_trunc(fit, err, e)
 *   truncate cheby_fit FIT to relative error ERR by dropping trailing
 *   Chebyshev coefficients smaller than ERR.  If ERR is omitted, it
 *   defaults to 1.e-9.  Optionally returns E, which is the list of
 *   relative errors incurred by dropping each order [e0, e1, ... eN].
 *   Often there will be a sudden improvement with some order, which
 *   can assist you with selecting an appropriate truncation.
 * SEE ALSO: cheby_fit
 */
{
  if (is_void(err)) err = 1.e-9;
  else err = min(abs(err), 0.5);
  e = abs(fit(0:3:-1))(psum);
  e = e(::-1) / e(0);
  m = -sum(e < err);
  return fit(1:m);
}
