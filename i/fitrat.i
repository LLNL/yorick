/*
 * $Id: fitrat.i,v 1.2 2010-04-18 10:33:38 thiebaut Exp $
 * Polynomial and rational function interpolators.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func fitpol(y, x, xp, keep=)
/* DOCUMENT yp= fitpol(y, x, xp)
         or yp= fitpol(y, x, xp, keep=1)
     is an interpolation routine similar to interp, except that fitpol
     returns the polynomial of degree numberof(X)-1 which passes through
     the given points (X,Y), evaluated at the requested points XP.

     The X must either increase or decrease monotonically.

     If the KEEP keyword is present and non-zero, the external variable
     yperr will contain a list of error estimates for the returned values
     yp on exit.

     The algorithm is taken from Numerical Recipes (Press, et. al.,
     Cambridge University Press, 1988); it is called Neville's algorithm.
     The rational function interpolator fitrat is better for "typical"
     functions.  The Yorick implementaion requires numberof(X)*numberof(XP)
     temporary arrays, so the X and Y arrays should be reasonably small.

   SEE ALSO: fitrat, interp
 */
{
  /* find lower and upper indices in x of the interval containing xp */
  nx= numberof(x);
  u= digitize(xp, x);
  l= max(u-1, 1);
  u= min(u, nx);

  /* initialize yp to the y(x) closest to xp */
  mask= (abs(x(l)-xp)<=abs(x(u)-xp));
  ns= l*mask + u*(!mask);
  yp= y(ns);
  ns--;

  c= d= y= array(double(y), dimsof(xp));
  dx= x - xp(-,);
  if (dimsof(ns)(1)) {
    is= ns;
    is(*)= nx*indgen(0:numberof(xp)-1);
  } else {
    is= 0;
  }
  for (m=1 ; m<nx ; m++) {
    ho= dx(1:nx-m,..);
    hp= dx(1+m:nx,..);
    den= (c(2:nx-m+1,..)-d(1:nx-m,..))/(ho-hp);
    d(1:nx-m,..)= hp*den;
    c(1:nx-m,..)= ho*den;
    mask= (2*ns>=(nx-m));
    y= d(max(ns,1)+is)*mask + c(ns+1+is)*(!mask); /* this is error estimate */
    yp+= y;
    ns-= mask;
  }

  if (keep) {
    extern yperr;
    yperr= y;
  }

  return yp;
}

func fitrat(y, x, xp, keep=)
/* DOCUMENT yp= fitrat(y, x, xp)
         or yp= fitrat(y, x, xp, keep=1)
     is an interpolation routine similar to interp, except that fitpol
     returns the diagonal rational function of degree numberof(X)-1 which
     passes through the given points (X,Y), evaluated at the requested
     points XP.  (The numerator and denominator polynomials have equal
     degree, or the denominator has one larger degree.)

     The X must either increase or decrease monotonically.  Also, this
     algorithm works by recursion, fitting successively to consecutive
     pairs of points, then consecutive triples, and so forth.
     If there is a pole in any of these fits to subsets, the algorithm
     fails even though the rational function for the final fit is non-
     singular.  In particular, if any of the Y values is zero, the
     algorithm fails, and you should be very wary of lists for which
     Y changes sign.  Note that if numberof(X) is even, the rational
     function is Y-translation invariant, while numberof(X) odd generally
     results in a non-translatable fit (because it decays to y=0).

     If the KEEP keyword is present and non-zero, the external variable
     yperr will contain a list of error estimates for the returned values
     yp on exit.

     The algorithm is taken from Numerical Recipes (Press, et. al.,
     Cambridge University Press, 1988); it is called the Bulirsch-Stoer
     algorithm.  The Yorick implementaion requires numberof(X)*numberof(XP)
     temporary arrays, so the X and Y arrays should be reasonably small.

   SEE ALSO: fitpol, interp
 */
{
  /* find lower and upper indices in x of the interval containing xp */
  nx= numberof(x);
  u= digitize(xp, x);
  l= max(u-1, 1);
  u= min(u, nx);

  /* initialize yp to the y(x) closest to xp */
  mask= (abs(x(l)-xp)<=abs(x(u)-xp));
  ns= l*mask + u*(!mask);
  yp= y(ns);

  exact= (x(ns)==xp);
  list= where(exact);
  if (numberof(list)) {
    yexact= yp(list);
    yerr= 0.0*yexact;
  }

  list= where(!exact);
  if (numberof(list)) {
    yp= yp(list);
    xp= xp(list);
    ns= ns(list);

    c= d= y= array(double(y), numberof(xp));
    d+= fitrat_tiny;  /* I question whether this ever rescues a
                         failed fit, but I'll leave it here anyway.  */
    dx= x - xp(-,);
    ns--;
    is= nx*indgen(0:numberof(xp)-1);
    for (m=1 ; m<nx ; m++) {
      di= d(1:nx-m,);
      ci= c(2:nx-m+1,);
      t= dx(1:nx-m,)*di/dx(1+m:nx,);
      dd= (ci-di)/(t-ci);
      d(1:nx-m,)= ci*dd;
      c(1:nx-m,)= t*dd;
      mask= (2*ns>=(nx-m));
      y= d(max(ns,1)+is)*mask + c(ns+1+is)*(!mask);
      yp+= y;
      ns-= mask;
    }

  } else {
    yp= [];
  }

  if (keep) {
    extern yperr;
    yperr= merge(yerr, y, exact);
  }

  return merge(yexact, yp, exact);
}

fitrat_tiny= 1.e-30;
