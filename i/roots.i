/*
 * $Id: roots.i,v 1.1 2005-09-18 22:06:07 dhmunro Exp $
 * Collection of root, maximum, and minimum finders.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

local roots;
/* DOCUMENT roots.i
       defines:
     nraphson     - Newton-Raphson/bisection root solver (scalar)
     f_inverse    - function inverse by Newton-Raphson (vectorized)
     mnbrent      - Brent's method minimizer (scalar)
     mxbrent      - Brent's method maximizer (scalar)
 */

/* ------------------------------------------------------------------------ */

func nraphson(f_and_dfdx, x0, x1, xerr)
/* DOCUMENT nraphson(f_and_dfdx, x0, x1)
         or nraphson(f_and_dfdx, x0, x1, xerr)

     Find a root of a function by Newton-Raphson iteration, backed
     up by bisection if the convergence seems poor.  The subroutine
     F_AND_DFDX must be defined as:
          func F_AND_DFDX (x, &f, &dfdx)
     returning both the function value f(x) and derivative dfdx(x).
     If F_AND_DFDX always returns dfdx==0, nraphson uses bisection.
     The value of x is constrained to lie within the interval from
     X0 to X1; the function values at these two points must have
     opposite sign.  The iteration stops when the root is known to
     within XERR, or to machine precision if XERR is nil or zero.

     f_inverse is a "vectorized" version of nraphson.

     Based on rtsafe from Press, et. al. Numerical Recipes, Ch 9.

   SEE ALSO: mnbrent, mxbrent, f_inverse
 */
{
  if (is_void(xerr) || xerr<0.0) xerr= 0.0;
  x0= double(x0);
  x1= double(x1);
  local f, dfdx;

  /* get function value at endpoints -- derivatives unused */
  f_and_dfdx, x0, f, dfdx;
  if (f == 0.0) return x0;
  dxo= f;
  f_and_dfdx, x1, f, dfdx;
  if (f == 0.0) return x1;
  if (f*dxo > 0.0) error, "f(x0) and f(x1) have same sign";

  if (f > 0.0) {
    xlo= x0;
    xhi= x1;
  } else {
    xlo= x1;
    xhi= x0;
  }

  /* first guess is midpoint */
  x= 0.5*(xhi+xlo);
  dx= dxo= abs(xhi-xlo);
  f_and_dfdx, x, f, dfdx;
  if (f < 0.0)      xlo= x;
  else if (f > 0.0) xhi= x;
  else return x;

  for (i=1 ; i<=nr_maxits ; ++i) {
    xo= x;
    if (((x-x1)*dfdx-f)*((x-x0)*dfdx-f) >= 0.0 ||
        abs(2.*f) > abs(dxo*dfdx)) {
      /* take bisection step if N-R step would be out of bounds
         or if previous step did not converge fast enough */
      dxo= dx;
      dx= 0.5*(xhi-xlo);
      x= xlo+dx;

    } else {
      /* take N-R step */
      dxo= dx;
      dx= f/double(dfdx);
      x-= dx;
    }

    /* quit on either machine precision or requested precision */
    if (x==xo || abs(dx)<xerr) return x;

    f_and_dfdx, x, f, dfdx;
    if (f < 0.0) xlo= x;
    else         xhi= x;
  }

  error, "nr_maxits iteration count exceeded";
}

nr_maxits= 100;

func f_inverse(f_and_dfdx, y, x0, x1, xerr)
/* DOCUMENT f_inverse(f_and_dfdx, y, x0, x1, xerr)
         or f_inverse(f_and_dfdx, y, x0, x1, xerr)

     Find values of an inverse function by Newton-Raphson iteration,
     backed up by bisection if the convergence seems poor.  The
     subroutine F_AND_DFDX must be defined as:
          func F_AND_DFDX (x, &f, &dfdx)
     returning both the function value f(x) and derivative dfdx(x).
     If the input x is an array, the returned f and dfdx must have
     the same shape as the input x.  If F_AND_DFDX always returns
     zero dfdx, f_inverse will use bisection.

     The result x will have the same shape as the input Y values.

     The values of x are constrained to lie within the interval from
     X0 to X1; the function value must be on opposite sides of the
     required Y at these interval endpoints.  The iteration stops
     when the root is known to within XERR, or to machine precision
     if XERR is nil or zero.  X0, X1, and XERR may be arrays conformable
     with Y.

     f_inverse takes the same number of iterations for every Y value;
     it does not notice that some may have converged before others.

   SEE ALSO: nraphson
 */
{
  if (is_void(xerr) || xerr<0.0) xerr= 0.0;
  is_scalar = !dimsof(y)(1);
  if (is_scalar) y = [y];
  x0+= 0.0*y;
  x1+= 0.0*y;
  local f, dfdx;

  /* get function value at endpoints -- derivatives unused */
  f_and_dfdx, x0, f, dfdx;
  f-= y;
  dxo= f;
  f_and_dfdx, x1, f, dfdx;
  f-= y;
  if (anyof(f*dxo > 0.0)) error, "f(x0)-y and f(x1)-y have same sign";

  dfdx= x0;
  mask= double(f > dxo);
  maskc= 1.0-mask;
  xlo= x0*mask + x1*maskc;
  xhi= x1*mask + x0*maskc;

  /* first guess is midpoint */
  x= 0.5*(xhi+xlo);
  dx= dxo= abs(xhi-xlo);
  f_and_dfdx, x, f, dfdx;
  f-= y;
  mask= (f < 0.0);
  list= where(mask);
  if (numberof(list)) xlo(list)= x(list);
  list= where(!mask);
  if (numberof(list)) xhi(list)= x(list);

  for (i=1 ; i<=nr_maxits ; ++i) {
    xo= x;
    mask= ((((x-x1)*dfdx-f)*((x-x0)*dfdx-f) >= 0.0) |
           (abs(2.*f) > abs(dxo*dfdx)));
    list= where(mask);
    if (numberof(list)) {
      /* take bisection step where N-R step would be out of bounds
         or if previous step did not converge fast enough */
      dxob= dx(list);
      xob= xlo(list);
      dxb= 0.5*(xhi(list)-xob);
      xb= xob+dxb;
    } else {
      xb= dxb= dxob= [];
    }

    list= where(!mask);
    if (numberof(list)) {
      /* otherwise take N-R step */
      dxon= dx(list);
      dxn= f(list)/dfdx(list);
      xon= x(list);
      xn= xon-dxn;
    } else {
      xn= dxn= dxon= [];
    }

    x= merge(xb, xn, mask);
    dx= merge(dxb, dxn, mask);
    dxo= merge(dxob, dxon, mask);
    /* check for uniform convergence either to requested precision
       or to machine precision */
    if (allof((x==xo) | (abs(dx)<xerr))) return is_scalar? x(1) : x;

    f_and_dfdx, x, f, dfdx;
    f-= y;
    mask= (f < 0.0);
    list= where(mask);
    if (numberof(list)) xlo(list)= x(list);
    list= where(!mask);
    if (numberof(list)) xhi(list)= x(list);
  }

  error, "nr_maxits iteration count exceeded";
}

/* ------------------------------------------------------------------------ */

func mxbrent(f, x0, x1, x2, &xmax, xerr)
/* DOCUMENT fmax= mxbrent(f, x0, x1, x2)
         or fmax= mxbrent(f, x0, x1, x2, xmax)
         or fmax= mxbrent(f, x0, x1, x2, xmax, xerr)

     returns the maximum of the function F (of a single argument x),
     given three points X0, X1, and X2 such that F(X1) is greater than
     either F(X0) or F(X2), and X1 is between X0 and X2.  If the
     XMAX argument is provided, it is set to the x value which
     produced FMAX.  If XERR is supplied, the search stops when
     a fractional error of XERR in x is reached; note that XERR
     smaller than the square root of the machine precision (or
     omitted) will cause convergence to machine precision in FMAX.

     The algorithm is Brent's method - a combination of inverse
     parabolic interpolation and golden section search - as adapted
     from Numerical Recipes Ch. 10 (Press, et. al.).

   SEE ALSO: mxbrent, nraphson, f_inverse
 */
{
  from_mxbrent= 1;
  return mnbrent(f, x0, x1, x2, xmax, xerr);
}

func mnbrent(f, x0, x1, x2, &xmin, xerr)
/* DOCUMENT fmin= mnbrent(f, x0, x1, x2)
         or fmin= mnbrent(f, x0, x1, x2, xmin)
         or fmin= mnbrent(f, x0, x1, x2, xmin, xerr)

     returns the minimum of the function F (of a single argument x),
     given three points X0, X1, and X2 such that F(X1) is less than
     either F(X0) or F(X2), and X1 is between X0 and X2.  If the
     XMIN argument is provided, it is set to the x value which
     produced FMIN.  If XERR is supplied, the search stops when
     a fractional error of XERR in x is reached; note that XERR
     smaller than the square root of the machine precision (or
     omitted) will cause convergence to machine precision in FMIN.

     The algorithm is Brent's method - a combination of inverse
     parabolic interpolation and golden section search - as adapted
     from Numerical Recipes Ch. 10 (Press, et. al.).

   SEE ALSO: mxbrent, nraphson, f_inverse
 */
{
  if (is_void(xerr) || xerr<2.e-8) xerr= 2.e-8;
  while (1.+xerr*xerr == 1.) xerr*= 2.0;
  golden= (sqrt(5.)-1.)/(sqrt(5.)+1.);
  epsilon= 1.e-20;

  x0= double(x0);
  x1= double(x1);
  x2= double(x2);
  xlo= min(x0, x2);
  xhi= max(x0, x2);

  x= w= v= x1;
  e= 0.0;
  fx= fw= fv= (from_mxbrent? -f(x) : f(x));

  for (i=1 ; i<=br_maxits ; ++i) {
    xm= 0.5*(xlo+xhi);
    abserr= xerr*abs(x) + epsilon;
    abserr2= 2.*abserr;
    if (abs(x-xm) <= (abserr2-0.5*(xhi-xlo))) {
      xmin= x;
      return (from_mxbrent? -fx : fx);
    }

    if (abs(e) > abserr) {
      /* attempt a trial parabolic fit to (w,v,x) -- the three
         smallest points seen so far */
      r= (x-w)*(fx-fv);
      q= (x-v)*(fx-fw);
      p= (x-v)*q - (x-w)*r;
      q= 2.*(q-r);
      if (q > 0.0) p= -p;
      q= abs(q);
      e1= e;
      e= d;
      if (abs(p)>=abs(0.5*q*e1) || p<=q*(xlo-x) || p>=q*(xhi-x)) {
        /* take golden section step -- parabolic step is crazy */
        e= (x>=xm? xlo-x : xhi-x);
        d= golden*e;
      } else {
        /* take inverse parabolic step */
        d= p/q;
        u= x+d;
        if ((u-xlo)<abserr2 || (xhi-u)<abserr2) d= sign(xm-x)*abserr;
      }

    } else {
      /* take golden section step */
      e= (x>=xm? xlo-x : xhi-x);
      d= golden*e;
    }

    /* evaluate at next point, avoiding meaninglessly small step size */
    u= x + (abs(d)>=abserr? d : sign(d)*abserr);
    fu= (from_mxbrent? -f(u) : f(u));

    if (fu <= fx) {
      /* new point is best so far */
      if (u >= x) xlo= x;
      else        xhi= x;
      v= w;   fv= fw;
      w= x;   fw= fx;
      x= u;   fx= fu;

    } else {
      if (u < x) xlo= u;
      else       xhi= u;
      if (fu<=fw || w==x) {
        /* new point is second best so far */
        v= w;   fv= fw;
        w= u;   fw= fu;
      } else if (fu<=fw || v==x || v==w) {
        /* new point is third best so far */
        v= u;   fv= fu;
      }
    }
  }

  error, "br_maxits iteration count exceeded";
}

br_maxits= 100;

/* ------------------------------------------------------------------------ */
