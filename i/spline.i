/*
 * $Id: spline.i,v 1.2 2010-04-18 10:33:38 thiebaut Exp $
 * Cubic spline interpolator.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func spline(dydx, y, x, xp, dydx1=, dydx0=)
/* DOCUMENT dydx= spline(y, x)
         or   yp= spline(dydx, y, x, xp)
         or   yp= spline(y, x, xp)
     computes the cubic spline curve passing through the points (X, Y).

     With two arguments, Y and X, spline returns the derivatives DYDX at
     the points, an array of the same length as X and Y.  The DYDX values
     are chosen so that the piecewise cubic function returned by the four
     argument call will have a continuous second derivative.

     The X array must be strictly monotonic; it may either increase or
     decrease.

     The values Y and the derivatives DYDX uniquely determine a piecewise
     cubic function, whose value is returned in the four argument form.
     In this form, spline is analogous to the piecewise linear interpolator
     interp; usually you will regard it as a continuous function of its
     fourth argument, XP.  The first argument, DYDX, will normally have
     been computed by a previous call to the two argument spline function.
     However, this need not be the case; another DYDX will generate a
     piecewise cubic function with continuous first derivative, but a
     discontinuous second derivative.  For XP outside the extreme values
     of X, spline is linear (if DYDX1 or DYDX0 keywords were specified,
     the function will NOT have continuous second derivative at the
     endpoint).

     The XP array may have any dimensionality; the result YP will have
     the same dimensions as XP.

     If you only want the spline evaluated at a single set of XP, use the
     three argument form.  This is equivalent to:
          yp= spline(spline(y,x), y, x, xp)

     The keywords DYDX1 and DYDX0 can be used to set the values of the
     returned DYDX(1) and DYDX(0) -- the first and last values of the
     slope, respectively.  If either is not specified or nil, the slope at
     that end will be chosen so that the second derivative is zero there.

     The function tspline (tensioned spline) gives an interpolation
     function which lies between spline and interp, at the cost of
     requiring you to specify another parameter (the tension).

   SEE ALSO: interp, tspline
 */
{
  if (is_void(x)) {
    /* spline(y,x) form */
    x= y;
    y= dydx;
    dx= x(dif);
    dy= y(dif);
    diag= (2./dx)(pcen);
    if (numberof(x)>2) diag(2:-1)*= 2.;
    rhs= (3.*dy/(dx*dx))(pcen);
    if (numberof(x)>2) rhs(2:-1)*= 2.;
    dx= 1./dx;
    if (is_void(dydx1)) {
      if (is_void(dydx0)) {
        return TDsolve(dx, diag, dx, rhs);  /* simple natural spline */
      } else {
        dx= dx(1:-1);
        rhs= rhs(1:-1);
        rhs(0)-= dydx0*dx(0);
        return grow(TDsolve(dx, diag(1:-1), dx, rhs), dydx0);
      }
    } else {
      dydx1= double(dydx1);
      if (is_void(dydx0)) {
        dx= dx(2:0);
        rhs= rhs(2:0);
        rhs(1)-= dydx1*dx(1);
        return grow(dydx1, TDsolve(dx, diag(2:0), dx, rhs));
      } else {
        if (numberof(x)==2) return double([dydx1, dydx0]);
        dx= dx(2:-1);
        rhs= rhs(2:-1);
        rhs(1)-= dydx1*dx(1);
        rhs(0)-= dydx0*dx(0);
        return grow(dydx1, TDsolve(dx, diag(2:-1), dx, rhs), dydx0);
      }
    }
  }

  if (is_void(xp)) {
    /* spline(y,x,xp) form */
    xp= x;
    x= y;
    y= dydx;
    dydx= spline(y,x,dydx1=dydx1,dydx0=dydx0);
  }

  /* spline(dydx,y,x,xp) form */
  l= digitize(xp, x); /* index of lower boundary of interval containing xp */
  u= l+1;

  /* extend x, y, dydx so that l and u can be used as index lists */
  dx= x(0)-x(1);
  x= grow(x(1)-dx, x, x(0)+dx);
  y= grow(y(1)-dydx(1)*dx, y, y(0)+dydx(0)*dx);
  dydx= grow(dydx(1), dydx, dydx(0));

  xl= x(l);
  dx= double(x(u)-xl);
  yl= y(l);
  dy= y(u)-yl;
  dl= dydx(l);
  du= dydx(u);
  dydx= dy/dx;
  return poly(xp-xl, yl, dl, (3.*dydx-du-2.*dl)/dx, (du+dl-2.*dydx)/(dx*dx));
}

func tspline(tension, d2ydx2, y, x, xp, dydx1=, dydx0=)
/* DOCUMENT d2ydx2= tspline(tension, y, x)
         or     yp= tspline(tension, d2ydx2, y, x, xp)
         or     yp= tspline(tension, y, x, xp)
     computes a tensioned spline curve passing through the points (X, Y).

     The first argument, TENSION, is a positive number which determines
     the "tension" in the spline.  In a cubic spline, the second derivative
     of the spline function varies linearly between the points X.  In the
     tensioned spline, the curvature is concentrated near the points X,
     falling off at a rate proportional to the tension.  Between the points
     of X, the function varies as:
           y= C1*exp(k*x) + C2*exp(-k*x) + C3*x + C4
     The parameter k is proportional to the TENSION; for k->0, the function
     reduces to the cubic spline (a piecewise cubic function), while for
     k->infinity, the function reduces to the piecewise linear function
     connecting the points.  The TENSION argument may either be a scalar
     value, in which case, k will be TENSION*(numberof(X)-1)/(max(X)-min(X))
     in every interval of X, or TENSION may be an array of length one less
     than the length of X, in which case the parameter k will be
     abs(TENSION/X(dif)), possibly varying from one interval to the next.
     You can use a variable tension to flatten "bumps" in one interval
     without affecting nearby intervals.  Internally, tspline forces
     k*X(dif) to lie between 0.01 and 100.0 in every interval, independent
     of the value of TENSION.  Typically, the most dramatic variation
     occurs between TENSION of 1.0 and 10.0.

     With three arguments, Y and X, spline returns the derivatives D2YDX2 at
     the points, an array of the same length as X and Y.  The D2YDX2 values
     are chosen so that the tensioned spline function returned by the five
     argument call will have a continuous first derivative.

     The X array must be strictly monotonic; it may either increase or
     decrease.

     The values Y and the derivatives D2YDX2 uniquely determine a tensioned
     spline function, whose value is returned in the five argument form.
     In this form, tspline is analogous to the piecewise linear interpolator
     interp; usually you will regard it as a continuous function of its
     fifth (or fourth) argument, XP.

     The XP array may have any dimensionality; the result YP will have
     the same dimensions as XP.

     The D2YDX2 argument will normally have been computed by a previous call
     to the three argument tspline function.  If you will be computing the
     values of the spline function for many sets of XP, use this five
     argument form.

     If you only want the tspline evaluated at a single set of XP, use the
     four argument form.  This is equivalent to:
          yp= tspline(tension, tspline(tension,y,x), y, x, xp)

     The keywords DYDX1 and DYDX0 can be used to set the values of the
     returned DYDX(1) and DYDX(0) -- the first and last values of the
     slope, respectively.  If either is not specified or nil, the slope at
     that end will be chosen so that the second derivative is zero there.

     The function tspline (tensioned spline) gives an interpolation
     function which lies between spline and interp, at the cost of
     requiring you to specify another parameter (the tension).

   SEE ALSO: interp, tspline
 */
{
  if (is_void(x)) {
    /* tspline(tension, y,x) form */
    x= double(y);
    y= d2ydx2;
    dx= x(dif);
    dy= y(dif);
    if (numberof(tension)==numberof(dx)) k= tension/abs(dx);
    else k= tension*numberof(dx)/(max(x)-min(x));
    k= max(min(k, 100./abs(dx)), 0.01/abs(dx));
    kdx= k*dx;
    skdx= sinh(kdx);
    diag= (cosh(kdx)/skdx-1./kdx)/k;
    diag= diag(pcen);
    if (numberof(x)>2) diag(2:-1)*= 2.;
    offd= (1./kdx-1./skdx)/k;
    ddydx= (dy/dx)(dif);
    if (is_void(dydx1)) {
      if (is_void(dydx0)) {
        if (numberof(x)==2) return [0., 0.];
        diag= diag(2:-1);
        offd= offd(2:-1);
        return grow(0., TDsolve(offd, diag, offd, ddydx), 0.);
      } else {
        dydx0-= dy(0)/dx(0);
        if (numberof(x)==2) return [0., dydx0/diag(0)];
        diag= diag(2:0);
        offd= offd(2:0);
        ddydx= grow(ddydx, dydx0);
        return grow(0., TDsolve(offd, diag, offd, ddydx));
      }
    } else {
      dydx1= dy(1)/dx(1) - dydx1;
      if (is_void(dydx0)) {
        if (numberof(x)==2) return [dydx1/diag(1), 0.];
        diag= diag(1:-1);
        offd= offd(1:-1);
        ddydx= grow(dydx1, ddydx);
        return grow(TDsolve(offd, diag, offd, ddydx), 0.);
      } else {
        dydx0-= dy(0)/dx(0);
        if (numberof(x)==2) return [dydx1/diag(1), dydx0/diag(0)];
        ddydx= grow(dydx1, ddydx, dydx0);
        return TDsolve(offd, diag, offd, ddydx);
      }
    }
  }

  if (is_void(xp)) {
    /* tspline(tension, y,x,xp) form */
    xp= x;
    x= y;
    y= d2ydx2;
    d2ydx2= tspline(tension, y,x,dydx1=dydx1,dydx0=dydx0);
  }

  /* tspline(tension, d2ydx2,y,x,xp) form */
  l= digitize(xp, x); /* index of lower boundary of interval containing xp */
  u= l+1;

  /* extend x so that l and u can be used as index lists --
     be careful not to make new intervals larger than necessary */
  n= numberof(x)-1;  /* number of original intervals */
  dxavg= (x(0)-x(1))/n;
  if (dxavg>0.) {
    dx0= max(max(xp)-x(0), dxavg);
    dx1= max(x(1)-min(xp), dxavg);
  } else {
    dx0= min(min(xp)-x(0), dxavg);
    dx1= min(x(1)-max(xp), dxavg);
  }
  x= grow(x(1)-dx1, x, x(0)+dx0);

  /* compute k so that sinh(k*dx) is safe to compute */
  dx= x(dif);
  if (numberof(tension)==n) {
    k= grow(0., tension, 0.)/abs(dx);
  } else {
    k= tension/abs(dxavg);
  }
  k= max(min(k, 100./abs(dx)), 0.01/abs(dx));

  /* extend y carefully so that linear extrapolation happens automatically */
  k1= k(2);
  k0= k(-1);
  dydx1= (y(2)-y(1))/dx1;
  kdx= k1*dx1;
  d2u= d2ydx2(2);
  d2l= d2ydx2(1);
  dydx1+= ((d2u-d2l*cosh(kdx))/sinh(kdx) - (d2u-d2l)/kdx)/k1;
  dydx0= (y(0)-y(-1))/dx0;
  kdx= k0*dx0;
  d2u= d2ydx2(0);
  d2l= d2ydx2(-1);
  dydx0+= ((d2u*cosh(kdx)-d2l)/sinh(kdx) - (d2u-d2l)/kdx)/k0;

  y= grow(y(1)-dydx1*dx1, y, y(0)+dydx0*dx0);
  d2ydx2= grow(0., d2ydx2, 0.);

  /* begin interpolation */
  xu= x(u);
  xl= x(l);
  dx= xu-xl;
  dxl= xp-xl;
  yl= y(l);
  dydx= (y(u)-yl)/dx;

  km2= 1./(k*k);
  km2(1)= 0.;
  km2(0)= 0.;
  km2= km2(l);
  k= k(l);
  skdx= sinh(k*dx);

  d2u= d2ydx2(u);
  d2l= d2ydx2(l);
  d3= km2*(d2u-d2l)/dx;

  d2ydx2= d2u*sinh(k*dxl)/skdx + d2l*(sinh(k*(xu-xp))/skdx-1.);

  return yl + km2*d2ydx2 + (dydx-d3)*dxl;
}

func sprime(dydx, y, x, xp)
/* DOCUMENT ypprime= sprime(dydx, y, x, xp)
     computes the derivative of the cubic spline curve passing through the
     points (X, Y) at the points XP.

     The DYDX values will have been computed by a previous call to SPLINE,
     and are chosen so that the piecewise cubic function returned by the four
     argument call will have a continuous second derivative.

     The X array must be strictly monotonic; it may either increase or
     decrease.
*/
{
  /* spline(dydx,y,x,xp) form */
  l= digitize(xp, x); /* index of lower boundary of interval containing xp */
  u= l+1;

  /* extend x, y, dydx so that l and u can be used as index lists */
  dx= x(0)-x(1);
  x= grow(x(1)-dx, x, x(0)+dx);
  y= grow(y(1)-dydx(1)*dx, y, y(0)+dydx(0)*dx);
  dydx= grow(dydx(1), dydx, dydx(0));

  xl= x(l);
  dx= double(x(u)-xl);
  yl= y(l);
  dy= y(u)-yl;
  dl= dydx(l);
  du= dydx(u);
  dydx= dy/dx;
  return poly(xp-xl, dl, 2.*(3.*dydx-du-2.*dl)/dx, 3.*(du+dl-2.*dydx)/(dx*dx));
}
