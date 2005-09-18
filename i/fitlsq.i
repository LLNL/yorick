/*
 * $Id: fitlsq.i,v 1.1 2005-09-18 22:05:56 dhmunro Exp $
 * Least squares fit a piecewise linear function to data.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func fitlsq(y, x, xp, weight=, stats=)
/* DOCUMENT yp= fitlsq(y, x, xp)
            ...
            yfit= interp(yp, xp, xfit)
     performs a least squares fit to the data points (X, Y).  The input
     XP are the abcissas of the piecewise linear function passing through
     (XP, YP) which is the best fit to the data (X,Y) in a least squares
     sense.  The XP must be strictly monotone, either increasing or
     decreasing.  As for interp, the piecewise linear fit is taken to be
     constant outside the limits of the XP.

     The result YP is linearly interpolated through any consecutive
     intervals of XP which contain no data points X, and linearly
     extrapolated beyond the extreme values of X (if any of the intervals
     of XP lie outside these extremes).

     A WEIGHT keyword of the same length as X and Y may be supplied in
     order to weight the various data points differently; a typical
     WEIGHT function is 1/sigma^2 where sigma are the standard deviations
     associated with the Y values.

   SEE ALSO: interp
 */
{
  np1= numberof(xp)+1;

  /* bin the input data into the xp, and create an extended version
     of xp for which the bins may be directly used as an index list */
  l= digitize(x, xp);
  dx= (xp(0)-xp(1))*1.e30;
  xx= grow(xp(1)-dx, xp, xp(0)+dx);

  xl= xx(l);
  xu= xx(l+1);
  dx= xu-xl;
  g= (x-xl)/dx;
  h= (xu-x)/dx;

  if (is_void(weight)) weight= 1.0;
  hy= histogram(l, h*y*weight, top=np1);
  hh= histogram(l, h*h*weight, top=np1);
  gh= histogram(l, g*h*weight, top=np1);
  gg= histogram(l, g*g*weight, top=np1);
  gy= histogram(l, g*y*weight, top=np1);

  diag= hh(2:0)+gg(1:-1);
  rhs= hy(2:0)+gy(1:-1);

  /* the triadiagonal system will be singular if there are any pairs
     of consecutive bins -- remove these first */
  list= where(diag);
  diag= diag(list);
  rhs= rhs(list);
  off= gh(list)(2:0);  /* sub and super diagonal */
  xpp= double(xp(list));
  yp= TDsolve(off, diag, off, rhs);

  /* special treatment if endpoints removed allows linear extrapolation
     of the fit until the true endpoints of the xp */
  if (list(1)>1) {
    yp= grow(yp(1)+(xp(1)-xpp(1))*(yp(2)-yp(1))/(xpp(2)-xpp(1)), yp);
    xpp= grow(xp(1), xpp);
  }
  if (list(0)<numberof(xp)) {
    grow, yp, yp(0)+(xp(0)-xpp(0))*(yp(0)-yp(-1))/(xpp(0)-xpp(-1));
    grow, xpp, xp(0)
  }

  /* add back any removed points */
  if (numberof(xpp)<numberof(xp)) yp= interp(yp,xpp, xp);

  return yp;
}
