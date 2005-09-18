/*
 * $Id: series.i,v 1.1 2005-09-18 22:06:07 dhmunro Exp $
 * Routines for handling geometric series (e.g.- tapered zoning).
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func series_s(r, n)
/* DOCUMENT series_s(r, n)
     returns the sum s of the finite geometric series
        1 + r + r^2 + r^3 + ... + r^n
     Using n<0 is equivalent to using the reciprocal of r, that is,
        series_s(r, -n) == series_s(1./r, n)
     R or N or both may be arrays, as long as they are conformable.
   SEE ALSO: series_r, series_n
 */
{
  /* force conformability immediately */
  dims= dimsof(r, n);
  rr= r + array(0.0, dims);
  nn= n + array(0, dims);

  /* form result array, initialized to n==0 result */
  s= array(1.0, dims);

  /* subdivide into n==0, n>0, n<0, and r==1.0 cases */
  rnot= rr!=1.0;

  mask= (nn>0 & rnot);   /* n>0 case */
  list= where(mask);
  if (numberof(list)) {
    rrx= rr(list);
    s= merge((rrx^(1+nn(list))-1.0)/(rrx-1.0), s(where(!mask)), mask);
  }

  mask= (nn<0 & rnot);   /* n<0 case */
  list= where(mask);
  if (numberof(list)) {
    rrx= 1.0/rr(list);
    s= merge((rrx^(1-nn(list))-1.0)/(rrx-1.0), s(where(!mask)), mask);
  }

  list= where(!rnot);   /* r==1.0 case */
  if (numberof(list)) s= merge(s(where(rnot)), abs(nn(list)), rnot);

  return s;
}

func series_r(s, n)
/* DOCUMENT series_r(s, n)
     returns the ratio r of the finite geometric series, given the sum s:
        1 + r + r^2 + r^3 + ... + r^n = s
     Using n<0 will return the the reciprocal of n>0 result, that is,
        series_r(s, -n) == 1.0/series_r(s, n)
     If n==0, returns s-1 (the n==1 result).
     S or N or both may be arrays, as long as they are conformable.
   SEE ALSO: series_s, series_n
 */
{
  /* force conformability immediately */
  dims= dimsof(s, n);
  ss= s + array(0.0, dims);
  nn= n + array(0, dims);

  /* form result array, initialized to abs(n)<2 result */
  r= ss-1.0;

  /* work only with n>0 -- take reciprocals at end if necessary */
  nneg= nn<0;
  nn= abs(nn)+1;
  nbig= nn>2;

  /* compute an approximate result which has exact values and
     derivatives at s==1, s==n, and s->infinity --
     different approximations apply for s>n and s<n */
  mask= nbig & (ss>nn);
  list= where(mask);
  if (numberof(list)) {
    sx= ss(list);
    nx= nn(list);
    pow= 1.0/(nx-1.0);
    npow= nx^pow - 1.0;
    n2r= 1.0/(nx-2.0);
    A= (2.0-nx*npow)*n2r;
    B= (2.0*npow-nx*pow)*nx*n2r;
    r= merge(sx^pow - pow + A*(nx/sx)^pow + B/sx, r(where(!mask)), mask);
  }
  mask= nbig & (ss<=nn);
  list= where(mask);
  if (numberof(list)) {
    sx= ss(list);
    nx= nn(list);
    sn= (sx-1.0)/(nx-1.0);
    n2r= 1.0/(nx*nx);
    r= merge(1.0 - 1.0/sx + n2r*sn*sn*(nx+1.0 - sn), r(where(!mask)), mask);
  }

  /* Polish the approximation using Newton-Raphson iterations.
     There are never many of these, so do the entire vector together.  */
  mask= nbig & (ss!=nn);
  list= where(mask);
  if (numberof(list)) {
    rx= r(list);
    ss= ss(list);
    nn= nn(list);
    for (;;) {
      rr= rx-1.0;
      rn= rx^(nn-1);
      rrss= rr*ss;
      delta= rrss - (rx*rn-1.0);
      if (allof(abs(delta)<=1.e-9*abs(rrss))) break;
      rx+= delta/(nn*rn-ss);
    }
    /* try to get it to machine precision */
    if (anyof(delta)) rx+= delta/(nn*rn-ss);
    r= merge(rx, r(where(!mask)), mask);
  }

  list= where(nneg);
  if (numberof(list)) r= merge(1.0/r(list), r(where(!nneg)), nneg);

  return r;
}

func series_n(r, s)
/* DOCUMENT series_n(r, s)
     returns the minimum number n of terms required for the geometric
     series
        1 + r + r^2 + r^3 + ... + r^n = s
     to reach at least the given value s.  An alternate viewpoint is
     that n is the minimum number of terms required to achieve the
     sum s, with a ratio no larger than r.
     Returns 0 if r<1 and s>1/(1-r), or if s<1.
     The routine makes the most sense for r>1 and s substantially
     greater than 1.  The intended use is to determine the minimum
     number of zones required to span a given thickness t with a given
     minimum zone size z, and maximum taper ratio r (assumed >1 here):
        n= series_n(r, t/z);
     With this n, you have the option of adjusting r or z downwards
     (using series_r or series_s, respectively) to achieve the final
     desired zoning.
     R or S or both may be arrays, as long as they are conformable.
   SEE ALSO: series_s, series_r
 */
{
  n= 1.0 + (r-1.0)*s;
  bad= (n<=0.0 | s<1.0);
  list= where(bad);
  if (numberof(list))
    n= merge(array(1.0, numberof(list)), n(where(!bad)), bad);
  mask= r==1.0 & !bad;
  list= where(mask);
  if (numberof(list))
    n= merge((s+0.0*n)(list), n(where(!mask)), mask);
  mask= !mask & !bad;
  list= where(mask);
  if (numberof(list))
    n= merge(log(n(list))/log(r(list)), n(where(!mask)), mask);
  return long(ceil(n))-1;
}
