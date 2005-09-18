/*
 * $Id: fermii.i,v 1.1 2005-09-18 22:06:15 dhmunro Exp $
 * incomplete Fermi-Dirac integrals of orders -1/2, 1/2, 3/2, 5/2
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

local fermii;
/* DOCUMENT #include "fermii.i"
 * Incomplete Fermi-Dirac integrals of orders -1/2, 1/2, 3/2, 5/2
 *
 * after M. Goano, Algorithm 745, ACM TOMS 21, #3, pp 221-232 (Sept. 95)
 *
 * SEE ALSO: fdim12, fdi12, fdi32, fdi52
 */

require, "fermi.i";
require, "dawson.i";

func fdim12(x, b)
/* DOCUMENT fdim12(x, b)
 *   return incomplete Fermi-Dirac integral of order -1/2,
 *      fdim12(x, b) = integral[b to inf]{ dt * t^-0.5 / (exp(t-x)+1) }
 *   default accuracy to about 1e-10
 * SEE ALSO: fdi12, fdi32, fdi52
 */
{
  name = "FDIM12";
  x += 0.*b; b += 0.*x;
  big = (x > b);
  list = where(big);
  if (numberof(list)) {
    xx = x(list); bb = b(list);
    fl = _fd_work(bb, exp(bb-xx), _s_m12);
    fl = fdm12(xx) - sqrt(bb)/.5*(1.-fl);
  }

  list = where(!big);
  if (numberof(list)) {
    fs = x(list); bb = b(list);
    fs = 1.77245385090551603*_fd_work(bb, exp(fs-bb), _b_m12);
  }
  return merge(fl, fs, big);
}

func fdi12(x, b)
/* DOCUMENT fdi12(x, b)
 *   return incomplete Fermi-Dirac integral of order 1/2,
 *      fdi12(x, b) = integral[b to inf]{ dt * t^0.5 / (exp(t-x)+1) }
 *   default accuracy to about 1e-10
 * SEE ALSO: fdim12, fdi32, fdi52
 */
{
  name = "FDI12";
  x += 0.*b; b += 0.*x;
  big = (x > b);
  list = where(big);
  if (numberof(list)) {
    xx = x(list); bb = b(list);
    fl = _fd_work(bb, exp(bb-xx), _s_12);
    fl = fd12(xx) - bb*sqrt(bb)/1.5*(1.-fl);
  }

  list = where(!big);
  if (numberof(list)) {
    fs = x(list); bb = b(list);
    fs = 0.5*1.77245385090551603*_fd_work(bb, exp(fs-bb), _b_12);
  }
  return merge(fl, fs, big);
}

func fdi32(x, b)
/* DOCUMENT fdi32(x, b)
 *   return incomplete Fermi-Dirac integral of order 1/2,
 *      fdi32(x, b) = integral[b to inf]{ dt * t^1.5 / (exp(t-x)+1) }
 *   default accuracy to about 1e-10
 * SEE ALSO: fdim12, fdi12, fdi52
 */
{
  name = "FDI32";
  x += 0.*b; b += 0.*x;
  big = (x > b);
  list = where(big);
  if (numberof(list)) {
    xx = x(list); bb = b(list);
    fl = _fd_work(bb, exp(bb-xx), _s_32);
    fl = fd32(xx) - bb*bb*sqrt(bb)/2.5*(1.-fl);
  }

  list = where(!big);
  if (numberof(list)) {
    fs = x(list); bb = b(list);
    fs = 0.75*1.77245385090551603*_fd_work(bb, exp(fs-bb), _b_32);
  }
  return merge(fl, fs, big);
}

func fdi52(x, b)
/* DOCUMENT fdi52(x, b)
 *   return incomplete Fermi-Dirac integral of order 1/2,
 *      fdi32(x, b) = integral[b to inf]{ dt * t^2.5 / (exp(t-x)+1) }
 *   default accuracy to about 1e-10
 * SEE ALSO: fdim12, fdi12, fdi32
 */
{
  name = "FDI52";
  x += 0.*b; b += 0.*x;
  big = (x > b);
  list = where(big);
  if (numberof(list)) {
    xx = x(list); bb = b(list);
    fl = _fd_work(bb, exp(bb-xx), _s_52);
    fl = fd52(xx) - bb*bb*bb*sqrt(bb)/3.5*(1.-fl);
  }

  list = where(!big);
  if (numberof(list)) {
    fs = x(list); bb = b(list);
    fs = 1.875*1.77245385090551603*_fd_work(bb, exp(fs-bb), _b_52);
  }
  return merge(fl, fs, big);
}

/* power series coefficients involve
 * x<b (_s_xx): related to half-integer incomplete gamma functions
 * x>b (_b_xx): Kummer's M(1, k/2, -x) == Hypergeometric 1F1[1, k/2, -x]
 */

func _s_m12(b, list)
{
  small = (b<=0.005);
  list = where(small);
  if (numberof(list)) {
    /* 1e-13 rational fit to 2/3(-x+2/5(x-2/7(x+2/9(x-2/11 x ...)))) */
    bs = b(list);
    bs = 1.-0.666666666666666667*bs*(1.+bs* 0.044572733182971606) /
      (1.+bs*(0.44457273316463447+0.06354339731587551*bs));
  }
  list = where(!small);
  if (numberof(list)) {
    bl = sqrt(b(list));
    bl = dawson(bl)/bl;
  }
  return merge(bs,bl, small);
}
func _b_m12(b, i)
{
  return _fd_eerfc(sqrt(b))/sqrt(i);
}

func _s_12(b, list)
{
  small = (b<=0.05);
  list = where(small);
  if (numberof(list)) {
    /* 1e-13 rational fit to 2/5(-x+2/7(x-2/9(x+2/11(x-2/13 x ...)))) */
    bs = b(list);
    bs = 1. - 
      0.4*bs*(1.+bs*(0.022623753252252726+0.0035112573452158458*bs)) /
      (1.+bs*(0.3083380389584768+0.028115777703514408*bs));
  }
  list= where(!small);
  if (numberof(list)) {
    bl = b(list);
    b = sqrt(bl); 
    bl = 1.5*(1.-dawson(b)/b)/bl;
  }
  return merge(bs,bl, small);
}
func _b_12(b, i)
{ /* 1.12837916709551257 = 2/sqrt(pi) */
  bs = sqrt(b);
  i = double(i);
  return (_fd_eerfc(bs)+1.12837916709551257*bs)/(i*sqrt(i));
}

func _s_32(b, list)
{
  small = (b<=0.15);
  list = where(small);
  if (numberof(list)) {
    /* 1e-13 rational fit to 2/7(-x+2/9(x-2/11(x+2/13(x-2/15 x ...)))) */
    bs = b(list);
    bs = 1. - 0.2857142857142857*bs*
      (1.+bs*(0.13216490580251333+0.0091563412585936*bs)) /
      (1.+bs*(0.3543871280251466+
              bs*(0.047504995939997935+0.0024540010292377927*bs)));
  }
  list = where(!small);
  if (numberof(list)) {
    bl = b(list);
    b = sqrt(bl);
    bl = 1./bl;
    bl = 2.5*(1.-1.5*(1.-dawson(b)/b)*bl)*bl;
  }
  return merge(bs,bl, small);
}
func _b_32(b, i)
{ /* 1.12837916709551257 = 2/sqrt(pi) */
  bs = sqrt(b);
  i = double(i);
  return (_fd_eerfc(bs)+1.12837916709551257*bs*
          (1.+b*0.666666666666666667)) / (i*i*sqrt(i));
}

func _s_52(b, list)
{
  small = (b<=0.3);
  list = where(small);
  if (numberof(list)) {
    /* 1e-13 rational fit to 2/9(-x+2/11(x-2/13(x+2/15(x-2/17 x ...)))) */
    bs = b(list);
    bs = 1. - 0.2222222222222222*bs*
      (1.+bs*(0.13615817872751315 +0.007895641703391007*bs)) /
      (1.+bs*(0.3179763605496484  +
              bs*(0.037737497320199126+0.0016965253200319766*bs)));
  }
  list= where(!small);
  if (numberof(list)) {
    bl = b(list);
    b = sqrt(bl);
    bl = 1./bl;
    bl = 3.5*(1.-2.5*(1.-1.5*(1.-dawson(b)/b)*bl)*bl)*bl;
  }
  return merge(bs,bl, small);
}
func _b_52(b, i)
{ /* 1.12837916709551257 = 2/sqrt(pi) */
  bs = sqrt(b);
  i = double(i);
  return (_fd_eerfc(bs)+1.12837916709551257*bs*
          (1.+b*0.666666666666666667*(1.+b*.4))) / (i*i*i*sqrt(i));
}

func _fd_eerfc(x)
{
  /*  exp(x^2) * erfc(x),  see erfc in dawson.i */
  { local mask1,mask2,mask3,mask4,xx,yy,zz; }
  y = x*x;
  if (any_in(,y,1., mask1, yy, x,xx))
    e1 = exp(yy)*(1.0 - xx * (1.0 + _cheby_eval(_erf_1, 2.*yy-1.)));
  if (any_in(1.,y,, mask2, z, x,xx)) {
    yy = 1./xx;
    if (any_in(,z,4., mask3, zz, yy,y))
      e3 = y * (0.5 + _cheby_eval(_erfc_1, (8./zz-5.)/3.));
    if (any_in(4.,z,, mask4, zz, yy,y))
      e4 = y * (0.5 + _cheby_eval(_erfc_2, 8./zz-1.));
    e2 = merge_n(e3,mask3, e4,mask4);
  }
  return merge_n(e1,mask1, e2,mask2);
}


func _fd_work(bb, fac, f)
{
  /* note that fac always <=1 */
  n = numberof(bb); /* always scalar or 1d */
  if (n==1) { bb=[bb(1)]; fac=[fac(1)]; }
  fd = fac*f(bb,1);
  master = where(abs(fd) > 1e-99);
  if (numberof(master)) {
    bb = bb(master);
    fac = fac(master);
    facn = fac*fac;
    sfacn = -1.;
    bn = bb+bb;
    fdi = summi = fd(master);
    frac = [1.,1./summi];
    /* biggest i recorded is 15 for _fd_tol=1e-9, 22 for 1e-13 */
    for (i=2; i<=_fd_itmax; i++,bn+=bb,facn*=fac,sfacn*=-1.) {
      fdiold = fdi;
      /* levin u-algorithm to accelerate series convergence */
      ri = 1./double(i);
      summi += sfacn*(fdi = facn*f(bn,i));
      cofb = (((1.-ri)^indgen(i-2:0:-1)*indgen(i-1)*ri))(-,-,);
      frac = (cofb*frac)(,,cum);
      frac += ([summi,1.]*(sfacn*ri*ri)/(fdi+1.e-99) - frac(,,0));
      fd(master) = fdi = frac(,1,1)/frac(,2,1);
      /* -- */
      list = where(abs(fdi-fdiold) >= _fd_tol*abs(fdi));
      if (!(n = numberof(list))) break;
      if (n==numberof(fdi)) continue;
      master = master(list);
      fdi = fdi(list);
      summi = summi(list);
      frac = frac(list,,);
      bn = bn(list);
      bb = bb(list);
      facn = facn(list);
      fac = fac(list);
    }
    if (i > _fd_itmax) error, "convergence failure in "+name;
  }
  return fd;
}

_fd_itmax = 100;
_fd_tol = 1.e-9;
