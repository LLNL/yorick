/*
 * $Id: bessel.i,v 1.2 2007-11-10 20:03:49 dhmunro Exp $
 * A few Bessel functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* Taken from Numerical Recipes, repaired bessj, bessi for x<<1.  */

/* ------------------------------------------------------------------------ */

func bessj0(x)
/* DOCUMENT bessj0(x)
     returns Bessel function J0 at points X.
   SEE ALSO: bessj
 */
{
  if (structof(x)==complex) error, "Bessel functions not valid for complex";
  return mergef(x, _bessj0_1, abs(x)<8.0, _bessj0_2);
}

func _bessj0_1(x)
{
  y = x*x;
  return poly(y, 57568490574.0, -13362590354.0, 651619640.7, -11214424.18,
              77392.33017, -184.9052456) /
    poly(y, 57568490411.0, 1029532985.0, 9494680.718, 59272.64853,
         267.8532712, 1.0);
}

func _bessj0_2(x)
{
  ax = abs(x);
  z = 8.0/ax;
  y = z*z;
  x = ax-0.785398164;  /* pi/4, rounded incorrectly */
  return sqrt(0.636619772/ax) *
    (cos(x)*poly(y, 1.0, -0.1098628627e-2,
                  0.2734510407e-4, -0.2073370639e-5, 0.2093887211e-6) -
     sin(x)*z*poly(y, -0.1562499995e-1, 0.1430488765e-3,
                    -0.6911147651e-5, 0.7621095161e-6, -0.934935152e-7));
}

func bessj1(x)
/* DOCUMENT bessj1(x)
     returns Bessel function J1 at points X.
   SEE ALSO: bessj
 */
{
  if (structof(x)==complex) error, "Bessel functions not valid for complex";
  return mergef(x, _bessj1_1, abs(x)<8.0, _bessj1_2);
}

func _bessj1_1(x)
{
  y = x*x;
  return x * poly(y, 72362614232.0, -7895059235.0, 242396853.1, -2972611.439,
                  15704.48260, -30.16036606) /
    poly(y, 144725228442.0, 2300535178.0, 18583304.74, 99447.43394,
         376.9991397, 1.0);
}

func _bessj1_2(x)
{
  ax = abs(x);
  z = 8.0/ax;
  y = z*z;
  xx = ax-2.356194491;  /* 3*pi/4 */
  return sign(x) * sqrt(0.636619772/ax) *
    (cos(xx)*poly(y, 1.0, 0.183105e-2, -0.3516396496e-4,
                  0.2457520174e-5, -0.240337019e-6) -
     sin(xx)*z*poly(y, 0.04687499995, -0.2002690873e-3, 0.8449199096e-5,
                    -0.88228987e-6, 0.105787412e-6));
}

func bessj(n, x)
/* DOCUMENT bessj(n, x)
     returns Bessel function Jn of order N at points X.  N must be scalar.
   SEE ALSO: bessy, bessi, bessk, bessj0, bessj1
 */
{
  if (n>1) {
    ax = abs(x);
    bj = mergef(ax, _bessj_0, ax<0.02*sqrt(n), _bessj_1, ax>n, _bessj_2);
    if (n%2) bj *= sign(x);
    return bj;
  } else if (n==1) {
    return bessj1(x);
  } else if (!n) {
    return bessj0(x);
  }
}

func _bessj_0(x)
{
  x *= 0.5;
  nn = double(n);
  rnf = exp(-sum(log(indgen(n))));
  rn1 = 1./(nn+1.);  rn2 = 0.5*rn1/(nn+2.);
  return (x^n*rnf) * poly(x*x, 1., -rn1, rn2, -rn2/(3.*(nn+3.)));
}

func _bessj_1(x)
{
  /* upward recurrence abs(x)>n */
  ax = abs(x);
  tox = 2.0/ax;
  bjm = bessj0(ax);
  bj = bessj1(ax);
  for (i=1 ; i<n ; i++) {
    bjp = i*tox*bj-bjm;
    bjm = bj;
    bj = bjp;
  }
  return bj;
}

func _bessj_2(x)
{
  /* downward recurrence abs(x)<=n */
  ax = abs(x);
  tox = 2.0/ax;   /* < 100/sqrt(n) */
  m = 2*((n+long(sqrt(bess_acc*n)))/2);
  jsum = 0;
  bjp = ans = add = array(0.0, numberof(ax));
  bj = array(1.0, numberof(ax));
  for (i=m ; i>0 ; i--) {
    bjm = i*tox*bj-bjp;
    bjp = bj;
    bj = bjm;
    list = where(abs(bj) > bess_big);
    if (numberof(list)) {
      bess_nrm = 1./bess_big;
      bj(list) *= bess_nrm;
      bjp(list) *= bess_nrm;
      ans(list) *= bess_nrm;
      add(list) *= bess_nrm;
    }
    if (jsum) add += bj;
    jsum = !jsum;
    if (i==n) ans = bjp;
  }
  bj = ans/(2.0*add-bj);
  return bj;
}

/* ------------------------------------------------------------------------ */

func bessy0(x)
/* DOCUMENT bessy0(x)
     returns Bessel function Y0 at points X.
   SEE ALSO: bessy
 */
{
  if (structof(x)==complex) error, "Bessel functions not valid for complex";
  return mergef(x, _bessy0_1, abs(x)<8.0, _bessy0_2);
}

func _bessy0_1(x)
{
  y= x*x;
  return poly(y, -2957821389.0, 7062834065.0, -512359803.6, 10879881.29,
              -86327.92757, 228.4622733) /
    poly(y, 40076544269.0, 745249964.8, 7189466.438, 47447.26470,
         226.1030244, 1.0) + 0.636619772*bessj0(x)*log(x);
}

func _bessy0_2(x)
{
  ax = abs(x);
  z = 8.0/ax;
  y = z*z;
  xx = ax-0.785398164;  /* pi/4, rounded incorrectly */
  return sqrt(0.636619772/ax) *
    (sin(xx)*poly(y, 1.0, -0.1098628627e-2, 0.2734510407e-4,
                  -0.2073370639e-5, 0.2093887211e-6) +
     cos(xx)*z*poly(y, -0.1562499995e-1, 0.1430488765e-3,
                    -0.6911147651e-5, 0.7621095161e-6, -0.934935152e-7));
}

func bessy1(x)
/* DOCUMENT bessy1(x)
     returns Bessel function Y1 at points X.
   SEE ALSO: bessy
 */
{
  if (structof(x)==complex) error, "Bessel functions not valid for complex";
  return mergef(x, _bessy1_1, abs(x)<8.0, _bessy1_2);
}

func _bessy1_1(x)
{
  y = x*x;
  return x * poly(y, -0.4900604943e13, 0.1275274390e13, -0.5153438139e11,
               0.7349264551e9, -0.4237922726e7, 0.8511937935e4) /
    poly(y, 0.2499580570e14, 0.4244419664e12, 0.3733650367e10,
         0.2245904002e8, 0.1020426050e6, 0.3549632885e3, 1.0) +
    0.636619772*(bessj1(x)*log(x)-1.0/x);
}

func _bessy1_2(x)
{
  ax = abs(x);
  z = 8.0/ax;
  y = z*z;
  xx = ax-2.356194491;  /* 3*pi/4 */
  return sqrt(0.636619772/x) *
    (sin(xx)*poly(y, 1.0, 0.183105e-2, -0.3516396496e-4,
                  0.2457520174e-5, -0.240337019e-6) +
     cos(xx)*z*poly(y, 0.04687499995, -0.2002690873e-3, 0.8449199096e-5,
                    -0.88228987e-6, 0.105787412e-6));
}

func bessy(n, x)
/* DOCUMENT bessy(n, x)
     returns Bessel function Yn of order N at points X.  N must be scalar.
   SEE ALSO: bessj, bessi, bessk, bessy0, bessy1
 */
{
  if (n>1) {
    /* upward recurrence */
    tox = 2.0/x;
    bym = bessy0(x);
    by = bessy1(x);
    for (i=1 ; i<n ; i++) {
      byp = i*tox*by-bym;
      bym = by;
      by = byp;
    }
    return by;
  } else if (n==1) {
    return bessy1(x);
  } else if (!n) {
    return bessy0(x);
  }
}

/* ------------------------------------------------------------------------ */

func bessi0(x)
/* DOCUMENT bessi0(x)
     returns Bessel function I0 at points X.
   SEE ALSO: bessi
 */
{
  if (structof(x)==complex) error, "Bessel functions not valid for complex";
  x = abs(x);
  return mergef(x, _bessi0_1, x<3.75, _bessi0_2);
}

func _bessi0_1(x)
{
  x = x/3.75;
  return poly(x*x, 1.0, 3.5156229, 3.0899424, 1.2067492, 0.2659732,
              0.360768e-1, 0.45813e-2);
}

func _bessi0_2(x)
{
  y = 3.75/x;
  return (exp(x)/sqrt(x)) * poly(y, 0.39894228, 0.1328592e-1, 0.225319e-2,
                                 -0.157565e-2, 0.916281e-2, -0.2057706e-1,
                                 0.2635537e-1, -0.1647633e-1, 0.392377e-2);
}

func bessi1(x)
/* DOCUMENT bessi1(x)
     returns Bessel function I1 at points X.
   SEE ALSO: bessi
 */
{
  if (structof(x)==complex) error, "Bessel functions not valid for complex";
  return mergef(x, _bessi1_1, abs(x)<3.75, _bessi1_2);
}

func _bessi1_1(x)
{
  y = x/3.75;
  y *= y;
  return x * poly(y, 0.5, 0.87890594, 0.51498869, 0.15084934,
                  0.2658733e-1, 0.301532e-2, 0.32411e-3);
}

func _bessi1_2(x)
{
  ax = abs(x);
  y = 3.75/ax;
  return sign(x) * (exp(ax)/sqrt(ax)) *
    poly(y, 0.39894228, -0.3988024e-1, -0.362018e-2, 0.163801e-2,
         -0.1031555e-1, 0.2282967e-1, -0.2895312e-1, 0.1787654e-1,
         -0.420059e-2);
}

func bessi(n, x)
/* DOCUMENT bessi(n, x)
     returns Bessel function In of order N at points X.  N must be scalar.
   SEE ALSO: bessk, bessj, bessy, bessi0, bessi1
 */
{
  if (n>1) {
    ax = abs(x);
    bi = mergef(ax, _bessi_0, ax<0.02*sqrt(n), _bessi_1);
    if (n%2) bi *= sign(x);
    return bi;
  } else if (n==1) {
    return bessi1(x);
  } else if (!n) {
    return bessi0(x);
  }
}

func _bessi_0(x)
{
  x *= 0.5;
  nn = double(n);
  rnf = exp(-sum(log(indgen(n))));
  rn1 = 1./(nn+1.);  rn2 = 0.5*rn1/(nn+2.);
  return (x^n*rnf) * poly(x*x, 1., rn1, rn2, rn2/(3.*(nn+3.)));
}

func _bessi_1(x)
{
  /* downward recurrence abs(x)<=n */
  tox = 2.0/x;
  m = 2*(n+long(sqrt(bess_acc*n)));
  bip = ans = array(0.0, numberof(x));
  bi = array(1.0, numberof(x));
  for (i=m ; i>0 ; i--) {
    bim = i*tox*bi+bip;
    bip = bi;
    bi = bim;
    list = where(abs(bi) > bess_big);
    if (numberof(list)) {
      bess_nrm = 1./bess_big;
      ans(list) *= bess_nrm;
      bi(list) *= bess_nrm;
      bip(list) *= bess_nrm;
    }
    if (i==n) ans = bip;
  }
  bi = ans*bessi0(x)/bi;
  return bi;
}

/* ------------------------------------------------------------------------ */

func bessk0(x)
/* DOCUMENT bessk0(x)
     returns Bessel function K0 at points X.
   SEE ALSO: bessk
 */
{
  if (structof(x)==complex) error, "Bessel functions not valid for complex";
  return mergef(x, _bessk0_1, x<=2.0, _bessk0_2);
}

func _bessk0_1(x)
{
  y= x*x/4.0;
  return (-log(x/2.0)*bessi0(x)) +
    poly(y, -0.57721566, 0.42278420, 0.23069756, 0.3488590e-1, 0.262698e-2,
         0.10750e-3, 0.74e-5);
}

func _bessk0_2(x)
{
  y = 2.0/x;
  return (exp(-x)/sqrt(x)) *
    poly(y, 1.25331414, -0.7832358e-1, 0.2189568e-1, -0.1062446e-1,
         0.587872e-2, -0.251540e-2, 0.53208e-3);
}

func bessk1(x)
/* DOCUMENT bessk1(x)
     returns Bessel function K1 at points X.
   SEE ALSO: bessk
 */
{
  if (structof(x)==complex) error, "Bessel functions not valid for complex";
  return mergef(x, _bessk1_1, x<=2.0, _bessk1_2);
}

func _bessk1_1(x)
{
  y = x*x/4.0;
  return (log(x/2.0)*bessi1(x)) +
    (1.0/x) * poly(y, 1.0, 0.15443144, -0.67278579, -0.18156897,
                   -0.1919402e-1, -0.110404e-2, -0.4686e-4);
}

func _bessk1_2(x)
{
  y = 2.0/x;
  return (exp(-x)/sqrt(x)) *
    poly(y, 1.25331414, 0.23498619, -0.3655620e-1, 0.1504268e-1,
         -0.780353e-2, 0.325614e-2, -0.68245e-3);
}

func bessk(n, x)
/* DOCUMENT bessk(n, x)
     returns Bessel function Kn of order N at points X.  N must be scalar.
   SEE ALSO: bessi, bessj, bessy, bessi0, bessi1
 */
{
  if (n>1) {
    /* upward recurrence */
    tox = 2.0/x;
    bkm = bessk0(x);
    bk = bessk1(x);
    for (i=1 ; i<n ; i++) {
      bkp = i*tox*bk+bkm;
      bkm = bk;
      bk = bkp;
    }
    return bk;
  } else if (n==1) {
    return bessk1(x);
  } else if (!n) {
    return bessk0(x);
  }
}

/* ------------------------------------------------------------------------ */

bess_acc= 40.0;
bess_big= 1.e10;

/* ------------------------------------------------------------------------ */

#if 0
func bess_check(void)
{
  /* values copied from Abramowitz and Stegun tables */
  eg = 0.5772156649;
  eps = 0.5e-30;
  x = [2.*eps, 0.6, 3.0, 17.0];
  fac5 = 5.*4.*3.*2.;  fac6 = 6.*fac5;
  j0 = [1., 0.912004863497211, -0.260051954901933, -0.169854252151184];
  j1 = [eps, 0.2867009881, 0.3390589585, -0.0976684928];
  j5 = [eps^5/fac5, 1.9948e-5, 4.3028e-2, -0.18704];
  j6 = [eps^6/fac6, 9.9956e-7, 1.1394e-2, 0.00071533];
  y0 = [2./pi*(log(eps)+eg), -0.3085098701, 0.3768500100, -0.0926371984];
  y1 = [-1./(pi*eps), -1.2603913472, 0.3246744248, 0.1672050361];
  y5 = [-24./(pi*eps^5), -3.2156e3, -1.9059, 0.06455];
  y6 = [-120./(pi*eps^6), -5.3351e4, -5.4365, 0.19996];
  i0 = [1., 0.5993272031, 0.2430003542, 0.0974943005]*exp(x);
  i1 = [eps, 0.1721644195, 0.1968267133, 0.0945819107]*exp(x);
  i5 = [eps^5/fac5, 1.1281e-5, 4.5409e-3, 4.5951e-2]*exp(x);
  i6 = [eps^6/fac6, 5.6286e-7, 1.0796e-3, 3.3128e-2]*exp(x);
  k0 = [-(log(eps)+eg), 1.4167376214, 0.6977615980, 0.3018080193]*exp(-x);
  k1 = [0.5/eps, 2.3739200376, 0.8065634800, 0.3105612340]*exp(-x);
  k5 = [12./eps^5, 8.7987e3, 1.8836e1, 6.1420e-1]*exp(-x);
  k6 = [60./eps^6, 1.4730e5, 6.8929e1, 8.3734e-1]*exp(-x);
  y = x;
  for (i=1 ; i<=numberof(x) ; i++) y(i) = bessj(5,x(i));
  if (anyof(y != bessj(5,x))) write, "ERROR - problem with scalar args";
  write, "j0:", max(abs(bessj(0,x)/j0-1.)), max(abs(bessj(0,-x)/j0-1.));
  write, "j1:", max(abs(bessj(1,x)/j1-1.)), max(abs(bessj(1,-x)/j1+1.));
  write, "j5:", max(abs(bessj(5,x)/j5-1.)), max(abs(bessj(5,-x)/j5+1.));
  write, "j6:", max(abs(bessj(6,x)/j6-1.)), max(abs(bessj(6,-x)/j6-1.));
  write, "y0:", max(abs(bessy(0,x)/y0-1.));
  write, "y1:", max(abs(bessy(1,x)/y1-1.));
  write, "y5:", max(abs(bessy(5,x)/y5-1.));
  write, "y6:", max(abs(bessy(6,x)/y6-1.));
  write, "i0:", max(abs(bessi(0,x)/i0-1.)), max(abs(bessi(0,-x)/i0-1.));
  write, "i1:", max(abs(bessi(1,x)/i1-1.)), max(abs(bessi(1,-x)/i1+1.));
  write, "i5:", max(abs(bessi(5,x)/i5-1.)), max(abs(bessi(5,-x)/i5+1.));
  write, "i6:", max(abs(bessi(6,x)/i6-1.)), max(abs(bessi(6,-x)/i6-1.));
  write, "k0:", max(abs(bessk(0,x)/k0-1.));
  write, "k1:", max(abs(bessk(1,x)/k1-1.));
  write, "k5:", max(abs(bessk(5,x)/k5-1.));
  write, "k6:", max(abs(bessk(6,x)/k6-1.));
}
#endif
