/*
 * $Id: gammp.i,v 1.1 2005-09-18 22:06:14 dhmunro Exp $
 * Incomplete gamma (chi-square distribution) and beta functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* Algorithms from Numerical Recipes, Press, et. al., section 6.2,
 * Cambridge University Press (1988).
 */

require, "gamma.i";

func gammp(a, x, &q, &lg)
/* DOCUMENT gammp(a, x)
 *       or gammp(a, x, q, lg)
 *   return P(a,x) = int[0 to x]{ du * u^(a-1)*exp(-u) } / gamma(a)
 *   optionally return Q(a,x) = 1-P(a,x) and ln(gamma(a))
 *
 *   Note that erf(x)=gammp(0.5,x^2) and erfc(x)=gammq(0.5,x^2)
 *   Also P(chi2|nu)=gammp(0.5*nu,0.5*chi2)
 *    and Q(chi2|nu)=gammq(0.5*nu,0.5*chi2)
 *   are the probabilities that an observed chi-square be less than
 *   or greater than (P or Q) chi2 when there are nu degrees of freedom
 *   (terms in the chi-square sum).
 *
 * SEE ALSO: gammq, betai, ln_gamma (gamma.i), (dawson.i)
 */
{
  lg = ln_gamma(a);
  fact = double(!x);
  fact = (1.-fact)*exp(-x+a*log(x+fact)-lg);
  x += 0.*a;
  a += 0.*x;
  mask = x < a+1.;
  list = where(mask);
  if (numberof(list)) {
    p1 = fact(list)*_gammp(a(list), x(list));
    q1 = 1. - p1;
  }
  list = where(!mask);
  if (numberof(list)) {
    q = fact(list)*_gammq(a(list), x(list));
    p = 1. - q;
  }
  q = merge(q1, q, mask);
  return merge(p1, p, mask);
}

func gammq(a, x, &p, &lg)
/* DOCUMENT gammq(a, x)
 *       or gammq(a, x, p, lg)
 *   return Q(a,x) = 1 - int[0 to x]{ du * u^(a-1)*exp(-u) } / gamma(a)
 *   optionally return P(a,x) = 1-Q(a,x) and ln(gamma(a))
 *
 *   Note that erf(x)=gammp(0.5,x^2) and erfc(x)=gammq(0.5,x^2)
 *   Also P(chi2|nu)=gammp(0.5*nu,0.5*chi2)
 *    and Q(chi2|nu)=gammq(0.5*nu,0.5*chi2)
 *   are the probabilities that an observed chi-square be less than
 *   or greater than (P or Q) chi2 when there are nu degrees of freedom
 *   (terms in the chi-square sum).
 *
 * SEE ALSO: gammp, betai, ln_gamma (gamma.i), (dawson.i)
 */
{
  { local q; }
  p = gammp(a, x, q, lg);
  return q;
}

func _gammp(a, x)
{
  term = tot = total = 1./a;
  ndx = indgen(numberof(total));
  if (numberof(total) == 1) total = [total(1)];
  for (i=0 ; i<_gamm_max ; ++i) {
    a += 1.0;
    term *= x/a;
    tot += term;
    mask = abs(term) < abs(tot)*_gamm_err;
    list = where(mask);
    if (numberof(list)) total(ndx(list)) = tot(list);
    list = where(!mask);
    if (!numberof(list)) break;
    ndx = ndx(list);
    a = a(list);
    x = x(list);
    term = term(list);
    tot = tot(list);
  }
  if (numberof(list))
    error, "global variable _gamm_err or _gamm_max too small";
  if (numberof(total) == 1) total = total(1);
  return total;
}

func _gammq(a, x)
{
  r = rold = b0 = 0.*x;
  if (numberof(r) == 1) r = [r(1)];
  a0 = b1 = ren = 1. + b0;
  a1 = x;
  ndx = indgen(numberof(r));
  for (i=1 ; i<=_gamm_max ; ++i) {
    tmp = i - a;
    a0 = (a1+a0*tmp)*ren;
    b0 = (b1+b0*tmp)*ren;
    tmp = i*ren;
    a1 = x*a0+tmp*a1;
    b1 = x*b0+tmp*b1;
    mask = abs(b1-a1*rold) < abs(b1)*_gamm_err;
    list = where(mask);
    if (numberof(list)) r(ndx(list)) = b1(list)/a1(list);
    list = where(!mask);
    if (!numberof(list)) break;
    ndx = ndx(list);
    a = a(list);
    x = x(list);
    a0 = a0(list);
    a1 = a1(list);
    b0 = b0(list);
    b1 = b1(list);
    ren = 1./(a1+!a1);
    rold = b1*ren;
  }
  if (numberof(list))
    error, "global variable _gamm_err or _gamm_max too small";
  if (numberof(r) == 1) r = r(1);
  return r;
}

_gamm_err = 1.e-13;
_gamm_max = 200;

func betai(a, b, x)
/* DOCUMENT betai(a, b, x)
 *   return I_x(a,x) = int[0 to x]{ du * u^(a-1)*(1-u)^(b-1) } / beta(a,b)
 *   the incomplete beta function
 *
 *   betai(a,b,x) = 1 - betai(b,a,1-x)
 *
 *   Note that Student's t-distribution is
 *     A(t|nu) = 1 - betai(0.5*nu,0.5, nu/(nu+t^2))
 *   The F-distribution is
 *     Q(F|nu1,nu2) = betai(0.5*nu2,0.5*nu1, nu2/(nu2+F*nu1))
 *
 * SEE ALSO: gammp, gammq, ln_gamma (gamma.i), (dawson.i)
 */
{
  mask = x < (a+1.)/(a+b+2.);
  fact = 0.*mask;
  x += fact;
  a += fact;
  b += fact;
  xc = 1.-x;
  fact = double((!x) | (!xc));
  fact = (1.-fact)*exp(ln_gamma(a+b)-ln_gamma(a)-ln_gamma(b)+
                       a*log(x+fact)+b*log(xc+fact));
  list = where(mask);
  if (numberof(list)) {
    r1 = a(list);
    r1 = fact(list)*_betai(r1, b(list), x(list))/r1;
  }
  list = where(!mask);
  if (numberof(list)) {
    r = b(list);
    r = 1. - fact(list)*_betai(r, a(list), xc(list))/r;
  }
  return merge(r1, r, mask);
}

func _betai(a, b, x)
{
  r = 0.*x;
  a0 = b0 = b1 = rold = 1. + r;
  a1 = 1. - (a+b)*x/(a+1.);
  if (numberof(r) == 1) r = [r(1)];
  ndx = indgen(numberof(r));
  for (i=1 ; i<=_gamm_max ; ++i) {
    ii = double(i);
    ii2a = ii+ii+a;
    tmp = ii*(b-ii)*x/((ii2a-1)*ii2a);
    a0 = (a1+a0*tmp);
    b0 = (b1+b0*tmp);
    ii += a;
    tmp = -ii*(ii+b)*x/((ii2a+1.)*ii2a);
    a1 = a0+tmp*a1;
    b1 = b0+tmp*b1;
    mask = abs(b1-a1*rold) < abs(b1)*_gamm_err;
    list = where(mask);
    if (numberof(list)) r(ndx(list)) = b1(list)/a1(list);
    list = where(!mask);
    if (!numberof(list)) break;
    ndx = ndx(list);
    a = a(list);
    b = b(list);
    x = x(list);
    ren = 1./a1(list);
    a0 = a0(list)*ren;
    b0 = b0(list)*ren;
    b1 = rold = b1(list)*ren;
    a1 = 1.;
  }
  if (numberof(list))
    error, "global variable _gamm_err or _gamm_max too small";
  if (numberof(r) == 1) r = r(1);
  return r;
}
