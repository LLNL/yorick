/*
 * $Id: romberg.i,v 1.3 2010-08-25 14:07:16 thiebaut Exp $
 * Romberg integrator, after qromb in Numerical Recipes (Press, et.al.)
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func romberg(function, a, b, epsilon, notvector=)
/* DOCUMENT integral= romberg(function, a, b)
         or integral= romberg(function, a, b, epsilon)
     returns the integral of FUNCTION(x) from A to B.  If EPSILON is
     given, Simpson's rule is refined until that fractional accuracy
     is obtained.  EPSILON defaults to 1.e-6.

     If the notvector= keyword is supplied and non-zero, then FUNCTION
     may not be called with a list of x values to return a list of
     results.  By default, FUNCTION is assumed to be a vector function.

     If the function is not very smooth, simpson may work better.

   SEE ALSO: simpson, max_doublings
 */
{
  local _trapezoid_i, _trapezoid_s;
  if (!epsilon || epsilon<0.) epsilon= 1.e-6;
  a= double(a);
  b= double(b);
  s= array(0.0, 5);
  h= [1.0, 0.25, 0.0625, 0.015625, 0.00390625];
  for (i=1 ; i<=max_doublings ; ++i) {
    ss= trapezoid(function, a, b, i, notvector);
    if (i >= 5) {
      s(5)= ss;
      c= d= s;  /* Neville algorithm tableau */
      ns= 4;    /* one less than smallest h, always last */
      for (m=1 ; m<5 ; ++m) {
        m5= 5-m;
        ho= h(1:m5);
        hp= h(m+1:5);
        w= (c(2:m5+1)-d(1:m5))/(ho-hp);
        d(1:m5)= hp*w;
        c(1:m5)= ho*w;
        dss= (2*ns<m5)? c(ns+1) : d(ns--);
        ss+= dss;
      }
      if (abs(dss) <= epsilon*abs(ss)) return ss;
      /* extrapolation to h=0 always uses last 5 points */
      s(1:4)= s(2:5);
      h*= 0.25;
    } else {
      s(i)= ss;
    }
  }
  error, "no convergence after "+pr1(2^i)+" function evaluations";
}

func simpson(function, a, b, epsilon, notvector=)
/* DOCUMENT integral= simpson(function, a, b)
         or integral= simpson(function, a, b, epsilon)
     returns the integral of FUNCTION(x) from A to B.  If EPSILON is
     given, Simpson's rule is refined until that fractional accuracy
     is obtained.  EPSILON defaults to 1.e-6.

     If the notvector= keyword is supplied and non-zero, then FUNCTION
     may not be called with a list of x values to return a list of
     results.  By default, FUNCTION is assumed to be a vector function.

     If the function is very smooth, romberg may work better.

   SEE ALSO: romberg, max_doublings
 */
{
  local _trapezoid_i, _trapezoid_s;
  if (!epsilon || epsilon<0.) epsilon= 1.e-6;
  a= double(a);
  b= double(b);
  ost= os= -1.e-30;
  for (i=1 ; i<=max_doublings ; ++i) {
    st= trapezoid(function, a, b, i, notvector);
    s= (4.*st - ost)/3.;
    if (abs(s-os) <= epsilon*abs(os)) return s;
    os= s;
    ost= st;
  }
  error, "no convergence after "+pr1(2^i)+" function evaluations";
}

local max_doublings;
/* DOCUMENT max_doublings= 20
     is the maximum number of times romberg or simpson will split the
     integration interval in half.  Default is 20.
 */
max_doublings= 20;

func trapezoid(function, a, b, n, notvector)
{
  extern _trapezoid_i, _trapezoid_s;
  if (n==1) {
    _trapezoid_i= 1;
    return _trapezoid_s= 0.5*(b-a)*(function(a)+function(b));
  }
  dx= (b-a)/_trapezoid_i;
  if (!notvector) {
    /* conserve memory-- dont try more than 8192 points at a time */
    if (_trapezoid_i <= 8192) {
      s= sum(function(span(a,b,_trapezoid_i+1)(zcen)));
    } else {
      x= a+(indgen(8192)-0.5)*dx;
      s= sum(function(x));
      dx2= 8192*dx;
      for (i=8193 ; i<=_trapezoid_i ; i+=8192) s+= sum(function((x+=dx2)));
    }
  } else {
    x= a+0.5*dx;
    s= function(x);
    for (i=2 ; i<=_trapezoid_i ; ++i) s+= function((x+=dx));
  }
  _trapezoid_i*= 2;
  return _trapezoid_s= 0.5*(_trapezoid_s + dx*s);
}
