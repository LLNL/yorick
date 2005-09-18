/*
 * $Id: ellipse.i,v 1.1 2005-09-18 22:05:55 dhmunro Exp $
 * Complete elliptic integrals.
 * (Maybe someday can find cn, sn, dn algorithms...)
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* ------------------------------------------------------------------------ */

func EllipticK(k)
/* DOCUMENT EllipticK(k)
   return E(k), the complete Elliptic Function 
   integral from 0 to pi/2 of  1/sqrt(1-k^2 *(sin(x))^2) dx
   uses the arithmetic/geometric mean method. (Abramowitz & Stegun p598)
   k must lie in   -1 < k <1.

   E(k) diverges as log(4/sqrt(1-k^2)) as k->1

   SEE ALSO: EllipticE
 */
{
  EPS= 1.E-15;
  a= 1.0;
  b= sqrt(1.0-k*k);
  c= abs(k);
  do {
    an= 0.5*(a+b);
    bn= sqrt(a*b);
    cn= 0.5*(a-b);
    a= an;
    b= bn;
    c= cn;
  } while (anyof(abs(c)>EPS));

  return 0.5*pi/a;
}

func EllipticE(k)
/* DOCUMENT EllipticE(k)
     return E(k), the complete Elliptic Function
     integral from 0 to pi/2 of  sqrt(1-k^2 *(sin(x))^2) dx
     k must lie in -1<= k <=1

     Uses the arithmetic/geometric mean method. (Abramowitz & Stegun p598) 

   SEE ALSO: EllipticK
 */
{
  EPS= 1.E-15;

  /* Separate out k=1 since K diverges and d->1 */
  mask= (abs(k)==1.0);

  list= where(mask);
  if (numberof(list)) E1= array(1.0,numberof(list));

  list= where(!mask);
  if (numberof(list)) {
    c= k(list);
    a= 1.0;
    b= sqrt(1.0-c*c);
    d= 0.0;
    twofact= 0.5;
    do {
      an= 0.5*(a+b);
      bn= sqrt(a*b);
      cn= 0.5*(a-b);
      dn= d + twofact*c*c;
      twofact*= 2.0;
      a= an;
      b= bn;
      c= cn;
      d= dn;
    } while (anyof(abs(c)>EPS));
    En1= 0.5*pi*(1.0-d)/a;
  }

  /* merge k=1 and k!=1 results */
  return merge(E1,En1,mask);
}

/* ------------------------------------------------------------------------ */
