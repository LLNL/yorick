/*
 * $Id: zroots.i,v 1.1 2005-09-18 22:06:13 dhmunro Exp $
 * Laguerre's method for finding roots of complex polynomial.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func zroots(a, imsort=)
/* DOCUMENT zroots(a)
     returns the vector of (complex) roots of the (complex) 
     polynomial: Sum(a(i)*x^(i-1)) using Laguerre's method.
     The roots are sorted in order of increasing real parts.  Call with
     non-zero imsort keyword to sort into increasing imaginary parts.
     See Numerical Recipes (Press, et. al., Cambridge Univ. Press 1988),
     section 9.5.
 */
{
  m= numberof(a)-1;   /* degree of poly */
  EPS= 1.e-6;
  EPS*= 2.*EPS;
  is_complex= (structof(a)==complex);
  if (!is_complex) a= a+0i;
  ad= a;              /* copy of coeffs for successive deflation */
  roots= array(0i, m);

  for (j=m ; j>=1 ; j--) {
    /* Loop over each root to be found */
    x= laguerre(ad,0i);     /* start at zero to favor smallest rem root */
    if (abs(x.im)<=EPS*abs(x.re)) x= x.re+0i;
    roots(j)= x;
    b= ad(j+1);             /* forward deflation */
    for (jj=j ; jj>=1 ; jj--) {
      c= ad(jj);
      ad(jj)= b;
      b= x*b+c;
    }
    ad= ad(1:j);            /* poly has one lower degree */
  }

  /* polish */
  for (j=1 ; j<=m ; j++) roots(j)= laguerre(a,roots(j));

  /* sort roots */
  if (numberof(roots) == 1) return roots;
  re= roots.re;
  im= roots.im;
  if (!is_complex &&
      allof(abs(im)<=EPS*abs(re))) return re(sort(re));

  /* sorting on multiple keys is difficult because all fast sorting
     algorithms (such as the quicksort used by sort) randomize the
     order of equal elements in the list to be sorted -- the following
     is a modification of the general deterministic sorting algorithm
     implemented in msort.i, modified to ensure that conjugate roots
     are recognized even when their real parts are not quite equal */
  if (imsort) {
    rank= re;
    re= im;
    im= rank;
  }
  list= rank= sort(re);
  re= re(list);
  rank(list)= (re(dif)>EPS*abs(re(zcen)))(cum);  /* see msort.i */
  list= rerank= sort(im);
  rerank(list)= indgen(m);
  return roots(sort(rank+double(rerank)/m));
}

func laguerre(a,x)
/* DOCUMENT laguerre(a,x)
     Given the coefficients a(1:m+1) of the m'th degree
     complex polynomial Sum(a(i)*x^(i-1)) and a guess x, returns a root.
     See Numerical Recipes (Press, et. al., Cambridge Univ. Press 1988),
     section 9.5.
 */
{
  EPSS=3.e-15;
  MR=8;
  MT=10;
  MAXIT=MT*MR;
  frac=[.5,.25,.75,.13,.38,.62,.88,1.]; /* Fractions to break limit cycles */

  m= numberof(a)-1;       /* degree of poly */
  a= a+0i;                /* make complex */
  if (m<2) return -a(1)/a(2);

  for (iter=1 ; iter<=MAXIT ; iter++) {
    b= a(0);                    /* coefft of max degree */
    d= f= 0i;
    err= abs(b);
    abx=abs(x);
    for (j=m ; j>=1 ; j--) {    /* compute poly and 2 derivs */
      f= f*x+d;
      d= d*x+b;
      b= b*x+a(j);
      err= abs(b)+abx*err;
    }
    err*= EPSS;                 /* estimated round-off error in poly */
    if (abs(b)<=err) return x;  /* we're on the root already */

    g= d/b;
    g2= g*g;
    h= g2-2.*f/b;
    sq= sqrt((m-1)*(m*h-g2));
    gp= g+sq;
    gm= g-sq;
    abp= abs(gp);
    abm= abs(gm);
    if (abp<abm) gp= gm;

    if (gp) dx= m/gp;
    else dx= exp(log(1+abx)+1.i*iter);

    x1= x-dx;
    if (abs(x-x1)<EPSS) return x1; /* converged to machine accuracy */

    if (iter%MT) x= x1;            /* next iteration */
    else x= x-dx*frac(iter/MT);    /* break limit cycle? */
  }
  write, "WARNING: too many iterations in laguerre";
  return x;
}
      


