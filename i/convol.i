/*
 * $Id: convol.i,v 1.1 2005-09-18 22:06:14 dhmunro Exp $
 * convolution of two vectors using fft
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func convol(a,b,n0=,n1=)
/* DOCUMENT convol(a,b)

     returns convolution of vector a with vector b, a vector
     of length na+nb-1 where na=numberof(a), nb=numberof(b).

     In detail, for i=[1 to na+nb-1]
       result(i) = sum j=[max(1,1+i-nb) to min(na,i)] (a(j)*b(1+i-j))

     The n0= and n1= keywords can be used to control the section of
     the full array that is actually returned, 1<=n0<n1<=na+nb-1.

   SEE ALSO: fft_good, fft
 */
{
  na= numberof(a);
  nb= numberof(b);
  nc= na+nb-1;
  nt= fft_good(nc);
  at= bt= array(0i, nt);
  at(1:na)= a;
  bt(1:nb)= b;
  work= fft_setup(dimsof(at));
  fft_inplace, at, 1, setup=work;
  fft_inplace, bt, 1, setup=work;
  c= at*bt;
  fft_inplace, c, -1, setup=work;
  if (is_void(n0)) n0= 1;
  if (is_void(n1)) n1= nc;
  return double(c(n0:n1))/nt;
}

func fft_good(n)
/* DOCUMENT fft_good(n)

     returns the smallest number of the form 2^x*3^y*5^z greater
     than or equal to n.  An fft of this length will be much faster
     than a number with larger prime factors; the speed difference
     can be an order of magnitude or more.

     For n>100, the worst cases result in a little over a 11% increase
     in n; for n>1000, the worst are a bit over 6%; still larger n are
     better yet.  The median increase for n<=10000 is about 1.5%.

   SEE ALSO: fft, fft_setup, convol
 */
{
  if (n<7) return max(n,1);
  logn= log(n);
  n5= 5.^indgen(0:long(logn/log(5.) + 1.e-6));  /* exact integers */
  n3= 3.^indgen(0:long(logn/log(3.) + 1.e-6));  /* exact integers */
  n35= n3*n5(-,);             /* fewer than 300 numbers for n<5e9 */
  n35= n35(where(n35<=n));
  n235= 2^long((logn-log(n35))/log(2.) + 0.999999) * long(n35);
  return min(n235);
}

/*
func convol_check(a,b)
{
  na= numberof(a);  nb= numberof(b);  c= array(0.,na+nb-1);
  for (i=1 ; i<na+nb ; i++)
    for (j=max(1,1+i-nb) ; j<=min(na,i) ; j++) c(i)+= a(j)*b(1+i-j);
  return c;
}
*/
