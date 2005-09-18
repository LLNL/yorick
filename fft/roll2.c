/*
 * $Id: roll2.c,v 1.1 2005-09-18 22:04:53 dhmunro Exp $
 * In place roll function for dealing with FFT results.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "cfft.h"

/*-----Fortran intrinsics converted-----*/
#define iabs(x) ((x)>=0?(x):-(x))
/*-----end of Fortran intrinsics-----*/



void roll2(double a[], long ioff, long istd, long n, long n2, double ws[])
{
#undef ws_1
#define ws_1(a1) ws[a1-1]
#undef a_3
#define a_3(a1,a2,a3) a[a1-1+istd*(a2-1+n*(a3-1))]
  /*c
    c     a      double precision a_3(istd,n,n2)
    c            to be "rolled", in place, on its middle dimension
    c
    c     ioff   the offset by which to "roll" a_3 (iabs(ioff) .lt. n)
    c            for ioff>0:
    c               output a_3(1+ioff)=  input a_3(1)
    c               output a_3(2+ioff)=  input a_3(2)
    c                      ...         ...
    c               output a_3(n-1)=     input a_3(n-ioff-1)
    c               output a_3(n)=       input a_3(n-ioff)
    c               output a_3(1)=       input a_3(n-ioff+1)
    c               output a_3(2)=       input a_3(n-ioff+2)
    c                      ...         ...
    c               output a_3(ioff-1)=  input a_3(n-1)
    c               output a_3(ioff)=    input a_3(n)
    c            for ioff<0:
    c               output a_3(1)=        input a_3(1-ioff)
    c               output a_3(2)=        input a_3(2-ioff)
    c                      ...          ...
    c               output a_3(n+ioff-1)= input a_3(n-1)
    c               output a_3(n+ioff)=   input a_3(n)
    c               output a_3(n+ioff+1)= input a_3(1)
    c               output a_3(n+ioff+2)= input a_3(2)
    c                      ...          ...
    c               output a_3(n-1)=      input a_3(-ioff-1)
    c               output a_3(n)=        input a_3(-ioff)
    c
    c     istd   stride of values in a, that is, length of dimensions
    c            of a before the one of length n
    c
    c     n2     length of dimensions of a after the one of length n
    c
    c     ws     double precision ws_1(n) of working space
    c*/
  /*c
    c     internal variables
    c*/
  long i, i2, j, joff;

  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if (ioff == 0) goto L_100;
  if (ioff < 0) goto L_50;
  joff= n-ioff;
  for (i2=1 ; i2<=n2 ; i2+=1) {
    for (i=1 ; i<=istd ; i+=1) {
      for (j=1 ; j<=joff ; j+=1) {
        ws_1(j+ioff)= a_3(i, j, i2);
      }
      for (j=1 ; j<=ioff ; j+=1) {
        ws_1(j)= a_3(i, j+joff, i2);
      }
      for (j=1 ; j<=n ; j+=1) {
        a_3(i, j, i2)= ws_1(j);
      }
    }
  }
  goto L_100;

 L_50:
  joff= n+ioff;
  for (i2=1 ; i2<=n2 ; i2+=1) {
    for (i=1 ; i<=istd ; i+=1) {
      for (j=1 ; j<=-ioff ; j+=1) {
        ws_1(j+joff)= a_3(i, j, i2);
      }
      for (j=1 ; j<=joff ; j+=1) {
        ws_1(j)= a_3(i, j-ioff, i2);
      }
      for (j=1 ; j<=n ; j+=1) {
        a_3(i, j, i2)= ws_1(j);
      }
    }
  }

 L_100:
  return;
}
