/*
 * $Id: cfft2.c,v 1.1 2005-09-18 22:04:52 dhmunro Exp $
 * FFT with strides.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "cfft.h"

void cfft2(long idir, double c[], long istd, long n, long n2, double ws[])
{
  long j, i0, iw3, iw2, iw1, jw, nistd;

  if (n == 1) return;
  iw1 = n+n;
  iw2 = iw1+n+n;
  iw3 = iw2+15;
  istd *= 2;
  nistd = istd*n;
  for (; (n2--)>0 ; c+=nistd) {
    if (istd > 1) {
      for (i0=0 ; i0<istd ; i0+=2) {
        for (j=jw=0 ; j<nistd ; j+=istd,jw+=2) {
          ws[iw3+jw] = c[i0+j];
          ws[iw3+1+jw] = c[1+i0+j];
        }
        if (idir >= 0)
          cfftf1 (n,&ws[iw3],ws,&ws[iw1],(long *)&ws[iw2]);
        else
          cfftb1 (n,&ws[iw3],ws,&ws[iw1],(long *)&ws[iw2]);
        for (j=jw=0 ; j<nistd ; j+=istd,jw+=2) {
          c[i0+j] = ws[iw3+jw];
          c[1+i0+j] = ws[iw3+1+jw];
        }
      }
    } else {
      if (idir >= 0)
        cfftf1 (n,c,ws,&ws[iw1],(long *)&ws[iw2]);
      else
        cfftb1 (n,c,ws,&ws[iw1],(long *)&ws[iw2]);
    }
  }
  return;
}
