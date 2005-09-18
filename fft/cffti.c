/*
 * $Id: cffti.c,v 1.1 2005-09-18 22:04:52 dhmunro Exp $
 * Swarztrauber complex FFT routines (initialization).
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "cfft.h"

/*-----Fortran intrinsics converted-----*/
extern double sin(double);
extern double cos(double);
/*-----end of Fortran intrinsics-----*/



void cffti (long n,double wsave[])
{
  /*      implicit double  (a-h,o-z);*/
#undef wsave_1
#define wsave_1(a1) wsave[a1-1]
  /*-----implicit-declarations-----*/
  long iw2;
  long iw1;
  /*-----end-of-declarations-----*/
  if (n == 1) return;
  iw1 = n+n+1;
  iw2 = iw1+n+n;
  cffti1 (n,&wsave_1(iw1),(long *)&wsave_1(iw2));
  return;
}



void cffti1 (long n,double wa[],long ifac[])
{
  /*      implicit double  (a-h,o-z);*/
#undef wa_1
#define wa_1(a1) wa[a1-1]
#undef ntryh_1
#define ntryh_1(a1) ntryh[a1-1]
#undef ifac_1
#define ifac_1(a1) ifac[a1-1]
  static       long ntryh[4]={3,4,2,5};
  /*-----implicit-declarations-----*/
  double arg;
  long ii;
  double argld;
  double fi;
  long i1;
  long ipm;
  long idot;
  long ido;
  long l2;
  long ld;
  long ip;
  long k1;
  long l1;
  double argh;
  double tpi;
  long ib;
  long i;
  long nr;
  long nq;
  long ntry= 0;
  long j;
  long nf;
  long nl;
  /*-----end-of-declarations-----*/
  nl = n;
  nf = 0;
  j = 0;
  do {
    j = j+1;
    if (j<=4) ntry = ntryh_1(j);
    else ntry = ntry+2;
  L_104: nq = nl/ntry;
    nr = nl-ntry*nq;
  } while (nr!=0);
  nf = nf+1;
  ifac_1(nf+2) = ntry;
  nl = nq;
  if (ntry != 2) goto L_107;
  if (nf == 1) goto L_107;
  for (i=2 ; i<=nf ; i+=1) {
    ib = nf-i+2;
    ifac_1(ib+2) = ifac_1(ib+1);
  }
  ifac_1(3) = 2;
 L_107: if (nl != 1) goto L_104;
  ifac_1(1) = n;
  ifac_1(2) = nf;
  /* Original Swarztrauber constants rather imprecise
     -- thanks to Eric Thiebaut for better values
     tpi = 6.28318530717959;
  */
  tpi = 6.283185307179586476925286766559005768394;
  argh = tpi/n;
  i = 2;
  l1 = 1;
  for (k1=1 ; k1<=nf ; k1+=1) {
    ip = ifac_1(k1+2);
    ld = 0;
    l2 = l1*ip;
    ido = n/l2;
    idot = ido+ido+2;
    ipm = ip-1;
    for (j=1 ; j<=ipm ; j+=1) {
      i1 = i;
      wa_1(i-1) = 1.;
      wa_1(i) = 0.;
      ld = ld+l1;
      fi = 0.;
      argld = ld*argh;
      for (ii=4 ; ii<=idot ; ii+=2) {
        i = i+2;
        fi = fi+1.;
        arg = fi*argld;
        wa_1(i-1) = cos(arg);
        wa_1(i) = sin(arg);
      }
      if (ip <= 5) continue;
      wa_1(i1-1) = wa_1(i-1);
      wa_1(i1) = wa_1(i);
    }
    l1 = l2;
  }
  return;
}
