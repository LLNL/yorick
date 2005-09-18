/*
 * $Id: regul.c,v 1.1 2005-09-18 22:05:49 dhmunro Exp $
 * routine to track rays through a regular mesh
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "regul.h"

static long reg_scan(long n, real x[], real x0, int forward);

void reg_rays(long nxyz[], real **xyz, long n, real p[][3],
              real q[][3], TK_result *result)
{
  for ( ; (n--)>0 ; p++,q++) reg_track(nxyz, xyz, p[0], q[0], result);
}

void reg_track(long nxyz[], real **xyz, real p[], real q[],
               TK_result *result)
{
  int i, j, forward[3], twice;
  long inext[3], stride[3], cell;
  real snext[3], qr[3];

  /* set up for tracking, finding entry */
  for (i=0 ; i<3 ; i++) {
    if (i) stride[i]= stride[i-1]*nxyz[i];
    else   stride[i]= 1;
    if      (q[i]<-1.e-20) qr[i]= 1./q[i];
    else if (q[i]<0.)      qr[i]= -1.e20;
    else if (q[i]<1.e-20)  qr[i]= 1.e20;
    else                   qr[i]= 1./q[i];
    if (qr[i]<0.) {
      forward[i]= xyz[i][0] >= xyz[i][nxyz[i]-1];
    } else {
      forward[i]= xyz[i][0] < xyz[i][nxyz[i]-1];
    }
    inext[i]= forward[i]? 0 : nxyz[i]-1;
    snext[i]= (xyz[i][inext[i]]-p[i])*qr[i];
  }

  /* find entry point */
  i= snext[2]>snext[1]? ((snext[2]>snext[0])<<1) : (snext[1]>snext[0]);
  cell= (inext[i]+!forward[i])*stride[i];
  for (twice=2,j=i ; twice ; twice--) {
    j= j? j-1 : 2;
    inext[j]= reg_scan(nxyz[j], xyz[j], p[j]+q[j]*snext[i], forward[j]);
    if (inext[j]>=0 && cell>=0) {
      cell+= (inext[j]+!forward[j])*stride[j];
      snext[j]= (xyz[j][inext[j]]-p[j])*qr[j];
    } else {
      cell= -1;
    }
  }

  ray_store(result, cell, snext[i], 1);
  if (cell<0) return;

  /* track the ray */
  if (snext[j]>snext[3-i-j]) j= 3-i-j;
  for (;;) {
    if (forward[i]) {
      if ((++inext[i]) >= nxyz[i]) break;
      cell+= stride[i];
    } else {
      if ((--inext[i]) < 0) break;
      cell-= stride[i];
    }
    snext[i]= (xyz[i][inext[i]]-p[i])*qr[i];
    if (snext[i]>snext[j]) {
      int k= 3-i-j;
      if (snext[i]<snext[k]) k= i;
      i= j;
      j= k;
    }
    if (ray_store(result, cell, snext[i], 0)) break;
  }
}

static long reg_scan(long n, real x[], real x0, int forward)
{
  long i, m;
  if (--n<1 || x[0]==x[n]) return -1;
  m= 0;
  if (x[0]<x[n]) {
    if (x0<x[0] || x0>x[n]) return -1;
    for (i=n/2 ; i>m ; i=(m+n)/2)
      if (x[i]<x0) m= i;   /* x[m] < x0 <= x[n] */
      else         n= i;
  } else {
    if (x0>x[0] || x0<x[n]) return -1;
    for (i=n/2 ; i>m ; i=(m+n)/2)
      if (x[i]>x0) m= i;   /* x[n] <= x0 < x[m] */
      else         n= i;
  }
  return forward? n : m;
}
