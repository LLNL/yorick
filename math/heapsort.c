/*
 * $Id: heapsort.c,v 1.2 2008-04-13 19:18:03 dhmunro Exp $
 * heapsort sorting routines
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "heapsort.h"
#include <string.h>

/* disaster if s<=0, but n<=0 is okay */

void hs_d_sort(long n, long s, double *vals, long *ndxs)
{
  double v;
  long w, i, j;
  long k = (n/2)*s;
  n *= s;
  if (n <= 0) return;

  for (;;) {
    if (k > 0) {
      k -= s;
      w = ndxs[k];
      i = k;
    } else {
      n -= s;
      if (n <= 0) break;
      w = ndxs[n];
      ndxs[n] = ndxs[0];
      i = 0;
    }
    v = vals[w];

    for (j=i+i+s ; j<n ; j=j+j+s) {
      if (j<n-s && vals[ndxs[j]]<vals[ndxs[j+s]]) j += s;
      if (v >= vals[ndxs[j]]) break;
      ndxs[i] = ndxs[j];
      i = j;
    }
    ndxs[i] = w;
  }
}

void hs_l_sort(long n, long s, long *vals, long *ndxs)
{
  long v;
  long w, i, j;
  long k = (n/2)*s;
  n *= s;
  if (n <= 0) return;

  for (;;) {
    if (k > 0) {
      k -= s;
      w = ndxs[k];
      i = k;
    } else {
      n -= s;
      if (n <= 0) break;
      w = ndxs[n];
      ndxs[n] = ndxs[0];
      i = 0;
    }
    v = vals[w];

    for (j=i+i+s ; j<n ; j=j+j+s) {
      if (j<n-s && vals[ndxs[j]]<vals[ndxs[j+s]]) j += s;
      if (v >= vals[ndxs[j]]) break;
      ndxs[i] = ndxs[j];
      i = j;
    }
    ndxs[i] = w;
  }
}

void hs_q_sort(long n, long s, char **vals, long *ndxs)
{
  char *v, vj, vj1;
  long w, i, j;
  long k = (n/2)*s;
  n *= s;
  if (n <= 0) return;

  for (;;) {
    if (k > 0) {
      k -= s;
      w = ndxs[k];
      i = k;
    } else {
      n -= s;
      if (n <= 0) break;
      w = ndxs[n];
      ndxs[n] = ndxs[0];
      i = 0;
    }
    v = vals[w];

    for (j=i+i+s ; j<n ; j=j+j+s) {
      vj = vals[ndxs[j]];
      if (j<n-s) {
        vj1 = vals[ndxs[j+s]];
        if (vj1 && strcmp(vj,vj1)<0) vj=vj1, j+=s;
      }
      if (!vj || (v && strcmp(v,vj)>=0)) break;
      ndxs[i] = ndxs[j];
      i = j;
    }
    ndxs[i] = w;
  }
}
