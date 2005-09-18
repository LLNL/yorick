/*
 * $Id: lecuyer.c,v 1.1 2005-09-18 22:03:44 dhmunro Exp $
 * small, fast, portable, certified good random number generator
 * L'Ecuyer, Mathematics of Computation, 65, pp 203-213 (1996)
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "lecuyer.h"

/* three non-zero uncertified but most likely random numbers */
unsigned long le_generator[3] = { 0x9ad5e0d7, 0xf08ff6d8, 0xe7ed3a47 };

unsigned long
le_next(unsigned long *g)
{
  if (!g) g = le_generator;
  {
    unsigned long s1 = g[0];
    unsigned long s2 = g[1];
    unsigned long s3 = g[2];
    unsigned long b = (((s1 & 0x0007ffff) << 13) ^ s1) >> 19;
    s1 = ((s1 & 0x000ffffe) << 12) ^ b;
    b = (((s2 & 0x3fffffff) << 2) ^ s2) >> 25;
    s2 = ((s2 & 0x0ffffff8) << 4) ^ b;
    b = (((s3 & 0x1fffffff) << 3) ^ s3) >> 11;
    s3 = ((s3 & 0x00007ff0) << 17) ^ b;
    g[0] = s1;
    g[1] = s2;
    g[2] = s3;
    return s1 ^ s2 ^ s3;
  }
}

double
le_random(unsigned long *g)
{
  /* 1/(2^32-1) */
  return 2.3283064370807974e-10*(le_next(g)-0.5001);
}

void
le_nrandom(unsigned long *g, long n, double *r)
{
  if (!g) g = le_generator;
  {
    unsigned long s1 = g[0];
    unsigned long s2 = g[1];
    unsigned long s3 = g[2];
    unsigned long b;
    while ((n--)>0) {
      b = (((s1 & 0x0007ffff) << 13) ^ s1) >> 19;
      s1 = ((s1 & 0x000ffffe) << 12) ^ b;
      b = (((s2 & 0x3fffffff) << 2) ^ s2) >> 25;
      s2 = ((s2 & 0x0ffffff8) << 4) ^ b;
      b = (((s3 & 0x1fffffff) << 3) ^ s3) >> 11;
      s3 = ((s3 & 0x00007ff0) << 17) ^ b;
      *(r++)= 2.3283064370807974e-10*((s1^s2^s3)-0.5001);
    }
    g[0] = s1;
    g[1] = s2;
    g[2] = s3;
  }
}

void
le_iseed(unsigned long *g, unsigned long seed)
{
  /* initialize the L'Ecuyer random sequence at an arbitrary place
   * 0 starts at a "standard" place */
  if (!g) g = le_generator;
  g[0] = 0x9ad5e0d7;
  g[1] = 0xf08ff6d8;
  g[2] = 0xe7ed3a47;
  seed &= 0xffffffff;
  if (seed) {
    g[seed&1] = seed;  /* never get same sequence as seed=0 */
    le_next(g);
    le_next(g);
    le_next(g);
  }
}

void
le_rseed(unsigned long *g, double seed)
{
  /* 2^32-1 */
  unsigned long lseed = (seed<=0. || seed>=1.)? 0 :
    ((unsigned long)(4294967295.0*seed) + 1);
  le_iseed(g, lseed);
}
