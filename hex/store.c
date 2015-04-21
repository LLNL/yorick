/*
 * $Id: store.c,v 1.1 2005-09-18 22:05:49 dhmunro Exp $
 * routines to store ray trace results
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "tools.h"
#include "pstdlib.h"

#ifndef TK_BLKSZ
#define TK_BLKSZ 10000
#endif

/* ------------------------------------------------------------------------ */

typedef struct TK_block TK_block;
struct TK_block {
  TK_block *next;
  real *s;          /* s[TK_BLKSZ] distances along ray */
  long cell[1];     /* cell[TK_BLKSZ] cell indices along ray */
};

struct TK_result {
  long n;           /* total number of intersections currently stored */
  long nmax;        /* total capacity of all current blocks */
  long *cell0;      /* current initial cell */
  TK_block *block;  /* current block */
  int nback;        /* infinite loop detectors */
  real sprev, smin, smax;
  TK_block block0;  /* first block */
};

/* ------------------------------------------------------------------------ */

TK_result *ray_result(void)
{
  TK_result *result= p_malloc(sizeof(TK_result)+(TK_BLKSZ-1)*sizeof(long));
  result->block0.next= 0;
  result->block0.s= 0;
  result->block= &result->block0;
  result->cell0= 0;
  result->block0.cell[0]= 0;
  result->n= 0;
  result->nmax= TK_BLKSZ;
  result->nback = 0;
  result->sprev = result->smin = result->smax = 0.;
  result->block->s= p_malloc(TK_BLKSZ*sizeof(real));
  return result;
}

void ray_reset(TK_result *result)
{
  TK_block *block;
  real *s;
  result->cell0= 0;
  result->block0.cell[0]= 0;
  while (result->block0.next) {
    block= result->block0.next;
    s= block->s;
    block->s= 0;
    p_free(s);
    result->block0.next= block->next;
    p_free(block);
  }
  result->nmax= TK_BLKSZ;
  result->n= 0;
  result->nback = 0;
  result->sprev = result->smin = result->smax = 0.;
}

void ray_free(TK_result *result)
{
  real *s= result->block0.s;
  ray_reset(result);
  result->block0.s= 0;
  p_free(s);
  p_free(result);
}

/* ------------------------------------------------------------------------ */

static void ray_discard(TK_result *result);

/* maximum number of steps to make forward progress */
static int loop_nmax = 200;

int
ray_store(TK_result *result, long cell, real s, int first)
{
  int loopflag = 0;
  static double loop_s;
  static int loop_n;
  if (first || s>loop_s) {
    loop_s = s;
    loop_n = 0;
  } else {
    loopflag = (++loop_n >= loop_nmax);
  }
  if (result) {
    long i = result->n++;
    int check_back = 1;

    if (i >= result->nmax) {
      /* time to create a new block */
      TK_block *block= p_malloc(sizeof(TK_block)+(TK_BLKSZ-1)*sizeof(long));
      block->next = 0;
      block->s = 0;
      result->block= result->block->next= block;
      block->s = p_malloc(TK_BLKSZ*sizeof(real));
      result->nmax += TK_BLKSZ;
    }
    i -= result->nmax - TK_BLKSZ;

    if (first) {
      result->cell0 = &result->block->cell[i];
      if (first==1) result->cell0[0] = 1;
      else          result->cell0[0] = -1;
      check_back = 0;
      result->nback = 0;
      result->smin = result->smax = s;
    } else {
      if (result->cell0[0]>0) result->cell0[0]++;
      else                    result->cell0[0]--;
      result->block->cell[i] = cell;
      if (check_back && result->sprev > s) {
        /* too much stepping backwards indicates an infinite loop */
        if (result->sprev > result->smax) {
          result->smax = result->sprev;
          result->nback = 0;   /* made some forward progress */
          result->smin = s;
        } else if (s < result->smin) {
          result->nback = 0;   /* made some backward progress */
          result->smin = s;
        } else {
          if (!loopflag) loopflag = (++result->nback > 10);
          if (loopflag) {
            /* discarding better than returning bogus track? */
            ray_discard(result);
            i = result->n - 1 - (result->nmax - TK_BLKSZ);
            s = -1.e35;
          }
        }
      }
    }
    result->block->s[i] = result->sprev = s;
  }
  return loopflag;
}

static void
ray_discard(TK_result *result)
{
  if (result && result->cell0) {
    TK_block *block = &result->block0, *next;
    long i, nmax = TK_BLKSZ;
    real *s;
    while (block) {
      i = result->cell0 - block->cell;
      if (i>=0 && i<TK_BLKSZ) break;
      nmax += TK_BLKSZ;
      block = block->next;
    }
    if (block) {
      /* back up to block containing cell0 for this ray chunk */
      result->block = block;
      if (result->cell0[0]>0) result->cell0[0] = 1;
      else                    result->cell0[0] = -1;
      result->n = nmax-TK_BLKSZ + i + 1;
      result->nmax = nmax;
      next = block->next;
      block->next = 0;
      while (next) {  /* remove any subsequent blocks */
        block = next;
        next = block->next;
        block->next = 0;
        s = block->s;
        block->s = 0;
        p_free(s);
        p_free(block);
      }
    } else {
      /* else serious problem, not clear what to do, s reset in caller */
    }
  }
}

long ray_collect(TK_result *result, long *cell, real *s, long orig)
{
  long n= result->n;
  if (cell) {
    long i, b;
    TK_block *block= &result->block0;
    long *rcell;
    real *rs;
    for (i=0 ; i<n ; ) {
      rs= block->s;
      rcell= block->cell;
      for (b=TK_BLKSZ ; b && i<n ; b--,i++) {
        s[i]= *rs++;
        cell[i]= *rcell++;
      }
      block= block->next;
    }
    for (i=0 ; i<n ; ) {
      b= cell[i];
      for (i++ ; i<n && (--b)>0 ; i++) cell[i]+= orig;
    }
  }
  return n;
}

/* ------------------------------------------------------------------------ */

long ray_reduce(long len, long *cell, real *s, long *nlist,
                real slims[][2])
{
  long n, i, c;
  int flip = len<0;
  if (flip) len = -len;

  if (!nlist) {
    for (n=i=0 ; i<len ; i+=c) {
      c = cell[i];
      if (!c || (!i && c<0)) c = 1;  /* illegal, garbage out */
      if (c<0) c = -c;
      else n++;
    }

  } else if (!slims) {
    long j;
    for (n=i=j=0 ; i<len ; ) {
      c = cell[i];
      if (!c || (!i && c<0)) c = 1;
      if (i+c > len) c = len-i;
      if (c<0) { c = -c;  nlist[n] += c-1; }
      else     { nlist[n++] = c-1; }
      for (i++ ; --c ; i++,j++) {
        cell[j] = cell[i];
        s[j] = s[i]-s[i-1];
      }
    }

  } else {
    long j;
    real sn=0., sx=0., s0, s1;
    for (n=i=j=0 ; i<len ; ) {
      c = cell[i];
      if (!c || (!i && c<0)) c = 1;
      if (i+c > len) c = len-i;
      if (c<0) { c = -c;  nlist[n] += c-1; }
      else     { sn = slims[n][0];  sx = slims[n][1];  nlist[n++] = c-1; }
      for (s0=s[i++] ; --c ; i++,s0=s1) {
        s1 = s[i];
        if (s0<sn) {
          if (s1<=sn) {
            nlist[n-1]--;
            continue;
          }
          s0 = sn;
        }
        if (s1>sx) {
          if (s0>=sx) {
            nlist[n-1]--;
            continue;
          }
          s1 = sx;
        }
        cell[j] = cell[i];
        s[j] = s1-s0;
        j++;
      }
    }
  }

  if (flip && nlist) {
    long j, k, ctmp;
    real stmp;
    for (i=0 ; i<n ; i++) {
      c= nlist[i];
      for (j=0,k=c-1 ; j<k ; j++,k--) {
        ctmp= cell[j];
        stmp= s[j];
        cell[j]= cell[k];
        s[j]= s[k];
        cell[k]= ctmp;
        s[k]= stmp;
      }
      cell+= c;
      s+= c;
    }
  }

  return n;
}

/* ------------------------------------------------------------------------ */
