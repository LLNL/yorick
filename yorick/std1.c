/*
 * $Id: std1.c,v 1.4 2010-10-12 01:42:23 dhmunro Exp $
 * More Yorick built-in functions declared in std.i
 *
 *  See std.i for documentation on the functions defined here.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "ydata.h"
#include "pstdlib.h"
#include "play.h"
#include <string.h>
/* ANSI C time.h for Y_timestamp */
#include <time.h>

extern BuiltIn Y_indgen, Y_span, Y_digitize, Y_interp, Y_integ, Y_sort,
  Y_transpose, Y_grow, Y__, Y_timestamp, Y_timer,
  Y_random, Y_random_seed, Y_merge, Y_histogram, Y_poly, Y_noop;

/*--------------------------------------------------------------------------*/

extern void PopToD(Symbol *s);
extern DataBlock *ForceToDB(Symbol *s);

/* Intended for use by the print() and grow() functions -- dangerous
   because it zeroes the contents of the source array to avoid
   having to deal with pointers.  */
extern Array *GrowArray(Array *array, long extra);  /* ydata.c */

static long hunt(double *x, long n, double xp, long ip);

/*--------------------------------------------------------------------------*/

void Y_indgen(int nArgs)
{
  long number, origin, stride, i;
  Array *array;
  Dimension *tmp;
  Operand op;
  if (nArgs != 1) YError("indgen takes exactly one argument");

  sp->ops->FormOperand(sp, &op);
  if (op.ops==&rangeOps) {
    Range *range= op.value;
    if (range->rf || range->nilFlags)
      YError("range function and/or nil range component in indgen");
    origin= range->min;
    stride= range->inc;
    if (stride>0) number= (range->max-origin)/stride;
    else number= (origin-range->max)/(-stride);
    number++;  /* number of footprints, not number of strides */
  } else if (op.ops->promoteID<=T_LONG && !op.type.dims) {
    op.ops->ToLong(&op);
    number= *(long *)op.value;
    origin= 1L;
    stride= 1;
  } else {
    YError("indgen argument must be range or scalar integer");
    return;
  }

  if (number>0) {
    tmp= tmpDims;
    tmpDims= 0;
    FreeDimension(tmp);
    tmpDims= NewDimension(number, 1L, (Dimension *)0);
    array= PushDataBlock(NewArray(&longStruct, tmpDims));

    for (i=0 ; i<number ; i++) {
      array->value.l[i]= origin;
      origin+= stride;
    }

  } else {
    /* indgen(0) returns default origin */
    PushLongValue(1L);
  }
}

void Y_span(int nArgs)
{
  long which, nDims, number, nfast, i, j, k, kx;
  double rnumber, dp, *p0, *p1, *p;
  Array *array;
  Dimension *tmp;
  Operand op0, op1;
  if (nArgs!=3 && nArgs!=4)
    YError("span takes exactly three or four arguments");

  if (nArgs==4) { which= YGetInteger(sp)-1; Drop(1); }
  else which= 0;  /* use 0-origin which here */

  number= YGetInteger(sp);
  if (number<1) YError("3rd argument to span function must be >0");
  Drop(1);

  sp->ops->FormOperand(sp, &op1);
  (sp-1)->ops->FormOperand(sp-1, &op0);
  op1.ops->ToDouble(&op1);
  op0.ops->ToDouble(&op0);
  if (BinaryConform(&op1, &op0) & 4)
    YError("start and stop not conformable in span function");

  tmp= tmpDims;
  tmpDims= 0;
  FreeDimension(tmp);

  /* compute dimensions of result -- nfast-by-number-by-(slow),
     where nfast are the first which indices of op0 (or op1),
     and (slow) are the rest */
  nDims= CountDims(op0.type.dims);
  if (which<0) which= nDims+1+which; /* handle which<0 like array index<0 */
  if (which>8 || nDims-which>8)
    YError("the 4th argument to span is unreasonably large");
  while (which<0) {
    tmpDims= NewDimension(1L, 1L, tmpDims);
    which++;
  }
  if (which>=nDims) {
    /* can just tack new element of dimension list onto old */
    tmpDims= Ref(op0.type.dims);
    while (which>nDims) {
      tmpDims= NewDimension(1L, 1L, tmpDims);
      which--;
    }
    tmpDims= NewDimension(number, 1L, tmpDims);
    nfast= op0.type.number;
  } else {
    /* make a fresh copy of the index list, then insert new element */
    Dimension *prev;

    which= nDims-which;  /* guaranteed this is >0 */
    tmpDims= tmp= CopyDims(op0.type.dims, tmpDims, 1);
    do {
      prev= tmp;
      tmp= tmp->next;
    } while (--which);
    prev->next= NewDimension(number, 1L, tmp);
    nfast= TotalNumber(tmp);
  }

  /* create result array and fill it */
  array= PushDataBlock(NewArray(&doubleStruct, tmpDims));
  p= array->value.d;
  p0= op0.value;
  p1= op1.value;
  rnumber= 1.0/(double)(number-1);
  kx= array->type.number - nfast;
  for (i=0 ; i<op0.type.number ; i+=nfast) {
    for (j=0 ; j<nfast ; j++) {
      dp= (p1[i+j]-p0[i+j])*rnumber;
      k= j+number*i;
      p[k]= p0[i+j];
      for (k+=nfast ; k<kx ; k+=nfast) p[k]= p[k-nfast]+dp;
      p[k]= p1[i+j];  /* do after loop to assure exact equality */
    }
  }
}

static long hunt(double *x, long n, double xp, long ip)
{
  /* Based on the hunt routine given in Numerical Recipes (Press, et al.,
     Cambridge University Press, 1988), section 3.4.

     Here, x[n] is a monotone array and, if xp lies in the interval
     from x[0] to x[n-1], then
       x[h-1] <= xp < x[h]  (h is the value returned by hunt), or
       x[h-1] >= xp > x[h], as x is ascending or descending
     The value 0 or n will be returned if xp lies outside the interval.
   */
  int ascend= x[n-1]>x[0];
  long jl, ju;

  if (ip<1 || ip>n-1) {
    /* caller has declined to make an initial guess, so fall back to
       garden variety bisection method */
    if ((xp>=x[n-1]) == ascend) return n;
    if ((xp<x[0]) == ascend) return 0;
    jl= 0;
    ju= n-1;

  } else {
    /* search from initial guess ip in ever increasing steps to bracket xp */
    int inc= 1;
    jl= ip;
    if ((xp>=x[ip]) == ascend) { /* search toward larger index values */
      if (ip==n-1) return n;
      jl= ip;
      ju= ip+inc;
      while ((xp>=x[ju]) == ascend) {
        jl= ju;
        inc+= inc;
        ju+= inc;
        if (ju>=n) {
          if ((xp>=x[n-1]) == ascend) return n;
          ju= n;
          break;
        }
      }
    } else {                     /* search toward smaller index values */
      if (ip==0) return 0;
      ju= ip;
      jl= ip-inc;
      while ((xp<x[jl]) == ascend) {
        ju= jl;
        inc+= inc;
        jl-= inc;
        if (jl<0) {
          if ((xp<x[0]) == ascend) return 0;
          jl= 0;
          break;
        }
      }
    }
  }

  /* have x[jl]<=xp<x[ju] for ascend, x[jl]>=xp>x[ju] for !ascend */
  while (ju-jl > 1) {
    ip= (jl+ju)>>1;
    if ((xp>=x[ip]) == ascend) jl= ip;
    else ju= ip;
  }

  return ju;
}

void Y_digitize(int nArgs)
{
  long number, origin, nbins, i, ip;
  double *x, *bins;
  Dimension *dimsx, *dimsb;
  long *ibin;
  if (nArgs!=2) YError("digitize takes exactly two arguments");

  bins= YGet_D(sp, 0, &dimsb);
  x= YGet_D(sp-1, 0, &dimsx);

  if (!dimsb || dimsb->number<2 || dimsb->next)
    YError("2nd argument to digitize must be 1D with >=2 elements");
  nbins= dimsb->number;
  origin= dimsb->origin;
  number= TotalNumber(dimsx);

  if (dimsx) {
    Array *array= PushDataBlock(NewArray(&longStruct, dimsx));
    ibin= array->value.l;
  } else {
    PushLongValue(0L);
    ibin= &sp->value.l;
  }
  ip= 0;
  for (i=0 ; i<number ; i++)
    ibin[i]= ip= origin+hunt(bins, nbins, x[i], ip);
}

void Y_interp(int nArgs)
{
  long which, nDims, number, nfast;
  long i, j, k, l, m, js, ms, ip, ipy;
  double *y, *x, *xp, *yp, c0, c1;
  Array *array;
  Dimension *tmp;
  Operand opy, opx, opxp;
  if (nArgs!=3 && nArgs!=4)
    YError("interp takes exactly three or four arguments");

  if (nArgs==4) { which= YGetInteger(sp)-1; Drop(1); }
  else which= 0;  /* use 0-origin which here */

  sp->ops->FormOperand(sp, &opxp);
  (sp-1)->ops->FormOperand(sp-1, &opx);
  (sp-2)->ops->FormOperand(sp-2, &opy);
  opxp.ops->ToDouble(&opxp);
  opx.ops->ToDouble(&opx);
  opy.ops->ToDouble(&opy);

  tmp= tmpDims;
  tmpDims= 0;
  FreeDimension(tmp);

  /* compute dimensions of y array -- nfast-by-number-by-(slow), where
     nfast are the first which dimensions of y, number is the length of
     the dimension to interpolate on, and (slow) are the rest */
  nDims= CountDims(opy.type.dims);
  if (which<0) which= nDims+which; /* handle which<0 like array index<0 */
  if (which>=nDims || which<0) YError("bad 4th argument to interp");
  i= nDims-1-which;
  tmp= opy.type.dims;
  while (i) { tmp= tmp->next; i--; }
  number= tmp? tmp->number : 1;
  if (number<2) YError("bad dimension (length 1) in interp");
  nfast= TotalNumber(tmp->next);
  if (opx.type.number!=number || opx.type.dims->next)
    YError("dimension of x does not match target dimension of y in interp");

  if (which==nDims-1) {
    /* can just tack new element(s) of dimension list onto old */
    tmpDims= Ref(opy.type.dims->next);
    tmpDims= CopyDims(opxp.type.dims, tmpDims, 1);
  } else {
    /* make a fresh copy of the index list and find where to insert new */
    Dimension *prev, *tmpprev;
    i= nDims-which;   /* guaranteed >1 AND opy is at least 2D */
    tmpDims= prev= CopyDims(opy.type.dims, tmpDims, 1);
    tmp= prev->next;  i--;
    tmp= tmp->next;  i--;   /* prev is two behind tmp */
    while (i) { prev= prev->next; tmp= tmp->next; i--; }
    tmpprev= prev->next;
    prev->next= 0;     /* cut off tail of dimension list */
    tmpprev->next= 0;  /* this pointed to tmp */
    FreeDimension(tmpprev);
    prev->next= CopyDims(opxp.type.dims, tmp, 1);
  }

  /* create result array */
  array= PushDataBlock(NewArray(&doubleStruct, tmpDims));
  yp= array->value.d;
  y= opy.value;
  x= opx.value;
  xp= opxp.value;

  /* The problem is as follows:
     For each value of the faster indices, and each value of the slower
     indices of y, and for each value of xp, find the value of yp
     corresponding to xp.  Note that for each interpolation, the y
     vector has stride nfast, while the x vector always has stride 0.
     Note also that ALL of the yp for a given xp should be computed
     once xp is found.  */
  js= nfast*number;
  ms= nfast*opxp.type.number;
  c0= c1= 0.0;
  ip= 0;  /* hunt takes this as no guess on 1st pass */
  for (i=l=0 ; i<opxp.type.number ; i++, l+=nfast) {
    ip= hunt(x, number, xp[i], ip);
    ipy= ip*nfast;
    if (ip>=1 && ip<number) {
      c0= (x[ip]-xp[i])/(x[ip]-x[ip-1]);
      c1= 1.0-c0;
    }
    for (j=m=0 ; j<opy.type.number ; j+=js, m+=ms) {
      for (k=0 ; k<nfast ; k++) {
        if (ip<1) {               /* point below minimum */
          yp[k+l+m]= y[k+j];
        } else if (ip<number) {   /* point in range */
          yp[k+l+m]= c0*y[k+(ipy-nfast)+j]+c1*y[k+ipy+j];
        } else {                  /* point above maximum */
          yp[k+l+m]= y[k+(ipy-nfast)+j];
        }
      }
    }
  }

  PopToD(sp-4);
  Drop(3);
}

void Y_integ(int nArgs)
{
  long which, nDims, number, nfast;
  long i, j, k, l, m, js, ms, ip, ipy;
  double *y, *x, *xp, *yi, *psum, c0, c1, dx;
  Array *array;
  Dimension *tmp;
  Operand opy, opx, opxp;
  if (nArgs!=3 && nArgs!=4)
    YError("integ takes exactly three or four arguments");

  if (nArgs==4) {
    which= YGetInteger(sp)-1;  /* use 0-origin which here */
    Drop(1);
  } else {
    which= 0;  /* use 0-origin which here */
    ClearTmpArray();  /* need a temporary for this calculation */
  }

  sp->ops->FormOperand(sp, &opxp);
  (sp-1)->ops->FormOperand(sp-1, &opx);
  (sp-2)->ops->FormOperand(sp-2, &opy);
  opxp.ops->ToDouble(&opxp);
  opx.ops->ToDouble(&opx);
  opy.ops->ToDouble(&opy);

  tmp= tmpDims;
  tmpDims= 0;
  FreeDimension(tmp);

  /* compute dimensions of y array -- nfast-by-number-by-(slow), where
     nfast are the first which dimensions of y, number is the length of
     the dimension to interpolate on, and (slow) are the rest */
  nDims= CountDims(opy.type.dims);
  if (which<0) which= nDims+which; /* handle which<0 like array index<0 */
  if (which>=nDims || which<0) YError("bad 4th argument to integ");
  i= nDims-1-which;
  tmp= opy.type.dims;
  while (i) { tmp= tmp->next; i--; }
  number= tmp? tmp->number : 1;
  if (number<2) YError("bad dimension (length 1) in integ");
  nfast= TotalNumber(tmp->next);
  if (opx.type.number!=number || opx.type.dims->next)
    YError("dimension of x does not match target dimension of y in integ");

  if (which==nDims-1) {
    /* can just tack new element(s) of dimension list onto old */
    tmpDims= Ref(opy.type.dims->next);
    tmpDims= CopyDims(opxp.type.dims, tmpDims, 1);
  } else {
    /* make a fresh copy of the index list and find where to insert new */
    Dimension *prev, *tmpprev;
    i= nDims-which;   /* guaranteed >1 AND opy is at least 2D */
    tmpDims= prev= CopyDims(opy.type.dims, tmpDims, 1);
    tmp= prev->next;  i--;
    tmp= tmp->next;  i--;   /* prev is two behind tmp */
    while (i) { prev= prev->next; tmp= tmp->next; i--; }
    tmpprev= prev->next;
    prev->next= 0;     /* cut off tail of dimension list */
    tmpprev->next= 0;  /* this pointed to tmp */
    FreeDimension(tmpprev);
    prev->next= CopyDims(opxp.type.dims, tmp, 1);
  }

  /* create partial sum array (integrals up to each xp) */
  array= NewTmpArray(&doubleStruct, opy.type.dims);
  psum= array->value.d;

  /* create result array */
  array= PushDataBlock(NewArray(&doubleStruct, tmpDims));
  yi= array->value.d;
  y= opy.value;
  x= opx.value;
  xp= opxp.value;

  /* The problem is as follows (same as interp):
     For each value of the faster indices, and each value of the slower
     indices of y, and for each value of xp, find the value of yi
     corresponding to xp.  Note that for each interpolation, the y
     vector has stride nfast, while the x vector always has stride 0.
     Note also that ALL of the yi for a given xp should be computed
     once xp is found.  */

  /* first compute partial sums */
  js= nfast*number;
  for (j=0 ; j<opy.type.number ; j+=js) {
    for (k=0 ; k<nfast ; k++) {
      psum[k+j]= 0.0;
      for (i=1, l=nfast ; i<number ; i++, l+=nfast)
        psum[k+l+j]= psum[k+l-nfast+j]+
          0.5*(y[k+l-nfast+j]+y[k+l+j])*(x[i]-x[i-1]);
    }
  }

  /* use that to compute interpolated integrals */
  ms= nfast*opxp.type.number;
  c0= c1= 0.0;
  ip= 0;  /* hunt takes this as no guess on 1st pass */
  for (i=l=0 ; i<opxp.type.number ; i++, l+=nfast) {
    ip= hunt(x, number, xp[i], ip);
    ipy= ip*nfast;
    if (ip>=1 && ip<number) {
      dx= xp[i]-x[ip-1];
      c1= 0.5*dx*dx/(x[ip]-x[ip-1]);
      c0= dx-c1;
    }
    for (j=m=0 ; j<opy.type.number ; j+=js, m+=ms) {
      for (k=0 ; k<nfast ; k++) {
        if (ip<1) {               /* point below minimum */
          yi[k+l+m]= 0.0;
        } else if (ip<number) {   /* point in range */
          yi[k+l+m]= c0*y[k+(ipy-nfast)+j]+c1*y[k+ipy+j]
            +psum[k+(ipy-nfast)+j];
        } else {                  /* point above maximum */
          yi[k+l+m]= psum[k+(ipy-nfast)+j];
        }
      }
    }
  }

  ClearTmpArray();
  PopToD(sp-4);
  Drop(3);
}

/*--------------------------------------------------------------------------*/

/* The sorting algorithm implemented here is a hybrid of the algorithms
   in section 4.10 of 2nd ed Kernighan and Ritchie and section 8.4 of
   Numerical Recipes in C (Press et. al.).
   The C library qsort routine does not allow for strides in an obvious
   way, which is why Yorick needs these sorting routines.  */

static long sortSize, sortStride, sortLimit;
static long *longData;
static double *doubleData;
static char **stringData;

static long Random(long range);
static void ysortQ(long *list, long n);
static void ysortD(long *list, long n);
static void ysortL(long *list, long n);

static long sortSeed= 0;     /* for linear congruential random number
                                between 0 and 7874 inclusive,
                                (sortSeed*211+1663)%7875 next time */

static long Random(long range)
{
  sortSeed= (sortSeed*211+1663) % 7875;
  if (range <= 272730)
    return (range*sortSeed)/7875;
  else
    return (((unsigned long)range)>>1) + (136365*sortSeed)/7875;
}

static int ystrcmp(const char *s, const char *t);
int
ystrcmp(const char *s, const char *t)
{
  if (!s) return t? -1 : 0;
  if (!t) return s? 1 : 0;
  return strcmp(s, t);
}

static void ysortQ(long *list, long n)
{
  long j;
  if (n < sortLimit) {
    /* straight insertion fastest for short sorts */
    long i, listel;
    char *partition;
    for (i=sortStride ; i<n ; i+=sortStride) {
      listel= list[i];
      partition= stringData[listel];
      for (j=i-sortStride ;
           j>=0 && ystrcmp(stringData[list[j]], partition)>0 ;
           j-=sortStride) list[j+sortStride]= list[j];
      list[j+sortStride]= listel;
    }
    return;  /* halt recursion */

  } else {
    /* generate random partition element, remember it (listel), and
       replace it by the first element */
    long i;
    long partel= Random(n/sortStride)*sortStride;
    long listel= list[partel];
    char *partition= stringData[listel];
    int scmp, flipper = 0;
    list[partel]= list[0];
    /* partition the remainder of the list into elements which precede
       the partel then elements which follow it */
    for (j=sortStride ; j<n ; j+=sortStride)
      if (ystrcmp(stringData[list[j]], partition) >= 0) break;
    for (i=j+sortStride ; i<n ; i+=sortStride) {
      scmp = ystrcmp(stringData[list[i]], partition);
      if (scmp > 0) continue;
      if (scmp || ((flipper^=1))) {
        register long tmp= list[j];
        list[j]= list[i];
        list[i]= tmp;
        j+= sortStride;  /* known to be >= partel (or == n) */
      }
    }
    /* re-insert partition element at beginning of 2nd part of list
       -- this will be its final resting place */
    list[0]= list[j-sortStride];
    list[j-sortStride]= listel;
  }

  /* recurse to sort the < and >= partitions themselves
     This is outside previous if block to minimize stack space required
     for the recursion.  */
  if (j>sortStride) ysortQ(list, j-sortStride);
  if (j<n) ysortQ(&list[j], n-j);
}

static void ysortD(long *list, long n)
{
  long j;
  if (n < sortLimit) {
    /* straight insertion fastest for short sorts */
    long i, listel;
    double partition;
    for (i=sortStride ; i<n ; i+=sortStride) {
      listel= list[i];
      partition= doubleData[listel];
      for (j=i-sortStride ;
           j>=0 && doubleData[list[j]]>partition ;
           j-=sortStride) list[j+sortStride]= list[j];
      list[j+sortStride]= listel;
    }
    return;  /* halt recursion */

  } else {
    /* generate random partition element, remember it (listel), and
       replace it by the first element */
    long i;
    long partel= Random(n/sortStride)*sortStride;
    long listel= list[partel];
    double partition= doubleData[listel];
    int flipper = 0;
    list[partel]= list[0];
    /* partition the remainder of the list into elements which precede
       the partel then elements which follow it */
    for (j=sortStride ; j<n ; j+=sortStride)
      if (doubleData[list[j]] >= partition) break;
    for (i=j+sortStride ; i<n ; i+=sortStride) {
      if (doubleData[list[i]] > partition) continue;
      if ((doubleData[list[i]] != partition) || ((flipper^=1))) {
        register long tmp= list[j];
        list[j]= list[i];
        list[i]= tmp;
        j+= sortStride;  /* known to be >= partel (or == n) */
      }
    }
    /* re-insert partition element at beginning of 2nd part of list
       -- this will be its final resting place */
    list[0]= list[j-sortStride];
    list[j-sortStride]= listel;
  }

  /* recurse to sort the < and >= partitions themselves
     This is outside previous if block to minimize stack space required
     for the recursion.  */
  if (j>sortStride) ysortD(list, j-sortStride);
  if (j<n) ysortD(&list[j], n-j);
}

static void ysortL(long *list, long n)
{
  long j;
  if (n < sortLimit) {
    /* straight insertion fastest for short sorts */
    long i, listel;
    long partition;
    for (i=sortStride ; i<n ; i+=sortStride) {
      listel= list[i];
      partition= longData[listel];
      for (j=i-sortStride ;
           j>=0 && longData[list[j]]>partition ;
           j-=sortStride) list[j+sortStride]= list[j];
      list[j+sortStride]= listel;
    }
    return;  /* halt recursion */

  } else {
    /* generate random partition element, remember it (listel), and
       replace it by the first element */
    long i;
    long partel= Random(n/sortStride)*sortStride;
    long listel= list[partel];
    long partition= longData[listel];
    int flipper = 0;
    list[partel]= list[0];
    /* partition the remainder of the list into elements which precede
       the partel then elements which follow it */
    for (j=sortStride ; j<n ; j+=sortStride)
      if (longData[list[j]] >= partition) break;
    for (i=j+sortStride ; i<n ; i+=sortStride) {
      if (longData[list[i]] > partition) continue;
      if ((longData[list[i]] != partition) || ((flipper^=1))) {
        register long tmp= list[j];
        list[j]= list[i];
        list[i]= tmp;
        j+= sortStride;  /* known to be >= partel (or == n) */
      }
    }
    /* re-insert partition element at beginning of 2nd part of list
       -- this will be its final resting place */
    list[0]= list[j-sortStride];
    list[j-sortStride]= listel;
  }

  /* recurse to sort the < and >= partitions themselves
     This is outside previous if block to minimize stack space required
     for the recursion.  */
  if (j>sortStride) ysortL(list, j-sortStride);
  if (j<n) ysortL(&list[j], n-j);
}

void Y_sort(int nArgs)
{
  Operand op;
  Array *result;
  long *ilist, i, j, which, nDims, origin;
  Dimension *tmp;
  void (*ysort)(long *list, long n);
  if (nArgs!=1 && nArgs!=2)
    YError("sort takes exactly one or two arguments");

  if (nArgs==2) { which= YGetInteger(sp)-1; Drop(1); }
  else which= 0;  /* use 0-origin which here */

  /* get array to be sorted */
  sp->ops->FormOperand(sp, &op);
  if (op.ops->typeID <= T_LONG) {
    op.ops->ToLong(&op);
    ysort= &ysortL;
    longData= op.value;
  } else if (op.ops->typeID <= T_DOUBLE) {
    op.ops->ToDouble(&op);
    ysort= &ysortD;
    doubleData= op.value;
  } else if (op.ops==&stringOps) {
    ysort= &ysortQ;
    stringData= op.value;
  } else {
    YError("sort function requires integer, real, or string operand");
    ysort= 0;
  }

  /* figure out stride for the sort */
  nDims= CountDims(op.type.dims);
  if (nDims==0) {
    PushIntValue(0);
    sp->ops= &longScalar;
    sp->value.l= 0;
    return;
  }
  if (which<0) which+= nDims;
  if (which<0 || which>=nDims)
    YError("2nd argument to sort function out of range");
  if (nDims<2) {
    sortStride= 1;
    sortSize= op.type.number;
  } else {
    which= nDims-1-which;
    tmp= op.type.dims;
    while (which--) tmp= tmp->next;
    sortStride= TotalNumber(tmp->next);
    sortSize= sortStride*tmp->number;
  }
  sortLimit= 7*sortStride;  /* use straight insertion for <7 elements */

  /* push result Array, then fill it with index to be sorted */
  result= PushDataBlock(NewArray(&longStruct, op.type.dims));
  ilist= result->value.l;
  for (i=0 ; i<op.type.number ; i++) ilist[i]= i;

  for (i=0 ; i<sortStride ; i++)
    for (j=0 ; j<op.type.number ; j+=sortSize)
      ysort(&ilist[i+j], sortSize);

  if ((origin= op.type.dims->origin))
    for (i=0 ; i<op.type.number ; i++) ilist[i]+= origin;
}

/*--------------------------------------------------------------------------*/

void Y_transpose(int nArgs)
{
  Symbol *stack= sp-nArgs+1;
  Operand op, opp;
  int i, nDims, order[10];
  long numbers[10], origins[10], strides[10];
  long stride, *cycle, index, prev, next, last;
  Dimension *dims;
  LValue *lvalue;
  Strider *strider;
  if (nArgs < 1) YError("transpose needs at least argument");

  stack->ops->FormOperand(stack, &op);
  if (!op.ops->isArray) YError("1st argument to transpose must be array");
  nDims= CountDims(op.type.dims);
  if (nDims>10) YError("transpose fails for arrays with >10 dimensions");
  if (nDims<1) {
    if (nArgs>1) Drop(nArgs-1);
    return;
  }

  /* collect dimension lengths and strides into arrays to be permuted */
  dims= op.type.dims;
  stride= 1;
  for (i=0 ; i<nDims ; i++) {
    numbers[nDims-1-i]= dims->number;
    origins[nDims-1-i]= dims->origin;
    dims= dims->next;
  }
  stride= op.type.base->size;
  for (i=0 ; i<nDims ; i++) {
    strides[i]= stride;
    stride*= numbers[i];
  }

  /* compute the permutation from the remaining arguments */
  for (i=0 ; i<nDims ; i++) order[i]= i;
  if (nArgs<2) {
    /* default is to swap first and last indices */
    if (nDims) {
      prev= order[0];
      order[0]= order[nDims-1];
      order[nDims-1]= prev;
    }
  } else {
    /* read permutation list */
    while (stack<sp) {
      stack++;
      stack->ops->FormOperand(stack, &opp);
      if (opp.ops->promoteID>T_LONG ||
          (opp.type.dims && opp.type.dims->next))
        YError("bad permutation list in transpose");
      opp.ops->ToLong(&opp);
      cycle= opp.value;
      if (opp.type.dims) {
        /* this is a cycle list */
        last= cycle[0]-1;
        if (last<0) last+= nDims;
        if (last<0 || last>=nDims)
          YError("permutation list references non-existent dimension "
                 "in transpose");
        prev= order[last];
        for (i=1 ; i<opp.type.number ; i++) {
          index= cycle[i]-1;
          if (index<0) index+= nDims;
          if (index<0 || index>=nDims)
            YError("permutation list references non-existent dimension "
                   "in transpose");
          next= order[index];
          order[index]= prev;
          prev= next;
        }
        order[last]= prev;
      } else {
        /* this is a cyclic permutation of all nDims indices */
        long inc= cycle[0]-1;           /* index which 0 should go to */
        long now;
        if (inc<0) inc= nDims - (-inc)%nDims;
        if (inc>=nDims) inc%= nDims;
        prev= order[0];
        now= inc;
        last= now;
        for (i=0 ; i<nDims ; i++) {
          next= order[now];
          order[now]= prev;
          prev= next;
          now+= inc;
          if (now>=nDims) now-= nDims;
          if (last==now) {
            /* handle case of several independent cycles when nDims is
               evenly divisible by inc */
            prev= order[++now];
            now+= inc;
            if (now>=nDims) now-= nDims;
            last= now;
          }
        }
      }
    }
    Drop(nArgs-1);
  }

  /* build re-ordered dimension list */
  dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  for (i=0 ; i<nDims ; i++)
    tmpDims= NewDimension(numbers[order[i]], origins[order[i]], tmpDims);

  /* push LValue describing result onto stack */
  lvalue= PushDataBlock(NewLValueM((Array *)sp->value.db, op.value,
                                   op.type.base, tmpDims));

  /* build strider list describing re-ordering */
  for (i=0 ; i<nDims ; i++) {
    strider= NewStrider(strides[order[i]], numbers[order[i]]);
    strider->next= lvalue->strider;
    lvalue->strider= strider;
  }

  PopTo(sp-2);
  Drop(1);
  FetchLValue(lvalue, sp);
}

static Dimension *growDims= 0;

void Y_grow(int nArgs)
{
  Symbol *s0, *s= sp-nArgs+1;
  long index= s->index;
  Array *array;
  Dimension *dims;
  StructDef *base;
  Operand op;
  long extra, number;
  int nDims;
  DataBlock *db;
  int amSubroutine= CalledAsSubroutine();

  if (nArgs < 2) YError("grow function needs at least two arguments");
  if (amSubroutine && s->ops!=&referenceSym)
    YError("1st argument to grow must be a variable reference");
  if (!s->ops) YError("unxepected keyword argument in grow");

  dims= growDims;
  growDims= 0;
  FreeDimension(dims);

  /* scan argument list to find first non-nil argument */
  base= 0;
  s0= 0;
  for (;;) {
    if (!s0 && amSubroutine) array= (Array *)ForceToDB(&globTab[index]);
    else array= (Array *)ForceToDB(s);  /* does ReplaceRef if required */
    s0= s;
    if (array->ops==&lvalueOps) array= FetchLValue(array, s);
    if (array->ops->isArray) {
      base= array->type.base;
      if (array->references) {
        /* the grow operation is destructive, must copy 1st arg */
        Array *copy= PushDataBlock(NewArray(base, array->type.dims));
        base->Copy(base, copy->value.c, array->value.c, array->type.number);
        PopTo(s);
        array= copy;
      }
      if (array->type.dims) {
        growDims= NewDimension(1L, 1L, Ref(array->type.dims->next));
      } else {
        growDims= NewDimension(1L, 1L, (Dimension *)0);
        array->type.dims= NewDimension(1L, 1L, (Dimension *)0);
      }
      break;
    } else if (array->ops!=&voidOps) {
      YError("bad data type in function grow");
    }
    if (++s > sp) {  /* all arguments void, will return nil */
      Drop(nArgs-1);
      PopTo(sp-1);
      return;
    }
  }
  nDims= CountDims(growDims);

  /* scan through remaining arguments to force right-conformability with
     growDims and count the number of extra dimensions */
  extra= 0;
  while (s<sp) {
    s++;
    if (!s->ops) YError("unxepected keyword argument in grow");
    s->ops->FormOperand(s, &op);
    if (op.ops->isArray) {
      if (nDims==CountDims(op.type.dims))
        growDims->number= op.type.dims->number;
      else
        growDims->number= 1;
      if (RightConform(growDims, &op))
        YError("later arguments not conformable with 1st in grow");
      extra+= growDims->number;
    } else if (op.ops!=&voidOps) {
      YError("illegal data type in function grow");
    }
  }

  if (extra) {
    LValue lvalue;
    long size;
    BinaryOp *Assign= base->dataOps->Assign;

    /* phony LValue necessary for Assign virtual function */
    lvalue.references= nArgs;    /* NEVER want to free this */
    lvalue.ops= &lvalueOps;
    lvalue.owner= 0;             /* not true, but safer */
    lvalue.type.base= base;      /* NOT Ref(base) -- won't be freed */
    lvalue.address.m= 0;
    lvalue.strider= 0;

    size= base->size;
    /* copy 1st non-nil argument */
    number= array->type.number;
    array= PushDataBlock(GrowArray(array, extra));
    lvalue.address.m= array->value.c + size*number;

    /* second pass through argument list copies 2nd-Nth arguments
       into result array using the Assign virtual function */
    s= s0;
    while (++s<sp) {  /* note that sp is bigger than for previous loop */
      s->ops->FormOperand(s, &op);
      if (op.ops->isArray) {
        lvalue.type.dims= op.type.dims; /* NOT Ref(dims) -- won't be freed */
        lvalue.type.number= op.type.number;
        /* Assign virtual functions assume their first parameter is an
           LValue* rather than an Operation* (like all other BinaryOps).  */
        (*Assign)((Operand *)&lvalue, &op);
        lvalue.address.m+= size*lvalue.type.number;
      }
    }
  }

  /* store result back to first reference -- will also be left on stack
     by EvalBI */
  if (amSubroutine) {
    s= &globTab[index];  /* guaranteed this is a dataBlockSym by ForceToDB */
    db= s->value.db;
    s->value.db= (DataBlock *)Ref(array);
    Unref(db);
    if (extra) Drop(nArgs);
    else Drop(nArgs-1);
    ReplaceRef(sp);  /* result is 1st argument */
    PopTo(sp-1);
  } else {
    if (extra) {   /* result is on top of stack */
      PopTo(sp-nArgs-1);
      Drop(nArgs);
    } else {       /* result is unchanged s0 argument */
      int nAbove= sp-s0;
      Drop(nAbove);
      nArgs-= nAbove;
      PopTo(sp-nArgs);
      Drop(nArgs-1);
    }
  }
}

void Y__(int nArgs)
{
  Y_grow(nArgs);
}

/*--------------------------------------------------------------------------*/

extern char *Ytimestamp(void);  /* 25 character return */

void
Y_timestamp(int argc)
{
  time_t n_time = time((void *)0);
  char *time = ctime(&n_time);
  long index = yget_ref(0);
  if (argc != 1) y_error("timestamp takes exactly one argument");
  if (index >= 0) {
    ypush_long((long)n_time);
    yput_global(index, 0);
  }
  if (!yarg_subroutine()) {
    char **q = ypush_q(0);
    q[0] = p_strcpy(strtok(time, "\n"));
  }
}

static double y_wall0 = 0.;

void Y_timer(int nArgs)
{
  Operand op;
  double *absTime, *incTime, cpu, sys, wall;

  if (nArgs < 0) {
    y_wall0= p_wall_secs();
    return;
  }

  if (nArgs<1 || nArgs>2 || !sp->ops)
    YError("timer takes exactly one or two arguments");
  sp->ops->FormOperand(sp, &op);
  if (op.ops!=&doubleOps || op.type.number!=3)
    YError("timer arguments must be array(double,3)");
  if (nArgs==1) {
    absTime= op.value;
    incTime= 0;
  } else {
    incTime= op.value;
    (sp-1)->ops->FormOperand(sp-1, &op);
    if (op.ops!=&doubleOps || op.type.number!=3)
      YError("timer arguments must be array(double,3)");
    absTime= op.value;
  }

  cpu= p_cpu_secs(&sys);
  wall= p_wall_secs() - y_wall0;
  if (incTime) {
    incTime[0]+= cpu-absTime[0];
    incTime[1]+= sys-absTime[1];
    incTime[2]+= wall-absTime[2];
  }
  absTime[0]= cpu;
  absTime[1]= sys;
  absTime[2]= wall;

  Drop(nArgs);
}

/*--------------------------------------------------------------------------*/

extern void BuildDimList(Symbol *stack, int nArgs);  /* ops3.c */
static void NextRandom(double *random, long n);
static void InitRandom(double seed);

void Y_random(int nArgs)
{
  Symbol *stack= sp-nArgs+1;
  double *random;
  long n;
  if (nArgs==1 && !YNotNil(stack)) {
    /* return scalar result */
    PushDoubleValue(0.0);
    random= &sp->value.d;
    n= 1;
  } else {
    /* return array result */
    Array *array;
    BuildDimList(stack, nArgs);
    array= PushDataBlock(NewArray(&doubleStruct, tmpDims));
    random= array->value.d;
    n= array->type.number;
  }
  NextRandom(random, n);
}

void Y_random_seed(int nArgs)
{
  double seed= 0.0;
  if (nArgs==1) {
    if (YNotNil(sp)) seed= YGetReal(sp);
  } else if (nArgs>0) {
    YError("random_seed takes exactly zero or one arguments");
  }
  InitRandom(seed);
}

/* Algorithm from Press and Teukolsky, Computers in Physics, 6, #5,
   Sep/Oct 1992, pp. 522-524.  They offer a $1000 reward to anyone
   who finds a statistical test this generator fails non-trivially.
   Based on a generator of L'Ecuyer with a shuffle algorithm of
   Bays-Durham and other improvements.
   The period of the generator is 2.3e18.  */

/* The long period is achieved by combining two sequences with
   nearly incomensurate periods.  */
static long idum= 0;       /* this is the primary seed */
static long idum2= 0;      /* this is the secondary seed */
#undef NTAB
#define NTAB 32
static long iv[NTAB], iy;  /* shuffle table (of idum) and previous result */

/* Choose two sets of linear congruential parameters-- the
   multiplier is IA, and the modulus is IM.  The IR and IQ are
   cunningly chosen so that (IA*x)%IM can be computed as
   IA*(x%IQ) - IR*(x/IQ), the latter expression having the
   advantage that the product will not overflow.  The IM values
   are near 2^31-1, the largest guaranteed positive long.
   The clever calculation of (IA*x)%IM is due to Schrage,
   ACM Trans. Mathem. Software 5, 132-138;
   IM= IA*IQ+IR with IR<IQ is the required relation.  */
#undef IM1
#define IM1 2147483563
#undef IA1
#define IA1 40014
#undef IQ1
#define IQ1 53668
#undef IR1
#define IR1 12211

#undef IM2
#define IM2 2147483399
#undef IA2
#define IA2 40692
#undef IQ2
#define IQ2 52774
#undef IR2
#define IR2 3791

#undef AM
#define AM (1.0/(double)IM1)
#undef IMM1
#define IMM1 (IM1-1)
#undef NDIV
#define NDIV (1+IMM1/NTAB)

static void NextRandom(double *random, long n)
{
  long j;
  if (idum<=0) InitRandom(0.0);

  while (n--) {
    /* compute idum= (IA1*idum)%IM1 without overflow */
    idum= IA1*(idum%IQ1) - IR1*(idum/IQ1);
    if (idum<0) idum+= IM1;

    /* compute idum2= (IA2*idum2)%IM2 without overflow */
    idum2= IA2*(idum2%IQ2) - IR2*(idum2/IQ2);
    if (idum2<0) idum2+= IM2;

    /* previous result is used to determine which element of the shuffle
       table to use for this result */
    j= iy/NDIV;            /* in range 0..NTAB-1 */
    iy= iv[j]-idum2;
    iv[j]= idum;
    if (iy<1) iy+= IMM1;

    /* Really only IMM1 possible values can be returned, 1<=iy<=IMM1.
       Algorithm given by Press and Teukolsky has a slight bug.
       Here, the return values are centered in IMM1 equal bins.
       If 2.e9 distinct values are not enough, could use, say, idum2
       to scoot the points around randomly within bins...  */
    *random++= AM*(iy-0.5);
  }
}

static void InitRandom(double seed)
{
  long j;

  /* translate seed to integer between 1 and IM2-1 inclusive */
  if (seed<=0.0 || seed>=1.0) seed= 0.6180339885;  /* default seed */
  idum= (long)(seed*(double)IM2);
  if (idum<1 || idum>=IM2) idum= 1;
  idum2= idum;

  /* do 8 warm-ups, then load shuffle table */
  for (j=NTAB+7 ; j>=0 ; j--) {
    idum= IA1*(idum%IQ1) - IR1*(idum/IQ1);
    if (idum<0) idum+= IM1;
    if (j<NTAB) iv[j]= idum;
  }
  iy= iv[0];
}

/*--------------------------------------------------------------------------*/

static void MrgCpyC(StructDef *, void *, void *, void *, int *, long);
static void MrgCpyS(StructDef *, void *, void *, void *, int *, long);
static void MrgCpyI(StructDef *, void *, void *, void *, int *, long);
static void MrgCpyL(StructDef *, void *, void *, void *, int *, long);
static void MrgCpyF(StructDef *, void *, void *, void *, int *, long);
static void MrgCpyD(StructDef *, void *, void *, void *, int *, long);
static void MrgCpyZ(StructDef *, void *, void *, void *, int *, long);
static void MrgCpyX(StructDef *, void *, void *, void *, int *, long);

static void (*MrgCpy[10])(StructDef*, void*, void*, void*, int*, long)= {
  &MrgCpyC, &MrgCpyS, &MrgCpyI, &MrgCpyL, &MrgCpyF, &MrgCpyD, &MrgCpyZ,
  &MrgCpyX, &MrgCpyX, &MrgCpyX };

extern VMaction True;

void Y_merge(int nArgs)
{
  Operand t, f;
  Operations *ops;
  Dimension *dims;
  int *cond;
  long i, n;
  void *rslt= 0;
  StructDef *base;

  if (nArgs!=3) YError("merge function takes exactly three arguments");
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  True();    /* convert condition to type int, values 0 or 1 */
  sp->ops->FormOperand(sp, &t);
  dims= t.type.dims;
  n= t.type.number;
  cond= t.value;
  if (!(sp-2)->ops)
    YError("merge function recognizes no keyword arguments");
  (sp-2)->ops->FormOperand(sp-2, &t);
  (sp-1)->ops->FormOperand(sp-1, &f);
  if (t.ops==&voidOps) {
    t.type.number= 0;
    if (f.ops==&voidOps) f.type.number= 0;
    ops= f.ops;
    base= f.type.base;
  } else if (f.ops==&voidOps) {
    f.type.number= 0;
    ops= t.ops;
    base= t.type.base;
  } else {
    ops= t.ops->Promote[f.ops->promoteID](&t, &f);
    base= t.type.base;
    if (ops==&structOps && !StructEqual(base, f.type.base))
      YError("two different struct instance types cannot be merged");
  }
  if (!ops || !ops->isArray)
    YError("merge requires array or nil arguments");
  if (t.type.number+f.type.number != n)
    YError("number of trues + number of falses not number of conditions");

  if (!dims) {
    if (base==&doubleStruct) {
      PushDoubleValue(0.0);
      rslt= &sp->value.d;
    } else if (base==&longStruct) {
      PushLongValue(0L);
      rslt= &sp->value.l;
    } else if (base==&intStruct) {
      PushIntValue(0);
      rslt= &sp->value.i;
    }
  }
  if (!rslt) {
    Array *result= PushDataBlock(NewArray(base, dims));
    rslt= result->value.c;
  }

  for (i=0 ; i<n ; i++) if (cond[i]) t.type.number--;
  if (t.type.number)
    YError("number of falses does not match number of 0 conditions");
  MrgCpy[ops->typeID](base, rslt, t.value, f.value, cond, n);
}

#undef OPERATION
#define OPERATION(op, typd) \
static void op(StructDef *base, void*rr, void*tt, void*ff, int*c, long n) \
{ typd *r= rr, *t= tt, *f= ff;   long i; \
  for (i=0 ; i<n ; i++) r[i]= c[i]? *t++ : *f++; }

/* ARGSUSED */
OPERATION(MrgCpyC, char)
/* ARGSUSED */
OPERATION(MrgCpyS, short)
/* ARGSUSED */
OPERATION(MrgCpyI, int)
/* ARGSUSED */
OPERATION(MrgCpyL, long)
/* ARGSUSED */
OPERATION(MrgCpyF, float)
/* ARGSUSED */
OPERATION(MrgCpyD, double)

/* ARGSUSED */
static void MrgCpyZ(StructDef *base, void*rr, void*tt, void*ff, int*c, long n)
{
  double *r= rr, *t= tt, *f= ff;
  long i;
  n*= 2;
  for (i=0 ; i<n ; i+=2) {
    if (c[i>>1]) { r[i]= *t++; r[i+1]= *t++; }
    else         { r[i]= *f++; r[i+1]= *f++; }
  }
}

static void MrgCpyX(StructDef *base, void*rr, void*tt, void*ff, int*c, long n)
{
  char *r= rr, *t= tt, *f= ff;
  long size= base->size;
  Copier *Copy= base->Copy;
  int cc= c[0];
  long nn, i= 0;
  do {
    i++;
    if (cc) {
      for (nn=1 ; i<n && c[i] ; i++) nn++;
      Copy(base, r, t, nn);
      nn*= size;
      r+= nn;
      t+= nn;
      cc= 0;
    } else {
      for (nn=1 ; i<n && !c[i] ; i++) nn++;
      Copy(base, r, f, nn);
      nn*= size;
      r+= nn;
      f+= nn;
      cc= 1;
    }
  } while (i<n);
}

/*--------------------------------------------------------------------------*/

#undef N_KEYWORDS
#define N_KEYWORDS 1
static char *histKeys[N_KEYWORDS+1]= { "top", 0 };

void Y_histogram(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, histKeys, keySymbols);
  int iPass= 0;
  long *list= 0;
  double *weight= 0;
  Dimension *dimsl, *dimsw;
  long number, i, top;

  while (stack<=sp) {
    if (!stack->ops) { stack+= 2; continue; }
    if (iPass==0) list= YGet_L(stack, 0, &dimsl);
    else if (iPass==1) weight= YGet_D(stack, 0, &dimsw);
    else list= 0;  /* error */
    iPass++;
    stack++;
  }
  if (!list) YError("histogram takes one or two non-keyword arguments");
  number= TotalNumber(dimsl);
  if (weight && TotalNumber(dimsw)!=number)
    YError("histogram weight array must be same length as list");

  top= 1;
  for (i=0 ; i<number ; i++) {
    if (list[i]>top) top= list[i];
    else if (list[i]<1) YError("histogram list element < 1 illegal");
  }

  if (YNotNil(keySymbols[0])) {
    i= YGetInteger(keySymbols[0]);
    if (i<top) YError("histogram list element > top illegal");
    top= i;
  }

  dimsl= tmpDims;
  tmpDims= 0;
  FreeDimension(dimsl);
  tmpDims= NewDimension(top, 1L, (Dimension *)0);

  if (!weight) {
    Array *array= PushDataBlock(NewArray(&longStruct, tmpDims));
    long *hist= array->value.l;
    for (i=0 ; i<top ; i++) hist[i]= 0;
    for (i=0 ; i<number ; i++) hist[list[i]-1]++;
  } else {
    Array *array= PushDataBlock(NewArray(&doubleStruct, tmpDims));
    double *hist= array->value.d;
    for (i=0 ; i<top ; i++) hist[i]= 0.0;
    for (i=0 ; i<number ; i++) hist[list[i]-1]+= weight[i];
  }
}

/*--------------------------------------------------------------------------*/

void Y_poly(int nArgs)
{
  Symbol *stack, *xsp= sp - nArgs + 1;
  extern void Multiply(void);
  extern void Add(void);

  for (stack=xsp ; stack<=sp ; stack++) {
    if (!stack->ops) YError("poly accepts no keyword arguments");
    if (stack->ops==&referenceSym) ReplaceRef(stack);
    if (stack->ops==&dataBlockSym && stack->value.db->ops==&lvalueOps)
      FetchLValue(stack->value.db, stack);
  }

  while (sp>xsp+1) {
    PushCopy(xsp);
    Multiply();
    Add();
  }
}

/*--------------------------------------------------------------------------*/

void
Y_noop(int argc)
{
  if (argc && sp->ops==&referenceSym) ReplaceRef(sp);
  return;
}

/*--------------------------------------------------------------------------*/
