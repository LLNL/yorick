/*
 * $Id: range.c,v 1.1 2005-09-18 22:04:13 dhmunro Exp $
 * Implement range functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*
    A range function operates on one index of an Array, while the
    remaining indices are "spectators".  The range function may
    preserve rank, so that the result Array has the same number of
    dimensions as the input Array, or it may reduce rank, so that
    the result Array has one fewer dimensions than the input Array.
    In either case, the spectator indices are identical in the
    input and result Array.  A rank preserving range function may
    change the length of the index it operates on, or the result
    may have identical dimensions as the input.

    Yorick supports the following range functions:

    RANK REDUCING
    -------------
       min:
       max:
           the minimum (or maximum) value along the index
       ptp:
           the difference between the maximum and minimum values
           along the index, positive if the maximum occurs for a
           larger index than the minimum, otherwise negative
       sum:
           the sum of the values along the index
           (concatentation for strings)

       avg:
       rms:
           the arithmetic mean (or square root of the arithmetic mean
           of the squares) of the values along the index, the result
           is always a double

       mnx:
       mxx:
           the index at which the minimum (or maximum) value along the
           index occured (the first such index if the extreme value
           occurs more than once), the result is always a long

       Complex input is allowed for the sum and avg functions only.

    RANK PRESERVING
    ---------------
       psum:
           the sums of the values at equal or lesser indices along the
           index
       dif:
           the pairwise differences between adjacent values along the
           index (there is one fewer pairwise difference than value)
       cum:
           psum, but first result value is 0 -- true inverse of dif

       zcen:
           the pairwise averages between adjacent values along the
           index (there is one fewer pairwise average than value),
           zcen is for "zone center"
       pcen:
           the pairwise averages between adjacent values along the
           index, with the first and last values copied (there is one
           more result than input than value), pcen is for "point center"
       uncp:
           the inverse of the pcen operation (there is one fewer result
           than input value), uncp is for "uncenter point"

       Complex input is allowed for all rank preserving range functions.

 */

/* ------------------------------------------------------------------------ */

#include "ydata.h"
#include "pstdlib.h"

/* A range function takes an Array and an integer indicating which index
   of the array to operate on (0 for fastest varying, 1 for next, etc.).
   The result of the operation is placed on top of the stack, and the
   function returns 0 if the result Array (the result MUST be an Array)
   has the same rank as the input Array, 1 if the result array has
   reduced rank (the input index is missing in the result).  */
/* typedef int RangeFunc(Array *array, int index); */

/* Here are the range functions themselves.  Pointers to these functions
   are introduced into the virtual machine code by the parser.  */
extern RangeFunc RFmin, RFmax, RFptp, RFsum, RFavg, RFrms, RFmnx, RFmxx,
  RFpsum, RFdif, RFzcen, RFpcen, RFuncp, RFcum;

/* ------------------------------------------------------------------------ */

/* rms function needs square root */
extern double sqrt(double);

/* ------------------------------------------------------------------------ */

/* RFCopyDims is used to copy the part of the input dimension list
   which varies more slowly than the range function index.
   The copy is placed in tmpDims.  */
static void RFCopyDims(Dimension *start, Dimension *stop);

/* RankReduce and RankPreserve build the result dimension list tmpDims,
   and set the scalar parameters step, number, nout, and nslow for the
   range function worker routines.  */
static void RankReduce(Dimension *dims, int index);
static void RankPreserve(Dimension *dims, int index, long change);

/* The RFPushN routines push the result array onto the stack.  The four
   routines correspond to the four different mappings of input data type
   to output data type (see the enumeration below).  */
static Array *RFPush1(int typeID);
static Array *RFPush2(void);
static Array *RFPush3(int typeID);
static Array *RFPush4(int typeID);

/* RFLoop calls the raw range function routine once for each combination
   of spectator indices.  RFLoopC is a special version for complex
   input (all routines which allow complex input produce a complex
   result).  */
typedef void RawRF(void *, void *);
static void RFLoop(RawRF *RFRaw, Array *input, Array *result);
static void RFLoopC(RawRF *RFRaw, Array *input, Array *result);

/* ------------------------------------------------------------------------ */

/* step is the number of elements in dimensions which vary faster than
        the dimension which is the domain of the range function
   number is the number of elements in the domain dimension
   nout   is the number of elements in the result dimension
   nslow  is the number of elements in dimensions which vary slower than
        the domain dimension */
static long step, number, origin, nout, nslow;

static void RFCopyDims(Dimension *start, Dimension *stop)
{
  if (start==stop) {
    nslow= 1;
  } else {
    RFCopyDims(start->next, stop);
    nslow*= start->number;
    tmpDims= NewDimension(start->number, start->origin, tmpDims);
  }
}

static void RankReduce(Dimension *dims, int index)
{
  Dimension *dims0, *dimsx;
  dims0= tmpDims;
  tmpDims= 0;
  FreeDimension(dims0);
  dims0= dimsx= dims;
  while (index--) dimsx= dimsx->next;
  while (dimsx->next) { dimsx= dimsx->next; dims= dims->next; }
  tmpDims= Ref(dims->next);
  step= TotalNumber(dims->next);
  number= dims->number;
  origin= yForceOrigin? 1L : dims->origin;
  RFCopyDims(dims0, dims);
  number*= step;
  nout= step;
}

static void RankPreserve(Dimension *dims, int index, long change)
{
  Dimension *dims0, *dimsx;
  dims0= tmpDims;
  tmpDims= 0;
  FreeDimension(dims0);
  dims0= dimsx= dims;
  while (index--) dimsx= dimsx->next;
  while (dimsx->next) { dimsx= dimsx->next; dims= dims->next; }
  tmpDims= Ref(dims->next);
  step= TotalNumber(dims->next);
  number= dims->number;
  nout= number+change;
  tmpDims= NewDimension(nout, 1L, tmpDims); /* note default origin */
  RFCopyDims(dims0, dims);
  number*= step;
  nout*= step;
}

/* ------------------------------------------------------------------------ */

/* The range functions fall into 4 categories as far as the relationship
   between the input and result data types:
   (1) min max                 input same as output
   (2) mnx mxx                 output always long
   (3) sum ptp psum dif cum    integers->long, reals unchanged
   (4) avg rms zcen pcen uncp  integers->double, reals unchanged
   In all cases, complex input is either an error, or is implemented
   by two calls to the double routine.  The sum, avg, psum, dif, cum, zcen,
   pcen, and uncp functions accept complex input.  */

static StructDef *rfBase1[]= {
  &charStruct, &shortStruct, &intStruct, &longStruct,
  &floatStruct, &doubleStruct, &complexStruct };

static StructDef *rfBase3[]= {
  &longStruct, &longStruct, &longStruct, &longStruct,
  &floatStruct, &doubleStruct, &complexStruct, &stringStruct };

static StructDef *rfBase4[]= {
  &doubleStruct, &doubleStruct, &doubleStruct, &doubleStruct,
  &floatStruct, &doubleStruct, &complexStruct };

static Array *RFPush1(int typeID)
{
  return PushDataBlock(NewArray(rfBase1[typeID], tmpDims));
}

static Array *RFPush2(void)
{
  return PushDataBlock(NewArray(&longStruct, tmpDims));
}

static Array *RFPush3(int typeID)
{
  return PushDataBlock(NewArray(rfBase3[typeID], tmpDims));
}

static Array *RFPush4(int typeID)
{
  return PushDataBlock(NewArray(rfBase4[typeID], tmpDims));
}

/* ------------------------------------------------------------------------ */
/* The main loop is the same for all range functions -- the basic idea
   is to call the raw range function once per each value of all spectator
   indices.  The spectator indices are divided into those which vary
   faster than the range function index and those which vary slower.  */

static void RFLoop(RawRF *RFRaw, Array *input, Array *result)
{
  char *inp= input->value.c;
  char *out= result->value.c;
  long inpSize= input->type.base->size;
  long outSize= result->type.base->size;
  long inpNext= (number-step)*inpSize;
  long outNext= (nout-step)*outSize;
  long i, j;

  for (j=0 ; j<nslow ; j++) {
    for (i=0 ; i<step ; i++) {
      RFRaw(inp, out);
      inp+= inpSize;
      out+= outSize;
    }
    inp+= inpNext;
    out+= outNext;
  }
}

/* Those range functions for which complex input is legal are all
   implemented as two passes through the corresponding double function.  */
static void RFLoopC(RawRF *RFRaw, Array *input, Array *result)
{
  double *inp= input->value.d;
  double *out= result->value.d;
  long inpNext, outNext, i, j;

  number*= 2;
  nout*= 2;
  step*= 2;
  inpNext= number-step;
  outNext= nout-step;

  for (j=0 ; j<nslow ; j++) {
    for (i=0 ; i<step ; i++) RFRaw(inp++, out++);
    inp+= inpNext;
    out+= outNext;
  }
}

/* ------------------------------------------------------------------------ */
/* The actual raw range functions are generated by macros, since only
   the data types differ, not the algorithms.  */

/* ---- min - returns minimum along index */

static RawRF RFminC, RFminS, RFminI, RFminL, RFminF, RFminD;
static RawRF *RawMin[]= {
  &RFminC, &RFminS, &RFminI, &RFminL, &RFminF, &RFminD };

int RFmin(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_DOUBLE) YError("bad data type in min: range function");

  /* set tmpDims to result dimensions */
  RankReduce(array->type.dims, index);
  /* result data type same as input data type (behavior #1) */
  result= RFPush1(typeID);

  RFLoop(RawMin[typeID], array, result);
  return 1;
}

#define RF_MIN(rfname, type) \
static void rfname(void *in, void *out) \
{ type *inp= in, *res= out, cur= inp[0];  long i; \
  for (i=step ; i<number ; i+=step) if (inp[i]<cur) cur= inp[i]; \
  res[0]= cur; }

RF_MIN(RFminC, unsigned char)
RF_MIN(RFminS, short)
RF_MIN(RFminI, int)
RF_MIN(RFminL, long)
RF_MIN(RFminF, float)
RF_MIN(RFminD, double)

/* ---- max - returns maximum along index */

static RawRF RFmaxC, RFmaxS, RFmaxI, RFmaxL, RFmaxF, RFmaxD;
static RawRF *RawMax[]= {
  &RFmaxC, &RFmaxS, &RFmaxI, &RFmaxL, &RFmaxF, &RFmaxD };

int RFmax(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_DOUBLE) YError("bad data type in max: range function");

  /* set tmpDims to result dimensions */
  RankReduce(array->type.dims, index);
  /* result data type same as input data type (behavior #1) */
  result= RFPush1(typeID);

  RFLoop(RawMax[typeID], array, result);
  return 1;
}

#define RF_MAX(rfname, type) \
static void rfname(void *in, void *out) \
{ type *inp= in, *res= out, cur= inp[0];  long i; \
  for (i=step ; i<number ; i+=step) if (inp[i]>cur) cur= inp[i]; \
  res[0]= cur; }

RF_MAX(RFmaxC, unsigned char)
RF_MAX(RFmaxS, short)
RF_MAX(RFmaxI, int)
RF_MAX(RFmaxL, long)
RF_MAX(RFmaxF, float)
RF_MAX(RFmaxD, double)

/* ---- mnx - returns first 0-origin index where value is minimum */

static RawRF RFmnxC, RFmnxS, RFmnxI, RFmnxL, RFmnxF, RFmnxD;
static RawRF *RawMnx[]= {
  &RFmnxC, &RFmnxS, &RFmnxI, &RFmnxL, &RFmnxF, &RFmnxD };

int RFmnx(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_DOUBLE) YError("bad data type in mnx: range function");

  /* set tmpDims to result dimensions */
  RankReduce(array->type.dims, index);
  /* result data type always long (behavior #2) */
  result= RFPush2();

  RFLoop(RawMnx[typeID], array, result);
  return 1;
}

#define RF_MNX(rfname, type) \
static void rfname(void *in, void *out) \
{ type *inp= in;  type cur= inp[0];  long *res= out,  i, j=1, jcur=0; \
  for (i=step ; i<number ; i+=step,j++) if (inp[i]<cur) cur= inp[jcur=j,i]; \
  res[0]= jcur+origin; }

RF_MNX(RFmnxC, unsigned char)
RF_MNX(RFmnxS, short)
RF_MNX(RFmnxI, int)
RF_MNX(RFmnxL, long)
RF_MNX(RFmnxF, float)
RF_MNX(RFmnxD, double)

/* ---- mxx - returns first 0-origin index where value is maximum */

static RawRF RFmxxC, RFmxxS, RFmxxI, RFmxxL, RFmxxF, RFmxxD;
static RawRF *RawMxx[]= {
  &RFmxxC, &RFmxxS, &RFmxxI, &RFmxxL, &RFmxxF, &RFmxxD };

int RFmxx(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_DOUBLE) YError("bad data type in mxx: range function");

  /* set tmpDims to result dimensions */
  RankReduce(array->type.dims, index);
  /* result data type always long (behavior #2) */
  result= RFPush2();

  RFLoop(RawMxx[typeID], array, result);
  return 1;
}

#define RF_MXX(rfname, type) \
static void rfname(void *in, void *out) \
{ type *inp= in;  type cur= inp[0];  long *res= out,  i, j=1, jcur=0; \
  for (i=step ; i<number ; i+=step,j++) if (inp[i]>cur) cur= inp[jcur=j,i]; \
  res[0]= jcur+origin; }

RF_MXX(RFmxxC, unsigned char)
RF_MXX(RFmxxS, short)
RF_MXX(RFmxxI, int)
RF_MXX(RFmxxL, long)
RF_MXX(RFmxxF, float)
RF_MXX(RFmxxD, double)

/* ---- sum - returns sum along index */

static RawRF RFsumC, RFsumS, RFsumI, RFsumL, RFsumF, RFsumD;
static RawRF *RawSum[]= {
  &RFsumC, &RFsumS, &RFsumI, &RFsumL, &RFsumF, &RFsumD };
static void RFSumQ(Array *input, Array *result);

int RFsum(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID>T_COMPLEX && typeID!=T_STRING)
    YError("bad data type in sum: range function");

  /* set tmpDims to result dimensions */
  RankReduce(array->type.dims, index);
  /* result data type long, float, or double, or string (behavior #3) */
  result= RFPush3(typeID);

  if (typeID==T_STRING) RFSumQ(array, result);
  else if (typeID!=T_COMPLEX) RFLoop(RawSum[typeID], array, result);
  else RFLoopC(&RFsumD, array, result);
  return 1;
}

#define RF_SUM(rfname, type1, type2, type3) \
static void rfname(void *in, void *out) \
{ type1 *inp= in;  type2 *res= out; type3 cur= inp[0];  long i; \
  for (i=step ; i<number ; i+=step) cur+= inp[i]; \
  res[0]= (type2)cur; }

RF_SUM(RFsumC, unsigned char, long, long)
RF_SUM(RFsumS, short, long, long)
RF_SUM(RFsumI, int, long, long)
RF_SUM(RFsumL, long, long, long)
RF_SUM(RFsumF, float, float, double)
RF_SUM(RFsumD, double, double, double)

static void
RFSumQ(Array *input, Array *result)
{
  char **inp = input->value.q;
  char **out = result->value.q;
  long inpNext = number-step;
  /* long outNext = nout-step = 0; */
  long i, j, k, m, len;

  for (j=0 ; j<nslow ; j++) {
    for (i=0 ; i<step ; i++) {
      for (k=0,len=-1 ; k<number ; k+=step) {
        if (!inp[k]) continue;
        for (m=0 ; inp[k][m] ; m++);
        if (len < 0) len = m;
        else len += m;
      }
      if (len>=0) {
        out[0] = p_malloc(len+1);
        out[0][0] = '\0';
        for (k=0,len=0 ; k<number ; k+=step) {
          if (!inp[k]) continue;
          for (m=0 ; (out[0][len] = inp[k][m]) ; len++,m++);
        }
      }
      inp += 1;
      out += 1;
    }
    inp += inpNext;
    /* out += outNext; */
  }
}

/* ---- ptp - returns peak-to-peak variation along index,
              >0 if minimum occurs before maximum, else <0 */

static RawRF RFptpC, RFptpS, RFptpI, RFptpL, RFptpF, RFptpD;
static RawRF *RawPtp[]= {
  &RFptpC, &RFptpS, &RFptpI, &RFptpL, &RFptpF, &RFptpD };

int RFptp(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_DOUBLE) YError("bad data type in ptp: range function");

  /* set tmpDims to result dimensions */
  RankReduce(array->type.dims, index);
  /* result data type long, float, or double (behavior #3) */
  result= RFPush3(typeID);

  RFLoop(RawPtp[typeID], array, result);
  return 1;
}

#define RF_PTP(rfname, type1, type2) \
static void rfname(void *in, void *out) \
{ type1 *inp= in;  type2 *res= out, cn, cx;  long i, jn, jx; \
  jn= jx= 0;  cn= cx= inp[0]; \
  for (i=step ; i<number ; i+=step) \
    if ((type2)inp[i]>cx) cx= inp[jx= i]; \
    else if ((type2)inp[i]<cn) cn= inp[jn= i]; \
  res[0]= jn<jx? cx-cn : cn-cx; }

RF_PTP(RFptpC, unsigned char, long)
RF_PTP(RFptpS, short, long)
RF_PTP(RFptpI, int, long)
RF_PTP(RFptpL, long, long)
RF_PTP(RFptpF, float, float)
RF_PTP(RFptpD, double, double)

/* ---- psum - returns psum along index */

static RawRF RFpsumC, RFpsumS, RFpsumI, RFpsumL, RFpsumF, RFpsumD;
static RawRF *RawPsum[]= {
  &RFpsumC, &RFpsumS, &RFpsumI, &RFpsumL, &RFpsumF, &RFpsumD };

int RFpsum(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_COMPLEX) YError("bad data type in psum: range function");

  /* set tmpDims to result dimensions */
  RankPreserve(array->type.dims, index, 0L);
  /* result data type long, float, or double (behavior #3) */
  result= RFPush3(typeID);

  if (typeID!=T_COMPLEX) RFLoop(RawPsum[typeID], array, result);
  else RFLoopC(&RFpsumD, array, result);
  return 0;
}

#define RF_PSUM(rfname, type1, type2) \
static void rfname(void *in, void *out) \
{ type1 *inp= in;  type2 *res= out;  long i; \
  res[0]= inp[0]; \
  for (i=step ; i<number ; i+=step) res[i]= res[i-step]+inp[i]; }

RF_PSUM(RFpsumC, unsigned char, long)
RF_PSUM(RFpsumS, short, long)
RF_PSUM(RFpsumI, int, long)
RF_PSUM(RFpsumL, long, long)
RF_PSUM(RFpsumF, float, float)
RF_PSUM(RFpsumD, double, double)

/* ---- dif - returns dif along index */

static RawRF RFdifC, RFdifS, RFdifI, RFdifL, RFdifF, RFdifD;
static RawRF *RawDif[]= {
  &RFdifC, &RFdifS, &RFdifI, &RFdifL, &RFdifF, &RFdifD };

int RFdif(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_COMPLEX) YError("bad data type in dif: range function");

  /* set tmpDims to result dimensions */
  RankPreserve(array->type.dims, index, -1L);
  if (number==step) YError("can't use dif: on index of length 1");
  /* result data type long, float, or double (behavior #3) */
  result= RFPush3(typeID);

  if (typeID!=T_COMPLEX) RFLoop(RawDif[typeID], array, result);
  else RFLoopC(&RFdifD, array, result);
  return 0;
}

#define RF_DIF(rfname, type1, type2) \
static void rfname(void *in, void *out) \
{ type1 *inp= in;  type2 *res= out;  long i; \
  for (i=0 ; i<number-step ; i+=step) res[i]= inp[i+step]-inp[i]; }

RF_DIF(RFdifC, unsigned char, long)
RF_DIF(RFdifS, short, long)
RF_DIF(RFdifI, int, long)
RF_DIF(RFdifL, long, long)
RF_DIF(RFdifF, float, float)
RF_DIF(RFdifD, double, double)

/* ---- cum - returns cum along index */

static RawRF RFcumC, RFcumS, RFcumI, RFcumL, RFcumF, RFcumD;
static RawRF *RawCum[]= {
  &RFcumC, &RFcumS, &RFcumI, &RFcumL, &RFcumF, &RFcumD };

int RFcum(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_COMPLEX) YError("bad data type in cum: range function");

  /* set tmpDims to result dimensions */
  RankPreserve(array->type.dims, index, 1L);
  /* result data type long, float, or double (behavior #3) */
  result= RFPush3(typeID);

  if (typeID!=T_COMPLEX) RFLoop(RawCum[typeID], array, result);
  else RFLoopC(&RFcumD, array, result);
  return 0;
}

#define RF_CUM(rfname, type1, type2) \
static void rfname(void *in, void *out) \
{ type1 *inp= in;  type2 *res= out;  long i; \
  res[0]= 0; for (i=0 ; i<number ; i+=step) res[i+step]= res[i]+inp[i]; }

RF_CUM(RFcumC, unsigned char, long)
RF_CUM(RFcumS, short, long)
RF_CUM(RFcumI, int, long)
RF_CUM(RFcumL, long, long)
RF_CUM(RFcumF, float, float)
RF_CUM(RFcumD, double, double)

/* ---- avg - returns avg along index */

static RawRF RFavgC, RFavgS, RFavgI, RFavgL, RFavgF, RFavgD;
static RawRF *RawAvg[]= {
  &RFavgC, &RFavgS, &RFavgI, &RFavgL, &RFavgF, &RFavgD };

int RFavg(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_COMPLEX) YError("bad data type in avg: range function");

  /* set tmpDims to result dimensions */
  RankReduce(array->type.dims, index);
  /* result data type double, float, or double (behavior #4) */
  result= RFPush4(typeID);

  if (typeID!=T_COMPLEX) RFLoop(RawAvg[typeID], array, result);
  else RFLoopC(&RFavgD, array, result);
  return 1;
}

#define RF_AVG(rfname, type1, type2) \
static void rfname(void *in, void *out) \
{ type1 *inp= in;  type2 *res= out; double cur= inp[0];  long i, n= 1; \
  for (i=step ; i<number ; i+=step) { cur+= inp[i]; n++; } \
  res[0]= (type2)(cur/n); }

RF_AVG(RFavgC, unsigned char, double)
RF_AVG(RFavgS, short, double)
RF_AVG(RFavgI, int, double)
RF_AVG(RFavgL, long, double)
RF_AVG(RFavgF, float, float)
RF_AVG(RFavgD, double, double)

/* ---- rms - returns rms along index */

static RawRF RFrmsC, RFrmsS, RFrmsI, RFrmsL, RFrmsF, RFrmsD;
static RawRF *RawRms[]= {
  &RFrmsC, &RFrmsS, &RFrmsI, &RFrmsL, &RFrmsF, &RFrmsD };

int RFrms(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_DOUBLE) YError("bad data type in rms: range function");

  /* set tmpDims to result dimensions */
  RankReduce(array->type.dims, index);
  /* result data type double, float, or double (behavior #4) */
  result= RFPush4(typeID);

  RFLoop(RawRms[typeID], array, result);
  return 1;
}

#define RF_RMS(rfname, type1, type2) \
static void rfname(void *in, void *out) \
{ type1 *inp= in;  type2 *res= out, mxa, avg, cur, tmp;  long i, n= 1; \
  avg= inp[0]; mxa= avg>0.0? avg : -avg; \
  for (i=step ; i<number ; i+=step) { avg+= inp[i]; n++; \
    if (mxa<(type2)inp[i]) mxa= (type2)inp[i]; \
    else if (mxa<-(type2)inp[i]) mxa= -(type2)inp[i]; } \
  if (mxa) { avg/= n;  tmp= ((type2)inp[0]-avg)/mxa;  cur= tmp*tmp; \
    for (i=step ; i<number ; i+=step) { \
      tmp= ((type2)inp[i]-avg)/mxa;  cur+= tmp*tmp; } \
    res[0]= (type2)(mxa*sqrt(cur/n)); \
  } else res[0]= (type2)(0.0); }

RF_RMS(RFrmsC, unsigned char, double)
RF_RMS(RFrmsS, short, double)
RF_RMS(RFrmsI, int, double)
RF_RMS(RFrmsL, long, double)
RF_RMS(RFrmsF, float, float)
RF_RMS(RFrmsD, double, double)

/* ---- zcen - returns zcen along index */

static RawRF RFzcenC, RFzcenS, RFzcenI, RFzcenL, RFzcenF, RFzcenD;
static RawRF *RawZcen[]= {
  &RFzcenC, &RFzcenS, &RFzcenI, &RFzcenL, &RFzcenF, &RFzcenD };

int RFzcen(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_COMPLEX) YError("bad data type in zcen: range function");

  /* set tmpDims to result dimensions */
  RankPreserve(array->type.dims, index, -1L);
  if (number==step) YError("can't use zcen: on index of length 1");
  /* result data type double, float, or double (behavior #4) */
  result= RFPush4(typeID);

  if (typeID!=T_COMPLEX) RFLoop(RawZcen[typeID], array, result);
  else RFLoopC(&RFzcenD, array, result);
  return 0;
}

#define RF_ZCEN(rfname, type1, type2) \
static void rfname(void *in, void *out) \
{ type1 *inp= in;  type2 *res= out;  long i; \
  for (i=0 ; i<number-step ; i+=step) res[i]= \
    (type2)(0.5*((type2)inp[i+step]+(type2)inp[i])); }

RF_ZCEN(RFzcenC, unsigned char, double)
RF_ZCEN(RFzcenS, short, double)
RF_ZCEN(RFzcenI, int, double)
RF_ZCEN(RFzcenL, long, double)
RF_ZCEN(RFzcenF, float, float)
RF_ZCEN(RFzcenD, double, double)

/* ---- pcen - returns pcen along index */

static RawRF RFpcenC, RFpcenS, RFpcenI, RFpcenL, RFpcenF, RFpcenD;
static RawRF *RawPcen[]= {
  &RFpcenC, &RFpcenS, &RFpcenI, &RFpcenL, &RFpcenF, &RFpcenD };

int RFpcen(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_COMPLEX) YError("bad data type in pcen: range function");

  /* set tmpDims to result dimensions */
  RankPreserve(array->type.dims, index, 1L);
  /* result data type double, float, or double (behavior #4) */
  result= RFPush4(typeID);

  if (typeID!=T_COMPLEX) RFLoop(RawPcen[typeID], array, result);
  else RFLoopC(&RFpcenD, array, result);
  return 0;
}

#define RF_PCEN(rfname, type1, type2) \
static void rfname(void *in, void *out) \
{ type1 *inp= in;  type2 *res= out;  long i; \
  res[0]= inp[0]; \
  for (i=step ; i<number ; i+=step) res[i]= \
    (type2)(0.5*((type2)inp[i]+(type2)inp[i-step])); res[i]= inp[i-step]; }

RF_PCEN(RFpcenC, unsigned char, double)
RF_PCEN(RFpcenS, short, double)
RF_PCEN(RFpcenI, int, double)
RF_PCEN(RFpcenL, long, double)
RF_PCEN(RFpcenF, float, float)
RF_PCEN(RFpcenD, double, double)

/* ---- uncp - returns uncp along index */

static RawRF RFuncpC, RFuncpS, RFuncpI, RFuncpL, RFuncpF, RFuncpD;
static RawRF *RawUncp[]= {
  &RFuncpC, &RFuncpS, &RFuncpI, &RFuncpL, &RFuncpF, &RFuncpD };

int RFuncp(Array *array, int index)
{
  Operations *ops= array->ops;
  int typeID= ops->typeID;
  Array *result;
  if (typeID > T_COMPLEX) YError("bad data type in uncp: range function");

  /* set tmpDims to result dimensions */
  RankPreserve(array->type.dims, index, -1L);
  if (number==step) YError("can't use uncp: on index of length 1");
  /* result data type double, float, or double (behavior #4) */
  result= RFPush4(typeID);

  if (typeID!=T_COMPLEX) RFLoop(RawUncp[typeID], array, result);
  else RFLoopC(&RFuncpD, array, result);
  return 0;
}

#define RF_UNCP(rfname, type1, type2) \
static void rfname(void *in, void *out) \
{ type1 *inp= in;  type2 *res= out; double cur= inp[0];  long i; \
  for (i=step,res[0]=(type2)cur ; i<number-step ; i+=step) \
    res[i]= (type2)(cur= 2.0*inp[i]-cur); }

RF_UNCP(RFuncpC, unsigned char, double)
RF_UNCP(RFuncpS, short, double)
RF_UNCP(RFuncpI, int, double)
RF_UNCP(RFuncpL, long, double)
RF_UNCP(RFuncpF, float, float)
RF_UNCP(RFuncpD, double, double)

/* ------------------------------------------------------------------------ */
