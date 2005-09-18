/*
 * $Id: bcast.c,v 1.1 2005-09-18 22:03:48 dhmunro Exp $
 * Implement generic broadcast, scatter, and gather functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "bcast.h"

static char *daddr, *saddr;
static StructDef *copyBase;
static long size;
static Copier *Copy;

static void BlockScat(const Strider *strider);
static void BlockGath(const Strider *strider);

extern void YErrorIO(const char *msg);

/*--------------------------------------------------------------------------*/
/* Broadcast function is only relevant to Yorick, not binary file package.  */
#ifndef NOT_YORICK

/* Broadcast(dst, ddims, src, sdims, base)
   broadcasts the src array into the dst array (where both src
   and dst array elements are size bytes long), returning 0 on
   success, 1 if sdims is not L-conformable with ddims. */

/* Currently, FastCopier is selected by base->dataOps->promoteID, which
   requires knowledge of Yorick's Operations struct.  This only affects
   the Broadcast operation, and only Scatter and Gather are relevant to
   the binary file package.  */
static int BlockCast(const Dimension *ddims, const Dimension *sdims,
                     long nCopies);

/* Without special routines to do scalar broadcasts, Yorick bogs down
   unacceptably doing the memcpy called from CopyX.  */
typedef void FastCopier(long, long);
static FastCopier *FastCopy;
static FastCopier FastCCopy, FastSCopy, FastICopy, FastLCopy,
  FastFCopy, FastDCopy, FastZCopy, FastXCopy;
static FastCopier *FCopiers[]= { &FastCCopy, &FastSCopy, &FastICopy,
              &FastLCopy, &FastFCopy, &FastDCopy, &FastZCopy, &FastXCopy };

#undef OPERATION
#define OPERATION(opname, typeD) \
static void opname(long n, long nCopies) \
{ typeD *dst= (typeD *)daddr,  *src= (typeD *)saddr; \
  long i, j, jx= n*nCopies;  register typeD src0; \
  if (nCopies>n) { for (i=0 ; i<n ; i++) { \
      src0= src[i];  for (j=i ; j<jx ; j+=n) dst[j]= src0; } \
    } else { for (j=0 ; j<jx ; j+=n) \
      for (i=0 ; i<n ; i++) dst[i+j]= src[i]; } \
  daddr= (char *)(dst+jx);  saddr= (char *)(src+n); \
}

OPERATION(FastCCopy, char)
OPERATION(FastSCopy, short)
OPERATION(FastICopy, int)
OPERATION(FastLCopy, long)
OPERATION(FastFCopy, float)
OPERATION(FastDCopy, double)

static void FastZCopy(long n, long nCopies)
{
  double *dst= (double *)daddr;
  double *src= (double *)saddr;
  long i, j, jx= (n*=2)*nCopies;
  register double src0, src1;
  if (nCopies>n) {
    for (i=0 ; i<n ; i+=2) {
      src0= src[i];  src1= src[i+1];
      for (j=i ; j<jx ; j+=n) { dst[j]= src0; dst[j+1]= src1; }
    }
  } else {
    for (j=0 ; j<jx ; j+=n)
      for (i=0 ; i<n ; i+=2) { dst[i+j]= src[i]; dst[i+j+1]= src[i+1]; }
  }
  daddr= (char *)(dst+jx);
  saddr= (char *)(src+n);
}

/* No help for the general case--this is how I originally programmed it.  */
static void FastXCopy(long n, long nCopies)
{
  long nbytes= n*size;
  while (nCopies--) {
    Copy(copyBase, daddr, saddr, n);
    daddr+= nbytes;
  }
  saddr+= nbytes;
}

static int BlockCast(const Dimension *ddims, const Dimension *sdims,
                     long nCopies)
{
  long n= 1;

  /* skip past leading source 1's, which are just more copies */
  while (sdims && sdims->number==1) {
    nCopies*= ddims->number;
    sdims= sdims->next;
    ddims= ddims->next;
  }

  /* as long as sdims and dims agree, a block copy is possible */
  while (sdims && sdims->number==ddims->number) {
    n*= ddims->number;
    ddims= ddims->next;
    sdims= sdims->next;
  }

  if (!sdims) {
    /* if the dimension lists are exhausted, just make nCopies of
       blocks of n copyBase StructDefs */
    FastCopy(n, nCopies);

  } else if (sdims->number==1) {
    /* must recurse to do a sparse copy */
    char *saddr0= saddr;
    long i;
    for (;;) {
      for (i=0 ; i<n ; i++) BlockCast(ddims, sdims, 1L);
      if (! --nCopies) break;
      saddr= saddr0;
    }

  } else {
    /* sdims->number!=ddims->number and sdims->number!=1 is an error */
    return 1;
  }
  return 0;
}

int Broadcast(void *dst, const Dimension *ddims,
              void *src, const Dimension *sdims, StructDef *base)
{
  int n= CountDims(ddims)-CountDims(sdims);
  long nCopies= 1;
  saddr= src;
  daddr= dst;
  FastCopy= FCopiers[base->dataOps->promoteID];
  if (FastCopy==&FastXCopy) {
    copyBase= base;
    size= base->size;
    Copy= base->Copy;
  }
  if (n<0) {
    do {
      if (sdims->number > 1) return 1;
      sdims= sdims->next;
    } while (++n);
  } else if (n>0) {
    do {
      nCopies*= ddims->number;
      ddims= ddims->next;
    } while (--n);
  }
  return BlockCast(ddims, sdims, nCopies);
}

#endif
/* Scatter and Gather functions relevant even if NOT_YORICK.  */
/*--------------------------------------------------------------------------*/

/* Scatter(dst, strider, src, base, number)
   scatters the src array into the dst array, given the strider for
   the dst array and the base StructDef for the objects to be scattered.
   If the strider is 0, the given number of contiguous objects is
   copied.
      x(index list)= expression      */

/* Without special routines for gather and scatter, Yorick bogs down
   unacceptably of some platforms (e.g.- Crays).  */
typedef void FastScatter(long, long, long*);
static FastScatter *FastScat;
static FastScatter FastCScat, FastSScat, FastIScat, FastLScat,
  FastFScat, FastDScat;
static FastScatter *FScatters[]= { &FastCScat, &FastSScat, &FastIScat,
              &FastLScat, &FastFScat, &FastDScat, 0, 0 };

#undef OPERATION
#define OPERATION(opname, typeD) \
static void opname(long stride, long n, long *list) \
{ \
  typeD *dst= (typeD *)daddr,  *src= (typeD *)saddr; \
  if (stride!=size) { \
    if (!list) for (;n--;src++,daddr+=stride) *((typeD*)daddr)= src[0]; \
    else for (;n--;src++,list++) *((typeD*)(daddr+list[0]*stride))= src[0]; \
  } else { \
    if (!list) for ( ; n-- ; src++,dst++) dst[0]= src[0]; \
    else for (; n-- ; src++,list++) dst[list[0]]= src[0]; \
  } \
  saddr= (char *)src; \
}

OPERATION(FastCScat, char)
OPERATION(FastSScat, short)
OPERATION(FastIScat, int)
OPERATION(FastLScat, long)
OPERATION(FastFScat, float)
OPERATION(FastDScat, double)

static void BlockScat(const Strider *strider)
{
  Strider *next= strider->next;
  long stride= strider->stride;
  long i, n= strider->number;
  Array *indexList= strider->indexList;

  char *daddr0= daddr;

  if (!indexList) {
    if (next) {
      for (i=0 ; i<n ; i++) {
        BlockScat(next);
        daddr+= stride;
        /* saddr already incremented by other branches of BlockScat */
      }
    } else if (FastScat) {
      FastScat(stride, n, (long *)0);
    } else if (stride!=size) {
      for (i=0 ; i<n ; i++) {
        Copy(copyBase, daddr, saddr, 1L);
        daddr+= stride;
        saddr+= size;
      }
    } else {
      Copy(copyBase, daddr, saddr, n);
      saddr+= n*size;
    }

  } else {
    long *list= indexList->value.l;
    if (next) {
      for (i=0 ; i<n ; i++) {
        daddr= daddr0+stride*list[i];
        BlockScat(next);
        /* saddr already incremented by other branches of BlockScat */
      }
    } else if (FastScat) {
      FastScat(stride, n, list);
    } else {
      if (size==stride) {
        long j, li, ni;
        for (i=0 ; i<n ; i++) {
          ni= 1;  /* "where" function often leaves sequential lists */
          li= list[i];
          for (j=li+1 ; i+1<n && j==list[i+1] ; i++, j++) ni++;
          Copy(copyBase, daddr+stride*li, saddr, ni);
          saddr+= size*ni;
        }
      } else {
        for (i=0 ; i<n ; i++) {
          Copy(copyBase, daddr+stride*list[i], saddr, 1L);
          saddr+= size;
        }
      }
    }
  }

  /* Always leave daddr as we found it for proper recursion. */
  daddr= daddr0;
}

void Scatter(void *src, void *dst, StructDef *base, long number,
             const Strider *strider)
{
  if (strider) {
    saddr= src;
    daddr= dst;
    FastScat= FScatters[base->dataOps->promoteID];
    if (!FastScat) {
      copyBase= base;
      Copy= base->Copy;
    }
    size= base->size;
    BlockScat(strider);
  } else {
    /* 0 Strider* uses the specified number instead */
    base->Copy(base, dst, src, number);
  }
}

/*--------------------------------------------------------------------------*/

/* Gather(dst, src, strider, base, number)
   gathers into the dst array from the src array, given the strider for
   the src array and the base StructDef for the objects to be gathered.
   If the strider is 0, the given number of contiguous objects is
   copied.
      ... evaluate x(index list) ...  */

/* Without special routines for gather and scatter, Yorick bogs down
   unacceptably of some platforms (e.g.- Crays).  */
typedef void FastGather(long, long, long*);
static FastGather *FastGath;
static FastGather FastCGath, FastSGath, FastIGath, FastLGath,
  FastFGath, FastDGath;
static FastGather *FGathers[]= { &FastCGath, &FastSGath, &FastIGath,
              &FastLGath, &FastFGath, &FastDGath, 0, 0 };

#undef OPERATION
#define OPERATION(opname, typeD) \
static void opname(long stride, long n, long *list) \
{ \
  typeD *dst= (typeD *)daddr,  *src= (typeD *)saddr; \
  if (stride!=size) { \
    if (!list) for (;n--;dst++,saddr+=stride) dst[0]= *((typeD*)saddr); \
    else for (;n--;dst++,list++) dst[0]= *((typeD*)(saddr+list[0]*stride)); \
  } else { \
    if (!list) for (; n-- ; dst++,src++) dst[0]= src[0]; \
    else for (; n-- ; dst++,list++) dst[0]= src[list[0]]; \
  } \
  daddr= (char *)dst; \
}

OPERATION(FastCGath, char)
OPERATION(FastSGath, short)
OPERATION(FastIGath, int)
OPERATION(FastLGath, long)
OPERATION(FastFGath, float)
OPERATION(FastDGath, double)

static void BlockGath(const Strider *strider)
{
  Strider *next= strider->next;
  long stride= strider->stride;
  long i, n= strider->number;
  Array *indexList= strider->indexList;

  char *saddr0= saddr;

  if (!indexList) {
    if (next) {
      for (i=0 ; i<n ; i++) {
        BlockGath(next);
        saddr+= stride;
        /* daddr already incremented by other branches of BlockGath */
      }
    } else if (FastGath) {
      FastGath(stride, n, (long *)0);
    } else if (stride!=size) {
      for (i=0 ; i<n ; i++) {
        Copy(copyBase, daddr, saddr, 1L);
        saddr+= stride;
        daddr+= size;
      }
    } else {
      Copy(copyBase, daddr, saddr, n);
      n*= size;
      daddr+= n;
    }

  } else {
    long *list= indexList->value.l;
    if (next) {
      for (i=0 ; i<n ; i++) {
        saddr= saddr0+stride*list[i];
        BlockGath(next);
        /* daddr already incremented by other branches of BlockGath */
      }
    } else if (FastGath) {
      FastGath(stride, n, list);
    } else {
      if (size==stride) {
        long j, li, ni;
        for (i=0 ; i<n ; i++) {
          ni= 1;  /* "where" function often leaves sequential lists */
          li= list[i];
          for (j=li+1 ; i+1<n && j==list[i+1] ; i++, j++) ni++;
          Copy(copyBase, daddr, saddr+stride*li, ni);
          daddr+= size*ni;
        }
      } else {
        for (i=0 ; i<n ; i++) {
          Copy(copyBase, daddr, saddr+stride*list[i], 1L);
          daddr+= size;
        }
      }
    }
  }

  /* Always leave saddr as we found it for proper recursion. */
  saddr= saddr0;
}

void Gather(void *dst, void *src, StructDef *base, long number,
            const Strider *strider)
{
  if (strider) {
    saddr= src;
    daddr= dst;
    FastGath= FGathers[base->dataOps->promoteID];
    if (!FastGath) {
      copyBase= base;
      Copy= base->Copy;
    }
    size= base->size;
    BlockGath(strider);
  } else {
    /* 0 Strider* uses the specified number instead */
    base->Copy(base, dst, src, number);
  }
}

/*--------------------------------------------------------------------------*/

static IOStream *file;

/* Reuse BlockScat code by creating a fake copy operation.
   This trick relies on (char* + long) - (char*) always returning the
   original long -- may not be true if there is some machine with a
   long longer than the largest char*...  */
static Copier CopyWrite;
static void CopyWrite(StructDef *base, void *dst, const void *src, long n)
{
  long trick= ((char *)dst) - ((char *)0);
  YcWrite(file, src, trick, n*size);
}

void CastWrite(void *src, long dst, StructDef *base, long number,
               const Strider *strider)
{
  char *trick= ((char *)0) + dst;
  file= base->file;   /* fails disastrously if this is 0 */
  size= base->size;
  if (!(file->permissions & 2))
    YErrorIO("attempt to write to read-only file");
  if (strider) {
    saddr= src;
    daddr= trick;
    copyBase= base;
    Copy= &CopyWrite;
    FastScat= 0;
    BlockScat(strider);
  } else {
    /* 0 Strider* uses the specified number instead */
    CopyWrite(base, trick, src, number);
  }
}

/* Reuse BlockGath code by creating a fake copy operation.
   This trick relies on (char* + long) - (char*) always returning the
   original long -- may not be true if there is some machine with a
   long longer than the largest char*...  */
static Copier CopyRead;
static void CopyRead(StructDef *base, void *dst, const void *src, long n)
{
  long trick= ((char *)src) - ((char *)0);
  n*= size;
  if (YcRead(file, dst, trick, n) < n)
    YErrorIO("encountered end-of-file before read completed");
}

void CastRead(void *dst, long src, StructDef *base, long number,
              const Strider *strider)
{
  char *trick= ((char *)0) + src;
  file= base->file;   /* fails disastrously if this is 0 */
  size= base->size;
  if (!(file->permissions & 1))
    YErrorIO("attempt to read from write-only file");
  if (strider) {
    saddr= trick;
    daddr= dst;
    copyBase= base;
    Copy= &CopyRead;
    FastGath= 0;
    BlockGath(strider);
  } else {
    /* 0 Strider* uses the specified number instead */
    CopyRead(base, dst, trick, number);
  }
}

/*--------------------------------------------------------------------------*/
