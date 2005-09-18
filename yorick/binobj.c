/*
 * $Id: binobj.c,v 1.1 2005-09-18 22:03:50 dhmunro Exp $
 * Implement functions for generic blocks of data.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "bcast.h"
#include "defmem.h"
#include "pstdlib.h"
#include <string.h>

extern void FreeMemberList(Member *member, long n);

/*--------------------------------------------------------------------------*/

/* Set up a block allocator which grabs space for 64 scalar array objects
   at a time.  Since Array contains an ops pointer, the alignment
   of an Array must be at least as strict as a void*.  */
static MemryBlock arrayBlock= {0, 0, sizeof(Array),
                                   64*sizeof(Array)};

Array *NewArray(StructDef *base, Dimension *dims)
{
  long number= TotalNumber(dims);
  long size= base->size;
  Array *array= (number*size>2*sizeof(double))?
    p_malloc(sizeof(Array)+number*size) : NextUnit(&arrayBlock);
  array->references= 0;
  array->ops= base->dataOps;
  array->type.base= Ref(base);
  array->type.dims= Ref(dims);
  array->type.number= number;
  /* Dangerous to try to initialize things with copy -- could leave
     junk in padding between struct members, which prevents comparison
     operations from working properly.  On the other hand, an array
     will normally be filled when it is initialized, so setting
     everything to 0 first may be a substantial performance penalty... */
  /* base->Copy(base, array->value.c, array->value.c, number);
     is NOT legal, since this is used by FreeArray to deallocate
     valid strings or pointers
     On the other hand,
     memset(array->value.c, 0, size*number);
     is gratuitous in the case of non-pointer objects */
  if (base->Copy!=&CopyX) memset(array->value.c, 0, size*number);
  return array;
}

void FreeArray(void *v)  /* ******* Use Unref(array) ******* */
{
  Array *array= v;
  long number;
  long size;
  StructDef *base;
  if (!array) return;
  base= array->type.base;
  number= TotalNumber(array->type.dims);
  size= base->size;
  base->Copy(base, array->value.c, array->value.c, number);
  Unref(base);
  FreeDimension(array->type.dims);
  if (number*size>2*sizeof(double)) p_free(array);
  else FreeUnit(&arrayBlock, array);
}

/* Set up a block allocator which grabs space for 16 StructDefs
   at a time.  Since StructDef contains an ops pointer, the alignment
   of a StructDef must be at least as strict as a void*.  */
static MemryBlock sdefBlock= {0, 0, sizeof(StructDef),
                                  16*sizeof(StructDef)};

StructDef *NewStructDef(IOStream *file, long index)
{
  StructDef *base= NextUnit(&sdefBlock);
  base->references= 0;
  base->ops= yOpsStructDef;
  base->dataOps= 0;   /* set by InstallStruct to validate */

  base->size= 0;
  base->alignment= 0;

  base->table.nItems= 0;
  base->table.maxItems= 0;
  base->table.names= 0;
  base->table.nSlots= 0;
  base->table.items= 0;

  base->members= 0;
  base->offsets= 0;

  base->Copy= &CopyX;

  base->file= file;     /* NOT Ref(file), since file owns base->reference */
  base->index= index;
  base->addressType= 0;
  base->model= 0;
  base->Convert= 0;
  base->order= 0;
  base->fpLayout= 0;
  return base;
}

void FreeStructDef(void *v)   /* *** Use Unref(base) *** */
{
  StructDef *base= v;
  if (!base->table.nItems && !base->file && base->index<=8) {
    base->references= 0;
    YError("(BUG?) attempt to free StructDef of basic data type");
  }
  FreeMemberList(base->members, base->table.nItems);
  HashClear(&base->table);
  p_free(base->members);
  p_free(base->offsets);
  Unref(base->model);
  /* NO Unref(base->file), see NewStructDef above */
  FreeFPLayout(base->fpLayout);
  FreeUnit(&sdefBlock, base);
}

void FreeMemberList(Member *member, long n)
{
  while (n > 0) {
    Unref(member->base);
    FreeDimension(member->dims);
    member++;
    n--;
  }
}

/* Set up a block allocator which grabs space for 256 shape objects
   at a time. */
union FourLongs {
  /* Since each of these structs contain a pointer, the
     alignment of a FourLongs must be at least as strict as a void*.  */
  Dimension d;
  Strider q;
};
static MemryBlock fourBlock= {0, 0, sizeof(union FourLongs),
                                 256*sizeof(union FourLongs)};

int yForceOrigin= 1;

Dimension *NewDimension(long number, long origin, Dimension *next)
{
  Dimension *dims= NextUnit(&fourBlock);
  dims->next= next;   /* THINK CAREFULLY -- usually don't want Ref(next) */
  dims->number= number;
  dims->origin= origin;
  dims->references= 0;
  return dims;
}

Dimension *
ynew_dim(long n, Dimension *next)
{
  Dimension *dims = tmpDims;
  tmpDims = 0;
  if (dims) FreeDimension(dims);
  return tmpDims = NewDimension(n, 1L, next);
}

void FreeDimension(Dimension *dims)
{
  if (dims && --dims->references<0) {
    FreeDimension(dims->next);
    FreeUnit(&fourBlock, dims);
  }
}

Strider *NewStrider(long stride, long number)
{
  Strider *strider= NextUnit(&fourBlock);
  strider->next= 0;
  strider->stride= stride;
  strider->number= number;
  strider->indexList= 0;
  return strider;
}

void FreeStrider(Strider *strider)
{
  if (strider) {
    FreeStrider(strider->next);
    Unref(strider->indexList);
    FreeUnit(&fourBlock, strider);
  }
}

/*--------------------------------------------------------------------------*/

Dimension *tmpDims= 0;

long TotalNumber(const Dimension *dims)
{
  long number= 1;
  while (dims) {
    number*= dims->number;
    dims= dims->next;
  }
  return number;
}

int CountDims(const Dimension *dims)
{
  int count;
  for (count=0 ; dims ; dims= dims->next) count++;
  return count;
}

/* Note-- like NewDimension, CopyDims does NOT do Ref(next) */
Dimension *CopyDims(Dimension *dims, Dimension *next, int copyOrigin)
{
  if (dims)
    return NewDimension(dims->number, copyOrigin? dims->origin : 1L,
                        CopyDims(dims->next, next, copyOrigin));
  else
    return next;
}

Strider *CopyStrider(Strider *strider, Strider *next)
{
  Strider *s;
  if (!strider) return next;
  s= NewStrider(strider->stride, strider->number);
  s->next= CopyStrider(strider->next, next);
  s->indexList= Ref(strider->indexList);
  return s;
}

/*--------------------------------------------------------------------------*/

/* The offsetof macro may be defined in <stddef.h> (it is ANSI standard).  */
/* #define offsetof(structure, member)  ((long)&(((structure*)0)->member))
   (does not work on Crays) */
#ifndef offsetof
#define offsetof(structure, member) \
  ((long)((char *)&(((structure*)0)->member) - (char *)0))
#endif

void *Pointee(void *pointer)
{
  if (pointer) return (char *)pointer - offsetof(Array, value);
  else return &nilDB;
}

/*--------------------------------------------------------------------------*/
/* Here are the four possibilities for the StructDef->Copy function.
   Note that they function properly when s==t on input-- namely,
   Copy(base, address, address, n) will properly initialize all pointers
   to 0 without any other initialization.  */

void CopyX(StructDef *base, void *s, const void *t, long n)
{
  /* memcpy is a serious performance bottleneck for basic data types
     -- this is a kludge to work around the fact that I misused
        &CopyX as a flag in a couple of places in the code
     many C compilers need hand unrolled loops */
  if (s!=t) {
    if (base==&doubleStruct) {
      double *ss= s;
      const double *tt= t;
      long i;
      for (i=0 ; i<(n&7) ; i++) ss[i]= tt[i];
      for (    ; i<n     ; i+=8) {
        ss[i  ]= tt[i];
        ss[i+1]= tt[i+1];
        ss[i+2]= tt[i+2];
        ss[i+3]= tt[i+3];
        ss[i+4]= tt[i+4];
        ss[i+5]= tt[i+5];
        ss[i+6]= tt[i+6];
        ss[i+7]= tt[i+7];
      }
    } else if (base==&longStruct) {
      long *ss= s;
      const long *tt= t;
      long i;
      for (i=0 ; i<(n&7) ; i++) ss[i]= tt[i];
      for (    ; i<n     ; i+=8) {
        ss[i  ]= tt[i];
        ss[i+1]= tt[i+1];
        ss[i+2]= tt[i+2];
        ss[i+3]= tt[i+3];
        ss[i+4]= tt[i+4];
        ss[i+5]= tt[i+5];
        ss[i+6]= tt[i+6];
        ss[i+7]= tt[i+7];
      }
    } else if (base==&intStruct) {
      int *ss= s;
      const int *tt= t;
      long i;
      for (i=0 ; i<(n&7) ; i++) ss[i]= tt[i];
      for (    ; i<n     ; i+=8) {
        ss[i  ]= tt[i];
        ss[i+1]= tt[i+1];
        ss[i+2]= tt[i+2];
        ss[i+3]= tt[i+3];
        ss[i+4]= tt[i+4];
        ss[i+5]= tt[i+5];
        ss[i+6]= tt[i+6];
        ss[i+7]= tt[i+7];
      }
    } else if (base==&floatStruct) {
      float *ss= s;
      const float *tt= t;
      long i;
      for (i=0 ; i<(n&7) ; i++) ss[i]= tt[i];
      for (    ; i<n     ; i+=8) {
        ss[i  ]= tt[i];
        ss[i+1]= tt[i+1];
        ss[i+2]= tt[i+2];
        ss[i+3]= tt[i+3];
        ss[i+4]= tt[i+4];
        ss[i+5]= tt[i+5];
        ss[i+6]= tt[i+6];
        ss[i+7]= tt[i+7];
      }
    } else {
      memcpy(s, t, n*base->size);
    }
  }
}

typedef char *charPtr;

/* ARGSUSED */
void CopyQ(StructDef *base, void *s, const void *t, long n)
{
  char **dst= s;
  const charPtr *src= (const charPtr *)t;
  while (n--) {
    p_free(*dst);
    *dst= 0;
    *dst++= p_strcpy(*src++);
  }
}

typedef void *voidPtr;

/* ARGSUSED */
void CopyP(StructDef *base, void *s, const void *t, long n)
{
  void **dst= s;
  const voidPtr *src= (const voidPtr *)t;
  void *oldArray;
  while (n--) {
    if ((oldArray= *dst)) {
      *dst= 0;   /* Unref might take some time, delete reference first */
      oldArray= Pointee(oldArray);
      Unref((Array *)oldArray);
    }
    if (*src) {                 /* Can't take this branch if dst==src. */
      oldArray= Pointee(*dst++= *src++);
      ((Array *)oldArray)->references++;
    } else {
      *dst++= 0;
      src++;
    }
  }
}

void CopyS(StructDef *base, void *s, const void *t, long n)
{
  long size= base->size;
  long nItems= base->table.nItems;
  long off, *offsets= base->offsets;
  Member *members= base->members;

  char *dst= s;
  const char *src= t;
  int initializing= (s==t);

  StructDef *subBase;
  long m, subTotal, subSize;
  Copier *SubCopy;

  /* outer loop is on number of items */
  while (n--) {

    /* inner loop is on base structure members */
    for (m=0 ; m<nItems ; /* m++ in loop body */) {
      subBase= members[m].base;
      SubCopy= subBase->Copy;
      /* skip initialization of direct data */
      if (initializing && SubCopy==CopyX) { m++; continue; }

      subTotal= members[m].number;
      off= offsets[m++];
      /* include contiguous members of the same type to avoid multiple
         calls to SubCopy */
      subSize= subBase->size;
      while (m<nItems && members[m].base==subBase &&
             offsets[m]==off+subSize*subTotal) {
        subTotal+= members[m].number;
        m++;
      }
      /* copy member or contiguous members (possibly recursive) */
      SubCopy(subBase, dst+off, src+off, subTotal);
    }

    /* end of loop on structure members */
    dst+= size;
    src+= size;
  } /* end of loop on number of items */
}

/*--------------------------------------------------------------------------*/
