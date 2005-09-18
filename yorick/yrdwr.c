/*
 * $Id: yrdwr.c,v 1.1 2005-09-18 22:04:16 dhmunro Exp $
 * Implement YRead and YWrite functions, plus general gather/scatter
 * routines with StructDef format Converter.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "bcast.h"
#include "pstdlib.h"

/* Things to note:
   (1) If base->file!=0, then base->model!=0 (since the in-memory
       version of the StructDef can't refer to any particular file).
   (2) The Convert operator will be 0 for non-pointer,
       native-format disk files, since no conversion operation
       is required.
 */

extern void ReadGather(void *dst, void *srcM, long srcD, StructDef *base,
                       long number, const Strider *strider);
extern void WriteScatter(void *src, void *dstM, long dstD, StructDef *base,
                         long number, const Strider *strider);

static void *WriteRecurse(void *src, StructDef *base, long number);
extern void SetSequentialWrite(IOStream *file, long last);

static void ClearTmp(Array **tmpArray);
static StructDef *NeedsConversion(StructDef *base);

/*--------------------------------------------------------------------------*/

/* YRead and YWrite assume that the buffer passed to them is to hold
   the data in its FINAL format, that is, as base->model->...->model.  */

void YRead(void *dst, long src, StructDef *base, long number,
           const Strider *strider)
{
  if (base->file && base->addressType==2 && strider)
    YError("cannot use a strider reading sequential object from file");
  ReadGather(dst, (void *)0, src, base, number, strider);
}

void YWrite(void *src, long dst, StructDef *base, long number,
            const Strider *strider)
{
  if (base->file && base->addressType==2) {
    if (strider)
      YError("cannot use a strider writing sequential object to file");
    SetSequentialWrite(base->file, dst+base->size*number);
  }
  WriteScatter(src, (void *)0, dst, base, number, strider);
}

void SetSequentialWrite(IOStream *file, long last)
{
  if (last<file->nextAddress)
    YError("sequential object must be written at end of binary file");
  /* Converter for sequential object may need to know address at
     end of object being written, since the corresponding write
     occurs AFTER the Converter is called.  */
  file->seqAddress= last;
}

/*--------------------------------------------------------------------------*/

static Array *tmp1Array= 0, *tmp2Array= 0;

void ClearTmpArray(void)
{
  ClearTmp(&tmp1Array);
  ClearTmp(&tmp2Array);
}

Array *NewTmpArray(StructDef *base, Dimension *dims)
{
  Array *array= NewArray(base, dims);
  if (tmp1Array) {
    if (tmp2Array) {   /* both tmp1 and tmp2 exist -- deallocate tmp1 */
      Array *tmp= tmp2Array;
      ClearTmp(&tmp1Array);
      tmp2Array= array;
      tmp1Array= tmp;
    } else {           /* only tmp1 exists, leave it alone */
      tmp2Array= array;
    }
  } else {             /* neither tmp1 nor tmp2 exist, use tmp1 */
    tmp1Array= array;
  }
  return array;
}

static void ClearTmp(Array **tmpArray)
{
  Array *array= *tmpArray;
  *tmpArray= 0;
  Unref(array);
}

static StructDef *NeedsConversion(StructDef *base)
{
  while (base) {
    if (base->Convert) return base;
    base= base->model;
  }
  return 0;
}

/*--------------------------------------------------------------------------*/

/* In a read/gather operation, base describes the data type of the
   source (which is either in memory or on disk).  The destination is
   assumed to have been allocated prior to this call, and to have the
   data type base->model->...->model.
   If base->file, then srcM==0 will cause a subsequent call to
   ReadPointees, while if srcM!=0, the call will not be made
   (this is intended to prevent recursion).  */
void ReadGather(void *dst, void *srcM, long srcD, StructDef *base,
                long number, const Strider *strider)
{
  StructDef *preModel= NeedsConversion(base);
  void *dstM;
  int doPointees= (srcM==0);

  if (preModel) {
    Array *array;
    Dimension *dims;
    ClearTmpArray();
    dims= NewDimension(number, 1L, (Dimension *)0);
    array= NewTmpArray(preModel, dims);
    dims->references--;
    dstM= array->value.c;
  } else {
    dstM= dst;
  }

  if (base->file) CastRead(dstM, srcD, base, number, strider);
  else Gather(dstM, srcM, base, number, strider);

  if (preModel) {
    StructDef *model= preModel->model;
    Converter *Convert= preModel->Convert;
    Converter *NextConvert= 0;

    while (model) {
      while (model->model && !model->Convert) model= model->model;

      srcM= dstM;
      if ((NextConvert= model->Convert)) {
        /* There are two temporary arrays; NewTmpArray automatically
           frees the array it allocated two calls before.  Both
           arrays are deallocated by ClearTmpArray, which is also called
           from YError.  */
        Dimension *dims= NewDimension(number, 1L, (Dimension *)0);
        Array *array= NewTmpArray(model, dims);
        dims->references--;
        dstM= array->value.c;
      } else {
        /* last pass always takes this branch */
        dstM= dst;
      }

      Convert(preModel, srcM, dstM, number, 0);
      preModel= model;
      model= model->model;
      Convert= NextConvert;
    }

    ClearTmpArray();
  }

  if (doPointees && base->file) ReadPointees(base->file);
}

/* In a write/scatter operation, base describes the data type of the
   destination (which is either in memory or on disk).  The source is
   assumed to have been allocated prior to this call, and to have the
   data type base->model->...->model.
   If base->file, then dstM==0 will cause a subsequent call to
   WritePointees, while if dstM!=0, the call will not be made
   (this is intended to prevent recursion).  */
void WriteScatter(void *src, void *dstM, long dstD, StructDef *base,
                  long number, const Strider *strider)
{
  StructDef *preModel= NeedsConversion(base);
  void *sbuffer;

  if (preModel) {
    ClearTmpArray();
    /* Note that SetSequentialWrite has set seqAddress properly BEFORE
       the call to WriteScatter -- DO NOT do anything like this:
       if (base->file && base->addressType==2) base->file->seqAddress= dstD;
     */
    sbuffer= WriteRecurse(src, preModel, number);

  } else {
    sbuffer= src;
  }

  if (base->file) {
    IOStream *file= base->file;
    long nextAddress= file->nextAddress;
    CastWrite(sbuffer, dstD, base, number, strider);
    if (!dstM) {
      WritePointees(file);
      if (nextAddress<file->nextAddress) FlushFile(file, 0);
    }
  } else {
    Scatter(sbuffer, dstM, base, number, strider);
  }

  if (preModel) ClearTmpArray();
}

static void *WriteRecurse(void *src, StructDef *base, long number)
{
  StructDef *model= base->model;
  Converter *Convert= base->Convert;  /* guaranteed non-zero */
  Array *darray;
  void *dst;
  Dimension *dims;

  while (model->model && !model->Convert) model= model->model;

  if (model->Convert) src= WriteRecurse(src, model, number);

  /* There are two temporary arrays; NewTmpArray automatically
     frees the array it allocated two calls before.  Both
     arrays are deallocated by ClearTmpArray, which is also called
     from YError.  */
  dims= NewDimension(number, 1L, (Dimension *)0);
  darray= NewTmpArray(base, dims);
  dims->references--;
  dst= darray->value.c;

  Convert(base, dst, src, number, 1);
  return dst;
}

/*--------------------------------------------------------------------------*/

void ReadPointees(IOStream *file)
{
  long nValid= file->pointeeList.nValid;
  long n= file->pointeeList.table.nItems - nValid;
  MDinfo *mdInfo= &file->pointeeList.mdInfo[nValid];
  Array *array;
  int noRecursion= 1;

  if (file->pointeeList.writing!=0) return;

  /* mark that read is in progress */
  file->pointeeList.writing= 2;

  /* The Convert routine has already read the header, storing
     the addresses of the header, the data, and the memory address
     of a newly created Array into pointeeList.mdInfo.  */

  while (n>0) {
    if (mdInfo->m) {
      array= Pointee(mdInfo->m);
      ReadGather(array->value.c, &noRecursion, mdInfo->data,
                 mdInfo->base, array->type.number, (Strider *)0);
    }

    /* note that pointeeList.table.nItems may have increased
       during the reading of this pointee */
    file->pointeeList.nValid= (++nValid);
    mdInfo= &file->pointeeList.mdInfo[nValid];
    n= file->pointeeList.table.nItems - nValid;
  }

  /* successful completion of read */
  file->pointeeList.writing= 0;
}

void WritePointees(IOStream *file)
{
  long nValid= file->pointeeList.nValid;
  long n= file->pointeeList.table.nItems - nValid;
  MDinfo *mdInfo= &file->pointeeList.mdInfo[nValid];
  Array *array;
  int noRecursion= 1;

  if (file->pointeeList.writing!=1) return;

  /* mark that write is in progress */
  file->pointeeList.writing= 3;

  /* The Convert routine has already written the header, storing
     the addresses of the header, the data, and the memory data
     into pointeeList.mdInfo.  */

  while (n>0) {
    if (mdInfo->m) {
      array= Pointee(mdInfo->m);
      WriteScatter(array->value.c, &noRecursion, mdInfo->data,
                   mdInfo->base, array->type.number, (Strider *)0);
    }

    /* note that pointeeList.table.nItems may have increased
       during the writing of this pointee */
    file->pointeeList.nValid= (++nValid);
    mdInfo= &file->pointeeList.mdInfo[nValid];
    n= file->pointeeList.table.nItems - nValid;
  }

  /* successful completion of write */
  file->pointeeList.writing= 1;
}

void ClearPointees(IOStream *file, int writing)
{
  long i, nItems= file->pointeeList.table.nItems;
  MDinfo *mdInfo;
  Array *array;

  if (nItems && !(file->pointeeList.writing&2)) {
    /* Note that this is skipped if failed at previous attempt to
       call WritePointees or ReadPointees.  */
    if (file->pointeeList.writing) WritePointees(file);
    else ReadPointees(file);
    /* tracking down pointers may have changed nItems, mdInfo */
    nItems= file->pointeeList.table.nItems;
  }

  mdInfo= file->pointeeList.mdInfo;
  HashXClear(&file->pointeeList.table);
  file->pointeeList.nValid= 0;
  file->pointeeList.mdInfo= 0;
  file->pointeeList.writing= writing;

  for (i=0 ; i<nItems ; i++) {
    array= Pointee(mdInfo[i].m);
    Unref(array);
  }
  p_free(mdInfo);
}

/*--------------------------------------------------------------------------*/
