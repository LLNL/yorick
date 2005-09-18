/*
 * $Id: convrt.c,v 1.1 2005-09-18 22:04:00 dhmunro Exp $
 * Implement primitive data type conversion.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "binio.h"
#include "pstdlib.h"
#include <string.h>

#ifndef NO_STDIO_H
#include <stdio.h>
#else
/* *********WARNING*********
   old SunOS (BSD?) declaration of sprintf is "char *sprintf()", so it is
   best to avoid using the return value from this function... */
extern int sprintf(char *s, const char *format, ...);
#endif

extern char *PDBvarType(char *typeName);

extern Converter PDBconvert;

static void PDBwriteHeader(IOStream *file, void *pointee);
static void PDBwRecurse(IOStream *file, StructDef *base, StructDef *model,
                        void *memory, long number);
static long PDBreadHeader(IOStream *file, long *nitems, StructDef **base,
                          long *address, int *dataHere, int skip);
static void PDBskipHeader(IOStream *file, StructDef *base, long number);
static char *PDBinitScratch(long n);

static int Mcompare(void *keyTable, long index, const void *key);
static void *Mindex(void *keyTable, long index);
static int Dcompare(void *keyTable, long index, const void *key);
static void *Dindex(void *keyTable, long index);

static void ReadLongs(StructDef *longS, long *buf, long addr, long n);
static void WriteLongs(StructDef *longS, long *buf, long addr, long n);

/*--------------------------------------------------------------------------*/

/* The proper Convert procedure for a compound data type is similar to
   the Copy procedure -- namely to descend into the structure members.  */

void ConvertS(StructDef *base, void *baseData, void *modelData,
              long n, int toBase)
{
  long size= base->size;
  long nItems= base->table.nItems;
  long off, *offsets= base->offsets;
  Member *members= base->members;

  StructDef *model= base->model;  /* type of modelData --  must have
                                     same nItems as base to use ConvertS */
  long msize= model->size;
  long moff, *moffs= model->offsets;
  Member *mmems= model->members;

  char *bd= baseData;
  char *md= modelData;

  StructDef *subBase, *msubBase;
  long m, subTotal, subSize, msubSize;

  /* outer loop is on number of items */
  while (n--) {

    /* inner loop is on base structure members */
    for (m=0 ; m<nItems ; /* m++ in loop body */) {
      subBase= members[m].base;
      msubBase= mmems[m].base;

      subTotal= members[m].number;
      off= offsets[m];
      moff= moffs[m++];
      /* include contiguous members of the same type to avoid multiple
         calls to Convert (must be contiguous in both base and model) */
      subSize= subBase->size;
      msubSize= msubBase->size;
      while (m<nItems && members[m].base==subBase &&
             offsets[m]==off+subSize*subTotal &&
             moffs[m]==moff+msubSize*subTotal) {
        subTotal+= members[m].number;
        m++;
      }

      /* convert member or contiguous members (possibly recursive) */
      if (subBase->Convert) {
        subBase->Convert(subBase, bd+off, md+moff, subTotal, toBase);
      } else if (subBase->Copy==&CopyX) {
        /* CopyX relies on alignment restrictions, but the foreign
           type here might not be aligned properly even if the binary
           format of the type itself agrees */
        if (toBase)
          memcpy(bd+off, md+moff, subTotal*subBase->model->size);
        else
          memcpy(md+moff, bd+off, subTotal*subBase->model->size);
      } else {
        /* is this branch unreachable?
           absence of Converter probably implies Copier is CopyX... */
        if (toBase)
          subBase->Copy(subBase->model, bd+off, md+moff, subTotal);
        else
          subBase->Copy(subBase->model, md+moff, bd+off, subTotal);
      }
    }

    /* end of loop on structure members */
    bd+= size;
    md+= msize;
  } /* end of loop on number of items */
}

/*--------------------------------------------------------------------------*/

/* A string is written as a long byte count followed by the data.  */

void ConvertQ(StructDef *base, void *baseData, void *modelData,
              long n, int toBase)
{
  char **modelP= modelData;
  char *baseAddr= baseData;
  long baseSize= base->size;
  IOStream *file= base->file;
  StructDef *longS= file->structList[3];
  long longSize= longS->size;
  Copier *LongCopy= longS->Copy;
  Converter *LongConvert= longS->Convert;
  long address, length;

  if (toBase) {
    /* this is a write operation */
    int longAlign= file->dataAlign? file->dataAlign : longS->alignment;

    while (n--) {
      if (!*modelP) {
        address= -1;

      } else {
        /* disk address is next available, after long alignment */
        address= AlignAdjust(file->nextAddress, longAlign);
        length= strlen(*modelP);
        file->nextAddress= address + longSize+length;
        WriteLongs(longS, &length, address, 1L);
        if (length) YcWrite(file, *modelP, address+longSize, length);
      }

      if (LongConvert) LongConvert(longS, baseAddr, &address, 1L, 1);
      else LongCopy(longS, baseAddr, &address, 1L);
      modelP++;
      baseAddr+= baseSize;
    }

  } else {
    /* this is a read operation */
    while (n--) {
      if (LongConvert) LongConvert(longS, baseAddr, &address, 1L, 0);
      else LongCopy(longS, &address, baseAddr, 1L);

      if (address<0) {
        *modelP= 0;

      } else {
        ReadLongs(longS, &length, address, 1L);
        if (length<0) {
          *modelP= 0;
        } else {
          *modelP= p_malloc(length+1);
          if (length) YcRead(file, *modelP, address+longSize, length);
          (*modelP)[length]= '\0';
        }
      }

      modelP++;
      baseAddr+= baseSize;
    }
  }
}

/*--------------------------------------------------------------------------*/

void ConvertP(StructDef *base, void *baseData, void *modelData,
              long n, int toBase)
{
  void **modelP= modelData;
  char *baseAddr= baseData;
  long baseSize= base->size;

  IOStream *file= base->file;
  StructDef **structList= file->structList;
  StructDef *longS= structList[3];
  long longSize= longS->size;
  int longAlign= file->dataAlign? file->dataAlign : longS->alignment;
  Copier *LongCopy= longS->Copy;
  Converter *LongConvert= longS->Convert;

  Array *array;
  Dimension *dims;
  long address, index;
  int dataAlign, nDims, i;
  long scratch[12];

  if (toBase) {
    /* this is a write operation */
    if (!(file->pointeeList.writing&1)) ClearPointees(file, 1);
    while (n--) {
      if (!*modelP) {
        /* this is a NULL pointer */
        address= -1;

      } else if (HashXAdd(&file->pointeeList.table,
                          (char *)modelP, sizeof(void *),
                          &Mcompare, &Mindex, file->pointeeList.mdInfo)) {
        /* this pointee has already been written */
        address= file->pointeeList.mdInfo[hashIndex].d;

      } else {
        /* this pointee has not been written yet */
        long dataAddr;
        HASH_MANAGE(file->pointeeList.table, MDinfo,
                    file->pointeeList.mdInfo);
        file->pointeeList.mdInfo[hashIndex].m= 0;  /* do this quickly */
        file->pointeeList.mdInfo[hashIndex].base= 0;  /* do this quickly */
        index= hashIndex;

        /* find disk equivalent StructDef for this pointee
           NOTE THAT DISK NAME WILL MATCH MEMORY NAME ALWAYS */
        array= Pointee(*modelP);
        if (!HashFind(&file->structTable, StructName(array->type.base), 0L))
          YError("data type of pointee not installed writing binary file");
        base= structList[hashIndex];

        /* disk address is next available, after long alignment */
        address= AlignAdjust(file->nextAddress, longAlign);

        file->pointeeList.mdInfo[index].m= Ref(array)->value.c;
        file->pointeeList.mdInfo[index].d= address;
        /* Note that the reference count to base need not be incremented,
           since base is owned by file, which also owns the pointeeList.  */
        file->pointeeList.mdInfo[index].base= base;
        dataAlign= file->dataAlign? file->dataAlign : base->alignment;
        dims= array->type.dims;
        nDims= CountDims(dims);
        if (nDims>10)
          YError("pointee has>10 dimensions writing binary file");
        file->pointeeList.mdInfo[index].data= dataAddr=
          AlignAdjust(address + (2+nDims)*longSize, dataAlign);

        /* update next available disk address */
        file->nextAddress= dataAddr + array->type.number*base->size;

        /* write the pointee header (type,#dims,dim1,dim2,...) */
        scratch[0]= base->index;
        scratch[1]= nDims;
        for (i=2 ; i<2+nDims ; i++) {
          scratch[i]= dims->number;
          dims= dims->next;
        }
        WriteLongs(longS, scratch, address, 2L+nDims);
      }

      if (LongConvert) LongConvert(longS, baseAddr, &address, 1L, 1);
      else LongCopy(longS, baseAddr, &address, 1L);
      modelP++;
      baseAddr+= baseSize;
    }

  } else {
    /* this is a read operation */
    long nStructs= file->structTable.nItems;
    if (file->pointeeList.writing&1) ClearPointees(file, 0);
    while (n--) {
      if (LongConvert) LongConvert(longS, baseAddr, &address, 1L, 0);
      else LongCopy(longS, &address, baseAddr, 1L);

      if (address<0) {
        /* this is a NULL pointer */
        *modelP= 0;

      } else if (HashXAdd(&file->pointeeList.table,
                          (char *)&address, sizeof(long),
                          &Dcompare, &Dindex, file->pointeeList.mdInfo)) {
        /* this pointee has already been read */
        array= Pointee(file->pointeeList.mdInfo[hashIndex].m);
        *modelP= Ref(array)->value.c;

      } else {
        /* this pointee has not been read yet */
        StructDef *model;
        HASH_MANAGE(file->pointeeList.table, MDinfo,
                    file->pointeeList.mdInfo);
        file->pointeeList.mdInfo[hashIndex].m= 0;
        file->pointeeList.mdInfo[hashIndex].d= 0;
        file->pointeeList.mdInfo[hashIndex].data= 0;
        file->pointeeList.mdInfo[hashIndex].base= 0;
        index= hashIndex;

        /* read the pointee header (type,#dims,dim1,dim2,...) */
        ReadLongs(longS, scratch, address, 2L);
        nDims= scratch[1];
        if (scratch[0]<0 || scratch[0]>=nStructs || nDims<0 || nDims>10)
          YError("illegal pointee while reading binary file");
        base= structList[scratch[0]];
        if (nDims) ReadLongs(longS, scratch, address+2*longS->size,
                             (long)nDims);

        /* create an uninitialized pointee */
        model= base->model;
        while (model->model) model= model->model;
        dims= tmpDims;
        tmpDims= 0;
        FreeDimension(dims);
        for (i=nDims-1 ; i>=0 ; i--)
          tmpDims= NewDimension(scratch[i], 1L, tmpDims);
        array= NewArray(model, tmpDims);

        /* fill in the new mdInfo */
        file->pointeeList.mdInfo[index].m= *modelP= Ref(array)->value.c;
        file->pointeeList.mdInfo[index].d= address;
        dataAlign= file->dataAlign? file->dataAlign : base->alignment;
        file->pointeeList.mdInfo[index].data=
          AlignAdjust(address + (2+nDims)*longSize, dataAlign);
        /* Note that the reference count to base need not be incremented,
           since base is owned by file, which also owns the pointeeList.  */
        file->pointeeList.mdInfo[index].base= base;
      }

      modelP++;
      baseAddr+= baseSize;
    }
  }
}

/*--------------------------------------------------------------------------*/

/* Converting PDB pointer types is a tedious task.
   The problem is to reconcile the depth-first sequential pointee
   layout of PDB-style pointers with the breadth-first random
   pointee layout of the Yorick format conversion machinery.  */

void PDBconvert(StructDef *base, void *baseData, void *modelData,
                long n, int toBase)
{
  void **modelP= modelData;

  IOStream *file= base->file;

  Array *array;
  Dimension *dims;
  long headAddr, dataAddr, nitems, index;
  int dataHere;

  if (toBase) {
    /* this is a write operation */
    if (!(file->pointeeList.writing&1)) ClearPointees(file, 1);
    if (file->seqAddress<file->nextAddress) {
      /* This must be a call generated by WritePointees, rather than
         from the top-level object being written.  Top level calls
         all start from StoreLValue or YWrite, both of which ensure
         that file->seqAddress>=file->nextAddress, and that this
         write is at the end-of-file.  Only Converters have been
         called since then, and the other branch of this if test
         assures that file->seqAddress==file->nextAddress throughout
         the top-level Converter calls.
         There is nothing left to do here, since PDB-style pointers
         either contain garbage or do not exist.  */
      return;
    }

    /* This Converter was called while converting a top-level
       object, as opposed to a pointee.  The headers for the entire
       pointee tree must be written now, since PDB-style pointers
       are written depth-first, and Yorick's Converter logic wants
       to write them one level at a time.  Care is taken here to
       assure that file->seqAddress==file->nextAddress when this
       routine exits, so that subsequent calls to this Converter
       from this same top-level object will also take this branch.
       YWrite and StoreLValue both assure this condition for
       top-level objects, but when the top-level object itself is
       written, after the top-level Converter calls, then
       file->seqAddress<file->nextAddress, owing to the space
       reserved for the pointees here.  The call to WritePointees
       which follows the top-level write operation, therefore takes
       the above no-op branch.  */

    while (n--) PDBwriteHeader(file, *modelP++);

  } else {
    /* this is a read operation */
    if (file->pointeeList.writing&1) ClearPointees(file, 0);

    while (n--) {
      dataAddr=
        PDBreadHeader(file, &nitems, &base, &headAddr, &dataHere, 1);

      if (headAddr<0) {
        /* this is a NULL pointer */
        *modelP= 0;

      } else if (HashXAdd(&file->pointeeList.table,
                          (char *)&headAddr, sizeof(long),
                          &Dcompare, &Dindex, file->pointeeList.mdInfo)) {
        /* this pointee has already been read */
        array= Pointee(file->pointeeList.mdInfo[hashIndex].m);
        *modelP= Ref(array)->value.c;

      } else {
        /* this pointee has not been read yet */
        StructDef *model;
        HASH_MANAGE(file->pointeeList.table, MDinfo,
                    file->pointeeList.mdInfo);
        file->pointeeList.mdInfo[hashIndex].m= 0;
        file->pointeeList.mdInfo[hashIndex].d= 0;
        file->pointeeList.mdInfo[hashIndex].data= 0;
        file->pointeeList.mdInfo[hashIndex].base= 0;
        index= hashIndex;

        /* if the data is not here, find it now */
        if (!dataHere) {
          long seqAddr= file->seqAddress;
          file->seqAddress= headAddr;
          dataAddr=
            PDBreadHeader(file, &nitems, &base, &headAddr, &dataHere, 0);
          file->seqAddress= seqAddr;
          if (dataAddr<0)
            YError("bad not-here PDB-style pointer reading binary file");
        }

        /* create an uninitialized pointee */
        model= base->model;
        while (model->model) model= model->model;
        dims= tmpDims;
        tmpDims= 0;
        FreeDimension(dims);
        if (nitems>1) tmpDims= NewDimension(nitems, 1L, tmpDims);
        array= NewArray(model, tmpDims);

        /* fill in the new mdInfo */
        file->pointeeList.mdInfo[index].m= *modelP= Ref(array)->value.c;
        file->pointeeList.mdInfo[index].d= headAddr;
        file->pointeeList.mdInfo[index].data= dataAddr;
        /* Note that the reference count to base need not be incremented,
           since base is owned by file, which also owns the pointeeList.  */
        file->pointeeList.mdInfo[index].base= base;
      }

      modelP++;
    }
  }
}

static char voidType[]= "char";

static void PDBwriteHeader(IOStream *file, void *pointee)
{
  StructDef *base= 0;
  Array *array= 0;
  char *header, *typeName;
  long headLen, number, address, index= 0;
  int dataHere;

  if (!pointee) {
    /* this is a NULL pointer -- write header to scratch space */
    number= 0;
    typeName= voidType;
    address= -1;
    dataHere= 0;

    header= PDBinitScratch(76L);
    sprintf(header, "%ld\001%s\001%ld\001%d\001\n", 0L, "char", -1L, 0);
    headLen= strlen(header);

  } else {
    if (HashXAdd(&file->pointeeList.table, (char *)&pointee, sizeof(void *),
                 &Mcompare, &Mindex, file->pointeeList.mdInfo)) {
      dataHere= 0;
      address= file->pointeeList.mdInfo[hashIndex].d;

    } else {
      HASH_MANAGE(file->pointeeList.table, MDinfo,
                  file->pointeeList.mdInfo);
      file->pointeeList.mdInfo[hashIndex].m= 0;  /* quickly */
      file->pointeeList.mdInfo[hashIndex].base= 0;  /* quickly */
      index= hashIndex;
      dataHere= 1;
      address= file->seqAddress;
    }

    index= hashIndex;

    /* Find structure equivalent to array->type.base in file.  */
    array= Pointee(pointee);
    number= array->type.number;
    typeName= StructName(array->type.base);
    if (!HashFind(&file->structTable, typeName, 0L))
      YError("data type of pointee not installed writing binary file");
    base= file->structList[hashIndex];
  }

  header= PDBinitScratch(72+strlen(typeName));

  /* write header to scratch space */
  sprintf(header, "%ld\001%s\001%ld\001%d\001\n",
          number, PDBvarType(typeName), address, dataHere);
  headLen= strlen(header);
  if (address<0) address= file->seqAddress;

  /* if the data is here, update the pointeeList */
  if (dataHere) {
    file->pointeeList.mdInfo[index].m= Ref(array)->value.c;
    file->pointeeList.mdInfo[index].d= address;
    file->pointeeList.mdInfo[index].data= address+headLen;
    /* Note that the reference count to base need not be incremented,
       since base is owned by file, which also owns the pointeeList.  */
    file->pointeeList.mdInfo[index].base= base;
  }

  /* write the header to the file --
     This always updates file->seqAddress to address+headLen.
     Less obviously, file->nextAddress will always be updated to this
     same value, since file->seqAddress>=file->nextAddress is
     guaranteed on entry.  */
  YcWrite(file, header, address, headLen);

  if (dataHere) {
    /* set seqAddress beyond where DATA will be written */
    file->seqAddress= (file->nextAddress+= base->size*number);

    if (dataHere && base->addressType==2) {
      /* recurse to write out headers for all pointees at this level */
      if (base->Convert==&PDBconvert) {
        void **modelP= pointee;
        while (number--) PDBwriteHeader(file, *modelP++);

      } else if (base->Convert==&ConvertS) {
        PDBwRecurse(file, base, array->type.base, pointee, number);
      }
    }
  }
}

static void PDBwRecurse(IOStream *file, StructDef *base, StructDef *model,
                        void *memory, long number)
{
  long size= model->size;
  long n, i, nItems= model->table.nItems;
  Member *members= model->members;
  long *offsets= model->offsets;
  char *outer= memory;
  char *inner;
  void **modelP;

  while (number--) {
    for (i=0 ; i<nItems ; i++) {
      model= members[i].base;
      if (model->addressType==2) {
        inner= outer+offsets[i];
        n= members[i].number;
        if (model->Convert==&PDBconvert) {
          modelP= (void **)inner;
          while (n--) PDBwriteHeader(file, *modelP++);
        } else if (model->Convert==&ConvertS) {
          PDBwRecurse(file, base, model, inner, n);
        }
      }
    }
    outer+= size;
  }
}

/* this must position the file to the next address after the entire
   pointer chain descending from the current file->seqAddress */
static long PDBreadHeader(IOStream *file, long *nitems, StructDef **base,
                          long *address, int *dataHere, int skip)
{
  long seqAddress= file->seqAddress;
  long len, nChars;
  char *header, *token, *typeName;
  long number, dataAddr;
  int here;
  StructDef *pteBase;

  /* read 1st 128 characters, and scan for terminating "\n" */
  header= PDBinitScratch(0L);
  nChars= YcRead(file, header, seqAddress, 128L);
  header[nChars]= '\0';
  len= nChars= strcspn(header, "\012\015\037");  /* scan for PDB "\n" */

  /* continue reading 128 character blocks until "\n" found --
     but give up after 8 tries */
  while (nChars==128 && len<1024) {
    header= PDBinitScratch(-1L);
    token= header+len;
    nChars= YcRead(file, token, seqAddress, 128L);
    token[nChars]= '\0';
    nChars= strcspn(token, "\012\015\037");  /* scan for PDB "\n" */
    len+= nChars;
  }

  /* reset file->seqAddress to reflect the byte just after the "\n" */
  if (len<1024) header[len++]= '\0';  /* replace "\n" by "\0" */
  seqAddress= seqAddress+len;
  file->seqAddress= seqAddress;

  /* pointee header format is:
     "%ld\001%s\001%ld\001%d\001\n", nitems, full_type, address, dataHere
     -- a NULL pointer is represented by nitems==0, address==-1, and
     dataHere==0 */

  token= strtok(header, "\001");
  if (!token)
    YError("no nitems reading PDB-style pointee from binary file");
  number= strtol(token, (char **)0, 10);

  typeName= strtok((char *)0, "\001");
  if (!typeName)
    YError("no data type reading PDB-style pointee from binary file");

  token= strtok((char *)0, "\001");
  if (!token) {
    /* current PDBLib version treats this as if it were NULL, although
       fossilized remains within the coding suggest that it used to
       mean dataAddr==seqAddr and here==1 ... */
    number= 0;
    dataAddr= -1;
    here= 0;

  } else {
    dataAddr= strtol(token, (char **)0, 10);
    token= strtok((char *)0, "\001");
    if (!token) here= 1;
    else here= (int)strtol(token, (char **)0, 10);

    if (dataAddr==-1 || number==0) {
      /* ensure consistent treatment of NULL pointers */
      number= 0;
      dataAddr= -1;
      here= 0;
    }
  }

  typeName= PDBvarType(typeName);
  if (!HashFind(&file->structTable, typeName, 0L))
    YError("unknown data type reading PDB-style pointee from binary file");
  pteBase= file->structList[hashIndex];

  /* if data is here, set seqAddress beyond end-of-data --
     note that all PDB-style pointers have pteBase->size==0 */
  if (here) {
    file->seqAddress+= number*pteBase->size;

    if (skip && pteBase->addressType==2) {
      /* Do NOT add to the file->pointeeList here -- these will be read
         again directly by PDBconvert when the pointeeList is read.
         Because PDB-style pointees are written depth first, it is
         impossible to read them in a single pass without keeping a
         stack of partially read objects of arbitrary depth.  */
      PDBskipHeader(file, pteBase, number);
    }

  } else {
    seqAddress= -1;
  }

  *nitems= number;
  *base= pteBase;
  *address= dataAddr;
  *dataHere= here;

  /* Return the seqAddress of the data for THIS pointee
     (file->seqAddress is possibly far beyond this), or -1 if the
     dataHere flag is not set.  */
  return seqAddress;
}

static void PDBskipHeader(IOStream *file, StructDef *base, long number)
{
  long n, addr;
  int here;

  if (base->Convert==PDBconvert) {
    /* pointee is itself a pointee -- eventually the recursion into
       structure containing pointer members must end here */
    while (number--)
      PDBreadHeader(file, &n, &base, &addr, &here, 1);

  } else if (base->Convert==ConvertS) {
    /* pointee is a structure instance which itself contains pointees */
    long im, nm= base->table.nItems;
    Member *members= base->members;
    while (number--) {
      for (im=0 ; im<nm ; im++) {
        base= members[im].base;
        if (base->addressType==2) {
          /* the indirect members may either themselves be pointers,
             or direct data structures which contain pointers */
          PDBskipHeader(file, base, members[im].number);
        }
      }
    }
  }
}

static char *pdbScratch= 0;
static long maxPDBscratch= 0;

static char *PDBinitScratch(long n)
{
  if (n==0) {
    if (maxPDBscratch==128) return pdbScratch;
    n= 128;

  } else if (n<0) {
    n= maxPDBscratch+128;

  } else {
    n= (1+(n-1)/128)*128;
    if (n<=maxPDBscratch && maxPDBscratch<=256) return pdbScratch;
  }

  pdbScratch= p_realloc(pdbScratch, n+1);
  maxPDBscratch= n;
  return pdbScratch;
}

/*--------------------------------------------------------------------------*/

static void ReadLongs(StructDef *longS, long *buf, long addr, long n)
{
  if (longS->Convert) {
    char foreign[160];  /* never need to read more than 10 at a time
                           -- assume longS->size <= 16 bytes */
    YcRead(longS->file, foreign, addr, n*longS->size);
    longS->Convert(longS, foreign, buf, n, 0);
  } else {
    YcRead(longS->file, buf, addr, n*longS->size);
  }
}

static void WriteLongs(StructDef *longS, long *buf, long addr, long n)
{
   if (longS->Convert) {
    char foreign[192];  /* never need to write more than 12 at a time
                           -- assume longS->size <= 16 bytes */
    longS->Convert(longS, foreign, buf, n, 1);
    YcWrite(longS->file, foreign, addr, n*longS->size);
  } else {
    YcWrite(longS->file, buf, addr, n*longS->size);
  }
}

/*--------------------------------------------------------------------------*/

static int Mcompare(void *keyTable, long index, const void *key)
{
  MDinfo *mdInfo= keyTable;
  return mdInfo[index].m!=*(void**)key;
}

static void *Mindex(void *keyTable, long index)
{
  MDinfo *mdInfo= keyTable;
  return &mdInfo[index].m;
}

static int Dcompare(void *keyTable, long index, const void *key)
{
  MDinfo *mdInfo= keyTable;
  return mdInfo[index].d!=*(long *)key;
}

static void *Dindex(void *keyTable, long index)
{
  MDinfo *mdInfo= keyTable;
  return &mdInfo[index].d;
}

/*--------------------------------------------------------------------------*/

static void IntConvert(char *dst, long dstSize, int dstOrder,
                       char *src, long srcSize, int srcOrder, long n);
static void PartialCopy(char *dst, int dstSize, char *src, int srcSize,
                        long n, int nBytes);
static void SignCopy(char *dst, int dstSize, char *src, int srcSize,
                     long n, int nBytes);
static void Swap(char *dst, int dstSize, long n);
static void SlowCopy(char *dst, int dstSize, char *src, int srcSize,
                     long n, int srcOrder);

void ConvertI(StructDef *base, void *baseData, void *modelData,
              long n, int toBase)
{
  StructDef *model= base->model;

  if (toBase) {
    IntConvert(baseData, base->size, base->order,
               modelData, model->size, model->order, n);
  } else {
    IntConvert(modelData, model->size, model->order,
               baseData, base->size, base->order, n);
  }
}

static void FloatConvert(char *dst, long dstSize, int dstOrder,
                         FPLayout *dstLayout, char *src, long srcSize,
                         int srcOrder, FPLayout *srcLayout, long n);
static void DoMantissa(char *dst, long dstSize, char *src, long srcSize,
                       int srcOrder, int srcWord, long n);
static void GenericMant(char *dst, long dstSize, char *src, long srcSize,
                        int srcOrder, int srcWord, long n,
                        int dst0, int dst1, int src0, int src1,
                        int shft, int mask0, int mask1, int offs);
static void DoSign(char *dst, long dstSize, char *src, long srcSize,
                   int srcOrder, int srcWord, long n);
static void DoExponent(char *dst, long dstSize, char *src, long srcSize,
                       int srcOrder, int srcWord, long n);

void ConvertF(StructDef *base, void *baseData, void *modelData,
              long n, int toBase)
{
  StructDef *model= base->model;

  if (toBase) {
    FloatConvert(baseData, base->size, base->order, base->fpLayout,
                 modelData, model->size, model->order, model->fpLayout, n);
  } else {
    FloatConvert(modelData, model->size, model->order, model->fpLayout,
                 baseData, base->size, base->order, base->fpLayout, n);
  }
}

/*--------------------------------------------------------------------------*/

/* Integer format conversion is a matter of byte swapping, with the
   added complication of sign-extension if the destination format is
   longer than the source format.  */

static void IntConvert(char *dst, long dstSize, int dstOrder,
                       char *src, long srcSize, int srcOrder, long n)
{
  int dstWord= dstOrder<0? -dstOrder : dstOrder;
  int srcWord= srcOrder<0? -srcOrder : srcOrder;
  int opposite= ((srcOrder<0)^(dstOrder<0));
  int drop= dstSize-srcSize;

  if (drop==0) {
    n*= dstSize;
    memcpy(dst, src, n);
    if (srcOrder==dstOrder) return;

  } else if (drop<0) {
    /* drop the most significant bytes of the src to fit into dst */
    if (srcWord==1 || dstSize%srcWord) {
      /* conversions between all known integer formats take this branch */
      if (srcOrder>0) src-= drop;
      PartialCopy(dst, (int)dstSize, src, (int)srcSize, n, (int)dstSize);

    } else {
      /* provide this for completeness */
      SlowCopy(dst, (int)dstSize, src, (int)srcSize, n, srcOrder);
      srcWord= 1;  /* SlowCopy undoes middle-endian src */
    }

    n*= dstSize;

  } else {
    /* copy src into least significant bytes of dst, then sign extend */
    char *tmp= dst;
    if (srcOrder>0) tmp+= drop;  /* dst will have src order initially */

    if (srcWord==1 || dstSize%srcWord) {
      /* conversions between all known integer formats take this branch */
      PartialCopy(tmp, (int)dstSize, src, (int)srcSize, n, (int)srcSize);

    } else {
      /* provide this for completeness */
      SlowCopy(tmp, (int)dstSize, src, (int)srcSize, n, srcOrder);
      srcWord= 1;  /* SlowCopy undoes middle-endian src */
    }

    /* set src to byte containing the src sign bit, where it has
       been copied to dst */
    if (srcOrder>0) src= tmp + srcWord-1;
    else src= tmp + srcSize-srcWord;

    /* set tmp to 1st byte of dst sign bytes,
       assuming dst to have src order */
    if (srcOrder>0) tmp= dst;
    else tmp= dst+srcSize;
    SignCopy(tmp, (int)dstSize, src, (int)dstSize, n, (int)drop);

    n*= dstSize;

  }

  /* get word order of dst correct by swapping whole dst */
  if (opposite) Swap(dst, (int)dstSize, n);

  if (srcWord!=dstWord) {
    /* get byte order of dst correct by swapping words within dst */
    if (srcWord>1) Swap(dst, srcWord, n);
    if (dstWord>1) Swap(dst, dstWord, n);
  }
}

static void PartialCopy(char *dst, int dstSize, char *src, int srcSize,
                        long n, int nBytes)
{
  if (nBytes==2) {
    while (n--) {
      dst[0]= src[0];  dst[1]= src[1];
      dst+= dstSize;
      src+= srcSize;
    }

  } else if (nBytes==4) {
    while (n--) {
      dst[0]= src[0];  dst[1]= src[1];
      dst[2]= src[2];  dst[3]= src[3];
      dst+= dstSize;
      src+= srcSize;
    }

  } else {
    int j;
    while (n--) {
      for (j=0 ; j<nBytes ; j++) dst[j]= src[j];
      dst+= dstSize;
      src+= srcSize;
    }
  }
}

static void SignCopy(char *dst, int dstSize, char *src, int srcSize,
                     long n, int nBytes)
{
  if (nBytes==2) {
    while (n--) {
      dst[0]= dst[1]= (src[0]&0x80)? 0xff : 0x00;
      dst+= dstSize;
      src+= srcSize;
    }

  } else if (nBytes==4) {
    while (n--) {
      dst[0]= dst[1]= dst[2]= dst[3]= (src[0]&0x80)? 0xff : 0x00;
      dst+= dstSize;
      src+= srcSize;
    }

  } else if (nBytes==6) {
    while (n--) {
      dst[0]= dst[1]= dst[2]= dst[3]= dst[4]= dst[5]=
        (src[0]&0x80)? 0xff : 0x00;
      dst+= dstSize;
      src+= srcSize;
    }

  } else {
    int j;
    register char c;
    while (n--) {
      c= (src[0]&0x80)? 0xff : 0x00;
      for (j=0 ; j<nBytes ; j++) dst[j]= c;
      dst+= dstSize;
      src+= srcSize;
    }
  }
}

static void Swap(char *dst, int dstSize, long n)
{
  long i;
  register char c;

  if (dstSize==2) {
    for (i=0 ; i<n ; i+=2) {
      c= dst[i];  dst[i]= dst[i+1];  dst[i+1]= c;
    }

  } else if (dstSize==4) {
    for (i=0 ; i<n ; i+=4) {
      c= dst[i];    dst[i]= dst[i+3];    dst[i+3]= c;
      c= dst[i+1];  dst[i+1]= dst[i+2];  dst[i+2]= c;
    }

  } else if (dstSize==8) {
    for (i=0 ; i<n ; i+=8) {
      c= dst[i];    dst[i]= dst[i+7];    dst[i+7]= c;
      c= dst[i+1];  dst[i+1]= dst[i+6];  dst[i+6]= c;
      c= dst[i+2];  dst[i+2]= dst[i+5];  dst[i+5]= c;
      c= dst[i+3];  dst[i+3]= dst[i+4];  dst[i+4]= c;
    }

  } else {
    int j, jMax= dstSize>>1;
    int jOff= dstSize-1;
    for (i=0 ; i<n ; i+=dstSize) {
      for (j=0 ; j<jMax ; j++) {
        c= dst[i+j];
        dst[i+j]= dst[i+jOff-j];
        dst[i+jOff-j]= c;
      }
    }
  }
}

static void SlowCopy(char *dst, int dstSize, char *src, int srcSize,
                     long n, int srcOrder)
{
  int srcWord= srcOrder<0? -srcOrder : srcOrder;
  int nBytes= srcSize>dstSize? dstSize : srcSize;
  int i, j, w, srcOffset= srcOrder<0? 0 : srcSize-nBytes;
  /* reverse src words while copying
     nBytes least significant bytes from src to dst --
     dst winds up with a copy of src as if srcWord==1 */
  while (n--) {
    for (i=0 ; i<nBytes ; i++) {
      j= i + srcOffset;         /* position in src if srcWord==1 */
      w= j%srcWord;             /* byte within word */
      j-= w;                    /* byte at beginning of word */
      dst[i]= src[j + (srcWord-1-w)];
    }
    dst+= dstSize;
    src+= srcSize;
  }
}

/*--------------------------------------------------------------------------*/

/* Floating point format conversion is complicated by the possibility
   of bit shifts, the necessity of adjusting for exponent bias, detecting
   possible overflows and underflows, and detecting zero.
   The basic algorithm is to transfer the mantissa first, then transfer
   the sign if it is separated from the exponent, then assemble
   the exponent into a long, adjusting its bias.  If the exponent is
   zero or underflows, the mantissa and sign are zeroed; overflows do not
   affect the mantissa or sign, but the exponent is brought within range.  */

static FPLayout dstL, srcL;

static void FloatConvert(char *dst, long dstSize, int dstOrder,
                         FPLayout *dstLayout, char *src, long srcSize,
                         int srcOrder, FPLayout *srcLayout, long n)
{
  int dstWord= dstOrder<0? -dstOrder : dstOrder;
  int srcWord= srcOrder<0? -srcOrder : srcOrder;
  int opposite= ((srcOrder<0)^(dstOrder<0));

  if (dstLayout==srcLayout || SameFPLayout(dstLayout, srcLayout)) {
    /* when the floating point layouts match, swapping the word and
       byte order is no more difficult than for integer conversion */
    int drop= dstSize-srcSize;

    if (drop==0) memcpy(dst, src, n*dstSize);
    else PartialCopy(dst, dstSize, src, srcSize, n,
                     drop>0? srcSize : dstSize);

  } else {
    /* zero the destination */
    memset(dst, 0, n*dstSize);

    /* move layouts to standard location for non-pointered use by
       DoMantissa, DoSign, and DoExponent */
    dstL.sgnAddr= dstLayout->sgnAddr;
    dstL.expAddr= dstLayout->expAddr;
    dstL.expSize= dstLayout->expSize;
    dstL.manAddr= dstLayout->manAddr;
    dstL.manSize= dstLayout->manSize;
    dstL.manNorm= dstLayout->manNorm;
    dstL.expBias= dstLayout->expBias;

    srcL.sgnAddr= srcLayout->sgnAddr;
    srcL.expAddr= srcLayout->expAddr;
    srcL.expSize= srcLayout->expSize;
    srcL.manAddr= srcLayout->manAddr;
    srcL.manSize= srcLayout->manSize;
    srcL.manNorm= srcLayout->manNorm;
    srcL.expBias= srcLayout->expBias;

    /* The DoMantissa, DoSign, and DoExponent routines translate the
       srcLayout into the dstLayout, leaving the src order unchanged,
       except that the resulting dst has abs(order)==1.  */
    DoMantissa(dst, dstSize, src, srcSize, srcOrder, srcWord, n);
    DoSign(dst, dstSize, src, srcSize, srcOrder, srcWord, n);
    DoExponent(dst, dstSize, src, srcSize, srcOrder, srcWord, n);
  }

  /* get word order of dst correct by swapping whole dst */
  n*= dstSize;
  if (opposite) Swap(dst, (int)dstSize, n);

  /* get byte order of dst correct by swapping words within dst */
  if (dstWord>1) Swap(dst, dstWord, n);
}

static void DoMantissa(char *dst, long dstSize, char *src, long srcSize,
                       int srcOrder, int srcWord, long n)
{
  int manSize= dstL.manSize>srcL.manSize? srcL.manSize : dstL.manSize;
  int normChange= dstL.manNorm-srcL.manNorm;
  int dstEnd, dstBit= dstL.manAddr;
  int srcEnd, srcBit= srcL.manAddr;
  int dst0, src0, dst1, src1, mask0, mask1, shft, offs, tmp, i, j;
  long nnorm= n;
  char *norm= dst;

  /* If the mantissa normalization is different, don't want to copy
     the always-1 high order mantissa bit.  */
  if (normChange) {
    if (normChange>0){
      dstBit++;               /* dst has 1 norm bit, src doesn't */
      if (manSize==dstL.manSize) manSize--;
    } else {
      srcBit++;               /* src has 1 norm bit, dst doesn't */
      if (manSize==srcL.manSize) manSize--;
    }
  }

  /* find "ideal" initial and final byte locations, assuming srcWord==1 */
  dstEnd= dstBit+manSize-1;
  srcEnd= srcBit+manSize-1;

  if (srcOrder>0) {
    /* MSB first -- 0 is hi-order end, 1 is lo-order end */
    src0= srcBit>>3;
    src1= srcEnd>>3;
    dst0= dstBit>>3;
    dst1= dstEnd>>3;
    mask0= ~((~0)<<(8-(dstBit&7)));
    mask1= (~0)<<(7-(dstEnd&7));
    /* Beware-- bit numbering opposite to byte numbering */
    shft= (srcBit&7)-(dstBit&7);

    offs= src0-dst0;

    if (shft==0) {
      /* rare to have no shift, but far less work */
      if (srcWord==1) {
        /* common branch -- MSB 1st, no shift */
        while (n--) {
          dst[dst0]= src[src0]&mask0;
          for (i=dst0+1 ; i<dst1 ; i++) dst[i]= src[offs+i];
          dst[dst1]= src[src1]&mask1;
          dst+= dstSize;
          src+= srcSize;
        }

      } else if (srcWord==2 && offs==0) {
        /* special case for VAX->IEEE -- no shift */
        while (n--) {
          dst[dst0]= src[(src0&(~1)) + 1-(src0&1)]&mask0;
          for (i=dst0+1 ; i<dst1 ; i++)
            dst[i]= src[(i&(~1)) + 1-(i&1)];
          dst[dst1]= src[(src1&(~1)) + 1-(src1&1)]&mask1;
          dst+= dstSize;
          src+= srcSize;
        }

      } else {
        /* no known examples -- for completeness only */
        GenericMant(dst, dstSize, src, srcSize, srcOrder, srcWord, n,
                    dst0, dst1, src0, src1, shft, mask0, mask1, offs);
      }

    } else if (shft>0) {
      /* dst is left of src... */

      if (srcWord==1) {
        /* common branch -- MSB 1st, left shift */
        while (n--) {
          tmp= ((int)((unsigned char)src[src0])) << shft;
          dst[dst0]= tmp&mask0;
          for (i=dst0+1 ; i<=dst1 ; i++) {
            tmp= ((int)((unsigned char)src[offs+i])) << shft;
            dst[i]= tmp;         /* hi order bits */
            dst[i-1]|= (tmp>>8); /* lo order bits */
          }
          if (offs+dst1<src1) {
            tmp= ((int)((unsigned char)src[src1])) << shft;
            dst[dst1]|= (tmp>>8);
          }
          dst[dst1]&= mask1;
          dst+= dstSize;
          src+= srcSize;
        }

      } else if (srcWord==2) {
        /* special case for VAX src -- left shift */
        while (n--) {
          j= (src0&(~1)) + 1-(src0&1);
          tmp= ((int)((unsigned char)src[j])) << shft;
          dst[dst0]= tmp&mask0;
          for (i=dst0+1 ; i<=dst1 ; i++) {
            j= offs+i;
            j= (j&(~1)) + 1-(j&1);
            tmp= ((int)((unsigned char)src[j])) << shft;
            dst[i]= tmp;         /* hi order bits */
            dst[i-1]|= (tmp>>8); /* lo order bits */
          }
          if (offs+dst1<src1) {
            j= (src1&(~1)) + 1-(src1&1);
            tmp= ((int)((unsigned char)src[j])) << shft;
            dst[dst1]|= (tmp>>8);
          }
          dst[dst1]&= mask1;
          dst+= dstSize;
          src+= srcSize;
        }

      } else {
        /* no known examples -- for completeness only */
        GenericMant(dst, dstSize, src, srcSize, srcOrder, srcWord, n,
                    dst0, dst1, src0, src1, shft, mask0, mask1, offs);
      }

    } else {
      /* dst is right of src... */

      if (srcWord==1) {
        /* common branch -- MSB 1st, right shift */
        shft+= 8;
        while (n--) {
          tmp= ((int)((unsigned char)src[src0])) << shft;
          dst[dst0+1]= tmp;
          dst[dst0]= (tmp>>8)&mask0;
          for (i=dst0+1 ; i<dst1 ; i++) {
            tmp= ((int)((unsigned char)src[offs+i])) << shft;
            dst[i+1]= tmp;      /* hi order bits */
            dst[i]|= (tmp>>8);  /* lo order bits */
          }
          if (offs+dst1==src1) {
            tmp= ((int)((unsigned char)src[src1])) << shft;
            dst[dst1]|= (tmp>>8);
          }
          dst[dst1]&= mask1;
          dst+= dstSize;
          src+= srcSize;
        }

      } else if (srcWord==2) {
        /* special case for VAX src -- right shift */
        shft+= 8;
        while (n--) {
          j= (src0&(~1)) + 1-(src0&1);
          tmp= ((int)((unsigned char)src[j])) << shft;
          dst[dst0+1]= tmp;
          dst[dst0]= (tmp>>8)&mask0;
          for (i=dst0+1 ; i<dst1 ; i++) {
            j= offs+i;
            j= (j&(~1)) + 1-(j&1);
            tmp= ((int)((unsigned char)src[j])) << shft;
            dst[i+1]= tmp;      /* hi order bits */
            dst[i]|= (tmp>>8);  /* lo order bits */
          }
          if (offs+dst1==src1) {
            j= (src1&(~1)) + 1-(src1&1);
            tmp= ((int)((unsigned char)src[j])) << shft;
            dst[dst1]|= (tmp>>8);
          }
          dst[dst1]&= mask1;
          dst+= dstSize;
          src+= srcSize;
        }

      } else {
        /* no known examples -- for completeness only */
        GenericMant(dst, dstSize, src, srcSize, srcOrder, srcWord, n,
                    dst0, dst1, src0, src1, shft, mask0, mask1, offs);
      }
    }

  } else {
    /* LSB first -- 0 is lo-order end, 1 is hi-order end */
    src0= srcSize-1 - (srcEnd>>3);
    src1= srcSize-1 - (srcBit>>3);
    dst0= dstSize-1 - (dstEnd>>3);
    dst1= dstSize-1 - (dstBit>>3);
    mask0= (~0)<<(7-(dstEnd&7));
    mask1= ~((~0)<<(8-(dstBit&7)));
    /* Beware-- bit numbering opposite to byte numbering */
    shft= (srcEnd&7)-(dstEnd&7);

    offs= src0-dst0;

    if (shft==0) {
      /* rare to have no shift, but far less work */
      if (srcWord==1) {
        /* common branch -- LSB 1st, no shift */
        while (n--) {
          dst[dst0]= src[src0]&mask0;
          for (i=dst0+1 ; i<dst1 ; i++) dst[i]= src[offs+i];
          dst[dst1]= src[src1]&mask1;
          dst+= dstSize;
          src+= srcSize;
        }

      } else {
        /* no known examples -- for completeness only */
        GenericMant(dst, dstSize, src, srcSize, srcOrder, srcWord, n,
                    dst0, dst1, src0, src1, shft, mask0, mask1, offs);
      }

    } else if (shft>0) {
      /* dst is left of src... */

      if (srcWord==1) {
        /* common branch -- LSB 1st, left shift */
        while (n--) {
          tmp= ((int)((unsigned char)src[src0])) << shft;
          dst[dst0]= tmp&mask0;
          dst[dst0+1]= (tmp>>8);
          for (i=dst0+1 ; i<dst1 ; i++) {
            tmp= ((int)((unsigned char)src[offs+i])) << shft;
            dst[i]|= tmp;        /* hi order bits */
            dst[i+1]= (tmp>>8);  /* lo order bits */
          }
          if (offs+dst1==src1) {
            tmp= ((int)((unsigned char)src[src1])) << shft;
            dst[dst1]|= tmp;
          }
          dst[dst1]&= mask1;
          dst+= dstSize;
          src+= srcSize;
        }

      } else {
        /* no known examples -- for completeness only */
        GenericMant(dst, dstSize, src, srcSize, srcOrder, srcWord, n,
                    dst0, dst1, src0, src1, shft, mask0, mask1, offs);
      }

    } else {
      /* dst is right of src... */

      if (srcWord==1) {
        /* common branch -- LSB 1st, right shift */
        shft+= 8;
        while (n--) {
          tmp= ((int)((unsigned char)src[src0])) << shft;
          dst[dst0]= tmp&mask0;
          for (i=dst0+1 ; i<=dst1 ; i++) {
            tmp= ((int)((unsigned char)src[offs+i])) << shft;
            dst[i-1]|= tmp;     /* hi order bits */
            dst[i]= (tmp>>8);   /* lo order bits */
          }
          if (offs+dst1<src1) {
            tmp= ((int)((unsigned char)src[src1])) << shft;
            dst[dst1]|= tmp;
          }
          dst[dst1]&= mask1;
          dst+= dstSize;
          src+= srcSize;
        }

      } else {
        /* no known examples -- for completeness only */
        GenericMant(dst, dstSize, src, srcSize, srcOrder, srcWord, n,
                    dst0, dst1, src0, src1, shft, mask0, mask1, offs);
      }
    }
  }

  if (normChange>0) {
    /* set normalization bit in dst */
    dst0= dstL.manAddr>>3;
    mask0= 0x80 >> (dstL.manAddr&7);

    /* get true byte location containing normalization bit
       remember that dst has sign of srcOrder, magnitude 1 */
    if (srcOrder<0) dst0= dstSize-1 - dst0;
    norm+= dst0;

    while (nnorm--) {
      norm[0]|= mask0;
      norm+= dstSize;
    }
  }
}

static void GenericMant(char *dst, long dstSize, char *src, long srcSize,
                        int srcOrder, int srcWord, long n,
                        int dst0, int dst1, int src0, int src1,
                        int shft, int mask0, int mask1, int offs)
{
  int tmp, i, j;
  /* These are slow, but can handle any conversion allowed by the
     floating point parametrization FPLayout.  */

  if (srcOrder>0) {
    if (shft>=0) {
      while (n--) {
        j= src0 - 2*(src0%srcWord) + srcWord-1;
        tmp= ((int)((unsigned char)src[j])) << shft;
        dst[dst0]= tmp&mask0;
        for (i=dst0+1 ; i<=dst1 ; i++) {
          j= offs+i;
          j= j - 2*(j%srcWord) + srcWord-1;
          tmp= ((int)((unsigned char)src[j])) << shft;
          dst[i]= tmp;          /* hi order bits */
          dst[i-1]|= (tmp>>8);  /* lo order bits */
        }
        if (offs+dst1<src1) {
          j= src1 - 2*(src1%srcWord) + srcWord-1;
          tmp= ((int)((unsigned char)src[j])) << shft;
          dst[dst1]|= (tmp>>8);
        }
        dst[dst1]&= mask1;
        dst+= dstSize;
        src+= srcSize;
      }

    } else {
      shft+= 8;
      while (n--) {
        j= src0 - 2*(src0%srcWord) + srcWord-1;
        tmp= ((int)((unsigned char)src[j])) << shft;
        dst[dst0+1]= tmp;
        dst[dst0]= (tmp>>8)&mask0;
        for (i=dst0+1 ; i<dst1 ; i++) {
          j= offs+i;
          j= j - 2*(j%srcWord) + srcWord-1;
          tmp= ((int)((unsigned char)src[j])) << shft;
          dst[i+1]= tmp;        /* hi order bits */
          dst[i]|= (tmp>>8);    /* lo order bits */
        }
        if (offs+dst1==src1) {
          j= src1 - 2*(src1%srcWord) + srcWord-1;
          tmp= ((int)((unsigned char)src[j])) << shft;
          dst[dst1]|= (tmp>>8);
        }
        dst[dst1]&= mask1;
        dst+= dstSize;
        src+= srcSize;
      }
    }

  } else {
    if (shft>=0) {
      while (n--) {
        j= src0 - 2*(src0%srcWord) + srcWord-1;
        tmp= ((int)((unsigned char)src[j])) << shft;
        dst[dst0]= tmp&mask0;
        dst[dst0+1]= (tmp>>8);
        for (i=dst0+1 ; i<dst1 ; i++) {
          j= offs+i;
          j= j - 2*(j%srcWord) + srcWord-1;
          tmp= ((int)((unsigned char)src[j])) << shft;
          dst[i]|= tmp;         /* hi order bits */
          dst[i+1]= (tmp>>8);   /* lo order bits */
        }
        if (offs+dst1==src1) {
          j= src1 - 2*(src1%srcWord) + srcWord-1;
          tmp= ((int)((unsigned char)src[j])) << shft;
          dst[dst1]|= tmp;
        }
        dst[dst1]&= mask1;
        dst+= dstSize;
        src+= srcSize;
      }

    } else {
      shft+= 8;
      while (n--) {
        j= src0 - 2*(src0%srcWord) + srcWord-1;
        tmp= ((int)((unsigned char)src[j])) << shft;
        dst[dst0]= tmp&mask0;
        for (i=dst0+1 ; i<=dst1 ; i++) {
          j= offs+i;
          j= j - 2*(j%srcWord) + srcWord-1;
          tmp= ((int)((unsigned char)src[j])) << shft;
          dst[i-1]|= tmp;       /* hi order bits */
          dst[i]= (tmp>>8);     /* lo order bits */
        }
        if (offs+dst1<src1) {
          j= src1 - 2*(src1%srcWord) + srcWord-1;
          tmp= ((int)((unsigned char)src[j])) << shft;
          dst[dst1]|= tmp;
        }
        dst[dst1]&= mask1;
        dst+= dstSize;
        src+= srcSize;
      }
    }
  }
}

static void DoSign(char *dst, long dstSize, char *src, long srcSize,
                   int srcOrder, int srcWord, long n)
{
  int dstByte= dstL.sgnAddr>>3;
  int dstMask= 0x80 >> (dstL.sgnAddr&7);
  int srcByte= srcL.sgnAddr>>3;
  int srcMask= 0x80 >> (srcL.sgnAddr&7);

  /* get true byte location containing sign bit
     remember that dst has sign of srcOrder, magnitude 1 */
  if (srcOrder>0) {
    if (srcWord!=1) {
      if (srcWord==2) srcByte= (srcByte&(~1)) + 1-(srcByte&1);
      else srcByte= srcByte-2*(srcByte%srcWord)+srcWord-1;
    }
  } else {
    if (srcWord==1) srcByte= srcSize-1 - srcByte;
    else srcByte= srcSize-srcWord+2*(srcByte%srcWord)-srcByte;
    dstByte= dstSize-1 - dstByte;
  }
  src+= srcByte;
  dst+= dstByte;

  while (n--) {
    if (src[0]&srcMask) dst[0]|= dstMask;
    dst+= dstSize;
    src+= srcSize;
  }
}

static void DoExponent(char *dst, long dstSize, char *src, long srcSize,
                       int srcOrder, int srcWord, long n)
{
  int dstBit= dstL.expAddr;
  int dstEnd= dstBit+dstL.expSize-1;
  int dstShift= 7-(dstEnd&7);
  long antiMask= (~0L)<<dstL.expSize;
  int srcBit= srcL.expAddr;
  int srcEnd= srcBit+srcL.expSize-1;
  int srcShift= 7-(srcEnd&7);
  long mask= ~((~0L)<<srcL.expSize);
  int src0= srcBit>>3;
  int src1= srcEnd>>3;
  int dst0= dstBit>>3;
  int dst1= dstEnd>>3;
  int expBytes= src1-src0;  /* this must be <sizeof(long) for the generic
                               algorithm below to function -- it is no
                               more than 1 for any known floating point
                               format... */
  long deltaBias= dstL.expBias+dstL.manNorm - srcL.expBias-srcL.manNorm;
  long maxExponent= (1<<dstL.expSize)-2;  /* this may not always
                                             be reasonable... */
  long exponent;
  int i;

  if (srcOrder<0) {
    src0= srcSize-1 - src0;
    src1= srcSize-1 - src1;
    dst0= dstSize-1 - dst0;
    dst1= dstSize-1 - dst1;
  }

  if (expBytes==1) {
    /* This covers all known hardware formats... */
    if (srcWord>1) {
      src0+= srcWord-1 - 2*(src0%srcWord);
      src1+= srcWord-1 - 2*(src1%srcWord);
    }

    while (n--) {
      /* shift bytes containing portions of exponent into exponent,
         most significant first, then shift into position and mask */
      exponent= (((((long)(unsigned char)src[src0])<<8) |
                  ((long)(unsigned char)src[src1])) >> srcShift) & mask;
      /* adjust the exponent bias */
      exponent= exponent? exponent+deltaBias : 0;
      if (exponent<=0) {
        /* zero or underflow -- zero entire number */
        for (i=0 ; i<dstSize ; i++) dst[i]= 0;
      } else {
        if (exponent&antiMask) {
          /* overflow -- set exponent near maximum */
          exponent= maxExponent;
        }
        exponent<<= dstShift;
        dst[dst1]|= exponent;       /* least significant byte */
        dst[dst0]|= (exponent>>8);  /* most significant byte */
      }
      src+= srcSize;
      dst+= dstSize;
    }

    return;

  } else {
    /* This is included for generality -- at least one "home-brew"
       compressed data format has expBytes==0; the algorithm should
       work for expBytes==2 and 3 on all machines (sizeof(long)>=4).  */
    int srcInc= srcOrder>0? 1 : -1;

    if (srcWord==1 || expBytes<2) {
      while (n--) {
        /* shift bytes containing portions of exponent into exponent,
           most significant first, then shift into position and mask */
        exponent= (long)(unsigned char)src[src0];
        for (i=src0 ; i!=src1 ; ) {
          i+= srcInc;
          exponent= (exponent<<8) | (long)(unsigned char)src[i];
        }
        exponent= (exponent>>srcShift)&mask;
        /* adjust the exponent bias */
        exponent= exponent? exponent+deltaBias : 0;
        if (exponent<=0) {
          /* zero or underflow -- zero entire number */
          for (i=0 ; i<dstSize ; i++) dst[i]= 0;
        } else {
          if (exponent&antiMask) {
            /* overflow -- set exponent near maximum */
            exponent= maxExponent;
          }
          exponent<<= dstShift;
          /* put exponent back (in src order!) least significant byte
             first, followed by next, etc */
          dst[dst1]|= exponent;
          for (i=dst1 ; i!=dst0 ; ) {
            exponent>>= 8;
            i-= srcInc;
            dst[i]|= exponent;
          }
        }
        src+= srcSize;
        dst+= dstSize;
      }

    } else {
      while (n--) {
        /* shift bytes containing portions of exponent into exponent,
           most significant first, then shift into position and mask */
        exponent=
          (long)(unsigned char)src[src0 - 2*(src0%srcWord) + srcWord-1];
        for (i=src0 ; i!=src1 ; ) {
          i+= srcInc;
          exponent= (exponent<<8) |
            (long)(unsigned char)src[i - 2*(i%srcWord) + srcWord-1];
        }
        exponent= (exponent>>srcShift)&mask;
        /* adjust the exponent bias */
        exponent= exponent? exponent+deltaBias : 0;
        if (exponent<=0) {
          /* zero or underflow -- zero entire number */
          for (i=0 ; i<dstSize ; i++) dst[i]= 0;
        } else {
          if (exponent&antiMask) {
            /* overflow -- set exponent near maximum */
            exponent= maxExponent;
          }
          exponent<<= dstShift;
          /* put exponent back (in src order!) least significant byte
             first, followed by next, etc */
          dst[dst1]|= exponent;
          for (i=dst1 ; i!=dst0 ; ) {
            exponent>>= 8;
            i-= srcInc;
            dst[i]|= exponent;
          }
        }
        src+= srcSize;
        dst+= dstSize;
      }
    }
  }
}

/*--------------------------------------------------------------------------*/
