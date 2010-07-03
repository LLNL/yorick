/*
 * $Id: binio.c,v 1.7 2010-07-03 19:42:31 dhmunro Exp $
 * Define Yorick functions for dealing with binary I/O
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifdef NOT_YORICK
#include "binio.h"
#else
#include "ydata.h"
#endif
#include "yio.h"
#include "defmem.h"
#include "pstdlib.h"
#include <string.h>

#undef NEVER_USE

/*--------------------------------------------------------------------------*/

/* Default raw I/O operations use fread, fwrite, ftell, fseek, and fclose */
extern long YReadIO(IOStream *file, void *buf, long size, long n);
extern void YWriteIO(IOStream *file, const void *buf, long size, long n);
extern long YTellIO(IOStream *file, long offset);
extern void YSeekIO(IOStream *file, long offset);
extern void YSeekEndIO(IOStream *file, long offset);
extern void YCloseIO(IOStream *file);

extern void FreeMemberList(Member *member, long n);

static void ChildOpen(HistoryInfo *history, int ifile);

static void FreeHistoryInfo(HistoryInfo *history);
static int HasReferences(StructDef *base);

extern void YErrorIO(const char *msg);

/*--------------------------------------------------------------------------*/

int JumpToTime(HistoryInfo *history, double time)
{
  long i, n;
  double *times, dtime=0.0, oldtime;

  if (!history) return 1;
  n= history->nRecords;
  times= history->time;
  if (n<=0 || !times) return 1;

  oldtime= 0.0;
  for (i=0 ; i<n ; i++) {
    dtime= times[i]-time;
    if (dtime>=0.0) break;
    oldtime= dtime;
  }
  if (dtime>0.0 && i>0) { if (oldtime+dtime > 0.0) i--; }
  else if (i>=n) i--;

  return JumpRecord(history, i);
}

int JumpToCycle(HistoryInfo *history, long ncyc)
{
  long i, n;
  long *ncycs, dncyc=0, oldncyc;

  if (!history) return 1;
  n= history->nRecords;
  ncycs= history->ncyc;
  if (n<=0 || !ncycs) return 1;

  oldncyc= 0;
  for (i=0 ; i<n ; i++) {
    dncyc= ncycs[i]-ncyc;
    if (dncyc>=0) break;
    oldncyc= dncyc;
  }
  if (dncyc>0 && i>0) { if (oldncyc+dncyc > 0) i--; }
  else if (i>=n) i--;

  return JumpRecord(history, i);
}

/* JumpRecord returns 0 on success, 1 on recNumber out of range.
   In the latter case, there will be no current record.  */
int JumpRecord(HistoryInfo *history, long recNumber)
{
  IOStream *child;
  if (!history) return 1;
  child= history->child;
  history->recNumber= -1;   /* safe against failure until actually set */
  if (recNumber<0 || recNumber>=history->nRecords) return 1;
  ChildOpen(history, history->ifile[recNumber]);
  child->offset= history->offset[recNumber];
  history->recNumber= recNumber;
  return 0;
}

static void ChildOpen(HistoryInfo *history, int ifile)
{
  IOStream *child, *parent;
  int fileNumber= history->fileNumber;
  int permissions;

  if (ifile==fileNumber) return;              /* this child already open */
  child= history->child;
  ClearPointees(child, 0);
  FreeClogFile(child);
  if (fileNumber>0) child->ioOps->Close(child);
  else FlushFile(child, 1); /* automatic with Close, necessary otherwise */

  history->recNumber= -1;      /* current record about to become invalid */

  parent= history->parent;
  if (ifile==0) {                        /* requested file is the parent */
    child->stream= parent->stream;
    child->fullname= parent->fullname;
    history->fileNumber= 0;
    return;
  }

  /* must open a new child */
  permissions= child->permissions;
  if (permissions&1) {
    char *name= history->famNames[ifile];
    if (permissions&2) child->stream= p_fopen(name, "r+b");
    else child->stream= p_fopen(name, "rb");
    if (child->stream) {
      child->fullname= name;
      history->fileNumber= ifile;
    } else {
      child->stream= parent->stream;
      child->fullname= parent->fullname;
      history->fileNumber= 0;
      YErrorIO("previously opened binary history file now un-openable");
    }
  } else {
    YErrorIO("attempt to seek history record in write-only binary file");
  }

#ifdef NEVER_USE
  /* BUFFERING
     This might seem like a good idea, but it turns out that both on UNIX
     and MacIntosh (and probably DOS), "unbuffered" means to read the
     stream one byte at a time, NOT to use the space passed to fread/fwrite
     as the buffer, as I originally assumed.  See yio.h.  */
  if (file) setbuf(file, (char *)0);
#endif
}

/*--------------------------------------------------------------------------*/

static int HasReferences(StructDef *base)
{
  if (!base->file && base->index<yStructTable.nItems &&
      yStructList[base->index]==base) return base->references>1;
  else return base->references>0;
}

StructDef *AddStruct(IOStream *file, const char *name, long n)
{
  StructDef *base;

  if (file) {
    if (file->history) {
      HistoryInfo *history= file->history;
      if (history->nRecords>1)
        YError("illegal to add struct to file after 1st record");
      if (file!=history->child) file= history->child;
    }

    if (HashAdd(&file->structTable, name, n)) {
      StructDef **structList= file->structList;
      base= structList[hashIndex];
      if (base->references || (base->model!=&stringStruct &&
                               base->model!=&pointerStruct)) return 0;
      /* redefinition of unreferenced default string or pointer
         is the exception -- it is OK */
      Unref(base->model);
      base->model= 0;
      base->dataOps= 0;
      base->size= 0;
      base->alignment= 0;
      base->Convert= 0;
      base->order= 0;
    } else {
      HASH_MANAGE(file->structTable, StructDef *, file->structList);
      file->structList[hashIndex]= base= NewStructDef(file, hashIndex);
      base->addressType= 1;
    }

  } else {
    if (HashAdd(&yStructTable, name, 0L)) {
      StructDef *old= yStructList[hashIndex];
      if (hashIndex<=8)
        YError("attempt to replace in-memory StructDef of basic data type");
      yStructList[hashIndex]= 0;
      Unref(old);
    } else {
      HASH_MANAGE(yStructTable, StructDef *, yStructList);
    }
    base= NewStructDef(file/* 0 */, hashIndex);
    yStructList[hashIndex]= Ref(base);
  }

  return base;
}

long AlignAdjust(long address, int alignment)
{
  long mis= alignment>1? address%alignment : 0;
  return mis? address+(alignment-mis) : address;
}

#define N_PRIMITIVES 8
#define NDX_LONG 3
#define NDX_STRING 6
#define NDX_POINTER 7
static StructDef *primStructs[N_PRIMITIVES]= {
  &charStruct, &shortStruct, &intStruct, &longStruct,
  &floatStruct, &doubleStruct, &stringStruct, &pointerStruct };
/* never needed?
static char *primNames[N_PRIMITIVES]= {
  "char", "short", "int", "long", "float", "double", "string", "pointer" };
 */

int DefinePrimitive(StructDef *base, long size, int alignment,
                    int addressType, int order, FPLayout *fpLayout,
                    StructDef *model, Converter *Convert)
{
  int i;

  if (HasReferences(base))
    YError("illegal to change a previously referenced struct");
  if (base->table.nItems)
    YError("illegal to make a compound struct into a primitive");

  if (base->file) {
    for (i=0 ; i<N_PRIMITIVES ; i++) if (model==primStructs[i]) break;
    if (base->index<NDX_STRING && base->index!=i) return 1;
    /* redefinition of char, short, int, long, float, or double */
    else if ((i==NDX_STRING || i==NDX_POINTER) && base->index!=i &&
             !Convert) return 2;
    /* illegal to alias another type to string or pointer */
  } else {
    i= N_PRIMITIVES;
  }

  base->size= size;
  base->alignment= alignment;
  base->addressType= addressType;
  base->order= order;
  if (size && fpLayout) {
    FreeFPLayout(base->fpLayout);
    base->fpLayout= MakeFPLayout(fpLayout, size);
  }

  if (model) {
    if (Convert)                         base->Convert= Convert;
    else if (i==NDX_POINTER)             base->Convert= &ConvertP;
    else if (i==NDX_STRING)              base->Convert= &ConvertQ;
    else if (i<NDX_STRING && i>NDX_LONG)
      base->Convert= (base->order==model->order && base->size==model->size &&
                      SameFPLayout(base->fpLayout, model->fpLayout))?
        0 : &ConvertF;
    else if (i<=NDX_LONG && i>0)
      base->Convert=
        (base->order==model->order && base->size==model->size)? 0 : &ConvertI;
    else if (i==0)                       base->Convert= 0;
    else if (model->size!=size) return 3;
    /* model size differs--require Converter */

    if (base->model!=model) {
      Unref(base->model);
      base->model= Ref(model);
    }

    /* string and pointer are represented by long (address) on disk */
    if ((i==NDX_STRING || i==NDX_POINTER) && !Convert) {
      /* default string and pointer must always agree with long */
      model= base->file->structList[NDX_LONG];
      base->size= model->size;
      base->alignment= model->alignment;
      base->addressType= model->addressType;
      base->order= model->order;

    } else if (i==NDX_LONG && base->index==NDX_LONG) {
      /* any change to long must also change string and pointer */
      IOStream *file= base->file;
      long nStructs= file->structTable.nItems;
      if (nStructs>NDX_STRING) {
        model= file->structList[NDX_STRING];
        if (model->Convert==&ConvertQ) {
          model->size= base->size;
          model->alignment= base->alignment;
          model->addressType= base->addressType;
          model->order= base->order;
        }
      }
      if (nStructs>NDX_POINTER) {
        model= file->structList[NDX_POINTER];
        if (model->Convert==&ConvertP) {
          model->size= base->size;
          model->alignment= base->alignment;
          model->addressType= base->addressType;
          model->order= base->order;
        }
      }
    }

  } else if (Convert) {
    return 4;
    /* Converter supplied without a model */

  } else if (order || size==1) {
    if (fpLayout) {
      if (i<N_PRIMITIVES) model= primStructs[i];
      else if (size>4 || size>sizeof(float)) model= &doubleStruct;
      else model= &floatStruct;
      base->model= Ref(model);
      base->Convert= &ConvertF;
    } else {
      if (i<N_PRIMITIVES) model= primStructs[i];
      else if (size>2) model= &longStruct;
      else if (size>1) model= &shortStruct;
      else model= &charStruct;
      base->model= Ref(model);
      base->Convert= &ConvertI;
    }

  } else {
    return 5;
    /* no model supplied for opaque primitive */
  }

  return 0;
}

int AddMember(StructDef *base, long offset, const char *name,
              StructDef *memType, Dimension *dims)
{
  long number, index;
  IOStream *file= base->file;

  if (base->dataOps)
    YError("illegal to add member to a previously installed struct");
  if (HasReferences(base))
    YError("illegal to add member to a previously referenced struct");
  if (!memType->dataOps)
    YError("struct representing member type has not been installed");

  if (HashAdd(&base->table, name, 0L)) {
    return 1;  /* signal duplicate member name */
  } else {
    HASH_MANAGE(base->table, Member, base->members);
    HASH_MANAGE(base->table, long, base->offsets);
  }
  index= hashIndex;  /* protect against ravages of CopyStruct */

  /* fill in new member  */
  memType= CopyStruct(file, memType);  /* allows memType to be a
                                          memory struct as well as
                                          a struct for this file */
  if (!memType) return 2;
  base->members[index].base= Ref(memType);
  base->members[index].dims= Ref(dims);
  base->members[index].number= number= TotalNumber(dims);

  /* NOTE-- base->size is NOT necessarily correct while the struct
            is being built.  InstallStruct adjusts base->size to be
            a multiple of base->alignment if necessary.  */
  if (offset<0) {
    /* must compute offset/alignment for the new data, which goes at
       next available offset in base */
    int alignment= memType->alignment;
    base->size= AlignAdjust(base->size, alignment);
    base->offsets[index]= base->size;
    base->size+= memType->size*number;
    if (alignment>base->alignment) base->alignment= alignment;

  } else {
    /* offset has been specified, just accept it
       and check whether this is beyond nextOffset */
    long size= offset+memType->size*number;
    base->offsets[index]= offset;
    if (size > base->size) base->size= size;
  }

  if (index==0) {
    /* for first member, check for alignment restriction on all structs */
    int structAlign= file? file->structAlign : yStructAlign;
    /* PowerPC version of gcc bumps alignment of struct to sizeof(double)
     * if first member is a scalar double (but not an array of double) */
    if (structAlign==-2 && memType==&doubleStruct && !dims)
      structAlign = memType->size;
    if (base->alignment < structAlign) base->alignment= structAlign;

    /* single member struct can use Copier for first member
       -- unless the struct alignment is so strict that it will
          eventually force base->size > memType->size */
    base->Copy= (base->alignment>memType->size)? &CopyS : memType->Copy;

  } else if (base->Copy!=&CopyS) {
    /* be sure correct Copy virtual function is in place
       -- CopyX is adequate until at least one member (possibly the first)
          needs a more elaborate Copier */
    if (base->Copy!=&CopyX || memType->Copy!=&CopyX) base->Copy= &CopyS;
  }

  /* be sure correct Convert virtual function is in place --
     this actually misses the possibility that a primitive with no
     Converter may have a different alignment on disk */
  if (memType->Convert) base->Convert= &ConvertS;

  /* be sure struct inherits "sequential" property from its members */
  if (memType->addressType>1) base->addressType= 2;

  return 0;
}

void InstallStruct(StructDef *base, StructDef *model)
{
  IOStream *file= base->file;
  int align_for_size = base->alignment;

  if (base->dataOps)
    YError("illegal to install a previously installed struct");
  if (HasReferences(base))
    YError("illegal to install a previously referenced struct");

  /* force size to a multiple of alignment (otherwise arrays of instances
     would not be properly aligned) */
  if (base->members && (file? file->structAlign : yStructAlign)==-1) {
    /* IBM PowerPC alignment idiocy (partly rubbed off on gcc above)
     * must round up base->size to sizeof(double) if first primitive
     * data type is double */
    StructDef *first = base->members[0].base;
    while (first->members) first = first->members[0].base;
    if (first==&doubleStruct && align_for_size<(int)first->size)
      align_for_size = (int)first->size;
  }
  base->size = AlignAdjust(base->size, align_for_size);

  /* shrink-wrap the member table */
  HashShrink(&base->table);

  if (file) {
    /* Now is the time to find the appropriate memory model corresponding
       to this StructDef.  Check yStructTable for this name to see if
       any in-memory structs have the right name.  */
    if (!model && !base->model &&
        HashFind(&yStructTable, StructName(base), 0L)) {
      model= yStructList[hashIndex];
      if (!EquivStruct(base, model)) model= 0;
    }

    if (base->model) {
      /* Caller has already filled in base->model -- presume everything
         else (Convert, order, fpLayout) is also correct.  */
      model= base->model;

    } else if (model) {
      /* An equivalent in-memory model for this StructDef already exists
         and has the same name -- use it.  */
      if (!base->table.nItems) {
        DefinePrimitive(base, base->size, base->alignment,
                        base->addressType, base->order, base->fpLayout,
                        model, (Converter *)0);

      } else {
        base->model= Ref(model);
        /* AddMember will not catch on to the fact that a Converter is
           necessary if the primitive alignments differ.  */
        if (!base->Convert && !StructEqual(base, model))
          base->Convert= &ConvertS;
      }

    } else if (base->table.nItems || (base->order==0 && base->size!=1)) {
      /* Must create a suitable in-memory model for this StructDef now.  */
      long i, nItems= base->table.nItems;
      char **names= base->table.names;
      Member *members= base->members;
      StructDef *submodel;

      /* Don't want to call AddStruct here, as this would replace an
         existing in-memory data structure of the same name.  However,
         if no such in-memory struct exists, go ahead and make one now.  */
      if (HashAdd(&yStructTable, StructName(base), 0L)) {
        base->model= model= NewStructDef((IOStream *)0, hashIndex);
      } else {
        HASH_MANAGE(yStructTable, StructDef *, yStructList);
        base->model= model= NewStructDef((IOStream *)0, hashIndex);
        yStructList[hashIndex]= Ref(model);
      }

      for (i=0 ; i<nItems ; i++) {
        submodel= members[i].base->model;
        while (submodel->model) submodel= submodel->model;
        if (AddMember(model, -1L, names[i], submodel, members[i].dims))
          YError("bad StructDef passed to InstallStruct");
      }
      if (!nItems) {
        model->size= base->size;
        model->alignment= base->alignment;
      }
      InstallStruct(model, 0);

      /* AddMember will not catch on to the fact that a Converter is
         necessary if the primitive alignments differ.  */
      if (!base->Convert && !StructEqual(base, model))
        base->Convert= &ConvertS;

    } else {
      DefinePrimitive(base, base->size, base->alignment,
                      base->addressType, base->order, base->fpLayout,
                      (StructDef *)0, (Converter *)0);
      model= base->model;
    }

    base->dataOps= model->dataOps;

  } else {
    /* even if this is a primitive, it is effectively an opaque structure */
    base->dataOps= yOpsStruct;

    /* If the global variable corresponding to this struct is nil,
       install it now.  */
    if (DefInstallHook)
      DefInstallHook(yStructTable.names[base->index], base);
  }
}

int EquivStruct(StructDef *fil, StructDef *mem)
{
  StructDef *model= fil->model;
  if (model) {
    while(model->model) model= model->model;
    return (model==mem);

  } else {
    long nItems= fil->table.nItems;
    if (mem->table.nItems!=nItems) return 0;
    if (nItems<=0) {
      if ((fil->fpLayout!=0)^(mem->fpLayout!=0)) return 0;
      if (fil->size==1 && mem->size==1) return 1;
      if ((fil->order!=0)^(mem->order!=0)) return 0;
      return 1;

    } else {
      long i;
      Member *fm= fil->members;
      Member *mm= mem->members;
      Dimension *fd, *md;
      for (i=0 ; i<nItems ; i++) {
        if (!EquivStruct(fm[i].base, mm[i].base)) return 0;
        fd= fm[i].dims;
        md= mm[i].dims;
        while (fd && md) {
          if (fd->number != md->number) return 0;
          fd= fd->next;
          md= md->next;
        }
        if (fd || md) return 0;
      }
      return 1;
    }
  }
}

int AddVariable(IOStream *file, long address, const char *name,
                StructDef *varType, Dimension *dims)
{
  long number, index;
  HistoryInfo *history= file->history;

  if (history) {
    if (file!=history->child) file= history->child;
    if (HashFind(&file->dataTable, name, 0L))
      return 1;  /* signal duplicate data name */
    if (history->nRecords>1)
      YError("illegal to add record variable to file after 1st record");
    else if (history->nRecords>0 && history->recNumber>=0 &&
             file->nextAddress-file->offset > history->recordSize)
      YError("non-contiguous record: be careful with pointers/strings?");
  }

  if (!varType->dataOps)
    YError("StructDef representing variable type has not been installed");

  if (HashAdd(&file->dataTable, name, 0L)) {
    return 1;  /* signal duplicate data name */
  } else {
    HASH_MANAGE(file->dataTable, Member, file->types);
    HASH_MANAGE(file->dataTable, long, file->addresses);
  }
  index= hashIndex;  /* protect against ravages of CopyStruct */

  /* fill in new member  */
  varType= CopyStruct(file, varType);  /* allows varType to be a
                                          memory struct as well as
                                          a struct for this file */
  if (!varType) return 2;
  file->types[index].base= Ref(varType);
  file->types[index].dims= Ref(dims);
  file->types[index].number= number= TotalNumber(dims);

  if (address<0) {
    /* must compute address/alignment for the new data, which goes at
       next available address in file */
    long next= AlignAdjust(history? history->recordSize : file->nextAddress,
                           file->dataAlign? file->dataAlign :
                                            varType->alignment);
    file->addresses[index]= next;
    next+= varType->size*number;
    if (history) history->recordSize= next;
    else file->nextAddress= next;

  } else {
    /* address has been specified, just accept it
       and check whether this is beyond nextAddress */
    long next= address+varType->size*number;
    file->addresses[index]= address;
    if (history) {
      if (next > history->recordSize) history->recordSize= next;
    } else {
      if (next > file->nextAddress) file->nextAddress= next;
    }
  }

  hashIndex= index;
  return 0;
}

HistoryInfo *AddHistory(IOStream *file, long size)
{
  IOStream *child;
  HistoryInfo *history;
  long nStructs, i;
  StructDef **structList, *base;

  if (y_vopen_file(file->stream))
    YError("vopen binary file handles do not support history records");

  history= p_malloc(sizeof(HistoryInfo));
  history->parent= file;
  history->child= 0;  /* temporary */
  history->nFamily= 1;
  history->famNames= p_malloc(sizeof(char *)*4);
  history->famNames[0]= file->fullname;  /* never free this one */
  history->fileNumber= 0;
  history->fileSize= size>1023? size : DEFAULT_FILE_SIZE;
  history->copyParent= 1;   /* default is conservative? */

  history->recordSize= 0;
  history->recordAlign= file->dataAlign;
  history->nRecords= 0;
  history->recNumber= -1;

  history->ifile= 0;
  history->offset= 0;
  history->time= 0;
  history->ncyc= 0;

  file->history= history;
  history->child= child=
    NewIOStream(file->fullname, (void *)0, file->permissions);

  child->stream= file->stream;
  child->ioOps= file->ioOps;
  child->blockSize= file->blockSize;
  child->structAlign= file->structAlign;
  child->dataAlign= file->dataAlign;

  child->CloseHook= file->CloseHook;
  child->contentsLog= file->contentsLog;

  structList= file->structList;
  for (i=0 ; i<N_PRIMITIVES ; i++) {
    base= child->structList[i];
    base->size= structList[i]->size;
    base->alignment= structList[i]->alignment;
    base->addressType= structList[i]->addressType;
    Unref(base->model);
    base->model= Ref(structList[i]->model);
    base->Convert= structList[i]->Convert;
    base->order= structList[i]->order;
    base->fpLayout= Ref(structList[i]->fpLayout);
  }
  nStructs= file->structTable.nItems;
  for ( ; i<nStructs ; i++) CopyStruct(child, structList[i]);

  RemoveIOLink(yBinaryFiles, child);
  return child->history= history;
}

int AddRecord(HistoryInfo *history,
              int flags, double time, long ncyc, long address)
{
  long nRecords= history->nRecords;
  IOStream *child= history->child;
  int alignment= history->recordAlign;

  if (nRecords>0) {
    /* all records must agree on whether time and ncyc present */
    if (((history->time!=0)^((flags&1)!=0)) ||
        ((history->ncyc!=0)^((flags&2)!=0))) return 1;

  } else {
    if (alignment<=0) {
      /* struct-like alignment is difficult for records, since most
         restrictive alignment information has not been kept --
         Indeed, the child->dataTable may not yet have been generated
         at the time the first record is declared (now).
         The best we can do is to assume the most restrictive alignment
         of any StructDef which has already been defined.
         A "custom" struct definition with more restrictive alignment
         may not follow this call, and a specification of record alignment
         which is less restrictive that the following guess must be done
         as a special case by setting child->dataAlign before calling
         this routine.  */
      long i, n= child->structTable.nItems;
      StructDef **structList= child->structList;
      for (i=0 ; i<n ; i++)
        if (structList[i]->alignment>alignment)
          alignment= structList[i]->alignment;
      history->recordAlign= alignment;
    }

    /* Adding this first record "locks" the parent -- that is,
       parent->nextAddress may never change afterwards.  It also sets
       the initial child->offset.  */
    child->nextAddress= child->offset= history->parent->nextAddress;
  }

  if ((nRecords&0xf)==0) {
    long newRecs= nRecords+16;
    history->ifile= p_realloc(history->ifile, sizeof(int)*newRecs);
    history->offset= p_realloc(history->offset, sizeof(long)*newRecs);
    if (flags&1)
      history->time= p_realloc(history->time, sizeof(double)*newRecs);
    if (flags&2)
      history->ncyc= p_realloc(history->ncyc, sizeof(long)*newRecs);
  }

  if (address<0) {
    /* Create new file for this record if at least one record has been
       written to this file, and the new record would push the file
       over the fileSize limit.  */
    long after;
    if (history->fileNumber != history->nFamily-1)
      ChildOpen(history, history->nFamily-1);
    address= AlignAdjust(child->nextAddress, alignment);
    after= address + history->recordSize;
    if (nRecords>0 && after>history->fileSize &&
        history->ifile[nRecords-1]==history->fileNumber) {
      if (AddNextFile(history, (char *)0, 1))
        YErrorIO("failed to create next member of file family");
      address= child->nextAddress;
      after= address + history->recordSize;
    } else if (nRecords>0 && (child->permissions&2)) {
      /* dont go incredibly long without flushing history files */
      if (history->recordSize>=8192 ||
          !(nRecords % (1 + 8192/(history->recordSize+1))))
        FlushFile(child, 0);
    }
    child->nextAddress= after;

  } else if (address+history->recordSize > child->nextAddress) {
    /* This nextAddress is actually a minimum if the record contains
       pointers, strings, or other indirections.  */
    child->nextAddress= address+history->recordSize;
  }

  /* After the first record has been added, YcWrite denies write access
     to the non-record (parent) part of the file.  Be sure to clear out
     any pending writes now to avoid two sets of write-armed cache blocks
     for the same file.  */
  if (!nRecords) FlushFile(history->parent, 1);

  history->ifile[nRecords]= history->fileNumber;
  history->offset[nRecords]= address;
  if (flags&1) history->time[nRecords]= time;
  if (flags&2) history->ncyc[nRecords]= ncyc;
  history->recNumber= nRecords;
  history->nRecords= nRecords+1;
  history->child->offset= address;

  return 0;
}

/*--------------------------------------------------------------------------*/

IOOperations defaultIOops= {
  &YReadIO, &YWriteIO, &YTellIO, &YSeekIO, &YSeekEndIO, &YCloseIO
};

/* Set up a block allocator which grabs space for 16 IOStream objects
   at a time.  Since IOStream contains several pointers, the alignment
   of an IOStream must be at least as strict as a void*.  */
static MemryBlock iosBlock= {0, 0, sizeof(IOStream),
                                 16*sizeof(IOStream)};

IOFileLink *yBinaryFiles= 0;
long y_block_size_0 = 0x3fff;  /* must be 2^n-1, see Y_default_blocksize */

IOStream *NewIOStream(char *fullname, void *stream, int permissions)
{
  IOStream *ios= NextUnit(&iosBlock);
  int i;

  ios->references= 0;
  ios->ops= yOpsStream;
  ios->stream= stream;
  ios->fullname= fullname;
  ios->permissions= permissions;
  ios->ioOps= &defaultIOops;

  ios->blockSize= y_block_size_0;
  ios->blockList= 0;
  ios->seqAddress= 0;

  ios->structAlign= yStructAlign;
  ios->dataAlign= 0;

  HashInit(&ios->structTable, N_PRIMITIVES);
  ios->structList= p_malloc(sizeof(StructDef *)*ios->structTable.maxItems);

  ios->dataTable.maxItems= ios->dataTable.nSlots= ios->dataTable.nItems= 0;
  ios->dataTable.items= 0;
  ios->dataTable.names= 0;
  ios->types= 0;
  ios->addresses= 0;
  ios->nextAddress= 0;
  ios->offset= 0;

  ios->history= 0;
  ios->contentsLog= 0;

  ios->pointeeList.writing= 0;
  ios->pointeeList.nValid= ios->pointeeList.table.nItems=
    ios->pointeeList.table.maxItems= ios->pointeeList.table.nSlots= 0;
  ios->pointeeList.table.items= 0;
  ios->pointeeList.mdInfo= 0;

  ios->CloseHook= 0;

  /* All binary files have definitions for the 8 primitive types.  */
  for (i=0 ; i<N_PRIMITIVES ; i++) CopyStruct(ios, primStructs[i]);

  AddIOLink(&yBinaryFiles, ios);

  /* Do not initialize contents log here -- otherwise file is opened twice
     when history child is created.  */
  /* if (permissions&2) CLupdate(ios); */
  return ios;
}

/* deadFile is a bogus IOStream with 0 stream pointer to cause error
   if read or write is attempted.  When an IOStream is freed, any of
   its StructDefs which has outstanding references is set to point to
   deadFile.  */
static IOStream deadFile;

void FreeIOStream(void *ios)
{
  IOStream *io= ios;
  p_file *stream= io->stream;
  Member *types= io->types;
  StructDef *base, **structList= io->structList;
  HistoryInfo *history= io->history;
  long i, nItems;
  IOStream *parent= 0;

  /* clear out any pointees to assure that all data has been written */
  ClearPointees(io, 0);

  /* Recurse to free any history child.
     Note that CloseHook will be called on child if child is present
          -- AddNextFile should have called CloseHook on parent.  */
  if (history) {
    IOStream *child= history->child;
    if (child!=io) {
      /* this is the parent -- recurse to free child */
      Unref(child);
      FreeHistoryInfo(history);

    } else {
      /* this is the child -- don't close file if same as parent */
      parent= history->parent;
      if (io->fullname==parent->fullname) io->fullname= 0;
      if (stream==parent->stream) stream= 0;
      if (io->CloseHook) {
        io->CloseHook(io);
        io->CloseHook = parent->CloseHook = 0;
      }
      /* if io->ioOps->Close will be called for parent only, flush child */
      if (!stream && io->stream) FlushFile(io, 1);
    }
    io->history= 0;

  } else {
    if (io->CloseHook) {
      io->CloseHook(io);
      io->CloseHook = 0;
    }
  }

  /* closing the file discards any cache blocks */
  if (stream) io->ioOps->Close(io);
  io->stream= 0;

  /* contents log is a serious problem -- call to ioOps->Close above
     potentially creates a contents log during flush if there wasn't
     one, and parent->contentsLog is either 0 or child->contentsLog
     depending on whether the file has been familied
     want to call FreeClogFile for parent only, unless there are
     separate contents logs for parent and child (impossible currently?) */
  if (parent && !parent->contentsLog)
    parent->contentsLog= io->contentsLog;
  if (!parent || parent->contentsLog!=io->contentsLog)
    FreeClogFile(io);

  p_free(io->fullname);
  io->fullname= 0;

  /* free dataTable first to clear references to structures */
  FreeMemberList(types, io->dataTable.nItems);
  HashClear(&io->dataTable);
  p_free(types);
  io->types= 0;
  p_free(io->addresses);
  io->addresses= 0;

  /* free structure table in reverse order to clear dependencies */
  nItems= io->structTable.nItems;
  for (i=nItems-1 ; i>=0 ; i--) {
    base= structList[i];
    structList[i]= 0;
    if (base->references) {
      if (!deadFile.ops) {
        deadFile.ops= yOpsStream;
        deadFile.ioOps= &defaultIOops;
        deadFile.blockSize= 0x3fff;  /* why? */
        deadFile.structAlign= 1;     /* why? */
      }
      base->file= RefNC(&deadFile);
    }
    Unref(base);
  }
  HashClear(&io->structTable);
  p_free(structList);

  RemoveIOLink(yBinaryFiles, io);
  FreeUnit(&iosBlock, io);
}

/*--------------------------------------------------------------------------*/

static void FreeHistoryInfo(HistoryInfo *history)
{
  long i, n= history->nFamily;
  char **famNames= history->famNames;

  history->famNames= 0;
  for (i=1 ; i<n ; i++)
    if (i!=history->fileNumber) p_free(famNames[i]);
  p_free(famNames);

  p_free(history->ifile);
  history->ifile= 0;
  p_free(history->offset);
  history->offset= 0;
  p_free(history->time);
  history->time= 0;
  p_free(history->ncyc);
  history->ncyc= 0;

  p_free(history);
}

/*--------------------------------------------------------------------------*/

/* Note-- filename is copied.  */
int AddNextFile(HistoryInfo *history, char *filename, int create)
{
  IOStream *file= history->child;
  int n= history->nFamily;
  void *stream;

  if (!filename) {
    filename= NextFileName(history->famNames[history->nFamily-1]);
  } else {
    filename= YExpandName(filename);
  }

  if (!create) {
    if (!file->permissions&1) stream= 0;
    else if (file->permissions&2) stream= p_fopen(filename, "r+b");
    else stream= p_fopen(filename, "rb");

  } else {
    if (!(file->permissions&2)) stream= 0;
    else if (file->permissions&1) stream= p_fopen(filename, "w+b");
    else stream= p_fopen(filename, "wb");
  }

  if (stream) {
    p_fclose(stream);  /* will reopen it momentarily */
  } else {
    p_free(filename);
    return 1;
  }

  /* run any CloseHook on old file, then re-open new file */
  ClearPointees(file, 0);
  if (file->CloseHook) file->CloseHook(file);

  /* add new filename to history list */
  if (!(n&3))
    history->famNames= p_realloc(history->famNames, sizeof(char *)*(n+4));
  history->famNames[n]= filename;
  history->nFamily++;
  ChildOpen(history, n);

  /* optionally copy static part of file to the new file */
  if (history->copyParent) {
    IOStream *parent= history->parent;
    file->nextAddress= parent->nextAddress;
    if (create && YCopyFile(file, parent))
      YErrorIO("failed to copy non-record data to new file family member");
  } else {
    file->nextAddress= 0;
  }

  /* create Contents Log for new file if necessary */
  if (file->permissions&64) CLupdate(file);

  return 0;
}

/* Return value must be freed with p_free.
   Algorithm:
   (1) Strip directory (if any) from name.
   (2) If final character is a digit (0-9), it becomes increment character,
       else the character before the final dot (.) is the increment
            character (or the final character if there is no dot).
   (3) If the increment character is not A-Z, a-z, or 0-9, scan backwards
       through the name until a character in this range is found.
   (4) Loop:
         If the increment character is before the beginning of name,
            or if it is not in the range A-Z, a-z, or 0-9, insert the
            character "A" and break out of this loop.
         If it is 9 or z or Z, it becomes 0,
         else increment it and break out of this loop.
         Back up so the increment character becomes the previous character.
 */
static char *headName= 0, *tailName= 0;
char *NextFileName(const char *name)
{
  char *nextName;
  unsigned char c;
  long len, i, j, lenN;
  p_free(headName);
  p_free(tailName);
  headName= YNameHead(name);   /* directory name (if any) */
  tailName= YNameTail(name);   /* file name (if any) */

  len= tailName? strlen(tailName) : 0;
  c= len? tailName[len-1] : '0';

  /* find index i of increment character */
  if (c>='0' && c<='9') {
    i= len-1;
  } else {
    for (i=len-1 ; i>0 ; i--) if (tailName[i]=='.') break;
    /* i>0 if and only if there is a character before last dot */
    if (i>0) {
      c= tailName[--i];
      while (i>=0 && ((c<'0'||c>'9') && (c<'a'||c>'z') && (c<'A'||c>'Z')))
        c= tailName[--i];
    } else {
      i= len-1;
    }
  }

  nextName= p_strncat(name, " ", 0);  /* leave a blank just in case... */
  lenN= strlen(name);
  nextName[lenN]= '\0';
  j= i + lenN-len;
  for (;;) {
    if (i<0 || (((c=tailName[i])<'0'||c>'9') &&
                (c<'a'||c>'z') && (c<'A'||c>'Z'))) {
      /* out of decent characters to increment, insert one */
      for (i=lenN ; i>j ; i--) nextName[i+1]= nextName[i];
      nextName[j+1]= 'A';
      break;
    }
    if (c=='9' || c=='z' || c=='Z') nextName[j]= '0';  /* need to carry */
    else { nextName[j]++; break; }                     /* usual case */
    i--;
    j--;
  }

  {
    char *tmp= headName;
    headName= 0;
    p_free(tmp);
    tmp= tailName;
    tailName= 0;
    p_free(tmp);
  }
  return nextName;
}

/*--------------------------------------------------------------------------*/

char *StructName(StructDef *base)
{
  IOStream *file;
  if (!base) return 0;
  file= base->file;
  if (file) return file->structTable.names[base->index];
  else return yStructTable.names[base->index];
}

/* CopyStruct returns an equivalent to the input StructDef for the
   input IOStream.  If the StructDef already belongs to the IOStream,
   it is returned.  Otherwise, if the name of the StructDef already
   exists, CopyStruct ensures that the existing structure is equivalent
   to the input structure.  If so, the equivalent StructDef is returned,
   if not, 0 is returned (this is the only reason CopyStruct fails).
   If the name has never been seen before, CopyStruct attempts to
   build an equivalent data structure, recursing as necessary.
   If file==0, CopyStruct returns base->model->model->...  */
StructDef *CopyStruct(IOStream *file, StructDef *base)
{
  StructDef *copy;
  long i, n;
  IOStream *baseFile= base->file;
  int verbatim;
  char **names;
  Member *members;

  if (file && baseFile==file) return base;

  /* reduce base to its ultimate in-memory model structure */
  verbatim= (baseFile && baseFile->history && baseFile->history->child==file);
  if (!verbatim) while (base->model) base= base->model;

  if (!file) return base;

  if (HashFind(&file->structTable, StructName(base), 0L)) {
    /* structure of this name already exists, check for equivalence
       -- this branch always halts following recursion as well */
    copy= file->structList[hashIndex];
    if (verbatim) return copy;
    while (base->model) base= base->model;
    return EquivStruct(copy, base)? copy : 0;
  }

  /* copy the member table -- first ensure that all structs referenced
     by any members have been added to file->structTable */
  names= base->table.names;
  members= base->members;
  n= base->table.nItems;
  for (i=0 ; i<n ; i++) CopyStruct(file, members[i].base);
  /* it is finally safe to add the struct copy to file->structTable... */
  copy= AddStruct(file, StructName(base), 0L);
  /* ...and fill in the members in the copy */
  for (i=0 ; i<n ; i++) {
    AddMember(copy, verbatim? base->offsets[i] : -1L,
              names[i],
              CopyStruct(file, members[i].base),
              members[i].dims);
  }

  if (verbatim) {
    copy->size= base->size;
    copy->alignment= base->alignment;
    copy->Copy= base->Copy;
    copy->addressType= base->addressType;
    copy->model= Ref(base->model);
    copy->Convert= base->Convert;
    copy->order= base->order;
    copy->fpLayout= Ref(base->fpLayout);
    copy->dataOps= base->dataOps;

  } else {
    if (n==0) {
      copy->size= base->size;
      copy->alignment= base->alignment;
      copy->order= base->order;
      copy->fpLayout= Ref(base->fpLayout);
    }
    InstallStruct(copy, base);
  }

  return copy;
}

/*--------------------------------------------------------------------------*/

long YTellIO(IOStream *file, long offset)
{
  p_file *stream= file->stream;
  if (stream) {
    long pos= p_ftell(stream);
    if (pos<0) {
      p_ferror(stream);
      YErrorIO("ftell failed in YTellIO (binary file I/O)");
    }
    return pos;
  } else {
    YErrorIO("attempt to query position in closed binary file");
    return -1L;
  }
}

void YSeekIO(IOStream *file, long offset)
{
  p_file *stream= file->stream;
  if (stream) {
    if (p_fseek(stream, offset)) {
      p_ferror(stream);
      YErrorIO("fseek failed in YSeekIO (binary file I/O)");
    }
  } else {
    YErrorIO("attempt to seek position in closed binary file");
  }
}

void YSeekEndIO(IOStream *file, long offset)
{
  p_file *stream= file->stream;
  if (stream) {
    if (p_fseek(stream, p_fsize(stream)+offset)) {
      p_ferror(stream);
      YErrorIO("fseek failed in YSeekEndIO (binary file I/O)");
    }
  } else {
    YErrorIO("attempt to seek position in closed binary file");
  }
}

void YCloseIO(IOStream *file)
{
  if (file->stream) {
    FlushFile(file, 1);
    p_fclose(file->stream);
    file->stream= 0;
  }
}

/*--------------------------------------------------------------------------*/

long YReadIO(IOStream *file, void *buf, long size, long n)
{
  p_file *stream= file->stream;
  /* can't detect permissions here because write might require this read */
  if (stream) {
    long nbytes= n*size;
    long nio= p_fread(stream, buf, nbytes);
    if (nio<nbytes) {
      if (p_ferror(stream)) {
        YErrorIO("fread failed in YReadIO (binary file I/O)");
      } else {
        nio/= size;
      }
    } else {
      nio= n;
    }
    return nio;
  } else {
    YErrorIO("attempt to read from closed binary file");
    return -1L;
  }
}

void YWriteIO(IOStream *file, const void *buf, long size, long n)
{
  p_file *stream= file->stream;
  if (!(file->permissions & 2))
    YErrorIO("attempt to write to binary file opened in r mode");
  if (stream) {
    long nbytes= n*size;
    long nio= p_fwrite(stream, buf, nbytes);
    if (nio<nbytes) {
      p_ferror(stream);
      YErrorIO("fwrite failed in YWriteIO (binary file I/O)");
    }
  } else {
    YErrorIO("attempt to write to closed binary file");
  }
}

/*--------------------------------------------------------------------------*/
