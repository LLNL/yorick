/*
 * $Id: std2.c,v 1.2 2010-05-12 04:03:30 dhmunro Exp $
 * Define standard Yorick built-in functions for binary I/O
 *
 *  See std.i for documentation on the interface functions defined here.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "ydata.h"
#include "yio.h"
#include "pstdlib.h"
#include "play.h"
#include <string.h>

extern BuiltIn Y_save, Y_restore, Y_add_record, Y__jt, Y__jc, Y__jr,
  Y_get_times, Y_get_ncycs, Y_get_vars, Y_get_addrs, Y_add_variable,
  Y_set_filesize, Y_set_blocksize, Y_add_member, Y_install_struct;
extern BuiltIn Y_set_cachesize;

extern BuiltIn Y_edit_times, Y_add_next_file, Y__read, Y__write,
  Y_data_align, Y_struct_align, Y__not_pdb, Y__init_pdb, Y__set_pdb,
  Y__init_clog, Y_dump_clog, Y_read_clog, Y_rename, Y_remove, Y_eq_nocopy;

extern BuiltIn Y_get_primitives, Y_set_vars, Y_lsdir, Y_mkdir, Y_rmdir;

extern void BuildDimList(Symbol *stack, int nArgs);  /* ops3.c */

extern void YPDBpointers(IOStream *file, long size, int align); /* binpdb.c */

extern void SetSequentialWrite(IOStream *file, long last);  /* yrdwr.c */

static void GrabVariable(IOStream *file, long iFile, long iGlob);
static void StuffVariable(IOStream *file, long iFile,
                          Operand *op, int notNew);

extern int YtestPDB(IOStream *file, int familyOK);
extern int YinitPDB(IOStream *file, int close102);
extern void (*YPDBcloser[16])(IOStream *file);

/*--------------------------------------------------------------------------*/

void Y_save(int nArgs)
{
  Operand op;
  IOStream *file;
  long globIndex;
  char *name;
  Symbol *stack= sp-nArgs+1;
  int notNew, structsOnly;
  HistoryInfo *history;
  if (nArgs<1) YError("save requires at least one argument");

  file= YGetFile(stack);
  history= file->history;
  if (history) {
    structsOnly= (history->nRecords<=0 || history->recNumber<0);
    file= history->child;
  } else {
    structsOnly= 0;
  }

  stack++;
  nArgs--;
  if (nArgs<=0) {
    if (history) YError("no save,f (save all) to history record");
    globIndex= 0;
    nArgs= 1;
  } else {
    globIndex= -1;
  }
  while (nArgs!=0) {
    if (globIndex<0) {
      /* get name and value to be saved */
      if (stack->ops!=&referenceSym)
        YError("save needs simple variable references as arguments");
      name= globalTable.names[stack->index];
      stack->ops->FormOperand(stack, &op);
      if (!op.ops || (!op.ops->isArray && op.ops!=&structDefOps))
        YError("save can output array data only");
      stack++;
      nArgs--;

    } else {
      /* save everything that's savable */
      Symbol *global;
      OpTable *gops;
      if (globIndex>=globalTable.nItems) break;
      global= &globTab[globIndex];
      name= globalTable.names[globIndex++];
      gops= global->ops;
      if (gops==&dataBlockSym) {
        Operations *ops= global->value.db->ops;
        if (!ops->isArray && ops!=&lvalueOps && ops!=&structDefOps) continue;
      } else if (gops!=&doubleScalar &&
                 gops!=&longScalar && gops!=&intScalar) {
        continue;
      }
      gops->FormOperand(global, &op);
    }

    /* look up name in dataTable (history child first, if any) */
    if (op.ops!=&structDefOps) {
      if (structsOnly) YError("file has no current record for save");
      notNew= AddVariable(file, -1L, name, op.type.base, op.type.dims);
      if (notNew>1)
        YError("data type (struct) name conflict in save to binary file");

      StuffVariable(file, hashIndex, &op, notNew);

    } else {
      if (!CopyStruct(file, (StructDef *)op.value))
        YError("problem saving struct to binary file (name conflict?)");
    }
  }

  /* be sure data is actually written to file */
  ClearPointees(file, 1);
  FlushFile(file, 0);
}

void Y_restore(int nArgs)
{
  IOStream *file, *parent= 0;
  long i, index;
  char *name;
  Symbol *stack= sp-nArgs+1;
  if (nArgs<1) YError("restore requires at least one argument");

  file= YGetFile(stack);
  if (file->history) {
    HistoryInfo *history= file->history;
    if (history->nRecords>0 && history->recNumber>=0) {
      file= history->child;
      parent= history->parent;
    }
  }

  stack++;
  nArgs--;

  if (nArgs) {
    /* specific list to be restored */
    do {
      /* get name and value to be restore */
      if (stack->ops!=&referenceSym)
        YError("restore needs simple variable references as arguments");
      index= stack->index;
      name= globalTable.names[index];
      stack++;
      nArgs--;

      /* look up name in dataTable (in child if a current record) */
      if (HashFind(&file->dataTable, name, 0L))
        GrabVariable(file, hashIndex, index);
      else if (parent && HashFind(&parent->dataTable, name, 0L))
        GrabVariable(parent, hashIndex, index);
      else
        YError("restore variable does not exist in binary file");

    } while (nArgs);

  } else {
    /* restore everything (only one of record or non-record) */
    long n= file->dataTable.nItems;
    char **dataNames= file->dataTable.names;
    for (i=0 ; i<n ; i++) {
      index= Globalize(dataNames[i], 0L);
      GrabVariable(file, i, index);
    }
  }

  ClearPointees(file, 0);
}

/*--------------------------------------------------------------------------*/

void Y_add_variable(int nArgs)
{
  Operand op;
  IOStream *file;
  long address;
  char *name;
  StructDef *base;
  Symbol *stack= sp-nArgs+1;
  if (nArgs<4) YError("add_variable requires at least four arguments");

  file= YGetFile(stack++);
  address= YGetInteger(stack++);
  name= YGetString(stack++);

  stack->ops->FormOperand(stack, &op);
  if (op.ops==&structDefOps) base= op.value;
  else if (op.ops==&stringOps && !op.type.dims) {
    char *typeName= ((char **)op.value)[0];
    if (!typeName || !HashFind(&file->structTable, typeName, 0L))
      YError("4th argument refers to non-existent data type");
    base= file->structList[hashIndex];
  } else {
    YError("4th argument must be either string or struct definition");
    base= 0;
  }

  nArgs-= 4;
  stack++;
  BuildDimList(stack, nArgs);

  AddVariable(file, address, name, base, tmpDims);
}

void Y_add_record(int nArgs)
{
  IOStream *file;
  HistoryInfo *history;
  Dimension *dims;
  double *time= 0;
  long *ncyc= 0, *address= 0;
  long nRecs= 0;
  int flags;
  Symbol *stack= sp-nArgs+1;
  if (nArgs<1 || nArgs>4)
    YError("add_record requires between one and four arguments");

  file= YGetFile(stack++);
  if (stack<=sp) {
    time= YGet_D(stack++, 1, &dims);
    if (time) nRecs= TotalNumber(dims);
    if (stack<=sp) {
      ncyc= YGet_L(stack++, 1, &dims);
      if (ncyc) {
        if (nRecs) {
          if (nRecs!=TotalNumber(dims))
            YError("inconsistent number of ncycs in add_record");
        } else {
          nRecs= TotalNumber(dims);
        }
      }
      if (stack<=sp) {
        address= YGet_L(stack++, 1, &dims);
        if (address) {
          if (nRecs) {
            if (nRecs!=TotalNumber(dims))
              YError("inconsistent number of addresses in add_record");
          } else {
            nRecs= TotalNumber(dims);
          }
        }
      }
    }
  }

  /* if this file has no history, add one */
  history= file->history;
  if (!history) history= AddHistory(file, 0L);

  /* if no records were specified, current record becomes none */
  if (!nRecs) history->recNumber= -1;

  /* add the specified records */
  flags= (time? 1 : 0)|(ncyc? 2 : 0);
  while (nRecs--) {
    AddRecord(history, flags, time? time[0]:0.0, ncyc? ncyc[0]:0L,
              (address && address[0]>=0)? address[0]:-1L);
    if (time) time++;
    if (ncyc) ncyc++;
    if (address) address++;
  }
  if (history->nRecords>0) JumpRecord(history, history->nRecords-1);
}

/*--------------------------------------------------------------------------*/

static void GetTNworker(int nArgs, int cycs);

void Y_get_times(int nArgs)
{
  GetTNworker(nArgs, 0);
}

void Y_get_ncycs(int nArgs)
{
  GetTNworker(nArgs, 1);
}

static void GetTNworker(int nArgs, int cycs)
{
  IOStream *file;
  long i, n= 0;
  long *ncycs= 0;
  double *times= 0;
  Array *array;
  HistoryInfo *history;
  Dimension *dims;
  if (nArgs!=1) YError("get_times/get_ncycs takes exactly one argument");

  file= yarg_file(0);
  history= file->history;

  if (history) {
    n= history->nRecords;
    if (cycs) ncycs= history->ncyc;
    else times= history->time;
  }
  if (n<=0 || (cycs? (!ncycs) : (!times))) {
    PushDataBlock(RefNC(&nilDB));
    return;
  }

  dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  tmpDims= NewDimension(n, 1L, (Dimension *)0);
  array= PushDataBlock(NewArray(cycs? &longStruct : &doubleStruct, tmpDims));

  if (cycs)
    for (i=0 ; i<n ; i++) array->value.l[i]= ncycs[i];
  else
    for (i=0 ; i<n ; i++) array->value.d[i]= times[i];
}

/*--------------------------------------------------------------------------*/

void Y__jt(int nArgs)
{
  Operand op;
  IOStream *file;
  HistoryInfo *history;
  double targetTime= 0.0;
  Symbol *stack= sp-nArgs+1;
  long recNumber;
  int noRecord;
  if (nArgs!=1 && nArgs!=2)
    YError("jt takes exactly one or two arguments");

  stack->ops->FormOperand(stack, &op);
  if (op.ops==&streamOps) {
    /* first argument is explicit binary file */
    file= op.value;
    history= file->history;
    if (!history)
      YError("binary file passed to jt has no history records");
    recNumber= history->recNumber;
    if (nArgs>1) {
      sp->ops->FormOperand(sp, &op);
      if (op.ops==&rangeOps) {        /* jt, file, - */
        /* look for   jt, file, -    (backup 1 record) */
        Range *range= op.value;
        if ((range->nilFlags&(~(R_MINNIL|R_MAXNIL|R_PSEUDO))) ||
            range->inc!=1)
          YError("did you want   jt, file, -   (jump to previous record)?");
        noRecord= recNumber>=0? JumpRecord(history, recNumber-1) :
                                JumpRecord(history, history->nRecords-1);

      } else if (op.ops==&voidOps) {  /* jt, file, [] */
        noRecord= recNumber>=0? JumpRecord(history, recNumber+1) :
                                JumpRecord(history, 0);

      } else {                        /* jt, file, time */
        targetTime= YGetReal(sp);
        noRecord= JumpToTime(history, targetTime);
        if (noRecord) {
          if (history->time) {
            noRecord= 2;
            targetTime= history->time[history->recNumber];
          } else {
            noRecord= 3;
          }
        }
      }

    } else {                          /* jt, file */
      noRecord= recNumber>=0? JumpRecord(history, recNumber+1) :
                              JumpRecord(history, 0);
    }

  } else {                            /* jt, time */
    IOFileLink *link;
    if (nArgs!=1)
      YError("did you want jt, file, time (instead of jt, time, file)?");

    targetTime= YGetReal(sp);
    for (link=yBinaryFiles ; link ; link=link->next) {
      file= link->ios;
      if (!file) continue;
      history= file->history;
      if (!history || history->recNumber<0) continue;
      JumpToTime(history, targetTime);
    }
    noRecord= 3;
  }

  if (!CalledAsSubroutine()) {
    if (noRecord>2) PushDataBlock(RefNC(&nilDB));
    if (noRecord>1) PushDoubleValue(targetTime);
    else if (noRecord) PushIntValue(0);
    else PushIntValue(1);
  }
}

void Y__jc(int nArgs)
{
  IOStream *file;
  HistoryInfo *history;
  int noRecord;
  if (nArgs!=2) YError("jc takes exactly two arguments");

  file= yarg_file(1);
  history= file->history;
  if (!history)
    YError("binary file passed to jc has no history records");

  noRecord= JumpToCycle(history, YGetInteger(sp));

  if (!CalledAsSubroutine()) {
    if (noRecord) PushDataBlock(RefNC(&nilDB));
    else PushLongValue(history->ncyc[history->recNumber]);
  }
}

void Y__jr(int nArgs)
{
  IOStream *file;
  HistoryInfo *history;
  int hasRecord;
  int amSubroutine= CalledAsSubroutine();
  if (nArgs!=2) YError("jr takes exactly two arguments");

  file= yarg_file(1);
  history= file->history;
  if (!history) {
    if (amSubroutine)
      YError("binary file passed to jc has no history records");
    hasRecord= 0;

  } else {
    long n= history->nRecords;
    long i= YGetInteger(sp);
    if (i<1) i+= n;
    if (i>=1 && i<=n) hasRecord= !JumpRecord(history, i-1);
    else hasRecord= 0;
  }

  if (!amSubroutine) PushIntValue(hasRecord);
}

/*--------------------------------------------------------------------------*/

void Y_get_vars(int nArgs)
{
  IOStream *file, *child;
  Array *array;
  char **pNames= 0, **cNames= 0;
  long i, nParent, nChild;
  Dimension *dims;
  if (nArgs!=1) YError("get_vars takes exactly one argument");

  file= yarg_file(0);

  child= file->history? file->history->child : 0;

  /* create result array */
  dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  tmpDims= NewDimension(2L, 1L, (Dimension *)0);
  array= PushDataBlock(NewArray(&pointerStruct, tmpDims));
  nParent= file->dataTable.nItems;
  nChild= child? child->dataTable.nItems : 0;
  if (nParent) {
    dims= tmpDims;
    tmpDims= 0;
    FreeDimension(dims);
    tmpDims= NewDimension(nParent, 1L, (Dimension *)0);
    array->value.p[0]= pNames= NewArray(&stringStruct, tmpDims)->value.q;
  }
  if (nChild) {
    dims= tmpDims;
    tmpDims= 0;
    FreeDimension(dims);
    tmpDims= NewDimension(nChild, 1L, (Dimension *)0);
    array->value.p[1]= cNames= NewArray(&stringStruct, tmpDims)->value.q;
  }

  /* and fill it */
  for (i=0 ; i<nParent ; i++) pNames[i]= p_strcpy(file->dataTable.names[i]);
  for (i=0 ; i<nChild ; i++) cNames[i]= p_strcpy(child->dataTable.names[i]);
}

typedef struct y_hashtmp y_hashtmp;
struct y_hashtmp {
  int references;
  Operations *ops;
  void (*zapper)(void *to);
  HashTable table;
};

static void y_zap_hashtmp(void *ht);

static void
y_zap_hashtmp(void *ht)
{
  y_hashtmp *h = ht;
  HashClear(&h->table);
  p_free(h);
}

void
Y_set_vars(int nArgs)
{
  Symbol *stack = sp - nArgs + 1;
  IOStream *file, *child;
  char **vars, **rvars;
  long i, nvars, nrvars;
  Dimension *dims;
  if (nArgs<2 || nArgs>3 || !stack[0].ops || !stack[1].ops)
    YError("set_vars takes exactly two or three arguments");

  file = YGetFile(stack++);
  child = file->history? file->history->child : 0;
  vars = YGet_Q(stack++, 1, &dims);
  nvars = (vars&&dims)? (dims->next? -1 : dims->number) : 0;
  if (nArgs==3) {
    rvars = YGet_Q(stack++, 1, &dims);
    nrvars = (vars&&dims)? (dims->next? -1 : dims->number) : 0;
  } else {
    rvars = 0;
    nrvars = 0;
  }
  if (nvars<0 || nrvars<0) YError("set_vars var lists must be 1D");
  if ((nvars && nvars!=file->dataTable.nItems) ||
      (nrvars && nrvars!=(child? child->dataTable.nItems : 0)))
    YError("set_vars var lists must match number of vars in file");

  if (nvars) {
    HashTable tmp;
    y_hashtmp *h = y_new_tmpobj(sizeof(y_hashtmp), y_zap_hashtmp);
    HashInit(&h->table, nvars);
    PushDataBlock(h);
    for (i=0 ; i<nvars ; i++)
      if (HashAdd(&h->table, vars[i], 0)) break;
    if (i<nvars) YError("duplicate names in set_vars static var list");
    if (p_signalling) p_abort();
    tmp = h->table;
    h->table = file->dataTable;
    file->dataTable = tmp;
    Drop(1);
  }

  if (nrvars) {
    HashTable tmp;
    y_hashtmp *h = y_new_tmpobj(sizeof(y_hashtmp), y_zap_hashtmp);
    HashInit(&h->table, nvars);
    PushDataBlock(h);
    for (i=0 ; i<nrvars ; i++)
      if (HashAdd(&h->table, rvars[i], 0)) break;
    if (i<nrvars) YError("duplicate names in set_vars record var list");
    if (p_signalling) p_abort();
    tmp = h->table;
    h->table = child->dataTable;
    child->dataTable = tmp;
    Drop(1);
  }
}

void Y_get_addrs(int nArgs)
{
  IOStream *file, *child;
  Array *array;
  long *pAddrs= 0, *cAddrs= 0, *rAddrs= 0;
  int *rFiles= 0;
  char **rNames= 0;
  long i, nParent, nChild, nFamily, nRecords;
  HistoryInfo *history;
  Dimension *dims;
  if (nArgs!=1) YError("get_addrs takes exactly one argument");

  file= yarg_file(0);

  history= file->history;
  child= history? history->child : 0;

  /* create result array */
  dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  tmpDims= NewDimension(5L, 1L, (Dimension *)0);
  array= PushDataBlock(NewArray(&pointerStruct, tmpDims));
  nParent= file->dataTable.nItems;
  nChild= child? child->dataTable.nItems : 0;
  nRecords= child? history->nRecords : 0;
  nFamily= child? history->nFamily : 0;
  if (nParent) {
    dims= tmpDims;
    tmpDims= 0;
    FreeDimension(dims);
    tmpDims= NewDimension(nParent, 1L, (Dimension *)0);
    array->value.p[0]= pAddrs= NewArray(&longStruct, tmpDims)->value.l;
  }
  if (nChild) {
    dims= tmpDims;
    tmpDims= 0;
    FreeDimension(dims);
    tmpDims= NewDimension(nChild, 1L, (Dimension *)0);
    array->value.p[1]= cAddrs= NewArray(&longStruct, tmpDims)->value.l;
  }
  if (nRecords>0) {
    dims= tmpDims;
    tmpDims= 0;
    FreeDimension(dims);
    tmpDims= NewDimension(nRecords, 1L, (Dimension *)0);
    array->value.p[2]= rAddrs= NewArray(&longStruct, tmpDims)->value.l;
    array->value.p[3]= rFiles= NewArray(&intStruct, tmpDims)->value.i;
  }
  if (nFamily>0) {
    dims= tmpDims;
    tmpDims= 0;
    FreeDimension(dims);
    tmpDims= NewDimension(nFamily, 1L, (Dimension *)0);
    array->value.p[4]= rNames= NewArray(&stringStruct, tmpDims)->value.q;
  }

  /* and fill it */
  for (i=0 ; i<nParent ; i++) pAddrs[i]= file->addresses[i]+file->offset;
  for (i=0 ; i<nChild ; i++) cAddrs[i]= child->addresses[i];
  for (i=0 ; i<nRecords ; i++) rAddrs[i]= history->offset[i];
  for (i=0 ; i<nRecords ; i++) rFiles[i]= history->ifile[i];
  for (i=0 ; i<nFamily ; i++) rNames[i]= p_strcpy(history->famNames[i]);
}

void Y_set_filesize(int nArgs)
{
  IOStream *file;
  HistoryInfo *history;
  long size;
  if (nArgs!=2) YError("set_filesize takes exactly two arguments");

  file= yarg_file(1);
  history= file->history;
  if (!history)
    YError("binary file passed to set_filesize has no history records");

  size= YGetInteger(sp);
  if (size<file->blockSize) size= file->blockSize;
  history->fileSize= size;
}

static long y_legal_blksz(long nbytes);
static long 
y_legal_blksz(long nbytes)
{
  long size = 4096;
  while (size < nbytes) {
    if ((size<<1) >= yMaxBlockSize) break;
    size <<= 1;
  }
  return size;
}

void
Y_set_blocksize(int nArgs)
{
  IOStream *file = 0;
  long size;

  if (nArgs == 2) file = yarg_file(1);
  else if (nArgs != 1) YError("set_blocksize takes one or two arguments");

  size = y_legal_blksz(YGetInteger(sp)) - 1;

  if (file) file->blockSize = size;
  else y_block_size_0 = size;
}

void Y_set_cachesize(int nArgs)
{
  IOStream *file;
  long size;
  /* yMaxBlockSize, yCacheSize declared in binio.h */

  if (nArgs!=2) YError("set_cachesize takes exactly two arguments");

  yMaxBlockSize = y_legal_blksz(YGetInteger(sp-1));
  size = YGetInteger(sp);
  if (size < 4*yMaxBlockSize) size = 4*yMaxBlockSize;
  yCacheSize = size;
}

/*--------------------------------------------------------------------------*/

void Y_edit_times(int nArgs)
{
  Symbol *stack= sp-nArgs+1;
  IOStream *file;
  HistoryInfo *history;
  int *ifiles;
  long *offsets;
  Dimension *dims;
  long nKeep, *keepList= 0;
  double *newTimes= 0, *times, bad= 0.0;
  long *newNcycs= 0, *ncycs, nbad= 0;
  long i, nRecs, j;
  if (nArgs<1) YError("edit_times needs at least one argument");

  file= YGetFile(stack);
  history= file->history;
  if (!history)
    YError("binary file in edit_times has no history records");
  ifiles= history->ifile;
  offsets= history->offset;
  times= history->time;
  ncycs= history->ncyc;
  nRecs= history->nRecords;

  /* collect keepList, newTimes, and newNcycs arguments */
  nKeep= 0;
  if (nArgs>1) {
    long lastKept= -1;
    stack++;
    keepList= YGet_L(stack, 1, &dims);
    if (keepList) nKeep= TotalNumber(dims);
    else nKeep= 0;
    for (i=0 ; i<nKeep ; i++) {
      if (keepList[i]<0 || keepList[i]>=nRecs || keepList[i]<=lastKept)
        YError("keep_list out of range or not increasing in edit_times");
      lastKept= keepList[i];
    }
    if (nArgs>2) {
      stack++;
      newTimes= YGet_D(stack, 1, &dims);
      i= TotalNumber(dims);
      if (newTimes && (nKeep? (i!=nKeep) : (i!=nRecs)))
        YError("new_times has wrong length in edit_times");
      if (nArgs>3) {
        stack++;
        newNcycs= YGet_L(stack, 1, &dims);
        i= TotalNumber(dims);
        if (newNcycs && (nKeep? (i!=nKeep) : (i!=nRecs)))
          YError("new_ncycs has wrong length in edit_times");
      }
    }
  }

  /* find appropriate bad value to mark records to be deleted */
  if (times) {
    bad= times[0];
    for (i=1 ; i<nRecs ; i++) if (times[i]>bad) bad= times[i];
    if (bad>0.5) bad*= 1.001;
    else if (bad<-0.5) bad*= 0.999;
    else bad= 1.0;
  } else if (ncycs) {
    nbad= ncycs[0];
    for (i=1 ; i<nRecs ; i++) if (ncycs[i]>nbad) nbad= ncycs[i];
    nbad++;
  }

  /* mark the records to be deleted (if any) */
  if (!keepList) nKeep= nRecs;
  if (!keepList && !newTimes && !newNcycs) {
    /* filter to record lists to force strictly increasing times or ncycs */
    if (times) {
      double downTo;
      downTo= times[nRecs-1];
      for (i=nRecs-2 ; i>=0 ; i--) {
        if (times[i]<downTo) downTo= times[i];
        else { times[i]= bad; nKeep--; }  /* mark records to be deleted */
      }
    } else if (ncycs) {
      long downTo;
      downTo= ncycs[nRecs-1];
      for (i=nRecs-2 ; i>=0 ; i--) {
        if (ncycs[i]<downTo) downTo= ncycs[i];
        else { ncycs[i]= nbad; nKeep--; }  /* mark records to be deleted */
      }
    }
  }

  /* delete requested records */
  if (nKeep && nKeep<nRecs) {
    if (keepList) {
      for (i=0 ; i<nKeep ; i++) {
        j= keepList[i];
        ifiles[i]= ifiles[j];
        offsets[i]= offsets[j];
        if (times) times[i]= times[j];
        if (ncycs) ncycs[i]= ncycs[j];
      }
      history->nRecords= nRecs= nKeep;

    } else if (times) {
      for (i=j=0 ; i<nRecs ; i++) {
        if (times[i]!=bad) {
          ifiles[j]= ifiles[i];
          offsets[j]= offsets[i];
          if (ncycs) ncycs[j]= ncycs[i];
          times[j++]= times[i];
        }
      }
      history->nRecords= nRecs= j;

    } else if (ncycs) {
      for (i=j=0 ; i<nRecs ; i++) {
        if (ncycs[i]!=nbad) {
          ifiles[j]= ifiles[i];
          offsets[j]= offsets[i];
          ncycs[j++]= ncycs[i];
        }
      }
      history->nRecords= nRecs= j;
    }
  }

  /* install new times */
  if (newTimes && nRecs>0) {
    if (!times) times = history->time =
                  p_malloc(sizeof(double)*(16+((nRecs-1)&~0xf)));
    for (i=0 ; i<nRecs ; i++) times[i]= newTimes[i];
  }

  /* install new ncycs */
  if (newNcycs && nRecs>0) {
    if (!ncycs) ncycs = history->ncyc =
                  p_malloc(sizeof(long)*(16+((nRecs-1)&~0xf)));
    for (i=0 ; i<nRecs ; i++) ncycs[i]= newNcycs[i];
  }
}

/*--------------------------------------------------------------------------*/

void Y__read(int nArgs)
{
  Operand op;
  IOStream *file;
  long address;
  StructDef *base;
  if (nArgs!=3) YError("_read takes exactly three arguments");

  file= yarg_file(2);
  if (file->history) file= file->history->child;

  address= YGetInteger(sp-1);

  if (sp->ops!=&referenceSym)
    YError("third argument to _read must be a simple variable reference");
  op.owner= &globTab[sp->index];
  op.owner->ops->FormOperand(op.owner, &op);
  if (!op.ops->isArray)
    YError("third argument to _read must be array or scalar data");
  if (!HashFind(&file->structTable, StructName(op.type.base), 0L))
    YError("data type of third argument to _read undefined for this file");
  base= file->structList[hashIndex];

  if (op.type.base==&charStruct) {
    /* special case type char, so that such a read works even if an
       EOF occurs before the read completes */
    long nbytes= YcRead(file, op.value, address, op.type.number);
    if (nbytes<op.type.number) {
      char *data= op.value;
      memset(data+nbytes, 0, op.type.number-nbytes);
    }
    PushLongValue(nbytes);

  } else {
    YRead(op.value, address, base, op.type.number, (Strider *)0);
    PushLongValue(op.type.number);
  }
}

/* this keyword does not work perfectly -- it will not work correctly
   unless the on read an object of the specified type in the file
   would be translated to the actual type of the argument
   note that unrecognized integers will be read as short if they are
   2 bytes, long otherwise, independent of the memory integer sizes */
static char *wrtKeys[2]= { "as", 0 };

void Y__write(int nArgs)
{
  Symbol *keySymbols[1];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, wrtKeys, keySymbols);
  IOStream *file= 0;
  long address= 0;
  int got_address= 0;
  Symbol *object= 0;
  Operand op;
  StructDef *base;
  char *type;

  while (stack<=sp) {
    if (!stack->ops) { stack+= 2; continue; }
    if (!file) {
      file= YGetFile(stack);
      if (file->history) file= file->history->child;
    } else if (!got_address) {
      got_address= 1;
      address= YGetInteger(stack);
    } else if (!object) {
      object= stack;
    } else {
      object= 0;
    }
    stack++;
  }
  if (!object) YError("_write takes exactly three arguments");

  sp->ops->FormOperand(object, &op);
  if (!op.ops->isArray)
    YError("third argument to _write must be array or scalar data");
  if (YNotNil(keySymbols[0])) type= YGetString(keySymbols[0]);
  else type= StructName(op.type.base);
  if (!HashFind(&file->structTable, type, 0L))
    YError("data type of third argument to _write undefined for this file");
  base= file->structList[hashIndex];

  if (op.type.base==&charStruct) {
    /* special case type char, to have a way to do literal writes */
    YcWrite(file, op.value, address, op.type.number);

  } else {
    YWrite(op.value, address, base, op.type.number, (Strider *)0);
  }

  PushDataBlock(RefNC(&nilDB));
}

/*--------------------------------------------------------------------------*/

void Y_add_member(int nArgs)
{
  Operand op;
  IOStream *file;
  long offset;
  char *structName, *name;
  StructDef *memType, *base;
  Symbol *stack= sp-nArgs+1;
  if (nArgs<5) YError("add_member requires at least five arguments");

  file= YGetFile(stack++);
  structName= YGetString(stack++);
  offset= YGetInteger(stack++);
  name= YGetString(stack++);

  stack->ops->FormOperand(stack, &op);
  if (op.ops==&structDefOps) memType= op.value;
  else if (op.ops==&stringOps && !op.type.dims) {
    char *typeName= ((char **)op.value)[0];
    if (!HashFind(&file->structTable, typeName, 0L))
      YError("5th argument refers to non-existent data type");
    memType= file->structList[hashIndex];
  } else {
    YError("5th argument must be either string or struct definition");
    memType= 0;
  }

  if (HashFind(&file->structTable, structName, 0L))
    base= file->structList[hashIndex];
  else
    base= AddStruct(file, structName, 0L);

  if (!base) YError("unable to create given struct_name in add_member");

  nArgs-= 5;
  stack++;
  BuildDimList(stack, nArgs);

  if (AddMember(base, offset, name, memType, tmpDims))
    YError("add_member failed -- duplicate member name?");

  Drop(nArgs);
}

void Y_install_struct(int nArgs)
{
  IOStream *file;
  long size= 0, align= 0, order= 0, *layout= 0;
  Dimension *dims;
  FPLayout fpLayout;
  char *structName;
  StructDef *base, *model;
  Symbol *stack= sp-nArgs+1;
  if (nArgs!=2 && nArgs!=5 && nArgs!=6)
    YError("install_struct requires 2, 5, or 6 arguments");

  file= YGetFile(stack++);
  structName= YGetString(stack++);

  if (nArgs>2) {
    size= YGetInteger(stack++);
    align= YGetInteger(stack++);
    order= YGetInteger(stack++);
    if (nArgs==6) {
      layout= YGet_L(stack, 1, &dims);
      if (!layout || TotalNumber(dims)!=7)
        YError("layout argument must be array of 7 longs in install_struct");
      fpLayout.sgnAddr= (int)layout[0];
      fpLayout.expAddr= (int)layout[1];
      fpLayout.expSize= (int)layout[2];
      fpLayout.manAddr= (int)layout[3];
      fpLayout.manSize= (int)layout[4];
      fpLayout.manNorm= (int)layout[5];
      fpLayout.expBias= layout[6];
    }
  }

  if (HashFind(&file->structTable, structName, 0L)) {
    if (hashIndex<=6 && nArgs<=2)
      YError("install_struct cannot change primitive type into a struct");
    base= file->structList[hashIndex];
    if (hashIndex>=8 && base->dataOps)
      YError("install_struct cannot redefine non-primitive data type");
    model= base->model;
    if (model) while (model->model) model= model->model;
    base->dataOps= 0;
  } else {
    base= AddStruct(file, structName, 0L);
    model= 0;
  }

  if (!base) YError("unable to create given struct_name in install_struct");

  if (nArgs>2) {
    int addressType= 1;
    if (order>=size && size>1) {
      order= 0;
      addressType= 2;
    }
    if (DefinePrimitive(base, size, (int)align, addressType, (int)order,
                        layout? &fpLayout : 0, model, (Converter *)0))
      YError("failed to define primitive data type in add_member");
  }

  InstallStruct(base, (StructDef *)0);

  Drop(nArgs);
}

void Y_get_primitives(int nArgs)
{
  IOStream *file;
  Dimension *dims;
  Array *array;
  long *p;
  FPLayout *fpl;
  int i, j;
  if (nArgs!=1) YError("get_primitives requires exactly one argument");

  file = yarg_file(0);
  dims = tmpDims;
  tmpDims = 0;
  FreeDimension(dims);
  tmpDims = NewDimension(32L, 1L, (Dimension *)0);
  array = PushDataBlock(NewArray(&longStruct, tmpDims));
  p = array->value.l;

  for (i=j=0 ; i<6 ; i++) {
    p[j++] = file->structList[i]->size;
    p[j++] = file->structList[i]->alignment;
    p[j++] = file->structList[i]->order;
  }
  p[1] = file->structAlign;
  for (i=4 ; i<6 ; i++) {
    fpl = file->structList[i]->fpLayout;
    p[j++] = fpl->sgnAddr;
    p[j++] = fpl->expAddr;
    p[j++] = fpl->expSize;
    p[j++] = fpl->manAddr;
    p[j++] = fpl->manSize;
    p[j++] = fpl->manNorm;
    p[j++] = fpl->expBias;
  }
}

/*--------------------------------------------------------------------------*/

void Y_add_next_file(int nArgs)
{
  Operand op;
  IOStream *file;
  HistoryInfo *history;
  char *filename= 0;
  int create;
  Symbol *stack= sp-nArgs+1;
  if (nArgs<1 || nArgs>3)
    YError("add_next_file takes exactly 1, 2, or 3 arguments");

  file= YGetFile(stack++);
  history= file->history;
  if (!history || history->nRecords<1)
    YError("file has no history records in add_next_file");
  create= (file->permissions&2)? 1 : 0;

  if (stack<=sp) {
    stack->ops->FormOperand(stack, &op);
    stack++;
    if (op.ops==&stringOps && !op.type.dims)
      filename= ((char **)op.value)[0];
    else if (op.ops!=&voidOps)
      YError("bad filename argument in add_next_file");
    if (stack<=sp) {
      stack->ops->FormOperand(stack, &op);
      if (op.ops!=&voidOps) create= (YGetInteger(stack)!=0);
    }
  }

  PushIntValue(AddNextFile(history, filename, create));
}

/*--------------------------------------------------------------------------*/

void Y_data_align(int nArgs)
{
  IOStream *file;
  int alignment;
  if (nArgs!=2) YError("data_align takes exactly two arguments");

  file= yarg_file(1);
  alignment= (int)YGetInteger(sp);

  file->dataAlign= alignment>0? alignment : 0;
  Drop(2);
}

void Y_struct_align(int nArgs)
{
  IOStream *file;
  int alignment;
  if (nArgs!=2) YError("struct_align takes exactly two arguments");

  file= yarg_file(1);
  alignment= (int)YGetInteger(sp);

  file->structAlign= (alignment!=0)? alignment : yStructAlign;
  Drop(2);
}

/*--------------------------------------------------------------------------*/

static int doNotRecurse= 1;  /* dummy for ReadGather/WriteScatter */

extern void ReadGather(void *dst, void *srcM, long srcD, StructDef *base,
                       long number, const Strider *strider);
extern void WriteScatter(void *src, void *dstM, long dstD, StructDef *base,
                         long number, const Strider *strider);

/* Grab the variable file->dataTable.names[iFile] and stuff its value
   into globTab[iGlob].  */
static void GrabVariable(IOStream *file, long iFile, long iGlob)
{
  StructDef *base= file->types[iFile].base;
  Dimension *dims= file->types[iFile].dims;
  long address= file->addresses[iFile]+file->offset;
  void *memory= 0;
  long number;

  StructDef *model= base->model;
  while (model->model) model= model->model;

  /* delete current in-memory value */
  if (globTab[iGlob].ops==&dataBlockSym) {
    globTab[iGlob].ops= &intScalar;
    Unref(globTab[iGlob].value.db);
  }

  /* check for scalar types and simplify if possible */
  if (!dims) {
    Operations *ops= model->dataOps;
    if (ops==&doubleOps) {
      globTab[iGlob].ops= &doubleScalar;
      memory= &globTab[iGlob].value.d;
    } else if (ops==&longOps) {
      globTab[iGlob].ops= &longScalar;
      memory= &globTab[iGlob].value.l;
    } else if (ops==&intOps) {
      globTab[iGlob].ops= &intScalar;
      memory= &globTab[iGlob].value.i;
    }
  }

  /* otherwise, create an array to hold the result */
  if (!memory) {
    Array *array;
    array= NewArray(model, dims);
    globTab[iGlob].value.db= (DataBlock *)array;
    globTab[iGlob].ops= &dataBlockSym;
    memory= array->value.c;
    number= array->type.number;
  } else {
    number= 1;
  }

  ReadGather(memory, &doNotRecurse, address, base, number, (Strider *)0);
}

static void StuffVariable(IOStream *file, long iFile,
                          Operand *op, int notNew)
{
  StructDef *base= file->types[iFile].base;
  long address= file->addresses[iFile]+file->offset;

  if (notNew) {
    /* this is an assignment to an existing variable --
       verify operand data type and number */
    long number= file->types[iFile].number;
    if (!EquivStruct(base, op->type.base) || number!=op->type.number)
      YError("variable type or dimensions have changed since last save");
  }

  if (base->addressType==2)
    SetSequentialWrite(file, address+base->size*op->type.number);
  WriteScatter(op->value, &doNotRecurse, address, base, op->type.number,
               (Strider *)0);
}

/*--------------------------------------------------------------------------*/

extern int yPDBopen;  /* in binpdb.c */
static long pdb_open= 0;

void Y__not_pdb(int nArgs)
{
  IOStream *file;
  int familyOK, notOK;
  if (nArgs!=2) YError("_not_pdb takes exactly two arguments");

  file= yarg_file(1);
  familyOK= (int)YGetInteger(sp);

  if (!pdb_open) pdb_open= Globalize("yPDBopen", 0L);
  if (globTab[pdb_open].ops!=&longScalar &&
      globTab[pdb_open].ops!=&intScalar)
    YError("yPDBopen variable must be an int or long scalar");

  yPDBopen= (int)YGetInteger(&globTab[pdb_open]);
  notOK= YtestPDB(file, familyOK);

  if (notOK>1) {
    YWarning("file is open as a PDB file, but partially broken");
    notOK= 0;

  } else if (notOK==1) {
    /* check for a Clog file if it didn't have a PDB header */
    notOK= CLopen(file, familyOK);
  }

  PushIntValue(notOK);
  PopTo(sp-3);
  Drop(2);
}

void Y__init_pdb(int nArgs)
{
  IOStream *file;
  int close102;
  if (nArgs!=2) YError("_init_pdb takes exactly two arguments");

  file= yarg_file(1);
  close102= ((int)YGetInteger(sp)) & 017;

  if (YinitPDB(file, close102))
    YError("unable to initialize PDB file (no write permission?)");
  Drop(1);
}

void Y__set_pdb(int nArgs)
{
  IOStream *file;
  int close102;
  if (nArgs!=2) YError("_set_pdb takes exactly two arguments");

  file= yarg_file(1);
  close102= ((int)YGetInteger(sp)) & 017;

  if (file->history) {
    file->history->parent->CloseHook=
      file->history->child->CloseHook= YPDBcloser[close102];
  } else {
    file->CloseHook= YPDBcloser[close102];
  }
  Drop(1);
}

void Y__init_clog(int nArgs)
{
  IOStream *file;
  if (nArgs!=1) YError("_init_clog takes exactly one argument");

  file= yarg_file(0);

  if (file->history) {
    file->history->parent->CloseHook=
      file->history->child->CloseHook= &CLclose;
  } else {
    file->CloseHook= &CLclose;
  }
  Drop(1);
}

void Y_dump_clog(int nArgs)
{
  IOStream *file;
  char *name;
  if (nArgs!=2) YError("dump_clog takes exactly two arguments");

  file= yarg_file(1);
  name= YGetString(sp);

  if (DumpClogFile(file, name))
    YError("dump_clog failed -- unable to open output file");
  Drop(2);
}

void Y_read_clog(int nArgs)
{
  IOStream *file;
  char *name;
  if (nArgs!=2) YError("read_clog takes exactly two arguments");

  file= yarg_file(1);
  name= yarg_sq(0);

  if (ReadClogFile(file, name))
    YError("read_clog failed -- unable to read input file");
  Drop(1);
}

/*--------------------------------------------------------------------------*/

void Y_rename(int nArgs)
{
  if (nArgs!=2) YError("rename takes exactly two arguments");
  p_rename(YGetString(sp-1), YGetString(sp));
}

void Y_remove(int nArgs)
{
  if (nArgs!=1) YError("remove takes exactly one argument");
  p_remove(YGetString(sp));
}

/*--------------------------------------------------------------------------*/

void Y_eq_nocopy(int nArgs)
{
  Symbol *stack= sp-nArgs+1;
  Symbol *glob;
  if (nArgs!=2) YError("eq_nocopy takes exactly two arguments");
  if (stack->ops!=&referenceSym)
    YError("eq_nocopy first argument not simple variable reference");
  glob= &globTab[stack->index];

  /* destroy current value of variable */
  if (glob->ops==&dataBlockSym) {
    glob->ops= &intScalar;
    Unref(glob->value.db);
  }

  /* copy top of stack to variable */
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  if (sp->ops==&dataBlockSym) {
    Array *array= (Array *)sp->value.db;
    /* fetch an LValue, but otherwise just increase the reference
     * count of the object
     * unlike Define() action, which copies arrays that have other
     * references */
    if (array->ops==&lvalueOps) {
      LValue *lvalue= (LValue *)array;
      Array *owner= lvalue->owner;
      /* actually only want to fetch a non-trivial LValue */
      if (!owner || lvalue->strider ||
          lvalue->type.dims!=owner->type.dims ||
          lvalue->type.base!=owner->type.base) {
        array= FetchLValue(array, sp);
      } else {
        array= owner;
      }
    }
    glob->value.db= (DataBlock *)Ref(array);
  } else {
    glob->value= sp->value;
  }
  glob->ops= sp->ops;
}

/*--------------------------------------------------------------------------*/

/* temporary stack object is a foreign yorick data type */

typedef struct y_tmpobj y_tmpobj;
struct y_tmpobj {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  void (*zapper)(void *to);
};

static void y_zap_tmpobj(void *to);

Operations yo_tmpobj = {
  &y_zap_tmpobj, T_OPAQUE, 0, T_STRING, "tmpobj",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &EvalX, &SetupX, &GetMemberX, &MatMultX, &PrintX
};

static void
y_zap_tmpobj(void *to)
{
  y_tmpobj *tmpobj = to;
  tmpobj->zapper(to);
}

void *
y_new_tmpobj(unsigned long size, void (*zapper)(void *to))
{
  y_tmpobj *tmpobj = p_malloc(size);
  tmpobj->references = 0;
  tmpobj->ops = &yo_tmpobj;
  tmpobj->zapper = zapper;
  return tmpobj;
}

/*--------------------------------------------------------------------------*/

/* temporary for building directory list is a foreign yorick data type */
typedef struct y_dirlist y_dirlist;
struct y_dirlist {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  void (*zapper)(void *to);
  p_dir *dir;
  char **fils;
  char **subs;
  long nfils, nsubs;
};

static void y_zap_dirlist(void *dl);

static void
y_zap_dirlist(void *dl)
{
  y_dirlist *dlist = dl;
  p_dir *dir = dlist->dir;
  char **fils = dlist->fils;
  char **subs = dlist->subs;
  long i;
  dlist->dir = 0;
  dlist->nfils = dlist->nsubs = 0;
  if (dir) p_dclose(dir);
  if (fils) {
    for (i=0 ; i<dlist->nfils ; i++) p_free(fils[i]);
    p_free(fils);
  }
  if (subs) {
    for (i=0 ; i<dlist->nsubs ; i++) p_free(subs[i]);
    p_free(subs);
  }
  p_free(dlist);
}

static void y_add_item(void *plist, long *pn, void *item);

static void
y_add_item(void *plist, long *pn, void *item)
{
  void **list = *(void ***)plist;
  long n = *pn;
  if (!list) {
    *(void ***)plist = list = p_malloc(sizeof(void *)*32);
    n = 0;
  } else if (n>31 && !((n-1)&n)) {
    *(void ***)plist = list = p_realloc(list, sizeof(void *)*2*n);
  }
  list[n] = item;
  *pn = n+1;
}

void
Y_lsdir(int nArgs)
{
  Symbol *stack = sp-nArgs+1;
  Symbol *glob = 0;
  Dimension *dims;
  y_dirlist *dlist;
  char *name, **list;
  long i;
  int is_dir = 0;
  if (nArgs<1 || nArgs>2) YError("lsdir takes one or two arguments");
  if (nArgs==2) {
    if (!stack->ops || sp->ops!=&referenceSym)
      YError("lsdir second argument not simple variable reference");
    glob = &globTab[sp->index];
    sp--;
  }

  dlist = y_new_tmpobj(sizeof(y_dirlist), y_zap_dirlist);
  dlist->dir = 0;
  dlist->fils = dlist->subs = 0;
  dlist->nfils = dlist->nsubs = 0;
  PushDataBlock(dlist);

  dlist->dir = p_dopen(YGetString(stack));

  if (!dlist->dir) {
    Drop(1);
    PushLongValue(0);
    return;
  }

  while ((name = p_dnext(dlist->dir, &is_dir))) {
    if (is_dir && nArgs==2)
      y_add_item(&dlist->subs, &dlist->nsubs, p_strcpy(name));
    else
      y_add_item(&dlist->fils, &dlist->nfils, p_strcpy(name));
  }

  if (stack->ops==&dataBlockSym) {
    stack->ops = &intScalar;
    Unref(stack->value.db);
  }
  if (dlist->nfils) {
    char *nm;
    dims = tmpDims;
    tmpDims = 0;
    FreeDimension(dims);
    tmpDims = NewDimension(dlist->nfils, 1L, (Dimension *)0);
    stack->value.db = (DataBlock *)NewArray(&stringStruct, tmpDims);
    stack->ops = &dataBlockSym;
    list = ((Array *)stack->value.db)->value.q;
    for (i=0 ; i<dlist->nfils ; i++) {
      nm = dlist->fils[i];
      dlist->fils[i] = 0;
      list[i] = nm;
    }
    list = dlist->fils;
    dlist->fils = 0;
    p_free(list);
  } else {
    stack->value.db = (DataBlock *)(RefNC(&nilDB));
    stack->ops = &dataBlockSym;
  }

  if (glob) {
    if (glob->ops==&dataBlockSym) {
      glob->ops = &intScalar;
      Unref(glob->value.db);
    }
    if (dlist->nsubs) {
      char *nm;
      dims = tmpDims;
      tmpDims = 0;
      FreeDimension(dims);
      tmpDims = NewDimension(dlist->nsubs, 1L, (Dimension *)0);
      glob->value.db = (DataBlock *)NewArray(&stringStruct, tmpDims);
      glob->ops = &dataBlockSym;
      list = ((Array *)glob->value.db)->value.q;
      for (i=0 ; i<dlist->nsubs ; i++) {
        nm = dlist->subs[i];
        dlist->subs[i] = 0;
        list[i] = nm;
      }
      list = dlist->subs;
      dlist->subs = 0;
      p_free(list);
    } else {
      glob->value.db = (DataBlock *)(RefNC(&nilDB));
      glob->ops = &dataBlockSym;
    }
  }

  Drop(1);
}

void Y_mkdir(int nArgs)
{
  if (nArgs!=1) YError("mkdir takes exactly one argument");
  p_mkdir(YGetString(sp));
}

void Y_rmdir(int nArgs)
{
  if (nArgs!=1) YError("rmdir takes exactly one argument");
  p_rmdir(YGetString(sp));
}

/*--------------------------------------------------------------------------*/
