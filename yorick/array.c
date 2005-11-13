/*
 * $Id: array.c,v 1.2 2005-11-13 21:01:56 dhmunro Exp $
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*
    Implement Yorick array indexing.  This is one half of the Yorick
    Eval instruction -- the other half of Eval is function invocation,
    which is handled in fnctn.c.  An expression of the form:
       expr(index_list)
    where index_list is a comma delimited list of indices, is an indexed
    array if expr is any array data type (or a function invocation if
    expr is any function data type).  The following index types are
    handled here:

    (1) nil   - the corresponding dimension is copied to the result
    (2) any integer scalar  - reduces the rank of the result
    (3) any integer array   - replaces the corresponding dimension by
        (index list)          a copy of the index list dimensions
    (4) start:stop:step     - replaces the corresponding dimension by
        (range)               a 0-origin dimension 1+(stop-start)/step
    (5) rf:start:stop:step  - as (4), but the specified range function
                              is applied after gathering the result
    (6) -  or -:start:stop:step  - "pseudo index" adds a dimension to
                                    the result not in the original array
    (7) +  or +:start:stop:step   - "marked index" is a special form for
                                    matrix multiplication, pushes the
                                    result index number onto the stack
    (8) ..    - "rubber index" causes any subsequent indices to be right
                justified, copying as many dimensions to the result as
                required in order to do this
    (9) where(0)   - "nuller index" causes result to be nil, a special
                     case of the index list for a zero length list

    Index values may fall into 3 ranges.  For a 0-origin dimension of
    length n, these ranges are:
      (A) wrap around [-n, -1], which are mapped to [0, n-1] by adding n.
      (B) ordinary [0, n-1]
      (C) overreach [n, infinity], which effectively specify offsets in
              subsequent dimensions.  No index may overreach beyond the
              end of the entire array.
    All 3 ranges are supported for Range indices and scalar indices, but
    wrap around indexing is not supported for index lists.

    Note that a Range index or a rank-preserving range function (see
    range.c) always produces a 0-origin dimension in the result.
    A nil index or rubber index produces result dimension(s) with the
    same origin as the input array, and an index list produces result
    dimensions with origins copied from the index list dimensions.
 */

#include "ydata.h"

extern VMaction Eval2;
extern UnaryOp EvalAY, EvalLV;

extern Operand *FormOperandDB(Symbol *owner, Operand *op);

#define MAX_INDICES (Y_DIMSIZE-1)

/*
   For several types of DataBlock, the Eval operation means to index
   into an array.  This operation proceeds in several steps:

   1. ArrayDims
      Count the dummy indices, accumulating the corresponding strides.
      The result of this operation is the static values:
          nDummy       - number of dimensions in dummy dimension list
          dims[]       - Dimension* for each dimension

   2. EvalSetup
      Scan through the actual indices to set:
          overreach   - 1 if any actual index beyond range of
                        corresponding dummy index
          overOrigin  - maximum origin of current actual index due to
                        overreach from previous actual indices
          offsets[]   - offset from beginning for each actual index
                        for rubber index, number of actual indices after,
                        for index list, -origin (so index values correct)
          numbers[]   - number of elements for each actual index
          steps[]     - index increments for each actual index
          rangeFlags[] - range->nilFlags for each actual index
          indexLists[] - index list for each actual index
          Builder[]   - function to build result is one of
             BuildScalar  - long scalar (offsets)
             BuildList    - long array (offsets, indexLists)
             BuildRange   - min:max:inc  or  rf:min:max:inc
                            (offsets, numbers, steps, rangeFlags)
             The arrays actually used by each Builder are shown in
             parentheses.  BuildRange also uses strides from step 1.

          rsltIndex    - index in result array for current actual index
          nRF          - number of range functions encountered
          rfs[]        - nRF range function pointers
          rfIndices[]  - index in result array for each range function
          markedIndex  - index in result array for marked index, or -1

          tmpDims      - Dimension list for result Array or LValue

      After all actual indices have been processed, EvalSetup checks
      for any overreach errors.

   3. ArrayStrides (called from BuildResult) or LValueStrides
      Compute the stride associated with each dummy index
          strides[]    - stride (in bytes) for each dimension

   4. loop on BuildScalar, BuildList, BuildRange (from BuildResult or EvalLV)
      Process the indices from fastest to slowest varying.  The resulting
      LValue is built on the top of the stack by successive calls to the
      Builder routines set in step 2.

   5. DoRangeFuncs (called from BuildResult)
      Apply any range functions.  If the LValue produced in (3) is
      disk data, it is gathered before further processing.

   6. (end of BuildResult)
      Push number of marked index onto stack after result, if any.
      This is a special form used by the matrix multiply routines.
 */

extern void FormEvalOp(int nArgs, Operand *obj);
extern DataBlock *ForceToDB(Symbol *s);

static int EvalSetup(Symbol *stack, int n);
static void SetupScalar(long index);
static void DotdotScan(Symbol *stack, int singleIndex);
static void ArrayDims(Dimension *adims);
static void ArrayStrides(long stride);
static int LValueStrides(Strider *strider);
static void DoRangeFuncs(Array *array);
static void BuildResult(char *mem, StructDef *base,
                        Array *array, Symbol *stack, int n);

/*--------------------------------------------------------------------------*/

/* current dummy and actual indices */
static int iDummy, iActual;

/* Characteristics of dummy indices (MAX_INDICES==nDummy) */
static int nDummy;
static Dimension *dims[MAX_INDICES];     /* dummy dimensions, fastest 1st */
static long strides[MAX_INDICES];        /* strides for dummy dimensions */

/* Characteristics of actual indices (MAX_INDICES==nActual) */
static int overreach;
static long overOrigin;                  /* maximum origin if overreach */
static long offsets[MAX_INDICES];        /* offsets for actual indices
                                            used by scalars, ranges -- also
                                            index count for rubber index */
static long numbers[MAX_INDICES];        /* numbers for actual indices */
static long steps[MAX_INDICES];          /* ::steps for actual indices */
static int rangeFlags[MAX_INDICES];      /* nilFlags for actual indices */
static Array *indexLists[MAX_INDICES];   /* index lists for actual indices */
static long (*Builder[MAX_INDICES])(LValue *);
                                         /* routine to build result */

/* Characteristics of result */
static int rsltIndex, markedIndex, nRF, iRubber;
  /* in following arrays (MAX_INDICES==nRF) */
static RangeFunc *rfs[MAX_INDICES];      /* range functions */
static int rfIndices[MAX_INDICES];       /* index # for rfs */

/* possible functions for Builder */
static long BuildScalar(LValue *result);
static long BuildList(LValue *result);
static long BuildRange(LValue *result);

/* set up function tables for initializing Builder and bParam arrays */

extern UnaryOp SetupC, SetupS, SetupI, SetupL, SetupX, SetupR, SetupVD;
extern UnaryOp ToLongC, ToLongS, ToLongI;

/*--------------------------------------------------------------------------*/

static int EvalSetup(Symbol *stack, int n)
{
  Operand index;
  Dimension *tmp= tmpDims;
  if (n>MAX_INDICES) YError("too many array indices");

  /* Note: There is no point in freeing tmpDims until here, where it
     is needed again.  Until now, tmpDims is simply an extra reference
     to the Dimension* created in the previous Eval operation.  The
     amount of storage associated with tmpDims is miniscule, so even
     if this is the last remaining reference, the fact that it wasn't
     freed as soon as possible is unimportant.  */
  tmpDims= 0;
  FreeDimension(tmp);

  overreach= rsltIndex= nRF= 0;
  markedIndex= iRubber= -1;
  overOrigin= 0;

  /* sort through all of the actual indices, setting up Builder routines */
  iDummy= 0;
  for (iActual=0 ; iActual<n ; iActual++) {
    stack++;
    if (!stack->ops) YError("array index cannot be keyword");
    stack->ops->FormOperand(stack, &index);
    index.ops->Setup(&index);
    if (iDummy<0) { /* nullifier, e.g.- where(0) */
      Drop(n+1);
      PushDataBlock(RefNC(&nilDB));
      return 1;
    }
  }
  if (iDummy<nDummy && n>0) {
    if (index.ops==&voidOps) {
      /* pick up all remaining dummy indices, just as if final index
         had been .. instead of nil */
      long number= numbers[--iActual];
      long na= offsets[iActual];
      DotdotScan(stack, 0);
      rangeFlags[iActual]|= R_RUBBER;
      numbers[iActual]*= number;
      offsets[iActual++]+= na;
    } /* else, should check that there wasn't a nil index followed by
         one or more pseudo index -- however, this error is rather
         expensive to detect, and I'm omitting the check for now.  */
  }

  /* make sure any references beyond index bounds are kosher */
  if (overOrigin) {
    while (overOrigin && iDummy<nDummy) overOrigin/= dims[iDummy++]->number;
    if (overOrigin) YError("index overreach beyond array bounds");
  }

  return 0;
}

/*--------------------------------------------------------------------------*/
/* Three distinct DataBlock types for Index operation:
   longType, rangeType, and voidType.
   char, short, and int also legal, as is LValue.
   These routines set up the LValue Builder routines.  */

static void SetupScalar(long index)
{
  long number;
  if (iDummy<nDummy) {
    number= dims[iDummy]->number;
    index-= yForceOrigin? 1L : dims[iDummy]->origin;
  } else {
    number= 1;
    index-= 1L;
  }

  if (index<0 && (index+=number)<0) {    /* notice behavior when index<0 */
    YError("array index is too small");
  } else if (index+overOrigin>=number) {
    overreach= 1;
    overOrigin+= index;
  }
  if (overOrigin) overOrigin/= number;  /* origin for next index */

  Builder[iActual]= &BuildScalar;
  offsets[iActual]= index;

  iDummy++;
}

void SetupL(Operand *op)
{
  /* Scalar is simple index, dimensioned array is index list */
  if (!op->type.dims) {
    SetupScalar(((long *)op->value)[0]);
    return;

  } else {            /* index list */
    long i, n= op->type.number;
    long *il= op->value;
    long ilMin, ilMax;
    long number= dims[iDummy]->number;
    long origin= yForceOrigin? 1L : dims[iDummy]->origin;

    if (iDummy>=nDummy) YError("index list beyond final array index");

    Builder[iActual]= &BuildList;
    /* Since dims is non-0, we know that the op->owner is a dataBlockSym
       which points to an Array (any LValue has been fetched).  */
    indexLists[iActual]= (Array *)op->owner->value.db;
    if (indexLists[iActual]->ops==&lvalueOps) {
      indexLists[iActual]= FetchLValue(indexLists[iActual], op->owner);
      il= indexLists[iActual]->value.l;
    }
    offsets[iActual]= -origin;  /* compensate for non-0 origin */
    iDummy++;

    /* find minimum and maximum index list values */
    ilMin= ilMax= il[0];
    for (i=1 ; i<n ; i++) {
      if (ilMin > il[i]) ilMin= il[i];
      else if (ilMax < il[i]) ilMax= il[i];
    }
    if (ilMin<origin) {
      YError("minimum array index in index list is too small");
    } else if (ilMax-origin+overOrigin>=number) {
      overreach= 1;
      overOrigin+= ilMax-origin;
    }
    if (overOrigin) overOrigin/= number;

    /* copy dimension list from index list to tmpDims (copy origins) */
    if (tmpDims) tmpDims= CopyDims(op->type.dims, tmpDims, 1);
    else tmpDims= Ref(op->type.dims);

    /* increment count of result indices */
    rsltIndex+= CountDims(op->type.dims);
    return;
  }
}

void SetupR(Operand *op)
{
  /* Range functions specify several cases:
     -:   this is not an actual index
     ..:  expand to fill remaining indices
     +:   mark this index  */
  Range *range= op->value;
  int nilFlags= range->nilFlags;
  long offset, origin, number, step, largest, n;

  /* Return iDummy==-1 to force void result before any Builders called.  */
  if (nilFlags & R_NULLER) { iDummy= -1;  return; }

  Builder[iActual]= &BuildRange;
  rangeFlags[iActual]= nilFlags;

  if (nilFlags & R_RUBBER) {
    DotdotScan(op->owner, nilFlags&R_PSEUDO);
    return;
  }

  /* set origin and n for dummy index corresponding to this actual index */
  step= range->inc;
  if (nilFlags & R_PSEUDO) {
    origin= step>0? range->min : range->max;
    n= 1;
    /* iDummy NOT incremented on this branch */
  } else {
    if (iDummy>=nDummy) YError("index range beyond final array index");
    origin= yForceOrigin? 1L : dims[iDummy]->origin;
    n= dims[iDummy]->number;
    if (nilFlags & R_MARKED) markedIndex= rsltIndex;
    iDummy++;
  }

  /* get min:max:inc and compensate for dummy origin */
  if (nilFlags&R_MINNIL) offset= step>0? 0 : n-1;
  else offset= range->min - origin;
  if (nilFlags&R_MAXNIL) number= step>0? n-1 : 0;
  else number= range->max - origin;

  /* perform negative index wrap around if necessary */
  if (offset<0 && (offset+=n)<0) {
    YError("array index range start is too small");
  } else if (number<0 && (number+=n)<0) {
    YError("array index range stop is too small");
  }

  /* compute the number of footprints (#strides+1) */
  if (step>=0 && number>=offset) {
    if (step>1) {
      largest= (number-offset)/step;
      number= largest+1;
      largest= offset + largest*step;
    } else {
      largest= number;
      number= number-offset+1;
    }
  } else if (step<0 && offset>=number) {
    if (step<-1) number= (offset-number)/(-step) + 1;
    else number= offset-number + 1;
    largest= offset;
  } else {
    YError("array index range step has wrong sign");
    largest= 0;
  }

  /* detect and handle overreach */
  if (largest+overOrigin>=n && (nilFlags&R_PSEUDO)==0) {
    overreach= 1;
    overOrigin+= largest;
  }
  if (overOrigin) overOrigin/= n;

  offsets[iActual]= offset;
  numbers[iActual]= number;
  steps[iActual]= step;

  tmpDims= NewDimension(number, 1L, tmpDims);  /* default origin */

  if (range->rf) {
    rfs[nRF]= range->rf;
    rfIndices[nRF++]= rsltIndex;
  }
  rsltIndex++;
}

static void DotdotScan(Symbol *stack, int singleIndex)
{
  /* Scan through remaining actual indices to see how many dummy
     indices to skip.  We only need to detect pseudo-indices, since
     these are the only type which break the one-one correspondence
     between actual and dummy indices.  */
  int na= 0;                    /* # remaining non-pseudo actuals */
  long number, n;

  while (stack<sp) {
    stack++;
    if (stack->ops==&referenceSym) ReplaceRef(stack);
    if (stack->ops!=&dataBlockSym || stack->value.db->ops!=&rangeOps) {
      na++;
    } else {
      int nilFlags= ((Range *)stack->value.db)->nilFlags;
      if (nilFlags & R_RUBBER)
        YError("multiple rubber (.. or *) indices in one index list");
      if ((nilFlags & R_PSEUDO)==0) na++;
    }
  }

  if (singleIndex && (iDummy<nDummy-na-1)) iRubber= iActual;
  offsets[iActual]= na;         /* offset is actually 0, intentionally
                                   misused here and in BuildRange */
  number= 1;
  while (iDummy < nDummy-na) {
    n= dims[iDummy]->number;
    if (overOrigin) overOrigin= (overOrigin+n-1)/n;
    number*= n;
    /* copy origin here */
    if (!singleIndex)
      tmpDims= NewDimension(n, dims[iDummy]->origin, tmpDims);
    rsltIndex++;
    iDummy++;
  }
  if (singleIndex) tmpDims= NewDimension(number, 1L, tmpDims);
  numbers[iActual]= number;
  steps[iActual]= 1;
  return;
}

void SetupC(Operand *op)
{
  if (!op->type.dims) {
    SetupScalar((long)((char *)op->value)[0]);
  } else {
    ToLongC(op);
    SetupL(FormOperandDB(op->owner, op));
  }
}

void SetupS(Operand *op)
{
  if (!op->type.dims) {
    SetupScalar((long)((short *)op->value)[0]);
  } else {
    ToLongS(op);
    SetupL(FormOperandDB(op->owner, op));
  }
}

void SetupI(Operand *op)
{
  if (!op->type.dims) {
    SetupScalar((long)((int *)op->value)[0]);
  } else {
    ToLongI(op);
    SetupL(FormOperandDB(op->owner, op));
  }
}

/* ARGSUSED */
void SetupVD(Operand *op)
{
  long n= iDummy<nDummy? dims[iDummy]->number : 1L;
  Builder[iActual]= &BuildRange;
  rangeFlags[iActual]= 0;
  offsets[iActual]= 0;
  numbers[iActual]= n;
  steps[iActual]= 1;
  if (overOrigin) overOrigin= (overOrigin+n-1)/n;
  if (iDummy<nDummy) {
    tmpDims= NewDimension(n, dims[iDummy]->origin, tmpDims);
    rsltIndex++;
  }
  iDummy++;
}

/* ARGSUSED */
void SetupX(Operand *op)
{
  YError("bad data type for array index");
}

/*--------------------------------------------------------------------------*/
/* ArrayDims initializes dims[] and nDummy, ArrayStrides and LValueStrides
   initialize strides[].  */

static void ArrayDims(Dimension *adims)
{ /* ArrayDims(lvalue->type.dims); */
  if (!adims) {                  /* halt recursion */
    nDummy= 0;
    return;
  }
  ArrayDims(adims->next);              /* recurse */
  if (nDummy>=MAX_INDICES) YError("too many array dimensions");
  dims[nDummy++]= adims;
}

static void ArrayStrides(long stride)
{ /* ArrayDims(array->type.dims);  ArrayStrides(array->type.base->size); */
  int i;
  for (i=0 ; i<nDummy ; i++) {
    strides[i]= stride;
    stride*= dims[i]->number;
  }
}

static int LValueStrides(Strider *strider)
{ /* ArrayDims(lvalue->type.dims);  LValueStrides(lvalue->strider); */
  int iDummy;
  if (!strider) {                    /* halt recursion */
    iDummy= 0;

  } else {
    long n, number= strider->number;
    long stride= strider->stride;

    iDummy= LValueStrides(strider->next);  /* recurse */
    if (iDummy<0 || strider->indexList) return -1;

    if (iDummy<nDummy) {
      strides[iDummy]= stride;
      n= dims[iDummy++]->number;
      while (n<number) {
        strides[iDummy]= stride*n;
        n*= dims[iDummy++]->number;
      }
    }
  }
  return iDummy;
}

/*--------------------------------------------------------------------------*/
/* The Builder routines actually build up the result LValue.  */

static long BuildScalar(LValue *result)
{
  long offset= offsets[iActual] * strides[iDummy];
  iDummy++;
  return offset;
}

static long BuildRange(LValue *result)
{
  int nilFlags= rangeFlags[iActual];
  long offset, stride, number= numbers[iActual];

  /* Compute appropriate stride, offset, and increment iDummy */
  if (nilFlags&R_RUBBER) {   /* this is .. index (right justify remaining) */
    int na= offsets[iActual];       /* # remaining non-pseudo actuals */
    if (iDummy >= nDummy-na) return 0;
    if (number>1 && (iDummy<nDummy-na-1) && !(nilFlags&R_PSEUDO)) {
      /* a .. rubber index may need several striders */
      Strider *strider= result->strider;
      long n= 1, total= number;
      for (; iDummy<nDummy-na ; iDummy++) {
        if (n<total) {
          stride= strides[iDummy];
          number= dims[iDummy]->number;
          if (number>1) {  /* (See comments below.) */
            if (strider) {
              if (!strider->indexList &&
                  strider->stride*strider->number == stride) {
                strider->number*= number;
              } else {
                result->strider= NewStrider(stride, number);
                result->strider->next= strider;
                strider= result->strider;
              }
            } else {
              result->strider= strider= NewStrider(stride, number);
            }
            n*= number;
          }
        }
      }
      return 0;

    } else {
      stride= strides[iDummy];        /* steps[iActual] always 1 */
      offset= 0;
      iDummy= nDummy-na;
    }

  } else {
    if (nilFlags&R_PSEUDO) { /* this is - index (insert new dimension) */
      stride= offset= 0;

    } else {                 /* this is ordinary rf:min:max:inc */
      stride= steps[iActual] * strides[iDummy];
      offset= offsets[iActual] * strides[iDummy];
      iDummy++;
    }
  }

  /* Add or append to result->strider */
  if (number>1) {
    if (result->strider) {
      Strider *strider= result->strider;
      if (!strider->indexList &&
          strider->stride*strider->number == stride) {
        /* If the current strider is not an index list, and its total
           span (stride*number) is the same as the current stride, then
           we can simply increment its number to include the current
           dimension.  */
        strider->number*= number;
      } else {
        /* Otherwise, we need a new strider.  */
        result->strider= NewStrider(stride, number);
        result->strider->next= strider;
      }

    } else {
      result->strider= NewStrider(stride, number);
    }
  }

  return offset;
}

static long BuildList(LValue *result)
{
  Array *array= indexLists[iActual];
  long offset= offsets[iActual] * strides[iDummy];
  Strider *newStrider, *strider= result->strider;

  /* Add index list strider to result->strider */
  newStrider=
    result->strider= NewStrider(strides[iDummy], array->type.number);
  newStrider->next= strider;
  newStrider->indexList= Ref(array);

  iDummy++;
  return offset;
}

/*--------------------------------------------------------------------------*/

static void DoRangeFuncs(Array *array)
{
  /* Range functions are performed in left-to-right order, that is,
     from the fastest varying index to the slowest.  */
  int i, j;
  for (i=0 ; i<nRF ; i++) {
    if (rfs[i](array, rfIndices[i])) {
      /* This was a rank reducing operation, adjust the index numbering
         for any remaining range functions.  */
      for (j=i+1 ; j<nRF ; j++) rfIndices[j]--;
      /* Note: parser responsible for ensuring that range->rf==0
         whenever range->nilFlags&R_MARKED */
      if (markedIndex>i) markedIndex--;
    }
    /* range function left result on top of stack */
    sp--;
    sp->value= (sp+1)->value;
    sp->ops= (sp+1)->ops;
    Unref(array);
    array= (Array *)sp->value.db;
  }
}

static void BuildResult(char *mem, StructDef *base,
                        Array *array, Symbol *stack, int n)
{
  LValue *result= PushDataBlock(NewLValueM(array, mem, base, tmpDims));

  /* build an appropriate LValue on top of stack */
  ArrayStrides(base->size);
  iDummy= 0;
  for (iActual=0 ; iActual<n ; iActual++) mem+= Builder[iActual](result);
  result->address.m= mem;

  /* swap result LValue with array on stack */
  PopTo(stack);     /* note that result owns use of array */

  /* discard actual indices */
  Drop(n);

  /* If range functions were specified, do them now.
     If the original array is a temporary, or if an index is marked,
     may as well fetch the result LValue now, and leave an Array on
     the stack instead.  Non-temporaries should be left as they are,
     since they might be destined for further indexing.  */
  if (nRF)
    DoRangeFuncs(FetchLValue(result, stack));
  else if (array->references==0 || markedIndex>=0)
    FetchLValue(result, stack);
}

/*--------------------------------------------------------------------------*/

void FormEvalOp(int nArgs, Operand *obj)
{
  Symbol *s= sp-nArgs;
  DataBlock *db= (s->ops==&dataBlockSym)? s->value.db : ForceToDB(s);
  obj->owner= s;
  obj->references= nArgs;   /* intentionally misused */
  obj->ops= db->ops;
  obj->value= db;
}

void Eval2(void)
{
  int nArgs= (pc++)->count;
  Operand obj;
  FormEvalOp(nArgs, &obj);

  /* guard against x(+)*y(+) where x or y is a function */
  if (!obj.ops->isArray && obj.ops!=&lvalueOps)
    YError("matrix multiply index marker (+) not in array index list");

  obj.ops->Eval(&obj);
  /* One index MUST be marked; Eval2 leaves 2 elements on the top of the
     stack-- the Array, and an intScalar to tell which index is marked.
     The parser guarantees that markedIndex>0 here, but only here.  */
  PushIntValue(markedIndex);
}

void EvalAY(Operand *op)
{
  Symbol *stack= op->owner;
  int nArgs= op->references;    /* interpret misuse in FormEvalOp */
  Array *array= op->value;

  /* initialize nDummy, dims, strides */
  ArrayDims(array->type.dims);

  /* initialize Builder, offsets, numbers, steps, indexLists, rfs, nRF */
  if (EvalSetup(stack, nArgs)) return;  /* nullifier index */

  BuildResult(array->value.c, array->type.base, array, stack, nArgs);
}

void EvalLV(Operand *op)
{
  Symbol *stack= op->owner;
  int nArgs= op->references;    /* interpret misuse in FormEvalOp */
  LValue *lvalue= op->value;
  Strider *strider= lvalue->strider;
  StructDef *base= lvalue->type.base;
  int addressType= base->addressType;

  /* initialize nDummy, dims */
  ArrayDims(lvalue->type.dims);

  /* initialize Builder, offsets, numbers, steps, indexLists, rfs, nRF */
  if (EvalSetup(stack, nArgs)) return;  /* nullifier index */

  if (nRF || markedIndex>=0 || (overreach&&strider) ||
      LValueStrides(strider)<0 || addressType>1 || iRubber>=0) {
    /* Must gather the original LValue into a compact Array if any of
       the following conditions are met:
       (1) The actual indices include range functions (the range
           function will need the data immediately).
       (2) There is a marked index (the matrix multiply operation
           will need the data)
       (3) Any actual index overreached its corresponding dummy index,
           and the LValue has strides (the overreached elements will
           not be at the expected address)
       (4) The original LValue includes an index list (some of the
           actual indices may require extracting a subset of the index
           list, rather than a subset of the data array)
       (5) The original LValue references a sequential object in a
           disk file.
       (6) A rubber * index contracts two or more dimensions into one
       As soon as one of these conditions is met, gather the LValue and
       proceed as for an Array.  */
    Array *dst= FetchLValue(lvalue, stack);

    /* The new Array is guaranteed to have simple strides, but nDummy,
       dims, Builder, numbers, offsets, steps, etc. have not changed.  */
    BuildResult(dst->value.c, dst->type.base, dst, stack, nArgs);

  } else {
    /* The input LValue is described by a simple hierarchy of strides.
       The data itself need never be touched to build the result LValue.  */
    long totalOffset;
    LValue *result;

    result= PushDataBlock(addressType?
                          NewLValueD(lvalue->address.d, base, tmpDims) :
                          NewLValueM(lvalue->owner, lvalue->address.m,
                                     base, tmpDims));

    if (!strider) ArrayStrides(base->size);
    /* build an appropriate LValue on top of stack */
    totalOffset= 0;
    iDummy= 0;
    for (iActual=0 ; iActual<nArgs ; iActual++)
      totalOffset+= Builder[iActual](result);
    if (addressType) result->address.d+= totalOffset;
    else result->address.m+= totalOffset;

    /* swap result LValue with array on stack */
    PopTo(stack);

    /* discard actual indices, leaving result on top of stack */
    Drop(nArgs);
  }
}

/*--------------------------------------------------------------------------*/
