/*
 * $Id: ops3.c,v 1.2 2010-02-28 21:52:29 dhmunro Exp $
 * Implement assignment operations:
 *
 *  Assign =   Increment += (and ++)   Decrement -= (and --)
 *
 *  Also, implement struct definition and various Eval operations.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "bcast.h"
#include "pstdlib.h"
#include "play.h"

/*--------------------------------------------------------------------------*/

extern VMaction Assign, Define;

extern BinaryOp AssignC, AssignS, AssignI, AssignL,
                AssignF, AssignD, AssignZ, AssignQ, AssignP,
                AssignSI, AssignX;

extern VMaction OpenStruct, DeclareMember, CloseStruct;
extern VMaction Eval, Deref, Address;
extern VMaction GetMember, DerefMember;

extern MemberOp GetMemberAY, GetMemberLV, GetMemberIO, GetMemberX;

extern BuiltIn Y_get_member;

extern void FormEvalOp(int nArgs, Operand *obj);

extern void BuildDimList(Symbol *stack, int nArgs);  /* ops3.c */

/*--------------------------------------------------------------------------*/

extern Operand *FormOperandIS(Symbol *owner, Operand *op);
extern Operand *FormOperandLS(Symbol *owner, Operand *op);
extern Operand *FormOperandDS(Symbol *owner, Operand *op);
extern Operand *FormOperandDB(Symbol *owner, Operand *op);

extern void PopToI(Symbol *s);
extern void PopToL(Symbol *s);
extern void PopToD(Symbol *s);

static void DoAssign(LValue *lvalue, Symbol *rowner);

extern DataBlock *ForceToDB(Symbol *s);
static char *GetMemberName(void *db);

/*--------------------------------------------------------------------------*/
/* Assign */

/* Assignment syntax "x(index_list)= expression" assures that an LValue
   is under the expression at the top of the stack.  */

static void DoAssign(LValue *lvalue, Symbol *rowner)
{
  StructDef *base= lvalue->type.base;
  Operations *ops= base->dataOps;
  Operand rhs;

  /* rhs is ordinary operand */
  rowner->ops->FormOperand(sp, &rhs);

  /* for assignment, only makes sense to broadcast rhs */
  if (rhs.ops->isArray && RightConform(lvalue->type.dims, &rhs))
    YError("rhs not conformable with lhs in assign =");

  /* Assign virtual functions assume their first parameter is an LValue*
     rather than an Operation* (like all other BinaryOps).  */
  ops->Assign((Operand *)lvalue, &rhs);
}

void Assign(void)
{
  Symbol *spl= sp-1;
  LValue *lvalue= (LValue *)spl->value.db;

  P_SOFTFPE_TEST;

  /* guard against x(...)= expr, where x turns out to be a function */
  if (spl->ops!=&dataBlockSym || lvalue->ops!=&lvalueOps)
    YError("LHS of assignment not an l-value (scalar double, long, int?)");

  DoAssign(lvalue, sp);
  PopTo(spl);     /* leaves (broadcast and retyped) rhs on top of stack */
}

#undef OPERATION
#define OPERATION(opname, typeD, ToType) \
void opname(Operand *l, Operand *r) \
{ if (r->ops!=&typeD) r->ops->ToType(r); StoreLValue(l, r->value); }
/* Note-- parameter l is actually a cast LValue* in DoAssign */

OPERATION(AssignC, charOps, ToChar)
OPERATION(AssignS, shortOps, ToShort)
OPERATION(AssignI, intOps, ToInt)
OPERATION(AssignL, longOps, ToLong)
OPERATION(AssignF, floatOps, ToFloat)
OPERATION(AssignD, doubleOps, ToDouble)
OPERATION(AssignZ, complexOps, ToComplex)

static void AssignError(void);
static void AssignError(void)
{ YError("cannot convert rhs of assign = to pointer or string"); }

#undef OPERATION
#define OPERATION(opname, typeD) \
void opname(Operand *l, Operand *r) \
{ if (r->ops!=&typeD) AssignError(); StoreLValue(l, r->value); }
/* Note-- parameter l is actually a cast LValue* in DoAssign */

OPERATION(AssignQ, stringOps)
OPERATION(AssignP, pointerOps)

void AssignSI(Operand *l, Operand *r)
{
  StructDef *base = l->type.base;
  while (base->model) base = base->model;
  if (r->ops->typeID>T_STRUCT || !StructEqual(base, r->type.base))
    YError("rhs struct not equivalent to lhs struct in assign =");
  StoreLValue(l, r->value);
}
/* Note-- parameter l is actually a cast LValue* in DoAssign */

void AssignX(Operand *l, Operand *r)
{ YError("(BUG) impossible lvalue->type.base in assign ="); }

/*--------------------------------------------------------------------------*/
/* Define */

/* Definition syntax "x= expression".
   If the referenceSym points to an LValue, Define calls DoAssign.
   Otherwise, Define replaces the referenced globTab entry with the
   expression, which remains on the stack.  */

void Define(void)
{
  Symbol *glob= &globTab[(pc++)->index];
  P_SOFTFPE_TEST;
  if (glob->ops==&dataBlockSym) {
    DataBlock *db= glob->value.db;
    if (db->ops==&lvalueOps) {
      /* Note that an explicit UnDefine operation is required in order
         for the interpreter to be able to ever get rid of a reference
         to an LValue in globTab.  This is provided as a part of a
         non-kernal Yorick package, since it is not a crucial feature
         of the language (see Y_reshape).  */
      DoAssign((LValue *)db, sp);
      return;
    } else {
      glob->ops= &intScalar;
      Unref(db);
    }
  }
  if (sp->ops==&dataBlockSym) {
    Array *array= (Array *)sp->value.db;
    if (array->references && array->ops->isArray) {
      /* copy non-temporary arrays to avoid unexpected aliasing */
      Array *result= NewArray(array->type.base, array->type.dims);
      glob->value.db= (DataBlock *)result;
      array->type.base->Copy(array->type.base, result->value.c,
                             array->value.c, array->type.number);
    } else {
      if (array->ops==&lvalueOps) FetchLValue(array, sp);
      glob->value.db= Ref(sp->value.db);
    }
  } else {
    glob->value= sp->value;
  }
  glob->ops= sp->ops;
}

/*--------------------------------------------------------------------------*/

void OpenStruct(void)
{
  long index= (pc++)->index;
  StructDef *base= AddStruct((IOStream *)0, globalTable.names[index], 0L);
  DataBlock *db= 0;
  if (!base) YError("(BUG?) unable to create struct for some reason");
  PushDataBlock(base);
  /* zap struct-building function */
  if (globTab[index].ops==&dataBlockSym) db= globTab[index].value.db;
  globTab[index].value.db= RefNC(&nilDB);
  globTab[index].ops= &dataBlockSym;
  Unref(db);
}

void CloseStruct(void)
{
  StructDef *base= (StructDef *)sp->value.db;
  InstallStruct(base, (StructDef *)0);
  Drop(1);
}

void BuildDimList(Symbol *stack, int nArgs)
{
  Dimension *tmp= tmpDims;
  tmpDims= 0;
  FreeDimension(tmp);

  while (nArgs--) {
    if (stack->ops==&referenceSym) ReplaceRef(stack);
    if (stack->ops==&longScalar) {
      if (stack->value.l<=0) goto badl;
      tmpDims= NewDimension(stack->value.l, 1L, tmpDims);
    } else if (stack->ops==&intScalar) {
      if (stack->value.i<=0) goto badl;
      tmpDims= NewDimension(stack->value.i, 1L, tmpDims);

    } else if (stack->ops==&dataBlockSym) {
      Operand op;
      FormOperandDB(stack, &op);
      if (op.ops==&rangeOps) {
        Range *range= op.value;
        long len;
        if (range->rf || range->nilFlags || range->inc!=1)
          YError("only min:max ranges allowed in dimension list");
        len= range->max-range->min+1;
        if (len<=0) goto badl;
        tmpDims= NewDimension(len, range->min, tmpDims);

      } else if (op.ops->promoteID<=T_LONG &&
                 (!op.type.dims || !op.type.dims->next)) {
        long len;
        op.ops->ToLong(&op);
        if (!op.type.dims) {
          len= *(long *)op.value;
          if (len<=0) goto badl;
          tmpDims= NewDimension(len, 1L, tmpDims);
        } else {
          long *dim= op.value;
          long n= *dim++;
          if (n>10 || n>=op.type.number)
            YError("dimension list format [#dims, len1, len2, ...]");
          while (n--) {
            len= *dim++;
            if (len<=0) goto badl;
            tmpDims= NewDimension(len, 1L, tmpDims);
          }
        }

      } else if (op.ops!=&voidOps) {
        goto badl;
      }
    } else {
    badl:
      YError("bad dimension list");
    }
    stack++;
  }
}

void DeclareMember(void)
{
  /* struct_def, member_name, dimlist, type */
  int nDims= (pc++)->count;
  StructDef *base, *memType= (StructDef *)sp->value.db;
  Symbol *stack= sp-nDims;   /* first dimension in dimlist */
  Array *name;

  /* get data type off top of stack */
  if (sp->ops!=&dataBlockSym || memType->ops!=&structDefOps)
    YError("invalid member data type in struct definition");

  /* build tmpDims from next nDims stack elements (under data type) */
  BuildDimList(stack, nDims);

  /* get member name from under dimlist-- guaranteed a non-0 string
     by the parser */
  stack--;
  name= (Array *)stack->value.db;

  /* get struct itself from under member name-- guaranteed by the parser */
  stack--;
  base= (StructDef *)stack->value.db;

  /* add name to hash table for this struct */
  if (AddMember(base, -1L, name->value.q[0], memType, tmpDims))
    YError("duplicate member name in struct definition");

  /* clean stack,
     leaving struct itself for next DeclareMember or CloseStruct */
  Drop(nDims+2);
}

/*--------------------------------------------------------------------------*/

int StructEqual(StructDef *l, StructDef *r)
{
  long i, nItems;

  /* return quickly if same */
  if (l==r) return 1;

  /* if number of items, size, or alignment differs, structs are not equal */
  nItems= l->table.nItems;
  if (nItems!=r->table.nItems || l->size!=r->size ||
      l->alignment!=r->alignment ||
      (nItems==0 && (l->order!=r->order || l->fpLayout!=r->fpLayout)))
    return 0;

  /* otherwise, the structs are equal if and only if all offsets are
     the same and all member types are equal */
  for (i=0 ; i<nItems ; i++)
    if (l->offsets[i]!=r->offsets[i] ||
        !StructEqual(l->members[i].base, r->members[i].base)) return 0;
  return 1;
}

/*--------------------------------------------------------------------------*/

DataBlock *ForceToDB(Symbol *s)
{
  OpTable *ops= s->ops;
  if (ops==&referenceSym) { ReplaceRef(s); ops= s->ops; }
  if (ops==&doubleScalar) {
    Array *array= NewArray(&doubleStruct, (Dimension *)0);
    array->value.d[0]= s->value.d;
    s->value.db= (DataBlock *)array;
    s->ops= &dataBlockSym;
    return (DataBlock *)array;
  } else if (ops==&longScalar) {
    Array *array= NewArray(&longStruct, (Dimension *)0);
    array->value.l[0]= s->value.l;
    s->value.db= (DataBlock *)array;
    s->ops= &dataBlockSym;
    return (DataBlock *)array;
  } else if (ops==&intScalar) {
    Array *array= NewArray(&intStruct, (Dimension *)0);
    array->value.i[0]= s->value.i;
    s->value.db= (DataBlock *)array;
    s->ops= &dataBlockSym;
    return (DataBlock *)array;
  } else if (ops==&dataBlockSym) {
    return s->value.db;
  } else {
    YError("(BUG) bad Symbol type in ForceToDB");
  }
  return 0;
}

void Address(void)
{
  Array *pointer, *array;
  void *value= 0;

  /* The & operator can only return the address of an Array.  */
  if (sp->ops==&referenceSym) {
    /* Taking the address of a variable x, where x is a scalar constant,
       causes x to be replaced by an Array.  This is obscure, but there
       is no other obvious way to get both the efficiency of the scalar
       Symbols, AND the reference-count safety of Yorick pointers.  Notice
       that if the address of a scalar is taken, the efficient
       representation is lost.  */
    Symbol *glob= &globTab[sp->index];
    if (glob->ops==&dataBlockSym) array= (Array *)glob->value.db;
    else array= (Array *)ForceToDB(glob);
    ReplaceRef(sp);
  } else {
    /* The address of a Yorick temporary actually makes more sense than
       the address of a scalar variable.  The semantics of Yorick pointers
       are really dramatically different than C pointers...  Sigh.  */
    if (sp->ops==&dataBlockSym) array= (Array *)sp->value.db;
    else array= (Array *)ForceToDB(sp);
  }

  if (array->ops->isArray) {
    value= array->value.c;
  } else if (array->ops==&lvalueOps) {
    /* Yes, an LValue address is always the address of temporary data.  */
    array= FetchLValue(array, sp);
    value= array->value.c;
  } else if (array->ops==&voidOps) {
    /* Since dereferencing 0 gives nil, the address of nil is 0.  */
    value= 0;
  } else {
    YError("no address (&) for non-array object");
  }

  pointer= NewArray(&pointerStruct, (Dimension *)0);
  /* array->references remains the same-- the owner of array merely
     switches from sp->value.db to the pointer value
     EXCEPT for nil, which loses a reference... */
  pointer->value.p[0]= value;
  sp->value.db= (DataBlock *)pointer;
  if (!value) UnrefNC(&nilDB);  /* ...since sp->value.db was &nilDB */
}

void Deref(void)
{
  Array *array, *pnte= 0;

  if (sp->ops==&referenceSym) ReplaceRef(sp);
  if (sp->ops==&dataBlockSym) {
    array= (Array *)sp->value.db;
    if (array->ops==&lvalueOps) array= FetchLValue(array, sp);
    if (array->ops==&pointerOps && !array->type.dims) {
      pnte= Pointee(array->value.p[0]);
    } else {
      array= 0;
    }
  } else {
    array= 0;
  }

  if (!array)
    YError("cannot dereference (* or ->) non-pointer or non-scalar pointer");

  if ((DataBlock *)pnte != &nilDB)
    sp->value.db= (DataBlock *)NewLValueM(pnte, pnte->value.c,
                                          pnte->type.base, pnte->type.dims);
  else
    sp->value.db= RefNC(&nilDB);
  Unref(array);
}

/*--------------------------------------------------------------------------*/

void Eval(void)
{
  int nArgs= (pc++)->count;
  Operand obj;
  FormEvalOp(nArgs, &obj);
  obj.ops->Eval(&obj);
}

/*--------------------------------------------------------------------------*/

static char *GetMemberName(void *db)
{
  Array *array= db;
  return array->value.q[0];
}

void GetMember(void)
{
  char *name= GetMemberName((pc++)->constant->value.db);
  Operand op;
  FormEvalOp(0, &op);
  op.ops->GetMember(&op, name);
}

void Y_get_member(int nArgs)
{
  char *name;
  Operand op;
  if (nArgs!=2) YError("get_member function requires exactly two arguments");
  name= YGetString(sp);
  FormEvalOp(1, &op);
  op.ops->GetMember(&op, name);
  Drop(1);
}

void DerefMember(void)
{
  char *name= GetMemberName((pc++)->constant->value.db);
  Operand op;
  Deref();   /* now guaranteed to have an Array or Void on stack */
  FormEvalOp(0, &op);
  op.ops->GetMember(&op, name);
}

static void GetMembErr(long nItems);
static void GetMembErr(long nItems)
{
  if (nItems)
    YError("right operand to . or -> names non-existent member");
  else
    YError("left operand to . (->) not a struct (struct*)");
}

void GetMemberAY(Operand *op, char *name)
{
  Array *array= op->value;
  StructDef *subbase, *base= array->type.base;
  char *address= array->value.c;
  LValue *result;
  Dimension *dims;
  Member *member;

  if (!HashFind(&base->table, name, 0L)) GetMembErr(base->table.nItems);

  address+= base->offsets[hashIndex];
  member= &base->members[hashIndex];
  subbase= member->base;
  if (member->dims) dims= CopyDims(array->type.dims, Ref(member->dims), 1);
  else dims= Ref(array->type.dims);

  result= PushDataBlock(NewLValueM(array, address, subbase, dims));
  FreeDimension(dims);
  if (subbase->size!=base->size) {
    result->strider= NewStrider(base->size, array->type.number);
    if (member->number>1)
      result->strider->next= NewStrider(subbase->size, member->number);
  }

  PopTo(op->owner);
}

void GetMemberLV(Operand *op, char *name)
{
  LValue *lvalue= op->value;
  StructDef *subbase, *base= lvalue->type.base;
  Strider *strider;
  LValue *result;
  Dimension *dims;
  Member *member;
  int addressType= base->addressType;
  long offset;

  if (addressType>1) {
    /* sequential objects in a disk file must be read as a whole */
    op->value= FetchLValue(lvalue, op->owner);  /* sic - see FormEvalOp */
    GetMemberAY(op, name);
    return;
  }

  if (!HashFind(&base->table, name, 0L)) GetMembErr(base->table.nItems);

  member= &base->members[hashIndex];
  offset= base->offsets[hashIndex];
  subbase= member->base;
  if (member->dims) dims= CopyDims(lvalue->type.dims, Ref(member->dims), 1);
  else dims= Ref(lvalue->type.dims);

  result= PushDataBlock(addressType?
                        NewLValueD(lvalue->address.d, subbase, dims) :
                        NewLValueM(lvalue->owner, lvalue->address.m,
                                   subbase, dims));
  FreeDimension(dims);
  if (addressType) result->address.d+= offset;
  else result->address.m+= offset;
  if (subbase->size!=base->size) {
    if (!lvalue->strider && lvalue->type.number>1)
      lvalue->strider= NewStrider(base->size, lvalue->type.number);
    if (member->number>1)
      strider= NewStrider(subbase->size, member->number);
    else
      strider= 0;
  } else {
    strider= 0;
  }
  result->strider= CopyStrider(lvalue->strider, strider);

  PopTo(op->owner);
}

void GetMemberIO(Operand *op, char *name)
{
  IOStream *file= op->value;
  long address;
  Member *member;

  if (file->history) {
    /* check history child data table first */
    HistoryInfo *history= file->history;
    IOStream *child= history->child;
    if (HashFind(&child->dataTable, name, 0L) &&
        history->recNumber>=0 && history->recNumber<history->nRecords)
      file= child;
    else if (!HashFind(&file->dataTable, name, 0L))
      file= 0;
  } else {
    if (!HashFind(&file->dataTable, name, 0L)) file= 0;
  }

  if (!file) YError("no such variable in binary file");

  address= file->addresses[hashIndex]+file->offset;
  member= &file->types[hashIndex];

  PushDataBlock(NewLValueD(address, member->base, member->dims));
  PopTo(op->owner);
}

/* ARGSUSED */
void GetMemberX(Operand *op, char *name)
{
  YError("left operand to . or -> has illegal data type");
}

/*--------------------------------------------------------------------------*/
