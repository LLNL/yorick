/*
 * $Id: ops0.c,v 1.1 2005-09-18 22:04:07 dhmunro Exp $
 * Implement type conversion and promotion operations:
 *
 *  ToChar, ToShort, ToInt, ToLong, ToFloat, ToDouble, ToComplex
 *  Promote
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "ydata.h"

/*--------------------------------------------------------------------------*/
/* Type conversion operations */

VMaction ToChar, ToShort, ToInt, ToLong, ToFloat, ToDouble, ToComplex;

extern StackOp ToCharIS, ToCharLS, ToCharDS, ToCharDB;
extern StackOp ToShortIS, ToShortLS, ToShortDS, ToShortDB;
extern StackOp          ToIntLS, ToIntDS, ToIntDB;
extern StackOp ToLongIS,            ToLongDS, ToLongDB;
extern StackOp ToFloatIS, ToFloatLS, ToFloatDS, ToFloatDB;
extern StackOp ToDoubleIS, ToDoubleLS,             ToDoubleDB;
extern StackOp ToComplexIS, ToComplexLS, ToComplexDS, ToComplexDB;

extern StackOp NoOp;

extern UnaryOp          ToShortC, ToIntC, ToLongC,
               ToFloatC, ToDoubleC, ToComplexC;
extern UnaryOp ToCharS,           ToIntS, ToLongS,
               ToFloatS, ToDoubleS, ToComplexS;
extern UnaryOp ToCharI, ToShortI,         ToLongI,
               ToFloatI, ToDoubleI, ToComplexI;
extern UnaryOp ToCharL, ToShortL, ToIntL,
               ToFloatL, ToDoubleL, ToComplexL;
extern UnaryOp ToCharF, ToShortF, ToIntF, ToLongF,
                         ToDoubleF, ToComplexF;
extern UnaryOp ToCharD, ToShortD, ToIntD, ToLongD,
               ToFloatD,            ToComplexD;
extern UnaryOp ToCharZ, ToShortZ, ToIntZ, ToLongZ,
               ToFloatZ, ToDoubleZ;

extern UnaryOp No_Op;

/*--------------------------------------------------------------------------*/
/* Define several functions used by many unary and binary operators */

extern Operand *FormOperandIS(Symbol *owner, Operand *op);
extern Operand *FormOperandLS(Symbol *owner, Operand *op);
extern Operand *FormOperandDS(Symbol *owner, Operand *op);
extern Operand *FormOperandDB(Symbol *owner, Operand *op);
extern Operand *FormOperandRF(Symbol *owner, Operand *op);

extern void PopToI(Symbol *s);
extern void PopToL(Symbol *s);
extern void PopToD(Symbol *s);

extern void *BuildResult2(Operand *l, Operand *r);
extern void *BuildResult1(Operand *l, Operand *r);
extern void *BuildResult0(Operand *l, Operand *r, StructDef *base);

extern void *BuildResultU(Operand *op, StructDef *base);

/*--------------------------------------------------------------------------*/

void NoOp(void) { }
void No_Op(Operand *op) { }

/*--------------------------------------------------------------------------*/

Operand *FormOperandIS(Symbol *owner, Operand *op)
{
  op->owner= owner;
  op->ops= &intOps;
  op->references= 0;
  op->type.base= &intStruct;
  op->type.dims= 0;
  op->type.number= 1;
  op->value= &owner->value.i;
  return op;
}

Operand *FormOperandLS(Symbol *owner, Operand *op)
{
  op->owner= owner;
  op->ops= &longOps;
  op->references= 0;
  op->type.base= &longStruct;
  op->type.dims= 0;
  op->type.number= 1;
  op->value= &owner->value.l;
  return op;
}

Operand *FormOperandDS(Symbol *owner, Operand *op)
{
  op->owner= owner;
  op->ops= &doubleOps;
  op->references= 0;
  op->type.base= &doubleStruct;
  op->type.dims= 0;
  op->type.number= 1;
  op->value= &owner->value.d;
  return op;
}

Operand *FormOperandDB(Symbol *owner, Operand *op)
{
  DataBlock *db= owner->value.db;
  Operations *ops= db->ops;
  op->owner= owner;
  if (ops->isArray) {
    Array *array= (Array *)db;
    op->ops= ops;
    op->references= array->references;
    op->type.base= array->type.base;
    op->type.dims= array->type.dims;
    op->type.number= array->type.number;
    op->value= array->value.c;
  } else if (ops==&lvalueOps) {
    LValue *lvalue= (LValue *)db;
    StructDef *base= lvalue->type.base;
    if (lvalue->strider || base->model) {
      Array *array= FetchLValue(lvalue, owner);
      op->ops= array->ops;
      op->references= array->references;
      op->type.base= array->type.base;
      op->type.dims= array->type.dims;
      op->type.number= array->type.number;
      op->value= array->value.c;
    } else {
      op->ops= base->dataOps;
      op->references= 1;     /* NEVER try to use this as result */
      op->type.base= base;
      op->type.dims= lvalue->type.dims;
      op->type.number= lvalue->type.number;
      op->value= lvalue->address.m;
    }
  } else {
    op->ops= ops;
    op->references= db->references;
    op->type.base= 0;
    op->type.dims= 0;
    op->type.number= 0;
    op->value= db;
  }
  return op;
}

Operand *FormOperandRF(Symbol *owner, Operand *op)
{
  ReplaceRef(owner);
  return owner->ops->FormOperand(owner, op);
}

/* BuildResult2 forces conformability for a binary operation, then
   creates an appropriate result array, which may be the same as either
   the left or right operand if one or the other is a temporary.  */
void *BuildResult2(Operand *l, Operand *r)
{
  int casts= BinaryConform(l, r);
  if (casts & 4) return 0;

  if (casts & 2) {
    return PushCopy(l->owner)? l->value : &sp->value;
  } else if (casts & 1) {
    return PushCopy(r->owner)? r->value : &sp->value;
  } else {
    Array *array= PushDataBlock(NewArray(l->type.base, tmpDims));
    return (void *)array->value.c;
  }
}

/* BuildResult1 forces conformability for a binary operation, then
   creates an appropriate result array, which may be the same as
   the left operand if that is a temporary.  */
void *BuildResult1(Operand *l, Operand *r)
{
  int casts= BinaryConform(l, r);
  if (casts & 4) return 0;

  if (casts & 2) {
    return PushCopy(l->owner)? l->value : &sp->value;
  } else {
    Array *array= PushDataBlock(NewArray(l->type.base, tmpDims));
    return (void *)array->value.c;
  }
}

/* BuildResult0 forces conformability for a binary operation, then
   creates an appropriate result array.  */
void *BuildResult0(Operand *l, Operand *r, StructDef *base)
{
  Array *array;
  if (BinaryConform(l, r) & 4) return 0;
  array= PushDataBlock(NewArray(base, tmpDims));
  return (void *)array->value.c;
}

void *BuildResultU(Operand *op, StructDef *base)
{
  if (!op->references && op->type.base==base) {
    PushCopy(op->owner);
    return (sp->ops==&dataBlockSym)? op->value : &sp->value;
  } else {
    Array *array= PushDataBlock(NewArray(base, op->type.dims));
    return (void *)array->value.c;
  }
}

/*--------------------------------------------------------------------------*/

void PopToI(Symbol *s)
{
  Array *array= (Array *)sp->value.db;
  PopTo(s);
  if (s->ops==&dataBlockSym && !array->type.dims) {
    s->ops= &intScalar;
    s->value.i= array->value.i[0];
    Unref(array);
  }
}

void PopToL(Symbol *s)
{
  Array *array= (Array *)sp->value.db;
  PopTo(s);
  if (s->ops==&dataBlockSym && !array->type.dims) {
    s->ops= &longScalar;
    s->value.l= array->value.l[0];
    Unref(array);
  }
}

void PopToD(Symbol *s)
{
  Array *array= (Array *)sp->value.db;
  PopTo(s);
  if (s->ops==&dataBlockSym && !array->type.dims) {
    s->ops= &doubleScalar;
    s->value.d= array->value.d[0];
    Unref(array);
  }
}

/*--------------------------------------------------------------------------*/

static Operand lop;

/*--------------------------------------------------------------------------*/

void ToChar(void) { sp->ops->ToChar(); }
void ToCharIS(void) { ToCharI(FormOperandIS(sp, &lop)); }
void ToCharLS(void) { ToCharL(FormOperandLS(sp, &lop)); }
void ToCharDS(void) { ToCharD(FormOperandDS(sp, &lop)); }
void ToCharDB(void)
{ sp->value.db->ops->ToChar(FormOperandDB(sp, &lop)); }

void ToShort(void) { sp->ops->ToShort(); }
void ToShortIS(void) { ToShortI(FormOperandIS(sp, &lop)); }
void ToShortLS(void) { ToShortL(FormOperandLS(sp, &lop)); }
void ToShortDS(void) { ToShortD(FormOperandDS(sp, &lop)); }
void ToShortDB(void)
{ sp->value.db->ops->ToShort(FormOperandDB(sp, &lop)); }

void ToInt(void) { sp->ops->ToInt(); }
void ToIntLS(void) { sp->value.i= sp->value.l; sp->ops= &intScalar; }
void ToIntDS(void) { ToIntD(FormOperandDS(sp, &lop)); }
void ToIntDB(void)
{ sp->value.db->ops->ToInt(FormOperandDB(sp, &lop)); }

void ToLong(void) { sp->ops->ToLong(); }
void ToLongIS(void) { sp->value.l= sp->value.i; sp->ops= &longScalar; }
void ToLongDS(void) { ToLongD(FormOperandDS(sp, &lop)); }
void ToLongDB(void)
{ sp->value.db->ops->ToLong(FormOperandDB(sp, &lop)); }

void ToFloat(void) { sp->ops->ToFloat(); }
void ToFloatIS(void) { ToFloatI(FormOperandIS(sp, &lop)); }
void ToFloatLS(void) { ToFloatL(FormOperandLS(sp, &lop)); }
void ToFloatDS(void) { ToFloatD(FormOperandDS(sp, &lop)); }
void ToFloatDB(void)
{ sp->value.db->ops->ToFloat(FormOperandDB(sp, &lop)); }

void ToDouble(void) { sp->ops->ToDouble(); }
void ToDoubleIS(void) { sp->value.d= sp->value.i; sp->ops= &doubleScalar; }
void ToDoubleLS(void) { sp->value.d= sp->value.l; sp->ops= &doubleScalar; }
void ToDoubleDB(void)
{ sp->value.db->ops->ToDouble(FormOperandDB(sp, &lop)); }

void ToComplex(void) { sp->ops->ToComplex(); }
void ToComplexIS(void) { ToComplexI(FormOperandIS(sp, &lop)); }
void ToComplexLS(void) { ToComplexL(FormOperandLS(sp, &lop)); }
void ToComplexDS(void) { ToComplexD(FormOperandDS(sp, &lop)); }
void ToComplexDB(void)
{ sp->value.db->ops->ToComplex(FormOperandDB(sp, &lop)); }

/*--------------------------------------------------------------------------*/

/* All type converters follow a generic pattern expressed by CONV_TYPE */

#define CONV_TYPE(ctname, typeS, typeD, vD, baseD, Popper) \
void ctname(Operand *op) \
{ long i, n= op->type.number; \
  Array *res= PushDataBlock(NewArray(&baseD, op->type.dims)); \
  typeS *src= op->value;  typeD *dst= (typeD *)res->value.vD; \
  for (i=0 ; i<n ; i++) dst[i]= (typeD)src[i]; \
  Popper(op->owner);  op->ops= baseD.dataOps; op->type.base= &baseD; \
  if (op->owner->ops==&dataBlockSym) op->value= dst; \
  else op->value= &op->owner->value; \
}

#define CONV_TYPE_Z(ctname, typeS) \
void ctname(Operand *op) \
{ long i, n= op->type.number; \
  Array *res= PushDataBlock(NewArray(&complexStruct, op->type.dims)); \
  typeS *src= op->value;  double *dst= res->value.d; \
  for (i=0 ; i<n ; i++) { dst[2*i]= src[i]; dst[2*i+1]= 0.0; } \
  op->ops= &complexOps; op->type.base= &complexStruct; \
  op->value= dst;  PopTo(op->owner); \
}

#define CONV_Z_TYPE(ctname, typeD, vD, baseD, Popper) \
void ctname(Operand *op) \
{ long i, n= op->type.number; \
  Array *res= PushDataBlock(NewArray(&baseD, op->type.dims)); \
  double *src= op->value;  typeD *dst= (typeD *)res->value.vD; \
  for (i=0 ; i<n ; i++) dst[i]= (typeD)src[2*i]; \
  Popper(op->owner);  op->ops= baseD.dataOps; op->type.base= &baseD; \
  if (op->owner->ops==&dataBlockSym) op->value= dst; \
  else op->value= &op->owner->value; \
}

CONV_TYPE(ToShortC, unsigned char, short, s, shortStruct, PopTo)
CONV_TYPE(ToIntC, unsigned char, int, i, intStruct, PopToI)
CONV_TYPE(ToLongC, unsigned char, long, l, longStruct, PopToL)
CONV_TYPE(ToFloatC, unsigned char, float, f, floatStruct, PopTo)
CONV_TYPE(ToDoubleC, unsigned char, double, d, doubleStruct, PopToD)
CONV_TYPE_Z(ToComplexC, unsigned char)

CONV_TYPE(ToCharS, short, unsigned char, c, charStruct, PopTo)
CONV_TYPE(ToIntS, short, int, i, intStruct, PopToI)
CONV_TYPE(ToLongS, short, long, l, longStruct, PopToL)
CONV_TYPE(ToFloatS, short, float, f, floatStruct, PopTo)
CONV_TYPE(ToDoubleS, short, double, d, doubleStruct, PopToD)
CONV_TYPE_Z(ToComplexS, short)

CONV_TYPE(ToCharI, int, unsigned char, c, charStruct, PopTo)
CONV_TYPE(ToShortI, int, short, s, shortStruct, PopTo)
CONV_TYPE(ToLongI, int, long, l, longStruct, PopToL)
CONV_TYPE(ToFloatI, int, float, f, floatStruct, PopTo)
CONV_TYPE(ToDoubleI, int, double, d, doubleStruct, PopToD)
CONV_TYPE_Z(ToComplexI, int)

CONV_TYPE(ToCharL, long, unsigned char, c, charStruct, PopTo)
CONV_TYPE(ToShortL, long, short, s, shortStruct, PopTo)
CONV_TYPE(ToIntL, long, int, i, intStruct, PopToI)
CONV_TYPE(ToFloatL, long, float, f, floatStruct, PopTo)
CONV_TYPE(ToDoubleL, long, double, d, doubleStruct, PopToD)
CONV_TYPE_Z(ToComplexL, long)

CONV_TYPE(ToCharF, float, unsigned char, c, charStruct, PopTo)
CONV_TYPE(ToShortF, float, short, s, shortStruct, PopTo)
CONV_TYPE(ToIntF, float, int, i, intStruct, PopToI)
CONV_TYPE(ToLongF, float, long, l, longStruct, PopToL)
CONV_TYPE(ToDoubleF, float, double, d, doubleStruct, PopToD)
CONV_TYPE_Z(ToComplexF, float)

CONV_TYPE(ToCharD, double, unsigned char, c, charStruct, PopTo)
CONV_TYPE(ToShortD, double, short, s, shortStruct, PopTo)
CONV_TYPE(ToIntD, double, int, i, intStruct, PopToI)
CONV_TYPE(ToLongD, double, long, l, longStruct, PopToL)
CONV_TYPE(ToFloatD, double, float, f, floatStruct, PopTo)
CONV_TYPE_Z(ToComplexD, double)

CONV_Z_TYPE(ToCharZ, unsigned char, c, charStruct, PopTo)
CONV_Z_TYPE(ToShortZ, short, s, shortStruct, PopTo)
CONV_Z_TYPE(ToIntZ, int, i, intStruct, PopToI)
CONV_Z_TYPE(ToLongZ, long, l, longStruct, PopToL)
CONV_Z_TYPE(ToFloatZ, float, f, floatStruct, PopTo)
CONV_Z_TYPE(ToDoubleZ, double, d, doubleStruct, PopToD)

/*--------------------------------------------------------------------------*/
/* Type promotion operators take two Operand*s, and do
   arithmetic promotion (using one stack element for protected
   scratch space).  Either the left or the right owner (but not both)
   is updated, and the operator returns the Operations* for the result
   type, or 0 if the required promotion operation was impossible.
   Legal types for promotion are: char, short, int, long, float, double,
   complex.  An Operand cannot point to an LValue (see FormOperandDB).
   Non-numeric types return l->ops if the l->ops==r->ops, else 0.  */
/* typedef Operations *PromoteOp(Operand *l, Operand *r); */

extern PromoteOp PromNOP;
extern PromoteOp         PromCS, PromCI, PromCL, PromCF, PromCD, PromCZ;
extern PromoteOp PromSC,         PromSI, PromSL, PromSF, PromSD, PromSZ;
extern PromoteOp PromIC, PromIS,         PromIL, PromIF, PromID, PromIZ;
extern PromoteOp PromLC, PromLS, PromLI,         PromLF, PromLD, PromLZ;
extern PromoteOp PromFC, PromFS, PromFI, PromFL,         PromFD, PromFZ;
extern PromoteOp PromDC, PromDS, PromDI, PromDL, PromDF,         PromDZ;
extern PromoteOp PromZC, PromZS, PromZI, PromZL, PromZF, PromZD;

Operations *PromNOP(Operand *l, Operand *r) { return l->ops; }
Operations *PromXX(Operand *l, Operand *r)
{ return l->ops==r->ops? l->ops : 0; }

Operations *PromCS(Operand *l, Operand *r) { ToShortC(l); return r->ops; }
Operations *PromCI(Operand *l, Operand *r) { ToIntC(l); return r->ops; }
Operations *PromCL(Operand *l, Operand *r) { ToLongC(l); return r->ops; }
Operations *PromCF(Operand *l, Operand *r) { ToFloatC(l); return r->ops; }
Operations *PromCD(Operand *l, Operand *r) { ToDoubleC(l); return r->ops; }
Operations *PromCZ(Operand *l, Operand *r) { ToComplexC(l); return r->ops; }

Operations *PromSC(Operand *l, Operand *r) { ToShortC(r); return l->ops; }
Operations *PromSI(Operand *l, Operand *r) { ToIntS(l); return r->ops; }
Operations *PromSL(Operand *l, Operand *r) { ToLongS(l); return r->ops; }
Operations *PromSF(Operand *l, Operand *r) { ToFloatS(l); return r->ops; }
Operations *PromSD(Operand *l, Operand *r) { ToDoubleS(l); return r->ops; }
Operations *PromSZ(Operand *l, Operand *r) { ToComplexS(l); return r->ops; }

Operations *PromIC(Operand *l, Operand *r) { ToIntC(r); return l->ops; }
Operations *PromIS(Operand *l, Operand *r) { ToIntS(r); return l->ops; }
Operations *PromIL(Operand *l, Operand *r) { ToLongI(l); return r->ops; }
Operations *PromIF(Operand *l, Operand *r) { ToFloatI(l); return r->ops; }
Operations *PromID(Operand *l, Operand *r) { ToDoubleI(l); return r->ops; }
Operations *PromIZ(Operand *l, Operand *r) { ToComplexI(l); return r->ops; }

Operations *PromLC(Operand *l, Operand *r) { ToLongC(r); return l->ops; }
Operations *PromLS(Operand *l, Operand *r) { ToLongS(r); return l->ops; }
Operations *PromLI(Operand *l, Operand *r) { ToLongI(r); return l->ops; }
Operations *PromLF(Operand *l, Operand *r) { ToFloatL(l); return r->ops; }
Operations *PromLD(Operand *l, Operand *r) { ToDoubleL(l); return r->ops; }
Operations *PromLZ(Operand *l, Operand *r) { ToComplexL(l); return r->ops; }

Operations *PromFC(Operand *l, Operand *r) { ToFloatC(r); return l->ops; }
Operations *PromFS(Operand *l, Operand *r) { ToFloatS(r); return l->ops; }
Operations *PromFI(Operand *l, Operand *r) { ToFloatI(r); return l->ops; }
Operations *PromFL(Operand *l, Operand *r) { ToFloatL(r); return l->ops; }
Operations *PromFD(Operand *l, Operand *r) { ToDoubleF(l); return r->ops; }
Operations *PromFZ(Operand *l, Operand *r) { ToComplexF(l); return r->ops; }

Operations *PromDC(Operand *l, Operand *r) { ToDoubleC(r); return l->ops; }
Operations *PromDS(Operand *l, Operand *r) { ToDoubleS(r); return l->ops; }
Operations *PromDI(Operand *l, Operand *r) { ToDoubleI(r); return l->ops; }
Operations *PromDL(Operand *l, Operand *r) { ToDoubleL(r); return l->ops; }
Operations *PromDF(Operand *l, Operand *r) { ToDoubleF(r); return l->ops; }
Operations *PromDZ(Operand *l, Operand *r) { ToComplexD(l); return r->ops; }

Operations *PromZC(Operand *l, Operand *r) { ToComplexC(r); return l->ops; }
Operations *PromZS(Operand *l, Operand *r) { ToComplexS(r); return l->ops; }
Operations *PromZI(Operand *l, Operand *r) { ToComplexI(r); return l->ops; }
Operations *PromZL(Operand *l, Operand *r) { ToComplexL(r); return l->ops; }
Operations *PromZF(Operand *l, Operand *r) { ToComplexF(r); return l->ops; }
Operations *PromZD(Operand *l, Operand *r) { ToComplexD(r); return l->ops; }

/*--------------------------------------------------------------------------*/
