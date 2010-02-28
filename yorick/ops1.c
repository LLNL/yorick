/*
 * $Id: ops1.c,v 1.2 2010-02-28 21:52:29 dhmunro Exp $
 * Implement bitwise binary operations and unary operations:
 *
 *  Or |   And &   Xor ~
 *  ShiftL <<   ShiftR >>
 *
 *  Negate -   Complement ~   Not !   True (no explicit operator)
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "ydata.h"

/*--------------------------------------------------------------------------*/

extern VMaction ShiftL, ShiftR,
  AndBits, XorBits, OrBits, AndLogical, OrLogical;
extern VMaction Negate, Complement, Not, True;

/* ..X versions now declared in ydata.h */

extern StackOp OrII, OrIL, OrIB,
               OrLI, OrLL, OrLB,
               OrBI, OrBL, OrBB, OrXX;
extern BinaryOp OrC, OrS, OrI, OrL;

extern StackOp AndII, AndIL, AndIB,
               AndLI, AndLL, AndLB,
               AndBI, AndBL, AndBB, AndXX;
extern BinaryOp AndC, AndS, AndI, AndL;

extern StackOp XorII, XorIL, XorIB,
               XorLI, XorLL, XorLB,
               XorBI, XorBL, XorBB, XorXX;
extern BinaryOp XorC, XorS, XorI, XorL;

extern StackOp ShiftLII, ShiftLIL, ShiftLIB,
               ShiftLLI, ShiftLLL, ShiftLLB,
               ShiftLBI, ShiftLBL, ShiftLBB, ShiftLXX;
extern BinaryOp ShiftLC, ShiftLS, ShiftLI, ShiftLL;

extern StackOp ShiftRII, ShiftRIL, ShiftRIB,
               ShiftRLI, ShiftRLL, ShiftRLB,
               ShiftRBI, ShiftRBL, ShiftRBB, ShiftRXX;
extern BinaryOp ShiftRC, ShiftRS, ShiftRI, ShiftRL;

extern StackOp ComplementIS, ComplementLS,
               ComplementDB, ComplementXX;
extern UnaryOp ComplementC, ComplementS, ComplementI, ComplementL;

extern StackOp NegateIS, NegateLS, NegateDS, NegateDB, NegateXX;
extern UnaryOp NegateC, NegateS, NegateI, NegateL,
               NegateF, NegateD, NegateZ;

extern StackOp NotIS, NotLS, NotDS, NotDB;
extern UnaryOp NotC, NotS, NotI, NotL, NotF, NotD, NotZ,
               NotQ, NotP, NotSI, NotVD;

extern StackOp TrueIS, TrueLS, TrueDS, TrueDB;
extern UnaryOp TrueC, TrueS, TrueI, TrueL, TrueF, TrueD, TrueZ,
               TrueQ, TrueP, TrueSI, TrueVD;

/*--------------------------------------------------------------------------*/

extern Operand *FormOperandIS(Symbol *owner, Operand *op);
extern Operand *FormOperandLS(Symbol *owner, Operand *op);
extern Operand *FormOperandDS(Symbol *owner, Operand *op);
extern Operand *FormOperandDB(Symbol *owner, Operand *op);

extern void PopToI(Symbol *s);
extern void PopToL(Symbol *s);
extern void PopToD(Symbol *s);

extern void *BuildResult2(Operand *l, Operand *r);
extern void *BuildResult1(Operand *l, Operand *r);
extern void *BuildResult0(Operand *l, Operand *r, StructDef *base);

extern void *BuildResultU(Operand *op, StructDef *base);

extern void PopToI(Symbol *s);
extern void PopToL(Symbol *s);
extern void PopToD(Symbol *s);

/*--------------------------------------------------------------------------*/

static Operand lop, rop;

/*--------------------------------------------------------------------------*/
/* OrBits */

void OrBits(void) { (sp-1)->ops->Or[sp->ops->id](); }

static void Or_BB(Operand *l, Operand *r);

void OrII(void) { Symbol *spr= sp--; sp->value.i|= spr->value.i; }
void OrIL(void) { Symbol *spr= sp--;
  sp->value.l= sp->value.i|spr->value.l; sp->ops= &longScalar; }
void OrIB(void)
{ Or_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void OrLI(void) { Symbol *spr= sp--; sp->value.l|= spr->value.i; }
void OrLL(void) { Symbol *spr= sp--; sp->value.l|= spr->value.l; }
void OrLB(void)
{ Or_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void OrBI(void)
{ Or_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void OrBL(void)
{ Or_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void OrBB(void)
{ Or_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

void OrXX(void) { OrX((Operand *)0, (Operand *)0); }

static void Or_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (!ops) YError("bad data type(s) in binary |");
  ops->Or(l, r);
  Drop(1);
}

static void OrError(void);
static void OrError(void)
{ YError("operands not conformable in binary |"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]|rv[i]; \
    Popper(l->owner); \
  } else OrError(); }

OPERATION(OrC, char, PopTo)
OPERATION(OrS, short, PopTo)
OPERATION(OrI, int, PopToI)
OPERATION(OrL, long, PopToL)
void OrX(Operand *l, Operand *r)
{ YError("non-integer data type in binary |"); }

/*--------------------------------------------------------------------------*/
/* AndBits */

void AndBits(void) { (sp-1)->ops->And[sp->ops->id](); }

static void And_BB(Operand *l, Operand *r);

void AndII(void) { Symbol *spr= sp--; sp->value.i&= spr->value.i; }
void AndIL(void) { Symbol *spr= sp--;
  sp->value.l= sp->value.i&spr->value.l; sp->ops= &longScalar; }
void AndIB(void)
{ And_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void AndLI(void) { Symbol *spr= sp--; sp->value.l&= spr->value.i; }
void AndLL(void) { Symbol *spr= sp--; sp->value.l&= spr->value.l; }
void AndLB(void)
{ And_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void AndBI(void)
{ And_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void AndBL(void)
{ And_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void AndBB(void)
{ And_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

void AndXX(void) { AndX((Operand *)0, (Operand *)0); }

static void And_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (!ops) YError("bad data type(s) in binary &");
  ops->And(l, r);
  Drop(1);
}

static void AndError(void);
static void AndError(void)
{ YError("operands not conformable in binary &"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]&rv[i]; \
    Popper(l->owner); \
  } else AndError(); }

OPERATION(AndC, char, PopTo)
OPERATION(AndS, short, PopTo)
OPERATION(AndI, int, PopToI)
OPERATION(AndL, long, PopToL)
void AndX(Operand *l, Operand *r)
{ YError("non-integer data type in binary &"); }

/*--------------------------------------------------------------------------*/
/* XorBits */

void XorBits(void) { (sp-1)->ops->Xor[sp->ops->id](); }

static void Xor_BB(Operand *l, Operand *r);

void XorII(void) { Symbol *spr= sp--; sp->value.i^= spr->value.i; }
void XorIL(void) { Symbol *spr= sp--;
  sp->value.l= sp->value.i^spr->value.l; sp->ops= &longScalar; }
void XorIB(void)
{ Xor_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void XorLI(void) { Symbol *spr= sp--; sp->value.l^= spr->value.i; }
void XorLL(void) { Symbol *spr= sp--; sp->value.l^= spr->value.l; }
void XorLB(void)
{ Xor_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void XorBI(void)
{ Xor_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void XorBL(void)
{ Xor_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void XorBB(void)
{ Xor_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

void XorXX(void) { XorX((Operand *)0, (Operand *)0); }

static void Xor_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (!ops) YError("bad data type(s) in binary ~");
  ops->Xor(l, r);
  Drop(1);
}

static void XorError(void);
static void XorError(void)
{ YError("operands not conformable in binary ~"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]^rv[i]; \
    Popper(l->owner); \
  } else XorError(); }

OPERATION(XorC, char, PopTo)
OPERATION(XorS, short, PopTo)
OPERATION(XorI, int, PopToI)
OPERATION(XorL, long, PopToL)
void XorX(Operand *l, Operand *r)
{ YError("non-integer data type in binary ~"); }

/*--------------------------------------------------------------------------*/
/* ShiftL */

void ShiftL(void) { (sp-1)->ops->ShiftL[sp->ops->id](); }

static void ShiftL_BB(Operand *l, Operand *r);

void ShiftLII(void) { Symbol *spr= sp--; sp->value.i<<= spr->value.i; }
void ShiftLIL(void) { Symbol *spr= sp--; sp->value.i<<= spr->value.l; }
void ShiftLIB(void)
{ ShiftL_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void ShiftLLI(void) { Symbol *spr= sp--; sp->value.l<<= spr->value.i; }
void ShiftLLL(void) { Symbol *spr= sp--; sp->value.l<<= spr->value.l; }
void ShiftLLB(void)
{ ShiftL_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void ShiftLBI(void)
{ ShiftL_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void ShiftLBL(void)
{ ShiftL_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void ShiftLBB(void)
{ ShiftL_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

void ShiftLXX(void) { ShiftLX((Operand *)0, (Operand *)0); }

static void ShiftL_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops;
  if (r->ops->promoteID>T_LONG)
    YError("non-integer right operand in binary <<");
  r->ops->ToInt(r);
  ops->ShiftL(l, r);
  Drop(1);
}

static void ShiftLError(void);
static void ShiftLError(void)
{ YError("operands not conformable in binary <<"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult1(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value; int *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]<<rv[i]; \
    Popper(l->owner); \
  } else ShiftLError(); }

OPERATION(ShiftLC, unsigned char, PopTo)
OPERATION(ShiftLS, short, PopTo)
OPERATION(ShiftLI, int, PopToI)
OPERATION(ShiftLL, long, PopToL)
void ShiftLX(Operand *l, Operand *r)
{ YError("non-integer left operand in binary <<"); }

/*--------------------------------------------------------------------------*/
/* ShiftR */

void ShiftR(void) { (sp-1)->ops->ShiftR[sp->ops->id](); }

static void ShiftR_BB(Operand *l, Operand *r);

void ShiftRII(void) { Symbol *spr= sp--; sp->value.i>>= spr->value.i; }
void ShiftRIL(void) { Symbol *spr= sp--; sp->value.i>>= spr->value.l; }
void ShiftRIB(void)
{ ShiftR_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void ShiftRLI(void) { Symbol *spr= sp--; sp->value.l>>= spr->value.i; }
void ShiftRLL(void) { Symbol *spr= sp--; sp->value.l>>= spr->value.l; }
void ShiftRLB(void)
{ ShiftR_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void ShiftRBI(void)
{ ShiftR_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void ShiftRBL(void)
{ ShiftR_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void ShiftRBB(void)
{ ShiftR_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

void ShiftRXX(void) { ShiftRX((Operand *)0, (Operand *)0); }

static void ShiftR_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops;
  if (r->ops->promoteID>T_LONG)
    YError("non-integer right operand in binary >>");
  r->ops->ToInt(r);
  ops->ShiftR(l, r);
  Drop(1);
}

static void ShiftRError(void);
static void ShiftRError(void)
{ YError("operands not conformable in binary >>"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult1(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value; int *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]>>rv[i]; \
    Popper(l->owner); \
  } else ShiftRError(); }

OPERATION(ShiftRC, unsigned char, PopTo)
OPERATION(ShiftRS, short, PopTo)
OPERATION(ShiftRI, int, PopToI)
OPERATION(ShiftRL, long, PopToL)
void ShiftRX(Operand *l, Operand *r)
{ YError("non-integer left operand in binary >>"); }

/*--------------------------------------------------------------------------*/
/* Complement */

void Complement(void) { sp->ops->Complement(); }

void ComplementXX(void) { ComplementX((Operand *)0); }

void ComplementIS(void) { sp->value.i= ~sp->value.i; }
void ComplementLS(void) { sp->value.l= ~sp->value.l; }
void ComplementDB(void) { FormOperandDB(sp, &lop); lop.ops->Complement(&lop); }

#undef OPERATION
#define OPERATION(opname, typd, baseD, Popper) \
void opname(Operand *op) \
{ typd *dst= BuildResultU(op, &baseD); \
  long i, n= op->type.number; \
  typd *lv= op->value; \
  for (i=0 ; i<n ; i++) dst[i]= ~lv[i]; \
  Popper(op->owner); }

OPERATION(ComplementC, char, charStruct, PopTo)
OPERATION(ComplementS, short, shortStruct, PopTo)
OPERATION(ComplementI, int, intStruct, PopToI)
OPERATION(ComplementL, long, longStruct, PopToL)

void ComplementX(Operand *op)
{ YError("non-integer data type in unary ~"); }

/*--------------------------------------------------------------------------*/
/* Negate */

void Negate(void) { sp->ops->Negate(); }

void NegateXX(void) { NegateX((Operand *)0); }

void NegateIS(void) { sp->value.i= -sp->value.i; }
void NegateLS(void) { sp->value.l= -sp->value.l; }
void NegateDS(void) { sp->value.d= -sp->value.d; }
void NegateDB(void) { FormOperandDB(sp, &lop); lop.ops->Negate(&lop); }

#undef OPERATION
#define OPERATION(opname, typd, baseD, Popper) \
void opname(Operand *op) \
{ typd *dst= BuildResultU(op, &baseD); \
  long i, n= op->type.number; \
  typd *lv= op->value; \
  for (i=0 ; i<n ; i++) dst[i]= -lv[i]; \
  Popper(op->owner); }

OPERATION(NegateC, char, charStruct, PopTo)
OPERATION(NegateS, short, shortStruct, PopTo)
OPERATION(NegateI, int, intStruct, PopToI)
OPERATION(NegateL, long, longStruct, PopToL)
OPERATION(NegateF, float, floatStruct, PopTo)
OPERATION(NegateD, double, doubleStruct, PopToD)
void NegateZ(Operand *op)
{ double *dst= BuildResultU(op, &complexStruct);
  long i, n= 2*op->type.number;
  double *lv= op->value;
  for (i=0 ; i<n ; i++) dst[i]= -lv[i];
  PopTo(op->owner);
}

void NegateX(Operand *op)
{ YError("non-numeric data type in unary -"); }

/*--------------------------------------------------------------------------*/
/* Not */

void Not(void) { sp->ops->Not(); }

void NotIS(void) { sp->value.i= !sp->value.i; }
void NotLS(void) { sp->value.i= !sp->value.l; sp->ops= &intScalar; }
void NotDS(void) { sp->value.i= !sp->value.d; sp->ops= &intScalar; }
void NotDB(void) { FormOperandDB(sp, &lop); lop.ops->Not(&lop); }

#undef OPERATION
#define OPERATION(opname, typd) \
void opname(Operand *op) \
{ int *dst= BuildResultU(op, &intStruct); \
  long i, n= op->type.number; \
  typd *lv= op->value; \
  for (i=0 ; i<n ; i++) dst[i]= !lv[i]; \
  PopToI(op->owner); }

OPERATION(NotC, char)
OPERATION(NotS, short)
OPERATION(NotI, int)
OPERATION(NotL, long)
OPERATION(NotF, float)
OPERATION(NotD, double)
void NotZ(Operand *op)
{ int *dst= BuildResultU(op, &intStruct);
  long i, n= op->type.number;
  double *lv= op->value;
  for (i=0 ; i<n ; i++) dst[i]= !(lv[2*i]||lv[2*i+1]);
  PopToI(op->owner);
}

OPERATION(NotQ, char *)
OPERATION(NotP, void *)

void NotSI(Operand *op)
{ int *dst= BuildResultU(op, &intStruct);
  long i, n= op->type.number, j, size= op->type.base->size;
  char *lv= op->value;
  for (i=0 ; i<n ; i++) { dst[i]= 1;
    for (j=0 ; j<size ; j++) if (lv[j]) { dst[i]= 0; break; }}
  PopToI(op->owner);
}

void NotVD(Operand *op)
{ Symbol *s= op->owner; s->ops=&intScalar;s->value.i=1; UnrefNC(&nilDB); }

void NotX(Operand *op)
{
  Symbol *s= op->owner;
  DataBlock *db= (s->ops==&dataBlockSym)? s->value.db : 0;
  s->ops= &intScalar;
  s->value.i= 0;
  Unref(db);
}

/*--------------------------------------------------------------------------*/
/* True */

void True(void) { sp->ops->True(); }

void TrueIS(void) { sp->value.i= sp->value.i!=0; }
void TrueLS(void) { sp->value.i= sp->value.l!=0; sp->ops= &intScalar; }
void TrueDS(void) { sp->value.i= sp->value.d!=0; sp->ops= &intScalar; }
void TrueDB(void) { FormOperandDB(sp, &lop); lop.ops->True(&lop); }

#undef OPERATION
#define OPERATION(opname, typd) \
void opname(Operand *op) \
{ int *dst= BuildResultU(op, &intStruct); \
  long i, n= op->type.number; \
  typd *lv= op->value; \
  for (i=0 ; i<n ; i++) dst[i]= lv[i]!=0; \
  PopToI(op->owner); }

OPERATION(TrueC, char)
OPERATION(TrueS, short)
OPERATION(TrueI, int)
OPERATION(TrueL, long)
OPERATION(TrueF, float)
OPERATION(TrueD, double)
void TrueZ(Operand *op)
{ int *dst= BuildResultU(op, &intStruct);
  long i, n= op->type.number;
  double *lv= op->value;
  for (i=0 ; i<n ; i++) dst[i]= (lv[2*i]||lv[2*i+1]);
  PopToI(op->owner);
}

OPERATION(TrueQ, char *)
OPERATION(TrueP, void *)

void TrueSI(Operand *op)
{ int *dst= BuildResultU(op, &intStruct);
  long i, n= op->type.number, j, size= op->type.base->size;
  char *lv= op->value;
  for (i=0 ; i<n ; i++) { dst[i]= 0;
    for (j=0 ; j<size ; j++) if (lv[j]) { dst[i]= 1; break; }}
  PopToI(op->owner);
}

void TrueVD(Operand *op)
{ Symbol *s= op->owner; s->ops=&intScalar;s->value.i=0; UnrefNC(&nilDB); }

void TrueX(Operand *op)
{
  Symbol *s= op->owner;
  DataBlock *db= (s->ops==&dataBlockSym)? s->value.db : 0;
  s->ops= &intScalar;
  s->value.i= 1;
  Unref(db);
}

/*--------------------------------------------------------------------------*/
