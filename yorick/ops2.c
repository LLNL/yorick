/*
 * $Id: ops2.c,v 1.1 2005-09-18 22:03:58 dhmunro Exp $
 * Implement arithmetic binary operations:
 *
 *  Add +   Subtract -   Multiply *   Divide /   Modulo %   Power ^
 *  Greater >   Less <   GreaterEQ >=   LessEQ <=   Equal ==   NotEqual !=
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "ydata.h"
#include "pstdlib.h"
#include <string.h>

#include <errno.h>

/*--------------------------------------------------------------------------*/

extern VMaction Power, Multiply, Divide, Modulo, Add, Subtract,
  ShiftL, ShiftR, Less, Greater, LessEQ, GreaterEQ, Equal, NotEqual;

/* ..X versions now declared in ydata.h */

extern StackOp AddII, AddIL, AddID, AddIB,
               AddLI, AddLL, AddLD, AddLB,
               AddDI, AddDL, AddDD, AddDB,
               AddBI, AddBL, AddBD, AddBB;
extern BinaryOp AddC, AddS, AddI, AddL,
                AddF, AddD, AddZ, AddQ;

extern StackOp SubtractII, SubtractIL, SubtractID, SubtractIB,
               SubtractLI, SubtractLL, SubtractLD, SubtractLB,
               SubtractDI, SubtractDL, SubtractDD, SubtractDB,
               SubtractBI, SubtractBL, SubtractBD, SubtractBB;
extern BinaryOp SubtractC, SubtractS, SubtractI, SubtractL,
                SubtractF, SubtractD, SubtractZ;

extern StackOp MultiplyII, MultiplyIL, MultiplyID, MultiplyIB,
               MultiplyLI, MultiplyLL, MultiplyLD, MultiplyLB,
               MultiplyDI, MultiplyDL, MultiplyDD, MultiplyDB,
               MultiplyBI, MultiplyBL, MultiplyBD, MultiplyBB;
extern BinaryOp MultiplyC, MultiplyS, MultiplyI, MultiplyL,
                MultiplyF, MultiplyD, MultiplyZ;

extern StackOp DivideII, DivideIL, DivideID, DivideIB,
               DivideLI, DivideLL, DivideLD, DivideLB,
               DivideDI, DivideDL, DivideDD, DivideDB,
               DivideBI, DivideBL, DivideBD, DivideBB;
extern BinaryOp DivideC, DivideS, DivideI, DivideL,
                DivideF, DivideD, DivideZ;

extern StackOp ModuloII, ModuloIL, ModuloID, ModuloIB,
               ModuloLI, ModuloLL, ModuloLD, ModuloLB,
               ModuloDI, ModuloDL, ModuloDD, ModuloDB,
               ModuloBI, ModuloBL, ModuloBD, ModuloBB;
extern BinaryOp ModuloC, ModuloS, ModuloI, ModuloL,
                ModuloF, ModuloD, ModuloZ;

extern StackOp PowerII, PowerIL, PowerID, PowerIB,
               PowerLI, PowerLL, PowerLD, PowerLB,
               PowerDI, PowerDL, PowerDD, PowerDB,
               PowerBI, PowerBL, PowerBD, PowerBB;
extern BinaryOp PowerC, PowerS, PowerI, PowerL,
                PowerF, PowerD, PowerZ;

extern BinaryOp PowerXF, PowerXD, PowerXZ;

extern StackOp GreaterII, GreaterIL, GreaterID, GreaterIB,
               GreaterLI, GreaterLL, GreaterLD, GreaterLB,
               GreaterDI, GreaterDL, GreaterDD, GreaterDB,
               GreaterBI, GreaterBL, GreaterBD, GreaterBB;
extern BinaryOp GreaterC, GreaterS, GreaterI, GreaterL,
                GreaterF, GreaterD, GreaterZ, GreaterQ;

extern StackOp LessII, LessIL, LessID, LessIB,
               LessLI, LessLL, LessLD, LessLB,
               LessDI, LessDL, LessDD, LessDB,
               LessBI, LessBL, LessBD, LessBB;

extern StackOp GreaterEQII, GreaterEQIL, GreaterEQID, GreaterEQIB,
               GreaterEQLI, GreaterEQLL, GreaterEQLD, GreaterEQLB,
               GreaterEQDI, GreaterEQDL, GreaterEQDD, GreaterEQDB,
               GreaterEQBI, GreaterEQBL, GreaterEQBD, GreaterEQBB;
extern BinaryOp GreaterEQC, GreaterEQS, GreaterEQI, GreaterEQL,
                GreaterEQF, GreaterEQD, GreaterEQZ, GreaterEQQ;

extern StackOp LessEQII, LessEQIL, LessEQID, LessEQIB,
               LessEQLI, LessEQLL, LessEQLD, LessEQLB,
               LessEQDI, LessEQDL, LessEQDD, LessEQDB,
               LessEQBI, LessEQBL, LessEQBD, LessEQBB;

extern StackOp EqualII, EqualIL, EqualID, EqualIB,
               EqualLI, EqualLL, EqualLD, EqualLB,
               EqualDI, EqualDL, EqualDD, EqualDB,
               EqualBI, EqualBL, EqualBD, EqualBB;
extern BinaryOp EqualC, EqualS, EqualI, EqualL,
                EqualF, EqualD, EqualZ, EqualQ, EqualP,
                EqualSI, EqualR;

extern StackOp NotEqualII, NotEqualIL, NotEqualID, NotEqualIB,
               NotEqualLI, NotEqualLL, NotEqualLD, NotEqualLB,
               NotEqualDI, NotEqualDL, NotEqualDD, NotEqualDB,
               NotEqualBI, NotEqualBL, NotEqualBD, NotEqualBB;
extern BinaryOp NotEqualC, NotEqualS, NotEqualI, NotEqualL,
                NotEqualF, NotEqualD, NotEqualZ, NotEqualQ, NotEqualP,
                NotEqualSI, NotEqualR;

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

extern double fmod(double, double);  /* required for real % operands */
#define RMOD(x,y) fmod((x),(y))
/* could also use x - y*floor(x/y), which differs for negative arguments */

/* Only one of the power operations is a standard C-library function */
extern double pow(double, double); /* pow is double-to-double in math.h */
extern long powLL(long, long);     /* powLL is long-to-long in nonc.c */
extern double powDL(double, long); /* powDL is double-to-long in nonc.c */
extern void powZL(double *, double *, long);
                                   /* powZL is complex-to-long in nonc.c */
extern void powZZ(double *, double *, double *);
                                   /* powZZ is complex-to-complex in nonc.c */

/*--------------------------------------------------------------------------*/

static Operand lop, rop;

/*--------------------------------------------------------------------------*/
/* Add */

void Add(void) { (sp-1)->ops->Add[sp->ops->id](); }

static void Add_BB(Operand *l, Operand *r);

void AddII(void) { Symbol *spr= sp--; sp->value.i+= spr->value.i; }
void AddIL(void) { Symbol *spr= sp--;
  sp->value.l= sp->value.i+spr->value.l; sp->ops= &longScalar; }
void AddID(void) { Symbol *spr= sp--;
  sp->value.d= sp->value.i+spr->value.d; sp->ops= &doubleScalar; }
void AddIB(void)
{ Add_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void AddLI(void) { Symbol *spr= sp--; sp->value.l+= spr->value.i; }
void AddLL(void) { Symbol *spr= sp--; sp->value.l+= spr->value.l; }
void AddLD(void) { Symbol *spr= sp--;
  sp->value.d= sp->value.l+spr->value.d; sp->ops= &doubleScalar; }
void AddLB(void)
{ Add_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void AddDI(void) { Symbol *spr= sp--; sp->value.d+= spr->value.i; }
void AddDL(void) { Symbol *spr= sp--; sp->value.d+= spr->value.l; }
void AddDD(void) { Symbol *spr= sp--; sp->value.d+= spr->value.d; }
void AddDB(void)
{ Add_BB(FormOperandDS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void AddBI(void)
{ Add_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void AddBL(void)
{ Add_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void AddBD(void)
{ Add_BB(FormOperandDB(sp-1, &lop), FormOperandDS(sp, &rop)); }
void AddBB(void)
{ Add_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

static void Add_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (!ops) YError("bad data type(s) in binary +");
  ops->Add(l, r);
  Drop(1);
}

static void AddError(void);
static void AddError(void)
{ YError("operands not conformable in binary +"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]+rv[i]; \
    Popper(l->owner); \
  } else AddError(); }

OPERATION(AddC, char, PopTo)
OPERATION(AddS, short, PopTo)
OPERATION(AddI, int, PopToI)
OPERATION(AddL, long, PopToL)
OPERATION(AddF, float, PopTo)
OPERATION(AddD, double, PopToD)
void AddZ(Operand *l, Operand *r)
{ double *dst= BuildResult2(l, r);
  if (dst) {
    long i, n= 2*l->type.number;
    double *lv= l->value, *rv= r->value;
    for (i=0 ; i<n ; i++) dst[i]= lv[i]+rv[i];
    PopTo(l->owner);
  } else AddError();
}

/* + also represents string concatenation */
void AddQ(Operand *l, Operand *r)
{
  char **dst= BuildResult2(l, r);
  char *tmp;
  if (dst) {
    long i, n= l->type.number;
    char **lv= l->value, **rv= r->value;
    for (i=0 ; i<n ; i++) {
      tmp= dst[i];  /* may be lv[i] or rv[i] */
      dst[i]= p_strncat(lv[i], rv[i], 0);
      p_free(tmp);
    }
    PopTo(l->owner);
  } else AddError();
}

void AddX(Operand *l, Operand *r)
{ YError("non-numeric data type in binary +"); }

/*--------------------------------------------------------------------------*/
/* Subtract */

void Subtract(void) { (sp-1)->ops->Subtract[sp->ops->id](); }

static void Subtract_BB(Operand *l, Operand *r);

void SubtractII(void) { Symbol *spr= sp--; sp->value.i-= spr->value.i; }
void SubtractIL(void) { Symbol *spr= sp--;
  sp->value.l= sp->value.i-spr->value.l; sp->ops= &longScalar; }
void SubtractID(void) { Symbol *spr= sp--;
  sp->value.d= sp->value.i-spr->value.d; sp->ops= &doubleScalar; }
void SubtractIB(void)
{ Subtract_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void SubtractLI(void) { Symbol *spr= sp--; sp->value.l-= spr->value.i; }
void SubtractLL(void) { Symbol *spr= sp--; sp->value.l-= spr->value.l; }
void SubtractLD(void) { Symbol *spr= sp--;
  sp->value.d= sp->value.l-spr->value.d; sp->ops= &doubleScalar; }
void SubtractLB(void)
{ Subtract_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void SubtractDI(void) { Symbol *spr= sp--; sp->value.d-= spr->value.i; }
void SubtractDL(void) { Symbol *spr= sp--; sp->value.d-= spr->value.l; }
void SubtractDD(void) { Symbol *spr= sp--; sp->value.d-= spr->value.d; }
void SubtractDB(void)
{ Subtract_BB(FormOperandDS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void SubtractBI(void)
{ Subtract_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void SubtractBL(void)
{ Subtract_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void SubtractBD(void)
{ Subtract_BB(FormOperandDB(sp-1, &lop), FormOperandDS(sp, &rop)); }
void SubtractBB(void)
{ Subtract_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

static void Subtract_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (!ops) YError("bad data type(s) in binary -");
  ops->Subtract(l, r);
  Drop(1);
}

static void SubtractError(void);
static void SubtractError(void)
{ YError("operands not conformable in binary -"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]-rv[i]; \
    Popper(l->owner); \
  } else SubtractError(); }

OPERATION(SubtractC, char, PopTo)
OPERATION(SubtractS, short, PopTo)
OPERATION(SubtractI, int, PopToI)
OPERATION(SubtractL, long, PopToL)
OPERATION(SubtractF, float, PopTo)
OPERATION(SubtractD, double, PopToD)
void SubtractZ(Operand *l, Operand *r)
{ double *dst= BuildResult2(l, r);
  if (dst) {
    long i, n= 2*l->type.number;
    double *lv= l->value, *rv= r->value;
    for (i=0 ; i<n ; i++) dst[i]= lv[i]-rv[i];
    PopTo(l->owner);
  } else SubtractError();
}

void SubtractX(Operand *l, Operand *r)
{ YError("non-numeric data type in binary -"); }

/*--------------------------------------------------------------------------*/
/* Multiply */

void Multiply(void) { (sp-1)->ops->Multiply[sp->ops->id](); }

static void Multiply_BB(Operand *l, Operand *r);

void MultiplyII(void) { Symbol *spr= sp--; sp->value.i*= spr->value.i; }
void MultiplyIL(void) { Symbol *spr= sp--;
  sp->value.l= sp->value.i*spr->value.l; sp->ops= &longScalar; }
void MultiplyID(void) { Symbol *spr= sp--;
  sp->value.d= sp->value.i*spr->value.d; sp->ops= &doubleScalar; }
void MultiplyIB(void)
{ Multiply_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void MultiplyLI(void) { Symbol *spr= sp--; sp->value.l*= spr->value.i; }
void MultiplyLL(void) { Symbol *spr= sp--; sp->value.l*= spr->value.l; }
void MultiplyLD(void) { Symbol *spr= sp--;
  sp->value.d= sp->value.l*spr->value.d; sp->ops= &doubleScalar; }
void MultiplyLB(void)
{ Multiply_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void MultiplyDI(void) { Symbol *spr= sp--; sp->value.d*= spr->value.i; }
void MultiplyDL(void) { Symbol *spr= sp--; sp->value.d*= spr->value.l; }
void MultiplyDD(void) { Symbol *spr= sp--; sp->value.d*= spr->value.d; }
void MultiplyDB(void)
{ Multiply_BB(FormOperandDS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void MultiplyBI(void)
{ Multiply_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void MultiplyBL(void)
{ Multiply_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void MultiplyBD(void)
{ Multiply_BB(FormOperandDB(sp-1, &lop), FormOperandDS(sp, &rop)); }
void MultiplyBB(void)
{ Multiply_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

static void Multiply_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (!ops) YError("bad data type(s) in binary *");
  ops->Multiply(l, r);
  Drop(1);
}

static void MultiplyError(void);
static void MultiplyError(void)
{ YError("operands not conformable in binary *"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]*rv[i]; \
    Popper(l->owner); \
  } else MultiplyError(); }

OPERATION(MultiplyC, char, PopTo)
OPERATION(MultiplyS, short, PopTo)
OPERATION(MultiplyI, int, PopToI)
OPERATION(MultiplyL, long, PopToL)
OPERATION(MultiplyF, float, PopTo)
OPERATION(MultiplyD, double, PopToD)
void MultiplyZ(Operand *l, Operand *r)
{ double *dst= BuildResult2(l, r);
  if (dst) {
    long i, n= l->type.number;
    double *lv= l->value, *rv= r->value;
    double lr, li, rr, ri;  /* watch out for dst==lv or rv */
    for (i=0 ; i<n ; i++) {
      lr= lv[2*i];  li= lv[2*i+1];
      rr= rv[2*i];  ri= rv[2*i+1];
      dst[2*i]= lr*rr-li*ri;  dst[2*i+1]= lr*ri+li*rr;
    }
    PopTo(l->owner);
  } else MultiplyError();
}

void MultiplyX(Operand *l, Operand *r)
{ YError("non-numeric data type in binary *"); }

/*--------------------------------------------------------------------------*/
/* Divide */

void Divide(void) { (sp-1)->ops->Divide[sp->ops->id](); }

static void Divide_BB(Operand *l, Operand *r);

void DivideII(void) { Symbol *spr= sp--; sp->value.i/= spr->value.i; }
void DivideIL(void) { Symbol *spr= sp--;
  sp->value.l= sp->value.i/spr->value.l; sp->ops= &longScalar; }
void DivideID(void) { Symbol *spr= sp--;
  sp->value.d= sp->value.i/spr->value.d; sp->ops= &doubleScalar; }
void DivideIB(void)
{ Divide_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void DivideLI(void) { Symbol *spr= sp--; sp->value.l/= spr->value.i; }
void DivideLL(void) { Symbol *spr= sp--; sp->value.l/= spr->value.l; }
void DivideLD(void) { Symbol *spr= sp--;
  sp->value.d= sp->value.l/spr->value.d; sp->ops= &doubleScalar; }
void DivideLB(void)
{ Divide_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void DivideDI(void) { Symbol *spr= sp--; sp->value.d/= spr->value.i; }
void DivideDL(void) { Symbol *spr= sp--; sp->value.d/= spr->value.l; }
void DivideDD(void) { Symbol *spr= sp--; sp->value.d/= spr->value.d; }
void DivideDB(void)
{ Divide_BB(FormOperandDS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void DivideBI(void)
{ Divide_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void DivideBL(void)
{ Divide_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void DivideBD(void)
{ Divide_BB(FormOperandDB(sp-1, &lop), FormOperandDS(sp, &rop)); }
void DivideBB(void)
{ Divide_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

static void Divide_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (!ops) YError("bad data type(s) in binary /");
  ops->Divide(l, r);
  Drop(1);
}

static void DivideError(void);
static void DivideError(void)
{ YError("operands not conformable in binary /"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]/rv[i]; \
    Popper(l->owner); \
  } else DivideError(); }

OPERATION(DivideC, unsigned char, PopTo)
OPERATION(DivideS, short, PopTo)
OPERATION(DivideI, int, PopToI)
OPERATION(DivideL, long, PopToL)
OPERATION(DivideF, float, PopTo)
OPERATION(DivideD, double, PopToD)
void DivideZ(Operand *l, Operand *r)
{ double *dst= BuildResult2(l, r);
  if (dst) {
    long i, n = l->type.number;
    double *lv = l->value, *rv= r->value;
    double lr, li, rr, ri;               /* watch out for dst==lv or rv */
    for (i=0 ; i<n ; i++) {
      lr = lv[2*i];  li = lv[2*i+1];
      rr = rv[2*i];  ri = rv[2*i+1];
      if ((rr>0?rr:-rr)>(ri>0?ri:-ri)) { /* be careful about overflow... */
        ri/=rr; rr=1.0/((1.0+ri*ri)*rr);
        dst[2*i] = (lr+li*ri)*rr;
        dst[2*i+1] = (li-lr*ri)*rr;
      } else {
        rr/=ri; ri=1.0/((1.0+rr*rr)*ri);
        dst[2*i] = (lr*rr+li)*ri;
        dst[2*i+1] = (li*rr-lr)*ri;
      }
    }
    PopTo(l->owner);
  } else DivideError();
}

void DivideX(Operand *l, Operand *r)
{ YError("non-numeric data type in binary /"); }

/*--------------------------------------------------------------------------*/
/* Modulo */

void Modulo(void) { (sp-1)->ops->Modulo[sp->ops->id](); }

static void Modulo_BB(Operand *l, Operand *r);

void ModuloII(void) { Symbol *spr= sp--; sp->value.i%= spr->value.i; }
void ModuloIL(void) { Symbol *spr= sp--;
  sp->value.l= sp->value.i%spr->value.l; sp->ops= &longScalar; }
void ModuloID(void) { Symbol *spr= sp--;
  sp->value.d= RMOD(sp->value.i,spr->value.d); sp->ops= &doubleScalar; }
void ModuloIB(void)
{ Modulo_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void ModuloLI(void) { Symbol *spr= sp--; sp->value.l%= spr->value.i; }
void ModuloLL(void) { Symbol *spr= sp--; sp->value.l%= spr->value.l; }
void ModuloLD(void) { Symbol *spr= sp--;
  sp->value.d= RMOD(sp->value.l,spr->value.d); sp->ops= &doubleScalar; }
void ModuloLB(void)
{ Modulo_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void ModuloDI(void) { Symbol *spr= sp--;
                      sp->value.d= RMOD(sp->value.d, spr->value.i); }
void ModuloDL(void) { Symbol *spr= sp--;
                      sp->value.d= RMOD(sp->value.d, spr->value.l); }
void ModuloDD(void) { Symbol *spr= sp--;
                      sp->value.d= RMOD(sp->value.d, spr->value.d); }
void ModuloDB(void)
{ Modulo_BB(FormOperandDS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void ModuloBI(void)
{ Modulo_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void ModuloBL(void)
{ Modulo_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void ModuloBD(void)
{ Modulo_BB(FormOperandDB(sp-1, &lop), FormOperandDS(sp, &rop)); }
void ModuloBB(void)
{ Modulo_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

static void Modulo_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (!ops) YError("bad data type(s) in binary %");
  ops->Modulo(l, r);
  Drop(1);
}

static void ModuloError(void);
static void ModuloError(void)
{ YError("operands not conformable in binary %"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]%rv[i]; \
    Popper(l->owner); \
  } else ModuloError(); }

OPERATION(ModuloC, unsigned char, PopTo)
OPERATION(ModuloS, short, PopTo)
OPERATION(ModuloI, int, PopToI)
OPERATION(ModuloL, long, PopToL)

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= (typd)RMOD(lv[i],rv[i]); \
    Popper(l->owner); \
  } else ModuloError(); }

OPERATION(ModuloF, float, PopTo)
OPERATION(ModuloD, double, PopToD)
void ModuloZ(Operand *l, Operand *r)
{ YError("complex operand not allowed in binary %"); }

void ModuloX(Operand *l, Operand *r)
{ YError("non-numeric data type in binary %"); }

/*--------------------------------------------------------------------------*/
/* Power -- really two operations:
      raising to integer power leaves data type of left operand unchanged
      raising to real or complex power follows ordinary binary promotion
              rules
 */

void Power(void) { (sp-1)->ops->Power[sp->ops->id](); }

static void Power_BB(Operand *l, Operand *r);

void PowerII(void) { Symbol *spr= sp--;
  sp->value.i= powLL(sp->value.i, spr->value.i); }
void PowerIL(void) { Symbol *spr= sp--;
  sp->value.l= powLL(sp->value.i, spr->value.l); sp->ops= &longScalar; }
void PowerID(void) {
  Symbol *spr= sp--;
  errno = 0;
  sp->value.d= pow(sp->value.i, spr->value.d);
  if (errno) {
    if (errno!=ERANGE || sp->value.d!=0.)
      YError("mathlib pow() function signals error");
  }
  sp->ops= &doubleScalar;
}
void PowerIB(void)
{ Power_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void PowerLI(void) { Symbol *spr= sp--;
  sp->value.l= powLL(sp->value.l, spr->value.i); }
void PowerLL(void) { Symbol *spr= sp--;
  sp->value.l= powLL(sp->value.l, spr->value.l); }
void PowerLD(void) {
  Symbol *spr= sp--;
  errno = 0;
  sp->value.d= pow(sp->value.l, spr->value.d);
  if (errno) {
    if (errno!=ERANGE || sp->value.d!=0.)
      YError("mathlib pow() function signals error");
  }
  sp->ops= &doubleScalar;
}
void PowerLB(void)
{ Power_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void PowerDI(void) { Symbol *spr= sp--;
  sp->value.d= powDL(sp->value.d, spr->value.i); }
void PowerDL(void) { Symbol *spr= sp--;
  sp->value.d= powDL(sp->value.d, spr->value.l); }
void PowerDD(void) {
  Symbol *spr= sp--;
  errno = 0;
  sp->value.d= pow(sp->value.d, spr->value.d);
  if (errno) {
    if (errno!=ERANGE || sp->value.d!=0.)
      YError("mathlib pow() function signals error");
  }
}
void PowerDB(void)
{ Power_BB(FormOperandDS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void PowerBI(void)
{ Power_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void PowerBL(void)
{ Power_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void PowerBD(void)
{ Power_BB(FormOperandDB(sp-1, &lop), FormOperandDS(sp, &rop)); }
void PowerBB(void)
{ Power_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

static void Power_BB(Operand *l, Operand *r)
{
  int promoteID= r->ops->promoteID;
  if (promoteID<T_FLOAT && l->ops->promoteID<=T_COMPLEX) {
    /* raising to integer power does not change type of left operand,
       but right operand must always be long */
    r->ops->ToLong(r);
    l->ops->Power(l, r);
  } else {
    /* raising to non-integer power follows same promotion rules as
       all other arithmetic binary operators (only 3 possibilities) */
    Operations *ops= l->ops->Promote[promoteID](l, r);
    if (!ops) YError("bad data type(s) in binary ^");
    if (ops==&floatOps) PowerXF(l, r);
    else if (ops==&doubleOps) PowerXD(l, r);
    else if (ops==&complexOps) PowerXZ(l, r);
    else YError("bad data type(s) in binary ^");
  }
  Drop(1);
}

static void PowerError(void);
static void PowerError(void)
{ YError("operands not conformable in binary ^"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult1(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value; long *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= (typd)powLL(lv[i], rv[i]); \
    Popper(l->owner); \
  } else PowerError(); }

OPERATION(PowerC, char, PopTo)
OPERATION(PowerS, short, PopTo)
OPERATION(PowerI, int, PopToI)
OPERATION(PowerL, long, PopToL)

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult1(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value; long *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= (typd)powDL(lv[i], rv[i]); \
    Popper(l->owner); \
  } else PowerError(); }

OPERATION(PowerF, float, PopTo)
OPERATION(PowerD, double, PopToD)
void PowerZ(Operand *l, Operand *r)
{ double *dst= BuildResult1(l, r);
  if (dst) {
    long i, n= l->type.number;
    double *lv= l->value; long *rv= r->value;
    for (i=0 ; i<n ; i++) powZL(&dst[2*i], &lv[2*i], rv[i]);
    PopTo(l->owner);
  } else PowerError();
}

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0,errno=0 ; i<n ; i++) { dst[i]= (typd)pow(lv[i], rv[i]); \
    if (errno) { if (errno==ERANGE && !dst[i]) errno=0; \
                 else YError("mathlib pow() function signals error"); }} \
    Popper(l->owner); \
  } else PowerError(); }

OPERATION(PowerXF, float, PopTo)
OPERATION(PowerXD, double, PopToD)
void PowerXZ(Operand *l, Operand *r)
{ double *dst= BuildResult2(l, r);
  if (dst) {
    long i, n= 2*l->type.number;
    double *lv= l->value, *rv= r->value;
    for (i=0,errno=0 ; i<n && !errno ; i+=2) powZZ(&dst[i], &lv[i], &rv[i]);
    if (errno) YError("mathlib error in complex^complex");
    PopTo(l->owner);
  } else PowerError();
}

void PowerX(Operand *l, Operand *r)
{ YError("non-numeric data type in binary ^"); }

/*--------------------------------------------------------------------------*/
/* Greater */

void Greater(void) { (sp-1)->ops->Greater[sp->ops->id](); }

static void Greater_BB(Operand *l, Operand *r);

void GreaterII(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i>spr->value.i); }
void GreaterIL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i>spr->value.l); }
void GreaterID(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i>spr->value.d); }
void GreaterIB(void)
{ Greater_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void GreaterLI(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l>spr->value.i); sp->ops= &intScalar; }
void GreaterLL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l>spr->value.l); sp->ops= &intScalar; }
void GreaterLD(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l>spr->value.d); sp->ops= &intScalar; }
void GreaterLB(void)
{ Greater_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void GreaterDI(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d>spr->value.i); sp->ops= &intScalar; }
void GreaterDL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d>spr->value.l); sp->ops= &intScalar; }
void GreaterDD(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d>spr->value.d); sp->ops= &intScalar; }
void GreaterDB(void)
{ Greater_BB(FormOperandDS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void GreaterBI(void)
{ Greater_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void GreaterBL(void)
{ Greater_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void GreaterBD(void)
{ Greater_BB(FormOperandDB(sp-1, &lop), FormOperandDS(sp, &rop)); }
void GreaterBB(void)
{ Greater_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

static void Greater_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (!ops) YError("bad data type(s) in binary >");
  ops->Greater(l, r);
  Drop(1);
}

static void GreaterError(void);
static void GreaterError(void)
{ YError("operands not conformable in binary > or <"); }

#undef OPERATION
#define OPERATION(opname, typd) \
void opname(Operand *l, Operand *r) \
{ int *dst= BuildResult0(l, r, &intStruct); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]>rv[i]; \
    PopToI(l->owner); \
  } else GreaterError(); }

OPERATION(GreaterC, unsigned char)
OPERATION(GreaterS, short)
OPERATION(GreaterI, int)
OPERATION(GreaterL, long)
OPERATION(GreaterF, float)
OPERATION(GreaterD, double)
void GreaterZ(Operand *l, Operand *r)
{ YError("complex operand not allowed in binary > or <"); }

void GreaterQ(Operand *l, Operand *r)
{
  int *dst= BuildResult0(l, r, &intStruct);
  if (dst) {
    long i, n= l->type.number;
    char **lv= l->value, **rv= r->value, *ls, *rs;
    for (i=0 ; i<n ; i++) {
      ls= lv[i];  rs= rv[i];
      if (ls && rs) dst[i]= strcmp(ls, rs)>0;
      else dst[i]= (ls && !rs);
    }
    PopTo(l->owner);
  } else GreaterError();
}

void GreaterX(Operand *l, Operand *r)
{ YError("non-numeric data type in binary > or <"); }

/*--------------------------------------------------------------------------*/
/* Less */

void Less(void) { (sp-1)->ops->Less[sp->ops->id](); }

static void Less_BB(Operand *l, Operand *r);

void LessII(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i<spr->value.i); }
void LessIL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i<spr->value.l); }
void LessID(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i<spr->value.d); }
void LessIB(void)
{ Less_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void LessLI(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l<spr->value.i); sp->ops= &intScalar; }
void LessLL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l<spr->value.l); sp->ops= &intScalar; }
void LessLD(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l<spr->value.d); sp->ops= &intScalar; }
void LessLB(void)
{ Less_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void LessDI(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d<spr->value.i); sp->ops= &intScalar; }
void LessDL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d<spr->value.l); sp->ops= &intScalar; }
void LessDD(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d<spr->value.d); sp->ops= &intScalar; }
void LessDB(void)
{ Less_BB(FormOperandDS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void LessBI(void)
{ Less_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void LessBL(void)
{ Less_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void LessBD(void)
{ Less_BB(FormOperandDB(sp-1, &lop), FormOperandDS(sp, &rop)); }
void LessBB(void)
{ Less_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

static void Less_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (!ops) YError("bad data type(s) in binary <");
  ops->Greater(r, l);
  PopTo(sp-1);   /* instead of Drop(1), since args reversed */
}

/*--------------------------------------------------------------------------*/
/* GreaterEQ */

void GreaterEQ(void) { (sp-1)->ops->GreaterEQ[sp->ops->id](); }

static void GreaterEQ_BB(Operand *l, Operand *r);

void GreaterEQII(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i>=spr->value.i); }
void GreaterEQIL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i>=spr->value.l); }
void GreaterEQID(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i>=spr->value.d); }
void GreaterEQIB(void)
{ GreaterEQ_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void GreaterEQLI(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l>=spr->value.i); sp->ops= &intScalar; }
void GreaterEQLL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l>=spr->value.l); sp->ops= &intScalar; }
void GreaterEQLD(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l>=spr->value.d); sp->ops= &intScalar; }
void GreaterEQLB(void)
{ GreaterEQ_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void GreaterEQDI(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d>=spr->value.i); sp->ops= &intScalar; }
void GreaterEQDL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d>=spr->value.l); sp->ops= &intScalar; }
void GreaterEQDD(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d>=spr->value.d); sp->ops= &intScalar; }
void GreaterEQDB(void)
{ GreaterEQ_BB(FormOperandDS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void GreaterEQBI(void)
{ GreaterEQ_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void GreaterEQBL(void)
{ GreaterEQ_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void GreaterEQBD(void)
{ GreaterEQ_BB(FormOperandDB(sp-1, &lop), FormOperandDS(sp, &rop)); }
void GreaterEQBB(void)
{ GreaterEQ_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

static void GreaterEQ_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (!ops) YError("bad data type(s) in binary >=");
  ops->GreaterEQ(l, r);
  Drop(1);
}

static void GreaterEQError(void);
static void GreaterEQError(void)
{ YError("operands not conformable in binary >= or <="); }

#undef OPERATION
#define OPERATION(opname, typd) \
void opname(Operand *l, Operand *r) \
{ int *dst= BuildResult0(l, r, &intStruct); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]>=rv[i]; \
    PopToI(l->owner); \
  } else GreaterEQError(); }

OPERATION(GreaterEQC, unsigned char)
OPERATION(GreaterEQS, short)
OPERATION(GreaterEQI, int)
OPERATION(GreaterEQL, long)
OPERATION(GreaterEQF, float)
OPERATION(GreaterEQD, double)
void GreaterEQZ(Operand *l, Operand *r)
{ YError("complex operand not allowed in binary >= or <="); }

void GreaterEQQ(Operand *l, Operand *r)
{
  int *dst= BuildResult0(l, r, &intStruct);
  if (dst) {
    long i, n= l->type.number;
    char **lv= l->value, **rv= r->value, *ls, *rs;
    for (i=0 ; i<n ; i++) {
      ls= lv[i];  rs= rv[i];
      if (ls && rs) dst[i]= strcmp(ls, rs)>=0;
      else dst[i]= (ls && !rs);
    }
    PopTo(l->owner);
  } else GreaterEQError();
}

void GreaterEQX(Operand *l, Operand *r)
{ YError("non-numeric data type in binary >= or <="); }

/*--------------------------------------------------------------------------*/
/* LessEQ */

void LessEQ(void) { (sp-1)->ops->LessEQ[sp->ops->id](); }

static void LessEQ_BB(Operand *l, Operand *r);

void LessEQII(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i<=spr->value.i); }
void LessEQIL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i<=spr->value.l); }
void LessEQID(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i<=spr->value.d); }
void LessEQIB(void)
{ LessEQ_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void LessEQLI(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l<=spr->value.i); sp->ops= &intScalar; }
void LessEQLL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l<=spr->value.l); sp->ops= &intScalar; }
void LessEQLD(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l<=spr->value.d); sp->ops= &intScalar; }
void LessEQLB(void)
{ LessEQ_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void LessEQDI(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d<=spr->value.i); sp->ops= &intScalar; }
void LessEQDL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d<=spr->value.l); sp->ops= &intScalar; }
void LessEQDD(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d<=spr->value.d); sp->ops= &intScalar; }
void LessEQDB(void)
{ LessEQ_BB(FormOperandDS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void LessEQBI(void)
{ LessEQ_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void LessEQBL(void)
{ LessEQ_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void LessEQBD(void)
{ LessEQ_BB(FormOperandDB(sp-1, &lop), FormOperandDS(sp, &rop)); }
void LessEQBB(void)
{ LessEQ_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

static void LessEQ_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (!ops) YError("bad data type(s) in binary <=");
  ops->GreaterEQ(r, l);
  PopTo(sp-1);   /* instead of Drop(1), since args reversed */
}

/*--------------------------------------------------------------------------*/
/* Equal */

void Equal(void) { (sp-1)->ops->Equal[sp->ops->id](); }

static void Equal_BB(Operand *l, Operand *r);

void EqualII(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i==spr->value.i); }
void EqualIL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i==spr->value.l); }
void EqualID(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i==spr->value.d); }
void EqualIB(void)
{ Equal_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void EqualLI(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l==spr->value.i); sp->ops= &intScalar; }
void EqualLL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l==spr->value.l); sp->ops= &intScalar; }
void EqualLD(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l==spr->value.d); sp->ops= &intScalar; }
void EqualLB(void)
{ Equal_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void EqualDI(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d==spr->value.i); sp->ops= &intScalar; }
void EqualDL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d==spr->value.l); sp->ops= &intScalar; }
void EqualDD(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d==spr->value.d); sp->ops= &intScalar; }
void EqualDB(void)
{ Equal_BB(FormOperandDS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void EqualBI(void)
{ Equal_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void EqualBL(void)
{ Equal_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void EqualBD(void)
{ Equal_BB(FormOperandDB(sp-1, &lop), FormOperandDS(sp, &rop)); }
void EqualBB(void)
{ Equal_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

static void Equal_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (ops) {
    ops->Equal(l, r);
    Drop(1);
  } else {
    /* If the data types don't match, the operands certainly aren't == */
    /* Can pointer or string be ==0? */
    Drop(2);
    PushIntValue(0);
  }
}

static void EqualError(void);
static void EqualError(void)
{ YError("operands not conformable in binary =="); }

#undef OPERATION
#define OPERATION(opname, typd) \
void opname(Operand *l, Operand *r) \
{ int *dst= BuildResult0(l, r, &intStruct); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value; typd *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]==rv[i]; \
    PopToI(l->owner); \
  } else EqualError(); }

OPERATION(EqualC, char)
OPERATION(EqualS, short)
OPERATION(EqualI, int)
OPERATION(EqualL, long)
OPERATION(EqualF, float)
OPERATION(EqualD, double)
void EqualZ(Operand *l, Operand *r)
{ int *dst= BuildResult0(l, r, &intStruct);
  if (dst) {
    long i, n= l->type.number;
    double *lv= l->value, *rv= r->value;
    for (i=0 ; i<n ; i++) dst[i]= (lv[2*i]==rv[2*i] && lv[2*i+1]==rv[2*i+1]);
    PopToI(l->owner);
  } else EqualError();
}

void EqualQ(Operand *l, Operand *r)
{
  int *dst= BuildResult0(l, r, &intStruct);
  if (dst) {
    long i, n= l->type.number;
    char **lv= l->value, **rv= r->value, *ls, *rs;
    for (i=0 ; i<n ; i++) {
      ls= lv[i];  rs= rv[i];
      if (ls && rs) dst[i]= strcmp(ls, rs)==0;
      else dst[i]= (ls==rs);
    }
    PopToI(l->owner);
  } else EqualError();
}

OPERATION(EqualP, void *)

void EqualSI(Operand *l, Operand *r)
{
  int *dst= BuildResult0(l, r, &intStruct);
  if (dst) {
    long i, n= l->type.number;
    if (StructEqual(l->type.base, r->type.base)) {
      long size= l->type.base->size;
      char *lv= l->value, *rv= r->value;
      for (i=0 ; i<n ; i++) {
        dst[i]= memcmp(lv, rv, size)==0;
        lv+= size;
        rv+= size;
      }
    } else {
      for (i=0 ; i<n ; i++) dst[i]= 0;
    }
    PopToI(l->owner);
  } else EqualError();
}

void EqualR(Operand *l, Operand *r)
{
  Range *lop= (Range *)l->owner->value.db, *rop= (Range *)r->owner->value.db;
  long lmin= lop->min, linc= lop->inc;
  int value= (lop->nilFlags==rop->nilFlags) && (lmin==rop->min) &&
    (linc==rop->inc) && ((lop->max-lmin)/linc == (rop->max-lmin)/linc);
  PushIntValue(value);
  PopToI(l->owner);
}

void EqualX(Operand *l, Operand *r)
{
  DataBlock *lop= l->owner->value.db,  *rop= r->owner->value.db;
  PushIntValue(lop==rop);
  PopToI(l->owner);
}

/*--------------------------------------------------------------------------*/
/* NotEqual */

void NotEqual(void) { (sp-1)->ops->NotEqual[sp->ops->id](); }

static void NotEqual_BB(Operand *l, Operand *r);

void NotEqualII(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i!=spr->value.i); }
void NotEqualIL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i!=spr->value.l); }
void NotEqualID(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.i!=spr->value.d); }
void NotEqualIB(void)
{ NotEqual_BB(FormOperandIS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void NotEqualLI(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l!=spr->value.i); sp->ops= &intScalar; }
void NotEqualLL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l!=spr->value.l); sp->ops= &intScalar; }
void NotEqualLD(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.l!=spr->value.d); sp->ops= &intScalar; }
void NotEqualLB(void)
{ NotEqual_BB(FormOperandLS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void NotEqualDI(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d!=spr->value.i); sp->ops= &intScalar; }
void NotEqualDL(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d!=spr->value.l); sp->ops= &intScalar; }
void NotEqualDD(void) { Symbol *spr= sp--;
  sp->value.i= (sp->value.d!=spr->value.d); sp->ops= &intScalar; }
void NotEqualDB(void)
{ NotEqual_BB(FormOperandDS(sp-1, &lop), FormOperandDB(sp, &rop)); }

void NotEqualBI(void)
{ NotEqual_BB(FormOperandDB(sp-1, &lop), FormOperandIS(sp, &rop)); }
void NotEqualBL(void)
{ NotEqual_BB(FormOperandDB(sp-1, &lop), FormOperandLS(sp, &rop)); }
void NotEqualBD(void)
{ NotEqual_BB(FormOperandDB(sp-1, &lop), FormOperandDS(sp, &rop)); }
void NotEqualBB(void)
{ NotEqual_BB(FormOperandDB(sp-1, &lop), FormOperandDB(sp, &rop)); }

static void NotEqual_BB(Operand *l, Operand *r)
{
  Operations *ops= l->ops->Promote[r->ops->promoteID](l, r);
  if (ops) {
    ops->NotEqual(l, r);
    Drop(1);
  } else {
    /* If the data types don't match, the operands certainly are != */
    /* Can pointer or string be !=0? */
    Drop(2);
    PushIntValue(1);
  }
}

static void NotEqualError(void);
static void NotEqualError(void)
{ YError("operands not conformable in binary !="); }

#undef OPERATION
#define OPERATION(opname, typd) \
void opname(Operand *l, Operand *r) \
{ int *dst= BuildResult0(l, r, &intStruct); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value; typd *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]!=rv[i]; \
    PopToI(l->owner); \
  } else NotEqualError(); }

OPERATION(NotEqualC, char)
OPERATION(NotEqualS, short)
OPERATION(NotEqualI, int)
OPERATION(NotEqualL, long)
OPERATION(NotEqualF, float)
OPERATION(NotEqualD, double)
void NotEqualZ(Operand *l, Operand *r)
{ int *dst= BuildResult0(l, r, &intStruct);
  if (dst) {
    long i, n= l->type.number;
    double *lv= l->value, *rv= r->value;
    for (i=0 ; i<n ; i++) dst[i]= (lv[2*i]!=rv[2*i] || lv[2*i+1]!=rv[2*i+1]);
    PopToI(l->owner);
  } else NotEqualError();
}

void NotEqualQ(Operand *l, Operand *r)
{
  int *dst= BuildResult0(l, r, &intStruct);
  if (dst) {
    long i, n= l->type.number;
    char **lv= l->value, **rv= r->value, *ls, *rs;
    for (i=0 ; i<n ; i++) {
      ls= lv[i];  rs= rv[i];
      if (ls && rs) dst[i]= strcmp(ls, rs)!=0;
      else dst[i]= (ls!=rs);
    }
    PopToI(l->owner);
  } else NotEqualError();
}

OPERATION(NotEqualP, void *)

void NotEqualSI(Operand *l, Operand *r)
{
  int *dst= BuildResult0(l, r, &intStruct);
  if (dst) {
    long i, n= l->type.number;
    if (StructEqual(l->type.base, r->type.base)) {
      long size= l->type.base->size;
      char *lv= l->value, *rv= r->value;
      for (i=0 ; i<n ; i++) {
        dst[i]= memcmp(lv, rv, size)!=0;
        lv+= size;
        rv+= size;
      }
    } else {
      for (i=0 ; i<n ; i++) dst[i]= 1;
    }
    PopToI(l->owner);
  } else NotEqualError();
}

void NotEqualR(Operand *l, Operand *r)
{
  Range *lop= (Range *)l->owner->value.db, *rop= (Range *)r->owner->value.db;
  long lmin= lop->min, linc= lop->inc;
  int value= (lop->nilFlags!=rop->nilFlags) || (lmin!=rop->min) ||
    (linc!=rop->inc) || ((lop->max-lmin)/linc != (rop->max-lmin)/linc);
  PushIntValue(value);
  PopToI(l->owner);
}

void NotEqualX(Operand *l, Operand *r)
{
  DataBlock *lop= l->owner->value.db,  *rop= r->owner->value.db;
  PushIntValue(lop!=rop);
  PopToI(l->owner);
}

/*--------------------------------------------------------------------------*/
