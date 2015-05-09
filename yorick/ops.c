/*
 * $Id: ops.c,v 1.4 2006-07-20 04:28:30 dhmunro Exp $
 *
 * Define miscellaneous virtual machine functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "bcast.h"
#include "pstdlib.h"
#include "play.h"
#include <string.h>

/* ------------------------------------------------------------------------ */

extern BuiltIn Y_system;

extern VMaction PushChar, PushShort, PushInt, PushLong,
  PushFloat, PushDouble, PushImaginary, PushString, Push0, Push1;
extern VMaction PushVariable, PushReference, PushNil, FormKeyword,
  FormRange, FormRangeFunc, FormRangeFlag, AddRangeFunc, AddRangeFlag;
extern VMaction DupUnder, EvalUnder, DropTop;
extern VMaction AndOrLogical, BranchFalse, BranchTrue, Branch;
extern VMaction Build, MatrixMult, CallShell;

extern UnaryOp EvalSD;

extern BinaryOp MatMultC, MatMultS, MatMultI, MatMultL,
                MatMultF, MatMultD, MatMultZ, MatMultX;

/* The branching functions call the True action.  */
extern VMaction True;

extern Operand *FormOperandDB(Symbol *owner, Operand *op);

extern void PopToI(Symbol *s);
extern void PopToL(Symbol *s);
extern void PopToD(Symbol *s);

/* ------------------------------------------------------------------------ */

void PushChar(void)
{
  Array *array= PushDataBlock(NewArray(&charStruct, (Dimension *)0));
  Symbol *cnst= (pc++)->constant;
  array->value.c[0]= (unsigned char)cnst->value.l;
}

void PushShort(void)
{
  Array *array= PushDataBlock(NewArray(&shortStruct, (Dimension *)0));
  Symbol *cnst= (pc++)->constant;
  array->value.s[0]= (short)cnst->value.l;
}

void PushInt(void)
{
  Symbol *cnst= (pc++)->constant;
  Symbol *spp= sp+1;
  spp->ops= &intScalar;
  spp->value.i= (int)cnst->value.l;
  sp= spp;
}

void PushLong(void)
{
  Symbol *cnst= (pc++)->constant;
  Symbol *spp= sp+1;
  spp->ops= &longScalar;
  spp->value.l= cnst->value.l;
  sp= spp;
}

void PushFloat(void)
{
  Array *array= PushDataBlock(NewArray(&floatStruct, (Dimension *)0));
  Symbol *cnst= (pc++)->constant;
  array->value.f[0]= (float)cnst->value.d;
}

void PushDouble(void)
{
  Symbol *cnst= (pc++)->constant;
  Symbol *spp= sp+1;
  spp->ops= &doubleScalar;
  spp->value.d= cnst->value.d;
  sp= spp;
}

void PushImaginary(void)
{
  Array *array= PushDataBlock(NewArray(&complexStruct, (Dimension *)0));
  Symbol *cnst= (pc++)->constant;
  array->value.d[0]= 0.0;
  array->value.d[1]= cnst->value.d;
}

void PushString(void)
{
  Array *array= PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  Symbol *cnst= (pc++)->constant;
  Array *string= (Array *)cnst->value.db;
  array->value.q[0]= p_strcpy(string->value.q[0]);
}

void Push0(void)
{
  Symbol *spp= sp+1;
  spp->ops= &intScalar;
  spp->value.i= 0;
  sp= spp;
}

void Push1(void)
{
  Symbol *spp= sp+1;
  spp->ops= &intScalar;
  spp->value.i= 1;
  sp= spp;
}

void PushVariable(void)
{
  Symbol *var= &globTab[(pc++)->index];
  Symbol *spp= sp+1;
  spp->ops= var->ops;
  if (var->ops!=&dataBlockSym) spp->value= var->value;
  else spp->value.db= Ref(var->value.db);
    /* might be useful to have a version which didn't do the
       Ref(), but set var->ops to voidOps */
  sp= spp;
}

void PushReference(void)
{
  Symbol *spp= sp+1;
  spp->ops= &referenceSym;
  spp->index= (pc++)->index;
  sp= spp;
}

void PushNil(void)
{
  Symbol *spp= sp+1;
  spp->ops= &dataBlockSym;
  spp->value.db= RefNC(&nilDB);
  sp= spp;
}

void DropTop(void)
{
  Drop(1);
}

void DupUnder(void)
{
  /* used for prefix increment operators acting on LValue
     initially, stack is (LValue,increment) -->
     after this function (LValue,copy of LValue,increment)  */
  Symbol *spp= sp+1, *spm= sp-1;
  spp->ops= sp->ops;
  spp->value= sp->value;
  sp->ops= &intScalar;
  if (spm->ops!=&dataBlockSym) sp->value= spm->value;
  else sp->value.db= Ref(spm->value.db);
  sp->ops= spm->ops;
  sp= spp;
}

void EvalUnder(void)
{
  /* used for postfix increment operators acting on LValue
     initially, stack is (LValue,increment) -->
     after this (fetched LV,original LV,fetched LV,increment)  */
  Symbol *spp2= sp+2, *spp= sp+1, *spm= sp-1;
  spp2->ops= sp->ops;
  spp2->value= sp->value;
  sp->ops= &intScalar;
  if (spm->ops!=&dataBlockSym) {
    sp->value= spp->value= spm->value;
  } else {
    sp->value.db= Ref(spm->value.db);
    if (spm->value.db->ops==&lvalueOps) FetchLValue(spm->value.db, spm);
    spp->value.db= Ref(spm->value.db);
  }
  sp->ops= spp->ops= spm->ops;
  sp= spp2;
}

/* ------------------------------------------------------------------------ */

void FormKeyword(void)
{
  Symbol *spp= sp+1;
  spp->ops= sp->ops;
  spp->value= sp->value;
  sp->ops= &intScalar;
  sp->index= (pc++)->index;
  sp->ops= 0;                /* special marker for keyword */
  sp= spp;
}

void FormRange(void)
{
  int count= (pc++)->count;   /* 2 (min:max) or 3 (min:max:inc) on stack */
  int flags= 0;
  long values[3];
  values[0]= values[1]= 0;  values[2]= 1;
  while (count--) {
    flags<<= 1;
    if (sp->ops==&dataBlockSym && sp->value.db==&nilDB) flags|= 1;
    else values[count]= YGetInteger(sp);
    Drop(1);
  }
  flags&= 3;
  if (values[2]==0) {
    if (flags || values[1]!=values[0])
      YError("0 step in index range (start:stop:step)");
    values[2]= 1;
  }
  PushDataBlock(NewRange(values[0], values[1], values[2], flags));
}

void FormRangeFunc(void)
{
  Range *range= PushDataBlock(NewRange(0L, 0L, 1L, R_MINNIL|R_MAXNIL));
  range->rf= (pc++)->rf;
}

void FormRangeFlag(void)
{
  int flag= (pc++)->count;
  PushDataBlock(NewRange(0L, 0L, 1L, R_MINNIL|R_MAXNIL|flag));
}

void AddRangeFunc(void)
{
  Range *range= (Range *)sp->value.db;
  range->rf= (pc++)->rf;
}

void AddRangeFlag(void)
{
  Range *range= (Range *)sp->value.db;
  range->nilFlags|= (pc++)->count;
}

/* ------------------------------------------------------------------------ */

void AndOrLogical(void)
{
  True();        /* test 2nd argument to && or || */
  if (sp->ops!=&intScalar) YError("non-scalar right operand to && or ||");
  pc++;          /* skip Push0 (&&) or Push1 (||) instruction */
}

void BranchFalse(void)
{
  P_SOFTFPE_TEST;
  True();        /* test branch condition */
  if (sp->ops!=&intScalar) YError("non-scalar operand to BranchFalse");
  if (!sp->value.i) pc+= pc->displace;
  else pc++;
  Drop(1);
}

void BranchTrue(void)
{
  P_SOFTFPE_TEST;
  True();        /* test branch condition */
  if (sp->ops!=&intScalar) YError("non-scalar operand to BranchTrue");
  if (sp->value.i) pc+= pc->displace;
  else pc++;
  Drop(1);
}

void Branch(void)
{
  P_SOFTFPE_TEST;
  pc+= pc->displace;
}

/* ------------------------------------------------------------------------ */

static Operand *buildOps= 0;
static int nBuildOps= 0;
static Dimension *buildDims= 0;

void Build(void)
{
  int i, nArgs= (pc++)->count;
  Symbol *first= sp-nArgs+1;
  int promoteID= 0, needPromote= 0;
  Operations *ops;
  StructDef *base= 0;
  Array *result;
  char *rslt;
  long size;

  /* get space for nArgs operands */
  if (nArgs>nBuildOps) {
    buildOps= p_realloc(buildOps, sizeof(Operand)*nArgs);
    nBuildOps= nArgs;
  }

  /* first pass gets operands, determines dimensions, detects errors */
  for (i=0 ; i<nArgs ; i++) {
    first[i].ops->FormOperand(&first[i], &buildOps[i]);
    ops= buildOps[i].ops;
    if (!ops->isArray)
      YError("non-array argument to build array operator [...]");
    if (i) {
      if (Conform(buildDims, buildOps[i].type.dims) & 4)
        YError("non-conformable arguments to build array operator [...]");
      FreeDimension(buildDims);
      buildDims= Ref(tmpDims);
      if (buildOps[i].type.base==base ||
          ((ops->promoteID<=T_COMPLEX) && (promoteID<=T_COMPLEX))) {
        if (ops->promoteID!=promoteID) needPromote= 1;
        if (ops->promoteID>promoteID) promoteID= ops->promoteID;
      } else {
        YError("mixed struct/pointer/string arguments to [...]");
      }
    } else {
      FreeDimension(buildDims);
      buildDims= Ref(buildOps[i].type.dims);
      promoteID= ops->promoteID;
      base= buildOps[i].type.base;
    }
  }

  /* assure that all operands have the same data type */
  if (needPromote) {
    Operand phony;
    phony.ops= 0;   /* Promote will return this, but the result of Promote
                       is dicarded here, so no need to properly initialize */
    for (i=0 ; i<nArgs ; i++) {
      buildOps[i].ops->Promote[promoteID](&buildOps[i], &phony);
      first[i].ops->FormOperand(&first[i], &buildOps[i]);
    }
  }

  /* create result array on top of stack */
  result= PushDataBlock(NewArray(buildOps[0].type.base,
                  NewDimension(nArgs, 1L, Ref(buildDims))));
  result->type.dims->references--;

  /* copy each component of result into place */
  rslt= result->value.c;
  size= result->type.base->size*result->type.number/nArgs;
  for (i=0 ; i<nArgs ; i++) {
    Broadcast(rslt, buildDims, buildOps[i].value, buildOps[i].type.dims,
              buildOps[i].type.base);
    rslt+= size;
  }

  /* pop result into place and clean up stack */
  PopTo(first);   /* result can never be a scalar! */
  Drop(nArgs-1);

  /* free operand space if it is significant */
  if (nBuildOps>32) {
    p_free(buildOps);
    buildOps= 0;
    nBuildOps= 0;
  }
}

/* ------------------------------------------------------------------------ */

/* Scorecard:
   result(l,k,j,i)= lop(l,+,k)*rop(j,+,i)
     where lengths are l_len, k_len, j_len, i_len, and +_len
   lx= l_len
   kx= l_len*k_len               mx= l_len*+_len
   jx= l_len*k_len*j_len         mri= j_len
   ix= l_len*k_len*j_len*i_len   iri= j_len*+_len
 */

static long mx, kx, lx, jx, ix;
static void *mmdst;

void MatrixMult(void)
{
  Operand lop, rop;
  int lindex= (sp-2)->value.i;
  int rindex= (sp  )->value.i;
  Array *result;
  Dimension *dims, *prev, *next;
  int rindex0= rindex;
  Operations *ops;

  if ((sp-1)->ops!=&dataBlockSym || (sp-3)->ops!=&dataBlockSym) {
    ops= 0;
  } else {
    FormOperandDB(sp-3, &lop);
    FormOperandDB(sp-1, &rop);
    ops= lop.ops->Promote[rop.ops->promoteID](&lop, &rop);
  }

  if (!ops) YError("illegal data type for matrix multiply operand(s)");

  dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  lx= kx= jx= ix= 1;

  dims= lop.type.dims;
  lindex= CountDims(dims) - lindex - 1;
  dims= tmpDims= CopyDims(dims, tmpDims, 1);
  prev= 0;
  while (lindex--) {
    prev= dims;
    kx*= dims->number;
    dims= dims->next;
  }
  next= dims->next;
  if (prev) prev->next= next;
  else tmpDims= next;
  dims->next= 0;
  mx= dims->number;
  FreeDimension(dims);
  while (next) {
    lx*= next->number;
    next= next->next;
  }

  dims= rop.type.dims;
  rindex= CountDims(dims) - rindex - 1;
  dims= tmpDims= CopyDims(dims, tmpDims, 1);
  prev= 0;
  while (rindex--) {
    prev= dims;
    ix*= dims->number;
    dims= dims->next;
  }
  next= dims->next;
  if (prev) prev->next= next;
  else tmpDims= next;
  dims->next= 0;
  mx= (dims->number==mx)? mx : 0;
  FreeDimension(dims);
  while (rindex0--) {
    jx*= next->number;
    next= next->next;
  }

  if (!mx) YError("marked indices not same length in matrix multiply");

  /* Unlike other BinaryOps, MatMult functions do NOT create their own
     result (they are not as self-contained as the others anyway).  */
  Drop(1);  /* no need for rindex operand now */
  result= PushDataBlock(NewArray(lop.type.base, tmpDims));
  mmdst= result->value.c;

  ops->MatMult(&lop, &rop);
  Drop(2);
}

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *lop, Operand *rop) \
{ long i, j, k, l, mr, lx0; \
  typd *dst=mmdst, *lv=lop->value, *rv=rop->value, *lvj,*rvj,rv0; \
  if (lx>1) { typd *dkji=dst; lx0=lx&3; mx*=jx; \
    for (i=0 ; i<ix ; i++,rv+=mx) { \
      for (j=0,lvj=lv,rvj=rv ; j<jx ; j++,rvj++,lvj=lv) { \
        for (k=0 ; k<kx ; k++,dst=dkji) { \
          for (l=0 ; l<lx0 ; l++,dkji++) dkji[0]= 0; \
          for ( ; l<lx ; l+=4,dkji+=4) \
            dkji[0]= dkji[1]= dkji[2]= dkji[3]= 0; \
          for (mr=0,dkji=dst ; mr<mx ; mr+=jx) { \
            dkji= dst;  rv0= rvj[mr]; \
            for (l=0 ; l<lx0 ; l++,dkji++,lvj++) \
              dkji[0]+= lvj[0]*rv0; \
            for ( ; l<lx ; l+=4,dkji+=4,lvj+=4) { \
              dkji[0]+= lvj[0]*rv0;  dkji[1]+= lvj[1]*rv0; \
              dkji[2]+= lvj[2]*rv0;  dkji[3]+= lvj[3]*rv0; }}}}} \
  } else { long mjx=jx*mx, jx2=jx+jx, jx3=jx2+jx; lx=jx3+jx; lx0=mx&3; \
    for (i=0 ; i<ix ; i++,rv+=mjx) { \
      for (j=0,lvj=lv,rvj=rv ; j<jx ; j++,rvj++,lvj=lv) { \
        for (k=0 ; k<kx ; k++,dst++) { \
          rv0= 0; \
          for (mr=l=0 ; l<lx0 ; l++,mr+=jx,lvj++) rv0+= lvj[0]*rvj[mr]; \
          for ( ; l<mx ; l+=4,mr+=lx,lvj+=4) \
            rv0+= lvj[0]*rvj[mr]+lvj[1]*rvj[mr+jx]+ \
                  lvj[2]*rvj[mr+jx2]+lvj[3]*rvj[mr+jx3]; \
          dst[0]= rv0; }}}} \
  Popper(sp-3); }

OPERATION(MatMultC, char, PopTo)
OPERATION(MatMultS, short, PopTo)
OPERATION(MatMultI, int, PopToI)
OPERATION(MatMultL, long, PopToL)
OPERATION(MatMultF, float, PopTo)
OPERATION(MatMultD, double, PopToD)

void MatMultZ(Operand *lop, Operand *rop)
{
  long i, j, k, l, mr, lx0;
  double *dst=mmdst, *lv= lop->value, *rv= rop->value;
  double *lvj, *rvj, rv0, rv1;
  jx*= 2;
  if (lx>1) {
    double *dkji= dst;
    lx0= lx&3;
    mx*= jx;
    for (i=0 ; i<ix ; i++,rv+=mx) {
      for (j=0,lvj=lv,rvj=rv ; j<jx ; j+=2,rvj+=2,lvj=lv) {
        for (k=0 ; k<kx ; k++,dst=dkji) {
          for (l=0 ; l<lx0 ; l++,dkji+=2) dkji[0]= dkji[1]= 0.0;
          for ( ; l<lx ; l+=4,dkji+=8)
            dkji[0]= dkji[1]= dkji[2]= dkji[3]=
              dkji[4]= dkji[5]= dkji[6]= dkji[7]= 0.0;
          for (mr=0 ; mr<mx ; mr+=jx) {
            dkji= dst;
            rv0= rvj[mr];  rv1= rvj[mr+1];
            for (l=0 ; l<lx0 ; l++,dkji+=2,lvj+=2) {
              dkji[0]+= lvj[0]*rv0 - lvj[1]*rv1;
              dkji[1]+= lvj[1]*rv0 + lvj[0]*rv1;
            }
            for ( ; l<lx ; l+=4,dkji+=8,lvj+=8) {
              dkji[0]+=lvj[0]*rv0-lvj[1]*rv1;  dkji[1]+=lvj[1]*rv0+lvj[0]*rv1;
              dkji[2]+=lvj[2]*rv0-lvj[3]*rv1;  dkji[3]+=lvj[3]*rv0+lvj[2]*rv1;
              dkji[4]+=lvj[4]*rv0-lvj[5]*rv1;  dkji[5]+=lvj[5]*rv0+lvj[4]*rv1;
              dkji[6]+=lvj[6]*rv0-lvj[7]*rv1;  dkji[7]+=lvj[7]*rv0+lvj[6]*rv1;
            }
          }
        }
      }
    }
  } else {
    long mx2=mx*jx, jx2=jx+jx, jx3=jx2+jx;
    lx= jx3+jx;
    lx0= mx&3;
    for (i=0 ; i<ix ; i++,rv+=mx2) {
      for (j=0,lvj=lv,rvj=rv ; j<jx ; j+=2,rvj+=2,lvj=lv) {
        for (k=0 ; k<kx ; k++,dst+=2) {
          rv0= rv1= 0;
          for (mr=l=0 ; l<lx0 ; l++,mr+=jx,lvj+=2) {
            rv0+= lvj[0]*rvj[mr]-lvj[1]*rvj[mr+1];
            rv1+= lvj[1]*rvj[mr]+lvj[0]*rvj[mr+1];
          }
          for ( ; l<mx ; l+=4,mr+=lx,lvj+=8) {
            rv0+= lvj[0]*rvj[mr]-lvj[1]*rvj[mr+1]+
                  lvj[2]*rvj[mr+jx]-lvj[3]*rvj[mr+jx+1]+
                  lvj[4]*rvj[mr+jx2]-lvj[5]*rvj[mr+jx2+1]+
                  lvj[6]*rvj[mr+jx3]-lvj[7]*rvj[mr+jx3+1];
            rv1+= lvj[1]*rvj[mr]+lvj[0]*rvj[mr+1]+
                  lvj[3]*rvj[mr+jx]+lvj[2]*rvj[mr+jx+1]+
                  lvj[5]*rvj[mr+jx2]+lvj[4]*rvj[mr+jx2+1]+
                  lvj[7]*rvj[mr+jx3]+lvj[6]*rvj[mr+jx3+1];
          }
          dst[0]= rv0;
          dst[1]= rv1;
        }
      }
    }
  }
  PopTo(sp-3);
}

/* ARGSUSED */
void MatMultX(Operand *l, Operand *r)
{
  YError("illegal data type in matrix multiply");
}

/* ------------------------------------------------------------------------ */

void CallShell(void)
{
  Array *array= (Array *)(pc++)->constant->value.db;
  if (array->value.q[0]) p_system(array->value.q[0]);
}

void Y_system(int nArgs)
{
  char *line;
  if (nArgs!=1) YError("system function takes exactly one argument");
  line= YGetString(sp);
  if (line && line[0]) p_system(line);
}

/* ------------------------------------------------------------------------ */

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

static UnaryOp *SelfConverter[7][7]= {
  {0, &ToShortC, &ToIntC, &ToLongC, &ToFloatC, &ToDoubleC, &ToComplexC},
  {&ToCharS, 0, &ToIntS, &ToLongS, &ToFloatS, &ToDoubleS, &ToComplexS},
  {&ToCharI, &ToShortI, 0, &ToLongI, &ToFloatI, &ToDoubleI, &ToComplexI},
  {&ToCharL, &ToShortL, &ToIntL, 0, &ToFloatL, &ToDoubleL, &ToComplexL},
  {&ToCharF, &ToShortF, &ToIntF, &ToLongF, 0, &ToDoubleF, &ToComplexF},
  {&ToCharD, &ToShortD, &ToIntD, &ToLongD, &ToFloatD, 0, &ToComplexD},
  {&ToCharZ, &ToShortZ, &ToIntZ, &ToLongZ, &ToFloatZ, &ToDoubleZ, 0}
};

/* require an LValue for Assign virtual functions */
static LValue evalLV;

void EvalSD(Operand *op)
{
  Symbol *stack= op->owner;
  Symbol *arg= stack+1;
  int nArgs= op->references;    /* interpret misuse in FormEvalOp */
  StructDef *base= op->value;
  Operations *dataOps= base->dataOps;
  Operand argOp;

  if (nArgs<1) YError("type converter requires at least one argument");

  /* get back to legitimate in-memory structure definition */
  while (base->model) base= base->model;

  if (nArgs==1) {
    /* this may be a type converter for a primitive data type, or
       a structure instance initializing to 0 */
    arg->ops->FormOperand(arg, &argOp);
    if (argOp.ops==&voidOps) {
      /* single void argument is same as zero */
      if (arg->ops==&dataBlockSym) {
        arg->ops= &longScalar;
        Unref(arg->value.db);
        arg->value.l= 0;
      }
      argOp.ops= &longOps;
      argOp.references= 1;
      argOp.type.base= &longStruct;
      argOp.type.dims= 0;
      argOp.type.number= 1;
      argOp.value= &arg->value.l;
    }
    if (argOp.ops==dataOps) {
      PopTo(stack);
      return;   /* argument already has the requested data type */
    }

    if (dataOps->promoteID>T_COMPLEX || argOp.ops->promoteID>T_COMPLEX) {
      int bad= 0;
      int argIsZero= (argOp.ops->promoteID<=T_LONG && !argOp.type.dims &&
        ((argOp.ops==&longOps && !((long*)argOp.value)[0]) ||
         (argOp.ops==&intOps && !((int*)argOp.value)[0]) ||
         (argOp.ops==&shortOps && !((short*)argOp.value)[0]) ||
         (argOp.ops==&charOps && !((char*)argOp.value)[0])));

      if (dataOps==&structOps && argOp.ops->isArray && argIsZero) {
        /* initialize a zero-valued scalar instance */
        Array *result= PushDataBlock(NewArray(base, (Dimension *)0));
        if (base->Copy==&CopyX)
          memset(result->value.c, 0,
                 result->type.number*result->type.base->size);

      } else if (argOp.ops==&structOps &&
                 StructEqual(argOp.type.base, base)) {
        /* equivalent but non-identical types just get copied */
        Array *result= PushDataBlock(NewArray(base, argOp.type.dims));
        base->Copy(base, result->value.c, argOp.value, argOp.type.number);

      } else if (argIsZero &&
                 (dataOps==&stringOps || dataOps==&pointerOps)) {
        PushDataBlock(NewArray(base, argOp.type.dims));

      } else if (dataOps==&stringOps && argOp.ops==&pointerOps) {
        /* want to create string array from pointer array */
        Array *result= PushDataBlock(NewArray(base, argOp.type.dims));
        void **ptr= (void **)argOp.value;
        char **str= result->value.q;
        long i, n= argOp.type.number;
        Array *array;
        for (i=0 ; i<n ; i++) {
          if (ptr[i]) {
            array= Pointee(ptr[i]);
            if (array->ops!=&charOps)
              YError("only pointer to char can be converted to string");
            str[i]= p_strncat(0, ptr[i], array->type.number);
          }
        }

      } else if (dataOps==&pointerOps && argOp.ops==&stringOps) {
        /* want to create pointer array from string array */
        Array *result= PushDataBlock(NewArray(base, argOp.type.dims));
        void **ptr= result->value.p;
        char **str= (char **)argOp.value;
        long len, i, n= argOp.type.number;
        Array *array;
        for (i=0 ; i<n ; i++) {
          if (str[i]) {
            len= strlen(str[i]);
            array= NewArray(&charStruct,
                            NewDimension(len+1, 1L, (Dimension *)0));
            array->type.dims->references--;
            ptr[i]= array->value.c;
            strcpy(ptr[i], str[i]);
          }
        }

      } else {
        bad= 1;
      }
      if (bad) YError("illegal data type conversion");

      PopTo(stack);
      Drop(1);
      return;
    }

    /* this is a non-trivial primitive type conversion */
    SelfConverter[argOp.ops->promoteID][dataOps->promoteID](&argOp);

  } else {
    /* expect a keyword argument list of values for each member */
    char *name;
    long index;

    Array *result= PushDataBlock(NewArray(base, (Dimension *)0));
    char *address= result->value.c;
    if (base->Copy==&CopyX)
      memset(result->value.c, 0,
             result->type.number*result->type.base->size);
    evalLV.ops= &lvalueOps;

    do {
      if (arg->ops)
        YError("expecting keyword arguments to create struct instance");
      name= globalTable.names[arg->index];
      if (!HashFind(&base->table, name, 0L))
        YError("keyword corresponds to no struct member name");
      index= hashIndex;  /* someday FormOperand might clobber this... */

      arg++;
      arg->ops->FormOperand(arg, &argOp);
      if (!argOp.ops->isArray) YError("keyword argument has bad data type");

      /* cook up phony LValue to be able to use Assign virtual function */
      evalLV.type.base= base->members[index].base;
      evalLV.type.dims= base->members[index].dims;
      evalLV.type.number= base->members[index].number;
      evalLV.address.m= address+base->offsets[index];
      /* as in DoAssign in ops3.c */
      evalLV.type.base->dataOps->Assign((Operand *)&evalLV, &argOp);
      arg++;
    } while (nArgs-=2);
  }

  PopTo(stack);
  Drop((int)(sp-stack));
}

/* ------------------------------------------------------------------------ */
