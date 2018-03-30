/*
 * $Id: yhex.c,v 1.1 2005-09-18 22:05:51 dhmunro Exp $
 * Yorick-specific interface routines, see hex.i
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "hex.h"
#include "regul.h"
#include "ydata.h"
#include "yio.h"
#include "pstdlib.h"

extern BuiltIn Y_hex_mesh, Y_hex5_track, Y_hex24f_track, Y_hex24b_track;
extern BuiltIn Y_reg_track, Y_hex_query;

/* Implement HX_mesh as a foreign Yorick data type.  */
typedef struct YHX_mesh YHX_mesh;
struct YHX_mesh {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  HX_mesh mesh;
  TK_result *result;
};

extern YHX_mesh *new_YHX(double *xyz, long *bound, long nbnds, HX_blkbnd *bnds,
                         long nblks, HX_block *blks, long start);
extern void free_YHX(void *yhx);  /* ******* Use Unref(dm) ******* */
extern YHX_mesh *YGet_YHX_mesh(Symbol *s);
extern Operations yhx_mesh_ops;

void Y_hex_mesh(int nArgs)
{
  Symbol *s= sp-nArgs+1;
  if (nArgs==7) {
    double *xyz= YGet_D(s, 0, (Dimension **)0);
    long *bound= YGet_L(s+1, 0, (Dimension **)0);
    long nbnds= YGetInteger(s+2);
    void **pbnds= YGet_P(s+3, 1, (Dimension **)0);
    long nblks= YGetInteger(s+4);
    void **pblks= YGet_P(s+5, 0, (Dimension **)0);
    long start= YGetInteger(s+6);
    if (!pblks) YError("hex_mesh blks parameter bad");
    PushDataBlock(new_YHX(xyz, bound, pbnds? nbnds : 0,
                          pbnds? *pbnds : 0, nblks, *pblks, start));
  } else {
    YError("hex_mesh needs exactly seven arguments");
  }
}

void Y_hex_query(int nArgs)
{
  Symbol *s= sp-nArgs+1;
  YHX_mesh *mesh;
  if (nArgs<1 || nArgs>5) YError("hex_query needs 1-5 arguments");
  if (s->ops==&referenceSym) ReplaceRef(s);
  if (s->ops!=&dataBlockSym || s->value.db->ops!=&yhx_mesh_ops)
    YError("hex_query 1st argument must be a hex mesh");
  mesh= (YHX_mesh *)s->value.db;
  if (++s<=sp) {
    Symbol sy;
    long index= YGet_Ref(s);
    sy.ops= &dataBlockSym;
    sy.value.db= Pointee(mesh->mesh.xyz);
    YPut_Result(&sy, index);
    if (++s<=sp) {
      index= YGet_Ref(s);
      sy.value.db= Pointee(mesh->mesh.bound);
      YPut_Result(&sy, index);
      if (++s<=sp) {
        index= YGet_Ref(s);
        sy.value.db= Pointee(mesh->mesh.bnds);
        YPut_Result(&sy, index);
        if (++s<=sp) {
          index= YGet_Ref(s);
          sy.value.db= Pointee(mesh->mesh.blks);
          YPut_Result(&sy, index);
        }
      }
    }
  }
  PushLongValue(mesh->mesh.start);
}

YHX_mesh *new_YHX(double *xyz, long *bound, long nbnds, HX_blkbnd *bnds,
                  long nblks, HX_block *blks, long start)
{
  YHX_mesh *mesh= p_malloc(sizeof(YHX_mesh));
  Array *ary;
  mesh->references= 0;
  mesh->ops= &yhx_mesh_ops;
  mesh->result= 0;
  mesh->mesh.xyz= (void *)xyz;
  mesh->mesh.orient= 0;
  mesh->mesh.stride= 0;
  mesh->mesh.bound= (void *)bound;
  mesh->mesh.nbnds= nbnds;
  mesh->mesh.bnds= bnds;
  mesh->mesh.nblks= nblks;
  mesh->mesh.blks= blks;
  mesh->mesh.block= 0;
  mesh->mesh.start= start;
  if (xyz) {
    ary= Pointee(xyz);
    (void)Ref(ary);
  }
  if (bound) {
    ary= Pointee(bound);
    (void)Ref(ary);
  }
  if (bnds) {
    ary= Pointee(bnds);
    (void)Ref(ary);
  }
  if (blks) {
    ary= Pointee(blks);
    (void)Ref(ary);
  }
  return mesh;
}

void free_YHX(void *yhx)
{
  YHX_mesh *mesh= yhx;
  Array *ary;
  TK_result *result= mesh->result;
  mesh->result= 0;
  if (result) ray_free(result);
  ary= mesh->mesh.xyz? Pointee(mesh->mesh.xyz) : 0;
  mesh->mesh.xyz= 0;
  if (ary) Unref(ary);
  ary= mesh->mesh.bound? Pointee(mesh->mesh.bound) : 0;
  mesh->mesh.bound= 0;
  if (ary) Unref(ary);
  ary= mesh->mesh.bnds? Pointee(mesh->mesh.bnds) : 0;
  mesh->mesh.bnds= 0;
  if (ary) Unref(ary);
  ary= mesh->mesh.blks? Pointee(mesh->mesh.blks) : 0;
  mesh->mesh.blks= 0;
  if (ary) Unref(ary);
  p_free(mesh);
}

static void hex_tracker(int nArgs, int which);

void Y_hex5_track(int nArgs)
{
  hex_tracker(nArgs, 0);
}

void Y_hex24f_track(int nArgs)
{
  hex_tracker(nArgs, 1);
}

void Y_hex24b_track(int nArgs)
{
  hex_tracker(nArgs, 2);
}

static double *normalize_rays(double **p, long n);

static void hex_tracker(int nArgs, int which)
{
  YHX_mesh *mesh;
  double *p, *q;
  Dimension *dims;
  long index, dlist[10], n;
  int ndims, i;
  TK_result *result;
  Array *c, *s;
  if (nArgs!=3) YError("hexN_track takes exactly 3 arguments");
  mesh= YGet_YHX_mesh(sp-2);
  p= YGet_D(sp-1, 0, &dims);
  index= YGet_Ref(sp);
  Drop(1);  /* s argument is just an output */
  ndims= YGet_dims(dims, dlist, 10);
  if (ndims>10 || ndims<2 || dlist[0]!=3 || dlist[ndims-1]!=2)
    YError("hexN_track rays must be 3 x ray_dims x 2 array of [p,q]");
  for (n=1,i=1 ; i<ndims-1 ; i++) n*= dlist[i];
  q= normalize_rays(&p, n);
  result= mesh->result;
  if (result) ray_reset(result);
  else result= mesh->result= ray_result();
  if (!which)
    hex5_rays(&mesh->mesh, n, (void *)p, (void *)q, result);
  else if (which==1)
    hex24_rays(&mesh->mesh, n, (void *)p, (void *)q, 0, result);
  else
    hex24_rays(&mesh->mesh, n, (void *)p, (void *)q, 1, result);
  n= ray_collect(result, (long *)0, (double *)0, 1L);
  dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  tmpDims= NewDimension(n, 1L, (Dimension *)0);
  s= PushDataBlock(NewArray(&doubleStruct, tmpDims));
  YPut_Result(sp, index);
  c= PushDataBlock(NewArray(&longStruct, tmpDims));
  ray_collect(result, c->value.l, s->value.d, 1L);
  mesh->result= 0;
  ray_free(result);
}

void Y_reg_track(int nArgs)
{
  YHX_mesh *mesh;
  double *p, *q, *xyz[3];
  Dimension *dims;
  long index, dlist[10], n, nxyz[3];
  int ndims, i;
  TK_result *result;
  Array *c, *s;
  if (nArgs!=5) YError("reg_track takes exactly 5 arguments");
  for (i=0 ; i<3 ; i++) {
    xyz[i]= YGet_D(sp-4+i, 0, &dims);
    if (YGet_dims(dims, dlist, 2)!=1 || dlist[0]<2)
      YError("reg_track x,y,z arguments must be 1D with >=2 elements");
    nxyz[i]= dlist[0];
  }
  p= YGet_D(sp-1, 0, &dims);
  index= YGet_Ref(sp);
  Drop(1);  /* s argument is just an output */
  ndims= YGet_dims(dims, dlist, 10);
  if (ndims>10 || ndims<2 || dlist[0]!=3 || dlist[ndims-1]!=2)
    YError("reg_track rays must be 3 x ray_dims x 2 array of [p,q]");
  for (n=1,i=1 ; i<ndims-1 ; i++) n*= dlist[i];
  q= normalize_rays(&p, n);
  /* push dummy mesh onto stack to hold result, allowing proper
   * deallocation in case of an interrupt */
  mesh= PushDataBlock(new_YHX((double *)0, (long *)0, 0, (HX_blkbnd *)0,
                              0, (HX_block *)0, 0));
  result= mesh->result= ray_result();
  reg_rays(nxyz, xyz, n, (void *)p, (void *)q, result);
  n= ray_collect(result, (long *)0, (double *)0, 1L);
  dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  tmpDims= NewDimension(n, 1L, (Dimension *)0);
  s= PushDataBlock(NewArray(&doubleStruct, tmpDims));
  YPut_Result(sp, index);
  Drop(1);
  c= PushDataBlock(NewArray(&longStruct, tmpDims));
  ray_collect(result, c->value.l, s->value.d, 1L);
}

static double *normalize_rays(double **p, long n)
{
  extern double sqrt(double);
  double *q, qnrm, qtst;
  long i;
  Array *array= (Array *)sp->value.db;
  if (sp->ops!=&dataBlockSym || !array->ops->isArray)
    YError("(BUG) normalize_rays failed");
  if (array->references) {
    /* copy non-temporary arrays to avoid unexpected aliasing */
    Array *result= PushDataBlock(NewArray(array->type.base,
                                          array->type.dims));
    array->type.base->Copy(array->type.base, result->value.c,
                           array->value.c, array->type.number);
    PopTo(sp-1);
    *p= result->value.d;
  }
  q= *p + 3*n;
  for (i=0 ; i<3*n ; i+=3) {
    qnrm= q[i]<0.? -q[i] : q[i];
    qtst= q[i+1]<0.? -q[i+1] : q[i+1];
    if (qtst > qnrm) qnrm= qtst;
    qtst= q[i+2]<0.? -q[i+2] : q[i+2];
    if (qtst > qnrm) qnrm= qtst;
    if (qnrm) {
      qnrm= 1.0/qnrm;
      q[i]*= qnrm;
      q[i+1]*= qnrm;
      q[i+2]*= qnrm;
      qnrm= 1.0/sqrt(q[i]*q[i]+q[i+1]*q[i+1]+q[i+2]*q[i+2]);
      q[i]*= qnrm;
      q[i+1]*= qnrm;
      q[i+2]*= qnrm;
    } else {
      q[i]= q[i+1]= 0.0;
      q[i+2]= 1.0;
    }
  }
  return q;
}

/*--------------------------------------------------------------------------*/
/* Define opaque mesh object YHX_mesh as a foreign Yorick data type.  */

extern PromoteOp PromXX;
extern UnaryOp ToAnyX, NegateX, ComplementX, NotX, TrueX;
extern BinaryOp AddX, SubtractX, MultiplyX, DivideX, ModuloX, PowerX;
extern BinaryOp EqualX, NotEqualX, GreaterX, GreaterEQX;
extern BinaryOp ShiftLX, ShiftRX, OrX, AndX, XorX;
extern BinaryOp AssignX, MatMultX;
extern UnaryOp EvalX, SetupX, PrintX;
extern MemberOp GetMemberX;

static UnaryOp PrintYHX;

Operations yhx_mesh_ops = {
  &free_YHX, T_OPAQUE, 0, T_STRING, "Hex-Mesh",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &EvalX, &SetupX, &GetMemberX, &MatMultX, &PrintYHX
};

static void PrintYHX(Operand *op)
{
  YHX_mesh *yhx= op->value;
  char line[96];
  ForceNewline();
  sprintf(line, "hex mesh: %ld blocks, %ld nodes", yhx->mesh.nblks,
          yhx->mesh.blks[yhx->mesh.nblks-1].final);
  PrintFunc(line);
  ForceNewline();
}

YHX_mesh *YGet_YHX_mesh(Symbol *s)
{
  if (s->ops==&referenceSym) ReplaceRef(s);
  if (s->ops!=&dataBlockSym || s->value.db->ops!=&yhx_mesh_ops)
    YError("expecting Hex-Mesh argument");
  return (YHX_mesh *)s->value.db;
}

/*--------------------------------------------------------------------------*/
