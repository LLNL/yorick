/*
 * $Id: yapi.c,v 1.27 2010-08-29 00:39:57 dhmunro Exp $
 * API implementation for interfacing yorick packages to the interpreter
 *  - yorick package source should not need to include anything
 *    not here or in the play headers
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "ydata.h"
#include "yio.h"
#include "bcast.h"

#include "pstdlib.h"
#include <string.h>
#include <stdio.h>

int
yarg_subroutine(void)
{
  extern VMaction DropTop;
  return pc->Action == &DropTop;
}

void
yarg_kw_init(char **knames, long *kglobs, int *kiargs)
{
  long n = kglobs[0];
  if (!n) {
    long *globs = kglobs+1;
    char *name;
    for (name=*knames++ ; name ; n++, name=*knames++)
      *(globs++) = yget_global(name, 0);
    kglobs[0] = n;
  }
  while (n-- > 0) *(kiargs++) = -1;
}

int
yarg_kw(int iarg, long *kglobs, int *kiargs)
{
  long n, *globs, vndex;
  while (iarg>=0 && !sp[-iarg].ops) {
    if (!iarg)
      y_error("(BUG) stack corrupted in yarg_kw");
    vndex = sp[-iarg].index;
    n = kglobs[0];
    for (globs=kglobs+1 ; --n >= 0 ; globs++)
      if (globs[0] == vndex) break;
    if (n < 0)
      y_errorq("unrecognized keyword: %s", globalTable.names[vndex]);
    kiargs[kglobs[0]-1-n] = --iarg;
    --iarg;
  }
  return iarg;
}

long
yarg_key(int iarg)
{
  if (iarg>=0 && !sp[-iarg].ops) {
    if (!iarg) y_error("(BUG) stack corrupted in yarg_key");
    return sp[-iarg].index;
  }
  return -1;
}

int
yarg_nil(int iarg)
{
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops==&referenceSym) s = &globTab[s->index];
    return s->ops==&dataBlockSym && s->value.db==&nilDB;
  }
  return 0;
}

int
yarg_rank(int iarg)
{
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops==&referenceSym) s = &globTab[s->index];
    if (s->ops == &dataBlockSym) {
      Dimension *d;
      int rank;
      if (s->value.db->ops->isArray)
        d = ((Array *)s->value.db)->type.dims;
      else if (s->value.db->ops == &lvalueOps)
        d = ((LValue *)s->value.db)->type.dims;
      else
        return -1;
      for (rank=0 ; d ; d=d->next) rank++;
      return rank;
    } else if (s->ops==&doubleScalar || s->ops==&longScalar
               || s->ops==&intScalar) {
      return 0;
    }
  }
  return -1;
}

int
yarg_number(int iarg)
{
  unsigned int id = yarg_typeid(iarg);
  if (id > T_COMPLEX) return 0;
  else if (id < T_FLOAT) return 1;
  else if (id < T_COMPLEX) return 2;
  else return 3;
}

int
yarg_string(int iarg)
{
  int is_string = 0;
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops==&referenceSym) s = &globTab[s->index];
    if (s->ops == &dataBlockSym) {
      if (s->value.db->ops == &stringOps) {
        is_string = ((Array *)s->value.db)->type.dims? 2 : 1;
      } else if (s->value.db->ops == &lvalueOps) {
        StructDef *base= ((LValue *)s->value.db)->type.base;
        while (base->model) base = base->model;
        if (base->dataOps == &stringOps)
          is_string = ((LValue *)s->value.db)->type.dims? 2 : 1;
      }
    }
  }
  return is_string;
}

/* yarg_func moved to fwrap.c */

int
yarg_typeid(int iarg)
{
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops==&referenceSym) s = &globTab[s->index];
    if (s->ops == &dataBlockSym) {
      int id = s->value.db->ops->typeID;
      if (id == T_LVALUE) {
        StructDef *base= ((LValue *)s->value.db)->type.base;
        while (base->model) base = base->model;
        id = base->dataOps->typeID;
      }
      return id;
    } else if (s->ops==&doubleScalar) {
      return T_DOUBLE;
    } else if (s->ops==&longScalar) {
      return T_LONG;
    } else if (s->ops==&intScalar) {
      return T_INT;
    }
  }
  return T_OPAQUE + 100;
}

int
yarg_true(int iarg)
{
  Symbol *s = (iarg>=0)? sp - iarg : 0;
  int x = 0;
  if (s) {
    CheckStack(1);
    sp[1].ops = &intScalar;
    if (s->ops==&referenceSym) s = &globTab[s->index];
    if (s->ops != &dataBlockSym) sp[1].value = s->value;
    else sp[1].value.db = Ref(s->value.db);
    sp++;
    sp->ops = s->ops;
    sp->ops->True();
    x = (int)ygets_l(0);
    yarg_drop(1);
  }
  return x;
}

int
yarg_scratch(int iarg)
{
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops==&referenceSym) return 1;
    if (s->ops == &dataBlockSym) {
      if (s->value.db->ops == &lvalueOps) {
        LValue *lv = (LValue *)s->value.db;
        if (lv->strider || lv->type.base->model) return 1;
        return 0;
      } else {
        if (s->value.db->references) return 0;
        if (s->value.db->ops->isArray) return 1;
      }
    } else if (s->ops==&doubleScalar
               || s->ops==&longScalar || s->ops==&intScalar) {
      return 1;
    }
    return 2;
  }
  return -1;
}

static void *ygeta_array(int iarg, Operations **pops, Member **ptype);
static void yget_dims(long *ntot, long *dims, Member *type);
static Dimension *ypush_dims(long *dims);
static void *ypush_array(StructDef *base, Dimension *dims);

static void y_s_c(void *x,unsigned char *y,long n)
{ short *z=x; while (n--) *y++ = (unsigned char)*z++; }
static void y_i_c(void *x,unsigned char *y,long n)
{ int *z=x; while (n--) *y++ = (unsigned char)*z++; }
static void y_l_c(void *x,unsigned char *y,long n)
{ long *z=x; while (n--) *y++ = (unsigned char)*z++; }
static void y_f_c(void *x,unsigned char *y,long n)
{ float *z=x; while (n--) *y++ = (unsigned char)*z++; }
static void y_d_c(void *x,unsigned char *y,long n)
{ double *z=x; while (n--) *y++ = (unsigned char)*z++; }
static void y_z_c(void *x,unsigned char *y,long n)
{ unsigned char *z=x; while (n--) *y++ = (unsigned char)*z++, z++; }
static void y_x_c(void *x,unsigned char *y,long n)
{ y_error("cannot convert non-number to char"); }
static void (*y_to_c[])(void *,unsigned char *,long) = {
  0, y_s_c, y_i_c, y_l_c, y_f_c, y_d_c, y_z_c, y_x_c };

char *
ygeta_c(int iarg, long *ntot, long *dims)
{
  Operations *ops;
  Member *type;
  void *p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  yget_dims(ntot, dims, type);
  if (ops != &charOps) {
    int is_db = (sp[-iarg].ops == &dataBlockSym);
    unsigned char *q = ypush_array(&charStruct, type->dims);
    y_to_c[ops->promoteID](p, q, type->number);
    if (is_db) {
      sp[-iarg-1].ops = &intScalar;
      Unref(sp[-iarg-1].value.db);
    }
    sp--;
    sp[-iarg].value.db = sp[1].value.db;
    sp[-iarg].ops = &dataBlockSym;
    return (char *)q;
  }
  return p;
}

static void y_c_s(void *x,short *y,long n)
{ unsigned char *z=x; while (n--) *y++ = (short)*z++; }
static void y_i_s(void *x,short *y,long n)
{ int *z=x; while (n--) *y++ = (short)*z++; }
static void y_l_s(void *x,short *y,long n)
{ long *z=x; while (n--) *y++ = (short)*z++; }
static void y_f_s(void *x,short *y,long n)
{ float *z=x; while (n--) *y++ = (short)*z++; }
static void y_d_s(void *x,short *y,long n)
{ double *z=x; while (n--) *y++ = (short)*z++; }
static void y_z_s(void *x,short *y,long n)
{ double *z=x; while (n--) *y++ = (short)*z++, z++; }
static void y_x_s(void *x,short *y,long n)
{ y_error("cannot convert non-number to short"); }
static void (*y_to_s[])(void *,short *,long) = {
  y_c_s, 0, y_i_s, y_l_s, y_f_s, y_d_s, y_z_s, y_x_s };

short *
ygeta_s(int iarg, long *ntot, long *dims)
{
  Operations *ops;
  Member *type;
  void *p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  yget_dims(ntot, dims, type);
  if (ops != &shortOps) {
    int is_db = (sp[-iarg].ops == &dataBlockSym);
    short *q = ypush_array(&shortStruct, type->dims);
    y_to_s[ops->promoteID](p, q, type->number);
    if (is_db) {
      sp[-iarg-1].ops = &intScalar;
      Unref(sp[-iarg-1].value.db);
    }
    sp--;
    sp[-iarg].value.db = sp[1].value.db;
    sp[-iarg].ops = &dataBlockSym;
    return q;
  }
  return p;
}

static void y_c_i(void *x,int *y,long n)
{ unsigned char *z=x; while (n--) *y++ = (int)*z++; }
static void y_s_i(void *x,int *y,long n)
{ short *z=x; while (n--) *y++ = (int)*z++; }
static void y_l_i(void *x,int *y,long n)
{ long *z=x; while (n--) *y++ = (int)*z++; }
static void y_f_i(void *x,int *y,long n)
{ float *z=x; while (n--) *y++ = (int)*z++; }
static void y_d_i(void *x,int *y,long n)
{ double *z=x; while (n--) *y++ = (int)*z++; }
static void y_z_i(void *x,int *y,long n)
{ double *z=x; while (n--) *y++ = (int)*z++, z++; }
static void y_x_i(void *x,int *y,long n)
{ y_error("cannot convert non-number to int"); }
static void (*y_to_i[])(void *,int *,long) = {
  y_c_i, y_s_i, 0, y_l_i, y_f_i, y_d_i, y_z_i, y_x_i };

int *
ygeta_i(int iarg, long *ntot, long *dims)
{
  Operations *ops;
  Member *type;
  void *p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  yget_dims(ntot, dims, type);
  if (ops != &intOps) {
    if (type->dims) {
      int *q = ypush_array(&intStruct, type->dims);
      y_to_i[ops->promoteID](p, q, type->number);
      sp[-iarg-1].ops = &intScalar;
      Unref(sp[-iarg-1].value.db);
      sp--;
      sp[-iarg].value.db = sp[1].value.db;
      sp[-iarg].ops = &dataBlockSym;
      return q;
    } else {
      int is_db = (sp[-iarg].ops == &dataBlockSym);
      int x;
      y_to_i[ops->promoteID](p, &x, type->number);
      sp[-iarg].ops = &intScalar;
      if (is_db) Unref(sp[-iarg].value.db);
      sp[-iarg].value.i = x;
      return &(sp[-iarg].value.i);
    }
  }
  return p;
}

static void y_c_l(void *x,long *y,long n)
{ unsigned char *z=x; while (n--) *y++ = (long)*z++; }
static void y_s_l(void *x,long *y,long n)
{ short *z=x; while (n--) *y++ = (long)*z++; }
static void y_i_l(void *x,long *y,long n)
{ int *z=x; while (n--) *y++ = (long)*z++; }
static void y_f_l(void *x,long *y,long n)
{ float *z=x; while (n--) *y++ = (long)*z++; }
static void y_d_l(void *x,long *y,long n)
{ double *z=x; while (n--) *y++ = (long)*z++; }
static void y_z_l(void *x,long *y,long n)
{ double *z=x; while (n--) *y++ = (long)*z++, z++; }
static void y_x_l(void *x,long *y,long n)
{ y_error("cannot convert non-number to long"); }
static void (*y_to_l[])(void *,long *,long) = {
  y_c_l, y_s_l, y_i_l, 0, y_f_l, y_d_l, y_z_l, y_x_l };

long *
ygeta_l(int iarg, long *ntot, long *dims)
{
  Operations *ops;
  Member *type;
  void *p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  yget_dims(ntot, dims, type);
  if (ops != &longOps) {
    if (type->dims) {
      long *q = ypush_array(&longStruct, type->dims);
      y_to_l[ops->promoteID](p, q, type->number);
      sp[-iarg-1].ops = &intScalar;
      Unref(sp[-iarg-1].value.db);
      sp--;
      sp[-iarg].value.db = sp[1].value.db;
      sp[-iarg].ops = &dataBlockSym;
      return q;
    } else {
      int is_db = (sp[-iarg].ops == &dataBlockSym);
      long x;
      y_to_l[ops->promoteID](p, &x, type->number);
      sp[-iarg].ops = &longScalar;
      if (is_db) Unref(sp[-iarg].value.db);
      sp[-iarg].value.l = x;
      return &(sp[-iarg].value.l);
    }
  }
  return p;
}

long
ygets_l(int iarg)
{
  Operations *ops;
  Member *type;
  void *p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  if (!type->dims && ops->promoteID<T_FLOAT) {
    if (ops != &longOps) {
      int is_db = (sp[-iarg].ops == &dataBlockSym);
      long x;
      y_to_l[ops->promoteID](p, &x, type->number);
      sp[-iarg].ops = &longScalar;
      if (is_db) Unref(sp[-iarg].value.db);
      sp[-iarg].value.l = x;
      return x;
    }
    return *(long *)p;
  }
  y_error("expecting scalar integer argument");
  return 0;
}

static void y_c_f(void *x,float *y,long n)
{ unsigned char *z=x; while (n--) *y++ = (float)*z++; }
static void y_s_f(void *x,float *y,long n)
{ short *z=x; while (n--) *y++ = (float)*z++; }
static void y_i_f(void *x,float *y,long n)
{ int *z=x; while (n--) *y++ = (float)*z++; }
static void y_l_f(void *x,float *y,long n)
{ long *z=x; while (n--) *y++ = (float)*z++; }
static void y_d_f(void *x,float *y,long n)
{ double *z=x; while (n--) *y++ = (float)*z++; }
static void y_z_f(void *x,float *y,long n)
{ double *z=x; while (n--) *y++ = (float)*z++, z++; }
static void y_x_f(void *x,float *y,long n)
{ y_error("cannot convert non-number to float"); }
static void (*y_to_f[])(void *,float *,long) = {
  y_c_f, y_s_f, y_i_f, y_l_f, 0, y_d_f, y_z_f, y_x_f };

float *
ygeta_f(int iarg, long *ntot, long *dims)
{
  Operations *ops;
  Member *type;
  void *p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  yget_dims(ntot, dims, type);
  if (ops != &floatOps) {
    int is_db = (sp[-iarg].ops == &dataBlockSym);
    float *q = ypush_array(&floatStruct, type->dims);
    y_to_f[ops->promoteID](p, q, type->number);
    if (is_db) {
      sp[-iarg-1].ops = &intScalar;
      Unref(sp[-iarg-1].value.db);
    }
    sp--;
    sp[-iarg].value.db = sp[1].value.db;
    sp[-iarg].ops = &dataBlockSym;
    return q;
  }
  return p;
}

static void y_c_d(void *x,double *y,long n)
{ unsigned char *z=x; while (n--) *y++ = (double)*z++; }
static void y_s_d(void *x,double *y,long n)
{ short *z=x; while (n--) *y++ = (double)*z++; }
static void y_i_d(void *x,double *y,long n)
{ int *z=x; while (n--) *y++ = (double)*z++; }
static void y_l_d(void *x,double *y,long n)
{ long *z=x; while (n--) *y++ = (double)*z++; }
static void y_f_d(void *x,double *y,long n)
{ float *z=x; while (n--) *y++ = (double)*z++; }
static void y_z_d(void *x,double *y,long n)
{ double *z=x; while (n--) *y++ = (double)*z++, z++; }
static void y_x_d(void *x,double *y,long n)
{ y_error("cannot convert non-number to double"); }
static void (*y_to_d[])(void *,double *,long) = {
  y_c_d, y_s_d, y_i_d, y_l_d, y_f_d, 0, y_z_d, y_x_d };

double *
ygeta_d(int iarg, long *ntot, long *dims)
{
  Operations *ops;
  Member *type;
  void *p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  yget_dims(ntot, dims, type);
  if (ops != &doubleOps) {
    if (type->dims) {
      double *q = ypush_array(&doubleStruct, type->dims);
      y_to_d[ops->promoteID](p, q, type->number);
      sp[-iarg-1].ops = &intScalar;
      Unref(sp[-iarg-1].value.db);
      sp--;
      sp[-iarg].value.db = sp[1].value.db;
      sp[-iarg].ops = &dataBlockSym;
      return q;
    } else {
      int is_db = (sp[-iarg].ops == &dataBlockSym);
      double x;
      y_to_d[ops->promoteID](p, &x, type->number);
      sp[-iarg].ops = &doubleScalar;
      if (is_db) Unref(sp[-iarg].value.db);
      sp[-iarg].value.d = x;
      return &(sp[-iarg].value.d);
    }
  }
  return p;
}

double
ygets_d(int iarg)
{
  Operations *ops;
  Member *type;
  void *p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  if (!type->dims && ops->promoteID<T_COMPLEX) {
    if (ops != &doubleOps) {
      int is_db = (sp[-iarg].ops == &dataBlockSym);
      double x;
      y_to_d[ops->promoteID](p, &x, type->number);
      sp[-iarg].ops = &doubleScalar;
      if (is_db) Unref(sp[-iarg].value.db);
      sp[-iarg].value.d = x;
      return x;
    }
    return *(double *)p;
  }
  y_error("expecting scalar real argument");
  return 0;
}

static void y_c_z(void *x,double *y,long n)
{ unsigned char *z=x; while (n--) *y++ = (double)*z++, *y++ = 0.; }
static void y_s_z(void *x,double *y,long n)
{ short *z=x; while (n--) *y++ = (double)*z++, *y++ = 0.; }
static void y_i_z(void *x,double *y,long n)
{ int *z=x; while (n--) *y++ = (double)*z++, *y++ = 0.; }
static void y_l_z(void *x,double *y,long n)
{ long *z=x; while (n--) *y++ = (double)*z++, *y++ = 0.; }
static void y_f_z(void *x,double *y,long n)
{ float *z=x; while (n--) *y++ = (double)*z++, *y++ = 0.; }
static void y_d_z(void *x,double *y,long n)
{ double *z=x; while (n--) *y++ = (double)*z++, *y++ = 0.; }
static void y_x_z(void *x,double *y,long n)
{ y_error("cannot convert non-number to complex"); }
static void (*y_to_z[])(void *,double *,long) = {
  y_c_z, y_s_z, y_i_z, y_l_z, y_f_z, y_d_z, 0, y_x_z };

double *
ygeta_z(int iarg, long *ntot, long *dims)
{
  Operations *ops;
  Member *type;
  void *p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  yget_dims(ntot, dims, type);
  if (ops != &complexOps) {
    int is_db = (sp[-iarg].ops == &dataBlockSym);
    double *q = ypush_array(&complexStruct, type->dims);
    y_to_z[ops->promoteID](p, q, type->number);
    if (is_db) {
      sp[-iarg-1].ops = &intScalar;
      Unref(sp[-iarg-1].value.db);
    }
    sp--;
    sp[-iarg].value.db = sp[1].value.db;
    sp[-iarg].ops = &dataBlockSym;
    return q;
  }
  return p;
}

ystring_t *
ygeta_q(int iarg, long *ntot, long *dims)
{
  Operations *ops;
  Member *type;
  void *p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  yget_dims(ntot, dims, type);
  if (ops != &stringOps)
    y_error("expecting string argument");
  return p;
}

ystring_t
ygets_q(int iarg)
{
  Operations *ops;
  Member *type;
  char **q;
  if (iarg < 0) return 0;
  q = ygeta_array(iarg, &ops, &type);
  if (ops!=&stringOps || type->dims)
    y_error("expecting scalar string argument");
  return q[0];
}

ypointer_t *
ygeta_p(int iarg, long *ntot, long *dims)
{
  Operations *ops;
  Member *type;
  void *p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  yget_dims(ntot, dims, type);
  if (ops != &pointerOps)
    y_error("expecting pointer argument");
  return p;
}

ypointer_t
ygets_p(int iarg)
{
  Operations *ops;
  Member *type;
  void **p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  if (ops!=&pointerOps || type->dims)
    y_error("expecting scalar pointer argument");
  return p[0];
}

double *
ygeta_dz(int iarg, long *ntot, long *dims, int *is_z)
{
  Operations *ops;
  Member *type;
  void *p;
  if (iarg < 0) return 0;
  p = ygeta_array(iarg, &ops, &type);
  yget_dims(ntot, dims, type);
  *is_z = (ops==&complexOps);
  if (ops!=&doubleOps && !*is_z) {
    if (type->dims) {
      double *q = ypush_array(&doubleStruct, type->dims);
      y_to_d[ops->promoteID](p, q, type->number);
      sp[-iarg-1].ops = &intScalar;
      Unref(sp[-iarg-1].value.db);
      sp--;
      sp[-iarg].value.db = sp[1].value.db;
      sp[-iarg].ops = &dataBlockSym;
      return q;
    } else {
      int is_db = (sp[-iarg].ops == &dataBlockSym);
      double x;
      y_to_d[ops->promoteID](p, &x, type->number);
      sp[-iarg].ops = &doubleScalar;
      if (is_db) Unref(sp[-iarg].value.db);
      sp[-iarg].value.d = x;
      return &(sp[-iarg].value.d);
    }
  }
  return p;
}

void *
ygeta_any(int iarg, long *ntot, long *dims, int *typeid)
{
  if (iarg >= 0) {
    Operations *ops;
    Member *type;
    void *p = ygeta_array(iarg, &ops, &type);
    yget_dims(ntot, dims, type);
    if (typeid) *typeid = ops->typeID;
    return p;
  }
  return 0;
}

static void *
ypush_array(StructDef *base, Dimension *dims)
{
  Array *a = PushDataBlock(NewArray(base, dims));
  return a->value.c;
}

void *
ygeta_coerce(int iarg, void *p, long ntot, long *dims, int oldid, int newid)
{
  if (oldid == newid) {
    return p;
  } else if (oldid<0 || oldid>T_COMPLEX || newid<0 || newid>T_COMPLEX) {
    y_error("non-numeric typeid illegal in ygeta_coerce");
  } else if (iarg < 0) {
    y_error("illegal iarg in ygeta_coerce");
  } else {
    static StructDef *types[] = {
      &charStruct, &shortStruct, &intStruct, &longStruct,
      &floatStruct, &doubleStruct, &complexStruct };
    static void (**converters[])(void*,void*,long) = {
      (void(**)(void*,void*,long))y_to_c, (void(**)(void*,void*,long))y_to_s,
      (void(**)(void*,void*,long))y_to_i, (void(**)(void*,void*,long))y_to_l,
      (void(**)(void*,void*,long))y_to_f, (void(**)(void*,void*,long))y_to_d,
      (void(**)(void*,void*,long))y_to_z };
    Array *a = PushDataBlock(NewArray(types[newid], ypush_dims(dims)));
    void *q = a->value.c;
    int is_db = (sp[-iarg].ops == &dataBlockSym);
    converters[newid][oldid](p, q, ntot);
    if (is_db) {
      sp[-iarg-1].ops = &intScalar;
      Unref(sp[-iarg-1].value.db);
    }
    sp--;
    sp[-iarg].value.db = sp[1].value.db;
    sp[-iarg].ops = &dataBlockSym;
    return q;
  }
  return 0;
}

static void
yget_dims(long *ntot, long *dims, Member *type)
{
  if (ntot) *ntot = type->number;
  if (dims) {
    Dimension *d = type->dims;
    int i, rank;
    long n;
    for (rank=0 ; d ; d=d->next) dims[++rank] = d->number;
    dims[0] = rank;
    for (i=1 ; i<rank ; i++,rank--)
      n = dims[i], dims[i] = dims[rank], dims[rank] = n;
  }
}

static Member y_i_type = { &intStruct, 0, 1L };
static Member y_l_type = { &longStruct, 0, 1L };
static Member y_d_type = { &doubleStruct, 0, 1L };

int
yarg_reform(int iarg, long *dims)
{
  if (iarg >= 0) {
    Operations *ops;
    Member *type;
    int i, rank = dims? dims[0] : 0;
    long n = 1;
    ygeta_array(iarg, &ops, &type);
    if (rank) for (n=dims[1],i=2 ; i<=rank ; i++) n *= dims[i];
    if (n == type->number) {
      Dimension *d = type->dims;
      if (n == 1) {
        Array *a;
        if (type==&y_d_type || type==&y_l_type || type==&y_i_type) {
          if (!rank) return 1;
          a = (Array *)ForceToDB(sp - iarg);
          type = &a->type;
          d = type->dims;
        }
      }
      type->dims = 0;
      if (d) FreeDimension(d);
      type->dims = ypush_dims(dims);
      if (type->dims) type->dims->references++;
      return 1;
    }
  }
  return 0;
}

extern void ReadGather(void *dst, void *srcM, long srcD, StructDef *base,
                       long number, const Strider *strider);

static void *
ygeta_array(int iarg, Operations **pops, Member **ptype)
{
  Symbol *s = sp - iarg;
  if (s->ops==&referenceSym) {
    /* change reference to global object into object itself */
    Symbol *g = &globTab[s->index];
    if (g->ops == &dataBlockSym) s->value.db = Ref(g->value.db);
    else s->value = g->value;
    s->ops = g->ops;
  }

  if (s->ops == &dataBlockSym) {
    Operations *ops = s->value.db->ops;
    if (ops->isArray) {
      /* whole array in memory */
      *pops = ops;
      *ptype = &((Array *)s->value.db)->type;
      return ((Array *)s->value.db)->value.c;

    } else if (ops == &lvalueOps) {
      LValue *lv = (LValue *)s->value.db;
      StructDef *base = lv->type.base;
      if (lv->strider || base->model) {
        /* read array from file or as subset of another array to stack */
        StructDef *model = base->model;
        IOStream *file = base->file;
        Array *a;
        if (model)
          while (model->model) model = model->model;
        else
          model = base;
        a = PushDataBlock(NewArray(model, lv->type.dims));
        ReadGather(a->value.c, file? 0 : lv->address.m, lv->address.d,
                   base, lv->type.number, lv->strider);
        if (file && file->pointeeList.table.nItems) ClearPointees(file, 0);
        /* pop array back to iarg on stack */
        s->ops = &intScalar;
        Unref(s->value.db);
        sp--;
        s->value.db = (DataBlock *)a;
        s->ops = &dataBlockSym;
        *pops = a->ops;
        *ptype = &a->type;
        return a->value.c;

      } else {
        /* array in unmanaged memory */
        *pops = base->dataOps;
        *ptype = &lv->type;
        return lv->address.m;
      }
    }

  } else if (s->ops == &doubleScalar) {
    *pops = &doubleOps;
    *ptype = &y_d_type;
    return &s->value.d;
  } else if (s->ops == &longScalar) {
    *pops = &longOps;
    *ptype = &y_l_type;
    return &s->value.l;
  } else if (s->ops == &intScalar) {
    *pops = &intOps;
    *ptype = &y_i_type;
    return &s->value.i;
  }

  y_error("expecting array argument");
  *pops = NULL;
  *ptype = NULL;
  return 0;
}

int
yget_range(int iarg, long min_max_step[3])
{
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops==&referenceSym) s = &globTab[s->index];
    if (s->ops==&dataBlockSym && s->value.db->ops==&rangeOps) {
      Range *r = (Range *)s->value.db;
      int flags = 0;
      min_max_step[0] = r->min;
      min_max_step[1] = r->max;
      min_max_step[2] = r->inc;
      if (r->nilFlags & R_MINNIL) flags |= Y_MIN_DFLT;
      if (r->nilFlags & R_MAXNIL) flags |= Y_MAX_DFLT;
      if (r->nilFlags & R_PSEUDO) {
        flags |= (r->nilFlags & R_RUBBER)? Y_RUBBER1 : Y_PSEUDO;
      } else if (r->nilFlags & R_RUBBER) {
        flags |= Y_RUBBER;
      } else if (r->nilFlags & R_NULLER) {
        flags |= Y_NULLER;
      } else if (r->nilFlags & R_MARKED) {
        flags |= Y_MMMARK;
      } else if (r->rf) {
        flags |= 7;
      } else {
        flags |= 1;
      }
      return flags;
    }
  }
  return 0;
}

void
ypush_check(int n)
{
  CheckStack(n);
}

void
ypush_nil(void)
{
  PushDataBlock(RefNC(&nilDB));
}

void
ypush_int(int value)
{
  sp[1].value.i = value;
  (++sp)->ops = &intScalar;
}

void
ypush_long(long value)
{
  sp[1].value.l = value;
  (++sp)->ops = &longScalar;
}

void
ypush_double(double value)
{
  sp[1].value.d = value;
  (++sp)->ops = &doubleScalar;
}

char *
ypush_c(long *dims)
{
  Array *a = PushDataBlock(NewArray(&charStruct, ypush_dims(dims)));
  memset(a->value.c, 0, a->type.number*a->type.base->size);
  return a->value.c;
}

short *
ypush_s(long *dims)
{
  Array *a = PushDataBlock(NewArray(&shortStruct, ypush_dims(dims)));
  memset(a->value.c, 0, a->type.number*a->type.base->size);
  return a->value.s;
}

int *
ypush_i(long *dims)
{
  if (!dims || !dims[0]) {
    sp[1].value.i = 0;
    (++sp)->ops = &intScalar;
    return &sp[0].value.i;
  } else {
    Array *a = PushDataBlock(NewArray(&intStruct, ypush_dims(dims)));
    memset(a->value.c, 0, a->type.number*a->type.base->size);
    return a->value.i;
  }
}

long *
ypush_l(long *dims)
{
  if (!dims || !dims[0]) {
    sp[1].value.l = 0;
    (++sp)->ops = &longScalar;
    return &sp[0].value.l;
  } else {
    Array *a = PushDataBlock(NewArray(&longStruct, ypush_dims(dims)));
    memset(a->value.c, 0, a->type.number*a->type.base->size);
    return a->value.l;
  }
}

float *
ypush_f(long *dims)
{
  Array *a = PushDataBlock(NewArray(&floatStruct, ypush_dims(dims)));
  memset(a->value.c, 0, a->type.number*a->type.base->size);
  return a->value.f;
}

double *
ypush_d(long *dims)
{
  if (!dims || !dims[0]) {
    sp[1].value.d = 0.;
    (++sp)->ops = &doubleScalar;
    return &sp[0].value.d;
  } else {
    Array *a = PushDataBlock(NewArray(&doubleStruct, ypush_dims(dims)));
    memset(a->value.c, 0, a->type.number*a->type.base->size);
    return a->value.d;
  }
}

double *
ypush_z(long *dims)
{
  Array *a = PushDataBlock(NewArray(&complexStruct, ypush_dims(dims)));
  memset(a->value.c, 0, a->type.number*a->type.base->size);
  return a->value.d;
}

ystring_t *
ypush_q(long *dims)
{
  Array *a = PushDataBlock(NewArray(&stringStruct, ypush_dims(dims)));
  return a->value.q;
}

ypointer_t *
ypush_p(long *dims)
{
  Array *a = PushDataBlock(NewArray(&pointerStruct, ypush_dims(dims)));
  return a->value.p;
}

static Dimension *
ypush_dims(long *dims)
{
  if (dims && dims[0]>0) {
    long rank = *dims++;
    Dimension *d = tmpDims;
    tmpDims = 0;
    FreeDimension(d);
    while (rank--) tmpDims = NewDimension(*dims++, 1L, tmpDims);
    return tmpDims;
  } else {
    return 0;
  }
}

void
ypush_range(long mnmxst[3], int flags)
{
  int flgs = 0;
  if (flags & Y_MIN_DFLT) flgs |= R_MINNIL;
  if (flags & Y_MAX_DFLT) flgs |= R_MAXNIL;
  flags &= 15;
  if (flags == Y_MMMARK) flgs |= R_MARKED;
  else if (flags == Y_PSEUDO) flgs |= R_PSEUDO;
  else if (flags == Y_RUBBER) flgs |= R_RUBBER;
  else if (flags == Y_RUBBER1) flgs |= R_PSEUDO | R_RUBBER;
  else if (flags == Y_NULLER) flgs |= R_NULLER;
  PushDataBlock(NewRange(mnmxst[0], mnmxst[1], mnmxst[2], flgs));
}

int
ypush_ptr(ypointer_t ptr, long *ntot)
{
  int typeid = Y_VOID;
  Array *a = Pointee(ptr);
  PushDataBlock(Ref(a));
  if (a && (a != (Array*)&nilDB)) {
    yget_dims(ntot, (long*)0, &a->type);
    typeid = a->ops->typeID;
  } else {
    if (ntot) *ntot = 0;
  }
  return typeid;
}

void
yarg_drop(int n)
{
  while (n-- > 0) {
    sp--;
    if (sp[1].ops==&dataBlockSym) Unref(sp[1].value.db);
  }
}

void
yarg_swap(int iarg1, int iarg2)
{
  OpTable *ops1 = sp[-iarg1].ops;
  long index1 = sp[-iarg1].index;
  SymbolValue value1 = sp[-iarg1].value;
  OpTable *ops2 = sp[-iarg2].ops;
  sp[-iarg1].ops = &intScalar;
  sp[-iarg1].index = sp[-iarg2].index;
  sp[-iarg1].value = sp[-iarg2].value;
  sp[-iarg2].ops = &intScalar;
  sp[-iarg1].ops = ops2;
  sp[-iarg2].value = value1;
  sp[-iarg2].index = index1;
  sp[-iarg2].ops = ops1;
}

/* result bits:
 * Y_1_BCAST if dims1 must be broadcast
 * Y_2_BCAST if dims2 must be broadcast
 * Y_1_EXTEND if rank1 < rank2
 * Y_2_EXTEND if rank1 > rank2
 */
int
yarg_conform(long *dims1, long *dims2, long *cfmdims)
{
  int flags = 0;
  long len1, len2;
  long n1 = *dims1++;
  long n2 = *dims2++;
  long cdims[Y_DIMSIZE];
  if (!cfmdims) cfmdims = cdims;
  *cfmdims++ = (n1>n2)? n1 : n2;
  while (n1>0 && n2>0) {
    n1--;
    n2--;
    len1 = *dims1++;
    len2 = *dims2++;
    if (len1 == len2) *cfmdims++ = len1;
    else if (len2 == 1) flags |= Y_2_BCAST, *cfmdims++ = len1;
    else if (len1 == 1) flags |= Y_1_BCAST, *cfmdims++ = len2;
    else flags = -1, *cfmdims++ = 0;
  }
  if (n1 > 0) {
    flags |= Y_2_EXTEND;
    do {
      len1 = *dims1++;
      if (len1 != 1) flags |= Y_2_BCAST;
      *cfmdims++ = len1;
    } while (--n1 > 0);
  } else if (n2 > 0) {
    flags |= Y_1_EXTEND;
    do {
      len2 = *dims2++;
      if (len2 != 1) flags |= Y_1_BCAST;
      *cfmdims++ = len2;
    } while (--n2 > 0);
  }
  return flags;
}

int
yarg_dims(int iarg, long *dims, long *cfmdims)
{
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    long tdims[Y_DIMSIZE];
    if (!dims) dims = tdims;
    if (s->ops==&referenceSym) s = &globTab[s->index];
    if (s->ops == &dataBlockSym) {
      if (s->value.db->ops->isArray)
        yget_dims(0, dims, &((Array *)s->value.db)->type);
      else if (s->value.db->ops == &lvalueOps)
        yget_dims(0, dims, &((LValue *)s->value.db)->type);
      else
        return -1;
    } else if (s->ops==&doubleScalar) {
      yget_dims(0, dims, &y_d_type);
    } else if (s->ops==&longScalar) {
      yget_dims(0, dims, &y_l_type);
    } else if (s->ops==&intScalar) {
      yget_dims(0, dims, &y_i_type);
    } else {
      return -1;
    }
    return cfmdims? yarg_conform(dims, cfmdims, cfmdims) : 0;
  }
  return -1;
}

int
yarg_bcast(int iarg, long *newdims)
{
  if (iarg >= 0) {
    Operations *ops;
    Member *type;
    void *p = ygeta_array(iarg, &ops, &type);
    if (p) {
      Symbol *s = sp - iarg;
      Array *a = PushDataBlock(NewArray(type->base, ypush_dims(newdims)));
      if (!Broadcast(a->value.c, a->type.dims, p, type->dims, type->base)) {
        if (s->ops == &dataBlockSym) {
          s->ops = &intScalar;
          Unref(s->value.db);
        }
        s->value = sp->value;
        s->ops = (sp--)->ops;
        return 0;
      }
      yarg_drop(1);
    }
  }
  return -1;
}

char *
yfind_name(long vndex)
{
  return (vndex>=0 && vndex<globalTable.nItems)?
    globalTable.names[vndex] : 0;
}

long
yfind_global(const char *name, long len)
{
  if (HashFind(&globalTable, name, len))
    return hashIndex;
  else
    return -1;
}

long
yget_global(const char *name, long len)
{
  if (!HashAdd(&globalTable, name, len)) {
    HASH_MANAGE(globalTable, Symbol, globTab);
    globTab[hashIndex].ops= &dataBlockSym;
    globTab[hashIndex].value.db= RefNC(&nilDB);
  }
  return hashIndex;
}

int
ypush_global(long vndex)
{
  if (vndex>=0 && vndex<globalTable.nItems) {
    if (globTab[vndex].ops == &dataBlockSym)
      sp[1].value.db = Ref(globTab[vndex].value.db);
    else
      sp[1].value = globTab[vndex].value;
    sp[1].ops = globTab[vndex].ops;
    sp++;
    return 0;
  } else {
    return 1;
  }
}

void
yput_global(long vndex, int iarg)
{
  if (vndex>=0 && iarg>=0) {
    Symbol *s = sp -iarg;
    if (s->ops == &referenceSym) {
      if (s->index == vndex) return;
      s = &globTab[s->index];
    } else if (s->ops!=&dataBlockSym && s->ops!=&doubleScalar &&
               s->ops!=&longScalar && s->ops!=&intScalar) {
      y_error("illegal stack element type in yput_global");
    }
    if (globTab[vndex].ops == &dataBlockSym) {
      globTab[vndex].ops = &intScalar;
      Unref(globTab[vndex].value.db);
    }
    if (s->ops == &dataBlockSym)
      globTab[vndex].value.db = Ref(s->value.db);
    else
      globTab[vndex].value = s->value;
    globTab[vndex].ops = s->ops;
  }
}

long
yget_ref(int iarg)
{
  if (iarg>=0 && sp[-iarg].ops==&referenceSym)
    return sp[-iarg].index;
  else
    return -1;
}

typedef struct y_userinst_t y_userinst_t;

union y_uo_body_t {
  char c[8];
  double d;
  void *p;
  void (*f)(void);
};

typedef struct y_uo_t y_uo_t;
struct y_uo_t {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  y_userobj_t *uo_type;
  union y_uo_body_t body;
};

static void y_uo_free(void *vuo);
static void y_uo_eval(Operand *op);
static void y_uo_extract(Operand *op, char *name);
static void y_uo_print(Operand *op);

static void y_scratch_free(void *vuo);

static Operations y_uo_ops = {
  &y_uo_free, T_OPAQUE, 0, T_STRING, "scratch_object",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &y_uo_eval, &SetupX, &y_uo_extract, &MatMultX, &y_uo_print
};

void *ypush_obj(y_userobj_t *uo_type, unsigned long size)
{
  y_uo_t *uo;
  if (! uo_type->uo_ops) {
    /* side effect on first call -- somewhat dangerous! */
    Operations *ops = p_malloc(sizeof(Operations));
    memcpy(ops, &y_uo_ops, sizeof(Operations));
    ops->typeName = uo_type->type_name;
    uo_type->uo_ops = ops; /* AFTER ops properly initialized */
  }
  uo = p_malloc(sizeof(y_uo_t) - sizeof(union y_uo_body_t) + size);
  memset(uo, 0, sizeof(y_uo_t) - sizeof(union y_uo_body_t) + size);
  uo->ops = uo_type->uo_ops;
  uo->uo_type = uo_type;
  PushDataBlock(uo);
  return uo->body.c;
}

/* The function yfunc_obj initializes uo_ops member in a special way
 * for function like objects.  It must be applied prior to the first
 * call to ypush_obj.  Alternatively, you may push a new
 * function-like object by:
 *   user_object = ypush_obj(yfunc_obj(uo_type), sizeof(user_object_type))
 */
y_userobj_t *
yfunc_obj(y_userobj_t *uo_type)
{
  Operations *ops;
  if (! uo_type->uo_ops) {
    if (! uo_type->on_eval) {
      y_error("(BUG) foreign function-like object with no on_eval method makes no sense");
    }
    ops = p_malloc(sizeof(Operations));
    memcpy(ops, &y_uo_ops, sizeof(Operations));
    ops->Setup = y_setup_func_hack;
    ops->typeName = uo_type->type_name;
    uo_type->uo_ops = ops; /* AFTER ops properly initialized */
  }
  return uo_type;
}

static void
y_uo_free(void *vuo)
{
  y_uo_t *uo = vuo;
  if (uo->uo_type->uo_ops != uo->ops)
    y_error("(BUG) corrupted user object in y_uo_free");
  if (uo->uo_type->on_free)
    uo->uo_type->on_free(uo->body.c);
  p_free(uo);
}

static void
y_uo_eval(Operand *op)
{
  y_uo_t *uo = op->value;
  if (uo->uo_type->uo_ops != uo->ops)
    y_error("(BUG) corrupted user object in y_uo_eval");
  if (uo->uo_type->on_eval) {
    Symbol *stack;
    long owner = op->owner - spBottom;
    uo->uo_type->on_eval(uo->body.c, op->references); /*argc in references*/
    /* put result in correct place on stack, unless an interpreted
     * function has been pushed into place, in which case assume on_eval
     * took care of proper stack alignment
     */
    if (sp>spBottom+owner && sp->ops!=&returnSym) {
      Symbol *s = spBottom + owner;
      PopTo(s);
      while (sp - s > 0) {
        stack = sp--; /* sp decremented BEFORE stack element is deleted */
        if (stack->ops == &dataBlockSym) Unref(stack->value.db);
      }
    }
  } else {
    y_error("user object has no on_eval method");
  }
}

static void
y_uo_extract(Operand *op, char *name)
{
  y_uo_t *uo = op->value;
  if (uo->uo_type->uo_ops != uo->ops)
    y_error("(BUG) corrupted user object in y_uo_extract");
  if (uo->uo_type->on_extract) {
    Symbol *stack;
    long owner = op->owner - spBottom, sp0 = sp - spBottom;
    uo->uo_type->on_extract(uo->body.c, name);
    PopTo(spBottom+owner);
    while (sp - spBottom > sp0) {
      stack = sp--; /* sp decremented BEFORE stack element is deleted */
      if (stack->ops == &dataBlockSym) Unref(stack->value.db);
    }
  } else {
    y_error("user object has no on_extract method");
  }
}

static void
y_uo_print(Operand *op)
{
  y_uo_t *uo = op->value;
  if (uo->uo_type->uo_ops != uo->ops)
    y_error("(BUG) corrupted user object in y_uo_print");
  if (uo->uo_type->on_print) {
    ForceNewline();
    uo->uo_type->on_print(uo->body.c);
    ForceNewline();
  } else {
    PrintX(op);
  }
}

/* similar to yget_use, but no use increment */
void *
yget_obj_s(DataBlock *db)
{
  y_uo_t *uo = (y_uo_t *)db;
  if (uo->uo_type->uo_ops != uo->ops)
    y_error("(BUG) corrupted user object in yget_obj_s");
  return uo->body.c;
}

void *
yget_obj(int iarg, y_userobj_t *uo_type)
{
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops==&referenceSym) {
      /* change reference to global object into object itself */
      Symbol *g = &globTab[s->index];
      if (g->ops == &dataBlockSym) s->value.db = Ref(g->value.db);
      else s->value = g->value;
      s->ops = g->ops;
    }
    if (s->ops == &dataBlockSym) {
      if (!uo_type) {
        return s->value.db->ops->typeName;
      } else if (s->value.db->ops == uo_type->uo_ops) {
        y_uo_t *uo = (y_uo_t *)s->value.db;
        if (uo->uo_type->uo_ops != uo->ops)
          y_error("(BUG) corrupted user object in yget_obj");
        return uo->body.c;
      }
    }
    if (uo_type)
      y_errorq("expecting argument of type %s", uo_type->type_name);
  }
  return 0;
}

void
y_print(const char *text, int newline)
{
  PrintFunc(text);
  if (newline) ForceNewline();
}

static Operations y_scratch_ops = {
  &y_scratch_free, T_OPAQUE, 0, T_STRING, "scratch_object",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &y_uo_eval, &SetupX, &y_uo_extract, &MatMultX, &y_uo_print
};

y_userobj_t y_scratch_obj = { "scratch", 0, 0, 0, 0, &y_scratch_ops };

typedef struct y_scratch_t y_scratch_t;
struct y_scratch_t {
  y_userobj_t uot;
  union y_uo_body_t body;
};

static void
y_scratch_free(void *vuo)
{
  y_uo_t *uo = vuo;
  y_scratch_t *obj = (y_scratch_t *)uo->body.c;
  if (uo->uo_type->uo_ops != uo->ops)
    y_error("(BUG) corrupted user object in y_uo_free");
  if (obj->uot.on_free)
    obj->uot.on_free(obj->body.c);
  p_free(uo);
}

void *
ypush_scratch(unsigned long size, void (*on_free)(void *))
{
  y_uo_t *uo;
  y_scratch_t *obj;
  uo = p_malloc(sizeof(y_uo_t) + sizeof(y_scratch_t)
                - 2*sizeof(union y_uo_body_t) + size);
  /* uo is a y_uo_t whose body begins with a copy of y_scratch_obj,
   * which is a y_userobj_t -- the specified on_free method gets
   * inserted into this copy, becoming a special y_userobj_t for
   * this scratch object only
   * after y_scratch_obj comes the size bytes of scratch space
   */
  uo->references = 0;
  uo->ops = &y_scratch_ops;
  obj = (y_scratch_t *)uo->body.c;
  uo->uo_type = &obj->uot;
  memcpy(&obj->uot, &y_scratch_obj, sizeof(y_userobj_t));
  obj->uot.on_free = on_free;
  memset(obj->body.c, 0, size);
  PushDataBlock(uo);
  return obj->body.c;
}

void *
yget_use(int iarg)
{
  if (iarg >= 0) {
    DataBlock *db = ForceToDB(sp - iarg);
    if (db != &nilDB) return Ref(db);
  }
  return 0;
}

void
ypush_use(void *handle)
{
  if (!handle) ypush_nil();
  else PushDataBlock(handle);
}

void
ykeep_use(void *handle)
{
  if (handle) {
    DataBlock *db = handle;
    PushDataBlock(RefNC(db));
  } else {
    ypush_nil();
  }
}

void
ydrop_use(void *handle)
{
  DataBlock *db = handle;
  Unref(db);
}

/* defined in task.c */
extern void (*CleanUpForExit)(void);
static void (*y_obsolete_cleanup)(void) = 0;
static void yon_quit_caller(void);
static struct yon_quit_t {
  void (*on_quit)(void);
  struct yon_quit_t *next;
} *yon_quit_stack = 0;
static
void yon_quit_caller(void)
{
  struct yon_quit_t *oqcb = yon_quit_stack;
  while (oqcb) {
    oqcb->on_quit();
    oqcb = oqcb->next;
  }
  if (y_obsolete_cleanup) y_obsolete_cleanup();
}
static int yon_quit_installed = 0;

void
ycall_on_quit(void (*on_quit)(void))
{
  struct yon_quit_t *oqcb = yon_quit_stack;
  while (oqcb) {
    if (oqcb->on_quit == on_quit) return;
    oqcb = oqcb->next;
  }
  oqcb = p_malloc(sizeof(struct yon_quit_t));
  oqcb->on_quit = on_quit;
  oqcb->next = yon_quit_stack;
  yon_quit_stack = oqcb;
  if (!yon_quit_installed) {
    y_obsolete_cleanup = CleanUpForExit;
    CleanUpForExit = &yon_quit_caller;
    yon_quit_installed = 1;
  }
}

void
ycancel_on_quit(void (*on_quit)(void))
{
  struct yon_quit_t *oqcb = yon_quit_stack, **prev = &yon_quit_stack;
  while (oqcb) {
    if (oqcb->on_quit == on_quit) {
      *prev = oqcb->next;
      p_free(oqcb);
      return;
    }
    prev = &oqcb->next;
    oqcb = *prev;
  }
}

void
y_error(const char *msg)
{
  YError(msg);
}

static void y_ew_n(const char *msg_format, long n, int warn);
static void y_ew_q(const char *msg_format, const char *q, int warn);

void
y_errorn(const char *msg_format, long n)
{
  y_ew_n(msg_format, n, 0);
}

void
y_errorq(const char *msg_format, const char *q)
{
  y_ew_q(msg_format, q, 0);
}

void
y_warn(const char *msg)
{
  YWarning(msg);
}

void
y_warnn(const char *msg_format, long n)
{
  y_ew_n(msg_format, n, 1);
}

void
y_warnq(const char *msg_format, const char *q)
{
  y_ew_q(msg_format, q, 1);
}

static void
y_ew_n(const char *msg_format, long n, int warn)
{
  char msg[192];
  long nmax = 130;
  long nmsg = 0;
  const char *fmt = strstr(msg_format, "%ld");
  if (!fmt) {
    fmt = strstr(msg_format, "%d");
    if (!fmt) fmt = msg_format + strlen(msg_format);
  }
  msg[0] = '\0';
  if (nmax > nmsg) {
    long len = fmt-msg_format;
    if (len > nmax-nmsg) len = nmax-nmsg;
    strncat(msg+nmsg, msg_format, len);
    nmsg += len;
  }
  if (nmax > nmsg) {
    sprintf(msg+nmsg, "%ld", n);
    nmsg += strlen(msg+nmsg);
  }
  if (nmax>nmsg && fmt[0]=='%') {
    fmt += 2;
    if (fmt[-1]=='l') fmt++;
    strncat(msg+nmsg, fmt, nmax-nmsg);
  }
  if (warn) YWarning(msg);
  else YError(msg);
}

static void
y_ew_q(const char *msg_format, const char *q, int warn)
{
  char msg[192];
  long nmax = 130;
  long nmsg = 0;
  const char *fmt = strstr(msg_format, "%s");
  if (!fmt)
    fmt = msg_format + strlen(msg_format);
  msg[0] = '\0';
  if (nmax > nmsg) {
    long len = fmt-msg_format;
    if (len > nmax-nmsg) len = nmax-nmsg;
    strncat(msg+nmsg, msg_format, len);
    nmsg += len;
  }
  if (nmax > nmsg) {
    strncat(msg+nmsg, q, nmax-nmsg);
    nmsg += strlen(msg+nmsg);
  }
  if (nmax>nmsg && fmt[0]=='%') {
    fmt += 2;
    strncat(msg+nmsg, fmt, nmax-nmsg);
  }
  if (warn) YWarning(msg);
  else YError(msg);
}

void
y_errquiet(void)
{
  YError(".SYNC.");
}
