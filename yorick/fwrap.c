/*
 * $Id: fwrap.c,v 1.6 2010-08-08 04:05:22 dhmunro Exp $
 * implement function argument wrapping with wrap_args
 */
/* Copyright (c) 2009, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "yapi.h"
#include "ydata.h"
#include "pstdlib.h"
#include <stdio.h>
#include <string.h>

/* interpreted interface consists of wrap_args function and
 * wrapped_func and wrapped_args objects
 */

extern ybuiltin_t Y_wrap_args;
typedef struct y_wrapped_func y_wrapped_func;
typedef struct y_wrapped_args y_wrapped_args;

static void ywrap_f_free(void *obj);
static void ywrap_f_print(void *obj);
static void ywrap_fake_eval(void *obj, int argc);
static void ywrap_f_eval(Operand *op);
y_userobj_t ywrap_f_ops = {
  "wrapped_func", &ywrap_f_free, &ywrap_f_print, &ywrap_fake_eval, 0, 0 };

static void ywrap_a_free(void *obj);
static void ywrap_a_print(void *obj);
static void ywrap_a_eval(void *obj, int argc);
y_userobj_t ywrap_a_ops = {
  "wrapped_args", &ywrap_a_free, &ywrap_a_print, &ywrap_a_eval, 0, 0 };

struct y_wrapped_func {
  Function *f;
};

struct y_wrapped_args {
  int npos, nkey;
  Symbol *args;
};

int
yarg_func(int iarg)
{
  int is_func = 0;
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops==&referenceSym) s = &globTab[s->index];
    if (s->ops == &dataBlockSym) {
      Operations *ops= s->value.db->ops;
      if (ops == &functionOps) is_func = 1;
      else if (ops == &builtinOps) is_func = 2;
      else if (ops == &auto_ops) is_func = 3;
      else if (s->value.db->ops->typeName==ywrap_f_ops.type_name) is_func = 4;
      else if (yo_is_closure(iarg)) is_func = 5;
    }
  }
  return is_func;
}

void
Y_wrap_args(int argc)
{
  y_wrapped_func *wf;
  Function *f;
  long index = sp->index;
  if (sp->ops == &referenceSym) ReplaceRef(sp);
  else index = -1;
  f = (Function *)sp->value.db;
  if (argc!=1 || sp->ops!=&dataBlockSym || f->ops!=&functionOps || index<0)
    y_error("wrap_args takes one interpreted function as its argument");
  /* hasPosList bit 1 is .. parameter (va), bit 2 marks 1st param as output */
  if (f->nPos!=1 || f->nKey!=0 || f->hasPosList&3)
    y_error("wrap_args argument function must have one non-output parameter");
  if (!ywrap_f_ops.uo_ops) {
    yfunc_obj(&ywrap_f_ops);
    ((Operations *)ywrap_f_ops.uo_ops)->Eval = &ywrap_f_eval;
    yfunc_obj(&ywrap_a_ops);
  }
  wf = ypush_obj(&ywrap_f_ops, sizeof(y_wrapped_func));
  wf->f = Ref(f);
  yput_global(index, 0);
}

static void
ywrap_f_print(void *obj)
{
  y_wrapped_func *wf = obj;
  Instruction *pc = wf->f->code;
  char *name = yfind_name(pc->index);
  y_print("wrapped func ", 0);
  y_print(name? name : "<nameless>", 0);
  y_print("(args)", 1);
}

static void
ywrap_a_print(void *obj)
{
  y_wrapped_args *wa = obj;
  char msg[80];
  sprintf(msg, "wrapped args, %d positionals, %d keywords",
          wa->npos, wa->nkey);
  y_print(msg, 1);
}

static void
ywrap_f_free(void *obj)
{
  y_wrapped_func *wf = obj;
  Unref(wf->f);
}

static void
ywrap_a_free(void *obj)
{
  y_wrapped_args *wa = obj;
  Symbol *args = wa->args;
  if (args && wa->npos) {
    /* handle output args now
     * - we are being invoked from the Drop call in the final line
     *   of fnctn.c:Return()
     * - note that weird stuff happens if wrapped function is written
     *   in such a way that args survives beyond function lifetime
     */
    Symbol *glob;
    int i;
    /* move output argument to globTab, or discard non-output argument */
    for (i=0 ; i<wa->npos+(wa->nkey<<1) ; ++i) {
      if (!args[i].ops) continue;
      if (args[i].index != -1) {
        /* this referenceSym argument may have been set with output value
         * all symbols have been restored to their external values, so
         * can go ahead and overwrite with output value
         */
        glob = globTab + args[i].index;
        if (glob->ops == &dataBlockSym) {
          /* delete current value */
          glob->ops = &intScalar;
          Unref(glob->value.db);
        }
        /* transfer args[i] dataBlockSym use counter value to glob */
        glob->value = args[i].value;
        glob->ops = args[i].ops;
      } else if (args[i].ops == &dataBlockSym) {
        Unref(args[i].value.db);
      }
      args[i].ops = &intScalar;  /* dud Symbol just to be safe */
    }
    wa->args = 0;
  }
  p_free(args);
}

static void
ywrap_fake_eval(void *obj, int argc)
{
  y_error("(BUG) wrap_args should have overridden ywrap_fake_eval");
}

static void
ywrap_f_eval(Operand *op)
{
  int argc = op->references;   /* (sic) # of actual parameters supplied */
  Symbol *args, *me = sp - argc;
  DataBlock *uo = op->value;
  y_wrapped_func *wf = (y_wrapped_func *)yget_obj_s(uo);
  y_wrapped_args *wa;
  int i, j, k;
  /* wf has been called, time to wrap its arguments */
  wa = ypush_obj(&ywrap_a_ops, sizeof(y_wrapped_args));
  wa->npos = wa->nkey = 0;
  if (argc) {
    wa->args = args = p_malloc(sizeof(Symbol)*argc);
    for (i=j=k=0 ; i<argc ; i++) {
      if (!me[1+i].ops) {
        if (!k) k = 1+i;
        i++;
        continue;
      }
      args[j] = me[1+i];
      if (args[j].ops == &referenceSym) {
        ReplaceRef(args+j);  /* crucial: leaves args[].index untouched */
      } else {
        args[j].index = -1;  /* mark as not a reference sym */
      }
      j++;
    }
    if (k) {
      for (i=0 ; k<argc ; k++) {
        if (me[k].ops) continue;
        args[j+(i++)] = me[k++];
        args[j+i] = me[k];
        if (args[j+i].ops == &referenceSym) {
          /* never actually get here, keywords never reference syms */
          ReplaceRef(args+j+i);  /* crucial: leaves args[].index untouched */
        } else {
          args[j+i].index = -1;  /* mark as not a reference sym */
        }
        i++;
      }
    } else {
      i = 0;
    }
    me[1].ops = &intScalar;
    me[1].value.db = sp->value.db;
    sp = me + 1;
    me[1].ops = &dataBlockSym;
  } else {
    i = j = 0;
    wa->args = 0;
  }
  wa->npos = j;
  wa->nkey = i>>1;
  /* chain to EvalFN as f(args), post-call work done in ywrap_a_free */
  me[0].ops = &intScalar;
  Unref(uo);
  me[0].value.db = op->value = Ref(wf->f);
  op->references = 1;   /* (sic) set # of actual parameters supplied */
  wf->f->ops->Eval(op);
}

static void
ywrap_a_eval(void *obj, int argc)
{
  y_wrapped_args *wa = obj;
  long junk[3];
  int isquery = (argc>0)? yget_range(argc-1, junk) : 0;
  int iskey = 0;
  Symbol *arg = 0;
  long iarg = 0;
  char *key = 0;
  if (argc!=1 && argc!=2)
    y_error("wrapped arg only accepts one or two arguments");
  if (!sp->ops)
    y_error("wrapped arg only accepts no keyword arguments");
  if (isquery == (Y_PSEUDO|Y_MIN_DFLT|Y_MAX_DFLT)) {
    isquery = 1;
  } else if (isquery == (Y_RUBBER1|Y_MIN_DFLT|Y_MAX_DFLT)) {
    if (argc == 1) isquery = 2;
    else isquery = 3;
  }
  if (!isquery) iskey = yarg_string(argc-1)==1;
  if (!iskey && !isquery) {
    iarg = ygets_l(argc-1);
    if (!iarg) isquery = 2;
  } else if (iskey) {
    key = ygets_q(argc-1);
  }
  if (isquery && argc==2) {
    if (isquery != 3) {
      iskey = (yarg_string(0) == 1);
      if (!iskey) iarg = ygets_l(0);
      else key = ygets_q(0);
    } else {
      isquery = 1;
      if (!yarg_nil(0)) iarg = ygets_l(0);
    }
  }
  if (key) {
    char *name;
    int i;
    for (i=0 ; i<(wa->nkey<<1) ; i+=2) {
      name = yfind_name(wa->args[wa->npos+i].index);
      if (name && !strcmp(name, key)) break;
    }
    iarg = -1 - (i>>1);
  }
  if (iarg > 0) {
    if (iarg <= wa->npos) arg = wa->args + iarg-1;
  } else if (iarg < 0) {
    if (iarg >= -wa->nkey) arg = wa->args + wa->npos + 1 + ((-1-iarg)<<1);
  }
  if (isquery == 2) {
    if (!iarg) {         /* ARGS(0) or ARGS(*) */
      ypush_long(wa->npos);
    } else {             /* ARGS(0,i) */
      if (!arg) ypush_long(2);
      else if (arg->index == -1) ypush_long(1);
      else ypush_long(0);
    }
  } else if (isquery == 1) {
    char **q;
    if (!iarg) {         /* ARGS(-) */
      if (wa->nkey) {
        int i;
        junk[0] = 1;
        junk[1] = wa->nkey;
        q = ypush_q(junk);
        for (i=0 ; i<wa->nkey ; i++)
          q[i] = p_strcpy(yfind_name(wa->args[wa->npos+2*i].index));
      } else {
        ypush_nil();
      }
    } else {             /* ARGS(-,i) */
      q = ypush_q(0);
      if (arg && arg->index!=-1) {
        q[0] = p_strcpy(yfind_name(arg->index));
      } else {
        q[0] = 0;
      }
    }
  } else {
    if (argc==2 && yget_range(0, junk)==(1|Y_MIN_DFLT|Y_MAX_DFLT))
      argc = 3;
    if (argc == 1 || argc == 3) {     /* ARGS(i) or ARGS(i,:) */
      if (arg) {
        if (arg->ops == &dataBlockSym) {
          /* fetch lvalues now unless specifically directed not to */
          if (argc==1 && arg->value.db->ops == &lvalueOps)
            FetchLValue(arg->value.db, arg);
          sp[1].ops = &dataBlockSym;
          sp[1].value.db = Ref(arg->value.db);
        } else {
          sp[1].ops = arg->ops;
          sp[1].value = arg->value;
        }
        sp++;
      } else {
        ypush_nil();
      }
    } else {             /* ARGS, i, value */
      long index = arg? arg->index : -1;
      Symbol *s = (sp->ops == &referenceSym)? globTab+sp->index : sp;
      if (index != -1) {
        if (arg->ops == &dataBlockSym) {
          arg->ops = &intScalar;
          Unref(arg->value.db);
        }
        if (s->ops == &dataBlockSym) {
          arg->value.db = Ref(s->value.db);
          arg->ops = &dataBlockSym;
          if (arg->value.db->ops == &lvalueOps)
            FetchLValue(arg->value.db, arg);
        } else {
          arg->value = s->value;
          arg->ops = s->ops;
        }
      }
      if (!yarg_subroutine()) ypush_nil();
    }
  }
}
