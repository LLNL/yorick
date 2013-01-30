/*
 * $Id: oxy.c,v 1.6 2010-11-09 02:39:45 dhmunro Exp $
 * implementation of object extension
 */
/* Copyright (c) 2010 David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* changed:
 * fnctn.c, ydata.h, yapi.h, yapi.c, fwrap.c
 */

#include "ydata.h"
#include "pstdlib.h"
#include "phash.h"
#include <stdio.h>
#include <string.h>

extern BuiltIn Y_use, Y_save, Y_restore, Y_is_obj, Y_closure, Y_gaccess;
extern BuiltIn Y_use_method;

/* ------------------------------------------------------------------------ */
/* oxy_object is the base class for yorick's object oriented extension
 */

static void yo_on_free(void *uo);
static void yo_on_print(void *uo);
static void yo_on_extract(void *uo, char *name);
static void yo_on_eval(void *uo, int nargs);
static void yo_on_evalx(void *uo, int nargs, int icx, int meth);

static y_userobj_t yo_uops =
  { "oxy_object", yo_on_free, yo_on_print, yo_on_eval, yo_on_extract, 0 };

typedef struct yo_data_t yo_data_t;
struct yo_data_t {
  yo_ops_t *ops;
  void *on_destroy;  /* all oxy_object have optional destructor(s) */
  void *obj;
};

union yo_align_t { double d; void *p; void (*f)(void); };
struct yo_extend_t {
  yo_data_t c;
  union yo_align_t u;
};
#define YO_DATA_T_SZ (sizeof(struct yo_extend_t)-sizeof(union yo_align_t))

void *
yo_push_alloc(yo_ops_t *ops, unsigned long size)
{
  yo_data_t *uo = ypush_obj(&yo_uops, YO_DATA_T_SZ+size);
  uo->ops = ops;
  uo->on_destroy = 0;
  return uo->obj = ((char*)uo) + YO_DATA_T_SZ;
}

void *
yo_push(yo_ops_t *ops, void *obj)
{
  yo_data_t *uo = ypush_obj(&yo_uops, sizeof(yo_data_t));
  uo->ops = ops;
  uo->on_destroy = 0;
  return uo->obj = obj;
}

static yo_data_t *yo_iostream(int iarg);  /* create IOStream wrapper */

void *
yo_get(int iarg, yo_ops_t **ops)
{
  yo_data_t *uo = 0;
  if (yget_obj(iarg,0) == yo_uops.type_name) uo = yget_obj(iarg, &yo_uops);
  else if (yarg_typeid(iarg) == Y_STREAM) uo = yo_iostream(iarg);
  if (uo && ops) *ops = uo->ops;
  return uo? uo->obj : 0;
}

static void
yo_on_free(void *vuo)
{
  yo_data_t *uo = vuo;
  /* interpreted on_destroy callbacks independent of object type */
  /* if (uo->on_destroy) yo_do_hooks(uo->u.on_destroy, uo); */
  if (uo->obj) {
    /* dealloc method does p_free if yo_push, but not if yo_push_alloc */
    if (uo->ops->dealloc) uo->ops->dealloc(uo->obj);
    uo->obj = 0;
  }
}

static void
yo_on_print(void *vuo)
{
  yo_ops_t *ops = ((yo_data_t*)vuo)->ops;
  void *obj = ((yo_data_t*)vuo)->obj;
  if (ops->print) {
    ops->print(obj);
  } else {
    char msg[256];
    long count = ops->count(obj);
    sprintf(msg, "object [%ld]: ", count);
    strncat(msg, ops->type_name, 255);
    msg[255] = '\0';
    y_print(msg, 1);
  }
}

static void
yo_on_extract(void *uo, char *name)
{
  yo_ops_t *ops = ((yo_data_t*)uo)->ops;
  void *obj = ((yo_data_t*)uo)->obj;
  ops->get_q(obj, name, -1L);
}

/* ------------------------------------------------------------------------ */
/* oxy_context is internal object to implement use function
 * -replaces oxy_object, just under func/wrapped_func/builtin on stack
 * three external functions:
 *   void yo_mk_context(int iarg)        replace oxy_object by oxy_context
 *   int yo_cswap(int iarg, long index)  add one use item to oxy_context
 *   void yo_cupdate(int iarg)           update oxy_object from all use items
 *
 * on obj(member,...) on_eval, stack begins like this:
 *   obj  member  arg1  ...
 * mk_context replaces obj by context:
 *   context  member  arg1  ...
 *      +-->obj    <context owns stack reference to obj>
 *      +-->xlist  <context maintains list of externals>
 * The xlist contains external values of variables shadowed by object
 * members swapped into globtab by the use function.  Return from the
 * (interpreted) caller of use moves those variables from globtab back
 * into the object, and replaces the external values stored in xlist.
 *
 * What we really want is to replace the object itself by this context,
 * so that anyone requesting a member gets the thing in globtab.  But
 * this doesn't work for a general object, since its members might not
 * be anything storable in globtab.  When the object is such a thing,
 * however, the use function is always going to be problematic.  We also
 * want compiled functions to "play nice" with the use function, so that
 * instead of directly reading or writing from their context object,
 * they use globtab for members being "use"d.
 * Problem 1:
 *   Functions called in an object context should be able to call sibling
 * methods via use(sibling, ...).  This should work even if the object
 * (or its sibling caller) has called use to work with some members in
 * globtab.  This requires use to be smart enough to realize when a member
 * is already in use by a function up the call chain.
 * A related problem is that compiled methods also need to be able to
 * detect when a "use" function has swapped object members into globtab,
 * so it can operate on those instead of the obsolete copies in the
 * object itself.  Do this with yo_use analogue to interpreted use.
 * To handle the main problem, it would suffice for use to replace itself
 * by an "indirect context" that refers back to the original context,
 * rather than to the object.  Any interpreted returns with this modified
 * context would not unswap globtab with object members, while any use
 * functions would act on the original context, so that the outermost
 * return would pick them up.
 * --This requires that cswap be a no-op when a member is already in use,
 *   making it somewhat difficult to handle situations in which the
 *   context has new members added during the call.
 * --Note that save,use and restore,use don't play nice with use.
 *   Probably want just use instead of save,use.  The restore,use form
 *   would undo any changes to a used variable.
 * Problem 2:
 *   There should be a way to invoke a "friend" method in the current
 * context, even when the "friend" is not a member of the context
 * object.  The primary example is a method of a derived class which
 * needs to call the method of the same name in the base class.  This
 * can be done easily if use(base_api(method), ...) were accepted, that
 * is, if use accepts a non-simple variable reference.
 */
static void yo_mk_context(int iarg, int icx);
static int yo_cswap(int iarg, long index);
static void *yo_seek_context(int iarg, int *piarg, int push);

static void yo_con_free(void *uo);
static y_userobj_t yo_cops =
  { "oxy_context", yo_con_free, 0, 0, 0, 0 };

typedef struct yo_symbol_t yo_symbol_t;
struct yo_symbol_t {
  long m;    /* member index */
  Symbol s;  /* external value, s.index is globtab index */
};

typedef struct yo_context_t yo_context_t;
struct yo_context_t {
  /* obj is the actual context object
   * ocount is the last known number of members in obj, which
   * determines how many bits in xlist are allocated
   * ocount<0 indicates that this is an "indirect" context generated
   *   by the use function, with the true context at this sp[ocount]
   *   nxlist = xlist = 0 in that case
   */
  DataBlock *obj;
  long ocount;
  /* xlist starts nil, grows on subroutine calls to use
   * xlist[i] is external value of item saved before loading from obj
   * xlist[i].index is member index in obj, from which global index
   *   can be retrieved
   * - bit flags begin at xlist[nxlist], set to mark members which
   *   currently reside in globtab; there are ocount bit flags
   * - on delete, restored in reverse order created
   */
  long nxlist;
  yo_symbol_t *xlist;
  unsigned char *bits;  /* set indicates already swapped */
};

static void
yo_mk_context(int iarg, int icx)
{
  if (icx>iarg || yget_obj(iarg,0)==yo_uops.type_name) {
    yo_context_t *uo = ypush_obj(&yo_cops, sizeof(yo_context_t));
    uo->nxlist = 0;
    uo->xlist = 0;
    uo->bits = 0;
    if (icx > iarg) {  /* make indirect context */
      uo->ocount = iarg - icx;  /* location of direct context */
      uo->obj = 0;
    } else {           /* make direct context */
      uo->ocount = 0;
      uo->obj = yget_use(iarg+1);
    }
    yarg_swap(0, iarg+1);
    yarg_drop(1);
  } else {
    y_error("(BUG) yo_mk_context expected oxy_object");
  }
}

static yo_symbol_t *yo_xlistx(yo_symbol_t *s, long n,
                              long ocount, long ncount);

static int
yo_cswap(int iarg, long index)
{
  if (yget_obj(iarg,0) == yo_cops.type_name) {
    yo_context_t *uc = yget_obj(iarg, &yo_cops);
    yo_data_t *uo;
    yo_ops_t *ops;
    void *obj;
    yo_symbol_t *s;
    long m = 0, ocount, n;
    if (uc->ocount < 0) {
      iarg -= uc->ocount;
      uc = (yget_obj(iarg,0)==yo_cops.type_name)? yget_obj(iarg,&yo_cops) : 0;
      if (!uc || uc->ocount<0)
        y_error("(BUG) yo_cswap bad indirect oxy_context");
    }
    uo = yget_obj_s(uc->obj);
    ops = uo->ops;
    obj = uo->obj;
    if (index<0 || index>=globalTable.nItems)
      return 1;  /* no such index in globtab */
    if (ops->find_mndx) {
      m = ops->find_mndx(obj, globalTable.names[index], index);
      if (m < 1)
        return 2;  /* no member of same name in oxy_object */
    }
    ocount = uc->ocount;
    s = uc->xlist;
    n = uc->nxlist;
    if (!n || (n>=4 && !(n&(n-1)))) {
      long ncount = ops->count(obj);
      uc->xlist = s = yo_xlistx(s, n, ocount, ncount);
      uc->ocount = ocount = ncount;
      uc->bits = (unsigned char*)(s + (n?n+n:4));
    }
    if (m > ocount)
      y_error("(BUG) yo_cswap object count less than returned index");
    if (uc->bits[((unsigned long)m-1)>>3] & (1<<((m-1)&7)))
      return 0;  /* this member already in global */
    /* copy globtab[index] to oxy_context xlist */
    ypush_global(index);
    s[n].m = m;
    s[n].s.ops = sp->ops;
    s[n].s.index = index;
    s[n].s.value = sp->value;
    sp--;  /* just transfer use from stack to xlist */
    uc->nxlist = ++n;
    /* copy corresponding oxy_object member to globtab[index] */
    if (ops->find_mndx) ops->get_i(obj, m);
    else ops->get_q(obj, globalTable.names[index], index);
    yput_global(index, 0);
    yarg_drop(1);
    /* mark this member */
    uc->bits[((unsigned long)m-1)>>3] |= 1<<((m-1)&7);
  } else {
    y_error("(BUG) yo_cswap expected oxy_context");
  }
  return 0;  /* success */
}

static yo_symbol_t *
yo_xlistx(yo_symbol_t *s, long n, long ocount, long ncount)
{
  long i, nn = n? n+n : 4;
  unsigned char *bo, *bn;
  ncount = ((unsigned long)ncount >> 3) + 1;  /* overestimate, at least 1 */
  s = p_realloc(s, nn*sizeof(yo_symbol_t)+ncount);
  bo = (unsigned char *)(s+n);
  bn = (unsigned char *)(s+nn);
  ocount = (ocount>0)? ((unsigned long)(ocount-1) >> 3)+1 : 0; /* exact */
  for (i=ncount-1 ; i>=ocount ; i--) bn[i] = 0;
  for ( ; i>=0 ; i--) bn[i] = bo[i];
  return s;
}

void
yo_cupdate(int iarg)
{
  if (yget_obj(iarg,0) == yo_cops.type_name) {
    yo_context_t *uc = yget_obj(iarg, &yo_cops);
    yo_data_t *uo;
    yo_ops_t *ops;
    void *obj;
    yo_symbol_t *s;
    long i;
    int zap = 1;
    if (uc->ocount < 0) {
      iarg -= uc->ocount;
      uc = (yget_obj(iarg,0)==yo_cops.type_name)? yget_obj(iarg,&yo_cops) : 0;
      if (!uc || uc->ocount<0)
        y_error("(BUG) yo_cupdate bad indirect oxy_context");
      zap = 0;
    }
    uo = yget_obj_s(uc->obj);
    ops = uo->ops;
    obj = uo->obj;
    s = uc->xlist;
    i = uc->nxlist;
    /* in reverse order of yo_cswap */
    while (--i >= 0) {
      if (s[i].s.index<0 || s[i].m<0) continue;
      /* copy globtab[index] to corresponding oxy_object member */
      ypush_global(s[i].s.index);
      if (ops->find_mndx) ops->set_i(obj, s[i].m, 0);
      else ops->set_q(obj, globalTable.names[s[i].s.index], s[i].s.index, 0);
      if (zap)
        s[i].m = -1;  /* remove xlist correspondance to oxy_object member */
      yarg_drop(1);
    }
  } else {
    y_error("(BUG) yo_cupdate expected oxy_context");
  }
}

static void
yo_con_free(void *vuc)
{
  yo_context_t *uc = vuc;
  DataBlock *db = uc->obj;
  if (db) {
    yo_symbol_t *s = uc->xlist;
    long i = uc->nxlist;
    if (s) {
      long index;
      /* in reverse order of yo_cswap */
      while (--i >= 0) {
        index = s[i].s.index;
        if (index < 0) continue;
        /* move oxy_context xlist element onto stack, transferring use */
        sp[1].ops = s[i].s.ops;
        sp[1].value = s[i].s.value;
        s[i].s.index = -1;  /* disable this xlist element */
        sp++;
        /* restore corresponding globtab[index] */
        yput_global(index, 0);
        yarg_drop(1);
      }
      uc->nxlist = 0;
      uc->xlist = 0;
      uc->bits = 0;
      p_free(s);
    }
    /* now that xlist is gone, decrement object uses */
    uc->obj = 0;
    Unref(db);
  } else {
    uc->ocount = 0;
  }
}

static void *
yo_seek_context(int iarg, int *piarg, int push)
{
  if (iarg >= 0) {
    iarg++;
  } else {
    for (iarg=0 ; sp[-iarg].ops!=&returnSym ; iarg++)
      if (sp-iarg <= spBottom) return 0;
    iarg += sp[-iarg].index;
  }
  if (yget_obj(iarg,0) == yo_cops.type_name) {
    yo_context_t *uc = yget_obj(iarg, &yo_cops);
    yo_data_t *uo;
    if (uc->ocount < 0) {
      iarg -= uc->ocount;
      uc = (yget_obj(iarg,0)==yo_cops.type_name)? yget_obj(iarg,&yo_cops) : 0;
      if (!uc || uc->ocount<0)
        y_error("(BUG) yo_seek_context bad indirect oxy_context");
    }
    uo = yget_obj_s(uc->obj);
    if (push) {
      sp[1].ops = &dataBlockSym;
      sp[1].value.db = Ref(uc->obj);
      sp++;
    }
    if (piarg) *piarg = iarg;
    return uo;
  }
  return 0;
}

void *
yo_get_context(int iarg, yo_ops_t **ops, int push)
{
  yo_data_t *uo = yo_seek_context(-1, 0, push);
  if (uo) {
    if (ops) *ops = uo->ops;
    return uo->obj;
  }
  if (ops) *ops = 0;
  return 0;
}

/* ------------------------------------------------------------------------ */

static void yo_use_guts(int argc, int meth);

void
Y_use(int argc)
{
  yo_use_guts(argc, 0);
}

void
Y_use_method(int argc)
{
  yo_use_guts(argc, 1);
}

static void
yo_use_guts(int argc, int meth)
{
  int icx, sub = meth? 0 : yarg_subroutine();
  yo_data_t *obj = yo_seek_context(-1, &icx, !sub);

  if (sub) {
    int i;
    if (!obj) y_error("use called without any context object");
    for (i=0 ; i<argc ; i++)
      if (yget_ref(i) < 0)
        y_error("use accepts only simple variable references");
    for (i=argc-1 ; i>=0 ; i--)
      if (yo_cswap(icx, yget_ref(i)))
        y_errorq("use: context object has no member %s",
                 yfind_name(yget_ref(i)));

  } else if (obj) {
    /* use(arg1, arg2, ...) same as obj(arg1, arg2, ...) */
    long isp = (sp-spBottom) - (argc+1);
    yarg_swap(argc+1, 0);
    yarg_drop(1);
    /* context obj replaced use builtin, just invoke on_eval obj method */
    yo_on_evalx(obj, argc, icx, meth);
    /* ensure EvalBI gets correctly positioned stack if possible */
    isp = (sp-spBottom) - isp;
    if (isp>0 && sp->ops!=&returnSym) {
      yarg_swap(isp, 0);
      yarg_drop(isp);
    }

  } else {
    /* use() with no context */
    ypush_nil();
  }
}

int
yo_use(long index)
{
  int icx;
  yo_data_t *obj = yo_seek_context(-1, &icx, 0);
  return obj? yo_cswap(icx, index) : 3;
}

typedef struct yo_membarg_t yo_membarg_t;
struct yo_membarg_t {
  long n;   /* n<0 indicates error, n=0 single or special form, n>=1 multi */
  long dims[Y_DIMSIZE];
  long mndx;
  char *name;
  long iname;
  char **names;
  long *mndxs;
  long *range;     /* points into unused part of dims */
  int special;     /* 0 not, 1 nil, 2 -, 3 *, 4 .. */
  int frnd;
};

/* may want to publish this API... */
static int yo_membarg(int iarg, void *obj, yo_ops_t *ops, yo_membarg_t *ma,
                      int flag);
/* flag&1 = abort on error, flag&2 = permit scalar mndx=nmembers+1 */
static int
yo_membarg(int iarg, void *obj, yo_ops_t *ops, yo_membarg_t *ma, int flag)
{
  ma->n = ma->dims[0] = ma->mndx = 0;
  ma->name = 0;
  ma->iname = yget_ref(iarg);
  ma->names = 0;
  ma->mndxs = ma->range = 0;
  ma->special = 0;
  ma->frnd = 0;

  if (ma->iname >= 0) {
    ma->name = yfind_name(ma->iname);
    ma->n = 0;
    return 0;

  } else {
    int tid = yarg_typeid(iarg);

    if (tid == Y_STRING) {        /* member name(s) */
      ma->names = ygeta_q(iarg, &ma->n, ma->dims);
      if (!ma->dims[0]) {
        ma->name = ma->names[0];
        ma->names = 0;
        ma->n = 0;
      }

    } else if (tid <= Y_LONG) {   /* member index(s) */
      if (ops->find_mndx) {
        long n = ops->count(obj);
        ma->mndxs = ygeta_l(iarg, &ma->n, ma->dims);
        if (!ma->dims[0]) {
          ma->mndx = ma->mndxs[0];
          ma->mndxs = 0;
          ma->n = 0;
          if (ma->mndx <= 0) ma->mndx += n;
          if (ma->mndx<=0 || ma->mndx>n+((flag&2)!=0)) n = -3;
        } else {
          long i;
          for (i=0 ; i<ma->n ; i++)
            if (ma->mndxs[i]<=0 || ma->mndxs[i]>n) break;
          if (i < ma->n) {
            if (!yarg_scratch(iarg)) {
              long *mndxs = ypush_l(ma->dims);
              for (i=0 ; i<ma->n ; i++) mndxs[i] = ma->mndxs[i];
              yarg_swap(iarg, 0);
              yarg_drop(1);
              ma->mndxs = mndxs;
            }
            for (i=0 ; i<ma->n ; i++) {
              if (ma->mndxs[i] <= 0) ma->mndxs[i] += n;
              if (ma->mndxs[i]<=0 || ma->mndxs[i]>n) n = -3;
              if (ma->mndxs[i]<=0 || ma->mndxs[i]>n) break;
            }
            if (i < ma->n) n = -3;
          }
        }
        if (n < 0) {
          if (flag&1) y_error("bad member index argument");
          ma->n = -3;
        }
      } else {
        if (flag&1) y_error("this object does not support member index");
        ma->n = -2;
      }

    } else if (tid == Y_VOID) {
      ma->n = 0;
      ma->special = 1;

    } else if (tid==Y_RANGE) {  /* indices or special forms */
      int flags = yget_range(iarg, (ma->range = ma->dims+1));
      if ((flags&7) == 1) {
        /* member indices */
        if (ops->find_mndx) {
          long n = ops->count(obj);
          if (flags&Y_MIN_DFLT) ma->dims[1] = (ma->dims[3]<0)? n : 1;
          if (flags&Y_MAX_DFLT) ma->dims[2] = (ma->dims[3]<0)? 1 : n;
          if (ma->dims[1] <= 0) ma->dims[1] += n;
          if (ma->dims[2] <= 0) ma->dims[2] += n;
          if (ma->dims[1]<=0 || ma->dims[2]<=0 || ma->dims[1]>n ||
              ma->dims[2]>n) {
            ma->dims[3] = 0;
          } else if (!ma->dims[3]) {
            if (ma->dims[1]==ma->dims[2]) ma->dims[3] = 1;
          } else if ((ma->dims[1]!=ma->dims[2])
                     && (ma->dims[1]>ma->dims[2]) != (ma->dims[3]<0)) {
            ma->dims[3] = 0;
          }
          if (ma->dims[3]) {
            ma->n = 1 + (ma->dims[2]-ma->dims[1])/ma->dims[3];
            ma->dims[2] = ma->dims[1] + (ma->n-1)*ma->dims[3];
          } else {
            if (flag&1) y_error("bad member index range argument");
            ma->n = -3;
          }
        } else {
          if (flag&1)
            y_error("this object does not support member index range");
          ma->n = -2;
        }

      } else if ((flags & (Y_MIN_DFLT|Y_MAX_DFLT)) ==
                 (Y_MIN_DFLT|Y_MAX_DFLT)) { /* special forms */
        flags &= ~(Y_MIN_DFLT|Y_MAX_DFLT);
        if (flags == Y_PSEUDO) ma->special = 2;        /*  -  */
        else if (flags == Y_RUBBER) ma->special = 4;   /*  .. */
        else if (flags == Y_RUBBER1) ma->special = 3;  /*  *  */
        else if (flag&1) y_error("unrecognized member argument");
        else ma->n = -4;

      } else {
        if (flag&1) y_error("unrecognized member index range argument");
        ma->n = -5;
      }

    } else {
      int ff = yarg_func(iarg);
      if (ff<1 || ff>4) {
        if (flag&1) y_error("unrecognized member specifier argument");
        ma->n = -1;
      } else {
        ma->frnd = 1; /* function that will be called in context of object */
      }
    }
  }

  return 1;
}

void
Y_is_obj(int argc)
{
  yo_ops_t *ops;
  void *obj = (argc>0)? yo_get(argc-1, &ops) : 0;
  int err = 0, isobj = (obj != 0);
  int hasmndx = (isobj && ops->find_mndx);
  if (argc<1 || argc>3) y_error("is_obj accepts only 1, 2, or 3 arguments");

  if (argc == 1) {
    if (hasmndx) isobj |= 2;
    /* should also set bit for read-only?  any other properties? */
    ypush_int(isobj);
    return;

  } else if (argc == 3) {
    err = (!yarg_nil(0) && ygets_l(0));
    yarg_drop(1);
  }

  if (isobj) {
    long i;
    yo_membarg_t ma;
    int erri = 0;
    yo_membarg(0, obj, ops, &ma, !err);
    if (ma.n >= 0) {
      if (!ma.n) {
        if (!ma.special) {   /* is_obj a single member */
          if (ma.name) {
            if (ops->get_q(obj, ma.name, ma.iname)) {
              if (!err) y_errorq("is_obj object has no member %s", ma.name);
              erri = 1;
            }
          } else {
            if (!ma.mndx || ops->get_i(obj, ma.mndx)) {
              if (!err) y_errorn("is_obj object has no member %ld", ma.mndx);
              erri = 1;
            }
          }
          isobj = erri? 0 : (yo_get(0, &ops) != 0);
          if (isobj && ops->find_mndx) isobj |= 2;
          yarg_drop(1);
          ypush_int(erri? -1 : isobj);
        } else if (ma.special == 1) {  /* nil */
          yo_ops_t *ops2;
          int *rslt;
          ma.dims[0] = 1;
          ma.dims[1] = ops->count(obj);
          if (ma.dims[1]) {
            rslt = ypush_i(ma.dims);
            for (i=0 ; i<ma.dims[1] ; i++) {
              if (ops->get_i(obj, i+1)) {
                if (!err) y_errorn("(BUG) is_obj object has no member %ld", i);
                erri = 1;
              }
              isobj = erri? 0 : (yo_get(0, &ops2) != 0);
              if (isobj && ops2->find_mndx) isobj |= 2;
              rslt[i] = erri? -1 : isobj;
              yarg_drop(1);
              erri = 0;
            }
          } else {
            ypush_nil();
          }
        } else {             /* is_obj some special form */
          y_error("unrecognized member specifier in is_obj");
        }

      } else {               /* is_obj multiple members as a group */
        yo_ops_t *ops2;
        int *rslt;
        long j = ma.range? ma.range[0] : 0;
        ma.dims[4] = 1;
        ma.dims[5] = ma.n;
        rslt = ypush_i(ma.dims+4);
        for (i=0 ; i<ma.n ; i++) {
          if (ma.names) {
            if (ops->get_q(obj, ma.names[i], -1L)) {
              if(!err) y_errorq("is_obj object has no member %s", ma.names[i]);
              erri = 1;
            }
          } else {
            if (!ma.range) j = ma.mndxs[i];
            if (ops->get_i(obj, j)) {
              if (!err) y_errorn("is_obj object has no member %ld", j);
              erri = 1;
            }
            if (ma.range) j += ma.range[2];
          }
          isobj = erri? 0 : (yo_get(0, &ops2) != 0);
          if (isobj && ops2->find_mndx) isobj |= 2;
          rslt[i] = erri? -1 : isobj;
          yarg_drop(1);
          erri = 0;
        }
      }
    } else {
      ypush_int(-1);
    }
  } else {
    if (!err) y_error("is_obj(obj,m) obj argument not an object");
    ypush_int(-2);
  }
}

static long yo_use_iname = -1L;

void
Y_restore(int argc)
{
  yo_ops_t *ops;
  void *obj;
  if (yo_use_iname<0) yo_use_iname = yget_global("use", 0);
  if (yget_ref(--argc) == yo_use_iname) {
    /* special form restore, use, arg1, arg2, ... */
    obj = yo_get_context(argc, &ops, 1);
    if (!obj) y_error("no context for restore,use");
    yarg_swap(argc+1, 0);  /* replace use by context */
    yarg_drop(1);
  } else {
    obj = yo_get(argc, &ops);
  }
  if (obj) {
    char *name;
    long iname;
    yo_ops_t *ops2 = 0;
    void *obj2 = 0;
    if (!yarg_subroutine())
      /* create output group containing global values before restore */
      obj2 = yo_new_group(&ops2);
    else
      /* called as subroutine, make argc same as function call case */
      ypush_nil();

    if (!argc) {       /* restore all non-anonymous members */
      long mndx, n = ops->count(obj);
      for (mndx=1 ; mndx<=n ; mndx++) {
        name = ops->find_name(obj, mndx, &iname);
        if (!name) continue;
        if (iname<0) iname = yget_global(name, 0);
        if (obj2) {  /* save globtab variable in return object */
          if (ypush_global(iname)) y_error("(BUG) bad iname in restore()");
          ops2->set_q(obj2, name, iname, 0);
          yarg_drop(1);
        }
        /* copy object member to globtab */
        ops->get_q(obj, name, iname);
        yput_global(iname, 0);
        yarg_drop(1);
      }

    } else {                    /* restore specific members */
      yo_ops_t *ops3 = 0;
      void *obj3 = 0;
      long i, j;
      yo_membarg_t ma;
      do {
        argc -= yo_membarg(argc, obj, ops, &ma, 1);
        iname = (argc>0)? yget_ref(argc) : -1;
        if (iname < 0)
          y_error("restore output argument must be simple variable reference");
        if (obj2) {  /* save globtab variable in return object */
          if (ypush_global(iname)) y_error("(BUG) bad iname in restore()");
          ops2->set_q(obj2, yfind_name(iname), iname, 0);
          yarg_drop(1);
        }

        if (!ma.n) {
          if (!ma.special) {   /* restore a single member */
            if (ma.name) {
              if (ops->get_q(obj, ma.name, ma.iname))
                y_errorq("restore object has no member %s", ma.name);
            } else {
              if (!ma.mndx || ops->get_i(obj, ma.mndx))
                y_errorn("restore object has no member %ld", ma.mndx);
            }

          } else {             /* restore some special form */
            y_error("unrecognized member specifier in restore");
          }

        } else {               /* restore multiple members as a group */
          obj3 = yo_new_group(&ops3);
          j = ma.range? ma.range[0] : 0;
          for (i=0 ; i<ma.n ; i++) {
            if (ma.names) {
              if (ops->get_q(obj, ma.names[i], -1L))
                y_errorq("restore object has no member %s", ma.names[i]);
              ops3->set_q(obj3, ma.names[i], -1L, 0);
            } else {
              if (!ma.range) j = ma.mndxs[i];
              if (ops->get_i(obj, j))
                y_errorn("restore object has no member %ld", j);
              if (ma.range) j += ma.range[2];
              /* preserve member names in restored object */
              ma.name = ops->find_name(obj, j, &ma.iname);
              /* if ma.name==0, set_q creates next anonymous member */
              ops3->set_q(obj3, ma.name, ma.iname, 0);
            }
            yarg_drop(1);
          }
        }
        /* move restored object to globtab */
        yput_global(iname, 0);
        yarg_drop(1);
      } while (--argc > 0);
    }
    if (ops->sr_hook) ops->sr_hook(obj, 1);
  } else {
    y_error("restore needs a source object or file");
  }
}

static void yo_new_globobj(void);

void
Y_save(int argc)
{
  long i, j, n, iname;
  char *name;
  yo_ops_t *ops;
  void *obj;
  if (yo_use_iname<0) yo_use_iname = yget_global("use", 0);
  if (yarg_subroutine()) {
    if (yget_ref(--argc) == yo_use_iname) {
      /* special form save, use, arg1, arg2, ... */
      obj = yo_get_context(argc, &ops, 1);
      if (!obj) y_error("no context for save,use");
      yarg_swap(argc+1, 0);  /* replace use by context */
      yarg_drop(1);
    } else {
      obj = yo_get(argc, &ops);
      if (!obj) y_error("save needs a destination object or file");
    }
    ypush_nil();  /* make stack look same as function call */
  } else {
    long range[3];
    if ((argc==1)
        && yget_range(0, range)==(Y_RUBBER1|Y_MAX_DFLT|Y_MIN_DFLT)) {
      /* create special object representing all of globtab */
      yo_new_globobj();
      return;
    }
    obj = yo_new_group(&ops);
    if (argc==1 && yget_ref(1)<0 && yarg_nil(1))
      return;  /* save() creates empty object */
  }

  /* argc = number of sspec arguments, one non-sspec on top of stack */
  if (ops->sr_hook) ops->sr_hook(obj, argc?2:6);
  if (!argc) {           /* save all of globtab */
    for (iname=1 ; (name=yfind_name(iname)) ; iname++) {
      if (ypush_global(iname))
        y_error("(BUG) problem with globtab in save");
      argc = ops->set_q(obj, name, iname, 0);
      if (argc == 2)
        y_errorq("cannot save to read-only member %s", name);
      else if (argc == 3)
        y_errorq("object to be saved incommensurate with member %s", name);
      /* argc==4 just skips this member, unsupported type */
      yarg_drop(1);
    }

  } else {           /* save specific list of variables */
    yo_membarg_t ma;
    yo_ops_t *ops3 = 0;
    void *obj3 = 0;
    do {
      if (sp[-argc].ops) {
        argc -= yo_membarg(argc, obj, ops, &ma, 3);
      } else {
        /* permit member= keywords on same basis as simple variable refs */
        ma.n = ma.dims[0] = ma.mndx = 0;
        ma.iname = sp[-argc].index;
        ma.name = yfind_name(ma.iname);
        ma.names = 0;
        ma.mndxs = ma.range = 0;
        ma.special = 0;
        ma.frnd = 0;
        argc--;
      }
      if (argc <= 0)
        y_error("save: missing final argument of sspec pair");

      if (!ma.n) {
        if (ma.special == 1) {  /* nil */
          obj3 = yo_get(argc, &ops3);
          if (obj3) {      /* merge object argument */
            n = ops3->count(obj3);
            for (i=0 ; i<n ; i++) {
              name = ops3->find_name(obj3, i+1, &iname);
              if (!name) {
                if (!ops3->get_i || ops3->get_i(obj3, i+1))
                  y_error("(BUG) object to be saved has unfetchable member");
                if (ops->set_q(obj, 0, -1, argc-1))
                  y_error("unable to save by appending anonymous member");
              } else {
                if (ops3->get_q(obj3, name, iname))
                  y_error("(BUG) object to be saved has unreadable member");
                if (ops->set_q(obj, name, iname, 0))
                  y_errorq("unable to save to member %s",
                           name? name : "<anon>");
              }
              yarg_drop(1);
            }
          } else {    /* append non-object argument */
            ma.special = 0;    /* same as string(0) */
          }
        }
        if (!ma.special && !ma.frnd) {   /* save a single member */
          if (ma.name) {
            if (ops->set_q(obj, ma.name, ma.iname, argc))
              y_errorq("unable to save to member %s", ma.name);
          } else if (ma.mndx) {
            if (ops->set_i(obj, ma.mndx, argc))
              y_errorn("unable to save to member %ld", ma.mndx);
          } else { 
            /* string(0) means append anonymously */
            if (ops->set_q(obj, 0, -1, argc))
              y_error("unable to save by appending anonymous member");
          }

        } else if (ma.special != 1) { /* 1 nil, 2 -, 3 *, 4 .. */
          y_error("unrecognized member specifier in save");
        }

      } else {               /* save multiple members from a group */
        obj3 = yo_get(argc, &ops3);
        j = ma.range? ma.range[0] : 0;
        for (i=0 ; i<ma.n ; i++) {
          if (ma.names) {
            if (ops3->get_q(obj3, ma.names[i], -1L))
              y_errorq("object to be saved has no member %s", ma.names[i]);
            if (ops->set_q(obj, ma.names[i], -1L, 0))
              y_errorq("unable to save member %s",
                       ma.names[i]? ma.names[i] : "<anon>");
          } else {
            if (ops3->get_i(obj3, i+1))
              y_errorn("object to be saved has no member %ld", j);
            if (!ma.range) j = ma.mndxs[i];
            if (ops->set_i(obj, j, 0))
              y_errorn("unable to save member %ld", ma.mndxs[i]);
            if (ma.range) j += ma.range[2]; 
          }
          yarg_drop(1);
        }
      }
    } while (--argc > 0);
  }

  if (ops->sr_hook) ops->sr_hook(obj, 0);
}

extern void FormEvalOp(int nArgs, Operand *obj);

static void
yo_on_eval(void *uo, int nargs)
{
  yo_on_evalx(uo, nargs, -1, 0);
}

static void
yo_on_evalx(void *uo, int nargs, int icx, int meth)
{
  yo_ops_t *ops = ((yo_data_t*)uo)->ops;
  void *obj = ((yo_data_t*)uo)->obj;
  yo_membarg_t ma;
  int iarg = nargs-1;

  if (!meth && iarg>0 && !sp[-iarg].ops && yarg_subroutine()) {
    /* obj, member1=expr1, member2=expr2, ... */
    for (;;) {
      ma.iname = sp[-iarg].index;
      ma.name = yfind_name(ma.iname);
      if (ops->set_q(obj, ma.name, ma.iname, --iarg))
        y_errorq("unable to save to member %s", ma.name);
      if (iarg <= 0) return;
      iarg--;
      if (iarg<1 || sp[-iarg].ops)
        y_error("obj,m=value,... idiom can only accept keyword arguments");
    }
  }

  yo_membarg(iarg, obj, ops, &ma, 1);
  if (meth && (ma.special || ma.n))
    y_error("use_method requires single member specifier");

  if (!ma.special) {             /* obj(m) */
    if (ma.n) {  /* just extract member(s) */
      yo_ops_t *ops2;
      void *obj2;
      long i, j;
      if (iarg)
        y_error("obj(m,args) illegal when m specifies multiple members");
      obj2 = yo_new_group(&ops2);
      j = ma.range? ma.range[0] : 0;
      for (i=0 ; i<ma.n ; i++) {
        if (ma.names) {
          if (!ma.names[0])
            y_error("string(0) does not specify an object member");
          if (ops->get_q(obj, ma.names[i], -1L))
            y_errorq("unable to get member %s", ma.names[i]);
          if (ops2->set_q(obj2, ma.names[i], -1L, 0))
            y_errorq("unable to set member %s", ma.names[i]);
        } else {
          if (ma.mndxs) j = ma.mndxs[i];
          if (ops->get_i(obj, j))
            y_errorn("unable to get member %ld", j);
          /* preserve member name in result object */
          ma.name = ops->find_name(obj, j, &ma.iname);
          if (ma.range) j += ma.range[2];
          if (ops2->set_q(obj2, ma.name, ma.iname, 0))
            y_errorq("unable to set member %s", ma.name? ma.name : "<anon>");
        }
        yarg_drop(1);
      }
    } else { /* extract single member, possibly eval */
      if (ma.name) {
        if (ops->get_q(obj, ma.name, -1L))
          y_errorq("unable to get member %s", ma.name);
      } else if (ma.mndx) {
        if (ops->get_i(obj, ma.mndx))
          y_errorn("unable to get member %ld", ma.mndx);
      } else if (ma.frnd) {
        ypush_use(yget_use(iarg)); /* duplicate friend to top of stack */
      } else {
        y_error("string(0) does not specify an object member");
      }
      if (meth || yarg_subroutine() || iarg) {
        Operand obj;
        long isp;
        /* instead of simply extracting the member, eval it */
        /* move member into stack slot occupied by member specifier */
        yarg_swap(iarg+1, 0);
        yarg_drop(1); /* throw away member specifier, have thing itself */
        if (yarg_func(iarg))
          yo_mk_context(iarg+1, icx);
        else if (meth)
          y_error("use_method requires first argument to be a function");
        FormEvalOp(iarg, &obj);
        isp = sp - spBottom;
        obj.ops->Eval(&obj);
        if (sp->ops == &returnSym) {
          /* context is at spBottom+isp - (iarg+1)
           * want sp-iarg = spBottom+isp - (iarg+1)
           *   iarg = iarg+1 + (sp-spBottom)-isp
           */
          iarg += 1 + sp - (spBottom + isp);  /* new location of context */
          /* if (sp->index) this function has been called recursively
           * obj1(obj2,obj3,obj4,func,args)
           * will end up with stack = [ctx1,ctx2,ctx3,ctx4,func,...return]
           * sp->index points to ctx4
           */
          if (!sp->index) sp->index = iarg;  /* set for use */
          /* also must move -2 marker */
          if (sp[1-iarg].index != -2)
            y_error("(BUG) stack end-of-func return marker garbled");
          sp[1-iarg].index = -1;
          sp[-iarg].index = -2;
        }
      }
    }

  } else if (ma.special == 1) {  /* obj() */
    if (iarg) y_error("obj(,extra) bad argument list to object");
    yarg_drop(1);   /* leaves obj on top of stack as return value */

  } else if (ma.special == 3) {  /* obj(*) */
    if (!iarg) {
      ypush_long(ops->count(obj));
    } else if (iarg == 1) {
      long i;
      int tid = yarg_typeid(0);
      if (tid == Y_VOID) {
        ma.dims[0] = 1;
        ma.dims[1] = ops->count(obj);
        if (ma.dims[1]) {
          ma.names = ypush_q(ma.dims);
          for (i=0 ; i<ma.dims[1] ; i++)
            ma.names[i] = p_strcpy(ops->find_name(obj, i+1, &ma.iname));
        } else {
          ypush_nil();
        }
      } else if (!ops->find_mndx) {
        y_error("object does not support member indices");
      } else if (tid>=Y_CHAR && tid<=Y_LONG) {
        long j, n = ops->count(obj);
        ma.mndxs = ygeta_l(0, &ma.n, ma.dims);
        ma.names = ypush_q(ma.dims);
        for (i=0 ; i<ma.n ; i++) {
          j = ma.mndxs[i];
          if (j <= 0) j += n;
          if (j<=0 || j>n)
            y_error("bad obj(*,m) call, index m out of range");
          ma.names[i] = p_strcpy(ops->find_name(obj, j, &ma.iname));
        }
      } else if (tid == Y_STRING) {
        ma.names = ygeta_q(0, &ma.n, ma.dims);
        ma.mndxs = ypush_l(ma.dims);
        for (i=0 ; i<ma.n ; i++)
          ma.mndxs[i] = ops->find_mndx(obj, ma.names[i], -1L);
      } else if (tid == Y_RANGE) {
        long j;
        int flags = yget_range(0, ma.dims+1);
        if ((flags&7) != 1)
          y_error("bad obj(*,m) call, unrecognized index range m");
        ma.n = ops->count(obj);
        if (flags&Y_MIN_DFLT) ma.dims[1] = (ma.dims[3]<0)? ma.n : 1;
        if (flags&Y_MAX_DFLT) ma.dims[2] = (ma.dims[3]<0)? 1 : ma.n;
        if (ma.dims[1] <= 0) ma.dims[1] += ma.n;
        if (ma.dims[2] <= 0) ma.dims[2] += ma.n;
        if (ma.dims[1]<=0 || ma.dims[2]<=0 || ma.dims[1]>ma.n ||
            ma.dims[2]>ma.n) {
          ma.dims[3] = 0;
        } else if (!ma.dims[3]) {
          if (ma.dims[1]==ma.dims[2]) ma.dims[3] = 1;
        } else if ((ma.dims[1]!=ma.dims[2])
                   && (ma.dims[1]>ma.dims[2]) != (ma.dims[3]<0)) {
          ma.dims[3] = 0;
        }
        if (!ma.dims[3])
          y_error("bad obj(*,m) call, bad index range m");
        ma.n = (ma.dims[2]-ma.dims[1])/ma.dims[3] + 1;
        ma.dims[4] = 1;
        ma.dims[5] = ma.n;
        ma.names = ypush_q(ma.dims+4);
        for (i=0,j=ma.dims[1] ; i<ma.n ; i++,j+=ma.dims[3])
          ma.names[i] = p_strcpy(ops->find_name(obj, j, &ma.iname));
      } else {
        y_error("bad obj(*,m) call, unrecognized m");
      }
    } else {
      y_error("bad obj(*,m) call, too many arguments");
    }

  } else if (ma.special == 4) {  /* obj(..) */
    if (!ops->get_atts) ypush_nil();
    else ops->get_atts(obj);

  } else if (ma.special == 2) {  /* obj(-) */

  } else {
    y_error("(BUG) impossible ma.special in yo_on_eval");
  }
}

/* yo_do_hooks */
/* if (uo->on_destroy) yo_do_hooks(uo->u.on_destroy, uo); */

/* ------------------------------------------------------------------------ */
/* basic group object */

static void yog_dealloc(void *obj);
static long yog_count(void *obj);
static long yog_findm(void *obj, const char *name, long iname);
static char *yog_findn(void *obj, long mndx, long *iname);
static int yog_geti(void *obj, long mndx);
static int yog_getq(void *obj, const char *name, long iname);
static int yog_seti(void *obj, long mndx, int iarg);
static int yog_setq(void *obj, const char *name, long iname, int iarg);
static void yog_geta(void *obj);

static yo_ops_t yog_ops = {
  "group", yog_dealloc, yog_count, yog_findm, yog_findn,
  yog_geti, yog_getq, yog_seti, yog_setq, yog_geta, 0, 0 };

typedef struct yog_t yog_t;
struct yog_t {
  long n_memb;    /* memb[] size is 2^n, n>=2 */
  Symbol *memb;   /* yfind_name(memb->index) is member name */
  p_hashtab *ht;  /* globtab index --> member index */
  void *attrib;   /* attribute object */
  void *destruct; /* reserved for destructor */
  int flags;
};

void *
yo_new_group(yo_ops_t **ops)
{
  yog_t *grp = yo_push_alloc(&yog_ops, sizeof(yog_t));
  grp->n_memb = 0;
  grp->memb = 0;
  grp->ht = 0;
  grp->attrib = grp->destruct = 0;
  grp->flags = 0;
  if (ops) *ops = &yog_ops;
  return grp;
}

static void
yog_dealloc(void *vgrp)
{
  yog_t *grp = vgrp;
  void *obj = grp->destruct;
  p_hashtab *ht = grp->ht;
  if (ht) {
    grp->ht = 0;
    p_hfree(ht, 0);
  }
  if (obj) {
    grp->destruct = 0;
    ypush_use(obj);
    /* this is where destructor(s) would be invoked */
    yarg_drop(1);
  }
  obj = grp->attrib;
  if (obj) {
    grp->attrib = 0;
    ypush_use(obj);
    yarg_drop(1);
  }
  obj = grp->memb;
  if (obj) {
    long n;
    for (n=--grp->n_memb ; n>=0 ; n=--grp->n_memb)
      if (grp->memb[n].ops == &dataBlockSym) {
        Unref(grp->memb[n].value.db);
        grp->memb[n].ops = &intScalar;
      }
    grp->memb = 0;
    p_free(obj);
  }
}

static long
yog_count(void *obj)
{
  yog_t *grp = obj;
  return grp->n_memb;
}

static long
yog_findm(void *obj, const char *name, long iname)
{
  yog_t *grp = obj;
  long i, n = grp->n_memb;
  if (!n || !name || !name[0]) return 0;
  if (iname < 0) {
    iname = yfind_global(name, 0);
    if (iname < 0) return 0;
  }
  if (n <= 4) {
    /* just do straight search for <= 4 members */
    for (i=0 ; i<n ; ++i) if (grp->memb[i].index == iname) break;
    return (i >= n)? 0 : i+1;
  } else {
    /* create and use hash table for > 4 members */
    if (!grp->ht) {
      long jname;
      grp->ht = p_halloc(n);
      for (i=0 ; i<n ; ++i) {
        jname = grp->memb[i].index;
        if (jname < 0) continue;
        p_hinsert(grp->ht, P_IHASH(jname), i+1+(char*)0);
      }
    }
    return (char*)p_hfind(grp->ht, P_IHASH(iname)) - (char*)0;
  }
}

static char *
yog_findn(void *obj, long mndx, long *iname)
{
  yog_t *grp = obj;
  if (mndx<1 || mndx>grp->n_memb || !grp->n_memb) {
    if (iname) *iname = -1;
    return 0;
  } else {
    if (iname) *iname = grp->memb[mndx-1].index;
    return yfind_name(grp->memb[mndx-1].index);
  }
}

static int
yog_geti(void *obj, long mndx)
{
  yog_t *grp = obj;
  (void)CheckStack(1);
  if (mndx<1 || mndx>grp->n_memb) {
    ypush_nil();
    return 1;
  }
  sp[1] = grp->memb[mndx-1];
  if (sp[1].ops==&dataBlockSym) (void)Ref(sp[1].value.db);
  sp++;
  return 0;
}

static int
yog_getq(void *obj, const char *name, long iname)
{
  return yog_geti(obj, yog_findm(obj, name, iname));
}

static int
yog_seti(void *obj, long mndx, int iarg)
{
  yog_t *grp = obj;
  Symbol *s = sp - iarg;
  if (mndx<1 || mndx>grp->n_memb || iarg<0) return 1;
  if (s->ops == &referenceSym) ReplaceRef(s);

  if (grp->flags & 2) {  /* implement gaccess assign= semantics */
    LValue lvfake, *lv = 0;
    Operand op;
    Symbol *d = grp->memb + mndx-1;
    s->ops->FormOperand(s, &op);
    if (d->ops == &dataBlockSym) {
      DataBlock *db = d->value.db;
      if (db->ops == &lvalueOps) {
        lv = (LValue *)db;
      } else if (db->ops->isArray) {
        Array *a = (Array *)db;
        lvfake.references = 10;
        lvfake.owner = 0;
        lvfake.type.base = a->type.base;
        lvfake.type.dims = a->type.dims;
        lvfake.type.number = a->type.number;
        lvfake.address.m = a->value.c;
        lvfake.strider = 0;
        lv = &lvfake;
      } else if (db->ops == op.ops) {
        return (op.value == db)? 0 : 2;
      }
    } else {
      lvfake.references = 10;
      lvfake.owner = 0;
      lvfake.type.dims = 0;
      lvfake.type.number = 1;
      lvfake.strider = 0;
      if (d->ops==&doubleScalar) {
        lvfake.ops = &doubleOps;
        lvfake.type.base = &doubleStruct;
        lvfake.address.m = (char *)&d->value.d;
      } else if (d->ops==&longScalar) {
        lvfake.ops = &longOps;
        lvfake.type.base = &longStruct;
        lvfake.address.m = (char *)&d->value.l;
      } else if (d->ops==&intScalar) {
        lvfake.ops = &intOps;
        lvfake.type.base = &intStruct;
        lvfake.address.m = (char *)&d->value.i;
      } else {
        return 4; /* this is returnSym, keyword, etc */
      }
      lv = &lvfake;
    }
    /* see ops3.c:DoAssign */
    if (!op.ops->isArray || RightConform(lv->type.dims, &op))
      return 3;
    lv->type.base->dataOps->Assign((Operand *)lv, &op);
    return 0;
  }
  if (grp->memb[mndx-1].ops == &dataBlockSym) {
    /* note that this discards LValue created by reshape,
     * which is same behavior as pre-2.1.06 restore function
     */
    grp->memb[mndx-1].ops = &intScalar;
    Unref(grp->memb[mndx-1].value.db);
  }
  if (s->ops == &dataBlockSym) {
    if (s->value.db->ops==&lvalueOps) FetchLValue(s->value.db, s);
    if (s->ops != &dataBlockSym) grp->memb[mndx-1].value = s->value;
    else grp->memb[mndx-1].value.db = Ref(s->value.db);
  } else if (s->ops==&doubleScalar ||
             s->ops==&longScalar || s->ops==&intScalar) {
    grp->memb[mndx-1].value = s->value;
  } else {
    return 4;  /* this is returnSym, keyword, etc */
  }
  grp->memb[mndx-1].ops = s->ops;
  return 0;
}

static int
yog_setq(void *obj, const char *name, long iname, int iarg)
{
  yog_t *grp = obj;
  long mndx = yog_findm(obj, name, iname);
  if (!mndx) {
    /* create new group member */
    long n = grp->n_memb;
    if (grp->flags & 1) return 1;
    if (name && !name[0]) name = 0;
    if (name && iname<0) iname = yget_global(name, 0);
    if (!n || (n>2 && !(n&(n-1))))
      grp->memb = p_realloc(grp->memb, ((n>2)? n+n : 4)*sizeof(Symbol));
    grp->memb[n].ops = &dataBlockSym;
    grp->memb[n].index = name? iname : -1;
    grp->memb[n].value.db = RefNC(&nilDB);
    grp->n_memb += 1;
    mndx = n+1;
    if (iname>=0 && grp->ht)
      p_hinsert(grp->ht, P_IHASH(iname), mndx+(char*)0);
  }
  return yog_seti(obj, mndx, iarg);
}

static void
yog_geta(void *obj)
{
  yog_t *grp = obj;
  if (grp->attrib) {
    ypush_use(grp->attrib);
    grp->attrib = yget_use(0);
  }
}

void
Y_gaccess(int argc)
{
  yo_ops_t *ops = 0;
  yog_t *obj = (argc>0)? yo_get(argc-1, &ops) : 0;
  if (!obj || ops!=&yog_ops)
    y_error("gaccess first argument not a group object");
  if (argc > 1) {  /* gaccess(grp,flags) sets group access flags */
    if (argc > 2) y_error("gaccess takes at most two arguments");
    obj->flags = ygets_i(0);
    yarg_drop(1);
  } else {         /* gaccess(grp) returns group access flags */
    ypush_long(obj->flags);
  }
}

/* ------------------------------------------------------------------------ */
/* make globtab a simple object (obsoletes symbol_def, symbol_set?) */

static void yo_gt_dealloc(void *obj);
static long yo_gt_count(void *obj);
static long yo_gt_findm(void *obj, const char *name, long iname);
static char *yo_gt_findn(void *obj, long mndx, long *iname);
static int yo_gt_geti(void *obj, long mndx);
static int yo_gt_getq(void *obj, const char *name, long iname);
static int yo_gt_seti(void *obj, long mndx, int iarg);
static int yo_gt_setq(void *obj, const char *name, long iname, int iarg);
/* static void yo_gt_geta(void *obj); */
/* static void yo_gt_print(void *obj); */

static yo_ops_t yo_globtab_ops = {
  "oxy_globtab", yo_gt_dealloc, yo_gt_count, yo_gt_findm, yo_gt_findn,
  yo_gt_geti, yo_gt_getq, yo_gt_seti, yo_gt_setq, 0, 0, 0 };

static void 
yo_new_globobj(void)
{
  yo_push(&yo_globtab_ops, yo_globtab_ops.type_name);
}

static void
yo_gt_dealloc(void *obj)
{
  return;
}

static long
yo_gt_count(void *obj)
{
  return globalTable.nItems;
}

static long
yo_gt_findm(void *obj, const char *name, long iname)
{
  /*      iname=-1 if correspondence with globtab unknown
   *      find_mndx==0 permitted, means no fixed member indices
   */
  return (iname>=0)? iname : yfind_global(name, 0);
}

static char *
yo_gt_findn(void *obj, long mndx, long *iname)
{
  /*      returns iname=-1 if correspondence with globtab unknown
   *      return value owned by obj, caller must copy string
   *        next call to find_name may invalidate return value
   *      if find_mndx==0, this will only be called in a sequence
   *        from mndx=1 to mndx=count(obj) to list all names
   *        get_q or set_q may be called during listing sequence,
   *        but only with name just returned
   */
  char *name = yfind_name(mndx);
  if (iname) *iname = name? mndx : -1;
  return name;
}

static int
yo_gt_geti(void *obj, long mndx)
{
  int err = ypush_global(mndx);
  if (err) ypush_nil();
  return err;
}

static int
yo_gt_getq(void *obj, const char *name, long iname)
  /*      push member onto stack
   *      get_i unused (0 permitted) when find_mndx==0
   *      pass iname=-1 if correspondence with globtab unknown
   *      returns 0 on success, otherwise push nil and return:
   *        1 if no such member
   */
{
  /* should this be yget_global?? */
  return yo_gt_geti(obj, (iname>=0)? iname : yfind_global(name,0));
}

static int
yo_gt_seti(void *obj, long mndx, int iarg)
{
  int err = (mndx>=0 && mndx<globalTable.nItems);
  if (!err) yput_global(mndx, iarg);
  return err;
}

static int
yo_gt_setq(void *obj, const char *name, long iname, int iarg)
{
  /*      set member to value at iarg on stack
   *      set_i unused (0 permitted) when find_mndx==0
   *      both set_i==0, set_q==0 permitted if changing values unsupported
   *      pass iname=-1 if correspondence with globtab unknown
   *      returns 0 on success, otherwise return:
   *        1 if no such member and creating member not allowed
   *        2 if member is read-only
   *        3 if type or shape of iarg cannot be converted to member type
   *        4 if type not supported by this object
   */
  if (iname<0) iname = yget_global(name, 0);
  return yo_gt_seti(obj, iname, iarg);
}

/* ------------------------------------------------------------------------ */
/* wrap IOStream as an object for backward compatibility */

static void yo_io_dealloc(void *obj);
static long yo_io_count(void *obj);
static char *yo_io_findn(void *obj, long mndx, long *iname);
static int yo_io_getq(void *obj, const char *name, long iname);
static int yo_io_setq(void *obj, const char *name, long iname, int iarg);
static void yo_sr_hook(void *obj, int flags);

static yo_ops_t yo_io_ops = {
  "oxy_iostream", yo_io_dealloc, yo_io_count, 0, yo_io_findn,
  0, yo_io_getq, 0, yo_io_setq, 0, 0, yo_sr_hook };

typedef struct yo_io_t yo_io_t;
struct yo_io_t {
  IOStream *ios;
  int flags;  /* used by sr_hook */
};

static yo_data_t *
yo_iostream(int iarg)
{
  /* create oxy wrapper for IOStream */
  IOStream *ios = YGetFile(sp-iarg);  /* will not fail */
  yo_io_t *io = yo_push_alloc(&yo_io_ops, sizeof(yo_io_t));
  io->ios = Ref(ios);
  io->flags = 0;
  yarg_swap(iarg+1, 0);
  yarg_drop(1);
  return yget_obj(iarg, &yo_uops);
}

static void
yo_io_dealloc(void *obj)
{
  yo_io_t *io = obj;
  sp[1].ops = &dataBlockSym;
  sp[1].value.db = (DataBlock *)io->ios;
  io->ios = 0;
  sp++;
  yarg_drop(1);
}

static long
yo_io_count(void *obj)
{
  IOStream *file = ((yo_io_t *)obj)->ios;
  /* only used by restore all, which restores either record or non-record
   * variables, but not both
   * also used for save all
   */
  if (file->history) {
    HistoryInfo *history = file->history;
    if (history->nRecords>0 && history->recNumber>=0)
      file = history->child;
  }
  return file->dataTable.nItems;
}

static char *
yo_io_findn(void *obj, long mndx, long *iname)
{
  IOStream *file = ((yo_io_t *)obj)->ios;
  long n;
  /* this is never actually used??  make it work like restore all */
  if (file->history) {
    HistoryInfo *history = file->history;
    if (history->nRecords>0 && history->recNumber>=0)
      file = history->child;
  }
  n = file->dataTable.nItems;
  mndx--;
  if (iname) *iname = -1;
  return (mndx<0 || mndx>=n)? 0 : file->dataTable.names[mndx];
}

extern void ReadGather(void *dst, void *srcM, long srcD, StructDef *base,
                       long number, const Strider *strider);

static int
yo_io_getq(void *obj, const char *name, long iname)
{
  IOStream *file = ((yo_io_t *)obj)->ios;
  StructDef *model, *base;
  Dimension *dims;
  long address, number;
  void *memory;
  int no_recurse = 1;

  if (!HashFind(&file->dataTable, name, 0L)) {
    HistoryInfo *history = file->history;
    if (!history || history->nRecords<=0 || history->recNumber<0 ||
        !history->child) return 1;
    file = history->child;
    if (!HashFind(&file->dataTable, name, 0L)) return 1;
  }
  base = file->types[hashIndex].base;
  dims = file->types[hashIndex].dims;
  address = file->addresses[hashIndex] + file->offset;
  model = base->model;
  while (model->model) model = model->model;

  /* check for scalar types and simplify if possible */
  memory = 0;
  if (!dims) {
    Operations *ops = model->dataOps;
    if (ops==&doubleOps) {
      sp[1].ops = &doubleScalar;
      memory = &sp[1].value.d;
    } else if (ops==&longOps) {
      sp[1].ops = &longScalar;
      memory = &sp[1].value.l;
    } else if (ops==&intOps) {
      sp[1].ops = &intScalar;
      memory = &sp[1].value.i;
    }
  }

  /* otherwise, create an array to hold the result */
  if (!memory) {
    Array *array = NewArray(model, dims);
    sp[1].value.db = (DataBlock *)array;
    sp[1].ops= &dataBlockSym;
    memory = array->value.c;
    number = array->type.number;
  } else {
    number = 1;
  }
  sp++;

  ReadGather(memory, &no_recurse, address, base, number, (Strider *)0);
  return 0;
}

extern void WriteScatter(void *src, void *dstM, long dstD, StructDef *base,
                         long number, const Strider *strider);
extern void SetSequentialWrite(IOStream *file, long last);

static int
yo_io_setq(void *obj, const char *name, long iname, int iarg)
{
  yo_io_t *io = obj;
  IOStream *file = io->ios;
  HistoryInfo *history = file->history;
  Symbol *s = sp - iarg;
  StructDef *base;
  long address;
  int no_recurse = 1;

  Operand op;
  int not_new;

  s->ops->FormOperand(s, &op);
  if (op.ops == &structDefOps) {
    if (!CopyStruct(file, (StructDef *)op.value))
      y_error("problem saving struct to binary file (name conflict?)");
    return 0;
  }
  if (!op.type.base)
    return 4;

  if (history) {
    if (io->flags & 4)
      y_error("no save,f (save all) to history record");
    if (history->nRecords<=0 || history->recNumber<0)
      y_error("file has no current record for save");
    file = history->child;
  }

  not_new = AddVariable(file, -1L, name, op.type.base, op.type.dims);
  if (not_new > 1)
    y_error("data type (struct) name conflict in save to binary file");

  base= file->types[hashIndex].base;
  address= file->addresses[hashIndex]+file->offset;

  if (not_new) {
    /* this is an assignment to an existing variable --
     * verify operand data type and number */
    long number = file->types[hashIndex].number;
    if (!EquivStruct(base, op.type.base) || number!=op.type.number)
      y_error("variable type or dimensions have changed since last save");
    /* possibly return 3 instead of error */
  }

  if (base->addressType==2)
    SetSequentialWrite(file, address+base->size*op.type.number);
  WriteScatter(op.value, &no_recurse, address, base, op.type.number,
               (Strider *)0);

  return 0;
}

static void
yo_sr_hook(void *obj, int flags)
{
  yo_io_t *io = obj;
  IOStream *file = io->ios;
  IOStream *child = file->history? file->history->child : 0;
  if (flags & 1) {           /* after restore */
    ClearPointees(file, 0);
    if (child) ClearPointees(child, 0);
  } else if (flags & 2) {    /* before save */
    io->flags = flags;
  } else {                   /* after save */
    io->flags = 0;
    ClearPointees(file, 1);
    FlushFile(file, 0);
    if (child) {
      ClearPointees(child, 1);
      FlushFile(child, 0);
    }
  }
}

/* ------------------------------------------------------------------------ */
/* closure, invented by Eric Thiebaut */

static void yoc_on_free(void *uo);
static void yoc_on_extract(void *uo, char *name);
static void yoc_on_eval(void *uo, int nargs);

static y_userobj_t yoc_ops =
  { "closure", yoc_on_free, 0, yoc_on_eval, yoc_on_extract, 0 };

typedef struct yoc_obj_t yoc_obj_t;
struct yoc_obj_t {
  void *f, *d;
  long fndx, dndx;
};

int
yo_is_closure(int iarg)
{
  return (yget_obj(iarg,0) == yoc_ops.type_name);
}

int
yo_closure(int farg, int darg)
{
  long fref = -1L;
  long dref = yget_ref(darg);
  int fid = yarg_func(farg);
  yoc_obj_t *co;
  if (!fid) {
    yo_ops_t *ops;
    if (yo_get(farg, &ops)) {
      fid = -1;
    } else if (yarg_string(farg)==1) {
      char *name = ygets_q(farg);
      if (name[0]=='o' && name[1]==':') name+=2, fid=-1;
      else fid = -2;
      fref = yget_global(name, 0L);
    } else {
      return 1;
    }
  }
  if (fid>0 || dref<0) {
    if (yarg_typeid(darg) >= 100) return 1;
    dref = -1L;
  }
  co = ypush_obj(yfunc_obj(&yoc_ops), sizeof(yoc_obj_t));
  co->f = (fref>=0)? 0 : yget_use(farg+1);
  co->fndx = fref;
  co->d = (dref>=0 && fid==-1)? 0 : yget_use(darg+1);
  co->dndx = dref;
  if (!co->d && dref<0)
    y_error("bad second argument passed to closure()");
  return 0;
}

static void
yoc_on_free(void *vco)
{
  yoc_obj_t *co = vco;
  void *p = co->f;
  if (p) {
    co->f = 0;
    ydrop_use(p);
  }
  p = co->d;
  if (p) {
    co->d = 0;
    ydrop_use(p);
  }
}

static void
yoc_on_extract(void *vco, char *name)
{
  yoc_obj_t *co = vco;
  if (!strcmp(name, "function")) {
    if (!co->f && co->fndx>=0) ypush_global(co->fndx);
    else ykeep_use(co->f);
  } else if (!strcmp(name, "data")) {
    if (!co->d && co->dndx>=0) ypush_global(co->dndx);
    else ykeep_use(co->d);
  } else if (!strcmp(name, "function_name")) {
    char **q = ypush_q(0);
    if (co->fndx >= 0) q[0] = p_strcpy(globalTable.names[co->fndx]);
  } else if (!strcmp(name, "data_name")) {
    char **q = ypush_q(0);
    if (co->dndx >= 0) q[0] = p_strcpy(globalTable.names[co->dndx]);
  } else {
    y_error("unrecognized closure object member name");
  }
}

static void
yoc_on_eval(void *vco, int nargs)
{
  Operand op;
  int i;
  yoc_obj_t *co = vco;
  long dref = co->dndx;

  if (co->fndx >= 0) {
    ypush_global(co->fndx);
    if (yarg_func(0)) dref = -1L;
  } else {
    ykeep_use(co->f);
  }
  /* sp[-nargs-1] is co, sp[0] is func */
  for (i=0 ; i<=nargs ; i++) yarg_swap(i, i+1);
  /* sp[-nargs-1] is func, sp[-nargs] is co (func moved back nargs+1 steps) */

  if (dref < 0) {
    ykeep_use(co->d);
  } else {  /* object(member,...) semantics */
    sp[1].ops = &referenceSym;
    sp[1].index = dref;
    sp++;
  }
  /* sp[-nargs-2] is func, sp[-nargs-1] is co, sp[0] is data */
  nargs++;
  yarg_swap(nargs, 0);
  yarg_drop(1);
  /* sp[-nargs] is func, sp[1-nargs] is data (with incremented nargs) */

  FormEvalOp(nargs, &op);
  op.ops->Eval(&op);
}

void
Y_closure(int argc)
{
  if (argc != 2)
    y_error("closure requires exactly two arguments");
  if (yo_closure(1, 0))
    y_error("illegal argument type in closure(func,data)");
}

/* ------------------------------------------------------------------------ */
