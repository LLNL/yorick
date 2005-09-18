/*
 * $Id: ystr.c,v 1.1 2005-09-18 22:03:45 dhmunro Exp $
 * Yorick string manipulation functions
 *
 * This interface was inspired by the regexp package written by
 * Francois Rigaut.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "yfnmatch.h"
#include "yregexp.h"

#include "pstdlib.h"
#include "ydata.h"
#include <string.h>

extern BuiltIn Y_strlen, Y_strcase, Y_strchar, Y_strpart, Y__strtok;
extern BuiltIn Y_strword, Y_strfind, Y_strglob, Y_strgrep, Y_streplace;

/* must match array.c, should move to ydata.h */
#define MAX_INDICES 10

void
Y_strlen(int nargs)
{
  Dimension *dims = 0;
  char **q = yarg_q(0, &dims);
  if (nargs != 1) YError("strlen takes exactly one argument");
  if (dims) {
    Array *result = PushDataBlock(NewArray(&longStruct, dims));
    long n = TotalNumber(dims);
    long i, *lens = result->value.l;
    for (i=0 ; i<n ; i++) lens[i] = q[i]? strlen(q[i]) : 0;
  } else {
    PushLongValue(q[0]? (long)strlen(q[0]) : 0L);
  }
}

/* actually, all categories should be bits for multiple use
 * see is_upper, to_upper implementation in ansi ctype.h
 * this table picks up iso8859-1 european alphabets, like text plotting
 */
/* 0 control, 1 whitespace, 2 punctuation, 3 digit, 4 uc, 5 lc, 8 no case */
static char ys_table[256] =
  { 0,0,0,0,0,0,0,0, 0,1,1,1,1,1,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    1,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3, 3,3,2,2,2,2,2,2,
    2,4,4,4,4,4,4,4, 4,4,4,4,4,4,4,4, 4,4,4,4,4,4,4,4, 4,4,4,2,2,2,2,2,
    2,5,5,5,5,5,5,5, 5,5,5,5,5,5,5,5, 5,5,5,5,5,5,5,5, 5,5,5,2,2,2,2,0,
    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2, 2,2,2,2,2,2,2,2,
    4,4,4,4,4,4,4,4, 4,4,4,4,4,4,4,4, 4,4,4,4,4,4,4,2, 4,4,4,4,4,4,4,8,
    5,5,5,5,5,5,5,5, 5,5,5,5,5,5,5,5, 5,5,5,5,5,5,5,2, 5,5,5,5,5,5,5,8 };

void
Y_strcase(int nargs)
{
  int up = (yarg_sl(nargs-1) != 0);
  Dimension *dims = 0;
  char **q = yarg_q(nargs-2, &dims);
  long n = TotalNumber(dims);
  char **r = CalledAsSubroutine()? q : 0;
  unsigned char c, *t;
  if (nargs!=2 || !q)
    YError("strcase takes exactly two arguments");
  if (!r) {
    long i;
    r = ((Array*)PushDataBlock(NewArray(&stringStruct, dims)))->value.q;
    for (i=0 ; i<n ; i++) r[i] = p_strcpy(q[i]);
  }
  for (; n-- ; r++) {
    t = (unsigned char *)r[0];
    if (!t) continue;
    if (up) {
      for (c=t[0] ; c ; c=(++t)[0]) if (ys_table[c]==5) t[0] ^= ('A'^'a');
    } else {
      for (c=t[0] ; c ; c=(++t)[0]) if (ys_table[c]==4) t[0] ^= ('A'^'a');
    }
  }
}

void
Y_strchar(int nargs)
{
  Operand op;
  char **q, *c;
  long i, j, n;
  if (nargs!=1 || !yarg_op(0, &op))
    YError("strchar takes exactly one argument");

  n = TotalNumber(op.type.dims);

  if (op.type.base == &stringStruct) {
    char *qi;
    long nc = n;  /* number of terminal 0s */
    q = op.value;
    for (i=0 ; i<n ; i++) if (q[i]) nc += strlen(q[i]);
    c = ((Array*)PushDataBlock(NewArray(&charStruct,
                                        ynew_dim(nc,(void*)0))))->value.c;
    for (i=j=0 ; i<n ; i++,j++) {
      qi = q[i];
      if (qi) while (*qi) c[j++] = *qi++;
      c[j] = '\0';
    }

  } else if (op.type.base == &charStruct) {
    long nlead, ns;
    /* find leading dimension of multidimensional char array */
    Dimension *dims = 0, *lead = op.type.dims? op.type.dims->next : 0;
    if (lead) while (lead->next) lead = lead->next;
    nlead = lead? lead->number : n;
    c = op.value;
    for (i=j=ns=0 ; i<n ; i++,j++) {
      if (j == nlead) j = 0;
      if (!c[i] || j==nlead-1) ns++;
    }
    if (!ns) YError("(BUG) strchar impossible ns");
    if (ns>1) dims = ynew_dim(ns, (void*)0);
    q = ((Array*)PushDataBlock(NewArray(&stringStruct, dims)))->value.q;
    for (i=j=ns=0 ; i<n ;) {
      if (j >= nlead) j = 0;
      if (c[i]) {
        q[ns++] = p_strncat(0, &c[i], nlead-j);
        for (i++,j++ ; j<nlead && c[i] ; i++,j++);
        if (j<nlead) i++,j++;
      } else {
        i++;
        j++;
        ns++;
      }
    }

  } else {
    YError("strchar input must be type string or char");
  }
}

/* ------------------------------------------------------------------------ */

typedef struct ys_iter_t ys_iter_t;
struct ys_iter_t {
  long ndx[MAX_INDICES+1];
  long str[MAX_INDICES+1];
  long off[MAX_INDICES+1];
  long len[MAX_INDICES+1];
};

static int ys_stroff(int iarg, char ***pstr, Dimension **sdims,
                     long **poff, Dimension **odims);
static int ys_conform(int sel, long *str, long *off, long *rslt);
static Dimension *ys_dim(long nlead);
static void ys_error(char *fname, char *errmsg);

static void ys_increment(ys_iter_t *iter);
static void ys_inc3(ys_iter_t *iter, long *to);
static void ys_dcombine(long *str, long *off, long *rslt);
static void ys_strides(long *s);
static long ys_initer(ys_iter_t *iter, Dimension *sdims, Dimension *odims,
                      int mx_indices, char *fname);
static void ys_3combine(long *str, long *off, long *to, long *rslt);

static int
ys_stroff(int iarg, char ***pstr, Dimension **sdims,
          long **poff, Dimension **odims)
{
  *pstr = 0;
  *poff = 0;
  *sdims = *odims = 0;
  if (iarg >= 0) {
    *pstr = yarg_q(iarg--, sdims);
    if (*pstr) {
      Operand op;
      int iarg0 = iarg;
      while (!yarg_op(iarg0--, &op)) {
        if (iarg0 < 0) return iarg;
        /* skip over keywords */
        iarg0--;
      }
      if (op.ops->promoteID <= T_LONG) {
        /* argument was an offset, retrieve it */
        op.ops->ToLong(&op);
        *poff = op.value;
        *odims = op.type.dims;
        iarg = iarg0;
      }
    }
  }
  return iarg;
}

static int
ys_conform(int sel, long *str, long *off, long *rslt)
{
  Dimension *dims = tmpDims;
  int i, ndim = str[0];
  if (sel) {  /* remove leading dimension of off (it is really sel) */
    if (off[0] > 0) {
      off[0]--;
      for (i=1 ; i<=off[0] ; i++) off[i] = off[i+1];
    }
  }
  if (ndim < off[0]) ndim = off[0];
  for (i=str[0]+1 ; i<=ndim ; i++) str[i] = 1;
  for (i=off[0]+1 ; i<=ndim ; i++) off[i] = 1;
  rslt[0] = ndim;
  for (i=1 ; i<=ndim ; i++) {
    if (i>str[0] || str[i]==1) rslt[i] = off[i];
    else if (i>off[0] || off[i]==1 || off[i]==str[i]) rslt[i] = str[i];
    else return i;
  }

  if (dims) {
    tmpDims = 0;
    FreeDimension(dims);
  }
  for (i=1 ; i<=ndim ; i++) tmpDims = NewDimension(rslt[i], 1L, tmpDims);

  return 0;
}

static Dimension *
ys_dim(long nlead)
{
  if (tmpDims) {  /* assumes created by ys_conform */
    Dimension *dims = tmpDims;
    while (dims->next) dims = dims->next;
    dims->next = NewDimension(nlead, 1L, (Dimension *)0);
  } else {
    tmpDims = NewDimension(nlead, 1L, (Dimension *)0);
  }
  return tmpDims;
}

/* ndx = counter used to iterate
 * str = strides in string, str[0] is current index in string
 * off = strides in offset, off[0] is current index in offset
 * len = result strides and cumulative dimension lengths, len[1] = 1
 *       stride len[i+1] corresponds to str[i] and off[i]
 *
 * initially,
 *   str = [rank, ls1, ls2, ls3, ls4, ls5] dimensions of string
 *   off = [rank, lo1, lo2, lo3, lo4, lo5] dimensions of offset
 * each lsN=loN, or lsN=1, or loN=1 (else not conformable)
 * these dimensions can be reduced as follows:
 * while lsN=loN, accumulate their products
 *   when one becomes 1, accumulate products of the other until not 1
 * this reduces the two lists to forms like
 *   [rank, 1, la1, 1, la2, la3, 1, etc]
 *   [rank, lb1, 1, lb2, 1, la3, lb4, etc]
 * where neither list contains consecutive 1 lengths,
 * and there are never two consecutive non-1 lengths in both lists
 * now this information can be combined into the result length array,
 * plus str and off stride arrays:
 *   [rank, lb1, la1, lb2,  la2,     la3,      lb4,         etc]
 *   [rank, 0,   1,  -la1,  la1,     0,       -la1*la2*la3, etc]
 *   [rank, 1,  -lb1, lb1, -lb1*lb2, 1b1*lb2,  0,           etc]
 * the negative strides are the required retrace for the 1 dimensions
 * a negative entry is always immediately followed by an equal positive
 * consecutive positive entries are always equal
 */

static void
ys_increment(ys_iter_t *iter)
{
  int rank = iter->len[0];
  int i;
  for (i=1 ; i<=rank ; i++) {
    iter->str[0] += iter->str[i];
    iter->off[0] += iter->off[i];
    if (++iter->ndx[i] < iter->len[i]) break;
    iter->ndx[i] = 0;
  }
}

static void
ys_inc3(ys_iter_t *iter, long *to)
{
  int rank = iter->len[0];
  int i;
  for (i=1 ; i<=rank ; i++) {
    iter->str[0] += iter->str[i];
    iter->off[0] += iter->off[i];
    to[0] += to[i];
    if (++iter->ndx[i] < iter->len[i]) break;
    iter->ndx[i] = 0;
  }
}

static void
ys_dcombine(long *str, long *off, long *rslt)
{
  /* combines dimensions wherever possible */
  int i, j, ndim = rslt[0];
  /* note that ys_conform has filled in trailing 1s */
  for (i=1,j=0 ; i<=ndim ;) {
    j++;
    if (str[i] == 1) {
      rslt[j] = rslt[i];
      for (i++ ; i<=ndim && str[i]==1 ; i++) rslt[j] *= rslt[i];
      str[j] = 1;
      off[j] = rslt[j];
    } else if (off[i] == 1) {
      rslt[j] = rslt[i];
      for (i++ ; i<=ndim && off[i]==1 ; i++) rslt[j] *= rslt[i];
      off[j] = 1;
      str[j] = rslt[j];
    } else {
      rslt[j] = rslt[i];
      for (i++ ; i<=ndim && str[i]==off[i] ; i++) rslt[j] *= rslt[i];
      str[j] = off[j] = rslt[j];
    }
  }
  str[0] = off[0] = rslt[0] = j;
}

static void
ys_strides(long *s)
{
  /* converts list of lengths to list of strides */
  long si, n = 1;
  int i, ndim = s[0];
  if (!ndim) return;
  for (i=1 ; i<=ndim ; i++) {
    if (s[i] > 1) {
      n = s[i];
      s[i] = 1;
      break;
    }
    /* skip leading unit length dimension, actually at most i=1 */
    s[i] = 0;
  }
  for (i++ ; i<=ndim ; i++) {
    si = s[i];
    if (si > 1) {
      /* no extra stride unless previous dimension was retrace */
      s[i] = (s[i-1]<0)? n : 0;
      n *= si;
    } else {
      /* unit length dimension retraces to beginning of array */
      s[i] = (s[i-1]<0)? 0 : -n;
    }
  }
}

static long
ys_initer(ys_iter_t *iter, Dimension *sdims, Dimension *odims,
          int mx_indices, char *fname)
{
  long i, nstr;

  /* check s,off conformability, put result in tmpDims */
  iter->str[0] = YGet_dims(sdims, &iter->str[1], MAX_INDICES);
  iter->off[0] = YGet_dims(odims, &iter->off[1], MAX_INDICES);
  if (ys_conform(0, iter->str, iter->off, iter->len))
    ys_error(fname, "str,off arguments not conformable");
  nstr = TotalNumber(tmpDims);
  /* note that strfind result has one more dimension than input str,off */
  if (iter->len[0] > mx_indices)
    ys_error(fname, "inputs have too many dimensions");

  /* reduce dimensions as much as possible, convert str,off to strides */
  ys_dcombine(iter->str, iter->off, iter->len);
  ys_strides(iter->str);
  ys_strides(iter->off);
  for (i=0 ; i<=iter->len[0] ; i++) iter->ndx[i] = 0;
  iter->str[0] = iter->off[0] = 0;

  return nstr;
}

/* streplace version of dcombine */
static void
ys_3combine(long *str, long *off, long *to, long *rslt)
{
  /* combines dimensions wherever possible */
  int i, j, tp, ndim = rslt[0];
  for (i=str[0]+1 ; i<=ndim ; i++) str[i] = 1;
  for (i=off[0]+1 ; i<=ndim ; i++) off[i] = 1;
  for (i=to[0]+1 ; i<=ndim ; i++) to[i] = 1;
  /* to>1 if and only if either str>1 or off>1 */
  for (i=1,j=0 ; i<=ndim ;) {
    tp = (to[i]==1);
    j++;
    if (str[i] == 1) {
      rslt[j] = rslt[i];
      for (i++ ; i<=ndim && str[i]==1 && tp==(to[i]==1) ; i++)
        rslt[j] *= rslt[i];
      str[j] = 1;
      off[j] = rslt[j];
    } else if (off[i] == 1) {
      rslt[j] = rslt[i];
      for (i++ ; i<=ndim && off[i]==1 && tp==(to[i]==1) ; i++)
        rslt[j] *= rslt[i];
      off[j] = to[j] = 1;
      str[j] = rslt[j];
    } else {
      rslt[j] = rslt[i];
      for (i++ ; i<=ndim && str[i]==off[i] && tp==(to[i]==1) ; i++)
        rslt[j] *= rslt[i];
      str[j] = off[j] = rslt[j];
    }
    to[j] = tp? 1 : rslt[j];
  }
  str[0] = off[0] = to[0] = rslt[0] = j;
}

/* ------------------------------------------------------------------------ */

void
Y_strpart(int nargs)
{
  int in_place = CalledAsSubroutine();
  Operand op;
  long i, j, n, len, mn, mx;
  Dimension *dims;
  char **input, **output, *inp;

  if (nargs != 2) YError("strpart takes exactly two arguments");

  input = yarg_q(1, &dims);

  yarg_op(0, &op);
  if (op.ops==&rangeOps) {
    long min, max;
    int maxNil;
    Range *range = op.value;
    if (range->rf || range->inc!=1 ||
        range->nilFlags&(R_PSEUDO|R_RUBBER|R_NULLER))
      YError("bad 2nd argument to strpart");
    min = range->nilFlags&R_MINNIL? 0 : range->min-1L;
    maxNil = range->nilFlags&R_MAXNIL;
    max = range->max-1L;

    if (in_place) {
      output = input;
    } else {
      Array *result = PushDataBlock(NewArray(&stringStruct, dims));
      output = result->value.q;
    }

    n = TotalNumber(dims);
    for (i=0 ; i<n ; i++) {
      inp = input[i];
      if (inp) {
        len = strlen(inp);
        mn = min<0? len+min : min;
        if (maxNil) mx= len-1;
        else mx = max<0? len+max : max;
        if (mn<0) mn = 0;
        if (mx<0) mx = -1;
        if (mx>=len) mx = len-1;
        if (mn<len && mx>=mn) output[i] = p_strncat(0, inp+mn, mx-mn+1);
        else output[i] = p_strcpy("");
        if (in_place) p_free(inp);
      }
    }

  } else if (op.ops->promoteID <= T_LONG) {
    ys_iter_t iter;
    long *sel, isel, nlead;
    op.ops->ToLong(&op);
    sel = op.value;
    iter.str[0] = YGet_dims(dims, &iter.str[1], MAX_INDICES);
    iter.off[0] = YGet_dims(op.type.dims, &iter.off[1], MAX_INDICES);
    nlead = iter.off[0]? iter.off[1] : 1;
    if (nlead & 1)
      YError("strpart leading dimension of sel argument must be even");
    if (ys_conform(1, iter.str, iter.off, iter.len))
      YError("strpart str,sel arguments not conformable");
    n = TotalNumber(tmpDims);
    if (nlead > 2) {
      if (iter.len[0]==MAX_INDICES)
        YError("strpart result would have too many dimensions");
      ys_dim(nlead>>1);
    }

    if (in_place) {
      for (i=1 ; i<=iter.len[0] ; i++)
        if (i>iter.str[0] || iter.str[i]!=iter.len[i]) break;
      if (i<=iter.len[0] || nlead>2)
        YError("strpart in place requires result same shape as input");
      output = input;
    } else {
      Array *result = PushDataBlock(NewArray(&stringStruct, tmpDims));
      output = result->value.q;
    }

    /* reduce dimensions as much as possible, convert str,off to strides */
    ys_dcombine(iter.str, iter.off, iter.len);
    ys_strides(iter.str);
    ys_strides(iter.off);
    for (i=0 ; i<=iter.len[0] ; i++) iter.ndx[i] = 0;
    iter.str[0] = iter.off[0] = 0;

    for (i=0 ; i<n ; i++) {
      inp = input[iter.str[0]];
      len = inp? strlen(inp) : 0;
      isel = iter.off[0]*nlead;

      for (j=0 ; j<nlead ; j+=2) {
        mn = sel[isel+j];
        mx = sel[isel+j+1];
        if (inp && mx>=mn && mx<=len && mn>=0) {
          output[0] = (mx>mn)? p_strncat(0, inp+mn, mx-mn) : p_strcpy("");
          if (in_place && inp) p_free(inp);  /* nlead==2 in this case */
        } else if (in_place && inp) {
          output[0] = 0;
          p_free(inp);
        }
        output++;
      }

      ys_increment(&iter);
    }
  }
}

void
Y_streplace(int nargs)
{
  int in_place = CalledAsSubroutine();
  long i, n, len, mn, mx, j, m, im=0, lfrom, lto, ito, k;
  Dimension *sdims, *odims, *tdims=0;
  char **s, **t, **out, *inp, *to=0;
  ys_iter_t iter;
  long todim[MAX_INDICES+1], *sel, isel, nlead, tlead;

  int iarg = ys_stroff(nargs-1, &s, &sdims, &sel, &odims);
  if (!sel)
    YError("streplace second argument must be start-end indices");
  t = yarg_q(iarg--, &tdims);
  if (!t)
    YError("streplace third argument must be to-string");
  if (iarg >= 0)
    YError("streplace takes three non-keyword arguments");

  iter.str[0] = YGet_dims(sdims, &iter.str[1], MAX_INDICES);
  iter.off[0] = YGet_dims(odims, &iter.off[1], MAX_INDICES);
  nlead = iter.off[0]? iter.off[1] : 1;
  if (nlead & 1)
    YError("streplace leading dimension of start-end list must be even");
  if (ys_conform(1, iter.str, iter.off, iter.len))
    YError("streplace str,sel arguments not conformable");
  n = TotalNumber(tmpDims);
  todim[0] = YGet_dims(tdims, &todim[1], MAX_INDICES);
  tlead = 1;
  if (nlead > 2) {
    /* to-string needs a leading dimension matching sel's */
    if (todim[0]) tlead = todim[1];
    if (tlead!=1 && tlead+tlead!=nlead)
      YError("streplace to-string not conformable (leading dimension)");
    for (i=1 ; i<todim[0] ; i++) todim[i] = todim[i+1];
    if (todim[0]) todim[0]--;
    if (!todim[0]) todim[0] = todim[1] = 1;
  }
  /* to-string must not force broadcast */
  for (i=1 ; i<=todim[0] ; i++) {
    if (todim[i] == 1) continue;
    if (i>iter.len[0] || todim[i]!=iter.len[i])
      YError("streplace to-string not conformable (trailing dimensions)");
  }

  if (in_place) {
    for (i=1 ; i<=iter.len[0] ; i++)
      if (i>iter.str[0] || iter.str[i]!=iter.len[i])
        YError("strpart in place requires result same shape as input");
    out = s;
  } else {
    Array *result = PushDataBlock(NewArray(&stringStruct, tmpDims));
    out = result->value.q;
  }

  /* reduce dimensions as much as possible, convert str,off to strides */
  ys_3combine(iter.str, iter.off, todim, iter.len);
  ys_strides(iter.str);
  ys_strides(iter.off);
  ys_strides(todim);
  for (i=0 ; i<=iter.len[0] ; i++) iter.ndx[i] = 0;
  iter.str[0] = iter.off[0] = todim[0] = 0;

  for (i=0 ; i<n ; i++,out++) {
    inp = s[iter.str[0]];
    if (!inp) continue;
    len = strlen(inp);
    isel = iter.off[0]*nlead;
    ito = todim[0]*tlead;

    lfrom = lto = 0;
    for (j=0,m=-1 ; j<nlead ; j+=2) {
      iarg = (tlead>1 || !j);
      if (iarg) im = -1, to = t[ito++];
      mn = sel[isel+j];
      mx = sel[isel+j+1];
      if (mx>=mn && mx<=len && mn>=0) {
        if (m<0 || mn>=m)
          m = mx;
        else
          YError("streplace start-end list not disjoint and increasing");
        lfrom += mx-mn;
        if (im < 0) {
          if (to && to[0]) for (im=1 ; to[im] ; im++);
          else im = 0;
        }
        lto += im;
      }
    }
    ito -= tlead;
    out[0] = p_malloc(len - lfrom + lto + 1);
    out[0][0] = '\0';
    for (j=m=im=0 ; j<nlead ; j+=2) {
      if (tlead>1 || !j) to = t[ito++];
      mn = sel[isel+j];
      mx = sel[isel+j+1];
      if (mx>=mn && mx<=len && mn>=0) {
        while (im < mn) out[0][m++] = inp[im++];
        im = mx;
        if (to) for (k=0 ; to[k] ; k++) out[0][m++] = to[k];
      }
    }
    while ((out[0][m++] = inp[im++]));

    if (in_place && inp) p_free(inp);  /* nlead==2 in this case */

    ys_inc3(&iter, todim);
  }
}

static char *ys_default = " \t\n";
static void ys_strword(int nargs, int tok);

void
Y__strtok(int nargs)
{
  ys_strword(nargs, 1);
}

void
Y_strword(int nargs)
{
  ys_strword(nargs, 0);
}

void
ys_strword(int nargs, int tok)
{
  char **delim=&ys_default, **s, *str;
  long ndelim=1, ncount=1, *o, npairs, idel;
  Dimension *sdims, *odims, *ddims=0;
  int iarg, final, trailing, supress;
  long nstr, i, off, iout, *out, o0=0;
  ys_iter_t iter;
  char is_delim[256];

  iarg = ys_stroff(nargs-1, &s, &sdims, &o, &odims);
  if (!o) o = &o0;
  else if (tok) YError("strtok takes no offset argument, use strword");
  if (iarg<nargs-3)
    ys_error(tok?"strtok":"strword", "accepts no keywords");
  if (iarg>=0) {
    if (!yarg_nil(iarg)) delim = yarg_q(iarg--, &ddims);
    else iarg--;
    ndelim = TotalNumber(ddims);
    if (iarg>=0) {
      ncount = yarg_sl(iarg--);
      if (iarg>=0)
        ys_error(tok?"strtok":"strword", "argument list bad");
    }
  }
  if (tok) {
    if (ncount<=0) YError("strtok takes positive delim count, use strword");
    if (ndelim<2 && ncount<2) ncount = 2;
  }

  nstr = ys_initer(&iter, sdims, odims, MAX_INDICES-1, tok?"strtok":"strword");

  trailing = (ncount == 0);
  supress = (ncount <= 0);
  if (supress && !trailing) ncount = -ncount-1;
  npairs = ndelim+ncount-1;
  if (npairs <= 0)
    YError("strword N<=0 needs at least two delimiters");

  out = ((Array*)
         PushDataBlock(NewArray(&longStruct, ys_dim(2*npairs))))->value.l;

  /* outer loop is over [start,end] pairs in result */
  for (idel=final=0 ; !final ; idel++) {

    final = (idel == npairs+supress-1);

    /* initialize is_delim array for next element of delim */
    if (idel<ndelim) {
      char *d = delim? *delim++ : ys_default;
      char v = (d && d[0]=='^' && d[1]);
      /* v=1 means everything is a delim unless it is listed */
      for (i=0 ; i<256 ; i++) is_delim[i] = v;
      if (d) {
        unsigned char dprev = 0;
        if (v) d++;
        v = !v;
        while (d[0]) {
          if (d[0]=='-' && dprev && d[1]) {  /* range of characters */
            while (++dprev <= (unsigned char)d[1]) is_delim[dprev] = v;
            d += 2;
            dprev = 0;
          } else {  /* single character or first character of range */
            dprev = *d++;
            is_delim[dprev] = v;
          }
        }
      }
    }

    /* loop over input string array elements */
    for (i=0,iout=idel+idel ; i<nstr ; i++,iout+=npairs+npairs) {
      str = s[iter.str[0]];

      /* prev pass, if any, reached start but not end of prev word
       * - begin by scanning to end of prev word
       */
      if (!str) {
        off = 0;
      } else if (!idel) {
        for (off=0 ; off<o[iter.off[0]] ; off++) if (!str[off]) break;
      } else {
        off = out[iout-2];    /* start of previous word */
        if (str[off]) {
          if (!final || !trailing) {
            is_delim[0] = 1;
            while (!is_delim[(unsigned int)str[off]]) off++;
          } else {
            /* ncount==0 special case, trim trailing delimeter */
            while (str[off]) off++;
            while (off && is_delim[(unsigned int)str[off-1]]) off--;
          }
          out[iout-1] = off;  /* end of previous word (this delimiter) */
          if (str[off]) off++;
        } else {
          /* already hit end of string */
          out[iout-1] = -1;
        }
      }

      if (!final || !supress) {
        /* scan past any delimiters to find start of current word
         * (strtok has slightly different semantics for trailing part) */
        if (str && (!tok || !final)) {
          is_delim[0] = 0;
          while (is_delim[(unsigned int)str[off]]) off++;
        }
        out[iout] = off;

        if (final) {
          /* unless supressed, final word extends to end of string */
          if (str && str[off]) while (str[off]) off++;
          else off = -1;
          out[iout+1] = off;
        }
      }

      ys_increment(&iter);
    }

    iter.str[0] = iter.off[0] = 0;
  }
}

typedef struct ys_state_t ys_state_t;
struct ys_state_t {
  char *pat;
  long off, off0, lpat, *out, nrep, pass;
};

#define YS_MAX_KEYS 3
static char *ys_kglob[] = { "case", "path", "esc", 0 };

void
Y_strglob(int nargs)
{
  Symbol *keys[YS_MAX_KEYS];
  long i, nstr, *o = 0;
  char *str, **s = 0;
  Dimension *sdims=0, *odims=0;
  int iarg, flags;
  ys_iter_t iter;
  ys_state_t state;
  int *out;

  for (i=0 ; i<YS_MAX_KEYS ; i++) keys[i] = 0;
  iarg = yarg_keys(nargs-1, ys_kglob, keys);
  if (iarg >= 0) {
    state.pat = yarg_sq(iarg--);
    iarg = yarg_keys(iarg, ys_kglob, keys);
    iarg = ys_stroff(iarg, &s, &sdims, &o, &odims);
    iarg = yarg_keys(iarg, ys_kglob, keys);
  }
  if (!s || iarg>=0)
    YError("strfind takes 2 or 3 non-key arguments");
  nstr = ys_initer(&iter, sdims, odims, MAX_INDICES, "strglob");

  /* analyze other keywords (FNM_CASEFOLD is a GNU extension) */
  flags = (keys[0] && !YGetInteger(keys[0]))? FNM_CASEFOLD : 0;
  iarg = keys[1]? YGetInteger(keys[1]) : 0;
  if (iarg&1) flags |= FNM_PATHNAME;
  if (iarg&2) flags |= FNM_PERIOD;
  if (keys[2] && !YGetInteger(keys[2])) flags |= FNM_NOESCAPE;

  out = ((Array*)PushDataBlock(NewArray(&intStruct, tmpDims)))->value.i;

  for (i=0 ; i<nstr ; i++) {
    str = s[iter.str[0]];

    if (state.pat && str) {
      if (!o)
        state.off = 0;
      else
        for (state.off=0 ; state.off<o[iter.off[0]] ; state.off++)
          if (!str[state.off]) break;
      *(out++) = !fnmatch_fr(state.pat, str+state.off, flags);
    } else {
      *(out++) = 0;
    }

    ys_increment(&iter);
  }
}

static void ys_findf(ys_state_t *state, char *str);
static void ys_findcf(ys_state_t *state, char *str);
static void ys_findb(ys_state_t *state, char *str);
static void ys_findcb(ys_state_t *state, char *str);
static void (*ys_finder[4])(ys_state_t *state, char *str) = {
  ys_findf, ys_findcf, ys_findb, ys_findcb };

static char *ys_kfind[] = { "n", "case", "back", 0 };

void
Y_strfind(int nargs)
{
  void (*searcher)(ys_state_t *state, char *str);
  Symbol *keys[YS_MAX_KEYS];
  long i, nstr, *o;
  char *str, **s=0;
  Dimension *sdims=0, *odims=0;
  int iarg;
  ys_iter_t iter;
  ys_state_t state;

  for (i=0 ; i<YS_MAX_KEYS ; i++) keys[i] = 0;
  iarg = yarg_keys(nargs-1, ys_kfind, keys);
  if (iarg >= 0) {
    state.pat = yarg_sq(iarg--);
    iarg = yarg_keys(iarg, ys_kfind, keys);
    iarg = ys_stroff(iarg, &s, &sdims, &o, &odims);
    iarg = yarg_keys(iarg, ys_kfind, keys);
  }
  if (!s || iarg>=0)
    YError("strfind takes 2 or 3 non-key arguments");
  nstr = ys_initer(&iter, sdims, odims, MAX_INDICES-1, "strfind");

  /* analyze keywords */
  state.nrep = keys[0]? YGetInteger(keys[0]) : 1;
  if (state.nrep < 1)
    YError("strfind illegal n= repeat count");
  iarg = (keys[1] && !YGetInteger(keys[1]));
  if (keys[2] && YGetInteger(keys[2])) {
    iarg |= 2;
    state.lpat = 0;
    if (state.pat) for ( ; state.pat[state.lpat] ; state.lpat++);
  }
  searcher = ys_finder[iarg];

  state.out = ((Array*)
               PushDataBlock(NewArray(&longStruct,
                                      ys_dim(2*state.nrep))))->value.l;

  for (i=0 ; i<nstr ; i++) {
    str = s[iter.str[0]];

    if (state.pat && str) {
      if (!o)
        state.off = 0;
      else
        for (state.off=0 ; state.off<o[iter.off[0]] ; state.off++)
          if (!str[state.off]) break;
      for (state.pass=0 ; state.pass<state.nrep ; state.pass++)
        searcher(&state, str);
    } else {
      for (state.pass=0 ; state.pass<state.nrep ; state.pass++) {
        *(state.out++) = 0;
        *(state.out++) = -1;
      }
    }

    ys_increment(&iter);
  }
}

static void
ys_findf(ys_state_t *state, char *str)
{
  /* forward search */
  char *pat = state->pat;
  long i, j, k = state->off;
  for (;; k++) {
    if (!str[k]) {
      *(state->out++) = state->off = k;
      *(state->out++) = -1;
      return;
    }
    for (i=0,j=k ; pat[i] && pat[i]==str[j] ; i++) j++;
    if (!pat[i]) {
      *(state->out++) = j-i;
      *(state->out++) = state->off = j;
      return;
    }
  }
}

#define YS_EQ_NOCASE(c,d) ((ys_table[(unsigned int)c]&4)? !((c^d)&0xdf):(c==d))

static void
ys_findcf(ys_state_t *state, char *str)
{
  /* forward search, case insensitive */
  char *pat = state->pat;
  long i, j, k = state->off;
  for (;; k++) {
    if (!str[k]) {
      *(state->out++) = state->off = k;
      *(state->out++) = -1;
      return;
    }
    for (i=0,j=k ; pat[i] && YS_EQ_NOCASE(pat[i],str[j]) ; i++) j++;
    if (!pat[i]) {
      *(state->out++) = j-i;
      *(state->out++) = state->off = j;
      return;
    }
  }
}

static void
ys_findb(ys_state_t *state, char *str)
{
  /* backward search */
  char *pat = state->pat;
  long lpat = state->lpat;
  long i, j, k = state->off, k0 = state->off0;
  long iout = -(state->pass + state->pass);
  if (!state->pass) {
    for (k=0 ; str[k] ; k++);
    k0 = state->off0 = state->off;
    state->out += state->nrep + state->nrep;
  }
  for (k-- ;; k--) {
    if (k < k0+lpat-1) {
      state->out[iout-2] = state->off = k0;
      state->out[iout-1] = -1;
      return;
    }
    for (i=lpat-1,j=k ; i>=0 && j>=k0 && pat[i]==str[j] ; i--) j--;
    if (i<0) {
      state->out[iout-2] = state->off = ++j;
      state->out[iout-1] = j + lpat;
      return;
    }
  }
}

static void
ys_findcb(ys_state_t *state, char *str)
{
  /* backward search, case insensitive */
  char *pat = state->pat;
  long lpat = state->lpat;
  long i, j, k = state->off, k0 = state->off0;
  long iout = -(state->pass + state->pass);
  if (!state->pass) {
    for (k=0 ; str[k] ; k++);
    k0 = state->off0 = state->off;
    state->out += state->nrep + state->nrep;
  }
  for (k-- ;; k--) {
    if (k < k0+lpat-1) {
      state->out[iout-2] = state->off = k0;
      state->out[iout-1] = -1;
      return;
    }
    for (i=lpat-1,j=k; i>=0 && j>=k0 && YS_EQ_NOCASE(pat[i],str[j]) ;i--) j--;
    if (i<0) {
      state->out[iout-2] = state->off = ++j;
      state->out[iout-1] = j + lpat;
      return;
    }
  }
}

static regexp *ys_re = 0;
static void ys_reclear(void);
static void
ys_reclear(void)
{
  regexp *re = ys_re;
  ys_re = 0;
  if (re) p_free(re);  /* garbage from prior asynchronous interrupt */
  regerr_dm();         /* reset (very crude) regexp error detection */
}
static char *ys_kgrep[] = { "n", "sub", 0 };

void
Y_strgrep(int nargs)
{
  Symbol *keys[YS_MAX_KEYS];
  long i, j, is, ntot, nstr, nsub=1, *sub=0, *o;
  long default_sub[1];
  char *str, **s=0;
  Dimension *sdims=0, *odims=0;
  int iarg;
  ys_iter_t iter;
  ys_state_t state;

  for (i=0 ; i<YS_MAX_KEYS ; i++) keys[i] = 0;
  iarg = yarg_keys(nargs-1, ys_kgrep, keys);
  if (iarg >= 0) {
    state.pat = yarg_sq(iarg--);
    iarg = yarg_keys(iarg, ys_kgrep, keys);
    iarg = ys_stroff(iarg, &s, &sdims, &o, &odims);
    iarg = yarg_keys(iarg, ys_kgrep, keys);
  }
  if (!s || iarg>=0)
    YError("strgrep takes 2 or 3 non-key arguments");
  nstr = ys_initer(&iter, sdims, odims, MAX_INDICES-1, "strgrep");

  /* analyze keywords */
  state.nrep = keys[0]? YGetInteger(keys[0]) : 1;
  if (state.nrep < 1)
    YError("strgrep illegal n= repeat count");
  if (keys[1]) {
    sdims = 0;
    sub = YGet_L(keys[1], 1, &sdims);
    if (sub) {
      if (sdims) {
        if (sdims->next)
          YError("strgrep sub= not scalar or 1D list of subexpressions");
        nsub = sdims->number;
      }
      if (nsub > 10)
        YError("strgrep sub= list can contain at most 10 items");
      for (i=0 ; i<nsub ; i++) {
        if (sub[i]<0 || sub[i]>=NSUBEXP)
          YError("strgrep sub= subexpression numbers run from 0 to 9");
      }
    }
  }
  if (!sub) {
    default_sub[0] = 0;
    sub = default_sub;
  }

  /* compile the regular expression */
  ys_reclear();
  if (state.pat) {
    ys_re = regcomp_hs(state.pat);
    if (!ys_re) {
      char msg[80], *errmsg;
      strcpy(msg, "strgrep: pattern failed to compile: ");
      errmsg = regerr_dm();
      if (errmsg)
        strncat(msg+36, errmsg? errmsg : "(unknown reason)", 79-36);
      YError(msg);
    }
    /* mxsub = regnpar_dm();   dont second guess, trust the user */
  }

  ntot = state.nrep*nsub;
  state.out = ((Array*)
               PushDataBlock(NewArray(&longStruct,
                                      ys_dim(ntot+ntot))))->value.l;

  for (i=0 ; i<nstr ; i++) {
    str = s[iter.str[0]];

    if (state.pat && str) {
      if (!o)
        state.off = 0;
      else
        for (state.off=0 ; state.off<o[iter.off[0]] ; state.off++)
          if (!str[state.off]) break;
      for (state.pass=0 ; state.pass<state.nrep ; state.pass++) {
        if (regexec_hs(ys_re, str+state.off)) {
          state.off =  ys_re->endp[0] - str;
          for (j=0 ; j<nsub ; j++) {
            is = sub[j];
            if (ys_re->startp[is]) {
              *(state.out++) = ys_re->startp[is] - str;
              *(state.out++) = ys_re->endp[is] - str;
            } else {
              *(state.out++) = state.off;
              *(state.out++) = -1;
            }
          }
        } else {
          /* interface does not distinguish error from match failure
           * - however only errors are bad input data,
           *   which we presume impossible here
           */
          for (state.off=0 ; str[state.off] ; state.off++);
          do {
            for (j=0 ; j<nsub ; j++) {
              *(state.out++) = state.off;
              *(state.out++) = -1;
            }
          } while (++state.pass<state.nrep);
          break;
        }
      }
    } else {
      for (state.pass=0 ; state.pass<ntot ; state.pass++) {
        *(state.out++) = 0;
        *(state.out++) = -1;
      }
    }

    ys_increment(&iter);
  }

  ys_reclear();
}

static void
ys_error(char *fname, char *errmsg)
{
  char msg[120];
  strcpy(msg, fname);  /* fname must be <18 characters */
  strcat(msg, ": ");
  strncat(msg, errmsg, 100);
  YError(msg);
}
