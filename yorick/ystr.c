/*
 * $Id: ystr.c,v 1.4 2008-10-19 22:19:24 dhmunro Exp $
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
#include "yapi.h"
#include <string.h>

extern void Y_strlen(int argc);
extern void Y_strcase(int argc);
extern void Y_strchar(int argc);
extern void Y_strpart(int argc);
extern void Y__strtok(int argc);
extern void Y_strword(int argc);
extern void Y_strfind(int argc);
extern void Y_strglob(int argc);
extern void Y_strgrep(int argc);
extern void Y_streplace(int argc);

void
Y_strlen(int argc)
{
  long n, dims[Y_DIMSIZE];
  char **q = ygeta_q(0, &n, dims);
  if (argc != 1) y_error("strlen takes exactly one argument");
  if (dims[0]) {
    long i, *lens = ypush_l(dims);
    for (i=0 ; i<n ; i++) lens[i] = q[i]? strlen(q[i]) : 0;
  } else {
    ypush_long(q[0]? (long)strlen(q[0]) : 0L);
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
Y_strcase(int argc)
{
  int up = (ygets_l(argc-1) != 0);
  long n, dims[Y_DIMSIZE];
  char **q = ygeta_q(argc-2, &n, dims);
  unsigned char c, *t;
  if (argc!=2 || !q)
    y_error("strcase takes exactly two arguments");
  if (!yarg_subroutine() && !yarg_scratch(0)) {
    long i;
    char **r = ypush_q(dims);
    for (i=0 ; i<n ; i++) r[i] = p_strcpy(q[i]);
    q = r;
  }
  for (; n-- ; q++) {
    t = (unsigned char *)q[0];
    if (!t) continue;
    if (up) {
      for (c=t[0] ; c ; c=(++t)[0]) if (ys_table[c]==5) t[0] ^= ('A'^'a');
    } else {
      for (c=t[0] ; c ; c=(++t)[0]) if (ys_table[c]==4) t[0] ^= ('A'^'a');
    }
  }
}

void
Y_strchar(int argc)
{
  char **q, *c;
  long i, j, n, dims[Y_DIMSIZE];
  int typeid = yarg_typeid(0);
  if (argc!=1)
    y_error("strchar takes exactly one argument");

  if (typeid == Y_STRING) {
    char *qi;
    q = ygeta_q(0, &n, 0);
    for (dims[0]=1,dims[1]=n, i=0 ; i<n ; i++)
      if (q[i]) dims[1] += strlen(q[i]);
    c = ypush_c(dims);
    for (i=j=0 ; i<n ; i++,j++) {
      qi = q[i];
      if (qi) while (*qi) c[j++] = *qi++;
      c[j] = '\0';
    }

  } else if (typeid == Y_CHAR) {
    long nlead, ns;
    /* find leading dimension of multidimensional char array */
    c = ygeta_c(0, &n, dims);
    nlead = dims[0]? dims[1] : n;
    for (i=j=ns=0 ; i<n ; i++,j++) {
      if (j == nlead) j = 0;
      if (!c[i] || j==nlead-1) ns++;
    }
    if (!ns) y_error("(BUG) strchar impossible ns");
    dims[0] = (ns>1);
    dims[1] = ns;
    q = ypush_q(dims);
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
    y_error("strchar input must be type string or char");
  }
}

/* ------------------------------------------------------------------------ */

typedef struct ys_iter_t ys_iter_t;
struct ys_iter_t {
  long ndx[Y_DIMSIZE];
  long str[Y_DIMSIZE];
  long off[Y_DIMSIZE];
  long len[Y_DIMSIZE];
};

static int ys_stroff(int iarg, char ***pstr, long *sdims,
                     long **poff, long *odims);
static int ys_conform(int sel, long *str, long *off, long *rslt);
static int ys_dim(long *dims, long nlead, long *dims0);

static void ys_increment(ys_iter_t *iter);
static void ys_inc3(ys_iter_t *iter, long *to);
static void ys_dcombine(long *str, long *off, long *rslt);
static void ys_strides(long *s);
static long ys_initer(ys_iter_t *iter, int mx_indices, char *fname);
static void ys_initer2(ys_iter_t *iter);
static void ys_3combine(long *str, long *off, long *to, long *rslt);

static int
ys_stroff(int iarg, char ***pstr, long *sdims, long **poff, long *odims)
{
  *pstr = 0;
  *poff = 0;
  sdims[0] = odims[0] = 0;
  if (iarg >= 0) {
    *pstr = ygeta_q(iarg--, 0, sdims);
    if (*pstr) {
      while (yarg_key(iarg) >= 0) {
        iarg -= 2;  /* skip over keywords */
        if (iarg < 0) return iarg;
      }
      if (yarg_typeid(iarg) <= Y_LONG)
        /* argument was an offset, retrieve it */
        *poff = ygeta_l(iarg--, 0, odims);
    }
  }
  return iarg;
}

static int
ys_conform(int sel, long *str, long *off, long *rslt)
{
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

  return 0;
}

static int
ys_dim(long *dims, long nlead, long *dims0)
{
  long n = *dims0++;
  if (n >= Y_DIMSIZE-1) return 1;
  *dims++ = n+1;
  *dims++ = nlead;
  while (n-- > 0) *dims++ = *dims0++;
  return 0;
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
ys_initer(ys_iter_t *iter, int mx_indices, char *fname)
{
  long i, nstr;

  /* check s,off conformability */
  if (ys_conform(0, iter->str, iter->off, iter->len))
    y_errorq("%s: str,off arguments not conformable", fname);
  for (nstr=1,i=1 ; i<=iter->len[0] ; i++) nstr *= iter->len[i];
  /* note that strfind result has one more dimension than input str,off */
  if (iter->len[0] > mx_indices)
    y_errorq("%s: inputs have too many dimensions", fname);

  return nstr;
}

static void
ys_initer2(ys_iter_t *iter)
{
  long i;

  /* reduce dimensions as much as possible, convert str,off to strides */
  ys_dcombine(iter->str, iter->off, iter->len);
  ys_strides(iter->str);
  ys_strides(iter->off);
  for (i=0 ; i<=iter->len[0] ; i++) iter->ndx[i] = 0;
  iter->str[0] = iter->off[0] = 0;
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
Y_strpart(int argc)
{
  int in_place = yarg_subroutine();
  int typeid = yarg_typeid(0);
  long i, j, n, len, mn, mx;
  ys_iter_t iter;
  char **input, **output, *inp;

  if (argc != 2) y_error("strpart takes exactly two arguments");

  input = ygeta_q(1, &n, iter.str);

  if (typeid == Y_RANGE) {
    long min, max, nxs[3];
    int flags = yget_range(0, nxs);
    if (flags&14 || nxs[2]!=1)
      y_error("strpart 2nd argument cannot have range function or step");
    min = (flags&Y_MIN_DFLT)? 0 : nxs[0]-1L;
    max = nxs[1]-1L;
    flags &= Y_MAX_DFLT;

    output = in_place? input : ypush_q(iter.str);

    for (i=0 ; i<n ; i++) {
      inp = input[i];
      if (inp) {
        len = strlen(inp);
        mn = min<0? len+min : min;
        if (flags) mx = len-1;
        else       mx = max<0? len+max : max;
        if (mn<0) mn = 0;
        if (mx<0) mx = -1;
        if (mx>=len) mx = len-1;
        if (mn<len && mx>=mn) output[i] = p_strncat(0, inp+mn, mx-mn+1);
        else output[i] = p_strcpy("");
        if (in_place) p_free(inp);
      }
    }

  } else if (typeid <= Y_LONG) {
    long *sel, isel, nlead;
    sel = ygeta_l(0, 0, iter.off);
    nlead = iter.off[0]? iter.off[1] : 1;
    if (nlead & 1)
      y_error("strpart leading dimension of sel argument must be even");
    if (ys_conform(1, iter.str, iter.off, iter.len))
      y_error("strpart str,sel arguments not conformable");
    for (n=1,i=1 ; i<=iter.len[0] ; i++) n *= iter.len[i];

    if (in_place) {
      for (i=1 ; i<=iter.len[0] ; i++)
        if (i>iter.str[0] || iter.str[i]!=iter.len[i]) break;
      if (i<=iter.len[0] || nlead>2)
        y_error("strpart in place requires result same shape as input");
      output = input;
    } else {
      if (nlead > 2) {
        long dims[Y_DIMSIZE];
        if (ys_dim(dims, nlead>>1, iter.len))
          y_error("strpart result would have too many dimensions");
        output = ypush_q(dims);
      } else {
        output = ypush_q(iter.len);
      }
    }

    /* reduce dimensions as much as possible, convert str,off to strides */
    ys_initer2(&iter);

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

  } else {
    y_error("strpart 2nd argument must be range or sel");
  }
}

void
Y_streplace(int argc)
{
  int in_place = yarg_subroutine();
  long i, n, len, mn, mx, j, m, im=0, lfrom, lto, ito, k;
  char **s, **t, **out, *inp, *to=0;
  ys_iter_t iter;
  long todim[Y_DIMSIZE], *sel, isel, nlead, tlead;

  int iarg = ys_stroff(argc-1, &s, iter.str, &sel, iter.off);
  if (!sel)
    y_error("streplace second argument must be start-end indices");
  t = ygeta_q(iarg--, 0, todim);
  if (!t)
    y_error("streplace third argument must be to-string");
  if (iarg >= 0)
    y_error("streplace takes three non-keyword arguments");

  nlead = iter.off[0]? iter.off[1] : 1;
  if (nlead & 1)
    y_error("streplace leading dimension of start-end list must be even");
  if (ys_conform(1, iter.str, iter.off, iter.len))
    y_error("streplace str,sel arguments not conformable");
  for (n=1,i=1 ; i<=iter.len[0] ; i++) n *= iter.len[i];
  tlead = 1;
  if (nlead > 2) {
    /* to-string needs a leading dimension matching sel's */
    if (todim[0]) tlead = todim[1];
    if (tlead!=1 && tlead+tlead!=nlead)
      y_error("streplace to-string not conformable (leading dimension)");
    for (i=1 ; i<todim[0] ; i++) todim[i] = todim[i+1];
    if (todim[0]) todim[0]--;
    if (!todim[0]) todim[0] = todim[1] = 1;
  }
  /* to-string must not force broadcast */
  for (i=1 ; i<=todim[0] ; i++) {
    if (todim[i] == 1) continue;
    if (i>iter.len[0] || todim[i]!=iter.len[i])
      y_error("streplace to-string not conformable (trailing dimensions)");
  }

  if (in_place) {
    for (i=1 ; i<=iter.len[0] ; i++)
      if (i>iter.str[0] || iter.str[i]!=iter.len[i])
        y_error("streplace in place requires result same shape as input");
    out = s;
  } else {
    out = ypush_q(iter.len);
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
    if (inp) {
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
            y_error("streplace start-end list not disjoint and increasing");
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

      if (in_place) p_free(inp);  /* nlead==2 in this case */
    }
    ys_inc3(&iter, todim);
  }
}

static char *ys_default = " \t\n";
static void ys_strword(int argc, int tok);

void
Y__strtok(int argc)
{
  ys_strword(argc, 1);
}

void
Y_strword(int argc)
{
  ys_strword(argc, 0);
}

void
ys_strword(int argc, int tok)
{
  char **delim=&ys_default, **s, *str;
  long ndelim=1, ncount=1, *o, npairs, idel;
  int iarg, final, trailing, supress;
  long nstr, i, off, iout, *out, o0=0;
  ys_iter_t iter;
  char is_delim[256];
  long dims[Y_DIMSIZE];

  iarg = ys_stroff(argc-1, &s, iter.str, &o, iter.off);
  if (!o) o = &o0;
  else if (tok) y_error("strtok takes no offset argument, use strword");
  if (iarg<argc-3)
    y_errorq("%s: accepts no keywords", tok?"strtok":"strword");
  if (iarg>=0) {
    if (!yarg_nil(iarg)) delim = ygeta_q(iarg, &ndelim, 0);
    iarg--;
    if (iarg>=0) {
      ncount = ygets_l(iarg--);
      if (iarg>=0)
        y_errorq("%s: argument list bad", tok?"strtok":"strword");
    }
  }
  if (tok) {
    if (ncount<=0) y_error("strtok takes positive delim count, use strword");
    if (ndelim<2 && ncount<2) ncount = 2;
  }
  nstr = ys_initer(&iter, Y_DIMSIZE-2, tok?"strtok":"strword");

  trailing = (ncount == 0);
  supress = (ncount <= 0);
  if (supress && !trailing) ncount = -ncount-1;
  npairs = ndelim+ncount-1;
  if (npairs <= 0)
    y_error("strword N<=0 needs at least two delimiters");

  ys_dim(dims, npairs+npairs, iter.len);
  ys_initer2(&iter);
  out = ypush_l(dims);

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
            while (!is_delim[(unsigned char)str[off]]) off++;
          } else {
            /* ncount==0 special case, trim trailing delimeter */
            while (str[off]) off++;
            while (off && is_delim[(unsigned char)str[off-1]]) off--;
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
          while (is_delim[(unsigned char)str[off]]) off++;
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
static long ys_nkglob[YS_MAX_KEYS+1];

void
Y_strglob(int argc)
{
  int keys[YS_MAX_KEYS];
  long i, nstr, *o = 0;
  char *str, **s = 0;
  int iarg, flags;
  ys_iter_t iter;
  ys_state_t state;
  int *out;

  yarg_kw_init(ys_kglob, ys_nkglob, keys);
  iarg = yarg_kw(argc-1, ys_nkglob, keys);
  if (iarg >= 0) {
    state.pat = ygets_q(iarg--);
    iarg = yarg_kw(iarg, ys_nkglob, keys);
    flags = ys_stroff(iarg, &s, iter.str, &o, iter.off);
    for (iarg-- ; iarg>=flags ; iarg--)
      iarg = yarg_kw(iarg, ys_nkglob, keys);
  } else {
    state.pat = 0;
  }
  if (!s || iarg>=0)
    y_error("strfind takes 2 or 3 non-key arguments");
  nstr = ys_initer(&iter, Y_DIMSIZE-1, "strglob");

  /* analyze other keywords (FNM_CASEFOLD is a GNU extension) */
  flags = (keys[0]>=0 && !ygets_l(keys[0]))? FNM_CASEFOLD : 0;
  iarg = (keys[1]>=0)? ygets_l(keys[1]) : 0;
  if (iarg&1) flags |= FNM_PATHNAME;
  if (iarg&2) flags |= FNM_PERIOD;
  if (keys[2]>=0 && !ygets_l(keys[2])) flags |= FNM_NOESCAPE;

  out = ypush_i(iter.len);
  ys_initer2(&iter);

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
static long ys_nkfind[YS_MAX_KEYS+1];

void
Y_strfind(int argc)
{
  void (*searcher)(ys_state_t *state, char *str);
  int keys[YS_MAX_KEYS];
  long i, nstr, *o=NULL;
  char *str, **s=0;
  int iarg;
  ys_iter_t iter;
  ys_state_t state;
  long dims[Y_DIMSIZE];

  yarg_kw_init(ys_kfind, ys_nkfind, keys);
  iarg = yarg_kw(argc-1, ys_nkfind, keys);
  if (iarg >= 0) {
    int iarg0;
    state.pat = ygets_q(iarg--);
    iarg = yarg_kw(iarg, ys_nkfind, keys);
    iarg0 = ys_stroff(iarg, &s, iter.str, &o, iter.off);
    for (iarg-- ; iarg>=iarg0 ; iarg--)
      iarg = yarg_kw(iarg, ys_nkfind, keys);
  }
  if (!s || iarg>=0)
    y_error("strfind takes 2 or 3 non-key arguments");
  nstr = ys_initer(&iter, Y_DIMSIZE-2, "strfind");

  /* analyze keywords */
  state.nrep = (keys[0]>=0)? ygets_l(keys[0]) : 1;
  if (state.nrep < 1)
    y_error("strfind illegal n= repeat count");
  iarg = (keys[1]>=0 && !ygets_l(keys[1]));
  if (keys[2]>=0 && ygets_l(keys[2])) {
    iarg |= 2;
    state.lpat = 0;
    if (state.pat) for ( ; state.pat[state.lpat] ; state.lpat++);
  }
  searcher = ys_finder[iarg];

  ys_dim(dims, state.nrep+state.nrep, iter.len);
  ys_initer2(&iter);
  state.out = ypush_l(dims);

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

#define YS_EQ_NOCASE(c,d) ((ys_table[(unsigned char)c]&4)?!((c^d)&0xdf):(c==d))

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
static long ys_nkgrep[YS_MAX_KEYS];

void
Y_strgrep(int argc)
{
  int keys[YS_MAX_KEYS-1];
  long i, j, is, ntot, nstr, nsub=1, *sub=0, *o=NULL;
  long default_sub[1];
  char *str, **s=0;
  int iarg;
  ys_iter_t iter;
  ys_state_t state;
  long dims[Y_DIMSIZE];

  yarg_kw_init(ys_kgrep, ys_nkgrep, keys);
  iarg = yarg_kw(argc-1, ys_nkgrep, keys);
  if (iarg >= 0) {
    int iarg0;
    state.pat = ygets_q(iarg--);
    iarg = yarg_kw(iarg, ys_nkgrep, keys);
    iarg0 = ys_stroff(iarg, &s, iter.str, &o, iter.off);
    for (iarg-- ; iarg>=iarg0 ; iarg--)
      iarg = yarg_kw(iarg, ys_nkgrep, keys);
  } else {
    state.pat = 0;
  }
  if (!s || iarg>=0)
    y_error("strgrep takes 2 or 3 non-key arguments");
  nstr = ys_initer(&iter, Y_DIMSIZE-2, "strgrep");

  /* analyze keywords */
  state.nrep = (keys[0]>=0)? ygets_l(keys[0]) : 1;
  if (state.nrep < 1)
    y_error("strgrep illegal n= repeat count");
  if (keys[1]>=0 && !yarg_nil(keys[1])) {
    long sdims[Y_DIMSIZE];
    sub = ygeta_l(keys[1], 0, sdims);
    if (sdims[0]) {
      if (sdims[0] != 1)
        y_error("strgrep sub= not scalar or 1D list of subexpressions");
      nsub = sdims[1];
    }
    if (nsub > NSUBEXP)
      y_error("strgrep sub= list can contain at most 10 items");
    for (i=0 ; i<nsub ; i++) {
      if (sub[i]<0 || sub[i]>=NSUBEXP)
        y_error("strgrep sub= subexpression numbers run from 0 to 9");
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
      y_error(msg);
    }
    /* mxsub = regnpar_dm();   dont second guess, trust the user */
  }

  ntot = state.nrep*nsub;
  ys_dim(dims, ntot+ntot, iter.len);
  ys_initer2(&iter);
  state.out = ypush_l(dims);

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
