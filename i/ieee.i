/*
 * $Id: ieee.i,v 1.1 2005-09-18 22:06:15 dhmunro Exp $
 * work out native numeric formats, provide ieee NaN and Inf tests
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

local ieee;
/* DOCUMENT ieee.i
     native_align, native_fix, native_flt, native_flim, native_dlim
       describe the binary formats of the native primitive numeric types
     as_chars(x)      -- gets/sets bits of x as char array
     ieee_test(x)     -- tests for ieee754 special values
     ieee_set(x,what) -- sets ieee754 special values
 */

local native_align;
/* DOCUMENT native_align =
     [char_align, short_align, int_align, long_align,
      float_align, double_align, pointer_align, struct_align]

    struct_align may be -1 for ppc/ibm interpretation
                        -2 for ppc/gcc interpretation
 */

local native_fix;
/* DOCUMENT native_fix = [short_format, int_format, long_format]
     format = [sizeof(type), order]
     order = 1 for big-endian (most significant byte first)
            -1 for little-endian (least significant byte first)
 */

local native_flt;
/* DOCUMENT native_flt = [float_format, double_format]
     format = [sizeof(type), order, sgn_addr, exp_addr, exp_size,
               man_addr, man_size, man_norm, exp_bias, denorm]
     order = 1 for big-endian (most significant byte first)
            -1 for little-endian (least significant byte first)
        (interpreted code here does not yet handle VAX middle-endian)
     addr, size are bit address (from msb), number of bits
     man_norm is 0 if mantissa 1-bit implied, 1 if 1-bit explicit
     exp_bias is the exponent field for 1.0
     denorm is 0 if there are no denormals
               1 if there are functioning denormals
               2 if denormals exist but are treated as NaNs
 */

local native_flim, native_dlim;
/* DOCUMENT native_flim = [big, tiny, eps]
     big is the biggest number
     tiny is the smallest non-denormal number
     eps is smallest number such that 1+eps != 1
 */

func as_chars(x, xnew)
/* DOCUMENT as_chars(x)
            as_chars, x, xnew
     return the bits of X as an array of char
     return value has leading dimension of sizeof(x(1)), otherwise
       same dimensions as X
     in second form, sets bits of X to char array XNEW
   SEE ALSO: ieee_test, ieee_set
 */
{
  size = sizeof(x)/numberof(x);
  if (size < 2) return x;
  dims = dimsof(x);
  dims = grow([dims(1)+1],dims);
  dims(2) = size;
  xc = [];
  reshape, xc, &x, char, dims;
  if (!am_subroutine()) x = xc;
  if (!is_void(xnew)) xc = xnew;
  reshape, xc;
  if (am_subroutine()) return;
  return x;
}

/* note: struct and func names are bogus --
 * intended to be overwritten later
 */

struct _numfmt2 { char x; char y; }
native_align = [sizeof(_numfmt2)-sizeof(char)];
struct _numfmt2 { char x; short y; }
grow, native_align, [sizeof(_numfmt2)-sizeof(short)];
struct _numfmt2 { char x; int y; }
grow, native_align, [sizeof(_numfmt2)-sizeof(int)];
struct _numfmt2 { char x; long y; }
grow, native_align, [sizeof(_numfmt2)-sizeof(long)];
struct _numfmt2 { char x; float y; }
grow, native_align, [sizeof(_numfmt2)-sizeof(float)];
struct _numfmt2 { char x; double y; }
grow, native_align, [sizeof(_numfmt2)-sizeof(double)];
struct _numfmt2 { char x; pointer y; }
grow, native_align, [sizeof(_numfmt2)-sizeof(pointer)];
struct _numfmt1 { char c; }
struct _numfmt2 { char x; _numfmt1 y; }
grow, native_align, [sizeof(_numfmt2)-sizeof(_numfmt1)];

struct _numfmt1 { double d; char c; }
struct _numfmt2 { char x; _numfmt1 y; }
native_fix = [sizeof(_numfmt1), sizeof(_numfmt2)];
struct _numfmt1 { double d(1); char c; }
grow, native_fix, [sizeof(_numfmt1)];
if (native_fix(1)!=sizeof(double)+native_align(6) &&
    native_fix(1)==2*sizeof(double)) {
  if (native_fix(2) == 2*sizeof(double)+native_align(6))
    native_align(8) = -1;
  else if (native_fix(2) == 3*sizeof(double) &&
           native_fix(3) == sizeof(double)+native_align(6))
    native_align(8) = -2;
}

func _numfmt1(type)   /* computes integer (fix point) order */
{
  x = type();
  size = sizeof(x);
  if (size<2 || structof(x+0)!=long) return 0;
  xc = [];
  reshape, xc, &x, char, [1,size];
  for (i=1 ; i<=size ; i++) xc(i) = i;
  reshape, xc;
  for (i=0 ; i<size ; i++) {
    y = (x>>(8*i)) & 0xff;
    if (y == 1) break;
  }
  if (i >= size) error, "this can't be happening?";
  if (2*i < size) {
    word = order = i+1;
    if (word == 1) order = -1;
  } else {
    word = order = size-i;
    if (word != 1) order = -word;
  }
  if (word != 1) error, "middle endian integers (previously) unknown!";
  return order;
}

native_fix = [[sizeof(short),_numfmt1(short)],
              [sizeof(int),_numfmt1(int)],
              [sizeof(long),_numfmt1(long)]];

func _numfmt1(aa, bb, msb)   /* returns bit address where aa!=bb */
{
  if (is_void(msb)) msb = 1;
  size = sizeof(aa);
  if (size<2) {
    a = aa;
    b = bb;
  } else {
    reshape, a, &aa, char, [1,size];
    reshape, b, &bb, char, [1,size];
  }
  i = where(a != b);
  diff = (a ~ b) & 0xff;
  reshape, a;
  reshape, b;
  if (!numberof(i)) return -1;
  i = i(msb);
  diff = diff(i);
  addr = msb? 8*(i-1) : 8*(size-i);
  for (mask=0x80 ; !(mask&diff) ; mask>>=1) addr++;
  return addr;
}

func _numfmt2(type, &epsilon)   /* analyzes floating point type */
{
  x = type();
  size = sizeof(x);
  if (structof(x+0)==long || structof(x+0.)!=double) return 0;

  /* 1.0, 1.5, 1.5+0.5/256., ... 1.5+0.5/256.^n
   * each term differs from previous by one bit, and the
   * bit moves by one byte toward lower significance each time
   */
  one = type(1.0);
  p256 = array(type, size);
  for (i=1,s=0.5 ; i<=size ; i++,s*=1./256.) p256(i) = s;
  p256c = one+p256(cum);
  addr = array(0, size);
  for (i=1 ; i<=size ; i++)
    addr(i) = _numfmt1(p256c(i), p256c(i+1));
  len = where(addr>=0)(0);
  step = addr(dif:1:len);
  if (anyof(step(dif))) error, "can't handle VAX float format yet";
  if (step(1)==8) order = 1;
  else if (step(1)==-8) order = -1;
  else error, "unknown floating point format";

  if (order > 0) msb = 1;
  else msb = 0;

  /* sign address is where 1.0 differs from -1.0 */
  sgn_addr = _numfmt1(type(1.0), type(-1.0), msb);

  /* exponent address is where 2.0^96 differs from 0.5^96 */
  exp_addr = _numfmt1(type(65536.^6), type((1.0/65536.)^6), msb);

  /* mantissa address is where 1.0 differs from 1.5 */
  man_addr = _numfmt1(type(1.0), type(1.5), msb);

  /* mantissa size p256c(len+1) is last different 1.5+0.5/256.^n
   * scan down in steps of 0.5 to find epsilon */
  base = p256c(len+1);
  epsilon = p256(len);
  man_size = addr(1)-addr(len)+1;
  for (i=1 ; i<=8 ; epsilon*=type(0.5),man_size++,i++)
    if (_numfmt1(base, base+type(0.5)*epsilon, msb) < 0) break;

  /* mantissa normalization: check whether bit before man_addr
   * is always set in series 1.5*2.0^n, -1.75*1.5*2.0^n, n=0 to 15 */
  i = (man_addr-1)%8;
  mask = 0x80 >> i;
  i = (man_addr-1)/8;
  if (order < 0) i = size-i;
  else i++;
  norm = type(1.5)*type(2.0)^indgen(0:15);
  grow, norm, type(-1.75)*norm;
  norm = as_chars(norm) & mask;
  man_norm = allof(norm);
  if (man_norm) {
    man_addr--;
    man_size++;
  }

  /* exponent size is where 1.0 and 2.0 differ, scanned from lsb */
  test = as_chars(type(1.0)) ~ as_chars(type(2.0));
  if (order < 0) test = test(::-1);
  a = where(test)(0);
  test = test(a);
  for (i=0 ; i<8 ; i++) if ((1<<i)&test) break;
  a = 8*(a-1) + 7-i;
  exp_size = a - exp_addr + 1;

  /* exponent bias is exponent for 1.0 */
  one = as_chars(type(1.0));
  if (order < 0) one = one(::-1);
  i1 = a/8 + 1;
  i0 = exp_addr/8 + 1;
  one(i0) &= (1<<(8-exp_addr%8))-1;
  s = 7-(a%8);
  bias = long(one(i1)) >> s;
  for (i=i1-1,s=8-s ; i>=i0 ; i--,s+=8) { bias |= long(one(i))<<s; }

  /* try to decide if this is ieee754 type
   * idea is to look at whether denormals exist */
  denorm = 0;
  if (!man_norm) {
    small = [type(0.)];
    c = array(char, size);
    c(i1) = 1<<(7-(a%8));
    if (order < 0) c = c(::-1);
    as_chars, small, c;
    denorm = (type(0.25)*small(1)) != 0.;
    if (!denorm && (type(0.5)*small(1)) == 0.) denorm = 2;
  }

  return [size, order, sgn_addr, exp_addr, exp_size,
          man_addr, man_size, man_norm, bias, denorm];
}

native_flt = [_numfmt2(float, native_flim), _numfmt2(double, native_dlim)];

func _numfmt2(type)
{
  if (type==float) fmt = native_flt(,1);
  else if (type==double) fmt = native_flt(,2);

  sgn_addr = fmt(3);
  exp_addr = fmt(4);
  exp_size = fmt(5);
  man_addr = fmt(6);
  man_size = fmt(7);
  man_norm = fmt(8);

  c = array(char, fmt(1), 2);
  if (fmt(2) < 0) c = c(::-1,..);

  a = exp_size+exp_addr-1;
  if (fmt(10)) a--;
  i1 = a/8 + 1;
  i0 = exp_addr/8 + 1;
  mask = (1<<(8-(exp_addr%8)))-1;
  if (i1==i0) {
    mask &= ~((1<<(7-(a%8)))-1);
  } else {
    c(i1,1) |= ~((1<<(7-(a%8)))-1);
  }
  c(i0,1) |= mask;
  for (i=i0+1 ; i<i1 ; i++) c(i,1) |= 0xff;

  a = man_size+man_addr-1;
  i1 = a/8 + 1;
  i0 = man_addr/8 + 1;
  mask = (1<<(8-(man_addr%8)))-1;
  if (i1==i0) {
    mask &= ~((1<<(7-(a%8)))-1);
  } else {
    c(i1,1) |= ~((1<<(7-(a%8)))-1);
  }
  c(i0,1) |= mask;
  for (i=i0+1 ; i<i1 ; i++) c(i,1) |= 0xff;

  if (man_norm) {
    i = man_addr/8 + 1;
    mask = 1 << (7-man_addr%8);
    c(i,2) |= mask;
  }
  if (fmt(10) || !man_norm) {
    a = exp_size+exp_addr-1;
    i = a/8 + 1;
    mask = 1 << (7-a%8);
    c(i,2) |= mask;
  }

  if (fmt(2) < 0) c = c(::-1,..);
  x = array(type, 2);
  as_chars, x, c;
  return x;
}

native_flim = grow(_numfmt2(float), native_flim);
native_dlim = grow(_numfmt2(double), native_dlim);

_numfmt1 = _numfmt2 = [];

func ieee_test(x)
/* DOCUMENT ieee_test(x)
     return values:
     0 if this is an ordinary number
     -1 if this is -Inf
     1 if this is Inf
     2 if this is qNaN
     3 if this is sNaN
     4 if this is a denormal
     5 if this is a denormal which will be treated as NaN

   Warning-- apparently there is no universal standard for what
     constitutes signalling versus quiet NaN
     on MIPS and HPPA architectures, qNaN and sNaN are reversed

   SEE ALSO: ieee_set, as_chars
 */
{
  if (structof(x)==float) fmt = native_flt(,1);
  else if (structof(x)==double) fmt = native_flt(,2);
  else error, "can only test float or double arrays";
  if (!fmt(10)) return array(0n, dimsof(x));

  x = as_chars(x);

  if (fmt(2) < 0) x = x(::-1,..);
  sgn_addr = fmt(3);
  exp_addr = fmt(4);
  exp_size = fmt(5);
  man_addr = fmt(6);
  man_size = fmt(7);

  i0 = sgn_addr/8+1;
  i1 = 7-sgn_addr%8;
  p = int(x(i0,..)>>i1) & 1n;  /* sign bit */
  x(i0,..) &= ~(1<<i1);
  z = !x(sum,..);

  a = exp_size+exp_addr-1;
  i1 = a/8 + 1;
  i0 = exp_addr/8 + 1;
  s = 7-(a%8);
  e = long(x(i1,..)) >> s;
  for (i=i1-1,s=8-s ; i>=i0 ; i--,s+=8) { e |= long(x(i,..))<<s; }
  mask = (1<<exp_size) - 1;
  e &= mask;

  d = !e & !z;    /* denormals */
  i = (e==mask);  /* inf and nan */

  i0 = man_addr/8+1;
  i1 = 7-man_addr%8;
  q = int(x(i0,..)>>i1) & 1n;  /* first mantissa bit */

  a = man_size+man_addr-1;
  i2 = a/8+1;
  x = x(i0:i2,..);
  x(1,..) &= (1<<(1+i1))-1;
  x(0,..) &= ~((1<<(7-a%8))-1);
  n = i & (x(sum,..)!=0);
  q &= n;
  i &= !n;

  if (dimsof(i)(1)==0) {
    scalar = 1;
    x = [i];
  } else {
    x = i;
  }
  list = where(p & i);
  if (numberof(list)) x(list) = -1;
  list = where(q);
  if (numberof(list)) x(list) = 2;
  list = where(n & !q);
  if (numberof(list)) x(list) = 3;
  list = where(d);
  if (numberof(list)) x(list) = (fmt(10)==2? 5 : 4);
  if (scalar) x = x(1);

  return x;
}

func ieee_set(x, what)
/* DOCUMENT ieee_set, x, what
     set X to ieee754 special value WHAT
     X must be an array of float or double values
       (note that X cannot be a scalar double value)
     WHAT = 0  means leave unchanged
     WHAT = 1  means set to Inf
     WHAT = 2  means set to qNaN
     WHAT = 3  means set to sNaN
     WHAT = 4  means set to 0.0
       negate WHAT to set the sign bit of X as well

     WHAT may be an array conformable with X, in order to set only
     some values of X

     this routine is a no-op if this machine is not known to
     support these ieee754 special values

   Warning-- apparently there is no universal standard for what
     constitutes signalling versus quiet NaN
     on MIPS and HPPA architectures, qNaN and sNaN are reversed

   SEE ALSO: ieee_test, as_chars
 */
{
  if (structof(x)==float) fmt = native_flt(,1);
  else if (structof(x)==double) fmt = native_flt(,2);
  else error, "can only test float or double arrays";
  if (!fmt(10) || noneof(what)) return;
  sgn_addr = fmt(3);
  exp_addr = fmt(4);
  exp_size = fmt(5);
  man_addr = fmt(6);
  man_size = fmt(7);

  dims = dimsof(x);
  what += array(0, dims);
  s = what < 0;
  what = abs(what);
  c = as_chars(x);
  if (fmt(2) < 0) c = c(::-1,..);

  list = where((what>=1) & (what<=5));
  if (numberof(list)) c(,list) = 0;

  list = where((what>=1) & (what<=3));
  if (numberof(list)) {
    a = exp_size+exp_addr-1;
    i1 = a/8 + 1;
    i0 = exp_addr/8 + 1;
    mask = (1<<(8-(exp_addr%8)))-1;
    if (i1==i0) {
      mask &= ~((1<<(7-(a%8)))-1);
    } else {
      c(i1,list) |= ~((1<<(7-(a%8)))-1);
    }
    c(i0,list) |= mask;
    for (i=i0+1 ; i<i1 ; i++) c(i,list) |= 0xff;

    list = where(what==2);
    if (numberof(list)) {
      i = man_addr/8 + 1;
      mask = 1 << (7-man_addr%8);
      c(i,list) |= mask;
    }

    list = where(what==3);
    if (numberof(list)) {
      a = man_size+man_addr-1;
      i1 = a/8 + 1;
      i0 = (man_addr+1)/8 + 1;
      mask = (1<<(8-((man_addr+1)%8)))-1;
      if (i1==i0) {
        mask &= ~((1<<(7-(a%8)))-1);
      } else {
        c(i1,list) |= ~((1<<(7-(a%8)))-1);
      }
      c(i0,list) |= mask;
      for (i=i0+1 ; i<i1 ; i++) c(i,list) |= 0xff;
    }
  }

  list = where(s);
  if (numberof(list)) {
    i = sgn_addr/8 + 1;
    mask = 1 << (7-sgn_addr%8);
    c(i,list) |= mask;
  }

  if (fmt(2) < 0) c = c(::-1,..);
  as_chars, x, c;
}
