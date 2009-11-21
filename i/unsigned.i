/* unsigned.i
 * $Id: unsigned.i,v 1.1 2009-11-21 22:59:11 dhmunro Exp $
 * support for unsigned integers
 */

/* Supporting unsigned integers as distinct interpreted data types
 * in yorick would cause a large jump in the number of numeric types,
 * without (in my judgment) producing a significant gain in functionality.
 * Unsigned arithmetic in C actually has many unavoidable misfeatures,
 * which I do not want yorick to inherit.  For example, binary operators
 * implicitly promote signed integers to unsigned, which is a terrible
 * problem.  For example, in graphics code, the width of a window is
 * often unsigned, which makes it impossible to write a correct algebraic
 * expression for centering something in the window without an explicit
 * cast operation.  People have a tendency to use the unsigned type when
 * what they mean to do is to assert that a quantity is non-negative.
 * This is undoubtedly why width is unsigned.  But of course, what unsigned
 * really means is that the arithmetic operations are slightly different
 * than what you expect -- in particular, subtracting a larger number
 * has a very surprising result, and division is simply different (which
 * is why the centering formula doesn't work).
 *
 * This file contains implementations of all the operations which
 * differ between signed and unsigned arithmetic.  These are:
 *   u_gt(x,y)   x>y
 *   u_lt(x,y)   x<y
 *   u_ge(x,y)   x>=y
 *   u_le(x,y)   x<=y
 *   u_eq(x,y)   x==y
 *   u_ne(x,y)   x!=y
 *   u_shr(x,n)  x>>n
 *   u_div(n,d)  n/d
 *   u_mod(n,d)  n%d
 * Although the x==y and x!=y comparisons are the same for signed
 * and unsigned operands when x and y have the same type, they
 * differ when the types of the two operands differ, because of the
 * implicit cast operation.  The standalone cast operation is:
 *   u_cast(x, s)   x as an unsigned s (or unsigned x as an s)
 * For all other operations (x+y, x*y, x<<n, x!=y, etc.), the bit
 * pattern produced by the signed operation is identical to the bit
 * pattern produced by the unsigned operation.
 *
 * Note that char is unsigned by default in yorick, so these functions
 * do nothing different than the operators.  For completeness, a cast
 * operation is provided to interpret char as a signed quantity:
 *   s_char(x)  return signed char x as short
 *
 * To print unsigned integers:
 * 1. print in %x format, which is intrinsically unsigned
 * 2. if sizeof(structof(x))<sizeof(long), print u_cast(x,long)
 * 3. print highest decimal digit separately from the rest, e.g.-
 *    xhi = u_div(x,1000000000, xlo);  // if sizeof(long)==4
 *    xhi = swrite(format="%ld",xhi);
 *    list = where(xhi=="0");
 *    if (numberof(list)) xhi(list) = "";
 *    write,format="... %s%ld ...", ..., xhi,xlo, ...;
 */

func s_char(x)
/* DOCUMENT s_char(x)
 *   return short(x) interpreting char x as a signed integer.
 * SEE ALSO: u_cast
 */
{
  if (structof(x) != char) return short(x);
  x = short(x);
  return ((-(x>>7)) & (~0xffS)) | x;
}

func u_cast2(&x, &y)
{
  sx = structof(x);
  sy = structof(y);
  if (sx == sy) return sy;
  sr = structof(sx(0)+sy(0));
  if (sx == sr) {
    if (sy==char) b = '\0';
    else b = sy(1) << ((sizeof(sy)<<3)-1);
    b &= y;
    y = sr(y~b) + sr(~b) + sr(1);
  } else {
    if (sx==char) b = '\0';
    else b = sx(1) << ((sizeof(sx)<<3)-1);
    b &= x;
    x = sr(x~b) + sr(~b) + sr(1);
  }
  return sr;
}

func u_gt(x,y)
/* DOCUMENT u_gt(x,y)
 *   return x>y treating x and y as unsigned integers.
 * SEE ALSO: u_lt, u_ge, u_le, u_eq, u_ne, u_shr, u_div, u_mod, u_cast
 */
{
  z = u_cast2(x, y)(0);
  if (structof(0L+z) != long) return x>y;
  sx = (x < z);
  sy = (y < z);
  return ((x > y)&(sx==sy)) | (sx & !sy);
}

func u_lt(x,y)
/* DOCUMENT u_gt(x,y)
 *   return x<y treating x and y as unsigned integers.
 * SEE ALSO: u_gt, u_ge, u_le, u_eq, u_ne, u_shr, u_div, u_mod, u_cast
 */
{
  return u_gt(y,x);
}

func u_ge(x,y)
/* DOCUMENT u_gt(x,y)
 *   return x>=y treating x and y as unsigned integers.
 * SEE ALSO: u_gt, u_lt, u_le, u_eq, u_ne, u_shr, u_div, u_mod, u_cast
 */
{
  return !u_gt(y,x);
}

func u_le(x,y)
/* DOCUMENT u_gt(x,y)
 *   return x<=y treating x and y as unsigned integers.
 * SEE ALSO: u_gt, u_lt, u_ge, u_eq, u_ne, u_shr, u_div, u_mod, u_cast
 */
{
  return !u_gt(x,y);
}

func u_eq(x,y)
/* DOCUMENT u_eq(x,y)
 *   return x==y treating x and y as unsigned integers.
 * SEE ALSO: u_gt, u_lt, u_ge, u_ne, u_shr, u_div, u_mod, u_cast
 */
{
  u_cast2, x, y;
  return x==y;
}

func u_ne(x,y)
/* DOCUMENT u_ne(x,y)
 *   return x!=y treating x and y as unsigned integers.
 * SEE ALSO: u_gt, u_lt, u_ge, u_eq, u_shr, u_div, u_mod, u_cast
 */
{
  u_cast2, x, y;
  return x!=y;
}

func u_cast(x, sy)
/* DOCUMENT u_cast(x, structof(y))
 *       or u_cast(x, y)
 *   return x as an unsigned integer of type y.
 * SEE ALSO: u_gt, u_lt, u_ge, u_le, u_shr, u_div, u_mod, s_char
 */
{
  if (is_array(sy)) sy = structof(sy);
  sx = structof(x);
  if (sx == sy) return x;
  else if (sx == char) return sy(x);
  b = sx(1) << ((sizeof(sx)<<3)-1);
  b &= x;
  return sy(x~b) + sy(~b) + sy(1);
}

func u_shr(x,n)
/* DOCUMENT u_shr(x,n)
 *   return x>>n treating x as an unsigned integer.
 * SEE ALSO: u_gt, u_lt, u_ge, u_le, u_div, u_mod, u_cast
 */
{
  sx = structof(x);
  if (sx == char) return x >> n;
  b = sx(1) << ((sizeof(sx)<<3)-1);
  b &= x;
  return (sx(x~b)>>n) | ((sx(~b)>>n) + sx(1));
}

func u_mod(x,y,&q)
/* DOCUMENT u_mod(n,d)
 *       or u_mod(n,d, q)
 *   return n%d treating n and d as unsigned integers.
 *   The optional third argument Q is an output, set to n/d
 * SEE ALSO: u_gt, u_lt, u_ge, u_le, u_shr, u_div, u_cast
 */
{
  local r;
  q = u_div(x,y, r);
  return r;
}

func u_div(x,y,&r)
/* DOCUMENT u_div(n,d)
 *       or u_div(n,d, r)
 *   return n/d treating n and d as unsigned integers.
 *   The optional third argument R is an output, set to n%d
 * SEE ALSO: u_gt, u_lt, u_ge, u_le, u_shr, u_mod, u_cast
 */
{
  sr = u_cast2(x,y);
  if (structof(0L+sr(0))!=long || sr==char) {
    r = x%y;
    return x/y;
  }
  r = [];
  z = array(sr, dimsof(x,y));
  x += z;
  y += z;
  sx = (x < z);
  sy = (y < z);
  list = where(sy);
  if (numberof(list)) {   /* y<0 */
    yy = y(list);
    ra = x(list);
    qa = sr(!u_lt(ra,yy)); /* quotient=1 iff x>=y, else 0 */
    ra -= qa*yy;
  }
  list = where(!sy);
  if (numberof(list)) {
    x = x(list);
    y = y(list);
    sx = sx(list);
    list = where(sx);
    if (numberof(list)) { /* x<0, y>=0 */
      xx = x(list);
      yy = y(list);
      b = sr(1) << ((sizeof(sr)<<3)-1);
      xx ~= b;
      b = ~b;
      qb = xx/yy + b/yy + sr(1)/yy;
      rb = xx%yy + b%yy + sr(1)%yy;
      b = sr(u_ge(rb,yy));
      qb += b;
      rb -= b*yy;
    }
    list = where(!sx);
    if (numberof(list)) { /* x>=0, y>=0 */
      x = x(list);
      y = y(list);
      q = x/y;
      r = x%y;
    }
    q = merge(qb, q, sx);
    r = merge(rb, r, sx);
  }
  r = merge(ra, r, sy);
  return merge(qa, q, sy);
}
