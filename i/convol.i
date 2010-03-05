/*
 * $Id: convol.i,v 1.3 2010-03-05 04:13:56 dhmunro Exp $
 * convolution of two vectors using fft
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func convol(a,b,n0=,n1=)
/* DOCUMENT convol(a,b)

     returns convolution of vector a with vector b, a vector
     of length na+nb-1 where na=numberof(a), nb=numberof(b).

     In detail, for i=[1 to na+nb-1]
       result(i) = sum j=[max(1,1+i-nb) to min(na,i)] (a(j)*b(1+i-j))

     The n0= and n1= keywords can be used to control the section of
     the full array that is actually returned, 1<=n0<n1<=na+nb-1.

     Use the convoln function for multi-dimensional convolution, and
     see gaussm and boxcar for smoothing array data.

   SEE ALSO: fft_good, fft, convoln, gaussm, boxcar
 */
{
  na= numberof(a);
  nb= numberof(b);
  nc= na+nb-1;
  nt= fft_good(nc);
  at= bt= array(0i, nt);
  at(1:na)= a;
  bt(1:nb)= b;
  work= fft_setup(dimsof(at));
  fft_inplace, at, 1, setup=work;
  fft_inplace, bt, 1, setup=work;
  c= at*bt;
  fft_inplace, c, -1, setup=work;
  if (is_void(n0)) n0= 1;
  if (is_void(n1)) n1= nc;
  return double(c(n0:n1))/nt;
}

func fft_good(n)
/* DOCUMENT fft_good(n)

     returns the smallest number of the form 2^x*3^y*5^z greater
     than or equal to n.  An fft of this length will be much faster
     than a number with larger prime factors; the speed difference
     can be an order of magnitude or more.

     For n>100, the worst cases result in a little over a 11% increase
     in n; for n>1000, the worst are a bit over 6%; still larger n are
     better yet.  The median increase for n<=10000 is about 1.5%.

   SEE ALSO: fft, fft_setup, convol
 */
{
  if (n<7) return max(n,1);
  logn= log(n);
  n5= 5.^indgen(0:long(logn/log(5.) + 1.e-6));  /* exact integers */
  n3= 3.^indgen(0:long(logn/log(3.) + 1.e-6));  /* exact integers */
  n35= n3*n5(-,);             /* fewer than 300 numbers for n<5e9 */
  n35= n35(where(n35<=n));
  n235= 2^long((logn-log(n35))/log(2.) + 0.999999) * long(n35);
  return min(n235);
}

/*
func convol_check(a,b)
{
  na= numberof(a);  nb= numberof(b);  c= array(0.,na+nb-1);
  for (i=1 ; i<na+nb ; i++)
    for (j=max(1,1+i-nb) ; j<=min(na,i) ; j++) c(i)+= a(j)*b(1+i-j);
  return c;
}
*/

func convoln(a, b, n0=, n1=, zc=)
/* DOCUMENT convoln(a, b)

     returns convolution of array A with array B.  This is naturally
     of length na+nb-1 where na=length of A, nb=length of B.  However,
     convoln returns a result the same size as A, which is extracted
     from the center of this full array.  Typically, B will be a much
     smaller array than A, which you are using to smooth A.  If the
     dimensions of B are odd, then the elements of the returned array
     will be centered as you would expect.

     In detail, for i=[1 to na+nb-1]
       result(i) = sum j=[max(1,1+i-nb) to min(na,i)] (A(j)*B(1+i-j))
     with this operation repeated along each dimension of A and B.

     The n0= and n1= keywords can be used to control the section of
     the full array that is actually returned, 1<=n0<n1<=na+nb-1.
     Both n0 and n1 must be vectors with length equal to the number
     of dimensions of A and B.  By default,
       n0 = (1+nb)/2
       n1 = na + n0 - 1
     which gives the expected centering when nb is odd.  If nb is
     even, you might want to consider returning a result of length
     na-1 which is "zone centered" relative to A.  You can do this
     by setting the zc=1 keyword, which gives n0 = 1 + nb/2 and
     n1 = n1 = na + n0 - 2 when nb is even.

     Note that if some dimension of B is length 1, there will be
     no smoothing along that dimension of A.  The pseudo-index -
     is useful for generating such dimensions.  For example, if x
     is a 2D array and y is a 1D smoothing function which you want
     to apply to the second dimension of x, use convoln(x, y(-,)).

   SEE ALSO: convol, gaussm, boxcar
 */
{
  na = dimsof(a);
  nb = dimsof(b);
  ndims = nb(1);
  if (na(1) != ndims)
    error, "a and b must have same numberof of dimensions";
  na = na(2:);
  nb = nb(2:);
  nc = na+nb-1;
  nt = array(ndims, 1+ndims);
  for (i=1 ; i<=ndims ; ++i) nt(1+i) = fft_good(nc(i));
  at = bt = array(complex, nt);
  nt = nt(2:);

  ia = indgen(0:na(ndims)-1);
  ib = indgen(0:nb(ndims)-1);
  for (i=ndims-1 ; i>=1 ; --i) {
    ia = nt(i)*ia(-,..) + indgen(0:na(i)-1);
    ib = nt(i)*ib(-,..) + indgen(0:nb(i)-1);
  }
  ia += 1;
  ib += 1;
  at(ia) = a;
  bt(ib) = b;
  ia = ib = [];

  work = fft_setup(dimsof(at));
  fft_inplace, at, 1, setup=work;
  fft_inplace, bt, 1, setup=work;
  at *= bt;
  bt = [];
  fft_inplace, at, -1, setup=work;

  if (is_void(n0)) n0 = (1+nb)/2;
  if (is_void(n1)) n1 = na + n0 - 1;
  if (numberof(n0)!=ndims || numberof(n1)!=ndims)
    error, "n0=, n1= keywords must have length = number of dimensions";
  if (zc) n0 += (1+nb)%2;
  ia = indgen(n0(ndims)-1:n1(ndims)-1);
  for (i=ndims-1 ; i>=1 ; --i)
    ia = nt(i)*ia(-,..) + indgen(n0(i)-1:n1(i)-1);
  ia += 1;
  nt = numberof(at);
  return double(at(ia)) / nt;
}

func gaussm(a, n, fwhm=)
/* DOCUMENT gaussm(a, n)

     returns array A smoothed by a Gaussian with a sigma of N pixels.
     If A is multi-dimensional, N may be a vector with as many
     components as A has dimensions, specifying how many pixels
     to smooth in that dimension.  N may be shorter than the number
     of dimensions of A; unspecified dimensions are unsmoothed
     (as if N were 0.0).  If A is multi-dimensional and N is scalar,
     that N is applied to all dimensions.

     With the fwhm=1 keyword, N is the full width at half maximum
     of the Guassian.  The fwhm= keyword may also be a vector of
     the same length as N, 1 where N is to be interpreted as a FWHM
     and 0 where N is to be interpreted as a sigma.

   SEE ALSO: convoln, boxcar
 */
{
  rank = dimsof(a)(1);
  n = abs(double(n));
  if (numberof(fwhm) <= 1) {
    if (!is_void(fwhm) && fwhm(1)) n *= sqrt(0.125/log(2.));
  } else if (numberof(fhwm) == numberof(n)) {
    list = where(fwhm);
    if (numberof(list)) n(list) *= sqrt(0.125/log(2.));
  } else {
    error, "fwhm= keyword must be scalar or same length as sigma";
  }
  if (!dimsof(n)(1)) n = array(n, rank);
  else if (numberof(n) < rank) grow, n, array(0., rank-numberof(n));
  else if (numberof(n) > rank) error, "length of sigma exceeds rank of a";

  rsig = 1.0/(n + (n<0.1));  /* rsig multiplied by 0 if n<0.1 */
  n = max(long(4.*n), n>=0.1);
  g = (rsig(rank)*indgen(-n(rank):n(rank)))^2;
  for (i=rank-1 ; i>=1 ; --i) g = g(-,..) + (rsig(i)*indgen(-n(i):n(i)))^2;
  g = exp(-0.5*g);
  g *= 1./sum(g);

  return convoln(a, g);
}

func boxcar(a, n)
/* DOCUMENT boxcar(a, n)

     returns array A smoothed by a boxcar of 2*N+1 pixels.
     If A is multi-dimensional, N may be a vector with as many
     components as A has dimensions, specifying how many pixels
     to smooth in that dimension.  N may be shorter than the number
     of dimensions of A; unspecified dimensions are unsmoothed
     (as if N were 0).  If A is multi-dimensional and N is scalar,
     that N is applied to all dimensions.

     Each pixel of the result is averaged with the N pixels to
     its left and N pixels to its right (so N=0 means no averaging).
     For pixels less than N from the left edge, the averaging includes
     fewer pixels on the left, but still N on the right, and similarly
     for pixels less than N from the right edge.  Hence, the effective
     smoothing is reduced and the centering is skewed, near the edges
     of the array.

   SEE ALSO: convoln, gaussm
 */
{
  na = dimsof(a);
  rank = na(1);
  if (!rank) return a;
  na = na(2:);
  if (structof(n+0)!=long || anyof(n<0)) error, "n must be positive integer";
  if (numberof(n) > rank) error, "length of n exceeds rank of a";
  else if (!dimsof(n)(1)) n = array(n, rank);
  else if (numberof(n) < rank) rank = numberof(n);
  if (rank > 6) error, "only first six dimensions can be smoothed";
  n = min(na(1:rank)/2, n);

  local n0, n1;
  i = 0;
  if (rank > i++) {
    x = a(cum,..);
    a = _boxcar_(i)*(x(n1,..) - x(n0,..));
    if (rank > i++) {
      x = a(,cum,..);
      a = _boxcar_(i)(-,)*(x(,n1,..) - x(,n0,..));
      if (rank > i++) {
        x = a(,,cum,..);
        a = _boxcar_(i)(-,-,)*(x(,,n1,..) - x(,,n0,..));
        if (rank > i++) {
          x = a(,,,cum,..);
          a = _boxcar_(i)(-,-,-,)*(x(,,,n1,..) - x(,,,n0,..));
          if (rank > i++) {
            x = a(,,,,cum,..);
            a = _boxcar_(i)(-,-,-,-,)*(x(,,,,n1,..) - x(,,,,n0,..));
            if (rank > i++) {
              x = a(,,,,,cum,..);
              a = _boxcar_(i)(-,-,-,-,-,)*(x(,,,,,n1,..) - x(,,,,,n0,..));
            }
          }
        }
      }
    }
  }
  return a;
}

/* worker for boxcar */
func _boxcar_(i)
{
  extern n0, n1;
  ni = n(i);
  m = na(i);
  list = indgen(m);
  n0 = max(list-ni, 1);
  n1 = min(list+ni, m) + 1;
  return 1./(n1-n0);
}
