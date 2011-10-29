/*
 * $Id: fft.i,v 1.1 2005-09-18 22:05:23 dhmunro Exp $
 * Yorick interface to Swarztrauber FFT routines, modified for strides.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*= SECTION() Fast Fourier Transform functions =============================*/

/* ------------------------------------------------------------------------ */

func fft(x, ljdir, rjdir, setup=)
/* DOCUMENT fft(x, direction)
            fft(x, ljdir, rjdir)
         or fft(x, ljdir, rjdir, setup=workspace)
     returns the complex Fast Fourier Transform of array X.
     The DIRECTION determines which direction the transform is in --
     e.g.- from time to frequency or vice-versa -- as follows:

     DIRECTION    meaning
     ---------    -------
         1        "forward" transform (coefficients of exp(+i * 2*pi*kl/N))
                  on every dimension of X
        -1        "backward" transform (coefficients of exp(-i * 2*pi*kl/N))
                  on every dimension of X
     [1,-1,1]     forward transform on first and third dimensions of X,
                  backward transform on second dimension of X (any other
                  dimensions remain untransformed)
     [-1,0,0,1]   backward transform on first dimension of X, forward
                  transform on fourth dimension of X
        etc.

     The third positional argument, if present, allows the direction
     of dimensions of X to be specified relative to the final dimension
     of X, instead of relative to the first dimension of X.  In this
     case, both LJDIR and RJDIR must be vectors of integers -- the
     scalar form is illegal:

        LJDIR    RJDIR      meaning
        -----    -----      -------
        []        [1]       forward transform last dimension of X
        [1]        []       forward transform first dimension of X
        []        [-1,-1]   backward transform last two dimensions of X,
                            leaving any other dimensions untransformed
     [-1,0,0,1]    []       backward transform on first dimension of X,
                            forward transform on fourth dimension of X
        []      [-1,0,0,1]  backward transform on 4th to last dimension of X,
                            forward transform on last dimension of X
        etc.

     Note that the final element of RJDIR corresponds to the last dimension
     of X, while the initial element of LJDIR corresponds to the first
     dimension of X.

     The explicit meaning of "forward" transform -- the coefficients of
     exp(+i * 2*pi*kl/N) -- is:

     result for j=1,...,n

                result(j)=the sum from k=1,...,n of

                      x(k)*exp(-i*(j-1)*(k-1)*2*pi/n)

                            where i=sqrt(-1)

     Note that the result is unnormalized.  Applying the "backward"
     transform to the result of a "forward" transform returns N times
     the original vector of length N.  Equivalently, applying either
     the "forward" or "backward" transform four times in succession
     yields N^2 times the original vector of length N.

     Performing the transform requires some WORKSPACE, which can be
     set up beforehand by calling fft_setup, if fft is to be called
     more than once with arrays X of the same shape.  If no setup
     keyword argument is supplied, the workspace allocation and setup
     must be repeated for each call.

   SEE ALSO: roll, fft_setup, fft_inplace
 */
{
  result= complex(x);
  fft_inplace, result, ljdir, rjdir, setup=setup;
  return result;
}
errs2caller, fft;

func fft_inplace(x, ljdir, rjdir, setup=)
/* DOCUMENT fft_inplace, x, direction
         or fft_inplace, x, ljdir, rjdir
         or fft_inplace, x, ljdir, rjdir, setup=workspace
     is the same as the fft function, except that the transform is
     performed "in_place" on the array X, which must be of type complex.
   SEE ALSO: fft, fft_setup
 */
{
  if (structof(x)!=complex) error, "expecting complex argument";

  /* form list of dimension lengths (dims) and corresponding list of
     transform directions (dirs) */
  dims= dimsof(x);
  ndims= dims(1);
  if (ndims<1) return;
  dirs= fft_dirs(ndims, ljdir, rjdir);
  list= where(dirs);  /* list of dimensions to be transformed */
  n= numberof(list);
  if (!n) return;

  /* setup the workspace now if it hasn't already been done */
  if (is_void(setup)) setup= fft_setup(dims, dirs);

  /* get the strides associated with each dimension */
  dims= dims(2:0);
  stds= array(1, ndims);
  for (i=1 ; i<ndims ; i++) stds(i+1)= stds(i)*dims(i);
  tops= numberof(x)/(stds*dims);

  /* do the requested transforms */
  nlist= *setup(1);
  setup= *setup(2);
  if (is_void(setup)) error, "no workspace for any dimension length";
  for (i=1 ; i<=n ; i++) {
    j= list(i);
    ndims= dims(j);  /* clobbers original ndims */
    ws= setup(where(nlist==ndims)(1));
    if (is_void(ws)) error, "no workspace for dimension length "+pr1(ndims);
    fft_raw, dirs(j), x, stds(j), ndims, tops(j), ws;
  }
}
errs2caller, fft_inplace;

func fft_setup(dims, ljdir, rjdir)
/* DOCUMENT workspace= fft_setup(dimsof(x))
         or workspace= fft_setup(dimsof(x), direction)
         or workspace= fft_setup(dimsof(x), ljdir, rjdir)
     allocates and sets up the workspace for a subsequent call to
            fft(X, DIRECTION, setup=WORKSPACE)
     or
            fft(X, LJDIR, RJDIR, setup=WORKSPACE)
     The DIRECTION or LJDIR, RJDIR arguments compute WORKSPACE only for
     the dimensions which will actually be transformed.  If only the
     dimsof(x) argument is supplied, then WORKSPACE will be enough to
     transform any or all dimensions of X.  With DIRECTION or LJDIR, RJDIR
     supplied, WORKSPACE will only be enough to compute the dimensions
     which are actually to be transformed.  The WORKSPACE does not
     depend on the sign of any element in the DIRECTION (or LJDIR, RJDIR),
     so you can use the same WORKSPACE for both "forward" and "backward"
     transforms.

     Furthermore, as long as the length of any dimensions of the array
     X to be transformed are present in WORKSPACE, it may be used in
     a call to fft with the array.  Thus, if X were a 25-by-64 array,
     and Y were a 64-vector, the following sequence is legal:
          ws= fft_setup(dimsof(x));
          xf= fft(x, 1, setup=ws);
          yf= fft(y, -1, setup=ws);

     The WORKSPACE required for a dimension of length N is 6*N+15 doubles.

   SEE ALSO: fft, dimsof, fft_inplace
 */
{
  ndims= dims(1);
  if (ndims<1) return array(pointer, 2);
  dirs= fft_dirs(ndims, ljdir, rjdir);
  list= where(dirs);  /* list of dimensions to be transformed */
  n= numberof(list);
  if (!n) return array(pointer, 2);
  dims= dims(1+list);
  if (n>1) {
    dims= dims(sort(dims));
    list= grow([0],dims)(dif);
    /* eliminate duplicate dimension lengths */
    dims= dims(where(list));
    n= numberof(dims);
  }
  setup= array(pointer, n);
  for (i=1 ; i<=n ; i++) {
    ndims= dims(i);
    ws= array(0.0, 6*ndims+15);
    fft_init, ndims, ws;
    setup(i)= &ws;
  }
  return [&dims, &setup];
}

func fft_dirs(ndims, ljdir, rjdir)
{
  nolj= is_void(ljdir);
  norj= is_void(rjdir);
  if (nolj && norj) return array(1, ndims);  /* mostly for fft_setup --
                                                also makes fft(x) work */
  if (nolj || dimsof(ljdir)(1)) {
    /* detailed direction list present */
    dirs= array(0, ndims);          /* all 0 initially */
    if (!nolj) {                    /* fill in left-justified directions */
      list= where(ljdir);
      if (numberof(list)) dirs(list)= ljdir(list);
    }
    if (!norj) {                    /* fill in right-justified directions */
      list= where(rjdir);
      if (numberof(list)) dirs(list-numberof(rjdir) + ndims)= rjdir(list);
    }
  } else {
    /* all dimensions get the same direction */
    dirs= array(ljdir, ndims);
  }
  return dirs;
}

extern fft_init;
/* PROTOTYPE
   void cffti(long n, double array ws)
 */
/* DOCUMENT fft_init, n, wsave
     Swarztrauber's cffti.  This actually requires wsave=array(0.0, 4*n+15),
     instead of the 6*n+15 doubles of storage used by fft_raw to handle the
     possibility of multidimensional arrays.  If the storage matters, you
     can call cfftf and/or cfftb as the Yorick functions fft_fraw and/or
     fft_braw.
 */

extern fft_fraw;
/* PROTOTYPE
   void cfftf(long n, complex array c, double array ws)
 */
/* DOCUMENT fft_fraw, n, c, wsave
     Swarztrauber's cfftf.  You can use this to avoid the additional
     2*N storage incurred by fft_setup.
 */

extern fft_braw;
/* PROTOTYPE
   void cfftb(long n, complex array c, double array ws)
 */
/* DOCUMENT fft_braw, n, c, wsave
     Swarztrauber's cfftb.  You can use this to avoid the additional
     2*N storage incurred by fft_setup.
 */

extern fft_raw;
/* PROTOTYPE
   void cfft2(long idir, complex array c, long istd, long n, long n2,
              pointer ws)
 */

/* ------------------------------------------------------------------------ */

func roll(x, ljoff, rjoff)
/* DOCUMENT roll(x, ljoff, rjoff)
         or roll, x, ljoff, rjoff
         or roll(x)
         or roll, x

     "rolls" selected dimensions of the array X.  The roll offsets
     LJOFF and RJOFF (both optional) work in the same fashion as the
     LJDIR and RJDIR arguments to the fft function:

        A scalar LJDIR (and nil RJDIR) rolls all dimensions of X by
        the specified offset.
        Otherwise, the elements of the LJDIR vector [ljoff1, ljoff2, ...]
        are used as the roll offsets for the first, second, etc.
        dimensions of X.
        Similarly, the elements of the RJDIR vector [..., rjoff1, rjoff0]
        are matched to the final dimensions of X, so the next to last
        dimension is rolled by rjoff1 and the last dimension by rjoff0.

        As a special case (mostly for use with the fft function), if
        both LJDIR and RJDIR are nil, every dimension is rolled by
        half of its length.  Thus,
           roll(x)
        it equivalent to
           roll(x, dimsof(x)(2:0)/2)

     The result of the roll function is complex if X is complex, otherwise
     double (i.e.- any other array type is promoted to type double).  If
     roll is invoked as a subroutine, the operation is performed in place.

   SEE ALSO: fft
 */
{
  /* get array to be transformed */
  if (structof(x)==complex) {
    stds1= 2;
    if (!am_subroutine()) x= complex(x);  /* copy to avoid clobbering */
  } else {
    stds1= 1;
    if (!am_subroutine()) x= double(x);  /* copy to avoid clobbering */
    else if (structof(x)!=double)
      error, "in-place roll needs double or complex argument";
  }
  dims= dimsof(x);
  ndims= dims(1);
  n= numberof(x);
  if (ndims<1 || n<2) return x;
  dims= dims(2:0);

  /* get offset arguments */
  if (is_void(ljoff) && is_void(rjoff)) ljoff= dims/2;
  else ljoff= fft_dirs(ndims, ljoff, rjoff);
  ljoff%= dims;
  if (noneof(ljoff)) return x;

  /* get strides */
  stds= array(stds1, ndims);   /* note extra factor of 2 for complex */
  for (i=1 ; i<ndims ; i++) stds(i+1)= stds(i)*dims(i);
  tops= (stds1*n)/(stds*dims);

  /* perform roll operation */
  px= &x;   /* avoids complex<-->double type conversion problems */
  ws= array(0.0, max(dims(where(ljoff))));
  for (i=1 ; i<=ndims ; i++)
    _roll2, px, ljoff(i), stds(i), dims(i), tops(i), ws;

  return x;
}

extern _roll2;
/* PROTOTYPE
   void roll2(pointer a, long ioff, long istd, long n, long n2,
              double array ws)
 */
