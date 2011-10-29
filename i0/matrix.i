/*
 * $Id: matrix.i,v 1.2 2010-08-22 17:44:05 dhmunro Exp $
 * Yorick interface to LAPACK matrix solving routines.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*= SECTION() LAPACK linear algebra functions ==============================*/

/* ------------------------------------------------------------------------ */

/* Note: Aug/2010
   prototypes directly linking to LAPACK/BLAS functions removed because
   they cause severe linking problems when multiple yorick packages also
   link against different versions of these libraries:
   dgtsv dgesv dgetrf dgecox dgelx dgelss dgesvx
*/

func unit(n, m)
/* DOCUMENT unit(n)
         or unit(n, m)
     returns n-by-n (or n-by-m) unit matrix, i.e.- matrix with diagonal
     elements all 1.0, off diagonal elements 0.0
 */
{
  if (is_void(m)) m= n;
  u= array(0.0, n, m);
  u(1:numberof(u):n+1)= 1.0;
  return u;
}

/* ------------------------------------------------------------------------ */

func TDsolve(c, d, e, b, which=)
/* DOCUMENT TDsolve(c, d, e, b)
         or TDsolve(c, d, e, b, which=which)

     returns the solution to the tridiagonal system:
        D(1)*x(1)       + E(1)*x(2)                       = B(1)
        C(1:-1)*x(1:-2) + D(2:-1)*x(2:-1) + E(2:0)*x(3:0) = B(2:-1)
                          C(0)*x(-1)      + D(0)*x(0)     = B(0)
     (i.e.- C is the subdiagonal, D the diagonal, and E the superdiagonal;
     C and E have one fewer element than D, which is the same length as
     both B and x)

     B may have additional dimensions, in which case the returned x
     will have the same additional dimensions.  The WHICH dimension of B,
     and of the returned x is the one of length n which participates
     in the matrix solve.  By default, WHICH=1, so that the equations
     being solved involve B(,..) and x(+,..).
     Non-positive WHICH counts from the final dimension (as for the
     sort and transpose functions), so that WHICH=0 involves B(..,)
     and x(..,+).

     The C, D, and E arguments may be either scalars or vectors; they
     will be broadcast as appropriate.

  SEE ALSO: LUsolve, QRsolve, SVsolve, SVdec
 */
{
  /* check validity of b argument */
  if (structof(b)==complex) error, "expecting a non-complex RHS vector";
  dims= dimsof(b);
  ndb= is_void(dims)? 0 : dims(1);
  if (is_void(which)) which= 1;
  else if (which<=0) which+= ndb;
  if (!ndb) error, "RHS must have at least one dimension";
  n= dims(1+which);
  b= double(b);   /* copy of RHS to be transformed into solution */
  nrhs= numberof(b)/n;

  /* put first matrix dimension of b first */
  if (which!=1) b= transpose(b, [1,which]);

  /* copy, force to double, and broadcast matrix diagonals
     -- also will blow up on conformability error */
  cc= ee= array(0.0, n-1);
  dd= array(0.0, n);
  cc()= c;
  dd()= d;
  ee()= e;

  info= 0;
  _dgtsv, n, nrhs, cc, dd, ee, b, n, info;
  if (info) error, "tridiagonal element "+pr1(info)+" of became 0.0";

  /* restore proper order of result if necessary */
  if (which!=1) b= transpose(b, [1,which]);

  return b;
}
errs2caller, TDsolve;

extern _dgtsv;
/* PROTOTYPE
   void ygtsv(long n, long nrhs, double array c, double array d,
              double array e, double array b, long ldb, long array info)
 */
/* DOCUMENT _dgtsv
     LAPACK dgtsv routine.
 */

/* ------------------------------------------------------------------------ */

func LUsolve(a, b, which=)
/* DOCUMENT LUsolve(a, b)
         or LUsolve(a, b, which=which)
         or a_inverse= LUsolve(a)

     returns the solution x of the matrix equation:
        A(,+)*x(+) = B
     If A is an n-by-n matrix then B must have length n, and the returned
     x will also have length n.

     B may have additional dimensions, in which case the returned x
     will have the same additional dimensions.  The WHICH dimension of B,
     and of the returned x is the one of length n which participates
     in the matrix solve.  By default, WHICH=1, so that the equations
     being solved are:
        A(,+)*x(+,..) = B
     Non-positive WHICH counts from the final dimension (as for the
     sort and transpose functions), so that WHICH=0 solves:
        x(..,+)*A(,+) = B
     Other examples:
        A_ij X_jklm = B_iklm   (WHICH=1)
        A_ij X_kjlm = B_kilm   (WHICH=2)
        A_ij X_klmj = B_klmi   (WHICH=4 or WHICH=0)

     If the B argument is omitted, the inverse of A is returned:
     A(,+)*x(+,) and A(,+)*x(,+) will be unit matrices.

     LUsolve works by LU decomposition using Gaussian elimination with
     pivoting.  It is the fastest way to solve square matrices.  QRsolve
     handles non-square matrices, as does SVsolve.  SVsolve is slowest,
     but can deal with highly singular matrices sensibly.

   SEE ALSO: QRsolve, TDsolve, SVsolve, SVdec, LUrcond
 */
{
  { local dims, n, m, nrhs; }
  if (structof(a)==complex) {
    /* convert complex system to equivalent real system */
    is_complex= 1;
    if (is_void(b)) b= complex(unit(dimsof(a)(2)));
    a= _to_real_system(a, b, which);
    ww= which;
    which= 1;
  } else if (structof(b)==complex) {
    /* matrix is real, but RHS and solution complex */
    is_complex= 2;
    b= [b.re, b.im];
    if (!is_void(which) && which<=0) which-= 1;
  }

  /* get n, m, dims, nrhs, checking validity of a and b */
  _get_matrix, 1;
  if (m!=n) error, "expecting a square matrix";

  if (is_void(b)) {
    b= unit(n);
    nrhs= n;
    which= 1;
  }

  /* perform LU solve */
  pivot= array(0, n);
  info= 0;
  _dgesv, n, nrhs, a, n, pivot, b, n, info;
  /* row i interchanged with row pivot(i) --> permutation matrix P
     a now contains the L and U factors from the decomposition;
     original a= P*L*U */
  if (info) error, "matrix is (numerically) singular";

  /* restore proper order of result if necessary */
  if (which!=1) b= transpose(b, [1,which]);

  if (is_complex) {
    /* convert back to complex system from real system */
    if (is_complex==1) {
      dims= dimsof(b);
      dims(2)/= 2;
      reshape, b, complex, dims;
      if (ww!=1) z= transpose(b, [1,ww]);
      else z= b;
    } else {
      dims= dimsof(b);
      dims(1)-= 1;
      z= array(complex, dims);
      z.re= b(..,1);
      z.im= b(..,2);
    }
    return z;
  }

  return b;
}
errs2caller, LUsolve;

func _to_real_system(a, &b, &which)
{
  /* convert a complex matrix a to an equivalent real matrix; each
     element of the original matrix becomes a 2x2 block [[r,i],[-i,r]],
     so that complex multiplication is contraction on the second index */
  dims= dimsof(a);
  m= dims(2);
  n= dims(3);
  if (m!=n) error, "expecting a square matrix";
  z= array(0.0, 2*n, 2*n);
  z(1:-1:2,1:-1:2)= z(2:0:2,2:0:2)= a.re;
  z(1:-1:2,2:0:2)= -(z(2:0:2,1:-1:2)= a.im);
  if (!is_void(b)) {
    /* take care of RHS */
    if (structof(b)!=complex) b= complex(b);
    if (is_void(which)) which= 1;
    else if (which!=1) b= transpose(b,[1,which]);
    dims= dimsof(b);
    dims(2)*= 2;
    { local bp; }
    reshape, bp, &b, double, dims;
    b= bp;
  }
  return z;
}

func LUrcond(a, one_norm=)
/* DOCUMENT LUrcond(a)
         or LUrcond(a, one_norm=1)
     returns the reciprocal condition number of the N-by-N matrix A.
     If the ONE_NORM argument is non-nil and non-zero, the 1-norm
     condition number is returned, otherwise the infinity-norm condition
     number is returned.

     The condition number is the ratio of the largest to the smallest
     singular value, max(singular_values)*max(1/singular_values) (or
     sum(abs(singular_values)*sum(abs(1/singular_values)) if ONE_NORM
     is selected?).  If the reciprocal condition number is near zero
     then A is numerically singular; specifically, if
          1.0+LUrcond(a) == 1.0
     then A is numerically singular.

   SEE ALSO: LUsolve
 */
{
  dims= dimsof(a);
  if (is_void(dims) || dims(1)!=2 || dims(2)!=dims(3) ||
      structof(a)==complex)
    error, "expecting a square 2D real matrix";
  n= dims(2);
  a= double(a);
  pivot= array(0, n);
  info= 0;
  _dgetrf, n, n, a, n, pivot, info;
  /* a is now the LU decomposition of the original a, permuted according
     to pivot.  Note that the determinant of a is the product of the
     diagonal elements a(1:n*n:n+1).  */
  work= array(double, 4*n);
  iwork= array(0, n);
  rcond= 0.0;
  if (!one_norm) {
    one_norm= 0;
    anorm= abs(a)(max,sum);
  } else {
    one_norm= 1;
    anorm= abs(a)(sum,max);
  }
  _dgecox, one_norm, n, a, n, anorm, rcond, work, iwork, info;
  return rcond;
}
errs2caller, LUrcond;

extern _dgesv;
/* PROTOTYPE
   void ygesv(long n, long nrhs, double array a, long lda,
              long array pivot, double array b, long ldb, long array info)
 */
/* DOCUMENT _dgesv
     LAPACK dgesv routine.
 */

extern _dgetrf;
/* PROTOTYPE
   void ygetrf(long m, long n, double array a, long lda,
               long array pivot, long array info)
 */
/* DOCUMENT _dgetrf
     LAPACK dgetrf routine.  Performs LU factorization.
 */

extern _dgecox;
/* PROTOTYPE
   void ygecox(long norm, long n, double array a, long lda,
               double anorm, double array rcond, double array work,
               long array iwork, long array info)
 */
/* DOCUMENT _dgecox
     LAPACK dgecon routine, except norm argument not a string.
 */

/* ------------------------------------------------------------------------ */

func QRsolve(a, b, which=)
/* DOCUMENT QRsolve(a, b)
         or QRsolve(a, b, which=which)

     returns the solution x (in a least squares or least norm sense
     described below) of the matrix equation:
        A(,+)*x(+) = B
     If A is an m-by-n matrix (i.e.- m equations in n unknowns), then B
     must have length m, and the returned x will have length n.

     If n<m, the system is overdetermined -- no solutions are possible
             -- the returned x minimizes sqrt(sum((A(,+)*x(+) - B)^2))
     If n>m, the system is underdetermined -- many solutions are possible
             -- the returned x has minimum L2 norm among all solutions

     B may have additional dimensions, in which case the returned x
     will have the same additional dimensions also have those dimensions.
     The WHICH dimension of B and the returned x is the one of length m
     or n which participates in the matrix solve.  By default, WHICH=1,
     so that the equations being solved are:
        A(,+)*x(+,..) = B
     Non-positive WHICH counts from the final dimension (as for the
     sort and transpose functions), so that WHICH=0 solves:
        A(,+)*x(..,+) = B

     QRsolve works by QR factorization if n<m, or LQ factorization if n>m.
     QRsolve is slower than LUsolve.  Its main attraction is that it can
     handle overdetermined or underdetermined systems of equations
     (nonsquare matrices).  QRsolve may fail for singular systems; try
     SVsolve in this case.

   SEE ALSO: LUsolve, TDsolve, SVsolve, SVdec
 */
{
  /* get n, m, dims, nrhs, checking validity of a and b */
  { local dims, n, m, nrhs; }
  _get_matrix, 0;

  /* set up and perform QR or LQ solve --
     first call returns optimal workspace length */
  work= 0.0;
  info= 0;
  mnmax= max(m,n);
  _dgelx, 0, m, n, nrhs, a, m, b, mnmax, work, 1, info;
  if (info==-10) {
    lwork= long(work);
    work= array(0.0, lwork);
    _dgelx, 0, m, n, nrhs, a, m, b, mnmax, work, lwork, info;
  }
  if (info) error, "matrix is (numerically) singular"; /* impossible? */

  /* restore proper order of result if necessary */
  if (n<mnmax) b= b(1:n,..)
  if (which!=1) b= transpose(b, [1,which]);

  return b;
}
errs2caller, QRsolve;

extern _dgelx;
/* PROTOTYPE
   void ygelx(long trans, long m, long n, long nrhs,
              double array a, long lda, double array b, long ldb,
              double array work, long lwork, long array info)
 */
/* DOCUMENT _dgelx
     LAPACK dgels routine, except trans argument not a string.
 */

/* ------------------------------------------------------------------------ */

func SVsolve(a, b, rcond, which=)
/* DOCUMENT SVsolve(a, b)
         or SVsolve(a, b, rcond)
         or SVsolve(a, b, rcond, which=which)

     returns the solution x (in a least squares sense described below) of
     the matrix equation:
        A(,+)*x(+) = B
     If A is an m-by-n matrix (i.e.- m equations in n unknowns), then B
     must have length m, and the returned x will have length n.

     If n<m, the system is overdetermined -- no solutions are possible
             -- the returned x minimizes sqrt(sum((A(,+)*x(+) - B)^2))
     If n>m, the system is underdetermined -- many solutions are possible
             -- the returned x has minimum L2 norm among all solutions

     SVsolve works by singular value decomposition, therefore it is
     immune to failure due to singularity of the A matrix.  The optional
     RCOND argument defaults to 1.0e-9; singular values less than RCOND
     times the largest singular value (absolute value) will be set to 0.0.
     If RCOND<=0.0, machine precision is used.  The effective rank of the
     matrix is returned as the external variable SVrank.

     You can examine the details of the SVD by calling the SVdec routine,
     which returns the singular vectors as well as the singular values.
     Numerical Recipes (Press, et. al. Cambridge University Press 1988)
     has a good discussion of how to use the SVD -- see section 2.9.

     B may have additional dimensions, in which case the returned x
     will have the same additional dimensions.  The WHICH argument
     (default 1) controls which dimension of B takes part in the matrix
     solve.  See QRsolve or LUsolve for a complete discussion.

   SEE ALSO: SVdec, LUsolve, QRsolve, TDsolve
 */
{
  /* get n, m, dims, nrhs, checking validity of a and b */
  { local dims, n, m, nrhs; }
  _get_matrix, 0;

  if (is_void(rcond)) rcond= 1.e-9;
  else rcond= double(rcond);

  /* set up and perform SVD solve --
     first call returns optimal workspace length */
  { extern SVrank; }
  work= 0.0;
  info= SVrank= 0;
  s= array(0.0, min(m,n));
  mnmax= max(m,n);
  _dgelss, m, n, nrhs, a, m, b, mnmax, s, rcond, SVrank, work, 1, info;
  if (info==-12) {
    lwork= long(work);
    work= array(0.0, lwork);
    _dgelss, m, n, nrhs, a, m, b, mnmax, s, rcond, SVrank, work, lwork, info;
  }
  if (info) error, "SVD algorithm failed to converge - wow";

  /* restore proper order of result if necessary */
  if (n<mnmax) b= b(1:n,..)
  if (which!=1) b= transpose(b, [1,which]);

  return b;
}
errs2caller, SVsolve;

func SVdec(a, &u, &vt, full=)
/* DOCUMENT s= SVdec(a, u, vt)
         or s= SVdec(a, u, vt, full=1)

     performs the singular value decomposition of the m-by-n matrix A:
        A = (U(,+) * SIGMA(+,))(,+) * VT(+,)
     where U is an m-by-m orthogonal matrix, VT is an n-by-n orthogonal
     matrix, and SIGMA is an m-by-n matrix which is zero except for its
     min(m,n) diagonal elements.  These diagonal elements are the return
     value of the function, S.  The returned S is always arranged in
     order of descending absolute value.  U(,1:min(m,n)) are the left
     singular vectors corresponding to the min(m,n) elements of S;
     VT(1:min(m,n),) are the right singular vectors.  (The original A
     matrix maps a right singular vector onto the corresponding left
     singular vector, stretched by a factor of the singular value.)

     Note that U and VT are strictly outputs; if you don't need them,
     they need not be present in the calling sequence.

     By default, U will be an m-by-min(m,n) matrix, and V will be
     a min(m,n)-by-n matrix (i.e.- only the singular vextors are returned,
     not the full orthogonal matrices).  Set the FULL keyword to a
     non-zero value to get the full m-by-m and n-by-n matrices.

     On rare occasions, the routine may fail; if it does, the
     first SVinfo values of the returned S are incorrect.  Hence,
     the external variable SVinfo will be 0 after a successful call
     to SVdec.  If SVinfo>0, then external SVe contains the superdiagonal
     elements of the bidiagonal matrix whose diagonal is the returned
     S, and that bidiagonal matrix is equal to (U(+,)*A(+,))(,+) * V(+,).

     Numerical Recipes (Press, et. al. Cambridge University Press 1988)
     has a good discussion of how to use the SVD -- see section 2.9.

   SEE ALSO: SVsolve, LUsolve, QRsolve, TDsolve
 */
{
  /* get n, m, dims, nrhs, checking validity of a and b */
  { local dims, n, m, nrhs; }
  b= [];
  _get_matrix, 1;

  if (!full) full= 0;
  else full= 1;

  /* set up and perform SVD solve --
     first call returns optimal workspace length */
  work= 0.0;
  info= 0;
  s= array(0.0, min(m,n));
  if (full) {
    u= array(0.0, m, m);
    vt= array(0.0, n, n);
    ldvt= n;
  } else {
    ldvt= min(m, n);
    u= array(0.0, m, ldvt);
    vt= array(0.0, ldvt, n);
  }
  _dgesvx, full, m, n, a, m, s, u, m, vt, ldvt, work, 1, info;
  if (info==-13) {
    lwork= long(work);
    work= array(0.0, lwork);
    _dgesvx, full, m, n, a, m, s, u, m, vt, ldvt, work, lwork, info;
  }
  if (info) error, "SVD algorithm failed to converge - wow";

  return s;
}
errs2caller, SVdec;

extern _dgelss;
/* PROTOTYPE
   void ygelss(long m, long n, long nrhs, double array a, long lda,
               double array b, long ldb, double array s, double rcond,
               long array rank, double array work, long lwork,
               long array info)
 */
/* DOCUMENT _dgelss
     LAPACK dgelss routine.
 */

extern _dgesvx;
/* PROTOTYPE
   void ygesvx(long job, long m, long n, double array a, long lda,
               double array s, double array u, long ldu,
               double array vt, long ldvt, double array work, long lwork,
               long array info)
 */
/* DOCUMENT _dgesvx
     LAPACK dgesvd routine, except jobu and jobvt are not strings.
 */

/* ------------------------------------------------------------------------ */

func _get_matrix(b_optional)
{
  { extern dims, n, m, nrhs; }

  /* check validity of a argument */
  dims= dimsof(a);
  if (dims(1)!=2 || structof(a)==complex)
    error, "expecting a non-complex 2D matrix";
  a= double(a);  /* copy a to avoid clobbering, as well as force type */
  m= dims(2);
  n= dims(3);

  /* check validity of b argument */
  if (!b_optional || !is_void(b)) {
    dims= dimsof(b);
    ndb= is_void(dims)? 0 : dims(1);
    if (is_void(which)) which= 1;
    else if (which<=0) which+= ndb;
    if (!ndb || dims(1+which)!=m)
      error, "RHS dimensions not conformable with matrix dimensions";
    if (structof(b)==complex) error, "expecting a non-complex RHS vector";
    b= double(b);  /* copy to avoid clobbering, and force type */
    nrhs= numberof(b)/m;

    /* put first matrix dimension of b first */
    if (which!=1) b= transpose(b, [1,which]);

    /* be sure that the first dimension of b is at least n */
    if (n>m) {
      dims= dimsof(b);
      dims(2)= n;
      bn= array(0.0, dims);
      bn(1:m,..)= b;
      b= bn;
    }

  } else {
    nrhs= 0;
  }
}

/* ------------------------------------------------------------------------ */
