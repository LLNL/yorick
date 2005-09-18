/*
 * $Id: dgecon.c,v 1.1 2005-09-18 22:04:39 dhmunro Exp $
 * LAPACK routines to estimate the condition number of a matrix.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "dg.h"

/*---blas routines---*/
/* ddot dcopy dasum idamax daxpy dscal dtrsv */


/*---converted nutty string switches to single characters (lower case)---*/
#define lsame(x,y) ((x)==(y))


/*-----Fortran intrinsics converted-----*/
extern double pow(double,double);  /* performance never an issue here! */
#define abs(x) ((x)>=0?(x):-(x))
extern double sqrt(double);
extern double log10(double);
#define mod(x,y) ((x)%(y))
#define dble(x) ((double)x)
#define nint(x) ((long)((x)>=0?(x)+.5:(x)-.5))
#define int(x) ((long)x)
#define sign(x,y) ((((x)<0)!=((y)<0))?-(x):(x))
#define min(x,y) ((x)<(y)?(x):(y))
#define max(x,y) ((x)>(y)?(x):(y))
/*-----end of Fortran intrinsics-----*/


void dgecon( char norm, long n, double a[], long lda, double anorm,
            double *rcond, double work[], long iwork[],long *info )
{
  /**
   *  -- LAPACK routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     February 29, 1992
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *     .. Array Arguments ..*/
#undef iwork_1
#define iwork_1(a1) iwork[a1-1]
#undef work_1
#define work_1(a1) work[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGECON estimates the reciprocal of the condition number of a general
   *  real matrix A, in either the 1-norm or the infinity-norm, using
   *  the LU factorization computed by DGETRF.
   *
   *  An estimate is obtained for norm(inv(A)), and the reciprocal of the
   *  condition number is computed as
   *     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
   *
   *  Arguments
   *  =========
   *
   *  NORM    (input) CHARACTER*1
   *          Specifies whether the 1-norm condition number or the
   *          infinity-norm condition number is required:
   *          = '1' or 'O':  1-norm;
   *          = 'I':         Infinity-norm.
   *
   *  N       (input) INTEGER
   *          The order of the matrix A.  N >= 0.
   *
   *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
   *          The factors L and U from the factorization A = P*L*U
   *          as computed by DGETRF.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,N).
   *
   *  ANORM   (input) DOUBLE PRECISION
   *          If NORM = '1' or 'O', the 1-norm of the original matrix A.
   *          If NORM = 'I', the infinity-norm of the original matrix A.

   Note: 1-norm of matrix A is abs(A)(sum,max)
         infinity-norm of A is abs(A)(max,sum)

   *
   *  RCOND   (output) DOUBLE PRECISION
   *          The reciprocal of the condition number of the matrix A,
   *          computed as RCOND = 1/(norm(A) * norm(inv(A))).
   *
   *  WORK_1    (workspace) DOUBLE PRECISION array, dimension (4*N)
   *
   *  IWORK_1   (workspace) INTEGER array, dimension (N)
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument had an illegal value
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef one
#define one 1.0e+0
#undef zero
#define zero 0.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  int            onenrm;
  char          normin;
  long            ix, kase, kase1;
  double    ainvnm, scale, sl, smlnum, su;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, max;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input parameters.
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  onenrm = norm=='1' || lsame( norm, 'o' );
  if( !onenrm && !lsame( norm, 'i' ) ) {
    *info = -1;
  } else if( n<0 ) {
    *info = -2;
  } else if( lda<max( 1, n ) ) {
    *info = -4;
  } else if( anorm<zero ) {
    *info = -5;
  }
  if( *info!=0 ) {
    xerbla( "dgecon", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  *rcond = zero;
  if( n==0 ) {
    *rcond = one;
    return;
  } else if( anorm==zero ) {
    return;
  }

  smlnum = dlamch( 's'/*afe minimum*/ );
  /**
   *     Estimate the norm of inv(A).
   **/
  ainvnm = zero;
  normin = 'n';
  if( onenrm ) {
    kase1 = 1;
  } else {
    kase1 = 2;
  }
  kase = 0;
 L_10:
  dlacon( n, &work_1( n+1 ), work, iwork, &ainvnm, &kase );
  if( kase!=0 ) {
    if( kase==kase1 ) {
      /**
       *           Multiply by inv(L).
       **/
      dlatrs( 'l'/*ower*/, 'n'/*o transpose*/, 'u'/*nit*/, normin, n, a,
             lda, work, &sl, &work_1( 2*n+1 ), info );
      /**
       *           Multiply by inv(U).
       **/
      dlatrs( 'u'/*pper*/, 'n'/*o transpose*/, 'n'/*on-unit*/, normin, n,
             a, lda, work, &su, &work_1( 3*n+1 ), info );
    } else {
      /**
       *           Multiply by inv(U').
       **/
      dlatrs( 'u'/*pper*/, 't'/*ranspose*/, 'n'/*on-unit*/, normin, n, a,
             lda, work, &su, &work_1( 3*n+1 ), info );
      /**
       *           Multiply by inv(L').
       **/
      dlatrs( 'l'/*ower*/, 't'/*ranspose*/, 'u'/*nit*/, normin, n, a,
             lda, work, &sl, &work_1( 2*n+1 ), info );
    }
    /**
     *        Divide X by 1/(SL*SU) if doing so will not cause overflow.
     **/
    scale = sl*su;
    normin = 'y';
    if( scale!=one ) {
      ix = 1+cblas_idamax( n, work, 1 );
      if( scale<abs( work_1( ix ) )*smlnum || scale==zero )
        goto L_20;
      drscl( n, scale, work, 1 );
    }
    goto L_10;
  }
  /**
   *     Compute the estimate of the reciprocal condition number.
   **/
  if( ainvnm!=zero )
    *rcond = ( one / ainvnm ) / anorm;

 L_20:
  return;
  /**
   *     End of DGECON
   **/
}



void drscl( long n, double sa, double sx[], long incx )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     October 31, 1992
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *     .. Array Arguments ..*/
#undef sx_1
#define sx_1(a1) sx[a1-1]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DRSCL multiplies an n-element real vector x by the real scalar 1/a.
   *  This is done without overflow or underflow as long as
   *  the final result x/a does not overflow or underflow.
   *
   *  Arguments
   *  =========
   *
   *  N       (input) INTEGER
   *          The number of components of the vector x.
   *
   *  SA      (input) DOUBLE PRECISION
   *          The scalar a which is used to divide each component of x.
   *          SA must be >= 0, or the subroutine will divide by zero.
   *
   *  SX_1      (input/output) DOUBLE PRECISION array, dimension
   *                         (1+(N-1)*abs(INCX))
   *          The n-element vector x.
   *
   *  INCX    (input) INTEGER
   *          The increment between successive values of the vector SX.
   *          > 0:  SX_1(1) = X(1) and SX_1(1+(i-1)*INCX) = x(i),     1< i<= n
   *          < 0:  SX_1(1) = X(n) and SX_1(1+(i-1)*INCX) = x(n-i+1), 1< i<= n
   *
   * =====================================================================
   *
   *     .. Parameters ..*/
#undef one
#define one 1.0e+0
#undef zero
#define zero 0.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  int            done;
  double    bignum, cden, cden1, cnum, cnum1, mul, smlnum;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Quick return if possible
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if( n<=0 )
    return;
  /**
   *     Get machine parameters
   **/
  smlnum = dlamch( 's' );
  bignum = one / smlnum;
  dlabad( &smlnum, &bignum );
  /**
   *     Initialize the denominator to SA and the numerator to 1.
   **/
  cden = sa;
  cnum = one;

 L_10:
  cden1 = cden*smlnum;
  cnum1 = cnum / bignum;
  if( abs( cden1 )>abs( cnum ) && cnum!=zero ) {
    /**
     *        Pre-multiply X by SMLNUM if CDEN is large compared to CNUM.
     **/
    mul = smlnum;
    done = 0;
    cden = cden1;
  } else if( abs( cnum1 )>abs( cden ) ) {
    /**
     *        Pre-multiply X by BIGNUM if CDEN is small compared to CNUM.
     **/
    mul = bignum;
    done = 0;
    cnum = cnum1;
  } else {
    /**
     *        Multiply X by CNUM / CDEN and return.
     **/
    mul = cnum / cden;
    done = 1;
  }
  /**
   *     Scale the vector X by MUL
   **/
  cblas_dscal( n, mul, sx, incx );

  if( !done )
    goto L_10;

  return;
  /**
   *     End of DRSCL
   **/
}



void dlabad( double *small, double *large )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     October 31, 1992
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLABAD takes as input the values computed by SLAMCH for underflow and
   *  overflow, and returns the square root of each of these values if the
   *  log of LARGE is sufficiently large.  This subroutine is intended to
   *  identify machines with a large exponent range, such as the Crays, and
   *  redefine the underflow and overflow limits to be the square roots of
   *  the values computed by DLAMCH.  This subroutine is needed because
   *  DLAMCH does not compensate for poor arithmetic in the upper half of
   *  the exponent range, as is found on a Cray.
   *
   *  Arguments
   *  =========
   *
   *  SMALL   (input/output) DOUBLE PRECISION
   *          On entry, the underflow threshold as computed by DLAMCH.
   *          On exit, if LOG10(LARGE) is sufficiently large, the square
   *          root of SMALL, otherwise unchanged.
   *
   *  LARGE   (input/output) DOUBLE PRECISION
   *          On entry, the overflow threshold as computed by DLAMCH.
   *          On exit, if LOG10(LARGE) is sufficiently large, the square
   *          root of LARGE, otherwise unchanged.
   *
   *  =====================================================================
   *
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          log10, sqrt;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     If it looks like we're on a Cray, take the square root of
   *     SMALL and LARGE to avoid overflow and underflow problems.
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if( log10( *large )>2000.e0 ) {
    *small = sqrt( *small );
    *large = sqrt( *large );
  }

  return;
  /**
   *     End of DLABAD
   **/
}



void dlatrs( char uplo, char trans, char diag, char normin, long n,
            double a[], long lda, double x[], double *scale,
            double cnorm[], long *info )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     June 30, 1992
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *     .. Array Arguments ..*/
#undef x_1
#define x_1(a1) x[a1-1]
#undef cnorm_1
#define cnorm_1(a1) cnorm[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLATRS solves one of the triangular systems
   *
   *     A *x = s*b  or  A'*x = s*b
   *
   *  with scaling to prevent overflow.  Here A is an upper or lower
   *  triangular matrix, A' denotes the transpose of A, x and b are
   *  n-element vectors, and s is a scaling factor, usually less than
   *  or equal to 1, chosen so that the components of x will be less than
   *  the overflow threshold.  If the unscaled problem will not cause
   *  overflow, the Level 2 BLAS routine DTRSV is called.  If the matrix A
   *  is singular (A_2(j,j) = 0 for some j), then s is set to 0 and a
   *  non-trivial solution to A*x = 0 is returned.
   *
   *  Arguments
   *  =========
   *
   *  UPLO    (input) CHARACTER*1
   *          Specifies whether the matrix A is upper or lower triangular.
   *          = 'U':  Upper triangular
   *          = 'L':  Lower triangular
   *
   *  TRANS   (input) CHARACTER*1
   *          Specifies the operation applied to A.
   *          = 'N':  Solve A * x = s*b  (No transpose)
   *          = 'T':  Solve A'* x = s*b  (Transpose)
   *          = 'C':  Solve A'* x = s*b  (Conjugate transpose = Transpose)
   *
   *  DIAG    (input) CHARACTER*1
   *          Specifies whether or not the matrix A is unit triangular.
   *          = 'N':  Non-unit triangular
   *          = 'U':  Unit triangular
   *
   *  NORMIN  (input) CHARACTER*1
   *          Specifies whether CNORM has been set or not.
   *          = 'Y':  CNORM contains the column norms on entry
   *          = 'N':  CNORM is not set on entry.  On exit, the norms will
   *                  be computed and stored in CNORM.
   *
   *  N       (input) INTEGER
   *          The order of the matrix A.  N >= 0.
   *
   *  A_2       (input) DOUBLE PRECISION array, dimension (LDA,N)
   *          The triangular matrix A.  If UPLO = 'U', the leading n by n
   *          upper triangular part of the array A contains the upper
   *          triangular matrix, and the strictly lower triangular part of
   *          A is not referenced.  If UPLO = 'L', the leading n by n lower
   *          triangular part of the array A contains the lower triangular
   *          matrix, and the strictly upper triangular part of A is not
   *          referenced.  If DIAG = 'U', the diagonal elements of A are
   *          also not referenced and are assumed to be 1.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max (1,N).
   *
   *  X_1       (input/output) DOUBLE PRECISION array, dimension (N)
   *          On entry, the right hand side b of the triangular system.
   *          On exit, X is overwritten by the solution vector x.
   *
   *  SCALE   (output) DOUBLE PRECISION
   *          The scaling factor s for the triangular system
   *             A * x = s*b  or  A'* x = s*b.
   *          If SCALE = 0, the matrix A is singular or badly scaled, and
   *          the vector x is an exact or approximate solution to A*x = 0.
   *
   *  CNORM_1   (input or output) DOUBLE PRECISION array, dimension (N)
   *
   *          If NORMIN = 'Y', CNORM is an input variable and CNORM_1(j)
   *          contains the norm of the off-diagonal part of the j-th column
   *          of A.  If TRANS = 'N', CNORM_1(j) must be greater than or equal
   *          to the infinity-norm, and if TRANS = 'T' or 'C', CNORM_1(j)
   *          must be greater than or equal to the 1-norm.
   *
   *          If NORMIN = 'N', CNORM is an output variable and CNORM_1(j)
   *          returns the 1-norm of the offdiagonal part of the j-th column
   *          of A.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -k, the k-th argument had an illegal value
   *
   *  Further Details
   *  ======= =======
   *
   *  A rough bound on x is computed; if that is less than overflow, DTRSV
   *  is called, otherwise, specific code is used which checks for possible
   *  overflow or divide-by-zero at every operation.
   *
   *  A columnwise scheme is used for solving A*x = b.  The basic algorithm
   *  if A is lower triangular is
   *
   *       x[1:n] := b[1:n]
   *       for j = 1, ..., n
   *            x_1(j) := x_1(j) / A_2(j,j)
   *            x[j+1:n] := x[j+1:n] - x_1(j) * A[j+1:n,j]
   *       end
   *
   *  Define bounds on the components of x after j iterations of the loop:
   *     M(j) = bound on x[1:j]
   *     G(j) = bound on x[j+1:n]
   *  Initially, let M(0) = 0 and G(0) = max{x_1(i), i=1,...,n}.
   *
   *  Then for iteration j+1 we have
   *     M(j+1) <= G(j) / | A_2(j+1,j+1) |
   *     G(j+1) <= G(j) + M(j+1) * | A[j+2:n,j+1] |
   *            <= G(j) ( 1 + CNORM_1(j+1) / | A_2(j+1,j+1) | )
   *
   *  where CNORM_1(j+1) is greater than or equal to the infinity-norm of
   *  column j+1 of A, not counting the diagonal.  Hence
   *
   *     G(j) <= G(0) product ( 1 + CNORM_1(i) / | A_2(i,i) | )
   *                  1<=i<=j
   *  and
   *
   *     |x_1(j)| <= ( G(0) / |A_2(j,j)| ) product ( 1 + CNORM_1(i) / |A_2(i,i)| )
   *                                   1<=i< j
   *
   *  Since |x_1(j)| <= M(j), we use the Level 2 BLAS routine DTRSV if the
   *  reciprocal of the largest M(j), j=1,..,n, is larger than
   *  max(underflow, 1/overflow).
   *
   *  The bound on x_1(j) is also used to determine when a step in the
   *  columnwise method can be performed without fear of overflow.  If
   *  the computed bound is greater than a large constant, x is scaled to
   *  prevent overflow, but if the bound overflows, x is set to 0, x_1(j) to
   *  1, and scale to 0, and a non-trivial solution to A*x = 0 is found.
   *
   *  Similarly, a row-wise scheme is used to solve A'*x = b.  The basic
   *  algorithm for A upper triangular is
   *
   *       for j = 1, ..., n
   *            x_1(j) := ( b(j) - A[1:j-1,j]' * x[1:j-1] ) / A_2(j,j)
   *       end
   *
   *  We simultaneously compute two bounds
   *       G(j) = bound on ( b(i) - A[1:i-1,i]' * x[1:i-1] ), 1<=i<=j
   *       M(j) = bound on x_1(i), 1<=i<=j
   *
   *  The initial values are G(0) = 0, M(0) = max{b(i), i=1,..,n}, and we
   *  add the constraint G(j) >= G(j-1) and M(j) >= M(j-1) for j >= 1.
   *  Then the bound on x_1(j) is
   *
   *       M(j) <= M(j-1) * ( 1 + CNORM_1(j) ) / | A_2(j,j) |
   *
   *            <= M(0) * product ( ( 1 + CNORM_1(i) ) / |A_2(i,i)| )
   *                      1<=i<=j
   *
   *  and we can safely call DTRSV if 1/M(n) and 1/G(n) are both greater
   *  than max(underflow, 1/overflow).
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e+0
#undef half
#define half 0.5e+0
#undef one
#define one 1.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  int            notran, nounit, upper;
  long            i, imax, j, jfirst, jinc, jlast;
  double    bignum, grow, rec, smlnum, sumj, tjj, tjjs=0.0,
  tmax, tscal, uscal, xbnd, xj, xmax;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, max, min;*/
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  upper = lsame( uplo, 'u' );
  notran = lsame( trans, 'n' );
  nounit = lsame( diag, 'n' );
  /**
   *     Test the input parameters.
   **/
  if( !upper && !lsame( uplo, 'l' ) ) {
    *info = -1;
  } else if( !notran && !lsame( trans, 't' ) && !
            lsame( trans, 'c' ) ) {
    *info = -2;
  } else if( !nounit && !lsame( diag, 'u' ) ) {
    *info = -3;
  } else if( !lsame( normin, 'y' ) && !
            lsame( normin, 'n' ) ) {
    *info = -4;
  } else if( n<0 ) {
    *info = -5;
  } else if( lda<max( 1, n ) ) {
    *info = -7;
  }
  if( *info!=0 ) {
    xerbla( "dlatrs", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( n==0 )
    return;
  /**
   *     Determine machine dependent parameters to control overflow.
   **/
  smlnum = dlamch( 's'/*afe minimum*/ ) / dlamch( 'p'/*recision*/ );
  bignum = one / smlnum;
  *scale = one;

  if( lsame( normin, 'n' ) ) {
    /**
     *        Compute the 1-norm of each column, not including the diagonal.
     **/
    if( upper ) {
      /**
       *           A is upper triangular.
       **/
      for (j=1 ; j<=n ; j+=1) {
        cnorm_1( j ) = cblas_dasum( j-1, &a_2( 1, j ), 1 );
      }
    } else {
      /**
       *           A is lower triangular.
       **/
      for (j=1 ; j<=n - 1 ; j+=1) {
        cnorm_1( j ) = cblas_dasum( n-j, &a_2( j+1, j ), 1 );
      }
      cnorm_1( n ) = zero;
    }
  }
  /**
   *     Scale the column norms by TSCAL if the maximum entry in CNORM is
   *     greater than BIGNUM.
   **/
  imax = 1+cblas_idamax( n, cnorm, 1 );
  tmax = cnorm_1( imax );
  if( tmax<=bignum ) {
    tscal = one;
  } else {
    tscal = one / ( smlnum*tmax );
    cblas_dscal( n, tscal, cnorm, 1 );
  }
  /**
   *     Compute a bound on the computed solution vector to see if the
   *     Level 2 BLAS routine DTRSV can be used.
   **/
  j = 1+cblas_idamax( n, x, 1 );
  xmax = abs( x_1( j ) );
  xbnd = xmax;
  if( notran ) {
    /**
     *        Compute the growth in A * *x = b.
     **/
    if( upper ) {
      jfirst = n;
      jlast = 1;
      jinc = -1;
    } else {
      jfirst = 1;
      jlast = n;
      jinc = 1;
    }

    if( tscal!=one ) {
      grow = zero;
      goto L_50;
    }

    if( nounit ) {
      /**
       *           A is non-unit triangular.
       *
       *           Compute GROW = 1/G(j) and XBND = 1/M(j).
       *           Initially, G(0) = max{x_1(i), i=1,...,n}.
       **/
      grow = one / max( xbnd, smlnum );
      xbnd = grow;
      for (j=jfirst ; jinc>0?j<=jlast:j>=jlast ; j+=jinc) {
        /**
         *              Exit the loop if the growth factor is too small.
         **/
        if( grow<=smlnum )
          goto L_50;
        /**
         *              M(j) = G(j-1) / abs(A_2(j,j))
         **/
        tjj = abs( a_2( j, j ) );
        xbnd = min( xbnd, min( one, tjj )*grow );
        if( tjj+cnorm_1( j )>=smlnum ) {
          /**
           *                 G(j) = G(j-1)*( 1 + CNORM_1(j) / abs(A_2(j,j)) )
           **/
          grow = grow*( tjj / ( tjj+cnorm_1( j ) ) );
        } else {
          /**
           *                 G(j) could overflow, set GROW to 0.
           **/
          grow = zero;
        }
      }
      grow = xbnd;
    } else {
      /**
       *           A is unit triangular.
       *
       *           Compute GROW = 1/G(j), where G(0) = max{x_1(i), i=1,...,n}.
       **/
      grow = min( one, one / max( xbnd, smlnum ) );
      for (j=jfirst ; jinc>0?j<=jlast:j>=jlast ; j+=jinc) {
        /**
         *              Exit the loop if the growth factor is too small.
         **/
        if( grow<=smlnum )
          goto L_50;
        /**
         *              G(j) = G(j-1)*( 1 + CNORM_1(j) )
         **/
        grow = grow*( one / ( one+cnorm_1( j ) ) );
      }
    }
  L_50:;

  } else {
    /**
     *        Compute the growth in A' * *x = b.
     **/
    if( upper ) {
      jfirst = 1;
      jlast = n;
      jinc = 1;
    } else {
      jfirst = n;
      jlast = 1;
      jinc = -1;
    }

    if( tscal!=one ) {
      grow = zero;
      goto L_80;
    }

    if( nounit ) {
      /**
       *           A is non-unit triangular.
       *
       *           Compute GROW = 1/G(j) and XBND = 1/M(j).
       *           Initially, M(0) = max{x_1(i), i=1,...,n}.
       **/
      grow = one / max( xbnd, smlnum );
      xbnd = grow;
      for (j=jfirst ; jinc>0?j<=jlast:j>=jlast ; j+=jinc) {
        /**
         *              Exit the loop if the growth factor is too small.
         **/
        if( grow<=smlnum )
          goto L_80;
        /**
         *              G(j) = max( G(j-1), M(j-1)*( 1 + CNORM_1(j) ) )
         **/
        xj = one + cnorm_1( j );
        grow = min( grow, xbnd / xj );
        /**
         *              M(j) = M(j-1)*( 1 + CNORM_1(j) ) / abs(A_2(j,j))
         **/
        tjj = abs( a_2( j, j ) );
        if( xj>tjj )
          xbnd = xbnd*( tjj / xj );
      }
      grow = min( grow, xbnd );
    } else {
      /**
       *           A is unit triangular.
       *
       *           Compute GROW = 1/G(j), where G(0) = max{x_1(i), i=1,...,n}.
       **/
      grow = min( one, one / max( xbnd, smlnum ) );
      for (j=jfirst ; jinc>0?j<=jlast:j>=jlast ; j+=jinc) {
        /**
         *              Exit the loop if the growth factor is too small.
         **/
        if( grow<=smlnum )
          goto L_80;
        /**
         *              G(j) = ( 1 + CNORM_1(j) )*G(j-1)
         **/
        xj = one + cnorm_1( j );
        grow = grow / xj;
      }
    }
  L_80:;
  }

  if( ( grow*tscal )>smlnum ) {
    /**
     *        Use the Level 2 BLAS solve if the reciprocal of the bound on
     *        elements of X is not too small.
     **/
    cblas_dtrsv(CblasColMajor, upper? CblasUpper : CblasLower,
                notran? CblasNoTrans : CblasTrans,
                nounit? CblasNonUnit : CblasUnit, n, a, lda, x, 1 );
  } else {
    /**
     *        Use a Level 1 BLAS solve, scaling intermediate results.
     **/
    if( xmax>bignum ) {
      /**
       *           Scale X so that its components are less than or equal to
       *           BIGNUM in absolute value.
       **/
      *scale = bignum / xmax;
      cblas_dscal( n, *scale, x, 1 );
      xmax = bignum;
    }

    if( notran ) {
      /**
       *           Solve A * *x = b
       **/
      for (j=jfirst ; jinc>0?j<=jlast:j>=jlast ; j+=jinc) {
        /**
         *              Compute x_1(j) = b(j) / A_2(j,j), scaling x if necessary.
         **/
        xj = abs( x_1( j ) );
        if( nounit ) {
          tjjs = a_2( j, j )*tscal;
        } else {
          tjjs = tscal;
          if( tscal==one )
            goto L_100;
        }
        tjj = abs( tjjs );
        if( tjj>smlnum ) {
          /**
           *                    abs(A_2(j,j)) > SMLNUM:
           **/
          if( tjj<one ) {
            if( xj>tjj*bignum ) {
              /**
               *                          Scale x by 1/b(j).
               **/
              rec = one / xj;
              cblas_dscal( n, rec, x, 1 );
              *scale = *scale*rec;
              xmax = xmax*rec;
            }
          }
          x_1( j ) = x_1( j ) / tjjs;
          xj = abs( x_1( j ) );
        } else if( tjj>zero ) {
          /**
           *                    0 < abs(A_2(j,j)) <= SMLNUM:
           **/
          if( xj>tjj*bignum ) {
            /**
             *                       Scale x by (1/abs(x_1(j)))*abs(A_2(j,j))*BIGNUM
             *                       to avoid overflow when dividing by A_2(j,j).
             **/
            rec = ( tjj*bignum ) / xj;
            if( cnorm_1( j )>one ) {
              /**
               *                          Scale by 1/CNORM_1(j) to avoid overflow when
               *                          multiplying x_1(j) times column j.
               **/
              rec = rec / cnorm_1( j );
            }
            cblas_dscal( n, rec, x, 1 );
            *scale = *scale*rec;
            xmax = xmax*rec;
          }
          x_1( j ) = x_1( j ) / tjjs;
          xj = abs( x_1( j ) );
        } else {
          /**
           *                    A_2(j,j) = 0:  Set x_1(1:n) = 0, x_1(j) = 1, and
           *                    *scale = 0, and compute a solution to A**x = 0.
           **/
          for (i=1 ; i<=n ; i+=1) {
            x_1( i ) = zero;
          }
          x_1( j ) = one;
          xj = one;
          *scale = zero;
          xmax = zero;
        }
      L_100:
        /**
         *              Scale x if necessary to avoid overflow when adding a
         *              multiple of column j of A.
         **/
        if( xj>one ) {
          rec = one / xj;
          if( cnorm_1( j )>( bignum-xmax )*rec ) {
            /**
             *                    Scale x by 1/(2*abs(x_1(j))).
             **/
            rec = rec*half;
            cblas_dscal( n, rec, x, 1 );
            *scale = *scale*rec;
          }
        } else if( xj*cnorm_1( j )>( bignum-xmax ) ) {
          /**
           *                 Scale x by 1/2.
           **/
          cblas_dscal( n, half, x, 1 );
          *scale = *scale*half;
        }

        if( upper ) {
          if( j>1 ) {
            /**
             *                    Compute the update
             *                       x_1(1:j-1) := x_1(1:j-1) - x_1(j) * A_2(1:j-1,j)
             **/
            cblas_daxpy( j-1, -x_1( j )*tscal, &a_2( 1, j ), 1, x,
                  1 );
            i = 1+cblas_idamax( j-1, x, 1 );
            xmax = abs( x_1( i ) );
          }
        } else {
          if( j<n ) {
            /**
             *                    Compute the update
             *                       x_1(j+1:n) := x_1(j+1:n) - x_1(j) * A_2(j+1:n,j)
             **/
            cblas_daxpy( n-j, -x_1( j )*tscal, &a_2( j+1, j ), 1,
                  &x_1( j+1 ), 1 );
            i = j + 1+cblas_idamax( n-j, &x_1( j+1 ), 1 );
            xmax = abs( x_1( i ) );
          }
        }
      }

    } else {
      /**
       *           Solve A' * *x = b
       **/
      for (j=jfirst ; jinc>0?j<=jlast:j>=jlast ; j+=jinc) {
        /**
         *              Compute x_1(j) = b(j) - sum A_2(k,j)*x_1(k).
         *                                    k<>j
         **/
        xj = abs( x_1( j ) );
        uscal = tscal;
        rec = one / max( xmax, one );
        if( cnorm_1( j )>( bignum-xj )*rec ) {
          /**
           *                 If x_1(j) could overflow, scale x by 1/(2*XMAX).
           **/
          rec = rec*half;
          if( nounit ) {
            tjjs = a_2( j, j )*tscal;
          } else {
            tjjs = tscal;
          }
          tjj = abs( tjjs );
          if( tjj>one ) {
            /**
             *                       Divide by A_2(j,j) when scaling x if A_2(j,j) > 1.
             **/
            rec = min( one, rec*tjj );
            uscal = uscal / tjjs;
          }
          if( rec<one ) {
            cblas_dscal( n, rec, x, 1 );
            *scale = *scale*rec;
            xmax = xmax*rec;
          }
        }

        sumj = zero;
        if( uscal==one ) {
          /**
           *                 If the scaling needed for A in the dot product is 1,
           *                 call CBLAS_DDOT to perform the dot product.
           **/
          if( upper ) {
            sumj = cblas_ddot( j-1, &a_2( 1, j ), 1, x, 1 );
          } else if( j<n ) {
            sumj = cblas_ddot( n-j, &a_2( j+1, j ), 1, &x_1( j+1 ), 1 );
          }
        } else {
          /**
           *                 Otherwise, use in-line code for the dot product.
           **/
          if( upper ) {
            for (i=1 ; i<=j - 1 ; i+=1) {
              sumj = sumj + ( a_2( i, j )*uscal )*x_1( i );
            }
          } else if( j<n ) {
            for (i=j + 1 ; i<=n ; i+=1) {
              sumj = sumj + ( a_2( i, j )*uscal )*x_1( i );
            }
          }
        }

        if( uscal==tscal ) {
          /**
           *                 Compute x_1(j) := ( x_1(j) - sumj ) / A_2(j,j) if 1/A_2(j,j)
           *                 was not used to scale the dotproduct.
           **/
          x_1( j ) = x_1( j ) - sumj;
          xj = abs( x_1( j ) );
          if( nounit ) {
            tjjs = a_2( j, j )*tscal;
          } else {
            tjjs = tscal;
            if( tscal==one )
              goto L_150;
          }
          /**
           *                    Compute x_1(j) = x_1(j) / A_2(j,j), scaling if necessary.
           **/
          tjj = abs( tjjs );
          if( tjj>smlnum ) {
            /**
             *                       abs(A_2(j,j)) > SMLNUM:
             **/
            if( tjj<one ) {
              if( xj>tjj*bignum ) {
                /**
                 *                             Scale X by 1/abs(x_1(j)).
                 **/
                rec = one / xj;
                cblas_dscal( n, rec, x, 1 );
                *scale = *scale*rec;
                xmax = xmax*rec;
              }
            }
            x_1( j ) = x_1( j ) / tjjs;
          } else if( tjj>zero ) {
            /**
             *                       0 < abs(A_2(j,j)) <= SMLNUM:
             **/
            if( xj>tjj*bignum ) {
              /**
               *                          Scale x by (1/abs(x_1(j)))*abs(A_2(j,j))*BIGNUM.
               **/
              rec = ( tjj*bignum ) / xj;
              cblas_dscal( n, rec, x, 1 );
              *scale = *scale*rec;
              xmax = xmax*rec;
            }
            x_1( j ) = x_1( j ) / tjjs;
          } else {
            /**
             *                       A_2(j,j) = 0:  Set x_1(1:n) = 0, x_1(j) = 1, and
             *                       *scale = 0, and compute a solution to A'**x = 0.
             **/
            for (i=1 ; i<=n ; i+=1) {
              x_1( i ) = zero;
            }
            x_1( j ) = one;
            *scale = zero;
            xmax = zero;
          }
        L_150:;
        } else {
          /**
           *                 Compute x_1(j) := x_1(j) / A_2(j,j)  - sumj if the dot
           *                 product has already been divided by 1/A_2(j,j).
           **/
          x_1( j ) = x_1( j ) / tjjs - sumj;
        }
        xmax = max( xmax, abs( x_1( j ) ) );
      }
    }
    *scale = *scale / tscal;
  }
  /**
   *     Scale the column norms by 1/TSCAL for return.
   **/
  if( tscal!=one ) {
    cblas_dscal( n, one / tscal, cnorm, 1 );
  }

  return;
  /**
   *     End of DLATRS
   **/
}



double   dlamch( char cmach )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     October 31, 1992
   *
   *     .. Scalar Arguments ..*/
  double dlamch_R;
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLAMCH determines double precision machine parameters.
   *
   *  Arguments
   *  =========
   *
   *  CMACH   (input) CHARACTER*1
   *          Specifies the value to be returned by DLAMCH:
   *          = 'E' or 'e',   DLAMCH := eps
   *          = 'S' or 's ,   DLAMCH := sfmin
   *          = 'B' or 'b',   DLAMCH := base
   *          = 'P' or 'p',   DLAMCH := eps*base
   *          = 'N' or 'n',   DLAMCH := t
   *          = 'R' or 'r',   DLAMCH := rnd
   *          = 'M' or 'm',   DLAMCH := emin
   *          = 'U' or 'u',   DLAMCH := rmin
   *          = 'L' or 'l',   DLAMCH := emax
   *          = 'O' or 'o',   DLAMCH := rmax
   *
   *          where
   *
   *          eps   = relative machine precision
   *          sfmin = safe minimum, such that 1/sfmin does not overflow
   *          base  = base of the machine
   *          prec  = eps*base
   *          t     = number of (base) digits in the mantissa
   *          rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
   *          emin  = minimum exponent before (gradual) underflow
   *          rmin  = underflow threshold - base**(emin-1)
   *          emax  = largest exponent before overflow
   *          rmax  = overflow threshold  - (base**emax)*(1-eps)
   *
   * =====================================================================
   *
   *     .. Parameters ..*/
#undef one
#define one 1.0e+0
#undef zero
#define zero 0.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  int       lrnd;
  long      beta, imax, imin, it;
  double    rmach=0.0, small;
  /**     ..
   *     .. Save statement ..*/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  static int first= 1;
  static double eps, sfmin, base, t, rnd, emin, rmin, emax, rmax, prec;
  /**     ..
   *     .. Executable Statements ..
   **/
  if( first ) {
    first = 0;
    dlamc2( &beta, &it, &lrnd, &eps, &imin, &rmin, &imax, &rmax );
    base = beta;
    t = it;
    if( lrnd ) {
      rnd = one;
      eps = ( pow(base,(double)( 1-it )) ) / 2;
    } else {
      rnd = zero;
      eps = pow(base,(double)( 1-it ));
    }
    prec = eps*base;
    emin = imin;
    emax = imax;
    sfmin = rmin;
    small = one / rmax;
    if( small>=sfmin ) {
      /**
       *           Use SMALL plus a bit, to avoid the possibility of rounding
       *           causing overflow when computing  1/sfmin.
       **/
      sfmin = small*( one+eps );
    }
  }

  if( lsame( cmach, 'e' ) ) {
    rmach = eps;
  } else if( lsame( cmach, 's' ) ) {
    rmach = sfmin;
  } else if( lsame( cmach, 'b' ) ) {
    rmach = base;
  } else if( lsame( cmach, 'p' ) ) {
    rmach = prec;
  } else if( lsame( cmach, 'n' ) ) {
    rmach = t;
  } else if( lsame( cmach, 'r' ) ) {
    rmach = rnd;
  } else if( lsame( cmach, 'm' ) ) {
    rmach = emin;
  } else if( lsame( cmach, 'u' ) ) {
    rmach = rmin;
  } else if( lsame( cmach, 'l' ) ) {
    rmach = emax;
  } else if( lsame( cmach, 'o' ) ) {
    rmach = rmax;
  }

  dlamch_R = rmach;
  return dlamch_R;
  /**
   *     End of DLAMCH
   **/
}
/**
************************************************************************
**/



void dlamc1( long *beta, long *t, int *rnd, int *ieee1 )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     October 31, 1992
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLAMC1 determines the machine parameters given by BETA, T, RND, and
   *  IEEE1.
   *
   *  Arguments
   *  =========
   *
   *  BETA    (output) INTEGER
   *          The base of the machine.
   *
   *  T       (output) INTEGER
   *          The number of ( BETA ) digits in the mantissa.
   *
   *  RND     (output) LOGICAL
   *          Specifies whether proper rounding  ( RND = .TRUE. )  or
   *          chopping  ( RND = .FALSE. )  occurs in addition. This may not
   *          be a reliable guide to the way in which the machine performs
   *          its arithmetic.
   *
   *  IEEE1   (output) LOGICAL
   *          Specifies whether rounding appears to be done in the IEEE
   *          'round to nearest' style.
   *
   *  Further Details
   *  ===============
   *
   *  The routine is based on the routine  ENVRON  by Malcolm and
   *  incorporates suggestions by Gentleman and Marovich. See
   *
   *     Malcolm M. A. (1972) Algorithms to reveal properties of
   *        floating-point arithmetic. Comms. of the ACM, 15, 949-951.
   *
   *     Gentleman W. M. and Marovich S. B. (1974) More on algorithms
   *        that reveal properties of floating point arithmetic units.
   *        Comms. of the ACM, 17, 276-277.
   *
   * =====================================================================
   *
   *     .. Local Scalars ..*/
#undef one
  double    a, b, c, f, one, qtr, savec, t1, t2;
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  static int first= 1;
  static int lieee1, lrnd;
  static long lbeta, lt;
  /**     ..
   *     .. Executable Statements ..
   **/
  if( first ) {
    first = 0;
    one = 1;
    /**
     *        LBETA,  LIEEE1,  LT and  LRND  are the  local values  of  BETA,
     *        IEEE1, T and RND.
     *
     *        Throughout this routine  we use the function  DLAMC3  to ensure
     *        that relevant values are  stored and not held in registers,  or
     *        are not affected by optimizers.
     *
     *        Compute  a = 2.0**m  with the  smallest positive integer m such
     *        that
     *
     *           fl( a + 1.0 ) = a.
     **/
    a = 1;
    c = 1;
    /**
     *+       WHILE( C.EQ.ONE )LOOP*/
  L_10:
    if( c==one ) {
      a = 2*a;
      c = dlamc3( a, one );
      c = dlamc3( c, -a );
      goto L_10;
    }
    /**+       END WHILE
     *
     *        Now compute  b = 2.0**m  with the smallest positive integer m
     *        such that
     *
     *           fl( a + b ) .gt. a.
     **/
    b = 1;
    c = dlamc3( a, b );
    /**
     *+       WHILE( C.EQ.A )LOOP*/
  L_20:
    if( c==a ) {
      b = 2*b;
      c = dlamc3( a, b );
      goto L_20;
    }
    /**+       END WHILE
     *
     *        Now compute the base.  a and c  are neighbouring floating point
     *        numbers  in the  interval  ( beta**t, beta**( t + 1 ) )  and so
     *        their difference is beta. Adding 0.25 to c is to ensure that it
     *        is truncated to beta and not ( beta - 1 ).
     **/
    qtr = one / 4;
    savec = c;
    c = dlamc3( c, -a );
    lbeta = (long)(c + qtr);
    /**
     *        Now determine whether rounding or chopping occurs,  by adding a
     *        bit  less  than  beta/2  and a  bit  more  than  beta/2  to  a.
     **/
    b = lbeta;
    f = dlamc3( b / 2, -b / 100 );
    c = dlamc3( f, a );
    if( c==a ) {
      lrnd = 1;
    } else {
      lrnd = 0;
    }
    f = dlamc3( b / 2, b / 100 );
    c = dlamc3( f, a );
    if( ( lrnd ) && ( c==a ) )
      lrnd = 0;
    /**
     *        Try and decide whether rounding is done in the  IEEE  'round to
     *        nearest' style. B/2 is half a unit in the last place of the two
     *        numbers A and SAVEC. Furthermore, A is even, i.e. has last  bit
     *        zero, and SAVEC is odd. Thus adding B/2 to A should not  change
     *        A, but adding B/2 to SAVEC should change SAVEC.
     **/
    t1 = dlamc3( b / 2, a );
    t2 = dlamc3( b / 2, savec );
    lieee1 = ( t1==a ) && ( t2>savec ) && lrnd;
    /**
     *        Now find  the  mantissa, t.  It should  be the  integer part of
     *        log to the base beta of a,  however it is safer to determine  t
     *        by powering.  So we find t as the smallest positive integer for
     *        which
     *
     *           fl( beta**t + 1.0 ) = 1.0.
     **/
    lt = 0;
    a = 1;
    c = 1;
    /**
     *+       WHILE( C.EQ.ONE )LOOP*/
  L_30:
    if( c==one ) {
      lt = lt + 1;
      a = a*lbeta;
      c = dlamc3( a, one );
      c = dlamc3( c, -a );
      goto L_30;
    }
    /**+       END WHILE
     **/
  }

  *beta = lbeta;
  *t = lt;
  *rnd = lrnd;
  *ieee1 = lieee1;
  return;
  /**
   *     End of DLAMC1
   **/
}
/**
************************************************************************
**/



void dlamc2( long *beta, long *t, int *rnd, double *eps,
            long *emin, double *rmin, long *emax, double *rmax )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     October 31, 1992
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLAMC2 determines the machine parameters specified in its argument
   *  list.
   *
   *  Arguments
   *  =========
   *
   *  BETA    (output) INTEGER
   *          The base of the machine.
   *
   *  T       (output) INTEGER
   *          The number of ( BETA ) digits in the mantissa.
   *
   *  RND     (output) LOGICAL
   *          Specifies whether proper rounding  ( RND = .TRUE. )  or
   *          chopping  ( RND = .FALSE. )  occurs in addition. This may not
   *          be a reliable guide to the way in which the machine performs
   *          its arithmetic.
   *
   *  EPS     (output) DOUBLE PRECISION
   *          The smallest positive number such that
   *
   *             fl( 1.0 - EPS ) .LT. 1.0,
   *
   *          where fl denotes the computed value.
   *
   *  EMIN    (output) INTEGER
   *          The minimum exponent before (gradual) underflow occurs.
   *
   *  RMIN    (output) DOUBLE PRECISION
   *          The smallest normalized number for the machine, given by
   *          BASE**( EMIN - 1 ), where  BASE  is the floating point value
   *          of BETA.
   *
   *  EMAX    (output) INTEGER
   *          The maximum exponent before overflow occurs.
   *
   *  RMAX    (output) DOUBLE PRECISION
   *          The largest positive number for the machine, given by
   *          BASE**EMAX * ( 1 - EPS ), where  BASE  is the floating point
   *          value of BETA.
   *
   *  Further Details
   *  ===============
   *
   *  The computation of  EPS  is based on a routine PARANOIA by
   *  W. Kahan of the University of California at Berkeley.
   *
   * =====================================================================
   *
   *     .. Local Scalars ..*/
#undef one
#undef zero
#undef half
  int            ieee, lieee1;
  long           gnmin, gpmin, i, ngnmin, ngpmin;
  double  a, b, c, half, one, rbase, sixth, small, third, two, zero;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, max, min;*/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  static int first= 1;
  /* static int iwarn= 0; */
  static int     lrnd;
  static long    lbeta, lemax, lemin, lt;
  static double  lrmax, lrmin, leps;
  /**     ..
   *     .. Executable Statements ..
   **/
  if( first ) {
    first = 0;
    zero = 0;
    one = 1;
    two = 2;
    /**
     *        LBETA, LT, LRND, LEPS, LEMIN and LRMIN  are the local values of
     *        BETA, T, RND, EPS, EMIN and RMIN.
     *
     *        Throughout this routine  we use the function  DLAMC3  to ensure
     *        that relevant values are stored  and not held in registers,  or
     *        are not affected by optimizers.
     *
     *        DLAMC1 returns the parameters  LBETA, LT, LRND and LIEEE1.
     **/
    dlamc1( &lbeta, &lt, &lrnd, &lieee1 );
    /**
     *        Start to find EPS.
     **/
    b = lbeta;
    a = pow(b,(double)( -lt ));
    leps = a;
    /**
     *        Try some tricks to see whether or not this is the correct  EPS.
     **/
    b = two / 3;
    half = one / 2;
    sixth = dlamc3( b, -half );
    third = dlamc3( sixth, sixth );
    b = dlamc3( third, -half );
    b = dlamc3( b, sixth );
    b = abs( b );
    if( b<leps )
      b = leps;

    leps = 1;
    /**
     *+       WHILE( ( LEPS.GT.B ).AND.( B.GT.ZERO ) )LOOP*/
  L_10:
    if( ( leps>b ) && ( b>zero ) ) {
      leps = b;
      c = dlamc3( half*leps, ( two*two*two*two*two )*( leps*leps ) );
      c = dlamc3( half, -c );
      b = dlamc3( half, c );
      c = dlamc3( half, -b );
      b = dlamc3( half, c );
      goto L_10;
    }
    /**+       END WHILE
     **/
    if( a<leps )
      leps = a;
    /**
     *        Computation of EPS complete.
     *
     *        Now find  EMIN.  Let A = + or - 1, and + or - (1 + BASE**(-3)).
     *        Keep dividing  A by BETA until (gradual) underflow occurs. This
     *        is detected when we cannot recover the previous A.
     **/
    rbase = one / lbeta;
    small = one;
    for (i=1 ; i<=3 ; i+=1) {
      small = dlamc3( small*rbase, zero );
    }
    a = dlamc3( one, small );
    dlamc4( &ngpmin, one, lbeta );
    dlamc4( &ngnmin, -one, lbeta );
    dlamc4( &gpmin, a, lbeta );
    dlamc4( &gnmin, -a, lbeta );
    ieee = 0;

    if( ( ngpmin==ngnmin ) && ( gpmin==gnmin ) ) {
      if( ngpmin==gpmin ) {
        lemin = ngpmin;
        /**            ( Non twos-complement machines, no gradual underflow;
         *              e.g.,  VAX )*/
      } else if( ( gpmin-ngpmin )==3 ) {
        lemin = ngpmin - 1 + lt;
        ieee = 1;
        /**            ( Non twos-complement machines, with gradual underflow;
         *              e.g., IEEE standard followers )*/
      } else {
        lemin = min( ngpmin, gpmin );
        /**            ( A guess; no known machine )*/
        /* iwarn = 1; */
      }

    } else if( ( ngpmin==gpmin ) && ( ngnmin==gnmin ) ) {
      if( abs( ngpmin-ngnmin )==1 ) {
        lemin = max( ngpmin, ngnmin );
        /**            ( Twos-complement machines, no gradual underflow;
         *              e.g., CYBER 205 )*/
      } else {
        lemin = min( ngpmin, ngnmin );
        /**            ( A guess; no known machine )*/
        /* iwarn = 1; */
      }

    } else if( ( abs( ngpmin-ngnmin )==1 ) &&
              ( gpmin==gnmin ) ) {
      if( ( gpmin-min( ngpmin, ngnmin ) )==3 ) {
        lemin = max( ngpmin, ngnmin ) - 1 + lt;
        /**            ( Twos-complement machines with gradual underflow;
         *              no known machine )*/
      } else {
        lemin = min( ngpmin, ngnmin );
        /**            ( A guess; no known machine )*/
        /* iwarn = 1; */
      }

    } else {
      lemin = min(  min(ngpmin, ngnmin), min(gpmin, gnmin) );
      /**         ( A guess; no known machine )*/
      /* iwarn = 1; */
    }
    /*****
     ** Comment out this if block if EMIN is ok
     *         IF( IWARN ) THEN
     *            FIRST = .TRUE.
     *            WRITE( 6, FMT = 9999 )LEMIN
     *         END IF
     ****
     *
     *        Assume IEEE arithmetic if we found denormalised  numbers above,
     *        or if arithmetic seems to round in the  IEEE style,  determined
     *        in routine DLAMC1. A true IEEE machine should have both  things
     *        true; however, faulty machines may have one or the other.
     **/
    ieee = ieee || lieee1;
    /**
     *        Compute  RMIN by successive division by  BETA. We could compute
     *        RMIN as BASE**( EMIN - 1 ),  but some machines underflow during
     *        this computation.
     **/
    lrmin = 1;
    for (i=1 ; i<=1 - lemin ; i+=1) {
      lrmin = dlamc3( lrmin*rbase, zero );
    }
    /**
     *        Finally, call DLAMC5 to compute EMAX and RMAX.
     **/
    dlamc5( lbeta, lt, lemin, ieee, &lemax, &lrmax );
  }

  *beta = lbeta;
  *t = lt;
  *rnd = lrnd;
  *eps = leps;
  *emin = lemin;
  *rmin = lrmin;
  *emax = lemax;
  *rmax = lrmax;

  return;
  /**
   * 9999 FORMAT( / / ' WARNING. The value EMIN may be incorrect:-',
   *     $      '  EMIN = ', I8, /
   *     $      ' If, after inspection, the value EMIN looks',
   *     $      ' acceptable please comment out ',
   *     $      / ' the IF block as marked within the code of routine',
   *     $      ' DLAMC2,', / ' otherwise supply EMIN explicitly.', / )
   *
   *     End of DLAMC2
   **/
}
/**
************************************************************************
**/



void dlamc4( long *emin, double start, long base )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     October 31, 1992
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLAMC4 is a service routine for DLAMC2.
   *
   *  Arguments
   *  =========
   *
   *  EMIN    (output) EMIN
   *          The minimum exponent before (gradual) underflow, computed by
   *          setting A = START and dividing by BASE until the previous A
   *          can not be recovered.
   *
   *  START   (input) DOUBLE PRECISION
   *          The starting point for determining EMIN.
   *
   *  BASE    (input) INTEGER
   *          The base of the machine.
   *
   * =====================================================================
   *
   *     .. Local Scalars ..*/
#undef one
#undef zero
  long            i;
  double    a, b1, b2, c1, c2, e1, e2, one, rbase, zero;
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  a = start;
  one = 1;
  rbase = one / base;
  zero = 0;
  *emin = 1;
  b1 = dlamc3( a*rbase, zero );
  c1 = a;
  c2 = a;
  e1 = a;
  e2 = a;
  /**+    WHILE( ( C1.EQ.A ).AND.( C2.EQ.A ).AND.
   *    $       ( D1.EQ.A ).AND.( D2.EQ.A )      )LOOP*/
 L_10:
  if( ( c1==a ) && ( c2==a ) && ( e1==a ) &&
     ( e2==a ) ) {
    *emin = *emin - 1;
    a = b1;
    b1 = dlamc3( a / base, zero );
    c1 = dlamc3( b1*base, zero );
    e1 = zero;
    for (i=1 ; i<=base ; i+=1) {
      e1 = e1 + b1;
    }
    b2 = dlamc3( a*rbase, zero );
    c2 = dlamc3( b2 / rbase, zero );
    e2 = zero;
    for (i=1 ; i<=base ; i+=1) {
      e2 = e2 + b2;
    }
    goto L_10;
  }
  /**+    END WHILE
   **/
  return;
  /**
   *     End of DLAMC4
   **/
}
/**
************************************************************************
**/



void dlamc5( long beta, long p, long emin, int ieee,
            long *emax, double *rmax )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     October 31, 1992
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLAMC5 attempts to compute RMAX, the largest machine floating-point
   *  number, without overflow.  It assumes that EMAX + abs(EMIN) sum
   *  approximately to a power of 2.  It will fail on machines where this
   *  assumption does not hold, for example, the Cyber 205 (EMIN = -28625,
   *  EMAX = 28718).  It will also fail if the value supplied for EMIN is
   *  too large (i.e. too close to zero), probably with overflow.
   *
   *  Arguments
   *  =========
   *
   *  BETA    (input) INTEGER
   *          The base of floating-point arithmetic.
   *
   *  P       (input) INTEGER
   *          The number of base BETA digits in the mantissa of a
   *          floating-point value.
   *
   *  EMIN    (input) INTEGER
   *          The minimum exponent before (gradual) underflow.
   *
   *  IEEE    (input) LOGICAL
   *          A logical flag specifying whether or not the arithmetic
   *          system is thought to comply with the IEEE standard.
   *
   *  EMAX    (output) INTEGER
   *          The largest exponent before overflow
   *
   *  RMAX    (output) DOUBLE PRECISION
   *          The largest machine floating-point number.
   *
   * =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e0
#undef one
#define one 1.0e0
  /**     ..
   *     .. Local Scalars ..*/
  long            exbits, expsum, i, lexp, nbits, try, uexp;
  double    oldy=0.0, recbas, y, z;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          mod;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     First compute LEXP and UEXP, two powers of 2 that bound
   *     abs(EMIN). We then assume that EMAX + abs(EMIN) will sum
   *     approximately to the bound that is closest to abs(EMIN).
   *     (EMAX is the exponent of the required number RMAX).
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  lexp = 1;
  exbits = 1;
 L_10:
  try = lexp*2;
  if( try<=( -emin ) ) {
    lexp = try;
    exbits = exbits + 1;
    goto L_10;
  }
  if( lexp==-emin ) {
    uexp = lexp;
  } else {
    uexp = try;
    exbits = exbits + 1;
  }
  /**
   *     Now -LEXP is less than or equal to EMIN, and -UEXP is greater
   *     than or equal to EMIN. EXBITS is the number of bits needed to
   *     store the exponent.
   **/
  if( ( uexp+emin )>( -lexp-emin ) ) {
    expsum = 2*lexp;
  } else {
    expsum = 2*uexp;
  }
  /**
   *     EXPSUM is the exponent range, approximately equal to
   *     EMAX - EMIN + 1 .
   **/
  *emax = expsum + emin - 1;
  nbits = 1 + exbits + p;
  /**
   *     NBITS is the total number of bits needed to store a
   *     floating-point number.
   **/
  if( ( mod( nbits, 2 )==1 ) && ( beta==2 ) ) {
    /**
     *        Either there are an odd number of bits used to store a
     *        floating-point number, which is unlikely, or some bits are
     *        not used in the representation of numbers, which is possible,
     *        (e.g. Cray machines) or the mantissa has an implicit bit,
     *        (e.g. IEEE machines, Dec Vax machines), which is perhaps the
     *        most likely. We have to assume the last alternative.
     *        If this is true, then we need to reduce EMAX by one because
     *        there must be some way of representing zero in an implicit-bit
     *        system. On machines like Cray, we are reducing EMAX by one
     *        unnecessarily.
     **/
    *emax = *emax - 1;
  }

  if( ieee ) {
    /**
     *        Assume we are on an IEEE machine which reserves one exponent
     *        for infinity and NaN.
     **/
    *emax = *emax - 1;
  }
  /**
   *     Now create RMAX, the largest machine number, which should
   *     be equal to (1.0 - BETA**(-P)) * BETA**EMAX .
   *
   *     First compute 1.0 - BETA**(-P), being careful that the
   *     result is less than 1.0 .
   **/
  recbas = one / beta;
  z = beta - one;
  y = zero;
  for (i=1 ; i<=p ; i+=1) {
    z = z*recbas;
    if( y<one )
      oldy = y;
    y = dlamc3( y, z );
  }
  if( y>=one )
    y = oldy;
  /**
   *     Now multiply by BETA**EMAX to get RMAX.
   **/
  for (i=1 ; i<=*emax ; i+=1) {
    y = dlamc3( y*beta, zero );
  }

  *rmax = y;
  return;
  /**
   *     End of DLAMC5
   **/
}



void dlacon( long n, double v[], double x[], long isgn[],
            double *est, long *kase )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     February 29, 1992
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *     .. Array Arguments ..*/
#undef isgn_1
#define isgn_1(a1) isgn[a1-1]
#undef x_1
#define x_1(a1) x[a1-1]
#undef v_1
#define v_1(a1) v[a1-1]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLACON estimates the 1-norm of a square, real matrix A.
   *  Reverse communication is used for evaluating matrix-vector products.
   *
   *  Arguments
   *  =========
   *
   *  N      (input) INTEGER
   *         The order of the matrix.  N >= 1.
   *
   *  V      (workspace) DOUBLE PRECISION array, dimension (N)
   *         On the final return, V = A*W,  where  EST = norm(V)/norm(W)
   *         (W is not returned).
   *
   *  X      (input/output) DOUBLE PRECISION array, dimension (N)
   *         On an intermediate return, X should be overwritten by
   *               A * X,   if KASE=1,
   *               A' * X,  if KASE=2,
   *         and DLACON must be re-called with all the other parameters
   *         unchanged.
   *
   *  ISGN   (workspace) INTEGER array, dimension (N)
   *
   *  EST    (output) DOUBLE PRECISION
   *         An estimate (a lower bound) for norm(A).
   *
   *  KASE   (input/output) INTEGER
   *         On the initial call to DLACON, KASE should be 0.
   *         On an intermediate return, KASE will be 1 or 2, indicating
   *         whether X should be overwritten by A * X  or A' * X.
   *         On the final return from DLACON, KASE will again be 0.
   *
   *  Further Details
   *  ======= =======
   *
   *  Contributed by Nick Higham, University of Manchester.
   *  Originally named SONEST, dated March 16, 1988.
   *
   *  Reference: N.J. Higham, "FORTRAN codes for estimating the one-norm of
   *  a real or complex matrix, with applications to condition estimation",
   *  ACM Trans. Math. Soft., vol. 14, no. 4, pp. 381-396, December 1988.
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef itmax
#define itmax 5
#undef zero
#define zero 0.0e+0
#undef one
#define one 1.0e+0
#undef two
#define two 2.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  static long      i, iter, j, jlast, jump;
  static double    altsgn, estold, temp;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, dble, nint, sign;*/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  /**     ..
   *     .. Executable Statements ..
   **/
  if( *kase==0 ) {
    for (i=1 ; i<=n ; i+=1) {
      x_1( i ) = one / dble( n );
    }
    *kase = 1;
    jump = 1;
    return;
  }

  switch (jump) {
  case 1: goto L_20;
  case 2: goto L_40;
  case 3: goto L_70;
  case 4: goto L_110;
  case 5: goto L_140;
  }
  /**
   *     ................ ENTRY   (JUMP = 1)
   *     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY A*X.
   **/
 L_20:
  if( n==1 ) {
    v_1( 1 ) = x_1( 1 );
    *est = abs( v_1( 1 ) );
    /**        ... QUIT*/
    goto L_150;
  }
  *est = cblas_dasum( n, x, 1 );

  for (i=1 ; i<=n ; i+=1) {
    x_1( i ) = sign( one, x_1( i ) );
    isgn_1( i ) = nint( x_1( i ) );
  }
  *kase = 2;
  jump = 2;
  return;
  /**
   *     ................ ENTRY   (JUMP = 2)
   *     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY TRANDPOSE(A)*X.
   **/
 L_40:
  j = 1+cblas_idamax( n, x, 1 );
  iter = 2;
  /**
   *     MAIN LOOP - ITERATIONS 2,3,...,ITMAX.
   **/
 L_50:
  for (i=1 ; i<=n ; i+=1) {
    x_1( i ) = zero;
  }
  x_1( j ) = one;
  *kase = 1;
  jump = 3;
  return;
  /**
   *     ................ ENTRY   (JUMP = 3)
   *     X HAS BEEN OVERWRITTEN BY A*X.
   **/
 L_70:
  cblas_dcopy( n, x, 1, v, 1 );
  estold = *est;
  *est = cblas_dasum( n, v, 1 );
  for (i=1 ; i<=n ; i+=1) {
    if( nint( sign( one, x_1( i ) ) )!=isgn_1( i ) )
      goto L_90;
  }
  /**     REPEATED SIGN VECTOR DETECTED, HENCE ALGORITHM HAS CONVERGED.*/
  goto L_120;

 L_90:
  /**     TEST FOR CYCLING.*/
  if( *est<=estold )
    goto L_120;

  for (i=1 ; i<=n ; i+=1) {
    x_1( i ) = sign( one, x_1( i ) );
    isgn_1( i ) = nint( x_1( i ) );
  }
  *kase = 2;
  jump = 4;
  return;
  /**
   *     ................ ENTRY   (JUMP = 4)
   *     X HAS BEEN OVERWRITTEN BY TRANDPOSE(A)*X.
   **/
 L_110:
  jlast = j;
  j = 1+cblas_idamax( n, x, 1 );
  if( ( x_1( jlast )!=abs( x_1( j ) ) ) && ( iter<itmax ) ) {
    iter = iter + 1;
    goto L_50;
  }
  /**
   *     ITERATION COMPLETE.  FINAL STAGE.
   **/
 L_120:
  altsgn = one;
  for (i=1 ; i<=n ; i+=1) {
    x_1( i ) = altsgn*( one+dble( i-1 ) / dble( n-1 ) );
    altsgn = -altsgn;
  }
  *kase = 1;
  jump = 5;
  return;
  /**
   *     ................ ENTRY   (JUMP = 5)
   *     X HAS BEEN OVERWRITTEN BY A*X.
   **/
 L_140:
  temp = two*( cblas_dasum( n, x, 1 ) / dble( 3*n ) );
  if( temp>*est ) {
    cblas_dcopy( n, x, 1, v, 1 );
    *est = temp;
  }

 L_150:
  *kase = 0;
  return;
  /**
   *     End of DLACON
   **/
}

/* Defeat optimizers by splitting dlamc3 across files and forcing the
   result to pass through a memory location.
   Actually, it would be more correct to go after the comparison
   operation than the sum, but this perturbs the existing code less.  */
extern void dlamc3_worker( double a, double b );
extern double dlamc3_sum;
void dlamc3_worker( double a, double b )
{
  dlamc3_sum= a+b;
}
