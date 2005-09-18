/*
 * $Id: dgtsv.c,v 1.1 2005-09-18 22:04:48 dhmunro Exp $
 * LAPACK routine to solve a tridiagonal matrix.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "dg.h"


/*-----Fortran intrinsics converted-----*/
#define abs(x) ((x)>=0?(x):-(x))
#define max(x,y) ((x)>(y)?(x):(y))
/*-----end of Fortran intrinsics-----*/



void dgtsv( long n, long nrhs, double dl[], double d[], double du[],
           double b[], long ldb, long *info )
{
  /**
   *  -- LAPACK routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     March 31, 1993
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *     .. Array Arguments ..*/
#undef du_1
#define du_1(a1) du[a1-1]
#undef dl_1
#define dl_1(a1) dl[a1-1]
#undef d_1
#define d_1(a1) d[a1-1]
#undef b_2
#define b_2(a1,a2) b[a1-1+ldb*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGTSV  solves the equation
   *
   *     A*X = B,
   *
   *  where A is an N-by-N tridiagonal matrix, by Gaussian elimination with
   *  partial pivoting.
   *
   *  Note that the equation  A'*X = B  may be solved by interchanging the
   *  order of the arguments DU and DL.
   *
   *  Arguments
   *  =========
   *
   *  N       (input) INTEGER
   *          The order of the matrix A.  N >= 0.
   *
   *  NRHS    (input) INTEGER
   *          The number of right hand sides, i.e., the number of columns
   *          of the matrix B.  NRHS >= 0.
   *
   *  DL_1      (input/output) DOUBLE PRECISION array, dimension (N-1)
   *          On entry, DL must contain the (n-1) subdiagonal elements of
   *          A.
   *          On exit, DL is overwritten by the (n-2) elements of the
   *          second superdiagonal of the upper triangular matrix U from
   *          the LU factorization of A, in DL_1(1), ..., DL_1(n-2).
   *
   *  D_1       (input/output) DOUBLE PRECISION array, dimension (N)
   *          On entry, D must contain the diagonal elements of A.
   *          On exit, D is overwritten by the n diagonal elements of U.
   *
   *  DU_1      (input/output) DOUBLE PRECISION array, dimension (N-1)
   *          On entry, DU must contain the (n-1) superdiagonal elements
   *          of A.
   *          On exit, DU is overwritten by the (n-1) elements of the first
   *          superdiagonal of U.
   *
   *  B_2       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
   *          On entry, the N-by-NRHS right hand side matrix B.
   *          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
   *
   *  LDB     (input) INTEGER
   *          The leading dimension of the array B.  LDB >= max(1,N).
   *
   *  INFO    (output)
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument had an illegal value
   *          > 0:  if INFO = i, U(i,i) is exactly zero, and the solution
   *                has not been computed.  The factorization has not been
   *                completed unless i = N.
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  long            j, k;
  double    mult, temp;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, max;*/
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  if( n<0 ) {
    *info = -1;
  } else if( nrhs<0 ) {
    *info = -2;
  } else if( ldb<max( 1, n ) ) {
    *info = -7;
  }
  if( *info!=0 ) {
    xerbla( "dgtsv ", -*info );
    return;
  }

  if( n==0 )
    return;

  for (k=1 ; k<=n - 1 ; k+=1) {
    if( dl_1( k )==zero ) {
      /**
       *           Subdiagonal is zero, no elimination is required.
       **/
      if( d_1( k )==zero ) {
        /**
         *              Diagonal is zero: set INFO = K and return; a unique
         *              solution can not be found.
         **/
        *info = k;
        return;
      }
    } else if( abs( d_1( k ) )>=abs( dl_1( k ) ) ) {
      /**
       *           No row interchange required
       **/
      mult = dl_1( k ) / d_1( k );
      d_1( k+1 ) = d_1( k+1 ) - mult*du_1( k );
      for (j=1 ; j<=nrhs ; j+=1) {
        b_2( k+1, j ) = b_2( k+1, j ) - mult*b_2( k, j );
      }
      if( k<( n-1 ) )
        dl_1( k ) = zero;
    } else {
      /**
       *           Interchange rows K and K+1
       **/
      mult = d_1( k ) / dl_1( k );
      d_1( k ) = dl_1( k );
      temp = d_1( k+1 );
      d_1( k+1 ) = du_1( k ) - mult*temp;
      if( k<( n-1 ) ) {
        dl_1( k ) = du_1( k+1 );
        du_1( k+1 ) = -mult*dl_1( k );
      }
      du_1( k ) = temp;
      for (j=1 ; j<=nrhs ; j+=1) {
        temp = b_2( k, j );
        b_2( k, j ) = b_2( k+1, j );
        b_2( k+1, j ) = temp - mult*b_2( k+1, j );
      }
    }
  }
  if( d_1( n )==zero ) {
    *info = n;
    return;
  }
  /**
   *     Back solve with the matrix U from the factorization.
   **/
  for (j=1 ; j<=nrhs ; j+=1) {
    b_2( n, j ) = b_2( n, j ) / d_1( n );
    if( n>1 )
      b_2( n-1, j ) = ( b_2( n-1, j )-du_1( n-1 )*b_2( n, j ) ) / d_1( n-1 );
    for (k=n - 2 ; k>=1 ; k+=-1) {
      b_2( k, j ) = ( b_2( k, j )-du_1( k )*b_2( k+1, j )-dl_1( k )*
                     b_2( k+2, j ) ) / d_1( k );
    }
  }

  return;
  /**
   *     End of DGTSV
   **/
}
