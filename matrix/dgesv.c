/*
 * $Id: dgesv.c,v 1.1 2005-09-18 22:04:44 dhmunro Exp $
 *  LAPACK routines to solve a matrix by LU decomposition.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "dg.h"


/*---blas routines---*/
/* idamax dswap dscal dger dgemm dtrsm */



/*---converted nutty string switches to single characters (lower case)---*/
#define lsame(x,y) ((x)==(y))

extern int strcmp(const char *, const char *);



/*-----Fortran intrinsics converted-----*/
#define abs(x) ((x)>=0?(x):-(x))
#define min(x,y) ((x)<(y)?(x):(y))
#define max(x,y) ((x)>(y)?(x):(y))
/*-----end of Fortran intrinsics-----*/



void dgesv( long n, long nrhs, double a[], long lda, long ipiv[],
           double b[], long ldb, long *info )
{
  /**
   *  -- LAPACK driver routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     March 31, 1993
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *     .. Array Arguments ..*/
#undef ipiv_1
#define ipiv_1(a1) ipiv[a1-1]
#undef b_2
#define b_2(a1,a2) b[a1-1+ldb*(a2-1)]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGESV computes the solution to a real system of linear equations
   *     A * X = B,
   *  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
   *
   *  The LU decomposition with partial pivoting and row interchanges is
   *  used to factor A as
   *     A = P * L * U,
   *  where P is a permutation matrix, L is unit lower triangular, and U is
   *  upper triangular.  The factored form of A is then used to solve the
   *  system of equations A * X = B.
   *
   *  Arguments
   *  =========
   *
   *  N       (input) INTEGER
   *          The number of linear equations, i.e., the order of the
   *          matrix A.  N >= 0.
   *
   *  NRHS    (input) INTEGER
   *          The number of right hand sides, i.e., the number of columns
   *          of the matrix B.  NRHS >= 0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the N-by-N coefficient matrix A.
   *          On exit, the factors L and U from the factorization
   *          A = P*L*U; the unit diagonal elements of L are not stored.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,N).
   *
   *  IPIV    (output) INTEGER array, dimension (N)
   *          The pivot indices that define the permutation matrix P;
   *          row i of the matrix was interchanged with row IPIV(i).
   *
   *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
   *          On entry, the N-by-NRHS matrix of right hand side matrix B.
   *          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
   *
   *  LDB     (input) INTEGER
   *          The leading dimension of the array B.  LDB >= max(1,N).
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument had an illegal value
   *          > 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
   *                has been completed, but the factor U is exactly
   *                singular, so the solution could not be computed.
   *
   *  =====================================================================
   **/
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input parameters.
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  if( n<0 ) {
    *info = -1;
  } else if( nrhs<0 ) {
    *info = -2;
  } else if( lda<max( 1, n ) ) {
    *info = -4;
  } else if( ldb<max( 1, n ) ) {
    *info = -7;
  }
  if( *info!=0 ) {
    xerbla( "dgesv ", -*info );
    return;
  }
  /**
   *     Compute the LU factorization of A.
   **/
  dgetrf( n, n, a, lda, ipiv, info );
  if( *info==0 ) {
    /**
     *        Solve the system A*X = B, overwriting B with X.
     **/
    dgetrs( 'n'/*o transpose*/, n, nrhs, a, lda, ipiv, b, ldb,
           info );
  }
  return;
  /**
   *     End of DGESV
   **/
}



void dgetrs( char trans, long n, long nrhs, double a[], long lda,
            long ipiv[], double b[], long ldb, long *info )
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
#undef ipiv_1
#define ipiv_1(a1) ipiv[a1-1]
#undef b_2
#define b_2(a1,a2) b[a1-1+ldb*(a2-1)]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGETRS solves a system of linear equations
   *     A * X = B  or  A' * X = B
   *  with a general N-by-N matrix A using the LU factorization computed
   *  by DGETRF.
   *
   *  Arguments
   *  =========
   *
   *  TRANS   (input) CHARACTER*1
   *          Specifies the form of the system of equations:
   *          = 'N':  A * X = B  (No transpose)
   *          = 'T':  A'* X = B  (Transpose)
   *          = 'C':  A'* X = B  (Conjugate transpose = Transpose)
   *
   *  N       (input) INTEGER
   *          The order of the matrix A.  N >= 0.
   *
   *  NRHS    (input) INTEGER
   *          The number of right hand sides, i.e., the number of columns
   *          of the matrix B.  NRHS >= 0.
   *
   *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
   *          The factors L and U from the factorization A = P*L*U
   *          as computed by DGETRF.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,N).
   *
   *  IPIV    (input) INTEGER array, dimension (N)
   *          The pivot indices from DGETRF; for 1<=i<=N, row i of the
   *          matrix was interchanged with row IPIV(i).
   *
   *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
   *          On entry, the right hand side matrix B.
   *          On exit, the solution matrix X.
   *
   *  LDB     (input) INTEGER
   *          The leading dimension of the array B.  LDB >= max(1,N).
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
  /**     ..
   *     .. Local Scalars ..*/
  int            notran;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input parameters.
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  notran = lsame( trans, 'n' );
  if( !notran && !lsame( trans, 't' ) && !
     lsame( trans, 'c' ) ) {
    *info = -1;
  } else if( n<0 ) {
    *info = -2;
  } else if( nrhs<0 ) {
    *info = -3;
  } else if( lda<max( 1, n ) ) {
    *info = -5;
  } else if( ldb<max( 1, n ) ) {
    *info = -8;
  }
  if( *info!=0 ) {
    xerbla( "dgetrs", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( n==0 || nrhs==0 )
    return;

  if( notran ) {
    /**
     *        Solve A * X = B.
     *
     *        Apply row interchanges to the right hand sides.
     **/
    dlaswp( nrhs, b, ldb, 1, n, ipiv, 1 );
    /**
     *        Solve L*X = B, overwriting B with X.
     **/
    cblas_dtrsm(CblasColMajor, CblasLeft, CblasLower, CblasNoTrans,
                CblasUnit, n, nrhs, one, a, lda, b, ldb );
    /**
     *        Solve U*X = B, overwriting B with X.
     **/
    cblas_dtrsm(CblasColMajor, CblasLeft, CblasUpper, CblasNoTrans,
                CblasNonUnit, n, nrhs, one, a, lda, b, ldb );
  } else {
    /**
     *        Solve A' * X = B.
     *
     *        Solve U'*X = B, overwriting B with X.
     **/
    cblas_dtrsm(CblasColMajor, CblasLeft, CblasUpper, CblasTrans,
                CblasNonUnit, n, nrhs, one, a, lda, b, ldb );
    /**
     *        Solve L'*X = B, overwriting B with X.
     **/
    cblas_dtrsm(CblasColMajor, CblasLeft, CblasLower, CblasTrans,
                CblasUnit, n, nrhs, one, a, lda, b, ldb );
    /**
     *        Apply row interchanges to the solution vectors.
     **/
    dlaswp( nrhs, b, ldb, 1, n, ipiv, -1 );
  }

  return;
  /**
   *     End of DGETRS
   **/
}



void dgetrf( long m, long n, double a[], long lda, long ipiv[], long *info )
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
#undef ipiv_1
#define ipiv_1(a1) ipiv[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGETRF computes an LU factorization of a general M-by-N matrix A
   *  using partial pivoting with row interchanges.
   *
   *  The factorization has the form
   *     A = P * L * U
   *  where P is a permutation matrix, L is lower triangular with unit
   *  diagonal elements (lower trapezoidal if m > n), and U is upper
   *  triangular (upper trapezoidal if m < n).
   *
   *  This is the right-looking Level 3 BLAS version of the algorithm.
   *
   *  Arguments
   *  =========
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix A.  M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix A.  N >= 0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the M-by-N matrix to be factored.
   *          On exit, the factors L and U from the factorization
   *          A = P*L*U; the unit diagonal elements of L are not stored.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  IPIV    (output) INTEGER array, dimension (min(M,N))
   *          The pivot indices; for 1 <= i <= min(M,N), row i of the
   *          matrix was interchanged with row IPIV(i).
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument had an illegal value
   *          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
   *                has been completed, but the factor U is exactly
   *                singular, and division by zero will occur if it is used
   *                to solve a system of equations.
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef one
#define one 1.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  long            i, iinfo, j, jb, nb;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max, min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input parameters.
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  if( m<0 ) {
    *info = -1;
  } else if( n<0 ) {
    *info = -2;
  } else if( lda<max( 1, m ) ) {
    *info = -4;
  }
  if( *info!=0 ) {
    xerbla( "dgetrf", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( m==0 || n==0 )
    return;
  /**
   *     Determine the block size for this environment.
   **/
  nb = ilaenv( 1, "dgetrf", " ", m, n, -1, -1 );
  if( nb<=1 || nb>=min( m, n ) ) {
    /**
     *        Use unblocked code.
     **/
    dgetf2( m, n, a, lda, ipiv, info );
  } else {
    /**
     *        Use blocked code.
     **/
    for (j=1 ; nb>0?j<=min( m, n ):j>=min( m, n ) ; j+=nb) {
      jb = min( min( m, n )-j+1, nb );
      /**
       *           Factor diagonal and subdiagonal blocks and test for exact
       *           singularity.
       **/
      dgetf2( m-j+1, jb, &a_2( j, j ), lda, &ipiv_1( j ), &iinfo );
      /**
       *           Adjust INFO and the pivot indices.
       **/
      if( *info==0 && iinfo>0 )
        *info = iinfo + j - 1;
      for (i=j ; i<=min( m, j+jb-1 ) ; i+=1) {
        ipiv_1( i ) = j - 1 + ipiv_1( i );
      }
      /**
       *           Apply interchanges to columns 1:J-1.
       **/
      dlaswp( j-1, a, lda, j, j+jb-1, ipiv, 1 );

      if( j+jb<=n ) {
        /**
         *              Apply interchanges to columns J+JB:N.
         **/
        dlaswp( n-j-jb+1, &a_2( 1, j+jb ), lda, j, j+jb-1,
               ipiv, 1 );
        /**
         *              Compute block row of U.
         **/
        cblas_dtrsm(CblasColMajor, CblasLeft, CblasLower, CblasNoTrans,
                    CblasUnit, jb, n-j-jb+1, one, &a_2( j, j ), lda,
                    &a_2( j, j+jb ), lda );
        if( j+jb<=m ) {
          /**
           *                 Update trailing submatrix.
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m-j-jb+1,
                      n-j-jb+1, jb, -one, &a_2( j+jb, j ), lda,
                      &a_2( j, j+jb ), lda, one, &a_2( j+jb, j+jb ), lda );
        }
      }
    }
  }
  return;
  /**
   *     End of DGETRF
   **/
}



void dlaswp( long n, double a[], long lda, long k1, long k2,
            long ipiv[], long incx )
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
#undef ipiv_1
#define ipiv_1(a1) ipiv[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLASWP performs a series of row interchanges on the matrix A.
   *  One row interchange is initiated for each of rows K1 through K2 of A.
   *
   *  Arguments
   *  =========
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix A.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the matrix of column dimension N to which the row
   *          interchanges will be applied.
   *          On exit, the permuted matrix.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.
   *
   *  K1      (input) INTEGER
   *          The first element of IPIV for which a row interchange will
   *          be done.
   *
   *  K2      (input) INTEGER
   *          The last element of IPIV for which a row interchange will
   *          be done.
   *
   *  IPIV    (input) INTEGER array, dimension (M*abs(INCX))
   *          The vector of pivot indices.  Only the elements in positions
   *          K1 through K2 of IPIV are accessed.
   *          IPIV(K) = L implies rows K and L are to be interchanged.
   *
   *  INCX    (input) INTEGER
   *          The increment between successive values of IPIV.  If IPIV
   *          is negative, the pivots are applied in reverse order.
   *
   * =====================================================================
   *
   *     .. Local Scalars ..*/
  long            i, ip, ix;
  /**     ..
   *     .. Executable Statements ..
   *
   *     Interchange row I with row IPIV(I) for each of rows K1 through K2.
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if( incx==0 )
    return;
  if( incx>0 ) {
    ix = k1;
  } else {
    ix = 1 + ( 1-k2 )*incx;
  }
  if( incx==1 ) {
    for (i=k1 ; i<=k2 ; i+=1) {
      ip = ipiv_1( i );
      if( ip!=i )
        cblas_dswap( n, &a_2( i, 1 ), lda, &a_2( ip, 1 ), lda );
    }
  } else if( incx>1 ) {
    for (i=k1 ; i<=k2 ; i+=1) {
      ip = ipiv_1( ix );
      if( ip!=i )
        cblas_dswap( n, &a_2( i, 1 ), lda, &a_2( ip, 1 ), lda );
      ix = ix + incx;
    }
  } else if( incx<0 ) {
    for (i=k2 ; i>=k1 ; i+=-1) {
      ip = ipiv_1( ix );
      if( ip!=i )
        cblas_dswap( n, &a_2( i, 1 ), lda, &a_2( ip, 1 ), lda );
      ix = ix + incx;
    }
  }

  return;
  /**
   *     End of DLASWP
   **/
}



void dgetf2( long m, long n, double a[], long lda, long ipiv[], long *info )
{
  /**
   *  -- LAPACK routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     June 30, 1992
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *     .. Array Arguments ..*/
#undef ipiv_1
#define ipiv_1(a1) ipiv[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGETF2 computes an LU factorization of a general m-by-n matrix A
   *  using partial pivoting with row interchanges.
   *
   *  The factorization has the form
   *     A = P * L * U
   *  where P is a permutation matrix, L is lower triangular with unit
   *  diagonal elements (lower trapezoidal if m > n), and U is upper
   *  triangular (upper trapezoidal if m < n).
   *
   *  This is the right-looking Level 2 BLAS version of the algorithm.
   *
   *  Arguments
   *  =========
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix A.  M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix A.  N >= 0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the m by n matrix to be factored.
   *          On exit, the factors L and U from the factorization
   *          A = P*L*U; the unit diagonal elements of L are not stored.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  IPIV    (output) INTEGER array, dimension (min(M,N))
   *          The pivot indices; for 1 <= i <= min(M,N), row i of the
   *          matrix was interchanged with row IPIV(i).
   *
   *  INFO    (output) INTEGER
   *          = 0: successful exit
   *          < 0: if INFO = -k, the k-th argument had an illegal value
   *          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
   *               has been completed, but the factor U is exactly
   *               singular, and division by zero will occur if it is used
   *               to solve a system of equations.
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
  long            j, jp;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max, min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input parameters.
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  if( m<0 ) {
    *info = -1;
  } else if( n<0 ) {
    *info = -2;
  } else if( lda<max( 1, m ) ) {
    *info = -4;
  }
  if( *info!=0 ) {
    xerbla( "dgetf2", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( m==0 || n==0 )
    return;

  for (j=1 ; j<=min( m, n ) ; j+=1) {
    /**
     *        Find pivot and test for singularity.
     **/
    jp = j + cblas_idamax( m-j+1, &a_2( j, j ), 1 );
    ipiv_1( j ) = jp;
    if( a_2( jp, j )!=zero ) {
      /**
       *           Apply the interchange to columns 1:N.
       **/
      if( jp!=j )
        cblas_dswap( n, &a_2( j, 1 ), lda, &a_2( jp, 1 ), lda );
      /**
       *           Compute elements J+1:M of J-th column.
       **/
      if( j<m )
        cblas_dscal( m-j, one / a_2( j, j ), &a_2( j+1, j ), 1 );

    } else if( *info==0 ) {

      *info = j;
    }

    if( j<min( m, n ) ) {
      /**
       *           Update trailing submatrix.
       **/
      cblas_dger(CblasColMajor, m-j, n-j, -one, &a_2( j+1, j ), 1,
                 &a_2( j, j+1 ), lda, &a_2( j+1, j+1 ), lda );
    }
  }
  return;
  /**
   *     End of DGETF2
   **/
}



long ilaenv( long ispec, char *name, char *opts,
            long n1, long n2, long n3,long n4 )
{
  /**
   *  -- LAPACK auxiliary routine (preliminary version) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     February 20, 1992
   *
   *     .. Scalar Arguments ..*/
  long ilaenv_R;
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  ILAENV is called from the LAPACK routines to choose problem-dependent
   *  parameters for the local environment.  See ISPEC for a description of
   *  the parameters.
   *
   *  This version provides a set of parameters which should give good,
   *  but not optimal, performance on many of the currently available
   *  computers.  Users are encouraged to modify this subroutine to set
   *  the tuning parameters for their particular machine using the option
   *  and problem size information in the arguments.
   *
   *  This routine will not function correctly if it is converted to all
   *  lower case.  Converting it to all upper case is allowed.
   *
   *  Arguments
   *  =========
   *
   *  ISPEC   (input) INTEGER
   *          Specifies the parameter to be returned as the value of
   *          ILAENV.
   *          = 1: the optimal blocksize; if this value is 1, an unblocked
   *               algorithm will give the best performance.
   *          = 2: the minimum block size for which the block routine
   *               should be used; if the usable block size is less than
   *               this value, an unblocked routine should be used.
   *          = 3: the crossover point (in a block routine, for N less
   *               than this value, an unblocked routine should be used)
   *          = 4: the number of shifts, used in the nonsymmetric
   *               eigenvalue routines
   *          = 5: the minimum column dimension for blocking to be used;
   *               rectangular blocks must have dimension at least k by m,
   *               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
   *          = 6: the crossover point for the SVD (when reducing an m by n
   *               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
   *               this value, a QR factorization is used first to reduce
   *               the matrix to a triangular form.)
   *          = 7: the number of processors
   *          = 8: the crossover point for the multishift QR and QZ methods
   *               for nonsymmetric eigenvalue problems.
   *
   *  NAME    (input) CHARACTER*(*)
   *          The name of the calling subroutine, in either upper case or
   *          lower case.
   *
   *  OPTS    (input) CHARACTER*(*)
   *          The character options to the subroutine NAME, concatenated
   *          into a single character string.  For example, UPLO = 'U',
   *          TRANS = 'T', and DIAG = 'N' for a triangular routine would
   *          be specified as OPTS = 'UTN'.
   *
   *  N1      (input) INTEGER
   *  N2      (input) INTEGER
   *  N3      (input) INTEGER
   *  N4      (input) INTEGER
   *          Problem dimensions for the subroutine NAME; these may not all
   *          be required.
   *
   * (ILAENV) (output) INTEGER
   *          >= 0: the value of the parameter specified by ISPEC
   *          < 0:  if ILAENV = -k, the k-th argument had an illegal value.
   *
   *  Further Details
   *  ===============
   *
   *  The following conventions have been used when calling ILAENV from the
   *  LAPACK routines:
   *  1)  OPTS is a concatenation of all of the character options to
   *      subroutine NAME, in the same order that they appear in the
   *      argument list for NAME, even if they are not used in determining
   *      the value of the parameter specified by ISPEC.
   *  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
   *      that they appear in the argument list for NAME.  N1 is used
   *      first, N2 second, and so on, and unused problem dimensions are
   *      passed a value of -1.
   *  3)  The parameter value returned by ILAENV is checked for validity in
   *      the calling subroutine.  For example, ILAENV is used to retrieve
   *      the optimal blocksize for STRTRI as follows:
   *
   *      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
   *      IF( NB.LE.1 ) NB = MAX( 1, N )
   *
   *  =====================================================================
   *
   *     .. Local Scalars ..*/
  int            cname, sname;
  char        c1;
  char        c2[3], c4[3];
  char        c3[4];
  char        subnam[6];
  long            i, ic, iz, nb, nbmin, nx;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          char, ichar, int, min, float;*/
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  switch (ispec) {
  case 1: goto L_100;
  case 2: goto L_100;
  case 3: goto L_100;
  case 4: goto L_400;
  case 5: goto L_500;
  case 6: goto L_600;
  case 7: goto L_700;
  case 8: goto L_800;
  }
  /**
   *     Invalid value for ISPEC
   **/
  ilaenv_R = -1;
  return ilaenv_R;

 L_100:
  /**
   *     Convert NAME to upper case if the first character is lower case.
   **/
  ilaenv_R = 1;
  for (iz=0 ; iz<6 && name[iz] ; iz++) subnam[iz]= name[iz];
  ic = subnam[0];
  iz = 'z';
  if( iz==90 || iz==122 ) {
    /**
     *        ASCII character set
     **/
    if( ic>=97 && ic<=122 ) {
      subnam[0] = ic-32;
      for (i=2 ; i<=6 ; i+=1) {
        ic = subnam[i-1];
        if( ic>=97 && ic<=122 ) subnam[i-1] = ic-32;
      }
    }

  } else if( iz==233 || iz==169 ) {
    /**
     *        EBCDIC character set
     **/
    if( ( ic>=129 && ic<=137 ) ||
       ( ic>=145 && ic<=153 ) ||
       ( ic>=162 && ic<=169 ) ) {
      subnam[0] = ic+64;
      for (i=2 ; i<=6 ; i+=1) {
        ic = subnam[i-1];
        if( ( ic>=129 && ic<=137 ) ||
           ( ic>=145 && ic<=153 ) ||
           ( ic>=162 && ic<=169 ) ) subnam[i-1] = ic+64;
      }
    }

  } else if( iz==218 || iz==250 ) {
    /**
     *        Prime machines:  ASCII+128
     **/
    if( ic>=225 && ic<=250 ) {
      subnam[0] = ic-32;
      for (i=2 ; i<=6 ; i+=1) {
        ic = subnam[i-1];
        if( ic>=225 && ic<=250 ) subnam[i-1] = ic-32;
      }
    }
  }

  c1 = subnam[0];
  sname = c1=='S' || c1=='D';
  cname = c1=='C' || c1=='Z';
  if( !( cname || sname ) )
    return ilaenv_R;
  c2[0]= subnam[1];  c2[1]= subnam[2];  c2[2]= '\0';
  c3[0]= subnam[3];  c3[1]= subnam[4];  c3[2]= subnam[5];  c3[3]= '\0';
  c4[0]= c3[1];  c4[1]= c3[2];  c4[2]= '\0';

  switch (ispec) {
  case 1: goto L_110;
  case 2: goto L_200;
  case 3: goto L_300;
  }

 L_110:
  /**
   *     ISPEC = 1:  block size
   *
   *     In these examples, separate code is provided for setting NB for
   *     real and complex.  We assume that NB will take the same value in
   *     single or double precision.
   **/
  nb = 1;

  if( !strcmp(c2,"GE") ) {
    if( !strcmp(c3,"TRF") ) {
      if( sname ) {
        nb = 64;
      } else {
        nb = 64;
      }
    } else if( !strcmp(c3,"QRF") || !strcmp(c3,"RQF") || !strcmp(c3,"LQF") ||
              !strcmp(c3,"QLF") ) {
      if( sname ) {
        nb = 32;
      } else {
        nb = 32;
      }
    } else if( !strcmp(c3,"HRD") ) {
      if( sname ) {
        nb = 32;
      } else {
        nb = 32;
      }
    } else if( !strcmp(c3,"BRD") ) {
      if( sname ) {
        nb = 32;
      } else {
        nb = 32;
      }
    } else if( !strcmp(c3,"TRI") ) {
      if( sname ) {
        nb = 64;
      } else {
        nb = 64;
      }
    }
  } else if( !strcmp(c2,"PO") ) {
    if( !strcmp(c3,"TRF") ) {
      if( sname ) {
        nb = 64;
      } else {
        nb = 64;
      }
    }
  } else if( !strcmp(c2,"SY") ) {
    if( !strcmp(c3,"TRF") ) {
      if( sname ) {
        nb = 64;
      } else {
        nb = 64;
      }
    } else if( sname && !strcmp(c3,"TRD") ) {
      nb = 1;
    } else if( sname && !strcmp(c3,"GST") ) {
      nb = 64;
    }
  } else if( cname && !strcmp(c2,"HE") ) {
    if( !strcmp(c3,"TRF") ) {
      nb = 64;
    } else if( !strcmp(c3,"TRD") ) {
      nb = 1;
    } else if( !strcmp(c3,"GST") ) {
      nb = 64;
    }
  } else if( sname && !strcmp(c2,"OR") ) {
    if( c3[0]=='G' ) {
      if( !strcmp(c4,"QR") || !strcmp(c4,"RQ") || !strcmp(c4,"LQ") ||
         !strcmp(c4,"QL") || !strcmp(c4,"HR") || !strcmp(c4,"TR") ||
         !strcmp(c4,"BR") ) {
        nb = 32;
      }
    } else if( c3[0]=='M' ) {
      if( !strcmp(c4,"QR") || !strcmp(c4,"RQ") || !strcmp(c4,"LQ") ||
         !strcmp(c4,"QL") || !strcmp(c4,"HR") || !strcmp(c4,"TR") ||
         !strcmp(c4,"BR") ) {
        nb = 32;
      }
    }
  } else if( cname && !strcmp(c2,"UN") ) {
    if( c3[0]=='G' ) {
      if( !strcmp(c4,"QR") || !strcmp(c4,"RQ") || !strcmp(c4,"LQ") ||
         !strcmp(c4,"QL") || !strcmp(c4,"HR") || !strcmp(c4,"TR") ||
         !strcmp(c4,"BR") ) {
        nb = 32;
      }
    } else if( c3[0]=='M' ) {
      if( !strcmp(c4,"QR") || !strcmp(c4,"RQ") || !strcmp(c4,"LQ") ||
         !strcmp(c4,"QL") || !strcmp(c4,"HR") || !strcmp(c4,"TR") ||
         !strcmp(c4,"BR") ) {
        nb = 32;
      }
    }
  } else if( !strcmp(c2,"GB") ) {
    if( !strcmp(c3,"TRF") ) {
      if( sname ) {
        if( n4<=64 ) {
          nb = 1;
        } else {
          nb = 32;
        }
      } else {
        if( n4<=64 ) {
          nb = 1;
        } else {
          nb = 32;
        }
      }
    }
  } else if( !strcmp(c2,"PB") ) {
    if( !strcmp(c3,"TRF") ) {
      if( sname ) {
        if( n2<=64 ) {
          nb = 1;
        } else {
          nb = 32;
        }
      } else {
        if( n2<=64 ) {
          nb = 1;
        } else {
          nb = 32;
        }
      }
    }
  } else if( !strcmp(c2,"TR") ) {
    if( !strcmp(c3,"TRI") ) {
      if( sname ) {
        nb = 64;
      } else {
        nb = 64;
      }
    }
  } else if( !strcmp(c2,"LA") ) {
    if( !strcmp(c3,"UUM") ) {
      if( sname ) {
        nb = 64;
      } else {
        nb = 64;
      }
    }
  } else if( sname && !strcmp(c2,"ST") ) {
    if( !strcmp(c3,"EBZ") ) {
      nb = 1;
    }
  }
  ilaenv_R = nb;
  return ilaenv_R;

 L_200:
  /**
   *     ISPEC = 2:  minimum block size
   **/
  nbmin = 2;
  if( !strcmp(c2,"GE") ) {
    if( !strcmp(c3,"QRF") || !strcmp(c3,"RQF") || !strcmp(c3,"LQF") ||
       !strcmp(c3,"QLF") ) {
      if( sname ) {
        nbmin = 2;
      } else {
        nbmin = 2;
      }
    } else if( !strcmp(c3,"HRD") ) {
      if( sname ) {
        nbmin = 2;
      } else {
        nbmin = 2;
      }
    } else if( !strcmp(c3,"BRD") ) {
      if( sname ) {
        nbmin = 2;
      } else {
        nbmin = 2;
      }
    } else if( !strcmp(c3,"TRI") ) {
      if( sname ) {
        nbmin = 2;
      } else {
        nbmin = 2;
      }
    }
  } else if( !strcmp(c2,"SY") ) {
    if( !strcmp(c3,"TRF") ) {
      if( sname ) {
        nbmin = 2;
      } else {
        nbmin = 2;
      }
    } else if( sname && !strcmp(c3,"TRD") ) {
      nbmin = 2;
    }
  } else if( cname && !strcmp(c2,"HE") ) {
    if( !strcmp(c3,"TRD") ) {
      nbmin = 2;
    }
  } else if( sname && !strcmp(c2,"OR") ) {
    if( c3[0]=='G' ) {
      if( !strcmp(c4,"QR") || !strcmp(c4,"RQ") || !strcmp(c4,"LQ") ||
         !strcmp(c4,"QL") || !strcmp(c4,"HR") || !strcmp(c4,"TR") ||
         !strcmp(c4,"BR") ) {
        nbmin = 2;
      }
    } else if( c3[0]=='M' ) {
      if( !strcmp(c4,"QR") || !strcmp(c4,"RQ") || !strcmp(c4,"LQ") ||
         !strcmp(c4,"QL") || !strcmp(c4,"HR") || !strcmp(c4,"TR") ||
         !strcmp(c4,"BR") ) {
        nbmin = 2;
      }
    }
  } else if( cname && !strcmp(c2,"UN") ) {
    if( c3[0]=='G' ) {
      if( !strcmp(c4,"QR") || !strcmp(c4,"RQ") || !strcmp(c4,"LQ") ||
         !strcmp(c4,"QL") || !strcmp(c4,"HR") || !strcmp(c4,"TR") ||
         !strcmp(c4,"BR") ) {
        nbmin = 2;
      }
    } else if( c3[0]=='M' ) {
      if( !strcmp(c4,"QR") || !strcmp(c4,"RQ") || !strcmp(c4,"LQ") ||
         !strcmp(c4,"QL") || !strcmp(c4,"HR") || !strcmp(c4,"TR") ||
         !strcmp(c4,"BR") ) {
        nbmin = 2;
      }
    }
  }
  ilaenv_R = nbmin;
  return ilaenv_R;

 L_300:
  /**
   *     ISPEC = 3:  crossover point
   **/
  nx = 0;
  if( !strcmp(c2,"GE") ) {
    if( !strcmp(c3,"QRF") || !strcmp(c3,"RQF") || !strcmp(c3,"LQF") ||
       !strcmp(c3,"QLF") ) {
      if( sname ) {
        nx = 128;
      } else {
        nx = 128;
      }
    } else if( !strcmp(c3,"HRD") ) {
      if( sname ) {
        nx = 128;
      } else {
        nx = 128;
      }
    } else if( !strcmp(c3,"BRD") ) {
      if( sname ) {
        nx = 128;
      } else {
        nx = 128;
      }
    }
  } else if( !strcmp(c2,"SY") ) {
    if( sname && !strcmp(c3,"TRD") ) {
      nx = 1;
    }
  } else if( cname && !strcmp(c2,"HE") ) {
    if( !strcmp(c3,"TRD") ) {
      nx = 1;
    }
  } else if( sname && !strcmp(c2,"OR") ) {
    if( c3[0]=='G' ) {
      if( !strcmp(c4,"QR") || !strcmp(c4,"RQ") || !strcmp(c4,"LQ") ||
         !strcmp(c4,"QL") || !strcmp(c4,"HR") || !strcmp(c4,"TR") ||
         !strcmp(c4,"BR") ) {
        nx = 128;
      }
    }
  } else if( cname && !strcmp(c2,"UN") ) {
    if( c3[0]=='G' ) {
      if( !strcmp(c4,"QR") || !strcmp(c4,"RQ") || !strcmp(c4,"LQ") ||
         !strcmp(c4,"QL") || !strcmp(c4,"HR") || !strcmp(c4,"TR") ||
         !strcmp(c4,"BR") ) {
        nx = 128;
      }
    }
  }
  ilaenv_R = nx;
  return ilaenv_R;

 L_400:
  /**
   *     ISPEC = 4:  number of shifts (used by xHSEQR)
   **/
  ilaenv_R = 6;
  return ilaenv_R;

 L_500:
  /**
   *     ISPEC = 5:  minimum column dimension (not used)
   **/
  ilaenv_R = 2;
  return ilaenv_R;

 L_600:
  /**
   *     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
   **/
  ilaenv_R = (long)(min( n1, n2 ) * 1.6e0);
  return ilaenv_R;

 L_700:
  /**
   *     ISPEC = 7:  number of processors (not used)
   **/
  ilaenv_R = 1;
  return ilaenv_R;

 L_800:
  /**
   *     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
   **/
  ilaenv_R = 50;
  return ilaenv_R;
  /**
   *     End of ILAENV
   **/
}
