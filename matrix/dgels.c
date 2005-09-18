/*
 * $Id: dgels.c,v 1.1 2005-09-18 22:04:43 dhmunro Exp $
 * LAPACK routines to solve (in least squares sense) a matrix
 * by QR or LQ decomposition.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "dg.h"


/*---blas routines---*/
/* dcopy dnrm2 dscal dger dgemv dgemm dtrmv dtrmm dtrsm */



/*---converted nutty string switches to single characters (lower case)---*/
#define lsame(x,y) ((x)==(y))


/*-----Fortran intrinsics converted-----*/
#define abs(x) ((x)>=0?(x):-(x))
extern double sqrt(double);
#define dble(x) ((double)x)
#define sign(x,y) ((((x)<0)!=((y)<0))?-(x):(x))
#define min(x,y) ((x)<(y)?(x):(y))
#define max(x,y) ((x)>(y)?(x):(y))
/*-----end of Fortran intrinsics-----*/



void dgels( char trans, long m, long n, long nrhs,
           double a[], long lda, double b[], long ldb,
           double work[], long lwork,long *info )
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
#undef work_1
#define work_1(a1) work[a1-1]
#undef b_2
#define b_2(a1,a2) b[a1-1+ldb*(a2-1)]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGELS solves overdetermined or underdetermined real linear systems
   *  involving an M-by-N matrix A, or its transpose, using a QR or LQ
   *  factorization of A.  It is assumed that A has full rank.
   *
   *  The following options are provided:
   *
   *  1. If TRANS = 'N' and m >= n:  find the least squares solution of
   *     an overdetermined system, i.e., solve the least squares problem
   *                  minimize || B - A*X ||.
   *
   *  2. If TRANS = 'N' and m < n:  find the minimum norm solution of
   *     an underdetermined system A * X = B.
   *
   *  3. If TRANS = 'T' and m >= n:  find the minimum norm solution of
   *     an undetermined system A**T * X = B.
   *
   *  4. If TRANS = 'T' and m < n:  find the least squares solution of
   *     an overdetermined system, i.e., solve the least squares problem
   *                  minimize || B - A**T * X ||.
   *
   *  Several right hand side vectors b and solution vectors x can be
   *  handled in a single call; they are stored as the columns of the
   *  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
   *  matrix X.
   *
   *  Arguments
   *  =========
   *
   *  TRANS   (input) CHARACTER
   *          = 'N': the linear system involves A;
   *          = 'T': the linear system involves A**T.
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix A.  M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix A.  N >= 0.
   *
   *  NRHS    (input) INTEGER
   *          The number of right hand sides, i.e., the number of
   *          columns of the matrices B and X. NRHS >=0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the M-by-N matrix A.
   *          On exit,
   *            if M >= N, A is overwritten by details of its QR
   *                       factorization as returned by DGEQRF;
   *            if M <  N, A is overwritten by details of its LQ
   *                       factorization as returned by DGELQF.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
   *          On entry, the matrix B of right hand side vectors, stored
   *          columnwise; B is M-by-NRHS if TRANS = 'N', or N-by-NRHS
   *          if TRANS = 'T'.
   *          On exit, B is overwritten by the solution vectors, stored
   *          columnwise:  if TRANS = 'N' and m >= n, rows 1 to n of B
   *          contain the least squares solution vectors; the residual
   *          sum of squares for the solution in each column is given by
   *          the sum of squares of elements N+1 to M in that column;
   *          if TRANS = 'N' and m < n, rows 1 to N of B contain the
   *          minimum norm solution vectors;
   *          if TRANS = 'T' and m >= n, rows 1 to M of B contain the
   *          minimum norm solution vectors;
   *          if TRANS = 'T' and m < n, rows 1 to M of B contain the
   *          least squares solution vectors; the residual sum of squares
   *          for the solution in each column is given by the sum of
   *          squares of elements M+1 to N in that column.
   *
   *  LDB     (input) INTEGER
   *          The leading dimension of the array B. LDB >= MAX(1,M,N).
   *
   *  WORK_1    (workspace) DOUBLE PRECISION array, dimension (LWORK)
   *          On exit, if INFO = 0, WORK_1(1) returns the optimal LWORK.
   *
   *  LWORK   (input) INTEGER
   *          The dimension of the array WORK.
   *          LWORK >= min(M,N) + MAX(1,M,N,NRHS).
   *          For optimal performance,
   *          LWORK >= min(M,N) + MAX(1,M,N,NRHS) * NB
   *          where NB is the optimum block size.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument had an illegal value
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e0
#undef one
#define one 1.0e0
  /**     ..
   *     .. Local Scalars ..*/
  int            tpsd=0;
  long            brow, i, iascl, ibscl, j, mn, nb, scllen, wsize=0;
  double    anrm, bignum, bnrm, smlnum;
  /**     ..
   *     .. Local Arrays ..*/
  double    rwork[1];
#undef rwork_1
#define rwork_1(a1) [a1-1]
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          dble, max, min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input arguments.
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  mn = min( m, n );
  if( !( lsame( trans, 'n' ) || lsame( trans, 't' ) ) ) {
    *info = -1;
  } else if( m<0 ) {
    *info = -2;
  } else if( n<0 ) {
    *info = -3;
  } else if( nrhs<0 ) {
    *info = -4;
  } else if( lda<max( 1, m ) ) {
    *info = -6;
  } else if( ldb<max( max(1, m), n ) ) {
    *info = -8;
  } else if( lwork<max( 1, mn+max( max(m, n), nrhs ) ) ) {
    *info = -10;
  }
  /**
   *     Figure out optimal block size
   **/
  if( *info==0 || *info==-10 ) {

    tpsd = 1;
    if( lsame( trans, 'n' ) )
      tpsd = 0;

    if( m>=n ) {
      nb = ilaenv( 1, "dgeqrf", " ", m, n, -1, -1 );
      if( tpsd ) {
        nb = max( nb, ilaenv( 1, "dormqr", "ln", m, nrhs, n,
                             -1 ) );
      } else {
        nb = max( nb, ilaenv( 1, "dormqr", "lt", m, nrhs, n,
                             -1 ) );
      }
    } else {
      nb = ilaenv( 1, "dgelqf", " ", m, n, -1, -1 );
      if( tpsd ) {
        nb = max( nb, ilaenv( 1, "dormlq", "lt", n, nrhs, m,
                             -1 ) );
      } else {
        nb = max( nb, ilaenv( 1, "dormlq", "ln", n, nrhs, m,
                             -1 ) );
      }
    }

    wsize = mn + max( max(m, n), nrhs )*nb;
    work_1( 1 ) = dble( wsize );

  }

  if( *info!=0 ) {
    xerbla( "dgels ", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( min( min(m, n), nrhs )==0 ) {
    dlaset( 'f'/*ull*/, max( m, n ), nrhs, zero, zero, b, ldb );
    return;
  }
  /**
   *     Get machine parameters
   **/
  smlnum = dlamch( 's' ) / dlamch( 'p' );
  bignum = one / smlnum;
  dlabad( &smlnum, &bignum );
  /**
   *     Scale A, B if max entry outside range [SMLNUM,BIGNUM]
   **/
  anrm = dlange( 'm', m, n, a, lda, rwork );
  iascl = 0;
  if( anrm>zero && anrm<smlnum ) {
    /**
     *        Scale matrix norm up to SMLNUM
     **/
    dlascl( 'g', 0, 0, anrm, smlnum, m, n, a, lda, info );
    iascl = 1;
  } else if( anrm>bignum ) {
    /**
     *        Scale matrix norm down to BIGNUM
     **/
    dlascl( 'g', 0, 0, anrm, bignum, m, n, a, lda, info );
    iascl = 2;
  } else if( anrm==zero ) {
    /**
     *        Matrix all zero. Return zero solution.
     **/
    dlaset( 'f', max( m, n ), nrhs, zero, zero, b, ldb );
    goto L_50;
  }

  brow = m;
  if( tpsd )
    brow = n;
  bnrm = dlange( 'm', brow, nrhs, b, ldb, rwork );
  ibscl = 0;
  if( bnrm>zero && bnrm<smlnum ) {
    /**
     *        Scale matrix norm up to SMLNUM
     **/
    dlascl( 'g', 0, 0, bnrm, smlnum, brow, nrhs, b, ldb,
           info );
    ibscl = 1;
  } else if( bnrm>bignum ) {
    /**
     *        Scale matrix norm down to BIGNUM
     **/
    dlascl( 'g', 0, 0, bnrm, bignum, brow, nrhs, b, ldb,
           info );
    ibscl = 2;
  }

  if( m>=n ) {
    /**
     *        compute QR factorization of A
     **/
    dgeqrf( m, n, a, lda, &work_1( 1 ), &work_1( mn+1 ), lwork-mn,
           info );
    /**
     *        workspace at least N, optimally N*NB
     **/
    if( !tpsd ) {
      /**
       *           Least-Squares Problem min || A * X - B ||
       *
       *           B(1:M,1:NRHS) := Q' * B(1:M,1:NRHS)
       **/
      dormqr( 'l'/*eft*/, 't'/*ranspose*/, m, nrhs, n, a, lda,
             &work_1( 1 ), b, ldb, &work_1( mn+1 ), lwork-mn,
             info );
      /**
       *           workspace at least NRHS, optimally NRHS*NB
       *
       *           B(1:N,1:NRHS) := inv(R) * B(1:N,1:NRHS)
       **/
      cblas_dtrsm(CblasColMajor, CblasLeft, CblasUpper, CblasNoTrans,
                  CblasNonUnit, n, nrhs, one, a, lda, b, ldb );

      scllen = n;

    } else {
      /**
       *           Overdetermined system of equations A' * X = B
       *
       *           B(1:N,1:NRHS) := inv(R') * B(1:N,1:NRHS)
       **/
      cblas_dtrsm(CblasColMajor, CblasLeft, CblasUpper, CblasTrans,
                  CblasNonUnit, n, nrhs, one, a, lda, b, ldb );
      /**
       *           B(N+1:M,1:NRHS) = ZERO
       **/
      for (j=1 ; j<=nrhs ; j+=1) {
        for (i=n + 1 ; i<=m ; i+=1) {
          b_2( i, j ) = zero;
        }
      }
      /**
       *           B(1:M,1:NRHS) := Q(1:N,:) * B(1:N,1:NRHS)
       **/
      dormqr( 'l'/*eft*/, 'n'/*o transpose*/, m, nrhs, n, a, lda,
             &work_1( 1 ), b, ldb, &work_1( mn+1 ), lwork-mn,
             info );
      /**
       *           workspace at least NRHS, optimally NRHS*NB
       **/
      scllen = m;

    }

  } else {
    /**
     *        Compute LQ factorization of A
     **/
    dgelqf( m, n, a, lda, &work_1( 1 ), &work_1( mn+1 ), lwork-mn,
           info );
    /**
     *        workspace at least M, optimally M*NB.
     **/
    if( !tpsd ) {
      /**
       *           underdetermined system of equations A * X = B
       *
       *           B(1:M,1:NRHS) := inv(L) * B(1:M,1:NRHS)
       **/
      cblas_dtrsm(CblasColMajor, CblasLeft, CblasLower, CblasNoTrans,
                  CblasNonUnit, m, nrhs, one, a, lda, b, ldb );
      /**
       *           B(M+1:N,1:NRHS) = 0
       **/
      for (j=1 ; j<=nrhs ; j+=1) {
        for (i=m + 1 ; i<=n ; i+=1) {
          b_2( i, j ) = zero;
        }
      }
      /**
       *           B(1:N,1:NRHS) := Q(1:N,:)' * B(1:M,1:NRHS)
       **/
      dormlq( 'l'/*eft*/, 't'/*ranspose*/, n, nrhs, m, a, lda,
             &work_1( 1 ), b, ldb, &work_1( mn+1 ), lwork-mn,
             info );
      /**
       *           workspace at least NRHS, optimally NRHS*NB
       **/
      scllen = n;

    } else {
      /**
       *           overdetermined system min || A' * X - B ||
       *
       *           B(1:N,1:NRHS) := Q * B(1:N,1:NRHS)
       **/
      dormlq( 'l'/*eft*/, 'n'/*o transpose*/, n, nrhs, m, a, lda,
             &work_1( 1 ), b, ldb, &work_1( mn+1 ), lwork-mn,
             info );
      /**
       *           workspace at least NRHS, optimally NRHS*NB
       *
       *           B(1:M,1:NRHS) := inv(L') * B(1:M,1:NRHS)
       **/
      cblas_dtrsm(CblasColMajor, CblasLeft, CblasLower, CblasTrans,
                  CblasNonUnit, m, nrhs, one, a, lda, b, ldb );

      scllen = m;

    }

  }
  /**
   *     Undo scaling
   **/
  if( iascl==1 ) {
    dlascl( 'g', 0, 0, anrm, smlnum, scllen, nrhs, b, ldb,
           info );
  } else if( iascl==2 ) {
    dlascl( 'g', 0, 0, anrm, bignum, scllen, nrhs, b, ldb,
           info );
  }
  if( ibscl==1 ) {
    dlascl( 'g', 0, 0, smlnum, bnrm, scllen, nrhs, b, ldb,
           info );
  } else if( ibscl==2 ) {
    dlascl( 'g', 0, 0, bignum, bnrm, scllen, nrhs, b, ldb,
           info );
  }

 L_50:
  work_1( 1 ) = dble( wsize );

  return;
  /**
   *     End of DGELS
   **/
}



void dormlq( char side, char trans, long m, long n, long k,
            double a[], long lda, double tau[], double c[], long ldc,
            double work[], long lwork, long *info )
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
#undef work_1
#define work_1(a1) work[a1-1]
#undef tau_1
#define tau_1(a1) tau[a1-1]
#undef c_2
#define c_2(a1,a2) c[a1-1+ldc*(a2-1)]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DORMLQ overwrites the general real M-by-N matrix C with
   *
   *                  SIDE = 'L'     SIDE = 'R'
   *  TRANS = 'N':      Q * C          C * Q
   *  TRANS = 'T':      Q**T * C       C * Q**T
   *
   *  where Q is a real orthogonal matrix defined as the product of k
   *  elementary reflectors
   *
   *        Q = H(k) . . . H(2) H(1)
   *
   *  as returned by DGELQF. Q is of order M if SIDE = 'L' and of order N
   *  if SIDE = 'R'.
   *
   *  Arguments
   *  =========
   *
   *  SIDE    (input) CHARACTER*1
   *          = 'L': apply Q or Q**T from the Left;
   *          = 'R': apply Q or Q**T from the Right.
   *
   *  TRANS   (input) CHARACTER*1
   *          = 'N':  No transpose, apply Q;
   *          = 'T':  Transpose, apply Q**T.
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix C. M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix C. N >= 0.
   *
   *  K       (input) INTEGER
   *          The number of elementary reflectors whose product defines
   *          the matrix Q.
   *          If SIDE = 'L', M >= K >= 0;
   *          if SIDE = 'R', N >= K >= 0.
   *
   *  A       (input) DOUBLE PRECISION array, dimension
   *                               (LDA,M) if SIDE = 'L',
   *                               (LDA,N) if SIDE = 'R'
   *          The i-th row must contain the vector which defines the
   *          elementary reflector H(i), for i = 1,2,...,k, as returned by
   *          DGELQF in the first k rows of its array argument A.
   *          A is modified by the routine but restored on exit.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A. LDA >= max(1,K).
   *
   *  TAU     (input) DOUBLE PRECISION array, dimension (K)
   *          TAU(i) must contain the scalar factor of the elementary
   *          reflector H(i), as returned by DGELQF.
   *
   *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
   *          On entry, the M-by-N matrix C.
   *          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
   *
   *  LDC     (input) INTEGER
   *          The leading dimension of the array C. LDC >= max(1,M).
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
   *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
   *
   *  LWORK   (input) INTEGER
   *          The dimension of the array WORK.
   *          If SIDE = 'L', LWORK >= max(1,N);
   *          if SIDE = 'R', LWORK >= max(1,M).
   *          For optimum performance LWORK >= N*NB if SIDE = 'L', and
   *          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
   *          blocksize.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument had an illegal value
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef nbmax
#define nbmax 64
#undef ldt
#define ldt (nbmax+1)
  /**     ..
   *     .. Local Scalars ..*/
  int            left, notran;
  char          transt, side_trans[3];
  long    i, i1, i2, i3, ib, ic=0, iinfo, iws, jc=0, ldwork,
          mi=0, nb, nbmin, ni=0, nq, nw;
  /**     ..
   *     .. Local Arrays ..*/
  double    t[nbmax][ldt];
#undef t_2
#define t_2(a1,a2) t[a2-1][a1-1]
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max, min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input arguments
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  left = lsame( side, 'l' );
  notran = lsame( trans, 'n' );
  /**
   *     NQ is the order of Q and NW is the minimum dimension of WORK
   **/
  if( left ) {
    nq = m;
    nw = n;
  } else {
    nq = n;
    nw = m;
  }
  if( !left && !lsame( side, 'r' ) ) {
    *info = -1;
  } else if( !notran && !lsame( trans, 't' ) ) {
    *info = -2;
  } else if( m<0 ) {
    *info = -3;
  } else if( n<0 ) {
    *info = -4;
  } else if( k<0 || k>nq ) {
    *info = -5;
  } else if( lda<max( 1, k ) ) {
    *info = -7;
  } else if( ldc<max( 1, m ) ) {
    *info = -10;
  } else if( lwork<max( 1, nw ) ) {
    *info = -12;
  }
  if( *info!=0 ) {
    xerbla( "dormlq", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( m==0 || n==0 || k==0 ) {
    work_1( 1 ) = 1;
    return;
  }
  /**
   *     Determine the block size.  NB may be at most NBMAX, where NBMAX
   *     is used to define the local array T.
   **/
  side_trans[0]= side;
  side_trans[1]= trans;
  side_trans[2]= '\0';
  nb = min( nbmax, ilaenv( 1, "dormlq", side_trans, m, n, k,
                          -1 ) );
  nbmin = 2;
  ldwork = nw;
  if( nb>1 && nb<k ) {
    iws = nw*nb;
    if( lwork<iws ) {
      nb = lwork / ldwork;
      nbmin = max( 2, ilaenv( 2, "dormlq", side_trans, m, n, k,
                             -1 ) );
    }
  } else {
    iws = nw;
  }

  if( nb<nbmin || nb>=k ) {
    /**
     *        Use unblocked code
     **/
    dorml2( side, trans, m, n, k, a, lda, tau, c, ldc, work,
           &iinfo );
  } else {
    /**
     *        Use blocked code
     **/
    if( ( left && notran ) ||
       ( !left && !notran ) ) {
      i1 = 1;
      i2 = k;
      i3 = nb;
    } else {
      i1 = ( ( k-1 ) / nb )*nb + 1;
      i2 = 1;
      i3 = -nb;
    }

    if( left ) {
      ni = n;
      jc = 1;
    } else {
      mi = m;
      ic = 1;
    }

    if( notran ) {
      transt = 't';
    } else {
      transt = 'n';
    }

    for (i=i1 ; i3>0?i<=i2:i>=i2 ; i+=i3) {
      ib = min( nb, k-i+1 );
      /**
       *           Form the triangular factor of the block reflector
       *           H = H(i) H(i+1) . . . H(i+ib-1)
       **/
      dlarft( 'f'/*orward*/, 'r'/*owwise*/, nq-i+1, ib, &a_2( i, i ),
             lda, &tau_1( i ), (double *)t, ldt );
      if( left ) {
        /**
         *              H or H' is applied to C(i:m,1:n)
         **/
        mi = m - i + 1;
        ic = i;
      } else {
        /**
         *              H or H' is applied to C(1:m,i:n)
         **/
        ni = n - i + 1;
        jc = i;
      }
      /**
       *           Apply H or H'
       **/
      dlarfb( side, transt, 'f'/*orward*/, 'r'/*owwise*/, mi, ni, ib,
             &a_2( i, i ), lda, (double *)t, ldt, &c_2( ic, jc ), ldc, work,
             ldwork );
    }
  }
  work_1( 1 ) = iws;
  return;
  /**
   *     End of DORMLQ
   **/
}



void dorml2( char side, char trans, long m, long n, long k,
            double a[], long lda, double tau[], double c[], long ldc,
            double work[], long *info )
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
#undef work_1
#define work_1(a1) work[a1-1]
#undef tau_1
#define tau_1(a1) tau[a1-1]
#undef c_2
#define c_2(a1,a2) c[a1-1+ldc*(a2-1)]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DORML2 overwrites the general real m by n matrix C with
   *
   *        Q * C  if SIDE = 'L' and TRANS = 'N', or
   *
   *        Q'* C  if SIDE = 'L' and TRANS = 'T', or
   *
   *        C * Q  if SIDE = 'R' and TRANS = 'N', or
   *
   *        C * Q' if SIDE = 'R' and TRANS = 'T',
   *
   *  where Q is a real orthogonal matrix defined as the product of k
   *  elementary reflectors
   *
   *        Q = H(k) . . . H(2) H(1)
   *
   *  as returned by DGELQF. Q is of order m if SIDE = 'L' and of order n
   *  if SIDE = 'R'.
   *
   *  Arguments
   *  =========
   *
   *  SIDE    (input) CHARACTER*1
   *          = 'L': apply Q or Q' from the Left
   *          = 'R': apply Q or Q' from the Right
   *
   *  TRANS   (input) CHARACTER*1
   *          = 'N': apply Q  (No transpose)
   *          = 'T': apply Q' (Transpose)
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix C. M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix C. N >= 0.
   *
   *  K       (input) INTEGER
   *          The number of elementary reflectors whose product defines
   *          the matrix Q.
   *          If SIDE = 'L', M >= K >= 0;
   *          if SIDE = 'R', N >= K >= 0.
   *
   *  A       (input) DOUBLE PRECISION array, dimension
   *                               (LDA,M) if SIDE = 'L',
   *                               (LDA,N) if SIDE = 'R'
   *          The i-th row must contain the vector which defines the
   *          elementary reflector H(i), for i = 1,2,...,k, as returned by
   *          DGELQF in the first k rows of its array argument A.
   *          A is modified by the routine but restored on exit.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A. LDA >= max(1,K).
   *
   *  TAU     (input) DOUBLE PRECISION array, dimension (K)
   *          TAU(i) must contain the scalar factor of the elementary
   *          reflector H(i), as returned by DGELQF.
   *
   *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
   *          On entry, the m by n matrix C.
   *          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
   *
   *  LDC     (input) INTEGER
   *          The leading dimension of the array C. LDC >= max(1,M).
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension
   *                                   (N) if SIDE = 'L',
   *                                   (M) if SIDE = 'R'
   *
   *  INFO    (output) INTEGER
   *          = 0: successful exit
   *          < 0: if INFO = -i, the i-th argument had an illegal value
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef one
#define one 1.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  int            left, notran;
  long            i, i1, i2, i3, ic=0, jc=0, mi=0, ni=0, nq;
  double    aii;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input arguments
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  left = lsame( side, 'l' );
  notran = lsame( trans, 'n' );
  /**
   *     NQ is the order of Q
   **/
  if( left ) {
    nq = m;
  } else {
    nq = n;
  }
  if( !left && !lsame( side, 'r' ) ) {
    *info = -1;
  } else if( !notran && !lsame( trans, 't' ) ) {
    *info = -2;
  } else if( m<0 ) {
    *info = -3;
  } else if( n<0 ) {
    *info = -4;
  } else if( k<0 || k>nq ) {
    *info = -5;
  } else if( lda<max( 1, k ) ) {
    *info = -7;
  } else if( ldc<max( 1, m ) ) {
    *info = -10;
  }
  if( *info!=0 ) {
    xerbla( "dorml2", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( m==0 || n==0 || k==0 )
    return;

  if( ( left && notran ) || ( !left && !notran ) )
    {
      i1 = 1;
      i2 = k;
      i3 = 1;
    } else {
      i1 = k;
      i2 = 1;
      i3 = -1;
    }

  if( left ) {
    ni = n;
    jc = 1;
  } else {
    mi = m;
    ic = 1;
  }

  for (i=i1 ; i3>0?i<=i2:i>=i2 ; i+=i3) {
    if( left ) {
      /**
       *           H(i) is applied to C(i:m,1:n)
       **/
      mi = m - i + 1;
      ic = i;
    } else {
      /**
       *           H(i) is applied to C(1:m,i:n)
       **/
      ni = n - i + 1;
      jc = i;
    }
    /**
     *        Apply H(i)
     **/
    aii = a_2( i, i );
    a_2( i, i ) = one;
    dlarf( side, mi, ni, &a_2( i, i ), lda, tau_1( i ),
          &c_2( ic, jc ), ldc, work );
    a_2( i, i ) = aii;
  }
  return;
  /**
   *     End of DORML2
   **/
}



void dgelqf( long m, long n, double a[], long lda, double tau[],
            double work[], long lwork, long *info )
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
#undef work_1
#define work_1(a1) work[a1-1]
#undef tau_1
#define tau_1(a1) tau[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGELQF computes an LQ factorization of a real M-by-N matrix A:
   *  A = L * Q.
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
   *          On entry, the M-by-N matrix A.
   *          On exit, the elements on and below the diagonal of the array
   *          contain the m-by-min(m,n) lower trapezoidal matrix L (L is
   *          lower triangular if m <= n); the elements above the diagonal,
   *          with the array TAU, represent the orthogonal matrix Q as a
   *          product of elementary reflectors (see Further Details).
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
   *          The scalar factors of the elementary reflectors (see Further
   *          Details).
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
   *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
   *
   *  LWORK   (input) INTEGER
   *          The dimension of the array WORK.  LWORK >= max(1,M).
   *          For optimum performance LWORK >= M*NB, where NB is the
   *          optimal blocksize.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument had an illegal value
   *
   *  Further Details
   *  ===============
   *
   *  The matrix Q is represented as a product of elementary reflectors
   *
   *     Q = H(k) . . . H(2) H(1), where k = min(m,n).
   *
   *  Each H(i) has the form
   *
   *     H(i) = I - tau * v * v'
   *
   *  where tau is a real scalar, and v is a real vector with
   *  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
   *  and tau in TAU(i).
   *
   *  =====================================================================
   *
   *     .. Local Scalars ..*/
  long            i, ib, iinfo, iws, k, ldwork=0, nb, nbmin, nx;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max, min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input arguments
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
  } else if( lwork<max( 1, m ) ) {
    *info = -7;
  }
  if( *info!=0 ) {
    xerbla( "dgelqf", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  k = min( m, n );
  if( k==0 ) {
    work_1( 1 ) = 1;
    return;
  }
  /**
   *     Determine the block size.
   **/
  nb = ilaenv( 1, "dgelqf", " ", m, n, -1, -1 );
  nbmin = 2;
  nx = 0;
  iws = m;
  if( nb>1 && nb<k ) {
    /**
     *        Determine when to cross over from blocked to unblocked code.
     **/
    nx = max( 0, ilaenv( 3, "dgelqf", " ", m, n, -1, -1 ) );
    if( nx<k ) {
      /**
       *           Determine if workspace is large enough for blocked code.
       **/
      ldwork = m;
      iws = ldwork*nb;
      if( lwork<iws ) {
        /**
         *              Not enough workspace to use optimal NB:  reduce NB and
         *              determine the minimum value of NB.
         **/
        nb = lwork / ldwork;
        nbmin = max( 2, ilaenv( 2, "dgelqf", " ", m, n, -1,
                               -1 ) );
      }
    }
  }

  if( nb>=nbmin && nb<k && nx<k ) {
    /**
     *        Use blocked code initially
     **/
    for (i=1 ; nb>0?i<=k - nx:i>=k - nx ; i+=nb) {
      ib = min( k-i+1, nb );
      /**
       *           Compute the LQ factorization of the current block
       *           A(i:i+ib-1,i:n)
       **/
      dgelq2( ib, n-i+1, &a_2( i, i ), lda, &tau_1( i ), work,
             &iinfo );
      if( i+ib<=m ) {
        /**
         *              Form the triangular factor of the block reflector
         *              H = H(i) H(i+1) . . . H(i+ib-1)
         **/
        dlarft( 'f'/*orward*/, 'r'/*owwise*/, n-i+1, ib, &a_2( i, i ),
               lda, &tau_1( i ), work, ldwork );
        /**
         *              Apply H to A(i+ib:m,i:n) from the right
         **/
        dlarfb( 'r'/*ight*/, 'n'/*o transpose*/, 'f'/*orward*/,
               'r'/*owwise*/, m-i-ib+1, n-i+1, ib, &a_2( i, i ),
               lda, work, ldwork, &a_2( i+ib, i ), lda,
               &work_1( ib+1 ), ldwork );
      }
    }
  } else {
    i = 1;
  }
  /**
   *     Use unblocked code to factor the last or only block.
   **/
  if( i<=k )
    dgelq2( m-i+1, n-i+1, &a_2( i, i ), lda, &tau_1( i ), work,
           &iinfo );

  work_1( 1 ) = iws;
  return;
  /**
   *     End of DGELQF
   **/
}



void dgelq2( long m, long n, double a[], long lda, double tau[], double work[], long *info )
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
#undef work_1
#define work_1(a1) work[a1-1]
#undef tau_1
#define tau_1(a1) tau[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGELQ2 computes an LQ factorization of a real m by n matrix A:
   *  A = L * Q.
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
   *          On entry, the m by n matrix A.
   *          On exit, the elements on and below the diagonal of the array
   *          contain the m by min(m,n) lower trapezoidal matrix L (L is
   *          lower triangular if m <= n); the elements above the diagonal,
   *          with the array TAU, represent the orthogonal matrix Q as a
   *          product of elementary reflectors (see Further Details).
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
   *          The scalar factors of the elementary reflectors (see Further
   *          Details).
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
   *
   *  INFO    (output) INTEGER
   *          = 0: successful exit
   *          < 0: if INFO = -i, the i-th argument had an illegal value
   *
   *  Further Details
   *  ===============
   *
   *  The matrix Q is represented as a product of elementary reflectors
   *
   *     Q = H(k) . . . H(2) H(1), where k = min(m,n).
   *
   *  Each H(i) has the form
   *
   *     H(i) = I - tau * v * v'
   *
   *  where tau is a real scalar, and v is a real vector with
   *  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
   *  and tau in TAU(i).
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef one
#define one 1.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  long            i, k;
  double    aii;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max, min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input arguments
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
    xerbla( "dgelq2", -*info );
    return;
  }

  k = min( m, n );

  for (i=1 ; i<=k ; i+=1) {
    /**
     *        Generate elementary reflector H(i) to annihilate A(i,i+1:n)
     **/
    dlarfg( n-i+1, &a_2( i, i ), &a_2( i, min( i+1, n ) ), lda,
           &tau_1( i ) );
    if( i<m ) {
      /**
       *           Apply H(i) to A(i+1:m,i:n) from the right
       **/
      aii = a_2( i, i );
      a_2( i, i ) = one;
      dlarf( 'r'/*ight*/, m-i, n-i+1, &a_2( i, i ), lda, tau_1( i ),
            &a_2( i+1, i ), lda, work );
      a_2( i, i ) = aii;
    }
  }
  return;
  /**
   *     End of DGELQ2
   **/
}



void dormqr( char side, char trans, long m, long n, long k, double a[], long lda, double tau[], double c[], long ldc,double work[], long lwork, long *info )
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
#undef work_1
#define work_1(a1) work[a1-1]
#undef tau_1
#define tau_1(a1) tau[a1-1]
#undef c_2
#define c_2(a1,a2) c[a1-1+ldc*(a2-1)]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DORMQR overwrites the general real M-by-N matrix C with
   *
   *                  SIDE = 'L'     SIDE = 'R'
   *  TRANS = 'N':      Q * C          C * Q
   *  TRANS = 'T':      Q**T * C       C * Q**T
   *
   *  where Q is a real orthogonal matrix defined as the product of k
   *  elementary reflectors
   *
   *        Q = H(1) H(2) . . . H(k)
   *
   *  as returned by DGEQRF. Q is of order M if SIDE = 'L' and of order N
   *  if SIDE = 'R'.
   *
   *  Arguments
   *  =========
   *
   *  SIDE    (input) CHARACTER*1
   *          = 'L': apply Q or Q**T from the Left;
   *          = 'R': apply Q or Q**T from the Right.
   *
   *  TRANS   (input) CHARACTER*1
   *          = 'N':  No transpose, apply Q;
   *          = 'T':  Transpose, apply Q**T.
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix C. M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix C. N >= 0.
   *
   *  K       (input) INTEGER
   *          The number of elementary reflectors whose product defines
   *          the matrix Q.
   *          If SIDE = 'L', M >= K >= 0;
   *          if SIDE = 'R', N >= K >= 0.
   *
   *  A       (input) DOUBLE PRECISION array, dimension (LDA,K)
   *          The i-th column must contain the vector which defines the
   *          elementary reflector H(i), for i = 1,2,...,k, as returned by
   *          DGEQRF in the first k columns of its array argument A.
   *          A is modified by the routine but restored on exit.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.
   *          If SIDE = 'L', LDA >= max(1,M);
   *          if SIDE = 'R', LDA >= max(1,N).
   *
   *  TAU     (input) DOUBLE PRECISION array, dimension (K)
   *          TAU(i) must contain the scalar factor of the elementary
   *          reflector H(i), as returned by DGEQRF.
   *
   *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
   *          On entry, the M-by-N matrix C.
   *          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
   *
   *  LDC     (input) INTEGER
   *          The leading dimension of the array C. LDC >= max(1,M).
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
   *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
   *
   *  LWORK   (input) INTEGER
   *          The dimension of the array WORK.
   *          If SIDE = 'L', LWORK >= max(1,N);
   *          if SIDE = 'R', LWORK >= max(1,M).
   *          For optimum performance LWORK >= N*NB if SIDE = 'L', and
   *          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
   *          blocksize.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument had an illegal value
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef nbmax
#define nbmax 64
#undef ldt
#define ldt (nbmax+1)
  /**     ..
   *     .. Local Scalars ..*/
  int            left, notran;
  long    i, i1, i2, i3, ib, ic=0, iinfo, iws, jc=0, ldwork,
          mi=0, nb, nbmin, ni=0, nq, nw;
  char side_trans[3];
  /**     ..
   *     .. Local Arrays ..*/
  double    t[nbmax][ldt];
#undef t_2
#define t_2(a1,a2) t[a2-1][a1-1]
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max, min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input arguments
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  left = lsame( side, 'l' );
  notran = lsame( trans, 'n' );
  /**
   *     NQ is the order of Q and NW is the minimum dimension of WORK
   **/
  if( left ) {
    nq = m;
    nw = n;
  } else {
    nq = n;
    nw = m;
  }
  if( !left && !lsame( side, 'r' ) ) {
    *info = -1;
  } else if( !notran && !lsame( trans, 't' ) ) {
    *info = -2;
  } else if( m<0 ) {
    *info = -3;
  } else if( n<0 ) {
    *info = -4;
  } else if( k<0 || k>nq ) {
    *info = -5;
  } else if( lda<max( 1, nq ) ) {
    *info = -7;
  } else if( ldc<max( 1, m ) ) {
    *info = -10;
  } else if( lwork<max( 1, nw ) ) {
    *info = -12;
  }
  if( *info!=0 ) {
    xerbla( "dormqr", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( m==0 || n==0 || k==0 ) {
    work_1( 1 ) = 1;
    return;
  }
  /**
   *     Determine the block size.  NB may be at most NBMAX, where NBMAX
   *     is used to define the local array T.
   **/
  side_trans[0]= side;
  side_trans[1]= trans;
  side_trans[2]= '\0';
  nb = min( nbmax, ilaenv( 1, "dormqr", side_trans, m, n, k,
                          -1 ) );
  nbmin = 2;
  ldwork = nw;
  if( nb>1 && nb<k ) {
    iws = nw*nb;
    if( lwork<iws ) {
      nb = lwork / ldwork;
      nbmin = max( 2, ilaenv( 2, "dormqr", side_trans, m, n, k,
                             -1 ) );
    }
  } else {
    iws = nw;
  }

  if( nb<nbmin || nb>=k ) {
    /**
     *        Use unblocked code
     **/
    dorm2r( side, trans, m, n, k, a, lda, tau, c, ldc, work,
           &iinfo );
  } else {
    /**
     *        Use blocked code
     **/
    if( ( left && !notran ) ||
       ( !left && notran ) ) {
      i1 = 1;
      i2 = k;
      i3 = nb;
    } else {
      i1 = ( ( k-1 ) / nb )*nb + 1;
      i2 = 1;
      i3 = -nb;
    }

    if( left ) {
      ni = n;
      jc = 1;
    } else {
      mi = m;
      ic = 1;
    }

    for (i=i1 ; i3>0?i<=i2:i>=i2 ; i+=i3) {
      ib = min( nb, k-i+1 );
      /**
       *           Form the triangular factor of the block reflector
       *           H = H(i) H(i+1) . . . H(i+ib-1)
       **/
      dlarft( 'f'/*orward*/, 'c'/*olumnwise*/, nq-i+1, ib, &a_2( i, i ),
             lda, &tau_1( i ), (double *)t, ldt );
      if( left ) {
        /**
         *              H or H' is applied to C(i:m,1:n)
         **/
        mi = m - i + 1;
        ic = i;
      } else {
        /**
         *              H or H' is applied to C(1:m,i:n)
         **/
        ni = n - i + 1;
        jc = i;
      }
      /**
       *           Apply H or H'
       **/
      dlarfb( side, trans, 'f'/*orward*/, 'c'/*olumnwise*/, mi, ni,
             ib, &a_2( i, i ), lda, (double *)t, ldt, &c_2( ic, jc ), ldc,
             work, ldwork );
    }
  }
  work_1( 1 ) = iws;
  return;
  /**
   *     End of DORMQR
   **/
#undef nbmax
#undef ldt
}



void dorm2r( char side, char trans, long m, long n, long k, double a[], long lda, double tau[], double c[], long ldc,double work[], long *info )
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
#undef work_1
#define work_1(a1) work[a1-1]
#undef tau_1
#define tau_1(a1) tau[a1-1]
#undef c_2
#define c_2(a1,a2) c[a1-1+ldc*(a2-1)]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DORM2R overwrites the general real m by n matrix C with
   *
   *        Q * C  if SIDE = 'L' and TRANS = 'N', or
   *
   *        Q'* C  if SIDE = 'L' and TRANS = 'T', or
   *
   *        C * Q  if SIDE = 'R' and TRANS = 'N', or
   *
   *        C * Q' if SIDE = 'R' and TRANS = 'T',
   *
   *  where Q is a real orthogonal matrix defined as the product of k
   *  elementary reflectors
   *
   *        Q = H(1) H(2) . . . H(k)
   *
   *  as returned by DGEQRF. Q is of order m if SIDE = 'L' and of order n
   *  if SIDE = 'R'.
   *
   *  Arguments
   *  =========
   *
   *  SIDE    (input) CHARACTER*1
   *          = 'L': apply Q or Q' from the Left
   *          = 'R': apply Q or Q' from the Right
   *
   *  TRANS   (input) CHARACTER*1
   *          = 'N': apply Q  (No transpose)
   *          = 'T': apply Q' (Transpose)
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix C. M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix C. N >= 0.
   *
   *  K       (input) INTEGER
   *          The number of elementary reflectors whose product defines
   *          the matrix Q.
   *          If SIDE = 'L', M >= K >= 0;
   *          if SIDE = 'R', N >= K >= 0.
   *
   *  A       (input) DOUBLE PRECISION array, dimension (LDA,K)
   *          The i-th column must contain the vector which defines the
   *          elementary reflector H(i), for i = 1,2,...,k, as returned by
   *          DGEQRF in the first k columns of its array argument A.
   *          A is modified by the routine but restored on exit.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.
   *          If SIDE = 'L', LDA >= max(1,M);
   *          if SIDE = 'R', LDA >= max(1,N).
   *
   *  TAU     (input) DOUBLE PRECISION array, dimension (K)
   *          TAU(i) must contain the scalar factor of the elementary
   *          reflector H(i), as returned by DGEQRF.
   *
   *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
   *          On entry, the m by n matrix C.
   *          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
   *
   *  LDC     (input) INTEGER
   *          The leading dimension of the array C. LDC >= max(1,M).
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension
   *                                   (N) if SIDE = 'L',
   *                                   (M) if SIDE = 'R'
   *
   *  INFO    (output) INTEGER
   *          = 0: successful exit
   *          < 0: if INFO = -i, the i-th argument had an illegal value
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef one
#define one 1.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  int            left, notran;
  long            i, i1, i2, i3, ic=0, jc=0, mi=0, ni=0, nq;
  double    aii;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input arguments
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  left = lsame( side, 'l' );
  notran = lsame( trans, 'n' );
  /**
   *     NQ is the order of Q
   **/
  if( left ) {
    nq = m;
  } else {
    nq = n;
  }
  if( !left && !lsame( side, 'r' ) ) {
    *info = -1;
  } else if( !notran && !lsame( trans, 't' ) ) {
    *info = -2;
  } else if( m<0 ) {
    *info = -3;
  } else if( n<0 ) {
    *info = -4;
  } else if( k<0 || k>nq ) {
    *info = -5;
  } else if( lda<max( 1, nq ) ) {
    *info = -7;
  } else if( ldc<max( 1, m ) ) {
    *info = -10;
  }
  if( *info!=0 ) {
    xerbla( "dorm2r", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( m==0 || n==0 || k==0 )
    return;

  if( ( left && !notran ) || ( !left && notran ) )
    {
      i1 = 1;
      i2 = k;
      i3 = 1;
    } else {
      i1 = k;
      i2 = 1;
      i3 = -1;
    }

  if( left ) {
    ni = n;
    jc = 1;
  } else {
    mi = m;
    ic = 1;
  }

  for (i=i1 ; i3>0?i<=i2:i>=i2 ; i+=i3) {
    if( left ) {
      /**
       *           H(i) is applied to C(i:m,1:n)
       **/
      mi = m - i + 1;
      ic = i;
    } else {
      /**
       *           H(i) is applied to C(1:m,i:n)
       **/
      ni = n - i + 1;
      jc = i;
    }
    /**
     *        Apply H(i)
     **/
    aii = a_2( i, i );
    a_2( i, i ) = one;
    dlarf( side, mi, ni, &a_2( i, i ), 1, tau_1( i ), &c_2( ic, jc ),
          ldc, work );
    a_2( i, i ) = aii;
  }
  return;
  /**
   *     End of DORM2R
   **/
}



void dgeqrf( long m, long n, double a[], long lda, double tau[], double work[], long lwork, long *info )
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
#undef work_1
#define work_1(a1) work[a1-1]
#undef tau_1
#define tau_1(a1) tau[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGEQRF computes a QR factorization of a real M-by-N matrix A:
   *  A = Q * R.
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
   *          On entry, the M-by-N matrix A.
   *          On exit, the elements on and above the diagonal of the array
   *          contain the min(M,N)-by-N upper trapezoidal matrix R (R is
   *          upper triangular if m >= n); the elements below the diagonal,
   *          with the array TAU, represent the orthogonal matrix Q as a
   *          product of min(m,n) elementary reflectors (see Further
   *          Details).
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
   *          The scalar factors of the elementary reflectors (see Further
   *          Details).
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
   *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
   *
   *  LWORK   (input) INTEGER
   *          The dimension of the array WORK.  LWORK >= max(1,N).
   *          For optimum performance LWORK >= N*NB, where NB is
   *          the optimal blocksize.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument had an illegal value
   *
   *  Further Details
   *  ===============
   *
   *  The matrix Q is represented as a product of elementary reflectors
   *
   *     Q = H(1) H(2) . . . H(k), where k = min(m,n).
   *
   *  Each H(i) has the form
   *
   *     H(i) = I - tau * v * v'
   *
   *  where tau is a real scalar, and v is a real vector with
   *  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
   *  and tau in TAU(i).
   *
   *  =====================================================================
   *
   *     .. Local Scalars ..*/
  long            i, ib, iinfo, iws, k, ldwork=0, nb, nbmin, nx;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max, min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input arguments
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
  } else if( lwork<max( 1, n ) ) {
    *info = -7;
  }
  if( *info!=0 ) {
    xerbla( "dgeqrf", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  k = min( m, n );
  if( k==0 ) {
    work_1( 1 ) = 1;
    return;
  }
  /**
   *     Determine the block size.
   **/
  nb = ilaenv( 1, "dgeqrf", " ", m, n, -1, -1 );
  nbmin = 2;
  nx = 0;
  iws = n;
  if( nb>1 && nb<k ) {
    /**
     *        Determine when to cross over from blocked to unblocked code.
     **/
    nx = max( 0, ilaenv( 3, "dgeqrf", " ", m, n, -1, -1 ) );
    if( nx<k ) {
      /**
       *           Determine if workspace is large enough for blocked code.
       **/
      ldwork = n;
      iws = ldwork*nb;
      if( lwork<iws ) {
        /**
         *              Not enough workspace to use optimal NB:  reduce NB and
         *              determine the minimum value of NB.
         **/
        nb = lwork / ldwork;
        nbmin = max( 2, ilaenv( 2, "dgeqrf", " ", m, n, -1,
                               -1 ) );
      }
    }
  }

  if( nb>=nbmin && nb<k && nx<k ) {
    /**
     *        Use blocked code initially
     **/
    for (i=1 ; nb>0?i<=k - nx:i>=k - nx ; i+=nb) {
      ib = min( k-i+1, nb );
      /**
       *           Compute the QR factorization of the current block
       *           A(i:m,i:i+ib-1)
       **/
      dgeqr2( m-i+1, ib, &a_2( i, i ), lda, &tau_1( i ), work,
             &iinfo );
      if( i+ib<=n ) {
        /**
         *              Form the triangular factor of the block reflector
         *              H = H(i) H(i+1) . . . H(i+ib-1)
         **/
        dlarft( 'f'/*orward*/, 'c'/*olumnwise*/, m-i+1, ib,
               &a_2( i, i ), lda, &tau_1( i ), work, ldwork );
        /**
         *              Apply H' to A(i:m,i+ib:n) from the left
         **/
        dlarfb( 'l'/*eft*/, 't'/*ranspose*/, 'f'/*orward*/,
               'c'/*olumnwise*/, m-i+1, n-i-ib+1, ib,
               &a_2( i, i ), lda, work, ldwork, &a_2( i, i+ib ),
               lda, &work_1( ib+1 ), ldwork );
      }
    }
  } else {
    i = 1;
  }
  /**
   *     Use unblocked code to factor the last or only block.
   **/
  if( i<=k )
    dgeqr2( m-i+1, n-i+1, &a_2( i, i ), lda, &tau_1( i ), work,
           &iinfo );

  work_1( 1 ) = iws;
  return;
  /**
   *     End of DGEQRF
   **/
}



void dlarfb( char side, char trans, char direct, char storev,
            long m, long n, long k, double v[], long ldv,
            double t[], long ldt, double c[], long ldc,
            double work[], long ldwork )
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
#undef work_2
#define work_2(a1,a2) work[a1-1+ldwork*(a2-1)]
#undef v_2
#define v_2(a1,a2) v[a1-1+ldv*(a2-1)]
#undef t_2
#define t_2(a1,a2) t[a1-1+ldt*(a2-1)]
#undef c_2
#define c_2(a1,a2) c[a1-1+ldc*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLARFB applies a real block reflector H or its transpose H' to a
   *  real m by n matrix C, from either the left or the right.
   *
   *  Arguments
   *  =========
   *
   *  SIDE    (input) CHARACTER*1
   *          = 'L': apply H or H' from the Left
   *          = 'R': apply H or H' from the Right
   *
   *  TRANS   (input) CHARACTER*1
   *          = 'N': apply H (No transpose)
   *          = 'T': apply H' (Transpose)
   *
   *  DIRECT  (input) CHARACTER*1
   *          Indicates how H is formed from a product of elementary
   *          reflectors
   *          = 'F': H = H(1) H(2) . . . H(k) (Forward)
   *          = 'B': H = H(k) . . . H(2) H(1) (Backward)
   *
   *  STOREV  (input) CHARACTER*1
   *          Indicates how the vectors which define the elementary
   *          reflectors are stored:
   *          = 'C': Columnwise
   *          = 'R': Rowwise
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix C.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix C.
   *
   *  K       (input) INTEGER
   *          The order of the matrix T (= the number of elementary
   *          reflectors whose product defines the block reflector).
   *
   *  V       (input) DOUBLE PRECISION array, dimension
   *                                (LDV,K) if STOREV = 'C'
   *                                (LDV,M) if STOREV = 'R' and SIDE = 'L'
   *                                (LDV,N) if STOREV = 'R' and SIDE = 'R'
   *          The matrix V. See further details.
   *
   *  LDV     (input) INTEGER
   *          The leading dimension of the array V.
   *          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
   *          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
   *          if STOREV = 'R', LDV >= K.
   *
   *  T       (input) DOUBLE PRECISION array, dimension (LDT,K)
   *          The triangular k by k matrix T in the representation of the
   *          block reflector.
   *
   *  LDT     (input) INTEGER
   *          The leading dimension of the array T. LDT >= K.
   *
   *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
   *          On entry, the m by n matrix C.
   *          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
   *
   *  LDC     (input) INTEGER
   *          The leading dimension of the array C. LDA >= max(1,M).
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (LDWORK,K)
   *
   *  LDWORK  (input) INTEGER
   *          The leading dimension of the array WORK.
   *          If SIDE = 'L', LDWORK >= max(1,N);
   *          if SIDE = 'R', LDWORK >= max(1,M).
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef one
#define one 1.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  enum CBLAS_TRANSPOSE transt, transf;
  long            i, j;
  /**     ..
   *     .. Executable Statements ..
   *
   *     Quick return if possible
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if( m<=0 || n<=0 )
    return;

  if( lsame( trans, 'n' ) ) {
    transf = CblasNoTrans;
    transt = CblasTrans;
  } else {
    transf = CblasTrans;
    transt = CblasNoTrans;
  }

  if( lsame( storev, 'c' ) ) {

    if( lsame( direct, 'f' ) ) {
      /**
       *           Let  V =  ( V1 )    (first K rows)
       *                     ( V2 )
       *           where  V1  is unit lower triangular.
       **/
      if( lsame( side, 'l' ) ) {
        /**
         *              Form  H * C  or  H' * C  where  C = ( C1 )
         *                                                  ( C2 )
         *
         *              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
         *
         *              W := C1'
         **/
        for (j=1 ; j<=k ; j+=1) {
          cblas_dcopy( n, &c_2( j, 1 ), ldc, &work_2( 1, j ), 1 );
        }
        /**
         *              W := W * V1
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasLower, CblasNoTrans,
                    CblasUnit, n, k, one, v, ldv, work, ldwork );
        if( m>k ) {
          /**
           *                 W := W + C2'*V2
           **/
          cblas_dgemm(CblasColMajor, CblasTrans, CblasNoTrans, n, k, m-k,
                      one, &c_2( k+1, 1 ), ldc, &v_2( k+1, 1 ), ldv,
                      one, work, ldwork );
        }
        /**
         *              W := W * T'  or  W * T
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasUpper, transt,
                    CblasNonUnit, n, k, one, t, ldt, work, ldwork );
        /**
         *              C := C - V * W'
         **/
        if( m>k ) {
          /**
           *                 C2 := C2 - V2 * W'
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasTrans, m-k, n, k,
                      -one, &v_2( k+1, 1 ), ldv, work, ldwork, one,
                      &c_2( k+1, 1 ), ldc );
        }
        /**
         *              W := W * V1'
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasLower,  CblasTrans,
                    CblasUnit, n, k, one, v, ldv, work, ldwork );
        /**
         *              C1 := C1 - W'
         **/
        for (j=1 ; j<=k ; j+=1) {
          for (i=1 ; i<=n ; i+=1) {
            c_2( j, i ) = c_2( j, i ) - work_2( i, j );
          }
        }

      } else if( lsame( side, 'r' ) ) {
        /**
         *              Form  C * H  or  C * H'  where  C = ( C1  C2 )
         *
         *              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
         *
         *              W := C1
         **/
        for (j=1 ; j<=k ; j+=1) {
          cblas_dcopy( m, &c_2( 1, j ), 1, &work_2( 1, j ), 1 );
        }
        /**
         *              W := W * V1
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasLower, CblasNoTrans,
                    CblasUnit, m, k, one, v, ldv, work, ldwork );
        if( n>k ) {
          /**
           *                 W := W + C2 * V2
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, k, n-k,
                      one, &c_2( 1, k+1 ), ldc, &v_2( k+1, 1 ), ldv,
                      one, work, ldwork );
        }
        /**
         *              W := W * T  or  W * T'
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasUpper, transf,
                    CblasNonUnit, m, k, one, t, ldt, work, ldwork );
        /**
         *              C := C - W * V'
         **/
        if( n>k ) {
          /**
           *                 C2 := C2 - W * V2'
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasTrans, m, n-k, k,
                      -one, work, ldwork, &v_2( k+1, 1 ), ldv, one,
                      &c_2( 1, k+1 ), ldc );
        }
        /**
         *              W := W * V1'
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasLower, CblasTrans,
                    CblasUnit, m, k, one, v, ldv, work, ldwork );
        /**
         *              C1 := C1 - W
         **/
        for (j=1 ; j<=k ; j+=1) {
          for (i=1 ; i<=m ; i+=1) {
            c_2( i, j ) = c_2( i, j ) - work_2( i, j );
          }
        }
      }

    } else {
      /**
       *           Let  V =  ( V1 )
       *                     ( V2 )    (last K rows)
       *           where  V2  is unit upper triangular.
       **/
      if( lsame( side, 'l' ) ) {
        /**
         *              Form  H * C  or  H' * C  where  C = ( C1 )
         *                                                  ( C2 )
         *
         *              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
         *
         *              W := C2'
         **/
        for (j=1 ; j<=k ; j+=1) {
          cblas_dcopy( n, &c_2( m-k+j, 1 ), ldc, &work_2( 1, j ), 1 );
        }
        /**
         *              W := W * V2
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasUpper, CblasNoTrans,
                    CblasUnit, n, k, one, &v_2( m-k+1, 1 ), ldv,
                    work, ldwork );
        if( m>k ) {
          /**
           *                 W := W + C1'*V1
           **/
          cblas_dgemm(CblasColMajor, CblasTrans, CblasNoTrans, n, k, m-k,
                      one, c, ldc, v, ldv, one, work, ldwork );
        }
        /**
         *              W := W * T'  or  W * T
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasLower, transt,
                    CblasNonUnit, n, k, one, t, ldt, work, ldwork );
        /**
         *              C := C - V * W'
         **/
        if( m>k ) {
          /**
           *                 C1 := C1 - V1 * W'
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasTrans, m-k, n, k,
                      -one, v, ldv, work, ldwork, one, c, ldc );
        }
        /**
         *              W := W * V2'
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasUpper, CblasTrans,
                    CblasUnit, n, k, one, &v_2( m-k+1, 1 ), ldv,
                    work, ldwork );
        /**
         *              C2 := C2 - W'
         **/
        for (j=1 ; j<=k ; j+=1) {
          for (i=1 ; i<=n ; i+=1) {
            c_2( m-k+j, i ) = c_2( m-k+j, i ) - work_2( i, j );
          }
        }

      } else if( lsame( side, 'r' ) ) {
        /**
         *              Form  C * H  or  C * H'  where  C = ( C1  C2 )
         *
         *              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
         *
         *              W := C2
         **/
        for (j=1 ; j<=k ; j+=1) {
          cblas_dcopy( m, &c_2( 1, n-k+j ), 1, &work_2( 1, j ), 1 );
        }
        /**
         *              W := W * V2
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasUpper, CblasNoTrans,
                    CblasUnit, m, k, one, &v_2( n-k+1, 1 ), ldv,
                    work, ldwork );
        if( n>k ) {
          /**
           *                 W := W + C1 * V1
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, k, n-k,
                      one, c, ldc, v, ldv, one, work, ldwork );
        }
        /**
         *              W := W * T  or  W * T'
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasLower, transf,
                    CblasNonUnit, m, k, one, t, ldt, work, ldwork );
        /**
         *              C := C - W * V'
         **/
        if( n>k ) {
          /**
           *                 C1 := C1 - W * V1'
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasTrans, m, n-k, k,
                      -one, work, ldwork, v, ldv, one, c, ldc );
        }
        /**
         *              W := W * V2'
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasUpper, CblasTrans,
                    CblasUnit, m, k, one, &v_2( n-k+1, 1 ), ldv,
                    work, ldwork );
        /**
         *              C2 := C2 - W
         **/
        for (j=1 ; j<=k ; j+=1) {
          for (i=1 ; i<=m ; i+=1) {
            c_2( i, n-k+j ) = c_2( i, n-k+j ) - work_2( i, j );
          }
        }
      }
    }

  } else if( lsame( storev, 'r' ) ) {

    if( lsame( direct, 'f' ) ) {
      /**
       *           Let  V =  ( V1  V2 )    (V1: first K columns)
       *           where  V1  is unit upper triangular.
       **/
      if( lsame( side, 'l' ) ) {
        /**
         *              Form  H * C  or  H' * C  where  C = ( C1 )
         *                                                  ( C2 )
         *
         *              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
         *
         *              W := C1'
         **/
        for (j=1 ; j<=k ; j+=1) {
          cblas_dcopy( n, &c_2( j, 1 ), ldc, &work_2( 1, j ), 1 );
        }
        /**
         *              W := W * V1'
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasUpper, CblasTrans,
                    CblasUnit, n, k, one, v, ldv, work, ldwork );
        if( m>k ) {
          /**
           *                 W := W + C2'*V2'
           **/
          cblas_dgemm(CblasColMajor, CblasTrans, CblasTrans, n, k, m-k, one,
                      &c_2( k+1, 1 ), ldc, &v_2( 1, k+1 ), ldv, one,
                      work, ldwork );
        }
        /**
         *              W := W * T'  or  W * T
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasUpper, transt,
                    CblasNonUnit, n, k, one, t, ldt, work, ldwork );
        /**
         *              C := C - V' * W'
         **/
        if( m>k ) {
          /**
           *                 C2 := C2 - V2' * W'
           **/
          cblas_dgemm(CblasColMajor, CblasTrans, CblasTrans, m-k, n, k, -one,
                      &v_2( 1, k+1 ), ldv, work, ldwork, one,
                      &c_2( k+1, 1 ), ldc );
        }
        /**
         *              W := W * V1
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasUpper, CblasNoTrans,
                    CblasUnit, n, k, one, v, ldv, work, ldwork );
        /**
         *              C1 := C1 - W'
         **/
        for (j=1 ; j<=k ; j+=1) {
          for (i=1 ; i<=n ; i+=1) {
            c_2( j, i ) = c_2( j, i ) - work_2( i, j );
          }
        }

      } else if( lsame( side, 'r' ) ) {
        /**
         *              Form  C * H  or  C * H'  where  C = ( C1  C2 )
         *
         *              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
         *
         *              W := C1
         **/
        for (j=1 ; j<=k ; j+=1) {
          cblas_dcopy( m, &c_2( 1, j ), 1, &work_2( 1, j ), 1 );
        }
        /**
         *              W := W * V1'
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasUpper, CblasTrans,
                    CblasUnit, m, k, one, v, ldv, work, ldwork );
        if( n>k ) {
          /**
           *                 W := W + C2 * V2'
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasTrans, m, k, n-k,
                      one, &c_2( 1, k+1 ), ldc, &v_2( 1, k+1 ), ldv,
                      one, work, ldwork );
        }
        /**
         *              W := W * T  or  W * T'
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasUpper, transf,
                    CblasNonUnit, m, k, one, t, ldt, work, ldwork );
        /**
         *              C := C - W * V
         **/
        if( n>k ) {
          /**
           *                 C2 := C2 - W * V2
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n-k, k,
                      -one, work, ldwork, &v_2( 1, k+1 ), ldv, one,
                      &c_2( 1, k+1 ), ldc );
        }
        /**
         *              W := W * V1
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasUpper, CblasNoTrans,
                    CblasUnit, m, k, one, v, ldv, work, ldwork );
        /**
         *              C1 := C1 - W
         **/
        for (j=1 ; j<=k ; j+=1) {
          for (i=1 ; i<=m ; i+=1) {
            c_2( i, j ) = c_2( i, j ) - work_2( i, j );
          }
        }

      }

    } else {
      /**
       *           Let  V =  ( V1  V2 )    (V2: last K columns)
       *           where  V2  is unit lower triangular.
       **/
      if( lsame( side, 'l' ) ) {
        /**
         *              Form  H * C  or  H' * C  where  C = ( C1 )
         *                                                  ( C2 )
         *
         *              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
         *
         *              W := C2'
         **/
        for (j=1 ; j<=k ; j+=1) {
          cblas_dcopy( n, &c_2( m-k+j, 1 ), ldc, &work_2( 1, j ), 1 );
        }
        /**
         *              W := W * V2'
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasLower, CblasTrans,
                    CblasUnit, n, k, one, &v_2( 1, m-k+1 ), ldv,
                    work, ldwork );
        if( m>k ) {
          /**
           *                 W := W + C1'*V1'
           **/
          cblas_dgemm(CblasColMajor, CblasTrans, CblasTrans, n, k, m-k, one,
                      c, ldc, v, ldv, one, work, ldwork );
        }
        /**
         *              W := W * T'  or  W * T
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasLower, transt,
                    CblasNonUnit, n, k, one, t, ldt, work, ldwork );
        /**
         *              C := C - V' * W'
         **/
        if( m>k ) {
          /**
           *                 C1 := C1 - V1' * W'
           **/
          cblas_dgemm(CblasColMajor, CblasTrans, CblasTrans, m-k, n, k, -one,
                      v, ldv, work, ldwork, one, c, ldc );
        }
        /**
         *              W := W * V2
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasLower, CblasNoTrans,
                    CblasUnit, n, k, one, &v_2( 1, m-k+1 ), ldv,
                    work, ldwork );
        /**
         *              C2 := C2 - W'
         **/
        for (j=1 ; j<=k ; j+=1) {
          for (i=1 ; i<=n ; i+=1) {
            c_2( m-k+j, i ) = c_2( m-k+j, i ) - work_2( i, j );
          }
        }

      } else if( lsame( side, 'r' ) ) {
        /**
         *              Form  C * H  or  C * H'  where  C = ( C1  C2 )
         *
         *              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
         *
         *              W := C2
         **/
        for (j=1 ; j<=k ; j+=1) {
          cblas_dcopy( m, &c_2( 1, n-k+j ), 1, &work_2( 1, j ), 1 );
        }
        /**
         *              W := W * V2'
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasLower, CblasTrans,
                    CblasUnit, m, k, one, &v_2( 1, n-k+1 ), ldv,
                    work, ldwork );
        if( n>k ) {
          /**
           *                 W := W + C1 * V1'
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasTrans, m, k, n-k,
                      one, c, ldc, v, ldv, one, work, ldwork );
        }
        /**
         *              W := W * T  or  W * T'
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasLower, transf,
                    CblasNonUnit, m, k, one, t, ldt, work, ldwork );
        /**
         *              C := C - W * V
         **/
        if( n>k ) {
          /**
           *                 C1 := C1 - W * V1
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n-k, k,
                      -one, work, ldwork, v, ldv, one, c, ldc );
        }
        /**
         *              W := W * V2
         **/
        cblas_dtrmm(CblasColMajor, CblasRight, CblasLower, CblasNoTrans,
                    CblasUnit, m, k, one, &v_2( 1, n-k+1 ), ldv,
                    work, ldwork );
        /**
         *              C1 := C1 - W
         **/
        for (j=1 ; j<=k ; j+=1) {
          for (i=1 ; i<=m ; i+=1) {
            c_2( i, n-k+j ) = c_2( i, n-k+j ) - work_2( i, j );
          }
        }

      }

    }
  }

  return;
  /**
   *     End of DLARFB
   **/
}



void dlarft( char direct, char storev, long n, long k,
            double v[], long ldv, double tau[], double t[], long ldt )
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
#undef v_2
#define v_2(a1,a2) v[a1-1+ldv*(a2-1)]
#undef tau_1
#define tau_1(a1) tau[a1-1]
#undef t_2
#define t_2(a1,a2) t[a1-1+ldt*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLARFT forms the triangular factor T of a real block reflector H
   *  of order n, which is defined as a product of k elementary reflectors.
   *
   *  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
   *
   *  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
   *
   *  If STOREV = 'C', the vector which defines the elementary reflector
   *  H(i) is stored in the i-th column of the array V, and
   *
   *     H  =  I - V * T * V'
   *
   *  If STOREV = 'R', the vector which defines the elementary reflector
   *  H(i) is stored in the i-th row of the array V, and
   *
   *     H  =  I - V' * T * V
   *
   *  Arguments
   *  =========
   *
   *  DIRECT  (input) CHARACTER*1
   *          Specifies the order in which the elementary reflectors are
   *          multiplied to form the block reflector:
   *          = 'F': H = H(1) H(2) . . . H(k) (Forward)
   *          = 'B': H = H(k) . . . H(2) H(1) (Backward)
   *
   *  STOREV  (input) CHARACTER*1
   *          Specifies how the vectors which define the elementary
   *          reflectors are stored (see also Further Details):
   *          = 'C': columnwise
   *          = 'R': rowwise
   *
   *  N       (input) INTEGER
   *          The order of the block reflector H. N >= 0.
   *
   *  K       (input) INTEGER
   *          The order of the triangular factor T (= the number of
   *          elementary reflectors). K >= 1.
   *
   *  V       (input/output) DOUBLE PRECISION array, dimension
   *                               (LDV,K) if STOREV = 'C'
   *                               (LDV,N) if STOREV = 'R'
   *          The matrix V. See further details.
   *
   *  LDV     (input) INTEGER
   *          The leading dimension of the array V.
   *          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
   *
   *  TAU     (input) DOUBLE PRECISION array, dimension (K)
   *          TAU(i) must contain the scalar factor of the elementary
   *          reflector H(i).
   *
   *  T       (output) DOUBLE PRECISION array, dimension (LDT,K)
   *          The k by k triangular factor T of the block reflector.
   *          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
   *          lower triangular. The rest of the array is not used.
   *
   *  LDT     (input) INTEGER
   *          The leading dimension of the array T. LDT >= K.
   *
   *  Further Details
   *  ===============
   *
   *  The shape of the matrix V and the storage of the vectors which define
   *  the H(i) is best illustrated by the following example with n = 5 and
   *  k = 3. The elements equal to 1 are not stored; the corresponding
   *  array elements are modified but restored on exit. The rest of the
   *  array is not used.
   *
   *  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
   *
   *               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
   *                   ( v1  1    )                     (     1 v2 v2 v2 )
   *                   ( v1 v2  1 )                     (        1 v3 v3 )
   *                   ( v1 v2 v3 )
   *                   ( v1 v2 v3 )
   *
   *  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
   *
   *               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
   *                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
   *                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
   *                   (     1 v3 )
   *                   (        1 )
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
  long            i, j;
  double    vii;
  /**     ..
   *     .. Executable Statements ..
   *
   *     Quick return if possible
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if( n==0 )
    return;

  if( lsame( direct, 'f' ) ) {
    for (i=1 ; i<=k ; i+=1) {
      if( tau_1( i )==zero ) {
        /**
         *              H(i)  =  I
         **/
        for (j=1 ; j<=i ; j+=1) {
          t_2( j, i ) = zero;
        }
      } else {
        /**
         *              general case
         **/
        vii = v_2( i, i );
        v_2( i, i ) = one;
        if( lsame( storev, 'c' ) ) {
          /**
           *                 T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' * V(i:n,i)
           **/
          cblas_dgemv(CblasColMajor, CblasTrans, n-i+1, i-1, -tau_1( i ),
                      &v_2( i, 1 ), ldv, &v_2( i, i ), 1, zero,
                      &t_2( 1, i ), 1 );
        } else {
          /**
           *                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:n) * V(i,i:n)'
           **/
          cblas_dgemv(CblasColMajor, CblasNoTrans, i-1, n-i+1,
                      -tau_1( i ), &v_2( 1, i ), ldv, &v_2( i, i ), ldv, zero,
                      &t_2( 1, i ), 1 );
        }
        v_2( i, i ) = vii;
        /**
         *              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i)
         **/
        cblas_dtrmv(CblasColMajor, CblasUpper, CblasNoTrans, CblasNonUnit,
                    i-1, t, ldt, &t_2( 1, i ), 1 );
        t_2( i, i ) = tau_1( i );
      }
    }
  } else {
    for (i=k ; i>=1 ; i+=-1) {
      if( tau_1( i )==zero ) {
        /**
         *              H(i)  =  I
         **/
        for (j=i ; j<=k ; j+=1) {
          t_2( j, i ) = zero;
        }
      } else {
        /**
         *              general case
         **/
        if( i<k ) {
          if( lsame( storev, 'c' ) ) {
            vii = v_2( n-k+i, i );
            v_2( n-k+i, i ) = one;
            /**
             *             T(i+1:k,i) :=
             *                   - tau(i) * V(1:n-k+i,i+1:k)' * V(1:n-k+i,i)
             **/
            cblas_dgemv(CblasColMajor, CblasTrans, n-k+i, k-i,
                        -tau_1( i ), &v_2( 1, i+1 ), ldv, &v_2( 1, i ),
                        1, zero, &t_2( i+1, i ), 1 );
            v_2( n-k+i, i ) = vii;
          } else {
            vii = v_2( i, n-k+i );
            v_2( i, n-k+i ) = one;
            /**
             *         T(i+1:k,i) :=
             *                  - tau(i) * V(i+1:k,1:n-k+i) * V(i,1:n-k+i)'
             **/
            cblas_dgemv(CblasColMajor, CblasNoTrans, k-i, n-k+i,
                        -tau_1( i ), &v_2( i+1, 1 ), ldv, &v_2( i, 1 ), ldv,
                        zero, &t_2( i+1, i ), 1 );
            v_2( i, n-k+i ) = vii;
          }
          /**
           *                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i)
           **/
          cblas_dtrmv(CblasColMajor, CblasLower, CblasNoTrans, CblasNonUnit,
                      k-i, &t_2( i+1, i+1 ), ldt, &t_2( i+1, i ), 1 );
        }
        t_2( i, i ) = tau_1( i );
      }
    }
  }
  return;
  /**
   *     End of DLARFT
   **/
}



void dgeqr2( long m, long n, double a[], long lda, double tau[],
            double work[], long *info )
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
#undef work_1
#define work_1(a1) work[a1-1]
#undef tau_1
#define tau_1(a1) tau[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGEQR2 computes a QR factorization of a real m by n matrix A:
   *  A = Q * R.
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
   *          On entry, the m by n matrix A.
   *          On exit, the elements on and above the diagonal of the array
   *          contain the min(m,n) by n upper trapezoidal matrix R (R is
   *          upper triangular if m >= n); the elements below the diagonal,
   *          with the array TAU, represent the orthogonal matrix Q as a
   *          product of elementary reflectors (see Further Details).
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
   *          The scalar factors of the elementary reflectors (see Further
   *          Details).
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
   *
   *  INFO    (output) INTEGER
   *          = 0: successful exit
   *          < 0: if INFO = -i, the i-th argument had an illegal value
   *
   *  Further Details
   *  ===============
   *
   *  The matrix Q is represented as a product of elementary reflectors
   *
   *     Q = H(1) H(2) . . . H(k), where k = min(m,n).
   *
   *  Each H(i) has the form
   *
   *     H(i) = I - tau * v * v'
   *
   *  where tau is a real scalar, and v is a real vector with
   *  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
   *  and tau in TAU(i).
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef one
#define one 1.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  long            i, k;
  double    aii;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max, min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input arguments
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
    xerbla( "dgeqr2", -*info );
    return;
  }

  k = min( m, n );

  for (i=1 ; i<=k ; i+=1) {
    /**
     *        Generate elementary reflector H(i) to annihilate A(i+1:m,i)
     **/
    dlarfg( m-i+1, &a_2( i, i ), &a_2( min( i+1, m ), i ), 1,
           &tau_1( i ) );
    if( i<n ) {
      /**
       *           Apply H(i) to A(i:m,i+1:n) from the left
       **/
      aii = a_2( i, i );
      a_2( i, i ) = one;
      dlarf( 'l'/*eft*/, m-i+1, n-i, &a_2( i, i ), 1, tau_1( i ),
            &a_2( i, i+1 ), lda, work );
      a_2( i, i ) = aii;
    }
  }
  return;
  /**
   *     End of DGEQR2
   **/
}



void dlarf( char side, long m, long n, double v[], long incv,
           double tau, double c[], long ldc, double work[] )
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
#undef work_1
#define work_1(a1) work[a1-1]
#undef v_1
#define v_1(a1) v[a1-1]
#undef c_2
#define c_2(a1,a2) c[a1-1+ldc*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLARF applies a real elementary reflector H to a real m by n matrix
   *  C, from either the left or the right. H is represented in the form
   *
   *        H = I - tau * v * v'
   *
   *  where tau is a real scalar and v is a real vector.
   *
   *  If tau = 0, then H is taken to be the unit matrix.
   *
   *  Arguments
   *  =========
   *
   *  SIDE    (input) CHARACTER*1
   *          = 'L': form  H * C
   *          = 'R': form  C * H
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix C.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix C.
   *
   *  V       (input) DOUBLE PRECISION array, dimension
   *                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
   *                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
   *          The vector v in the representation of H. V is not used if
   *          TAU = 0.
   *
   *  INCV    (input) INTEGER
   *          The increment between elements of v. INCV <> 0.
   *
   *  TAU     (input) DOUBLE PRECISION
   *          The value tau in the representation of H.
   *
   *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
   *          On entry, the m by n matrix C.
   *          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
   *          or C * H if SIDE = 'R'.
   *
   *  LDC     (input) INTEGER
   *          The leading dimension of the array C. LDC >= max(1,M).
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension
   *                         (N) if SIDE = 'L'
   *                      or (M) if SIDE = 'R'
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef one
#define one 1.0e+0
#undef zero
#define zero 0.0e+0
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if( lsame( side, 'l' ) ) {
    /**
     *        Form  H * C
     **/
    if( tau!=zero ) {
      /**
       *           w := C' * v
       **/
      cblas_dgemv(CblasColMajor, CblasTrans, m, n, one, c, ldc,
                  v, incv, zero, work, 1 );
      /**
       *           C := C - v * w'
       **/
      cblas_dger(CblasColMajor, m, n, -tau, v, incv, work, 1, c, ldc );
    }
  } else {
    /**
     *        Form  C * H
     **/
    if( tau!=zero ) {
      /**
       *           w := C * v
       **/
      cblas_dgemv(CblasColMajor, CblasNoTrans, m, n, one, c, ldc,
                  v, incv, zero, work, 1 );
      /**
       *           C := C - w * v'
       **/
      cblas_dger(CblasColMajor, m, n, -tau, work, 1, v, incv, c, ldc );
    }
  }
  return;
  /**
   *     End of DLARF
   **/
}



void dlarfg( long n, double *alpha, double x[], long incx, double *tau )
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
#undef x_1
#define x_1(a1) x[a1-1]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLARFG generates a real elementary reflector H of order n, such
   *  that
   *
   *        H * ( alpha ) = ( beta ),   H' * H = I.
   *            (   x   )   (   0  )
   *
   *  where alpha and beta are scalars, and x is an (n-1)-element real
   *  vector. H is represented in the form
   *
   *        H = I - tau * ( 1 ) * ( 1 v' ) ,
   *                      ( v )
   *
   *  where tau is a real scalar and v is a real (n-1)-element
   *  vector.
   *
   *  If the elements of x are all zero, then tau = 0 and H is taken to be
   *  the unit matrix.
   *
   *  Otherwise  1 <= tau <= 2.
   *
   *  Arguments
   *  =========
   *
   *  N       (input) INTEGER
   *          The order of the elementary reflector.
   *
   *  ALPHA   (input/output) DOUBLE PRECISION
   *          On entry, the value alpha.
   *          On exit, it is overwritten with the value beta.
   *
   *  X       (input/output) DOUBLE PRECISION array, dimension
   *                         (1+(N-2)*abs(INCX))
   *          On entry, the vector x.
   *          On exit, it is overwritten with the vector v.
   *
   *  INCX    (input) INTEGER
   *          The increment between elements of X. INCX <> 0.
   *
   *  TAU     (output) DOUBLE PRECISION
   *          The value tau.
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
  long            j, knt;
  double    beta, rsafmn, safmin, xnorm;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, sign;*/
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if( n<=1 ) {
    *tau = zero;
    return;
  }

  xnorm = cblas_dnrm2( n-1, x, incx );

  if( xnorm==zero ) {
    /**
     *        H  =  I
     **/
    *tau = zero;
  } else {
    /**
     *        general case
     **/
    beta = -sign( dlapy2( *alpha, xnorm ), *alpha );
    safmin = dlamch( 's' );
    if( abs( beta )<safmin ) {
      /**
       *           XNORM, BETA may be inaccurate; scale X and recompute them
       **/
      rsafmn = one / safmin;
      knt = 0;
    L_10:
      knt = knt + 1;
      cblas_dscal( n-1, rsafmn, x, incx );
      beta = beta*rsafmn;
      *alpha = *alpha*rsafmn;
      if( abs( beta )<safmin )
        goto L_10;
      /**
       *           New BETA is at most 1, at least SAFMIN
       **/
      xnorm = cblas_dnrm2( n-1, x, incx );
      beta = -sign( dlapy2( *alpha, xnorm ), *alpha );
      *tau = ( beta-*alpha ) / beta;
      cblas_dscal( n-1, one / ( *alpha-beta ), x, incx );
      /**
       *           If ALPHA is subnormal, it may lose relative accuracy
       **/
      *alpha = beta;
      for (j=1 ; j<=knt ; j+=1) {
        *alpha = *alpha*safmin;
      }
    } else {
      *tau = ( beta-*alpha ) / beta;
      cblas_dscal( n-1, one / ( *alpha-beta ), x, incx );
      *alpha = beta;
    }
  }

  return;
  /**
   *     End of DLARFG
   **/
}



double   dlapy2( double x, double y )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     October 31, 1992
   *
   *     .. Scalar Arguments ..*/
  double dlapy2_R;
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
   *  overflow.
   *
   *  Arguments
   *  =========
   *
   *  X       (input) DOUBLE PRECISION
   *  Y       (input) DOUBLE PRECISION
   *          X and Y specify the values x and y.
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e0
#undef one
#define one 1.0e0
  /**     ..
   *     .. Local Scalars ..*/
  double    w, xabs, yabs, z;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, max, min, sqrt;*/
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  xabs = abs( x );
  yabs = abs( y );
  w = max( xabs, yabs );
  z = min( xabs, yabs );
  if( z==zero ) {
    dlapy2_R = w;
  } else {
    dlapy2_R = w*sqrt( one+( z / w )*( z / w ) );
  }
  return dlapy2_R;
  /**
   *     End of DLAPY2
   **/
}



void dlascl( char type, long kl, long ku, double cfrom, double cto,
            long m, long n, double a[], long lda, long *info )
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
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLASCL multiplies the M by N real matrix A by the real scalar
   *  CTO/CFROM.  This is done without over/underflow as long as the final
   *  result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
   *  A may be full, upper triangular, lower triangular, upper Hessenberg,
   *  or banded.
   *
   *  Arguments
   *  =========
   *
   *  TYPE    (input) CHARACTER*1
   *          TYPE indices the storage type of the input matrix.
   *          = 'G':  A is a full matrix.
   *          = 'L':  A is a lower triangular matrix.
   *          = 'U':  A is an upper triangular matrix.
   *          = 'H':  A is an upper Hessenberg matrix.
   *          = 'B':  A is a symmetric band matrix with lower bandwidth KL
   *                  and upper bandwidth KU and with the only the lower
   *                  half stored.
   *          = 'Q':  A is a symmetric band matrix with lower bandwidth KL
   *                  and upper bandwidth KU and with the only the upper
   *                  half stored.
   *          = 'Z':  A is a band matrix with lower bandwidth KL and upper
   *                  bandwidth KU.
   *
   *  KL      (input) INTEGER
   *          The lower bandwidth of A.  Referenced only if TYPE = 'B',
   *          'Q' or 'Z'.
   *
   *  KU      (input) INTEGER
   *          The upper bandwidth of A.  Referenced only if TYPE = 'B',
   *          'Q' or 'Z'.
   *
   *  CFROM   (input) DOUBLE PRECISION
   *  CTO     (input) DOUBLE PRECISION
   *          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
   *          without over/underflow if the final result CTO*A(I,J)/CFROM
   *          can be represented without over/underflow.  CFROM must be
   *          nonzero.
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix A.  M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix A.  N >= 0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,M)
   *          The matrix to be multiplied by CTO/CFROM.  See TYPE for the
   *          storage type.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  INFO    (output) INTEGER
   *          0  - successful exit
   *          <0 - if INFO = -i, the i-th argument had an illegal value.
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e0
#undef one
#define one 1.0e0
  /**     ..
   *     .. Local Scalars ..*/
  int            done;
  long            i, itype, j, k1, k2, k3, k4;
  double    bignum, cfrom1, cfromc, cto1, ctoc, mul, smlnum;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, max, min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input arguments
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;

  if( lsame( type, 'g' ) ) {
    itype = 0;
  } else if( lsame( type, 'l' ) ) {
    itype = 1;
  } else if( lsame( type, 'u' ) ) {
    itype = 2;
  } else if( lsame( type, 'h' ) ) {
    itype = 3;
  } else if( lsame( type, 'b' ) ) {
    itype = 4;
  } else if( lsame( type, 'q' ) ) {
    itype = 5;
  } else if( lsame( type, 'z' ) ) {
    itype = 6;
  } else {
    itype = -1;
  }

  if( itype==-1 ) {
    *info = -1;
  } else if( cfrom==zero ) {
    *info = -4;
  } else if( m<0 ) {
    *info = -6;
  } else if( n<0 || ( itype==4 && n!=m ) ||
            ( itype==5 && n!=m ) ) {
    *info = -7;
  } else if( itype<=3 && lda<max( 1, m ) ) {
    *info = -9;
  } else if( itype>=4 ) {
    if( kl<0 || kl>max( m-1, 0 ) ) {
      *info = -2;
    } else if( ku<0 || ku>max( n-1, 0 ) ||
              ( ( itype==4 || itype==5 ) && kl!=ku ) )
      {
        *info = -3;
      } else if( ( itype==4 && lda<kl+1 ) ||
                ( itype==5 && lda<ku+1 ) ||
                ( itype==6 && lda<2*kl+ku+1 ) ) {
        *info = -9;
      }
  }

  if( *info!=0 ) {
    xerbla( "dlascl", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( n==0 || m==0 )
    return;
  /**
   *     Get machine parameters
   **/
  smlnum = dlamch( 's' );
  bignum = one / smlnum;

  cfromc = cfrom;
  ctoc = cto;

 L_10:
  cfrom1 = cfromc*smlnum;
  cto1 = ctoc / bignum;
  if( abs( cfrom1 )>abs( ctoc ) && ctoc!=zero ) {
    mul = smlnum;
    done = 0;
    cfromc = cfrom1;
  } else if( abs( cto1 )>abs( cfromc ) ) {
    mul = bignum;
    done = 0;
    ctoc = cto1;
  } else {
    mul = ctoc / cfromc;
    done = 1;
  }

  if( itype==0 ) {
    /**
     *        Full matrix
     **/
    for (j=1 ; j<=n ; j+=1) {
      for (i=1 ; i<=m ; i+=1) {
        a_2( i, j ) = a_2( i, j )*mul;
      }
    }

  } else if( itype==1 ) {
    /**
     *        Lower triangular matrix
     **/
    for (j=1 ; j<=n ; j+=1) {
      for (i=j ; i<=m ; i+=1) {
        a_2( i, j ) = a_2( i, j )*mul;
      }
    }

  } else if( itype==2 ) {
    /**
     *        Upper triangular matrix
     **/
    for (j=1 ; j<=n ; j+=1) {
      for (i=1 ; i<=min( j, m ) ; i+=1) {
        a_2( i, j ) = a_2( i, j )*mul;
      }
    }

  } else if( itype==3 ) {
    /**
     *        Upper Hessenberg matrix
     **/
    for (j=1 ; j<=n ; j+=1) {
      for (i=1 ; i<=min( j+1, m ) ; i+=1) {
        a_2( i, j ) = a_2( i, j )*mul;
      }
    }

  } else if( itype==4 ) {
    /**
     *        Lower half of a symmetric band matrix
     **/
    k3 = kl + 1;
    k4 = n + 1;
    for (j=1 ; j<=n ; j+=1) {
      for (i=1 ; i<=min( k3, k4-j ) ; i+=1) {
        a_2( i, j ) = a_2( i, j )*mul;
      }
    }

  } else if( itype==5 ) {
    /**
     *        Upper half of a symmetric band matrix
     **/
    k1 = ku + 2;
    k3 = ku + 1;
    for (j=1 ; j<=n ; j+=1) {
      for (i=max( k1-j, 1 ) ; i<=k3 ; i+=1) {
        a_2( i, j ) = a_2( i, j )*mul;
      }
    }

  } else if( itype==6 ) {
    /**
     *        Band matrix
     **/
    k1 = kl + ku + 2;
    k2 = kl + 1;
    k3 = 2*kl + ku + 1;
    k4 = kl + ku + 1 + m;
    for (j=1 ; j<=n ; j+=1) {
      for (i=max( k1-j, k2 ) ; i<=min( k3, k4-j ) ; i+=1) {
        a_2( i, j ) = a_2( i, j )*mul;
      }
    }

  }

  if( !done )
    goto L_10;

  return;
  /**
   *     End of DLASCL
   **/
}
/**
************************************************************************
**/



double   dlange( char norm, long m, long n, double a[], long lda, double work[] )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     October 31, 1992
   *
   *     .. Scalar Arguments ..*/
  double dlange_R;
  /**     ..
   *     .. Array Arguments ..*/
#undef work_1
#define work_1(a1) work[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLANGE  returns the value of the one norm,  or the Frobenius norm, or
   *  the  infinity norm,  or the  element of  largest absolute value  of a
   *  real matrix A.
   *
   *  Description
   *  ===========
   *
   *  DLANGE returns the value
   *
   *     DLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
   *              (
   *              ( norm1(A),         NORM = '1', 'O' or 'o'
   *              (
   *              ( normI(A),         NORM = 'I' or 'i'
   *              (
   *              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
   *
   *  where  norm1  denotes the  one norm of a matrix (maximum column sum),
   *  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
   *  normF  denotes the  Frobenius norm of a matrix (square root of sum of
   *  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.
   *
   *  Arguments
   *  =========
   *
   *  NORM    (input) CHARACTER*1
   *          Specifies the value to be returned in DLANGE as described
   *          above.
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix A.  M >= 0.  When M = 0,
   *          DLANGE is set to zero.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix A.  N >= 0.  When N = 0,
   *          DLANGE is set to zero.
   *
   *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
   *          The m by n matrix A.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(M,1).
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK),
   *          where LWORK >= M when NORM = 'I'; otherwise, WORK is not
   *          referenced.
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
  long            i, j;
  double    scale, sum, value=0.0;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, max, min, sqrt;*/
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if( min( m, n )==0 ) {
    value = zero;
  } else if( lsame( norm, 'm' ) ) {
    /**
     *        Find max(abs(A(i,j))).
     **/
    value = zero;
    for (j=1 ; j<=n ; j+=1) {
      for (i=1 ; i<=m ; i+=1) {
        value = max( value, abs( a_2( i, j ) ) );
      }
    }
  } else if( ( lsame( norm, 'o' ) ) || ( norm=='1' ) ) {
    /**
     *        Find norm1(A).
     **/
    value = zero;
    for (j=1 ; j<=n ; j+=1) {
      sum = zero;
      for (i=1 ; i<=m ; i+=1) {
        sum = sum + abs( a_2( i, j ) );
      }
      value = max( value, sum );
    }
  } else if( lsame( norm, 'i' ) ) {
    /**
     *        Find normI(A).
     **/
    for (i=1 ; i<=m ; i+=1) {
      work_1( i ) = zero;
    }
    for (j=1 ; j<=n ; j+=1) {
      for (i=1 ; i<=m ; i+=1) {
        work_1( i ) = work_1( i ) + abs( a_2( i, j ) );
      }
    }
    value = zero;
    for (i=1 ; i<=m ; i+=1) {
      value = max( value, work_1( i ) );
    }
  } else if( ( lsame( norm, 'f' ) ) || ( lsame( norm, 'e' ) ) ) {
    /**
     *        Find normF(A).
     **/
    scale = zero;
    sum = one;
    for (j=1 ; j<=n ; j+=1) {
      dlassq( m, &a_2( 1, j ), 1, &scale, &sum );
    }
    value = scale*sqrt( sum );
  }

  dlange_R = value;
  return dlange_R;
  /**
   *     End of DLANGE
   **/
}



void dlassq( long n, double x[], long incx, double *scale, double *sumsq )
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
#undef x_1
#define x_1(a1) x[a1-1]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLASSQ  returns the values  scl  and  smsq  such that
   *
   *     ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
   *
   *  where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
   *  assumed to be non-negative and  scl  returns the value
   *
   *     scl = max( scale, abs( x( i ) ) ).
   *
   *  scale and sumsq must be supplied in SCALE and SUMSQ and
   *  scl and smsq are overwritten on SCALE and SUMSQ respectively.
   *
   *  The routine makes only one pass through the vector x.
   *
   *  Arguments
   *  =========
   *
   *  N       (input) INTEGER
   *          The number of elements to be used from the vector X.
   *
   *  X       (input) DOUBLE PRECISION
   *          The vector for which a scaled sum of squares is computed.
   *             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
   *
   *  INCX    (input) INTEGER
   *          The increment between successive values of the vector X.
   *          INCX > 0.
   *
   *  SCALE   (input/output) DOUBLE PRECISION
   *          On entry, the value  scale  in the equation above.
   *          On exit, SCALE is overwritten with  scl , the scaling factor
   *          for the sum of squares.
   *
   *  SUMSQ   (input/output) DOUBLE PRECISION
   *          On entry, the value  sumsq  in the equation above.
   *          On exit, SUMSQ is overwritten with  smsq , the basic sum of
   *          squares from which  scl  has been factored out.
   *
   * =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  long            ix;
  double    absxi;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs;*/
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if( n>0 ) {
    for (ix=1 ; incx>0?ix<=1 + ( n-1 )*incx:ix>=1 + ( n-1 )*incx ; ix+=incx) {
      if( x_1( ix )!=zero ) {
        absxi = abs( x_1( ix ) );
        if( *scale<absxi ) {
          *sumsq = 1 + *sumsq*( *scale / absxi )*( *scale / absxi );
          *scale = absxi;
        } else {
          *sumsq = *sumsq + ( absxi / *scale )*( absxi / *scale );
        }
      }
    }
  }
  return;
  /**
   *     End of DLASSQ
   **/
}



void dlaset( char uplo, long m, long n, double alpha, double beta,
            double a[], long lda )
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
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLASET initializes an m-by-n matrix A to BETA on the diagonal and
   *  ALPHA on the offdiagonals.
   *
   *  Arguments
   *  =========
   *
   *  UPLO    (input) CHARACTER*1
   *          Specifies the part of the matrix A to be set.
   *          = 'U':      Upper triangular part is set; the strictly lower
   *                      triangular part of A is not changed.
   *          = 'L':      Lower triangular part is set; the strictly upper
   *                      triangular part of A is not changed.
   *          Otherwise:  All of the matrix A is set.
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix A.  M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix A.  N >= 0.
   *
   *  ALPHA   (input) DOUBLE PRECISION
   *          The constant to which the offdiagonal elements are to be set.
   *
   *  BETA    (input) DOUBLE PRECISION
   *          The constant to which the diagonal elements are to be set.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On exit, the leading m-by-n submatrix of A is set as follows:
   *
   *          if UPLO = 'U', A(i,j) = ALPHA, 1<=i<=j-1, 1<=j<=n,
   *          if UPLO = 'L', A(i,j) = ALPHA, j+1<=i<=m, 1<=j<=n,
   *          otherwise,     A(i,j) = ALPHA, 1<=i<=m, 1<=j<=n, i.ne.j,
   *
   *          and, for all UPLO, A(i,i) = BETA, 1<=i<=min(m,n).
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   * =====================================================================
   *
   *     .. Local Scalars ..*/
  long            i, j;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          min;*/
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if( lsame( uplo, 'u' ) ) {
    /**
     *        Set the strictly upper triangular or trapezoidal part of the
     *        array to ALPHA.
     **/
    for (j=2 ; j<=n ; j+=1) {
      for (i=1 ; i<=min( j-1, m ) ; i+=1) {
        a_2( i, j ) = alpha;
      }
    }

  } else if( lsame( uplo, 'l' ) ) {
    /**
     *        Set the strictly lower triangular or trapezoidal part of the
     *        array to ALPHA.
     **/
    for (j=1 ; j<=min( m, n ) ; j+=1) {
      for (i=j + 1 ; i<=m ; i+=1) {
        a_2( i, j ) = alpha;
      }
    }

  } else {
    /**
     *        Set the leading m-by-n submatrix to ALPHA.
     **/
    for (j=1 ; j<=n ; j+=1) {
      for (i=1 ; i<=m ; i+=1) {
        a_2( i, j ) = alpha;
      }
    }
  }
  /**
   *     Set the first min(M,N) diagonal elements to BETA.
   **/
  for (i=1 ; i<=min( m, n ) ; i+=1) {
    a_2( i, i ) = beta;
  }

  return;
  /**
   *     End of DLASET
   **/
}
