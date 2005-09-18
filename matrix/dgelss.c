/*
 * $Id: dgelss.c,v 1.1 2005-09-18 22:04:43 dhmunro Exp $
 * LAPACK matrix solver using SVD.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "dg.h"


/*---blas routines---*/
/* dcopy dgemv dgemm */


/*-----Fortran intrinsics converted-----*/
#define min(x,y) ((x)<(y)?(x):(y))
#define max(x,y) ((x)>(y)?(x):(y))
/*-----end of Fortran intrinsics-----*/



void dgelss( long m, long n, long nrhs, double a[], long lda,
            double b[], long ldb, double s[], double rcond,
            long *rank,double work[], long lwork, long *info )
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
#undef s_1
#define s_1(a1) s[a1-1]
#undef b_2
#define b_2(a1,a2) b[a1-1+ldb*(a2-1)]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGELSS computes the minimum norm solution to a real linear least
   *  squares problem:
   *
   *  Minimize 2-norm(| b - A*x |).
   *
   *  using the singular value decomposition (SVD) of A. A is an M-by-N
   *  matrix which may be rank-deficient.
   *
   *  Several right hand side vectors b and solution vectors x can be
   *  handled in a single call; they are stored as the columns of the
   *  M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
   *  X.
   *
   *  The effective rank of A is determined by treating as zero those
   *  singular values which are less than RCOND times the largest singular
   *  value.
   *
   *  Arguments
   *  =========
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix A. M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix A. N >= 0.
   *
   *  NRHS    (input) INTEGER
   *          The number of right hand sides, i.e., the number of columns
   *          of the matrices B and X. NRHS >= 0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the M-by-N matrix A.
   *          On exit, the first min(m,n) rows of A are overwritten with
   *          its right singular vectors, stored rowwise.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
   *          On entry, the M-by-NRHS right hand side matrix B.
   *          On exit, B is overwritten by the N-by-NRHS solution
   *          matrix X.  If m >= n and RANK = n, the residual
   *          sum-of-squares for the solution in the i-th column is given
   *          by the sum of squares of elements n+1:m in that column.
   *
   *  LDB     (input) INTEGER
   *          The leading dimension of the array B. LDB >= max(1,MAX(M,N)).
   *
   *  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
   *          The singular values of A in decreasing order.
   *          The condition number of A in the 2-norm = S(1)/S(min(m,n)).
   *
   *  RCOND   (input) DOUBLE PRECISION
   *          RCOND is used to determine the effective rank of A.
   *          Singular values S(i) <= RCOND*S(1) are treated as zero.
   *          If RCOND $<$ 0, machine precision is used instead.
   *
   *  RANK    (output) INTEGER
   *          The effective rank of A, i.e., the number of singular values
   *          which are greater than RCOND*S(1).
   *
   *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
   *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
   *
   *  LWORK   (input) INTEGER
   *          The dimension of the array WORK. LWORK >= 1, and also:
   *          LWORK >= 3*N+MAX(2*N,NRHS,M) if M >= N,
   *          LWORK >= 3*M+MAX(2*M,NRHS,N) if M < N.
   *          For good performance, LWORK should generally be larger.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument had an illegal value.
   *          > 0:  the algorithm for computing the SVD failed to converge;
   *                if INFO = i, i off-diagonal elements of an intermediate
   *                bidiagonal form did not converge to zero.
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
  long    bl, chunk, i, iascl, ibscl, ie, il, itau,
          itaup, itauq, iwork, ldwork, maxmn, maxwrk=0,
          minmn, minwrk, mm, mnthr;
  double    anrm, bignum, bnrm, eps, sfmin, smlnum, thr;
  /**     ..
   *     .. Local Arrays ..*/
  double    vdum[1];
#undef vdum_1
#define vdum_1(a1) [a1-1]
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
  minmn = min( m, n );
  maxmn = max( m, n );
  mnthr = ilaenv( 6, "dgelss", " ", m, n, nrhs, -1 );
  if( m<0 ) {
    *info = -1;
  } else if( n<0 ) {
    *info = -2;
  } else if( nrhs<0 ) {
    *info = -3;
  } else if( lda<max( 1, m ) ) {
    *info = -5;
  } else if( ldb<max( 1, maxmn ) ) {
    *info = -7;
  }
  /**
   *     Compute workspace
   *      (Note: Comments in the code beginning "Workspace:" describe the
   *       minimal amount of workspace needed at that point in the code,
   *       as well as the preferred amount for good performance.
   *       NB refers to the optimal block size for the immediately
   *       following subroutine, as returned by ILAENV.)
   **/
  minwrk = 1;
  if( *info==0 && lwork>=1 ) {
    maxwrk = 0;
    mm = m;
    if( m>=n && m>=mnthr ) {
      /**
       *           Path 1a - overdetermined, with many more rows than columns
       **/
      mm = n;
      maxwrk = max( maxwrk, n+n*ilaenv( 1, "dgeqrf", " ", m, n,
                                       -1, -1 ) );
      maxwrk = max( maxwrk, n+nrhs*
                   ilaenv( 1, "dormqr", "lt", m, nrhs, n, -1 ) );
    }
    if( m>=n ) {
      /**
       *           Path 1 - overdetermined or exactly determined
       **/
      maxwrk = max( maxwrk, 3*n+( mm+n )*
                   ilaenv( 1, "dgebrd", " ", mm, n, -1, -1 ) );
      maxwrk = max( maxwrk, 3*n+nrhs*
                   ilaenv( 1, "dormbr", "qlt", mm, nrhs, n, -1 ) );
      maxwrk = max( maxwrk, 3*n+( n-1 )*
                   ilaenv( 1, "dorgbr", "p", n, n, n, -1 ) );
      maxwrk = max( maxwrk, 5*n-4 );
      maxwrk = max( maxwrk, n*nrhs );
      minwrk = max( max(3*n+mm, 3*n+nrhs), 5*n-4 );
    }
    if( n>m ) {
      minwrk = max( max(3*m+nrhs, 3*m+n), 5*m-4 );
      if( n>=mnthr ) {
        /**
         *              Path 2a - underdetermined, with many more columns
         *              than rows
         **/
        maxwrk = m + m*ilaenv( 1, "dgelqf", " ", m, n, -1, -1 );
        maxwrk = max( maxwrk, m*m+4*m+2*m*
                     ilaenv( 1, "dgebrd", " ", m, m, -1, -1 ) );
        maxwrk = max( maxwrk, m*m+4*m+nrhs*
                     ilaenv( 1, "dormbr", "qlt", m, nrhs, m, -1 ) );
        maxwrk = max( maxwrk, m*m+4*m+( m-1 )*
                     ilaenv( 1, "dorgbr", "p", m, m, m, -1 ) );
        maxwrk = max( maxwrk, m*m+6*m-4 );
        if( nrhs>1 ) {
          maxwrk = max( maxwrk, m*m+m+m*nrhs );
        } else {
          maxwrk = max( maxwrk, m*m+2*m );
        }
        maxwrk = max( maxwrk, m+nrhs*
                     ilaenv( 1, "dormlq", "lt", n, nrhs, m, -1 ) );
      } else {
        /**
         *              Path 2 - underdetermined
         **/
        maxwrk = 3*m + ( n+m )*ilaenv( 1, "dgebrd", " ", m, n,
                                      -1, -1 );
        maxwrk = max( maxwrk, 3*m+nrhs*
                     ilaenv( 1, "dormbr", "qlt", m, nrhs, m, -1 ) );
        maxwrk = max( maxwrk, 3*m+m*
                     ilaenv( 1, "dorgbr", "p", m, n, m, -1 ) );
        maxwrk = max( maxwrk, 5*m-4 );
        maxwrk = max( maxwrk, n*nrhs );
      }
    }
    minwrk = min( minwrk, maxwrk );
    work_1( 1 ) = maxwrk;
  }

  minwrk = max( minwrk, 1 );
  if( lwork<minwrk )
    *info = -12;
  if( *info!=0 ) {
    xerbla( "dgelss", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( m==0 || n==0 ) {
    *rank = 0;
    return;
  }
  /**
   *     Get machine parameters
   **/
  eps = dlamch( 'p' );
  sfmin = dlamch( 's' );
  smlnum = sfmin / eps;
  bignum = one / smlnum;
  dlabad( &smlnum, &bignum );
  /**
   *     Scale A if max entry outside range [SMLNUM,BIGNUM]
   **/
  anrm = dlange( 'm', m, n, a, lda, work );
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
    dlaset( 'f', minmn, 1, zero, zero, s, 1 );
    *rank = 0;
    goto L_70;
  }
  /**
   *     Scale B if max entry outside range [SMLNUM,BIGNUM]
   **/
  bnrm = dlange( 'm', m, nrhs, b, ldb, work );
  ibscl = 0;
  if( bnrm>zero && bnrm<smlnum ) {
    /**
     *        Scale matrix norm up to SMLNUM
     **/
    dlascl( 'g', 0, 0, bnrm, smlnum, m, nrhs, b, ldb, info );
    ibscl = 1;
  } else if( bnrm>bignum ) {
    /**
     *        Scale matrix norm down to BIGNUM
     **/
    dlascl( 'g', 0, 0, bnrm, bignum, m, nrhs, b, ldb, info );
    ibscl = 2;
  }
  /**
   *     Overdetermined case
   **/
  if( m>=n ) {
    /**
     *        Path 1 - overdetermined or exactly determined
     **/
    mm = m;
    if( m>=mnthr ) {
      /**
       *           Path 1a - overdetermined, with many more rows than columns
       **/
      mm = n;
      itau = 1;
      iwork = itau + n;
      /**
       *           Compute A=Q*R
       *           (Workspace: need 2*N, prefer N+N*NB)
       **/
      dgeqrf( m, n, a, lda, &work_1( itau ), &work_1( iwork ),
             lwork-iwork+1, info );
      /**
       *           Multiply B by transpose(Q)
       *           (Workspace: need N+NRHS, prefer N+NRHS*NB)
       **/
      dormqr( 'l', 't', m, nrhs, n, a, lda, &work_1( itau ), b,
             ldb, &work_1( iwork ), lwork-iwork+1, info );
      /**
       *           Zero out below R
       **/
      if( n>1 )
        dlaset( 'l', n-1, n-1, zero, zero, &a_2( 2, 1 ), lda );
    }

    ie = 1;
    itauq = ie + n;
    itaup = itauq + n;
    iwork = itaup + n;
    /**
     *        Bidiagonalize R in A
     *        (Workspace: need 3*N+MM, prefer 3*N+(MM+N)*NB)
     **/
    dgebrd( mm, n, a, lda, s, &work_1( ie ), &work_1( itauq ),
           &work_1( itaup ), &work_1( iwork ), lwork-iwork+1,
           info );
    /**
     *        Multiply B by transpose of left bidiagonalizing vectors of R
     *        (Workspace: need 3*N+NRHS, prefer 3*N+NRHS*NB)
     **/
    dormbr( 'q', 'l', 't', mm, nrhs, n, a, lda, &work_1( itauq ),
           b, ldb, &work_1( iwork ), lwork-iwork+1, info );
    /**
     *        Generate right bidiagonalizing vectors of R in A
     *        (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
     **/
    dorgbr( 'p', n, n, n, a, lda, &work_1( itaup ),
           &work_1( iwork ), lwork-iwork+1, info );
    iwork = ie + n;
    /**
     *        Perform bidiagonal QR iteration
     *          multiply B by transpose of left singular vectors
     *          compute right singular vectors in A
     *        (Workspace: need 5*N-4)
     **/
    dbdsqr( 'u', n, n, 0, nrhs, s, &work_1( ie ), a, lda, vdum,
           1, b, ldb, &work_1( iwork ), info );
    if( *info!=0 )
      goto L_70;
    /**
     *        Multiply B by reciprocals of singular values
     **/
    thr = max( rcond*s_1( 1 ), sfmin );
    if( thr<zero )
      thr = max( eps*s_1( 1 ), sfmin );
    *rank = 0;
    for (i=1 ; i<=n ; i+=1) {
      if( s_1( i )>thr ) {
        drscl( nrhs, s_1( i ), &b_2( i, 1 ), ldb );
        *rank = *rank + 1;
      } else {
        dlaset( 'f', 1, nrhs, zero, zero, &b_2( i, 1 ), ldb );
      }
    }
    /**
     *        Multiply B by right singular vectors
     *        (Workspace: need N, prefer N*NRHS)
     **/
    if( lwork>=ldb*nrhs && nrhs>1 ) {
      cblas_dgemm(CblasColMajor, CblasTrans, CblasNoTrans, n, nrhs, n,
                  one, a, lda, b, ldb, zero, work, ldb );
      dlacpy( 'g', n, nrhs, work, ldb, b, ldb );
    } else if( nrhs>1 ) {
      chunk = lwork / n;
      for (i=1 ; chunk>0?i<=nrhs:i>=nrhs ; i+=chunk) {
        bl = min( nrhs-i+1, chunk );
        cblas_dgemm(CblasColMajor, CblasTrans, CblasNoTrans, n, bl, n,
                    one, a, lda, b, ldb, zero, work, n );
        dlacpy( 'g', n, bl, work, n, b, ldb );
      }
    } else {
      cblas_dgemv(CblasColMajor, CblasTrans, n, n, one, a, lda, b, 1, zero,
                  work, 1 );
      cblas_dcopy( n, work, 1, b, 1 );
    }

  } else if( n>=mnthr && lwork>=4*m+m*m+
            max( max(m, 2*m-4), max(nrhs, n-3*m) ) ) {
    /**
     *        Path 2a - underdetermined, with many more columns than rows
     *        and sufficient workspace for an efficient algorithm
     **/
    ldwork = m;
    if( lwork>=max( 4*m+m*lda+max( max(m, 2*m-4), max(nrhs, n-3*m) ),
                   m*lda+m+m*nrhs ) )ldwork = lda;
    itau = 1;
    iwork = m + 1;
    /**
     *        Compute A=L*Q
     *        (Workspace: need 2*M, prefer M+M*NB)
     **/
    dgelqf( m, n, a, lda, &work_1( itau ), &work_1( iwork ),
           lwork-iwork+1, info );
    il = iwork;
    /**
     *        Copy L to WORK(IL), zeroing out above it
     **/
    dlacpy( 'l', m, m, a, lda, &work_1( il ), ldwork );
    dlaset( 'u', m-1, m-1, zero, zero, &work_1( il+ldwork ),
           ldwork );
    ie = il + ldwork*m;
    itauq = ie + m;
    itaup = itauq + m;
    iwork = itaup + m;
    /**
     *        Bidiagonalize L in WORK(IL)
     *        (Workspace: need M*M+5*M, prefer M*M+4*M+2*M*NB)
     **/
    dgebrd( m, m, &work_1( il ), ldwork, s, &work_1( ie ),
           &work_1( itauq ), &work_1( itaup ), &work_1( iwork ),
           lwork-iwork+1, info );
    /**
     *        Multiply B by transpose of left bidiagonalizing vectors of L
     *        (Workspace: need M*M+4*M+NRHS, prefer M*M+4*M+NRHS*NB)
     **/
    dormbr( 'q', 'l', 't', m, nrhs, m, &work_1( il ), ldwork,
           &work_1( itauq ), b, ldb, &work_1( iwork ),
           lwork-iwork+1, info );
    /**
     *        Generate right bidiagonalizing vectors of R in WORK(IL)
     *        (Workspace: need M*M+5*M-1, prefer M*M+4*M+(M-1)*NB)
     **/
    dorgbr( 'p', m, m, m, &work_1( il ), ldwork, &work_1( itaup ),
           &work_1( iwork ), lwork-iwork+1, info );
    iwork = ie + m;
    /**
     *        Perform bidiagonal QR iteration,
     *           computing right singular vectors of L in WORK(IL) and
     *           multiplying B by transpose of left singular vectors
     *        (Workspace: need M*M+6*M-4)
     **/
    dbdsqr( 'u', m, m, 0, nrhs, s, &work_1( ie ), &work_1( il ),
           ldwork, a, lda, b, ldb, &work_1( iwork ), info );
    if( *info!=0 )
      goto L_70;
    /**
     *        Multiply B by reciprocals of singular values
     **/
    thr = max( rcond*s_1( 1 ), sfmin );
    if( thr<zero )
      thr = max( eps*s_1( 1 ), sfmin );
    *rank = 0;
    for (i=1 ; i<=m ; i+=1) {
      if( s_1( i )>thr ) {
        drscl( nrhs, s_1( i ), &b_2( i, 1 ), ldb );
        *rank = *rank + 1;
      } else {
        dlaset( 'f', 1, nrhs, zero, zero, &b_2( i, 1 ), ldb );
      }
    }
    iwork = ie;
    /**
     *        Multiply B by right singular vectors of L in WORK(IL)
     *        (Workspace: need M*M+2*M, prefer M*M+M+M*NRHS)
     **/
    if( lwork>=ldb*nrhs+iwork-1 && nrhs>1 ) {
      cblas_dgemm(CblasColMajor, CblasTrans, CblasNoTrans, m, nrhs, m,
                  one, &work_1( il ), ldwork, b, ldb, zero,
                  &work_1( iwork ), ldb );
      dlacpy( 'g', m, nrhs, &work_1( iwork ), ldb, b, ldb );
    } else if( nrhs>1 ) {
      chunk = ( lwork-iwork+1 ) / m;
      for (i=1 ; chunk>0?i<=nrhs:i>=nrhs ; i+=chunk) {
        bl = min( nrhs-i+1, chunk );
        cblas_dgemm(CblasColMajor, CblasTrans, CblasNoTrans, m, bl, m,
                    one, &work_1( il ), ldwork, &b_2( 1, i ), ldb, zero,
                    &work_1( iwork ), n );
        dlacpy( 'g', m, bl, &work_1( iwork ), n, b, ldb );
      }
    } else {
      cblas_dgemv(CblasColMajor, CblasTrans, m, m, one, &work_1( il ),
                  ldwork, &b_2( 1, 1 ), 1, zero, &work_1( iwork ), 1 );
      cblas_dcopy( m, &work_1( iwork ), 1, &b_2( 1, 1 ), 1 );
    }
    /**
     *        Zero out below first M rows of B
     **/
    dlaset( 'f', n-m, nrhs, zero, zero, &b_2( m+1, 1 ), ldb );
    iwork = itau + m;
    /**
     *        Multiply transpose(Q) by B
     *        (Workspace: need M+NRHS, prefer M+NRHS*NB)
     **/
    dormlq( 'l', 't', n, nrhs, m, a, lda, &work_1( itau ), b,
           ldb, &work_1( iwork ), lwork-iwork+1, info );

  } else {
    /**
     *        Path 2 - remaining underdetermined cases
     **/
    ie = 1;
    itauq = ie + m;
    itaup = itauq + m;
    iwork = itaup + m;
    /**
     *        Bidiagonalize A
     *        (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
     **/
    dgebrd( m, n, a, lda, s, &work_1( ie ), &work_1( itauq ),
           &work_1( itaup ), &work_1( iwork ), lwork-iwork+1,
           info );
    /**
     *        Multiply B by transpose of left bidiagonalizing vectors
     *        (Workspace: need 3*M+NRHS, prefer 3*M+NRHS*NB)
     **/
    dormbr( 'q', 'l', 't', m, nrhs, n, a, lda, &work_1( itauq ),
           b, ldb, &work_1( iwork ), lwork-iwork+1, info );
    /**
     *        Generate right bidiagonalizing vectors in A
     *        (Workspace: need 4*M, prefer 3*M+M*NB)
     **/
    dorgbr( 'p', m, n, m, a, lda, &work_1( itaup ),
           &work_1( iwork ), lwork-iwork+1, info );
    iwork = ie + m;
    /**
     *        Perform bidiagonal QR iteration,
     *           computing right singular vectors of A in A and
     *           multiplying B by transpose of left singular vectors
     *        (Workspace: need 5*M-4)
     **/
    dbdsqr( 'l', m, n, 0, nrhs, s, &work_1( ie ), a, lda, vdum,
           1, b, ldb, &work_1( iwork ), info );
    if( *info!=0 )
      goto L_70;
    /**
     *        Multiply B by reciprocals of singular values
     **/
    thr = max( rcond*s_1( 1 ), sfmin );
    if( thr<zero )
      thr = max( eps*s_1( 1 ), sfmin );
    *rank = 0;
    for (i=1 ; i<=m ; i+=1) {
      if( s_1( i )>thr ) {
        drscl( nrhs, s_1( i ), &b_2( i, 1 ), ldb );
        *rank = *rank + 1;
      } else {
        dlaset( 'f', 1, nrhs, zero, zero, &b_2( i, 1 ), ldb );
      }
    }
    /**
     *        Multiply B by right singular vectors of A
     *        (Workspace: need N, prefer N*NRHS)
     **/
    if( lwork>=ldb*nrhs && nrhs>1 ) {
      cblas_dgemm(CblasColMajor, CblasTrans, CblasNoTrans, n, nrhs, m,
                  one, a, lda, b, ldb, zero, work, ldb );
      dlacpy( 'f', n, nrhs, work, ldb, b, ldb );
    } else if( nrhs>1 ) {
      chunk = lwork / n;
      for (i=1 ; chunk>0?i<=nrhs:i>=nrhs ; i+=chunk) {
        bl = min( nrhs-i+1, chunk );
        cblas_dgemm(CblasColMajor, CblasTrans, CblasNoTrans, n, bl, m, one,
                    a, lda, &b_2( 1, i ), ldb, zero, work, n );
        dlacpy( 'f', n, bl, work, n, &b_2( 1, i ), ldb );
      }
    } else {
      cblas_dgemv(CblasColMajor, CblasTrans, m, n, one, a, lda, b, 1, zero,
                  work, 1 );
      cblas_dcopy( n, work, 1, b, 1 );
    }
  }
  /**
   *     Undo scaling
   **/
  if( iascl==1 ) {
    dlascl( 'g', 0, 0, anrm, smlnum, n, nrhs, b, ldb, info );
    dlascl( 'g', 0, 0, smlnum, anrm, minmn, 1, s, minmn,
           info );
  } else if( iascl==2 ) {
    dlascl( 'g', 0, 0, anrm, bignum, n, nrhs, b, ldb, info );
    dlascl( 'g', 0, 0, bignum, anrm, minmn, 1, s, minmn,
           info );
  }
  if( ibscl==1 ) {
    dlascl( 'g', 0, 0, smlnum, bnrm, n, nrhs, b, ldb, info );
  } else if( ibscl==2 ) {
    dlascl( 'g', 0, 0, bignum, bnrm, n, nrhs, b, ldb, info );
  }

 L_70:
  work_1( 1 ) = maxwrk;
  return;
  /**
   *     End of DGELSS
   **/
}
