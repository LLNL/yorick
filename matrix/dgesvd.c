/*
 * $Id: dgesvd.c,v 1.1 2005-09-18 22:04:48 dhmunro Exp $
 * LAPACK routine to return the SVD of a matrix (m>=n branch only).
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "dg.h"

/* Compilers have a hard time with this ridiculously large driver routine.
   Therefore, I split it into a separate m>=n and m<n branch.  The m<n
   branch is in the file dgesv2.c.  Here is its prototype: */
extern void dgesv2( char jobu, char jobvt, long m, long n,
                   double a[], long lda, double s[], double u[], long ldu,
                   double vt[], long ldvt,
                   double work[], long lwork, long *info );



/*---blas routines---*/
/* dgemm */



/*---converted nutty string switches to single characters (lower case)---*/
#define lsame(x,y) ((x)==(y))


/*-----Fortran intrinsics converted-----*/
extern double sqrt(double);
#define min(x,y) ((x)<(y)?(x):(y))
#define max(x,y) ((x)>(y)?(x):(y))
/*-----end of Fortran intrinsics-----*/



void dgesvd( char jobu, char jobvt, long m, long n,
            double a[], long lda, double s[], double u[], long ldu,
            double vt[], long ldvt,
            double work[], long lwork, long *info )
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
#undef vt_2
#define vt_2(a1,a2) vt[a1-1+ldvt*(a2-1)]
#undef u_2
#define u_2(a1,a2) u[a1-1+ldu*(a2-1)]
#undef s_1
#define s_1(a1) s[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGESVD computes the singular value decomposition (SVD) of a real
   *  M-by-N matrix A, optionally computing the left and/or right singular
   *  vectors. The SVD is written
   *
   *       A = U * SIGMA * transpose(V)
   *
   *  where SIGMA is an M-by-N matrix which is zero except for its
   *  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
   *  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
   *  are the singular values of A; they are real and non-negative, and
   *  are returned in descending order.  The first min(m,n) columns of
   *  U and V are the left and right singular vectors of A.
   *
   *  Note that the routine returns V**T, not V.
   *
   *  Arguments
   *  =========
   *
   *  JOBU    (input) CHARACTER*1
   *          Specifies options for computing all or part of the matrix U:
   *          = 'A':  all M columns of U are returned in array U:
   *          = 'S':  the first min(m,n) columns of U (the left singular
   *                  vectors) are returned in the array U;
   *          = 'O':  the first min(m,n) columns of U (the left singular
   *                  vectors) are overwritten on the array A;
   *          = 'N':  no columns of U (no left singular vectors) are
   *                  computed.
   *
   *  JOBVT   (input) CHARACTER*1
   *          Specifies options for computing all or part of the matrix
   *          V**T:
   *          = 'A':  all N rows of V**T are returned in the array VT;
   *          = 'S':  the first min(m,n) rows of V**T (the right singular
   *                  vectors) are returned in the array VT;
   *          = 'O':  the first min(m,n) rows of V**T (the right singular
   *                  vectors) are overwritten on the array A;
   *          = 'N':  no rows of V**T (no right singular vectors) are
   *                  computed.
   *
   *          JOBVT and JOBU cannot both be 'O'.
   *
   *  M       (input) INTEGER
   *          The number of rows of the input matrix A.  M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the input matrix A.  N >= 0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the M-by-N matrix A.
   *          On exit,
   *          if JOBU = 'O',  A is overwritten with the first min(m,n)
   *                          columns of U (the left singular vectors,
   *                          stored columnwise);
   *          if JOBVT = 'O', A is overwritten with the first min(m,n)
   *                          rows of V**T (the right singular vectors,
   *                          stored rowwise);
   *          if JOBU .ne. 'O' and JOBVT .ne. 'O', the contents of A
   *                          are destroyed.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
   *          The singular values of A, sorted so that S(i) >= S(i+1).
   *
   *  U       (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
   *          (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
   *          If JOBU = 'A', U contains the M-by-M orthogonal matrix U;
   *          if JOBU = 'S', U contains the first min(m,n) columns of U
   *          (the left singular vectors, stored columnwise);
   *          if JOBU = 'N' or 'O', U is not referenced.
   *
   *  LDU     (input) INTEGER
   *          The leading dimension of the array U.  LDU >= 1; if
   *          JOBU = 'S' or 'A', LDU >= M.
   *
   *  VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
   *          If JOBVT = 'A', VT contains the N-by-N orthogonal matrix
   *          V**T;
   *          if JOBVT = 'S', VT contains the first min(m,n) rows of
   *          V**T (the right singular vectors, stored rowwise);
   *          if JOBVT = 'N' or 'O', VT is not referenced.
   *
   *  LDVT    (input) INTEGER
   *          The leading dimension of the array VT.  LDVT >= 1; if
   *          JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
   *
   *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
   *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
   *          if INFO > 0, WORK(2:MIN(M,N)) contains the unconverged
   *          superdiagonal elements of an upper bidiagonal matrix B
   *          whose diagonal is in S (not necessarily sorted). B
   *          satisfies A = U * B * VT, so it has the same singular values
   *          as A, and singular vectors related by U and VT.
   *
   *  LWORK   (input) INTEGER
   *          The dimension of the array WORK. LWORK >= 1.
   *          LWORK >= MAX(3*MIN(M,N)+MAX(M,N),5*MIN(M,N)-4).
   *          For good performance, LWORK should generally be larger.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit.
   *          < 0:  if INFO = -i, the i-th argument had an illegal value.
   *          > 0:  if DBDSQR did not converge, INFO specifies how many
   *                superdiagonals of an intermediate bidiagonal form B
   *                did not converge to zero. See the description of WORK
   *                above for details.
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
  int            wntua, wntuas, wntun, wntuo, wntus, wntva,
  wntvas, wntvn, wntvo, wntvs;
  long   chunk, i, ie=0, ierr, ir, iscl, itau, itaup,
         itauq, iu, iwork, ldwrkr, ldwrku, maxwrk=0,
         minmn, minwrk, mnthr, ncu=0, ncvt=0, nru=0,
         wrkbl=0;
  double    anrm, bignum, eps, smlnum;
  char job_u_vt[3];
  /**     ..
   *     .. Local Arrays ..*/
  double    dum[1];
#undef dum_1
#define dum_1(a1) [a1-1]
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max, min, sqrt;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input arguments
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if (m<n) {
    dgesv2( jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt,
           work, lwork, info );
    return;
  }

  *info = 0;
  minmn = min( m, n );
  job_u_vt[0]= jobu;
  job_u_vt[1]= jobvt;
  job_u_vt[2]= '\0';
  mnthr = ilaenv( 6, "dgesvd", job_u_vt, m, n, 0, 0 );
  wntua = lsame( jobu, 'a' );
  wntus = lsame( jobu, 's' );
  wntuas = wntua || wntus;
  wntuo = lsame( jobu, 'o' );
  wntun = lsame( jobu, 'n' );
  wntva = lsame( jobvt, 'a' );
  wntvs = lsame( jobvt, 's' );
  wntvas = wntva || wntvs;
  wntvo = lsame( jobvt, 'o' );
  wntvn = lsame( jobvt, 'n' );
  minwrk = 1;

  if( !( wntua || wntus || wntuo || wntun ) ) {
    *info = -1;
  } else if( !( wntva || wntvs || wntvo || wntvn ) ||
            ( wntvo && wntuo ) ) {
    *info = -2;
  } else if( m<0 ) {
    *info = -3;
  } else if( n<0 ) {
    *info = -4;
  } else if( lda<max( 1, m ) ) {
    *info = -6;
  } else if( ldu<1 || ( wntuas && ldu<m ) ) {
    *info = -9;
  } else if( ldvt<1 || ( wntva && ldvt<n ) ||
            ( wntvs && ldvt<minmn ) ) {
    *info = -11;
  }
  /**
   *     Compute workspace
   *      (Note: Comments in the code beginning "Workspace:" describe the
   *       minimal amount of workspace needed at that point in the code,
   *       as well as the preferred amount for good performance.
   *       NB refers to the optimal block size for the immediately
   *       following subroutine, as returned by ILAENV.)
   **/
  if( *info==0 && lwork>=1 && m>0 && n>0 ) {
    if( m>=mnthr ) {
      if( wntun ) {
        /**
         *                 Path 1 (M much larger than N, JOBU='N')
         **/
        maxwrk = n + n*ilaenv( 1, "dgeqrf", " ", m, n, -1,
                              -1 );
        maxwrk = max( maxwrk, 3*n+2*n*
                     ilaenv( 1, "dgebrd", " ", n, n, -1, -1 ) );
        if( wntvo || wntvas )
          maxwrk = max( maxwrk, 3*n+( n-1 )*
                       ilaenv( 1, "dorgbr", "p", n, n, n, -1 ) );
        maxwrk = max( maxwrk, 5*n-4 );
        minwrk = max( 4*n, 5*n-4 );
      } else if( wntuo && wntvn ) {
        /**
         *                 Path 2 (M much larger than N, JOBU='O', JOBVT='N')
         **/
        wrkbl = n + n*ilaenv( 1, "dgeqrf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, n+n*ilaenv( 1, "dorgqr", " ", m,
                                       n, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+2*n*
                    ilaenv( 1, "dgebrd", " ", n, n, -1, -1 ) );
        wrkbl = max( wrkbl, 3*n+n*
                    ilaenv( 1, "dorgbr", "q", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 5*n-4 );
        maxwrk = max( n*n+wrkbl, n*n+m*n+n );
        minwrk = max( 3*n+m, 5*n-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntuo && wntvas ) {
        /**
         *                 Path 3 (M much larger than N, JOBU='O', JOBVT='S' or
         *                 'A')
         **/
        wrkbl = n + n*ilaenv( 1, "dgeqrf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, n+n*ilaenv( 1, "dorgqr", " ", m,
                                       n, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+2*n*
                    ilaenv( 1, "dgebrd", " ", n, n, -1, -1 ) );
        wrkbl = max( wrkbl, 3*n+n*
                    ilaenv( 1, "dorgbr", "q", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+( n-1 )*
                    ilaenv( 1, "dorgbr", "p", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 5*n-4 );
        maxwrk = max( n*n+wrkbl, n*n+m*n+n );
        minwrk = max( 3*n+m, 5*n-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntus && wntvn ) {
        /**
         *                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
         **/
        wrkbl = n + n*ilaenv( 1, "dgeqrf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, n+n*ilaenv( 1, "dorgqr", " ", m,
                                       n, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+2*n*
                    ilaenv( 1, "dgebrd", " ", n, n, -1, -1 ) );
        wrkbl = max( wrkbl, 3*n+n*
                    ilaenv( 1, "dorgbr", "q", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 5*n-4 );
        maxwrk = n*n + wrkbl;
        minwrk = max( 3*n+m, 5*n-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntus && wntvo ) {
        /**
         *                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
         **/
        wrkbl = n + n*ilaenv( 1, "dgeqrf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, n+n*ilaenv( 1, "dorgqr", " ", m,
                                       n, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+2*n*
                    ilaenv( 1, "dgebrd", " ", n, n, -1, -1 ) );
        wrkbl = max( wrkbl, 3*n+n*
                    ilaenv( 1, "dorgbr", "q", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+( n-1 )*
                    ilaenv( 1, "dorgbr", "p", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 5*n-4 );
        maxwrk = 2*n*n + wrkbl;
        minwrk = max( 3*n+m, 5*n-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntus && wntvas ) {
        /**
         *                 Path 6 (M much larger than N, JOBU='S', JOBVT='S' or
         *                 'A')
         **/
        wrkbl = n + n*ilaenv( 1, "dgeqrf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, n+n*ilaenv( 1, "dorgqr", " ", m,
                                       n, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+2*n*
                    ilaenv( 1, "dgebrd", " ", n, n, -1, -1 ) );
        wrkbl = max( wrkbl, 3*n+n*
                    ilaenv( 1, "dorgbr", "q", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+( n-1 )*
                    ilaenv( 1, "dorgbr", "p", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 5*n-4 );
        maxwrk = n*n + wrkbl;
        minwrk = max( 3*n+m, 5*n-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntua && wntvn ) {
        /**
         *                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
         **/
        wrkbl = n + n*ilaenv( 1, "dgeqrf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, n+m*ilaenv( 1, "dorgqr", " ", m,
                                       m, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+2*n*
                    ilaenv( 1, "dgebrd", " ", n, n, -1, -1 ) );
        wrkbl = max( wrkbl, 3*n+n*
                    ilaenv( 1, "dorgbr", "q", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 5*n-4 );
        maxwrk = n*n + wrkbl;
        minwrk = max( 3*n+m, 5*n-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntua && wntvo ) {
        /**
         *                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
         **/
        wrkbl = n + n*ilaenv( 1, "dgeqrf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, n+m*ilaenv( 1, "dorgqr", " ", m,
                                       m, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+2*n*
                    ilaenv( 1, "dgebrd", " ", n, n, -1, -1 ) );
        wrkbl = max( wrkbl, 3*n+n*
                    ilaenv( 1, "dorgbr", "q", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+( n-1 )*
                    ilaenv( 1, "dorgbr", "p", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 5*n-4 );
        maxwrk = 2*n*n + wrkbl;
        minwrk = max( 3*n+m, 5*n-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntua && wntvas ) {
        /**
         *                 Path 9 (M much larger than N, JOBU='A', JOBVT='S' or
         *                 'A')
         **/
        wrkbl = n + n*ilaenv( 1, "dgeqrf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, n+m*ilaenv( 1, "dorgqr", " ", m,
                                       m, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+2*n*
                    ilaenv( 1, "dgebrd", " ", n, n, -1, -1 ) );
        wrkbl = max( wrkbl, 3*n+n*
                    ilaenv( 1, "dorgbr", "q", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 3*n+( n-1 )*
                    ilaenv( 1, "dorgbr", "p", n, n, n, -1 ) );
        wrkbl = max( wrkbl, 5*n-4 );
        maxwrk = n*n + wrkbl;
        minwrk = max( 3*n+m, 5*n-4 );
        minwrk = min( minwrk, maxwrk );
      }
    } else {
      /**
       *              Path 10 (M at least N, but not much larger)
       **/
      maxwrk = 3*n + ( m+n )*ilaenv( 1, "dgebrd", " ", m, n,
                                    -1, -1 );
      if( wntus || wntuo )
        maxwrk = max( maxwrk, 3*n+n*
                     ilaenv( 1, "dorgbr", "q", m, n, n, -1 ) );
      if( wntua )
        maxwrk = max( maxwrk, 3*n+m*
                     ilaenv( 1, "dorgbr", "q", m, m, n, -1 ) );
      if( !wntvn )
        maxwrk = max( maxwrk, 3*n+( n-1 )*
                     ilaenv( 1, "dorgbr", "p", n, n, n, -1 ) );
      maxwrk = max( maxwrk, 5*n-4 );
      minwrk = max( 3*n+m, 5*n-4 );
    }
    work_1( 1 ) = maxwrk;
  }

  if( lwork<minwrk ) {
    *info = -13;
  }
  if( *info!=0 ) {
    xerbla( "dgesvd", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( m==0 || n==0 ) {
    if( lwork>=1 )
      work_1( 1 ) = one;
    return;
  }
  /**
   *     Get machine constants
   **/
  eps = dlamch( 'p' );
  smlnum = sqrt( dlamch( 's' ) ) / eps;
  bignum = one / smlnum;
  /**
   *     Scale A if max entry outside range [SMLNUM,BIGNUM]
   **/
  anrm = dlange( 'm', m, n, a, lda, dum );
  iscl = 0;
  if( anrm>zero && anrm<smlnum ) {
    iscl = 1;
    dlascl( 'g', 0, 0, anrm, smlnum, m, n, a, lda, &ierr );
  } else if( anrm>bignum ) {
    iscl = 1;
    dlascl( 'g', 0, 0, anrm, bignum, m, n, a, lda, &ierr );
  }

  /**
   *        A has at least as many rows as columns. If A has sufficiently
   *        more rows than columns, first reduce using the QR
   *        decomposition (if sufficient workspace available)
   **/
  if( m>=mnthr ) {

    if( wntun ) {
        /**
         *              Path 1 (M much larger than N, JOBU='N')
         *              No left singular vectors to be computed
         **/
        itau = 1;
        iwork = itau + n;
        /**
         *              Compute A=Q*R
         *              (Workspace: need 2*N, prefer N+N*NB)
         **/
        dgeqrf( m, n, a, lda, &work_1( itau ), &work_1( iwork ),
               lwork-iwork+1, &ierr );
        /**
         *              Zero out below R
         **/
        dlaset( 'l', n-1, n-1, zero, zero, &a_2( 2, 1 ), lda );
        ie = 1;
        itauq = ie + n;
        itaup = itauq + n;
        iwork = itaup + n;
        /**
         *              Bidiagonalize R in A
         *              (Workspace: need 4*N, prefer 3*N+2*N*NB)
         **/
        dgebrd( n, n, a, lda, s, &work_1( ie ), &work_1( itauq ),
               &work_1( itaup ), &work_1( iwork ), lwork-iwork+1,
               &ierr );
        ncvt = 0;
        if( wntvo || wntvas ) {
          /**
           *                 If right singular vectors desired, generate P'.
           *                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
           **/
          dorgbr( 'p', n, n, n, a, lda, &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          ncvt = n;
        }
        iwork = ie + n;
        /**
         *              Perform bidiagonal QR iteration, computing right
         *              singular vectors of A in A if desired
         *              (Workspace: need 5*N-4)
         **/
        dbdsqr( 'u', n, ncvt, 0, 0, s, &work_1( ie ), a, lda,
               dum, 1, dum, 1, &work_1( iwork ), info );
        /**
         *              If right singular vectors desired in VT, copy them there
         **/
        if( wntvas )
          dlacpy( 'f', n, n, a, lda, vt, ldvt );

    } else if( wntuo && wntvn ) {
        /**
         *              Path 2 (M much larger than N, JOBU='O', JOBVT='N')
         *              N left singular vectors to be overwritten on A and
         *              no right singular vectors to be computed
         **/
        if( lwork>=n*n+max( 4*n, 5*n-4 ) ) {
          /**
           *                 Sufficient workspace for a fast algorithm
           **/
          ir = 1;
          if( lwork>=max( wrkbl, lda*n+n )+lda*n ) {
            /**
             *                    WORK(IU) is LDA by N, WORK(IR) is LDA by N
             **/
            ldwrku = lda;
            ldwrkr = lda;
          } else if( lwork>=max( wrkbl, lda*n+n )+n*n ) {
            /**
             *                    WORK(IU) is LDA by N, WORK(IR) is N by N
             **/
            ldwrku = lda;
            ldwrkr = n;
          } else {
            /**
             *                    WORK(IU) is LDWRKU by N, WORK(IR) is N by N
             **/
            ldwrku = ( lwork-n*n-n ) / n;
            ldwrkr = n;
          }
          itau = ir + ldwrkr*n;
          iwork = itau + n;
          /**
           *                 Compute A=Q*R
           *                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
           **/
          dgeqrf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                 Copy R to WORK(IR) and zero out below it
           **/
          dlacpy( 'u', n, n, a, lda, &work_1( ir ), ldwrkr );
          dlaset( 'l', n-1, n-1, zero, zero, &work_1( ir+1 ),
                 ldwrkr );
          /**
           *                 Generate Q in A
           *                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
           **/
          dorgqr( m, n, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          ie = itau;
          itauq = ie + n;
          itaup = itauq + n;
          iwork = itaup + n;
          /**
           *                 Bidiagonalize R in WORK(IR)
           *                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
           **/
          dgebrd( n, n, &work_1( ir ), ldwrkr, s, &work_1( ie ),
                 &work_1( itauq ), &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                 Generate left vectors bidiagonalizing R
           *                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
           **/
          dorgbr( 'q', n, n, n, &work_1( ir ), ldwrkr,
                 &work_1( itauq ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          iwork = ie + n;
          /**
           *                 Perform bidiagonal QR iteration, computing left
           *                 singular vectors of R in WORK(IR)
           *                 (Workspace: need N*N+5*N-4)
           **/
          dbdsqr( 'u', n, 0, n, 0, s, &work_1( ie ), dum, 1,
                 &work_1( ir ), ldwrkr, dum, 1,
                 &work_1( iwork ), info );
          iu = ie + n;
          /**
           *                 Multiply Q in A by left singular vectors of R in
           *                 WORK(IR), storing result in WORK(IU) and copying to A
           *                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
           **/
          for (i=1 ; ldwrku>0?i<=m:i>=m ; i+=ldwrku) {
            chunk = min( m-i+1, ldwrku );
            cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, chunk,
                        n, n, one, &a_2( i, 1 ), lda, &work_1( ir ), ldwrkr,
                        zero, &work_1( iu ), ldwrku );
            dlacpy( 'f', chunk, n, &work_1( iu ), ldwrku,
                   &a_2( i, 1 ), lda );
          }

        } else {
          /**
           *                 Insufficient workspace for a fast algorithm
           **/
          ie = 1;
          itauq = ie + n;
          itaup = itauq + n;
          iwork = itaup + n;
          /**
           *                 Bidiagonalize A
           *                 (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
           **/
          dgebrd( m, n, a, lda, s, &work_1( ie ),
                 &work_1( itauq ), &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                 Generate left vectors bidiagonalizing A
           *                 (Workspace: need 4*N, prefer 3*N+N*NB)
           **/
          dorgbr( 'q', m, n, n, a, lda, &work_1( itauq ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          iwork = ie + n;
          /**
           *                 Perform bidiagonal QR iteration, computing left
           *                 singular vectors of A in A
           *                 (Workspace: need 5*N-4)
           **/
          dbdsqr( 'u', n, 0, m, 0, s, &work_1( ie ), dum, 1,
                 a, lda, dum, 1, &work_1( iwork ), info );

        }

    } else if( wntuo && wntvas ) {
        /**
         *              Path 3 (M much larger than N, JOBU='O', JOBVT='S' or 'A')
         *              N left singular vectors to be overwritten on A and
         *              N right singular vectors to be computed in VT
         **/
        if( lwork>=n*n+max( 4*n, 5*n-4 ) ) {
          /**
           *                 Sufficient workspace for a fast algorithm
           **/
          ir = 1;
          if( lwork>=max( wrkbl, lda*n+n )+lda*n ) {
            /**
             *                    WORK(IU) is LDA by N and WORK(IR) is LDA by N
             **/
            ldwrku = lda;
            ldwrkr = lda;
          } else if( lwork>=max( wrkbl, lda*n+n )+n*n ) {
            /**
             *                    WORK(IU) is LDA by N and WORK(IR) is N by N
             **/
            ldwrku = lda;
            ldwrkr = n;
          } else {
            /**
             *                    WORK(IU) is LDWRKU by N and WORK(IR) is N by N
             **/
            ldwrku = ( lwork-n*n-n ) / n;
            ldwrkr = n;
          }
          itau = ir + ldwrkr*n;
          iwork = itau + n;
          /**
           *                 Compute A=Q*R
           *                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
           **/
          dgeqrf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                 Copy R to VT, zeroing out below it
           **/
          dlacpy( 'u', n, n, a, lda, vt, ldvt );
          dlaset( 'l', n-1, n-1, zero, zero, &vt_2( 2, 1 ),
                 ldvt );
          /**
           *                 Generate Q in A
           *                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
           **/
          dorgqr( m, n, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          ie = itau;
          itauq = ie + n;
          itaup = itauq + n;
          iwork = itaup + n;
          /**
           *                 Bidiagonalize R in VT, copying result to WORK(IR)
           *                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
           **/
          dgebrd( n, n, vt, ldvt, s, &work_1( ie ),
                 &work_1( itauq ), &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          dlacpy( 'l', n, n, vt, ldvt, &work_1( ir ), ldwrkr );
          /**
           *                 Generate left vectors bidiagonalizing R in WORK(IR)
           *                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
           **/
          dorgbr( 'q', n, n, n, &work_1( ir ), ldwrkr,
                 &work_1( itauq ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          /**
           *                 Generate right vectors bidiagonalizing R in VT
           *                 (Workspace: need N*N+4*N-1, prefer N*N+3*N+(N-1)*NB)
           **/
          dorgbr( 'p', n, n, n, vt, ldvt, &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          iwork = ie + n;
          /**
           *                 Perform bidiagonal QR iteration, computing left
           *                 singular vectors of R in WORK(IR) and computing right
           *                 singular vectors of R in VT
           *                 (Workspace: need N*N+5*N-4)
           **/
          dbdsqr( 'u', n, n, n, 0, s, &work_1( ie ), vt, ldvt,
                 &work_1( ir ), ldwrkr, dum, 1,
                 &work_1( iwork ), info );
          iu = ie + n;
          /**
           *                 Multiply Q in A by left singular vectors of R in
           *                 WORK(IR), storing result in WORK(IU) and copying to A
           *                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
           **/
          for (i=1 ; ldwrku>0?i<=m:i>=m ; i+=ldwrku) {
            chunk = min( m-i+1, ldwrku );
            cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, chunk,
                        n, n, one, &a_2( i, 1 ), lda, &work_1( ir ), ldwrkr,
                        zero, &work_1( iu ), ldwrku );
            dlacpy( 'f', chunk, n, &work_1( iu ), ldwrku,
                   &a_2( i, 1 ), lda );
          }

        } else {
          /**
           *                 Insufficient workspace for a fast algorithm
           **/
          itau = 1;
          iwork = itau + n;
          /**
           *                 Compute A=Q*R
           *                 (Workspace: need 2*N, prefer N+N*NB)
           **/
          dgeqrf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                 Copy R to VT, zeroing out below it
           **/
          dlacpy( 'u', n, n, a, lda, vt, ldvt );
          dlaset( 'l', n-1, n-1, zero, zero, &vt_2( 2, 1 ),
                 ldvt );
          /**
           *                 Generate Q in A
           *                 (Workspace: need 2*N, prefer N+N*NB)
           **/
          dorgqr( m, n, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          ie = itau;
          itauq = ie + n;
          itaup = itauq + n;
          iwork = itaup + n;
          /**
           *                 Bidiagonalize R in VT
           *                 (Workspace: need 4*N, prefer 3*N+2*N*NB)
           **/
          dgebrd( n, n, vt, ldvt, s, &work_1( ie ),
                 &work_1( itauq ), &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                 Multiply Q in A by left vectors bidiagonalizing R
           *                 (Workspace: need 3*N+M, prefer 3*N+M*NB)
           **/
          dormbr( 'q', 'r', 'n', m, n, n, vt, ldvt,
                 &work_1( itauq ), a, lda, &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          /**
           *                 Generate right vectors bidiagonalizing R in VT
           *                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
           **/
          dorgbr( 'p', n, n, n, vt, ldvt, &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          iwork = ie + n;
          /**
           *                 Perform bidiagonal QR iteration, computing left
           *                 singular vectors of A in A and computing right
           *                 singular vectors of A in VT
           *                 (Workspace: need 5*N-4)
           **/
          dbdsqr( 'u', n, n, m, 0, s, &work_1( ie ), vt, ldvt,
                 a, lda, dum, 1, &work_1( iwork ), info );

        }

    } else if( wntus ) {

        if( wntvn ) {
          /**
           *                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
           *                 N left singular vectors to be computed in U and
           *                 no right singular vectors to be computed
           **/
          if( lwork>=n*n+max( 4*n, 5*n-4 ) ) {
            /**
             *                    Sufficient workspace for a fast algorithm
             **/
            ir = 1;
            if( lwork>=wrkbl+lda*n ) {
              /**
               *                       WORK(IR) is LDA by N
               **/
              ldwrkr = lda;
            } else {
              /**
               *                       WORK(IR) is N by N
               **/
              ldwrkr = n;
            }
            itau = ir + ldwrkr*n;
            iwork = itau + n;
            /**
             *                    Compute A=Q*R
             *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
             **/
            dgeqrf( m, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Copy R to WORK(IR), zeroing out below it
             **/
            dlacpy( 'u', n, n, a, lda, &work_1( ir ),
                   ldwrkr );
            dlaset( 'l', n-1, n-1, zero, zero,
                   &work_1( ir+1 ), ldwrkr );
            /**
             *                    Generate Q in A
             *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
             **/
            dorgqr( m, n, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            ie = itau;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            /**
             *                    Bidiagonalize R in WORK(IR)
             *                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
             **/
            dgebrd( n, n, &work_1( ir ), ldwrkr, s,
                   &work_1( ie ), &work_1( itauq ),
                   &work_1( itaup ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            /**
             *                    Generate left vectors bidiagonalizing R in WORK(IR)
             *                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
             **/
            dorgbr( 'q', n, n, n, &work_1( ir ), ldwrkr,
                   &work_1( itauq ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            iwork = ie + n;
            /**
             *                    Perform bidiagonal QR iteration, computing left
             *                    singular vectors of R in WORK(IR)
             *                    (Workspace: need N*N+5*N-4)
             **/
            dbdsqr( 'u', n, 0, n, 0, s, &work_1( ie ), dum,
                   1, &work_1( ir ), ldwrkr, dum, 1,
                   &work_1( iwork ), info );
            /**
             *                    Multiply Q in A by left singular vectors of R in
             *                    WORK(IR), storing result in U
             *                    (Workspace: need N*N)
             **/
            cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n, n,
                        one, a, lda, &work_1( ir ), ldwrkr, zero, u, ldu );

          } else {
            /**
             *                    Insufficient workspace for a fast algorithm
             **/
            itau = 1;
            iwork = itau + n;
            /**
             *                    Compute A=Q*R, copying result to U
             *                    (Workspace: need 2*N, prefer N+N*NB)
             **/
            dgeqrf( m, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            dlacpy( 'l', m, n, a, lda, u, ldu );
            /**
             *                    Generate Q in U
             *                    (Workspace: need 2*N, prefer N+N*NB)
             **/
            dorgqr( m, n, n, u, ldu, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            ie = itau;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            /**
             *                    Zero out below R in A
             **/
            dlaset( 'l', n-1, n-1, zero, zero, &a_2( 2, 1 ),
                   lda );
            /**
             *                    Bidiagonalize R in A
             *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
             **/
            dgebrd( n, n, a, lda, s, &work_1( ie ),
                   &work_1( itauq ), &work_1( itaup ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Multiply Q in U by left vectors bidiagonalizing R
             *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
             **/
            dormbr( 'q', 'r', 'n', m, n, n, a, lda,
                   &work_1( itauq ), u, ldu, &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            iwork = ie + n;
            /**
             *                    Perform bidiagonal QR iteration, computing left
             *                    singular vectors of A in U
             *                    (Workspace: need 5*N-4)
             **/
            dbdsqr( 'u', n, 0, m, 0, s, &work_1( ie ), dum,
                   1, u, ldu, dum, 1, &work_1( iwork ),
                   info );

          }

        } else if( wntvo ) {
          /**
           *                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
           *                 N left singular vectors to be computed in U and
           *                 N right singular vectors to be overwritten on A
           **/
          if( lwork>=2*n*n+max( 4*n, 5*n-4 ) ) {
            /**
             *                    Sufficient workspace for a fast algorithm
             **/
            iu = 1;
            if( lwork>=wrkbl+2*lda*n ) {
              /**
               *                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
               **/
              ldwrku = lda;
              ir = iu + ldwrku*n;
              ldwrkr = lda;
            } else if( lwork>=wrkbl+( lda+n )*n ) {
              /**
               *                       WORK(IU) is LDA by N and WORK(IR) is N by N
               **/
              ldwrku = lda;
              ir = iu + ldwrku*n;
              ldwrkr = n;
            } else {
              /**
               *                       WORK(IU) is N by N and WORK(IR) is N by N
               **/
              ldwrku = n;
              ir = iu + ldwrku*n;
              ldwrkr = n;
            }
            itau = ir + ldwrkr*n;
            iwork = itau + n;
            /**
             *                    Compute A=Q*R
             *                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
             **/
            dgeqrf( m, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Copy R to WORK(IU), zeroing out below it
             **/
            dlacpy( 'u', n, n, a, lda, &work_1( iu ),
                   ldwrku );
            dlaset( 'l', n-1, n-1, zero, zero,
                   &work_1( iu+1 ), ldwrku );
            /**
             *                    Generate Q in A
             *                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
             **/
            dorgqr( m, n, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            ie = itau;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            /**
             *                    Bidiagonalize R in WORK(IU), copying result to
             *                    WORK(IR)
             *                    (Workspace: need 2*N*N+4*N,
             *                                prefer 2*N*N+3*N+2*N*NB)
             **/
            dgebrd( n, n, &work_1( iu ), ldwrku, s,
                   &work_1( ie ), &work_1( itauq ),
                   &work_1( itaup ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            dlacpy( 'u', n, n, &work_1( iu ), ldwrku,
                   &work_1( ir ), ldwrkr );
            /**
             *                    Generate left bidiagonalizing vectors in WORK(IU)
             *                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
             **/
            dorgbr( 'q', n, n, n, &work_1( iu ), ldwrku,
                   &work_1( itauq ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            /**
             *                    Generate right bidiagonalizing vectors in WORK(IR)
             *                    (Workspace: need 2*N*N+4*N-1,
             *                                prefer 2*N*N+3*N+(N-1)*NB)
             **/
            dorgbr( 'p', n, n, n, &work_1( ir ), ldwrkr,
                   &work_1( itaup ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            iwork = ie + n;
            /**
             *                    Perform bidiagonal QR iteration, computing left
             *                    singular vectors of R in WORK(IU) and computing
             *                    right singular vectors of R in WORK(IR)
             *                    (Workspace: need 2*N*N+5*N-4)
             **/
            dbdsqr( 'u', n, n, n, 0, s, &work_1( ie ),
                   &work_1( ir ), ldwrkr, &work_1( iu ),
                   ldwrku, dum, 1, &work_1( iwork ), info );
            /**
             *                    Multiply Q in A by left singular vectors of R in
             *                    WORK(IU), storing result in U
             *                    (Workspace: need N*N)
             **/
            cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n, n,
                        one, a, lda, &work_1( iu ), ldwrku, zero, u, ldu );
            /**
             *                    Copy right singular vectors of R to A
             *                    (Workspace: need N*N)
             **/
            dlacpy( 'f', n, n, &work_1( ir ), ldwrkr, a,
                   lda );

          } else {
            /**
             *                    Insufficient workspace for a fast algorithm
             **/
            itau = 1;
            iwork = itau + n;
            /**
             *                    Compute A=Q*R, copying result to U
             *                    (Workspace: need 2*N, prefer N+N*NB)
             **/
            dgeqrf( m, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            dlacpy( 'l', m, n, a, lda, u, ldu );
            /**
             *                    Generate Q in U
             *                    (Workspace: need 2*N, prefer N+N*NB)
             **/
            dorgqr( m, n, n, u, ldu, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            ie = itau;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            /**
             *                    Zero out below R in A
             **/
            dlaset( 'l', n-1, n-1, zero, zero, &a_2( 2, 1 ),
                   lda );
            /**
             *                    Bidiagonalize R in A
             *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
             **/
            dgebrd( n, n, a, lda, s, &work_1( ie ),
                   &work_1( itauq ), &work_1( itaup ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Multiply Q in U by left vectors bidiagonalizing R
             *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
             **/
            dormbr( 'q', 'r', 'n', m, n, n, a, lda,
                   &work_1( itauq ), u, ldu, &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            /**
             *                    Generate right vectors bidiagonalizing R in A
             *                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
             **/
            dorgbr( 'p', n, n, n, a, lda, &work_1( itaup ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            iwork = ie + n;
            /**
             *                    Perform bidiagonal QR iteration, computing left
             *                    singular vectors of A in U and computing right
             *                    singular vectors of A in A
             *                    (Workspace: need 5*N-4)
             **/
            dbdsqr( 'u', n, n, m, 0, s, &work_1( ie ), a,
                   lda, u, ldu, dum, 1, &work_1( iwork ),
                   info );

          }

        } else if( wntvas ) {
          /**
           *                 Path 6 (M much larger than N, JOBU='S', JOBVT='S'
           *                         or 'A')
           *                 N left singular vectors to be computed in U and
           *                 N right singular vectors to be computed in VT
           **/
          if( lwork>=n*n+max( 4*n, 5*n-4 ) ) {
            /**
             *                    Sufficient workspace for a fast algorithm
             **/
            iu = 1;
            if( lwork>=wrkbl+lda*n ) {
              /**
               *                       WORK(IU) is LDA by N
               **/
              ldwrku = lda;
            } else {
              /**
               *                       WORK(IU) is N by N
               **/
              ldwrku = n;
            }
            itau = iu + ldwrku*n;
            iwork = itau + n;
            /**
             *                    Compute A=Q*R
             *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
             **/
            dgeqrf( m, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Copy R to WORK(IU), zeroing out below it
             **/
            dlacpy( 'u', n, n, a, lda, &work_1( iu ),
                   ldwrku );
            dlaset( 'l', n-1, n-1, zero, zero,
                   &work_1( iu+1 ), ldwrku );
            /**
             *                    Generate Q in A
             *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
             **/
            dorgqr( m, n, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            ie = itau;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            /**
             *                    Bidiagonalize R in WORK(IU), copying result to VT
             *                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
             **/
            dgebrd( n, n, &work_1( iu ), ldwrku, s,
                   &work_1( ie ), &work_1( itauq ),
                   &work_1( itaup ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            dlacpy( 'u', n, n, &work_1( iu ), ldwrku, vt,
                   ldvt );
            /**
             *                    Generate left bidiagonalizing vectors in WORK(IU)
             *                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
             **/
            dorgbr( 'q', n, n, n, &work_1( iu ), ldwrku,
                   &work_1( itauq ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            /**
             *                    Generate right bidiagonalizing vectors in VT
             *                    (Workspace: need N*N+4*N-1,
             *                                prefer N*N+3*N+(N-1)*NB)
             **/
            dorgbr( 'p', n, n, n, vt, ldvt, &work_1( itaup ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            iwork = ie + n;
            /**
             *                    Perform bidiagonal QR iteration, computing left
             *                    singular vectors of R in WORK(IU) and computing
             *                    right singular vectors of R in VT
             *                    (Workspace: need N*N+5*N-4)
             **/
            dbdsqr( 'u', n, n, n, 0, s, &work_1( ie ), vt,
                   ldvt, &work_1( iu ), ldwrku, dum, 1,
                   &work_1( iwork ), info );
            /**
             *                    Multiply Q in A by left singular vectors of R in
             *                    WORK(IU), storing result in U
             *                    (Workspace: need N*N)
             **/
            cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n, n,
                        one, a, lda, &work_1( iu ), ldwrku, zero, u, ldu );

          } else {
            /**
             *                    Insufficient workspace for a fast algorithm
             **/
            itau = 1;
            iwork = itau + n;
            /**
             *                    Compute A=Q*R, copying result to U
             *                    (Workspace: need 2*N, prefer N+N*NB)
             **/
            dgeqrf( m, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            dlacpy( 'l', m, n, a, lda, u, ldu );
            /**
             *                    Generate Q in U
             *                    (Workspace: need 2*N, prefer N+N*NB)
             **/
            dorgqr( m, n, n, u, ldu, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Copy R to VT, zeroing out below it
             **/
            dlacpy( 'u', n, n, a, lda, vt, ldvt );
            dlaset( 'l', n-1, n-1, zero, zero, &vt_2( 2, 1 ),
                   ldvt );
            ie = itau;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            /**
             *                    Bidiagonalize R in VT
             *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
             **/
            dgebrd( n, n, vt, ldvt, s, &work_1( ie ),
                   &work_1( itauq ), &work_1( itaup ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Multiply Q in U by left bidiagonalizing vectors
             *                    in VT
             *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
             **/
            dormbr( 'q', 'r', 'n', m, n, n, vt, ldvt,
                   &work_1( itauq ), u, ldu, &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            /**
             *                    Generate right bidiagonalizing vectors in VT
             *                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
             **/
            dorgbr( 'p', n, n, n, vt, ldvt, &work_1( itaup ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            iwork = ie + n;
            /**
             *                    Perform bidiagonal QR iteration, computing left
             *                    singular vectors of A in U and computing right
             *                    singular vectors of A in VT
             *                    (Workspace: need 5*N-4)
             **/
            dbdsqr( 'u', n, n, m, 0, s, &work_1( ie ), vt,
                   ldvt, u, ldu, dum, 1, &work_1( iwork ),
                   info );

          }

        }

    } else if( wntua ) {

        if( wntvn ) {
          /**
           *                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
           *                 M left singular vectors to be computed in U and
           *                 no right singular vectors to be computed
           **/
          if( lwork>=n*n+max( n+m, max(4*n, 5*n-4) ) ) {
            /**
             *                    Sufficient workspace for a fast algorithm
             **/
            ir = 1;
            if( lwork>=wrkbl+lda*n ) {
              /**
               *                       WORK(IR) is LDA by N
               **/
              ldwrkr = lda;
            } else {
              /**
               *                       WORK(IR) is N by N
               **/
              ldwrkr = n;
            }
            itau = ir + ldwrkr*n;
            iwork = itau + n;
            /**
             *                    Compute A=Q*R, copying result to U
             *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
             **/
            dgeqrf( m, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            dlacpy( 'l', m, n, a, lda, u, ldu );
            /**
             *                    Copy R to WORK(IR), zeroing out below it
             **/
            dlacpy( 'u', n, n, a, lda, &work_1( ir ),
                   ldwrkr );
            dlaset( 'l', n-1, n-1, zero, zero,
                   &work_1( ir+1 ), ldwrkr );
            /**
             *                    Generate Q in U
             *                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
             **/
            dorgqr( m, m, n, u, ldu, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            ie = itau;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            /**
             *                    Bidiagonalize R in WORK(IR)
             *                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
             **/
            dgebrd( n, n, &work_1( ir ), ldwrkr, s,
                   &work_1( ie ), &work_1( itauq ),
                   &work_1( itaup ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            /**
             *                    Generate left bidiagonalizing vectors in WORK(IR)
             *                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
             **/
            dorgbr( 'q', n, n, n, &work_1( ir ), ldwrkr,
                   &work_1( itauq ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            iwork = ie + n;
            /**
             *                    Perform bidiagonal QR iteration, computing left
             *                    singular vectors of R in WORK(IR)
             *                    (Workspace: need N*N+5*N-4)
             **/
            dbdsqr( 'u', n, 0, n, 0, s, &work_1( ie ), dum,
                   1, &work_1( ir ), ldwrkr, dum, 1,
                   &work_1( iwork ), info );
            /**
             *                    Multiply Q in U by left singular vectors of R in
             *                    WORK(IR), storing result in A
             *                    (Workspace: need N*N)
             **/
            cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n, n,
                        one, u, ldu, &work_1( ir ), ldwrkr, zero, a, lda );
            /**
             *                    Copy left singular vectors of A from A to U
             **/
            dlacpy( 'f', m, n, a, lda, u, ldu );

          } else {
            /**
             *                    Insufficient workspace for a fast algorithm
             **/
            itau = 1;
            iwork = itau + n;
            /**
             *                    Compute A=Q*R, copying result to U
             *                    (Workspace: need 2*N, prefer N+N*NB)
             **/
            dgeqrf( m, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            dlacpy( 'l', m, n, a, lda, u, ldu );
            /**
             *                    Generate Q in U
             *                    (Workspace: need N+M, prefer N+M*NB)
             **/
            dorgqr( m, m, n, u, ldu, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            ie = itau;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            /**
             *                    Zero out below R in A
             **/
            dlaset( 'l', n-1, n-1, zero, zero, &a_2( 2, 1 ),
                   lda );
            /**
             *                    Bidiagonalize R in A
             *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
             **/
            dgebrd( n, n, a, lda, s, &work_1( ie ),
                   &work_1( itauq ), &work_1( itaup ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Multiply Q in U by left bidiagonalizing vectors
             *                    in A
             *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
             **/
            dormbr( 'q', 'r', 'n', m, n, n, a, lda,
                   &work_1( itauq ), u, ldu, &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            iwork = ie + n;
            /**
             *                    Perform bidiagonal QR iteration, computing left
             *                    singular vectors of A in U
             *                    (Workspace: need 5*N-4)
             **/
            dbdsqr( 'u', n, 0, m, 0, s, &work_1( ie ), dum,
                   1, u, ldu, dum, 1, &work_1( iwork ),
                   info );

          }

        } else if( wntvo ) {
          /**
           *                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
           *                 M left singular vectors to be computed in U and
           *                 N right singular vectors to be overwritten on A
           **/
          if( lwork>=2*n*n+max( n+m, max(4*n, 5*n-4) ) ) {
            /**
             *                    Sufficient workspace for a fast algorithm
             **/
            iu = 1;
            if( lwork>=wrkbl+2*lda*n ) {
              /**
               *                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
               **/
              ldwrku = lda;
              ir = iu + ldwrku*n;
              ldwrkr = lda;
            } else if( lwork>=wrkbl+( lda+n )*n ) {
              /**
               *                       WORK(IU) is LDA by N and WORK(IR) is N by N
               **/
              ldwrku = lda;
              ir = iu + ldwrku*n;
              ldwrkr = n;
            } else {
              /**
               *                       WORK(IU) is N by N and WORK(IR) is N by N
               **/
              ldwrku = n;
              ir = iu + ldwrku*n;
              ldwrkr = n;
            }
            itau = ir + ldwrkr*n;
            iwork = itau + n;
            /**
             *                    Compute A=Q*R, copying result to U
             *                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
             **/
            dgeqrf( m, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            dlacpy( 'l', m, n, a, lda, u, ldu );
            /**
             *                    Generate Q in U
             *                    (Workspace: need 2*N*N+N+M, prefer 2*N*N+N+M*NB)
             **/
            dorgqr( m, m, n, u, ldu, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Copy R to WORK(IU), zeroing out below it
             **/
            dlacpy( 'u', n, n, a, lda, &work_1( iu ),
                   ldwrku );
            dlaset( 'l', n-1, n-1, zero, zero,
                   &work_1( iu+1 ), ldwrku );
            ie = itau;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            /**
             *                    Bidiagonalize R in WORK(IU), copying result to
             *                    WORK(IR)
             *                    (Workspace: need 2*N*N+4*N,
             *                                prefer 2*N*N+3*N+2*N*NB)
             **/
            dgebrd( n, n, &work_1( iu ), ldwrku, s,
                   &work_1( ie ), &work_1( itauq ),
                   &work_1( itaup ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            dlacpy( 'u', n, n, &work_1( iu ), ldwrku,
                   &work_1( ir ), ldwrkr );
            /**
             *                    Generate left bidiagonalizing vectors in WORK(IU)
             *                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
             **/
            dorgbr( 'q', n, n, n, &work_1( iu ), ldwrku,
                   &work_1( itauq ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            /**
             *                    Generate right bidiagonalizing vectors in WORK(IR)
             *                    (Workspace: need 2*N*N+4*N-1,
             *                                prefer 2*N*N+3*N+(N-1)*NB)
             **/
            dorgbr( 'p', n, n, n, &work_1( ir ), ldwrkr,
                   &work_1( itaup ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            iwork = ie + n;
            /**
             *                    Perform bidiagonal QR iteration, computing left
             *                    singular vectors of R in WORK(IU) and computing
             *                    right singular vectors of R in WORK(IR)
             *                    (Workspace: need 2*N*N+5*N-4)
             **/
            dbdsqr( 'u', n, n, n, 0, s, &work_1( ie ),
                   &work_1( ir ), ldwrkr, &work_1( iu ),
                   ldwrku, dum, 1, &work_1( iwork ), info );
            /**
             *                    Multiply Q in U by left singular vectors of R in
             *                    WORK(IU), storing result in A
             *                    (Workspace: need N*N)
             **/
            cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n, n,
                        one, u, ldu, &work_1( iu ), ldwrku, zero, a, lda );
            /**
             *                    Copy left singular vectors of A from A to U
             **/
            dlacpy( 'f', m, n, a, lda, u, ldu );
            /**
             *                    Copy right singular vectors of R from WORK(IR) to A
             **/
            dlacpy( 'f', n, n, &work_1( ir ), ldwrkr, a,
                   lda );

          } else {
            /**
             *                    Insufficient workspace for a fast algorithm
             **/
            itau = 1;
            iwork = itau + n;
            /**
             *                    Compute A=Q*R, copying result to U
             *                    (Workspace: need 2*N, prefer N+N*NB)
             **/
            dgeqrf( m, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            dlacpy( 'l', m, n, a, lda, u, ldu );
            /**
             *                    Generate Q in U
             *                    (Workspace: need N+M, prefer N+M*NB)
             **/
            dorgqr( m, m, n, u, ldu, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            ie = itau;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            /**
             *                    Zero out below R in A
             **/
            dlaset( 'l', n-1, n-1, zero, zero, &a_2( 2, 1 ),
                   lda );
            /**
             *                    Bidiagonalize R in A
             *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
             **/
            dgebrd( n, n, a, lda, s, &work_1( ie ),
                   &work_1( itauq ), &work_1( itaup ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Multiply Q in U by left bidiagonalizing vectors
             *                    in A
             *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
             **/
            dormbr( 'q', 'r', 'n', m, n, n, a, lda,
                   &work_1( itauq ), u, ldu, &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            /**
             *                    Generate right bidiagonalizing vectors in A
             *                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
             **/
            dorgbr( 'p', n, n, n, a, lda, &work_1( itaup ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            iwork = ie + n;
            /**
             *                    Perform bidiagonal QR iteration, computing left
             *                    singular vectors of A in U and computing right
             *                    singular vectors of A in A
             *                    (Workspace: need 5*N-4)
             **/
            dbdsqr( 'u', n, n, m, 0, s, &work_1( ie ), a,
                   lda, u, ldu, dum, 1, &work_1( iwork ),
                   info );

          }

        } else if( wntvas ) {
          /**
           *                 Path 9 (M much larger than N, JOBU='A', JOBVT='S'
           *                         or 'A')
           *                 M left singular vectors to be computed in U and
           *                 N right singular vectors to be computed in VT
           **/
          if( lwork>=n*n+max( n+m, max(4*n, 5*n-4) ) ) {
            /**
             *                    Sufficient workspace for a fast algorithm
             **/
            iu = 1;
            if( lwork>=wrkbl+lda*n ) {
              /**
               *                       WORK(IU) is LDA by N
               **/
              ldwrku = lda;
            } else {
              /**
               *                       WORK(IU) is N by N
               **/
              ldwrku = n;
            }
            itau = iu + ldwrku*n;
            iwork = itau + n;
            /**
             *                    Compute A=Q*R, copying result to U
             *                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
             **/
            dgeqrf( m, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            dlacpy( 'l', m, n, a, lda, u, ldu );
            /**
             *                    Generate Q in U
             *                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
             **/
            dorgqr( m, m, n, u, ldu, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Copy R to WORK(IU), zeroing out below it
             **/
            dlacpy( 'u', n, n, a, lda, &work_1( iu ),
                   ldwrku );
            dlaset( 'l', n-1, n-1, zero, zero,
                   &work_1( iu+1 ), ldwrku );
            ie = itau;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            /**
             *                    Bidiagonalize R in WORK(IU), copying result to VT
             *                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
             **/
            dgebrd( n, n, &work_1( iu ), ldwrku, s,
                   &work_1( ie ), &work_1( itauq ),
                   &work_1( itaup ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            dlacpy( 'u', n, n, &work_1( iu ), ldwrku, vt,
                   ldvt );
            /**
             *                    Generate left bidiagonalizing vectors in WORK(IU)
             *                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
             **/
            dorgbr( 'q', n, n, n, &work_1( iu ), ldwrku,
                   &work_1( itauq ), &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            /**
             *                    Generate right bidiagonalizing vectors in VT
             *                    (Workspace: need N*N+4*N-1,
             *                                prefer N*N+3*N+(N-1)*NB)
             **/
            dorgbr( 'p', n, n, n, vt, ldvt, &work_1( itaup ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            iwork = ie + n;
            /**
             *                    Perform bidiagonal QR iteration, computing left
             *                    singular vectors of R in WORK(IU) and computing
             *                    right singular vectors of R in VT
             *                    (Workspace: need N*N+5*N-4)
             **/
            dbdsqr( 'u', n, n, n, 0, s, &work_1( ie ), vt,
                   ldvt, &work_1( iu ), ldwrku, dum, 1,
                   &work_1( iwork ), info );
            /**
             *                    Multiply Q in U by left singular vectors of R in
             *                    WORK(IU), storing result in A
             *                    (Workspace: need N*N)
             **/
            cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n, n,
                        one, u, ldu, &work_1( iu ), ldwrku, zero, a, lda );
            /**
             *                    Copy left singular vectors of A from A to U
             **/
            dlacpy( 'f', m, n, a, lda, u, ldu );

          } else {
            /**
             *                    Insufficient workspace for a fast algorithm
             **/
            itau = 1;
            iwork = itau + n;
            /**
             *                    Compute A=Q*R, copying result to U
             *                    (Workspace: need 2*N, prefer N+N*NB)
             **/
            dgeqrf( m, n, a, lda, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            dlacpy( 'l', m, n, a, lda, u, ldu );
            /**
             *                    Generate Q in U
             *                    (Workspace: need N+M, prefer N+M*NB)
             **/
            dorgqr( m, m, n, u, ldu, &work_1( itau ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Copy R from A to VT, zeroing out below it
             **/
            dlacpy( 'u', n, n, a, lda, vt, ldvt );
            dlaset( 'l', n-1, n-1, zero, zero, &vt_2( 2, 1 ),
                   ldvt );
            ie = itau;
            itauq = ie + n;
            itaup = itauq + n;
            iwork = itaup + n;
            /**
             *                    Bidiagonalize R in VT
             *                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
             **/
            dgebrd( n, n, vt, ldvt, s, &work_1( ie ),
                   &work_1( itauq ), &work_1( itaup ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            /**
             *                    Multiply Q in U by left bidiagonalizing vectors
             *                    in VT
             *                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
             **/
            dormbr( 'q', 'r', 'n', m, n, n, vt, ldvt,
                   &work_1( itauq ), u, ldu, &work_1( iwork ),
                   lwork-iwork+1, &ierr );
            /**
             *                    Generate right bidiagonalizing vectors in VT
             *                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
             **/
            dorgbr( 'p', n, n, n, vt, ldvt, &work_1( itaup ),
                   &work_1( iwork ), lwork-iwork+1, &ierr );
            iwork = ie + n;
            /**
             *                    Perform bidiagonal QR iteration, computing left
             *                    singular vectors of A in U and computing right
             *                    singular vectors of A in VT
             *                    (Workspace: need 5*N-4)
             **/
            dbdsqr( 'u', n, n, m, 0, s, &work_1( ie ), vt,
                   ldvt, u, ldu, dum, 1, &work_1( iwork ),
                   info );

          }

        }

    }

  } else {
    /**
     *           M .LT. MNTHR
     *
     *           Path 10 (M at least N, but not much larger)
     *           Reduce to bidiagonal form without QR decomposition
     **/
    ie = 1;
    itauq = ie + n;
    itaup = itauq + n;
    iwork = itaup + n;
    /**
     *           Bidiagonalize A
     *           (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
     **/
    dgebrd( m, n, a, lda, s, &work_1( ie ), &work_1( itauq ),
             &work_1( itaup ), &work_1( iwork ), lwork-iwork+1,
             &ierr );
    if( wntuas ) {
        /**
         *              If left singular vectors desired in U, copy result to U
         *              and generate left bidiagonalizing vectors in U
         *              (Workspace: need 3*N+NCU, prefer 3*N+NCU*NB)
         **/
        dlacpy( 'l', m, n, a, lda, u, ldu );
        if( wntus )
          ncu = n;
        if( wntua )
          ncu = m;
        dorgbr( 'q', m, ncu, n, u, ldu, &work_1( itauq ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
    }
    if( wntvas ) {
        /**
         *              If right singular vectors desired in VT, copy result to
         *              VT and generate right bidiagonalizing vectors in VT
         *              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
         **/
        dlacpy( 'u', n, n, a, lda, vt, ldvt );
        dorgbr( 'p', n, n, n, vt, ldvt, &work_1( itaup ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
    }
    if( wntuo ) {
        /**
         *              If left singular vectors desired in A, generate left
         *              bidiagonalizing vectors in A
         *              (Workspace: need 4*N, prefer 3*N+N*NB)
         **/
        dorgbr( 'q', m, n, n, a, lda, &work_1( itauq ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
    }
    if( wntvo ) {
        /**
         *              If right singular vectors desired in A, generate right
         *              bidiagonalizing vectors in A
         *              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
         **/
        dorgbr( 'p', n, n, n, a, lda, &work_1( itaup ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
    }
    iwork = ie + n;
    if( wntuas || wntuo )
        nru = m;
    if( wntun )
        nru = 0;
    if( wntvas || wntvo )
        ncvt = n;
    if( wntvn )
        ncvt = 0;
    if( ( !wntuo ) && ( !wntvo ) ) {
        /**
         *              Perform bidiagonal QR iteration, if desired, computing
         *              left singular vectors in U and computing right singular
         *              vectors in VT
         *              (Workspace: need 5*N-4)
         **/
        dbdsqr( 'u', n, ncvt, nru, 0, s, &work_1( ie ), vt,
               ldvt, u, ldu, dum, 1, &work_1( iwork ), info );
    } else if( ( !wntuo ) && wntvo ) {
        /**
         *              Perform bidiagonal QR iteration, if desired, computing
         *              left singular vectors in U and computing right singular
         *              vectors in A
         *              (Workspace: need 5*N-4)
         **/
        dbdsqr( 'u', n, ncvt, nru, 0, s, &work_1( ie ), a, lda,
               u, ldu, dum, 1, &work_1( iwork ), info );
    } else {
        /**
         *              Perform bidiagonal QR iteration, if desired, computing
         *              left singular vectors in A and computing right singular
         *              vectors in VT
         *              (Workspace: need 5*N-4)
         **/
        dbdsqr( 'u', n, ncvt, nru, 0, s, &work_1( ie ), vt,
               ldvt, a, lda, dum, 1, &work_1( iwork ), info );
    }

  }

  /**
   *     If DBDSQR failed to converge, copy unconverged superdiagonals
   *     to WORK( 2:MINMN )
   **/
  if( *info!=0 ) {
    if( ie>2 ) {
      for (i=1 ; i<=minmn - 1 ; i+=1) {
        work_1( i+1 ) = work_1( i+ie-1 );
      }
    }
    if( ie<2 ) {
      for (i=minmn - 1 ; i>=1 ; i+=-1) {
        work_1( i+1 ) = work_1( i+ie-1 );
      }
    }
  }
  /**
   *     Undo scaling if necessary
   **/
  if( iscl==1 ) {
    if( anrm>bignum )
      dlascl( 'g', 0, 0, bignum, anrm, minmn, 1, s, minmn,
             &ierr );
    if( *info!=0 && anrm>bignum )
      dlascl( 'g', 0, 0, bignum, anrm, minmn-1, 1, &work_1( 2 ),
             minmn, &ierr );
    if( anrm<smlnum )
      dlascl( 'g', 0, 0, smlnum, anrm, minmn, 1, s, minmn,
             &ierr );
    if( *info!=0 && anrm<smlnum )
      dlascl( 'g', 0, 0, smlnum, anrm, minmn-1, 1, &work_1( 2 ),
             minmn, &ierr );
  }
  /**
   *     Return optimal workspace in WORK(1)
   **/
  work_1( 1 ) = maxwrk;

  return;
  /**
   *     End of DGESVD
   **/
}
