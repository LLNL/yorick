/*
 * $Id: dgesv2.c,v 1.1 2005-09-18 22:04:46 dhmunro Exp $
 * LAPACK routine to return the SVD of a matrix, m<n branch (see dgesvd.c).
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "dg.h"

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



void dgesv2( char jobu, char jobvt, long m, long n,
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
   * This is m<n branch of original dgesvd routine (DHM).
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
  /*     .. Parameters ..*/
#undef zero
#define zero 0.0e0
#undef one
#define one 1.0e0
  /**     ..
   *     .. Local Scalars ..*/
  int            wntua, wntuas, wntun, wntuo, wntus, wntva,
  wntvas, wntvn, wntvo, wntvs;
  long   blk, chunk, i, ie=0, ierr, ir, iscl, itau, itaup,
         itauq, iu, iwork, ldwrkr, ldwrku, maxwrk=0,
         minmn, minwrk, mnthr, ncvt=0, nru=0, nrvt=0,
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
    if( n>=mnthr ) {
      if( wntvn ) {
        /**
         *                 Path 1t(N much larger than M, JOBVT='N')
         **/
        maxwrk = m + m*ilaenv( 1, "dgelqf", " ", m, n, -1,
                              -1 );
        maxwrk = max( maxwrk, 3*m+2*m*
                     ilaenv( 1, "dgebrd", " ", m, m, -1, -1 ) );
        if( wntuo || wntuas )
          maxwrk = max( maxwrk, 3*m+m*
                       ilaenv( 1, "dorgbr", "q", m, m, m, -1 ) );
        maxwrk = max( maxwrk, 5*m-4 );
        minwrk = max( 4*m, 5*m-4 );
      } else if( wntvo && wntun ) {
        /**
         *                 Path 2t(N much larger than M, JOBU='N', JOBVT='O')
         **/
        wrkbl = m + m*ilaenv( 1, "dgelqf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, m+m*ilaenv( 1, "dorglq", " ", m,
                                       n, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+2*m*
                    ilaenv( 1, "dgebrd", " ", m, m, -1, -1 ) );
        wrkbl = max( wrkbl, 3*m+( m-1 )*
                    ilaenv( 1, "dorgbr", "p", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 5*m-4 );
        maxwrk = max( m*m+wrkbl, m*m+m*n+m );
        minwrk = max( 3*m+n, 5*m-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntvo && wntuas ) {
        /**
         *                 Path 3t(N much larger than M, JOBU='S' or 'A',
         *                 JOBVT='O')
         **/
        wrkbl = m + m*ilaenv( 1, "dgelqf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, m+m*ilaenv( 1, "dorglq", " ", m,
                                       n, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+2*m*
                    ilaenv( 1, "dgebrd", " ", m, m, -1, -1 ) );
        wrkbl = max( wrkbl, 3*m+( m-1 )*
                    ilaenv( 1, "dorgbr", "p", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+m*
                    ilaenv( 1, "dorgbr", "q", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 5*m-4 );
        maxwrk = max( m*m+wrkbl, m*m+m*n+m );
        minwrk = max( 3*m+n, 5*m-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntvs && wntun ) {
        /**
         *                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
         **/
        wrkbl = m + m*ilaenv( 1, "dgelqf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, m+m*ilaenv( 1, "dorglq", " ", m,
                                       n, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+2*m*
                    ilaenv( 1, "dgebrd", " ", m, m, -1, -1 ) );
        wrkbl = max( wrkbl, 3*m+( m-1 )*
                    ilaenv( 1, "dorgbr", "p", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 5*m-4 );
        maxwrk = m*m + wrkbl;
        minwrk = max( 3*m+n, 5*m-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntvs && wntuo ) {
        /**
         *                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
         **/
        wrkbl = m + m*ilaenv( 1, "dgelqf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, m+m*ilaenv( 1, "dorglq", " ", m,
                                       n, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+2*m*
                    ilaenv( 1, "dgebrd", " ", m, m, -1, -1 ) );
        wrkbl = max( wrkbl, 3*m+( m-1 )*
                    ilaenv( 1, "dorgbr", "p", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+m*
                    ilaenv( 1, "dorgbr", "q", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 5*m-4 );
        maxwrk = 2*m*m + wrkbl;
        minwrk = max( 3*m+n, 5*m-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntvs && wntuas ) {
        /**
         *                 Path 6t(N much larger than M, JOBU='S' or 'A',
         *                 JOBVT='S')
         **/
        wrkbl = m + m*ilaenv( 1, "dgelqf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, m+m*ilaenv( 1, "dorglq", " ", m,
                                       n, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+2*m*
                    ilaenv( 1, "dgebrd", " ", m, m, -1, -1 ) );
        wrkbl = max( wrkbl, 3*m+( m-1 )*
                    ilaenv( 1, "dorgbr", "p", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+m*
                    ilaenv( 1, "dorgbr", "q", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 5*m-4 );
        maxwrk = m*m + wrkbl;
        minwrk = max( 3*m+n, 5*m-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntva && wntun ) {
        /**
         *                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
         **/
        wrkbl = m + m*ilaenv( 1, "dgelqf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, m+n*ilaenv( 1, "dorglq", " ", n,
                                       n, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+2*m*
                    ilaenv( 1, "dgebrd", " ", m, m, -1, -1 ) );
        wrkbl = max( wrkbl, 3*m+( m-1 )*
                    ilaenv( 1, "dorgbr", "p", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 5*m-4 );
        maxwrk = m*m + wrkbl;
        minwrk = max( 3*m+n, 5*m-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntva && wntuo ) {
        /**
         *                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
         **/
        wrkbl = m + m*ilaenv( 1, "dgelqf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, m+n*ilaenv( 1, "dorglq", " ", n,
                                       n, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+2*m*
                    ilaenv( 1, "dgebrd", " ", m, m, -1, -1 ) );
        wrkbl = max( wrkbl, 3*m+( m-1 )*
                    ilaenv( 1, "dorgbr", "p", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+m*
                    ilaenv( 1, "dorgbr", "q", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 5*m-4 );
        maxwrk = 2*m*m + wrkbl;
        minwrk = max( 3*m+n, 5*m-4 );
        minwrk = min( minwrk, maxwrk );
      } else if( wntva && wntuas ) {
        /**
         *                 Path 9t(N much larger than M, JOBU='S' or 'A',
         *                 JOBVT='A')
         **/
        wrkbl = m + m*ilaenv( 1, "dgelqf", " ", m, n, -1, -1 );
        wrkbl = max( wrkbl, m+n*ilaenv( 1, "dorglq", " ", n,
                                       n, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+2*m*
                    ilaenv( 1, "dgebrd", " ", m, m, -1, -1 ) );
        wrkbl = max( wrkbl, 3*m+( m-1 )*
                    ilaenv( 1, "dorgbr", "p", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 3*m+m*
                    ilaenv( 1, "dorgbr", "q", m, m, m, -1 ) );
        wrkbl = max( wrkbl, 5*m-4 );
        maxwrk = m*m + wrkbl;
        minwrk = max( 3*m+n, 5*m-4 );
        minwrk = min( minwrk, maxwrk );
      }
    } else {
      /**
       *              Path 10t(N greater than M, but not much larger)
       **/
      maxwrk = 3*m + ( m+n )*ilaenv( 1, "dgebrd", " ", m, n,
                                    -1, -1 );
      if( wntvs || wntvo )
        maxwrk = max( maxwrk, 3*m+m*
                     ilaenv( 1, "dorgbr", "p", m, n, m, -1 ) );
      if( wntva )
        maxwrk = max( maxwrk, 3*m+n*
                     ilaenv( 1, "dorgbr", "p", n, n, m, -1 ) );
      if( !wntun )
        maxwrk = max( maxwrk, 3*m+( m-1 )*
                     ilaenv( 1, "dorgbr", "q", m, m, m, -1 ) );
      maxwrk = max( maxwrk, 5*m-4 );
      minwrk = max( 3*m+n, 5*m-4 );
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
   *        A has more columns than rows. If A has sufficiently more
   *        columns than rows, first reduce using the LQ decomposition (if
   *        sufficient workspace available)
   **/
  if( n>=mnthr ) {

    if( wntvn ) {
      /**
       *              Path 1t(N much larger than M, JOBVT='N')
       *              No right singular vectors to be computed
       **/
      itau = 1;
      iwork = itau + m;
      /**
       *              Compute A=L*Q
       *              (Workspace: need 2*M, prefer M+M*NB)
       **/
      dgelqf( m, n, a, lda, &work_1( itau ), &work_1( iwork ),
             lwork-iwork+1, &ierr );
      /**
       *              Zero out above L
       **/
      dlaset( 'u', m-1, m-1, zero, zero, &a_2( 1, 2 ), lda );
      ie = 1;
      itauq = ie + m;
      itaup = itauq + m;
      iwork = itaup + m;
      /**
       *              Bidiagonalize L in A
       *              (Workspace: need 4*M, prefer 3*M+2*M*NB)
       **/
      dgebrd( m, m, a, lda, s, &work_1( ie ), &work_1( itauq ),
             &work_1( itaup ), &work_1( iwork ), lwork-iwork+1,
             &ierr );
      if( wntuo || wntuas ) {
        /**
         *                 If left singular vectors desired, generate Q
         *                 (Workspace: need 4*M, prefer 3*M+M*NB)
         **/
        dorgbr( 'q', m, m, m, a, lda, &work_1( itauq ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
      }
      iwork = ie + m;
      nru = 0;
      if( wntuo || wntuas )
        nru = m;
      /**
       *              Perform bidiagonal QR iteration, computing left singular
       *              vectors of A in A if desired
       *              (Workspace: need 5*M-4)
       **/
      dbdsqr( 'u', m, 0, nru, 0, s, &work_1( ie ), dum, 1, a,
             lda, dum, 1, &work_1( iwork ), info );
      /**
       *              If left singular vectors desired in U, copy them there
       **/
      if( wntuas )
        dlacpy( 'f', m, m, a, lda, u, ldu );

    } else if( wntvo && wntun ) {
      /**
       *              Path 2t(N much larger than M, JOBU='N', JOBVT='O')
       *              M right singular vectors to be overwritten on A and
       *              no left singular vectors to be computed
       **/
      if( lwork>=m*m+max( 4*m, 5*m-4 ) ) {
        /**
         *                 Sufficient workspace for a fast algorithm
         **/
        ir = 1;
        if( lwork>=max( wrkbl, lda*n+m )+lda*m ) {
          /**
           *                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
           **/
          ldwrku = lda;
          chunk = n;
          ldwrkr = lda;
        } else if( lwork>=max( wrkbl, lda*n+m )+m*m ) {
          /**
           *                    WORK(IU) is LDA by N and WORK(IR) is M by M
           **/
          ldwrku = lda;
          chunk = n;
          ldwrkr = m;
        } else {
          /**
           *                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
           **/
          ldwrku = m;
          chunk = ( lwork-m*m-m ) / m;
          ldwrkr = m;
        }
        itau = ir + ldwrkr*m;
        iwork = itau + m;
        /**
         *                 Compute A=L*Q
         *                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
         **/
        dgelqf( m, n, a, lda, &work_1( itau ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        /**
         *                 Copy L to WORK(IR) and zero out above it
         **/
        dlacpy( 'l', m, m, a, lda, &work_1( ir ), ldwrkr );
        dlaset( 'u', m-1, m-1, zero, zero,
               &work_1( ir+ldwrkr ), ldwrkr );
        /**
         *                 Generate Q in A
         *                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
         **/
        dorglq( m, n, m, a, lda, &work_1( itau ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        ie = itau;
        itauq = ie + m;
        itaup = itauq + m;
        iwork = itaup + m;
        /**
         *                 Bidiagonalize L in WORK(IR)
         *                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
         **/
        dgebrd( m, m, &work_1( ir ), ldwrkr, s, &work_1( ie ),
               &work_1( itauq ), &work_1( itaup ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        /**
         *                 Generate right vectors bidiagonalizing L
         *                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
         **/
        dorgbr( 'p', m, m, m, &work_1( ir ), ldwrkr,
               &work_1( itaup ), &work_1( iwork ),
               lwork-iwork+1, &ierr );
        iwork = ie + m;
        /**
         *                 Perform bidiagonal QR iteration, computing right
         *                 singular vectors of L in WORK(IR)
         *                 (Workspace: need M*M+5*M-4)
         **/
        dbdsqr( 'u', m, m, 0, 0, s, &work_1( ie ),
               &work_1( ir ), ldwrkr, dum, 1, dum, 1,
               &work_1( iwork ), info );
        iu = ie + m;
        /**
         *                 Multiply right singular vectors of L in WORK(IR) by Q
         *                 in A, storing result in WORK(IU) and copying to A
         *                 (Workspace: need M*M+2*M, prefer M*M+M*N+M)
         **/
        for (i=1 ; chunk>0?i<=n:i>=n ; i+=chunk) {
          blk = min( n-i+1, chunk );
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, blk, m,
                      one, &work_1( ir ), ldwrkr, &a_2( 1, i ), lda, zero,
                      &work_1( iu ), ldwrku );
          dlacpy( 'f', m, blk, &work_1( iu ), ldwrku,
                 &a_2( 1, i ), lda );
        }

      } else {
        /**
         *                 Insufficient workspace for a fast algorithm
         **/
        ie = 1;
        itauq = ie + m;
        itaup = itauq + m;
        iwork = itaup + m;
        /**
         *                 Bidiagonalize A
         *                 (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
         **/
        dgebrd( m, n, a, lda, s, &work_1( ie ),
               &work_1( itauq ), &work_1( itaup ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        /**
         *                 Generate right vectors bidiagonalizing A
         *                 (Workspace: need 4*M, prefer 3*M+M*NB)
         **/
        dorgbr( 'p', m, n, m, a, lda, &work_1( itaup ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        iwork = ie + m;
        /**
         *                 Perform bidiagonal QR iteration, computing right
         *                 singular vectors of A in A
         *                 (Workspace: need 5*M-4)
         **/
        dbdsqr( 'l', m, n, 0, 0, s, &work_1( ie ), a, lda,
               dum, 1, dum, 1, &work_1( iwork ), info );

      }

    } else if( wntvo && wntuas ) {
      /**
       *              Path 3t(N much larger than M, JOBU='S' or 'A', JOBVT='O')
       *              M right singular vectors to be overwritten on A and
       *              M left singular vectors to be computed in U
       **/
      if( lwork>=m*m+max( 4*m, 5*m-4 ) ) {
        /**
         *                 Sufficient workspace for a fast algorithm
         **/
        ir = 1;
        if( lwork>=max( wrkbl, lda*n+m )+lda*m ) {
          /**
           *                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
           **/
          ldwrku = lda;
          chunk = n;
          ldwrkr = lda;
        } else if( lwork>=max( wrkbl, lda*n+m )+m*m ) {
          /**
           *                    WORK(IU) is LDA by N and WORK(IR) is M by M
           **/
          ldwrku = lda;
          chunk = n;
          ldwrkr = m;
        } else {
          /**
           *                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
           **/
          ldwrku = m;
          chunk = ( lwork-m*m-m ) / m;
          ldwrkr = m;
        }
        itau = ir + ldwrkr*m;
        iwork = itau + m;
        /**
         *                 Compute A=L*Q
         *                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
         **/
        dgelqf( m, n, a, lda, &work_1( itau ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        /**
         *                 Copy L to U, zeroing about above it
         **/
        dlacpy( 'l', m, m, a, lda, u, ldu );
        dlaset( 'u', m-1, m-1, zero, zero, &u_2( 1, 2 ),
               ldu );
        /**
         *                 Generate Q in A
         *                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
         **/
        dorglq( m, n, m, a, lda, &work_1( itau ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        ie = itau;
        itauq = ie + m;
        itaup = itauq + m;
        iwork = itaup + m;
        /**
         *                 Bidiagonalize L in U, copying result to WORK(IR)
         *                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
         **/
        dgebrd( m, m, u, ldu, s, &work_1( ie ),
               &work_1( itauq ), &work_1( itaup ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        dlacpy( 'u', m, m, u, ldu, &work_1( ir ), ldwrkr );
        /**
         *                 Generate right vectors bidiagonalizing L in WORK(IR)
         *                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
         **/
        dorgbr( 'p', m, m, m, &work_1( ir ), ldwrkr,
               &work_1( itaup ), &work_1( iwork ),
               lwork-iwork+1, &ierr );
        /**
         *                 Generate left vectors bidiagonalizing L in U
         *                 (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
         **/
        dorgbr( 'q', m, m, m, u, ldu, &work_1( itauq ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        iwork = ie + m;
        /**
         *                 Perform bidiagonal QR iteration, computing left
         *                 singular vectors of L in U, and computing right
         *                 singular vectors of L in WORK(IR)
         *                 (Workspace: need M*M+5*M-4)
         **/
        dbdsqr( 'u', m, m, m, 0, s, &work_1( ie ),
               &work_1( ir ), ldwrkr, u, ldu, dum, 1,
               &work_1( iwork ), info );
        iu = ie + m;
        /**
         *                 Multiply right singular vectors of L in WORK(IR) by Q
         *                 in A, storing result in WORK(IU) and copying to A
         *                 (Workspace: need M*M+2*M, prefer M*M+M*N+M))
         **/
        for (i=1 ; chunk>0?i<=n:i>=n ; i+=chunk) {
          blk = min( n-i+1, chunk );
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, blk, m,
                      one, &work_1( ir ), ldwrkr, &a_2( 1, i ), lda, zero,
                      &work_1( iu ), ldwrku );
          dlacpy( 'f', m, blk, &work_1( iu ), ldwrku,
                 &a_2( 1, i ), lda );
        }

      } else {
        /**
         *                 Insufficient workspace for a fast algorithm
         **/
        itau = 1;
        iwork = itau + m;
        /**
         *                 Compute A=L*Q
         *                 (Workspace: need 2*M, prefer M+M*NB)
         **/
        dgelqf( m, n, a, lda, &work_1( itau ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        /**
         *                 Copy L to U, zeroing out above it
         **/
        dlacpy( 'l', m, m, a, lda, u, ldu );
        dlaset( 'u', m-1, m-1, zero, zero, &u_2( 1, 2 ),
               ldu );
        /**
         *                 Generate Q in A
         *                 (Workspace: need 2*M, prefer M+M*NB)
         **/
        dorglq( m, n, m, a, lda, &work_1( itau ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        ie = itau;
        itauq = ie + m;
        itaup = itauq + m;
        iwork = itaup + m;
        /**
         *                 Bidiagonalize L in U
         *                 (Workspace: need 4*M, prefer 3*M+2*M*NB)
         **/
        dgebrd( m, m, u, ldu, s, &work_1( ie ),
               &work_1( itauq ), &work_1( itaup ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        /**
         *                 Multiply right vectors bidiagonalizing L by Q in A
         *                 (Workspace: need 3*M+N, prefer 3*M+N*NB)
         **/
        dormbr( 'p', 'l', 't', m, n, m, u, ldu,
               &work_1( itaup ), a, lda, &work_1( iwork ),
               lwork-iwork+1, &ierr );
        /**
         *                 Generate left vectors bidiagonalizing L in U
         *                 (Workspace: need 4*M, prefer 3*M+M*NB)
         **/
        dorgbr( 'q', m, m, m, u, ldu, &work_1( itauq ),
               &work_1( iwork ), lwork-iwork+1, &ierr );
        iwork = ie + m;
        /**
         *                 Perform bidiagonal QR iteration, computing left
         *                 singular vectors of A in U and computing right
         *                 singular vectors of A in A
         *                 (Workspace: need 5*M-4)
         **/
        dbdsqr( 'u', m, n, m, 0, s, &work_1( ie ), a, lda,
               u, ldu, dum, 1, &work_1( iwork ), info );

      }

    } else if( wntvs ) {

      if( wntun ) {
        /**
         *                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
         *                 M right singular vectors to be computed in VT and
         *                 no left singular vectors to be computed
         **/
        if( lwork>=m*m+max( 4*m, 5*m-4 ) ) {
          /**
           *                    Sufficient workspace for a fast algorithm
           **/
          ir = 1;
          if( lwork>=wrkbl+lda*m ) {
            /**
             *                       WORK(IR) is LDA by M
             **/
            ldwrkr = lda;
          } else {
            /**
             *                       WORK(IR) is M by M
             **/
            ldwrkr = m;
          }
          itau = ir + ldwrkr*m;
          iwork = itau + m;
          /**
           *                    Compute A=L*Q
           *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
           **/
          dgelqf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Copy L to WORK(IR), zeroing out above it
           **/
          dlacpy( 'l', m, m, a, lda, &work_1( ir ),
                 ldwrkr );
          dlaset( 'u', m-1, m-1, zero, zero,
                 &work_1( ir+ldwrkr ), ldwrkr );
          /**
           *                    Generate Q in A
           *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
           **/
          dorglq( m, n, m, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          ie = itau;
          itauq = ie + m;
          itaup = itauq + m;
          iwork = itaup + m;
          /**
           *                    Bidiagonalize L in WORK(IR)
           *                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
           **/
          dgebrd( m, m, &work_1( ir ), ldwrkr, s,
                 &work_1( ie ), &work_1( itauq ),
                 &work_1( itaup ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          /**
           *                    Generate right vectors bidiagonalizing L in
           *                    WORK(IR)
           *                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
           **/
          dorgbr( 'p', m, m, m, &work_1( ir ), ldwrkr,
                 &work_1( itaup ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          iwork = ie + m;
          /**
           *                    Perform bidiagonal QR iteration, computing right
           *                    singular vectors of L in WORK(IR)
           *                    (Workspace: need M*M+5*M-4)
           **/
          dbdsqr( 'u', m, m, 0, 0, s, &work_1( ie ),
                 &work_1( ir ), ldwrkr, dum, 1, dum, 1,
                 &work_1( iwork ), info );
          /**
           *                    Multiply right singular vectors of L in WORK(IR) by
           *                    Q in A, storing result in VT
           *                    (Workspace: need M*M)
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n, m,
                      one, &work_1( ir ), ldwrkr, a, lda, zero, vt, ldvt );

        } else {
          /**
           *                    Insufficient workspace for a fast algorithm
           **/
          itau = 1;
          iwork = itau + m;
          /**
           *                    Compute A=L*Q
           *                    (Workspace: need 2*M, prefer M+M*NB)
           **/
          dgelqf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Copy result to VT
           **/
          dlacpy( 'u', m, n, a, lda, vt, ldvt );
          /**
           *                    Generate Q in VT
           *                    (Workspace: need 2*M, prefer M+M*NB)
           **/
          dorglq( m, n, m, vt, ldvt, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          ie = itau;
          itauq = ie + m;
          itaup = itauq + m;
          iwork = itaup + m;
          /**
           *                    Zero out above L in A
           **/
          dlaset( 'u', m-1, m-1, zero, zero, &a_2( 1, 2 ),
                 lda );
          /**
           *                    Bidiagonalize L in A
           *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
           **/
          dgebrd( m, m, a, lda, s, &work_1( ie ),
                 &work_1( itauq ), &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Multiply right vectors bidiagonalizing L by Q in VT
           *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
           **/
          dormbr( 'p', 'l', 't', m, n, m, a, lda,
                 &work_1( itaup ), vt, ldvt,
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          iwork = ie + m;
          /**
           *                    Perform bidiagonal QR iteration, computing right
           *                    singular vectors of A in VT
           *                    (Workspace: need 5*M-4)
           **/
          dbdsqr( 'u', m, n, 0, 0, s, &work_1( ie ), vt,
                 ldvt, dum, 1, dum, 1, &work_1( iwork ),
                 info );

        }

      } else if( wntuo ) {
        /**
         *                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
         *                 M right singular vectors to be computed in VT and
         *                 M left singular vectors to be overwritten on A
         **/
        if( lwork>=2*m*m+max( 4*m, 5*m-4 ) ) {
          /**
           *                    Sufficient workspace for a fast algorithm
           **/
          iu = 1;
          if( lwork>=wrkbl+2*lda*m ) {
            /**
             *                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
             **/
            ldwrku = lda;
            ir = iu + ldwrku*m;
            ldwrkr = lda;
          } else if( lwork>=wrkbl+( lda+m )*m ) {
            /**
             *                       WORK(IU) is LDA by M and WORK(IR) is M by M
             **/
            ldwrku = lda;
            ir = iu + ldwrku*m;
            ldwrkr = m;
          } else {
            /**
             *                       WORK(IU) is M by M and WORK(IR) is M by M
             **/
            ldwrku = m;
            ir = iu + ldwrku*m;
            ldwrkr = m;
          }
          itau = ir + ldwrkr*m;
          iwork = itau + m;
          /**
           *                    Compute A=L*Q
           *                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
           **/
          dgelqf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Copy L to WORK(IU), zeroing out below it
           **/
          dlacpy( 'l', m, m, a, lda, &work_1( iu ),
                 ldwrku );
          dlaset( 'u', m-1, m-1, zero, zero,
                 &work_1( iu+ldwrku ), ldwrku );
          /**
           *                    Generate Q in A
           *                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
           **/
          dorglq( m, n, m, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          ie = itau;
          itauq = ie + m;
          itaup = itauq + m;
          iwork = itaup + m;
          /**
           *                    Bidiagonalize L in WORK(IU), copying result to
           *                    WORK(IR)
           *                    (Workspace: need 2*M*M+4*M,
           *                                prefer 2*M*M+3*M+2*M*NB)
           **/
          dgebrd( m, m, &work_1( iu ), ldwrku, s,
                 &work_1( ie ), &work_1( itauq ),
                 &work_1( itaup ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          dlacpy( 'l', m, m, &work_1( iu ), ldwrku,
                 &work_1( ir ), ldwrkr );
          /**
           *                    Generate right bidiagonalizing vectors in WORK(IU)
           *                    (Workspace: need 2*M*M+4*M-1,
           *                                prefer 2*M*M+3*M+(M-1)*NB)
           **/
          dorgbr( 'p', m, m, m, &work_1( iu ), ldwrku,
                 &work_1( itaup ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          /**
           *                    Generate left bidiagonalizing vectors in WORK(IR)
           *                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
           **/
          dorgbr( 'q', m, m, m, &work_1( ir ), ldwrkr,
                 &work_1( itauq ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          iwork = ie + m;
          /**
           *                    Perform bidiagonal QR iteration, computing left
           *                    singular vectors of L in WORK(IR) and computing
           *                    right singular vectors of L in WORK(IU)
           *                    (Workspace: need 2*M*M+5*M-4)
           **/
          dbdsqr( 'u', m, m, m, 0, s, &work_1( ie ),
                 &work_1( iu ), ldwrku, &work_1( ir ),
                 ldwrkr, dum, 1, &work_1( iwork ), info );
          /**
           *                    Multiply right singular vectors of L in WORK(IU) by
           *                    Q in A, storing result in VT
           *                    (Workspace: need M*M)
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n, m,
                      one, &work_1( iu ), ldwrku, a, lda, zero, vt, ldvt );
          /**
           *                    Copy left singular vectors of L to A
           *                    (Workspace: need M*M)
           **/
          dlacpy( 'f', m, m, &work_1( ir ), ldwrkr, a,
                 lda );

        } else {
          /**
           *                    Insufficient workspace for a fast algorithm
           **/
          itau = 1;
          iwork = itau + m;
          /**
           *                    Compute A=L*Q, copying result to VT
           *                    (Workspace: need 2*M, prefer M+M*NB)
           **/
          dgelqf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          dlacpy( 'u', m, n, a, lda, vt, ldvt );
          /**
           *                    Generate Q in VT
           *                    (Workspace: need 2*M, prefer M+M*NB)
           **/
          dorglq( m, n, m, vt, ldvt, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          ie = itau;
          itauq = ie + m;
          itaup = itauq + m;
          iwork = itaup + m;
          /**
           *                    Zero out above L in A
           **/
          dlaset( 'u', m-1, m-1, zero, zero, &a_2( 1, 2 ),
                 lda );
          /**
           *                    Bidiagonalize L in A
           *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
           **/
          dgebrd( m, m, a, lda, s, &work_1( ie ),
                 &work_1( itauq ), &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Multiply right vectors bidiagonalizing L by Q in VT
           *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
           **/
          dormbr( 'p', 'l', 't', m, n, m, a, lda,
                 &work_1( itaup ), vt, ldvt,
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Generate left bidiagonalizing vectors of L in A
           *                    (Workspace: need 4*M, prefer 3*M+M*NB)
           **/
          dorgbr( 'q', m, m, m, a, lda, &work_1( itauq ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          iwork = ie + m;
          /**
           *                    Perform bidiagonal QR iteration, compute left
           *                    singular vectors of A in A and compute right
           *                    singular vectors of A in VT
           *                    (Workspace: need 5*M-4)
           **/
          dbdsqr( 'u', m, n, m, 0, s, &work_1( ie ), vt,
                 ldvt, a, lda, dum, 1, &work_1( iwork ),
                 info );

        }

      } else if( wntuas ) {
        /**
         *                 Path 6t(N much larger than M, JOBU='S' or 'A',
         *                         JOBVT='S')
         *                 M right singular vectors to be computed in VT and
         *                 M left singular vectors to be computed in U
         **/
        if( lwork>=m*m+max( 4*m, 5*m-4 ) ) {
          /**
           *                    Sufficient workspace for a fast algorithm
           **/
          iu = 1;
          if( lwork>=wrkbl+lda*m ) {
            /**
             *                       WORK(IU) is LDA by N
             **/
            ldwrku = lda;
          } else {
            /**
             *                       WORK(IU) is LDA by M
             **/
            ldwrku = m;
          }
          itau = iu + ldwrku*m;
          iwork = itau + m;
          /**
           *                    Compute A=L*Q
           *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
           **/
          dgelqf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Copy L to WORK(IU), zeroing out above it
           **/
          dlacpy( 'l', m, m, a, lda, &work_1( iu ),
                 ldwrku );
          dlaset( 'u', m-1, m-1, zero, zero,
                 &work_1( iu+ldwrku ), ldwrku );
          /**
           *                    Generate Q in A
           *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
           **/
          dorglq( m, n, m, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          ie = itau;
          itauq = ie + m;
          itaup = itauq + m;
          iwork = itaup + m;
          /**
           *                    Bidiagonalize L in WORK(IU), copying result to U
           *                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
           **/
          dgebrd( m, m, &work_1( iu ), ldwrku, s,
                 &work_1( ie ), &work_1( itauq ),
                 &work_1( itaup ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          dlacpy( 'l', m, m, &work_1( iu ), ldwrku, u,
                 ldu );
          /**
           *                    Generate right bidiagonalizing vectors in WORK(IU)
           *                    (Workspace: need M*M+4*M-1,
           *                                prefer M*M+3*M+(M-1)*NB)
           **/
          dorgbr( 'p', m, m, m, &work_1( iu ), ldwrku,
                 &work_1( itaup ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          /**
           *                    Generate left bidiagonalizing vectors in U
           *                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
           **/
          dorgbr( 'q', m, m, m, u, ldu, &work_1( itauq ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          iwork = ie + m;
          /**
           *                    Perform bidiagonal QR iteration, computing left
           *                    singular vectors of L in U and computing right
           *                    singular vectors of L in WORK(IU)
           *                    (Workspace: need M*M+5*M-4)
           **/
          dbdsqr( 'u', m, m, m, 0, s, &work_1( ie ),
                 &work_1( iu ), ldwrku, u, ldu, dum, 1,
                 &work_1( iwork ), info );
          /**
           *                    Multiply right singular vectors of L in WORK(IU) by
           *                    Q in A, storing result in VT
           *                    (Workspace: need M*M)
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n, m,
                      one, &work_1( iu ), ldwrku, a, lda, zero, vt, ldvt );

        } else {
          /**
           *                    Insufficient workspace for a fast algorithm
           **/
          itau = 1;
          iwork = itau + m;
          /**
           *                    Compute A=L*Q, copying result to VT
           *                    (Workspace: need 2*M, prefer M+M*NB)
           **/
          dgelqf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          dlacpy( 'u', m, n, a, lda, vt, ldvt );
          /**
           *                    Generate Q in VT
           *                    (Workspace: need 2*M, prefer M+M*NB)
           **/
          dorglq( m, n, m, vt, ldvt, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Copy L to U, zeroing out above it
           **/
          dlacpy( 'l', m, m, a, lda, u, ldu );
          dlaset( 'u', m-1, m-1, zero, zero, &u_2( 1, 2 ),
                 ldu );
          ie = itau;
          itauq = ie + m;
          itaup = itauq + m;
          iwork = itaup + m;
          /**
           *                    Bidiagonalize L in U
           *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
           **/
          dgebrd( m, m, u, ldu, s, &work_1( ie ),
                 &work_1( itauq ), &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Multiply right bidiagonalizing vectors in U by Q
           *                    in VT
           *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
           **/
          dormbr( 'p', 'l', 't', m, n, m, u, ldu,
                 &work_1( itaup ), vt, ldvt,
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Generate left bidiagonalizing vectors in U
           *                    (Workspace: need 4*M, prefer 3*M+M*NB)
           **/
          dorgbr( 'q', m, m, m, u, ldu, &work_1( itauq ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          iwork = ie + m;
          /**
           *                    Perform bidiagonal QR iteration, computing left
           *                    singular vectors of A in U and computing right
           *                    singular vectors of A in VT
           *                    (Workspace: need 5*M-4)
           **/
          dbdsqr( 'u', m, n, m, 0, s, &work_1( ie ), vt,
                 ldvt, u, ldu, dum, 1, &work_1( iwork ),
                 info );

        }

      }

    } else if( wntva ) {

      if( wntun ) {
        /**
         *                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
         *                 N right singular vectors to be computed in VT and
         *                 no left singular vectors to be computed
         **/
        if( lwork>=m*m+max( n+m, max(4*m, 5*m-4) ) ) {
          /**
           *                    Sufficient workspace for a fast algorithm
           **/
          ir = 1;
          if( lwork>=wrkbl+lda*m ) {
            /**
             *                       WORK(IR) is LDA by M
             **/
            ldwrkr = lda;
          } else {
            /**
             *                       WORK(IR) is M by M
             **/
            ldwrkr = m;
          }
          itau = ir + ldwrkr*m;
          iwork = itau + m;
          /**
           *                    Compute A=L*Q, copying result to VT
           *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
           **/
          dgelqf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          dlacpy( 'u', m, n, a, lda, vt, ldvt );
          /**
           *                    Copy L to WORK(IR), zeroing out above it
           **/
          dlacpy( 'l', m, m, a, lda, &work_1( ir ),
                 ldwrkr );
          dlaset( 'u', m-1, m-1, zero, zero,
                 &work_1( ir+ldwrkr ), ldwrkr );
          /**
           *                    Generate Q in VT
           *                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
           **/
          dorglq( n, n, m, vt, ldvt, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          ie = itau;
          itauq = ie + m;
          itaup = itauq + m;
          iwork = itaup + m;
          /**
           *                    Bidiagonalize L in WORK(IR)
           *                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
           **/
          dgebrd( m, m, &work_1( ir ), ldwrkr, s,
                 &work_1( ie ), &work_1( itauq ),
                 &work_1( itaup ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          /**
           *                    Generate right bidiagonalizing vectors in WORK(IR)
           *                    (Workspace: need M*M+4*M-1,
           *                                prefer M*M+3*M+(M-1)*NB)
           **/
          dorgbr( 'p', m, m, m, &work_1( ir ), ldwrkr,
                 &work_1( itaup ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          iwork = ie + m;
          /**
           *                    Perform bidiagonal QR iteration, computing right
           *                    singular vectors of L in WORK(IR)
           *                    (Workspace: need M*M+5*M-4)
           **/
          dbdsqr( 'u', m, m, 0, 0, s, &work_1( ie ),
                 &work_1( ir ), ldwrkr, dum, 1, dum, 1,
                 &work_1( iwork ), info );
          /**
           *                    Multiply right singular vectors of L in WORK(IR) by
           *                    Q in VT, storing result in A
           *                    (Workspace: need M*M)
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n, m,
                      one, &work_1( ir ), ldwrkr, vt, ldvt, zero, a, lda );
          /**
           *                    Copy right singular vectors of A from A to VT
           **/
          dlacpy( 'f', m, n, a, lda, vt, ldvt );

        } else {
          /**
           *                    Insufficient workspace for a fast algorithm
           **/
          itau = 1;
          iwork = itau + m;
          /**
           *                    Compute A=L*Q, copying result to VT
           *                    (Workspace: need 2*M, prefer M+M*NB)
           **/
          dgelqf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          dlacpy( 'u', m, n, a, lda, vt, ldvt );
          /**
           *                    Generate Q in VT
           *                    (Workspace: need M+N, prefer M+N*NB)
           **/
          dorglq( n, n, m, vt, ldvt, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          ie = itau;
          itauq = ie + m;
          itaup = itauq + m;
          iwork = itaup + m;
          /**
           *                    Zero out above L in A
           **/
          dlaset( 'u', m-1, m-1, zero, zero, &a_2( 1, 2 ),
                 lda );
          /**
           *                    Bidiagonalize L in A
           *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
           **/
          dgebrd( m, m, a, lda, s, &work_1( ie ),
                 &work_1( itauq ), &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Multiply right bidiagonalizing vectors in A by Q
           *                    in VT
           *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
           **/
          dormbr( 'p', 'l', 't', m, n, m, a, lda,
                 &work_1( itaup ), vt, ldvt,
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          iwork = ie + m;
          /**
           *                    Perform bidiagonal QR iteration, computing right
           *                    singular vectors of A in VT
           *                    (Workspace: need 5*M-4)
           **/
          dbdsqr( 'u', m, n, 0, 0, s, &work_1( ie ), vt,
                 ldvt, dum, 1, dum, 1, &work_1( iwork ),
                 info );

        }

      } else if( wntuo ) {
        /**
         *                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
         *                 N right singular vectors to be computed in VT and
         *                 M left singular vectors to be overwritten on A
         **/
        if( lwork>=2*m*m+max( n+m, max(4*m, 5*m-4) ) ) {
          /**
           *                    Sufficient workspace for a fast algorithm
           **/
          iu = 1;
          if( lwork>=wrkbl+2*lda*m ) {
            /**
             *                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
             **/
            ldwrku = lda;
            ir = iu + ldwrku*m;
            ldwrkr = lda;
          } else if( lwork>=wrkbl+( lda+m )*m ) {
            /**
             *                       WORK(IU) is LDA by M and WORK(IR) is M by M
             **/
            ldwrku = lda;
            ir = iu + ldwrku*m;
            ldwrkr = m;
          } else {
            /**
             *                       WORK(IU) is M by M and WORK(IR) is M by M
             **/
            ldwrku = m;
            ir = iu + ldwrku*m;
            ldwrkr = m;
          }
          itau = ir + ldwrkr*m;
          iwork = itau + m;
          /**
           *                    Compute A=L*Q, copying result to VT
           *                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
           **/
          dgelqf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          dlacpy( 'u', m, n, a, lda, vt, ldvt );
          /**
           *                    Generate Q in VT
           *                    (Workspace: need 2*M*M+M+N, prefer 2*M*M+M+N*NB)
           **/
          dorglq( n, n, m, vt, ldvt, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Copy L to WORK(IU), zeroing out above it
           **/
          dlacpy( 'l', m, m, a, lda, &work_1( iu ),
                 ldwrku );
          dlaset( 'u', m-1, m-1, zero, zero,
                 &work_1( iu+ldwrku ), ldwrku );
          ie = itau;
          itauq = ie + m;
          itaup = itauq + m;
          iwork = itaup + m;
          /**
           *                    Bidiagonalize L in WORK(IU), copying result to
           *                    WORK(IR)
           *                    (Workspace: need 2*M*M+4*M,
           *                                prefer 2*M*M+3*M+2*M*NB)
           **/
          dgebrd( m, m, &work_1( iu ), ldwrku, s,
                 &work_1( ie ), &work_1( itauq ),
                 &work_1( itaup ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          dlacpy( 'l', m, m, &work_1( iu ), ldwrku,
                 &work_1( ir ), ldwrkr );
          /**
           *                    Generate right bidiagonalizing vectors in WORK(IU)
           *                    (Workspace: need 2*M*M+4*M-1,
           *                                prefer 2*M*M+3*M+(M-1)*NB)
           **/
          dorgbr( 'p', m, m, m, &work_1( iu ), ldwrku,
                 &work_1( itaup ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          /**
           *                    Generate left bidiagonalizing vectors in WORK(IR)
           *                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
           **/
          dorgbr( 'q', m, m, m, &work_1( ir ), ldwrkr,
                 &work_1( itauq ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          iwork = ie + m;
          /**
           *                    Perform bidiagonal QR iteration, computing left
           *                    singular vectors of L in WORK(IR) and computing
           *                    right singular vectors of L in WORK(IU)
           *                    (Workspace: need 2*M*M+5*M-4)
           **/
          dbdsqr( 'u', m, m, m, 0, s, &work_1( ie ),
                 &work_1( iu ), ldwrku, &work_1( ir ),
                 ldwrkr, dum, 1, &work_1( iwork ), info );
          /**
           *                    Multiply right singular vectors of L in WORK(IU) by
           *                    Q in VT, storing result in A
           *                    (Workspace: need M*M)
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n, m,
                      one, &work_1( iu ), ldwrku, vt, ldvt, zero, a, lda );
          /**
           *                    Copy right singular vectors of A from A to VT
           **/
          dlacpy( 'f', m, n, a, lda, vt, ldvt );
          /**
           *                    Copy left singular vectors of A from WORK(IR) to A
           **/
          dlacpy( 'f', m, m, &work_1( ir ), ldwrkr, a,
                 lda );

        } else {
          /**
           *                    Insufficient workspace for a fast algorithm
           **/
          itau = 1;
          iwork = itau + m;
          /**
           *                    Compute A=L*Q, copying result to VT
           *                    (Workspace: need 2*M, prefer M+M*NB)
           **/
          dgelqf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          dlacpy( 'u', m, n, a, lda, vt, ldvt );
          /**
           *                    Generate Q in VT
           *                    (Workspace: need M+N, prefer M+N*NB)
           **/
          dorglq( n, n, m, vt, ldvt, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          ie = itau;
          itauq = ie + m;
          itaup = itauq + m;
          iwork = itaup + m;
          /**
           *                    Zero out above L in A
           **/
          dlaset( 'u', m-1, m-1, zero, zero, &a_2( 1, 2 ),
                 lda );
          /**
           *                    Bidiagonalize L in A
           *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
           **/
          dgebrd( m, m, a, lda, s, &work_1( ie ),
                 &work_1( itauq ), &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Multiply right bidiagonalizing vectors in A by Q
           *                    in VT
           *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
           **/
          dormbr( 'p', 'l', 't', m, n, m, a, lda,
                 &work_1( itaup ), vt, ldvt,
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Generate left bidiagonalizing vectors in A
           *                    (Workspace: need 4*M, prefer 3*M+M*NB)
           **/
          dorgbr( 'q', m, m, m, a, lda, &work_1( itauq ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          iwork = ie + m;
          /**
           *                    Perform bidiagonal QR iteration, computing left
           *                    singular vectors of A in A and computing right
           *                    singular vectors of A in VT
           *                    (Workspace: need 5*M-4)
           **/
          dbdsqr( 'u', m, n, m, 0, s, &work_1( ie ), vt,
                 ldvt, a, lda, dum, 1, &work_1( iwork ),
                 info );

        }

      } else if( wntuas ) {
        /**
         *                 Path 9t(N much larger than M, JOBU='S' or 'A',
         *                         JOBVT='A')
         *                 N right singular vectors to be computed in VT and
         *                 M left singular vectors to be computed in U
         **/
        if( lwork>=m*m+max( n+m, max(4*m, 5*m-4) ) ) {
          /**
           *                    Sufficient workspace for a fast algorithm
           **/
          iu = 1;
          if( lwork>=wrkbl+lda*m ) {
            /**
             *                       WORK(IU) is LDA by M
             **/
            ldwrku = lda;
          } else {
            /**
             *                       WORK(IU) is M by M
             **/
            ldwrku = m;
          }
          itau = iu + ldwrku*m;
          iwork = itau + m;
          /**
           *                    Compute A=L*Q, copying result to VT
           *                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
           **/
          dgelqf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          dlacpy( 'u', m, n, a, lda, vt, ldvt );
          /**
           *                    Generate Q in VT
           *                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
           **/
          dorglq( n, n, m, vt, ldvt, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Copy L to WORK(IU), zeroing out above it
           **/
          dlacpy( 'l', m, m, a, lda, &work_1( iu ),
                 ldwrku );
          dlaset( 'u', m-1, m-1, zero, zero,
                 &work_1( iu+ldwrku ), ldwrku );
          ie = itau;
          itauq = ie + m;
          itaup = itauq + m;
          iwork = itaup + m;
          /**
           *                    Bidiagonalize L in WORK(IU), copying result to U
           *                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
           **/
          dgebrd( m, m, &work_1( iu ), ldwrku, s,
                 &work_1( ie ), &work_1( itauq ),
                 &work_1( itaup ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          dlacpy( 'l', m, m, &work_1( iu ), ldwrku, u,
                 ldu );
          /**
           *                    Generate right bidiagonalizing vectors in WORK(IU)
           *                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
           **/
          dorgbr( 'p', m, m, m, &work_1( iu ), ldwrku,
                 &work_1( itaup ), &work_1( iwork ),
                 lwork-iwork+1, &ierr );
          /**
           *                    Generate left bidiagonalizing vectors in U
           *                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
           **/
          dorgbr( 'q', m, m, m, u, ldu, &work_1( itauq ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          iwork = ie + m;
          /**
           *                    Perform bidiagonal QR iteration, computing left
           *                    singular vectors of L in U and computing right
           *                    singular vectors of L in WORK(IU)
           *                    (Workspace: need M*M+5*M-4)
           **/
          dbdsqr( 'u', m, m, m, 0, s, &work_1( ie ),
                 &work_1( iu ), ldwrku, u, ldu, dum, 1,
                 &work_1( iwork ), info );
          /**
           *                    Multiply right singular vectors of L in WORK(IU) by
           *                    Q in VT, storing result in A
           *                    (Workspace: need M*M)
           **/
          cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m, n, m,
                      one, &work_1( iu ), ldwrku, vt, ldvt, zero, a, lda );
          /**
           *                    Copy right singular vectors of A from A to VT
           **/
          dlacpy( 'f', m, n, a, lda, vt, ldvt );

        } else {
          /**
           *                    Insufficient workspace for a fast algorithm
           **/
          itau = 1;
          iwork = itau + m;
          /**
           *                    Compute A=L*Q, copying result to VT
           *                    (Workspace: need 2*M, prefer M+M*NB)
           **/
          dgelqf( m, n, a, lda, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          dlacpy( 'u', m, n, a, lda, vt, ldvt );
          /**
           *                    Generate Q in VT
           *                    (Workspace: need M+N, prefer M+N*NB)
           **/
          dorglq( n, n, m, vt, ldvt, &work_1( itau ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Copy L to U, zeroing out above it
           **/
          dlacpy( 'l', m, m, a, lda, u, ldu );
          dlaset( 'u', m-1, m-1, zero, zero, &u_2( 1, 2 ),
                 ldu );
          ie = itau;
          itauq = ie + m;
          itaup = itauq + m;
          iwork = itaup + m;
          /**
           *                    Bidiagonalize L in U
           *                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
           **/
          dgebrd( m, m, u, ldu, s, &work_1( ie ),
                 &work_1( itauq ), &work_1( itaup ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Multiply right bidiagonalizing vectors in U by Q
           *                    in VT
           *                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
           **/
          dormbr( 'p', 'l', 't', m, n, m, u, ldu,
                 &work_1( itaup ), vt, ldvt,
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          /**
           *                    Generate left bidiagonalizing vectors in U
           *                    (Workspace: need 4*M, prefer 3*M+M*NB)
           **/
          dorgbr( 'q', m, m, m, u, ldu, &work_1( itauq ),
                 &work_1( iwork ), lwork-iwork+1, &ierr );
          iwork = ie + m;
          /**
           *                    Perform bidiagonal QR iteration, computing left
           *                    singular vectors of A in U and computing right
           *                    singular vectors of A in VT
           *                    (Workspace: need 5*M-4)
           **/
          dbdsqr( 'u', m, n, m, 0, s, &work_1( ie ), vt,
                 ldvt, u, ldu, dum, 1, &work_1( iwork ),
                 info );

        }

      }

    }

  } else {
    /**
     *           N .LT. MNTHR
     *
     *           Path 10t(N greater than M, but not much larger)
     *           Reduce to bidiagonal form without LQ decomposition
     **/
    ie = 1;
    itauq = ie + m;
    itaup = itauq + m;
    iwork = itaup + m;
    /**
     *           Bidiagonalize A
     *           (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
     **/
    dgebrd( m, n, a, lda, s, &work_1( ie ), &work_1( itauq ),
           &work_1( itaup ), &work_1( iwork ), lwork-iwork+1,
           &ierr );
    if( wntuas ) {
      /**
       *              If left singular vectors desired in U, copy result to U
       *              and generate left bidiagonalizing vectors in U
       *              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
       **/
      dlacpy( 'l', m, m, a, lda, u, ldu );
      dorgbr( 'q', m, m, n, u, ldu, &work_1( itauq ),
             &work_1( iwork ), lwork-iwork+1, &ierr );
    }
    if( wntvas ) {
      /**
       *              If right singular vectors desired in VT, copy result to
       *              VT and generate right bidiagonalizing vectors in VT
       *              (Workspace: need 3*M+NRVT, prefer 3*M+NRVT*NB)
       **/
      dlacpy( 'u', m, n, a, lda, vt, ldvt );
      if( wntva )
        nrvt = n;
      if( wntvs )
        nrvt = m;
      dorgbr( 'p', nrvt, n, m, vt, ldvt, &work_1( itaup ),
             &work_1( iwork ), lwork-iwork+1, &ierr );
    }
    if( wntuo ) {
      /**
       *              If left singular vectors desired in A, generate left
       *              bidiagonalizing vectors in A
       *              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
       **/
      dorgbr( 'q', m, m, n, a, lda, &work_1( itauq ),
             &work_1( iwork ), lwork-iwork+1, &ierr );
    }
    if( wntvo ) {
      /**
       *              If right singular vectors desired in A, generate right
       *              bidiagonalizing vectors in A
       *              (Workspace: need 4*M, prefer 3*M+M*NB)
       **/
      dorgbr( 'p', m, n, m, a, lda, &work_1( itaup ),
             &work_1( iwork ), lwork-iwork+1, &ierr );
    }
    iwork = ie + m;
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
       *              (Workspace: need 5*M-4)
       **/
      dbdsqr( 'l', m, ncvt, nru, 0, s, &work_1( ie ), vt,
             ldvt, u, ldu, dum, 1, &work_1( iwork ), info );
    } else if( ( !wntuo ) && wntvo ) {
      /**
       *              Perform bidiagonal QR iteration, if desired, computing
       *              left singular vectors in U and computing right singular
       *              vectors in A
       *              (Workspace: need 5*M-4)
       **/
      dbdsqr( 'l', m, ncvt, nru, 0, s, &work_1( ie ), a, lda,
             u, ldu, dum, 1, &work_1( iwork ), info );
    } else {
      /**
       *              Perform bidiagonal QR iteration, if desired, computing
       *              left singular vectors in A and computing right singular
       *              vectors in VT
       *              (Workspace: need 5*M-4)
       **/
      dbdsqr( 'l', m, ncvt, nru, 0, s, &work_1( ie ), vt,
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
