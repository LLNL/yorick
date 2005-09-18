/*
 * $Id: dbdsqr.c,v 1.1 2005-09-18 22:04:39 dhmunro Exp $
 * LAPACK matrix solver using SVD.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "dg.h"


/*---blas routines---*/
/* dscal dswap drot */



/*---converted nutty string switches to single characters (lower case)---*/
#define lsame(x,y) ((x)==(y))



/*-----Fortran intrinsics converted-----*/
extern double pow(double,double);  /* used only to take 1/8th power */
#define abs(x) ((x)>=0?(x):-(x))
extern double sqrt(double);
#define dble(x) ((double)x)
#define sign(x,y) ((((x)<0)!=((y)<0))?-(x):(x))
#define min(x,y) ((x)<(y)?(x):(y))
#define max(x,y) ((x)>(y)?(x):(y))
/*-----end of Fortran intrinsics-----*/


void dbdsqr( char uplo, long n, long ncvt, long nru, long ncc,
            double d[], double e[], double vt[], long ldvt,
            double u[],long ldu, double c[], long ldc,
            double work[], long *info )
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
#undef vt_2
#define vt_2(a1,a2) vt[a1-1+ldvt*(a2-1)]
#undef u_2
#define u_2(a1,a2) u[a1-1+ldu*(a2-1)]
#undef e_1
#define e_1(a1) e[a1-1]
#undef d_1
#define d_1(a1) d[a1-1]
#undef c_2
#define c_2(a1,a2) c[a1-1+ldc*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DBDSQR computes the singular value decomposition (SVD) of a real
   *  N-by-N (upper or lower) bidiagonal matrix B:  B = Q * S * P' (P'
   *  denotes the transpose of P), where S is a diagonal matrix with
   *  non-negative diagonal elements (the singular values of B), and Q
   *  and P are orthogonal matrices.
   *
   *  The routine computes S, and optionally computes U * Q, P' * VT,
   *  or Q' * C, for given real input matrices U, VT, and C.
   *
   *  See "Computing  Small Singular Values of Bidiagonal Matrices With
   *  Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
   *  LAPACK Working Note #3, for a detailed description of the algorithm.
   *
   *  Arguments
   *  =========
   *
   *  UPLO    (input) CHARACTER*1
   *          = 'U':  B is upper bidiagonal;
   *          = 'L':  B is lower bidiagonal.
   *
   *  N       (input) INTEGER
   *          The order of the matrix B.  N >= 0.
   *
   *  NCVT    (input) INTEGER
   *          The number of columns of the matrix VT. NCVT >= 0.
   *
   *  NRU     (input) INTEGER
   *          The number of rows of the matrix U. NRU >= 0.
   *
   *  NCC     (input) INTEGER
   *          The number of columns of the matrix C. NCC >= 0.
   *
   *  D       (input/output) DOUBLE PRECISION array, dimension (N)
   *          On entry, the n diagonal elements of the bidiagonal matrix B.
   *          On exit, if INFO=0, the singular values of B in decreasing
   *          order.
   *
   *  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
   *          On entry, the (n-1) off-diagonal elements of the bidiagonal
   *          matrix B.
   *          On normal exit, E is destroyed.
   *
   *  VT      (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
   *          On entry, an N-by-NCVT matrix VT.
   *          On exit, VT is overwritten by P' * VT.
   *          VT is not referenced if NCVT = 0.
   *
   *  LDVT    (input) INTEGER
   *          The leading dimension of the array VT.
   *          LDVT >= max(1,N) if NCVT > 0; LDVT >= 1 if NCVT = 0.
   *
   *  U       (input/output) DOUBLE PRECISION array, dimension (LDU, N)
   *          On entry, an NRU-by-N matrix U.
   *          On exit, U is overwritten by U * Q.
   *          U is not referenced if NRU = 0.
   *
   *  LDU     (input) INTEGER
   *          The leading dimension of the array U.  LDU >= max(1,NRU).
   *
   *  C       (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
   *          On entry, an N-by-NCC matrix C.
   *          On exit, C is overwritten by Q' * C.
   *          C is not referenced if NCC = 0.
   *
   *  LDC     (input) INTEGER
   *          The leading dimension of the array C.
   *          LDC >= max(1,N) if NCC > 0; LDC >=1 if NCC = 0.
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension
   *                      (MAX( 1, 4*N-4 ))
   *          WORK is not referenced if NCVT = NRU = NCC = 0.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  If INFO = -i, the i-th argument had an illegal value
   *          > 0:  the algorithm did not converge; D and E contain the
   *                elements of a bidiagonal matrix which is orthogonally
   *                similar to the input matrix B;  if INFO = i, i
   *                elements of E have not converged to zero.
   *
   *  Internal Parameters
   *  ===================
   *
   *  TOLMUL  DOUBLE PRECISION, default = max(10,min(100,EPS**(-1/8)))
   *          TOLMUL controls the convergence criterion of the QR loop.
   *          If it is positive, TOLMUL*EPS is the desired relative
   *             precision in the computed singular values.
   *          If it is negative, abs(TOLMUL*EPS*sigma_max) is the
   *             desired absolute accuracy in the computed singular
   *             values (corresponds to relative accuracy
   *             abs(TOLMUL*EPS) in the largest singular value.
   *          abs(TOLMUL) should be between 1 and 1/EPS, and preferably
   *             between 10 (for fast convergence) and .1/EPS
   *             (for there to be some accuracy in the results).
   *          Default is to lose at either one eighth or 2 of the
   *             available decimal digits in each computed singular value
   *             (whichever is smaller).
   *
   *  MAXITR  INTEGER, default = 6
   *          MAXITR controls the maximum number of passes of the
   *          algorithm through its inner loop. The algorithms stops
   *          (and so fails to converge) if the number of passes
   *          through the inner loop exceeds MAXITR*N**2.
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e0
#undef one
#define one 1.0e0
#undef negone
#define negone -1.0e0
#undef hndrth
#define hndrth 0.01e0
#undef ten
#define ten 10.0e0
#undef hndrd
#define hndrd 100.0e0
#undef meigth
#define meigth -0.125e0
#undef maxitr
#define maxitr 6
  /**     ..
   *     .. Local Scalars ..*/
  int            rotate;
  long     i, idir=0, irot, isub, iter, iuplo, j, job, ll=0,
           lll, m, maxit, nm1, nm12, nm13, oldll, oldm;
  double    abse, abss, cosl, cosr, cs, eps, f, g, gap,
            gmax, h, mu, oldcs, oldsn, r, shift, sigmn,
            sigmx, sinl, sinr, sll, smax, smin, sminl,
            sminlo=0.0, sminoa=0.0, sn, thresh, tol, tolmul, unfl;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, dble, max, min, sign, sqrt;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input parameters.
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  *info = 0;
  iuplo = 0;
  if( lsame( uplo, 'u' ) )
    iuplo = 1;
  if( lsame( uplo, 'l' ) )
    iuplo = 2;
  if( iuplo==0 ) {
    *info = -1;
  } else if( n<0 ) {
    *info = -2;
  } else if( ncvt<0 ) {
    *info = -3;
  } else if( nru<0 ) {
    *info = -4;
  } else if( ncc<0 ) {
    *info = -5;
  } else if( ( ncvt==0 && ldvt<1 ) ||
            ( ncvt>0 && ldvt<max( 1, n ) ) ) {
    *info = -9;
  } else if( ldu<max( 1, nru ) ) {
    *info = -11;
  } else if( ( ncc==0 && ldc<1 ) ||
            ( ncc>0 && ldc<max( 1, n ) ) ) {
    *info = -13;
  }
  if( *info!=0 ) {
    xerbla( "dbdsqr", -*info );
    return;
  }
  if( n==0 )
    return;
  if( n==1 )
    goto L_190;
  /**
   *     ROTATE is true if any singular vectors desired, false otherwise
   **/
  rotate = ( ncvt>0 ) || ( nru>0 ) || ( ncc>0 );
  nm1 = n - 1;
  nm12 = nm1 + nm1;
  nm13 = nm12 + nm1;
  /**
   *     Get machine constants
   **/
  eps = dlamch( 'e'/*psilon*/ );
  unfl = dlamch( 's'/*afe minimum*/ );
  tolmul = max( ten, min( hndrd, pow(eps,meigth) ) );
  tol = tolmul*eps;
  /**
   *     If matrix lower bidiagonal, rotate to be upper bidiagonal
   *     by applying Givens rotations on the left
   **/
  if( iuplo==2 ) {
    for (i=1 ; i<=n - 1 ; i+=1) {
      dlartg( d_1( i ), e_1( i ), &cs, &sn, &r );
      d_1( i ) = r;
      e_1( i ) = sn*d_1( i+1 );
      d_1( i+1 ) = cs*d_1( i+1 );
      if( rotate ) {
        work_1( i ) = cs;
        work_1( nm1+i ) = sn;
      }
    }
    /**
     *        Update singular vectors if desired
     **/
    if( nru>0 )
      dlasr( 'r', 'v', 'f', nru, n, &work_1( 1 ), &work_1( n ), u,
            ldu );
    if( ncc>0 )
      dlasr( 'l', 'v', 'f', n, ncc, &work_1( 1 ), &work_1( n ), c,
            ldc );
  }
  /**
   *     Compute approximate maximum, minimum singular values
   **/
  smax = abs( d_1( n ) );
  for (i=1 ; i<=n - 1 ; i+=1) {
    smax = max( smax, max(abs( d_1( i ) ), abs( e_1( i ) )) );
  }
  sminl = zero;
  if( tol>=zero ) {
    sminoa = abs( d_1( 1 ) );
    if( sminoa==zero )
      goto L_40;
    mu = sminoa;
    for (i=2 ; i<=n ; i+=1) {
      mu = abs( d_1( i ) )*( mu / ( mu+abs( e_1( i-1 ) ) ) );
      sminoa = min( sminoa, mu );
      if( sminoa==zero )
        goto L_40;
    }
  L_40:
    sminoa = sminoa / sqrt( dble( n ) );
  }
  /**
   *     Prepare for main iteration loop for the singular values
   **/
  maxit = maxitr*n*n;
  iter = 0;
  oldll = -1;
  oldm = -1;
  if( ncc==0 && nru==0 && ncvt==0 ) {
    /**
     *        No singular vectors desired
     **/
    job = 0;
  } else {
    /**
     *        Singular vectors desired
     **/
    job = 1;
  }
  if( tol>=zero ) {
    /**
     *        Relative accuracy desired
     **/
    thresh = max( tol*sminoa, maxit*unfl );
  } else {
    /**
     *        Absolute accuracy desired
     **/
    thresh = max( abs( tol )*smax, maxit*unfl );
  }
  /**
   *     M points to last entry of unconverged part of matrix
   **/
  m = n;
  /**
   *     Begin main iteration loop
   **/
 L_50:
  /**
   *     Check for convergence or exceeding iteration count
   **/
  if( m<=1 )
    goto L_190;
  if( iter>maxit )
    goto L_230;
  /**
   *     Find diagonal block of matrix to work on
   **/
  if( tol<zero && abs( d_1( m ) )<=thresh )
    d_1( m ) = zero;
  smax = abs( d_1( m ) );
  smin = smax;
  for (lll=1 ; lll<=m ; lll+=1) {
    ll = m - lll;
    if( ll==0 )
      goto L_80;
    abss = abs( d_1( ll ) );
    abse = abs( e_1( ll ) );
    if( tol<zero && abss<=thresh )
      d_1( ll ) = zero;
    if( abse<=thresh )
      goto L_70;
    smin = min( smin, abss );
    smax = max( smax, max(abss, abse) );
  }
 L_70:
  e_1( ll ) = zero;
  /**
   *     Matrix splits since E(LL) = 0
   **/
  if( ll==m-1 ) {
    /**
     *        Convergence of bottom singular value, return to top of loop
     **/
    m = m - 1;
    goto L_50;
  }
 L_80:
  ll = ll + 1;
  /**
   *     E(LL) through E(M-1) are nonzero, E(LL-1) is zero
   **/
  if( ll==m-1 ) {
    /**
     *        2 by 2 block, handle separately
     **/
    dlasv2( d_1( m-1 ), e_1( m-1 ), d_1( m ), &sigmn, &sigmx, &sinr,
           &cosr, &sinl, &cosl );
    d_1( m-1 ) = sigmx;
    e_1( m-1 ) = zero;
    d_1( m ) = sigmn;
    /**
     *        Compute singular vectors, if desired
     **/
    if( ncvt>0 )
      cblas_drot( ncvt, &vt_2( m-1, 1 ), ldvt, &vt_2( m, 1 ), ldvt, cosr,
           sinr );
    if( nru>0 )
      cblas_drot( nru, &u_2( 1, m-1 ), 1, &u_2( 1, m ), 1, cosl, sinl );
    if( ncc>0 )
      cblas_drot( ncc, &c_2( m-1, 1 ), ldc, &c_2( m, 1 ), ldc, cosl,
           sinl );
    m = m - 2;
    goto L_50;
  }
  /**
   *     If working on new submatrix, choose shift direction
   *     (from larger end diagonal entry towards smaller)
   **/
  if( ll>oldm || m<oldll ) {
    if( abs( d_1( ll ) )>=abs( d_1( m ) ) ) {
      /**
       *           Chase bulge from top (big end) to bottom (small end)
       **/
      idir = 1;
    } else {
      /**
       *           Chase bulge from bottom (big end) to top (small end)
       **/
      idir = 2;
    }
  }
  /**
   *     Apply convergence tests
   **/
  if( idir==1 ) {
    /**
     *        Run convergence test in forward direction
     *        First apply standard test to bottom of matrix
     **/
    if( abs( e_1( m-1 ) )<=abs( tol )*abs( d_1( m ) ) ||
       ( tol<zero && abs( e_1( m-1 ) )<=thresh ) ) {
      e_1( m-1 ) = zero;
      goto L_50;
    }

    if( tol>=zero ) {
      /**
       *           If relative accuracy desired,
       *           apply convergence criterion forward
       **/
      mu = abs( d_1( ll ) );
      sminl = mu;
      for (lll=ll ; lll<=m - 1 ; lll+=1) {
        if( abs( e_1( lll ) )<=tol*mu ) {
          e_1( lll ) = zero;
          goto L_50;
        }
        sminlo = sminl;
        mu = abs( d_1( lll+1 ) )*( mu / ( mu+abs( e_1( lll ) ) ) );
        sminl = min( sminl, mu );
      }
      /**
       *           If singular values only wanted, apply gap test to bottom
       *           end of matrix
       **/
      if( job==0 ) {
        gap = sminlo / sqrt( dble( m-ll ) ) - abs( d_1( m ) );
        if( gap>zero ) {
          abss = abs( d_1( m ) );
          abse = abs( e_1( m-1 ) );
          gmax = max( gap, max(abss, abse) );
          if( ( abse / gmax )*( abse / gmax )<=tol*( gap / gmax )*
             ( abss / gmax ) ) {
            e_1( m-1 ) = zero;
            goto L_50;
          }
        }
      }
    }
  } else {
    /**
     *        Run convergence test in backward direction
     *        First apply standard test to top of matrix
     **/
    if( abs( e_1( ll ) )<=abs( tol )*abs( d_1( ll ) ) ||
       ( tol<zero && abs( e_1( ll ) )<=thresh ) ) {
      e_1( ll ) = zero;
      goto L_50;
    }

    if( tol>=zero ) {
      /**
       *           If relative accuracy desired,
       *           apply convergence criterion backward
       **/
      mu = abs( d_1( m ) );
      sminl = mu;
      for (lll=m - 1 ; lll>=ll ; lll+=-1) {
        if( abs( e_1( lll ) )<=tol*mu ) {
          e_1( lll ) = zero;
          goto L_50;
        }
        sminlo = sminl;
        mu = abs( d_1( lll ) )*( mu / ( mu+abs( e_1( lll ) ) ) );
        sminl = min( sminl, mu );
      }
      /**
       *           If singular values only wanted, apply gap test to top
       *           end of matrix
       **/
      if( job==0 ) {
        gap = sminlo / sqrt( dble( m-ll ) ) - abs( d_1( ll ) );
        if( gap>zero ) {
          abss = abs( d_1( ll ) );
          abse = abs( e_1( ll ) );
          gmax = max( gap, max(abss, abse) );
          if( ( abse / gmax )*( abse / gmax )<=tol*( gap / gmax )*
             ( abss / gmax ) ) {
            e_1( ll ) = zero;
            goto L_50;
          }
        }
      }
    }
  }
  oldll = ll;
  oldm = m;
  /**
   *     Compute shift.  First, test if shifting would ruin relative
   *     accuracy, and if so set the shift to zero.
   **/
  if( tol>=zero && n*tol*( sminl / smax )<=
     max( eps, hndrth*tol ) ) {
    /**
     *        Use a zero shift to avoid loss of relative accuracy
     **/
    shift = zero;
  } else {
    /**
     *        Compute the shift from 2-by-2 block at end of matrix
     **/
    if( idir==1 ) {
      sll = abs( d_1( ll ) );
      dlas2( d_1( m-1 ), e_1( m-1 ), d_1( m ), &shift, &r );
    } else {
      sll = abs( d_1( m ) );
      dlas2( d_1( ll ), e_1( ll ), d_1( ll+1 ), &shift, &r );
    }
    /**
     *        Test if shift negligible, and if so set to zero
     **/
    if( sll>zero ) {
      if( ( shift / sll )*( shift / sll )<eps )
        shift = zero;
    }
  }
  /**
   *     Increment iteration count
   **/
  iter = iter + m - ll;
  /**
   *     If SHIFT = 0, do simplified QR iteration
   **/
  if( shift==zero ) {
    if( idir==1 ) {
      /**
       *           Chase bulge from top to bottom
       **/
      cs = one;
      oldcs = one;
      /**
       *           Save cosines and sines if singular vectors desired
       **/
      if( rotate ) {

        dlartg( d_1( ll )*cs, e_1( ll ), &cs, &sn, &r );
        dlartg( oldcs*r, d_1( ll+1 )*sn, &oldcs, &oldsn,
               &d_1( ll ) );
        work_1( 1 ) = cs;
        work_1( 1+nm1 ) = sn;
        work_1( 1+nm12 ) = oldcs;
        work_1( 1+nm13 ) = oldsn;
        irot = 1;
        for (i=ll + 1 ; i<=m - 1 ; i+=1) {
          dlartg( d_1( i )*cs, e_1( i ), &cs, &sn, &r );
          e_1( i-1 ) = oldsn*r;
          dlartg( oldcs*r, d_1( i+1 )*sn, &oldcs, &oldsn,
                 &d_1( i ) );
          irot = irot + 1;
          work_1( irot ) = cs;
          work_1( irot+nm1 ) = sn;
          work_1( irot+nm12 ) = oldcs;
          work_1( irot+nm13 ) = oldsn;
        }
        h = d_1( m )*cs;
        d_1( m ) = h*oldcs;
        e_1( m-1 ) = h*oldsn;
        /**
         *              Update singular vectors
         **/
        if( ncvt>0 )
          dlasr( 'l', 'v', 'f', m-ll+1, ncvt, &work_1( 1 ),
                &work_1( n ), &vt_2( ll, 1 ), ldvt );
        if( nru>0 )
          dlasr( 'r', 'v', 'f', nru, m-ll+1,
                &work_1( nm12+1 ), &work_1( nm13+1 ),
                &u_2( 1, ll ), ldu );
        if( ncc>0 )
          dlasr( 'l', 'v', 'f', m-ll+1, ncc,
                &work_1( nm12+1 ), &work_1( nm13+1 ),
                &c_2( ll, 1 ), ldc );

      } else {

        dlartg( d_1( ll )*cs, e_1( ll ), &cs, &sn, &r );
        dlartg( oldcs*r, d_1( ll+1 )*sn, &oldcs, &oldsn,
               &d_1( ll ) );
        for (i=ll + 1 ; i<=m - 1 ; i+=1) {
          dlartg( d_1( i )*cs, e_1( i ), &cs, &sn, &r );
          e_1( i-1 ) = oldsn*r;
          dlartg( oldcs*r, d_1( i+1 )*sn, &oldcs, &oldsn,
                 &d_1( i ) );
        }
        h = d_1( m )*cs;
        d_1( m ) = h*oldcs;
        e_1( m-1 ) = h*oldsn;

      }
      /**
       *           Test convergence
       **/
      if( abs( e_1( m-1 ) )<=thresh )
        e_1( m-1 ) = zero;

    } else {
      /**
       *           Chase bulge from bottom to top
       **/
      cs = one;
      oldcs = one;
      /**
       *           Save cosines and sines if singular vectors desired
       **/
      if( rotate ) {

        dlartg( d_1( m )*cs, e_1( m-1 ), &cs, &sn, &r );
        dlartg( oldcs*r, d_1( m-1 )*sn, &oldcs, &oldsn, &d_1( m ) );
        work_1( m-ll ) = cs;
        work_1( m-ll+nm1 ) = -sn;
        work_1( m-ll+nm12 ) = oldcs;
        work_1( m-ll+nm13 ) = -oldsn;
        irot = m - ll;
        for (i=m - 1 ; i>=ll + 1 ; i+=-1) {
          dlartg( d_1( i )*cs, e_1( i-1 ), &cs, &sn, &r );
          e_1( i ) = oldsn*r;
          dlartg( oldcs*r, d_1( i-1 )*sn, &oldcs, &oldsn,
                 &d_1( i ) );
          irot = irot - 1;
          work_1( irot ) = cs;
          work_1( irot+nm1 ) = -sn;
          work_1( irot+nm12 ) = oldcs;
          work_1( irot+nm13 ) = -oldsn;
        }
        h = d_1( ll )*cs;
        d_1( ll ) = h*oldcs;
        e_1( ll ) = h*oldsn;
        /**
         *              Update singular vectors
         **/
        if( ncvt>0 )
          dlasr( 'l', 'v', 'b', m-ll+1, ncvt,
                &work_1( nm12+1 ), &work_1( nm13+1 ),
                &vt_2( ll, 1 ), ldvt );
        if( nru>0 )
          dlasr( 'r', 'v', 'b', nru, m-ll+1, &work_1( 1 ),
                &work_1( n ), &u_2( 1, ll ), ldu );
        if( ncc>0 )
          dlasr( 'l', 'v', 'b', m-ll+1, ncc, &work_1( 1 ),
                &work_1( n ), &c_2( ll, 1 ), ldc );

      } else {

        dlartg( d_1( m )*cs, e_1( m-1 ), &cs, &sn, &r );
        dlartg( oldcs*r, d_1( m-1 )*sn, &oldcs, &oldsn, &d_1( m ) );
        for (i=m - 1 ; i>=ll + 1 ; i+=-1) {
          dlartg( d_1( i )*cs, e_1( i-1 ), &cs, &sn, &r );
          e_1( i ) = oldsn*r;
          dlartg( oldcs*r, d_1( i-1 )*sn, &oldcs, &oldsn,
                 &d_1( i ) );
        }
        h = d_1( ll )*cs;
        d_1( ll ) = h*oldcs;
        e_1( ll ) = h*oldsn;

      }
      /**
       *           Test convergence
       **/
      if( abs( e_1( ll ) )<=thresh )
        e_1( ll ) = zero;
    }
  } else {
    /**
     *        Use nonzero shift
     **/
    if( idir==1 ) {
      /**
       *           Chase bulge from top to bottom
       **/
      f = ( abs( d_1( ll ) )-shift )*
        ( sign( one, d_1( ll ) )+shift / d_1( ll ) );
      g = e_1( ll );
      /**
       *           Save cosines and sines if singular vectors desired
       **/
      if( rotate ) {

        dlartg( f, g, &cosr, &sinr, &r );
        f = cosr*d_1( ll ) + sinr*e_1( ll );
        e_1( ll ) = cosr*e_1( ll ) - sinr*d_1( ll );
        g = sinr*d_1( ll+1 );
        d_1( ll+1 ) = cosr*d_1( ll+1 );
        dlartg( f, g, &cosl, &sinl, &r );
        d_1( ll ) = r;
        f = cosl*e_1( ll ) + sinl*d_1( ll+1 );
        d_1( ll+1 ) = cosl*d_1( ll+1 ) - sinl*e_1( ll );
        g = sinl*e_1( ll+1 );
        e_1( ll+1 ) = cosl*e_1( ll+1 );
        work_1( 1 ) = cosr;
        work_1( 1+nm1 ) = sinr;
        work_1( 1+nm12 ) = cosl;
        work_1( 1+nm13 ) = sinl;
        irot = 1;
        for (i=ll + 1 ; i<=m - 2 ; i+=1) {
          dlartg( f, g, &cosr, &sinr, &r );
          e_1( i-1 ) = r;
          f = cosr*d_1( i ) + sinr*e_1( i );
          e_1( i ) = cosr*e_1( i ) - sinr*d_1( i );
          g = sinr*d_1( i+1 );
          d_1( i+1 ) = cosr*d_1( i+1 );
          dlartg( f, g, &cosl, &sinl, &r );
          d_1( i ) = r;
          f = cosl*e_1( i ) + sinl*d_1( i+1 );
          d_1( i+1 ) = cosl*d_1( i+1 ) - sinl*e_1( i );
          g = sinl*e_1( i+1 );
          e_1( i+1 ) = cosl*e_1( i+1 );
          irot = irot + 1;
          work_1( irot ) = cosr;
          work_1( irot+nm1 ) = sinr;
          work_1( irot+nm12 ) = cosl;
          work_1( irot+nm13 ) = sinl;
        }
        dlartg( f, g, &cosr, &sinr, &r );
        e_1( m-2 ) = r;
        f = cosr*d_1( m-1 ) + sinr*e_1( m-1 );
        e_1( m-1 ) = cosr*e_1( m-1 ) - sinr*d_1( m-1 );
        g = sinr*d_1( m );
        d_1( m ) = cosr*d_1( m );
        dlartg( f, g, &cosl, &sinl, &r );
        d_1( m-1 ) = r;
        f = cosl*e_1( m-1 ) + sinl*d_1( m );
        d_1( m ) = cosl*d_1( m ) - sinl*e_1( m-1 );
        irot = irot + 1;
        work_1( irot ) = cosr;
        work_1( irot+nm1 ) = sinr;
        work_1( irot+nm12 ) = cosl;
        work_1( irot+nm13 ) = sinl;
        e_1( m-1 ) = f;
        /**
         *              Update singular vectors
         **/
        if( ncvt>0 )
          dlasr( 'l', 'v', 'f', m-ll+1, ncvt, &work_1( 1 ),
                &work_1( n ), &vt_2( ll, 1 ), ldvt );
        if( nru>0 )
          dlasr( 'r', 'v', 'f', nru, m-ll+1,
                &work_1( nm12+1 ), &work_1( nm13+1 ),
                &u_2( 1, ll ), ldu );
        if( ncc>0 )
          dlasr( 'l', 'v', 'f', m-ll+1, ncc,
                &work_1( nm12+1 ), &work_1( nm13+1 ),
                &c_2( ll, 1 ), ldc );

      } else {

        dlartg( f, g, &cosr, &sinr, &r );
        f = cosr*d_1( ll ) + sinr*e_1( ll );
        e_1( ll ) = cosr*e_1( ll ) - sinr*d_1( ll );
        g = sinr*d_1( ll+1 );
        d_1( ll+1 ) = cosr*d_1( ll+1 );
        dlartg( f, g, &cosl, &sinl, &r );
        d_1( ll ) = r;
        f = cosl*e_1( ll ) + sinl*d_1( ll+1 );
        d_1( ll+1 ) = cosl*d_1( ll+1 ) - sinl*e_1( ll );
        g = sinl*e_1( ll+1 );
        e_1( ll+1 ) = cosl*e_1( ll+1 );
        for (i=ll + 1 ; i<=m - 2 ; i+=1) {
          dlartg( f, g, &cosr, &sinr, &r );
          e_1( i-1 ) = r;
          f = cosr*d_1( i ) + sinr*e_1( i );
          e_1( i ) = cosr*e_1( i ) - sinr*d_1( i );
          g = sinr*d_1( i+1 );
          d_1( i+1 ) = cosr*d_1( i+1 );
          dlartg( f, g, &cosl, &sinl, &r );
          d_1( i ) = r;
          f = cosl*e_1( i ) + sinl*d_1( i+1 );
          d_1( i+1 ) = cosl*d_1( i+1 ) - sinl*e_1( i );
          g = sinl*e_1( i+1 );
          e_1( i+1 ) = cosl*e_1( i+1 );
        }
        dlartg( f, g, &cosr, &sinr, &r );
        e_1( m-2 ) = r;
        f = cosr*d_1( m-1 ) + sinr*e_1( m-1 );
        e_1( m-1 ) = cosr*e_1( m-1 ) - sinr*d_1( m-1 );
        g = sinr*d_1( m );
        d_1( m ) = cosr*d_1( m );
        dlartg( f, g, &cosl, &sinl, &r );
        d_1( m-1 ) = r;
        f = cosl*e_1( m-1 ) + sinl*d_1( m );
        d_1( m ) = cosl*d_1( m ) - sinl*e_1( m-1 );
        e_1( m-1 ) = f;

      }
      /**
       *           Test convergence
       **/
      if( abs( e_1( m-1 ) )<=thresh )
        e_1( m-1 ) = zero;

    } else {
      /**
       *           Chase bulge from bottom to top
       **/
      f = ( abs( d_1( m ) )-shift )*( sign( one, d_1( m ) )+shift /
                                     d_1( m ) );
      g = e_1( m-1 );
      /**
       *           Save cosines and sines if singular vectors desired
       **/
      if( rotate ) {

        dlartg( f, g, &cosr, &sinr, &r );
        f = cosr*d_1( m ) + sinr*e_1( m-1 );
        e_1( m-1 ) = cosr*e_1( m-1 ) - sinr*d_1( m );
        g = sinr*d_1( m-1 );
        d_1( m-1 ) = cosr*d_1( m-1 );
        dlartg( f, g, &cosl, &sinl, &r );
        d_1( m ) = r;
        f = cosl*e_1( m-1 ) + sinl*d_1( m-1 );
        d_1( m-1 ) = cosl*d_1( m-1 ) - sinl*e_1( m-1 );
        g = sinl*e_1( m-2 );
        e_1( m-2 ) = cosl*e_1( m-2 );
        work_1( m-ll ) = cosr;
        work_1( m-ll+nm1 ) = -sinr;
        work_1( m-ll+nm12 ) = cosl;
        work_1( m-ll+nm13 ) = -sinl;
        irot = m - ll;
        for (i=m - 1 ; i>=ll + 2 ; i+=-1) {
          dlartg( f, g, &cosr, &sinr, &r );
          e_1( i ) = r;
          f = cosr*d_1( i ) + sinr*e_1( i-1 );
          e_1( i-1 ) = cosr*e_1( i-1 ) - sinr*d_1( i );
          g = sinr*d_1( i-1 );
          d_1( i-1 ) = cosr*d_1( i-1 );
          dlartg( f, g, &cosl, &sinl, &r );
          d_1( i ) = r;
          f = cosl*e_1( i-1 ) + sinl*d_1( i-1 );
          d_1( i-1 ) = cosl*d_1( i-1 ) - sinl*e_1( i-1 );
          g = sinl*e_1( i-2 );
          e_1( i-2 ) = cosl*e_1( i-2 );
          irot = irot - 1;
          work_1( irot ) = cosr;
          work_1( irot+nm1 ) = -sinr;
          work_1( irot+nm12 ) = cosl;
          work_1( irot+nm13 ) = -sinl;
        }
        dlartg( f, g, &cosr, &sinr, &r );
        e_1( ll+1 ) = r;
        f = cosr*d_1( ll+1 ) + sinr*e_1( ll );
        e_1( ll ) = cosr*e_1( ll ) - sinr*d_1( ll+1 );
        g = sinr*d_1( ll );
        d_1( ll ) = cosr*d_1( ll );
        dlartg( f, g, &cosl, &sinl, &r );
        d_1( ll+1 ) = r;
        f = cosl*e_1( ll ) + sinl*d_1( ll );
        d_1( ll ) = cosl*d_1( ll ) - sinl*e_1( ll );
        irot = irot - 1;
        work_1( irot ) = cosr;
        work_1( irot+nm1 ) = -sinr;
        work_1( irot+nm12 ) = cosl;
        work_1( irot+nm13 ) = -sinl;
        e_1( ll ) = f;

      } else {

        dlartg( f, g, &cosr, &sinr, &r );
        f = cosr*d_1( m ) + sinr*e_1( m-1 );
        e_1( m-1 ) = cosr*e_1( m-1 ) - sinr*d_1( m );
        g = sinr*d_1( m-1 );
        d_1( m-1 ) = cosr*d_1( m-1 );
        dlartg( f, g, &cosl, &sinl, &r );
        d_1( m ) = r;
        f = cosl*e_1( m-1 ) + sinl*d_1( m-1 );
        d_1( m-1 ) = cosl*d_1( m-1 ) - sinl*e_1( m-1 );
        g = sinl*e_1( m-2 );
        e_1( m-2 ) = cosl*e_1( m-2 );
        for (i=m - 1 ; i>=ll + 2 ; i+=-1) {
          dlartg( f, g, &cosr, &sinr, &r );
          e_1( i ) = r;
          f = cosr*d_1( i ) + sinr*e_1( i-1 );
          e_1( i-1 ) = cosr*e_1( i-1 ) - sinr*d_1( i );
          g = sinr*d_1( i-1 );
          d_1( i-1 ) = cosr*d_1( i-1 );
          dlartg( f, g, &cosl, &sinl, &r );
          d_1( i ) = r;
          f = cosl*e_1( i-1 ) + sinl*d_1( i-1 );
          d_1( i-1 ) = cosl*d_1( i-1 ) - sinl*e_1( i-1 );
          g = sinl*e_1( i-2 );
          e_1( i-2 ) = cosl*e_1( i-2 );
        }
        dlartg( f, g, &cosr, &sinr, &r );
        e_1( ll+1 ) = r;
        f = cosr*d_1( ll+1 ) + sinr*e_1( ll );
        e_1( ll ) = cosr*e_1( ll ) - sinr*d_1( ll+1 );
        g = sinr*d_1( ll );
        d_1( ll ) = cosr*d_1( ll );
        dlartg( f, g, &cosl, &sinl, &r );
        d_1( ll+1 ) = r;
        f = cosl*e_1( ll ) + sinl*d_1( ll );
        d_1( ll ) = cosl*d_1( ll ) - sinl*e_1( ll );
        e_1( ll ) = f;

      }
      /**
       *           Test convergence
       **/
      if( abs( e_1( ll ) )<=thresh )
        e_1( ll ) = zero;
      /**
       *           Update singular vectors if desired
       **/
      if( ncvt>0 )
        dlasr( 'l', 'v', 'b', m-ll+1, ncvt, &work_1( nm12+1 ),
              &work_1( nm13+1 ), &vt_2( ll, 1 ), ldvt );
      if( nru>0 )
        dlasr( 'r', 'v', 'b', nru, m-ll+1, &work_1( 1 ),
              &work_1( n ), &u_2( 1, ll ), ldu );
      if( ncc>0 )
        dlasr( 'l', 'v', 'b', m-ll+1, ncc, &work_1( 1 ),
              &work_1( n ), &c_2( ll, 1 ), ldc );
    }
  }
  /**
   *     QR iteration finished, go back and check convergence
   **/
  goto L_50;
  /**
   *     All singular values converged, so make them positive
   **/
 L_190:
  for (i=1 ; i<=n ; i+=1) {
    if( d_1( i )<zero ) {
      d_1( i ) = -d_1( i );
      /**
       *           Change sign of singular vectors, if desired
       **/
      if( ncvt>0 )
        cblas_dscal( ncvt, negone, &vt_2( i, 1 ), ldvt );
    }
  }
  /**
   *     Sort the singular values into decreasing order (insertion sort on
   *     singular values, but only one transposition per singular vector)
   **/
  for (i=1 ; i<=n - 1 ; i+=1) {
    /**
     *        Scan for smallest D(I)
     **/
    isub = 1;
    smin = d_1( 1 );
    for (j=2 ; j<=n + 1 - i ; j+=1) {
      if( d_1( j )<=smin ) {
        isub = j;
        smin = d_1( j );
      }
    }
    if( isub!=n+1-i ) {
      /**
       *           Swap singular values and vectors
       **/
      d_1( isub ) = d_1( n+1-i );
      d_1( n+1-i ) = smin;
      if( ncvt>0 )
        cblas_dswap( ncvt, &vt_2( isub, 1 ), ldvt, &vt_2( n+1-i, 1 ),
              ldvt );
      if( nru>0 )
        cblas_dswap( nru, &u_2( 1, isub ), 1, &u_2( 1, n+1-i ), 1 );
      if( ncc>0 )
        cblas_dswap( ncc, &c_2( isub, 1 ), ldc, &c_2( n+1-i, 1 ), ldc );
    }
  }
  goto L_250;
  /**
   *     Maximum number of iterations exceeded, failure to converge
   **/
 L_230:
  *info = 0;
  for (i=1 ; i<=n - 1 ; i+=1) {
    if( e_1( i )!=zero )
      *info = *info + 1;
  }
 L_250:
  return;
  /**
   *     End of DBDSQR
   **/
}
