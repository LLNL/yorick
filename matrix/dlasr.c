/*
 * $Id: dlasr.c,v 1.1 2005-09-18 22:04:50 dhmunro Exp $
 * LAPACK matrix solver using SVD (split from dgelss.c).
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "dg.h"


/*---blas routines---*/
/* dscal dgemv dgemm */

/*---converted nutty string switches to single characters (lower case)---*/
#define lsame(x,y) ((x)==(y))

/*-----Fortran intrinsics converted-----*/
#define abs(x) ((x)>=0?(x):-(x))
extern double sqrt(double);
#define sign(x,y) ((((x)<0)!=((y)<0))?-(x):(x))
#define min(x,y) ((x)<(y)?(x):(y))
#define max(x,y) ((x)>(y)?(x):(y))
/*-----end of Fortran intrinsics-----*/



void dlasr( char side, char pivot, char direct, long m, long n,
           double c[], double s[], double a[], long lda )
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
#undef s_1
#define s_1(a1) s[a1-1]
#undef c_1
#define c_1(a1) c[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLASR   performs the transformation
   *
   *     A := P*A,   when SIDE = 'L' or 'l'  (  Left-hand side )
   *
   *     A := A*P',  when SIDE = 'R' or 'r'  ( Right-hand side )
   *
   *  where A is an m by n real matrix and P is an orthogonal matrix,
   *  consisting of a sequence of plane rotations determined by the
   *  parameters PIVOT and DIRECT as follows ( z = m when SIDE = 'L' or 'l'
   *  and z = n when SIDE = 'R' or 'r' ):
   *
   *  When  DIRECT = 'F' or 'f'  ( Forward sequence ) then
   *
   *     P = P( z - 1 )*...*P( 2 )*P( 1 ),
   *
   *  and when DIRECT = 'B' or 'b'  ( Backward sequence ) then
   *
   *     P = P( 1 )*P( 2 )*...*P( z - 1 ),
   *
   *  where  P( k ) is a plane rotation matrix for the following planes:
   *
   *     when  PIVOT = 'V' or 'v'  ( Variable pivot ),
   *        the plane ( k, k + 1 )
   *
   *     when  PIVOT = 'T' or 't'  ( Top pivot ),
   *        the plane ( 1, k + 1 )
   *
   *     when  PIVOT = 'B' or 'b'  ( Bottom pivot ),
   *        the plane ( k, z )
   *
   *  c_1( k ) and s_1( k )  must contain the  cosine and sine that define the
   *  matrix  P( k ).  The two by two plane rotation part of the matrix
   *  P( k ), R( k ), is assumed to be of the form
   *
   *     R( k ) = (  c_1( k )  s_1( k ) ).
   *              ( -s_1( k )  c_1( k ) )
   *
   *  This version vectorises across rows of the array A when SIDE = 'L'.
   *
   *  Arguments
   *  =========
   *
   *  SIDE    (input) CHARACTER*1
   *          Specifies whether the plane rotation matrix P is applied to
   *          A on the left or the right.
   *          = 'L':  Left, compute A := P*A
   *          = 'R':  Right, compute A:= A*P'
   *
   *  DIRECT  (input) CHARACTER*1
   *          Specifies whether P is a forward or backward sequence of
   *          plane rotations.
   *          = 'F':  Forward, P = P( z - 1 )*...*P( 2 )*P( 1 )
   *          = 'B':  Backward, P = P( 1 )*P( 2 )*...*P( z - 1 )
   *
   *  PIVOT   (input) CHARACTER*1
   *          Specifies the plane for which P(k) is a plane rotation
   *          matrix.
   *          = 'V':  Variable pivot, the plane (k,k+1)
   *          = 'T':  Top pivot, the plane (1,k+1)
   *          = 'B':  Bottom pivot, the plane (k,z)
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix A.  If m <= 1, an immediate
   *          return is effected.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix A.  If n <= 1, an
   *          immediate return is effected.
   *
   *  C, S    (input) DOUBLE PRECISION arrays, dimension
   *                  (M-1) if SIDE = 'L'
   *                  (N-1) if SIDE = 'R'
   *          c_1(k) and s_1(k) contain the cosine and sine that define the
   *          matrix P(k).  The two by two plane rotation part of the
   *          matrix P(k), R(k), is assumed to be of the form
   *          R( k ) = (  c_1( k )  s_1( k ) ).
   *                   ( -s_1( k )  c_1( k ) )
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          The m by n matrix A.  On exit, A is overwritten by P*A if
   *          SIDE = 'R' or by A*P' if SIDE = 'L'.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
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
  long            i, info, j;
  double    ctemp, stemp, temp;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input parameters
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  info = 0;
  if( !( lsame( side, 'l' ) || lsame( side, 'r' ) ) ) {
    info = 1;
  } else if( !( lsame( pivot, 'v' ) || lsame( pivot, 't' ) ||
               lsame( pivot, 'b' ) ) ) {
    info = 2;
  } else if( !( lsame( direct, 'f' ) || lsame( direct, 'b' ) ) )
    {
      info = 3;
    } else if( m<0 ) {
      info = 4;
    } else if( n<0 ) {
      info = 5;
    } else if( lda<max( 1, m ) ) {
      info = 9;
    }
  if( info!=0 ) {
    xerbla( "dlasr ", info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( ( m==0 ) || ( n==0 ) )
    return;
  if( lsame( side, 'l' ) ) {
    /**
     *        Form  P * A
     **/
    if( lsame( pivot, 'v' ) ) {
      if( lsame( direct, 'f' ) ) {
        for (j=1 ; j<=m - 1 ; j+=1) {
          ctemp = c_1( j );
          stemp = s_1( j );
          if( ( ctemp!=one ) || ( stemp!=zero ) ) {
            for (i=1 ; i<=n ; i+=1) {
              temp = a_2( j+1, i );
              a_2( j+1, i ) = ctemp*temp - stemp*a_2( j, i );
              a_2( j, i ) = stemp*temp + ctemp*a_2( j, i );
            }
          }
        }
      } else if( lsame( direct, 'b' ) ) {
        for (j=m - 1 ; j>=1 ; j+=-1) {
          ctemp = c_1( j );
          stemp = s_1( j );
          if( ( ctemp!=one ) || ( stemp!=zero ) ) {
            for (i=1 ; i<=n ; i+=1) {
              temp = a_2( j+1, i );
              a_2( j+1, i ) = ctemp*temp - stemp*a_2( j, i );
              a_2( j, i ) = stemp*temp + ctemp*a_2( j, i );
            }
          }
        }
      }
    } else if( lsame( pivot, 't' ) ) {
      if( lsame( direct, 'f' ) ) {
        for (j=2 ; j<=m ; j+=1) {
          ctemp = c_1( j-1 );
          stemp = s_1( j-1 );
          if( ( ctemp!=one ) || ( stemp!=zero ) ) {
            for (i=1 ; i<=n ; i+=1) {
              temp = a_2( j, i );
              a_2( j, i ) = ctemp*temp - stemp*a_2( 1, i );
              a_2( 1, i ) = stemp*temp + ctemp*a_2( 1, i );
            }
          }
        }
      } else if( lsame( direct, 'b' ) ) {
        for (j=m ; j>=2 ; j+=-1) {
          ctemp = c_1( j-1 );
          stemp = s_1( j-1 );
          if( ( ctemp!=one ) || ( stemp!=zero ) ) {
            for (i=1 ; i<=n ; i+=1) {
              temp = a_2( j, i );
              a_2( j, i ) = ctemp*temp - stemp*a_2( 1, i );
              a_2( 1, i ) = stemp*temp + ctemp*a_2( 1, i );
            }
          }
        }
      }
    } else if( lsame( pivot, 'b' ) ) {
      if( lsame( direct, 'f' ) ) {
        for (j=1 ; j<=m - 1 ; j+=1) {
          ctemp = c_1( j );
          stemp = s_1( j );
          if( ( ctemp!=one ) || ( stemp!=zero ) ) {
            for (i=1 ; i<=n ; i+=1) {
              temp = a_2( j, i );
              a_2( j, i ) = stemp*a_2( m, i ) + ctemp*temp;
              a_2( m, i ) = ctemp*a_2( m, i ) - stemp*temp;
            }
          }
        }
      } else if( lsame( direct, 'b' ) ) {
        for (j=m - 1 ; j>=1 ; j+=-1) {
          ctemp = c_1( j );
          stemp = s_1( j );
          if( ( ctemp!=one ) || ( stemp!=zero ) ) {
            for (i=1 ; i<=n ; i+=1) {
              temp = a_2( j, i );
              a_2( j, i ) = stemp*a_2( m, i ) + ctemp*temp;
              a_2( m, i ) = ctemp*a_2( m, i ) - stemp*temp;
            }
          }
        }
      }
    }
  } else if( lsame( side, 'r' ) ) {
    /**
     *        Form A * P'
     **/
    if( lsame( pivot, 'v' ) ) {
      if( lsame( direct, 'f' ) ) {
        for (j=1 ; j<=n - 1 ; j+=1) {
          ctemp = c_1( j );
          stemp = s_1( j );
          if( ( ctemp!=one ) || ( stemp!=zero ) ) {
            for (i=1 ; i<=m ; i+=1) {
              temp = a_2( i, j+1 );
              a_2( i, j+1 ) = ctemp*temp - stemp*a_2( i, j );
              a_2( i, j ) = stemp*temp + ctemp*a_2( i, j );
            }
          }
        }
      } else if( lsame( direct, 'b' ) ) {
        for (j=n - 1 ; j>=1 ; j+=-1) {
          ctemp = c_1( j );
          stemp = s_1( j );
          if( ( ctemp!=one ) || ( stemp!=zero ) ) {
            for (i=1 ; i<=m ; i+=1) {
              temp = a_2( i, j+1 );
              a_2( i, j+1 ) = ctemp*temp - stemp*a_2( i, j );
              a_2( i, j ) = stemp*temp + ctemp*a_2( i, j );
            }
          }
        }
      }
    } else if( lsame( pivot, 't' ) ) {
      if( lsame( direct, 'f' ) ) {
        for (j=2 ; j<=n ; j+=1) {
          ctemp = c_1( j-1 );
          stemp = s_1( j-1 );
          if( ( ctemp!=one ) || ( stemp!=zero ) ) {
            for (i=1 ; i<=m ; i+=1) {
              temp = a_2( i, j );
              a_2( i, j ) = ctemp*temp - stemp*a_2( i, 1 );
              a_2( i, 1 ) = stemp*temp + ctemp*a_2( i, 1 );
            }
          }
        }
      } else if( lsame( direct, 'b' ) ) {
        for (j=n ; j>=2 ; j+=-1) {
          ctemp = c_1( j-1 );
          stemp = s_1( j-1 );
          if( ( ctemp!=one ) || ( stemp!=zero ) ) {
            for (i=1 ; i<=m ; i+=1) {
              temp = a_2( i, j );
              a_2( i, j ) = ctemp*temp - stemp*a_2( i, 1 );
              a_2( i, 1 ) = stemp*temp + ctemp*a_2( i, 1 );
            }
          }
        }
      }
    } else if( lsame( pivot, 'b' ) ) {
      if( lsame( direct, 'f' ) ) {
        for (j=1 ; j<=n - 1 ; j+=1) {
          ctemp = c_1( j );
          stemp = s_1( j );
          if( ( ctemp!=one ) || ( stemp!=zero ) ) {
            for (i=1 ; i<=m ; i+=1) {
              temp = a_2( i, j );
              a_2( i, j ) = stemp*a_2( i, n ) + ctemp*temp;
              a_2( i, n ) = ctemp*a_2( i, n ) - stemp*temp;
            }
          }
        }
      } else if( lsame( direct, 'b' ) ) {
        for (j=n - 1 ; j>=1 ; j+=-1) {
          ctemp = c_1( j );
          stemp = s_1( j );
          if( ( ctemp!=one ) || ( stemp!=zero ) ) {
            for (i=1 ; i<=m ; i+=1) {
              temp = a_2( i, j );
              a_2( i, j ) = stemp*a_2( i, n ) + ctemp*temp;
              a_2( i, n ) = ctemp*a_2( i, n ) - stemp*temp;
            }
          }
        }
      }
    }
  }

  return;
  /**
   *     End of DLASR
   **/
}



void dlas2( double f, double g, double h, double *ssmin, double *ssmax )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     March 31, 1993
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLAS2  computes the singular values of the 2-by-2 matrix
   *     [  F   G  ]
   *     [  0   H  ].
   *  On return, SSMIN is the smaller singular value and SSMAX is the
   *  larger singular value.
   *
   *  Arguments
   *  =========
   *
   *  F       (input) DOUBLE PRECISION
   *          The (1,1) entry of the 2-by-2 matrix.
   *
   *  G       (input) DOUBLE PRECISION
   *          The (1,2) entry of the 2-by-2 matrix.
   *
   *  H       (input) DOUBLE PRECISION
   *          The (2,2) entry of the 2-by-2 matrix.
   *
   *  SSMIN   (output) DOUBLE PRECISION
   *          The smaller singular value.
   *
   *  SSMAX   (output) DOUBLE PRECISION
   *          The larger singular value.
   *
   *  Further Details
   *  ===============
   *
   *  Barring over/underflow, all output quantities are correct to within
   *  a few units in the last place (ulps), even in the absence of a guard
   *  digit in addition/subtraction.
   *
   *  In IEEE arithmetic, the code works correctly if one matrix entry is
   *  infinite.
   *
   *  Overflow will not occur unless the largest singular value itself
   *  overflows, or is within a few ulps of overflow. (On machines with
   *  partial overflow, like the Cray, overflow may occur if the largest
   *  singular value is within a factor of 2 of overflow.)
   *
   *  Underflow is harmless if underflow is gradual. Otherwise, results
   *  may correspond to a matrix modified by perturbations of size near
   *  the underflow threshold.
   *
   *  ====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e0
#undef one
#define one 1.0e0
#undef two
#define two 2.0e0
  /**     ..
   *     .. Local Scalars ..*/
  double    as, at, au, c, fa, fhmn, fhmx, ga, ha;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, max, min, sqrt;*/
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  fa = abs( f );
  ga = abs( g );
  ha = abs( h );
  fhmn = min( fa, ha );
  fhmx = max( fa, ha );
  if( fhmn==zero ) {
    *ssmin = zero;
    if( fhmx==zero ) {
      *ssmax = zero;
    } else {
      double min_max= ( min( fhmx, ga ) / max( fhmx, ga ) );
      *ssmax = max( fhmx, ga )*sqrt( one+min_max*min_max );
    }
  } else {
    if( ga<fhmx ) {
      as = one + fhmn / fhmx;
      at = ( fhmx-fhmn ) / fhmx;
      au = ( ga / fhmx )*( ga / fhmx );
      c = two / ( sqrt( as*as+au )+sqrt( at*at+au ) );
      *ssmin = fhmn*c;
      *ssmax = fhmx / c;
    } else {
      au = fhmx / ga;
      if( au==zero ) {
        /**
         *              Avoid possible harmful underflow if exponent range
         *              asymmetric (true SSMIN may not underflow even if
         *              AU underflows)
         **/
        *ssmin = ( fhmn*fhmx ) / ga;
        *ssmax = ga;
      } else {
        as = one + fhmn / fhmx;
        at = ( fhmx-fhmn ) / fhmx;
        c = one / ( sqrt( one+( as*au )*( as*au ) )+
                   sqrt( one+( at*au )*( at*au ) ) );
        *ssmin = ( fhmn*c )*au;
        *ssmin = *ssmin + *ssmin;
        *ssmax = ga / ( c+c );
      }
    }
  }
  return;
  /**
   *     End of DLAS2
   **/
}



void dlasv2( double f, double g, double h,
            double *ssmin, double *ssmax, double *snr,
            double *csr, double *snl, double *csl )
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
   *  DLASV2 computes the singular value decomposition of a 2-by-2
   *  triangular matrix
   *     [  F   G  ]
   *     [  0   H  ].
   *  On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the
   *  smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and
   *  right singular vectors for abs(SSMAX), giving the decomposition
   *
   *     [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ]
   *     [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ].
   *
   *  Arguments
   *  =========
   *
   *  F       (input) DOUBLE PRECISION
   *          The (1,1) entry of the 2-by-2 matrix.
   *
   *  G       (input) DOUBLE PRECISION
   *          The (1,2) entry of the 2-by-2 matrix.
   *
   *  H       (input) DOUBLE PRECISION
   *          The (2,2) entry of the 2-by-2 matrix.
   *
   *  SSMIN   (output) DOUBLE PRECISION
   *          abs(SSMIN) is the smaller singular value.
   *
   *  SSMAX   (output) DOUBLE PRECISION
   *          abs(SSMAX) is the larger singular value.
   *
   *  SNL     (output) DOUBLE PRECISION
   *  CSL     (output) DOUBLE PRECISION
   *          The vector (CSL, SNL) is a unit left singular vector for the
   *          singular value abs(SSMAX).
   *
   *  SNR     (output) DOUBLE PRECISION
   *  CSR     (output) DOUBLE PRECISION
   *          The vector (CSR, SNR) is a unit right singular vector for the
   *          singular value abs(SSMAX).
   *
   *  Further Details
   *  ===============
   *
   *  Any input parameter may be aliased with any output parameter.
   *
   *  Barring over/underflow and assuming a guard digit in subtraction, all
   *  output quantities are correct to within a few units in the last
   *  place (ulps).
   *
   *  In IEEE arithmetic, the code works correctly if one matrix entry is
   *  infinite.
   *
   *  Overflow will not occur unless the largest singular value itself
   *  overflows or is within a few ulps of overflow. (On machines with
   *  partial overflow, like the Cray, overflow may occur if the largest
   *  singular value is within a factor of 2 of overflow.)
   *
   *  Underflow is harmless if underflow is gradual. Otherwise, results
   *  may correspond to a matrix modified by perturbations of size near
   *  the underflow threshold.
   *
   * =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e0
#undef half
#define half 0.5e0
#undef one
#define one 1.0e0
#undef two
#define two 2.0e0
#undef four
#define four 4.0e0
  /**     ..
   *     .. Local Scalars ..*/
  int            gasmal, swap;
  long            pmax;
  double    a, clt=0.0, crt=0.0, d, fa, ft, ga, gt, ha, ht, l, m,
            mm, r, s, slt=0.0, srt=0.0, t, temp, tsign=0.0, tt;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, sign, sqrt;*/
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  ft = f;
  fa = abs( ft );
  ht = h;
  ha = abs( h );
  /**
   *     PMAX points to the maximum absolute entry of matrix
   *       PMAX = 1 if F largest in absolute values
   *       PMAX = 2 if G largest in absolute values
   *       PMAX = 3 if H largest in absolute values
   **/
  pmax = 1;
  swap = ( ha>fa );
  if( swap ) {
    pmax = 3;
    temp = ft;
    ft = ht;
    ht = temp;
    temp = fa;
    fa = ha;
    ha = temp;
    /**
     *        Now FA .ge. HA
     **/
  }
  gt = g;
  ga = abs( gt );
  if( ga==zero ) {
    /**
     *        Diagonal matrix
     **/
    *ssmin = ha;
    *ssmax = fa;
    clt = one;
    crt = one;
    slt = zero;
    srt = zero;
  } else {
    gasmal = 1;
    if( ga>fa ) {
      pmax = 2;
      if( ( fa / ga )<dlamch( 'e'/*ps*/ ) ) {
        /**
         *              Case of very large GA
         **/
        gasmal = 0;
        *ssmax = ga;
        if( ha>one ) {
          *ssmin = fa / ( ga / ha );
        } else {
          *ssmin = ( fa / ga )*ha;
        }
        clt = one;
        slt = ht / gt;
        srt = one;
        crt = ft / gt;
      }
    }
    if( gasmal ) {
      /**
       *           Normal case
       **/
      d = fa - ha;
      if( d==fa ) {
        /**
         *              Copes with infinite F or H
         **/
        l = one;
      } else {
        l = d / fa;
      }
      /**
       *           Note that 0 .le. L .le. 1
       **/
      m = gt / ft;
      /**
       *           Note that abs(M) .le. 1/macheps
       **/
      t = two - l;
      /**
       *           Note that T .ge. 1
       **/
      mm = m*m;
      tt = t*t;
      s = sqrt( tt+mm );
      /**
       *           Note that 1 .le. S .le. 1 + 1/macheps
       **/
      if( l==zero ) {
        r = abs( m );
      } else {
        r = sqrt( l*l+mm );
      }
      /**
       *           Note that 0 .le. R .le. 1 + 1/macheps
       **/
      a = half*( s+r );
      /**
       *           Note that 1 .le. A .le. 1 + abs(M)
       **/
      *ssmin = ha / a;
      *ssmax = fa*a;
      if( mm==zero ) {
        /**
         *              Note that M is very tiny
         **/
        if( l==zero ) {
          t = sign( two, ft )*sign( one, gt );
        } else {
          t = gt / sign( d, ft ) + m / t;
        }
      } else {
        t = ( m / ( s+t )+m / ( r+l ) )*( one+a );
      }
      l = sqrt( t*t+four );
      crt = two / l;
      srt = t / l;
      clt = ( crt+srt*m ) / a;
      slt = ( ht / ft )*srt / a;
    }
  }
  if( swap ) {
    *csl = srt;
    *snl = crt;
    *csr = slt;
    *snr = clt;
  } else {
    *csl = clt;
    *snl = slt;
    *csr = crt;
    *snr = srt;
  }
  /**
   *     Correct signs of SSMAX and SSMIN
   **/
  if( pmax==1 )
    tsign = sign( one, *csr )*sign( one, *csl )*sign( one, f );
  if( pmax==2 )
    tsign = sign( one, *snr )*sign( one, *csl )*sign( one, g );
  if( pmax==3 )
    tsign = sign( one, *snr )*sign( one, *snl )*sign( one, h );
  *ssmax = sign( *ssmax, tsign );
  *ssmin = sign( *ssmin, tsign*sign( one, f )*sign( one, h ) );
  return;
  /**
   *     End of DLASV2
   **/
}



void dlartg( double f, double g, double *cs, double *sn, double *r )
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
   *  DLARTG generate a plane rotation so that
   *
   *     [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
   *     [ -SN  CS  ]     [ G ]     [ 0 ]
   *
   *  This is a faster version of the BLAS1 routine YDROTG, except for
   *  the following differences:
   *     F and G are unchanged on return.
   *     If G=0, then CS=1 and SN=0.
   *     If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
   *        floating point operations (saves work in DBDSQR when
   *        there are zeros on the diagonal).
   *
   *  Arguments
   *  =========
   *
   *  F       (input) DOUBLE PRECISION
   *          The first component of vector to be rotated.
   *
   *  G       (input) DOUBLE PRECISION
   *          The second component of vector to be rotated.
   *
   *  CS      (output) DOUBLE PRECISION
   *          The cosine of the rotation.
   *
   *  SN      (output) DOUBLE PRECISION
   *          The sine of the rotation.
   *
   *  R       (output) DOUBLE PRECISION
   *          The nonzero component of the rotated vector.
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
  double    t, tt;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          abs, sqrt;*/
  /**     ..
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if( g==zero ) {
    *cs = one;
    *sn = zero;
    *r = f;
  } else if( f==zero ) {
    *cs = zero;
    *sn = one;
    *r = g;
  } else {
    if( abs( f )>abs( g ) ) {
      t = g / f;
      tt = sqrt( one+t*t );
      *cs = one / tt;
      *sn = t*(*cs);
      *r = f*tt;
    } else {
      t = f / g;
      tt = sqrt( one+t*t );
      *sn = one / tt;
      *cs = t*(*sn);
      *r = g*tt;
    }
  }
  return;
  /**
   *     End of DLARTG
   **/
}



void dorgbr( char vect, long m, long n, long k, double a[], long lda,
            double tau[], double work[], long lwork, long *info )
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
   *  DORGBR generates one of the matrices Q or P**T determined by DGEBRD
   *  when reducing a real matrix A to bidiagonal form: A = Q * B * P**T.
   *  Q and P**T are defined as products of elementary reflectors H(i) or
   *  G(i) respectively.
   *
   *  If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
   *  is of order M:
   *  if m >= k, Q = H(1) H(2) . . . H(k) and DORGBR returns the first n
   *  columns of Q, where m >= n >= k;
   *  if m < k, Q = H(1) H(2) . . . H(m-1) and DORGBR returns Q as an
   *  M-by-M matrix.
   *
   *  If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**T
   *  is of order N:
   *  if k < n, P**T = G(k) . . . G(2) G(1) and DORGBR returns the first m
   *  rows of P**T, where n >= m >= k;
   *  if k >= n, P**T = G(n-1) . . . G(2) G(1) and DORGBR returns P**T as
   *  an N-by-N matrix.
   *
   *  Arguments
   *  =========
   *
   *  VECT    (input) CHARACTER*1
   *          Specifies whether the matrix Q or the matrix P**T is
   *          required, as defined in the transformation applied by DGEBRD:
   *          = 'Q':  generate Q;
   *          = 'P':  generate P**T.
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix Q or P**T to be returned.
   *          M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix Q or P**T to be returned.
   *          N >= 0.
   *          If VECT = 'Q', M >= N >= min(M,K);
   *          if VECT = 'P', N >= M >= min(N,K).
   *
   *  K       (input) INTEGER
   *          K >= 0.
   *          If VECT = 'Q', the number of columns in the original M-by-K
   *          matrix reduced by DGEBRD.
   *          If VECT = 'P', the number of rows in the original K-by-N
   *          matrix reduced by DGEBRD.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the vectors which define the elementary reflectors,
   *          as returned by DGEBRD.
   *          On exit, the M-by-N matrix Q or P**T.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A. LDA >= max(1,M).
   *
   *  TAU     (input) DOUBLE PRECISION array, dimension
   *                                (min(M,K)) if VECT = 'Q'
   *                                (min(N,K)) if VECT = 'P'
   *          TAU(i) must contain the scalar factor of the elementary
   *          reflector H(i) or G(i), which determines Q or P**T, as
   *          returned by DGEBRD in its array argument TAUQ or TAUP.
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
   *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
   *
   *  LWORK   (input) INTEGER
   *          The dimension of the array WORK. LWORK >= max(1,min(M,N)).
   *          For optimum performance LWORK >= min(M,N)*NB, where NB
   *          is the optimal blocksize.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument had an illegal value
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e+0
#undef one
#define one 1.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  int            wantq;
  long            i, iinfo, j;
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
  wantq = lsame( vect, 'q' );
  if( !wantq && !lsame( vect, 'p' ) ) {
    *info = -1;
  } else if( m<0 ) {
    *info = -2;
  } else if( n<0 || ( wantq && ( n>m || n<min( m, k ) ) ) ||
            ( !wantq && ( m>n || m<min( n, k ) ) ) ) {
    *info = -3;
  } else if( k<0 ) {
    *info = -4;
  } else if( lda<max( 1, m ) ) {
    *info = -6;
  } else if( lwork<max( 1, min( m, n ) ) ) {
    *info = -9;
  }
  if( *info!=0 ) {
    xerbla( "dorgbr", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( m==0 || n==0 ) {
    work_1( 1 ) = 1;
    return;
  }

  if( wantq ) {
    /**
     *        Form Q, determined by a call to DGEBRD to reduce an m-by-k
     *        matrix
     **/
    if( m>=k ) {
      /**
       *           If m >= k, assume m >= n >= k
       **/
      dorgqr( m, n, k, a, lda, tau, work, lwork, &iinfo );

    } else {
      /**
       *           If m < k, assume *m = n
       *
       *           Shift the vectors which define the elementary reflectors one
       *           column to the right, and set the first row and column of Q
       *           to those of the unit matrix
       **/
      for (j=m ; j>=2 ; j+=-1) {
        a_2( 1, j ) = zero;
        for (i=j + 1 ; i<=m ; i+=1) {
          a_2( i, j ) = a_2( i, j-1 );
        }
      }
      a_2( 1, 1 ) = one;
      for (i=2 ; i<=m ; i+=1) {
        a_2( i, 1 ) = zero;
      }
      if( m>1 ) {
        /**
         *              Form Q(2:m,2:m)
         **/
        dorgqr( m-1, m-1, m-1, &a_2( 2, 2 ), lda, tau, work,
               lwork, &iinfo );
      }
    }
  } else {
    /**
     *        Form P', determined by a call to DGEBRD to reduce a k-by-n
     *        matrix
     **/
    if( k<n ) {
      /**
       *           If k < n, assume k <= m <= n
       **/
      dorglq( m, n, k, a, lda, tau, work, lwork, &iinfo );

    } else {
      /**
       *           If k >= n, assume *m = n
       *
       *           Shift the vectors which define the elementary reflectors one
       *           row downward, and set the first row and column of P' to
       *           those of the unit matrix
       **/
      a_2( 1, 1 ) = one;
      for (i=2 ; i<=n ; i+=1) {
        a_2( i, 1 ) = zero;
      }
      for (j=2 ; j<=n ; j+=1) {
        for (i=j - 1 ; i>=2 ; i+=-1) {
          a_2( i, j ) = a_2( i-1, j );
        }
        a_2( 1, j ) = zero;
      }
      if( n>1 ) {
        /**
         *              Form P'(2:n,2:n)
         **/
        dorglq( n-1, n-1, n-1, &a_2( 2, 2 ), lda, tau, work,
               lwork, &iinfo );
      }
    }
  }
  return;
  /**
   *     End of DORGBR
   **/
}



void dorglq( long m, long n, long k, double a[], long lda,
            double tau[], double work[], long lwork, long *info )
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
   *  DORGLQ generates an M-by-N real matrix Q with orthonormal rows,
   *  which is defined as the first M rows of a product of K elementary
   *  reflectors of order N
   *
   *        Q  =  H(k) . . . H(2) H(1)
   *
   *  as returned by DGELQF.
   *
   *  Arguments
   *  =========
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix Q. M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix Q. N >= M.
   *
   *  K       (input) INTEGER
   *          The number of elementary reflectors whose product defines the
   *          matrix Q. M >= K >= 0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the i-th row must contain the vector which defines
   *          the elementary reflector H(i), for i = 1,2,...,k, as returned
   *          by DGELQF in the first k rows of its array argument A.
   *          On exit, the M-by-N matrix Q.
   *
   *  LDA     (input) INTEGER
   *          The first dimension of the array A. LDA >= max(1,M).
   *
   *  TAU     (input) DOUBLE PRECISION array, dimension (K)
   *          TAU(i) must contain the scalar factor of the elementary
   *          reflector H(i), as returned by DGELQF.
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
   *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
   *
   *  LWORK   (input) INTEGER
   *          The dimension of the array WORK. LWORK >= max(1,M).
   *          For optimum performance LWORK >= M*NB, where NB is
   *          the optimal blocksize.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument has an illegal value
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  long    i, ib, iinfo, iws, j, ki=0, kk, l, ldwork=0, nb,
          nbmin, nx;
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
  } else if( n<m ) {
    *info = -2;
  } else if( k<0 || k>m ) {
    *info = -3;
  } else if( lda<max( 1, m ) ) {
    *info = -5;
  } else if( lwork<max( 1, m ) ) {
    *info = -8;
  }
  if( *info!=0 ) {
    xerbla( "dorglq", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( m<=0 ) {
    work_1( 1 ) = 1;
    return;
  }
  /**
   *     Determine the block size.
   **/
  nb = ilaenv( 1, "dorglq", " ", m, n, k, -1 );
  nbmin = 2;
  nx = 0;
  iws = m;
  if( nb>1 && nb<k ) {
    /**
     *        Determine when to cross over from blocked to unblocked code.
     **/
    nx = max( 0, ilaenv( 3, "dorglq", " ", m, n, k, -1 ) );
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
        nbmin = max( 2, ilaenv( 2, "dorglq", " ", m, n, k, -1 ) );
      }
    }
  }

  if( nb>=nbmin && nb<k && nx<k ) {
    /**
     *        Use blocked code after the last block.
     *        The first kk rows are handled by the block method.
     **/
    ki = ( ( k-nx-1 ) / nb )*nb;
    kk = min( k, ki+nb );
    /**
     *        Set A(kk+1:m,1:kk) to zero.
     **/
    for (j=1 ; j<=kk ; j+=1) {
      for (i=kk + 1 ; i<=m ; i+=1) {
        a_2( i, j ) = zero;
      }
    }
  } else {
    kk = 0;
  }
  /**
   *     Use unblocked code for the last or only block.
   **/
  if( kk<m )
    dorgl2( m-kk, n-kk, k-kk, &a_2( kk+1, kk+1 ), lda,
           &tau_1( kk+1 ), work, &iinfo );

  if( kk>0 ) {
    /**
     *        Use blocked code
     **/
    for (i=ki + 1 ; -nb>0?i<=1:i>=1 ; i+=-nb) {
      ib = min( nb, k-i+1 );
      if( i+ib<=m ) {
        /**
         *              Form the triangular factor of the block reflector
         *              H = H(i) H(i+1) . . . H(i+ib-1)
         **/
        dlarft( 'f'/*orward*/, 'r'/*owwise*/, n-i+1, ib, &a_2( i, i ),
               lda, &tau_1( i ), work, ldwork );
        /**
         *              Apply H' to A(i+ib:m,i:n) from the right
         **/
        dlarfb( 'r'/*ight*/, 't'/*ranspose*/, 'f'/*orward*/, 'r'/*owwise*/,
               m-i-ib+1, n-i+1, ib, &a_2( i, i ), lda, work,
               ldwork, &a_2( i+ib, i ), lda, &work_1( ib+1 ),
               ldwork );
      }
      /**
       *           Apply H' to columns i:n of current block
       **/
      dorgl2( ib, n-i+1, ib, &a_2( i, i ), lda, &tau_1( i ), work,
             &iinfo );
      /**
       *           Set columns 1:i-1 of current block to zero
       **/
      for (j=1 ; j<=i - 1 ; j+=1) {
        for (l=i ; l<=i + ib - 1 ; l+=1) {
          a_2( l, j ) = zero;
        }
      }
    }
  }

  work_1( 1 ) = iws;
  return;
  /**
   *     End of DORGLQ
   **/
}



void dorgl2( long m, long n, long k, double a[], long lda, double tau[],
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
   *  DORGL2 generates an m by n real matrix Q with orthonormal rows,
   *  which is defined as the first m rows of a product of k elementary
   *  reflectors of order n
   *
   *        Q  =  H(k) . . . H(2) H(1)
   *
   *  as returned by DGELQF.
   *
   *  Arguments
   *  =========
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix Q. M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix Q. N >= M.
   *
   *  K       (input) INTEGER
   *          The number of elementary reflectors whose product defines the
   *          matrix Q. M >= K >= 0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the i-th row must contain the vector which defines
   *          the elementary reflector H(i), for i = 1,2,...,k, as returned
   *          by DGELQF in the first k rows of its array argument A.
   *          On exit, the m-by-n matrix Q.
   *
   *  LDA     (input) INTEGER
   *          The first dimension of the array A. LDA >= max(1,M).
   *
   *  TAU     (input) DOUBLE PRECISION array, dimension (K)
   *          TAU(i) must contain the scalar factor of the elementary
   *          reflector H(i), as returned by DGELQF.
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
   *
   *  INFO    (output) INTEGER
   *          = 0: successful exit
   *          < 0: if INFO = -i, the i-th argument has an illegal value
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
  long            i, j, l;
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
  if( m<0 ) {
    *info = -1;
  } else if( n<m ) {
    *info = -2;
  } else if( k<0 || k>m ) {
    *info = -3;
  } else if( lda<max( 1, m ) ) {
    *info = -5;
  }
  if( *info!=0 ) {
    xerbla( "dorgl2", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( m<=0 )
    return;

  if( k<m ) {
    /**
     *        Initialise rows k+1:m to rows of the unit matrix
     **/
    for (j=1 ; j<=n ; j+=1) {
      for (l=k + 1 ; l<=m ; l+=1) {
        a_2( l, j ) = zero;
      }
      if( j>k && j<=m )
        a_2( j, j ) = one;
    }
  }

  for (i=k ; i>=1 ; i+=-1) {
    /**
     *        Apply H(i) to A(i:m,i:n) from the right
     **/
    if( i<n ) {
      if( i<m ) {
        a_2( i, i ) = one;
        dlarf( 'r'/*ight*/, m-i, n-i+1, &a_2( i, i ), lda,
              tau_1( i ), &a_2( i+1, i ), lda, work );
      }
      cblas_dscal( n-i, -tau_1( i ), &a_2( i, i+1 ), lda );
    }
    a_2( i, i ) = one - tau_1( i );
    /**
     *        Set A(1:i-1,i) to zero
     **/
    for (l=1 ; l<=i - 1 ; l+=1) {
      a_2( i, l ) = zero;
    }
  }
  return;
  /**
   *     End of DORGL2
   **/
}



void dorgqr( long m, long n, long k, double a[], long lda,
            double tau[], double work[], long lwork, long *info )
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
   *  DORGQR generates an M-by-N real matrix Q with orthonormal columns,
   *  which is defined as the first N columns of a product of K elementary
   *  reflectors of order M
   *
   *        Q  =  H(1) H(2) . . . H(k)
   *
   *  as returned by DGEQRF.
   *
   *  Arguments
   *  =========
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix Q. M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix Q. M >= N >= 0.
   *
   *  K       (input) INTEGER
   *          The number of elementary reflectors whose product defines the
   *          matrix Q. N >= K >= 0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the i-th column must contain the vector which
   *          defines the elementary reflector H(i), for i = 1,2,...,k, as
   *          returned by DGEQRF in the first k columns of its array
   *          argument A.
   *          On exit, the M-by-N matrix Q.
   *
   *  LDA     (input) INTEGER
   *          The first dimension of the array A. LDA >= max(1,M).
   *
   *  TAU     (input) DOUBLE PRECISION array, dimension (K)
   *          TAU(i) must contain the scalar factor of the elementary
   *          reflector H(i), as returned by DGEQRF.
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
   *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
   *
   *  LWORK   (input) INTEGER
   *          The dimension of the array WORK. LWORK >= max(1,N).
   *          For optimum performance LWORK >= N*NB, where NB is the
   *          optimal blocksize.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument has an illegal value
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  long     i, ib, iinfo, iws, j, ki=0, kk, l, ldwork=0, nb,
           nbmin, nx;
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
  } else if( n<0 || n>m ) {
    *info = -2;
  } else if( k<0 || k>n ) {
    *info = -3;
  } else if( lda<max( 1, m ) ) {
    *info = -5;
  } else if( lwork<max( 1, n ) ) {
    *info = -8;
  }
  if( *info!=0 ) {
    xerbla( "dorgqr", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( n<=0 ) {
    work_1( 1 ) = 1;
    return;
  }
  /**
   *     Determine the block size.
   **/
  nb = ilaenv( 1, "dorgqr", " ", m, n, k, -1 );
  nbmin = 2;
  nx = 0;
  iws = n;
  if( nb>1 && nb<k ) {
    /**
     *        Determine when to cross over from blocked to unblocked code.
     **/
    nx = max( 0, ilaenv( 3, "dorgqr", " ", m, n, k, -1 ) );
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
        nbmin = max( 2, ilaenv( 2, "dorgqr", " ", m, n, k, -1 ) );
      }
    }
  }

  if( nb>=nbmin && nb<k && nx<k ) {
    /**
     *        Use blocked code after the last block.
     *        The first kk columns are handled by the block method.
     **/
    ki = ( ( k-nx-1 ) / nb )*nb;
    kk = min( k, ki+nb );
    /**
     *        Set A(1:kk,kk+1:n) to zero.
     **/
    for (j=kk + 1 ; j<=n ; j+=1) {
      for (i=1 ; i<=kk ; i+=1) {
        a_2( i, j ) = zero;
      }
    }
  } else {
    kk = 0;
  }
  /**
   *     Use unblocked code for the last or only block.
   **/
  if( kk<n )
    dorg2r( m-kk, n-kk, k-kk, &a_2( kk+1, kk+1 ), lda,
           &tau_1( kk+1 ), work, &iinfo );

  if( kk>0 ) {
    /**
     *        Use blocked code
     **/
    for (i=ki + 1 ; -nb>0?i<=1:i>=1 ; i+=-nb) {
      ib = min( nb, k-i+1 );
      if( i+ib<=n ) {
        /**
         *              Form the triangular factor of the block reflector
         *              H = H(i) H(i+1) . . . H(i+ib-1)
         **/
        dlarft( 'f'/*orward*/, 'c'/*olumnwise*/, m-i+1, ib,
               &a_2( i, i ), lda, &tau_1( i ), work, ldwork );
        /**
         *              Apply H to A(i:m,i+ib:n) from the left
         **/
        dlarfb( 'l'/*eft*/, 'n'/*o transpose*/, 'f'/*orward*/,
               'c'/*olumnwise*/, m-i+1, n-i-ib+1, ib,
               &a_2( i, i ), lda, work, ldwork, &a_2( i, i+ib ),
               lda, &work_1( ib+1 ), ldwork );
      }
      /**
       *           Apply H to rows i:m of current block
       **/
      dorg2r( m-i+1, ib, ib, &a_2( i, i ), lda, &tau_1( i ), work,
             &iinfo );
      /**
       *           Set rows 1:i-1 of current block to zero
       **/
      for (j=i ; j<=i + ib - 1 ; j+=1) {
        for (l=1 ; l<=i - 1 ; l+=1) {
          a_2( l, j ) = zero;
        }
      }
    }
  }

  work_1( 1 ) = iws;
  return;
  /**
   *     End of DORGQR
   **/
}



void dorg2r( long m, long n, long k, double a[], long lda,
            double tau[], double work[], long *info )
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
   *  DORG2R generates an m by n real matrix Q with orthonormal columns,
   *  which is defined as the first n columns of a product of k elementary
   *  reflectors of order m
   *
   *        Q  =  H(1) H(2) . . . H(k)
   *
   *  as returned by DGEQRF.
   *
   *  Arguments
   *  =========
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix Q. M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix Q. M >= N >= 0.
   *
   *  K       (input) INTEGER
   *          The number of elementary reflectors whose product defines the
   *          matrix Q. N >= K >= 0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the i-th column must contain the vector which
   *          defines the elementary reflector H(i), for i = 1,2,...,k, as
   *          returned by DGEQRF in the first k columns of its array
   *          argument A.
   *          On exit, the m-by-n matrix Q.
   *
   *  LDA     (input) INTEGER
   *          The first dimension of the array A. LDA >= max(1,M).
   *
   *  TAU     (input) DOUBLE PRECISION array, dimension (K)
   *          TAU(i) must contain the scalar factor of the elementary
   *          reflector H(i), as returned by DGEQRF.
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
   *
   *  INFO    (output) INTEGER
   *          = 0: successful exit
   *          < 0: if INFO = -i, the i-th argument has an illegal value
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
  long            i, j, l;
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
  if( m<0 ) {
    *info = -1;
  } else if( n<0 || n>m ) {
    *info = -2;
  } else if( k<0 || k>n ) {
    *info = -3;
  } else if( lda<max( 1, m ) ) {
    *info = -5;
  }
  if( *info!=0 ) {
    xerbla( "dorg2r", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( n<=0 )
    return;
  /**
   *     Initialise columns k+1:n to columns of the unit matrix
   **/
  for (j=k + 1 ; j<=n ; j+=1) {
    for (l=1 ; l<=m ; l+=1) {
      a_2( l, j ) = zero;
    }
    a_2( j, j ) = one;
  }

  for (i=k ; i>=1 ; i+=-1) {
    /**
     *        Apply H(i) to A(i:m,i:n) from the left
     **/
    if( i<n ) {
      a_2( i, i ) = one;
      dlarf( 'l'/*eft*/, m-i+1, n-i, &a_2( i, i ), 1, tau_1( i ),
            &a_2( i, i+1 ), lda, work );
    }
    if( i<m )
      cblas_dscal( m-i, -tau_1( i ), &a_2( i+1, i ), 1 );
    a_2( i, i ) = one - tau_1( i );
    /**
     *        Set A(1:i-1,i) to zero
     **/
    for (l=1 ; l<=i - 1 ; l+=1) {
      a_2( l, i ) = zero;
    }
  }
  return;
  /**
   *     End of DORG2R
   **/
}



void dormbr( char vect, char side, char trans, long m, long n, long k,
            double a[], long lda, double tau[], double c[],long ldc,
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
   *  If VECT = 'Q', DORMBR overwrites the general real M-by-N matrix C
   *  with
   *                  SIDE = 'L'     SIDE = 'R'
   *  TRANS = 'N':      Q * C          C * Q
   *  TRANS = 'T':      Q**T * C       C * Q**T
   *
   *  If VECT = 'P', DORMBR overwrites the general real M-by-N matrix C
   *  with
   *                  SIDE = 'L'     SIDE = 'R'
   *  TRANS = 'N':      P * C          C * P
   *  TRANS = 'T':      P**T * C       C * P**T
   *
   *  Here Q and P**T are the orthogonal matrices determined by DGEBRD when
   *  reducing a real matrix A to bidiagonal form: A = Q * B * P**T. Q and
   *  P**T are defined as products of elementary reflectors H(i) and G(i)
   *  respectively.
   *
   *  Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
   *  order of the orthogonal matrix Q or P**T that is applied.
   *
   *  If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
   *  if nq >= k, Q = H(1) H(2) . . . H(k);
   *  if nq < k, Q = H(1) H(2) . . . H(nq-1).
   *
   *  If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
   *  if k < nq, P = G(1) G(2) . . . G(k);
   *  if k >= nq, P = G(1) G(2) . . . G(nq-1).
   *
   *  Arguments
   *  =========
   *
   *  VECT    (input) CHARACTER*1
   *          = 'Q': apply Q or Q**T;
   *          = 'P': apply P or P**T.
   *
   *  SIDE    (input) CHARACTER*1
   *          = 'L': apply Q, Q**T, P or P**T from the Left;
   *          = 'R': apply Q, Q**T, P or P**T from the Right.
   *
   *  TRANS   (input) CHARACTER*1
   *          = 'N':  No transpose, apply Q  or P;
   *          = 'T':  Transpose, apply Q**T or P**T.
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix C. M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix C. N >= 0.
   *
   *  K       (input) INTEGER
   *          K >= 0.
   *          If VECT = 'Q', the number of columns in the original
   *          matrix reduced by DGEBRD.
   *          If VECT = 'P', the number of rows in the original
   *          matrix reduced by DGEBRD.
   *
   *  A       (input) DOUBLE PRECISION array, dimension
   *                                (LDA,min(nq,K)) if VECT = 'Q'
   *                                (LDA,nq)        if VECT = 'P'
   *          The vectors which define the elementary reflectors H(i) and
   *          G(i), whose products determine the matrices Q and P, as
   *          returned by DGEBRD.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.
   *          If VECT = 'Q', LDA >= max(1,nq);
   *          if VECT = 'P', LDA >= max(1,min(nq,K)).
   *
   *  TAU     (input) DOUBLE PRECISION array, dimension (min(nq,K))
   *          TAU(i) must contain the scalar factor of the elementary
   *          reflector H(i) or G(i) which determines Q or P, as returned
   *          by DGEBRD in the array argument TAUQ or TAUP.
   *
   *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
   *          On entry, the M-by-N matrix C.
   *          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q
   *          or P*C or P**T*C or C*P or C*P**T.
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
   *     .. Local Scalars ..*/
  int            applyq, left, notran;
  char          transt;
  long            i1, i2, iinfo, mi, ni, nq, nw;
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
  applyq = lsame( vect, 'q' );
  left = lsame( side, 'l' );
  notran = lsame( trans, 'n' );
  /**
   *     NQ is the order of Q or P and NW is the minimum dimension of WORK
   **/
  if( left ) {
    nq = m;
    nw = n;
  } else {
    nq = n;
    nw = m;
  }
  if( !applyq && !lsame( vect, 'p' ) ) {
    *info = -1;
  } else if( !left && !lsame( side, 'r' ) ) {
    *info = -2;
  } else if( !notran && !lsame( trans, 't' ) ) {
    *info = -3;
  } else if( m<0 ) {
    *info = -4;
  } else if( n<0 ) {
    *info = -5;
  } else if( k<0 ) {
    *info = -6;
  } else if( ( applyq && lda<max( 1, nq ) ) ||
            ( !applyq && lda<max( 1, min( nq, k ) ) ) )
    {
      *info = -8;
    } else if( ldc<max( 1, m ) ) {
      *info = -11;
    } else if( lwork<max( 1, nw ) ) {
      *info = -13;
    }
  if( *info!=0 ) {
    xerbla( "dormbr", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  if( m==0 || n==0 ) {
    work_1( 1 ) = 1;
    return;
  }

  if( applyq ) {
    /**
     *        Apply Q
     **/
    if( nq>=k ) {
      /**
       *           Q was determined by a call to DGEBRD with nq >= k
       **/
      dormqr( side, trans, m, n, k, a, lda, tau, c, ldc,
             work, lwork, &iinfo );
    } else {
      /**
       *           Q was determined by a call to DGEBRD with nq < k
       **/
      if( left ) {
        mi = m - 1;
        ni = n;
        i1 = 2;
        i2 = 1;
      } else {
        mi = m;
        ni = n - 1;
        i1 = 1;
        i2 = 2;
      }
      dormqr( side, trans, mi, ni, nq-1, &a_2( 2, 1 ), lda, tau,
             &c_2( i1, i2 ), ldc, work, lwork, &iinfo );
    }
  } else {
    /**
     *        Apply P
     **/
    if( notran ) {
      transt = 't';
    } else {
      transt = 'n';
    }
    if( nq>k ) {
      /**
       *           P was determined by a call to DGEBRD with nq > k
       **/
      dormlq( side, transt, m, n, k, a, lda, tau, c, ldc,
             work, lwork, &iinfo );
    } else {
      /**
       *           P was determined by a call to DGEBRD with nq <= k
       **/
      if( left ) {
        mi = m - 1;
        ni = n;
        i1 = 2;
        i2 = 1;
      } else {
        mi = m;
        ni = n - 1;
        i1 = 1;
        i2 = 2;
      }
      dormlq( side, transt, mi, ni, nq-1, &a_2( 1, 2 ), lda,
             tau, &c_2( i1, i2 ), ldc, work, lwork, &iinfo );
    }
  }
  return;
  /**
   *     End of DORMBR
   **/
}



void dgebrd( long m, long n, double a[], long lda,
            double d[], double e[], double tauq[], double taup[],
            double work[], long lwork,long *info )
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
#undef tauq_1
#define tauq_1(a1) tauq[a1-1]
#undef taup_1
#define taup_1(a1) taup[a1-1]
#undef e_1
#define e_1(a1) e[a1-1]
#undef d_1
#define d_1(a1) d[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGEBRD reduces a general real M-by-N matrix A to upper or lower
   *  bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
   *
   *  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
   *
   *  Arguments
   *  =========
   *
   *  M       (input) INTEGER
   *          The number of rows in the matrix A.  M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns in the matrix A.  N >= 0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the M-by-N general matrix to be reduced.
   *          On exit,
   *          if m >= n, the diagonal and the first superdiagonal are
   *            overwritten with the upper bidiagonal matrix B; the
   *            elements below the diagonal, with the array TAUQ, represent
   *            the orthogonal matrix Q as a product of elementary
   *            reflectors, and the elements above the first superdiagonal,
   *            with the array TAUP, represent the orthogonal matrix P as
   *            a product of elementary reflectors;
   *          if m < n, the diagonal and the first subdiagonal are
   *            overwritten with the lower bidiagonal matrix B; the
   *            elements below the first subdiagonal, with the array TAUQ,
   *            represent the orthogonal matrix Q as a product of
   *            elementary reflectors, and the elements above the diagonal,
   *            with the array TAUP, represent the orthogonal matrix P as
   *            a product of elementary reflectors.
   *          See Further Details.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
   *          The diagonal elements of the bidiagonal matrix B:
   *          D(i) = A(i,i).
   *
   *  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
   *          The off-diagonal elements of the bidiagonal matrix B:
   *          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
   *          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
   *
   *  TAUQ    (output) DOUBLE PRECISION array dimension (min(M,N))
   *          The scalar factors of the elementary reflectors which
   *          represent the orthogonal matrix Q. See Further Details.
   *
   *  TAUP    (output) DOUBLE PRECISION array, dimension (min(M,N))
   *          The scalar factors of the elementary reflectors which
   *          represent the orthogonal matrix P. See Further Details.
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
   *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
   *
   *  LWORK   (input) INTEGER
   *          The length of the array WORK.  LWORK >= max(1,M,N).
   *          For optimum performance LWORK >= (M+N)*NB, where NB
   *          is the optimal blocksize.
   *
   *  INFO    (output) INTEGER
   *          = 0:  successful exit
   *          < 0:  if INFO = -i, the i-th argument had an illegal value.
   *
   *  Further Details
   *  ===============
   *
   *  The matrices Q and P are represented as products of elementary
   *  reflectors:
   *
   *  If m >= n,
   *
   *     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
   *
   *  Each H(i) and G(i) has the form:
   *
   *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
   *
   *  where tauq and taup are real scalars, and v and u are real vectors;
   *  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
   *  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
   *  tauq is stored in TAUQ(i) and taup in TAUP(i).
   *
   *  If m < n,
   *
   *     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
   *
   *  Each H(i) and G(i) has the form:
   *
   *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
   *
   *  where tauq and taup are real scalars, and v and u are real vectors;
   *  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
   *  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
   *  tauq is stored in TAUQ(i) and taup in TAUP(i).
   *
   *  The contents of A on exit are illustrated by the following examples:
   *
   *  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
   *
   *    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
   *    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
   *    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
   *    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
   *    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
   *    (  v1  v2  v3  v4  v5 )
   *
   *  where d and e denote diagonal and off-diagonal elements of B, vi
   *  denotes an element of the vector defining H(i), and ui an element of
   *  the vector defining G(i).
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef one
#define one 1.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  long            i, iinfo, j, ldwrkx, ldwrky, minmn, nb, nbmin,
  nx;
  double    ws;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max, min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input parameters
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
  } else if( lwork<max( 1, max(m, n) ) ) {
    *info = -10;
  }
  if( *info<0 ) {
    xerbla( "dgebrd", -*info );
    return;
  }
  /**
   *     Quick return if possible
   **/
  minmn = min( m, n );
  if( minmn==0 ) {
    work_1( 1 ) = 1;
    return;
  }

  ws = max( m, n );
  ldwrkx = m;
  ldwrky = n;
  /**
   *     Set the block size NB and the crossover point NX.
   **/
  nb = max( 1, ilaenv( 1, "dgebrd", " ", m, n, -1, -1 ) );

  if( nb>1 && nb<minmn ) {
    /**
     *        Determine when to switch from blocked to unblocked code.
     **/
    nx = max( nb, ilaenv( 3, "dgebrd", " ", m, n, -1, -1 ) );
    if( nx<minmn ) {
      ws = ( m+n )*nb;
      if( lwork<ws ) {
        /**
         *              Not enough work space for the optimal NB, consider using
         *              a smaller block size.
         **/
        nbmin = ilaenv( 2, "dgebrd", " ", m, n, -1, -1 );
        if( lwork>=( m+n )*nbmin ) {
          nb = lwork / ( m+n );
        } else {
          nb = 1;
          nx = minmn;
        }
      }
    }
  } else {
    nx = minmn;
  }

  for (i=1 ; nb>0?i<=minmn - nx:i>=minmn - nx ; i+=nb) {
    /**
     *        Reduce rows and columns i:i+nb-1 to bidiagonal form and return
     *        the matrices X and Y which are needed to update the unreduced
     *        part of the matrix
     **/
    dlabrd( m-i+1, n-i+1, nb, &a_2( i, i ), lda, &d_1( i ), &e_1( i ),
           &tauq_1( i ), &taup_1( i ), work, ldwrkx,
           &work_1( ldwrkx*nb+1 ), ldwrky );
    /**
     *        Update the trailing submatrix A(i+nb:m,i+nb:n), using an update
     *        of the form  A := A - V*Y' - X*U'
     **/
    cblas_dgemm(CblasColMajor, CblasNoTrans, CblasTrans, m-i-nb+1, n-i-nb+1,
                nb, -one, &a_2( i+nb, i ), lda,
                &work_1( ldwrkx*nb+nb+1 ), ldwrky, one,
                &a_2( i+nb, i+nb ), lda );
    cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, m-i-nb+1, n-i-nb+1,
                nb, -one, &work_1( nb+1 ), ldwrkx, &a_2( i, i+nb ), lda,
                one, &a_2( i+nb, i+nb ), lda );
    /**
     *        Copy diagonal and off-diagonal elements of B back into A
     **/
    if( m>=n ) {
      for (j=i ; j<=i + nb - 1 ; j+=1) {
        a_2( j, j ) = d_1( j );
        a_2( j, j+1 ) = e_1( j );
      }
    } else {
      for (j=i ; j<=i + nb - 1 ; j+=1) {
        a_2( j, j ) = d_1( j );
        a_2( j+1, j ) = e_1( j );
      }
    }
  }
  /**
   *     Use unblocked code to reduce the remainder of the matrix
   **/
  dgebd2( m-i+1, n-i+1, &a_2( i, i ), lda, &d_1( i ), &e_1( i ),
         &tauq_1( i ), &taup_1( i ), work, &iinfo );
  work_1( 1 ) = ws;
  return;
  /**
   *     End of DGEBRD
   **/
}



void dgebd2( long m, long n, double a[], long lda,
            double d[], double e[], double tauq[], double taup[],
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
#undef tauq_1
#define tauq_1(a1) tauq[a1-1]
#undef taup_1
#define taup_1(a1) taup[a1-1]
#undef e_1
#define e_1(a1) e[a1-1]
#undef d_1
#define d_1(a1) d[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DGEBD2 reduces a real general m by n matrix A to upper or lower
   *  bidiagonal form B by an orthogonal transformation: Q' * A * P = B.
   *
   *  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
   *
   *  Arguments
   *  =========
   *
   *  M       (input) INTEGER
   *          The number of rows in the matrix A.  M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns in the matrix A.  N >= 0.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the m by n general matrix to be reduced.
   *          On exit,
   *          if m >= n, the diagonal and the first superdiagonal are
   *            overwritten with the upper bidiagonal matrix B; the
   *            elements below the diagonal, with the array TAUQ, represent
   *            the orthogonal matrix Q as a product of elementary
   *            reflectors, and the elements above the first superdiagonal,
   *            with the array TAUP, represent the orthogonal matrix P as
   *            a product of elementary reflectors;
   *          if m < n, the diagonal and the first subdiagonal are
   *            overwritten with the lower bidiagonal matrix B; the
   *            elements below the first subdiagonal, with the array TAUQ,
   *            represent the orthogonal matrix Q as a product of
   *            elementary reflectors, and the elements above the diagonal,
   *            with the array TAUP, represent the orthogonal matrix P as
   *            a product of elementary reflectors.
   *          See Further Details.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
   *          The diagonal elements of the bidiagonal matrix B:
   *          D(i) = A(i,i).
   *
   *  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
   *          The off-diagonal elements of the bidiagonal matrix B:
   *          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
   *          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
   *
   *  TAUQ    (output) DOUBLE PRECISION array dimension (min(M,N))
   *          The scalar factors of the elementary reflectors which
   *          represent the orthogonal matrix Q. See Further Details.
   *
   *  TAUP    (output) DOUBLE PRECISION array, dimension (min(M,N))
   *          The scalar factors of the elementary reflectors which
   *          represent the orthogonal matrix P. See Further Details.
   *
   *  WORK    (workspace) DOUBLE PRECISION array, dimension (max(M,N))
   *
   *  INFO    (output) INTEGER
   *          = 0: successful exit.
   *          < 0: if INFO = -i, the i-th argument had an illegal value.
   *
   *  Further Details
   *  ===============
   *
   *  The matrices Q and P are represented as products of elementary
   *  reflectors:
   *
   *  If m >= n,
   *
   *     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
   *
   *  Each H(i) and G(i) has the form:
   *
   *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
   *
   *  where tauq and taup are real scalars, and v and u are real vectors;
   *  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
   *  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
   *  tauq is stored in TAUQ(i) and taup in TAUP(i).
   *
   *  If m < n,
   *
   *     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
   *
   *  Each H(i) and G(i) has the form:
   *
   *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
   *
   *  where tauq and taup are real scalars, and v and u are real vectors;
   *  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
   *  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
   *  tauq is stored in TAUQ(i) and taup in TAUP(i).
   *
   *  The contents of A on exit are illustrated by the following examples:
   *
   *  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
   *
   *    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
   *    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
   *    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
   *    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
   *    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
   *    (  v1  v2  v3  v4  v5 )
   *
   *  where d and e denote diagonal and off-diagonal elements of B, vi
   *  denotes an element of the vector defining H(i), and ui an element of
   *  the vector defining G(i).
   *
   *  =====================================================================
   *
   *     .. Parameters ..*/
#undef zero
#define zero 0.0e+0
#undef one
#define one 1.0e+0
  /**     ..
   *     .. Local Scalars ..*/
  long            i;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          max, min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Test the input parameters
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
  if( *info<0 ) {
    xerbla( "dgebd2", -*info );
    return;
  }

  if( m>=n ) {
    /**
     *        Reduce to upper bidiagonal form
     **/
    for (i=1 ; i<=n ; i+=1) {
      /**
       *           Generate elementary reflector H(i) to annihilate A(i+1:m,i)
       **/
      dlarfg( m-i+1, &a_2( i, i ), &a_2( min( i+1, m ), i ), 1,
             &tauq_1( i ) );
      d_1( i ) = a_2( i, i );
      a_2( i, i ) = one;
      /**
       *           Apply H(i) to A(i:m,i+1:n) from the left
       **/
      dlarf( 'l'/*eft*/, m-i+1, n-i, &a_2( i, i ), 1, tauq_1( i ),
            &a_2( i, i+1 ), lda, work );
      a_2( i, i ) = d_1( i );

      if( i<n ) {
        /**
         *              Generate elementary reflector G(i) to annihilate
         *              A(i,i+2:n)
         **/
        dlarfg( n-i, &a_2( i, i+1 ), &a_2( i, min( i+2, n ) ),
               lda, &taup_1( i ) );
        e_1( i ) = a_2( i, i+1 );
        a_2( i, i+1 ) = one;
        /**
         *              Apply G(i) to A(i+1:m,i+1:n) from the right
         **/
        dlarf( 'r'/*ight*/, m-i, n-i, &a_2( i, i+1 ), lda,
              taup_1( i ), &a_2( i+1, i+1 ), lda, work );
        a_2( i, i+1 ) = e_1( i );
      } else {
        taup_1( i ) = zero;
      }
    }
  } else {
    /**
     *        Reduce to lower bidiagonal form
     **/
    for (i=1 ; i<=m ; i+=1) {
      /**
       *           Generate elementary reflector G(i) to annihilate A(i,i+1:n)
       **/
      dlarfg( n-i+1, &a_2( i, i ), &a_2( i, min( i+1, n ) ), lda,
             &taup_1( i ) );
      d_1( i ) = a_2( i, i );
      a_2( i, i ) = one;
      /**
       *           Apply G(i) to A(i+1:m,i:n) from the right
       **/
      dlarf( 'r'/*ight*/, m-i, n-i+1, &a_2( i, i ), lda, taup_1( i ),
            &a_2( min( i+1, m ), i ), lda, work );
      a_2( i, i ) = d_1( i );

      if( i<m ) {
        /**
         *              Generate elementary reflector H(i) to annihilate
         *              A(i+2:m,i)
         **/
        dlarfg( m-i, &a_2( i+1, i ), &a_2( min( i+2, m ), i ), 1,
               &tauq_1( i ) );
        e_1( i ) = a_2( i+1, i );
        a_2( i+1, i ) = one;
        /**
         *              Apply H(i) to A(i+1:m,i+1:n) from the left
         **/
        dlarf( 'l'/*eft*/, m-i, n-i, &a_2( i+1, i ), 1, tauq_1( i ),
              &a_2( i+1, i+1 ), lda, work );
        a_2( i+1, i ) = e_1( i );
      } else {
        tauq_1( i ) = zero;
      }
    }
  }
  return;
  /**
   *     End of DGEBD2
   **/
}



void dlabrd( long m, long n, long nb, double a[], long lda,
            double d[], double e[], double tauq[], double taup[],
            double x[], long ldx, double y[],long ldy )
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
#undef y_2
#define y_2(a1,a2) y[a1-1+ldy*(a2-1)]
#undef x_2
#define x_2(a1,a2) x[a1-1+ldx*(a2-1)]
#undef tauq_1
#define tauq_1(a1) tauq[a1-1]
#undef taup_1
#define taup_1(a1) taup[a1-1]
#undef e_1
#define e_1(a1) e[a1-1]
#undef d_1
#define d_1(a1) d[a1-1]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLABRD reduces the first NB rows and columns of a real general
   *  m by n matrix A to upper or lower bidiagonal form by an orthogonal
   *  transformation Q' * A * P, and returns the matrices X and Y which
   *  are needed to apply the transformation to the unreduced part of A.
   *
   *  If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
   *  bidiagonal form.
   *
   *  This is an auxiliary routine called by DGEBRD
   *
   *  Arguments
   *  =========
   *
   *  M       (input) INTEGER
   *          The number of rows in the matrix A.
   *
   *  N       (input) INTEGER
   *          The number of columns in the matrix A.
   *
   *  NB      (input) INTEGER
   *          The number of leading rows and columns of A to be reduced.
   *
   *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
   *          On entry, the m by n general matrix to be reduced.
   *          On exit, the first NB rows and columns of the matrix are
   *          overwritten; the rest of the array is unchanged.
   *          If m >= n, elements on and below the diagonal in the first NB
   *            columns, with the array TAUQ, represent the orthogonal
   *            matrix Q as a product of elementary reflectors; and
   *            elements above the diagonal in the first NB rows, with the
   *            array TAUP, represent the orthogonal matrix P as a product
   *            of elementary reflectors.
   *          If m < n, elements below the diagonal in the first NB
   *            columns, with the array TAUQ, represent the orthogonal
   *            matrix Q as a product of elementary reflectors, and
   *            elements on and above the diagonal in the first NB rows,
   *            with the array TAUP, represent the orthogonal matrix P as
   *            a product of elementary reflectors.
   *          See Further Details.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  D       (output) DOUBLE PRECISION array, dimension (NB)
   *          The diagonal elements of the first NB rows and columns of
   *          the reduced matrix.  D(i) = A(i,i).
   *
   *  E       (output) DOUBLE PRECISION array, dimension (NB)
   *          The off-diagonal elements of the first NB rows and columns of
   *          the reduced matrix.
   *
   *  TAUQ    (output) DOUBLE PRECISION array dimension (NB)
   *          The scalar factors of the elementary reflectors which
   *          represent the orthogonal matrix Q. See Further Details.
   *
   *  TAUP    (output) DOUBLE PRECISION array, dimension (NB)
   *          The scalar factors of the elementary reflectors which
   *          represent the orthogonal matrix P. See Further Details.
   *
   *  X       (output) DOUBLE PRECISION array, dimension (LDX,NB)
   *          The m-by-nb matrix X required to update the unreduced part
   *          of A.
   *
   *  LDX     (input) INTEGER
   *          The leading dimension of the array X. LDX >= M.
   *
   *  Y       (output) DOUBLE PRECISION array, dimension (LDY,NB)
   *          The n-by-nb matrix Y required to update the unreduced part
   *          of A.
   *
   *  LDY     (output) INTEGER
   *          The leading dimension of the array Y. LDY >= N.
   *
   *  Further Details
   *  ===============
   *
   *  The matrices Q and P are represented as products of elementary
   *  reflectors:
   *
   *     Q = H(1) H(2) . . . H(nb)  and  P = G(1) G(2) . . . G(nb)
   *
   *  Each H(i) and G(i) has the form:
   *
   *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
   *
   *  where tauq and taup are real scalars, and v and u are real vectors.
   *
   *  If m >= n, v(1:i-1) = 0, v(i) = 1, and v(i:m) is stored on exit in
   *  A(i:m,i); u(1:i) = 0, u(i+1) = 1, and u(i+1:n) is stored on exit in
   *  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
   *
   *  If m < n, v(1:i) = 0, v(i+1) = 1, and v(i+1:m) is stored on exit in
   *  A(i+2:m,i); u(1:i-1) = 0, u(i) = 1, and u(i:n) is stored on exit in
   *  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
   *
   *  The elements of the vectors v and u together form the m-by-nb matrix
   *  V and the nb-by-n matrix U' which are needed, with X and Y, to apply
   *  the transformation to the unreduced part of the matrix, using a block
   *  update of the form:  A := A - V*Y' - X*U'.
   *
   *  The contents of A on exit are illustrated by the following examples
   *  with nb = 2:
   *
   *  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
   *
   *    (  1   1   u1  u1  u1 )           (  1   u1  u1  u1  u1  u1 )
   *    (  v1  1   1   u2  u2 )           (  1   1   u2  u2  u2  u2 )
   *    (  v1  v2  a   a   a  )           (  v1  1   a   a   a   a  )
   *    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
   *    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
   *    (  v1  v2  a   a   a  )
   *
   *  where a denotes an element of the original matrix which is unchanged,
   *  vi denotes an element of the vector defining H(i), and ui an element
   *  of the vector defining G(i).
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
  long            i;
  /**     ..
   *     .. Intrinsic Functions ..*/
  /*      intrinsic          min;*/
  /**     ..
   *     .. Executable Statements ..
   *
   *     Quick return if possible
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if( m<=0 || n<=0 )
    return;

  if( m>=n ) {
    /**
     *        Reduce to upper bidiagonal form
     **/
    for (i=1 ; i<=nb ; i+=1) {
      /**
       *           Update A(i:m,i)
       **/
      cblas_dgemv(CblasColMajor, CblasNoTrans, m-i+1, i-1, -one, &a_2( i, 1 ),
                  lda, &y_2( i, 1 ), ldy, one, &a_2( i, i ), 1 );
      cblas_dgemv(CblasColMajor, CblasNoTrans, m-i+1, i-1, -one, &x_2( i, 1 ),
                  ldx, &a_2( 1, i ), 1, one, &a_2( i, i ), 1 );
      /**
       *           Generate reflection Q(i) to annihilate A(i+1:m,i)
       **/
      dlarfg( m-i+1, &a_2( i, i ), &a_2( min( i+1, m ), i ), 1,
             &tauq_1( i ) );
      d_1( i ) = a_2( i, i );
      if( i<n ) {
        a_2( i, i ) = one;
        /**
         *              Compute Y(i+1:n,i)
         **/
        cblas_dgemv(CblasColMajor, CblasTrans, m-i+1, n-i, one, &a_2( i, i+1 ),
                    lda, &a_2( i, i ), 1, zero, &y_2( i+1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasTrans, m-i+1, i-1, one, &a_2(i,1), lda,
                    &a_2( i, i ), 1, zero, &y_2( 1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasNoTrans, n-i, i-1, -one, &y_2(i+1,1),
                    ldy, &y_2( 1, i ), 1, one, &y_2( i+1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasTrans, m-i+1, i-1, one, &x_2(i,1), ldx,
                    &a_2( i, i ), 1, zero, &y_2( 1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasTrans, i-1, n-i, -one, &a_2( 1, i+1 ),
                    lda, &y_2( 1, i ), 1, one, &y_2( i+1, i ), 1 );
        cblas_dscal( n-i, tauq_1( i ), &y_2( i+1, i ), 1 );
        /**
         *              Update A(i,i+1:n)
         **/
        cblas_dgemv(CblasColMajor, CblasNoTrans, n-i, i, -one, &y_2( i+1, 1 ),
                    ldy, &a_2( i, 1 ), lda, one, &a_2( i, i+1 ), lda );
        cblas_dgemv(CblasColMajor, CblasTrans, i-1, n-i, -one, &a_2( 1, i+1 ),
                    lda, &x_2( i, 1 ), ldx, one, &a_2( i, i+1 ), lda );
        /**
         *              Generate reflection P(i) to annihilate A(i,i+2:n)
         **/
        dlarfg( n-i, &a_2( i, i+1 ), &a_2( i, min( i+2, n ) ),
               lda, &taup_1( i ) );
        e_1( i ) = a_2( i, i+1 );
        a_2( i, i+1 ) = one;
        /**
         *              Compute X(i+1:m,i)
         **/
        cblas_dgemv(CblasColMajor, CblasNoTrans, m-i, n-i, one, &a_2(i+1,i+1),
                    lda, &a_2( i, i+1 ), lda, zero, &x_2( i+1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasTrans, n-i, i, one, &y_2(i+1, 1), ldy,
                    &a_2( i, i+1 ), lda, zero, &x_2( 1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasNoTrans, m-i, i, -one, &a_2( i+1, 1 ),
                    lda, &x_2( 1, i ), 1, one, &x_2( i+1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasNoTrans, i-1, n-i, one, &a_2( 1, i+1 ),
                    lda, &a_2( i, i+1 ), lda, zero, &x_2( 1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasNoTrans, m-i, i-1, -one, &x_2(i+1,1),
                    ldx, &x_2( 1, i ), 1, one, &x_2( i+1, i ), 1 );
        cblas_dscal( m-i, taup_1( i ), &x_2( i+1, i ), 1 );
      }
    }
  } else {
    /**
     *        Reduce to lower bidiagonal form
     **/
    for (i=1 ; i<=nb ; i+=1) {
      /**
       *           Update A(i,i:n)
       **/
      cblas_dgemv(CblasColMajor, CblasNoTrans, n-i+1, i-1, -one, &y_2( i, 1 ),
                  ldy, &a_2( i, 1 ), lda, one, &a_2( i, i ), lda );
      cblas_dgemv(CblasColMajor, CblasTrans, i-1, n-i+1, -one, &a_2(1,i), lda,
                  &x_2( i, 1 ), ldx, one, &a_2( i, i ), lda );
      /**
       *           Generate reflection P(i) to annihilate A(i,i+1:n)
       **/
      dlarfg( n-i+1, &a_2( i, i ), &a_2( i, min( i+1, n ) ), lda,
             &taup_1( i ) );
      d_1( i ) = a_2( i, i );
      if( i<m ) {
        a_2( i, i ) = one;
        /**
         *              Compute X(i+1:m,i)
         **/
        cblas_dgemv(CblasColMajor, CblasNoTrans, m-i, n-i+1, one, &a_2(i+1,i),
                    lda, &a_2( i, i ), lda, zero, &x_2( i+1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasTrans, n-i+1, i-1, one, &y_2(i,1), ldy,
                    &a_2( i, i ), lda, zero, &x_2( 1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasNoTrans, m-i, i-1, -one, &a_2(i+1,1),
                    lda, &x_2( 1, i ), 1, one, &x_2( i+1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasNoTrans, i-1, n-i+1, one, &a_2(1,i),
                    lda, &a_2( i, i ), lda, zero, &x_2( 1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasNoTrans, m-i, i-1, -one, &x_2(i+1,1),
                    ldx, &x_2( 1, i ), 1, one, &x_2( i+1, i ), 1 );
        cblas_dscal( m-i, taup_1( i ), &x_2( i+1, i ), 1 );
        /**
         *              Update A(i+1:m,i)
         **/
        cblas_dgemv(CblasColMajor, CblasNoTrans, m-i, i-1, -one, &a_2(i+1,1),
                    lda, &y_2( i, 1 ), ldy, one, &a_2( i+1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasNoTrans, m-i, i, -one, &x_2( i+1, 1 ),
                    ldx, &a_2( 1, i ), 1, one, &a_2( i+1, i ), 1 );
        /**
         *              Generate reflection Q(i) to annihilate A(i+2:m,i)
         **/
        dlarfg( m-i, &a_2( i+1, i ), &a_2( min( i+2, m ), i ), 1,
               &tauq_1( i ) );
        e_1( i ) = a_2( i+1, i );
        a_2( i+1, i ) = one;
        /**
         *              Compute Y(i+1:n,i)
         **/
        cblas_dgemv(CblasColMajor, CblasTrans, m-i, n-i, one, &a_2(i+1,i+1),
                    lda, &a_2( i+1, i ), 1, zero, &y_2( i+1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasTrans, m-i, i-1, one, &a_2( i+1, 1 ),
                    lda, &a_2( i+1, i ), 1, zero, &y_2( 1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasNoTrans, n-i, i-1, -one, &y_2(i+1, 1),
                    ldy, &y_2( 1, i ), 1, one, &y_2( i+1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasTrans, m-i, i, one, &x_2(i+1, 1), ldx,
                    &a_2( i+1, i ), 1, zero, &y_2( 1, i ), 1 );
        cblas_dgemv(CblasColMajor, CblasTrans, i, n-i, -one, &a_2(1, i+1), lda,
                    &y_2( 1, i ), 1, one, &y_2( i+1, i ), 1 );
        cblas_dscal( n-i, tauq_1( i ), &y_2( i+1, i ), 1 );
      }
    }
  }
  return;
  /**
   *     End of DLABRD
   **/
}



void dlacpy( char uplo, long m, long n, double a[], long lda,
            double b[], long ldb )
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
#undef b_2
#define b_2(a1,a2) b[a1-1+ldb*(a2-1)]
#undef a_2
#define a_2(a1,a2) a[a1-1+lda*(a2-1)]
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLACPY copies all or part of a two-dimensional matrix A to another
   *  matrix B.
   *
   *  Arguments
   *  =========
   *
   *  UPLO    (input) CHARACTER*1
   *          Specifies the part of the matrix A to be copied to B.
   *          = 'U':      Upper triangular part
   *          = 'L':      Lower triangular part
   *          Otherwise:  All of the matrix A
   *
   *  M       (input) INTEGER
   *          The number of rows of the matrix A.  M >= 0.
   *
   *  N       (input) INTEGER
   *          The number of columns of the matrix A.  N >= 0.
   *
   *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
   *          The m by n matrix A.  If UPLO = 'U', only the upper triangle
   *          or trapezoid is accessed; if UPLO = 'L', only the lower
   *          triangle or trapezoid is accessed.
   *
   *  LDA     (input) INTEGER
   *          The leading dimension of the array A.  LDA >= max(1,M).
   *
   *  B       (output) DOUBLE PRECISION array, dimension (LDB,N)
   *          On exit, B = A in the locations specified by UPLO.
   *
   *  LDB     (input) INTEGER
   *          The leading dimension of the array B.  LDB >= max(1,M).
   *
   *  =====================================================================
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
    for (j=1 ; j<=n ; j+=1) {
      for (i=1 ; i<=min( j, m ) ; i+=1) {
        b_2( i, j ) = a_2( i, j );
      }
    }
  } else if( lsame( uplo, 'l' ) ) {
    for (j=1 ; j<=n ; j+=1) {
      for (i=j ; i<=m ; i+=1) {
        b_2( i, j ) = a_2( i, j );
      }
    }
  } else {
    for (j=1 ; j<=n ; j+=1) {
      for (i=1 ; i<=m ; i+=1) {
        b_2( i, j ) = a_2( i, j );
      }
    }
  }
  return;
  /**
   *     End of DLACPY
   **/
}
