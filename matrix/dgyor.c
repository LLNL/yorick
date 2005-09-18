/*
 * $Id: dgyor.c,v 1.1 2005-09-18 22:04:48 dhmunro Exp $
 * Yorick wrappers for LAPACK routines.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "dg.h"

/**  -- Yorick wrappers for LAPACK routines:
*      dgecon - estimates matrix condition number
*      dgels  - QR or LQ decomposition least square matrix solver
*      dgesvd - SVD decomposition routine
*     plus XERBLA routine appropriate to Yorick
**/

extern void YError(const char *);

extern int strcmp(const char *, const char *);


void dgecox( long norm, long n, double a[], long lda, double anorm,
            double *rcond, double work[], long iwork[],long *info )
{
  /**
   *  Purpose
   *  =======
   *
   *  DGECOX is a wrapper for DGECON, more easily callable from non-FORTRAN
   *  language routines (no gratuitous string argument).
   *
   *  Arguments
   *  =========
   *
   *  NORM    (input) INTEGER
   *          Specifies whether the 1-norm condition number or the
   *          infinity-norm condition number is required:
   *          = 1:           1-norm;
   *          = 0:           Infinity-norm.
   **/
  char nrm;
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if ( norm!=0 ) {
    nrm = '1';
  } else {
    nrm = 'i';
  }
  dgecon( nrm, n, a, lda, anorm, rcond, work, iwork, info );
  return;
}



void dgelx( long itrn, long m, long n, long nrhs,
           double a[], long lda, double b[], long ldb,
           double work[], long lwork,long *info )
{
  /**
   *  Purpose
   *  =======
   *
   *  DGELX is a wrapper for DGELS, more easily callable from non-FORTRAN
   *  language routines (no gratuitous string argument).
   *
   *  Arguments
   *  =========
   *
   *  ITRN    (input) INTEGER
   *          Specifies whether A is to be transposed:
   *          = 1:           transpose (TRANS == 'T' in DGELS);
   *          = 0:           (TRANS == 'N' in DGELS).
   **/
  char trn;
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if ( itrn!=0 ) {
    trn = 't';
  } else {
    trn = 'n';
  }
  dgels( trn, m, n, nrhs, a, lda, b, ldb, work, lwork, info );
  return;
}



void dgesvx( long job, long m, long n, double a[], long lda,
            double s[], double u[], long ldu, double vt[], long ldvt,
            double work[], long lwork, long *info )
{
  /**
   *  Purpose
   *  =======
   *
   *  DGESVX is a wrapper for DGESVD, more easily callable from non-FORTRAN
   *  language routines (no gratuitous string arguments).
   *
   *  Arguments
   *  =========
   *
   *  JOB     (input) INTEGER
   *          Specifies whether min(m,n) or full matrix output:
   *          = 1:           JOBU= JOBVT= 'A' in DGESVD;
   *          = 0:           JOBU= JOBVT= 'S' in DGESVD.
   **/
  char jb;
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if ( job!=0 ) {
    jb = 'a';
  } else {
    jb = 's';
  }
  dgesvd( jb, jb, m, n, a, lda, s, u, ldu, vt, ldvt,
         work, lwork, info );
  return;
}



void xerbla( char *srname, long info )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     February 29, 1992
   *
   *     .. Scalar Arguments ..*/
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  XERBLA  is an error handler for the LAPACK routines.
   *  It is called by an LAPACK routine if an input parameter has an
   *  invalid value.  A message is printed and execution stops.
   *
   *  Installers may consider modifying the STOP statement in order to
   *  call system-specific exception-handling facilities.
   *
   *  Modified for use with Yorick.  Calls FBLOWUP, unless these are the
   *  calls used to determine the workspace size for DGELS, DGELSS, or DGESVD.
   *
   *  Arguments
   *  =========
   *
   *  SRNAME  (input) CHARACTER*6
   *          The name of the routine which called XERBLA.
   *
   *  INFO    (input) INTEGER
   *          The position of the invalid parameter in the parameter list
   *          of the calling routine.
   *
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  if ( !strcmp(srname,"dgels ") && info==10 ) {
    return;
  } else if ( !strcmp(srname,"dgelss") && info==12 ) {
    return;
  } else if ( !strcmp(srname,"dgesvd") && info==13 ) {
    return;
  } else {
    YError("(bug) LAPACK input error -- xerbla called");
    return;
  }
}
