/*
 * $Id: dg.h,v 1.2 2010-08-22 17:44:05 dhmunro Exp $
 * Headers for LAPACK and BLAS routines in this directory.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef DG_H_LAPACK
#define DG_H_LAPACK

#include "plugin.h"

#ifndef YLAPACK_NOALIAS
/* names referenced in PROTOTYPE DOCUMENT comments in i0/matrix.i:
   ygtsv ygesv ygetrf ygecox ygelx ygelss ygesvx
 */
# define dgtsv ygtsv
# define dgesv ygesv
# define dgetf2 ygetf2
# define dgetrf ygetrf
# define dgetrs ygetrs
# define dlaswp ylaswp
# define ilaenv ylaenv
# define dgecon ygecon
# define dlacon ylacon
# define dlatrs ylatrs
# define drscl yrscl
# define dlabad ylabad
# define dlamch ylamch
# define dlamc1 ylamc1
# define dlamc2 ylamc2
# define dlamc3 ylamc3
# define dlamc4 ylamc4
# define dlamc5 ylamc5
# define dgels ygels
# define dormqr yormqr
# define dorm2r yorm2r
# define dormlq yormlq
# define dorml2 yorml2
# define dgeqrf ygeqrf
# define dgeqr2 ygeqr2
# define dgelqf ygelqf
# define dgelq2 ygelq2
# define dlarf ylarf
# define dlarfg ylarfg
# define dlarft ylarft
# define dlarfb ylarfb
# define dlapy2 ylapy2
# define dlange ylange
# define dlascl ylascl
# define dlaset ylaset
# define dlassq ylassq
# define dlabrd ylabrd
# define dgebd2 ygebd2
# define dgebrd ygebrd
# define dormbr yormbr
# define dorg2r yorg2r
# define dorgqr yorgqr
# define dorgl2 yorgl2
# define dorglq yorglq
# define dorgbr yorgbr
# define dlartg ylartg
# define dlasr ylasr
# define dlasv2 ylasv2
# define dlas2 ylas2
# define dbdsqr ybdsqr
# define dlacpy ylacpy
# define dgelss ygelss
# define dgesvd ygesvd
/* these three are not LAPACK names, only change them for consistency */
# define dgecox ygecox
# define dgelx ygelx
# define dgesvx ygesvx

#else
# ifndef YCBLAS_NOALIAS
#  define YCBLAS_NOALIAS 1
# endif
#endif

/*--- dgtsv.c (tridiagonal solver) ---*/
PLUG_API void dgtsv( long n, long nrhs, double dl[], double d[], double du[],
                     double b[], long ldb, long *info );

/*--- dgesv.c (LU decomposition solver) ---*/
PLUG_API void dgesv( long n, long nrhs, double a[], long lda, long ipiv[],
                     double b[], long ldb, long *info );
PLUG_API void dgetf2( long m, long n, double a[], long lda, long ipiv[],
                      long *info );
PLUG_API void dgetrf( long m, long n, double a[], long lda, long ipiv[],
                      long *info );
PLUG_API void dgetrs( char trans, long n, long nrhs, double a[], long lda,
                      long ipiv[], double b[], long ldb, long *info );
PLUG_API void dlaswp( long n, double a[], long lda, long k1, long k2,
                      long ipiv[], long incx );
PLUG_API long ilaenv( long ispec, char *name, char *opts,
                      long n1, long n2, long n3,long n4 );

/*--- dgecon.c (estimate condition number) ---*/
PLUG_API void dgecon( char norm, long n, double a[], long lda, double anorm,
                      double *rcond, double work[], long iwork[],long *info );
PLUG_API void dlacon( long n, double v[], double x[], long isgn[],
                      double *est, long *kase );
PLUG_API void dlatrs( char uplo, char trans, char diag, char normin, long n,
                      double a[], long lda, double x[], double *scale,
                      double cnorm[], long *info );
PLUG_API void drscl( long n, double sa, double sx[], long incx );

PLUG_API void dlabad( double *small, double *large );
PLUG_API double   dlamch( char cmach );
PLUG_API void dlamc1( long *beta, long *t, int *rnd, int *ieee1 );
PLUG_API void dlamc2( long *beta, long *t, int *rnd, double *eps,
                      long *emin, double *rmin, long *emax, double *rmax );
PLUG_API double   dlamc3( double a, double b );
PLUG_API void dlamc4( long *emin, double start, long base );
PLUG_API void dlamc5( long beta, long p, long emin, int ieee,
                      long *emax, double *rmax );

/*--- dgels.c (QR/LQ solver - least squares sense) ---*/
PLUG_API void dgels( char trans, long m, long n, long nrhs,
                     double a[], long lda, double b[], long ldb,
                     double work[], long lwork,long *info );
PLUG_API void dormqr( char side, char trans, long m, long n, long k,
                      double a[], long lda, double tau[], double c[], long ldc,
                      double work[], long lwork, long *info );
PLUG_API void dorm2r( char side, char trans, long m, long n, long k,
                      double a[], long lda, double tau[],
                      double c[], long ldc,double work[], long *info );
PLUG_API void dormlq( char side, char trans, long m, long n, long k,
                      double a[], long lda, double tau[], double c[], long ldc,
                      double work[], long lwork, long *info );
PLUG_API void dorml2( char side, char trans, long m, long n, long k,
                      double a[], long lda, double tau[], double c[], long ldc,
                      double work[], long *info );
PLUG_API void dgeqrf( long m, long n, double a[], long lda, double tau[],
                      double work[], long lwork, long *info );
PLUG_API void dgeqr2( long m, long n, double a[], long lda, double tau[],
                      double work[], long *info );
PLUG_API void dgelqf( long m, long n, double a[], long lda, double tau[],
                      double work[], long lwork, long *info );
PLUG_API void dgelq2( long m, long n, double a[], long lda, double tau[],
                      double work[], long *info );
PLUG_API void dlarf( char side, long m, long n, double v[], long incv,
                     double tau, double c[], long ldc, double work[] );
PLUG_API void dlarfg( long n, double *alpha, double x[], long incx,
                      double *tau );
PLUG_API void dlarft( char direct, char storev, long n, long k,
                      double v[], long ldv, double tau[],double t[],long ldt );
PLUG_API void dlarfb( char side, char trans, char direct, char storev,
                      long m, long n, long k, double v[], long ldv,
                      double t[], long ldt, double c[], long ldc,
                      double work[], long ldwork );

PLUG_API double   dlapy2( double x, double y );
PLUG_API double   dlange( char norm, long m, long n, double a[], long lda,
                          double work[] );
PLUG_API void dlascl( char type, long kl, long ku, double cfrom, double cto,
                      long m, long n, double a[], long lda, long *info );
PLUG_API void dlaset( char uplo, long m, long n, double alpha, double beta,
                      double a[], long lda );
PLUG_API void dlassq( long n, double x[], long incx,
                      double *scale, double *sumsq );

/*--- dgelss.c (SVD solver - least squares sense) ---*/
PLUG_API void dlabrd( long m, long n, long nb, double a[], long lda,
                      double d[], double e[], double tauq[], double taup[],
                      double x[], long ldx, double y[],long ldy );
PLUG_API void dgebd2( long m, long n, double a[], long lda,
                      double d[], double e[], double tauq[], double taup[],
                      double work[], long *info );
PLUG_API void dgebrd( long m, long n, double a[], long lda,
                      double d[], double e[], double tauq[], double taup[],
                      double work[], long lwork,long *info );
PLUG_API void dormbr( char vect, char side, char trans, long m, long n, long k,
                      double a[], long lda, double tau[], double c[],long ldc,
                      double work[], long lwork, long *info );
PLUG_API void dorg2r( long m, long n, long k, double a[], long lda,
                      double tau[], double work[], long *info );
PLUG_API void dorgqr( long m, long n, long k, double a[], long lda,
                      double tau[], double work[], long lwork, long *info );
PLUG_API void dorgl2( long m, long n, long k, double a[], long lda,
                      double tau[], double work[], long *info );
PLUG_API void dorglq( long m, long n, long k, double a[], long lda,
                      double tau[], double work[], long lwork, long *info );
PLUG_API void dorgbr( char vect, long m, long n, long k, double a[], long lda,
                      double tau[], double work[], long lwork, long *info );
PLUG_API void dlartg( double f, double g, double *cs, double *sn, double *r );
PLUG_API void dlasr( char side, char pivot, char direct, long m, long n,
                     double c[], double s[], double a[], long lda );
PLUG_API void dlasv2( double f, double g, double h,
                      double *ssmin, double *ssmax, double *snr,
                      double *csr, double *snl, double *csl );
PLUG_API void dlas2( double f, double g, double h,
                     double *ssmin, double *ssmax );
PLUG_API void dbdsqr( char uplo, long n, long ncvt, long nru, long ncc,
                      double d[], double e[], double vt[], long ldvt,
                      double u[],long ldu, double c[], long ldc,
                      double work[], long *info );
PLUG_API void dlacpy( char uplo, long m, long n, double a[], long lda,
                      double b[], long ldb );
PLUG_API void dgelss( long m, long n, long nrhs, double a[], long lda,
                      double b[], long ldb, double s[], double rcond,
                      long *rank,double work[], long lwork, long *info );

/*--- dgesvd.c (returns SVD) ---*/
PLUG_API void dgesvd( char jobu, char jobvt, long m, long n,
                      double a[], long lda, double s[], double u[], long ldu,
                      double vt[], long ldvt,
                      double work[], long lwork, long *info );

/*--- dgyor.c (LAPACK wrappers) ---*/
extern void dgecox( long norm, long n, double a[], long lda, double anorm,
                    double *rcond, double work[], long iwork[],long *info );
extern void dgelx( long itrn, long m, long n, long nrhs,
                   double a[], long lda, double b[], long ldb,
                   double work[], long lwork,long *info );
extern void dgesvx( long job, long m, long n, double a[], long lda,
                    double s[], double u[], long ldu, double vt[], long ldvt,
                    double work[], long lwork, long *info );

/*--- dgblas.c (BLAS routines) ---*/
#include "cblasy.h"

#endif
