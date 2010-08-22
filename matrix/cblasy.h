/*
 * $Id: cblasy.h,v 1.2 2010-08-22 17:44:05 dhmunro Exp $
 * CBLAS routines used by yorick
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* Basic Linear algebra Subprograms Technical (BLAST) Forum Standard
 * August 21, 2001  (http://www.netlib.org/blas/blast-forum/)
 * Appendix B: Legacy BLAS
 */

#include "plugin.h"

#undef INT_IN

/* BLAST forum standard does two things I dont like */
#ifdef USE_CBLAS
# define INT_IN const int
# include <stddef.h>
# ifndef CBLAS_INDEX
/* this may not be correct, size_t (in stddef.h) is just a guess
 * if so, define CBLAS_INDEX to correct type
 * NB- since size_t unsigned, cannot do integer divide on result */
#  define CBLAS_INDEX size_t
# endif

/* this is more consistent with rest of yorick */
#else
# define INT_IN long
# define CBLAS_INDEX long
#endif

#ifndef YCBLAS_NOALIAS
# define xerbla yxerbla
# define cblas_ddot yblas_ddot
# define cblas_dnrm2 yblas_dnrm2
# define cblas_dasum yblas_dasum
# define cblas_idamax yblas_idamax
# define cblas_dswap yblas_dswap
# define cblas_dcopy yblas_dcopy
# define cblas_daxpy yblas_daxpy
# define cblas_drot yblas_drot
# define cblas_dscal yblas_dscal
# define cblas_dgemv yblas_dgemv
# define cblas_dtrmv yblas_dtrmv
# define cblas_dtrsv yblas_dtrsv
# define cblas_dger yblas_dger
# define cblas_dgemm yblas_dgemm
# define cblas_dtrmm yblas_dtrmm
# define cblas_dtrsm yblas_dtrsm
#endif

/*--- LAPACK error routine (see dgyor.c) ---*/
PLUG_API void xerbla( char *srname, long info );

/* these values are set in the standard */
enum CBLAS_ORDER {CblasRowMajor=101, CblasColMajor=102};
enum CBLAS_TRANSPOSE {CblasNoTrans=111, CblasTrans=112, CblasConjTrans=113};
enum CBLAS_UPLO {CblasUpper=121, CblasLower=122};
enum CBLAS_DIAG {CblasNonUnit=131, CblasUnit=132};
enum CBLAS_SIDE {CblasLeft=141, CblasRight=142};

/* level 1 */

PLUG_API double cblas_ddot(INT_IN n, const double *x, INT_IN incx,
                           const double *y, INT_IN incy);

PLUG_API double cblas_dnrm2(INT_IN n, const double *x, INT_IN incx);
PLUG_API double cblas_dasum(INT_IN n, const double *x, INT_IN incx);

PLUG_API CBLAS_INDEX cblas_idamax(INT_IN n, const double *x, INT_IN incx);

PLUG_API void cblas_dswap(INT_IN n, double *x, INT_IN incx, 
                          double *y, INT_IN incy);
PLUG_API void cblas_dcopy(INT_IN n, const double *x, INT_IN incx, 
                          double *y, INT_IN incy);
PLUG_API void cblas_daxpy(INT_IN n, const double alpha, const double *x,
                          INT_IN incx, double *y, INT_IN incy);

PLUG_API void cblas_drot(INT_IN n, double *x, INT_IN incx,
                         double *y, INT_IN incy,
                         const double c, const double  s);

PLUG_API void cblas_dscal(INT_IN n, const double alpha,
                          double *x, INT_IN incx);

/* level 2 */

PLUG_API void cblas_dgemv(const enum CBLAS_ORDER order,
                          const enum CBLAS_TRANSPOSE transa,
                          INT_IN m, INT_IN n,
                          const double alpha, const double *a, INT_IN lda,
                          const double *x, INT_IN incx, const double beta,
                          double *y, INT_IN incy);
PLUG_API void cblas_dtrmv(const enum CBLAS_ORDER order,
                          const enum CBLAS_UPLO uplo,
                          const enum CBLAS_TRANSPOSE transa,
                          const enum CBLAS_DIAG diag,
                          INT_IN n, const double *a, INT_IN lda, 
                          double *x, INT_IN incx);
PLUG_API void cblas_dtrsv(const enum CBLAS_ORDER order,
                          const enum CBLAS_UPLO uplo,
                          const enum CBLAS_TRANSPOSE transa,
                          const enum CBLAS_DIAG diag,
                          INT_IN n, const double *a, INT_IN lda,
                          double *x, INT_IN incx);

PLUG_API void cblas_dger(const enum CBLAS_ORDER order, INT_IN m, INT_IN n,
                         const double alpha, const double *x, INT_IN incx,
                         const double *y, INT_IN incy,
                         double *a, INT_IN lda);

/* level 3 */

PLUG_API void cblas_dgemm(const enum CBLAS_ORDER order,
                          const enum CBLAS_TRANSPOSE transa,
                          const enum CBLAS_TRANSPOSE transb,
                          INT_IN m, INT_IN n,
                          INT_IN k, const double alpha, const double *a,
                          INT_IN lda, const double *b, INT_IN ldb,
                          const double beta, double *c, INT_IN ldc);
PLUG_API void cblas_dtrmm(const enum CBLAS_ORDER order,
                          const enum CBLAS_SIDE side,
                          const enum CBLAS_UPLO uplo,
                          const enum CBLAS_TRANSPOSE transa,
                          const enum CBLAS_DIAG diag, INT_IN m, INT_IN n,
                          const double alpha, const double *a, INT_IN lda,
                          double *b, INT_IN ldb);
PLUG_API void cblas_dtrsm(const enum CBLAS_ORDER order,
                          const enum CBLAS_SIDE side,
                          const enum CBLAS_UPLO uplo,
                          const enum CBLAS_TRANSPOSE transa,
                          const enum CBLAS_DIAG diag, INT_IN m, INT_IN n,
                          const double alpha, const double *a, INT_IN lda,
                          double *b, INT_IN ldb);
