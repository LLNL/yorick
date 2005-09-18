/*
 * $Id: cfft.h,v 1.1 2005-09-18 22:04:53 dhmunro Exp $
 * Swarztrauber complex FFT routines
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "plugin.h"

PLUG_API void cffti1 (long n,double wa[],long ifac[]);
PLUG_API void cffti (long n,double wsave[]);

PLUG_API void cfftf (long n,double c[],double wsave[]);
PLUG_API void cfftf1 (long n,double c[],double ch[],double wa[],long ifac[]);

PLUG_API void cfftb (long n,double c[],double wsave[]);
PLUG_API void cfftb1 (long n,double c[],double ch[],double wa[],long ifac[]);

/*
 *     extension to swarztrauber fft routines to allow for
 *     multidimensional ffts
 *
 *     call cfft2(idir, c, istd, n, n2, ws)
 *
 *     idir    1 for forward transform (cfftf),
 *            -1 for backward transform (cfftb)
 *     c      the complex array to be transformed (in place)
 *     istd   the stride of the dimension of c to be transformed
 *            istd=1 means the first dimension of c is to be transformed,
 *            istd=len1 where len1 is the length of the first dimension
 *                      means to transform the second dimension
 *            istd=len1*len2 transforms the third dimension, and so on
 *     n      the length of the dimension to be transformed
 *     n2     the product of all dimension lengths after the dimension
 *            to be transformed -- the transform of length n will be
 *            repeated a total of istd*n2 times
 *     ws     the working array filled by cffti
 *            if istd=1, only 4*n+15 elements of storage will be used,
 *            as for cfftf or cfftb
 *            if istd>1, ws needs to have 6*n+15 elements of storage
 *            (as a working array for the dimension being transformed)
 */
PLUG_API void cfft2(long idir, double c[], long istd, long n, long n2,
                    double ws[]);

PLUG_API void roll2(double a[], long ioff, long istd, long n, long n2,
                    double ws[]);
