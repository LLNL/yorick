/*
 * $Id: regul.h,v 1.1 2005-09-18 22:05:49 dhmunro Exp $
 * regular mesh specific tracking routines
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef TK_REGUL_H
#define TK_REGUL_H

#include "tools.h"

/* ------------------------------------------------------------------------ */

extern void reg_rays(long nxyz[], real **xyz, long n, real p[][3],
                     real q[][3], TK_result *result);
/*
 *   reg_rays(nxyz, xyz, n, p, q, result)
 * Parameters:
 *   nxyz[3]   (input) [imax, jmax, kmax]
 *   xyz[3][]  (input) [&x[imax], &y[jmax], &k[kmax]]
 *   n         (input) number of rays
 *   p[n][3]   (input) points on rays
 *   q[n][3]   (input) normalized ray directions
 *   result   (in/out) this ray appended to result
 */

extern void reg_track(long nxyz[], real **xyz, real p[], real q[],
                      TK_result *result);
/*
 *   reg_track(nxyz, xyz, p, q, result)
 * Parameters:
 *   nxyz[3]   (input) [imax, jmax, kmax]
 *   xyz[3][]  (input) [&x[imax], &y[jmax], &k[kmax]]
 *   p[3]      (input) point on rays
 *   q[3]      (input) normalized ray direction
 *   result   (in/out) this ray appended to result
 */

/* ------------------------------------------------------------------------ */

#endif
