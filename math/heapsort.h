/*
 * $Id: heapsort.h,v 1.1 2005-09-18 22:03:44 dhmunro Exp $
 * heapsort interface
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* __cplusplus is for version 2.0, c_plusplus for version 1.2 */
#ifdef __cplusplus
// extern "C" {
#endif

/* heapsort double, long, or string values
 * n is the number of elements, s is the stride
 *   that is, vals[ndxs[0]], vals[ndxs[s]], ... vals[ndxs[n*s-s]]
 *   are to be sorted
 * only the values in ndxs[] are moved, so that
 *   vals[ndxs[0]] <= vals[ndxs[s]] <= ... <= vals[ndxs[n*s-s]]
 *   on exit
 */
extern void hs_d_sort(long n, long s, double *vals, long *ndxs);
extern void hs_l_sort(long n, long s, long *vals, long *ndxs);
extern void hs_q_sort(long n, long s, char **vals, long *ndxs);

#ifdef __cplusplus
}
#endif
