/*
 * $Id: hydram.h,v 1.1 2005-09-18 22:05:49 dhmunro Exp $
 * declare workers to convert hydra bnd specs into HX_boundary arrays
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef HYDRAM_H
#define HYDRAM_H

/* ------------------------------------------------------------------------ */

/* on input:
 *   blo[b][0]   - unused
 *   blo[b][1:3] - [ni, nj, nk] for this block
 * on output:
 *   blo[b][0]   - global offset to start of this block
 *   blo[b][1]   - stride for j in this block
 *   blo[b][2]   - stride for k in this block
 *   blo[b][3]   - total number of nodal points in this block
 * return value:
 *   largest ni*nj, nj*nk, or nk*ni in the entire mesh
 */
extern long hydra_blks(long nblks, long (*blo)[4]);

/* on input:
 *   ibnd        - if <0 the mark to place in the bound array
 *   scratch     - length is return from hydra_blks
 *   blos[4]     - returned from hydra_blks for s block
 *   blor[4]     - returned from hydra_blks for r block
 *   bnds[4]     - boundary description for s block
 *   bndr[4]     - boundary description for r block
 *    bnd[0]   - pn (+- 1 2 3) for this boundary
 *    bnd[1]   - jp (hydra in-memory 2nd dim stride) for this boundary
 *    bnd[2]   - kp (hydra in-memory 3rd dim stride) for this boundary
 *   n           - number of nodes in ndxr, ndxs lists
 *   ndxs[n]     - hydra in-memory indices of nodes in s block
 *   ndxr[n]     - hydra in-memory indices of corresponding nodes in r block
 *                 may be NULL to set bc instead of bnd
 *   mbnds       - if ndxr!=0, mbnds[ibnd] is the first blkbnd for this bnd
 *   rblock      - if ndxr!=0, the r mesh block
 * on output:
 *   bound[i][3] - set to ibnd on for all faces specified in ndxs[]
 * return value:
 *   ibnd for next call to hydra_bnd, or 0 if ibnd<0 on input
 *   <0 indicates failure (illegal or inconsistent input arrays)
 */
extern long hydra_bnd(long ibnd, long (*bound)[3], long *scratch,
                      long *blos, long *blor, long *bnds, long *bndr,
                      long n, long *ndxs,
                      long *ndxr, HX_blkbnd *mbnds, long rblock);

/* on input:
 *   ibnd        - if <0 the mark to place in the bound array
 *   blo[4]      - returned from hydra_blks for this block
 *   bnd[4]      - boundary description for this block
 *    bnd[0]   - pn (+- 1 2 3) for this boundary
 *    bnd[1]   - jp (hydra in-memory 2nd dim stride) for this boundary
 *    bnd[2]   - kp (hydra in-memory 3rd dim stride) for this boundary
 *   n           - number of nodes in ndxr, ndxs lists
 *   ndx[n]      - hydra in-memory indices of nodes in s block
 * on output:
 *   tbound[i][3] - set to ibnd on for all nodes specified in ndxs[]
 * return value:
 *   0-origin index along pn direction for this bc
 *   <0 indicates failure (illegal or inconsistent input arrays)
 */
extern long hydra_mrk(int ibnd, long (*tbound)[3], long *blo, long *bnd,
                      long n, long *ndx);

/* on input:
 *   tbound[i][3] - set to ibnd on for all nodes in block
 *   blo[4]       - returned from hydra_blks for this block
 *   n            - number of bcs in tcheck
 *   tcheck[2*n]  - tells which faces in tbound to scan:
 *     tcheck[0] - 1, 2, 3 for i, j, k, 0 means skip entry
 *     tcheck[1] - i j or k index value
 * on output:
 *   bound[i][3] - set to ibnd on for all faces implied by tbound
 * return value:
 *   0-origin start index
 *   <0 indicates no start cells
 */
extern long hydra_adj(long (*bound)[3], long (*tbound)[3],
                      long *blo, long n, long *tcheck);

/* ------------------------------------------------------------------------ */

#endif
