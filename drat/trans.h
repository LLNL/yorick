/*
 * $Id: trans.h,v 1.1 2005-09-18 22:04:57 dhmunro Exp $
 * Routines for transporting a ray through a cylindrical mesh.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef TRANS_H
#define TRANS_H
#include "track.h"

/* ---------------------------------------------------------------------- */

/* Solve the transport equation assuming that both the opacity and
 * source function are constant within each zone.  Return the overall
 * absorption and self-emission along the ray.
 */
extern void
FlatSource(double *opac, double *source,        /* opacity, emissivity */
           long kxlm, long ngroup,              /* opac, source dimens */
           RayPath *path,                       /* ray path through mesh */
           double *absorb, double *selfem,      /* attenuation factor and
                                                 * self-emission for
                                                 * entire path */
           double *work);                       /* scratch space --
                                                   3*(path->ncuts-1) */

/* Solve the transport equation assuming that the source function
 * varies linearly with optical depth across a zone.  The opacity is
 * assumed to be constant within each zone.  (Hence, opac must be zone
 * centered, while source is point centered.)  Return the overall
 * absorption and self-emission along the ray.
 */
extern void
LinearSource(double *opac, double *source,      /* opacity, emissivity */
             long kxlm, long ngroup,            /* opac, source dimens */
             RayPath *path,                     /* ray path through mesh */
             double *absorb, double *selfem,    /* attenuation factor and
                                                 * self-emission for
                                                 * entire path */
             double *work);                     /* scratch space --
                                                   3*(path->ncuts-1)+1 */

/* Reduce x[i+1]= a[i]*x[i]+b[i] for i=0,...,n-1 to
 *          x[n]= a[0]*x[0]+b[0]     -i.e.- return a[0], b[0]
 * May use an algorithm which clobbers a[1:n-1], b[1:n-1] as well.
 */
extern void Reduce(double *a, double *b, long n);

/* The LinearSource transport solver requires a point centered source
 * function; the given source function is zone centered.
 * This point centering scheme is local to the four zones surrounding
 * the point, with weights which are large for zones whose optical
 * depth (along either k or l) is near 1.
 * The algorithm requires 4*(kmax*(lmax+1)+1) doubles as working space.
 *
 * An unrelated Milne condition is applied for points which lie on
 * the vacuum boundary of the problem.
 */
extern void PtCenterSource(double *opac, double *source, long kxlm,
                           long ngroup, Mesh *mesh, Boundary *boundary,
                           double *work);

/* ---------------------------------------------------------------------- */
#endif
