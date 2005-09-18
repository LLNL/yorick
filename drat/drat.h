/*
 * $Id: drat.h,v 1.1 2005-09-18 22:04:54 dhmunro Exp $
 * High level interface for Drat (interpreter independent).
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef DRAT_H
#define DRAT_H

#include "trans.h"

/* ---------------------------------------------------------------------- */

typedef struct FullMesh FullMesh;
struct FullMesh {
  Mesh mesh;
  Boundary boundary;
  int *work;              /* 2*kmax*(lmax+1) working array required
                             by FindBoundaryPoints and TrimBoundary */
  long khold, lhold;      /* If >=0, DoPtCenter will add this entire
                             k-line (khold) oor l-line (lhold) to the
                             list of no-Milne edges.  */
};

/* Set mesh->mesh.? where ? is kmax, lmax, r, z, and zsym.
   Set mesh->work to 0 to initialize BOTH mesh->work and mesh->boundary.
   On the second and subsequent calls, you can either change the
   r and z pointers, or copy new r and z values -- the r and z
   arrays are yours to manage.
   Call UpdateMesh(mesh, ireg) to:
     1. Create or copy mesh->mesh.ireg from ireg -- mesh->mesh.ireg
        is larger than kmax-by-lmax, and is "owned" by drat.c.
        Use DiscardMesh to free it, or set it to zero and use
        Dfree to free it by hand.  If the input ireg==0, a default
        mesh->mesh.ireg will be created if necessary.
     2. Compute the new boundary zone list.  This is only done if
        ireg has changed since the previous call.
     3. Compute the boundary path in (z,r)-space.
     4. Allocate working space (mesh->work) if not already allocated.
        Again, this is "owned" by drat.c.  You must use Drealloc to
        reallocate it by hand if you change kmax or lmax.  */
extern void UpdateMesh(FullMesh *mesh, int *ireg);

/* DiscardMesh frees the mesh.ireg and work arrays allocated by SetupMesh,
   and erases the boundary.  Does NOT free input mesh pointer.  */
extern void DiscardMesh(FullMesh *mesh);

/* TrackRay is a higher level interface to RayTrack and RayTrackS:
   mesh, ray, and sLimits are inputs; path is an output.
   RayTrackS is used if mesh->mesh.zsym==2, otherwise RayTrack.  */
extern void TrackRay(FullMesh *mesh, Ray *ray, double *sLimits,
                     RayPath *path);

/* ---------------------------------------------------------------------- */
/* There are 2 high level transport equation integrators, depending
   on whether FlatSource or LinearSource is to be called.
   Note that the Ray *rays parameter can always be cast from
   type double (*)[6] -- the Ray struct is just 6 doubles.  */

/* PROTOTYPE
   void IntegFlat(double array opac, double array source,
                  long kxlm, long ngroup, double array rays, long nrays,
                  opaque mesh, double array slimits, double array result)
 */
extern void IntegFlat(double *opac, double *source,
                      long kxlm, long ngroup, Ray *rays, long nrays,
                      FullMesh *mesh, double *slimits, double *result);
extern void IntegLinear(double *opac, double *source,
                        long kxlm, long ngroup, Ray *rays, long nrays,
                        FullMesh *mesh, double *slimits, double *result);

/* memory management for workspace required by FlatSource, LinearSource */
extern double *IntegWorkspace(long ncuts);
extern void IntegClear(void);

/* ---------------------------------------------------------------------- */
/* The high level interface to PtCenterSource uses a FullMesh instead
   of a Mesh, and takes care of calling TrimBoundary if necessary.  */

/* PROTOTYPE
   void DoPtCenter(double array opac, double array source,
                   long kxlm, long ngroup, opaque mesh,
                   long array nomilne, long nedges)
 */
extern void DoPtCenter(double *opac, double *source,
                       long kxlm, long ngroup, FullMesh *mesh,
                       long *nomilne, long nedges);

/* ---------------------------------------------------------------------- */
#endif
