/*
 * $Id: bound.h,v 1.1 2005-09-18 22:04:53 dhmunro Exp $
 * Routines for finding the boundary of a cylindrical mesh.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef BOUND_H
#define BOUND_H

/* These function pointers are initialized to malloc, realloc, and free,
   but can be reset at runtime to an alternate memory manager.  */
extern void *(*Dmalloc)(long);
extern void *(*Drealloc)(void *, long);
extern void (*Dfree)(void *);

/* ---------------------------------------------------------------------- */

/* The Mesh structure includes pointers to the coordinate arrays
 * z[lmax][kmax] and r[lmax][kmax], the region number array
 * ireg[lmax+1][kmax] (which includes an extra row of region 0!),
 * and various flags which determine mesh symmetry conditions.
 */
typedef struct Mesh Mesh;
struct Mesh {
   long kmax, lmax;     /* mesh dimensions */
   long klmax;          /* kmax*lmax, overall number of points */
   double *z, *r;       /* kmax-by-lmax coordinate arrays */
   int *ireg;           /* kmax-by-(lmax+1) region number array --
                         * ireg[0][]= ireg[][0]= ireg[lmax][]= 0 */
   int zsym;            /* 1 if z=0 is symmetry plane, else 0
                           2 if RayTrackS (spherical tracker) */
};

/* ---------------------------------------------------------------------- */

/* Lists of boundary points and edges, normally running counterclockwise
 * around the bounded region (but see FindBoundaryPoints for flag to
 * alter boundary orientation)
 */
typedef struct Boundary Boundary;
struct Boundary {
   int zsym;            /* 1 if problem has z-symmetry, else 0 */
   long nk,nl;          /* number of k-edges and l-edges in boundary */
   long npoints;        /* number of boundary points */
   long *zone;          /* [npoints] arrays of entry zones and sides... */
   int *side;           /* the end of a contiguous section of boundary
                         * is marked by zone=0 */
   double *z, *r;       /* [npoints] arrays of boundary points */
};

/* Linked list of edges used to build Boundary lists */
typedef struct Edge Edge;
struct Edge {
   Edge *next;
   long zone, side;
};

/* ---------------------------------------------------------------------- */

/* Given a mesh and a region number, compute the boundary of that region
 * and present it in counterclockwise order in (z,r)-plane.
 * The working array is int work[2][klmax+kmax].
 * Split into kedges, ledges working arrays.
 * Note that for region=0, this returns the problem boundary clockwise.
 * If cw is non-zero, the boundary is returned in clockwise order (or
 * counterclockwise for region 0).
 * A quick check is performed first to determine whether the input
 * boundary is actually the mesh boundary.
 * Return 1 if boundary list had to be changed, 0 if list was correct.
 */
extern int FindBoundaryPoints(Mesh *mesh, int region, int cw,
                              Boundary *boundary, int *work);

/* Trim specific edges out of a boundary list made by FindBoundaryPoints. */
extern void
TrimBoundary(Boundary *trimmed,   /* Resulting trimmed boundary */
             Mesh *mesh,          /* Problem mesh */
             Boundary *boundary,  /* Initial boundary within mesh */
             long *rmlist,        /* rmlist[nsegs][2][2]= [[k1,l1],[k2,l2]]
                                   * are nsegs logical line segments, where
                                   * (k,l) are point coordinates and either
                                   * k1=k2 or l1=l2 */ 
             long nsegs,
             int *work);          /* work[2][klmax] */

/* Make a single boundary edge linked list element.  The edge list is
 * a temporary construct used by FindBoundaryPoints.
 */
extern Edge *
MakeEdge(long stride,           /* ==1 for k-edge, ==kmax for l-edge */
         long edgeIndex,        /* index into kedges or ledges array */
         int sign);             /* 1 (-1) if region left or below
                                 * (right or above) edgeIndex edge */

/* Free the storage used for an Edge, returning edge->next */
extern Edge *ReleaseEdge(Edge *edge);

/* Take one step along the boundary in the given direction starting at
 * the given edge.  Returns NULL at end of boundary.
 */
extern Edge *
WalkBoundary(int direction,     /* 0 clockwise, 1 counterclockwise */
             Edge *edge,        /* current edge */
             long kmax, long klmax,     /* mesh dimensions */
             int *kedges, int *ledges,  /* lists of edges not yet visited
                                         * 1 if region to left or below,
                                         * -1 if to right or above, 0 if
                                         * not a boundary edge --
                                         * UPDATED */
             long *nk, long *nl);       /* number of k or l edges --
                                         * UPDATED */

/* Extend boundary->zone and boundary->side arrays, initializing to
 * the values in the given linked list of edge(s)
 */
extern void
NewBoundaryEdges(Boundary *boundary,    /* boundary lists to be expanded */
              long nedges, Edge *edge); /* # and linked list of edges */

/* Create boundary->z and boundary->r arrays from mesh and
 * boundary->zone, boundary->side arrays.  (cw must be as FindBoundaryPoints)
 */
extern void MakeBoundaryZR(Boundary *boundary, int cw, Mesh *mesh);

/* Free the memory used for the zone, side, z, and r arrays in boundary */
extern void EraseBoundary(Boundary *boundary);

/* Create a Boundary with a given number of points, allocating zone,
 * side arrays, but NOT z, r (use MakeBoundaryZR). */
extern Boundary *MakeBoundary(int zsym, long nk, long nl, long npoints);

/* ---------------------------------------------------------------------- */
#endif
