/*
 * $Id: track.h,v 1.1 2005-09-18 22:04:56 dhmunro Exp $
 * Routines for tracking a straight ray in 3D through a cylindrical mesh.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef TRACK_H
#define TRACK_H
#include "bound.h"

/* ---------------------------------------------------------------------- */

/* The result of a ray tracking operation is a RayPath: */
typedef struct RayPath RayPath;
struct RayPath {
   long maxcuts, ncuts; /* physical lengths, current number zones cut */
   long *zone;          /* [ncuts] array of zone entered, if last exit
                         * (into region 0) then zone=0 */
   double *ds;          /* [ncuts] path length in zone (0.0 if zone=0) */
   long *pt1, *pt2;     /* [ncuts] arrays, pt1->pt2 is edge of zone cut */
   double *f;           /* [ncuts] cut at pt1 + (f+0.5)*(pt2-pt1) */
   double fi,ff;        /* fraction of distance into initial (final)
                         * zone at which tracking started (stopped)
                         * ds in initial (final) zone is 1-fi (1-ff) of
                         * the full length of the ray within the zone */
};

/* Although the mesh lives in a 2-D cylindrical coordinate space, a Ray
 * is a directed line in Cartesian 3-D space.  The z-axis is common to
 * the two coordinate systems.  The x-axis coincides with the r-axis
 * in a standard (z,r) mesh plot (although, of course, r>=0, while x
 * can have either sign).  The y-axis comes out of the page of the
 * standard mesh plot, toward your face.  Hence, (x,y,z) is a right
 * handed coordinate frame.
 *
 * A ray is assumed to lie in a plane of constant y.  The angle theta
 * is the angle from the +z direction to the ray direction, with
 * theta=+pi/2 corresponding to the +x-direction.  (This is opposite
 * the convention in TDG/DIRT, but corresponds to phi=0 azimuth in
 * the usual spherical coordinate system, e.g.- Jackson's E&M book.)
 *
 * The redundant data in the Ray structure allows the ray tracker to
 * step from edge to edge without having to compute a lot of extra
 * square roots, e.g.- to get r=sqrt(x^2+y^2) or sin=sqrt(1-cos^2).
 * (The alternative approach, used in TDG/DIRT, is to describe the ray
 * in an invariant fashion, i.e.- independent of the current zone in
 * the tracking process.)
 */
typedef struct Ray Ray;
struct Ray {
   double cos, sin;     /* cos(theta), sin(theta) */
   double y, z, x;      /* any point on the ray...
                         * y remains fixed,
                         * while z and x step from edge to edge */
   double r;            /* (z,r) are cylindrical coordinates corresponding
                         * to (x,y,z)   --   r^2 = x^2 + y^2 */
};

/* Structure to store results of a ray-edge intersection calculation
 * made by ExitEdge.  There are potentially two solutions, the "exit"
 * solution, representing a left-to-right crossing in (z,r) space, and
 * the "entry" solution, representing a right-to-left crossing.
 * Neither solution necessarily exists.  Several intermediate results
 * of the calculation are saved here, as well as the two roots.
 */
typedef struct RayEdgeInfo RayEdgeInfo;
struct RayEdgeInfo {
   double dz, dr;       /* edge vector in (z,r) plane */
   double area;         /* a useful cross product (see EdgeExit) */
   double A, B, C;      /* Af^2+Bf+C=0 is equation for fx, fn --
                         * neither B nor C valid unless D>0 */
   double D;            /* sqrt(discriminant) if >0, else discriminant */
   double fx;           /* the left-to-right crossing (edge fraction) */
   int validx;          /* fx not valid unless this is true */
   double fn;           /* the right-to-left crossing (edge fraction) */
   int validn;          /* fn not valid unless this is true */
};

/* A ray enters the mesh to begin tracking at one or more EntryPoint's: */
typedef struct EntryPoint EntryPoint;
struct EntryPoint {
   EntryPoint *next;
   Ray ray;
   RayEdgeInfo info;
   long zone;
   int side;
   double f;
   double s0;
};

/* ---------------------------------------------------------------------- */

/* Given the mesh boundary (i.e.- list of boundary edges) and a ray,
 * return a linked list of entry point(s), ordered by increasing time.
 */
extern EntryPoint *FindEntryPoints(Boundary *boundary, Ray *ray);

extern void FreeEntryPoints(EntryPoint *entry);

/* Starting at the given entry point(s), track the ray through the mesh.
 * The path arrays will be lengthened if necessary, but never shortened.
 * sLimits[0] <= s[1]<=s[ncuts-2] <= sLimits[1], if sLimits[0]<sLimits[1]
 * on entry.  (Note that s[0] and s[ncuts-1] may lie outside range.)
 */
extern void RayTrack(Mesh *mesh, EntryPoint *entry, RayPath *path,
                     double *sLimits);

/* Track the ray through the mesh, assuming it represents a sphere.
 * The path arrays will be lengthened if necessary, but never shortened.
 * sLimits[0] <= s[1]<=s[ncuts-2] <= sLimits[1], if sLimits[0]<sLimits[1]
 * on entry.  (Note that s[0] and s[ncuts-1] may lie outside range.)
 */
extern void RayTrackS(Mesh *mesh, Ray *ray, RayPath *path, double *sLimits);

/* Find the intersections of a ray with an edge (z,r); return true if
 * and only if the ray cuts the edge segment from left to right.
 */
extern int
ExitEdge(Ray *ray, double z[2], double r[2],    /* inputs */
         int *after, /* int *notafter, */       /* updates */
         RayEdgeInfo *info);                    /* output */

/* Compute the path length from the current point on the ray to the
 * exit (left-to-right) intersection computed by ExitEdge.
 */
extern double RayPathLength(Ray *ray, RayEdgeInfo *info);

/* Compute the path length from the exit point to the entry point (>0
 * if entry AFTER exit) for the intersections computed by ExitEdge.
 */
extern double RayPathDifference(RayEdgeInfo *info);

/* Given a ray with current point entering a particular zone and side of
 * the mesh, find the corresponding exit.  The path length ds is >=0
 * unless the ray segment lies in the negative area part of a bowtied
 * zone, in which case ds<0.  Return the exit side index (0<=side<=3).
 * Update the current point on the ray to the exit point.
 */
extern int
ExitZone(Mesh *mesh, long zone,         /* input mesh and zone number */
         int side,                      /* input side number on entry */
         Ray *ray,                      /* updated to exit point */
         RayEdgeInfo *info[4],          /* info[3] is entry side on entry,
                                         * updated to exit side on exit */
         double *ds,                    /* output ray path length */
         double *f);                    /* output edge fraction */

/* Although algebraically exact formulas are used for the ExitEdge
 * calculation, roundoff errors may make the redundant information
 * in the Ray structure (y,z,x, and r) inconsistent.  PolishExit
 * attempts to restore precise self-consistency of the ray, without
 * moving the point off of the current edge (remembered in info).
 */
extern void PolishExit(Ray *ray, RayEdgeInfo *info, double *ds, double *f);

/* Through roundoff errors, it is possible for the exit point found
 * in ExitZone (or FindEntryPoints) to lie slightly beyond the
 * endpoints of the exit edge.  AdjustRayXY moves the ray back to
 * the nearby endpoint when this occurs.
 */
extern void AdjustRayXY(Ray *ray, double* z, double *r);

/* Last ditch effort by ExitZone to find the exit edge */
extern int FindLostRay(Ray *ray, RayEdgeInfo *info[4],
                       double z[4], double r[4], double dsx[4]);

/* Rearrange the linked list of entry points into increasing order */
extern EntryPoint *EntrySort(EntryPoint *entry);

/* Make ray path arrays longer */
extern void ExtendRayPath(RayPath *path, long pathinc);

/* Free all pointers in a RayPath (but NOT the input path pointer).  */
extern void EraseRayPath(RayPath *path);

/* Adjust point on ray (x,y,z) and (z,r) to be point such that normal
 * plane to ray passes through origin.
 */
void NormalizeRay(Ray *ray);

/* ---------------------------------------------------------------------- */

/********** DEFAULT VALUES SET IN track.c ********/

/* Global variables control the root polishing routine PolishExit */
extern int polishRoot;
extern double polishTol1;
extern double polishTol2;

/* Global variable controlling roundoff tolerance in FindLostRay */
/* Setting this too large could result in an infinite loop, but in
 * any such situation, FindLostRay will be unable to find the ray
 * no matter what the setting of findRayTol */
extern double findRayTol;

/* ---------------------------------------------------------------------- */

#endif
