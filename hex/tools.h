/*
 * $Id: tools.h,v 1.1 2005-09-18 22:05:51 dhmunro Exp $
 * headers and descriptions for ray tracing toolkit
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef TK_TOOLS_H
#define TK_TOOLS_H

#ifndef real
#define real double
#endif

/* ------------------------------------------------------------------------ */

/* TK_result is opaque struct for building the result of
 * tracing a set of rays through a mesh.
 *   result= ray_result();
 *     mallocs and initializes result
 *   ray_store(result, cell, s, 1);
 *     begins a new ray -- cell is unused, and s is the position
 *     of the entry point for the new ray
 *   ray_store(result, cell, s, 0);
 *     appends ray intersection point exiting cell at s
 *     a single result struct can hold many rays
 *   ray_store(result, cell, s, 2);
 *     signals that current ray is re-entering mesh at s
 *   n= ray_collect(result, 0, 0);
 *     return number of intersections stored in result so far
 *   ray_collect(result, cell, s, origin);
 *     copy intersection values into contiguous cell and s arrays
 *     cell[] is an array of the form
 *       [countA, cell1, cell2, cell3, ..., countB, cell1, cell2, ...]
 *       where each count is the number of intersection points on a
 *       contiguous section of the ray, and is followed by count-1
 *       cell index values; counts can be negative to indicate that
 *       a single ray has re-entered the mesh
 *     s[] is the corresponding array of positions along the ray
 *       [s0, s1, s2, s3, ..., s0, s1, s2, ...]
 *   ray_reset(result);
 *     reset result to 0 intersections
 *   ray_free(result);
 *     free all memory associated with result, pointer becomes invalid
 *   nrays= ray_reduce(len_cell, cell, 0, 0);
 *   nlist= malloc(sizeof(long)*nrays);
 *   nrays= ray_reduce(len_cell, cell, s, nlist, slims);
 *     compress the cell and s arrays returning nrays and nlist[nrays],
 *     taking successive differences of s and moving the counts from
 *     cell to nlist
 *     if slims non-0, must be slims[nrays][2] [smin,smax] for each ray
 *   ray_integ(nrays, nlist, ngroup, transp, selfem, result)
 *     perform sums and products of transp and selfem
 *     if ngroup<0, group index is final index in transp, selfem
 */
typedef struct TK_result TK_result;
extern TK_result *ray_result(void);
extern int ray_store(TK_result *result, long cell, real s, int first);
extern long ray_collect(TK_result *result, long *cell, real *s, long orig);
extern void ray_reset(TK_result *result);
extern void ray_free(TK_result *result);
extern long ray_reduce(long len, long *cell, real *s, long *nlist,
                       real slims[][2]);
extern void ray_integ(long nr, long *nlist, long ng,
                      real *transp, real *selfem, real *result);

/* ------------------------------------------------------------------------ */

typedef struct TK_ray TK_ray;
struct TK_ray {
  /* describe the ray in space, used to partially project mesh coordinates
   *
   *   p      a point on the ray
   *   qr     reciprocal ray direction [q0/q2,q1/q2,1/q2]
   *   order  permutation of mesh axes required to make q2 largest
   *          component of q in absolute value
   *      that is, order[0] is the index in mesh coordinates
   *      corresponding to index 0 in p, qr, or qp, order[1]
   *      in the mesh corresponds to p[1], and order[2] to p[2]
   */
  real p[3], qr[3];
  int order[3];

  /* describe reflection history of ray
   *
   *   q      normalized ray direction, in non-permuted (mesh) order
   *   qp     vector perpendicular to ray, in permuted order
   *          -- this is the normal to the boundary plane during
   *             the entry search (unnormalized)
   *   odd    1 if odd number of reflections, 0 if even number
   *      -- qp, odd valid only during entry search
   */
  real q[3], qp[3];
  int odd;
};

/* ------------------------------------------------------------------------ */

extern int tet_traverse(real xy[][3], int tet[]);
/*
 * tetrahedron traversal machinery for straight rays
 * Parameters:
 *   xy[][3] (input) scratch array containing corner coordinates
 *   tet[4] (in/out) indices of tet coordinates in xy
 * Assumptions:
 *   ray enters triangle (tet[0],tet[1],tet[2]), whose area is
 *   non-zero, and area element is positive in projected coordinates
 * Output:
 *   tet[3] swapped with tet[return value], such that the two
 *   above assumptions ray_reflect (tet[0],tet[1],tet[2]) on output
 *   for the exit face from the tet
 * Notes:
 *   if ray hits an edge or the apex point tet[3], choose the
 *     triangle with largest area as the exit (makes loops impossible)
 *   xy[i][2] are unused
 */

extern real tri_intersect(real xy[][3], int tri[]);
/*
 * z= tri_intersect(tri, xy)   finds intersection of ray with triangle
 * Parameters:
 *   xy[][3] (input) are the xyz coordinates of the triangle
 *   tri[3]  (input) are the indices into xy (assumed a small working array)
 *                   of the triangle; the 2D area of tri[0:2] is non-zero
 * Return value:
 *   return value is z value where (x,y) equals (0,0)
 */

/* ------------------------------------------------------------------------ */

extern int tri_traverse(real qp[], real xy[][3], int tri[], real dot[]);
/*
 *    triangle traversing machinery for ray entry search
 * discard= tri_traverse(qp, xy, tri, dot)
 * Parameters:
 *   qp[2]    (input) normal to plane on which to search for entry
 *   xy[][3]  (input) scratch array containing corner coordinates
 *   tri[3]   (in/out) indices of triangle coordinates in xy
 *   dot[2]   (in/out) dot[0]>=0. and dot[1]<=0. are dot products
 *                     of plane with tri[0:1] corners, dot[] is
 *                     updated here(see below)
 * Assumptions:
 *   at least one of dot[0:1] is non-zero
 *   routine guarantees dot[0]>=0. and dot[1]<=0. with at least
 *   one non-zero on output as well
 * Output:
 *   tri[2] swapped with tri[return value], such that the two
 *     above assumptions ray_reflect (tri[0],tri[1]) on output
 *     for the exit edge from the triangle
 *   dot for tri[2] is computed, then becomes dot[return value]
 * Notes:
 *   if dot computes to zero, tri swaps with smaller of ABS(dot[0:1])
 *   xy[i][2] are unused
 */

extern real tri_find(real xy[][3], int tri[], real qr2);
/*
 * test whether triangle contains (0,0)
 * Parameters:
 *   xy[][3] (input) scratch array containing corner coordinates
 *   tri[3]  (input) indices of tri coordinates in xy
 *   qr2     (input) ray->qr[2]
 * Assumptions:
 *   triangle area is non-negative
 * Output:
 *   returns s on ray if (0,0) interior to tri, else 1e35
 *     always returns 1e35 if area is 0
 * Notes:
 *   algorithm identical to tri_intersect, used initializing ray track
 */

extern void tri_check(real xy[][3], int tri[]);
/*
 * test whether triangle area is non-negative, swap tri[0] and tri[1]
 *   so on output area is always non-negative
 * Parameters:
 *   xy[][3] (input) scratch array containing corner coordinates
 *   tri[3] (in/out) indices of tri coordinates in xy
 * Notes:
 *   xy[i][2] are unused
 */

extern int edge_test(real xy[][3], int tri[], real dot[], int flags[]);
/*
 *   test whether entry point has been found
 * hit_miss= edge_test(xy, tri, dot, flags)
 * Parameters:
 *   xy[][3]   (input) partially projected coordinates
 *   tri[2]    (input) indices of test edge in xy
 *   dot[4]   (in/out) dot[0]>=0, dot[1]<=0 are dot products of tri[0:1]
 *                     with normal to entry plane (input only)
 *                     dot[2] is previous "x" value, which is updated
 *                       to "x" on this edge by tri_test
 *                     dot[3] is typical scale length for changes in xy
 *   flags[3] (in/out) flags[0] = index of "x" direction in xy (0 or 1)
 *                     flags[1] = 1 if dx<0 means we're on the entry
 *                       side of the boundary (where the ray can enter)
 *                     flags[2] = 1 if we've ever been on entry side
 *                       flags[0:1] are input only, flags[2] updated
 * Return value:
 *   0  means the entry search should continue
 *   1  means the ray hit the boundary between previous edge and this edge
 *   2  means the ray misses the boundary
 */

extern int entry_setup(TK_ray *ray, real xy[][3], int tri[],
                       real dot[], int flags[]);
/*
 *   set up for entry point search
 * discard= entry_setup(ray, xy, tri, dot, flags)
 * Parameters:
 *   ray      (in/out) qp is output
 *   xy[][3]   (input) partially projected coordinates
 *   tri[3]   (in/out) indices of boundary triangle in xy
 *                     on entry, these are ordered so that triangle 012
 *                     surface element points toward mesh interior
 *                     on exit, they have been permuted to look as if
 *                     tri_traverse had just been called
 *   dot[4]   (output) dot[0]>=0, dot[1]<=0 are dot products of tri[0:1]
 *                     with normal to entry plane -- as by tri_traverse
 *                     dot[2] is previous "x" value -- as by edge_test
 *                     dot[3] is typical scale length for changes in xy
 *   flags[3] (output) flags[0] = index of "x" direction in xy (0 or 1)
 *                     flags[1] = 1 if dx<0 means we're on the entry
 *                       side of the boundary (where the ray can enter)
 *                     flags[2] = 1 if initial triangle on entry side
 * Return value:
 *   0 or 1 depending on whether output tri[0] or tri[1] swapped
 *          with tri[2], as for tri_traverse
 *   2 if ray lies in plane of boundary triangle
 * Actions:
 * (1) Select a point inside the initial triangle but not on the ray,
 *     set ray->qp to be normal to the entry plane so determined.
 * (2) Set the "x" direction (flags[0]) as the one least parallel to qp.
 * (3) Compute dot product of all three points with qp to determine
 *     which side of the entry plane each is on.  Two are always on
 *     one side and one on the other.
 * (4) Compute the two intersections of the entry plane with the initial
 *     triangle, and also to which side of this line in the entry plane
 *     the mesh lies.
 * (5) Select a direction for the entry search: Always toward the ray if
 *     initial triangle is on entry side of boundary, either toward or
 *     away from q[2] if on the exit side, depending on the value of
 *     the external variable interior_boundary.
 * (6) Load dot, flags, and tri as required.  May need to change sign of
 *     qp in order to make dot[0]>0, dot[1]<0.  On exit, 012 is an odd
 *     permutation of the order on entry, as after tri_traverse.
 */
extern int interior_boundary;

/* ------------------------------------------------------------------------ */

extern int ray_reflect(TK_ray *ray, real xy[][3], int tri[],
                       real dot[], int flags[]);
/*
 * ray_reflect(ray, xy, tri, dot, flags)
 * Parameters:
 *   ray      (in/out) the ray to reflect
 *   xy[][3]   (input) partially projected coordinates
 *                     *not updated* even though changes in ray change xy
 *   tri[3]    (input) indices into xy of triangle in reflection plane
 *   dot[3]   (in/out) dot[2] is updated if flags!=0 (dot[0:1] correct)
 *   flags[3] (in/out) flags==0 skips entry calculation, if non-zero:
 *                     ray->qp is updated
 *                     dot[2] and flags[0:2] updated
 *                     entry updates presume that tri[0:1] is boundary
 *                     edge, while tri[2] is inside mesh (not on boundary)
 * Return value:
 *   0  input tri handedness unchanged
 *   1  input tri changed handedness
 */

extern int ray_certify(TK_ray *ray, real xy[][3], int tri[], int nxy);
/*
 * ray_certify(ray, xy, tri, dot, flags)
 *   - workaround roundoff error in ray reflections
 * Parameters:
 *   ray      (in/out) the ray to adjust if it is outside tri
 *   xy[][3]   (input) partially projected coordinates
 *   tri[3]    (input) indices into xy of triangle in reflection plane
 *   nxy      (in/out) number of xy coordinates to adjust if ray adjusted
 * Return value:
 *   0  input ray unchanged
 *   1  input ray->p and xy adjusted
 *  -1  input triangle has negative or zero area
 */

/* ------------------------------------------------------------------------ */

extern void ray_init(TK_ray *ray, real p[], real q[], real matrix[][3]);
/*
 * ray_init(ray, p, q, matrix)
 * Parameters:
 *   ray         (output) the ray to initialize
 *   p[3]         (input) point on the ray
 *   q[3]         (input) normalized ray direction
 *   matrix[5][3] (input) transformation matrix to apply to p, q
 *                        matrix[3] is offset applied to p only
 *                        matrix==0 means no tranform
 *                        matrix[4] is
 *                        point on previous ray in mesh coordinates
 *                        corresponding to matrix[3] (only if matrix!=0)
 * Action:
 *   ray->qp is zeroed, the rest initialized properly
 */

extern int update_transform(TK_ray *ray, real p0[], real q0[],
                            real matrix[][3], int odd);
/*
 * odd= update_transform(ray, p0, q0, matrix, odd)
 * Parameters:
 *   ray          (output) the ray after entry search
 *   p0[3]         (input) point on the ray in mesh coordinates
 *   q0[3]         (input) normalized ray direction in mesh coordinates
 *   matrix[5][3] (in/out) transformation matrix to apply to p, q
 *                         matrix[3] is offset applied to p only on output
 *                           on input, must be qp0 vector after entry_setup
 *                           but before any calls to reflect_ray
 *                         matrix[4] is set to input p0
 *   odd           (input) non-zero if matrix is a reflection
 * Return value:
 *   odd for output matrix
 * Action:
 *   First applies inverse (transpose) matrix to matrix[3] to find
 *     qp0 in mesh coordinates.
 *   Next, computes new matrix[0:2] as the tranform that takes mesh
 *     coordinates to multiply reflected coordinates.
 *   Finally, stores ray->p in matrix[3].
 */

/* ------------------------------------------------------------------------ */

#endif
