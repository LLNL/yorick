/*
 * $Id: hex.h,v 1.1 2005-09-18 22:05:47 dhmunro Exp $
 * hexahedral mesh specific tracking routines
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef TK_HEX_H
#define TK_HEX_H

#include "tools.h"

/* ------------------------------------------------------------------------ */

typedef struct HX_block HX_block;
struct HX_block {
  long stride[3];         /* i, j, k strides for this block */
  long length[3];         /* i, j, k lengths for this block
                           * these are cumulative, so length[n]/stride[n]
                           * is number of coordinate points along n */
  long first;             /* global index of 1st coordinate in this block */
  long final;             /* global index of 1st coordinate in next block */
};

typedef struct HX_blkbnd HX_blkbnd;
struct HX_blkbnd {
  long block;             /* block index of neighboring cell */
  long cell;              /* global cell index of neighboring cell */
  int orient;             /* face orientation (0-23) required to map face
                           * in current cell to face in neighboring cell */
};

typedef struct HX_mesh HX_mesh;
struct HX_mesh {
  real (*xyz)[3];         /* multiblock vertex coordinate array */
  int orient;             /* current face orientation (0-23) */
  long *stride;           /* current pointer to array of 3 strides for xyz
                           * points into one of the blks arrays */
  long (*bound)[3];       /* per face indices into bnds list:
                           * bound[c][ijk]= 0 if face is not boundary
                           *              < 0 if face is problem boundary
                           *              = index+1 into bnds list
                           *                if face is block boundary */
  long nbnds;             /* number of block boundary faces in this mesh */
  HX_blkbnd *bnds;        /* bnds[nbnds] block boundary face descriptions */
  long nblks;             /* number of blocks in this mesh */
  HX_block *blks;         /* blks[nblks] block descriptions */
  long block;             /* current index into blks */
  long start;             /* face index = face (0-5) + 6*(cell index)
                           * of any face on the convex mesh boundary
                           * or -1-(cell index) for "central cell" to
                           * track from to given point p on ray */
};

extern int hex_enter(HX_mesh *mesh, TK_ray *ray, long cell[],
                     real xy[][3], int tri[], real qp0[]);
/*
 *   flag= hex_enter(mesh, ray, cell, xy, tri, qp0)
 * Parameters:
 *   mesh      (input) mesh
 *   ray      (in/out) may change if ray reflected
 *   cell[4]  (in/out) cell in which to begin entry search (also tri[0:2])
 *                     on exit, cell where ray enters mesh
 *   xy[8][3] (output) partially projected coordinates
 *   tri[4]   (in/out) indices of boundary triangle in xy
 *                     on entry, these are ordered so that triangle 012
 *                     surface element points toward mesh interior
 *                     on exit, tri[0:2] are base of entry tet
 *                     tri[3] is bits 0-7 determining along which
 *                     directions xy has been inverted -- both input
 *                     and output
 *   qp0[3]   (output) set to initial qp before search (if non-nil)
 * Return value:
 *   0 if entry found
 *   1 if clean miss
 *   2 if ray lies in plane of initial triangle
 */

extern int hex_init(HX_mesh *mesh, long cell[], int tri[]);
/*
 * initialize hex_enter cell and tri arrays
 *   flag= hex_init(mesh, cell, tri)
 * Parameters:
 *   mesh      (input) mesh
 *   cell[4]  (output) cell in which to begin entry search (also tri[0:2])
 *   tri[3]   (output) indices of boundary triangle in xy
 *                     area element of 012 points to mesh interior
 *                     triangulation determined by hex_triang
 * Return value:
 *   0 if success
 *   1 if mesh has no boundaries
 */

extern int hex_triang(int flag);
/*
 *   prev_flag= hex_step(flag)
 * Parameters:
 *   flag      (input) 0 for default triangulation (in hex_init)
 *                     1 for other triangulation
 *                     2 for no change
 * Return value:
 *   value of flag before hex_triang called
 */

extern int hex_step(HX_mesh *mesh, long cell[], int face);
/*
 *   flag= hex_step(mesh, cell, face)
 * Parameters:
 *   mesh      (input) mesh
 *   cell[4]  (in/out) cell in which to begin entry search (also tri[0:2])
 *   face      (input) 0, 2, or 4 (ijk) plus 1 for larger index
 * Return value:
 *   0 if no boundaries encountered
 *   1 if face was mesh boundary (cell unchanged)
 *   2 if face was reflecting boundary (cell unchanged)
 *   >3 other boundary index (periodic boundaries handled)
 */

extern void hex_face(HX_mesh *mesh, long cell, int face,
                     TK_ray *ray, int invert, real xy[][3]);
/*
 *   retrieve and partially project one quad face of hex
 *   hex_face(mesh, cell, face, ray, invert, xy)
 * Parameters:
 *   mesh      (input) mesh
 *   cell      (input) cell index in mesh (scalar)
 *   face      (input) 0, 2, or 4 (ijk) plus 1 for larger index
 *   ray       (input) ray determines partial projection
 *   invert    (input) bits 0-7 determining along which
 *                     directions xy should be inverted
 *   xy[8][3] (output) partially projected coordinates (4 change)
 */

extern void hex_edge(HX_mesh *mesh, long cell, int bndy, int face,
                     TK_ray *ray, int invert, real xy[][3]);
/*
 *   retrieve and partially project one edge of hex
 *   hex_edge(mesh, cell, bndy, face, ray, invert, xy)
 * Parameters:
 *   mesh      (input) mesh
 *   cell      (input) cell index in mesh (scalar)
 *   bndy      (input) 0, 2, or 4 (ijk) plus 1 for larger index
 *   face      (input) 0, 2, or 4 (ijk) plus 1 for larger index
 *                     the edge is the intersection of face and bndy
 *   ray       (input) ray determines partial projection
 *   invert    (input) bits 0-7 determining along which
 *                     directions xy should be inverted
 *   xy[8][3] (output) partially projected coordinates (2 change)
 */

/* ------------------------------------------------------------------------ */

extern void hex5_rays(HX_mesh *mesh, long n, real p[][3], real q[][3],
                      TK_result *result);
/*
 *   hex5_rays(mesh, n, p, q, result)
 * Parameters:
 *   mesh      (input) mesh
 *   n         (input) number of rays
 *   p[n][3]   (input) points on rays
 *   q[n][3]   (input) normalized ray directions
 *   result   (in/out) this ray appended to result
 */

extern void hex5_track(HX_mesh *mesh, TK_ray *ray, long cell[],
                       real xy[][3], int tet[], TK_result *result);
/*
 *   hex5_track(mesh, ray, cell, xy, tet, result)
 * Parameters:
 *   mesh      (input) mesh
 *   ray      (in/out) may change if ray reflected
 *   cell[4]  (in/out) cell in which to begin tracking (also tet[0:2])
 *   xy[8][3] (in/out) partially projected coordinates
 *   tet[4]   (in/out) indices of tet in xy
 *                     on entry, these are ordered so that triangle 012
 *                     surface element points toward mesh interior, and
 *                     tet[3] is set to bits 0-7 determining along which
 *                     directions xy has been inverted
 *   result   (in/out) this ray appended to result
 */

extern int hex5_begin(HX_mesh *mesh, TK_ray *ray, long cell[],
                      real xy[][3], int tri[]);
/*
 * like hex_enter except tracks from centroid of cell[] on entry
 * to ray->p, then backs up along ray to find entry point in
 * cell containing ray->p, which is returned in cell, xy, tri.
 * uses hex5_track to perform the tracking operation
 */

/* ------------------------------------------------------------------------ */

extern void hex24_rays(HX_mesh *mesh, long n, real p[][3], real q[][3],
                       int body, TK_result *result);
/*
 *   hex24_rays(mesh, n, p, q, body, result)
 * Parameters:
 *   mesh      (input) mesh
 *   n         (input) number of rays
 *   p[n][3]   (input) points on rays
 *   q[n][3]   (input) normalized ray directions
 *   body      (input) 0 to use face-centered decomposition,
 *                     1 to use body-centered decomposition
 *   result   (in/out) this ray appended to result
 */

extern void hex24f_track(HX_mesh *mesh, TK_ray *ray, long cell[],
                         real xy[][3], int tet[], TK_result *result);
extern void hex24b_track(HX_mesh *mesh, TK_ray *ray, long cell[],
                         real xy[][3], int tet[], TK_result *result);
/*
 *   hex24[fb]_track(mesh, ray, cell, xy, tet, result)
 * Parameters:
 *   mesh      (input) mesh
 *   ray      (in/out) may change if ray reflected
 *   cell[4]  (in/out) cell in which to begin tracking (also tet[0:2])
 *   xy[8][3] (in/out) partially projected coordinates
 *   tet[4]   (in/out) indices of tet in xy
 *                     on entry, these are ordered so that triangle 012
 *                     surface element points toward mesh interior, and
 *                     tet[3] is set to bits 0-7 determining along which
 *                     directions xy has been inverted
 *   result   (in/out) this ray appended to result
 */

extern int hex24_enter(real xy[][3], int tet[]);
/*
 *   flag= hex_enter(mesh, ray, cell, xy, tri, qp0);
 *   if (!flag) flag= hex24_enter(xy, tri);
 * Parameters:
 *   xy[14][3] (in/out) partially projected coordinates
 *   tri[4]    (in/out) indices of boundary triangle in xy
 *                      on entry, these are ordered so that triangle 012
 *                      surface element points toward mesh interior
 *                      on exit, tri[0:2] are base of entry tet
 *                      tri[3] is unchanged unless return non-0
 * Return value:
 *   0 if entry found
 *   4 if ray misses despite entering initial tri
 * Side Effects:
 *   computes the xy[face_center] for the entry triangle, then
 *   uses tet_traverse to adjust tri to the face-centered entry
 *   triangle
 */

extern void hex24_face(int face, int invert, real xy[][3], int body);
/*
 *   hex_face(mesh, cell[0], face, ray, invert, xy);
 *   hex24_face(face, invert, xy, body);
 * Parameters:
 *   face       (input) 0, 2, or 4 (ijk) plus 1 for larger index
 *   invert     (input) bits 0-7 determining along which
 *                      directions xy should be inverted
 *   xy[15][3] (output) partially projected coordinates (9 or 10 change)
 *   body       (input) 1 if xy[14] is to be set to body center, else 0
 * Side Effects:
 *   computes the xy[face_center] for all *except* entry face,
 *   optionally computes the body center xy[14]
 */

extern int hex24_begin(HX_mesh *mesh, TK_ray *ray, long cell[],
                       real xy[][3], int tri[]);
/*
 * like hex_enter except tracks from centroid of cell[] on entry
 * to ray->p, then backs up along ray to find entry point in
 * cell containing ray->p, which is returned in cell, xy, tri.
 * uses hex24_track to perform the tracking operation
 */

/* ------------------------------------------------------------------------ */

/* flag for trackers to start hex_entry from original point
 * determined by hex_init rather from previous ray entry point */
extern int start_from_orig;
extern int hex_startflag(int flag);

#endif
