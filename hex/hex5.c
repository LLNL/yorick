/*
 * $Id: hex5.c,v 1.1 2005-09-18 22:05:49 dhmunro Exp $
 * use 5-tet decomposition to track rays through hex mesh
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "hex.h"

#define FACE_INDEX(edge, vertex) (((edge)&6) | (((edge)&(vertex))!=0))

/* ------------------------------------------------------------------------ */

void hex5_rays(HX_mesh *mesh, long n, real p[][3], real q[][3],
               TK_result *result)
{
  TK_ray ray;
  long cell[4], cell0[4];
  real xy[8][3], matrix[5][3], qp0[3];
  int tet[4], i, j, odd, tri0[4];
  int use_enter= (mesh->start >= 0);

  if (n<1) return;

  ray_init(&ray, p[0], q[0], (void *)0);
  for (i=0 ; i<5 ; i++) for (j=0 ; j<3 ; j++)
    matrix[i][j]= (real)(i==j);
  hex_init(mesh, cell, tet);
  for (j=0 ; j<3 ; j++) tri0[j]= tet[j];
  tet[3]= tri0[3]= 0;
  for (j=0 ; j<4 ; j++) cell0[j]= cell[j];

  for (odd=j=0 ; ; p++,q++) {
    n--;
    if (use_enter) i= hex_enter(mesh, &ray, cell, xy, tet, qp0);
    else           i= hex5_begin(mesh, &ray, cell, xy, tet);
    if (!i) {
      if (n && !start_from_orig && use_enter) {
        for (j=0 ; j<3 ; j++) matrix[3][j]= qp0[j];
        odd= update_transform(&ray, p[0], q[0], matrix, odd);
        if (ray.qr[2]>0.) { tri0[0]= tet[0];  tri0[1]= tet[1]; }
        else              { tri0[0]= tet[1];  tri0[1]= tet[0]; }
        tri0[2]= tet[2];  tri0[3]= tet[3];
        for (j=0 ; j<4 ; j++) cell0[j]= cell[j];
      }
      hex5_track(mesh, &ray, cell, xy, tet, result);
    } else {
      ray_store(result, cell[0], (real)i, 1);
    }
    if (!n) break;
    ray_init(&ray, p[1], q[1], matrix);
    for (j=0 ; j<4 ; j++) tet[j]= tri0[j];
    for (j=0 ; j<4 ; j++) cell[j]= cell0[j];
  }
}

/* ------------------------------------------------------------------------ */

void hex5_track(HX_mesh *mesh, TK_ray *ray, long cell[],
                real xy[][3], int tet[], TK_result *result)
{
  real s;
  int corner, edge, face, flag, reflected=0;
  int invert= tet[3];  /* invert flag from hex_enter */
  static real dummy= 0.;
  real *qp_compute= result? 0 : &dummy;
  /* hex_enter already has xy loaded with entry face */

  /* initialize tet[3] and corner */
  corner= tet[0]^tet[1]^tet[2];
  tet[3]= corner^7;
  edge= (tet[0]&tet[1]&tet[2]) ^ (tet[0]|tet[1]|tet[2]) ^ 7;
  if ((tet[3]^edge)==tet[2]) corner= 2;
  else                       corner= (tet[3]^edge)==tet[1];

  /* store initial point */
  ray_store(result, cell[0], ray->qr[2]*tri_intersect(xy, tet), 1);

  face= FACE_INDEX(edge,tet[3]^invert);
  for (;;) {
    /* grab new face of current cell (contains tet[3]) */
    hex_face(mesh, cell[0], face, ray, invert, xy);
    if (reflected) {
      reflected = 0;
      ray_certify(ray, xy, tet, 8);
    }

    /* track ray through hex using 5-tet decomposition */
    if (tet_traverse(xy, tet)==corner) {   /* cross first corner tet */
      tet[3]^= 7;       /* diagonally opposite corner in central tet */
      tet_traverse(xy, tet);                    /* cross central tet */
      tet[3]^= 7;               /* this will become new corner point */
      corner= tet_traverse(xy, tet);       /* cross final corner tet */
    }
    s= ray->qr[2]*tri_intersect(xy, tet);
    if (!result && s>0.) {          /* reached hex5_begin goal point */
      /* record which way this cell triangulated */
      tet[3] = invert;
      break;
    }
    if (ray_store(result, cell[0], s, 0)) break;   /* looping? */

    edge= tet[3]^tet[corner];    /* 1, 2, or 4 edge perp to new face */
    face= FACE_INDEX(edge,tet[3]^invert) ^ 1;

    flag= hex_step(mesh, cell, face);
    if (flag) {
      if (flag!=2) break;
      if (ray_reflect(ray, xy, tet, qp_compute, (int *)0)) {
        int i= corner? corner-1 : 2;
        int j= i^corner^3;
        int t= tet[i];
        tet[i]= tet[j];
        tet[j]= t;
      }
      hex_face(mesh, cell[0], face^1, ray, invert, xy);
      reflected = 1;
    } else {
      invert^= edge;
    }
  }
}

/* ------------------------------------------------------------------------ */

static int hex5_pierce(HX_mesh *mesh, TK_ray *ray, long cell,
                       real xy[][3], int tri[]);

int hex5_begin(HX_mesh *mesh, TK_ray *ray, long cell[],
               real xy[][3], int tri[])
{
  int i, j;
  long k;
  real ctop, p[3], q[3];

  /* check that strides are consistent with block */
  if (mesh->block != cell[1]) {
    mesh->block= cell[1];
    mesh->stride= mesh->blks[cell[1]].stride;
    mesh->orient= 0;
  }

  /* find centroid of initial cell, store in xy[2] */
  for (i=0 ; i<3 ; i++) {
    xy[0][i]= 0.0;
    for (j=0 ; j<8 ; j++) {
      k= cell[0];
      if (j&1) k-= mesh->stride[0];
      if (j&2) k-= mesh->stride[1];
      if (j&4) k-= mesh->stride[2];
      xy[0][i]+= mesh->xyz[k][i];
    }
    xy[0][i]*= 0.125;
  }

  /* compute direction from centroid of cell to ray->p */
  for (i=0, ctop=0.0 ; i<3 ; i++) {
    j= ray->order[i];
    q[j]= ray->p[i] - xy[0][j];  /* will become ray1.q */
    ctop+= q[j]*q[j];
    p[j]= ray->p[i];             /* will become ray1.p */
  }

  /* initialize tri for hex5_pierce */
  tri[0] = 0;
  tri[1] = 1;
  tri[2] = 2;
  tri[3] = hex_triang(2);

  if (ctop) {
    /* invent a fake ray from centroid to ray->p */
    TK_ray ray1;
    extern double sqrt(double);
    real matrix[5][3], qp0[3];

    ctop= 1./sqrt(ctop);
    for (i=0 ; i<3 ; i++) q[i]*= ctop;
    ray_init(&ray1, p, q, (void *)0);

    /* find face triangle through which fake ray enters cell[0]
     * -- this may fail only if centroid not inside cell[0] */
    if (hex5_pierce(mesh, &ray1, cell[0], xy, tri)) return 1;

    /* initialize ray1.qp to any vector perpendicular to q */
    qp0[ray1.order[0]]= ray1.qp[0]= 0.;
    ray1.qp[1]= q[ray1.order[2]];
    ray1.qp[2]= -q[ray1.order[1]];
    ctop= 1./sqrt(ray1.qp[1]*ray1.qp[1] + ray1.qp[2]*ray1.qp[2]);
    qp0[ray1.order[1]]= (ray1.qp[1]*= ctop);
    qp0[ray1.order[2]]= (ray1.qp[2]*= ctop);
    for (i=0 ; i<5 ; i++) for (j=0 ; j<3 ; j++)
      matrix[i][j]= (real)(i==j);

    /* track fake ray to cell containing ray->p */
    hex5_track(mesh, &ray1, cell, xy, tri, (TK_result *)0);

    /* re-initialize original ray to be consistent with any
     * reflections suffered during tracking */
    for (j=0 ; j<3 ; j++) matrix[3][j]= qp0[j];
    update_transform(&ray1, p, q, matrix, 0);
    for (j=0 ; j<3 ; j++) q[j]= ray->q[j];
    ray_init(ray, p, q, matrix);
  }

  /* find entry face triangle
   * -- failure here is a bug or rounding error? */
  return hex5_pierce(mesh, ray, cell[0], xy, tri);
}

static void pierce5_setup(real xy[][3], int tri[], int t, int flag);

static int hex5_pierce(HX_mesh *mesh, TK_ray *ray, long cell,
                       real xy[][3], int tri[])
{
  int triangle_flag = tri[3];  /* invert flag */
  int t, t_min, test_tri[3];
  real s, s_min;

  /* the importance of tri[3] is that the call to the tracker
   * following this entry routine will assume that only one face
   * (determined by tri[3]) of the xy array has been loaded
   * - unlike the hex_enter entry point routine, this routine loads
   * all 8 corners of the cell, and so must merely set tri[3]
   * to be consistent with the current meaning of the points in xy */
  tri[3]= 0;

  /* even more important, after the call to hex5_track in hex5_begin,
   * tri[0:2] and triangle_flag (=invert) indicates which way cell
   * was triangulated during fake ray tracking
   * -- must ensure that it is triangualted in same sense when real ray
   *    tracking begins, because initial point on real ray might not lie
   *    inside the oppositely tirangulated cell at all
   */
  {
    int face = ((tri[0]^tri[1]) | (tri[1]^tri[2])) ^ 7; /* 1 2 or 4 */
    int sense = (tri[0] ^ tri[1] ^ tri[2]) & (~face);
    /* sense=0 if diagonal from 01 to 10, sense=1 if from 00 to 11 */
    sense = (sense ^ ((unsigned int)sense>>1) ^ ((unsigned int)sense>>2)) & 1;
    /* sense reversed for upper faces */
    sense ^= (tri[0] & face) != 0;
    /* count invert flag parity */
    triangle_flag = (triangle_flag ^ ((unsigned int)triangle_flag>>1) ^
                     ((unsigned int)triangle_flag>>2)) & 1;
    /* finally, combine sense with inversion parity */
    triangle_flag ^= sense;
  }

  /* move all 8 points of cell into xy scratch array */
  hex_face(mesh, cell, 0, ray, tri[3], xy);
  hex_face(mesh, cell, 1, ray, tri[3], xy);

  /* check all 12 face triangles for entry and exit points
   * -- remember most negative s */
  t_min= -1;
  s_min= 1.e35;
  for (t=0 ; t<12 ; t++) {
    pierce5_setup(xy, test_tri, t, triangle_flag);
    s = tri_find(xy, test_tri, ray->qr[2]);
    if (s < s_min) s_min = s, t_min= t;
  }
  if (t_min < 0) return 1;

  pierce5_setup(xy, tri, t_min, triangle_flag);
  return 0;
}

static void pierce5_setup(real xy[][3], int tri[], int t, int flag)
{
  /* based on hex_init */
  int i= ((unsigned int)t)>>2;
  int upper = (t&2) != 0;
  int k= i? i-1 : 2;
  int quad[4], d0, p0;

  quad[0]= upper? (1<<i) : 0;
  quad[1]= quad[0] | (1<<(i^k^3));
  quad[2]= quad[0] | (1<<k);
  quad[3]= quad[1] | quad[2];

  if (flag ^ upper) {
    d0= 0;  p0= 2;
  } else {
    d0= 1;  p0= 0;
  }
  if (t&1) p0= 3-p0;

  tri[0]= quad[d0];    tri[1]= quad[3-d0];  tri[2]= quad[p0];
  tri_check(xy, tri);  /* swap 0 and 1 if necessary */
}

/* ------------------------------------------------------------------------ */
