/*
 * $Id: hex24.c,v 1.1 2005-09-18 22:05:49 dhmunro Exp $
 * use 24-tet decomposition to track rays through hex mesh
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "hex.h"

#define EDGE_BIT(face) (((face)&6)? ((face)&6) : 1)

/* ------------------------------------------------------------------------ */

void hex24_rays(HX_mesh *mesh, long n, real p[][3], real q[][3],
                int body, TK_result *result)
{
  TK_ray ray;
  long cell[4], cell0[4];
  real xy[15][3], matrix[5][3], qp0[3];
  int tet[4], i, j, odd, tri0[4];
  int use_enter= (mesh->start >= 0);
  void (*tracker)(HX_mesh *, TK_ray *, long cell[],
                  real xy[][3], int tet[], TK_result *)=
                    body? hex24b_track : hex24f_track;

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
    else           i= hex24_begin(mesh, &ray, cell, xy, tet);
    if (!i) {
      if (n && !start_from_orig && use_enter) {
        for (j=0 ; j<3 ; j++) matrix[3][j]= qp0[j];
        odd= update_transform(&ray, p[0], q[0], matrix, odd);
        if (ray.qr[2]>0.) { tri0[0]= tet[0];  tri0[1]= tet[1]; }
        else              { tri0[0]= tet[1];  tri0[1]= tet[0]; }
        tri0[2]= tet[2];  tri0[3]= tet[3];
        for (j=0 ; j<4 ; j++) cell0[j]= cell[j];
      }
      if (use_enter) i= hex24_enter(xy, tet);
      if (!i) tracker(mesh, &ray, cell, xy, tet, result);
    }
    if (i) ray_store(result, cell[0], (real)i, 1);
    if (!n) break;
    ray_init(&ray, p[1], q[1], matrix);
    for (j=0 ; j<4 ; j++) tet[j]= tri0[j];
    for (j=0 ; j<4 ; j++) cell[j]= cell0[j];
  }
}

/* ------------------------------------------------------------------------ */

int hex24_enter(real xy[][3], int tet[])
{
  int invert= tet[3];  /* invert flag from hex_enter */
  int i, corner;
  int fourth= tet[0]^tet[1]^tet[2];
  int edge= (tet[0]&tet[1]&tet[2]) ^ (tet[0]|tet[1]|tet[2]) ^ 7;

  /* initialize tet[3] and corner */
  corner= fourth^7;
  if ((corner^edge)==tet[2]) corner= 2;
  else                       corner= (corner^edge)==tet[1];
  tet[3]= ((tet[0]&edge)!=0) | (edge&6) | 8;

  /* load face center into xy scratch space */
  for (i=0 ; i<3 ; i++)
    xy[tet[3]][i]=
      0.25*(xy[tet[0]][i]+xy[tet[1]][i]+xy[tet[2]][i]+xy[fourth][i]);

  if (tet_traverse(xy, tet)==corner) {             /* rare */
    tet[3]= fourth;
    if (tet_traverse(xy, tet)==corner) return 4;   /* really rare */
  }

  tet[3]= invert;
  return 0;
}

/* ------------------------------------------------------------------------ */

void hex24f_track(HX_mesh *mesh, TK_ray *ray, long cell[],
                  real xy[][3], int tet[], TK_result *result)
{
  real s;
  int center, edge, face, flag, reflected=0;
  int invert= tet[3];  /* invert flag from hex_enter, hex24_enter */
  static real dummy= 0.;
  real *qp_compute= result? 0 : &dummy;

  /* initialize tet[3], center, and face */
  if (tet[2]&8) center= 2;
  else          center= (tet[1]&8)!=0;
  face= tet[center]&7;
  edge= EDGE_BIT(face);
  if (!(edge&invert)) face^= 1;
  flag= (center==2)? 0 : center+1;
  tet[3]= tet[flag]^tet[3-flag-center]^edge^7;
  tet[3]= 8 | (tet[3]&6) | ((tet[3]&tet[flag])!=0);

  /* store initial point */
  ray_store(result, cell[0], ray->qr[2]*tri_intersect(xy, tet), 1);

  for (;;) {
    /* grab new face of current cell */
    hex_face(mesh, cell[0], face, ray, invert, xy);
    hex24_face(face, invert, xy, 0);
    if (reflected) {
      reflected = 0;
      ray_certify(ray, xy, tet, 14);
    }

    /* track ray through hex using face-centered 24-tet decomposition
     * there are three types of tets:
     * (0) edge tets have two adjacent hex corners and the two hex
     *          face centers adjacent to that edge
     * (1) body tets have four face centers (part of the central octahedron)
     * (2) corner tets have one hex corner and the three adjacent hex
     *          face centers
     * this loop is only on edge tets */
    for (tet_traverse(xy,tet) ; !(tet[3]&8) ; tet_traverse(xy,tet)) {
      if (tet[2]&8) edge= (tet[1]&8)? tet[0] : tet[1];
      else          edge= tet[2];
      edge^= tet[3];    /* of edge tet just traversed */
      /* get third face center for this corner tet and track through */
      tet[3]= 8 | (edge&6) | ((edge&tet[3])==0);
      tet_traverse(xy, tet);

      if (!(tet[3]&8)) {
        /* track through central octahedron
         * arbitrarily choose face opposite tet[0] as fourth
         * -- tet[1] or tet[2] would work just as well */
        tet[3]= tet[0]^1;
        edge= tet[0]&6;   /* 0, 2, or 4 axis of octahedron */
        for (tet_traverse(xy,tet) ; (tet[3]&6)!=edge ; tet_traverse(xy,tet))
          tet[3]^= 1;
        /* track across exit corner tet */
        if (tet[0]&1) tet[3]= EDGE_BIT(tet[0]);
        else          tet[3]= 0;
        if (tet[1]&1) tet[3]|= EDGE_BIT(tet[1]);
        if (tet[2]&1) tet[3]|= EDGE_BIT(tet[2]);
        tet_traverse(xy, tet);
      }

      /* back in an edge tet from corner tet */
      edge= EDGE_BIT(tet[3]);
      if (tet[2]&8) tet[3]= (tet[1]&8)? tet[0] : tet[1];
      else          tet[3]= tet[2];
      tet[3]^= edge;
    }

    s= ray->qr[2]*tri_intersect(xy, tet);
    if (!result && s>0.) break;  /* reached hex24_begin goal point */
    if (ray_store(result, cell[0], s, 0)) break;   /* looping? */

    if (tet[2]&8) center= 2;
    else          center= (tet[1]&8)!=0;
    face= (tet[center]&7);
    edge= EDGE_BIT(face);
    if (edge&invert) face^= 1;

    flag= hex_step(mesh, cell, face);
    if (flag) {
      if (flag!=2) break;
      if (ray_reflect(ray, xy, tet, qp_compute, (int *)0)) {
        int i= center? center-1 : 2;
        int j= i^center^3;
        int t= tet[i];
        tet[i]= tet[j];
        tet[j]= t;
      }
      hex_face(mesh, cell[0], face^1, ray, invert, xy);
      hex24_face(face^1, invert, xy, 0);
      reflected = 1;
    } else {
      invert^= edge;
    }
  }
}

/* ------------------------------------------------------------------------ */

void hex24b_track(HX_mesh *mesh, TK_ray *ray, long cell[],
                  real xy[][3], int tet[], TK_result *result)
{
  real s;
  int i, center, edge, face, flag, reflected=0;
  int invert= tet[3];  /* invert flag from hex_enter, hex24_enter */
  static real dummy= 0.;
  real *qp_compute= result? 0 : &dummy;

  /* initialize tet[3], center, edge, and face */
  if (tet[2]&8) center= 2;
  else          center= (tet[1]&8)!=0;
  face= tet[center]&7;
  edge= EDGE_BIT(face);
  if (!(edge&invert)) face^= 1;
  tet[3]= 14;

  /* store initial point */
  ray_store(result, cell[0], ray->qr[2]*tri_intersect(xy, tet), 1);

  for (;;) {
    /* grab new face of current cell */
    hex_face(mesh, cell[0], face, ray, invert, xy);
    hex24_face(face, invert, xy, 1);
    if (reflected) {
      reflected = 0;
      ray_certify(ray, xy, tet, 15);
    }

    /* track ray through hex using body-centered 24-tet decomposition
     * each of the 24 tets is identical; they all consist of two
     * corners, one face center, and the body center of the hex
     * initially the body center xy[14] is the apex tet[3]
     * when tet_traverse discards xy[14] we exit the hex */
    for (i=tet_traverse(xy,tet) ; tet[3]!=14 ; i=tet_traverse(xy,tet)) {
      if (center==i) {
        /* move to a new face */
        int v0= (tet[0]&8)!=0;  /* v0, v1 are the two hex corners */
        int v1= (v0 || (tet[1]&8))? 2 : 1;
        edge^= tet[v0]^tet[v1]^7;
        tet[3]= 8 | (edge&6) | ((tet[v0]&edge)!=0);
        center= 3;
      } else {
        /* remain on current face,
         * discarded point flips to diagonally opposite corner */
        tet[3]^= edge^7;
        if (center==3) center= i;
      }
    }
    if (center==3) center= i;

    s= ray->qr[2]*tri_intersect(xy, tet);
    if (!result && s>0.) break;  /* reached hex24_begin goal point */
    ray_store(result, cell[0], s, 0);

    face= (tet[center]&7);
    edge= EDGE_BIT(face);
    if (edge&invert) face^= 1;

    flag= hex_step(mesh, cell, face);
    if (flag) {
      if (flag!=2) break;
      if (ray_reflect(ray, xy, tet, qp_compute, (int *)0)) {
        int i= center? center-1 : 2;
        int j= i^center^3;
        int t= tet[i];
        tet[i]= tet[j];
        tet[j]= t;
      }
      hex_face(mesh, cell[0], face^1, ray, invert, xy);
      hex24_face(face^1, invert, xy, 1);
      reflected = 1;
    } else {
      invert^= edge;
    }
  }
}

/* ------------------------------------------------------------------------ */

static void face0(real xy[][3]);
static void face1(real xy[][3]);
static void face2(real xy[][3]);
static void face3(real xy[][3]);
static void face4(real xy[][3]);
static void face5(real xy[][3]);
static void (*facen[6])(real xy[][3])= {
  &face0, &face1, &face2, &face3, &face4, &face5 };

void hex24_face(int face, int invert, real xy[][3], int body)
{
  int i;
  if (EDGE_BIT(face)&invert) face^= 1;  /* convert from mesh to xy index */
  facen[face](xy);
  if (body) {
    face|= 8;
    for (i=0 ; i<3 ; i++) xy[14][i]= 0.5*(xy[face][i]+xy[face^1][i]);
  }
}

static void face0(real xy[][3])
{
  int i;
  for (i=0 ; i<3 ; i++) {
    xy[12][i]= 0.25*(xy[0][i]+xy[1][i]+xy[2][i]+xy[3][i]);
    xy[11][i]= 0.25*(xy[2][i]+xy[3][i]+xy[6][i]+xy[7][i]);
    xy[13][i]= 0.25*(xy[4][i]+xy[5][i]+xy[6][i]+xy[7][i]);
    xy[10][i]= 0.25*(xy[0][i]+xy[1][i]+xy[4][i]+xy[5][i]);
    xy[8][i]=  0.25*(xy[0][i]+xy[2][i]+xy[4][i]+xy[6][i]);
  }
}

static void face1(real xy[][3])
{
  int i;
  for (i=0 ; i<3 ; i++) {
    xy[12][i]= 0.25*(xy[0][i]+xy[1][i]+xy[2][i]+xy[3][i]);
    xy[11][i]= 0.25*(xy[2][i]+xy[3][i]+xy[6][i]+xy[7][i]);
    xy[13][i]= 0.25*(xy[4][i]+xy[5][i]+xy[6][i]+xy[7][i]);
    xy[10][i]= 0.25*(xy[0][i]+xy[1][i]+xy[4][i]+xy[5][i]);
    xy[9][i]=  0.25*(xy[1][i]+xy[3][i]+xy[5][i]+xy[7][i]);
  }
}

static void face2(real xy[][3])
{
  int i;
  for (i=0 ; i<3 ; i++) {
    xy[8][i]=  0.25*(xy[0][i]+xy[2][i]+xy[4][i]+xy[6][i]);
    xy[13][i]= 0.25*(xy[4][i]+xy[5][i]+xy[6][i]+xy[7][i]);
    xy[9][i]=  0.25*(xy[1][i]+xy[3][i]+xy[5][i]+xy[7][i]);
    xy[12][i]= 0.25*(xy[0][i]+xy[1][i]+xy[2][i]+xy[3][i]);
    xy[10][i]= 0.25*(xy[0][i]+xy[1][i]+xy[4][i]+xy[5][i]);
  }
}

static void face3(real xy[][3])
{
  int i;
  for (i=0 ; i<3 ; i++) {
    xy[8][i]=  0.25*(xy[0][i]+xy[2][i]+xy[4][i]+xy[6][i]);
    xy[13][i]= 0.25*(xy[4][i]+xy[5][i]+xy[6][i]+xy[7][i]);
    xy[9][i]=  0.25*(xy[1][i]+xy[3][i]+xy[5][i]+xy[7][i]);
    xy[12][i]= 0.25*(xy[0][i]+xy[1][i]+xy[2][i]+xy[3][i]);
    xy[11][i]= 0.25*(xy[2][i]+xy[3][i]+xy[6][i]+xy[7][i]);
  }
}

static void face4(real xy[][3])
{
  int i;
  for (i=0 ; i<3 ; i++) {
    xy[10][i]= 0.25*(xy[0][i]+xy[1][i]+xy[4][i]+xy[5][i]);
    xy[9][i]=  0.25*(xy[1][i]+xy[3][i]+xy[5][i]+xy[7][i]);
    xy[11][i]= 0.25*(xy[2][i]+xy[3][i]+xy[6][i]+xy[7][i]);
    xy[8][i]=  0.25*(xy[0][i]+xy[2][i]+xy[4][i]+xy[6][i]);
    xy[12][i]= 0.25*(xy[0][i]+xy[1][i]+xy[2][i]+xy[3][i]);
  }
}

static void face5(real xy[][3])
{
  int i;
  for (i=0 ; i<3 ; i++) {
    xy[10][i]= 0.25*(xy[0][i]+xy[1][i]+xy[4][i]+xy[5][i]);
    xy[9][i]=  0.25*(xy[1][i]+xy[3][i]+xy[5][i]+xy[7][i]);
    xy[11][i]= 0.25*(xy[2][i]+xy[3][i]+xy[6][i]+xy[7][i]);
    xy[8][i]=  0.25*(xy[0][i]+xy[2][i]+xy[4][i]+xy[6][i]);
    xy[13][i]= 0.25*(xy[4][i]+xy[5][i]+xy[6][i]+xy[7][i]);
  }
}

/* ------------------------------------------------------------------------ */

static int hex24_pierce(HX_mesh *mesh, TK_ray *ray, long cell,
                        real xy[][3], int tri[]);

int hex24_begin(HX_mesh *mesh, TK_ray *ray, long cell[],
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

  /* find centroid of initial cell, store in xy[0] */
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
    if (hex24_pierce(mesh, &ray1, cell[0], xy, tri)) return 1;

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
    hex24f_track(mesh, &ray1, cell, xy, tri, (TK_result *)0);

    /* re-initialize original ray to be consistent with any
     * reflections suffered during tracking */
    for (j=0 ; j<3 ; j++) matrix[3][j]= qp0[j];
    update_transform(&ray1, p, q, matrix, 0);
    for (j=0 ; j<3 ; j++) q[j]= ray->q[j];
    ray_init(ray, p, q, matrix);
  }

  /* find entry face triangle
   * -- failure here is a bug or rounding error? */
  return hex24_pierce(mesh, ray, cell[0], xy, tri);
}

static void pierce24_setup(real xy[][3], int tri[], int t);

static int hex24_pierce(HX_mesh *mesh, TK_ray *ray, long cell,
                        real xy[][3], int tri[])
{
  int t, t_min, test_tri[3];
  real s, s_min;

  /* the importance of tri[3] is that the call to the tracker
   * following this entry routine will assume that only one face
   * (determined by tri[3]) of the xy array has been loaded
   * - unlike the hex_enter entry point routine, this routine loads
   * all 8 corners of the cell, and so must merely set tri[3]
   * to be consistent with the current meaning of the points in xy */
  tri[3]= 0;

  /* move all 8 points of cell into xy scratch array */
  hex_face(mesh, cell, 0, ray, tri[3], xy);
  hex_face(mesh, cell, 1, ray, tri[3], xy);
  for (t=0 ; t<3 ; t++) xy[8][t]=
    0.25*(xy[0][t]+xy[2][t]+xy[4][t]+xy[6][t]);
  hex24_face(1, tri[3], xy, 0);

  /* check all 24 face triangles for entry and exit points
   * -- remember most negative s */
  t_min= -1;
  s_min= 1.e35;
  for (t=0 ; t<24 ; t++) {
    pierce24_setup(xy, test_tri, t);
    s = tri_find(xy, test_tri, ray->qr[2]);
    if (s < s_min) s_min = s, t_min= t;
  }
  if (t_min < 0) return 1;

  pierce24_setup(xy, tri, t_min);
  return 0;
}

static void pierce24_setup(real xy[][3], int tri[], int t)
{
  /* based on hex_init */
  int face= ((unsigned int)t)>>2;
  int i= ((unsigned int)face)>>1;
  int k= i? i-1 : 2;
  int quad[4];

  quad[0]= (t&4)? (1<<i) : 0;
  quad[1]= quad[0] | (1<<(i^k^3));
  /* this time, put them in cyclic order */
  quad[3]= quad[0] | (1<<k);
  quad[2]= quad[1] | quad[3];

  t= t&3;
  tri[0]= quad[t++];  tri[1]= quad[(t&4)? 0 : t];
  tri[2]= 8 | face;
  tri_check(xy, tri);
}

/* ------------------------------------------------------------------------ */
