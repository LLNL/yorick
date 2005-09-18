/*
 * $Id: hex.c,v 1.1 2005-09-18 22:05:47 dhmunro Exp $
 * generic tools for dealing with a hex mesh
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "hex.h"

#define FACE_INDEX(edge, vertex) (((edge)&6) | (((edge)&(vertex))!=0))
#define EDGE_BIT(face) (1<<((unsigned int)(face)>>1))

/* ------------------------------------------------------------------------ */

int hex_enter(HX_mesh *mesh, TK_ray *ray, long cell[],
              real xy[][3], int tri[], real qp0[])
{
  int bndy, edge, diag, face, invert, hit_miss, flag;
  real *qp= ray->qp;

  /* workspace for entry_setup, edge_test, and tri_traverse */
  real dot[4];
  int flags[3];

  /* check that strides are consistent with block */
  if (mesh->block != cell[1]) {
    mesh->block= cell[1];
    mesh->stride= mesh->blks[cell[1]].stride;
  }

  /* initialize xy for given cell, bndy face */
  invert= tri[3];     /* stored from previous call */
  face= (tri[0]&tri[1]&tri[2]) ^ (tri[0]|tri[1]|tri[2]);
  bndy= face ^ 7;
  bndy= FACE_INDEX(bndy,tri[0]^invert);
  hex_face(mesh, cell[0], bndy, ray, invert, xy);

  /* initialize dot, flags, possibly permute tri */
  edge= entry_setup(ray, xy, tri, dot, flags);
  if (qp0) {
    qp0[ray->order[0]]= qp[0];
    qp0[ray->order[1]]= qp[1];
    qp0[ray->order[2]]= qp[2];
  }
  if (edge>1) return 2;

  if ((tri[0]^face) == tri[1]) {
    /* about to step across entry face diagonal */
    diag= edge;
  } else if ((tri[edge]^face) != tri[2]) {
    /* about to step off of entry face, just crossed diagonal */
    diag= 2;
  } else {
    /* about to step off of entry face, never crossed diagonal */
    diag= !edge;
  }

  while (!(hit_miss= edge_test(xy, tri, dot, flags))) {

    if (edge!=diag) {
      /* step to next boundary face
       * -- note that face and bndy are *not* inverted by invert */
      int fedg;
      if (diag!=2) edge= diag;
      fedg= tri[edge]^tri[2];
      face= FACE_INDEX(fedg,tri[edge]^invert);
      flag= hex_step(mesh, cell, face);

      /* handle mesh boundaries */
      if (flag) {
        int tri2= tri[2];   /* save original bndy face value */
        tri[2]= tri[edge]^EDGE_BIT(bndy);
        if (flag!=2) {   /* handle corners that are "rounds" */
          int tmp= bndy;
          bndy= face;
          face= tmp^1;
        } else {
          /* grab interior edge on reflecting face
           * -- more work than strictly necessary, since ray_reflect
           *    will undo the partial projection
           * -- a mess anyway owing to necessity of following check that
           *    tri[2] is actually a distinct point */
          hex_edge(mesh, cell[0], bndy^1, face, ray, invert, xy);
          if ((xy[tri[2]][0]==xy[tri[0]][0] &&
               xy[tri[2]][1]==xy[tri[0]][1] &&
               xy[tri[2]][2]==xy[tri[0]][2]) ||
              (xy[tri[2]][0]==xy[tri[1]][0] &&
               xy[tri[2]][1]==xy[tri[1]][1] &&
               xy[tri[2]][2]==xy[tri[1]][2])) tri[2]= tri2^7;
          /* finally reflect the ray */
          ray_reflect(ray, xy, tri, dot, flags);
          tri[2]= tri2;   /* restore original bndy face value */
          /* ray_reflect reprojected the face edge already,
           * arrange for following hex_edge to reproject the
           * opposite face */
          face^= 1;
        }
        /* do not update invert here */
      } else {
        /* don't bother to check for "fillet" corners */
        invert^= EDGE_BIT(face);
      }

      /* grab coordinates of next edge on new boundary face */
      hex_edge(mesh, cell[0], bndy, face, ray, invert, xy);

      /* if just crossed diagonal, reset diagonal flag */
      if (diag==2) diag= edge;

    } else {
      /* step across face diagonal */
      tri[2]^= EDGE_BIT(bndy)^7;
      /* second tri_traverse always steps off face */
      diag= 2;
    }

    /* cross the current triangle */
    edge= tri_traverse(qp, xy, tri, dot);
  }

  if (hit_miss==2) return 1;

  /* check that triangle orientation is correct for tet_traverse */
  if ((xy[tri[1]][0]-xy[tri[0]][0])*(xy[tri[2]][1]-xy[tri[0]][1]) <
      (xy[tri[1]][1]-xy[tri[0]][1])*(xy[tri[2]][0]-xy[tri[0]][0])) {
    diag= tri[2];
    tri[2]= tri[edge];
    tri[edge]= diag;
  }

  /* allow xy scratch array to be interpreted correctly externally */
  tri[3]= invert;
  return 0;
}

/* ------------------------------------------------------------------------ */

/* enumerate all 24 orientations of a cube
 * - orientations[o][0] = o>>2 (==f0)
 * - orientations[o][2] = ((f0&4)?f0-4:f0+2) ^ ((o&2)?(f0&6)^6:0) ^ (o&1)
 * - that is, the 2 bit of o tells whether ijk cyclic order preserved
 *   while the 1 bit tells if the min or max face has switched
 */
static int orientations[24][6]= {
  {0,1,2,3,4,5}, {0,1,3,2,5,4}, {0,1,4,5,3,2}, {0,1,5,4,2,3},
  {1,0,3,2,4,5}, {1,0,2,3,5,4}, {1,0,5,4,3,2}, {1,0,4,5,2,3},
  {2,3,4,5,0,1}, {2,3,5,4,1,0}, {2,3,0,1,5,4}, {2,3,1,0,4,5},
  {3,2,5,4,0,1}, {3,2,4,5,1,0}, {3,2,1,0,5,4}, {3,2,0,1,4,5},
  {4,5,0,1,2,3}, {4,5,1,0,3,2}, {4,5,2,3,1,0}, {4,5,3,2,0,1},
  {5,4,1,0,2,3}, {5,4,0,1,3,2}, {5,4,3,2,1,0}, {5,4,2,3,0,1}
};

static int orient_compose(int second, int first);
static int orient_compose(int second, int first)
{
  int *o1= orientations[first];
  int *o2= orientations[second];
  /* destination of face 0 determines row in orientations array,
   *   when regarded as 6 rows of 4 columns
   * low order two bits determined by destination of face 2 */
  int f0= o2[o1[0]];
  int lo= o2[o1[2]] ^ ((f0&4)? f0-4 : f0+2);
  if (lo&4) lo^= 6;  /* set 2 bit, not 4 bit */
  return (f0<<2) | lo;
}

int hex_step(HX_mesh *mesh, long cell[], int face)
{
  int i= ((unsigned int)(
                         face= orientations[mesh->orient][face]
                         ))>>1;
  long stride= mesh->stride[i];
  long bound= mesh->bound[cell[0]-((face&1)?0:stride)][i];

  if (!bound) {
    /* usual case is to remain within current block */
    if (!(face&1)) stride= -stride;
    cell[0]+= stride;

  } else if (bound<0) {
    /* hit true boundary */
    return -bound;

  } else {
    /* hit block boundary, must switch to new block */
    HX_blkbnd *bnd= &mesh->bnds[bound-1];
    long block= bnd->block;
    mesh->block= block;
    mesh->stride= mesh->blks[block].stride;
    cell[0]= bnd->cell;
    cell[1]= block;
    if (bnd->orient) {
      /* need to reset mesh->orient */
      if (mesh->orient) {
        /* object is to find orient such that
         *   orientations[orient][face] == bnd_orient[mesh_orient[face]]
         * for every value of face */
        mesh->orient= orient_compose(bnd->orient, mesh->orient);
      } else {
        mesh->orient= bnd->orient;
      }
    }
  }
  return 0;
}

static int faces[6][4]= {
  { 0, 2, 4, 6 }, { 1, 3, 5, 7 },
  { 0, 4, 1, 5 }, { 2, 6, 3, 7 },
  { 0, 1, 2, 3 }, { 4, 5, 6, 7 }};

static int edges[6][3]= {
  { 0, 1, 2 }, { 0, 1, 2 },
  { 1, 2, 0 }, { 1, 2, 0 },
  { 2, 0, 1 }, { 2, 0, 1 }};

void hex_face(HX_mesh *mesh, long cell, int face,
              TK_ray *ray, int invert, real xy[][3])
{
  real *p= ray->p;
  real *qr= ray->qr;
  int *order= ray->order;
  int i, ix;
  real (*mxyz)[3]= mesh->xyz;
  int *fx= faces[(
                  face= orientations[mesh->orient][face]
                  )];
  int *em= edges[face];
  long im[4];
  long m= cell - mesh->stride[0] - mesh->stride[1] - mesh->stride[2];
  if (face&1) m+= mesh->stride[em[0]];
  mxyz= mxyz+m;  /* DEC cc doesn't like &mxyz[m] */

  im[0]= 0;
  im[1]= mesh->stride[em[1]];
  im[2]= mesh->stride[em[2]];
  im[3]= im[1]+im[2];

  for (i=0 ; i<4 ; i++) {
    ix= fx[i]^invert;
    m= im[i];
    xy[ix][2]= mxyz[m][order[2]] - p[2];
    xy[ix][1]= mxyz[m][order[1]] - xy[ix][2]*qr[1] - p[1];
    xy[ix][0]= mxyz[m][order[0]] - xy[ix][2]*qr[0] - p[0];
  }
}

void hex_edge(HX_mesh *mesh, long cell, int bndy, int face,
              TK_ray *ray, int invert, real xy[][3])
{
  real *p= ray->p;
  real *qr= ray->qr;
  int *order= ray->order;
  real (*mxyz)[3]= mesh->xyz+cell; /* DEC cc doesn't like &mesh->xyz[cell] */
  int f= edges[(
                face= orientations[mesh->orient][face]
                )][0];
  int b= edges[bndy][0];
  int fb= edges[face^bndy^6][0];
  long m= mesh->stride[fb];
  int ix, x= 0;
  if (face&1) x+= 1<<f;
  else        mxyz-= mesh->stride[f];
  if (bndy&1) x+= 1<<b;
  else        mxyz-= mesh->stride[b];
  ix= x^invert;
  xy[ix][2]= mxyz[-m][order[2]] - p[2];
  xy[ix][1]= mxyz[-m][order[1]] - p[1] - xy[ix][2]*qr[1];
  xy[ix][0]= mxyz[-m][order[0]] - p[0] - xy[ix][2]*qr[0];
  x+= 1<<fb;
  ix= x^invert;
  xy[ix][2]= mxyz[0][order[2]] - p[2];
  xy[ix][1]= mxyz[0][order[1]] - p[1] - xy[ix][2]*qr[1];
  xy[ix][0]= mxyz[0][order[0]] - p[0] - xy[ix][2]*qr[0];
}

static int triangle_flag= 0;

int hex_triang(int flag)
{
  int old= triangle_flag;
  if (flag==0 || flag==1) triangle_flag= flag;
  return old;
}

#undef ABS
#define ABS(x) ((x)<0? -(x) : (x))

int hex_init(HX_mesh *mesh, long cell[], int tri[])
{
  int i, j, k, edge, quad[4];
  long s, ndx[4], d0, d1, p0, p1;
  real v, l0, l1, tmp, dj, dk;
  real (*xyz)[3]= mesh->xyz;
  long start= mesh->start;
  int face;

  if (start>=0) {
    face= start%6;
    cell[0]= (start/= 6);
  } else {
    cell[0]= start= -1 - start;
    face= -1;
  }

  for (s=0 ; s<mesh->nblks ; s++)
    if (mesh->blks[s].first<=start && mesh->blks[s].final>start) break;
  if (s>=mesh->nblks) return 1;
  mesh->stride= mesh->blks[s].stride;
  mesh->orient= 0;
  mesh->block= cell[1]= s;

  if (face<0) return 0;

  i= ((unsigned int)face)>>1;
  k= i? i-1 : 2;
  j= i^k^3;

  edge= 1<<i;
  quad[0]= (face&1)? edge : 0;
  quad[1]= quad[0] | (1<<j);
  quad[2]= quad[0] | (1<<k);
  quad[3]= quad[1] | quad[2];

  s= ((face&1)? -mesh->stride[i] : mesh->stride[i]);
  ndx[3]= cell[0] - ((face&1)? 0 : mesh->stride[i]);
  ndx[2]= ndx[3] - mesh->stride[j];
  ndx[1]= ndx[3] - mesh->stride[k];
  ndx[0]= ndx[3] - mesh->stride[j] - mesh->stride[k];

  if (triangle_flag) {
    d0= 0;  d1= 3;  p0= 2;  p1= 1;
  } else {
    d0= 1;  d1= 2;  p0= 0;  p1= 3;
  }

  /* compute volume of cell to check mesh handedness,
   * compute distances of corners from diagonal */
  v= l0= l1= 0.;
  for (i=0 ; i<3 ; i++) {
    k= i? i-1 : 2;
    j= k^i^3;
    /* this actually gives 64*volume, v<0 for right-handed cell? */
    v+= (xyz[ndx[1]][i]+xyz[ndx[0]][i]+xyz[ndx[3]][i]+xyz[ndx[2]][i] -
         xyz[ndx[1]+s][i]-xyz[ndx[0]+s][i]-xyz[ndx[3]+s][i]-xyz[ndx[2]+s][i])*
      ((xyz[ndx[1]][j]-xyz[ndx[0]][j]+xyz[ndx[3]][j]-xyz[ndx[2]][j] +
        xyz[ndx[1]+s][j]-xyz[ndx[0]+s][j]+xyz[ndx[3]+s][j]-xyz[ndx[2]+s][j])*
       (xyz[ndx[2]][k]-xyz[ndx[0]][k]+xyz[ndx[3]][k]-xyz[ndx[1]][k] +
        xyz[ndx[2]+s][k]-xyz[ndx[0]+s][k]+xyz[ndx[3]+s][k]-xyz[ndx[1]+s][k]) -
       (xyz[ndx[1]][k]-xyz[ndx[0]][k]+xyz[ndx[3]][k]-xyz[ndx[2]][k] +
        xyz[ndx[1]+s][k]-xyz[ndx[0]+s][k]+xyz[ndx[3]+s][k]-xyz[ndx[2]+s][k])*
       (xyz[ndx[2]][j]-xyz[ndx[0]][j]+xyz[ndx[3]][j]-xyz[ndx[1]][j] +
        xyz[ndx[2]+s][j]-xyz[ndx[0]+s][j]+xyz[ndx[3]+s][j]-xyz[ndx[1]+s][j]));
    dj= xyz[ndx[d1]][j]-xyz[ndx[d0]][j];
    dk= xyz[ndx[d1]][k]-xyz[ndx[d0]][k];
    tmp= dj*(xyz[ndx[p0]][k]-xyz[ndx[d0]][k]) -
      dk*(xyz[ndx[p0]][j]-xyz[ndx[d0]][j]);
    l0+= ABS(tmp);
    tmp= dj*(xyz[ndx[p1]][k]-xyz[ndx[d0]][k]) -
      dk*(xyz[ndx[p1]][j]-xyz[ndx[d0]][j]);
    l1+= ABS(tmp);
  }

  if (l0>l1) {   /* points are p0, d0, d1 */
    if (v>0.) { /* left-handed cell? */
      tri[0]= quad[p0];
      tri[1]= quad[d1];
      tri[2]= quad[d0];
    } else {
      tri[0]= quad[p0];
      tri[1]= quad[d0];
      tri[2]= quad[d1];
    }
  } else {       /* points are p1, d1, d0 */
    if (v>0.) { /* left-handed cell? */
      tri[0]= quad[p1];
      tri[1]= quad[d0];
      tri[2]= quad[d1];
    } else {
      tri[0]= quad[p1];
      tri[1]= quad[d1];
      tri[2]= quad[d0];
    }
  }

  return 0;
}

/* ------------------------------------------------------------------------ */

int start_from_orig= 0;
int hex_startflag(int flag)
{
  int oldflag= start_from_orig;
  if (flag==0 || flag==1) start_from_orig= flag;
  return oldflag;
}

/* ------------------------------------------------------------------------ */
