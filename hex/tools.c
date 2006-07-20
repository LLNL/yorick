/*
 * $Id: tools.c,v 1.2 2006-07-20 04:28:30 dhmunro Exp $
 * toolkit for computing ray intersections with tets,
 * performing entry point calculations
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "tools.h"

#undef ABS
#define ABS(x) ((x)<0? -(x) : (x))

/* the minimum relative scale is a multiplier for the size of
 * the first zone encountered, to produce a scale length used
 * to determine whether a step during the entry search represents
 * a significant motion for the purposes of edge_test (and not
 * just a zero step to within roundoff
 * this is important because the "miss" criteria can otherwise
 * be triggered by roundoff errors
 * the danger in choosing this too small is that if this first zone
 * is unusually small, the scale criterion will be too short and
 * edge_test will erroniously declare a miss
 * the danger in choosing this too large is that the edge_test
 * criterion may never be satisfied, resulting in an infinite loop
 * since infinite loops are worse, choose pretty small */
#undef MIN_RELATIVE_SCALE
#define MIN_RELATIVE_SCALE 1.e-6

/* ------------------------------------------------------------------------ */

static int bigger_tri(real xy[][3], int tet[], int i0, int i1);

int
tet_traverse(real xy[][3], int tet[])
{
  int i, tet3= tet[3];    /* base is ABC, apex is D, compute AxD */
  real xd = xy[tet3][0];
  real yd = xy[tet3][1];
  real xa = xy[tet[0]][0];
  real ya = xy[tet[0]][1];
  real xb = xy[tet[1]][0];
  real yb = xy[tet[1]][1];
  real xc = xy[tet[2]][0];
  real yc = xy[tet[2]][1];

  /* handle degenerate hexahedron cases to avoid roundoff problems */
  if (xa==xd && ya==yd) {
    i = 0;
  } else if (xb==xd && yb==yd) {
    i = 1;
  } else if (xc==xd && yc==yd) {
    i = 2;

  } else {
    /* stepping into a zero projected area triangle is fatal (tri_intersect)
     * bigger_tri will never do that, but the other branches here may
     * - above branches prevent most common case, but do not cover
     *   case that D is collinear with AB, BC, or CA
     * definitive check is too expensive to put here?
     */
    real cross = xa*yd - ya*xd;

    if         (cross<0.) { /* ray to left of line AD, compute BxD */
      cross = xb*yd - yb*xd;
      if (cross) i = (cross>0.)<<1;
      else       i = bigger_tri(xy, tet, 2, 0);

    } else if (cross>0.) { /* ray to right of line AD, compute CxD */
      cross = xc*yd - yc*xd;
      if (cross) i = (cross<0.);
      else       i = bigger_tri(xy, tet, 0, 1);

    } else {           /* ray lies exactly on line AD, compute BxD */
      cross = xb*yd - yb*xd;
      if      (cross<0.) i = 0;
      else if (cross>0.) i = bigger_tri(xy, tet, 1, 2);
      else               i = bigger_tri(xy, tet, 0, 0); /* D on ray */
    }
  }

  tet[3] = tet[i];
  tet[i] = tet3;
  return i;
}

/* exception handler for tet_traverse, returns either i0 or i1,
 * depending on which replacement will leave the larger area base triangle
 */
static int
bigger_tri(real xy[][3], int tet[], int i0, int i1)
{
  int j = i0? i0-1 : 2;
  real x3 = xy[tet[3]][0];
  real y3 = xy[tet[3]][1];
  real area0, area1;             /* neither will be negative here */
  int three = (i1==i0);          /* i0=i1=0 means check all three */
  if (three) i1 = 1;

  area0 = (xy[tet[3^i0^j]][0]-x3)*(xy[tet[j]][1]-y3) -
    (xy[tet[3^i0^j]][1]-y3)*(xy[tet[j]][0]-x3);
  j = i1? i1-1 : 2;
  area1 = (xy[tet[3^i1^j]][0]-x3)*(xy[tet[j]][1]-y3) -
    (xy[tet[3^i1^j]][1]-y3)*(xy[tet[j]][0]-x3);
  j = (area0>area1)? i0 : i1;
  if (!three) {
    /* never knowingly step into a zero area triangle */
    if (area0<=0. && area1<=0.) return 3-i0-i1;
    return j;              /* otherwise larger of two */
  }

  if (j) area0 = area1;
  area1 = (xy[tet[0]][0]-x3)*(xy[tet[1]][1]-y3) -
    (xy[tet[0]][1]-y3)*(xy[tet[1]][0]-x3);
  return (area0>area1)? j : 2;  /* largest of all three */
}

/* ------------------------------------------------------------------------ */

real tri_intersect(real xy[][3], int tri[])
{
  real x0= xy[tri[0]][0];
  real y0= xy[tri[0]][1];
  real z0= xy[tri[0]][2];
  real x1= xy[tri[1]][0];
  real y1= xy[tri[1]][1];
  real z1= xy[tri[1]][2];
  real x2= xy[tri[2]][0];
  real y2= xy[tri[2]][1];
  real z2= xy[tri[2]][2];
  /* see note in tet_traverse about area==0, roundoff makes it possible */
  real area= (x0-x2)*(y1-y2) - (y0-y2)*(x1-x2);
  x1= x1*y2 - y1*x2;
  x0= x2*y0 - y2*x0;
  return z2 + ((z0-z2)*x1 + (z1-z2)*x0)/area;
  /* note that z2 is just an arbitrary linear function on the surface
   * of the triangle -- any other linear function value would be computed
   * using the same formula */
}

/* ------------------------------------------------------------------------ */

int tri_traverse(real qp[], real xy[][3], int tri[], real dot[])
{
  int i, tri2= tri[2];
  real d= qp[0]*xy[tri2][0] + qp[1]*xy[tri2][1];
  if      (d>0.) i= 0;      /* 2 on same side of plane as 0 */
  else if (d<0.) i= 1;      /* 2 on same side of plane as 1 */
  else           i= dot[0]+dot[1]>0.;    /* 2 lies in plane */
  tri[2]= tri[i];
  tri[i]= tri2;
  dot[i]= d;
  return i;
}

/* ------------------------------------------------------------------------ */

real
tri_find(real xy[][3], int tri[], real qr2)
{
  real x0 = xy[tri[0]][0];
  real y0 = xy[tri[0]][1];
  real z0 = xy[tri[0]][2];
  real x1 = xy[tri[1]][0];
  real y1 = xy[tri[1]][1];
  real z1 = xy[tri[1]][2];
  real x2 = xy[tri[2]][0];
  real y2 = xy[tri[2]][1];
  real z2 = xy[tri[2]][2];
  real area = (x0-x2)*(y1-y2) - (y0-y2)*(x1-x2);
  x1 = x1*y2 - y1*x2;
  x0 = x2*y0 - y2*x0;
  /* algorithm identical to tri_intersect to this point */
  if (x1<0. || x0<0. || area<x0+x1 || !area) return 1.e35;
  return qr2 * (z2 + ((z0-z2)*x1 + (z1-z2)*x0)/area);
}

void tri_check(real xy[][3], int tri[])
{
  if ((xy[tri[2]][1]-xy[tri[0]][1])*(xy[tri[1]][0]-xy[tri[0]][0]) <
      (xy[tri[2]][0]-xy[tri[0]][0])*(xy[tri[1]][1]-xy[tri[0]][1])) {
    int tri0= tri[0];
    tri[0]= tri[1];
    tri[1]= tri0;
  }
}

/* ------------------------------------------------------------------------ */

int edge_test(real xy[][3], int tri[], real dot[], int flags[])
{
  real dx, x= dot[0]/(dot[0]-dot[1]);
  int i= flags[0];
  x= xy[tri[0]][i] + (xy[tri[1]][i]-xy[tri[0]][i])*x;
  dx= x - dot[2];
  if (dx) {
    int heading= (dx<0.);
    if (heading == flags[1]) {
      i= (dot[2]<0.);
      if ((x<0.) != i) return 1;                 /* hit  */
      if ((heading? -dx : dx) > dot[3]) {
        /* change state only when step length significant */
        if ((dx<0.) == i) return 2;              /* miss */
        flags[2]= 1;
      }
    } else {
      /* only count a miss if we are really around the limb
       * -- don't die on roundoff */
      if (flags[2] &&
          (heading? -dx : dx)>dot[3]) return 2;  /* miss */
    }
    dot[2]= x;
  }
  return 0;             /* continue search */
}

/* ------------------------------------------------------------------------ */

int interior_boundary= 0;

int entry_setup(TK_ray *ray, real xy[][3], int tri[],
                real dot[], int flags[])
{
  int i, j, k, larger, sign, trit[3];
  real dott[3], fj, fk, xj, xk;
  real *qr= ray->qr;
  real *qp= ray->qp;

  for (i=0 ; i<3 ; i++) trit[i]= tri[i];

  /* find a direction piercing the initial triangle not on the ray
   * select furthest from ray of three directions near centroid */
  dott[0]= xy[trit[0]][0] + xy[trit[1]][0] + xy[trit[2]][0];
  dott[1]= xy[trit[0]][1] + xy[trit[1]][1] + xy[trit[2]][1];
  qp[0]= dott[0] + xy[trit[0]][0];
  qp[1]= dott[1] + xy[trit[0]][1];
  xk= ABS(qp[0]) + ABS(qp[1]);
  for (i=1 ; i<3 ; i++) {
    dot[0]= dott[0] + xy[trit[i]][0];
    dot[1]= dott[1] + xy[trit[i]][1];
    if (xk < ABS(dot[0]) + ABS(dot[1])) {
      xk= ABS(dot[0]) + ABS(dot[1]);
      qp[0]= dot[0];
      qp[1]= dot[1];
    }
  }

  /* normal vector to plane determined by the direction just found
   * and the ray direction has magically simple form
   * -- afterwards qp is (q cross direction)/qz,
   *    where direction threads the initial triangle
   * -- qp is in unprojected (mesh) coordinates, unlike xy
   * -- sign of qp may be reversed later */
  xk= qp[1];
  qp[1]= qp[0];
  qp[0]= -xk;
  qp[2]= -(qr[0]*qp[0]+qr[1]*qp[1]);

  /* intersect initial triangle with plane
   * this looks like a 2D dot product, but because xy are in
   * projected coordinates and qp is normal to q, its value is
   * actually the 3D dot product */
  dott[0]= qp[0]*xy[trit[0]][0] + qp[1]*xy[trit[0]][1];
  dott[1]= qp[0]*xy[trit[1]][0] + qp[1]*xy[trit[1]][1];
  dott[2]= qp[0]*xy[trit[2]][0] + qp[1]*xy[trit[2]][1];

  if ((dott[0]<0.)==(dott[1]<0.)) {
    /* quit if ray lies exactly in plane of initial triangle */
    if ((dott[2]<0.)==(dott[0]<0.)) return 2;
    /* intersection points are on 12 and 20 edges */
    i= 2;
  } else if ((dott[1]<0.)==(dott[2]<0.)) {
    /* intersection points are on 01 and 20 edges */
    i= 0;
  } else {
    /* intersection points are on 01 and 12 edges */
    i= 1;
  }
  k= i? i-1 : 2;
  j= i^k^3;

  /* use larger of two perpendicular coordinates to measure
   * distance of boundary points from ray in plane normal to qp
   * set sign flag if decreasing xy[][larger] going from j->k
   * corresponds to the ray hitting the entry side of triangle */
  larger= ABS(qp[1]) < ABS(qp[0]);
  sign= larger? (-qp[0]<0.) : (qp[1]<0.);
  sign= (sign != (qr[2]<0.));          /* set if qpXq < 0 */
  sign= (sign == (dott[k]-dott[i]<0.));
  if (ray->odd) sign= !sign;

  /* quit if triangle edges have zero length to roundoff
   * (this can happen if ray is very far from a small but
   *  finite triangle) */
  xk= xy[trit[k]][larger]-xy[trit[i]][larger];
  xj= xy[trit[j]][larger]-xy[trit[i]][larger];
  {
    real sidej= xy[trit[j]][!larger]-xy[trit[i]][!larger];
    real sidek= xy[trit[k]][!larger]-xy[trit[i]][!larger];
    real scale= ABS(xy[trit[i]][larger]) + ABS(xy[trit[j]][larger]) +
      ABS(xy[trit[k]][larger]);
    scale+= scale;
    sidej= ABS(sidej)+ABS(xj);
    sidek= ABS(sidek)+ABS(xk);
    if (sidej+scale==scale && sidek+scale==scale) return 2;
    /* save reasonable scale length for use in edge_test */
    dot[3]= MIN_RELATIVE_SCALE*(sidej+sidek);
  }

  /* previous logic guarantees following denominators non-zero,
   * ratios between 0 and 1 (inclusive) */
  fk= dott[i]/(dott[i]-dott[k]);
  fj= dott[i]/(dott[i]-dott[j]);
  xk= xy[trit[i]][larger] + xk*fk;
  xj= xy[trit[i]][larger] + xj*fj;

  flags[2]= (ABS(xk-xj)>dot[3] && (xk-xj<0.)==sign);
  if (flags[2]) {
    /* on entry side always head toward the ray */
    if ((xk-xj<0.) == (xj<0.)) sign|= 2;
  } else {
    /* on exit side head opposite to ray direction unless
     * flag is set for interior boundary -- this is purely a
     * performance question, either choice gets there eventually */
    real zk= xy[trit[i]][2] + (xy[trit[k]][2]-xy[trit[i]][2])*fk;
    real zj= xy[trit[i]][2] + (xy[trit[j]][2]-xy[trit[i]][2])*fj;
    if (((zk-zj<0.)==(qr[2]<0.)) != interior_boundary) sign|= 2;
  }

  if (sign&2) {
    /* want to head from k->j intersection
     * tri should have been kij, after tri_traverse jik */
    tri[0]= trit[j];
    tri[1]= trit[i];
    tri[2]= trit[k];
    dot[0]= dott[j];
    dot[1]= dott[i];
    dot[2]= xk;
    k= 0;
    flags[1]= !(sign&1);
  } else {
    /* want to head from j->k intersection
     * tri should have been ijk, after tri_traverse ikj */
    tri[0]= trit[i];
    tri[1]= trit[k];
    tri[2]= trit[j];
    dot[0]= dott[i];
    dot[1]= dott[k];
    dot[2]= xj;
    k= 1;
    flags[1]= (sign&1);
  }
  flags[0]= larger;

  if (dot[0]<dot[1]) {
    /* switch sign of qp for tri_traverse */
    for (i=0 ; i<3 ; i++) qp[i]= -qp[i];
    dot[0]= -dot[0];
    dot[1]= -dot[1];
  }

  return k;
}

/* ------------------------------------------------------------------------ */

int ray_reflect(TK_ray *ray, real xy[][3], int tri[],
                real dot[], int flags[])
{
  int i, sign= 0;
  real xyu[3][3], pnew[3], ptmp, qtmp;
  real *p= ray->p;
  real *q= ray->q;
  real *qr= ray->qr;
  int qr2= qr[2]<0.;
  int *order= ray->order;
  real *qp= ray->qp;
  int qp_compute= (flags!=0) || (dot!=0);

  if (flags) {
    /* flags[1] is hardest thing to compute -- save indicator
     * for whether qpXq lies along + or - "x" direction initially */
    sign= flags[0]? (-qp[0]<0.) : (qp[1]<0.);
    sign= (sign != (qr[2]<0.));          /* set if qpXq < 0 */
  }

  /* unproject xy[tri] coordinates */
  for (i=0 ; i<3 ; i++) {
    ptmp= xy[tri[i]][2];
    xyu[i][order[0]]= xy[tri[i]][0] + qr[0]*ptmp;
    xyu[i][order[1]]= xy[tri[i]][1] + qr[1]*ptmp;
    xyu[i][order[2]]= ptmp;
  }
  for (i=0 ; i<3 ; i++) {
    ptmp= pnew[i]= xyu[1][i] - xyu[0][i];
    qtmp= xyu[2][i]-= xyu[0][i];
    if (!ptmp && !qtmp) break;
  }
  /* note that origin for pnew, xyu[2] vectors is p, not 0 */

  if (i>2) {
    /* reflection plane is not parallel to any coordinate plane */
    real a[3], b[3], tmp;

    a[0]= pnew[1]*xyu[2][2] - pnew[2]*xyu[2][1];
    a[1]= pnew[2]*xyu[2][0] - pnew[0]*xyu[2][2];
    a[2]= pnew[0]*xyu[2][1] - pnew[1]*xyu[2][0];

    tmp= 0.0;
    for (i=0 ; i<3 ; i++) tmp+= a[i]*a[i];
    tmp= 2.0/tmp;

    ptmp= qtmp= 0.0;
    for (i=0 ; i<3 ; i++) {
      b[i]= a[i]*tmp;
      ptmp-= a[i]*xyu[0][i];  /* -xyu[0] is vector being reflected */
      qtmp+= a[i]*q[i];
    }

    for (i=0 ; i<3 ; i++) {
      pnew[order[i]]= p[i] - b[order[i]]*ptmp;
      q[i]-= b[i]*qtmp;
    }

    /* polish q direction to ensure it stays normalized */
    for (i=0 ; i<3 ; i++) if (4.+q[i]==4.) q[i]= 0.;
    qtmp= q[0]*q[0] + q[1]*q[1] + q[2]*q[2];
    qtmp= 1.0 + 0.5*(1.0-qtmp);
    if (qtmp!=1.0) for (i=0 ; i<3 ; i++) q[i]*= qtmp;

    if (qp_compute) {
      qtmp= 0.0;
      for (i=0 ; i<3 ; i++) {
        xyu[2][order[i]]= qp[i];
        qtmp+= a[order[i]]*qp[i];
      }
      for (i=0 ; i<3 ; i++) {
        xyu[0][i]+= b[i]*ptmp;
        xyu[1][i]+= b[i]*ptmp;
        xyu[2][i]-= b[i]*qtmp;
      }
    }

  } else {
    /* reflection plane is parallel to (mesh) coordinate plane i */
    int j;

    ptmp= -xyu[0][i];  /* -xyu[0] is vector being reflected */
    ptmp+= ptmp;
    for (j=0 ; j<3 ; j++) pnew[order[j]]= p[j];
    pnew[i]-= ptmp;
    q[i]= -q[i];

    if (qp_compute) {
      for (j=0 ; j<3 ; j++) xyu[2][order[j]]= qp[j];
      xyu[0][i]+= ptmp;
      xyu[1][i]+= ptmp;
      xyu[2][i]= -xyu[2][i];
    }
  }

  /* identify coordinate with largest projection onto ray */
  ptmp= ABS(q[0]);
  qtmp= ABS(q[1]);
  if (ptmp > qtmp) i= ptmp>ABS(q[2])? 0 : 2;
  else             i= qtmp>ABS(q[2])? 1 : 2;

  /* keep cyclic order (maybe should flip coordinate handedness?) */
  order[2]= i;
  order[1]= i? i-1 : 2;
  order[0]= order[1]^i^3;

  for (i=0 ; i<3 ; i++) p[i]= pnew[order[i]];
  qr[2]= 1.0/q[order[2]];
  qr[1]= q[order[1]] * qr[2];
  qr[0]= q[order[0]] * qr[2];

  if (qp_compute) {
    for (i=0 ; i<3 ; i++) qp[i]= xyu[2][order[i]];
    ray->odd= !ray->odd;
  }

  if (flags) {
    flags[0]= ABS(qp[1]) < ABS(qp[0]);
    /* flags[2] unchanged */

    /* recompute x=dot[2]
     * this requires actually reprojecting xy[0:1] for the
     * new reflected coordinates, so do that here too */
    for (i=0 ; i<2 ; i++) {
      ptmp= xyu[i][order[2]];
      xy[tri[i]][0]= xyu[i][order[0]] - qr[0]*ptmp;
      xy[tri[i]][1]= xyu[i][order[1]] - qr[1]*ptmp;
      xy[tri[i]][2]= ptmp;
    }
    ptmp= dot[0]/(dot[0]-dot[1]);
    i= flags[0];
    dot[2]= xy[tri[0]][i] + (xy[tri[1]][i]-xy[tri[0]][i])*ptmp;

    /* flags[1] is hardest thing to compute --
     * repeat above sign calculation for new q and qp directions
     * flip flags[1] if this has not changed */
    i= flags[0]? (-qp[0]<0.) : (qp[1]<0.);
    i= (i != (qr[2]<0.));
    if (i==sign) flags[1]= !flags[1];
  }

  return (qr[2]<0.)==qr2;
}

int
ray_certify(TK_ray *ray, real xy[][3], int tri[], int nxy)
{
  /* reflection operation must preserve critical topology
   * specifically, ray must remain inside tri after reflection
   * - if ray is very near an edge or vertex, roundoff error
   *   may move reflected ray outside tri, which is very bad
   */
  real xa = xy[tri[0]][0];
  real ya = xy[tri[0]][1];
  real xb = xy[tri[1]][0];
  real yb = xy[tri[1]][1];
  real xc = xy[tri[2]][0];
  real yc = xy[tri[2]][1];
  real axb = xa*yb - ya*xb;
  real bxc = xb*yc - yb*xc;
  real cxa = xc*ya - yc*xa;
  if (axb+bxc+cxa <= 0.)
    return -1;

  if (axb>=0. && bxc>=0. && cxa>=0.) {
    return 0;
  } else {
    /* strategy is to try to step back to nearest edge or vertex */
    int i;
    real dx, dy, r, ddx, ddy;
    volatile real x0, y0, x1, y1, x2, y2;
    if (axb < 0.) {
      if (bxc < 0.) {
        dx = xb; dy = yb;
      } else if (cxa < 0.) {
        dx = xa; dy = ya;
      } else {
        dy = xa-xb; dx = yb-ya; r = axb/(dx*dx+dy*dy);
        dx *= r; dy *= r;
        for (;;) {
          x0 = xa-dx;  y0 = ya-dy;  x1 = xb-dx;  y1 = yb-dy;
          if (x0!=xa || y0!=ya || x1!=xb || y1!=yb) break;
          dx += dx;  dy += dy;
        }
      }
    } else if (bxc < 0.) {
      if (cxa < 0.) {
        dx = xc; dy = yc;
      } else {
        dy = xb-xc; dx = yc-yb; r = bxc/(dx*dx+dy*dy);
        dx *= r; dy *= r;
        for (;;) {
          x0 = xb-dx;  y0 = yb-dy;  x1 = xc-dx;  y1 = yc-dy;
          if (x0!=xb || y0!=yb || x1!=xc || y1!=yc) break;
          dx += dx;  dy += dy;
        }
      }
    } else {
      dy = xc-xa; dx = ya-yc; r = cxa/(dx*dx+dy*dy);
      dx *= r; dy *= r;
      for (;;) {
        x0 = xc-dx;  y0 = yc-dy;  x1 = xa-dx;  y1 = ya-dy;
        if (x0!=xc || y0!=yc || x1!=xa || y1!=ya) break;
        dx += dx;  dy += dy;
      }
    }

    ddx = dx;  ddy = dy;
    for (i=0 ; i<10 ; i++) {
      x0 = xa - dx;  y0 = ya - dy;
      x1 = xb - dx;  y1 = yb - dy;
      x2 = xc - dx;  y2 = yc - dy;
      axb = x0*y1 - y0*x1;
      bxc = x1*y2 - y1*x2;
      cxa = x2*y0 - y2*x0;
      if (axb+bxc+cxa <=0.)
        return -1;
      if (axb>=0. && bxc>=0. && cxa>=0.) break;
      dx += ddx;  dy+= ddy;
    }
    if (i >= 10)
      return -1;  /* maybe should flag this -2? */

    /* move ray back into tri */
    ray->p[0] += dx;
    ray->p[1] += dy;
    for (i=0 ; i<nxy ; i++) xy[i][0] -= dx, xy[i][1] -= dy;
    return 1;
  }
}

/* ------------------------------------------------------------------------ */

void ray_init(TK_ray *ray, real pa[], real qa[], real matrix[][3])
{
  real pb[3], qb[3], *p, *q, ptmp, qtmp;
  int i;
  int *order= ray->order;

  if (matrix) {
    int j;
    p= pb;
    q= qb;
    for (i=0 ; i<3 ; i++) {
      p[i]= matrix[3][i];
      q[i]= 0.;
      for (j=0 ; j<3 ; j++) {
        p[i]+= (pa[j]-matrix[4][j])*matrix[j][i];
        q[i]+= qa[j]*matrix[j][i];
      }
    }
  } else {
    p= pa;
    q= qa;
  }

  /* polish q direction to ensure it stays normalized */
  for (i=0 ; i<3 ; i++) if (4.+q[i]==4.) q[i]= 0.;
  qtmp= q[0]*q[0] + q[1]*q[1] + q[2]*q[2];
  qtmp= 1.0 + 0.5*(1.0-qtmp);
  if (qtmp!=1.0) for (i=0 ; i<3 ; i++) q[i]*= qtmp;

  /* identify coordinate with largest projection onto ray */
  ptmp= ABS(q[0]);
  qtmp= ABS(q[1]);
  if (ptmp > qtmp) i= ptmp>ABS(q[2])? 0 : 2;
  else             i= qtmp>ABS(q[2])? 1 : 2;

  /* keep cyclic coordinate order */
  order[2]= i;
  order[1]= order[2]? order[2]-1 : 2;
  order[0]= order[1]^i^3;

  /* load p and q */
  for (i=0 ; i<3 ; i++) {
    ray->p[i]= p[order[i]];
    ray->q[i]= q[i];
    ray->qp[i]= 0.;
  }

  /* load reciprocal q */
  ray->qr[2]= 1.0/q[order[2]];
  ray->qr[1]= q[order[1]] * ray->qr[2];
  ray->qr[0]= q[order[0]] * ray->qr[2];

  ray->odd= 0;
}

int update_transform(TK_ray *ray, real p0[], real q0[],
                     real matrix[][3], int odd)
{
  int i, j, k;
  int *order= ray->order;
  real qp[3], qp0[3], a[3], a0[3];
  real *mesh[3], *refl[3];

  /* apply inverse of old transform to matrix[3] to get qp0,
   * compute square of length of qp0, use it to change length of
   * qp0 to its reciprocal
   * also depermute ray->qp */
  a[0]= 0.;
  for (i=0 ; i<3 ; i++) {
    qp0[i]= 0.;
    for (j=0 ; j<3 ; j++) qp0[i]+= matrix[i][j]*matrix[3][j];
    a[0]+= qp0[i]*qp0[i];
    qp[order[i]]= ray->qp[i];
  }
  a[0]= 1./a[0];
  for (i=0 ; i<3 ; i++) qp0[i]*= a[0];

  /* compute third basis vectors (besides qp, q)
   * also depermute ray->p and store in matrix[3] */
  for (i=0 ; i<3 ; i++) {
    k= i? i-1 : 2;
    j= k^i^3;
    a[i]= qp[j]*ray->q[k] - qp[k]*ray->q[j];
    a0[i]= qp0[j]*q0[k] - qp0[k]*q0[j];
    matrix[3][order[i]]= ray->p[i];
  }
  if (odd) for (i=0 ; i<3 ; i++) a0[i]= -a0[i];
  if (ray->odd) {
    for (i=0 ; i<3 ; i++) a[i]= -a[i];
    odd= !odd;
  }

  /* multiply [a,ray->qp,ray->q] by transpose of [a0,qp0,q0] to
   * obtain matrix which transforms mesh coordinates (like p0 and q0)
   * to multiply reflected coordinates */
  mesh[0]= a0;
  mesh[1]= qp0;
  mesh[2]= q0;
  refl[0]= a;
  refl[1]= qp;
  refl[2]= ray->q;
  for (i=0 ; i<3 ; i++) for (j=0 ; j<3 ; j++) {
    matrix[j][i]= 0.;
    for (k=0 ; k<3 ; k++) matrix[j][i]+= refl[k][i]*mesh[k][j];
    if (4.+matrix[j][i] == 4.) matrix[j][i]= 0.;
  }

  /* save p0 in matrix[4] */
  for (i=0 ; i<3 ; i++) matrix[4][i]= p0[i];

  return odd;
}

/* ------------------------------------------------------------------------ */

void ray_integ(long nr, long *nlist, long ng,
               real *transp, real *selfem, real *result)
{
  long r, g, n;
  long last= ng<0;

  if (last) {
    ng= -ng;
    if (!transp) {
      long rr;
      real se;
      for (g=0 ; g<ng ; g++) {
        for (r=rr=0 ; r<nr ; r++,rr+=ng) {
          se= 0.;
          for (n=nlist[r] ; n-- ; ) se+= *selfem++;
          result[g+rr]= se;
        }
      }
    } else if (!selfem) {
      long rr;
      real tr;
      for (g=0 ; g<ng ; g++) {
        for (r=rr=0 ; r<nr ; r++,rr+=ng) {
          tr= 1.;
          for (n=nlist[r] ; n-- ; ) tr*= *transp++;
          result[g+rr]= tr;
        }
      }
    } else {
      long rr, ng2= 2*ng;
      real se, tr;
      for (g=0 ; g<ng ; g++) {
        for (r=rr=0 ; r<nr ; r++,rr+=ng2) {
          se= 0.;
          tr= 1.;
          for (n=nlist[r] ; n-- ; ) {
            se= transp[0]*se + *selfem++;
            tr*= *transp++;
          }
          result[g+rr]= tr;
          result[ng+g+rr]= se;
        }
      }
    }

  } else {
    if (!transp) {
      for (r=0 ; r<nr ; r++,result+=ng) {
        for (g=0 ; g<ng ; g++) result[g]= 0.;
        for (n=nlist[r] ; n-- ; )
          for (g=0 ; g<ng ; g++) result[g]+= *selfem++;
      }
    } else if (!selfem) {
      for (r=0 ; r<nr ; r++,result+=ng) {
        for (g=0 ; g<ng ; g++) result[g]= 1.;
        for (n=nlist[r] ; n-- ; )
          for (g=0 ; g<ng ; g++) result[g]*= *transp++;
      }
    } else {
      long ng2= 2*ng;
      for (r=0 ; r<nr ; r++,result+=ng2) {
        for (g=0 ; g<ng ; g++) {
          result[ng+g]= 0.;
          result[g]= 1.;
        }
        for (n=nlist[r] ; n-- ; )
          for (g=0 ; g<ng ; g++) {
            result[ng+g]= transp[0]*result[ng+g] + *selfem++;
            result[g]*= *transp++;
          }
      }
    }
  }
}

/* ------------------------------------------------------------------------ */
