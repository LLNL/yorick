/*
 * $Id: graph0.c,v 1.2 2007-12-26 04:25:46 dhmunro Exp $
 * Define graphics related functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "ydata.h"

extern long QuickMeshZone(double xx, double yy, double *x, double *y,
                          int *reg, long ix, long ijx, long i,
                          long *bndy, long nbndy);

extern long *BuildMeshBndy(double *x, double *y, int *reg,
                           long ix, long ijx, long *nbndy);

/*--------------------------------------------------------------------------*/

/* return -1 if (xx,yy) not within ix-by-jx mesh (x,y),
   else 0-origin zone index -- reg==0 is allowed, and reg
   need not have overreach zones
   i is an initial guess, or <0 for no guess
   if bndy==0, may return -1 erroniously */
long QuickMeshZone(double xx, double yy, double *x, double *y,
                   int *reg, long ix, long ijx, long i,
                   long *bndy, long nbndy)
{
  /* coordinates of zone corners relative to (xx,yy) */
  double xz[4], yz[4], ar[4], sn=0., sd=0.;
  double xr=0., yr=0.;
  int wind, edge, negative, bcheck;
  long w= 0;

  /* algorithm:
     (1) let (xx,yy) be the coordinate origin
     (2) compute the midpoints of the two diagonals
         of the initial guess zone, and take the one
         with the largest absolute value coordinate as
         a reference point R
     (3) walk in a straight line from R to (xx,yy) (==0)
         by examining the intersections of the four zone
         edges with this line
         (a) examine all four edges of the initial guess
             if they wind around (xx,yy), done, otherwise
             take the edge which intersects 0R closest to
             0 and step across into that zone
         (b) examine the other three edges of the new zone
             to find whether they wind around 0; if not
             take another step and repeat
     (4) if encounter a boundary, quit and report failure
     (5) if bndy array exists, check if (xx,yy) is really
         outside mesh -- if inside, repeat search starting
         from boundary zone with closest edge intersection
         of x=xx to (xx,yy)
   */
  if (i<0) goto check;
  bcheck= 0;
 start:

  /* find corners of first zone */
  xz[0]= x[i-ix-1]-xx;   yz[0]= y[i-ix-1]-yy;
  xz[1]= x[i-ix]-xx;     yz[1]= y[i-ix]-yy;
  xz[2]= x[i]-xx;        yz[2]= y[i]-yy;
  xz[3]= x[i-1]-xx;      yz[3]= y[i-1]-yy;

  if (!bcheck) {
    /* compute midpoint of 02 diagonal */
    xr= 0.5*(xz[0]+xz[2]);
    yr= 0.5*(yz[0]+yz[2]);

    /* use 13 diagonal midpoint if that is further from (xx,yy) */
    {
      double t1= xr<0.? -xr : xr;
      double t2= yr<0.? -yr : yr;
      double t3= 0.5*(xz[1]+xz[3]);
      double t4= 0.5*(yz[1]+yz[3]);
      if (t2>t1) t1= t2;
      t2= t3<0.? -t3 : t3;
      if (t4<0.? (-t4>t2) : (t4>t2)) t2= t4;
      if (t2 > t1) {
        xr= t3;
        yr= t4;
      }
    }

    /* allow for pathological case of (xx,yy) precisely at the
       center of a perfect parallelogram */
    if (xr==0.0 && yr==0.0) return i;
  }

  /* compute the first four areas */
  ar[0]= xz[0]*yr - xr*yz[0];
  ar[1]= xz[1]*yr - xr*yz[1];
  ar[2]= xz[2]*yr - xr*yz[2];
  ar[3]= xz[3]*yr - xr*yz[3];

  /* compute winding number for any crossed edge
     and edge number of intersection nearest (xx,yy) */
  wind= 0;
  negative= ar[0]<0.;
  if ((ar[1]<0.)!=negative) {
    sn= xz[0]*yz[1] - xz[1]*yz[0];
    if (sn > 0.) { wind++;  edge= 0; }
    else { wind--;   sn= -sn;  edge= 4; }
    sd= ar[0] - ar[1];
    if (negative) sd= -sd;
    negative= !negative;
  } else {
    sn= 1.0;
    sd= 0.0;
    edge= 8;
  }
  if ((ar[2]<0.)!=negative) {
    double sn0= xz[1]*yz[2] - xz[2]*yz[1];
    double sd0= ar[1] - ar[2];
    int plus= sn0 > 0.;
    if (plus) wind++;
    else { wind--;   sn0= -sn0; }
    if (negative) sd0= -sd0;
    negative= !negative;
    if (sn0*sd<sn*sd0) {
      sn= sn0;
      sd= sd0;
      edge= plus? 1 : 5;
    }
  }
  if ((ar[3]<0.)!=negative) {
    double sn0= xz[2]*yz[3] - xz[3]*yz[2];
    double sd0= ar[2] - ar[3];
    int plus= sn0 > 0.;
    if (plus) wind++;
    else { wind--;   sn0= -sn0; }
    if (negative) sd0= -sd0;
    negative= !negative;
    if (sn0*sd<sn*sd0) {
      sn= sn0;
      sd= sd0;
      edge= plus? 2 : 6;
    }
  }
  if ((ar[0]<0.)!=negative) {
    double sn0= xz[3]*yz[0] - xz[0]*yz[3];
    double sd0= ar[3] - ar[0];
    int plus= sn0 > 0.;
    if (plus) wind++;
    else { wind--;   sn0= -sn0; }
    if (negative) sd0= -sd0;
    if (sn0*sd<sn*sd0) {
      sn= sn0;
      sd= sd0;
      edge= plus? 3 : 7;
    }
  }

  for (;;) {
    /* quit if we're lost */
    if (edge&8) break;

    /* quit if we have a winding number */
    if (wind) return i;

    /* set winding number for edge we just crossed */
    if (edge&4) {
      /* decremented wind on other side, increment here */
      wind++;
      edge-= 4;
    } else {
      /* incremented wind on other side, decrement here */
      wind--;
    }

    /* move into the new zone and find exit edge
       -- one case for each of the four possible edges */
    if (edge==0) {
      i-= ix;
      if (i<=ix || (reg && !reg[i])) break;
      xz[2]= xz[1];        yz[2]= yz[1];
      xz[3]= xz[0];        yz[3]= yz[0];
      xz[0]= x[i-ix-1]-xx;   yz[0]= y[i-ix-1]-yy;
      xz[1]= x[i-ix]-xx;     yz[1]= y[i-ix]-yy;
      ar[2]= ar[1];
      ar[3]= ar[0];
      ar[0]= xz[0]*yr - xr*yz[0];
      ar[1]= xz[1]*yr - xr*yz[1];
      negative= ar[0]<0.;
      if ((ar[1]<0.)!=negative) {
        sn= xz[0]*yz[1] - xz[1]*yz[0];
        if (sn > 0.) { wind++;  edge= 0; }
        else { wind--;   sn= -sn;  edge= 4; }
        sd= ar[0] - ar[1];
        if (negative) sd= -sd;
        negative= !negative;
      } else {
        edge= 8;
      }
      if ((ar[2]<0.)!=negative) {
        double sn0= xz[1]*yz[2] - xz[2]*yz[1];
        double sd0= ar[1] - ar[2];
        int plus= sn0 > 0.;
        if (plus) wind++;
        else { wind--;   sn0= -sn0; }
        if (negative) sd0= -sd0;
        negative= !negative;
        if ((edge&8) || sn0*sd<sn*sd0) {
          sn= sn0;
          sd= sd0;
          edge= plus? 1 : 5;
        }
      }
      negative= !negative;  /* we know we crossed edge 2 */
      if ((ar[0]<0.)!=negative) {
        double sn0= xz[3]*yz[0] - xz[0]*yz[3];
        double sd0= ar[3] - ar[0];
        int plus= sn0 > 0.;
        if (plus) wind++;
        else { wind--;   sn0= -sn0; }
        if (negative) sd0= -sd0;
        if ((edge&8) || sn0*sd<sn*sd0) {
          sn= sn0;
          sd= sd0;
          edge= plus? 3 : 7;
        }
      }

    } else if (edge==1) {
      i+= 1;
      if (!(i%ix) || (reg && !reg[i])) break;
      xz[0]= xz[1];      yz[0]= yz[1];
      xz[3]= xz[2];      yz[3]= yz[2];
      xz[1]= x[i-ix]-xx;     yz[1]= y[i-ix]-yy;
      xz[2]= x[i]-xx;        yz[2]= y[i]-yy;
      ar[0]= ar[1];
      ar[3]= ar[2];
      ar[1]= xz[1]*yr - xr*yz[1];
      ar[2]= xz[2]*yr - xr*yz[2];
      negative= ar[0]<0.;
      if ((ar[1]<0.)!=negative) {
        sn= xz[0]*yz[1] - xz[1]*yz[0];
        if (sn > 0.) { wind++;  edge= 0; }
        else { wind--;   sn= -sn;  edge= 4; }
        sd= ar[0] - ar[1];
        if (negative) sd= -sd;
        negative= !negative;
      } else {
        edge= 8;
      }
      if ((ar[2]<0.)!=negative) {
        double sn0= xz[1]*yz[2] - xz[2]*yz[1];
        double sd0= ar[1] - ar[2];
        int plus= sn0 > 0.;
        if (plus) wind++;
        else { wind--;   sn0= -sn0; }
        if (negative) sd0= -sd0;
        negative= !negative;
        if ((edge&8) || sn0*sd<sn*sd0) {
          sn= sn0;
          sd= sd0;
          edge= plus? 1 : 5;
        }
      }
      if ((ar[3]<0.)!=negative) {
        double sn0= xz[2]*yz[3] - xz[3]*yz[2];
        double sd0= ar[2] - ar[3];
        int plus= sn0 > 0.;
        if (plus) wind++;
        else { wind--;   sn0= -sn0; }
        if (negative) sd0= -sd0;
        if ((edge&8) || sn0*sd<sn*sd0) {
          sn= sn0;
          sd= sd0;
          edge= plus? 2 : 6;
        }
      }

    } else if (edge==2) {
      i+= ix;
      if (i>=ijx || (reg && !reg[i])) break;
      xz[0]= xz[3];     yz[0]= yz[3];
      xz[1]= xz[2];     yz[1]= yz[2];
      xz[2]= x[i]-xx;        yz[2]= y[i]-yy;
      xz[3]= x[i-1]-xx;      yz[3]= y[i-1]-yy;
      ar[0]= ar[3];
      ar[1]= ar[2];
      ar[2]= xz[2]*yr - xr*yz[2];
      ar[3]= xz[3]*yr - xr*yz[3];
      negative= ar[1]<0.;
      if ((ar[2]<0.)!=negative) {
        sn= xz[1]*yz[2] - xz[2]*yz[1];
        if (sn > 0.) { wind++;  edge= 1; }
        else { wind--;   sn= -sn;  edge= 5; }
        sd= ar[1] - ar[2];
        if (negative) sd= -sd;
        negative= !negative;
      } else {
        edge= 8;
      }
      if ((ar[3]<0.)!=negative) {
        double sn0= xz[2]*yz[3] - xz[3]*yz[2];
        double sd0= ar[2] - ar[3];
        int plus= sn0 > 0.;
        if (plus) wind++;
        else { wind--;   sn0= -sn0; }
        if (negative) sd0= -sd0;
        negative= !negative;
        if ((edge&8) || sn0*sd<sn*sd0) {
          sn= sn0;
          sd= sd0;
          edge= plus? 2 : 6;
        }
      }
      if ((ar[0]<0.)!=negative) {
        double sn0= xz[3]*yz[0] - xz[0]*yz[3];
        double sd0= ar[3] - ar[0];
        int plus= sn0 > 0.;
        if (plus) wind++;
        else { wind--;   sn0= -sn0; }
        if (negative) sd0= -sd0;
        if ((edge&8) || sn0*sd<sn*sd0) {
          sn= sn0;
          sd= sd0;
          edge= plus? 3 : 7;
        }
      }

    } else if (edge==3) {
      i-= 1;
      if (!(i%ix) || (reg && !reg[i])) break;
      xz[1]= xz[0];        yz[1]= yz[0];
      xz[2]= xz[3];        yz[2]= yz[3];
      xz[0]= x[i-ix-1]-xx;   yz[0]= y[i-ix-1]-yy;
      xz[3]= x[i-1]-xx;      yz[3]= y[i-1]-yy;
      ar[1]= ar[0];
      ar[2]= ar[3];
      ar[0]= xz[0]*yr - xr*yz[0];
      ar[3]= xz[3]*yr - xr*yz[3];
      negative= ar[0]<0.;
      if ((ar[1]<0.)!=negative) {
        sn= xz[0]*yz[1] - xz[1]*yz[0];
        if (sn > 0.) { wind++;  edge= 0; }
        else { wind--;   sn= -sn;  edge= 4; }
        sd= ar[0] - ar[1];
        if (negative) sd= -sd;
        negative= !negative;
      } else {
        edge= 8;
      }
      negative= !negative;  /* we know we crossed edge 1 */
      if ((ar[3]<0.)!=negative) {
        double sn0= xz[2]*yz[3] - xz[3]*yz[2];
        double sd0= ar[2] - ar[3];
        int plus= sn0 > 0.;
        if (plus) wind++;
        else { wind--;   sn0= -sn0; }
        if (negative) sd0= -sd0;
        negative= !negative;
        if ((edge&8) || sn0*sd<sn*sd0) {
          sn= sn0;
          sd= sd0;
          edge= plus? 2 : 6;
        }
      }
      if ((ar[0]<0.)!=negative) {
        double sn0= xz[3]*yz[0] - xz[0]*yz[3];
        double sd0= ar[3] - ar[0];
        int plus= sn0 > 0.;
        if (plus) wind++;
        else { wind--;   sn0= -sn0; }
        if (negative) sd0= -sd0;
        if ((edge&8) || sn0*sd<sn*sd0) {
          sn= sn0;
          sd= sd0;
          edge= plus? 3 : 7;
        }
      }

    }
  }

  /* if we have a boundary array, we can figure out whether we are
     really outside the mesh or if we just need to try harder */
 check:
  if (!w && bndy) {
    long imin= nbndy;
    double cross, den= 0.;
    for (i=0 ; i<nbndy ; i+=2) {
      xz[0]= x[bndy[i]] - xx;
      xz[1]= x[bndy[i+1]] - xx;
      if ((xz[0]<0.)==(xz[1]<0.)) continue;
      yz[0]= y[bndy[i]] - yy;
      yz[1]= y[bndy[i+1]] - yy;
      cross= xz[0]*yz[1] - xz[1]*yz[0];
      if (cross > 0.) w++;
      else { w--;  cross= -cross; }
      den= xz[0] - xz[1];
      if (xz[0]<0.) den= -den;
      if (imin==nbndy || sn*den>sd*cross) {
        /* keep track of x=xx boundary crossing nearest to (xx,yy) */
        imin= i;
        sn= cross;
        sd= den;
      }
    }

    if (w) {
      /* (xx,yy) is inside the mesh -- start over
         from boundary intersection closest to (xx,yy) this time */
      long j= bndy[imin+1];
      i= bndy[imin];
      yr= y[i] + (xx-x[i])/(x[j]-x[i]);
      xr= 0.0;
      if (i+1 == j) i= j+ix;
      else if (i+ix == j) i= j;
      else if (i-ix == j) i= i+1;
      bcheck= 1;
      goto start;
    }
  }

  return -1;
}

/*--------------------------------------------------------------------------*/

long *BuildMeshBndy(double *x, double *y, int *reg,
                    long ix, long ijx, long *nbndy)
{
  Array *barray;
  long *bndy;
  long i, j, jx= ijx/ix;
  long nedges;

  if (!reg) {
    nedges= 2*(ix+jx) - 4;
  } else {
    long i0= 0;
    nedges= 0;
    /* loop over zones */
    for (i=ix+1 ; i<ijx ; i++) {
      if ((++i0)==ix) { i0= 1; i++; }
      /* check if j=lower edge of zone is a boundary */
      if ((reg[i-ix]!=0)!=(reg[i]!=0)) nedges++;
      /* check if i=lower edge of zone is a boundary */
      if ((reg[i-1]!=0)!=(reg[i]!=0)) nedges++;
    }
    /* check i=upper edge of whole mesh */
    for (i=2*ix-1 ; i<ijx ; i+=ix) if (reg[i]!=0) nedges++;
    /* check j=upper edge of whole mesh */
    for (i=ijx-ix+1 ; i<ijx ; i++) if (reg[i]!=0) nedges++;
  }

  /* allocate workspace on stack to hold endpoint indices of the
     boundary segments */
  CheckStack(1);
  nedges*= 2;
  barray= 
    PushDataBlock(NewArray(&longStruct,
                           NewDimension(nedges, 1L, (Dimension *)0)));
  barray->type.dims->references--;
  bndy= barray->value.l;
  *nbndy= nedges;

  j= 0;
  if (!reg) {
    /* walk boundary with mesh to left */
    for (i=0 ; i<ix-1 ; i++) {
      bndy[j++]= i;
      bndy[j++]= i+1;
    }
    for (i=ix-1 ; i<ijx-ix ; i+=ix) {
      bndy[j++]= i;
      bndy[j++]= i+ix;
    }
    for (i=ijx-1 ; i>ijx-ix ; i--) {
      bndy[j++]= i;
      bndy[j++]= i-1;
    }
    for (i=ijx-ix ; i>0 ; i-=ix) {
      bndy[j++]= i;
      bndy[j++]= i-ix;
    }

  } else {
    long i0= 0;
    /* loop over zones */
    for (i=ix+1 ; i<ijx ; i++) {
      if ((++i0)==ix) { i0= 1; i++; }
      /* check if j=lower edge of zone is a boundary */
      if ((reg[i-ix]!=0)!=(reg[i]!=0)) {
        if (reg[i]) {
          bndy[j++]= i-ix-1;
          bndy[j++]= i-ix;
        } else {
          bndy[j++]= i-ix;
          bndy[j++]= i-ix-1;
        }
      }
      /* check if i=lower edge of zone is a boundary */
      if ((reg[i-1]!=0)!=(reg[i]!=0)) {
        if (reg[i]) {
          bndy[j++]= i-1;
          bndy[j++]= i-1-ix;
        } else {
          bndy[j++]= i-1-ix;
          bndy[j++]= i-1;
        }
      }
    }
    /* check i=upper edge of whole mesh */
    for (i=2*ix-1 ; i<ijx ; i+=ix) if (reg[i]!=0) {
      bndy[j++]= i-ix;
      bndy[j++]= i;
    }
    /* check j=upper edge of whole mesh */
    for (i=ijx-ix+1 ; i<ijx ; i++) if (reg[i]!=0) {
      bndy[j++]= i;
      bndy[j++]= i-1;
    }
  }

  return bndy;
}

/*--------------------------------------------------------------------------*/
