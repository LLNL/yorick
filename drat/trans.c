/*
 * $Id: trans.c,v 1.1 2005-09-18 22:04:57 dhmunro Exp $
 * Routines for transporting a ray through a cylindrical mesh.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "trans.h"
/* includes track.h and bound.h as well */

extern double exp(double);
extern double sqrt(double);
extern double fabs(double);
#define SQ(x) ((x)*(x))
#define FUZZ 1.0e-99

#ifdef NLTSS
/* Let Hybrid-C compiler find vector versions of exp, sqrt, etc. */
#include <lcc.h>
#endif
/* Also a CYCRED switch below, originally intended for Crays, but
   optimized incorrectly there when last checked.  */

/* 1-exp(-x) != x for small x.  For x<ONE_MINUS_EXP, just use x. */
#define ONE_MINUS_EXP 1.0e-4

/* ---------------------------------------------------------------------- */

/* Solve the transport equation assuming that both the opacity and
 * source function are constant within each zone.  Return the overall
 * absorption and self-emission along the ray.
 */
void
FlatSource(double *opac, double *source,        /* opacity, emissivity */
           long kxlm, long ngroup,              /* opac, source dimens */
           RayPath *path,                       /* ray path through mesh */
           double *absorb, double *selfem,      /* attenuation factor and
                                                 * self-emission for
                                                 * entire path */
           double *work)                        /* scratch space */
{
    /* Formulas:
     *   The transport equation is  d(inu)/ds= (knu)(jnu - inu), or
     *          d(inu)/d(tau)= jnu - inu.
     * In a region where knu and jnu are constant, the solution is
     *          inu= exp(-tau)*inu0 + (1-exp(-tau))*jnu.
     *
     * Thus, integrating inu along the path requires solving the
     * first order recurrence:
     *          x[i+1]= a[i]*x[i] + b[i]
     * where x is inu, a is exp(-tau), and b is (1-exp(-tau))*jnu.
     *
     * The solution to this recurrence is:
     *          inu= (absorb)*inu0 + (selfem)
     * where absorb is the product of all a[i], and selfem is x[last]
     * with x[0]=0.  This total attenuation factor and self-emission
     * specific intensity (in same units as input source function) are
     * the outputs of FlatSource.
     */

   long nzones= path->ncuts-1;
   long *zone= path->zone;
   double *ds= path->ds;
   double *tau= work;
   double *atten= tau+nzones;
   double *jnu= atten+nzones;
   long n,i;
   register long j;

   if (nzones<1) {
     if (absorb && selfem) {
       for (n=0 ; n<ngroup ; n++) {
         *absorb++= 1.0;
         *selfem++= 0.0;
       }
     }
     return;
   }

    /* Eventually, may want to branch to a separate algorithm if the
     * number of groups is very large, but the number of edge crossings
     * is small...
     * For now, the slightly less efficient inner loop on crossings
     * is a better choice, since the number of groups is often quite
     * small, while the number of crossings is rarely small.
     */
   for (n=0 ; n<ngroup ; n++) {

       /* Gather the opacity and source function along the ray path.
        * Assume that opac[0]==0 and source[0]==0, so no special coding
        * required for non-contiguous sections of the path.
        */
/* #pragma nohazard */
      for (i=0 ; i<nzones ; i++) {
         j= zone[i];
         tau[i]= opac[j]*ds[i];
         atten[i]= exp(-tau[i]);
         jnu[i]= source[j];
      }

       /* Replace jnu by appropriate "b" function.
        * Note that (1-exp(-tau))*jnu --> tau*jnu as tau-->0 may be
        * important even for very small tau... */
/* #pragma nohazard */
      for (i=0 ; i<nzones ; i++) {
         if (fabs(tau[i]) > ONE_MINUS_EXP) jnu[i]*= (1.0-atten[i]);
         else jnu[i]*= tau[i];
      }

       /* Solve the first order recurrence. */
      Reduce(atten,jnu,nzones); /* clobbers atten, jnu arrays... */
      *absorb= atten[0];
      *selfem= jnu[0];

      absorb++;
      selfem++;
      opac+= kxlm;
      source+= kxlm;
   }

}

/* ---------------------------------------------------------------------- */

/* Solve the transport equation assuming that the source function
 * varies linearly with optical depth across a zone.  The opacity is
 * assumed to be constant within each zone.  (Hence, opac must be zone
 * centered, while source is point centered.)  Return the overall
 * absorption and self-emission along the ray.
 */
void
LinearSource(double *opac, double *source,      /* opacity, emissivity */
             long kxlm, long ngroup,            /* opac, source dimens */
             RayPath *path,                     /* ray path through mesh */
             double *absorb, double *selfem,    /* attenuation factor and
                                                 * self-emission for
                                                 * entire path */
             double *work)                      /* scratch space */
{
    /* Formulas:
     *   The transport equation is  d(inu)/ds= (knu)(jnu - inu), or
     *          d(inu)/d(tau)= jnu - inu.
     * In a region where knu is constant and jnu linear, the solution is
     *          inu1= exp(-tau)*inu0 + (1-exp(-tau))*jnu0 +
     *                      (1-(1-exp(-tau))/tau)*(jnu1-jnu0).
     *
     * Thus, integrating inu along the path requires solving the
     * first order recurrence:
     *          x[i+1]= a[i]*x[i] + b[i]
     * where x is inu, a is exp(-tau), and b is (1-exp(-tau))*jnu0+
     *                            (1-(1-exp(-tau))/tau)*(jnu1-jnu0).
     *
     * The solution to this recurrence is:
     *          inu= (absorb)*inu0 + (selfem)
     * where absorb is the product of all a[i], and selfem is x[last]
     * with x[0]=0.  This total attenuation factor and self-emission
     * specific intensity (in same units as input source function) are
     * the outputs of LinearSource.
     */

   long nzones= path->ncuts-1;
   long *zone= path->zone;
   double *ds= path->ds;
   long *pt1= path->pt1;
   long *pt2= path->pt2;
   double *f= path->f;
   double fi= path->fi;
   double ff= path->ff;
   double *tau= work;
   double *atten= tau+nzones;
   double *jnu= atten+nzones;   /* jnu has nzones+1 elements! */
   long n,i;
   register long j,k;
   register double f1,f2;

   if (nzones<1) {
     if (absorb && selfem) {
       for (n=0 ; n<ngroup ; n++) {
         *absorb++= 1.0;
         *selfem++= 0.0;
       }
     }
     return;
   }

    /* Eventually, may want to branch to a separate algorithm if the
     * number of groups is very large, but the number of edge crossings
     * is small...
     * For now, the slightly less efficient inner loop on crossings
     * is a better choice, since the number of groups is often quite
     * small, while the number of crossings is rarely small.
     */
   for (n=0 ; n<ngroup ; n++) {

       /* Gather the opacity and source function along the ray path.
        * Assume that opac[0]==0 and source[0]==0, so no special coding
        * required for non-contiguous sections of the path.
        */
/* #pragma nohazard */
      for (i=0 ; i<nzones ; i++) {
         j= zone[i];
         tau[i]= opac[j]*ds[i];
         atten[i]= exp(-tau[i]);
      }
       /* Interpolate source to crossing point on each edge */
/* #pragma nohazard */
      for (i=0 ; i<=nzones ; i++) {
         j= pt1[i];   k= pt2[i];
         jnu[i]= (0.5-f[i])*source[j] + (0.5+f[i])*source[k];
      }
       /* Interpolate source into interior of first and last zone */
      f1= (1.0-fi)*jnu[0]+fi*jnu[1];    /* nzones=1 is possible... */
      jnu[nzones]= ff*jnu[nzones-1]+(1.0-ff)*jnu[nzones];
      jnu[0]= f1;

       /* Replace jnu by appropriate "b" function.
        * Note that (1-exp(-tau)-tau)*jnu --> (tau^2/2)*jnu as tau-->0
        * may be important even for very small tau...
        * Vectorizing algorithm requires small positive random number
        * in order to make zero divide very unlikely.  Magnitude is
        * chosen in order to have sub-roundoff effect on
        * tau > ONE_MINUS_EXP. */
#define RANDOM_SMALL_TAU (1.5261614e-20*ONE_MINUS_EXP)
       /* Note: this vectorizes on Cray as of 3/jul/90 */
/* #pragma nohazard */
      for (i=0 ; i<nzones ; i++) {
         f1= -atten[i]+(1-atten[i])/(tau[i]+RANDOM_SMALL_TAU);
         f2=  1.0     -(1-atten[i])/(tau[i]+RANDOM_SMALL_TAU);
         if (fabs(tau[i]) > ONE_MINUS_EXP) {
            jnu[i]= f1*jnu[i]+f2*jnu[i+1];
         } else {
            jnu[i]= 0.5*tau[i]*(jnu[i]+jnu[i+1]);
         }
      }

       /* Solve the first order recurrence. */
      Reduce(atten,jnu,nzones); /* clobbers atten, jnu arrays... */
      *absorb= atten[0];
      *selfem= jnu[0];

      absorb++;
      selfem++;
      opac+= kxlm;
      source+= kxlm;
   }

}

/* ---------------------------------------------------------------------- */

/* Reduce x[i+1]= a[i]*x[i]+b[i] for i=0,...,n-1 to
 *          x[n]= a[0]*x[0]+b[0]     -i.e.- return a[0], b[0]
 * May use an algorithm which clobbers a[1:n-1], b[1:n-1] as well.
 */
void Reduce(double *a, double *b, long n)
{
   long i;

#ifndef CYCRED
    /* on a non-vector machine, use the straightforward algorithm */
   register double tmpa,tmpb;

   tmpa= a[0];   tmpb= b[0];   /* miscoded tmpa*b[0] originally?? */
   for (i=1 ; i<n ; i++) {
      tmpa*= a[i];
      tmpb= tmpb*a[i]+b[i];
   }
   a[0]= tmpa;   b[0]= tmpb;

#else
    /* on vector machine, a cyclic reduction algorithm is faster */
   while (n > 2) {      /* log2(n) reduction passes */
/* #pragma nohazard */
      for (i=0 ; i<n/2 ; i++) {
         a[i]= a[2*i]*a[2*i+1];
         b[i]= b[2*i]*a[2*i+1]+b[2*i+1];
      }
      if (n & 01) {     /* keep final odd point if present */
         a[i]= a[2*i];
         b[i]= b[2*i];
         n= i+1;
      } else n= i;
   }
   if (n == 2) {
      a[0]= a[0]*a[1];
      b[0]= b[0]*a[1]+b[1];
   }
#endif
}

/* ---------------------------------------------------------------------- */

/* The LinearSource transport solver requires a point centered source
 * function; the given source function is zone centered.
 * This point centering scheme is local to the four zones surrounding
 * the point, with weights which are large for zones whose optical
 * depth (along either k or l) is near 1.
 * The algorithm requires 4*(kmax*(lmax+1)+1) doubles as working space.
 *
 * An unrelated Milne condition is applied for points which lie on
 * the vacuum boundary of the problem.
 */
void PtCenterSource(double *opac, double *source, long kxlm, long ngroup,
                    Mesh *mesh, Boundary *boundary, double *work)
/* Historical note: This algorithm should be algebraically equivalent
 * to the algorithm used in DIRT or TDG, excepting a case in the
 * boundary correction involving a boundary between non-void zones,
 * which can, in fact, never occur.
 */
{
  long klmax= mesh->klmax;
  long kmax= mesh->kmax;
  double *z= mesh->z;
  double *r= mesh->r;
  int *ireg= mesh->ireg;

  /* partition the working space */
  double *wp= work;                     /* wp only needs kmax*lmax... */
  double *wk= wp+klmax+kmax+1;
  double *wl= wk+klmax+kmax+1;
  double *b=  wl+klmax+kmax+1;

  long i,n;
  register double area,dddk,dddl;
  register double w00,w01,w10,w11;

  /* applying of Milne condition at boundaries logically complex... */
  long npoints= boundary->npoints;
  long *zone= boundary->zone;
  int *side= boundary->side;
  /* zone+pt1[side] --> zone+pt2[side] is boundary edge */
  long pt1index[4]= { -1L, -1L /* -kmax */, 0L /* -kmax */, 0L };
  long pt2index[4]= { 0L, -1L, -1L /* -kmax */, 0L /* -kmax */ };
  /* zone+zon[side] is zone interior to boundary by one more than zone */
  long zonindex[4]= { 0L /* -kmax */, +1L, 0L /* +kmax */, -1L };
  double b1,w1,milne1,b2,w2,milne2,dx,dtau,bx,dtaux,b1b,w1b,milne1b;
  long j1,j2,jz,jx,j1b;
  int beginSection;

  /* without this, not obvious that these are initialized */
  b1= w1= milne1= b2= w2= milne2= b1b= w1b= milne1b= 0.0;
  j2= j1b= 0;

  pt1index[1]-= kmax;
  pt1index[2]-= kmax;
  pt2index[2]-= kmax;
  pt2index[3]-= kmax;
  zonindex[0]-= kmax;
  zonindex[2]+= kmax;

  /* zero workspace in guard zones */
  for (i=0 ; i<=kmax ; i++) wk[i]= wl[i]= 0.0;
  for (i=klmax ; i<klmax+kmax+1 ; i++) wk[i]= wl[i]= b[i]= 0.0;

  for (n=0 ; n<ngroup ; n++) {

    /* Compute weighting factors -- zone centered, but separate
     * values wk, wl for the delta-k and delta-l directions. */
/* #pragma nohazard */
    for (i=kmax+1 ; i<klmax ; i++) {
      /* area is actually twice zonal area */
      area= (z[i]-z[i-kmax-1])*(r[i-1]-r[i-kmax]) -
        (z[i-1]-z[i-kmax])*(r[i]-r[i-kmax-1]);
      /* dddk, dddl actually twice median lengths */
      dddk= sqrt( SQ(z[i]-z[i-1] + z[i-kmax]-z[i-kmax-1]) +
                 SQ(r[i]-r[i-1] + r[i-kmax]-r[i-kmax-1]));
      dddl= sqrt( SQ(z[i]-z[i-kmax] + z[i-1]-z[i-kmax-1]) +
                 SQ(r[i]-r[i-kmax] + r[i-1]-r[i-kmax-1]));
      if (ireg[i]) {
        wp[i]= 1.0/(opac[i]*area+FUZZ);
        wk[i]= wp[i]*SQ(1.0-exp(-0.5*opac[i]*dddk));
        wl[i]= wp[i]*SQ(1.0-exp(-0.5*opac[i]*dddl));
      } else {
        wp[i]= wk[i]= wl[i]= 0.0;
      }
    }

    /* Copy source to temporary to add guard zones */
/* #pragma nohazard */
    for (i=0 ; i<klmax ; i++) b[i]= source[i];

    /* Point centered source is weighted average over
     * the four surrounding edges, each of which is centered
     * between its two bounding zones. */
/* #pragma nohazard */
    for (i=0 ; i<klmax ; i++) {
      w00= wk[i]+       wl[i];
      w10= wk[i+1]+     wl[i+1];
      w01= wk[i+kmax]+  wl[i+kmax];
      w11= wk[i+kmax+1]+wl[i+kmax+1];
      b[i]= (b[i]*w00+b[i+1]*w10+b[i+kmax]*w01+b[i+kmax+1]*w11) /
        (w00+w01+w10+w11+FUZZ);
    }

    /* Instead of interior weighting, use Milne condition at
     * (vacuum) boundaries.
     * Note that r=0 or z=0 symmetry boundaries are not in the list,
     * because FindBoundaryPoints will not find them.  TrimBoundary
     * must be used to eliminate khold, lhold, and norad boundaries. */
    /* Note that this loop is usually much shorter than the others. */
    beginSection= 1;
    for (i=0 ; i<npoints ; i++) {
      /* Get point indices j1, j2, zone index jz */
      jz= zone[i];
      if (jz) {
        if (beginSection) {
          j1= jz + pt1index[side[i]];
        } else {
          j1= j2;
          b1= b2;   w1= w2;   milne1= milne2;
        }
        j2= jz + pt2index[side[i]];
        b2= source[jz];
        if (side[i]&01) w2= wk[jz];
        else            w2= wl[jz];

        /* dtau is half the optical depth of the zone perpendicular
         * to the boundary edge -- i.e.- the optical depth from
         * zone center to boundary.  The Milne factor reduces the
         * source so a linear extrapolation reaches 0.0 at
         * optical depth 2/3 beyond the zone boundary.  If dtau>1
         * and the source function is decreasing toward the boundary,
         * then extrapolate the source function to optical depth 1
         * before applying the Milne reduction. */
        dx= sqrt(SQ(z[j2]-z[j1])+SQ(r[j2]-r[j1]));
        dtau= 0.25/(dx*wp[jz]+FUZZ);
        if (dtau>1.0 && ireg[jx= jz+zonindex[side[i]]]) {
          bx= source[jx];
          dtaux= 0.25/(dx*wp[jx]+FUZZ);
          if (bx > b2) {  /* flux (gradient) out of boundary */
            if (b2*(dtau+dtaux) > (bx-b2)*(dtau+(2./3.)))
              /* effectively extrapolate to optical depth 1 first */
              dtau= 1.0+(5./3.)*(bx-b2)*(dtau-1.0)/
                (b2*(dtau+dtaux)-(bx-b2)*(dtau-1.0));
          } else dtau= 1.0; /* Huh? At least this is continuous... */
        }
        milne2= 1.0+1.5*dtau;

        /* Source at boundary point is weighted mean between the
         * two adjacent boundary edges. */
        if (beginSection) {
          beginSection= 0;
          j1b= j1;
          b1b= b2;   w1b= w2;   milne1b= milne2;
          b[j1]= b2 / (milne2+FUZZ);  /* if closed boundary, changed later */
        } else {
          b[j1]= (b1*w1+b2*w2) / (w1*milne1+w2*milne2+FUZZ);
        }

      } else if (!beginSection) {       /* final point of boundary section */
        beginSection= 1;
        if (j2 == j1b) {  /* handle closed boundary */
          b[j2]= (b1b*w1b+b2*w2) / (w1b*milne1b+w2*milne2+FUZZ);
        } else {  /* open boundary */
          b[j2]= b2 / (milne2+FUZZ);
        }
      }

    }

    /* Copy point centered source over zone centered input */
/* #pragma nohazard */
    for (i=0 ; i<klmax ; i++) source[i]= b[i];

    source+= kxlm;
    opac+= kxlm;
  }
}

/* ---------------------------------------------------------------------- */
