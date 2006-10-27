/*
 * $Id: track.c,v 1.3 2006-10-27 06:10:00 dhmunro Exp $
 * Routines for tracking a straight ray in 3D through a cylindrical mesh.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "track.h"
#include "pstdlib.h"

/* math functions used in this files (from ANSI math.h) */
extern double fabs(double);
extern double sqrt(double);
#define SQ(x) ((x)*(x))
#define BIG 1.0e99

/* Size increment for ray path arrays */
#define PATHINC 256L

/* Global variables control the root polishing routine PolishExit */
int polishRoot= 1;
double polishTol1= 1.0e-3;
double polishTol2= 1.0e-6;
/* Global variable controlling roundoff tolerance in FindLostRay */
/* Setting this too large could result in an infinite loop, but in
 * any such situation, FindLostRay will be unable to find the ray
 * no matter what the setting of findRayTol */
double findRayTol= 0.0;

extern long SeekValue(double value, double *list, long n);

/* ---------------------------------------------------------------------- */

/* Definition of zone side and point numbering:
 *
 *              (zone-1)-------<--------(zone)
 *                  |         side         |
 *                  |          0           |
 *                  |                      |    
 *                  |                      |    ^
 *                  |side              side|    |
 *                  V  1     (zone)     3  ^    |
 *                  |                      |    L
 *                  |                      |     K--- >
 *                  |                      |
 *                  |                      |
 *                  |          side        |
 *                  |           2          |
 *              (zone-kmax-1)--->--(zone-kmax)
 *
 *
 */

#define NBLOCK_SIZE 8
static EntryPoint *entryBlock= 0;
static EntryPoint *nextEntry= 0;

/* Given the mesh boundary (i.e.- list of boundary edges) and a ray,
 * return a linked list of entry point(s), ordered by increasing time.
 */
EntryPoint *FindEntryPoints(Boundary *boundary, Ray *rayin)
{
    /* Caller is responsible for freeing the memory associated with
     * the returned pointer
     */
   Ray ray;
   double *z,*r,fex,ds;
   long *zone;
   int *side;
   RayEdgeInfo info;
   EntryPoint *entry,*newentry;
   int after /* ,notafter */;
   long i;
   int izsym;
   int zsym = boundary->zsym;
   if (zsym > 2) zsym = 0;

   entry= 0;
   ray= *rayin;
   ray.sin= -ray.sin;  /* time reverse ray so that exit points... */
   ray.cos= -ray.cos;  /* ...become entry points */
    /* two passes if zsym, else single pass */
   for (izsym= zsym?2:1 ; izsym ; izsym--) {

      z= boundary->z;
      r= boundary->r;
      zone= boundary->zone;
      side= boundary->side;

      after= /* notafter= */ 0;
      for (i=0 ; i<boundary->npoints-1 ; i++, z++, r++, zone++, side++) {
        if (*zone && ExitEdge(&ray, z,r, &after/*, &notafter */, &info)) {
          /* Unlike the exit from a zone which is known to have been
             entered, the after/notafter logic is incorrect for
             identifying entries to the whole problem boundary.
             In particular, to discriminate against false entries,
             check here that fex is reasonably (though arbitrarily)
             close to lying within the [-0.5, 0.5] interval.  */
          fex= info.fx;
          if (fex<-0.5000005 || fex>0.5000005) {
            /* notafter= 0; */   /* disable notafter as below */
            continue;
          }

          /* found an entry point, get next free EntryPoint for it */
          newentry= nextEntry;
          if (!newentry) {
            long n= NBLOCK_SIZE;
            nextEntry= (EntryPoint *)p_malloc(sizeof(EntryPoint)*n);
            /* first element of each block is used to form a
               linked list of the blocks -- not a valid EntryPoint */
            nextEntry->next= entryBlock;
            entryBlock= nextEntry;
            while (--n) {
              nextEntry++;
              nextEntry->next= newentry;   /* 0 on first pass */
              newentry= nextEntry;
            }
          }
          nextEntry= newentry->next;
          newentry->next= entry;
          entry= newentry;

          ds= RayPathLength(&ray, &info);
          entry->zone= *zone;
          entry->side= *side;
          entry->info= info;    /* not altered by time reversal */
          entry->ray.cos= -ray.cos;
          entry->ray.sin= -ray.sin;
          entry->ray.y= ray.y;
          entry->ray.z= *z + (fex+0.5)*info.dz;
          entry->ray.r= *r + (fex+0.5)*info.dr;
          entry->ray.x= ray.x + ds*ray.sin;

          if (polishRoot)     /* polish the root */
            PolishExit(&entry->ray, &info, &ds, &fex);
          /* make sure exit point is actually on exit edge segment
           * if not, put it at endpoint, and adjust the ray (x,y) */
          if (fex < -0.5) {
            fex= -0.5;
            AdjustRayXY(&entry->ray, z,r);
          } else if (fex > 0.5) {
            fex= 0.5;
            AdjustRayXY(&entry->ray, z+1,r+1);
          }

          entry->f= fex;      /* boundary counterclockwise relative to
                               * zone, side */
          entry->s0= -ds;     /* undo ray time-reversal */

        } else {
          /* The "double exit" rejection logic in ExitEdge is not
             correct for whole problem boundaries, so disable it
             here.  Leave the "missed exit" logic intact however;
             incorrectly reinstated roots are rejected in the
             if branch corresponding to this else.  */
          /* notafter= 0; */
        }
        /* Note: Conceivably could miss an entry point due to
         * roundoff, if the ray had a very near tangency to a
         * boundary edge precisely at a boundary point.
         * (ExitEdge might (erroniously) return 0 after the entry
         *  point (erroniously) fell outside [-0.5,0.5].)
         * Hopefully, this is not important enough to worry about...
         */
      }

       /* abort symmetry pass if ray lies in symmetry plane */
      if (ray.cos==0.0 && ray.z==0.0) break;
       /* symmetry pass will use reflected ray */
      ray.cos= -ray.cos;
      ray.z= -ray.z;
   }

   return EntrySort(entry);
}

void FreeEntryPoints(EntryPoint *entry)
{
   EntryPoint *next;
   while (entry) {
      next= entry->next;
      /* add to free list */
      entry->next= nextEntry;
      nextEntry= entry;
      entry= next;
   }
}

/* ---------------------------------------------------------------------- */

static double khold_reflect(Mesh *mesh, long j2, long j1, Ray *ray,
                            RayEdgeInfo *info);

/* Starting at the given entry point(s), track the ray through the mesh.
 * The path arrays will be lengthened if necessary, but never shortened.
 */
void RayTrack(Mesh *mesh, EntryPoint *entry, RayPath *path, double *sLimits)
{
/* Remains to: 1. handle hold lines??  */
    /* Logically, RayTrack should return the RayPath.
     * By building the routine like this, the RayPath arrays can
     * be reused without reallocating them for each ray to
     * be tracked.  The PATHINC parameter sets the chunk size for
     * increases in the array lengths. The maxcuts member of the
     * RayPath struct holds the current array lengths.
     *
     * See ExitZone for meaning of zone and side indices.
     * See ExitEdge and RayPathLength for algebraic formulas.
     */
   long zone, side;
   Ray ray;
   RayEdgeInfo info4[4];
   double ds, f;
   long i,j1,j2;
   long kmax= mesh->kmax;
   int *ireg= mesh->ireg;
   long alarm= mesh->klmax*8;
   int zsym = mesh->zsym;

   double s=0.0, dstest;
   double smin= sLimits[0];
   double smax= sLimits[1];
   int limits= smin<smax;
   int inlimits=0;

   long inczone[4]= { 0L /* kmax */, -1L, 0L /* -kmax */, +1L };
   long pt1index[4]= { 0L, -1L, /* -kmax */ -1L, 0L /* -kmax */ };
   long pt2index[4]= { -1L, /* -kmax */ -1L, 0L /* -kmax */, 0L };
   RayEdgeInfo *info[4] /* = { info4, info4+1, info4+2, info4+3 } */;
   inczone[0]+= kmax;
   inczone[2]-= kmax;
   pt1index[2]-= kmax;
   pt1index[3]-= kmax;
   pt2index[1]-= kmax;
   pt2index[2]-= kmax;
   info[0]= info4;
   info[1]= info4+1;
   info[2]= info4+2;
   info[3]= info4+3;

   if (zsym > 2) {
     /* special case for reflecting off of k-hold line,
      * to give weird pseudo-3D effect for wedges
      *   zsym = 2+khold   (khold>=1, usually 1)
      */
     zsym -= 2;  /* khold */
   } else {
     zsym = 0;
   }

    /* initially, assume that limits will not clip ray path */
   path->fi= path->ff= 0.0;

   for (i=0 ; entry ; entry= entry->next) {
       /* initialize tracking loop for this entry point */
      zone= entry->zone;
      side= entry->side;
      j1= zone+pt1index[side];
      j2= zone+pt2index[side];
      f= entry->f;
      ray= entry->ray;
      /* The entry->info is computed from the point of view of the
         zone the ray starts in.  All later info[3] are from the point
         of view of the previous zone, so dz and dr are reversed.
         This is maybe not quite truthful, since the ray was time
         reversed when entry was computed...  */
      info[3]->dz= -entry->info.dz;   /* sign */
      info[3]->dr= -entry->info.dr;   /* sign */
      info[3]->area= entry->info.area;
      info[3]->A= entry->info.A;
      info[3]->B= entry->info.B;
      info[3]->C= entry->info.C;
      info[3]->D= entry->info.D;
      info[3]->fx= -entry->info.fx;   /* sign */
      info[3]->validx= entry->info.validx;
      info[3]->fn= -entry->info.fn;   /* sign */
      info[3]->validn= entry->info.validn;

      if (limits) {
         s= ray.z*ray.cos+ray.x*ray.sin;
         if (s >= smax) break;
         inlimits= s>=smin;
      }

      for (;;) {
          /* store path values on entry to zone */
         if (i > alarm) goto lost;
         if (i >= path->maxcuts) ExtendRayPath(path, PATHINC);
         path->pt1[i]= j1;
         path->pt2[i]= j2;
         path->f[i]= f;
         if (ireg[zone]) {
            path->zone[i]= zone;
         } else if (zone && zsym && (side&1) && (j2%kmax)==(zsym-1)) {
           /* this is weird khold reflection case, actually reflect ray */
           zone += inczone[side];  /* back to zone on boundary */
           side ^= 02;
           path->zone[i]= zone;
           ds = khold_reflect(mesh, j2, j1, &ray, info[3]);
           if (limits) {
             smin += s;
             smax += s;
           }
         } else {
            path->zone[i]= 0;           /* mark exit to vacuum */
            path->ds[i]= 0.0;
            if (!limits || inlimits) i++;
            break;                      /* only exit from this loop */
         }

          /* exit this zone, enter new zone */
         side= ExitZone(mesh, zone,side, &ray,info, &ds,&f);
         if (side>3) goto lost;
         zone+= inczone[side];          /* update zone, flip to... */
         side= side^02;                 /* opposite side # in new zone */
         f= -f;                         /* ...sense of f changed as well */
         j1= zone+pt1index[side];
         j2= zone+pt2index[side];

          /* store path values on exit from zone (ds) */
         path->ds[i]= ds;

          /* increment i unless outside [smin,smax] */
         if (limits && ds>0.0) {
            dstest= smin-s;
            s= ray.z*ray.cos+ray.x*ray.sin;
            if (!inlimits) {
               inlimits= s>=smin;
               if (inlimits) {
                  path->fi= dstest/ds;
                  path->ds[i]-= dstest;
               }
            }
            if (inlimits) {
               dstest= s-smax;
               if (dstest >= 0.0) {
                  path->ff= dstest/ds;
                  path->ds[i]-= dstest;
                  zone= 0;      /* force exit at next opportunity */
               }
               i++;
            }
         } else i++;

          /* no need to reflect ray on arrival at symmetry plane (z=0)
             -- this is handled by exiting and re-entering the problem,
             otherwise there is no way to deal with the change in sense
             of the edge crossing right at the reflection point */
      }         /* end tracking loop */
      if (zone==0) break;       /* impossible unless beyond smax */

   }            /* end loop on entry points */

   path->ncuts= i;
   return;

lost:
   path->ncuts= 0;
   path->fi= path->ff= -1.0;    /* not great lost flag... */
}

static double
khold_reflect(Mesh *mesh, long j1, long j2, Ray *ray, RayEdgeInfo *info)
{
  double dz = mesh->z[j2] - mesh->z[j1];
  double dr = mesh->r[j2] - mesh->r[j1];
  double x = ray->x;
  double y = ray->y;
  double nx = dz*x;
  double ny = dz*y;
  double nz = -dr*sqrt(x*x + y*y);
  double rn2 = 1. / (nx*nx + ny*ny + nz*nz);
  double nr = (ray->sin*nx + ray->cos*nz) * rn2;
  double s = x*ray->sin + ray->z*ray->cos;
  double rx, ry, rr, z[2], r[2];
  int after = 0;

  /* reflect ray direction, rotate direction and point in (x,y)
   * to phi where y-component of direction is zero
   */
  nr += nr;
  rx = ray->sin - nr*nx;
  ry = - nr*ny;
  ray->cos -= nr*nz;
  ray->sin = rr = sqrt(rx*rx + ry*ry);
  rr = 1./rr;
  ray->x = (x*rx + y*ry) * rr;
  ray->y = (y*rx - x*ry) * rr;

  /* reverse order of points on side because next call is ExitZone */
  z[1] = mesh->z[j1];
  z[0] = mesh->z[j2];
  r[1] = mesh->r[j1];
  r[0] = mesh->r[j2];
  ExitEdge(ray, z, r, &after, info);

  /* return change in s to adjust slimits - this will always
   * be zero if khold line goes through (r,z) = (0,0)
   */
  return (ray->x*ray->sin + ray->z*ray->cos) - s;
}

/* Find index of a value in a monotonically increasing list of n
 * numbers.  Returns i such that list[i-1] < value <= list[i]
 * after a binary search. */
long SeekValue(double value, double *list, long n)
{
  long i0, i1, i;

  if (n<1 || value>list[n-1]) return n;
  if (value<=list[0]) return 0L;

  i0= 0;
  i= i1= n-1;
  while (i1-i0 > 1) {
    i= (i0+i1) >> 1;
    if (value <= list[i]) i1= i;
    else i0= i;
  }
  return i;
}

/* Track the ray through the mesh, assuming it represents a sphere.
 * The path arrays will be lengthened if necessary, but never shortened.
 * sLimits[0] <= s[1]<=s[ncuts-2] <= sLimits[1], if sLimits[0]<sLimits[1]
 * on entry.  (Note that s[0] and s[ncuts-1] may lie outside range.)
 * Assume that the zones are actually spherical, with spherical radii
 * given by mesh->z along k= kmax-1.  Further assume that l= 0 is the
 * center of the sphere, which occurs at mesh->z= mesh->r= 0.
 */
extern void RayTrackS(Mesh *mesh, Ray *ray, RayPath *path, double *sLimits)
{
  /* Logically, RayTrackS should return the RayPath.
   * By building the routine like this, the RayPath arrays can
   * be reused without reallocating them for each ray to
   * be tracked.  The PATHINC parameter sets the chunk size for
   * increases in the array lengths. The maxcuts member of the
   * RayPath struct holds the current array lengths.
   *
   * Corrects one error in TDG and DIRT (path->f[at s=0]).
   * Also handles void center problems (TDG and DIRT did not).
   *
   * See ExitZone for meaning of zone and side indices.
   * See ExitEdge and RayPathLength for algebraic formulas.
   */
  long i,j1,j2,ncuts;
  long kmax= mesh->kmax;
  long klmax= mesh->klmax;
  double *z= mesh->z;
  int *ireg= mesh->ireg;
  double radius2;
  double impact2= SQ(ray->y)+SQ(ray->cos*ray->x-ray->sin*ray->z);

  double smin= sLimits[0];
  double smax= sLimits[1];
  int limits= smin<smax;
  int voidCenter;

  /* Compute path length s of ray as it cuts concentric nested
   * spheres with radii z[klmax-1], ... z[0].
   * Only keep points bounded on at least one side by a real zone.
   * Temporarily use pt1 to store point index, ds to store s */
  ncuts= 0;
  for (i=klmax-1 ; i>=0 ; i-=kmax) {
    if (ireg[i] || ireg[i+kmax]) {
      radius2= SQ(z[i]);
      if (ncuts >= path->maxcuts) ExtendRayPath(path, PATHINC);
      path->pt1[ncuts]= i;
      if (radius2 <= impact2) { /* inside impact parameter */
        path->ds[ncuts++]= 0.0;
        break;
      } else                    /* still outside impact parameter */
        path->ds[ncuts++]= -sqrt(radius2-impact2);
    }
  }
  if (ncuts<2) {        /* ray misses problem entirely */
    path->ncuts= 0;
    return;
  }
  voidCenter= (path->ds[ncuts] != 0.0);

  /* Initially, assume that limits will not clip ray path */
  path->fi= path->ff= 0.0;

  /* Find which points lie within limits */
  if (limits) {
    /* Note:    s[SeekValue-1] < ss <= s[SeekValue] */
    if (smin <= path->ds[0]) j1= 0;
    else if (smin < 0.0) {
      j1= SeekValue(smin, path->ds, ncuts);
      if (path->ds[j1] != smin) j1--;
      if (j1<ncuts)
        path->fi= (smin-path->ds[j1])/(path->ds[j1+1]-path->ds[j1]);
    } else {
      j1= SeekValue(-smin, path->ds, ncuts);
      if (j1>0)
        path->fi= (smin+path->ds[j1])/(-path->ds[j1-1]+path->ds[j1]);
      j1= 2*ncuts-2+voidCenter - j1;
    }
    if (smax >= -path->ds[0]) j2= 2*ncuts-2+voidCenter;
    else if (smax <= 0.0) {
      j2= SeekValue(smax, path->ds, ncuts);
      if (j2>0)
        path->ff= (path->ds[j2]-smax)/(path->ds[j2]-path->ds[j2-1]);
    } else {
      j2= SeekValue(-smax, path->ds, ncuts);
      if (path->ds[j1] != -smax) j2--;
      if (j2<ncuts)
        path->ff= (-path->ds[j2]-smax)/(-path->ds[j2]+path->ds[j2+1]);
      j2= 2*ncuts-2+voidCenter - j2;
    }

  } else {
    j1= 0;
    j2= 2*ncuts-2+voidCenter;
  }

  /* Get enough space for repacking operation */
  while (j2+1 >= path->maxcuts) ExtendRayPath(path, PATHINC);

  /* Reflect inward path as outward path */
  for (i=ncuts ; i<=j2 ; i++) {
    path->pt1[i]= path->pt1[2*ncuts-2+voidCenter-i];
    path->ds[i]= -path->ds[2*ncuts-2+voidCenter-i];
  }

  /* Remove points prior to entry */
  if (j1 > 0) {
    for (i=j1 ; i<=j2 ; i++) {
      path->pt1[i-j1]= path->pt1[i];
      path->ds[i-j1]= path->ds[i];
    }
  }
  /* remember s=0 point (if any, otherwise i<0 or voidCenter) */
  i= ncuts-1-j1;
  ncuts= j2-j1+1;
  if (ncuts < 2) ncuts= 0;
  path->ncuts= ncuts;
  if (ncuts < 1) return;
  j2= i;        /* Remember index of s=0 (or exit to void center) */

  /* Final pass to flesh out other path structure members */

  for (i=0 ; i<ncuts-1 ; i++) path->ds[i]= path->ds[i+1]-path->ds[i];
  path->ds[ncuts-1]= 0.0;       /* This is actually redundant... */

  /* Inward leg of ray path */
  for (i=0 ; i<j2+voidCenter ; i++) {
    j1= path->pt1[i];
    path->pt2[i]= j1-1;         /* ok even if kmax=1 because... */
    path->f[i]= -0.5;           /* use value at pt1 (on k= kmax-1) */
    if (ireg[j1]) path->zone[i]= j1;
    else {      /* if voidCenter, this always done on last pass... */
      path->zone[i]= 0;
      path->ds[i]= 0.0;
    }
  }

  /* Handle point at s=0 (if any) */
  if (i==j2) {  /* never taken if voidCenter or smin>0 */
    j1= path->pt1[i];
    path->zone[i]= path->pt2[i]= j1+kmax;       /* yes, a k-line */
    path->f[i]= (sqrt(impact2)-z[j1])/(z[j1+kmax]-z[j1]);
    i++;
    /* Note: following incorrect formula was used in TDG and DIRT:
     *   path->f[i]= sqrt((impact2-SQ(z[j1]))/(SQ(z[j1+kmax])-SQ(z[j1]));
     * This only has an effect if the linear source function
     * interpolation is active. */
  }

  /* Outward leg of ray path */
  for ( ; i<ncuts ; i++) {
    j1= path->pt1[i];
    path->pt2[i]= j1;
    path->pt1[i]= j1-1;         /* ok even if kmax=1 because... */
    path->f[i]= +0.5;           /* use value at pt2 (on k= kmax-1) */
    if (ireg[j1+kmax]) path->zone[i]= j1+kmax;
    else {
      path->zone[i]= 0;
      path->ds[i]= 0.0;
    }
  }

  return;
}

/* ---------------------------------------------------------------------- */

/* Find the intersections of a ray with an edge (z,r); return true if
 * and only if the ray cuts the edge segment from left to right.
 */
int
ExitEdge(Ray *ray, double z[2], double r[2],    /* inputs */
         int *after, /* int *notafter, */       /* updates */
         RayEdgeInfo *info)                     /* output */
{
    /* Formulas:
     * Edge is ( z[0]+(f+.5)*(z[1]-z[0]), r[0]+(f+.5)*(r[1]-r[0]) )
     *      or ( zavg + f*dz, ravg + f*dr )
     * Assume rayr^2= rayx^2 + rayy^2   (where rayx is ray->x, etc.)
     * Ray intersects edge where:
     *   A*f^2 + 2*B*f + C = 0
     *
     *   A= (dr*cos)^2 - (dz*sin)^2
     *   B= -rayx*dz*sin*cos + ravg*dr*cos^2 - (zavg-rayz)*dz*sin^2
     *   C= (ravg+rayr)*(ravg-rayr)*cos^2 - ((zavg-rayz)*sin)^2 -
     *      2*rayx*(zavg-rayz)*sin*cos
     *
     *   area= ravg*dz - (zavg-rayz)*dr
     *   discrim= (-area*sin+rayx*dr*cos)^2 + rayy^2*A
     *
     * The solution corresponding to a left-to-right crossing of the edge:
     *   fexit= (cos*sqrt(discrim)-B) / A
     *        = -C / (cos*sqrt(discrim)+B)     (used if B*cos>0)
     *
     * The solution corresponding to a right-to-left crossing of the edge:
     *   fentry= -(cos*sqrt(discrim)+B) / A
     *         = C / (cos*sqrt(discrim)-B)     (used if B*cos<0)
     *
     * Notes:
     * 1. ExitEdge returns true only if discrim>0 (not equal 0).
     *    (In particular, no exit through edges with dr=dz=0 is possible,
     *     nor through edges with r[0]=r[1]=0.)
     * 2. ExitEdge returns true if and only if the exit root lies within
     *    given z,r segment.  That is, -0.5<=fexit<=0.5, (info->fx=fexit).
     * 3. If the *after flag is true (1) on input, an exception is made
     *    to note (2), accepting fexit<-0.5.  It is expected that this
     *    represents a roundoff error, so that fexit will be only very
     *    slightly less than -0.5 in such a case.
   Note: after as implemented here can incorrectly accept an exit at
         the interior corner of a chevron zone when it is actually very
         distant -- such an acceptance should be provisional and rejected
         if a subsequent exit is found
     * 4. If the *notafter flag is true (1) on input, an exception is made
     *    to note (2), rejecting fexit>=-0.5.  It is expected that this
     *    represents a roundoff error, so that fexit will be only very
     *    slightly greater than -0.5 in such a case.
   Note: notafter as it was implemented here can incorrectly reject
         the true exit in favor of a very distant exit on the previous
         edge -- let this rejection occur in ExitZone instead
     * 5. If there is no exit root, *after= *notafter= 0 on return.
     *    Otherwise, *after= fexit>0.5 and *notafter= !*after on return.
     *    On a subsequent edge whose first point [0] is the same as
     *    the second point [1] of the current edge, it is topologically
     *    impossible for an exit to have fexit<-0.5 (>-0.5) if, on the
     *    current edge, fexit>0.5 (<0.5).
     * 6. ExitEdge guaranteed to give same results to within sign if
     *    z[0]<-->z[1] and r[0]<-->r[1].  (I hope...)
     *
     */

   register double dz,zavg,dr,ravg,A,B,C,D,tmp;
   int before;

   dz= info->dz= z[1]-z[0];
   zavg= (z[1]+z[0])*0.5 - ray->z;
   dr= info->dr= r[1]-r[0];
   ravg= (r[1]+r[0])*0.5;       /* note difference from zavg... */

    /* Return if non-intersecting ray and edge cone (skew case) */
   info->area= ravg*dz - zavg*dr;
   A= info->A= (dr*ray->cos-dz*ray->sin)*(dr*ray->cos+dz*ray->sin);
   D= info->D= A*SQ(ray->y) + SQ(ray->x*dr*ray->cos-
                                        info->area*ray->sin);
   info->validx= info->validn= info->D>0.0;
   if (!info->validx) return *after= /* *notafter= */ 0;
   D= info->D= sqrt(D);

    /* The ray and the edge actually intersect, and the intersection
       is a clean cut not tangency (discrim!=0).  Also, at least one
       of dz or dr is non-zero. */
   B= info->B= ravg*dr*SQ(ray->cos) - zavg*dz*SQ(ray->sin) -
               ray->x*dz*ray->cos*ray->sin;
   C= info->C= (ravg+ray->r)*(ravg-ray->r)*SQ(ray->cos) -
               SQ(zavg*ray->sin) - 2.0*zavg*ray->x*ray->cos*ray->sin;

    /* Compute the location(s) of the intersection point(s)
     * return if exit crossing is at infinity (A=0) */
   if (B*ray->cos > 0.0) {
      tmp= -ray->cos*D - B ;
      info->fx= C/tmp;
      info->validx= 1;
      if ((info->validn= (A!=0.0))) info->fn= tmp/A;
   } else if ((tmp= ray->cos*D - B) != 0.0) {
      info->fn= C/tmp;
      info->validn= 1;
      if ((info->validx= (A!=0.0))) info->fx= tmp/A;
      else return *after= /* *notafter= */ 0;
    /* remaining cases have B= cos= 0 (since D>0) */
   } else if (A==0.0) { /* cos==0 && dz==0 --> no intersections */
      info->validx= info->validn= 0;
      return *after= /* *notafter= */ 0;
   } else {     /* obscure special case, only when zavg==0, cos==0 */
       /* slight worry about case that zavg is very near 0, but I think
        * that the above formulas are, in fact OK... */
      info->fx= info->fn= 0.0;
      info->validx= info->validn= 1;
   }

    /* Finally, check whether the exit point actually lies on the
     * edge...  The edge is divded into three parts:
     * before (f<-0.5), within (-0.5<=f<=0.5), and after (f>0.5)
     * The decision about "before" may be modified on the basis of
     * the after and notafter inputs, reflecting a topologically
     * impossible relationship between the present segment and
     * the previous (presumed due to roundoff errors) */
   before= info->fx < -0.5;
   if (before) { if (*after && info->fx>-0.505) before= 0; } /* missed exit */
   /* else     { if (*notafter) before= 1; } */ /* doubled exit */
   *after= info->fx > 0.5;
   /* *notafter= !*after; */

    /* Return true if exit point within the segment */
   return !before && !*after;
}

/* ---------------------------------------------------------------------- */

/* Compute the path length from the current point on the ray to the
 * exit (left-to-right) intersection computed by ExitEdge.
 */
double RayPathLength(Ray *ray, RayEdgeInfo *info)
{
    /* Formula:
     *
     * ds, the length of the ray within the zone, satisfies:
     *   A*ds^2 + 2*B*ds + C = 0
     *
     *   A= (dr*cos)^2 - (dz*sin)^2
     *   B= area*dr*cos -  rayx*dz^2*sin
     *   C= (area-rayr*dz)*(area+rayr*dz)
     *
     *   area= ravg*dz - (zavg-rayz)*dr
     *   discrim= (-area*sin+rayx*dr*cos)^2 + rayy^2*A
     *
     * The solution corresponding to a left-to-right crossing of the edge:
     *   ds= (dz*sqrt(discrim)-B) / A
     *     = -C / (dz*sqrt(discrim)+B)     (used if B*dz>0)
     *
     */

   register double B,C;

   B= info->area*info->dr*ray->cos - ray->x*SQ(info->dz)*ray->sin;

   if (B*info->dz<=0.0 && info->A!=0.0) /* Can A=0 from small errors?? */
      return (info->dz*info->D-B)/info->A;
   else {
      C= (info->area-ray->r*info->dz)*(info->area+ray->r*info->dz);
      return -C/(info->dz*info->D+B);
   }
}

/* ---------------------------------------------------------------------- */

/* Compute the path length from the exit point to the entry point (>0
 * if entry AFTER exit) for the intersections computed by ExitEdge.
 */
double RayPathDifference(RayEdgeInfo *info)
{
    /* Formula:
     *
     * The distance from the exit point to the entry point on the
     * same ray is (>0 if entry is further down the ray than exit):
     *   ds= -2*dz*sqrt(discrim)/A
     * Since both roots are assumed to exist, A!=0.
     *
     */
   return -2.0*info->dz*info->D/info->A;
}

/* ---------------------------------------------------------------------- */

/* Given a ray with current point entering a particular zone and side of
 * the mesh, find the corresponding exit.  The path length ds is >=0
 * unless the ray segment lies in the negative area part of a bowtied
 * zone, in which case ds<0.  Return the exit side index (0<=side<=3).
 * Update the current point on the ray to the exit point.
 */
int
ExitZone(Mesh *mesh, long zone,         /* input mesh and zone number */
         int side,                      /* input side number on entry */
         Ray *ray,                      /* updated to exit point */
         RayEdgeInfo *info[4],          /* info[3] is entry side on entry,
                                         * updated to exit side on exit */
         double *ds,                    /* output ray path length */
         double *f)                     /* output edge fraction */
{
    /*
     * On input, (ray->z,ray->r) lies on input 'side' of 'zone'.
     * An exit is a left-to-right crossing of an edge.
     *
     */

   int i,kmax;
   double z[4],r[4];
   int bow,corner,before,after /* ,notafter */, iplus,iminus,iexit[4];
   double fex,dsx[4], area;
   RayEdgeInfo *infotmp;

    /* load zone corners counter-clockwise starting from side */
   kmax= mesh->kmax;
   i= 3-side;
   z[i]= mesh->z[zone];          r[i]= mesh->r[zone];
   i= (i+1)&03;
   z[i]= mesh->z[zone-1];        r[i]= mesh->r[zone-1];
   i= (i+1)&03;
   z[i]= mesh->z[zone-1-kmax];   r[i]= mesh->r[zone-1-kmax];
   i= (i+1)&03;
   z[i]= mesh->z[zone-kmax];     r[i]= mesh->r[zone-kmax];

    /* Find exit points */
   bow= corner= iplus= iminus= 0;
   if (info[3]->validn) {       /* info[3] is based on PREVIOUS zone */
      fex= -info[3]->fn;        /* ...so edge had opposite sense */
      after= fex>0.5;
      /* notafter= !after; */
   } else after= /* notafter= */ 0;
   iexit[0]= iexit[1]= iexit[2]= iexit[3]= 0;
   for (i=0 ; i<3 ; i++) {      /* check three other sides... */
      if (ExitEdge(ray, z+i,r+i, &after/*, &notafter */, info[i])) {
         dsx[i]= RayPathLength(ray, info[i]);
         if (dsx[i]>=0.) iplus++;
         else iminus++;
         iexit[i]= 1;
      }
      area= ray->r*info[i]->dz-info[i]->area;
      if (area==0.0) corner= 1; /* not always corner in boomerang */
      else if (area<0.0) bow++;
   }
   if (info[3]->validn) {       /* then check entry side */
      before= fex<-0.5;
      if (before) { if (after && fex>-0.505) before= 0; }
      /* else     { if (notafter) before= 1; } */
      after= fex>0.5;
      if (!after && !before) {  /* exit on entry edge */
         dsx[3]= RayPathDifference(info[3]);
         if (dsx[3]>=0.) iplus++;
         else iminus++;
         iexit[3]= 1;
      }
   }

    /* Use dsplus exit unless passing through negative area of bowtie. */
    /* If entry is at corner of a potentially bowtied zone, or
     * no valid exit was detected, get help. */
   if ((corner&&bow) || !(bow!=2? iplus:iminus)) {
      i= FindLostRay(ray, info, z, r, dsx);
      if (i==4) return i;
   } else if (bow!=2) {
     /* normal positive crossing */
     double scale, dsy= BIG;
     iplus= iminus= 4;
     for (i=0 ; i<4 ; i++) {
       if (!iexit[i] || dsx[i]<0.0) continue;
       if (dsx[i]<dsy) {
         scale= (info[i]->dz>=0.?info[i]->dz:-info[i]->dz) +
           (info[i]->dr>=0.?info[i]->dr:-info[i]->dr);
         if (dsx[i]>1.e-9*scale && info[i]->fx>-0.5) {
           dsy= dsx[i];
           iplus= i;
         } else {
           iminus= i;
         }
       }
     }
     i= iplus<4? iplus : iminus;
   } else {
     /* crossing negative area part of bowtie */
     double scale, dsy= -BIG;
     iplus= iminus= 4;
     for (i=0 ; i<4 ; i++) {
       if (!iexit[i] || dsx[i]>=0.0) continue;
       if (dsx[i]>dsy) {
         scale= (info[i]->dz>=0.?info[i]->dz:-info[i]->dz) +
           (info[i]->dr>=0.?info[i]->dr:-info[i]->dr);
         if (dsx[i]<-1.e-9*scale && info[i]->fx>-0.5) {
           dsy= dsx[i];
           iplus= i;
         } else {
           iminus= i;
         }
       }
     }
     i= iplus<4? iplus : iminus;
   }

    /* Update the ray */
   infotmp= info[i];
   if (i!=3) {          /* swap proper info to info[3] */
      info[i]= info[3];
      info[3]= infotmp;
      fex= infotmp->fx;
   } else {             /* make sure this edge can't be used a 3rd time */
      info[3]->validn= 0;
      /* Also, swap sense of edge -- this is dangerous, since it leaves
         info[3] somewhat inconsistent.  The point is to get through the
         lines setting ray->(z,r) and PolishExit correctly.  */
      infotmp->dz= -infotmp->dz;
      infotmp->dr= -infotmp->dr;
   }
   ray->z= z[i] + (fex+0.5)*infotmp->dz;
   ray->r= r[i] + (fex+0.5)*infotmp->dr;
   ray->x+= dsx[i]*ray->sin;

    /* Polish the root */
   if (polishRoot) PolishExit(ray, infotmp, &dsx[i], &fex);

    /* Make sure exit point is actually on exit edge segment.
     * If not, put it at endpoint, and adjust the ray (x,y).
     * This can happen only if:
     *    1. fex<-0.5 on a root missed by roundoff, or
     *    2. fex moved out of [-0.5,0.5] by PolishExit
     */
   if (fex < -0.5) {
      fex= -0.5;
      AdjustRayXY(ray, z+i,r+i);
   } else if (fex > 0.5) {
      fex= 0.5;
      AdjustRayXY(ray, z+((i+1)&03),r+((i+1)&03));
   }

   *f= fex;
   *ds= dsx[i];
   return (side+i+1)&03;
}

/* ---------------------------------------------------------------------- */

/* Although algebraically exact formulas are used for the ExitEdge
 * calculation, roundoff errors may make the redundant information
 * in the Ray structure (y,z,x, and r) inconsistent.  PolishExit
 * attempts to restore precise self-consistency of the ray, without
 * moving the point off of the current edge (remembered in info).
 */
void PolishExit(Ray *ray, RayEdgeInfo *info, double *ds, double *f)
{
    /* Formulas:
     *   the ray should have rayr^2 = rayx^2 + rayy^2
     *   let
     *      error= rayr^2-rayx^2-rayy^2
     *   then the ray-edge intersection solution can be improved (error
     *   reduced) by using one of the following formulas:
     *
     *   if |rayr*dr*cos| < |rayx*dz*sin|, then use:
     *      delta(rayx)= (error/2*rayx)/(1 - rayr*dr*cos/(x*dz*sin))
     *      delta(rayz)= delta(rayx) * cos/sin
     *      delta(rayr)= delta(rayx) * dr*cos/(dz*sin)
     *      delta(f)= delta(rayx)/dz * cos/sin
     *      delta(ds)= delta(rayx) / sin
     *   if |rayx*dz*sin| < |rayr*dz*sin|, a better formula is:
     *      delta(rayr)= -(error/2*rayr)/(1 - x*dz*sin/(rayr*dr*cos))
     *      delta(rayz)= delta(rayr) * dz/dr
     *      delta(rayx)= delta(rayx) * dz*sin/(dr*cos)
     *      delta(f)= delta(rayr) / dr
     *      delta(ds)= delta(rayr) * dz/(dr*cos)
     *
     *   if rayr*dr*cos is very nearly equal rayx*dz*sin, or if error
     *   is relatively large, then this polishing operation is skipped
     *   (attempting to correct for a large error is probably a mistake,
     *   since if things are working as they should, error should be
     *   very small)
     */

   double error,rdrc,xdzs,ardrc,axdzs,diff,delx,delr;

   error= SQ(ray->r)-SQ(ray->x)-SQ(ray->y);
   if (error==0.0) return;

   rdrc= ray->r*info->dr*ray->cos;
   ardrc= fabs(rdrc);
   xdzs= ray->x*info->dz*ray->sin;
   axdzs= fabs(xdzs);
   diff= xdzs-rdrc;
   if ( fabs(diff) < polishTol1*((ardrc-axdzs)?ardrc:axdzs) ) return;

   if (ardrc <= axdzs) {
      if (fabs(error) > polishTol2*SQ(ray->x)) return;
      delx= 0.5*error*xdzs/(diff*ray->x);
      ray->x+= delx;
      ray->z+= delx * ray->cos/ray->sin;
      ray->r+= delx * (info->dr*ray->cos)/(info->dz*ray->sin);
      *f+= delx * ray->cos/(info->dz*ray->sin);
      *ds+= delx / ray->sin;
   } else {
      if (fabs(error) > polishTol2*SQ(ray->r)) return;
      delr= 0.5*error*rdrc/(diff*ray->r);
      ray->r+= delr;
      ray->z+= delr * info->dz/info->dr;
      ray->x+= delr * (info->dz*ray->sin)/(info->dr*ray->cos);
      *f+= delr / info->dr;
      *ds+= delr * info->dz/(info->dr*ray->cos);
   }
}

/* ---------------------------------------------------------------------- */

/* Through roundoff errors, it is possible for the exit point found
 * in ExitZone (or FindEntryPoints) to lie slightly beyond the
 * endpoints of the exit edge.  AdjustRayXY moves the ray back to
 * the nearby endpoint when this occurs.
 */
void AdjustRayXY(Ray *ray, double* z, double *r)
{
   double radius= sqrt(SQ(ray->x)+SQ(ray->y));

    /* set (z,r) as specified, then adjust (x,y) to nearest point on
     * circle (in z=const plane) to initial ray */
   ray->z= *z;
   ray->r= *r;
   if (radius != 0.0) {
      ray->x*= ray->r/radius;
      ray->y*= ray->r/radius;
   } else if (ray->x >= 0.0) ray->x= ray->r;
   else ray->x= -ray->r;
}

/* ---------------------------------------------------------------------- */

/* Last ditch effort by ExitZone to find the exit edge */
int FindLostRay(Ray *ray, RayEdgeInfo *info[4], double z[4], double r[4],
                double dsx[4])
{
    /* Returns edge index 0-3 of correct exit.
     *
     * This routine need not be efficient; it should be called only
     * on rare occasions:
     * 1. When a ray cuts directly through a corner of a
     *    bowtied or boomeranged zone.
     * 2. When roundoff causes dsx<0  when it was expected >0 or
     *    vice-versa.
     *
     * Here, sloppier limits are tolerated than in ExitZone, and
     * a detailed calculation of the zone shape is performed.
     * In principal, this search can still fail, although it is
     * difficult to see how...
     *
     * Note:
     *   info[3] is relative to the PREVIOUS zone, while 0-2 are
     *   for the current zone (whose corners are z,r).
     */

   double a301,a012,a123,a230;  /* 4 triangular areas */
   int bad;                     /* index of bad edge if bowtied */
   int backtrack;               /* set if this segment time reversed */
   double dsbest= 0.0;
   int i,iex;

    /* carefully check zone shape for bowtie */
   a301= (z[0]-z[3])*(r[1]-r[0]) - (z[1]-z[0])*(r[0]-r[3]);
   a012= (z[1]-z[0])*(r[2]-r[1]) - (z[2]-z[1])*(r[1]-r[0]);
   a123= (z[2]-z[1])*(r[3]-r[2]) - (z[3]-z[2])*(r[2]-r[1]);
   a230= (z[3]-z[2])*(r[0]-r[3]) - (z[0]-z[3])*(r[3]-r[2]);

   if (a301<0.0 && a012<0.0) bad= 0;
   else if (a301<0.0 && a230<0.0) bad= 3;
   else if (a123<0.0 && a012<0.0) bad= 1;
   else if (a123<0.0 && a230<0.0) bad= 2;
   else bad= 4;

    /* entry point is on edge 3, eliminate impossible exits */
   if (bad==3) { backtrack= 1;   info[1]->validx= 0; }
   else if (bad==1) { backtrack= 0;   info[1]->validx= 0; }
   else if (bad==0) {
      backtrack= ray->r*info[1]->dz-info[1]->area < 0.0;
      if (backtrack) info[2]->validx= 0;
      else info[0]->validx= 0;
   } else if (bad==2) {
      backtrack= ray->r*info[1]->dz-info[1]->area < 0.0;
      if (backtrack) info[0]->validx= 0;
      else info[2]->validx= 0;
   } else backtrack= 0;

    /* fill in any values of dsx (may not have been computed) */
   for (i=0 ; i<3 ; i++) {
      if (info[i]->validx) dsx[i]= RayPathLength(ray, info[i]);
   }
   if (info[3]->validn) dsx[3]= RayPathDifference(info[3]);

    /* scan for most likely exit point */
    /* maybe fix this to try to match entrance and exit points? */
   info[3]->validx= info[3]->validn;
   iex= 4;
   if (backtrack) {     /* passing through negative area region */
      for (i=0 ; i<4 ; i++) {
         if (info[i]->validx && dsx[i]<=0.0)
            if (iex==4 || dsx[i]>dsbest) { iex= i;   dsbest= dsx[i]; }
      }
      if (iex==4) {     /* check for plausible roundoff error in dsx */
         for (i=0 ; i<4 ; i++) {
            if (info[i]->validx)
               if (iex==4 || dsx[i]<dsbest) { iex= i;   dsbest= dsx[i]; }
         }
          /* reject if dsbest unreasonably large */
         if (iex!=4 && SQ(dsbest)>findRayTol*(a301+a012+a123+a230)) iex= 4;
      }
   } else {             /* passing through normal area */
      for (i=0 ; i<4 ; i++) {
         if (info[i]->validx && dsx[i]>=0.0)
            if (iex==4 || dsx[i]<dsbest) { iex= i;   dsbest= dsx[i]; }
      }
      if (iex==4) {     /* check for plausible roundoff error in dsx */
         for (i=0 ; i<4 ; i++) {
            if (info[i]->validx)
               if (iex==4 || dsx[i]>dsbest) { iex= i;   dsbest= dsx[i]; }
         }
          /* reject if dsbest unreasonably large */
         if (iex!=4 && SQ(dsbest)>findRayTol*(a301+a012+a123+a230)) iex= 4;
      }
   }

   return iex;
}

/* ---------------------------------------------------------------------- */

/* Rearrange the linked list of entry points into increasing order */
EntryPoint *EntrySort(EntryPoint *entry)
{
    /* Adaptation of qsort from Kernighan and Ritchie to linked list */
    /* According to Numerical Recipes, it may be a mistake to use
     * this algorithm on a very large list which could be already
     * sorted (i.e.- if earlier or later is NULL on every pass). */
   EntryPoint *next,*earlier,*later,*partition;
   double s0;

    /* empty list or single element already sorted */
   if (!entry || !(next= entry->next)) return entry;

    /* partition the input list into two lists: those earlier and
     * those later or equal than the first entry */
   partition= entry;
   s0= partition->s0;           /* partition value */
   earlier= later= 0;
   entry= next;
   do {
      next= entry->next;
      if (entry->s0 < s0) {     /* earlier than first entry */
         entry->next= earlier;
         earlier= entry;
      } else {                  /* later or equal to first entry */
         entry->next= later;
         later= entry;
      }
      entry= next;
   } while (entry);

    /* finally, sort the two partitions and concatenate */
   partition->next= EntrySort(later);
   earlier= EntrySort(earlier);

   if (earlier) {       /* must find end of earlier list */
      for (entry=earlier ; entry->next ; entry= entry->next);
      entry->next= partition;
      return earlier;
   } else return partition;
}

/* ---------------------------------------------------------------------- */

/* Make ray path arrays longer */
void ExtendRayPath(RayPath *path, long pathinc)
{
   long n,i;

   if (pathinc<=0L) return;

   i= path->maxcuts;
   n= i+pathinc;
   if (i) {
      path->zone= (long *)p_realloc((void *)path->zone,n*sizeof(long));
      path->ds= (double *)p_realloc((void *)path->ds,n*sizeof(double));
      path->pt1= (long *)p_realloc((void *)path->pt1,n*sizeof(long));
      path->pt2= (long *)p_realloc((void *)path->pt2,n*sizeof(long));
      path->f= (double *)p_realloc((void *)path->f,n*sizeof(double));
   } else {
      path->zone= (long *)p_malloc(n*sizeof(long));
      path->ds= (double *)p_malloc(n*sizeof(double));
      path->pt1= (long *)p_malloc(n*sizeof(long));
      path->pt2= (long *)p_malloc(n*sizeof(long));
      path->f= (double *)p_malloc(n*sizeof(double));
   }
   path->maxcuts= n;
}

void EraseRayPath(RayPath *path)
{
  long *zone= path->zone;
  double *ds= path->ds;
  long *pt1= path->pt1;
  long *pt2= path->pt2;
  double *f= path->f;
  path->ncuts= path->maxcuts= 0;
  path->zone= path->pt1= path->pt2= 0;
  path->ds= path->f= 0;
  p_free(zone);
  p_free(ds);
  p_free(pt1);
  p_free(pt2);
  p_free(f);
}

/* ---------------------------------------------------------------------- */

/* Adjust point on ray (x,y,z) and (z,r) to be point such that normal
 * plane to ray passes through origin.
 */
void NormalizeRay(Ray *ray)
{
   double dist;

   dist= ray->x*ray->cos-ray->z*ray->sin;
   ray->x= dist*ray->cos;
   ray->z= -dist*ray->sin;
}

/* ---------------------------------------------------------------------- */
