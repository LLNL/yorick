/*
 * $Id: bound.c,v 1.1 2005-09-18 22:04:53 dhmunro Exp $
 * Routines for finding the boundary of a cylindrical mesh.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "bound.h"

#include "pstdlib.h"

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

/* Given a mesh and a region number, compute the boundary of that region
 * and present it in counterclockwise order in (z,r)-plane.
 * The working array is int work[2][klmax+kmax].
 * Split into kedges, ledges working arrays.
 * Note that for region=0, this returns the problem boundary clockwise.
 * If cw is non-zero, the boundary is returned in clockwise order (or
 * counterclockwise for region 0).
 * A quick check is performed first to determine whether the input
 * boundary is actually the mesh boundary.
 * Return 1 if boundary list had to be changed, 0 if list was correct.
 */
int FindBoundaryPoints(Mesh *mesh, int region, int cw,
                       Boundary *boundary, int *work)
{
   long kmax= mesh->kmax;
   long klmax= mesh->klmax;
   int *ireg= mesh->ireg;       /* note: has klmax+kmax elements! */
   /* double *z= mesh->z; */
   double *r= mesh->r;
   /* int zsym= mesh->zsym; */
   int *kedges= work;
   int *ledges= work+klmax+kmax;

   long i,nk,nl,zone;
   int side;
   int scanl;
   long nedges;
   Edge *firstEdge,*nextEdge,*lastEdge;

    /* locate boundary edges in unordered manner */
    /* first, find k-edges (increment k and look for transitions)
     * kedges= +1 (-1) if region is to left (right) of edge */
   for (i=0 ; i<kmax ; i++)
     kedges[i]= kedges[klmax+i]= ledges[klmax+i]= 0;
   for (i=kmax ; i<klmax ; i++)
      kedges[i]= (ireg[i]==region) - (ireg[i+1]==region);
    /* next, find l-edges
     * ledges= +1 (-1) if region is to below (above) edge */
   ledges[0]= 0;
   for (i=1 ; i<klmax ; i++)
      ledges[i]= (ireg[i]==region) - (ireg[i+kmax]==region);

    /* count edges, eliminate r-axis
       zsym boundary must NOT be removed, since a ray actually reflects
       there, effectively exiting, then re-entering the problem */
   nk= 0;
   for (i=kmax ; i<klmax ; i++) {
      if (kedges[i]!=0) {
         if (r[i]+r[i-kmax] == 0.0) kedges[i]= 0;
         /* else if (zsym && z[i]+z[i-kmax]==0.0) kedges[i]= 0; */
         else nk++;
      }
   }
   nl= 0;
   for (i=1 ; i<klmax ; i++) {
      if (ledges[i]!=0) {
         if (r[i]+r[i-1] == 0.0) ledges[i]= 0;
         /* else if (zsym && z[i]+z[i-1]==0.0) ledges[i]= 0; */
         else nl++;
      }
   }

    /* do quick check to see if the boundaries are unchanged */
   if (boundary->nk==nk && boundary->nl==nl) {
      for (i=0 ; i<boundary->npoints-1 ; i++) {
         zone= boundary->zone[i];
         side= boundary->side[i];
         if (zone == 0) continue;
         if (side==0 && ledges[zone]==0) break;
         else if (side==1 && kedges[zone-1]==0) break;
         else if (side==2 && ledges[zone-kmax]==0) break;
         else if (side==3 && kedges[zone]==0) break;
      }
      if (i>=boundary->npoints-1) {             /* no change */
         MakeBoundaryZR(boundary, cw, mesh);
         return 0;
      }
   }
   EraseBoundary(boundary);
   boundary->zsym= mesh->zsym;
   boundary->nk= nk;
   boundary->nl= nl;

    /* scan to build linked list of boundary edges */
   i= 1;
   scanl= 1;
   while (nk+nl) {
       /* find one of the remaining boundary edges */
      if (nl) {
         for ( ; ledges[i]==0 ; i++);
         firstEdge= lastEdge= MakeEdge(kmax,i,ledges[i]);
         ledges[i]= 0;
         nl--;
      } else {
         if (scanl) { i= kmax;   scanl= 0; }
         for ( ; kedges[i]==0 ; i++);
         firstEdge= lastEdge= MakeEdge(1L,i,kedges[i]);
         kedges[i]= 0;
         nk--;
      }
      nedges= 1;

       /* walk backward to beginning of this boundary segment... */
      while (( nextEdge= WalkBoundary(cw,firstEdge,kmax,klmax,
                                     kedges,ledges,&nk,&nl) )) {
         nextEdge->next= firstEdge;
         firstEdge= nextEdge;
         nedges++;
      }
       /* then, walk forward to end of this boundary segment... */
      while (( nextEdge= WalkBoundary(!cw,lastEdge,kmax,klmax,
                                     kedges,ledges,&nk,&nl) )) {
         lastEdge->next= nextEdge;
         lastEdge= nextEdge;
         nedges++;
      }

       /* convert temporary Edge linked list into boundary lists */
      NewBoundaryEdges(boundary, nedges, firstEdge);

       /* release temporary Edge linked list */
      while (( firstEdge= ReleaseEdge(firstEdge) ));
   }

   MakeBoundaryZR(boundary, cw, mesh);
   return 1;
}

/* ---------------------------------------------------------------------- */

/* Trim specific edges out of a boundary list made by FindBoundaryPoints. */
void
TrimBoundary(Boundary *trimmed,   /* Resulting trimmed boundary */
             Mesh *mesh,          /* Problem mesh */
             Boundary *boundary,  /* Initial boundary within mesh */
             long *rmlist,        /* rmlist[nsegs][2][2]= [[k1,l1],[k2,l2]]
                                   * are nsegs logical line segments, where
                                   * (k,l) are point coordinates and either
                                   * k1=k2 or l1=l2 */
             long nsegs,
             int *work)           /* work[2][klmax] */
{
  long kmax= mesh->kmax;
  long klmax= mesh->klmax;
  int *kedges= work;
  int *ledges= work+klmax;

  long i,j,nk,nl,zone, k1,l1,k2,l2;
  int side, marking;

  /* Mark segments to be removed in work space. */
  for (i=0 ; i<klmax ; i++) kedges[i]= ledges[i]= 0;
  for (i=0 ; i<4*nsegs ; i+=4) {
    k1= rmlist[i  ];   k2= rmlist[i+2];
    l1= rmlist[i+1];   l2= rmlist[i+3];
    if (k1==k2) {                /* k1=k2, segment is a k-line */
      if (l1>l2) { j=l1; l1=l2; l2=j; }
      for (j=l1 ; j<=l2 ; j++) kedges[k1+j*kmax]= 1;
    } else if (l1==l2) {         /* l1=l2, segment is an l-line */
      if (k1>k2) { j=k1; k1=k2; k2=j; }
      for (j=k1 ; j<=k2 ; j++) ledges[j+l1*kmax]= 1;
    }
  }

  /* Initialize trimmed boundary */
  if (boundary->npoints > 0L) {
    trimmed->zone= (long *)p_malloc(sizeof(long)*boundary->npoints);
    trimmed->side= (int *)p_malloc(sizeof(int)*boundary->npoints);
  } else {
    trimmed->zone= (long *)0;
    trimmed->side= (int *)0;
  }
  trimmed->z= (double *)0;
  trimmed->r= (double *)0;

  nk= boundary->nk;
  nl= boundary->nl;
  marking= 0;
  j= 0L;
  for (i=0 ; i<boundary->npoints ; i++) {
    zone= boundary->zone[i];
    side= boundary->side[i];
    if (zone) {             /* if edge is marked, treat as zone==0 */
      if (side==0 && ledges[zone]!=0) { zone=0L; side= 0; nl--; }
      else if (side==1 && kedges[zone-1]!=0) { zone=0L; side= 0; nk--; }
      else if (side==2 && ledges[zone-kmax]!=0) { zone=0L; side= 0; nl--; }
      else if (side==3 && kedges[zone]!=0) { zone=0L; side= 0; nk--; }
    }
    if (zone || marking) {  /* accepts zone->0 transition... */
      marking= (zone!=0L);  /* ...but not two consecutive zone==0 */
      trimmed->zone[j]= zone;
      trimmed->side[j]= side;
      j++;
    }
  }
  trimmed->npoints= j;
  trimmed->nk= nk;
  trimmed->nl= nl;
}

/* ---------------------------------------------------------------------- */

#define EBLOCK_SIZE 256
static Edge *edgeBlock= 0;
static Edge *nextEdge= 0;             /* next free Edge in edgeBlock */

/* Make a single boundary edge linked list element.  The edge list is
 * a temporary construct used by FindBoundaryPoints.
 */
Edge *
MakeEdge(long stride,           /* ==1 for k-edge, ==kmax for l-edge */
         long edgeIndex,        /* index into kedges or ledges array */
         int sign)              /* 1 (-1) if region left or below
                                 * (right or above) edgeIndex edge */
    /* edge->zone and edge->side are for the zone NOT in the region */
{
   long ptindex[4]= { 0L, 1L, 0L /* stride */, 0L };
   Edge *edge= nextEdge;
   ptindex[2]= stride;

   if (!edge) {
     /* must allocate a new block of edges */
     long n= EBLOCK_SIZE;
     edge= (Edge *)p_malloc(sizeof(Edge)*n);

     /* the first element of each block is always a pointer to the
        previous block (not a valid edge) */
     edge->next= edgeBlock;
     edgeBlock= edge;

     /* the remaining edges are initialized into a free list */
     while (--n) {
       edge++;
       edge->next= nextEdge;   /* 0 on 1st pass */
       nextEdge= edge;
     }

   }
   /* update the free list */
   nextEdge= edge->next;

   edge->next= 0;
   if (stride==1) {     /* this is a k-edge */
      if (sign==1) edge->side= 1;
      else edge->side= 3;
   } else {             /* this is an l-edge */
      if (sign==1) edge->side= 2;
      else edge->side= 0;
   }
   edge->zone= edgeIndex+ptindex[edge->side];

   return edge;
}

/* Return an Edge to the free list, returning edge->next */
Edge *ReleaseEdge(Edge *edge)
{
   Edge *next;
   if (!edge) return 0;
   next= edge->next;
   /* add this edge to the free list */
   edge->next= nextEdge;
   nextEdge= edge;
   return next;         /* simplifies release of whole linked list */
}

/* ---------------------------------------------------------------------- */

/* Take one step along the boundary in the given direction starting at
 * the given edge.  Returns NULL at end of boundary.
 */
Edge *
WalkBoundary(int direction,     /* 0 clockwise, 1 counterclockwise */
             Edge *edge,        /* current edge */
             long kmax, long klmax,     /* mesh dimensions */
             int *kedges, int *ledges,  /* lists of edges not yet visited
                                         * 1 if region to left or below,
                                         * -1 if to right or above, 0 if
                                         * not a boundary edge --
                                         * UPDATED */
             long *nk, long *nl)        /* number of k or l edges --
                                         * UPDATED */
{
   int side= edge->side;
   long ptindex[4]= { 0L, 1L, 0L /* kmax */, 0L };
   long i,j,k;
   int sign;
   ptindex[2]= kmax;

    /* recover current edge index */
   i= edge->zone - ptindex[edge->side];

    /* The current edge attaches to 3 possible continuation edges,
     * which must be searched in a particular order, depending on
     * the current side and the direction of the search.  Since there
     * are 4 possible sides and 2 search directions, there are 8 cases.
     */
   if (side&01) {               /* starting on a k-edge, search l,k,l */
      if (side == 1) {
         if (direction) {
            j= i+kmax;   k= i+1;
         } else {
            i-= kmax;   j= i;   k= i+1;
         }
      } else {
         if (direction) {
            k= i-kmax;   i= k+1;   j= k;
         } else {
            k= i;   i+= 1;   j= k+kmax;
         }
      }
      if ((sign= ledges[i])) {
         ledges[i]= 0;   (*nl)--;
         return MakeEdge(kmax, i, sign);
      }
      if ((sign= kedges[j])) {
         kedges[j]= 0;   (*nk)--;
         return MakeEdge(1L, j, sign);
      }
      if ((sign= ledges[k])) {
         ledges[k]= 0;   (*nl)--;
         return MakeEdge(kmax, k, sign);
      }
   } else {                     /* starting on an l-edge, search k,l,k */
      if (side == 0) {
         if (direction) {
            k= i;   i+= kmax;   j= k+1;
         } else {
            k= i-1;   i= k+kmax;   j= k;
         }
      } else {
         if (direction) {
            i-= 1;   j= i;   k= i+kmax;
         } else {
            j= i+1;   k= i+kmax;
         }
      }
      if ((sign= kedges[i])) {
         kedges[i]= 0;   (*nk)--;
         return MakeEdge(1L, i, sign);
      }
      if ((sign= ledges[j])) {
         ledges[j]= 0;   (*nl)--;
         return MakeEdge(kmax, j, sign);
      }
      if ((sign= kedges[k])) {
         kedges[k]= 0;   (*nk)--;
         return MakeEdge(1L, k, sign);
      }
   }

    /* none of the 3 possible continuations was on the boundary */
   return 0;
}

/* ---------------------------------------------------------------------- */

/* Extend boundary->zone and boundary->side arrays, initializing to
 * the values in the given linked list of edge(s)
 */
void
NewBoundaryEdges(Boundary *boundary,      /* boundary lists to be expanded */
                 long nedges, Edge *edge) /* # and linked list of edges */
{
   long n,i;

   if (nedges<=0) return;

   i= boundary->npoints;
   n= i+nedges+1;
   if (i) {
      boundary->zone= (long*)p_realloc((void *)boundary->zone,n*sizeof(long));
      boundary->side= (int *)p_realloc((void *)boundary->side,n*sizeof(int));
   } else {
      boundary->zone= (long*)p_malloc(n*sizeof(long));
      boundary->side= (int *)p_malloc(n*sizeof(int));
   }
   boundary->npoints= n;

   for (n=0 ; n<nedges ; n++) {
      if (!edge) break;
      boundary->zone[i+n]= edge->zone;
      boundary->side[i+n]= edge->side;
      edge= edge->next;
   }
   boundary->zone[i+n]= 0;      /* mark end of this section of boundary */
   boundary->side[i+n]= 0;
}

/* ---------------------------------------------------------------------- */

/* Create boundary->z and boundary->r arrays from mesh and
 * boundary->zone, boundary->side arrays.  (cw must be as FindBoundaryPoints)
 */
void MakeBoundaryZR(Boundary *boundary, int cw, Mesh *mesh)
    /* add (z,r) lists to boundary, given (zone,side) lists and mesh */
{
   double *z= mesh->z;
   double *r= mesh->r;
   long kmax= mesh->kmax;
   long pt1index[4]= { -1L, -1L /* -kmax */, 0L /* -kmax */, 0L };
   long pt2index[4]= { 0L, -1L, -1L /* -kmax */, 0L /* -kmax */ };
   long *pt1, *pt2;
   long n= boundary->npoints;
   long *zone= boundary->zone;
   int *side= boundary->side;
   double *bz= boundary->z;
   double *br= boundary->r;
   long i,j;
   pt1index[1]-= kmax;
   pt1index[2]-= kmax;
   pt2index[2]-= kmax;
   pt2index[3]-= kmax;
   if (cw) {
     pt1= pt2index;
     pt2= pt1index;
   } else {
     pt1= pt1index;
     pt2= pt2index;
   }

   if (n<=1) {
     EraseBoundary(boundary);
     return;
   }
   if (!bz) boundary->z= bz= (double *)p_malloc(n*sizeof(double));
   if (!br) boundary->r= br= (double *)p_malloc(n*sizeof(double));

   for (i=0 ; i<n ; i++) {
      if (zone[i]) {
         j= zone[i] + pt1[side[i]];
      } else {  /* last point of a contiguous section of boundary */
         j= zone[i-1] + pt2[side[i-1]];
      }
      bz[i]= z[j];   br[i]= r[j];
   }
}

/* ---------------------------------------------------------------------- */

/* Free the memory used for the zone, side, z, and r arrays in boundary */
void EraseBoundary(Boundary *boundary)
{
  if (boundary->z) p_free((void *)boundary->z);
  if (boundary->r) p_free((void *)boundary->r);
  if (boundary->zone) p_free((void *)boundary->zone);
  if (boundary->side) p_free((void *)boundary->side);
  boundary->z= boundary->r= 0;
  boundary->zone= 0;
  boundary->side= 0;
  boundary->npoints= boundary->nk= boundary->nl= 0;
}

/* ---------------------------------------------------------------------- */

/* Create a Boundary with a given number of points, allocating zone,
 * side arrays, but NOT z, r (use MakeBoundaryZR). */
Boundary *MakeBoundary(int zsym, long nk, long nl, long npoints)
{
  Boundary *boundary= (Boundary *)p_malloc(sizeof(Boundary));
  boundary->zsym= zsym;
  boundary->nk= nk;
  boundary->nl= nl;
  boundary->npoints= npoints;
  if (npoints) {
    boundary->zone= (long *)p_malloc(sizeof(long)*npoints);
    boundary->side= (int *)p_malloc(sizeof(int)*npoints);
  } else {
    boundary->zone= (long *)0;
    boundary->side= (int *)0;
  }
  boundary->z= boundary->r= (double *)0;
  return boundary;
}

/* ---------------------------------------------------------------------- */
