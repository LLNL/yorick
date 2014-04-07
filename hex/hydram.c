/*
 * $Id: hydram.c,v 1.1 2005-09-18 22:05:49 dhmunro Exp $
 * workers to convert hydra bnd specs into HX_boundary arrays
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "hex.h"
#include "hydram.h"

/* ------------------------------------------------------------------------ */

static int get_2dims(int pn, long blo[], long sdims[]);
static long set_scratch(long n, long *ndx, long bnd[], long blo[],
                        long *scratch, long *sdims);
static int orient_compute(long bnds[], long bndr[], long *p2dp, long *p2dq,
                          long pq0, long *p2p, long *p2q);

long hydra_blks(long nblks, long (*blo)[4])
{
  long offset= 0;
  long lscratch= 0;
  long b, ni, nj, nk, nn;
  for (b=0 ; b<nblks ; b++) {
    blo[b][0]= offset;
    ni= blo[b][1];
    nj= blo[b][2];
    nk= blo[b][3];
    offset+= (blo[b][3]*= (blo[b][2]*= ni));
    if (ni<nj) nn= nk<ni? ni*nj :  nj*nk;
    else       nn= nk<nj? ni*nj : nk*ni;
    if (nn>lscratch) lscratch= nn;
  }
  return lscratch;
}

long hydra_mrk(int ibnd, long (*tbound)[3], long *blo, long *bnd,
               long n, long *ndx)
{
  int pn= (int)bnd[0];
  long jp= bnd[1];
  long kp= bnd[2];
  long jq= blo[1];  /* always jp-3?? */
  long kq= blo[2];  /* always kp-3*jp?? */
  long i, px0, px1, nijk[3];
  long ijk= -1;

  int p0, p1;
  if (pn<0) pn= -pn;
  pn--;
  p0= !pn;
  p1= pn^p0^3;

  nijk[0]= jq;
  if (pn!=1) nijk[1]= kq/jq;
  if (pn!=2) nijk[2]= blo[3]/kq;
  px0= nijk[p0];
  px1= nijk[p1];

  for (i=0 ; i<n ; i++) {
    /* use hydra in-memory strides from bnd[] to extract separate
     * i, j, and k indices from ndx[], then adjust these by subtracting
     * the 2 ghost planes hydra uses
     * -- could make -2 a parameter to this function... */
    nijk[0]=  ndx[i]%jp     - 2;
    nijk[1]= (ndx[i]%kp)/jp - 2;
    nijk[2]=  ndx[i]/kp     - 2;
    if (nijk[pn]<0) return -13;
    if (ijk!=nijk[pn]) {
      /* all the ndx[] must lie in a single plane of constant pn */
      if (i) return -1;
      ijk= nijk[pn];
    }
    if (nijk[p0]<0 || nijk[p1]<0) {
      /* hydra may undershoot BC by one node */
      if (nijk[p0]<-1 || nijk[p1]<-1) return -11;
      continue;
    }
    if (nijk[p0]>=px0 || nijk[p1]>=px1) {
      /* hydra may overshoot BC by one node */
      if (nijk[p0]>px0 || nijk[p1]>px1) return -12;
      continue;
    }
    tbound[nijk[0]+nijk[1]*jq+nijk[2]*kq][pn]= ibnd;
  }

  return ijk;  /* index of plane containing all of ndx[] */
}

long hydra_adj(long (*bound)[3], long (*tbound)[3],
               long *blo, long n, long *tcheck)
{
  long start= -1;
  long strides[4];
  long i, j, k, is, js, ix, jx, face;
  int pn, p0, p1;

  strides[0]= 1;
  strides[1]= blo[1];
  strides[2]= blo[2];
  strides[3]= blo[3];

  for (bound+=blo[0] ; n-- > 0 ; tcheck+=2) {
    pn= tcheck[0];
    if (!pn) continue;
    face= (pn>0);
    if (!face) pn= -pn;
    pn--;
    p0= !pn;
    p1= pn^p0^3;
    if (start<0) {
      face|= (pn<<1);
      if (!(face&1)) face+= 6*strides[pn];
    }
    k= tcheck[1]*strides[pn];
    is= strides[p0];
    ix= strides[p0+1];
    js= strides[p1];
    jx= k+strides[p1+1];
    for (j=k+js ; j<jx ; j+=js) {
      for (i=0 ; i<ix-is ;) {
        for ( ; i<ix-is ; i+=is)
          if (tbound[i+j-js][pn] && tbound[i+j][pn]) break;
        for (i+=is ; i<ix ; i+=is) {
          if (!tbound[i+j-js][pn] || !tbound[i+j][pn]) break;
          bound[i+j][pn]= tbound[i+j][pn];
          if (start<0 && tbound[i+j][pn]==-1)
            start= 6*(i+j+blo[0]) + face;
        }
      }
    }
  }

  return start;
}

long hydra_bnd(long ibnd, long (*bound)[3], long *scratch,
               long *blos, long *blor, long *bnds, long *bndr,
               long n, long *ndxs,
               long *ndxr, HX_blkbnd *mbnds, long rblock)
{
  long i, j, ii, jj, sdims[2];

  /* determine the face strides and orientations */
  int pns= get_2dims((int)bnds[0], blos, sdims);
  long js= sdims[0];   /* scratch 2nd dimension stride */
  long ks= sdims[1];   /* scratch size */
  long ib= pns? 1 : blos[1];
  long jb= (pns==2)? blos[1] : blos[2];
  long start;

  /* mark scratch space with nodes in ndxs */
  long splane= set_scratch(n, ndxs, bnds, blos, scratch, sdims);
  if (splane<0) return splane;

  /* scan to find first point */
  for (j=0 ; j<ks-js-1 ; j++)
    if (scratch[j] && scratch[j+1] && scratch[j+js] && scratch[j+js+1]) break;
  /* first quad face is j, j+1, j+js, j+js+1 */
  if (j>=ks-js) return -2;

  /* offset bound to the proper plane of the proper block
   * note that in the pns direction, the index is a nodal index, while
   * in the other two directions, the mark will be at a cell index */
  start= splane*(pns?blos[pns]:1) + blos[0] + ib + jb;
  bound+= start;

  i= j%js;
  j-= i;
  ii= i*ib;
  jj= (j/js)*jb;
  if (!ndxr) {

    /* THIS DOES NOT WORK AS INTENDED
     * the hydra_mrk and hydra_adj routines must be
     * used instead to mark BCs */
#ifdef NEVER_DEFINE_THIS
    for (; j<ks-js ; i=ii=0,j+=js,jj+=jb) {            /* loop on rows */
      do {                             /* loop on segments in this row */
        for (; i<js-1 ; i++,ii+=ib)   /* scan to first face in segment */
          if (scratch[j+i] && scratch[j+i+js]) break;
        for (i++ ; i<js ; i++,ii+=ib) {      /* scan to end of segment */
          if (!scratch[j+i] || !scratch[j+i+js]) break;
          bound[jj+ii][pns]= ibnd;                    /* mark the face */
        }
      } while (i<js);
    }
    /* need to return face index (6*cell+face) of first face in this bc */
    if (bnds[0]>0) {
      pns= (pns<<1) | 1;          /* 0-5 face index, no cell adjustment */
    } else {
      start+= (pns?blos[pns]:1);  /* cell index */
      pns<<= 1;                   /* 0-5 face index */
    }
    return 6*start + pns;
#endif
    return -4;

  } else {
    /* bnds, bndr, and the first quad determine
     * the relative orientation of the s and r blocks
     * - note that scratch markers are indices+1 into ndxs, ndxr */
    long pq0= ndxr[scratch[i+j]-1];
    long dp= ndxr[scratch[i+j+1]-1] - pq0;
    long dq= ndxr[scratch[i+j+js]-1] - pq0;
    long p= blor[1];
    long q= blor[2];
    long jsmi = js - i;
    int orient= orient_compute(bnds, bndr, &dp, &dq, pq0, &p, &q);
    if (orient<0) return -3;
    q+= blor[0]+p;  /* q is global index of first cell in r block */
    j += i;         /* like q, combine initial i into j ... */
    jj += ii;       /* ... and ii into jj */

    for (i=ii=p=0 ; j<ks-jsmi ; i=ii=p=0,j+=js,jj+=jb,q+=dq) {
      do {
        for (; i<jsmi-1 ; i++,ii+=ib,p+=dp)
          if (scratch[j+i] && scratch[j+i+js]) break;
        for (i++ ; i<jsmi ; i++,ii+=ib,p+=dp) {
          if (!scratch[j+i] || !scratch[j+i+js]) break;
          bound[jj+ii][pns]= ibnd+1;
          mbnds[ibnd].block= rblock;
          mbnds[ibnd].cell= p+q;
          mbnds[ibnd].orient= orient;
          ibnd++;
        }
      } while (i<jsmi);
    }
    return ibnd;
  }
}

static int get_2dims(int pn, long blo[], long sdims[])
{
  long jq= blo[1];
  long kq= blo[2];

  if (pn<0) pn= -pn;
  pn--;

  if (pn==0) {
    sdims[0]= kq/jq;
    sdims[1]= blo[3]/jq;
  } else if (pn==1) {
    sdims[0]= jq;
    sdims[1]= jq*(blo[3]/kq);
  } else {
    sdims[0]= jq;
    sdims[1]= kq;
  }

  return pn;
}

static long set_scratch(long n, long *ndx, long bnd[], long blo[],
                        long *scratch, long *sdims)
{
  int pn= (int)bnd[0];
  long jp= bnd[1];
  long kp= bnd[2];
  long jq= blo[1];  /* always jp-3?? */
  long kq= blo[2];  /* always kp-3*jp?? */
  long j0= sdims[0];
  long ns= sdims[1];
  long i, px0, px1, nijk[3];
  long ijk= -1;

  int p0, p1;
  if (pn<0) pn= -pn;
  pn--;
  p0= !pn;
  p1= pn^p0^3;

  nijk[0]= jq;
  if (pn!=1) nijk[1]= kq/jq;
  if (pn!=2) nijk[2]= blo[3]/kq;
  px0= nijk[p0];
  px1= nijk[p1];

  for (i=0 ; i<ns ; i++) scratch[i]= 0;

  for (i=0 ; i<n ; i++) {
    /* use hydra in-memory strides from bnd[] to extract separate
     * i, j, and k indices from ndx[], then adjust these by subtracting
     * the 2 ghost planes hydra uses
     * -- could make -2 a parameter to this function... */
    nijk[0]=  ndx[i]%jp     - 2;
    nijk[1]= (ndx[i]%kp)/jp - 2;
    nijk[2]=  ndx[i]/kp     - 2;
    if (nijk[pn]<0) return -13;
    if (ijk!=nijk[pn]) {
      /* all the ndx[] must lie in a single plane of constant pn */
      if (i) return -1;
      ijk= nijk[pn];
    }
    if (nijk[p0]<0 || nijk[p1]<0) {
      /* hydra may undershoot BC by one node */
      if (nijk[p0]<-1 || nijk[p1]<-1) return -11;
      continue;
    }
    if (nijk[p0]>=px0 || nijk[p1]>=px1) {
      /* hydra may overshoot BC by one node */
      if (nijk[p0]>px0 || nijk[p1]>px1) return -12;
      continue;
    }
    scratch[nijk[p0]+nijk[p1]*j0]= i+1;
  }

  return ijk;  /* index of plane containing all of ndx[] */
}

static int orient_compute(long bnds[], long bndr[], long *p2dp, long *p2dq,
                          long pq0, long *p2p, long *p2q)
{
  long i, j, k;
  int f0, f2;
  long dp= *p2dp;
  long dq= *p2dq;

  int sns= bnds[0]<0;
  int pns= (int)(sns? -bnds[0] : bnds[0]) - 1;
  /* int pps= !pns; */
  /* int pqs= pns^pps^3; */

  int snr= bndr[0]<0;
  int pnr= (int)(snr? -bndr[0] : bndr[0]) - 1;
  int ppr, pqr;

  /* direction pnr in r block corresponds to direction pns in s block,
   *   or opposite to pns if sns==snr
   * dp is r block stride corresponding to + step in direction pps
   * dq is r block stride corresponding to + step in direction pqs
   */

  int sp= dp<0;
  int sq= dq<0;
  int sr= sns==snr;
  if (sp) dp= -dp;
  if (sq) dq= -dq;

  /* check consistency of r block orientation */
  if (pnr) {
    if (dp==1) {
      ppr= 0;
      pqr= 3^pnr;
      if (dq!=bndr[pqr]) return -1;
    } else {
      ppr= 3^pnr;
      pqr= 0;
      if (dq!=1 || dp!=bndr[ppr]) return -1;
    }
  } else {
    if (dp==bndr[1]) {
      ppr= 1;
      pqr= 2;
      if (dq!=bndr[pqr]) return -1;
    } else {
      ppr= 2;
      pqr= 1;
      if (dp!=bndr[ppr] || dq!=bndr[pqr]) return -1;
    }
  }
  if ((pns==1) ^ (((pnr==2)?0:pnr+1)!=ppr) ^ sp^sq^sr) return -1;

  /* find which faces in r correspond to faces 0 and 2 in s */
  if (pns) {
    /* face 0s is +- dp direction (pps==0 if pns!=0) */
    f0= (ppr<<1) | sp;
    if (pns==1) {
      /* face 2s is +- bndr[0] direction */
      f2= (pnr<<1) | sr;
    } else {
      /* face 2s is +- dq direction */
      f2= (pqr<<1) | sq;
    }

  } else {
    /* face 0s is +- bndr[0] direction, face 2s is +-dp direction */
    f0= (pnr<<1) | sr;
    f2= (ppr<<1) | sp;
  }

  /* compute separate (p,q) for first cell
   * corners are pq0, pq0+dp, pq0+dq, pq0+dp+dq
   * if bndr[0]>0, the boundary is a min face,
   *   so the nodal index pq0 must be shifted to become a cell index
   * similar shifts must be applied in the other two directions
   * after all this, pq0 is the cell index in a mesh with ghosts,
   *   so the next step is to split it into its ijk and remove ghosts
   * note that *p2p=blor[1]=stride for j, *p2q=blor[2]=stride for k
   *   on input
   */
  if (snr) pq0+= pnr? bndr[pnr] : 1;
  if (!sp) pq0+= dp;
  if (!sq) pq0+= dq;
  i=  pq0%bndr[1]          - 2;
  j= (pq0%bndr[2])/bndr[1] - 2;
  k=  pq0/bndr[2]          - 2;
  if (i<0 || j<0 || k<0) return -1;
  j*= (*p2p);
  k*= (*p2q);
  if (ppr==0) {
    *p2dp= sp? -1 : 1;
    *p2dq= (pqr==1)? (*p2p) : (*p2q);
    if (sq) *p2dq= -(*p2dq);
    *p2p= i;
    *p2q= j+k;
  } else if (ppr==1) {
    *p2dp= sp? -(*p2p) : (*p2p);
    *p2dq= (pqr==0)? 1 : (*p2q);
    if (sq) *p2dq= -(*p2dq);
    *p2p= j;
    *p2q= k+i;
  } else {
    *p2dp= sp? -(*p2q) : (*p2q);
    *p2dq= (pqr==0)? 1 : (*p2p);
    if (sq) *p2dq= -(*p2dq);
    *p2p= k;
    *p2q= i+j;
  }

  /* return orientation */
  f2 ^= ((f0&4)? f0-4 : f0+2);
  if (f2&6) f2 = 2 | (f2&1);
  return (f0<<2) | f2;
}

/* ------------------------------------------------------------------------ */

#ifdef DEGHOST_ADJUSTS_NDX
static long deghost(long n, long *ndx, long bnd[], long blo[], long *sdims)
{
  int pn= (int)bnd[0];
  long jp= bnd[1];
  long kp= bnd[2];
  long jq= blo[1];  /* always jp-3?? */
  long kq= blo[2];  /* always kp-3*jp?? */
  long i, nijk[3];
  long ijk= -1;
  int p0, p1;
  if (pn<0) pn= -pn;
  pn--;
  p0= !pn;
  p1= pn^p0^3;

  for (i=0 ; i<n ; i++) {
    /* use hydra in-memory strides from bnd[] to extract separate
     * i, j, and k indices from ndx[], then adjust these by subtracting
     * the 2 ghost planes hydra uses
     * -- could make -2 a parameter to this function... */
    nijk[0]=  ndx[i]%jp     - 2;
    nijk[1]= (ndx[i]%kp)/jp - 2;
    nijk[2]=  ndx[i]/kp     - 2;
    if (nijk[0]<0 || nijk[1]<0 || nijk[2]<0) return -1;
    /* all the ndx[] must lie in a single plane of constant abs(pn) */
    if (ijk!=nijk[pn]) {
      if (i) return -1;
      ijk= nijk[pn];
    }
    /* reconstruct ndx[] adjusted to index into arrays without
     * any ghost planes, as in hex */
    ndx[i]= nijk[0] + jq*nijk[1] + kq*nijk[2];
  }

  return ijk;  /* index of plane containing all of ndx[] */
}
#endif
