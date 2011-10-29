/*
 * $Id: plclab.i,v 1.1 2005-09-18 22:06:04 dhmunro Exp $
 * Label contour levels with numerical values.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*= SECTION() put numeric labels on contours ==============================*/

func plc_label(z,y,x, levs, ndivs=, opaque=, lmgrid=, height=)
/* DOCUMENT plc_label, z,y,x, levs, ndivs=ndivs, opaque=opaque, lmgrid=lmgrid

     call after plc,z,y,x in order to put labels on an existing contour
     plot.  If you are going to set the limits (by means of the limits
     or range functions, or zooming or panning with the mouse), do so
     before calling plc_label, since it reads the current plot limits.

     finds points where numeric contours labels can be drawn 
     input
      z  : array to be contoured
      y: y coords of array to be contoured
      x: x coords of array to be contoured
      levs: contour levels to be labelled
      ndivs: divide domain into ndvis x ndivs subregions with label in each
      opaque: if defined, white out below label
      lmgrid: if defined, draw the subregion perimeters

      output --- none now

      authors: Phil Rasch and David Munro
      last revised: 20 July, 1996 (for 1.3 distribution)

   SEE ALSO: plc
*/
{
  if (is_void(ndivs)) ndivs= 6;
  if (is_void(opaque)) opaque= 0;
  if (is_void(lmgrid)) lmgrid= 0;
  if (is_void(height)) height= 12;

  /* Build list of edge indices cut by the contours:
   *   kzp= list of edges of the form (i,j) to (i+1,j) cut by a contour
   *   nzp= corresponding list of contour numbers
   *   kpz= list of edges of the form (i,j) to (i,j+1) cut by a contour
   *   npz= corresponding list of contour numbers
   * Mnemonic: zp= zone-point, pz= point-zone */
  dims = dimsof(z);                 /* shape of array */
  nx = dims(2);                     
  ny = dims(3);
  nc = numberof(levs);

  iar = indgen(nx)(,-:1:ny);  /* used to keep track of original indices */
  jar = indgen(ny)(-:1:nx,); 

  /* Loop over contour levels - note that this is almost guaranteed
   * to be a much shorter loop than the number of zones in the mesh,
   * or even the number of rows or columns separately.  */
  pkzp= pkpz= array(pointer, nc);
  pnzp= pnpz= 0;
  for (n=1 ; n<=nc ; ++n) {
    /* find all edges cut by contour level n */
    above= z > levs(n);       /* 1 where z > levs, 0 elsewhere */
    list= where(above(dif,)); /* where levs between two values of z */
    if (numberof(list)) {
      pkzp(n)= &list;
      pnzp+= numberof(list);
    }
    list= where(above(,dif));
    if (numberof(list)) {
      pkpz(n)= &list;
      pnpz+= numberof(list);
    }
  }
  if (pnzp) {
    kzp= nzp= array(0, pnzp);
    for (n=1,i=0 ; n<=nc && i<pnzp ; ++n,i+=nl) {
      eq_nocopy, list, *pkzp(n);
      nl= numberof(list);
      if (nl) {
        kzp(i+1:i+nl)= list;
        nzp(i+1:i+nl)= n;
      }
    }
  }
  if (pnpz) {
    kpz= npz= array(0, pnpz);
    for (n=1,i=0 ; n<=nc && i<pnpz ; ++n,i+=nl) {
      eq_nocopy, list, *pkpz(n);
      nl= numberof(list);
      if (nl) {
        kpz(i+1:i+nl)= list;
        npz(i+1:i+nl)= n;
      }
    }
  }
  pkzp= pkpz= [];

  /* Compute the exact coordinates where the contour cuts each edge.
   * Notice that the kzp and kpz indices are into arrays which are
   * one shorter than the original z array.  In the case of kpz, this
   * means that the corresponding points are kpz and kpz+nx, but
   * we need to adjust kzp in order to account for the shorter rows
   * in above(dif,) before it can be used as an index into x, y, or z.
   * After adjustment, those edges run from kzp to kzp+1.  */
  if (numberof(kzp)) {
    kzp+= (kzp-1)/(nx-1);
    izp = iar(kzp);
    jzp = jar(kzp);
    f= (levs(nzp)-z(kzp))/(z(kzp+1)-z(kzp));
    /* (the previous denominator is non-zero by construction) */
    xzp= x(kzp) + (x(kzp+1)-x(kzp))*f;
    yzp= y(kzp) + (y(kzp+1)-y(kzp))*f;
  }

  if (numberof(kpz)) {
    ipz = iar(kpz);
    jpz = jar(kpz);
    f= (levs(npz)-z(kpz))/(z(kpz+nx)-z(kpz));
    /* (the previous denominator is non-zero by construction) */
    xpz= x(kpz) + (x(kpz+nx)-x(kpz))*f;
    ypz= y(kpz) + (y(kpz+nx)-y(kpz))*f;
  }

  xc= grow(xzp, xpz);
  yc= grow(yzp, ypz);
  ic = grow(izp, ipz);
  jc = grow(jzp, jpz);
  kc = grow(kzp, kpz);
  nc= grow(nzp, npz);
  n= numberof(nc);
  if (!n) return;

  /* Redraw the current plot (including the plc) to be sure that the
   * plot limits are up to date.  */
  /* if (!testing_plc_label) redraw; */
  xy= limits();
  xmin= min(xy(1),xy(2));
  xmax= max(xy(1),xy(2));
  ymin= min(xy(3),xy(4));
  ymax= max(xy(3),xy(4));
  /* print, "xmin,max, ymin, max", xmin, xmax, ymin, ymax */

  /* Divide the plotting viewport into an ndivs-by-ndivs
   * grid, then allow at most one label in each grid square
   * in order to reject labels that are too close together.  */
  dx= (xmax-xmin)/ndivs;
  dy= (ymax-ymin)/ndivs;

  /* reject labels that are too close to or over the border */
  xth = (xmax-xmin)/25.;
  yth = (ymax-ymin)/50.;
  list= where((xc>xmin+xth) & (xc<xmax-xth) &
              (yc>ymin+yth) & (yc<ymax-yth));
  n= numberof(list);
  if (!n) return;
  xc= xc(list);
  ic= ic(list);
  jc= jc(list);
  yc= yc(list);
  nc= nc(list);

  if (lmgrid == 1) {
    /* draw the lines delimiting the low density mesh */
    xx= span(xmin,xmax,ndivs+1)(,-:1:ndivs+1);
    yy= span(ymin,ymax,ndivs+1)(-:1:ndivs+1,);
    plm, yy, xx;
  }

  /* choose the best of the available contour indices
   * store the low density mesh index containing 
   * the best label within that subregion
   * and the index of that label */
  ip1= min(iar+1,nx);
  jp1= min(jar+1,ny);
  listk= iar + (jar-1)*nx;
  listdx= ip1 + (jar-1)*nx;
  listdy= iar + (jp1-1)*nx;
  dzdx1= abs(z(listdx)-z(listk));
  dzdy1= abs(z(listdy)-z(listk));
  lgrad= dzdx1 + dzdy1;
  anc= abs(levs(dif));
  grow, anc, anc(0);

  ploc= lgrad(1:numberof(nc))/anc(nc);    /* local penalty for this label is
                             normalized measure of the local gradient */

  /* index into low density mesh for each prospective label */
  jld= min(long((xc-xmin)/dx) + 1, ndivs) +
    ndivs*min(long((yc-ymin)/dy), ndivs-1);
  /* index of prospective label with minimum penalty in ldm */
  kld= array(0, ndivs, ndivs);
  /* value of penalty for that label in ldm */
  pld= array(1.e20, ndivs, ndivs);

  for ( k=1 ; k<=n ; ++k ) {
    i= jld(k);
    if (ploc(k) < pld(i)) {
      kld(i)= k;
      pld(i)= ploc(k);
    }
  }

  dmin= 0.5*abs(dx,dy);
  kld= kld(where(kld));
  xlab= ylab= [-999.];
  for (m=1 ; m<=numberof(kld) ; ++m) { /* points for labelling */
    k= kld(m)
    dist= abs(xc(k)-xlab, yc(k)-ylab);
    /* this comparison requires O(n^2) work, as does grow operation */
    if (min(dist) > dmin) {
      plt, pr1(levs(nc(k))), xc(k),yc(k),
           height=height, tosys=1, justify="CH",opaque=opaque;
      grow, xlab, xc(k);
      grow, ylab, yc(k); 
    }
  }
}
