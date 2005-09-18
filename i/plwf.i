/*
 * $Id: plwf.i,v 1.1 2005-09-18 22:06:04 dhmunro Exp $
 * Simple "painter's algorithm"-class routine for making 3-D wire frames
 * and related models.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

require, "pl3d.i";

func plwf(z,y,x, fill=,shade=,edges=,ecolor=,ewidth=,cull=,scale=,cmax=)
/* DOCUMENT plwf, z
         or plwf, z, y,x

     plots a 3-D wire frame of the given Z array, which must have the
     same dimensions as the mesh (X, Y).  If X and Y are not given, they
     default to the first and second indices of Z, respectively.
     The drawing order of the zones is determined by a simple "painter's
     algorithm", which works fairly well if the mesh is reasonably near
     rectilinear, but can fail even then if the viewpoint is chosen to
     produce extreme fisheye perspective effects.  Look at the resulting
     plot carefully to be sure the algorithm has correctly rendered the
     model in each case.

   KEYWORDS: fill   -- optional colors to use (default is to make zones
                       have background color), same dimension options as
                       for z argument to plf function
             shade  -- set non-zero to compute shading from current
                       3D lighting sources
             edges  -- default is 1 (draw edges), but if you provide fill
                       colors, you may set to 0 to supress the edges
             ecolor, ewidth  -- color and width of edges
             cull   -- default is 1 (cull back surfaces), but if you want
                       to see the "underside" of the model, set to 0
             scale  -- by default, Z is scaled to "reasonable" maximum
                       and minimum values related to the scale of (X,Y).
                       This keyword alters the default scaling factor, in
                       the sense that scale=2.0 will produce twice the
                       Z-relief of the default scale=1.0.
             cmax   -- the ambient= keyword in light3 can be used to
                       control how dark the darkest surface is; use this
                       to control how light the lightest surface is
                       the lightwf routine can change this parameter
                       interactively

   SEE ALSO: lightwf, plm, plf, orient3, light3, window3, limit3
 */
{
  if (_draw3) {
    xyz= _nxt(z);
    fill= _nxt(z);
    shade= _nxt(z);
    edges= _nxt(z);
    ecolor= _nxt(z);
    ewidth= _nxt(z);
    cull= _nxt(z);
    cmax= _nxt(z);

    get3_xy, xyz, x, y, z, 1;

    /* rotate (x,y,0) into on-screen orientation to determine order
     * just use four corners for this */
    nx= dimsof(x);
    ny= nx(3);
    nx= nx(2);
    xyzc= xyz(,1:nx:nx-1,1:ny:ny-1);
    xyzc(3,,)= 0.0;
    get3_xy, xyzc, xc, yc, zc, 1;

    /* compute mean i-edge and j-edge vector z-components */
    iedge= avg(zc(0,)-zc(1,));
    jedge= avg(zc(,0)-zc(,1));

    /* compute shading if necessary */
    if (shade) {
      xyz(1,,)= x;
      xyz(2,,)= y;
      xyz(3,,)= z;
      fill= get3_light(xyz);
    }

    /* The order either requires a transpose or not, reversal of the
       order of the first dimension or not, and reversal of the order
       of the second dimension or not.  */

    /* The direction with the minimum magnitude average z-component must
       vary fastest in the painting order.  If this is the j-direction,
       a transpose will be required to make this the i-direction.  */
    if (abs(jedge)<abs(iedge)) {
      tmp= iedge;   iedge= jedge;   jedge= tmp;
      x= transpose(x);
      y= transpose(y);
      if (!is_void(fill)) fill= transpose(fill);
    }

    /* Zones must be drawn from back to front, which means that the
       average z-component of the edge vectors must be positive.  This
       can be arranged by reversing the order of the elements if
       necessary.  */
    if (iedge<0.0) {
      x= x(::-1,);
      y= y(::-1,);
      if (!is_void(fill)) fill= fill(::-1,);
    }
    if (jedge<0.0) {
      x= x(,::-1);
      y= y(,::-1);
      if (!is_void(fill)) fill= fill(,::-1);
    }

    plf, fill, y,x, edges=edges,ecolor=ecolor,ewidth=ewidth,
      cmin=0.0,cmax=cmax,legend=string(0);
    return;
  }

  xyz= xyz_wf(z, y, x, scale, scale=scale);

  if (is_void(edges)) edges= 1;
  if (is_void(shade)) shade= 0;
  else if (!is_void(fill))
    error, "specify either fill or shade, not both";

  clear3;
  limit3, scale;
  set3_object, plwf,
    _lst(xyz, fill, shade, edges, ecolor, ewidth, cull, cmax);
}

func lightwf(cmax)
/* DOCUMENT lightwf, cmax
     Sets the cmax= parameter interactively, assuming the current
     3D display list contains the result of a previous plwf call.
     This changes the color of the brightest surface in the picture.
     The darkest surface color can be controlled using the ambient=
     keyword to light3.
   SEE ALSO: plwf, light3
 */
{
  list= _cdr(_draw3_list, _draw3_n);
  if (_car(list)!=plwf) error, "current 3D display list is not a plwf";
  _undo3_set, lightwf, _car(_car(list,2), 8, cmax);
}

/* The function which scales the "topography" of z(x,y) is
 * potentially useful apart from plwf.
 * For example, the xyz array used by plwf can be converted from
 * a quadrilateral mesh plotted using plf to a polygon list plotted
 * using plfp like this:
 *   xyz= xyz_wf(z,y,x,lims,scale=scale);
 *   ni= dimsof(z)(2);
 *   nj= dimsof(z)(3);
 *   list= indgen(1:ni-1)+ni*indgen(0:nj-2)(-,);
 *   xyz= xyz(,([0,1,ni+1,ni]+list(-,))(*));
 *   nxyz= array(4, (ni-1)*(nj-1));
 *   ...
 *   limit3, lims;
 */
func xyz_wf(z, y, x, &lims, scale=)
{
  if (min(dimsof(z))<2) error, "impossible dimensions for z array";
  if (is_void(y) || is_void(x)) {
    if (!is_void(y) || !is_void(x)) error, "either give y,x both or neither";
    nx= dimsof(z)(2);
    ny= dimsof(z)(3);
    x= span(1,nx,nx)(,-:1:ny);
    y= span(1,ny,ny)(-:1:nx,);
  } else if (anyof(dimsof(x)!=dimsof(z)) ||
             anyof(dimsof(x)!=dimsof(z))) {
    error, "x, y, and z must all have same dimensions";
  }

  lims= array(0., 3,3);
  lims(1:2,1)= xnx= _wf_safe(x);
  lims(1:2,2)= ynx= _wf_safe(y);
  lims(1:2,3)= _wf_safe(z);
  lims(3,1)= xnx(ptp);
  lims(3,2)= ynx(ptp);
  xyscl= double(max(xnx(ptp),ynx(ptp)));
  if (!is_void(scale)) xyscl*= scale;
  lims(3,3)= 0.5*xyscl;

  xyz= array(0.0, 3,dimsof(z));
  xyz(1,,)= x;
  xyz(2,,)= y;
  xyz(3,,)= z;

  return xyz;
}

func _wf_safe(a)
{
  mx= max(a);
  mn= min(a);
  if (mx==mn) {
    d= mn? 0.01*abs(mn) : 0.01;
    mn-= d;
    mx+= d;
  }
  return [mn,mx];
}
