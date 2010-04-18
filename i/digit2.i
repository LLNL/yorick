/*
 * $Id: digit2.i,v 1.2 2010-04-18 10:33:38 thiebaut Exp $
 * 2D versions of digitize and interp functions (based on mesh_loc)
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func digit2(y0,x0, y,x,reg, pt=)
/* DOCUMENT digit2(y0,x0, y,x)
         or digit2(y0,x0, y,x,reg)

     return the index of the zone of the point or points (X0,Y0)
     in the quadrilateral mesh (X,Y) with the optional region
     array REG.  The result has the same dimensions as the input
     X0 and Y0 arrays.  The result is <=0 at points outside the mesh.

     By default, the zone index is an index into an (M-1)-by-(N-1)
     array, if X and Y are M-by-N.  However, if the keyword pt= is
     non-nil and non-zero, the return value is the index into an
     M-by-N array in which the first row and column are non-existent
     (like the optional REG array).

   SEE ALSO: digitize, interp2, mesh_loc, plm
 */
{
  zero= array(0, dimsof(x0,y0));
  ndx= mesh_loc(y0+zero,x0+zero, y,x,reg);
  ndx+= zero;  /* works around bug- mesh_loc can return scalar */
  if (pt) return ndx;
  m= dimsof(y)(2);
  return ndx - m - (ndx-1)/m;
}

func interp2(y0,x0, z,y,x,reg, outside=)
/* DOCUMENT z0= interp2(y0,x0, z,y,x)
         or z0= interp2(y0,x0, z,y,x,reg)

     return the bilinear interpolate of the function Z(X,Y) at the
     points (X0,Y0).  The X, Y, and optional REG arrays specify a
     quadrilateral mesh as for the plm function.  The Z values are
     specified at the vertices of this mesh, so Z must have the
     same dimensions as X and Y.

     Points outside the mesh get the value 0.0, unless the outside
     keyword is non-nil, in which case they get that value.

   SEE ALSO: interp, digit2, mesh_loc, plm
 */
{
  scalar= !dimsof(x0,y0)(1);
  if (scalar) { x0= [x0];  y0= [y0]; }
  ndx= digit2(y0,x0, y,x,reg, pt=1);
  mask= ndx>0;

  /* first handle points inside mesh */
  list= where(mask);
  if (numberof(list)) {
    ndx= ndx(list);
    zero= array(0., dimsof(x0,y0));
    x0= (x0+zero)(list);
    y0= (y0+zero)(list);
    /* here are corners of the interpolation tets */
    m= dimsof(y)(2);
    x00= [x(ndx-m-1), y(ndx-m-1), z(ndx-m-1)];
    x10= [x(ndx-m), y(ndx-m), z(ndx-m)];
    x11= [x(ndx), y(ndx), z(ndx)];
    x01= [x(ndx-1), y(ndx-1), z(ndx-1)];
    /* form the centroid, make centroid the origin */
    xc= 0.25*(x00+x10+x01+x11);
    x0= xc(,1)-x0;
    y0= xc(,2)-y0;
    /* form the median vectors */
    xm= 0.5*[x11+x10-x01-x00, x11+x01-x10-x00, x11+x00-x10-x01];
    xu= xm(,1,1);  yu= xm(,2,1);  zu= xm(,3,1);
    xv= xm(,1,2);  yv= xm(,2,2);  zv= xm(,3,2);
    xw= xm(,1,3);  yw= xm(,2,3);  zw= xm(,3,3);
    x00= x10= x11= x01= xm= [];
    /* form various cross products */
    cwu= xw*yu - yw*xu;
    cwv= xw*yv - yw*xv;
    cuv= xu*yv - yu*xv;
    cup= xu*y0 - yu*x0;
    cvp= xv*y0 - yv*x0;
    cwp= xw*y0 - yw*x0;
    /* compute the discriminant */
    cuv*= 0.5;
    cwu*= 2.0;
    cwv*= 2.0;
    bu= cwp - cuv;
    bv= cwp + cuv;
    tmpa= bu*bu;
    tmpb= bv*bv;
    use= double(tmpa<=tmpb);
    dis= sqrt(max(use*(tmpa-cwu*cvp) + (1.-use)*(tmpb-cwv*cup), 0.0));
    /* only one solution is the correct one, but there is no way to
       know which it is without computing both (want to allow bowtied
       zones and other pathologies) */
    mask2= bu>=0.0;
    list= where(mask2);
    if (numberof(list)) {
      tmpa= -bu(list)-dis(list);
      tmpb= cwu(list);
      epsa= double(!tmpa)*1.e-99;
      epsb= double(!tmpb)*1.e-99;
      u11= tmpa/(tmpb+epsb);
      u21= cvp(list)/(tmpa+epsa);
    }
    list= where(!mask2);
    if (numberof(list)) {
      tmpa= -bu(list)+dis(list);
      tmpb= cwu(list);
      epsa= double(!tmpa)*1.e-99;
      epsb= double(!tmpb)*1.e-99;
      u22= tmpa/(tmpb+epsb);
      u12= cvp(list)/(tmpa+epsa);
    }
    u1= merge(u11,u12,mask2);
    u2= merge(u21,u22,mask2);
    u11= u21= u12= u22= [];
    mask2= bv>=0.0;
    list= where(mask2);
    if (numberof(list)) {
      tmpa= -bv(list)-dis(list);
      tmpb= cwv(list);
      epsa= double(!tmpa)*1.e-99;
      epsb= double(!tmpb)*1.e-99;
      v21= tmpa/(tmpb+epsb);
      v11= cup(list)/(tmpa+epsa);
    }
    list= where(!mask2);
    if (numberof(list)) {
      tmpa= -bv(list)+dis(list);
      tmpb= cwv(list);
      epsa= double(!tmpa)*1.e-99;
      epsb= double(!tmpb)*1.e-99;
      v12= tmpa/(tmpb+epsb);
      v22= cup(list)/(tmpa+epsa);
    }
    v1= merge(v11,v12,mask2);
    v2= merge(v21,v22,mask2);
    v11= v21= v12= v22= [];
    /* compute the two z values and select the proper one */
    xc= xc(,3);
    z1= zw*2.*u1*v1 + zv*v1 + zu*u1 + xc;
    z2= zw*2.*u2*v2 + zv*v2 + zu*u2 + xc;
    mask2= max(abs(v1),abs(u1))<=max(abs(v2),abs(u2));
    za= merge(z1(where(mask2)),z2(where(!mask2)),mask2);
  }

  /* just punt on points outside mesh */
  list= where(!mask);
  if (numberof(list)) {
    if (is_void(outside)) outside= 0.0;
    zb= array(outside, numberof(list));
  }

  if (scalar) mask= mask(1);
  return merge(za,zb,mask);
}
