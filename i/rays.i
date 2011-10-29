/*
 * $Id: rays.i,v 1.1 2005-09-18 22:06:05 dhmunro Exp $
 * Yorick functions to manipulate 3-D rays on a cylindrical mesh.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*
   form_rays      -- convert from nrays lists to (5, 3, or 6)-by-nrays
   best_rays      -- convert to 5-by-nrays representation
   dirt_rays      -- convert to 3-by-nrays representation
   internal_rays  -- convert to 6-by-nrays representation
   picture_rays   -- return rays at centers of arbitrary "pixel mesh"

   get_s0         -- compute origins for slimits (internally, slimits
                     is based on an s coordinate which is zero at the
                     point of closest approach to the origin, rather
                     than at the x,y,z point selected in the best_rays
                     or internal_rays coordinate systems)

   plray          -- plot a ray
   plray_lims     -- set limits for plray
 */

/*= SECTION() create and plot ray sets for drat package ====================*/

func form_rays(rays)
/* DOCUMENT best= form_rays( [x, y, z, theta, phi] )
         or dirt= form_rays( [x, y, theta] )
         or internal= form_rays( [cos, sin, y, z, x, r] )
     forms 5-by-nrays, 3-by-nrays, or 6-by-nrays ray representation
     given individual lists of array coordinates.  The [...]
     operator builds an nrays-by-5, nrays-by-3, or nrays-by-6
     array, which form_rays transposes.  The "nrays" may represent
     zero or more actual dimensions.
   SEE ALSO: best_rays, dirt_rays, internal_rays, picture_rays
 */
{ return transpose(rays, 2); }

func best_rays(rays)
/* DOCUMENT best_rays(rays)
     returns 5-element (x,y,z,theta,phi) representation of RAYS.
     The first dimension of RAYS may be length 3, 5, or 6 to represent
     the ray(s) in TDG/DIRT coordinates (x,y,theta), "best" coordinates
     (x,y,z,theta,phi), or internal coordinates (cos,sin,y,z,x,r),
     respectively.  The first dimension of the result always has length 5.

     The "best" coordinate system is the easiest to visualize:
     (x,y,z) represents any point on the ray, while (theta,phi)
     represents the ray direction in standard spherical coordinates
     relative to the +z-axis.  Namely, theta is the angle from the
     +z-direction to the ray direction (between 0 and pi), and phi is
     the counterclockwise angle from the +x-axis to the projection of
     the ray direction into the xy-plane, assuming xyz is a right-handed
     coordinate system.

     As a specification of a ray, this system is doubly redundant because
     the point (x,y,z) could be any point on the ray, and the underlying
     mesh through which the ray propagates is cylindrically symmetric about
     the z-axis.

     However, the slimits parameter -- used to specify the points along
     a ray where the transport integration starts and stops -- is
     measured from the point (x,y,z) specified as a part of the
     (x,y,z,theta,phi) ray coordinate.  Thus, any change in the point
     (x,y,z) on a ray must be accompanied by a corresponding change in
     the slimits for that ray.

   SEE ALSO: form_rays, dirt_rays, internal_rays, get_s0, picture_rays
 */
{
  dummy= use_origins(0);

  dim= dimsof(rays)(2);
  if (dim==5) return rays;

  if (dim==3) {
    /* convert from DIRT/TDG (x,y,theta) to (x,y,z,theta,phi) */
    x= rays(1,..);
    y= rays(2,..);
    theta= rays(3,..);
    return form_rays([x*cos(theta), y, x*sin(theta),
                      abs(theta), (theta>=0.0)*pi]);

  } else if (dim==6) {
    /* convert from internal (cos,sin,y,z,x,r) to (x,y,z,theta,phi) */
    theta= atan(rays(2,..), rays(1,..));
    y= rays(3,..);
    z= rays(4,..);
    x= rays(5,..);
    return form_rays([x, y, z, abs(theta), (theta<0)*pi]);
  }

  return [];
}

func dirt_rays(rays)
/* DOCUMENT dirt_rays(rays)
     returns 3-element (x,y,theta) representation of RAYS.
     The first dimension of RAYS may be length 3, 5, or 6 to represent
     the ray(s) in TDG/DIRT coordinates (x,y,theta), "best" coordinates
     (x,y,z,theta,phi), or internal coordinates (cos,sin,y,z,x,r),
     respectively.  The first dimension of the result always has length 3.

     The TDG/DIRT coordinate system is based on the coordinates (x,y)
     in a plane normal to the ray.  Unfortunately, the old TDG and DIRT
     codes used an angle theta which has the opposite sense from the
     "best" and internal coordinates.  Therefore, conversion from
     TDG/DIRT coordinates to internal coordinates will reverse the
     sign of theta.  Conversion from TDG/DIRT coordinates to "best"
     coordinates always results in positive theta, but the angle phi
     will be pi for positive input theta.

     The slimits parameter -- used to specify the points along
     a ray where the transport integration starts and stops -- is
     measured from the point of closest approach of the ray described
     by (x,y,theta) to the origin x=y=z=0.  Therefore, slimits is
     independent of the TDG/DIRT ray coordinate representation.

   SEE ALSO: form_rays, best_rays, internal_rays, get_s0, picture_rays
 */
{
  dummy= use_origins(0);

  dim= dimsof(rays)(2);
  if (dim==3) return rays;

  if (dim==5) {
    /* convert from best (x,y,z,theta,phi) to DIRT/TDG (x,y,theta) */
    x= rays(1,..);
    y= rays(2,..);
    z= rays(3,..);
    theta= rays(4,..);
    phi= rays(5,..);
    cosp= cos(phi);
    sinp= sin(phi);
    sgn= double((cosp>=0.0)*2-1);
    return form_rays(sgn*[(x*cosp+y*sinp)*cos(theta) - z*sin(theta),
                          (y*cosp-x*sinp), -theta]);

  } else if (dim==6) {
    /* convert from internal (cos,sin,y,z,x,r) to DIRT/TDG (x,y,theta) */
    cost= rays(1,..);
    sint= rays(2,..)
    y= rays(3,..);
    z= rays(4,..);
    x= rays(5,..);
    /* NOTE SIGN CHANGE IN THETA */
    return form_rays([x*cost-z*sint, y, -atan(sint, cost)]);  
  }

  return [];
}

func internal_rays(rays)
/* DOCUMENT internal_rays(rays)
     returns 6-element (cos,sin,y,z,x,r) representation of RAYS.
     The first dimension of RAYS may be length 3, 5, or 6 to represent
     the ray(s) in TDG/DIRT coordinates (x,y,theta), "best" coordinates
     (x,y,z,theta,phi), or internal coordinates (cos,sin,y,z,x,r),
     respectively.  The first dimension of the result always has length 6.

     The internal coordinates are what Drat uses internally to
     describe the ray.  The coordinate system is rotated about the
     z-axis until the ray lies in a plane of constant y (there are at
     least two ways to do this).  The point (x,y,z) can be any point on
     the ray, and r=sqrt(x^2+y^2) is the corresponding cylindrical radius.
     The clockwise angle theta from the +z-axis to the ray direction
     (which always lies in the zx-plane) determines cos=cos(theta) and
     sin=sin(theta).

     As a specification of a ray, this system is triply redundant because
     the point (x,y,z) could be any point on the ray, both the sine and
     cosine of theta appear, and r=sqrt(x^2+y^2).

     However, the slimits parameter -- used to specify the points along
     a ray where the transport integration starts and stops -- is
     measured from the point (x,y,z) specified as a part of the
     (cos,sin,y,z,x,r) ray coordinate.  Thus, any change in the point
     (x,y,z) on a ray must be accompanied by a corresponding change in
     the slimits for that ray.

   SEE ALSO: form_rays, best_rays, dirt_rays, get_s0, picture_rays
 */
{
  dummy= use_origins(0);

  dim= dimsof(rays)(2);

  if (dim==6) {
    /* assure that r is consistent with x and y */
    rays(6,..)= abs(rays(3,..), rays(5,..));
    return rays;

  } else if (dim==5) {
    /* convert from best (x,y,z,theta,phi) to internal (cos,sin,y,z,x,r) */
    x= rays(1,..);
    y= rays(2,..);
    z= rays(3,..);
    theta= rays(4,..);
    phi= rays(5,..);
    cosp= cos(phi);
    sinp= sin(phi);
    sgn= double((cosp>=0.0)*2-1);
    return form_rays([cos(theta), sgn*sin(theta), sgn*(y*cosp-x*sinp), z,
                      sgn*(x*cosp+y*sinp), abs(x, y)]);

  } else if (dim==3) {
    /* convert from DIRT/TDG (x,y,theta) to internal (cos,sin,y,z,x,r) */
    x= rays(1,..);
    y= rays(2,..);
    theta= rays(3,..);
    cost= cos(theta);
    sint= sin(theta);
    xcost= x*cost;
    /* NOTE SIGN CHANGE IN THETA */
    return form_rays([cost, -sint, y, x*sint, xcost, abs(xcost, y)]);
  }

  return [];
}

func get_s0(rays)
/* DOCUMENT get_s0(rays)
     returns the s-coordinate of the point of closest approach of
     the RAYS to the origin x=y=z=0.  The length of the first dimension
     of RAYS may be either 3, 5, or 6; this first dimension will not
     be present in the result.

     The s-coordinate represents distance along the ray, increasing in
     the direction the ray moves.  The 5 and 6 component ray coordinates
     include a reference point (x,y,z) on the ray; s=0 at that point.
     For the 3 component ray coordinate, get_s0 always returns 0.

   SEE ALSO: best_rays, dirt_rays, internal_rays
 */
{
  dummy= use_origins(0);

  dim= dimsof(rays)(2);

  if (dim==3) {
    return array(0.0, dimsof(rays(1,..)));

  } else if (dim==5) {
    x= rays(1,..);
    y= rays(2,..);
    z= rays(3,..);
    theta= rays(4,..);
    phi= rays(5,..);
    cost= cos(theta);
    sint= sin(theta);
    x= x*cos(phi)+y*sin(phi);

  } else if (dim==6) {
    cost= rays(1,..);
    sint= rays(2,..)
    z= rays(4,..);
    x= rays(5,..);
  }

  return -z*cost-x*sint;
}

func picture_rays(xpict, ypict, ray, theta_up, phi_up)
/* DOCUMENT picture_rays(xpict, ypict, ray)
         or picture_rays(xpict, ypict, ray, theta_up, phi_up)
     returns 2-D array of rays, one at the center of each zone (which
     represents a pixel here) of the mesh (XPICT, YPICT).  The rays are
     all parallel to the given RAY (a 3, 5, or 6 element vector).  The
     (XPICT, YPICT) coordinates are in the plane perpendicular to the rays,
     with the origin XPICT=YPICT=0 at the given RAY.

     If (THETA_UP, PHI_UP) are given, the +YPICT-axis will lie along the
     projection of the (THETA_UP, PHI_UP) direction into the (XPICT, YPICT)
     plane.  The default (THETA_UP, PHI_UP) is (pi/2, pi/2) -- the +y-axis
     -- unless (THETA, PHI) is the y-axis, in which case it is (pi/2, 0)
     -- the +x-axis.  This matches the DIRT/TDG ray coordinate convention
     in the sense that if RAY is [0,0,theta], then
     (zncen(XPICT),zncen(YPICT)) are the DIRT/TDG (x,y) coordinates for
     the rays.

     If XPICT and YPICT are imax-by-jmax, the returned array will have
     dimensions 5-by-(imax-1)-by-(jmax-1).  That is, "best" coordinates
     are returned.  The (x,y,z) of all of the returned rays will lie in
     the plane perpendicular to the ray passing through the given central
     RAY.

   SEE ALSO: form_rays, best_rays, dirt_rays, internal_rays, area
 */
{
  dummy= use_origins(0);
  ray= best_rays(ray);
  x= ray(1);
  y= ray(2);
  z= ray(3);
  theta= ray(4);
  phi= ray(5);

  /* form (rx,ry,rz) ray direction cosines */
  sint= sin(theta);
  rx= sint*cos(phi);
  ry= sint*sin(phi);
  rz= cos(theta);

  /* form (ux,uy,uz) picture up direction cosines */
  if (!is_void(theta_up)) {
    sint= sin(theta_up);
    ux= sint*cos(phi_up);
    uy= sint*sin(phi_up);
    uz= cos(theta_up);
  } else {
    if (rz || rx) { ux= 0.0; uy= 1.0; }
    else { ux= 1.0; uy= 0.0; }
    uz= 0.0;
  }

  /* form (px,py,pz) picture right direction cosines */
  px= uy*rz - uz*ry;
  py= uz*rx - ux*rz;
  pz= ux*ry - uy*rx;
  len= abs(px, py, pz);
  px/= len;
  py/= len;
  pz/= len;

  /* project (ux,uy,uz) perpendicular to (rx,ry,rz) */
  ux= ry*pz - rz*py;
  uy= rz*px - rx*pz;
  uz= rx*py - ry*px;

  /* get coordinates of zone centers in picture plane */
  xp= xpict(zcen, zcen);
  yp= ypict(zcen, zcen);

  return form_rays([x+xp*px+yp*ux, y+xp*py+yp*uy, z+xp*pz+yp*uz,
                    theta, phi]);
}

func plray(ray)
/* DOCUMENT plray, ray
     where RAY is a vector of length 5 representing [x, y, z, theta, phi]
     -- a point (x,y,z) on the ray, and the ray direction (theta,phi)
     relative to the z-axis.  The ray hyperbola is plotted with the
     z-axis horizontal.  The portion of the ray plotted is determined
     by the plray_lims command, which must be issued prior to the first
     plray.
     The 3 and 6 component ray formats are also accepted.
   SEE ALSO: plray_lims, dirt_rays, internal_rays
 */
{
  dims= dimsof(ray);
  if (dims(1)!=1) error, "ray must be a vector of length 5 (or 3 or 6)";
  if (dims(2)!=5) ry= best_rays(ray);
  else ry= ray;

  dummy= use_origins(0);

  x0= ry(1);
  y0= ry(2);
  z0= ry(3);
  theta= ry(4);
  phi= ry(5);

  sint= sin(theta);
  cost= cos(theta);
  sinp= sin(phi);
  cosp= cos(phi);

  rtp= abs(x0*sinp - y0*cosp);  /* turning point radius */

  /**/ extern plray_zx, plray_zn, plray_rx;
  /**/ extern plray_n;   /* always odd! */

  if (sint < 1.e-4) {
    /* rays which are nearly parallel to the axis need special treatment */
    z= span(plray_zn, plray_zx, 5);
    if (cost<0.0) z= z(::-1);
    r= (z-z0)*sint/cost;
    r= abs(x0+r*cosp, y0+r*sinp);

  } else {
    /* form th == tan(angle)/abs(tan(theta)) where angle is evenly
       spaced as th varies from -1 to 1 */
    abstan= abs(sint)/(abs(cost)+1.e-20);
    absth= atan(abstan);
    fuzz= 1.e-6;
    th= span(-absth, absth, plray_n);
    th(1)= -(1.0-0.5*fuzz*fuzz);
    th(2:-1)= tan(th(2:-1))/abstan;
    th(0)= -th(1);

    sqth= sqrt((1.0-th)*(1.0+th));
    sqth(1)= sqth(0)= fuzz;

    /* rays which nearly hit the axis need special treatment */
    if (rtp < fuzz*plray_rx) rt= fuzz*plray_rx/sqth;
    else rt= rtp/sqth;

    zt= z0 + (rt*th - (x0*cosp-y0*sinp)*sign(sint))*sign(cost)/abstan;

    /* clip (zt,rt) to (z,r) with z between plray_zn and plray_zx */
    list= where(zt>plray_zn & zt<plray_zx);
    if (min(abs(zt(dif))) > 0.0) {
      if (cost>=0.0) {  /* z increasing */
        if (min(zt)<=plray_zn) {
          z= [plray_zn];
          r= interp(rt, zt, z);
        } else {
          z= [];
          r= [];
        }
        grow, z, zt(list);
        grow, r, rt(list);
        if (max(zt)>=plray_zx) {
          grow, z, [plray_zx];
          grow, r, interp(rt, zt, [plray_zx]);
        }
      } else {          /* z decreasing */
        if (max(zt)>=plray_zx) {
          z= [plray_zx];
          r= interp(rt, zt, z);
        } else {
          z= [];
          r= [];
        }
        grow, z, zt(list);
        grow, r, rt(list);
        if (min(zt)<=plray_zn) {
          grow, z, [plray_zn];
          grow, r, interp(rt, zt, [plray_zn]);
        }
      }
    } else {
      z= zt(list);
      r= rt(list);
    }
  }

  /* clip (z,r) to r < rx */
  if (numberof(r) && numberof((list= where(r<plray_rx)))) {
    if (numberof(list) < numberof(r)) {
      rt= r;
      zt= z;
      i= rt(mnx);  /* the interp operation seems to be safe even if many
                      of the rt are clustered at this minimum value */
      if (min(list)<=i && max(rt(:i))>=plray_rx) {
        r= [plray_rx];
        z= interp(zt(:i), rt(:i), r);
      } else {
        r= [];
        z= [];
      }
      grow, r, rt(list);
      grow, z, zt(list);
      if (max(list)>=i && max(rt(i:))>=plray_rx) {
        grow, r, [plray_rx];
        grow, z, interp(zt(i:), rt(i:), [plray_rx]);
      }
    }

    /* finally, plot the result */
    /* Note: Gist substitutes the marker for \001 if that is the
             first character of the legend.  */
    legend=
      swrite(format="\001: theta= %.4g, phi= %.4g, thru (%.4g, %.4g, %.4g)",
             theta, phi, x0, y0, z0)(1);
    plg, r, z, rays=1, legend=legend;

  } else {
    write, "WARNING (plray) ray not within limits (use plray_lims)";
  }
}

func plray_lims(zmin, zmax, rmax, rx)
/* DOCUMENT plray_lims, zmin, zmax, rmax
     sets the (z,r) coordinate limits for the plray command.  Subsequent
     rays will be clipped to the region from z=ZMIN to z=ZMAX, and from
     r=0 to r=RMAX.
   SEE ALSO: plray
 */
{
  /**/ extern plray_zx, plray_zn, plray_rx;
  if (!is_void(rx)) rmax= rx;  /* allow for limits syntax as well */
  if (zmin!=zmax && rmax>0.0) {
    plray_zx= double(max(zmin, zmax));
    plray_zn= double(min(zmin, zmax));
    plray_rx= double(rmax);
    limits, plray_zn, plray_zx, 0.0, plray_rx;
  } else {
    error, "zmin==zmax or rmax<=0.0 are not legal ray limits";
  }
}

plray_zx= 1.0;
plray_zn= -1.0;
plray_rx= 1.0;
plray_n= 31;    /* This should always be an odd number to ensure that
                   the turning point of the ray is always one of the
                   points plotted.  */
