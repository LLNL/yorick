/*
 * $Id: pl3d.i,v 1.1 2005-09-18 22:06:03 dhmunro Exp $
 * Viewing transforms and other aids for 3D plotting.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* General overview:

   (1) Viewing transform machinery.  Arguably the simplest model
       is the CAD/CAM notion that the object you see is oriented
       as you see it in the current picture.  You can then move
       it left, right, up, down, or toward or away from you,
       or you can rotate it about any of the three axes (horizontal,
       vertical, or out of the screen).  The xyz coordinates of the
       object remains unchanged throughout all of this, but this
       object coordinate system changes relative to the fixed
       xyz of the viewer, in which x is always to the right, y is
       up, and z is directed out of the screen.  Initially, the
       two coordinate systems coincide.

       rot3, xangle,yangle,zangle
         Rotate the object about viewer's x-axis by xangle, then
         about viewer's y-axis by yangle, then about viewer's
         z-axis by zangle
       mov3, xchange,ychange,zchange
         Move the object by the specified amounts.

       setz3, zcamera
         The "camera" is located at (0,0,zcamera) in the viewer's
         coordinate system, looking in the minus-z direction.
         Initially, zcamera is very large, and the magnification
         factor is correspondingly large, giving an isometric view.
         Decreasing zcamera makes the perspective more extreme.
         If parts of the object are behind the camera, strange things
         may happen.

       undo3
       undo3, n
         Undo the last N (default 1) viewpoint commands (rot3, mov3,
         or setz3).  Up to 100 viewpoint changes are remembered.
       viewpoint= save3()
       ...
       restore3, viewpoint
         The current viewpoint transformation can be saved and later
         restored.

       gnomon, on_off
         Toggle the gnomon (a simple display showing the orientation
         of the xyz axes of the object).
 */

/* ------------------------------------------------------------------------ */

func rot3(xa,ya,za)
/* DOCUMENT rot3, xa,ya,za
     rotate the current 3D plot by XA about viewer's x-axis,
     YA about viewer's y-axis, and ZA about viewer's z-axis.
   SEE ALSO: orient3, mov3, aim3, setz3, undo3, save3, restore3, light3
 */
{
  if (is_void(xa)) xa= 0.;
  if (is_void(ya)) ya= 0.;
  if (is_void(za)) za= 0.;
  x= [1.,0.,0.];
  y= [0.,1.,0.];
  z= [0.,0.,1.];
  _rot3, za, x, y;
  _rot3, ya, z, x;
  _rot3, xa, y, z;
  _setrot3, [x,y,z](,+)*_getrot3()(+,);
}

func _rot3(a, &x, &y)
{
  ca= cos(a);
  sa= sin(a);
  a= x;
  x=  ca*a + sa*y;
  y= -sa*a + ca*y;
}

func mov3(xa,ya,za)
/* DOCUMENT mov3, xa,ya,za
     move the current 3D plot by XA along viewer's x-axis,
     YA along viewer's y-axis, and ZA along viewer's z-axis.
   SEE ALSO: rot3, orient3, setz3, undo3, save3, restore3, light3
 */
{
  if (is_void(xa)) xa= 0.;
  if (is_void(ya)) ya= 0.;
  if (is_void(za)) za= 0.;
  _setorg3, _getorg3() - _getrot3()(+,)*[xa,ya,za](+);
}

func aim3(xa,ya,za)
/* DOCUMENT aim3, xa,ya,za
     move the current 3D plot to put the point (XA,YA,ZA) in object
     coordinates at the point (0,0,0) -- the aim point -- in the
     viewer's coordinates.  If any of XA, YA, or ZA is nil, it defaults
     to zero.
   SEE ALSO: mov3, rot3, orient3, setz3, undo3, save3, restore3, light3
 */
{
  if (is_void(xa)) xa= 0.;
  if (is_void(ya)) ya= 0.;
  if (is_void(za)) za= 0.;
  _setorg3, [xa,ya,za];
}

func setz3(zc)
/* DOCUMENT setz3, zc
     Set the camera position to z=ZC (x=y=0) in the viewer's coordinate
     system.  If ZC is nil, set the camera to infinity (default).
   SEE ALSO: rot3, orient3, undo3, save3, restore3, light3
 */
{
  if (!is_void(zc)) {
    if (dimsof(zc)(1)) error, "camera position must be scalar";
    zc= double(zc);
  }
  _setzc3, zc;
}

func orient3(phi, theta)
/* DOCUMENT orient3, phi, theta
         or orient3, phi
         or orient3, , theta
         or orient3
     Set the "orientation" of the object to (PHI,THETA).  "Orientations"
     are a subset of the possible rotation matrices in which the z-axis
     of the object appears vertical on the screen (that is, the object
     z-axis projects onto the viewer y-axis).  The THETA angle is the
     angle from the viewer y-axis to the object z-axis, positive if
     the object z-axis is tilted toward you (toward viewer +z).  PHI is
     zero when the object x-axis coincides with the viewer x-axis.  If
     neither PHI nor THETA is specified, PHI defaults to -pi/4 and
     THETA defaults to pi/6.  If only one of PHI or THETA is specified,
     the other remains unchanged, unless the current THETA is near pi/2,
     in which case THETA returns to pi/6, or unless the current
     orientation does not have a vertical z-axis, in which case the
     unspecified value returns to its default.

     Unlike rot3, orient3 is not a cumulative operation.

   SEE ALSO: rot3, mov3, aim3, save3, restore3, light3, limit3
 */
{
  if (is_void(theta) && is_void(phi)) {
    theta= _orient3_theta;
    phi= _orient3_phi;
  } else if (is_void(theta) || is_void(phi)) {
    z= _getrot3()(,+)*[0.,0.,1.](+);
    if (abs(z(1))>1.e-6) {
      /* object z-axis not aligned with viewer y-axis */
      if (is_void(theta)) theta= _orient3_theta;
      else phi= _orient3_phi;
    } else if (is_void(theta)) {
      if (abs(z(2))<1.e-6) theta= _orient3_theta;
      else theta= atan(z(3),z(2));
    } else /*if (is_void(phi))*/ {
      y= [0.,z(3),-z(2)];  /* in object xy-plane */
      x= _getrot3()(,+)*[1.,0.,0.](+);
      phi= atan(y(+)*x(+), x(1));
    }
  }
  x= [1.,0.,0.];
  y= [0.,1.,0.];
  z= [0.,0.,1.];
  _rot3, theta, y, z;
  _rot3, phi, z, x;
  _setrot3, [x,-z,y];
}

/* unless user has supplied alternative defaults, set orient3 defaults */
if (is_void(_orient3_theta)) _orient3_theta= pi/6;
if (is_void(_orient3_phi)) _orient3_phi= -pi/4;

func limit3(xn,xx,yn,yx,zn,zx,aspect=)
/* DOCUMENT limit3, xmin,xmax, ymin,ymax
         or limit3, xmin,xmax, ymin,ymax, zmin,zmax
     Set the 3D axis limits for use with the cage.
     Use keyword aspect=[ax,ay,az] to set the aspect ratios of the
     cage to ax:ay:az -- that is, the ratios of the lengths of the
     cage axes will become ax:ay:az.
   SEE ALSO: cage3, range3, plwf (include plwf.i), orient3
 */
{
  local limits;
  eq_nocopy, limits, _car(_draw3_list, _draw3_nll+1);
  if (dimsof(xn)(1)>1) {
    if (dimsof(xn)(1)!=2 || anyof(dimsof(xn)(2:3)!=3))
      error, "bad limit3 arguments";
    _setscl3, xn;
    return limits;
  }
  void= [[is_void(xn),is_void(xx)],[is_void(yn),is_void(yx)],
         [is_void(zn),is_void(zx)]];
  if (nallof(void) || !is_void(aspect)) {
    if (is_void(limits)) {
      if (anyof(void))
        error, "no xyz limits currently set -- you must set all six";
      lims= array([0.,1.,1.],3);
    } else {
      lims= limits;
    }
    if (!void(1,1)) lims(1,1)= xn;
    if (!void(2,1)) lims(2,1)= xx;
    if (!void(1,2)) lims(1,2)= yn;
    if (!void(2,2)) lims(2,2)= yx;
    if (!void(1,3)) lims(1,3)= zn;
    if (!void(2,3)) lims(2,3)= zx;
    if (!is_void(aspect)) lims(3,)= aspect;
    _setscl3, lims;
  }
  return limits;
}

func range3(zn,zx,aspect=)
/* DOCUMENT range3, zmin,zmax
     Set the 3D axis z limits for use with the cage.
     Use keyword aspect=[ax,ay,az] to set the aspect ratios of the
     cage to ax:ay:az -- that is, the ratios of the lengths of the
     cage axes will become ax:ay:az.
   SEE ALSO: cage3, limit3, plwf (include plwf.i), orient3
 */
{
  return limit3(,,,,zn,zx,aspect=aspect);
}

func save3(void)
/* DOCUMENT view= save3()
     Save the current 3D viewing transformation and lighting.
   SEE ALSO: restore3, rot3, mov3, aim3, light3
 */
{
  return _cpy(_draw3_list, _draw3_n);
}

func restore3(view)
/* DOCUMENT restore3, view
     Restore a previously saved 3D viewing transformation and lighting.
     If VIEW is nil, rotate object to viewer's coordinate system.
   SEE ALSO: restore3, rot3, mov3, aim3, light3
 */
{
  if (!is_void(view)) view= _cpy(view);
  else view= _cat(_cpy(_draw3_view), _cpy(_light3_list));
  _cdr, view, _draw3_n, _cdr(_draw3_list, _draw3_n, []);
  _draw3_list= view;
  _undo3_set, restore3, old;
}

/* set default viewing direction if user hasn't already done so */
if (is_void(_draw3_view)) _draw3_view= _lst(unit(3), [0.,0.,0.], []);
_draw3_nv= _len(_draw3_view);

/* ------------------------------------------------------------------------ */

func light3(ambient=,diffuse=,specular=,spower=,sdir=)
/* DOCUMENT light3, ambient=a_level,
                    diffuse=d_level,
                    specular=s_level,
                    spower=n,
                    sdir=xyz
     Sets lighting properties for 3D shading effects.
     A surface will be shaded according to its to its orientation
     relative to the viewing direction.

     The ambient level A_LEVEL is a light level (arbitrary units)
     that is added to every surface independent of its orientation.

     The diffuse level D_LEVEL is a light level which is proportional
     to cos(theta), where theta is the angle between the surface
     normal and the viewing direction, so that surfaces directly
     facing the viewer are bright, while surfaces viewed edge on are
     unlit (and surfaces facing away, if drawn, are shaded as if they
     faced the viewer).

     The specular level S_LEVEL is a light level proportional to a high
     power spower=N of 1+cos(alpha), where alpha is the angle between
     the specular reflection angle and the viewing direction.  The light
     source for the calculation of alpha lies in the direction XYZ (a
     3 element vector) in the viewer's coordinate system at infinite
     distance.  You can have ns light sources by making S_LEVEL, N, and
     XYZ (or any combination) be vectors of length ns (3-by-ns in the
     case of XYZ).  (See source code for specular_hook function
     definition if powers of 1+cos(alpha) aren't good enough for you.)

     With no arguments, return to the default lighting.

   EXAMPLES:
     light3, diffuse=.1, specular=1., sdir=[0,0,-1]
       (dramatic "tail lighting" effect)
     light3, diffuse=.5, specular=1., sdir=[1,.5,1]
       (classic "over your right shoulder" lighting)
     light3, ambient=.1,diffuse=.1,specular=1.,
             sdir=[[0,0,-1],[1,.5,1]],spower=[4,2]
       (two light sources combining previous effects)

   SEE ALSO: rot3, save3, restore3
 */
{
  old= _cpy(_cdr(_draw3_list,_draw3_nv),5);

  flags= 0;
  if (!is_void(ambient)) {
    if (dimsof(ambient)(1)) error, "ambient light level must be scalar";
    flags|= 1;
    _car, _draw3_list, _draw3_nv+1, double(ambient);
  }
  if (!is_void(diffuse)) {
    if (dimsof(diffuse)(1)) error, "diffuse light level must be scalar";
    flags|= 2;
    _car, _draw3_list, _draw3_nv+2, double(diffuse);
  }

  if (!is_void(specular)) flags|= 4;
  else specular= _car(_draw3_list, _draw3_nv+3);
  if (!is_void(spower)) flags|= 8;
  else spower= _car(_draw3_list, _draw3_nv+4);
  if (!is_void(sdir)) {
    dims= dimsof(sdir);
    if (dims(1)<1 || dims(2)!=3)
      error, "lighting direction must be 3 vector or 3-by-ns array"
    flags|= 16;
  } else {
    sdir= _car(_draw3_list, _draw3_nv+5);
  }
  if (flags&28) {
    if (is_void(dimsof(specular,spower,sdir(1,..))))
      error, "specular, spower, and sdir not conformable";
    if (flags&4) _car, _draw3_list, _draw3_nv+3, double(specular);
    if (flags&8) _car, _draw3_list, _draw3_nv+4, spower;
    if (flags&16) _car, _draw3_list, _draw3_nv+5, double(sdir);
  }

  if (!flags) {
    for (i=1 ; i<=5 ; ++i)
      _car, _draw3_list, _draw3_nv+i, _car(_light3_list,i);
  }

  _undo3_set, _light3, old;
}

func _light3(arg)
{
  for (i=1 ; i<=5 ; ++i)
    _car, _draw3_list, _draw3_nv+i, _car(arg,i);
}

/* set default values if user hasn't already done so */
if (is_void(_light3_ambient)) _light3_ambient= 0.2;
if (is_void(_light3_diffuse)) _light3_diffuse= 1.0;
if (is_void(_light3_specular)) _light3_specular= 0.0;
if (is_void(_light3_spower)) _light3_spower= 2;
if (is_void(_light3_sdir)) _light3_sdir= [1.0, 0.5, 1.0]/sqrt(2.25);
_light3_list= _lst(_light3_ambient,_light3_diffuse,_light3_specular,
                   _light3_spower,_light3_sdir);

func get3_light(xyz, nxyz)
/* DOCUMENT get3_light(xyz, nxyz)
         or get3_light(xyz)

     return 3D lighting for polygons with vertices XYZ.  If NXYZ is
     specified, XYZ should be 3-by-sum(nxyz), with NXYZ being the
     list of numbers of vertices for each polygon (as for the plfp
     function).  If NXYZ is not specified, XYZ should be a quadrilateral
     mesh, 3-by-ni-by-nj (as for the plf function).  In the first case,
     the return value is numberof(NXYZ); in the second case, the
     return value is (ni-1)-by-(nj-1).

     The parameters of the lighting calculation are set by the
     light3 function.

   SEE ALSO: light3, set3_object, get3_normal, get3_centroid
 */
{
  list= _cdr(_draw3_list, _draw3_nv);
  ambient= _nxt(list);
  diffuse= _nxt(list);
  specular= _nxt(list);
  spower= _nxt(list);
  sdir= _nxt(list);

  /* get normal */
  normal= get3_normal(xyz, nxyz);

  /* get direction to viewer's eye (camera) */
  zc= _getzc3();
  if (is_void(zc)) {
    view= [0.,0.,1.];
  } else {
    view= [0.,0.,zc]-get3_centroid(xyz, nxyz);
    m1= abs(view(1,..),view(2,..),view(3,..))(-,..);
    m1= m1 + (m1==0.0);
    view/= m1;
  }

  /* do lighting calculation */
  nv= (normal*view)(sum,..);
  light= ambient + diffuse*abs(nv);
  if (anyof(specular)) {
    sdir= sdir(,*);
    sdir/= abs(sdir(1,),sdir(2,),sdir(3,))(-,);
    sv= sdir(+,*)*view(+,..);
    sn= sdir(+,*)*normal(+,..);
    m1= max(sn*nv(-,) - 0.5*sv + 0.5, 1.e-30);  /* max(1+cos(alpha),0) */
    if (is_func(specular_hook))
      m1= specular_hook(m1, abs(nv)(-,..), spower);
    else
      m1= m1^spower;
    light+= (specular(*)*m1)(sum,..);
  }

  return light;
}

func get3_normal(xyz, nxyz)
/* DOCUMENT get3_normal(xyz, nxyz)
         or get3_normal(xyz)

     return 3D normals for polygons with vertices XYZ.  If NXYZ is
     specified, XYZ should be 3-by-sum(nxyz), with NXYZ being the
     list of numbers of vertices for each polygon (as for the plfp
     function).  If NXYZ is not specified, XYZ should be a quadrilateral
     mesh, 3-by-ni-by-nj (as for the plf function).  In the first case,
     the return value is 3-by-numberof(NXYZ); in the second case, the
     return value is 3-by-(ni-1)-by-(nj-1).

     The normals are constructed from the cross product of the lines
     joining the midpoints of two edges which as nearly quarter the
     polygon as possible (the medians for a quadrilateral).  No check
     is made that these not be parallel; the returned "normal" is
     [0,0,0] in that case.  Also, if the polygon vertices are not
     coplanar, the "normal" has no precisely definable meaning.

   SEE ALSO: get3_centroid, get3_light
 */
{
  if (is_void(nxyz)) {
    /* if no polygon list is given, assume xyz is 2D mesh */
    /* form normal as cross product of medians */
    m1= xyz(,zcen,dif);
    m2= xyz(,dif,zcen);

  } else {
    /* with polygon list, more elaborate calculation required */
    frst= nxyz(psum)-nxyz+1;

    /* form normal by getting two approximate diameters
     * (reduces to above medians for quads) */
    n2= (nxyz+1)/2;
    zero= frst-1;
    c0= 0.5*(xyz(,zero+1)+xyz(,zero+2));
    i= zero+n2;  /* n2>=2, nxyz>=3 */
    c1= 0.5*(xyz(,i)+xyz(,i+1));
    i= 1+n2/2;
    c2= 0.5*(xyz(,zero+i)+xyz(,zero+i+1));
    i= min(i+n2, nxyz);
    c3= 0.5*(xyz(,zero+i)+xyz(,zero+i%nxyz+1));
    m1= c1 - c0;
    m2= c3 - c2;
  }

  /* poly normal is cross product of two medians (or diameters) */
  normal= m1;
  normal(1,..)= n1= m1(2,..)*m2(3,..) - m1(3,..)*m2(2,..);
  normal(2,..)= n2= m1(3,..)*m2(1,..) - m1(1,..)*m2(3,..);
  normal(3,..)= n3= m1(1,..)*m2(2,..) - m1(2,..)*m2(1,..);
  m1= abs(n1,n2,n3)(-,..);
  m1= m1 + (m1==0.0);
  normal/= m1;

  return normal;
}

func get3_centroid(xyz, nxyz)
/* DOCUMENT get3_centroid(xyz, nxyz)
         or get3_centroid(xyz)

     return 3D centroids for polygons with vertices XYZ.  If NXYZ is
     specified, XYZ should be 3-by-sum(nxyz), with NXYZ being the
     list of numbers of vertices for each polygon (as for the plfp
     function).  If NXYZ is not specified, XYZ should be a quadrilateral
     mesh, 3-by-ni-by-nj (as for the plf function).  In the first case,
     the return value is 3-by-numberof(NXYZ); in the second case, the
     return value is 3-by-(ni-1)-by-(nj-1).

     The centroids are constructed as the mean value of all vertices
     of each polygon.

   SEE ALSO: get3_normal, get3_light
 */
{
  if (is_void(nxyz)) {
    /* if no polygon list is given, assume xyz is 2D mesh */
    centroid= xyz(,zcen,zcen);

  } else {
    /* with polygon list, more elaborate calculation required */
    last= nxyz(psum);
    list= histogram(1+last)(1:-1);
    list(1)+= 1;
    list= list(psum);
    centroid= array(0.0, 3, numberof(nxyz));
    centroid(1,)= histogram(list, xyz(1,));
    centroid(2,)= histogram(list, xyz(2,));
    centroid(3,)= histogram(list, xyz(3,));
    centroid/= double(nxyz);
  }

  return centroid;
}

/* ------------------------------------------------------------------------ */

func get3_xy(xyz, &x, &y, &z, getz)
/* DOCUMENT get3_xy, xyz, x, y
         or get3_xy, xyz, x, y, z, 1

     Given 3-by-anything coordinates XYZ, return X and Y in viewer's
     coordinate system (set by rot3, mov3, orient3, etc.).  If the
     fifth argument is present and non-zero, also return Z (for use
     in sort3d or get3_light, for example).  If the camera position
     has been set to a finite distance with setz3, the returned
     coordinates will be tangents of angles for a perspective
     drawing (and Z will be scaled by 1/zc).

   SEE ALSO: sort3d, get3_light, rot3, setz3, set3_object
 */
{
  /* rotate and translate to viewer's coordinate system */
  xyz= _getrot3()(,+)*(_getscl3()*xyz - _getorg3())(+,..);
  x= xyz(1,..);
  y= xyz(2,..);

  /* do optional perspective projection */
  zc= _getzc3();
  if (!is_void(zc)) {
    z= xyz(3,..);
    zc= max(zc-z, 0.0);  /* protect behind camera */
    zc+= (zc==0.0)*1.e-35;       /* avoid zero divide */
    x/= zc;
    y/= zc;
    if (getz) z/= zc;
  } else if (getz) {
    z= xyz(3,..);
  }
}

func _getrot3(void)
{
  return _car(_draw3_list, 1);
}
func _getorg3(void)
{
  local limits, org3;
  eq_nocopy, limits, _car(_draw3_list, _draw3_nll+1);
  eq_nocopy, org3, _car(_draw3_list, 2);
  if (is_void(limits)) return org3;
  return org3 + limits(avg:1:2,)*limits(3,)/limits(ptp:1:2,);
}
func _getzc3(void)
{
  return _car(_draw3_list, 3);
}
func _getscl3(void)
{
  local limits;
  eq_nocopy, limits, _car(_draw3_list, _draw3_nll+1);
  if (is_void(limits)) return [1.,1.,1.];
  return limits(3,)/limits(ptp:1:2,);
}

func _setrot3(x)
{
  _undo3_set, _setrot3, _car(_draw3_list, 1, x);
}
func _setorg3(x)
{
  _undo3_set, _setorg3, _car(_draw3_list, 2, x);
}
func _setzc3(x)
{
  _undo3_set, _setzc3, _car(_draw3_list, 3, x);
}
func _setscl3(x)
{  /* not undoable, cleared by clear3 */
  _car, _draw3_list, _draw3_nll+1, x;
  draw3_trigger;
}

func _undo3_set(fnc, arg)
{
  if (!_in_undo3) {
    if (_len(_undo3_list)>=2*_undo3_limit)
      _cdr, _undo3_list, 2*_undo3_limit-2, [];
    _undo3_list= _cat(_lst(fnc,arg), _undo3_list);
  }
  draw3_trigger;
}

_undo3_limit= 100;

func undo3(n)
/* DOCUMENT undo3
         or undo3, n
     Undo the effects of the last N (default 1) rot3, orient3, mov3, aim3,
     setz3, or light3 commands.
 */
{
  if (is_void(n)) n= 1;
  n= 2*(n-1);
  if (n<0 || n>_len(_undo3_list))
    error, "not that many items in undo list";
  _in_undo3= 1;  /* flag to skip _undo3_set */
  /* perhaps should save discarded items in a redo list? */
  if (n) _undo3_list= _cdr(_undo3_list, n);
  for (; n>=0 ; n-=2) {
    fnc= _nxt(_undo3_list);
    arg= _nxt(_undo3_list);
    fnc, arg;
  }
  draw3_trigger;
}

func set3_object(fnc, arg)
/* DOCUMENT set3_object, drawing_function, _lst(arg1,arg2,...)

     set up to trigger a call to draw3, adding a call to the
     3D display list of the form:

        DRAWING_FUNCTION, _lst(ARG1, ARG2, ...)

     When draw3 calls DRAWING_FUNCTION, the external variable _draw3
     will be non-zero, so DRAWING_FUNCTION can be written like this:

     func drawing_function(arg1,arg2,...)
     {
       require, "pl3d.i";
       if (_draw3) {
         list= arg1;
         arg1= _nxt(list);
         arg2= _nxt(list);
         ...
         ...<calls to get3_xy, sort3d, get3_light, etc.>...
         ...<calls to graphics functions plfp, plf, etc.>...
         return;
       }
       ...<verify args>...
       ...<do orientation and lighting independent calcs>...
       set3_object, drawing_function, _lst(arg1,arg2,...);
     }

  SEE ALSO: get3_xy, get3_light, sort3d
 */
{
  _draw3_list= _cat(_draw3_list, _lst(fnc,arg));
  draw3_trigger;
}

/* ------------------------------------------------------------------------ */

func draw3(called_as_idler)
/* DOCUMENT draw3
     Draw the current 3D display list.
     (Ordinarily triggered automatically when the drawing changes.)
 */
{
  if (_draw3_changes) {
    if (called_as_idler) fma;
    /* the first _draw3_n elements of _draw3_list are the viewing
     * transforms, lighting, etc.
     * thereafter, elements are (function,argument-list) pairs
     * the _draw3 flag alerts the functions that these are the draw
     * calls rather than the interactive setup calls */
    limits, square=1;
    if (_cage3) {
      local lims;
      eq_nocopy, lims, _car(_draw3_list, _draw3_nll+1);
      if (!is_void(lims)) _3cage, transpose(lims(1:2,));
    }
    _draw3= 1;
    for (list=_cdr(_draw3_list, _draw3_n) ; list ; list=_cdr(list)) {
      fnc= _car(list);
      list= _cdr(list);
      fnc, _car(list);
    }
    if (_gnomon) _gnomon_draw;
    _draw3_changes= [];
  }
}

_draw3_nll= _draw3_nv+_len(_light3_list);
_draw3_list= _cat(_cpy(_draw3_view), _cpy(_light3_list), _lst([]));
_draw3_n= _len(_draw3_list);

func draw3_trigger
{
  /* arrange to call draw3 when everything else is finished */
  set_idler, _draw3_idler;
  extern _draw3_changes;
  _draw3_changes= 1;
}

func _draw3_idler
{
  draw3, 1;
}

func clear3(void)
/* DOCUMENT clear3
     Clear the current 3D display list.
 */
{
  _cdr, _draw3_list, _draw3_n, [];
  _car, _draw3_list, _draw3_nll+1, [];  /* clear limits */
}

func window3(n)
/* DOCUMENT window3
         or window3, n
     initialize style="nobox.gs" window for 3D graphics
 */
{
  window, n, wait=1, style="nobox.gs", legends=0;
}

/* ------------------------------------------------------------------------ */

func gnomon(on)
/* DOCUMENT gnomon
         or gnomon, onoff
     Toggle the gnomon display.  If ONOFF is non-nil and non-zero,
     turn on the gnomon.  If ONOFF is zero, turn off the gnomon.

     The gnomon shows the X, Y, and Z axis directions in the
     object coordinate system.  The directions are labeled.
     The gnomon is always infinitely far behind the object
     (away from the camera).

     There is a mirror-through-the-screen-plane ambiguity in the
     display which is resolved in two ways: (1) The (X,Y,Z)
     coordinate system is right-handed, and (2) If the tip of an
     axis projects into the screen, it's label is drawn in opposite
     polarity to the other text on the screen.
 */
{
  old= _gnomon;
  if (is_void(on)) _gnomon~= 1;
  else if (on) _gnomon= 1;
  else _gnomon= 0;
  if (old!=_gnomon) draw3_trigger;
}

if (is_void(_gnomon)) _gnomon= 0;

func _gnomon_draw(void)
{
  o= [0.,0.,0.];
  x1= [1.,0.,0.];
  y1= [0.,1.,0.];
  z1= [0.,0.,1.];
  xyz= _getrot3()(,+)*[[o,x1],[o,y1],[o,z1]](+,,,);
  xyz*= 0.0013*_gnomon_scale;
  x1= xyz(1,,);
  y1= xyz(2,,);
  z1= xyz(3,2,);
  x0= x1(1,);
  x1= x1(2,);
  y0= y1(1,);
  y1= y1(2,);
  wid= min(_gnomon_scale/18.,6.);
  if (wid<0.5) wid= 0.0;
  plsys, 0;
  pldj, x0+_gnomon_x, y0+_gnomon_y, x1+_gnomon_x, y1+_gnomon_y,
    width=wid, type=1, legend=string(0);
  plsys, 1;

  /* compute point size of labels (1/3 of axis length) */
  pts= [8,10,12,14,18,24](digitize(_gnomon_scale/3.0,
                                   [9,11,13,16,21]));
  if (_gnomon_scale < 21.) {
    x1*= 21./_gnomon_scale;
    y1*= 21./_gnomon_scale;
  }
  /* label positions: first find shortest axis */
  xy= abs(x1,y1);
  i= xy(mnx);
  jk= [[2,3],[3,1],[1,2]](,i);
  if (xy(i)<1.e-7*xy(sum)) {  /* guarantee not exactly zero */
    x1(i)= -1.e-6*x1(jk)(sum);
    y1(i)= -1.e-6*y1(jk)(sum);
    xy(i)= abs(x1(i),y1(i));
  }
  xyi= xy(i);
  /* next find axis nearest to shortest */
  j= jk(1);
  k= jk(2);
  if (abs(x1(j)*y1(i)-y1(j)*x1(i))*xy(k) >
      abs(x1(k)*y1(i)-y1(k)*x1(i))*xy(j)) {
    jk= j;  j= k;  k= jk;
  }
  /* furthest axis first - move perpendicular to nearer axis */
  xk= -y1(j);
  yk= x1(j);
  xy= abs(xk,yk);
  xk/= xy;
  yk/= xy;
  if (xk*x1(k)+yk*y1(k) < 0.0) { xk= -xk;  yk= -yk; }
  /* nearer axis next - move perpendicular to furthest axis */
  xj= -y1(k);
  yj= x1(k);
  xy= abs(xj,yj);
  xj/= xy;
  yj/= xy;
  if (xj*x1(j)+yj*y1(j) < 0.0) { xj= -xj;  yj= -yj; }
  /* shortest axis last - move perpendicular to nearer */
  xi= -y1(j);
  yi= x1(j);
  xy= abs(xi,yi);
  xi/= xy;
  yi/= xy;
  if (xi*x1(i)+yi*y1(i) < 0.0) { xi= -xi;  yi= -yi; }

  /* shortest axis label may need adjustment */
  d= 0.0013*pts;
  if (xyi < d) {
    /* just center it in correct quadrant */
    jk= sign(xi*xj+yi*yj);
    yi= sign(xi*xk+yi*yk);
    xi= jk*xj + yi*xk;
    yi= jk*yj + yi*yk;
    jk= abs(xi, yi);
    xi/= jk;
    yi/= jk;
  }

  x= y= [0.,0.,0.];
  x([i,j,k])= [xi,xj,xk];
  y([i,j,k])= [yi,yj,yk];
  x*= d;
  y*= d;
  x+= x1 + _gnomon_x;
  y+= y1 + _gnomon_y;
  chr= ["X","Y","Z"];
  _gnomon_text, chr(i), x(i),y(i), pts, z1(i)<-1.e-6;
  _gnomon_text, chr(j), x(j),y(j), pts, z1(j)<-1.e-6;
  _gnomon_text, chr(k), x(k),y(k), pts, z1(k)<-1.e-6;
}

/* recommended _gnomon_scale: 24, 30, 36, 42, 54, or 72 */
if (is_void(_gnomon_scale))
  _gnomon_scale= 30.   /* X,Y,Z axis lengths in points */
if (is_void(_gnomon_x))
  _gnomon_x= 0.18;     /* gnomon origin in system 0 coordinates */
if (is_void(_gnomon_y))
  _gnomon_y= 0.42;

func _gnomon_text(chr, x, y, pts, invert)
{
  /* pts= 8, 10, 12, 14, 18, or 24 */
  col= "fg";
  if (invert) {
    plsys, 0;
    plg,[y,y],[x,x],
      type=1,width=2.2*pts,color=col,marks=0,legend=string(0);
    plsys, 1;
    col= "bg";
  }
  plt, chr, x,y,
    justify="CH",color=col,height=pts,font="helvetica",opaque=0;
}

/* ------------------------------------------------------------------------ */

func sort3d(z, npolys, &list, &vlist)
/* DOCUMENT sort3d(z, npolys, &list, &vlist)

     given Z and NPOLYS, with numberof(Z)==sum(npolys), return
     LIST and VLIST such that Z(VLIST) and NPOLYS(LIST) are
     sorted from smallest average Z to largest average Z, where
     the averages are taken over the clusters of length NPOLYS.
     Within each cluster (polygon), the cyclic order of Z(VLIST)
     remains unchanged, but the absolute order may change.

     This sorting order produces correct or nearly correct order
     for a plfp command to make a plot involving hidden or partially
     hidden surfaces in three dimensions.  It works best when the
     polys form a set of disjoint closed, convex surfaces, and when
     the surface normal changes only very little between neighboring
     polys.  (If the latter condition holds, then even if sort3d
     mis-orders two neighboring polys, their colors will be very
     nearly the same, and the mistake won't be noticeable.)  A truly
     correct 3D sorting routine is impossible, since there may be no
     rendering order which produces correct surface hiding (some polys
     may need to be split into pieces in order to do that).  There
     are more nearly correct algorithms than this, but they are much
     slower.

   SEE ALSO: get3_xy
 */
{
  /* first compute z, the z-centroid of every poly
   * get a list the same length as x, y, or z which is 1 for each
   * vertex of poly 1, 2 for each vertex of poly2, etc.
   * the goal is to make nlist with histogram(nlist)==npolys */
  nlist= histogram(1+npolys(psum))(1:-1);
  nlist(1)+= 1;  /* another problem with 1-origin indexing */
  nlist= nlist(psum);
  /* now sum the vertex values and divide by the number of vertices */
  z= histogram(nlist, double(z))/npolys;

  /* sort the polygons from smallest z to largest z
   * npolys(list) is the sorted list of lengths */
  list= sort(z);

  /* next, find the list which sorts the polygon vertices
   * first, find a list vlist such that sort(vlist) is above list */
  vlist= list;
  vlist(list)= indgen(numberof(list));
  /* then reset the nlist values to that pre-sorted order, so that
   * sort(nlist) will be the required vertex sorting list */
  nlist= vlist(nlist);
  /* the final hitch is to ensure that the vertices within each polygon
   * remain in their initial order (sort scrambles equal values)
   * since the vertices of a polygon can be cyclically permuted,
   * it suffices to add a sawtooth function to a scaled nlist to
   * produce a list in which each cluster of equal values will retain
   * the same cyclic order after the sort
   * (note that the more complicated msort routine would leave the
   *  clusters without even a cyclic permutation, if that were
   *  necessary) */
  nmax= max(npolys);  /* this must never be so large that
                       * numberof(npolys)*nmax > 2e9  */
  vlist= sort(nmax*nlist + indgen(numberof(nlist))%nmax);
  /* primary sort key ^            secondary key  ^  */
}

/* ------------------------------------------------------------------------ */

func spin3(nframes, axis, tlimit=, dtmin=, bracket_time=)
/* DOCUMENT spin3
         or spin3, nframes
         or spin3, nframes, axis
     Spin the current 3D display list about AXIS over NFRAMES.  Keywords
     tlimit= the total time allowed for the movie in seconds (default 60),
     dtmin= the minimum allowed interframe time in seconds (default 0.0),
     bracket_time= (as for movie function in movie.i)

     The default AXIS is [-1,1,0] and the default NFRAMES is 30.

   SEE ALSO: rot3
 */
{
  require, "movie.i";
  if (is_void(nframes)) nframes= 30;
  dtheta= 2*pi/(nframes-1);
  if (is_void(axis)) axis= [-1.,1.,0.];
  theta= acos(axis(3)/abs(axis(1),axis(2),axis(3)));
  phi= atan(axis(2),axis(1)+(!axis(1)&&!axis(2)));
  orig= save3();
  movie, _spin3, tlimit, dtmin, bracket_time;
  restore3, orig;
}

func _spin3(i)
{
  if (i>=nframes) return 0;
  rot3,,,-phi
  rot3,,-theta,dtheta;
  rot3,,theta,phi;
  draw3;
  return 1;
}

/* ------------------------------------------------------------------------ */

func cage3(on)
/* DOCUMENT cage3
         or cage3, onoff
     Toggle the cage display.  If ONOFF is non-nil and non-zero,
     turn on the cage.  If ONOFF is zero, turn off the cage.

     The cage draws a rectangular box "behind" the 3D object and
     attempts to put ticks and labels around the edge of the box.

   SEE ALSO: limit3, plwf (include plwf.i)
 */
{
  old= _cage3;
  if (is_void(on)) _cage3~= 1;
  else if (on) _cage3= 1;
  else _cage3= 0;
  if (old!=_cage3) draw3_trigger;
}

if (is_void(_cage3)) _cage3= 0;

/*
   Idea:

   Draw hexagon, representing the free edges of the three "backplanes"
   of a cube surrounding the 3D object being plotted.  Ticks should
   be perpendicular to the backplane associated with the edge.  Try
   to put numeric labels on the "leftmost" and "bottommost" edges,
   while "X", "Y", and "Z" markers can go on the"topmost" and
   "rightmost" edges.  In the pl3d.i interface, this "ticked box"
   should optionally replace the gnomon, since there is no reason
   to have both.  Should also draw (optionally?) the common edges of
   the three backplanes.
 */

func _3cage(xyzlim)
{
  /* make 3x2x2x2 = (vector,dx,dy,dz) coordinates of the box,
     starting from 3x2 (vector,min_max) limits values */
  dxyz= xyzlim(,2)-xyzlim(,1);
  xyz= xyzlim(,1,-:1:2,-:1:2,-:1:2);
  xyz(1,2,,)+= dxyz(1);
  xyz(2,,2,)+= dxyz(2);
  xyz(3,,,2)+= dxyz(3);

  /* transform to three 2x2x2 arrays (x,y,z) in camera coord system */
  local x, y, z;
  get3_xy, xyz, x, y, z, 1;

  /* find the three orthogonal backplanes
     owing to perspective, there may be up to five back facing planes
     of the box -- choose the most rearward, or the smaller object
     coordinate value (xn,yn,zn) in case of ties */
  i= z(,avg,avg)(mnx);
  j= z(avg,,avg)(mnx);
  k= z(avg,avg,)(mnx);
  /* now the common point of the backplanes is (i,j,k), the three common
     edges connect to the points (3-i,j,k), (i,3-j,k), and (i,j,3-k),
     and the three non-common points of the backplanes are
     (3-i,j,3-k), (3-i,3-j,k), and (i,3-j,3-k) */

  /* it is best to draw these as polylines, since polylines have
     round endcaps, while disjoint lines have square caps
     -- if the common edges are omitted, there is just a single
        closed hexagon.  otherwise, we draw two open polylines,
        one starting at the common point then traversing the
        hexagon (8 points), the other consisting of the two
        remaining common edges (3 points) */
  ip= 3-i;
  j= 2*(j-1);
  jp= 2-j;
  k= 4*(k-1);
  kp= 4-k;
  if (omit3_common)
    list= [ip+j+k, ip+jp+k, i+jp+k, i+jp+kp, i+j+kp, ip+j+kp];
  else
    list= [i+j+k, ip+j+k, ip+jp+k, i+jp+k, i+jp+kp, i+j+kp, ip+j+kp, ip+j+k];
  x1= x(list);
  y1= y(list);
  plg, y1, x1, closed=omit3_common, marks=0, legend="",
       color=_3kcolor, type=_3ktype, width=_3kwidth;
  xmin= min(x1);
  xmax= max(x1);
  ymin= min(y1);
  ymax= max(y1);

  if (!omit3_common) {
    list= [i+jp+k, i+j+k, i+j+kp];
    plg, y(list), x(list), closed=0, marks=0, legend="",
         color=_3kcolor, type=_3ktype, width=_3kwidth;
  }

  nlabel= 0;
  if (!omit3_ticks) {
    /* innermost index is [back,front] point,
       next index is [first, second] edge,
       third index is [x,y,z] */
    list= [[[i+j+kp,ip+j+kp],[i+jp+k,ip+jp+k]],
           [[i+j+kp,i+jp+kp],[ip+j+k,ip+jp+k]],
           [[ip+j+k,ip+j+kp],[i+jp+k,i+jp+kp]]];
    /* reorder first index so that first point is minimum */
    n= list(2,1,)<list(1,1,);
    for (m=1 ; m<=3 ; ++m) if (n(m)) list(,,m)= list(2:1:-1,,m);
    /* reorder second index so that first edge is the one which
       will get any labels -- leftmost or lower */
    x1= x(list);
    y1= y(list);
    horiz= (abs(y1(ptp,,))>abs(x1(ptp,,)));
    x2= x1(avg,,);
    y2= y1(avg,,);
    dir= sign(x2-x2(2:1:-1,))*horiz + sign(y2-y2(2:1:-1,))*(!horiz);
    for (m=1 ; m<=3 ; ++m) {
      if (dir(1,m)<0.) {
        if (dir(2,m)>0. || horiz(2,m)) continue;
      } else {
        if (dir(2,m)>0. && horiz(1,m)) continue;
      }
      list(,,m)= list(,2:1:-1,m);
      horiz(,m)= horiz(2:1:-1,m);
      dir(,m)= dir(2:1:-1,m);
    }
    xyz0= xyz(,i+j+k); /* back corner */
    xyz= xyz(,list);   /* 3x2x2x3 edge coordinates, with last
                          three indices as above */

    /* compute projected lengths of the labeled edges */
    n= list(,1,);
    len= abs(x(n)(ptp,),y(n)(ptp,),z(n)(ptp,));
    mxlen= max(len);
    len/= mxlen;       /* ...relative to longest edge */

    /* identify the two shared corners of the labeled edges --
       list(,1,) is 6 numbers, 2 are unique and 2 others shared;
       both endpoints of one of the three edges are shared */
    ms1= (n(*)==n(-,,))(sum,,)-1; /* 0 for unique, 1 for shared pt */
    ms1= where(ms1);      /* four indices, three possible pairings */
    if (n(ms1(1))==n(ms1(2))) {
      ms2= [ms1(3),ms1(4)];
      ms1= [ms1(1),ms1(2)];
    } else if (n(ms1(1))==n(ms1(3))) {
      ms2= [ms1(2),ms1(4)];
      ms1= [ms1(1),ms1(3)];
    } else {
      ms2= [ms1(2),ms1(3)];
      ms1= [ms1(1),ms1(4)];
    }

    /* First idea was to make the ticks lie along the perpendicular
       (in 3D) to the backplane of the associated edge.  This looks
       slick for orientations in which the hexagon is near regular,
       but very ugly when some angles project to near 0 or 180
       degrees.

       The second idea is more robust, but not quite as nice at its
       best: ticks are always either horizontal or vertical (after
       projection), with the choice made according to whether the
       edge is more nearly vertical or horizontal (respectively).
     */

    local xmajor, xminor, xlabel;
    unit= array(0.,3,3);
    unit(1:9:4,1)= 1.;
    llen= [0,0,0];
    xptr= yptr= lptr= array(pointer, 3);
    xylabs= array(0.,2,2,3);
    for (m=1 ; m<=3 ; ++m) {
      /* longest labeled edge gets specified maximum number of
         major ticks -- other two edges get scaled number */
      if (_3ticks(xyzlim(m,1), xyzlim(m,2), _3nmajor*len(m)))
        continue;        /* (no ticks on very short axes) */

      /* expand coordinate lists to vector lists */
      mask= unit(,m);
      xmajor= mask*xmajor(-,);
      xminor= mask*xminor(-,);
      mask= 1.-mask;

      /* do upper or right edge, then lower or left edge */
      for (n=2 ; n>=1 ; --n) {
        xyz0= xyz(,1,n,m)*mask;

        /* first do minor ticks */
        tend= xyz0 + xminor;
        get3_xy, tend, x1, y1;
        if (horiz(n,m)) tdir= [mxlen*dir(n,m),0.];
        else            tdir= [0.,mxlen*dir(n,m)];
        x2= x1 + tdir(1)*_3lminor;
        y2= y1 + tdir(2)*_3lminor;
        pldj, x1,y1,x2,y2, legend="",
          type=_3ktype,color=_3kcolor,width=_3kwidth;
        xmin= min(min(x2),xmin);
        xmax= max(max(x2),xmax);
        ymin= min(min(y2),ymin);
        ymax= max(max(y2),ymax);

        /* then do major ticks */
        tend= xyz0 + xmajor;
        get3_xy, tend, x1, y1;
        x2= x1 + tdir(1)*_3lmajor;
        y2= y1 + tdir(2)*_3lmajor;
        pldj, x1,y1,x2,y2, legend="",
          type=_3ktype,color=_3kcolor,width=_3kwidth;
        xmin= min(min(x2),xmin);
        xmax= max(max(x2),xmax);
        ymin= min(min(y2),ymin);
        ymax= max(max(y2),ymax);
      }

      /* save the tips of the major ticks for the labeled edge */
      xylabs(,,m)= [[x2(1),y2(1)],[x2(0),y2(0)]];
      xptr(m)= &x2;
      yptr(m)= &y2;
      lptr(m)= &xlabel;
      llen(m)= max(strlen(xlabel));
      nlabel+= numberof(xlabel);
    }

    if (!omit_labels && nlabel) {
      /* estimate horizontal and vertical size of labels in
         world coordinates -- obviously this makes assumptions about
         the size of the viewport and won't work if the cage is zoomed
         to a very different size
         the default numbers assume nobox.gs style (6 inch viewport) */
      scale= 6. /*inches/viewport*/ * 72.27 /*points/inch*/ /
        (2.2*mxlen) /*units/viewport*/;     /* net: points/unit */
      scale*= _3xfudge;         /* get serious */
      tvert= _3xheight/scale;   /* text size in world coordinates */
      thoriz= 0.6*tvert*llen;   /* assume text shape about 9x15 */

      /* locate and discard any labels
         which interfere near the shared corners */
      hflags= horiz(1,);
      _3interference, ms1;
      _3interference, ms2;

      dir= dir(1,);
      for (m=1 ; m<=3 ; ++m) {
        horiz= hflags(m);
        tdir= dir(m);
        offset= _3xoffset*mxlen*tdir;
        eq_nocopy, x2, *xptr(m);
        eq_nocopy, y2, *yptr(m);
        eq_nocopy, xlabel, *lptr(m);
        if (numberof(xlabel)) {
          if (horiz) x2+= offset;
          else       y2+= offset;   /* add term in thoriz(m)? */
          if (horiz) justify= (tdir<0.)? 15 : 13;
          else       justify= (tdir<0.)? 10 : 18;
          for (n=1 ; n<=numberof(xlabel) ; ++n) {
            plt, xlabel(n), x2(n),y2(n), tosys=1, justify=justify,
              color=_3xcolor,font=_3xfont,height=_3xheight;
          }
          if (horiz) {
            xmin= min(min(x2-thoriz(m)),xmin);
          } else {
            ymin= min(min(y2-tvert),ymin);
            xmin= min(min(x2-0.5*thoriz(m)),xmin);
            xmax= max(max(x2+0.5*thoriz(m)),xmax);
          }
        }
      }

      /* autoscaling will clip text */
      dx= xmax-xmin;
      dy= ymax-ymin;
      dd= 0.5*(dx-dy);
      if (dd>0.) {
        ymax+= dd;
        ymin-= dd;
      } else {
        xmax-= dd;
        xmin+= dd;
      }
      limits, xmin,xmax,ymin,ymax;
    }
  }
}

/* tick parameters */
_3nmajor= 4.5;     /* 1/(max allowed tick density on longest edge) */
_3lmajor= 0.06;    /* length of major ticks as fraction of longest edge */
_3lminor= 0.03;    /* length of minor ticks as fraction of longest edge */
_3ktype= 1;
_3kwidth= 0.0;
_3kcolor= -2;
/* label parameters */
_3xoffset= 0.02;   /* label offset from tick as fraction of longest edge */
_3xfudge= 1.0;     /* fudge factor for points/(longest edge) calc */
_3xfont= 8;        /* helvetica */
_3xheight= 14.0    /* point size of label text */
_3xcolor= -2;

func _3interference(ms)
{
  ms-= 1;
  m= ms/2 + 1;
  m1= m(1);
  m2= m(2);
  horiz= hflags(m1);

  /* interference only possible if ticks have like orientation */
  if (hflags(m2)!=horiz) return;

  n= 1-(ms%2);  /* 1 for 1st index, 0 for last index */
  n1= n(1);
  n2= n(2);

  local x1, y1, x2, y2;
  if (horiz) {
    eq_nocopy, y1, *yptr(m1);
    eq_nocopy, y2, *yptr(m2);
    if (!is_void(y1) && !is_void(y2))
      interfere= (abs(y1(n1)-y2(n2))<tvert);
  } else {
    eq_nocopy, x1, *xptr(m1);
    eq_nocopy, x2, *xptr(m2);
    if (!is_void(x1) && !is_void(x2))
      interfere= (abs(x1(n1)-x2(n2))<0.5*(thoriz(m1)+thoriz(m2)));
  }

  if (interfere) {
    _3remove, m1, n1;
    _3remove, m2, n2;
  }
}

func _3remove(m, n, a)
{
  if (!is_void(m)) {
    xptr(m)= &_3remove(,n,*xptr(m));
    yptr(m)= &_3remove(,n,*yptr(m));
    lptr(m)= &_3remove(,n,*lptr(m));
  } else if (numberof(a)>1) {
    if (n) return a(2:0);
    else return a(1:-1);
  }
}

func _3ticks(xmin, xmax, n)
{
  extern xmajor, xminor, xlabel;   /* results */
  xmajor= xminor= xlabel= [];

  /* dx is the minimum allowed spacing between ticks */
  if (n<1.0) return 1;
  dx= abs(xmax-xmin)/double(n);
  sdx= sign(xmax-xmin);
  xmin*= sdx;
  xmax*= sdx;

  /* round dx up to the nearest "nice" value */
  pwr= 10.^floor(log10(dx)+0.0001);
  base= digitize(dx/pwr,[2.0001,5.0001]);
  if (base==3) pwr*= 10.;
  dx= [2.,5.,1.](base)*pwr;

  /* find the major tick values */
  xn= ceil(xmin/dx - 0.0001);
  xx= floor(xmax/dx + 0.0001);
  if (xx<=xn) return 1;
  xmajor= (xn+indgen(0:long(xx-xn)))*dx*sdx;
  /* sigh -- don't want to print "-0" */
  xn= where(xmajor==0.);
  if (numberof(xn)) xmajor(xn(1))= 0.0;

  /* find the minor tick values */
  dxn= dx/(base==1? 4. : 5.);
  xn= ceil(xmin/dxn - 0.0001);
  xx= floor(xmax/dxn + 0.0001);
  xminor= (xn+indgen(0:long(xx-xn)))*dxn*sdx;
  /* remove minor ticks which are also major */
  xminor= xminor(where(abs(floor(xminor/dxn+0.01)*dxn -
                           floor(xminor/dx+0.01)*dx) > 0.01*dx));

  /* compute major tick labels */
  xn= abs(xmajor);
  xx= max(xn);
  ipwr= floor(log10(xx)+0.0001);
  xpwr= 10.^ipwr;
  ipwr= long(ipwr);
  if (ipwr>3 || min(xn+(!xn))<0.00099999999) {
    /* use e format -- normalize to 0<= xmajor <10 */
    npwr= xpwr;
    mpwr= ipwr;
    pwr/= npwr;
    ipwr= 0;
    xpwr= 1.0;
  } else {
    /* use f format */
    npwr= 1.0;
    mpwr= 0;
  }
  ndecimals= max(long(floor(log10(xpwr/pwr)+0.0001)) - ipwr, 0);
  format= swrite(format="%%.%ldf",ndecimals);
  xlabel= swrite(format=format, xmajor/npwr);
  /* sigh -- "0" often padded with blanks */
  xlabel= strtok(xlabel)(1,);
  if (mpwr)
    xlabel+= swrite(format="%se%+02ld",(ndecimals?"":"."),mpwr);

  return 0;
}

/* ------------------------------------------------------------------------ */
