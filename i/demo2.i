/*
 * $Id: demo2.i,v 1.1 2005-09-18 22:05:55 dhmunro Exp $
 * Mesh plotting demo
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func demo2(which, time_limit)
/* DOCUMENT demo2
     Exhibit quadrilateral mesh plots in 3 movies of a drumhead.
     The drumhead is initially stationary, but has a bump near one
     edge.  Yorick is solving a 2D wave equation to compute the
     evolution of this bump.

     The first movie is a filled mesh plot with color "proportional"
     to the height of the surface of the drum.  A few well chosen
     contour levels (here 3) add a lot to a filled mesh plot.

     The second movie is a "3D" perspective plot of the height of the
     drumhead.  In this movie, the mesh lines are drawn, which is
     slightly confusing since the cells are not all the same shape.

     The second movie is a "3D" shaded plot of the height of the
     drumhead.  Yorick computes surface shading based on the angle
     of each cell from a light source.

     As you watch this, you might reflect on the two dimensionality
     of your retina.  What Yorick lacks by way of 3D graphics is
     really just fancy hidden surface algorithms; the simple
     painter's algorithm used here and in plwf.i is easy to
     implement.

     There are two optional arguments to demo2: the first is the
     number fo the movie (1, 2, or 3) you want to watch; the second
     is a time limit on the duration of each movie in seconds (default
     is 60 seconds each).
 */
{
  require, "movie.i";

  /* generate a 30-by-30 cell mesh on the [-1,1] square */
  x= span(-1, 1, 31)(,-:1:31);
  y= transpose(x);
  /* map the square mesh into a mesh on the unit circle
     this mesh has more nearly equal area cells than a polar
     coordinate circle */
  scale= max(abs(y),abs(x))/(abs(y,x)+1.e-30);
  /* note that abs(y,x)=sqrt(x^2+y^2) */
  x*= scale;
  y*= scale;

  f= f0= exp(-8.*abs(y+.67,x+.25)^2)*(1.-abs(y,x)^2);
  fdot= 0.0*f(2:-1,2:-1);

  af= abs(f(2:-1,2:-1));
  lf= laplacian(f, y,x);
  xdz= x(dif,zcen);
  xzd= x(zcen,dif);
  ydz= y(dif,zcen);
  yzd= y(zcen,dif);
  dt= 0.375*sqrt(min(abs(xdz*yzd - xzd*ydz)));

  window, 0, wait=1, style="nobox.gs";
  palette, "inferno.gp";
  limits, -1, 1, -1, 1;

  /* roll the filled mesh movie */
  if (which && which!=1) goto persp;
  fc= f(zcen,zcen);
  cmin= cmax= max(abs(fc));
  cmin= -cmin;
  level= cmax/4.;
  movie, display_plf, time_limit;
  write,format="%f frames of filled mesh drumhead completed in %f sec\n", 
        movie_timing(4), movie_timing(3);
  write,format="Rate for filled mesh is %f frames/(CPU sec), %f frames(wall sec)\n",
    movie_timing(4)/(movie_timing(1)-movie_timing(5)+1.0e-4), 
    movie_timing(4)/(movie_timing(3)-movie_timing(5)+1.0e-4);

  /* roll the perspective movie */
  persp: if (which && which!=2) goto shade;
  f= f0;
  limits, -1, 1, -1, 1;
  movie, display_plm, time_limit;
  write,format="%f frames of wireframe surface drumhead completed in %f sec\n", 
        movie_timing(4), movie_timing(3);
  write,format="Rate for filled mesh is %f frames/(CPU sec), %f frames(wall sec)\n",
    movie_timing(4)/(movie_timing(1)-movie_timing(5)+1.0e-4), 
    movie_timing(4)/(movie_timing(3)-movie_timing(5)+1.0e-4);

  /* roll the shaded movie */
  shade: if (which && which!=3) return;
  f= f0;
  limits, -1, 1, -1, 1;
  movie, display_pl3, time_limit;
  write,format="%f frames of filled surface drumhead completed in %f sec\n", 
        movie_timing(4), movie_timing(3);
  write,format="Rate for filled mesh is %f frames/(CPU sec), %f frames(wall sec)\n",
    movie_timing(4)/(movie_timing(1)-movie_timing(5)+1.0e-4), 
    movie_timing(4)/(movie_timing(3)-movie_timing(5)+1.0e-4);

  fma;
  limits;
}

func display_plf(i)
{
  /* display first */
  fc= f(zcen,zcen);
  cmin= cmax= max(abs(fc));
  cmin= -cmin;
  plf, fc, -y, -x, cmin=cmin, cmax=cmax;
  /* the 0 contour level is too noisy without some smoothing... */
  fs= f(zcen,zcen)(pcen,pcen);
  plc, fs, -y, -x, levs=0., marks=0, color="green", type="solid";
  plc, f, -y, -x, levs=level, marks=0, color="black", type="dash";
  plc, f, -y, -x, levs=-level, marks=0, color="green", type="dash";

  /* then take a step forward in time */
  lf= laplacian(f, y,x);
  af= abs(f(2:-1,2:-1));
  fdot+= lf*dt;
  f(2:-1,2:-1)+= fdot*dt;

  return i<200;
}

func display_plm(i)
{
  /* display first */
  pl3d,0, f, y, x;

  /* then take a step forward in time */
  lf= laplacian(f, y,x);
  af= abs(f(2:-1,2:-1));
  fdot+= lf*dt;
  f(2:-1,2:-1)+= fdot*dt;

  return i<200;
}

func display_pl3(i)
{
  /* display first */
  pl3d,1, f, y, x;

  /* then take a step forward in time */
  lf= laplacian(f, y,x);
  af= abs(f(2:-1,2:-1));
  fdot+= lf*dt;
  f(2:-1,2:-1)+= fdot*dt;

  return i<200;
}

func laplacian(f, y,x)
{
  /* There are many ways to form the Laplacian as a finite difference.
     This one is nice in Yorick.  */
  /* Start with the two median vectors across each zone.  */
  fdz= f(dif,zcen);
  fzd= f(zcen,dif);
  xdz= x(dif,zcen);
  xzd= x(zcen,dif);
  ydz= y(dif,zcen);
  yzd= y(zcen,dif);

  /* Estimate the gradient at the center of each cell.  */
  area= xdz*yzd - xzd*ydz;
  gradfx= (fdz*yzd - fzd*ydz)/area;
  gradfy= (xdz*fzd - xzd*fdz)/area;

  /* Now consider the mesh formed by the center points of the original.  */
  x= x(zcen,zcen);
  y= y(zcen,zcen);
  xdz= x(dif,);
  xzd= x(,dif);
  ydz= y(dif,);
  yzd= y(,dif);
  area= xdz(,zcen)*yzd(zcen,) - xzd(zcen,)*ydz(,zcen);

  return ((xdz*gradfy(zcen,)-ydz*gradfx(zcen,))(,dif) +
          (yzd*gradfx(,zcen)-xzd*gradfy(,zcen))(dif,)) / area;
}

func pl3d(shading, z, y, x)
{
  /* rotate so that (zp,yp) are screen (y,x) */
  /* These orientations are cunningly chosen so that the painter's
     algorithm correctly draws hidden surfaces first -- see help, plf
     for a description of the order cells are drawn by plf.  */
  theta= 30. * pi/180.;  /* angle of viewer above drumhead */
  phi= 120. * pi/180;

  ct= cos(phi);
  st= sin(phi);
  yp= y*ct - x*st;
  xp= x*ct + y*st;

  ct= cos(theta);
  st= sin(theta);
  zp= z*ct - xp*st;
  xp= xp*ct + z*st;

  if (!shading) {
    color= [];
    edges= 1;
  } else {
    /* compute the two median vectors for each cell */
    m0x= xp(dif,zcen);
    m0y= yp(dif,zcen);
    m0z= zp(dif,zcen);
    m1x= xp(zcen,dif);
    m1y= yp(zcen,dif);
    m1z= zp(zcen,dif);
    /* define the normal vector to be their cross product */
    nx= m0y*m1z - m0z*m1y;
    ny= m0z*m1x - m0x*m1z;
    nz= m0x*m1y - m0y*m1x;
    n= abs(nx, ny, nz);
    nx/= n;
    ny/= n;
    nz/= n;
    color= bytscl(nx, cmin=0.0, cmax=1.0);
    edges= 0;
  }

  plf, color, zp, yp, edges=edges;
}
