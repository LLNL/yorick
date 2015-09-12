/*
 * $Id: demo5.i,v 1.1 2005-09-18 22:05:55 dhmunro Exp $
 * Demonstration of 3D plots with pl3d.i, plwf.i, slice3.i
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

require, "plwf.i"
require, "slice3.i"

require, "movie.i";

window3;
palette, "gray.gp";

func demo5(itest)
/* DOCUMENT demo5
         or demo5, i
     Run examples of use of pl3d.i, plwf.i, and slice3.i.  With
     argument I = 1, 2, or 3, run that particular demonstration.
     Read the source code to understand the details of how the
     various effects are obtained.

     demo5,1  demonstrates the various effects which can be obtained
     with the plwf (plot wire frame) function.
     demo5,2  demonstrates shading effects controlled by the light3
     function
     demo5,3  demonstrates the slice3, slice2, and pl3tree functions,
     as well as changing the orientation of the 3D object

  SEE ALSO: plwf (plwf.i), light3, rot3, orient3, spin3 (pl3d.i),
            mesh3, slice3, slice2, pl3tree, pl3surf (slice3.i)
 */
{
  if (!itest || itest==1) {
    x= span(-1,1,64)(,-:1:64);
    y= transpose(x);
    z= (x+y)*exp(-6*(x^2+y^2));
    write, "(plot wire frame) plwf,z,y,x";
    orient3;
    light3;
    cage3, 0;
    plwf, z, y, x;
    draw3,1;  /* not necessary interactively */
    rdline,prompt="hit RET or Enter to continue";
    write, "plwf,z,y,x, shade=1,ecolor=\"red\"";
    plwf, z, y, x, shade=1,ecolor="red";
    draw3,1;  /* not necessary interactively */
    rdline,prompt="hit RET or Enter to continue";
    write, "cage3,1; plwf,z,y,x, shade=1,ecolor=\"red\"";
    cage3,1; plwf, z, y, x, shade=1,ecolor="red";
    draw3,1;  /* not necessary interactively */
    rdline,prompt="hit RET or Enter to continue";
    limits;
    write, "cage3,0; plwf,z,y,x, shade=1,edges=0";
    cage3,0; plwf, z, y, x, shade=1,edges=0;
    draw3,1;  /* not necessary interactively */
    rdline,prompt="hit RET or Enter to continue";
  }

  if (!itest || itest==2) {
    x= span(-1,1,64)(,-:1:64);
    y= transpose(x);
    z= (x+y)*exp(-6*(x^2+y^2));
    write, "light3 function demo- default lighting";
    orient3;
    light3;
    plwf, z, y, x, shade=1,edges=0;
    draw3,1;  /* not necessary interactively */
    rdline,prompt="hit RET or Enter to continue";
    write, "light3,diffuse=.2,specular=1";
    light3,diffuse=.2,specular=1;
    limits;
    draw3,1;  /* not necessary interactively */
    rdline,prompt="hit RET or Enter to continue";
    write, "light3,sdir=[cos(theta),.25,sin(theta)]  -- movie";
    movie, demo5_light;
    fma; demo5_light, 1;
    rdline,prompt="hit RET or Enter to continue";
    light3;
  }

  if (!itest || itest==3) {
    nx= demo5_n(1);
    ny= demo5_n(2);
    nz= demo5_n(3);
    xyz= array(0.0, 3, nx,ny,nz);
    xyz(1,..)= span(-1,1,nx);
    xyz(2,..)= span(-1,1,nx)(-,);
    xyz(3,..)= span(-1,1,nx)(-,-,);
    r= abs(xyz(1,..),xyz(2,..),xyz(3,..));
    theta= acos(xyz(3,..)/r);
    phi= atan(xyz(2,..),xyz(1,..)+(!r));
    y32= sin(theta)^2*cos(theta)*cos(2*phi);
    m3= mesh3(xyz, r*(1.+y32));
    r= theta= phi= xyz= y32= [];

    write, "   test uses "+pr1((nx-1)*(ny-1)*(nz-1))+" cells";
    elapsed= [0.,0.,0.];
    timer, elapsed;
    elapsed0= elapsed;

    slice3, m3, 1,value=.5, nv,xyzv;  /* inner isosurface */
    slice3, m3, 1,value=1., nw,xyzw;  /* outer isosurface */
    pxy= plane3([0,0,1],[0,0,0]);
    pyz= plane3([1,0,0],[0,0,0]);
    vp= slice3(m3, pyz, np,xyzp, 1);  /* pseudo-colored slice */
    local nvb,xyzvb,nwb,xyzwb;
    slice2, pxy, np,xyzp,vp;          /* cut slice in half */
    slice2x, pxy, nv,xyzv,, nvb,xyzvb;  /* split inner in halves */
    slice2, -pyz, nv,xyzv;       /* ...halve one of those halves */
    slice2x, pxy, nw,xyzw,, nwb,xyzwb;  /* split outer in halves */
    slice2, -pyz, nw,xyzw;       /* ...halve one of those halves */

    timer, elapsed;
    timer_print,"slicing time",elapsed-elapsed0;

    fma;
    write, "split_palette,\"viridis.gp\" -- generate palette for pl3tree";
    split_palette, "viridis.gp";
    write, "gnomon -- turn on gnomon";
    gnomon, 1;

    write, "pl3tree with 1 slicing plane, 2 isosurfaces";
    clear3;
    pl3tree, np,xyzp,vp,pyz;
    pl3tree, nvb,xyzvb;
    pl3tree, nwb,xyzwb;
    pl3tree, nv,xyzv;
    pl3tree, nw,xyzw;
    orient3;
    light3,diffuse=.2,specular=1;
    limits;
    demo5_light, 1;
    rdline,prompt="hit RET or Enter to continue";

    write, "spin3 animated rotation, use rot3 or orient3 for one frame";
    /* don't want limits to autoscale during animation */
    lims= limits();
    dx= 1.1*max(lims(2),-lims(1));
    dy= 1.1*max(lims(4),-lims(3));
    limits, -dx,dx,-dy,dy;
    spin3;
    limits;  /* back to autoscaling */
    demo5_light, 1;
    rdline,prompt="hit RET or Enter to continue";

    light3;
    gnomon, 0;
    palette, "gray.gp";
  }
}

demo5_n= [20,20,20];

/* movie frame display function for third demo5 */
func demo5_light(i)
{
  if (i>=30) return 0;

  theta= pi/4 + (i-1)*2*pi/29;
  light3, sdir=[cos(theta),.25,sin(theta)];

  /* without an explicit call to draw3, the light3 function would
   * cause no changes until Yorick paused for input from the keyboard,
   * since unlike the primitive plotting functions (plg, plf, plfp, ...)
   * the fma call made by the movie function will not trigger the
   * 3D display list
   * any movie frame display function which uses the 3D drawing
   * functions in pl3d.i will need to do this
   * the !making_movie flag supresses the fma in draw3 if this function
   * is called by movie (which issues its own fma), but allows it
   * otherwise */
  draw3, !making_movie;

  return 1;
}
