/*
 * $Id: demo4.i,v 1.1 2005-09-18 22:05:55 dhmunro Exp $
 * Airfoil demo
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func demo4(mono=)
/* DOCUMENT demo4
         or demo4, mono=1
     solves for the flow past a 2D airfoil using Kutta-Jakowski theory.
     The colors represent static pressure (darker is lower pressure),
     red lines are streamlines.
     Solutions for various angles of attack are shown by animation.
     With the mono=1 keyword, only the streamlines are shown.  (On a
     monochrome terminal, the pressure makes the streamlines invisible.)
 */
{
  require, "movie.i";
  local nx, ny;
  imax= 50;
  movie, display;
  display, imax;
}

func display(i)
{
  attack= 28.*double(imax-i)/(imax-1);
  solve, attack, 2.0, 0.2;
  if (i==1) {
    extern cn, cx;
    cn= 0.8*min(pressure);
    cx= max(pressure);
  }
  limits, -2.8, 2.4, -2.4, 2.8;
  plmesh, z.im,z.re;
  if (!mono) plf, zncen(pressure), cmin=cn,cmax=cx;
  plc, potential.im, marks=0,color="red", levs=span(-2.5,3.5,33);
  plg,z.im(,1),z.re(,1), marks=0;
  return i<imax;
}

func solve(attack, chord, thick)
{
  extern w, z, potential, velocity, pressure; /* outputs */
  attack*= pi/180.;
  a= 0.5*chord;
  w= get_mesh(a, attack);
  /* the log(w) term adds just enough circulation to move the rear
     stagnation point to the trailing edge -- the Kutta condition */
  potential= jakowski(w, a) + 2i*a*sin(attack)*log(w);
  dpdw= 1.-(a/w)^2 + 2i*a*sin(attack)/w;
  emith= exp(-1i*attack);
  a= a*emith;
  b= thick*emith;  /* could add camber here too someday? */
  w-= b;
  a-= b;
  z= jakowski(w, a);
  dzdw= 1.-(a/w)^2;
  velocity= conj(dpdw/dzdw);
  pressure= 0.5*(1.0-abs(velocity)^2);
}

func get_mesh(a, attack)
{
  /* get a mesh in the w-plane
     -- the plane in which the airfoil is a circle
     the mesh splits at the point which will become the trailing edge */
  if (is_void(nx)) nx= 120;
  if (is_void(ny)) ny= 30;
  a= abs(a);
  theta= span(1.e-9, 2*pi-1.e-9, nx) - attack;
  r= a/span(1.0+1.e-9, 0.25, ny)(-,);
  return r*exp(1i*theta);
}

func jakowski(z, a)
{
  /* Jakowski transform - circle of radius a into slot of length 4a */
  return z + a*a/z;
}

func ijakowski(z, a)
{
  /* Inverse Jakowski - slot back into circle (unused here) */
  z*= 0.5;
  a= complex(a);
  sgn= sign((z*conj(a)).re);
  return z + sgn(z);
}
