/*
 * $Id: dratt.i,v 1.1 2005-09-18 22:04:55 dhmunro Exp $
 * Tests of Drat functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* Test mesh is 2-by-2 zones:

        r=2 -----------------
            | (2,3) | (3,3) |
        r=1 -----------------
            | (2,2) | (3,2) |
        r=0 -----------------
           z=0    z=0.5    z=1

   Four test rays, two parallel to the z-axis:
     1: (theta=0, phi=0, x=0.75, y=z=0) and
     2: (theta=pi, phi=0, x=1.25, y=z=0)
   and two perpendicular to the z-axis:
     3: (theta=pi/2, phi=0, x=y=0, z=.375) and
     4: (theta=pi/2, phi=2*pi/3, x=2.0, y=0, z=.625)
 */

func dratt
/* DOCUMENT dratt
     Run several simple tests of the ray tracking routines.
     This should be done (at minimum) whenever Drat is rebuilt.
 */
{
  test1; test2;
}

func make_mesh
{
  extern rt, zt;
  rt= span(0, 2, 3)(-:1:3,);
  zt= span(0, 1, 3)(,-:1:3);
}

func make_rays(dummy)
{
  return form_rays([[0.75, 1.25, 0.0,   2.0],     /* x values */
                    [0.0,  0.0,  0.0,   0.0],     /* y values */
                    [0.0,  0.0,  0.375, 0.625],   /* z values */
                    [0.0,  pi,   pi/2,  pi/2],    /* theta values */
                    [0.0,  0.0,  0.0,   2*pi/3]]); /* phi values */
}

func test0
{
  rays= internal_rays(make_rays());
  theta= atan(rays(2,..), rays(1,..));
  y= rays(3,..);
  z= rays(4,..);
  x= rays(5,..);
  save, createb("junkr.pdb"), x, y, z, theta;
}

func test1
{
  extern mesh, rt, zt, ireg, rays, paths;

  make_mesh;
  rays= make_rays();

  mesh= form_mesh(0,-1,-1);
  update_mesh, mesh, rt, zt;

  paths= track_rays(rays, mesh);
  checkpaths, paths, 0;

  mesh= form_mesh(0,-1,-1);
  ireg= array(int, dimsof(rt));
  ireg(2:,2:)= 1;
  update_mesh, mesh, rt, zt, ireg;

  paths= track_rays(rays, mesh);
  checkpaths, paths, 0;

  ireg(2,2)= 0;
  update_mesh, mesh, rt, zt, ireg;
  paths= track_rays(rays, mesh);
  checkpaths, paths, 1;

  mesh= form_mesh(1,-1,-1);
  update_mesh, mesh, rt, zt;
  paths= track_rays(rays, mesh);
  zones= [&[6,5,1,5,6,1], &[9,8,1,8,9,1], &[8,5,8,1], &[9,1]];
  for (i=1 ; i<=4 ; i++) {
    z= *paths(i).zone;
    if (numberof(z)!=numberof(*zones(i)) || anyof(z!=*zones(i)))
      write, "WARNING -- zsym path wrong for ray "+pr1(i);
  }

  mesh= form_mesh(1,-1,-1);
  ireg(2,2)= 0;
  update_mesh, mesh, rt, zt, ireg;
  paths= track_rays(rays, mesh);
  zones= [&[6,1,6,1], &[9,8,1,8,9,1], &[8,1,8,1], &[9,1]];
  for (i=1 ; i<=4 ; i++) {
    z= *paths(i).zone;
    if (numberof(z)!=numberof(*zones(i)) || anyof(z!=*zones(i)))
      write, "WARNING -- holey zsym path wrong for ray "+pr1(i);
  }
}

func test2
{
  /* Write phony data to a phony dump file.  */
  extern mesh, rt, zt, ireg, gav, gb, opac, source;
  extern rays;

  make_mesh;
  rays= make_rays();

  ireg= array(int, dimsof(rt));
  ireg(2:,2:)= 1;

  gb= [0.1, 0.2, 0.4, 0.8, 1.6];
  gav= sqrt(gb(2:)*gb(:-1));

  akap= ekap= 0.0*rt;
  akap(2,2)= -2.0*log(0.8);  /* transparency 0.8 in z, 0.64 in r */
  akap(3,2)= -2.0*log(0.5);  /* transparency 0.5 in z, 0.25 in r */
  akap(2,3)= -2.0*log(0.7);  /* transparency 0.7 in z, 0.49 in r */
  akap(3,3)= -2.0*log(0.6);  /* transparency 0.6 in z, 0.36 in r */
  ekap(2,2)= 1.0;
  ekap(3,2)= 2.0;
  ekap(2,3)= 3.0;
  ekap(3,3)= 4.0;
  akap+= 0.0*gav(-,-,);
  ekap+= 0.0*gav(-,-,);

  answer= [[0.8*0.5, 0.5*(1-0.8)*1.0+(1-0.5)*2.0],
           [0.6*0.7, 0.7*(1-0.6)*4.0+(1-0.7)*3.0],
           [0.49*0.64^2*0.49,
            (1-0.49)*3.0+0.49*((1-0.64^2)*1.0+0.64^2*((1-0.49)*3.0))],
           [0.36^2, (1-0.36^2)*4.0]];

  f= createb("junk00.pdb");
  save, f, gb, gav;
  time= 0.0; add_record, f, time;
  save, f, time, rt, zt, ireg, akap, ekap;
  time= 1.0; add_record, f, time;
  save, f, time, rt, zt, ireg, akap, ekap;
  time= 2.0; add_record, f, time;
  save, f, time, rt, zt, ireg, akap, ekap;
  time= 3.0; add_record, f, time;
  save, f, time, rt, zt, ireg, akap, ekap;
  close, f;

  /* next, reopen the file and try out the streak function */
  f= openb("junk00.pdb");

  write, "Testing streak..."
  result= streak(f, rays);

  if (!approx_eq(result,answer(-:1:4,,,-:1:4)))
    write, "WARNING -- streak function got wrong answer";

  write, "Testing snap..."
  result= snap(f,rays);

  if (!approx_eq(result,3.0*answer(-:1:4,2,)))
    write, "WARNING -- snap function got wrong answer";

  write, "Testing streak_save..."
  streak_save, "junks0.pdb", f, rays;
  g= openb("junks0.pdb");
  if (!approx_eq(gav,g.gav) || !approx_eq(gb,g.gb) ||
      !approx_eq(rays,g.rays))
    write, "WARNING -- streak_save static variables wrong";
  for (i=1 ; i<=4 ; i++) {
    jt, g, double(i-1);
    if (!approx_eq(answer(-:1:4,1,),g.transp) ||
        !approx_eq(answer(-:1:4,2,),g.selfem) || !approx_eq(i-1,g.time))
      write, "WARNING -- streak_save dynamic variables wrong";
  }
  close, g;


  extern drat_linear;
  drat_linear= 1;
  result= streak(f, rays);
  drat_linear= [];
  anslin= answer;
  anslin(2,)= [1.431736108e+00, 1.236208545e+00,
               1.853918756e+00, 1.802634491e+00];
  if (!approx_eq(result,anslin(-:1:4,,,-:1:4)))
    write, "WARNING -- streak function got wrong answer with drat_linear";
}

func grabpath(path)
{
  extern zone, ds, fi, ff, pt1, pt2, f;
  zone= *path.zone;   ds= *path.ds;     fi= path.fi;   ff= path.ff;
  pt1= *path.pt1;     pt2= *path.pt2;   f= *path.f;
}

func checkpaths(paths, hole)
{
  if (!hole) {
    zones= [&[5,6,1], &[9,8,1], &[8,5,8,1], &[9,1]];
    dss= [&[0.5,0.5,0], &[0.5,0.5,0], &[1.,2.,1.,0.], &[2.,0.]];
    pt1s= [&[4,5,6], &[6,5,4], &[8,5,4,7], &[9,8]];
    pt2s= [&[1,2,3], &[9,8,7], &[7,4,5,8], &[8,9]];
    fs= [&[-0.25,-0.25,-0.25], &[-0.25,-0.25,-0.25],
         &[-0.25,-0.25,0.25,0.25], &[0.25,-0.25]];
  } else {
    zones= [&[6,1], &[9,8,1], &[8,1,8,1], &[9,1]];
    dss= [&[0.5,0], &[0.5,0.5,0], &[1.,0.,1.,0.], &[2.,0.]];
    pt1s= [&[5,6], &[6,5,4], &[8,5,4,7], &[9,8]];
    pt2s= [&[2,3], &[9,8,7], &[7,4,5,8], &[8,9]];
    fs= [&[-0.25,-0.25], &[-0.25,-0.25,-0.25],
         &[-0.25,-0.25,0.25,0.25], &[0.25,-0.25]];
  }
  local zone, ds, fi, ff, pt1, pt2, f;
  for (i=1 ; i<=4 ; i++) {
    grabpath, paths(i);
    if (anyof(*zones(i)!=zone) || !approx_eq(*dss(i),ds) ||
        anyof(*pt1s(i)!=pt1) || anyof(*pt2s(i)!=pt2) ||
        !approx_eq(*fs(i),f)) {
      write, "WARNING -- path number "+print(i)(1)+" is bad:";
      write, "  zone= "+print(zone)(1)+"  should be "+print(*zones(i))(1);
      write, "  ds= "+print(ds)(1)+"  should be "+print(*dss(i))(1);
      write, "  pt1= "+print(pt1)(1)+"  should be "+print(*pt1s(i))(1);
      write, "  pt2= "+print(pt2)(1)+"  should be "+print(*pt2s(i))(1);
      write, "  f= "+print(f)(1)+"  should be "+print(*fs(i))(1);
    }
  }
}

func approx_eq(x, y)
{
  return allof((abs(x-y)/(abs(x+y)+1.e-35))<1.e-8);
}
