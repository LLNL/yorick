/*
 * $Id: plato.i,v 1.1 2005-09-18 22:06:16 dhmunro Exp $
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

local plato;
/* DOCUMENT plato.i
     Contains routines to generate points related to Platonic
     solids and other pleasing or simple 3D geometrical objects.

     pt_tet       All these return points of the solid imbedded in 
     pt_cube      a pleasing way inside the cube with corners +-1. 
     pt_oct       With a non-zero parameter, the points are instead
     pt_dodec     normalized to unit length.
     pt_ico

     bucky        return points or faces of geodesic dome-like
                  solids and their solid angles
 */

func pt_tet(norm)
{
  /* half of corners of a cube, no particular order */
  p= [[1,1,1],[-1,-1,1],[1,-1,-1],[-1,1,-1]];
  /* use other choice of corners if norm<0 */
  if (norm) p*= sqrt(1./3.)*sign(norm);
  return p;
}

func pt_cube(norm)
{
  /* points at corners of cube, hence 2x2x2 organization
   * where indices are x, y, z directions */
  p= array(-1,3,2,2,2);
  p(1,2,,)= p(2,,2,)= p(3,,,2)= 1;
  if (norm) p*= sqrt(1./3.);
  return p;
}

func pt_oct(norm)
{
  /* one point on each face of a cube, hence 2x3 organization
   * where 1st index selects + or - face of cube
   *       2nd index selects xyz face of cube */
  p= array(0,3,2,3);
  p(1,,1)= p(2,,2)= p(3,,3)= [-1,1];
  return p;
}

func pt_ico(norm)
{
  /* two points on each face of a cube, hence 2x2x3 organization,
   * where 1st index selects + or - point on face of cube,
   *       2nd index selects + or - face of cube
   *       3rd index selects xyz face of cube */
  g= 0.5*(sqrt(5.)-1.);  /* reciprocal golden ratio */
  p= [[[0,g,1],[0,-g,1]],[[0,g,-1],[0,-g,-1]]];
  p= [p, roll(p,[1,0,0]), roll(p,[2,0,0])];
  if (norm) p/= abs(p(1,..),p(2,..),p(3,..))(-,..);
  return p;
}

func pt_dodec(norm)
{
  /* two points on each face of a cube, followed by corners of cube */
  g= 0.5*(sqrt(5.)-1.);  /* reciprocal golden ratio */
  g2= 1.-g;  /* equals g*g */
  p= [[[0,g2,1],[0,-g2,1]],[[0,g2,-1],[0,-g2,-1]]];
  p= [p, roll(p,[1,0,0]), roll(p,[2,0,0])];
  p= grow(p(,*),g*pt_cube()(,*));
  if (norm) p/= abs(p(1,..),p(2,..),p(3,..))(-,..);
  return p;
}

func bucky(n,faces,&domega)
{
  /* two rings of five plus two points at poles
   * rings have cos(theta) = +- g (reciprocal golden ratio),
   *            sin(theta) = sqrt(g)
   *
   * The points are organized into five spiral strips running
   * southeast from the north pole to the south pole.  Each strip
   * consists of a 3x2 array of points which bound a strip of four
   * triangular faces; the diagonals run from (1,1) to (2,2) and
   * from (2,1) to (3,2).  The point (1,2) is always the north pole,
   * and the point (3,1) is always the south pole.  The points
   * (2,2) and (3,2) on the eastern edge of one strip are the same
   * as the points (1,1) and (3,2), respectively, on the western
   * edge of the strip immediately to the east.
   * Hence, the dimensionality of the returned array of points
   * is 3 by 3x2x5.  (30 - 4 duplications of north pole
   * - 4 duplications of south pole - 5*2 other duplicates = 12)
   *
   * When you specify n (default 0), bucky will halve each of the
   * initial 3x2 strips n times, to produce a (2^(n+1)+1)x(2^n+1)
   * array of equally spaced points in place of the 3x2 points of
   * the n=0 pattern.
   *   n=0 -->   12 points   20 faces
   *   n=1 -->   42 points   80 faces
   *   n=2 -->  162 points  320 faces
   *   n=4 -->  642 points 1280 faces
   *   n=5 --> 2562 points 5120 faces
   *
   * When radius==1, icosahedron apothem==sqrt((2+g)/(2-g)/3),
   * suggesting a worst case area ratio of (2+g)/(2-g)/3 = 0.631476.
   * However, by renormalizing the points after each doubling, this
   * ratio is considerably improved; the worst case in the limit of
   * many doublings is a little under 0.769.
   */
  g= 0.5*(sqrt(5.)-1.);  /* reciprocal golden ratio */
  p= array(0., 3,3,2,5);
  p(,1,2,)= [0,0,1];   /* north pole */
  p(,3,1,)= [0,0,-1];  /* south pole */
  c36= 0.5*(1.+g);  /* equals 0.5/g */
  s36= 0.5*sqrt(2.-g);
  c72= 0.5*g;
  s72= 0.5*sqrt(3.+g);
  g= 1./(1.+2.*g);  /* cos of ring angle */
  s= 2.*g;          /* sin is twice cos for this magic angle */
  ringn= s*[[1,0],[c72,s72],[-c36,s36],[-c36,-s36],[c72,-s72]];
  rings= s*[[c36,s36],[-c72,s72],[-1,0],[-c72,-s72],[c36,-s36]];
  p(3,1,1,)= p(3,2,2,)= g;
  p(1:2,1,1,)= ringn;
  p(1:2,2,2,)= roll(ringn,[0,-1]);
  p(3,2,1,)= p(3,3,2,)= -g;
  p(1:2,2,1,)= rings;
  p(1:2,3,2,)= roll(rings,[0,-1]);
  if (!n) n= 0;
  while (n--) {
    dims= dimsof(p);
    dims(3:4)= 2*dims(3:4)-1;
    q= array(0., dims);
    q(,1:0:2,1:0:2,)= p;
    q(,2:-1:2,1:0:2,)= p(,zcen,,);
    q(,1:0:2,2:-1:2,)= p(,,zcen,);
    q(,2:-1:2,2:-1:2,)= p(,2:0,2:0,)+p(,1:-1,1:-1,); /* *0.5 */
    p= q/abs(q(1,..),q(2,..),q(3,..))(-,..);
    q= x= [];
  }
  if (faces) {
    dims= dimsof(p);
    q= array(0.,3,2,dims(3)-1,dims(4)-1,5);
    llur= (pb=p(,1:-1,1:-1,)) + (pc=p(,2:0,2:0,));
    q(,1,,,)= llur + (pa=p(,1:-1,2:0,));  /* a-b-c */
    q(,2,,,)= llur + (pd=p(,2:0,1:-1,));  /* d-c-b */
    domega= q(1,..);
    p= q/abs(domega,q(2,..),q(3,..))(-,..);
    domega(1,..)= pt_solid2(pa,pb,pc);
    domega(2,..)= pt_solid2(pd,pc,pb);
  }
  return p;
}

func pt_cross(a,b)
{
  i = [2,3,1];
  j = [3,1,2];
  return a(i,..)*b(j,..) - a(j,..)*b(i,..);
}

func pt_solid(a,b,c)
{
  vab= pt_cross(a,b);
  sab= sqrt((vab*vab)(sum,..));
  vbc= pt_cross(b,c);
  sbc= sqrt((vbc*vbc)(sum,..));
  vca= pt_cross(c,a);
  sca= sqrt((vca*vca)(sum,..));
  cosa= -(vab*vca)(sum,..)/(sab*sca);
  cosb= -(vbc*vab)(sum,..)/(sbc*sab);
  cosc= -(vca*vbc)(sum,..)/(sca*sbc);
  /* this formula is simple, but inaccurate for small triangles */
  return acos(cosa)+acos(cosb)+acos(cosc)-pi;
}

func pt_solid2(a,b,c)
{
  vab= pt_cross(a,b);
  sab= sqrt((vab*vab)(sum,..));
  vbc= pt_cross(b,c);
  sbc= sqrt((vbc*vbc)(sum,..));
  vca= pt_cross(c,a);
  sca= sqrt((vca*vca)(sum,..));
  da= 1./(sab*sca);
  db= 1./(sbc*sab);
  dc= 1./(sca*sbc);
  cosa= -(vab*vca)(sum,..)*da;
  cosb= -(vbc*vab)(sum,..)*db;
  cosc= -(vca*vbc)(sum,..)*dc;
  sina= pt_cross(vab,vca);
  sina= sqrt((sina*sina)(sum,..))*da;
  sinb= pt_cross(vab,vca);
  sinb= sqrt((sinb*sinb)(sum,..))*db;
  sinc= pt_cross(vab,vca);
  sinc= sqrt((sinc*sinc)(sum,..))*dc;
  sabc= abs(sina*(cosb*cosc-sinb*sinc)+cosa*(sinb*cosc+cosb*sinc));
  cabc= 1. - cosa*(cosb*cosc-sinb*sinc) + sina*(sinb*cosc+cosb*sinc);
  /* this formula is good for small triangles,
   * bad for triangles with area near 2*pi */
  return 4.*asin(0.5*sabc/sqrt(cabc*(1.+sqrt(0.5*cabc))));
}
