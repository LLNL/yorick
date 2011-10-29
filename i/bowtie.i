/*
 * $Id: bowtie.i,v 1.1 2005-09-18 22:05:54 dhmunro Exp $
 * Detect bowties, boomerangs, and other topological oddities in
 * a quadrilateral mesh.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*= SECTION() detect quad mesh bowties =================================*/

func bowtie(rt, zt, ireg)
/* DOCUMENT map= bowtie(rt, zt)
         or map= bowtie(rt, zt, ireg)
     returns a "bowtie map" for the quadrilateral mesh defined by
     RT, ZT, and (optionally) IREG.  If IREG is present, it should be
     an integer array of the same dimensions as RT and ZT; its first
     row and column are ignored, otherwise each non-zero element of
     IREG marks an existing zone in the mesh.  (An IREG with one fewer
     row and column than RT and ZT will also be accepted.)  If IREG
     is omitted, every zone is presumed to exist.

     The returned MAP is a 2-D integer array with one fewer row and
     column than RT and ZT.  It's values have the following meanings:

          2   marks a convex zone with positive area
          1   marks a concave (boomerang) zone with positive area
          0   marks a bowtied zone
         -1   marks a concave (boomerang) zone with negative area
         -2   marks a convex zone with negative area
         -9   marks a non-existent zone

     Use the nbow function to print the results.

   SEE ALSO: nbow
 */
{
  dims= dimsof(rt);
  d= dimsof(zt);
  if (d(1)!=dims(1) || dims(1)!=2 || anyof(dims!=d) ||
      dims(2)<2 || dims(3)<2)
    error, "rt, zt are not quadrilateral mesh coordinates";

  dz1= zt(dif,1:-1);    dr1= rt(dif,1:-1);
  dz2= zt(2:0,dif);     dr2= rt(2:0,dif);
  dz3= zt(dif,2:0);     dr3= rt(dif,2:0);
  dz4= zt(1:-1,dif);    dr4= rt(1:-1,dif);

  a= array(0.0, 4, dimsof(dz1));
  a(1,,)= dz1*dr4 - dz4*dr1;
  a(2,,)= dz1*dr2 - dz2*dr1;
  a(3,,)= dz3*dr2 - dz2*dr3;
  a(4,,)= dz3*dr4 - dz4*dr3;

  map= long(sign(a))(sum,,)/2;

  if (is_void(ireg)) return map;

  if ((d=dimsof(ireg))(1)==2 && d(2)==dims(2) && d(3)==dims(3)) {
    ireg= ireg(2:0, 2:0);
  } else if (d(1)!=2 || d(2)!=dims(2)-1 || d(3)!=dims(3)-1) {
    error, "ireg not conformable with rt, zt arrays";
  }

  if (allof(ireg)) return map;

  map(where(!ireg))= -9;
  return map;
}

local nbow_negative;
/* DOCUMENT nbow_negative
     default value is 0
     set to 1 to reverse the sense of positive area for the nbow function
 */
nbow_negative= 0;

func nbow(rt, zt, ireg, all=)
/* DOCUMENT nbow, map
         or nbow, file
         or nbow, rt, zt
         or nbow, rt, zt, ireg
     prints information about topological oddities in a mesh.
     MAP is a bowtie map as returned by the bowtie function.
     FILE is a binary file containing rt, zt, and ireg arrays.
     RT, ZT and IREG are 2-D arrays defining a quadrilateral mesh.

     The information printed includes the zone index (corner with
     the largest indices) of zones which are concave (boomerangs)
     or bowtied, and of zones with negative area.  You can set
     the global variable nbow_negative to 1 to reverse the default
     sense of positive area.  By default, only the first 10 zones
     in each category are printed; use the all=1 keyword argument
     to print a complete (and maybe very long) list.

   SEE ALSO: bowtie
 */
{
  if (!is_void(zt)) map= bowtie(rt, zt, ireg);
  else if (is_stream(rt)) {
    f= rt;
    vars= get_vars(f);
    if ((vars(1) && anyof(*vars(1) == "ireg")) ||
        (vars(2) && anyof(*vars(2) == "ireg"))) ireg= f.ireg;
    else ireg= [];
    map= bowtie(f.rt, f.zt, ireg);
  } else {
    map= rt;
  }

  if (nbow_negative) map= -map;

  list= where2(abs(map)<=2 & map<0);
  _nbow_print, list, "Found %ld zones with negative areas:\n";

  list= where2(map==0);
  _nbow_print, list, "Found %ld bowtied zones:\n";

  list= where2(map==1);
  _nbow_print, list, "Found %ld concave (boomerang) zones:\n";

  list= where(map==2);
  write, format="Found %ld convex, positive area zones\n", numberof(list);
}

func _nbow_print(list, format)
{
  n= numberof(list);
  if (n) {
    list++;
    n/= 2;
    write, format=format, n;
    if (!all && n>10) np= 10;
    else np= n;
    write, format="     k= %3ld   l= %3ld\n", list(1,1:np), list(2,1:np);
    if (np<n)
      write, format="     (%ld more, use all=1 keyword to see them)\n", n-np;
  }
}
