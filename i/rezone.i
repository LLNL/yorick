/*
 * $Id: rezone.i,v 1.2 2010-04-18 10:33:38 thiebaut Exp $
 * Point and click rezoner, written in interpreted Yorick.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

require, "button.i";
require, "bowtie.i";

func toy_mesh(filename)
/* DOCUMENT toy_mesh, filename
     generates a toy mesh in the file FILENAME in order to be able to
     play with the rezone function.  (FILENAME must be a string enclosed
     in double quotes, of course.)
   SEE ALSO: rezone
 */
{
  rt= span(1., 0., 6)(,-:1:11);
  zt= span(0., 2., 11)(-:1:6,);
  rt(3,3)+= 0.15;
  zt(3,3)+= 0.15;
  rt(3,7)+= 0.05;
  zt(3,7)+= 0.25;
  save, createb(filename), rt, zt;
}

extern rez_load_hook;
/* DOCUMENT rez_load_hook
     If non-nil, filename to be included immediately after rez.i.
     This file can be used to set personalized default values for the
     many parameters.
     Some parameters may be set before loading rez.i, as well
   SEE ALSO: rez_kstyle, rez_lstyle
 */

extern rez_kstyle;
extern rez_lstyle;
extern rez_color;
/* DOCUMENT rez_kstyle, rez_lstyle, rez_color
     is a vector of three longs:  [type,width,color]
     type MUST be 1, 2, 3, 4, or 5,
     width MUST be 1, 2, 3, 4, 5, 6, 7, or 8,
     and color MUST be -1, -2, -3, -4, -5, -6, -7, -8, -9, or -10

     Either or both parameters may be set before or after including
     rez.i; by default:
       rez_kstyle= [4, 1, -2]    (dashdot, thin, foreground color)
       rez_lstyle= [1, 1, -2]    (solid, thin, foreground color)

     To distinguish K and L by color instead of type, try:
       rez_kstyle= [1, 1, -7]    (dashdot, thin, blue)
       rez_lstyle= [1, 1, -5]    (solid, thin, red)
     If the variable rez_color is true (non-nil and non-zero) when
     this file is included, then these will be the default.

   SEE ALSO: rez_load_hook
 */
/* This choice distinguishes by line type.  */
_old_rez_color= rez_color;
if (is_void(rez_kstyle)) rez_kstyle= rez_color? [1, 1, -7]:[4, 1, -2];
if (is_void(rez_lstyle)) rez_lstyle= rez_color? [1, 1, -5]:[1, 1, -2];

extern rez_mark;
/* DOCUMENT rez_mark
     Causes rezone functions to mark bowtied or chevroned (boomeranged)
     zones when the mesh is plotted.  MUST have one of the following values:
       0 to not mark bowtied or chevroned zones
       1 to put a B at the center of the positive triangle of bowtied zones
       2 to put a C at the center of the larger wing of chevroned zones
       3 to mark both bowtied and chevroned zones

     By default, rez_mark is 3; you may set it to a different value
     either before or after including rez.i.

   SEE ALSO: rez_mesh, rez_next, rez_prev, rez_drag
 */
if (is_void(rez_mark)) rez_mark= 3;

_rez_on_black= 0;

extern rez_palette;
/* DOCUMENT rez_palette
     3-by-ncolors array used as a palette when rez_regs region coloring
     is set.  The index in rez_regs is the second index into rez_palette.
     The length 3 first index is [r,g,b].
     The default is 15 pastels.
   SEE ALSO: rez_regs
 */
rez_palette= [[255,230,230],[230,255,230],[230,230,255],[242,242,230],
              [230,242,242],[242,230,242],[236,249,230],[236,236,242],
              [249,236,230],[230,236,249],[242,236,236],[230,249,236],
              [249,230,236],[236,242,236],[236,230,249]];
_old_rez_palette= [];

func rez_chk_palette
{
  if (typeof(rez_palette)!=typeof(_old_rez_palette) ||
      dimsof(rez_palette)(1)!=dimsof(_old_rez_palette)(1) ||
      anyof(dimsof(rez_palette)!=dimsof(_old_rez_palette)) ||
      anyof(rez_palette!=_old_rez_palette)) {
    if (dimsof(rez_palette)(2)!=3)
      error, "rez_palette must be [[r1,g1,b1],[r2,g2,b2],...]";
    if (!is_array(rez_palette) || typeof(rez_palette)=="struct_instance" ||
        typeof(rez_palette)=="string" || typeof(rez_palette)=="complex" ||
        min(rez_palette)<0 || max(rez_palette)>255)
      error, "rez_palette values should be between 0 and 255";
    palette, rez_palette(1,), rez_palette(2,), rez_palette(3,);
    _old_rez_palette= rez_palette;
    fma;
  }
}

extern rez_regs;
/* DOCUMENT rez_regs
     is a vector of region numbers which are to be drawn in the colors
     specified in rez_palette.  Thus:
        rez_regs= [3,5]
     causes regions 3 and 5 to be drawn in colors.  All other regions have
     white backgrounds.  Hilighting too many regions in this way may slow
     down the redraws unacceptably.  To highlight all regions, use:
        rez_regs= indgen(max(rez_ireg))
     This won't work on monochrome monitors, of course.
     The default is rez_regs=[].
   SEE ALSO: rez_palette
 */

extern rez_adjust_mesh;
/* DOCUMENT rez_adjust_mesh
     is an optional hook function which is called after any change to the
     mesh (e.g.- by dragging points) has been made.  If it is defined, it
     may modify the external variables rez_rt and rez_zt.  This hook can
     be used, for example, to force points along some k or l line to lie
     along a symmetry line.
   SEE ALSO: rezone, rez_all, rez_mesh
 */

func rez_plm(flag, adj_check)
{
  if (adj_check && is_func(rez_adjust_mesh)) rez_adjust_mesh;

  fma;

  extern rez_kstyle, rez_lstyle;
  if (rez_color!=_old_rez_color) {
    rez_kstyle= rez_color? [1, 1, -7]:[4, 1, -2];
    rez_lstyle= rez_color? [1, 1, -5]:[1, 1, -2];
    _old_rez_color= rez_color;
  }

  if (!is_void(rez_regs)) {
    rez_chk_palette;
    n= numberof(rez_regs);
    colors= array(char, dimsof(rez_rt)(2), dimsof(rez_rt)(3));
    for (i=1 ; i<=n ; i++) {
      reg= rez_regs(i);
      if (noneof(rez_ireg==reg) || reg<=0) continue;
      colors(where(rez_ireg==reg))= i-1;
      plf, colors, rez_rt, rez_zt, rez_ireg, legend=string(0), region=reg;
    }
  } else if (_rez_on_black) {
    rez_ptexist;
    list= where(rez_ptx);
    r= rez_rt(list);
    z= rez_zt(list);
    pli, [['\0']], min(z), min(r), max(z), max(r);
  }

  plm, rez_rt, rez_zt, rez_ireg, legend=string(0), inhibit=1,
       color= rez_kstyle(3), width= rez_kstyle(2), type= rez_kstyle(1);
  plm, rez_rt, rez_zt, rez_ireg, legend=string(0), inhibit=2,
       color= rez_lstyle(3), width= rez_lstyle(2), type= rez_lstyle(1);

  if (rez_mark&3) {
    extern rez_map;
    local rr, zz;
    if (flag || is_void(flag)) {
      if (_rez_polar) {
        zt= rez_zt*cos(rez_rt);
        rt= rez_zt*sin(rez_rt);
      } else {
        zt= rez_zt;
        rt= rez_rt;
      }
      rez_map= bowtie(rt,zt,rez_ireg);
    }
    for (i=1 ; i<=2 ; i++) {
      if (!(rez_mark&i)) continue;
      rez_getrz, where(rez_map==(i-1));
      if (!is_void(rr)) {
        a1= (rr(2,2,)-rr(2,1,))*(zz(2,1,)-zz(1,1,)) -
          (zz(2,2,)-zz(2,1,))*(rr(2,1,)-rr(1,1,));
        a2= (rr(1,1,)-rr(1,2,))*(zz(1,2,)-zz(2,2,)) -
          (zz(1,1,)-zz(1,2,))*(rr(1,2,)-rr(2,2,));
        a3= (rr(2,1,)-rr(1,1,))*(zz(1,1,)-zz(1,2,)) -
          (zz(2,1,)-zz(1,1,))*(rr(1,1,)-rr(1,2,));
        if (i==1) {
          /* this is a magic trick for bowtied zones */
          index= ((a3>=0.) | (a1>=0.)<<1 | (a2>=0.)<<2) - 1;
          p1= [1,0,3,2](index);
          p2= [3,1,2,0](index);
          p3= [2,3,0,1](index);
          p4= [0,2,1,3](index);
          index= indgen(1:numberof(rr):4);
          p1+= index;
          p2+= index;
          p3+= index;
          p4+= index;
          /* now p1 p2 p3 p4 are the indices of the points in (rr,zz)
             with p1 p2 being the positive edge of the bowtied zone,
             so that p1 p3 and p2 p4 cross */
          r1= rr(p1);
          z1= zz(p1);
          r2= rr(p2);
          z2= zz(p2);
          r3= rr(p3);
          z3= zz(p3);
          r4= rr(p4);
          z4= zz(p4);
          fa= (r3-r2)*(z4-z1);
          fb= -(r4-r1)*(z3-z2);
          det= fa+fb;
          fa/= det;
          fb/= det;
          rr= r1*fa + r2*fb + (r3-r2)*(r4-r1)*(z2-z1)/det;
          zz= z2*fa + z1*fb - (z3-z2)*(z4-z1)*(r2-r1)/det;
          rr= (rr+r1+r2)/3.0;
          zz= (zz+z1+z2)/3.0;
          marker= 'B';
          legend= "B: bowtied zones";
        } else {
          a4= (rr(1,2,)-rr(2,2,))*(zz(2,2,)-zz(2,1,)) -
            (zz(1,2,)-zz(2,2,))*(rr(2,2,)-rr(2,1,));
          r1= (rr(1,1,)+rr(2,1,)+rr(2,2,))/3.0;
          z1= (zz(1,1,)+zz(2,1,)+zz(2,2,))/3.0;
          r2= (rr(1,1,)+rr(2,2,)+rr(1,2,))/3.0;
          z2= (zz(1,1,)+zz(2,2,)+zz(1,2,))/3.0;
          r3= (rr(2,1,)+rr(1,1,)+rr(1,2,))/3.0;
          z3= (zz(2,1,)+zz(1,1,)+zz(1,2,))/3.0;
          r4= (rr(1,2,)+rr(2,2,)+rr(2,1,))/3.0;
          z4= (zz(1,2,)+zz(2,2,)+zz(2,1,))/3.0;
          mask= (a1>0. & a2>0.);
          which= [a1*mask,a2*mask,a3*(1-mask),a4*(1-mask)](,mxx);
          n= numberof(a1);
          list= indgen(n)+n*(which-1);
          rr= [r1,r2,r3,r4](list);
          zz= [z1,z2,z3,z4](list);
          marker= 'C';
          legend= "C: chevroned zones";
        }
        plg,rr,zz,type=0,marker=marker,legend=legend;
      }
    }
  }
}

_rez_polar= 0n;

func rez_toggle
{
  zt= rez_zt;
  rt= rez_rt;
  _rez_polar= !_rez_polar;
  if (_rez_polar) {
    rez_zt= abs(rt,zt);
    rez_rt= atan(rt,zt+1.e-99);
  } else {
    rez_zt= zt*cos(rt);
    rez_rt= zt*sin(rt);
  }
}

/* These four parameters specify the viewport position, which is actually
   a function of the stylesheet being used, and potentially changes as
   a function of window or coordinate system within the window.  The numbers
   here should work decently for the work.gs and boxed.gs styles.  If you
   set them very large, the only side effect is that clicking way outside
   the coordinate system (e.g.- missing the Done or Undo button) will pick
   up the nearest mesh vertex).  This is not that unreasonable a behavior,
   especially since any miscue can be undone.  See rez_load_hook.  */
_rez_xvp=  (0.615+0.175)*0.5;
_rez_dxvp= (0.615-0.175)*0.5;
_rez_yvp=  (0.895+0.455)*0.5;
_rez_dyvp= (0.895-0.455)*0.5;
/* Following setup could be placed in rez_load_hook to allow any style:
  _rez_xvp=  0.5;
  _rez_dxvp= 0.6;
  _rez_yvp=  0.5;
  _rez_dyvp= 0.6;
 */

func rezone(filename, all=, quiet=)
/* DOCUMENT rezone, filename
         or rezone

     Invoke the mouse-driven rezoner.  Input is taken from variables
     rt, zt, and ireg in the file FILENAME (which is prompted for if
     omitted).  Output is written to a different file as rt, zt, and
     ireg.  The output file also contains copies rez_rt, rez_zt, and
     rez_ireg in case the mesh under a different name is desirable,
     as well as rez_rt0, rez_zt0, and rez_ireg0, which are the original
     mesh.

     If you click on [Escape], you may continue rezoning where you left
     off by typing   rezone   with no arguments.

     When you are finished, click [OK] to write the output file.

     Use [Zoom or Pan] and [Set Styles] to change the plot limits or
     line colors and styles.  The [Zoom or Pan] screen has buttons which
     "warp" you to each bowtied or chevroned "problem" zone, as well as
     a button to toggle between rectangular and polar coordinates.

     Click [Drag Points] to drag points one at a time with the mouse.

     Click [Adjust Line] to smooth the points on logical line segments.
     You first click on the endpoints P and Q, then click a button to
     smooth or equal space the poionts between P and Q, or drag any
     point between P and Q to establish equal ratio zones ("ER" operation).

     Click [Adjust Region] to smooth the points in logically rectangular
     regions.  You first click on the opposite corners P and R of the
     rectangle PQRS, then click a button to smooth the interior points,
     or click on a point on one of the four edges to transfer the spacing
     on that edge throughout the region PQRS ("EQ" operation).

     The [Undo] (or [Redo]) button removes the effect of the most
     recent drag points, adjust line, or adjust region operation.

     Hints:
     (0) After [Escape], the current mesh is in the variables
         rez_zt, rez_rt, and rez_ireg.  You can change these
         variables "by hand" before restarting rezone to make
         algebraic adjustments.
     (1) Switch to polar coordinates during an [Adjust Line] or
         [Adjust Region] in order to get points to lie along a
         circular arc.  If your center point is not (0,0), you
         will need to [Escape] and subtract your center point
         from the rez_zt and rez_rt variables yourself, then
         [Escape] a second time to restore the proper coordinates
         before you write the output file.
     (2) You can select a center point as the second point during
         [Adjust Line] or [Adjust Region].  When you do this, the
         K (or L) value of this second point will be the same as
         that of the first point -- otherwise there is no way to
         distinguish among all of the logical points at the center.
         In an [Adjust Region], the center point will be labeled
         Q instead of R as usual, and you will need to choose a
         third point (S) to complete the rectangle PQRS.
     (3) If you set the variable rez_color=1 before you include
         rezone.i, you will get K lines and L lines drawn in different
         colors instead of the default (which is K lines dashdot and
         L lines solid).
     (4) If you have a color monitor, you can set rez_regs before
         invoking rezone in order to highlight a list of regions.

     The ALL keyword will be passed to the nbow function, which will
     not be called if the QUIET keyword is non-nil and non-zero.  By
     default, a short summary of bowtied and chevroned zones is printed.

     If you need help on how to use one of the screens, try getting
     help on the appropriate on of these see-also topics, e.g.-
        help, rez_style
     to get help on the [Set Styles] screen.

   SEE ALSO: rez_all, rez_drag, rez_ladj, rez_radj, rez_zoom, rez_style,
             rez_color, rez_regs, rez_adjust_mesh, toy_mesh
 */
{
  extern rez_name;
  if (is_void(filename)) {
    if (is_void(rez_name)) {
      filename= "";
      read, format="%s", prompt="Name of Z-file: ", filename;
    }
  }
  if (!is_void(filename)) {
    /* begin a new rezone */
    for (i=1 ; i<=2 ; i++) {
      if (!is_void(open(filename,"rb",1))) break;
      write, "Cannot open "+filename;
      read, format="%s", prompt="Name of Z-file: ", filename;
    }
    if (i>2) {
      write, "Quitting rezone";
      return;
    }
    rez_mesh, openb(filename), quiet=quiet, all=all;
    if (!is_void(rez_zt) && !is_void(rez_rt)) rez_name= filename;
    write, "For help, click [Escape], then type:    help, rezone";
  }

  esca= rez_all();
  if (_rez_polar) rez_toggle;
  if (esca) return;

  default= rez_name+"_rez";
  for (i=1 ; i<=3 ; i++) {
    line=
      rdline(prompt="Output filename (default "+default+", ? to escape): ");
    line= strtok(line)(1);
    if (!line) line= default;
    else if (line=="?") line= string(0);
    if (line!=rez_name) break;
  }
  if (!line || line==rez_name) {
    write, "Escaping from rezone -- to resume, type    rezone";
    return;
  }

  zt= rez_zt;
  rt= rez_rt;
  ireg= rez_ireg;
  at_pdb_close|= 1;
  save, createb(line), zt, rt, ireg, rez_zt, rez_rt, rez_ireg,
    rez_zt0, rez_rt0, rez_ireg0;

  rez_name= [];
}

func rez_all(dummy)
/* DOCUMENT rez_all
     Click on [Drag Points] to drag points around with the mouse with
     rez_drag.

     Click on [Adjust Line] to adjust points along a line segment with
     rez_ladj.

     Click on [Adjust Region] to adjust points in a rectangular region
     with rez_radj.

     Click on the [Undo] button to undo the previous change.
     Click on the [Redo] button to redo the previous undo.
     Click on the [Revert] to return to the original mesh.

     When you are finished moving points around, click on [OK].
     Click on [Escape] to exit with a non-zero return value.

     Click on [Zoom or Pan] to change the plot limits (see rez_zoom).

     Click on [Set Styles] to change the line styles or colors (see
     rez_style).

   SEE ALSO: rez_ladj, rez_mesh, rez_zoom, rez_style, rez_adjust_mesh
 */
{
  done= Button(text="OK",x=.203,y=.895,dx=.020,dy=.012);
  undo= Button(text="Undo",x=.254,y=.895,dx=.025,dy=.012);
  esca= Button(text="Escape",x=.344,y=.895,dx=.035,dy=.012);
  zorp= Button(text="Zoom or Pan",x=.464,y=.895,dx=.054,dy=.012);
  sets= Button(text="Set Styles",x=.569,y=.895,dx=.045,dy=.012);
  drag= Button(text="Drag Points",x=.240,y=.375,dx=.054,dy=.012);
  ladj= Button(text="Adjust Line",x=.350,y=.375,dx=.050,dy=.012);
  radj= Button(text="Adjust Region",x=.468,y=.375,dx=.062,dy=.012);
  reve= Button(text="Revert",x=.577,y=.375,dx=.030,dy=.012);

  rez_plm, 1;
  button_plot, done, undo, esca, zorp, sets, drag, ladj, radj, reve;

  zt= zt2= rez_zt;
  rt= rt2= rez_rt;

  for (;;) {
    m= mouse(1, 2, "");
    if (is_void(m)) { rdline,prompt=""; continue; }
    x= m(5);
    y= m(6);
    if (button_test(done,x,y)) break;
    if (button_test(esca,x,y)) {
      write, "Escaping from rezone -- to resume, type    rezone";
      rez_plm,1;
      return 1;
    }
    if (button_test(undo,x,y)) {
      if (undo.text=="Undo") {
        rez_zt= zt;
        rez_rt= rt;
        undo.text= "Redo";
      } else {
        rez_zt= zt2;
        rez_rt= rt2;
        undo.text= "Undo";
      }
      rez_plm, 1;
    } else if (button_test(reve,x,y)) {
      zt= rez_zt;
      rt= rez_rt;
      undo.text= "Undo";
      rez_zt= zt2= rez_zt0;
      rez_rt= rt2= rez_rt0;
      rez_plm, 1;
    } else if (button_test(zorp,x,y)) {
      rez_zoom;
      rez_plm, 0;
    } else if (button_test(sets,x,y)) {
      rez_style;
      rez_plm, 0;
    } else if (button_test(drag,x,y)) {
      zt= rez_zt;
      rt= rez_rt;
      undo.text= "Undo";
      rez_drag;
      zt2= rez_zt;
      rt2= rez_rt;
    } else if (button_test(ladj,x,y)) {
      zt= rez_zt;
      rt= rez_rt;
      undo.text= "Undo";
      rez_ladj;
      zt2= rez_zt;
      rt2= rez_rt;
    } else if (button_test(radj,x,y)) {
      zt= rez_zt;
      rt= rez_rt;
      undo.text= "Undo";
      rez_radj;
      zt2= rez_zt;
      rt2= rez_rt;
    } else {
      rez_ding;
      continue;
    }
    button_plot, done, undo, esca, zorp, sets, drag, ladj, radj, reve;
  }

  rez_plm, 1;
  return 0;
}

func rez_drag
/* DOCUMENT rez_drag
     Allow points in the rezone mesh to be dragged around with
     the mouse.  Press mouse button near a vertex, move mouse
     to desired position of that vertex, and release button.
     If the point where you click is less than twice the distance to the
     second closest point than to the nearest point, the point will not
     be moved (you will hear a beep when you release the button).
     Click on the [Undo] button to undo the previous move; you can undo
     up to 100 previous moves by successive [Undo] clicks.

     When you are finished moving points around, click on [OK].

     Click on [Zoom or Pan] to change the plot limits (see rez_zoom).

     Click on [Set Styles] to change the line styles or colors (see
     rez_style).

   SEE ALSO: rez_ladj, rez_radj, rez_mesh, rez_zoom, rez_style
 */
{
  done= Button(text="OK",x=.203,y=.895,dx=.020,dy=.012);
  undo= Button(text="Undo",x=.254,y=.895,dx=.025,dy=.012);
  lab1= Button(text="Drag points.",x=.344,y=.895,dx=.050,dy=.012,width=-1);
  zorp= Button(text="Zoom or Pan",x=.464,y=.895,dx=.054,dy=.012);
  sets= Button(text="Set Styles",x=.569,y=.895,dx=.045,dy=.012);

  rez_plm, 1;
  button_plot, done, undo, lab1, zorp, sets;

  list= array(0.0, 3, 100);  /* keep last 100 moves for undo */

  kmax= dimsof(rez_rt)(2);
  for (;;) {
    m= mouse(1, 2, "");
    if (is_void(m)) { rdline,prompt=""; continue; }
    flag= j= 0;
    if (button_test(done,m(5),m(6))) break;
    if (button_test(undo,m(5),m(6))) {
      j= long(list(1,1));
      zt= list(2,1);
      rt= list(3,1);
      list(,1:-1)= list(,2:0);
      list(,0)= 0.0;
      flag= 1;
    } else if (button_test(zorp,m(5),m(6))) {
      rez_zoom;
      flag= j= 2;
    } else if (button_test(sets,m(5),m(6))) {
      rez_style;
      flag= j= 2;
    } else if (abs(_rez_xvp-m(5))<=_rez_dxvp ||
               abs(_rez_yvp-m(6))<=_rez_dyvp) {
      j= rez_find(m(1), m(2));
      if (numberof(_rez_center)) j= 0;
      zt= m(3);
      rt= m(4);
    }
    if (j<1) {
      rez_ding;
    } else {
      if (flag<2) {
        if (!flag) {
          list(,2:0)= list(,1:-1);
          list(1,1)= j;
          list(2,1)= rez_zt(j);
          list(3,1)= rez_rt(j);
        }
        rez_zt(j)= zt;
        rez_rt(j)= rt;
      }
      rez_plm, (flag<2), 1;
      button_plot, done, undo, lab1, zorp, sets;
      if (!flag) {
        text= swrite(format="moved (%ld,%ld)",(j-1)%kmax+1,(j-1)/kmax+1);
        plt, text, 0.35, 0.375;
      }
    }
  }

  rez_plm, 1;
}

func rez_ladj
/* DOCUMENT rez_ladj
     Adjust the points along a logical line segment PQ:

     Click on [Set P] or [Set Q] to set the endpoints of the segment.
     When P and Q have been set, click [OK]; this button will
     read [Cancel] if P and Q do not have either the same K or the
     same L index.

     Click on [Smooth] to smooth the points along PQ.

     Click on [Equal Space] to make the points along PQ equally spaced.
     This forces PQ to be a straight line.

     Drag any point between P and Q to get equal ratio spacing of all
     the points between P and Q.  This also forces PQ to be a straight
     line; if PQ is initially curved, you may want to click on
     [Equal Space] before you attempt to drag points.

     Click on the [Undo] button to undo the previous adjustment; you can
     undo up to 20 previous adjustments by successive [Undo] clicks.

     When you are finished adjusting lines, click on [OK].

     Click on [Zoom or Pan] to change the plot limits (see rez_zoom).

     Click on [Set Styles] to change the line styles or colors (see
     rez_style).

     Hint:
         You can select a center point as the second point during
         rez_ladj.  When you do this, the K (or L) value of this second
         point will be the same as that of the first point --
         otherwise there is no way to distinguish among all of the
         logical points at the center.

   SEE ALSO: rez_drag, rez_radj, rez_mesh, rez_zoom, rez_style
 */
{
  done= Button(text="OK",x=.203,y=.895,dx=.020,dy=.012);
  undo= Button(text="Undo",x=.254,y=.895,dx=.025,dy=.012);
  lab1= Button(text="Adjust line.",x=.344,y=.895,dx=.050,dy=.012,width=-1);
  zorp= Button(text="Zoom or Pan",x=.464,y=.895,dx=.054,dy=.012);
  sets= Button(text="Set Styles",x=.569,y=.895,dx=.045,dy=.012);

  setp= Button(text="Set P",x=.208,y=.375,dx=.027,dy=.012);
  setq= Button(text="Set Q",x=.268,y=.375,dx=.027,dy=.012);
  drag= Button(text="Drag for ER",x=.355,y=.375,
               dx=.035,dy=.012,width=-1);
  smth= Button(text="Smooth",x=.454,y=.375,dx=.035,dy=.012);
  eqsp= Button(text="Equal Space",x=.550,y=.375,dx=.054,dy=.012);

  mode= 1;
  jP= jQ= oldjP= oldjQ= 0;
  rez_plm, 1;
  kmax= dimsof(rez_rt)(2);
  lab1.text= "Select P.";
  setp.text= "Set Q";
  done.text= "Cancel";
  done.dx= 0.033;
  button_plot, done, lab1, zorp, sets, setp;

  for (;;) {
    m= mouse(1, 2, "");
    if (is_void(m)) { rdline,prompt=""; continue; }
    x= m(5);
    y= m(6);
    if (button_test(zorp,x,y)) {
      rez_zoom;
    } else if (button_test(sets,x,y)) {
      rez_style;
    } else if (button_test(setp,x,y)) {
      mode= 1 + (setp.text!="Set P");
      if (mode==1) {
        lab1.text= "Select P.";
        setp.text= "Set Q";
      } else {
        lab1.text= "Select Q.";
        setp.text= "Set P";
      }

    } else if (mode) {
      if (button_test(done,x,y)) {
        if (done.text=="Cancel") {
          if (!oldjP || !oldjQ) break;
          jP= oldjP;
          jQ= oldjQ;
        } else {
          if (kP==kQ) {
            k= kP;
            l= rez_range(min(lP,lQ):max(lP,lQ));
          } else {
            l= lP;
            k= rez_range(min(kP,kQ):max(kP,kQ));
          }
          zt= rez_zt(k,l);
          rt= rez_rt(k,l);
          list= array([zt,rt], 20);  /* keep last 20 for undo */
        }
        lab1.text= "Adjust line.";
        setp.text= "Set P";
        mode= 0;
      } else if (abs(_rez_xvp-x)<=_rez_dxvp ||
               abs(_rez_yvp-y)<=_rez_dyvp) {
        j= rez_find(m(1), m(2));
        if (numberof(_rez_center)>1) {
          kc= (_rez_center-1)%kmax + 1;
          lc= (_rez_center-1)/kmax + 1;
          if (noneof(kc-kc(1))) kc= kc(1);
          else if (noneof(lc-lc(1))) lc= lc(1);
          if (mode==1 && jQ>0) {
            kd= (jQ-1)%kmax + 1;
            ld= (jQ-1)/kmax + 1;
            if (numberof(kc)==1 && kc!=kd && anyof(lc==ld))
              j= kc + (ld-1)*kmax;
            else if (numberof(lc)==1 && lc!=ld && anyof(kc==kd))
              j= kd + (lc-1)*kmax;
          } else if (mode!=1 && jP>0) {
            kd= (jP-1)%kmax + 1;
            ld= (jP-1)/kmax + 1;
            if (numberof(kc)==1 && kc!=kd && anyof(lc==ld))
              j= kc + (ld-1)*kmax;
            else if (numberof(lc)==1 && lc!=ld && anyof(kc==kd))
              j= kd + (lc-1)*kmax;
          }
        }
        if (j<1) {
          rez_ding;
          continue;
        } else {
          if (mode==1) jP= j;
          else jQ= j;
          kP= (jP-1)%kmax + 1;
          lP= (jP-1)/kmax + 1;
          kQ= (jQ-1)%kmax + 1;
          lQ= (jQ-1)/kmax + 1;
        }
      } else {
        rez_ding;
        continue;
      }

    } else {
      if (button_test(done,x,y)) break;
      if (button_test(undo,x,y)) {
        rez_zt(k,l)= list(,1,1);
        rez_rt(k,l)= list(,2,1);
        list(,,1:-1)= list(,,2:0);
      } else if (button_test(setq,x,y)) {
        oldjP= jP;
        oldjQ= jQ;
        lab1.text= "Select Q.";
        mode= 2;
      } else if (button_test(smth,x,y)) {
        zt= rez_zt(k,l);
        if (numberof(zt)>2) {
          rt= rez_rt(k,l);
          list(,,2:0)= list(,,1:-1);
          list(,1,1)= zt;
          list(,2,1)= rt;
          zt(2:-1)= zt(zcen)(zcen);
          rt(2:-1)= rt(zcen)(zcen);
          rez_zt(k,l)= zt;
          rez_rt(k,l)= rt;
        }
      } else if (button_test(eqsp,x,y)) {
        zt= rez_zt(k,l);
        n= numberof(zt);
        if (n>2) {
          rt= rez_rt(k,l);
          list(,,2:0)= list(,,1:-1);
          list(,1,1)= zt;
          list(,2,1)= rt;
          rez_zt(k,l)= span(zt(1), zt(0), n);
          rez_rt(k,l)= span(rt(1), rt(0), n);
        }
      } else if (abs(_rez_xvp-x)<=_rez_dxvp ||
               abs(_rez_yvp-y)<=_rez_dyvp) {
        j= rez_find(m(1), m(2));
        if (j<=min(jP,jQ) || j>=max(jP,jQ) ||
            (kP==kQ && abs(j-jP)%kmax)) {
          rez_ding;
          continue;
        } else {
          zP= rez_zt(jP);
          rP= rez_rt(jP);
          zQ= rez_zt(jQ);
          rQ= rez_rt(jQ);
          zx= m(3)-zP;
          rx= m(4)-rP;
          za= zQ-zP;
          ra= rQ-rP;
          /* set sx to projection of new point onto PQ, normalized to
             lie between 0 (at P) and 1 (at Q) */
          sx= (za*zx+ra*rx)/abs(za,ra,1.e-35)^2;
          if (sx>0. && sx<1.) {
            zt= rez_zt(k,l);
            rt= rez_rt(k,l);
            list(,,2:0)= list(,,1:-1);
            list(,1,1)= zt;
            list(,2,1)= rt;
            n= numberof(zt)-1;  /* already guaranteed >1 */
            if (kP==kQ) m= abs((j-1)/kmax + 1 - lP);
            else m= abs((j-1)%kmax + 1 - kP);
            m= 1./m;
            r= rez_solve(1./sx, n*m)^m;
            sa= spanl(1., r^(n-1), n)(cum);
            sx= [0.0, sa(0)];
            zt= interp([zP,zQ], sx, sa);
            rt= interp([rP,rQ], sx, sa);
            if (kP>kQ || lP>lQ) {
              zt= zt(0:1:-1);
              rt= rt(0:1:-1);
            }
            rez_zt(k,l)= zt;
            rez_rt(k,l)= rt;
          } else {
            rez_ding;
            continue;
          }
        }
      } else {
        rez_ding;
        continue;
      }
    }
    rez_plm, !mode, 1;
    if (!mode || (jP && jQ && ((kQ==kP) ~ (lQ==lP)))) {
      done.text= "OK";
      done.dx= 0.020;
    } else {
      done.text= "Cancel";
      done.dx= 0.033;
    }
    button_plot, done, lab1, zorp, sets, setp;
    if (jP) plg, rez_rt(kP,lP), rez_zt(kP,lP),
      marker='P', type=0, legend=string(0);
    if (jQ) plg, rez_rt(kQ,lQ), rez_zt(kQ,lQ),
      marker='Q', type=0, legend=string(0);
    if (!mode) button_plot, undo, setq, drag, smth, eqsp;
    else if (jP || jQ) {
      text= jP? swrite(format="P=(%ld,%ld)",kP,lP) : "";
      if (jQ) text+= swrite(format="    Q=(%ld,%ld)",kQ,lQ);
      plt, text, 0.35, 0.375;
    }
  }

  rez_plm, 1;
}

func rez_radj
/* DOCUMENT rez_radj
     Adjust the points in a logical rectangle PQRS:

     Click on [Set P] or [Set R] to set the endpoints of the segment.
     When P and R have been set, click [OK]; this button will
     read [Cancel] if P and R have the same K or the same L index.

     Click on a point on any edge of PQRS (but not one of the corners)
     to copy the spacings on that edge throughout the entire region.
     All of the lines of the same type (K-line or L-line) as the edge
     you clicked on will become straight lines.

     Click on [Smooth] to smooth the points interior to PQRS.
     Click on [Smooth 4] to perform the smooth operation four times.

     Click on the [Undo] button to undo the previous adjustment; you can
     undo up to 10 previous adjustments by successive [Undo] clicks.

     When you are finished adjusting regions, click on [OK].

     Click on [Zoom or Pan] to change the plot limits (see rez_zoom).

     Click on [Set Styles] to change the line styles or colors (see
     rez_style).

     Hint:
         You can select a center point as the second point during
         rez_radj.  When you do this, the K (or L) value of this second
         point will be the same as that of the first point --
         otherwise there is no way to distinguish among all of the
         logical points at the center.  The center point will be labeled
         Q instead of R as usual, and you will need to choose a third
         point (S) to complete the rectangle PQRS.

   SEE ALSO: rez_drag, rez_ladj, rez_mesh, rez_zoom, rez_style
 */
{
  done= Button(text="OK",x=.203,y=.895,dx=.020,dy=.012);
  undo= Button(text="Undo",x=.254,y=.895,dx=.025,dy=.012);
  lab1= Button(text="Adjust region.",x=.344,y=.895,dx=.050,dy=.012,width=-1);
  zorp= Button(text="Zoom or Pan",x=.464,y=.895,dx=.054,dy=.012);
  sets= Button(text="Set Styles",x=.569,y=.895,dx=.045,dy=.012);

  setp= Button(text="Set P",x=.208,y=.375,dx=.027,dy=.012);
  setr= Button(text="Set R",x=.268,y=.375,dx=.027,dy=.012);
  drag= Button(text="Click edge for EQ",x=.371,y=.375,
               dx=.035,dy=.012,width=-1);
  smth= Button(text="Smooth",x=.480,y=.375,dx=.035,dy=.012);
  eqsp= Button(text="Smooth 4",x=.563,y=.375,dx=.041,dy=.012);

  mode= 1;
  jP= jR= jQ= oldjP= oldjR= 0;
  rez_plm, 1;
  kmax= dimsof(rez_rt)(2);
  lab1.text= "Select P.";
  setp.text= "Set R";
  done.text= "Cancel";
  done.dx= 0.033;
  button_plot, done, lab1, zorp, sets, setp;

  for (;;) {
    m= mouse(1, 2, "");
    if (is_void(m)) { rdline,prompt=""; continue; }
    x= m(5);
    y= m(6);
    if (button_test(zorp,x,y)) {
      rez_zoom;
    } else if (button_test(sets,x,y)) {
      rez_style;
    } else if (button_test(setp,x,y)) {
      mode= 1 + (setp.text!="Set P");
      if (mode==1) {
        lab1.text= "Select P.";
        setp.text= "Set R";
      } else {
        lab1.text= "Select R.";
        setp.text= "Set P";
      }
      jQ= 0;

    } else if (mode) {
      if (button_test(done,x,y)) {
        if (done.text=="Cancel") {
          if (!oldjP || !oldjR) break;
          jP= oldjP;
          jR= oldjR;
        } else {
          k0= min(kP,kR);
          k1= max(kP,kR);
          l0= min(lP,lR);
          l1= max(lP,lR);
          zt= rez_zt(k0:k1,l0:l1);
          rt= rez_rt(k0:k1,l0:l1);
          list= array([zt,rt], 10);  /* keep last 10 for undo */
        }
        lab1.text= "Adjust region.";
        setp.text= "Set P";
        mode= jQ= 0;
      } else if (abs(_rez_xvp-x)<=_rez_dxvp ||
                 abs(_rez_yvp-y)<=_rez_dyvp) {
        j= rez_find(m(1), m(2));
        mold= mode;
        if (numberof(_rez_center)>1) {
          if (mode==2 && jP>0) {
            kc= (_rez_center-1)%kmax + 1;
            lc= (_rez_center-1)/kmax + 1;
            if (noneof(kc-kc(1))) kc= kc(1);
            else if (noneof(lc-lc(1))) lc= lc(1);
            kd= (jP-1)%kmax + 1;
            ld= (jP-1)/kmax + 1;
            if (numberof(kc)==1 && kc!=kd && anyof(lc==ld))
              j= kc + (ld-1)*kmax;
            else if (numberof(lc)==1 && lc!=ld && anyof(kc==kd))
              j= kd + (lc-1)*kmax;
            lab1.text= "Select S.";
            mode= 3;
          } else {
            j= 0;
          }
        }
        if (j<1) {
          rez_ding;
          continue;
        } else {
          if (mode==1) jP= j;
          else if (mode==2) jR= j;
          else if (mode!=mold) jQ= j;
          else if (kP==kQ) jR= (j-1)%kmax + 1 + (lQ-1)*kmax;
          else jR= kQ + ((j-1)/kmax)*kmax;
          kP= (jP-1)%kmax + 1;
          lP= (jP-1)/kmax + 1;
          kR= (jR-1)%kmax + 1;
          lR= (jR-1)/kmax + 1;
          kQ= (jQ-1)%kmax + 1;
          lQ= (jQ-1)/kmax + 1;
        }
      } else {
        rez_ding;
        continue;
      }

    } else {
      if (button_test(done,x,y)) break;
      if (button_test(undo,x,y)) {
        rez_zt(k0:k1,l0:l1)= list(,,1,1);
        rez_rt(k0:k1,l0:l1)= list(,,2,1);
        list(,,,1:-1)= list(,,,2:0);
      } else if (button_test(setr,x,y)) {
        oldjP= jP;
        oldjR= jR;
        lab1.text= "Select R.";
        mode= 2;
      } else if (button_test(smth,x,y)) {
        zt= rez_zt(k0:k1,l0:l1);
        if (dimsof(zt)(2)>2 && dimsof(zt)(3)>2) {
          rt= rez_rt(k0:k1,l0:l1);
          list(,,,2:0)= list(,,,1:-1);
          list(,,1,1)= zt;
          list(,,2,1)= rt;
          zt(2:-1,2:-1)= zt(zcen,zcen)(zcen,zcen);
          rt(2:-1,2:-1)= rt(zcen,zcen)(zcen,zcen);
          rez_zt(k0:k1,l0:l1)= zt;
          rez_rt(k0:k1,l0:l1)= rt;
        }
      } else if (button_test(eqsp,x,y)) {
        zt= rez_zt(k0:k1,l0:l1);
        if (dimsof(zt)(2)>2 && dimsof(zt)(3)>2) {
          rt= rez_rt(k0:k1,l0:l1);
          list(,,,2:0)= list(,,,1:-1);
          list(,,1,1)= zt;
          list(,,2,1)= rt;
          for (ii=0 ; ii<4 ; ii++) {
            zt(2:-1,2:-1)= zt(zcen,zcen)(zcen,zcen);
            rt(2:-1,2:-1)= rt(zcen,zcen)(zcen,zcen);
          }
          rez_zt(k0:k1,l0:l1)= zt;
          rez_rt(k0:k1,l0:l1)= rt;
        }
      } else if (abs(_rez_xvp-x)<=_rez_dxvp ||
                 abs(_rez_yvp-y)<=_rez_dyvp) {
        j= rez_find(m(1), m(2));
        kj= (j-1)%kmax + 1;
        lj= (j-1)/kmax + 1;
        inl= (lj>l0 && lj<l1);
        ink= (kj>k0 && kj<k1);
        zt= rez_zt(k0:k1,l0:l1);
        rt= rez_rt(k0:k1,l0:l1);
        if (j>0 && (kj==k0 || kj==k1) && inl) {
          list(,,,2:0)= list(,,,1:-1);
          list(,,1,1)= zt;
          list(,,2,1)= rt;
          ds= abs(zt(kj==k0,dif),rt(kj==k0,dif))(-,cum);
          ds/= ds(1,0);
          rez_zt(k0:k1,l0:l1)= zt(,1) + (zt(,0)-zt(,1))*ds
          rez_rt(k0:k1,l0:l1)= rt(,1) + (rt(,0)-rt(,1))*ds
        } else if (j>0 && (lj==l0 || lj==l1) && ink) {
          list(,,,2:0)= list(,,,1:-1);
          list(,,1,1)= zt;
          list(,,2,1)= rt;
          ds= abs(zt(dif,lj==l0),rt(dif,lj==l0))(cum);
          ds/= ds(0);
          rez_zt(k0:k1,l0:l1)= zt(1,-,) + (zt(0,-,)-zt(1,-,))*ds
          rez_rt(k0:k1,l0:l1)= rt(1,-,) + (rt(0,-,)-rt(1,-,))*ds
        } else {
          rez_ding;
          continue;
        }
      } else {
        rez_ding;
        continue;
      }
    }
    rez_plm, !mode, 1;
    if (!mode || (jP && jR && kR!=kP && lR!=lP)) {
      done.text= "OK";
      done.dx= 0.020;
    } else {
      done.text= "Cancel";
      done.dx= 0.033;
    }
    button_plot, done, lab1, zorp, sets, setp;
    if (jP) plg, rez_rt(kP,lP), rez_zt(kP,lP),
      marker='P', type=0, legend=string(0);
    if (jR) plg, rez_rt(kR,lR), rez_zt(kR,lR),
      marker='R', type=0, legend=string(0);
    if (jQ) plg, rez_rt(kQ,lQ), rez_zt(kQ,lQ),
      marker='Q', type=0, legend=string(0);
    if (done.text=="OK") {
      if (!jQ) plg, rez_rt(kP,lR), rez_zt(kP,lR),
                 marker='Q', type=0, legend=string(0);
      plg, rez_rt(kR,lP), rez_zt(kR,lP),
        marker='S', type=0, legend=string(0);
    }
    if (!mode) button_plot, undo, setr, drag, smth, eqsp;
    else if (jP || jR) {
      text= jP? swrite(format="P=(%ld,%ld)",kP,lP) : "";
      if (jR) text+= swrite(format="    R=(%ld,%ld)",kR,lR);
      else if (jQ) text+= swrite(format="    Q=(%ld,%ld)",kQ,lQ);
      plt, text, 0.35, 0.375;
    }
  }

  rez_plm, 1;
}

_rez_zoom= 0n;

func rez_zoom
/* DOCUMENT rez_zoom
     Change limits after setting rez_mesh.

     There are two modes of operation:
     (1) P-mode:
         Click on a point to magnify or demagnify about that point.
         Drag the point to move it to a different position in the
         viewport.  The left button zooms in (magnifies by 1.5), the
         right button zooms out (demagnifies by 1.5), and the middle
         button pans (no magnification change).  If you click just
         below or just above the tick marks, only the x coordinate
         is changed; clicking just to the left or just to the right
         of the viewport changes only the y coordinate.
     (2) R-mode:
         Left drag out a rectangle to set the limits to that rectangle.
         Right drag out a rectangle to cram the entire current viewport
         into that rectangle (an analog to zooming out).
         Middle drag to pan as in P-mode (the rubber band rectangle is
         meaningless in this case).

      Click the [R-mode] (or [P-mode]) button to change modes.

      Click the [OK] button when you are satisfied with the limits.

      Click the [Undo] button to remove the effect of the previous
      zoom-click.  Up to 100 successive undo operations are allowed.

      Click the [Polar] button to toggle polar coordinates.  The
      vertical axis will be theta, and the horizontal axis radius.
      The polar coordinate origin will be at (zt,rt) = (0,0).

      Click the [Next], [Prev], or [First] buttons to focus in on
      zones which are bowtied or chevroned.  These zones are marked
      by a B or a C, respectively (if rez_mark is set).

   SEE ALSO: rez_mesh
 */
{
  done= Button(text="OK",x=.203,y=.895,dx=.020,dy=.012);
  undo= Button(text="Undo",x=.254,y=.895,dx=.025,dy=.012);
  lab2= Button(text="Left=in  Middle=pan  Right=out",
               x=.413,y=.895,dx=.128,dy=.012,width=-1);
  mode= Button(text=(_rez_zoom?"P-Mode":"R-Mode"),
               x=.584,y=.895,dx=.037,dy=.012);

  full= Button(text="Full Mesh",x=.230,y=.375,dx=.046,dy=.012);
  polar= Button(text="Polar",x=.307,y=.375,dx=.025,dy=.012);
  lab3= Button(text="Go To Bowtie:",
               x=.402,y=.375,dx=.040,dy=.012,width=-1);
  next= Button(text="Next",x=.486,y=.375,dx=.025,dy=.012);
  prev= Button(text="Prev",x=.536,y=.375,dx=.025,dy=.012);
  first= Button(text="First",x=.587,y=.375,dx=.026,dy=.012);

  rez_plm, 1;
  button_plot, done, undo, lab2, mode, full, polar, lab3, next, prev, first;

  list= array(0.0, 5, 100);  /* keep last 100 limits for undo */
  len= 0;
  for (;;) {
    m= mouse(1, _rez_zoom, "");
    if (is_void(m)) { rdline,prompt=""; continue; }
    x= m(5);
    y= m(6);
    if (button_test(done,x,y)) break;
    if (button_test(undo,x,y)) {
      if (len) {
        limits, list(,1);
        list(,1:-1)= list(,2:0);
        len--;
      } else {
        rez_ding;
        continue;
      }
    } else if (button_test(mode,x,y)) {
      _rez_zoom= !_rez_zoom;
      mode.text= (_rez_zoom?"P-Mode":"R-Mode");
    } else if (button_test(full,x,y)) {
      limits;  redraw;
      continue;
    } else if (button_test(polar,x,y)) {
      rez_toggle;
      limits;
    } else if (button_test(next,x,y)) {
      rez_next;  redraw;
      continue;
    } else if (button_test(prev,x,y)) {
      rez_prev;  redraw;
      continue;
    } else if (button_test(first,x,y)) {
      rez_next, 1;  redraw;
      continue;
    } else if (abs(_rez_xvp-x)<=_rez_dxvp ||
               abs(_rez_yvp-y)<=_rez_dyvp) {
      x= m(1);
      y= m(2);
      x1= m(3);
      y1= m(4);
      button= long(m(10));
      lims= limits();
      if (!_rez_zoom || button==2) {  /* point mode */
        if (button==1) f= 1.5;
        else if (button==2) f= 1.0;
        else f= 1.0/1.5;
        x0= x - (x1-lims(1))/f;
        x1= x - (x1-lims(2))/f;
        y0= y - (y1-lims(3))/f;
        y1= y - (y1-lims(4))/f;
        if (x!=lims(1) && x!=lims(2)) {
          if (y!=lims(3) && y!=lims(4)) limits, x0,x1,y0,y1;
          else limits, x0,x1;
        } else {
          if (y!=lims(3) && y!=lims(4)) limits, ,,y0,y1;
          else limits, x0,x1,y0,y1;
        }
        list(,2:0)= list(,1:-1);
        list(,1)= lims;
        len++;
      } else if (x!=x1 && y!=y1) {  /* rectangle mode */
        x0= min(x,x1);
        x1= max(x,x1);
        y0= min(y,y1);
        y1= max(y,y1);
        if (button!=1) {
          f= (lims(2)-lims(1))/(x1-x0);
          x0= lims(1) - f*(x0-lims(1));
          x1= lims(2) + f*(lims(2)-x1);
          f= (lims(4)-lims(3))/(y1-y0);
          y0= lims(3) - f*(y0-lims(3));
          y1= lims(4) + f*(lims(4)-y1);
        }
        limits, x0,x1,y0,y1;
        list(,2:0)= list(,1:-1);
        list(,1)= lims;
        len++;
      } else {
        rez_ding;
        continue;
      }
    }
    rez_plm, 0;
    button_plot, done, undo, lab2, mode, full, polar, lab3, next, prev, first;
  }
}

func rez_style
{
  done= Button(text="OK",x=.203,y=.895,dx=.020,dy=.012);
  klin= Button(text="K-line",x=.290,y=.895,dx=.038,dy=.012,width=6.);
  llin= Button(text="L-line",x=.366,y=.895,dx=.038,dy=.012);
  type= Button(text="Type",x=.476,y=.895,dx=.025,dy=.012,width=6.);
  wdth= Button(text="Width",x=.530,y=.895,dx=.029,dy=.012);
  colr= Button(text="Color",x=.586,y=.895,dx=.027,dy=.012);

  bar1= ["1", "2", "3", "4", "5", "6", "7", "8"];
  bar2= ["bg", "fg", "blk", "wht",
         "red", "grn", "blu", "cyn", "mag", "yel"];
  bar= array(Button, 10);
  bar.dx= bardx= .022;
  bar.x= barx= .644;
  bar.dy= bardy= .012;
  bar.y= .895-(indgen(10)-1)*2.*bardy;
  bary= avg(bar.y);

  bar(1:5).text= bar1(1:5);
  barn= 5;
  bar(rez_kstyle(1)).width= 6.;
  change= 1;

  for (;;) {
    if (change) {
      i= where(bar.width)(1);
      if (type.width) {
        if (klin.width) rez_kstyle(1)= i;
        else rez_lstyle(1)= i;
      } else if (wdth.width) {
        if (klin.width) rez_kstyle(2)= i;
        else rez_lstyle(2)= i;
      } else if (colr.width) {
        if (klin.width) rez_kstyle(3)= -i;
        else rez_lstyle(3)= -i;
      }
      rez_plm, 0;
      button_plot, done, klin, llin, type, wdth, colr, bar(1:barn);
      change= 0;
    }
    m= mouse(0, _rez_zoom, "");
    if (is_void(m)) { rdline,prompt=""; continue; }
    x= m(5);
    y= m(6);
    if (button_test(done,x,y)) break;
    if (button_test(klin,x,y)) {
      if (klin.width) continue;
      klin.width= 6.;
      llin.width= 0.;
      bar.width= 0.;
      if (type.width) bar(rez_kstyle(1)).width= 6.;
      else if (wdth.width) bar(rez_kstyle(2)).width= 6.;
      else if (colr.width) bar(-rez_kstyle(3)).width= 6.;
    } else if (button_test(llin,x,y)) {
      if (llin.width) continue;
      llin.width= 6.;
      klin.width= 0.;
      bar.width= 0.;
      if (type.width) bar(rez_lstyle(1)).width= 6.;
      else if (wdth.width) bar(rez_lstyle(2)).width= 6.;
      else if (colr.width) bar(-rez_lstyle(3)).width= 6.;
    } else if (button_test(type,x,y)) {
      if (type.width) continue;
      type.width= 6.;
      wdth.width= colr.width= 0.;
      bar(1:5).text= bar1(1:5);
      barn= 5;
      bar.width= 0.;
      if (klin.width) bar(rez_kstyle(1)).width= 6.;
      else bar(rez_lstyle(1)).width= 6.;
    } else if (button_test(wdth,x,y)) {
      if (wdth.width) continue;
      wdth.width= 6.;
      type.width= colr.width= 0.;
      bar(1:8).text= bar1;
      barn= 8;
      bar.width= 0.;
      if (klin.width) bar(rez_kstyle(2)).width= 6.;
      else bar(rez_lstyle(2)).width= 6.;
    } else if (button_test(colr,x,y)) {
      if (colr.width) continue;
      colr.width= 6.;
      type.width= wdth.width= 0.;
      bar.text= bar2;
      barn= 10;
      bar.width= 0.;
      if (klin.width) bar(-rez_kstyle(3)).width= 6.;
      else bar(-rez_lstyle(3)).width= 6.;
    } else if (abs(barx-x)<bardx && abs(bary-y)<2.*barn*bardy) {
      i= long((bar(1).y+bardy - y)/(2.0*bardy)) + 1;
      if (i>0 && i<=barn) {
        bar.width= 0.;
        bar(i).width= 6.;
      } else {
        rez_ding;
        continue;
      }
    } else {
      rez_ding;
      continue;
    }
    change= 1;
  }
}

extern rez_qrt, rez_qzt, rez_qireg;
/* DOCUMENT rez_qrt, rez_qzt, rez_qireg
     are the names of the mesh variables for rez_mesh, "rt", "zt", and
     "ireg" by default.  Note that ireg is optional.
 */
rez_qzt= "zt";
rez_qrt= "rt";
rez_qireg= "ireg";

func rez_mesh(rt, zt, ireg, all=, quiet=)
/* DOCUMENT rez_mesh, rt, zt, ireg
         or rez_mesh, rt, zt
         or rez_mesh, zfile
     set mesh for rez_... commands.  If ZFILE is specified, it must be a
     binary file containing variables RT, ZT, and (optionally) IREG.
   SEE ALSO: rez_all, rez_adjust_mesh
 */
{
  extern rez_rt0, rez_zt0, rez_ireg0, rez_rt, rez_zt, rez_ireg;
  extern rez_mapi;

  if (is_stream(rt)) {
    f= rt;
    vars= get_vars(f);
    ireg= rez_getvar(f,vars, rez_qireg);
    zt= rez_getvar(f,vars, rez_qzt);
    rt= rez_getvar(f,vars, rez_qrt);
  }
  if (is_void(ireg)) {
    ireg= array(1, dimsof(rt));
    ireg(1,)= 0;
    ireg(,1)= 0;
  }

  rez_rt0= rez_rt= rt;
  rez_zt0= rez_zt= zt;
  rez_ireg0= rez_ireg= ireg;
  if (is_void(zt) || is_void(rt)) return;
  rez_ptexist;

  rez_mapi= 0;
  if (!quiet) nbow, rez_rt, rez_zt, rez_ireg, all=all;

  rez_plm, 1;
}

func rez_next(which)
/* DOCUMENT rez_next
     zoom in on next bowtied (or chevroned) zone (after rez_mesh).
     With numeric argument, zooms to that bowtie (or chevron).  With
     numeric argument 0, zooms to current bowtie (or chevron).
   SEE ALSO: rez_prev, rez_mark, rez_drag
 */
{
  list= where(rez_map==0);
  n= where(rez_map==1);
  if (!numberof(list)) list= [];
  if (!numberof(n)) n= [];
  list= grow(list,n);
  n= numberof(list);
  if (is_void(which)) which= rez_mapi+1;
  else if (!which) which= rez_mapi? rez_mapi : 1;
  else if (which<0) which= rez_mapi>1? rez_mapi-1 : n+1;
  if (!n || which>n) { rez_ding; return; }
  local rr, zz;
  rez_getrz, list(which);
  rn= min(rr);  rx= max(rr);
  zn= min(zz);  zx= max(zz);
  rr= rx-rn;
  zz= zx-zn;
  zn-= 0.3*zz;
  zx+= 0.3*zz;
  rn-= 0.3*rr;
  rx+= 0.3*rr;
  limits, zn, zx, rn, rx;
  rez_mapi= which;
}

func rez_prev
/* DOCUMENT rez_prev
     zoom in on previous bowtied (or chevroned) zone (after rez_mesh).
   SEE ALSO: rez_next, rez_mark, rez_drag
 */
{
  rez_next, -1;
}

func rez_getrz(zlist)
{
  extern rr, zz;   /* 2-by-2-by-dimsof(zlist) result */
  if (!numberof(zlist)) {
    rr= zz= [];
    return;
  }
  dims= dimsof(rez_rt);
  kmax= dims(2);
  klist= (zlist-1)%(kmax-1);
  llist= (zlist-1)/(kmax-1);
  list= llist*kmax + klist + 1;  /* lower left corner index */
  rr= zz= array(0.0, 2, 2, dimsof(list));
  rr(1,1,..)= rez_rt(list);
  zz(1,1,..)= rez_zt(list);
  rr(2,1,..)= rez_rt(list+1);
  zz(2,1,..)= rez_zt(list+1);
  rr(1,2,..)= rez_rt(list+kmax);
  zz(1,2,..)= rez_zt(list+kmax);
  rr(2,2,..)= rez_rt(list+kmax+1);
  zz(2,2,..)= rez_zt(list+kmax+1);
}

func rez_ptexist
{
  extern rez_ptx, rez_ptlist, rez_ireg;
  rez_ireg(1,)= 0;
  rez_ireg(,1)= 0;
  d= dimsof(rez_ireg);
  ix= d(2);
  jx= d(3);
  n= numberof(rez_ireg);
  ireg= array(0, n+ix+1);
  ireg(1:n)= rez_ireg(*);
  rez_ptx= array(0, dimsof(rez_ireg));
  rez_ptx(*)=
    (ireg(1:n) | ireg(2:n+1) | ireg(ix+1:n+ix) | ireg(ix+2:n+ix+1));
  rez_ptlist= where(rez_ptx);
}

extern _rez_center;

func rez_find(x, y)
{
  extern _rez_center;
  d= abs(rez_zt-x, rez_rt-y)(rez_ptlist);
  j= d(mnx);
  d0= d(j);
  j0= rez_ptlist(j);
  j= where(!(d-d0));  /* detect center points */
  if (numberof(j)>1) _rez_center= rez_ptlist(j);
  else _rez_center= [];
  return j0;
}

func rez_getvar(f, vars, name)
{
  v= vars(2);
  if (v && anyof(*v == name)) return get_member(f,name);
  v= vars(1);
  if (v && anyof(*v == name)) return get_member(f,name);
  return [];
}

func rez_range(x) { return x; }  /* parser quirk */

func rez_ding
{
  write, format="%s", "\a\a";
}

func rez_solve(ss, nn)
/* xxDOCUMENT adapted from gseries_r(s, n) in series.i
     returns the ratio r of the finite geometric series, given the sum s:
        1 + r + r^2 + r^3 + ... + r^(nn-1) = ss,
        or (r^nn-1)/(r-1) = ss

     Altered to solve only nn>0.0, but allows any real nn.
 */
{
  /* compute an approximate result which has exact values and
     derivatives at s==1, s==n, and s->infinity --
     different approximations apply for s>n and s<n */
  if (nn==2) return ss-1.0;
  if (ss>nn) {
    pow= 1.0/(nn-1.0);
    npow= nn^pow - 1.0;
    n2r= 1.0/(nn-2.0);
    A= (2.0-nn*npow)*n2r;
    B= (2.0*npow-nn*pow)*nn*n2r;
    r= ss^pow - pow + A*(nn/ss)^pow + B/ss;
  } else {
    sn= (ss-1.0)/(nn-1.0);
    n2r= 1.0/(nn*nn);
    r= 1.0 - 1.0/ss + n2r*sn*sn*(nn+1.0 - sn);
  }

  /* Polish the approximation using Newton-Raphson iterations.  */
  if (ss!=nn) {
    for (;;) {
      rr= r-1.0;
      rn= r^(nn-1);
      rrss= rr*ss;
      delta= rrss - (r*rn-1.0);
      if (abs(delta)<=1.e-9*abs(rrss)) break;
      r+= delta/(nn*rn-ss);
    }
    /* try to get it to machine precision */
    if (delta) r+= delta/(nn*rn-ss);
  }

  return r;
}

func rez_test
{
  extern rez_rt, rez_zt;
  x= span(0,1,10)(,-:1:7);  y= span(0,.5,7)(-:1:10,);
  rez_mesh, x+y, x-y+.5;
}

if (!is_void(rez_load_hook)) include, rez_load_hook;
