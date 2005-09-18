/*
 * $Id: testg.i,v 1.1 2005-09-18 22:06:12 dhmunro Exp $
 * This file runs mesh plot movies. It should produce the same
 * results as the grbench C program.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func testg
/* DOCUMENT testg
     runs a Yorick near-equivalent of Steve Langer's grbench graphics
     timing benchmark.
   SEE ALSO: lissajous, grtest, txtest
 */
{
  /* set parameters */
  NMID= 26;
  N= 2*NMID-1;
  xmax= ymax= 1.0;
  alpha= 0.5;
  dphi= 2.0*pi/8.3;
  npass= 50;
  ampy= cos(span(0.0, pi, N));

  /* set the maximum color index to use */
  /* maxcolr= 192;
     200 is the Yorick default -- grbench uses 192...
     Could use pldefault, maxcolors=192 to be sure.   */

  /* create room for the x-coordinates of the mesh */
  xmesh= array(double,N,N);
  /* the y-coordinates of the mesh are fixed */
  ymesh= span(0.0,ymax,N)(-:1:N,);

  /* be sure expose event is handled before starting into plots --
     otherwise nothing will show */
  if(!is_void(is_a_mac)) {
    /* use existing window if on a Mac */
  } else {
    winkill, 0;
    window, 0, wait=1, style="nobox.gs", dpi=100;
  }

  if (is_void(dont_plot) || !dont_plot) {
    write, "\nEach of the six tests will run no more than one minute,";
    write, "so just be patient if you have a slow terminal.\n";
    do_hollow, 0;
    use_pli= 1;
    do_filled, 0;
    use_pli= 0;
    do_filled, 0;
    do_hollow, 1;
    use_pli= 1;
    do_filled, 1;
    use_pli= 0;
    do_filled, 1;
  }
}

/* do a hollow mesh */
func do_hollow(animation)
{
  if (animation) animate, 1;
  else animate, 0;
  fma;

  write, format="%d frames of hollow %d-by-%d mesh %s\n",\
    npass, N, N, (animation? " animated" : " direct");
  now= yorick= gist= array(0.0, 3);
  timer, now;
  now0= now(3);

  phi= -dphi;
  for(np=1 ; np<=npass ; np++) {
    phi= phi+dphi;
    xdif= alpha*sin(phi);
    xmj= 0.5*xmax*(1.0+xdif*ampy);

    for(j=1; j<=N; j++) {
      xmesh(1:NMID,j)= span(0.0, xmj(j), NMID);
      xmesh(NMID:N,j)= xmj(j)+span(0.0, xmax-xmj(j), NMID);
    }

    timer, now, yorick;
    plm, ymesh, xmesh;
    fma;
    timer, now, gist;
    if (now(3)-now0 > 60.) {
      write, "aborting after one minute, "+pr1(np)+" frames";
      break;
    }
  }

  timer_print, "Yorick interpreter", yorick, "Gist graphics", gist,
    "Yorick+Gist total", yorick+gist;
  write, format="Frames per second (total/gist)=%.3f/%.3f\n",
    np/(yorick(3)+gist(3)),np/gist(3);

  if (animation) animate, 0;
}

/* do a filled mesh */
func do_filled(animation)
{
  if (animation) animate, 1;
  else animate, 0;
  fma;

  if (is_void(use_pli) || !use_pli)
    write, format="%d frames of filled %d-by-%d mesh %s\n",
      npass, N, N, (animation? " animated" : " direct");
  else
    write, format="%d frames of %d-by-%d cell array %s\n",
      npass, N, N, (animation? " animated" : " direct");
  now= yorick= gist= array(0.0, 3);
  timer, now;
  now0= now(3);

  dxmin= xmax*(1.0-alpha)/(2.0*NMID);
  dxmax= xmax*(1.0+alpha)/(2.0*NMID);
  phi= -dphi;
  for(np=1 ; np<=npass ; np++) {
    phi= phi+dphi;
    xdif= alpha*sin(phi);
    xmj= 0.5*xmax*(1.0+xdif*ampy);

    for(j=1; j<=N; j++) {
      xmesh(1:NMID,j)= span(0.0, xmj(j), NMID);
      xmesh(NMID:N,j)= xmj(j)+span(0.0, xmax-xmj(j), NMID);
    }

    timer, now, yorick;
    if (is_void(use_pli) || !use_pli)
      plf, xmesh(dif,2:N),ymesh,xmesh, cmin=dxmin,cmax=dxmax;
    else
      pli, xmesh(dif,2:N), cmin=dxmin,cmax=dxmax;
    fma;
    timer, now, gist;
    if (now(3)-now0 > 60.) {
      write, "aborting after one minute, "+pr1(np)+" frames";
      break;
    }
  }
  timer, now, gist;

  timer_print, "Yorick interpreter", yorick, "Gist graphics", gist,
    "Yorick+Gist total", yorick+gist;
  write, format="Frames per second (total/gist)= %.3f/%.3f\n",
    np/(yorick(3)+gist(3)),np/gist(3);

  if (animation) animate, 0;
}

#if 0
Results 75 MHz Pentium / Linux 1.1.87 / XF86_Mach64 server
($3000 from Gateway 2000 Jan/95 lets you animate nicely)
50 frames of hollow 51-by-51 mesh  direct
               Timing Category     CPU sec  System sec    Wall sec
            Yorick interpreter       0.800       0.000       0.800
                 Gist graphics       1.150       0.130       2.760
             Yorick+Gist total       1.950       0.130       3.560
Frames per second (total/gist)=14.326/18.478
50 frames of filled 51-by-51 mesh  direct
               Timing Category     CPU sec  System sec    Wall sec
            Yorick interpreter       0.820       0.010       0.830
                 Gist graphics       2.500       0.350      11.960
             Yorick+Gist total       3.320       0.360      12.790
Frames per second (total/gist)= 3.987/4.264
50 frames of hollow 51-by-51 mesh  animated
               Timing Category     CPU sec  System sec    Wall sec
            Yorick interpreter       0.870       0.010       0.920
                 Gist graphics       1.130       0.140       3.480
             Yorick+Gist total       2.000       0.150       4.400
Frames per second (total/gist)=11.591/14.655
50 frames of filled 51-by-51 mesh  animated
               Timing Category     CPU sec  System sec    Wall sec
            Yorick interpreter       0.860       0.000       0.860
                 Gist graphics       2.480       0.330       7.650
             Yorick+Gist total       3.340       0.330       8.510
Frames per second (total/gist)= 5.993/6.667
#endif

func lissajous(animation)
/* DOCUMENT lissajous
     runs the Yorick equivalent of an old graphics performance test
     used to compare PLAN, ALMA, and Basis with LTSS TMDS graphics.
   SEE ALSO: testg, grtest
 */
{
  /* Two figures with (x,y)= (cx,cy) + size*(cos(na*t), sin(nb*t+phase))
     -- the centers describe semi-circular arcs of radius rc.  */
  t= span(0, 2*pi, 400);
  na1= 1;    nb1= 5;
  na2= 2;    nb2= 7;
  rc1= 40.;  rc2= 160.;
  size= 40.;
  phase= theta= 0.;

  n= 50;   /* number of frames in animation */
  dtheta= pi/(n-1);
  dphase= 2*pi/(n-1);

  window, 0, wait=1;
  if (animation) animate, 1;
  else animate, 0;
  fma;

  split= now= array(0.0, 3);
  timer, now;

  for (i=0 ; i<n ; i++) {
    cost= cos(theta);
    sint= sin(theta);
    x= rc1*cost+size*cos(na1*t);  y= rc1*sint+size*sin(nb1*t+phase);
    plg, y, x;
    x= rc2*cost+size*cos(na2*t);  y= rc2*sint+size*sin(nb2*t+phase);
    plg, y, x;
    fma;
    theta+= dtheta;
    phase+= dphase;
  }

  timer, now, split;
  timer_print, "Lissajous test", split;
  write,"Frames per wall clock second=",n/(split(3)+1.0e-6);

  if (animation) {
    /* turn off animation and pop up final frame again */
    animate, 0;
    x= -rc1+size*cos(na1*t);  y= size*sin(nb1*t);
    plg, y, x;
    x= -rc2+size*cos(na2*t);  y= size*sin(nb2*t);
    plg, y, x;
  }
}

func gw(msg)
{
  if (!quick) {
    write, msg;
    return strtok(rdline(prompt=""))(1)=="q";
  } else {
    pause, 1000;
  }
}

func grtest(nstart,quick=)
/* DOCUMENT grtest
         or grtest, nstart
     Perform a comprehensive test of Yorick's graphics package.
     Pauses after each frame to let you check the result.  Each
     picture attempts to describe itself (of course, if text
     plotting is broken, this doesn't do any good).
     With NSTART, start with test number NSTART.
   SEE ALSO: testg, lissajous
 */
{
  if (is_void(nstart)) nstart= 0;
  if (batch()) quick = 1;
  write, "Yorick comprehensive graphics test.";
  write, "Each frame will be described at the terminal.";
  if (!quick) {
    write, "Compare what you see with the description, then";
    write, "hit <RETURN> to see the next test, or q <RETURN> to quit.\n";
  }
  pldefault, marks=1, width=0, type=1, style="work.gs", dpi=75;
  winkill, 0;
  if (nstart <= 1) {
    write, "Test 1     Commands: window, 0, wait=1, dpi=100; plg, [0,1]";
    window, 0, wait=1, dpi=100;
    plg, [0,1];
    if (gw("A large (100 dpi) window with line marked A from (1,0) to (2,1)."))
      return;
    winkill, 0;
  }
  window, 0, wait=1;
  if (nstart <= 2) {
    write, "Test 2     Commands: window, 0, wait=1; plg, [0,1]";
    plg, [0,1];
    if (gw("A small (75 dpi) window with line marked A from (1,0) to (2,1)."))
      return;
    unzoom;
  } else {
    plg, [0,1];
  }
  if (nstart <= 3) {
    write, "Test 3     Commands: plg, [1,0]";
    plg, [1,0];
    if (gw("Added line marked B from (1,1) to (2,0) to previous plot."))
      return;
    unzoom;
  } else {
    plg, [1,0];
  }
  if (nstart <= 4) {
    write, "Test 4     Commands: logxy, 1, 0";
    logxy, 1, 0;
    if (gw("X axis now a log scale."))
      return;
    unzoom;
  }
  if (nstart <= 5) {
    write, "Test 5     Commands: logxy, 0, 0";
    logxy, 0, 0;
    if (gw("X axis back to linear scale."))
      return;
    unzoom;
  }
  limits, 1.2, 1.8, 0.2, 0.8;
  if (nstart <= 6) {
    write, "Test 6     Commands: limits, 1.2, 1.8, 0.2, 0.8";
    limits, 1.2, 1.8, 0.2, 0.8;
    if (gw("Limits changed to 1.2<x<1.8, 0.2<y<0.8."))
      return;
    unzoom;
  }
  range, 0.4, 0.6;
  if (nstart <= 7) {
    write, "Test 7     Commands: range, 0.4, 0.6";
    if (gw("Limits changed to 1.2<x<1.8, 0.4<y<0.6."))
      return;
    unzoom;
  }
  limits;
  if (nstart <= 8) {
    write, "Test 8     Commands: limits";
    if (gw("Limits back to extreme values (1,0) to (2,1)."))
      return;
    unzoom;
  }
  fma;
  x= span(0, 10*pi, 200);
  plg, sin(x), x;
  if (nstart <= 9) {
    write, "Test 9     Commands: fma; plg, sin(x), x";
    write, "Five cycles of a sine wave on a new frame.";
    write, "Before you continue, try clicking with the mouse buttons:";
    write, "Left button zooms in, right button zooms out, middle no zoom";
    write, "In each case, the point where you press the mouse button will";
    write, "be translated to the point where you release the mouse button.";
    halt=gw("To zoom one axis only, click over the tick marks on that axis.");
    unzoom;
    if (halt) return;
  }
  pledit, marks=0, width=6, type="dash";
  if (nstart <= 10) {
    write, "Test 10     Commands: pledit, marks=0, width=6, type=\"dash\"";
    if (gw("Marker A on sine curve disappears, curve becomes bold dashed."))
      return;
    unzoom;
  }
  fma;
  x= span(0, 2*pi, 200);
  for (i=1 ; i<=6 ; i++) {
    r= 0.5*i - (5-0.5*i)*cos(x);
    plg, r*sin(x), r*cos(x), marks=0, color=-4-i;
  }
  if (nstart <= 11) {
    write, "Test 11     Commands: plg, r*sin(x), r*cos(x), color=-4-i";
    if (gw("A set of nested cardioids in the primary and secondary colors."))
      return;
    unzoom;
  }
  pltitle, "Nested Cardioids";
  if (nstart <= 12) {
    write, "Test 12     Commands: pltitle, \"Colored nested cardioids\"; plq";
    plq;
    write, "Adds the title above the upper tick marks.";
    if (gw("Also prints legends for the six curves at the terminal (plq)."))
      return;
    unzoom;
  }
  if (nstart <= 13) {
    write, "Test 13     Commands: pledit, color=\"fg\", type=0, marker=i";
    for (i=1 ; i<=5 ; i++) pledit,i, color="fg", type=0, marker=char(i);
    pledit,i, color="fg", type=0, marker='A';
    write, "Changes the colors to foreground, types to no lines.";
    if (gw("Markers are point, plus, asterisk, O, X, A."))
      return;
    unzoom;
  }
  if (nstart <= 14) {
    write, "Test 14     Commands: pledit, marks=0, type=i";
    for (i=1 ; i<=5 ; i++) pledit,i, marks=0, type=i;
    pledit,i, color="fg", type=1, width=4;
    write, "Changes line types to solid, dash, dot, dashdo, dashdotdot.";
    if (gw("Outermost cardioid becomes a thick, solid line."))
      return;
    unzoom;
  }
  fma;
  limits;
  x= span(-1, 1, 26)(,-:1:26);
  y= transpose(x);
  z= x+1i*y;
  z= 5.*z/(5.+z*z);
  xx= z.re;
  yy= z.im;
  if (nstart <= 15) {
    write, "Test 15     Commands: plm, y, x";
    plm, yy, xx;
    if (gw("Quadrilateral mesh -- round with bites out of its sides."))
      return;
    fma;
    unzoom;
  }
  plmesh, yy, xx;
  if (nstart <= 16) {
    write, "Test 16     Commands: plv, v, u, y, x";
    plv, x+.5, y-.5;
    if (gw("Velocity vectors.  Try zooming and panning with mouse."))
      return;
    fma;
    unzoom;
  }
  if (nstart <= 17) {
    write, "Test 17     Commands: plc, z, y,x; plm, y,x, boundary=1, type=2";
    plc, abs(x+.5,y-.5), marks=1;
    plm, boundary=1, type=2;
    if (gw("Contours A-H, with mesh boundary dashed."))
      return;
    fma;
    unzoom;
  }
  if (nstart <= 18) {
    write, "Test 18     Commands: plf, zncen(z), y,x;  plc, z, y,x";
    z= abs(x+.5,y-.5);
    plf, zncen(z);
    plc, z, marks=0, type=2, color="bg", levs=[0.5, 1.0, 1.5];
    if (gw("Filled mesh (color only) with three dashed contours overlayed."))
      return;
    fma;
    unzoom;
  }
  if (nstart <= 19) {
    write, "Test 19     Commands: palette, \"<various>.gp\"";
    write, "After each new palette is installed, hit <RETURN> for the next,";
    write, "or q<RETURN> to begin test 20.  There are 6 palettes altogether.";
    z= abs(x+.5,y-.5);
    plf, zncen(z);
    plc, z, marks=0, type=2, color="bg", levs=[0.5, 1.0, 1.5];
    pal= ["heat.gp", "stern.gp", "rainbow.gp",
          "gray.gp", "yarg.gp", "earth.gp"];
    for (i=1 ; i<=6 ; i++) {
      palette, pal(i);
      if (gw(swrite(format="Palette name: %s  ",pal(i)))) break;
    }
    if (i<6) palette, "earth.gp";
    fma;
    unzoom;
  }
  if (nstart <= 20) {
    write, "Test 20     Commands: window, style=\"<various>.gs\"";
    write, "After each new style is installed, hit <RETURN> for the next,";
    write, "or q<RETURN> to begin test 21.  There are 5 styles altogether.";
    pal= ["vg.gs", "boxed.gs", "vgbox.gs", "nobox.gs", "work.gs"];
    for (i=1 ; i<=5 ; i++) {
      window, style=pal(i);
      plc, abs(x+.5,y-.5), marks=1;
      plm, boundary=1, type=2;
      if (gw(swrite(format="Style name: %s  ",pal(i)))) break;
    }
    if (i<6) window, style="work.gs";
    fma;
    unzoom;
  }
  if (nstart <= 21) {
    write, "Test 21     Commands: pli, image";
    x= span(-6,6,200)(,-:1:200);
    y= transpose(x);
    r= abs(y,x);
    theta= atan(y,x);
    funky= cos(r)^2*cos(3*theta);
    pli, funky;
    if (gw("Cell array image (color only).  Three cycles in theta, r."))
      return;
    fma;
    unzoom;
  }
  if (nstart <= 22) {
    write, "Test 22     Commands: pldj, x0, y0, x1, y1";
    theta= span(0, 2*pi, 18)(zcen);
    x= cos(theta)(,-:1:17);
    y= sin(theta)(,-:1:17);
    pldj, x, y, transpose(x), transpose(y);
    pltitle, "Seventeen Pointed Stars"
    limits, square= 1;
    halt= gw("All 17 pointed stars.");
    limits, square= 0;
    fma;
    unzoom;
    if (halt) return;
  }
  if (nstart <= 23) {
    write, "Test 23     Commands: plfp, z, y, x, n";
    n= (indgen(2:4)+indgen(3)(-,))(*);
    x0= [-2.,0.,2.](,-:1:3)(*);
    y0= [-2.,0.,2.](-:1:3,)(*);
    list= histogram(n(cum)+1)(psum:1:-1);  /* print it! */
    phase= indgen(numberof(list))-n(cum:1:-1)(list);
    theta= 2.*pi*phase/n(list);
    x= cos(theta) + x0(list);
    y= sin(theta) + y0(list);
    plfp, x0-y0, y, x, n, cmin=-4.5,cmax=4.5;
    pltitle, "Three rows of three polygons"
    limits;
    halt= gw("Three rows of three polygons.");
    fma;
    unzoom;
    if (halt) return;
  }
  if (nstart <= 24) {
    write, "Test 24     Commands: plfc, z, y,x; plm, y,x, boundary=1, type=2";
    x= span(-1, 1, 26)(,-:1:26);
    y= transpose(x);
    plfc, abs(x+.5,y-.5), yy, xx;
    plm, boundary=1, type=2;
    if (gw("Filled contours, with mesh boundary dashed."))
      return;
    fma;
    unzoom;
  }
}

func txtest(nstart,quick=)
/* DOCUMENT txtest
            txtest, n
     Print some tests of Yorick's plt command.  Start with the nth
     page in the second form.
 */
{
  if (is_void(nstart)) nstart= 0;
  if (batch()) quick = 1;
  write, "Yorick comprehensive plt command test.";
  write, "Each frame will be described at the terminal.";
  if (!quick) {
    write, "Compare what you see with the description, then";
    write, "hit <RETURN> to see the next test, or q <RETURN> to quit.\n";
    write, "Enter name of hardcopy file or <RETURN> if none:\n";
    name= strtok(rdline(prompt="Hardcopy file: "))(1);
  }
  pldefault, marks=1, width=0, type=1, color="fg", style="work.gs";
  winkill, 0;
  window, 0, wait=1, dpi=75, legends=0;
  if (name) {
    hcp_finish;
    hcp_file, name;
    hcpon;
  } else {
    hcpoff;
  }
  l1= "A text line!";
  l2= "Longer line of text.";
  l3= "sub_scri!pt_ su!per^scri!pt^";
  l4= "10^23";
  l5= "!pr^2^ and G_!s!n_=8!pT_!s!n";
  x0= [.26,.40,.54](,-:1:3);
  y0= [.74,.60,.46](-:1:3,);
  xpat0= [-.01,0.] + x0(-,,);
  xpat1= [.01,0.] + x0(-,,);
  ypat0= [0.,-.01] + y0(-,,);
  ypat1= [0.,.01] + y0(-,,);
  just1= ["R","C","L"]+["A","H","C"](-,);
  just2= ["R","C","L"]+["B","H","T"](-,);
  if (nstart <= 1) {
    write, "Test 1     Justification right|center|left cap|half|base";
    plsys, 0;
    pldj, xpat0,ypat0,xpat1,ypat1, color="blue";
    for (i=1 ; i<=9 ; ++i) plt, l1, x0(i),y0(i), justify=just1(i);
    redraw;
    if(quick)pause,1000;else halt= (strtok(rdline(prompt=""))(1)=="q");
    fma;
    if (halt) goto done;
  }
  if (nstart <= 2) {
    write, "Test 2     Justification right|center|left top|half|bottom";
    plsys, 0;
    pldj, xpat0,ypat0,xpat1,ypat1, color="blue";
    for (i=1 ; i<=9 ; ++i) plt, l1, x0(i),y0(i), justify=just2(i);
    redraw;
    if(quick)pause,1000;else halt= (strtok(rdline(prompt=""))(1)=="q");
    fma;
    if (halt) goto done;
  }
  if (nstart <= 3) {
    write, "Test 3     multiline version, short line first";
    plsys, 0;
    pldj, xpat0,ypat0,xpat1,ypat1, color="blue";
    for (i=1 ; i<=9 ; ++i) plt, l1+"\n"+l2, x0(i),y0(i), justify=just1(i);
    redraw;
    if(quick)pause,1000;else halt= (strtok(rdline(prompt=""))(1)=="q");
    fma;
    if (halt) goto done;
  }
  if (nstart <= 4) {
    write, "Test 4     multiline version, long line first";
    plsys, 0;
    pldj, xpat0,ypat0,xpat1,ypat1, color="blue";
    for (i=1 ; i<=9 ; ++i) plt, l2+"\n"+l1, x0(i),y0(i), justify=just1(i);
    redraw;
    if(quick)pause,1000;else halt= (strtok(rdline(prompt=""))(1)=="q");
    fma;
    if (halt) goto done;
  }
  if (nstart <= 5) {
    write, "Test 5     escape sequences, cap|half|base";
    plsys, 0;
    pldj, xpat0,ypat0,xpat1,ypat1, color="blue";
    for (i=1 ; i<=9 ; ++i) plt, l3, x0(i),y0(i), justify=just1(i);
    redraw;
    if(quick)pause,1000;else halt= (strtok(rdline(prompt=""))(1)=="q");
    fma;
    if (halt) goto done;
  }
  if (nstart <= 6) {
    write, "Test 6     escape sequences, top|half|bottom";
    plsys, 0;
    pldj, xpat0,ypat0,xpat1,ypat1, color="blue";
    for (i=1 ; i<=9 ; ++i) plt, l3, x0(i),y0(i), justify=just2(i);
    redraw;
    if(quick)pause,1000;else halt= (strtok(rdline(prompt=""))(1)=="q");
    fma;
    if (halt) goto done;
  }
  if (nstart <= 7) {
    write, "Test 7     multiline escape sequences";
    plsys, 0;
    pldj, xpat0,ypat0,xpat1,ypat1, color="blue";
    for (i=1 ; i<=9 ; ++i) plt, l4+"\n"+l5, x0(i),y0(i), justify=just1(i);
    redraw;
    if(quick)pause,1000;else halt= (strtok(rdline(prompt=""))(1)=="q");
    fma;
    if (halt) goto done;
  }
  if (nstart <= 8) {
    write, "Test 8     multiline escape sequences part 2";
    plsys, 0;
    pldj, xpat0,ypat0,xpat1,ypat1, color="blue";
    for (i=1 ; i<=9 ; ++i) plt, l5+"\n"+l4, x0(i),y0(i), justify=just1(i);
    redraw;
    if(quick)pause,1000;else halt= (strtok(rdline(prompt=""))(1)=="q");
    fma;
    if (halt) goto done;
  }
  if (nstart <= 9) {
    write, "Test 9     text orientation";
    plsys, 0;
    pldj, [.2,.4],[.6,.4],[.6,.4],[.6,.8], color="blue";
    for (i=0 ; i<=3 ; ++i) plt, "Hello", .4,.6, orient=i, justify="LB";
    redraw;
    if(quick)pause,1000;else halt= (strtok(rdline(prompt=""))(1)=="q");
    fma;
    if (halt) goto done;
  }
done:
  hcpoff;
}
