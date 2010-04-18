/*
 * $Id: button.i,v 1.2 2010-04-18 10:33:38 thiebaut Exp $
 * Implement buttons in Yorick graphics window.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* ------------------------------------------------------------------------ */

struct Button {
  double x, y;      /* NDC coordinates of button center */
  double dx, dy;    /* button half widths in NDC */
  string text;      /* button text */
  string font;      /* text font (0 for helvetica) */
  double height;    /* text height */
  double width;     /* width of line around button (0 is 1.0, <0 no box) */
}

func button_build(button, which)
/* DOCUMENT button_build(button)
         or button_build(button, which)
     Returns a Button structure instance, modified interactively to be at
     the correct position and to have the correct box half widths, e.g.:

        button= button_build(Button(text="label",y=initial_y))

     You can either drag the center of the button to a new location
     (press down near the center of the button, move the pointer to
     where you want the center, and release at the new center point),
     or press the "Set Box" or "Done" button.  In the "Set Box" mode,
     you can either drag a new box over the button, or press "Set Center"
     (to return to the original mode) or "Done" button.

     Yorick has no way to determine the size of a text string produced
     by the plt command, which is why you need to be able to adjust
     the size of the box draawn around the text.  The idea is to use
     button_build to get the buttons where you like, then put those
     coordinates into the include file for the mouse-driven function
     you are writing.

     Also, the input BUTTON may be an array of buttons, and BUTTON(WHICH)
     will be the one that is modified.  WHICH defaults to 1.  By using an
     array of buttons, you can see all the other buttons in a group while
     you adjust one.

   SEE ALSO: Button, button_test, button_plot
 */
{
  if (is_void(button)) button= Button();
  if (is_void(which)) which= 1;
  x= button(which).x;     y= button(which).y;
  dx= button(which).dx;   dy= button(which).dy;
  text= button(which).text;
  h= button(which).height;
  if (h<=0) h= 14.0;

  while (!strlen(text)) read, text, prompt="Button text: ";
  len= strlen(text);
  button(which).text= text;

  if (x<=0.) x= 0.22;
  if (y<=0.) y= 0.915;
  if (dx<=0.) dx= 0.0013*h*0.6*len / 2.0;
  if (dy<=0.) dy= 0.0013*h / 2.0;
  if (dx<0.005) dx= 0.005;
  if (dy<0.005) dy= 0.005;
  button(which).x= x;     button(which).y= y;
  button(which).dx= dx;   button(which).dy= dy;

  done= Button(text="Done",x=.45,y=.65,dx=.025,dy=.012);
  switch= Button(text="Set Box",x=.35,y=.65,dx=.035,dy=.012);
  mode= 0;
  for (;;) {
    fma;
    limits;
    plg, 0, 0, type=0, mcolor="bg";
    if (!mode) text= "Drag center of button to desired location";
    else if (mode==1) text= "Drag out desired button box";
    else text= "Final button and box";
    plt, text, .395,.7, justify="CH";
    if (mode!=2) {
      button_plot, switch;
      button_plot, done;
    }
    button_plot, button;
    if (mode==2) break;
    xy= mouse(0, mode, "");
    if (is_void(xy)) {
      write, format="%s", "\a\a\a";
      continue;
    }
    x= xy(1);
    y= xy(2);
    if (button_test(done,x,y)) {
      mode= 2;
    } else if (button_test(switch,x,y)) {
      if (mode) { switch.text= "Set Box"; switch.dx= .035; }
      else { switch.text= "Set Center"; switch.dx= .045; }
      mode= 1-mode;
    } else if (mode) {
      dx= abs(xy(3)-x)*0.5;
      dy= abs(xy(4)-y)*0.5;
      x= (xy(3)+x)*0.5;
      y= (xy(4)+y)*0.5;
      if (dx>0.005 && dy>0.005 && abs(x-button(which).x)<dx &&
          abs(y-button(which).y)<dy) {
        button(which).dx= dx;
        button(which).dy= dy;
      } else {
        write, format="%s", "\a\a\a";
      }
    } else {
      if (button_test(button(which),x,y)) {
        button(which).x+= xy(3)-x;
        button(which).y+= xy(4)-y;
      } else {
        write, format="%s", "\a\a\a";
      }
    }
  }

  return button;
}

func button_plot(..)
/* DOCUMENT button_plot, button1, button2, ...
     plot the specified BUTTONs.  Each button in the list may be an array
     of Button structs.  Void arguments are no-ops.
   SEE ALSO: Button, button_build, button_test
 */
{
  while (more_args()) {
    button= next_arg();
    for (i=1 ; i<=numberof(button) ; i++) {
      font= button(i).font;
      h= button(i).height;
      if (!font) font= "helvetica";
      if (h<=0.) h= 14.0;
      x= button(i).x;
      y= button(i).y;
      plt, button(i).text, x, y, justify="CH", font=font, height=h, opaque=1;
      dx= button(i).dx;
      dy= button(i).dy;
      w= button(i).width;
      if (w==0.) w= 1.0;
      if (w>0.) {
        plsys, 0;
        plg, [y-dy,y-dy,y+dy,y+dy],[x-dx,x+dx,x+dx,x-dx],
          closed=1,width=w,type=1,marks=0;
        plsys, 1;
      }
    }
  }
}

func button_test(button, x, y)
/* DOCUMENT button_test(button, x, y)
     true if the BUTTON contains NDC coordinates (X,Y).
   SEE ALSO: Button, button_build, button_plot
 */
{
  return abs(y-button.y)<button.dy && abs(x-button.x)<button.dx;
}

/* ------------------------------------------------------------------------ */
