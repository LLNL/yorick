/*
 * $Id: pixels.i,v 1.1 2005-09-18 22:06:03 dhmunro Exp $
 * Attempt to plot images on an X display one pixel to the cell.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func pix_window(dpi, n)
/* DOCUMENT pix_window, dpi
         or pix_window, dpi, n
     create a new window for the pixels command with the given DPI
     (dots per inch).  If N is specified, the new window will be
     number N (0-7).  Also sets the pix_dpi variable appropriately.

     pix_window, 75           // makes a small window
     pix_window, 100          // makes a large window

   SEE ALSO: pix_dpi, pixels, window
 */
{
  dpi= (dpi<87.5? 75 : 100);
  if (is_void(n)) window, dpi=dpi;
  else window, n, dpi=dpi;
  pix_dpi= dpi;
}

func pixels(z,dx0,dy0,top=,cmin=,cmax=)
/* DOCUMENT pixels, z
         or pixels, z, dx0, dy0
     plots the image Z as a cell array -- an array of equal rectangular
     cells colored according to the 2-D array Z.  The first dimension
     of Z is plotted along x, the second dimension is along y.
     If Z is of type char, it is used "as is", otherwise it is linearly
     scaled to fill the current palette, as with the bytscl function.
     (See the bytscl function for explanation of top, cmin, cmax.)

     The image is placed in "coordinate system zero"; that is, outside
     Yorick's ordinary coordinate system, so zooming and coordinate
     system changes will not effect it.  Unlike pli, Yorick attempts
     to make each X pixel correspond to one cell of the Z array.
     In order to do this, the pix_dpi variable must be set to the
     dots-per-inch (either 75 or 100) of the X window in which the
     result of pixels will be displayed (see the dpi keyword of the
     window command).

     The default position of the upper left hand corner of the picture
     is specified by the pix_origin variable.  If DX0 and/or DY0 are
     present, they adjust this origin for this image.  The units of
     DX0 and DY0 are in pixels; DY0 is positive downwards.  (However,
     the 2nd index of the image increases upwards.)  Resizing the X
     window will probably necessitate changing pix_origin.

     The following keywords are legal (each has a separate help entry):
   KEYWORDS: top, cmin, cmax
   SEE ALSO: pix_window, window, palette, bytscl, histeq_scale
             pix_dpi, pix_origin
 */
{
  ny= dimsof(z);
  nx= ny(2);
  ny= ny(3);
  scale= 0.0013*72.27/(pix_dpi<87.5? 75.0 : 100.0);

  if (is_void(dx0)) dx0= 0.0;
  if (is_void(dy0)) dy0= 0.0;
  dx0*= scale;
  dy0*= -scale;
  dx0+= pix_origin(1);
  dy0+= pix_origin(2);

  plsys, 0;
  pli, z, dx0,dy0-ny*scale,dx0+nx*scale,dy0, top=top,cmin=cmin,cmax=cmax;
  plsys, 1;
  redraw;   /* yes, there is a bug with detecting updates when things
               in coordinate system 0 change... */
}

local pix_dpi;
/* DOCUMENT pix_dpi= 75
         or pix_dpi= 100
     set the number of dots per inch for the pixels function.
     X displays are either 75 dpi (default) or 100 dpi in Yorick.
     The pix_dpi number must match the dpi of the window in which
     the pixels command is to be issued; the other number will
     result in blurred images.

   SEE ALSO: pixels, pix_window, window
 */
pix_dpi= 75;

local pix_origin;
/* DOCUMENT pix_origin= [0.12,0.91]
     set [x,y] for the default upper left hand corner of the pixels
     image.  The default value is shown.  [0,0] is the lower left of
     an 8.5-by-11 sheet of paper.  Yorick units are 0.0013/point or
     0.0013*72.27/inch (11 inches is a little more than 1.0);  Yorick
     keeps the "middle" of an 8.5-by-11 sheet centered in the visible
     part of its X windows, so you might want to change the default
     pix_origin if you resize the Yorick window.

   SEE ALSO: pixels, window
 */
pix_origin= [0.12,0.91];
