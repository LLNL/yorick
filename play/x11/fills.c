/*
 * $Id: fills.c,v 1.1 2005-09-18 22:05:32 dhmunro Exp $
 * p_fill for X11
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"
#include "playx.h"

/* convexity 0 may self-intersect, 1 may not, 2 must be convex */
static int x_shape[3] = { Complex, Nonconvex, Convex };

void
p_fill(p_win *w, int convexity)
{
  p_scr *s = w->s;
  Display *dpy = s->xdpy->dpy;
  GC gc = x_getgc(s, w, FillSolid);
  int nmx = (XMaxRequestSize(dpy)-3)/2;
  int n = x_pt_count;
  x_pt_count = 0;
  /* note: this chunking does not produce a correct plot, but
   *       it does prevent Xlib from crashing an R4 server
   * you just can't fill a polygon with too many sides */
  while (n>2) {
    if (n<nmx) nmx = n;
    XFillPolygon(dpy, w->d, gc, x_pt_list, nmx,
                 x_shape[convexity], CoordModeOrigin);
    n -= nmx;
  }
  if (p_signalling) p_abort();
}
