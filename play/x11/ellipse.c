/*
 * $Id: ellipse.c,v 1.1 2005-09-18 22:05:34 dhmunro Exp $
 * p_ellipse for X11
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"
#include "playx.h"

void
p_ellipse(p_win *w, int x0, int y0, int x1, int y1, int border)
{
  p_scr *s = w->s;
  Display *dpy = s->xdpy->dpy;
  GC gc = x_getgc(s, w, FillSolid);
  int tmp;
  if (x1 > x0) x1 -= x0;
  else tmp = x0-x1, x0 = x1, x1 = tmp;
  if (y1 > y0) y1 -= y0;
  else tmp = y0-y1, y0 = y1, y1 = tmp;
  if (border)
    XDrawArc(dpy, w->d, gc, x0, y0, x1, y1, 0, 360*64);
  else
    XFillArc(dpy, w->d, gc, x0-1, y0-1, x1+2, y1+2, 0, 360*64);
  if (p_signalling) p_abort();
}
