/*
 * $Id: prect.c,v 1.1 2005-09-18 22:05:38 dhmunro Exp $
 * p_rect for MS Windows
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "playw.h"

void
p_rect(p_win *w, int x0, int y0, int x1, int y1, int border)
{
  if (border) {
    HDC dc = w_getdc(w, 18);
    if (dc)
      Rectangle(dc, x0, y0, x1+1, y1+1);
  } else {
    HDC dc = w_getdc(w, 4);
    if (dc && w->brush) {
      RECT r;
      r.left = x0;
      r.top = y0;
      r.right = x1;
      r.bottom = y1;
      FillRect(dc, &r, w->brush);
    }
  }
}
