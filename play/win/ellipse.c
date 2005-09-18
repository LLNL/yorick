/*
 * $Id: ellipse.c,v 1.1 2005-09-18 22:05:36 dhmunro Exp $
 * p_ellipse for MS Windows
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "playw.h"

void
p_ellipse(p_win *w, int x0, int y0, int x1, int y1, int border)
{
  HDC dc = w_getdc(w, border? 18 : 12);
  if (dc)
    Ellipse(dc, x0, y0, x1+1+(!border), y1+1+(!border));
}
