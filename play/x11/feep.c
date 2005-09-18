/*
 * $Id: feep.c,v 1.1 2005-09-18 22:05:32 dhmunro Exp $
 * p_feep for X11
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"
#include "playx.h"

void
p_feep(p_win *w)
{
  if (w->s->xdpy->dpy) XBell(w->s->xdpy->dpy, 100);
}
