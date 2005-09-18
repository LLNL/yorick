/*
 * $Id: timew.c,v 1.1 2005-09-18 22:05:39 dhmunro Exp $
 * p_wall_secs for UNIX machines (ANSI C version not UNIX specific)
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"
#include "play.h"

static int p_wall_init = 0;

#ifdef USE_GETTIMEOFDAY

/* BSD 4.3, SVr4 gettimeofday function used by X window system
 * to implement XtAddTimeout, except WIN32 uses ftime function instead */
#undef _POSIX_SOURCE
#undef _HPUX_SOURCE
#define _HPUX_SOURCE 1
#include <sys/time.h>
static double p_wall_0;
double
p_wall_secs(void)
{
  struct timeval wall;
  gettimeofday(&wall, (void *)0);
  if (!p_wall_init) {
    p_wall_0 = 1.0e-6*wall.tv_usec + wall.tv_sec;
    p_wall_init = 1;
  }
  return 1.0e-6*wall.tv_usec + wall.tv_sec - p_wall_0;
}

#else

/* ANSI C version should work anywhere */
#include <time.h>
static time_t p_wall_0;
double
p_wall_secs(void)
{
  if (!p_wall_init) {
    p_wall_0 = time((time_t *)0);
    p_wall_init = 1;
  }
  return difftime(time((time_t *)0), p_wall_0);
}

#endif
