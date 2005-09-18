/*
 * $Id: pmain.c,v 1.1 2005-09-18 22:05:40 dhmunro Exp $
 * play main function for UNIX with raw X11 (no X toolkit)
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "play.h"

BEGIN_EXTERN_C
extern void p_mminit(void);
extern int u_main_loop(int (*on_launch)(int,char**), int, char **);
END_EXTERN_C

int
main(int argc, char *argv[])
{
  p_mminit();
  return u_main_loop(on_launch, argc, argv);
}
