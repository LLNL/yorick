/*
 * $Id: cygmain.c,v 1.1 2005-09-18 22:05:37 dhmunro Exp $
 * cygwin (or uwin?) main program stub
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include <windows.h>

int WINAPI
WinMain(HINSTANCE me, HINSTANCE prev, LPSTR cmd_line, int show0)
{
  extern int on_launch(int, char **);
  extern int cyg_app(int (*on_launch)(int, char **),
                     HINSTANCE me, LPSTR cmd_line, int show0);
  return cyg_app(on_launch, me, cmd_line, show0);
}
