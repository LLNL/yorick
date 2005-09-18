/*
 * $Id: usernm.c,v 1.1 2005-09-18 22:05:35 dhmunro Exp $
 * p_getuser for MS Windows
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "pstdlib.h"
#include "play.h"

/* GetUserName in advapi32.lib */
#include <windows.h>

char *
p_getuser(void)
{
  DWORD sz = P_WKSIZ;
  if (GetUserName(p_wkspc.c, &sz))
    return p_wkspc.c;
  else
    return 0;
}
