/*
 * $Id: usernm.c,v 1.1 2005-09-18 22:05:39 dhmunro Exp $
 * p_getuser for UNIX machines
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE 1
#endif

#include "config.h"
#include "play.h"

#ifndef NO_PASSWD

#include <sys/types.h>
#include <unistd.h>
#include <pwd.h>

char *
p_getuser(void)
{
  char *user = getlogin();
  if (!user) {
    struct passwd *pw = getpwuid(getuid());  /* see also pathnm.c */
    if (pw) user = pw->pw_name;
  }
  return user;
}

#else

extern char *cuserid(char *);
char *
p_getuser(void)
{
  char *user = cuserid((char *)0);
  return user;
}

#endif
