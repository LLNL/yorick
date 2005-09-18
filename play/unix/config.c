/*
 * $Id: config.c,v 1.1 2005-09-18 22:05:39 dhmunro Exp $
 * configuration tester for UNIX machines
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#define MAIN_DECLARE int main(int argc, char *argv[])
#define MAIN_RETURN(value) return value

#ifdef TEST_GCC
# ifdef __GNUC__
MAIN_DECLARE {
  MAIN_RETURN(0); }
# else
#error not gcc
# endif
#endif

#ifdef TEST_UTIME
/* check settings of: USE_GETRUSAGE USE_TIMES */
#include "timeu.c"
MAIN_DECLARE {
  double s;
  double t = p_cpu_secs(&s);
  MAIN_RETURN(t!=0.); }
#endif

#ifdef TEST_WTIME
/* check settings of: USE_GETTIMEOFDAY */
#include "timew.c"
MAIN_DECLARE {
  double t = p_wall_secs();
  MAIN_RETURN(t!=0.); }
#endif

#ifdef TEST_USERNM
/* check settings of: NO_PASSWD */
#include "usernm.c"
MAIN_DECLARE {
  char *u = p_getuser();
  MAIN_RETURN(u==0); }
#endif

#ifdef TEST_TIOCGPGRP
/* check settings of: USE_TIOCGPGRP_IOCTL */
#include "uinbg.c"
MAIN_DECLARE {
  MAIN_RETURN(u_in_background()); }
#endif

#ifdef TEST_GETCWD
/* check settings of: USE_GETWD */
#include <unistd.h>
static char dirbuf[1024];
#ifdef USE_GETWD
#define getcwd(x,y) getwd(x)
#endif
MAIN_DECLARE {
  char *u = getcwd(dirbuf, 1024);
  MAIN_RETURN(u==0); }
#endif

#ifdef TEST_DIRENT
/* check settings of: DIRENT_HEADER USE_GETWD */
#include "dir.c"
p_twkspc p_wkspc;
MAIN_DECLARE {
  p_dir *d = p_dopen("no/such/thing");
  int value = 0;
  char *l = p_dnext(d, &value);
  MAIN_RETURN(p_chdir(l) || p_rmdir(l) || p_mkdir(l)); }
#endif

#ifdef TEST_POLL
/* check settings of: USE_SYS_POLL_H USE_SELECT HAVE_SYS_SELECT_H
                      NO_SYS_TIME_H NEED_SELECT_PROTO */
#include "uevent.c"
MAIN_DECLARE {
  int p = u_poll(1000);
  MAIN_RETURN(p!=0); }
#endif

#ifdef TEST_PLUG
/* check settings of: PLUG_LIBDL PLUG_HPUX PLUG_MACOSX */
/* also check that udltest.c can call function in main executable */
extern int testcall(int check);
int testcall(int check) {
  return (check == 13579);
}
#include "udltest.c"
MAIN_DECLARE {
  union {
    void *data;
    void (*function)(int);
  } addr;
  void *h = test_dlopen();
  if (!h) {
    return 1;
  } else if (!(test_dlsym(h, 1, &addr) & 1)) {
    return 2;
  } else {
    int *pdat = addr.data;
    if (!pdat || pdat[0]!=-1 || pdat[1]!=-2) {
      return 3;
    } else if (!(test_dlsym(h, 0, &addr) & 2)) {
      return 4;
    } else {
      void (*pfun)(int) = addr.function;
      pdat[0] = 24680;
      pfun(13579);
      if (pdat[0]!=13579 || pdat[1]!=24680) {
        return 5;
      }
    }
  }
  MAIN_RETURN(0); }
#endif
