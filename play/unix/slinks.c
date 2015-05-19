/*
 * $Id: slinks.c,v 1.3 2006-07-20 04:28:30 dhmunro Exp $
 * u_track_link and u_find_exe functions from playu.h
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* need these to pick up readlink prototype (unistd.h) */
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 1
#endif
#ifndef _XOPEN_SOURCE_EXTENDED
#define _XOPEN_SOURCE_EXTENDED 1
#endif

#include "config.h"

#include "playu.h"
#include "pstdlib.h"
#include "pstdio.h"

#include <unistd.h>
/* needed for stat() call */
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

char *
u_track_link(const char *name)
{
  if (name) {
    char link[P_WKSIZ+1];
    int i, j, len;
    if (name != p_wkspc.c)
      for (i=0 ; (p_wkspc.c[i] = name[i]) && i<P_WKSIZ ; i++);
    for (;;) {
      len = readlink(p_wkspc.c, link, P_WKSIZ);
      if (len < 0) break;
      /* handle relative links */
      if (link[0] == '/') i = 0;
      else for (i=j=0 ; p_wkspc.c[j] ; j++) if (p_wkspc.c[j]=='/') i = j+1;
      for (j=0 ; j<len && link[j]=='.' ; ) {
        /* remove ./ and ../ from link
         * FIXME: this is wrong for the ../ case if some of the path
         * components are themselves softlinks instead of directories
         */
        if (j+1>=len || link[j+1]=='/') {
          j = (j+1>=len)? j+1 : j+2;
        } else if (link[j+1]=='.' && (j+2>=len || link[j+2]=='/')) {
          if (p_wkspc.c[i-1] != '/') break;
          i--;
          while (i>0 && p_wkspc.c[--i]=='/');
          if (i) while (i>0 && p_wkspc.c[--i]!='/');
          i++;
          j = (j+2>=len)? j+2 : j+3;
        } else {
          break;
        }
      }
      for ( ; j<len ; i++,j++) p_wkspc.c[i] = link[j];
      p_wkspc.c[i] = '\0';
    }
    return p_wkspc.c;
  } else {
    return 0;
  }
}

static int u_executable_file(const char *path);

char *
u_find_exe(const char *argv0)
{
  char *wkspc = p_wkspc.c;
  int i = 0;
  if (!argv0) return 0;

  while (argv0[i] && argv0[i]!='/') i++;

  if (!argv0[i]) {   /* search for argv0 on PATH environment variable */
    char *path = getenv("PATH");
    char c = path? path[0] : 0;
    int s, j, k=0;
    while (c) {
      while (c==':' || c=='.') {
        if (c=='.' && path[k+1] && path[k+1]!=':') break;
        c = path[++k];
      }
      if (k) {   /* any run of ::.:... treated as a single . */
        path += k;
        wkspc = p_getcwd();
        for (j=0 ; wkspc[j] ; j++);
        k = j;
      } else {
        while (c && c!=':') c = path[++k];  /* at least one pass */
        for (j=0 ; j<k && j<P_WKSIZ ; j++) wkspc[j] = path[j];
        path += c? k+1 : k;   /* skip trailing : */
      }
      if (wkspc[j-1] == '/') s = 0;
      else s = 1, wkspc[j] = '/';
      for (; j<k+i+1 && j+s<P_WKSIZ ; j++) wkspc[j+s] = argv0[j-k];
      if (u_executable_file(wkspc) >= 0) break;
      k = 0;
      c = path[0];
      wkspc = p_wkspc.c;
    }
    return k? wkspc : 0;
  }

  if (i) {           /* argv0 refers to subdirectory of cwd */
    wkspc = p_getcwd();
    if (wkspc) {
      int j;
      for (j=0 ; wkspc[j] ; j++);
      if (j && wkspc[j-1] != '/') wkspc[j++] = '/';
      while (argv0[0]=='.' && argv0[1]=='/') argv0 += 2;
      for (i=j ; argv0[i-j] && i<P_WKSIZ ; i++) wkspc[i] = argv0[i-j];
    } else {
      i = 0;
      wkspc = p_wkspc.c;
    }

  } else {           /* argv0 is absolute pathname */
    for (i=0 ; argv0[i] && i<P_WKSIZ ; i++) wkspc[i] = argv0[i];
  }

  wkspc[i] = '\0';
  return (u_executable_file(wkspc) >= 0)? wkspc : 0;
}

static int
u_executable_file(const char *path)
{
  int err = access(path, X_OK);
  if (err >= 0) {
    struct stat buf;
    if (stat(path,&buf) || !S_ISREG(buf.st_mode)) err = -2;
  }
  return err;
}
