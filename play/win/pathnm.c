/*
 * $Id: pathnm.c,v 1.1 2005-09-18 22:05:35 dhmunro Exp $
 * p_getenv and w_pathname pathname preprocessing for MS Windows
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "playw.h"
#include "pstdlib.h"

#include <string.h>

/* GetEnvironmentVariable is in kernel32.lib */

char *
p_getenv(const char *name)
{
  DWORD flag = GetEnvironmentVariable(name, p_wkspc.c, P_WKSIZ);
  return (flag>0 && flag<P_WKSIZ)? p_wkspc.c : 0;
}

char *
w_pathname(const char *name)
{
  const char *tmp = name;
  long len = 0;
  long left = P_WKSIZ;

  if (name[0]=='$') {
    int delim = *(++name);
    if (delim=='(')      { delim = ')';  name++; }
    else if (delim=='{') { delim = '}';  name++; }
    else                   delim = '\0';
    for (tmp=name ; tmp[0] ; tmp++)
      if (delim? (tmp[0]==delim) : (tmp[0]=='/' || tmp[0]=='\\')) break;
    if (tmp>name+1024) {
      p_wkspc.c[0] = '\0';
      return p_wkspc.c;
    }
    len = tmp-name;
    if (len && delim) tmp++;
  } else if (name[0]=='~' && (!name[1] || name[1]=='/' || name[1]=='\\')) {
    tmp++;
    len = -1;
  }
  if (len) {
    char env_name[1024];
    if (len < 0) {
      strcpy(env_name, "HOME");
    } else {
      env_name[0] = '\0';
      strncat(env_name, name, len);
    }
    len = GetEnvironmentVariable(env_name, p_wkspc.c, P_WKSIZ);
    if (len>P_WKSIZ) len = left+1;
    left -= len;
    if (left<0) {
      p_wkspc.c[0] = '\0';
      return p_wkspc.c;
    }
    name = tmp;
  }

  if ((long)strlen(name)<=left) strcpy(p_wkspc.c+len, name);
  else p_wkspc.c[0] = '\0';

  for (left=0 ; p_wkspc.c[left] ; left++)
    if (p_wkspc.c[left]=='/') p_wkspc.c[left] = '\\';
  return p_wkspc.c;
}

char *
w_unixpath(const char *wname)
{
  int i;
  for (i=0 ; i<P_WKSIZ && wname[i] ; i++)
    p_wkspc.c[i] = (wname[i]=='\\')? '/' : wname[i];
  return p_wkspc.c;
}
