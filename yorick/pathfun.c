/*
 * $Id: pathfun.c,v 1.1 2005-09-18 22:04:16 dhmunro Exp $
 * Implement path name expansion and utility functions required by Yorick.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "yio.h"
#include "pstdlib.h"
#include <string.h>

#define DIR_SEP '/'
#define DIR_SEP_S "/"

char *yCWD = 0;
char *yHOME = "~/";

/* YSetCWD returns non-0 if operation fails, resets yCWD on success.  */
int
YSetCWD(const char *name)
{
  int notOK = name? p_chdir(name) : 0;
  if (!notOK) {
    p_free(yCWD);
    yCWD = p_strcpy(p_getcwd());
    if (yCWD) YNameToHead(&yCWD);
    else notOK = 1;
  }
  return notOK;
}

void
YGetHOME(void)
{
  char *hnm = Ygetenv("HOME");
  if (hnm && hnm[0]) {
    yHOME = p_strcpy(hnm);
    YNameToHead(&yHOME);
  } else {
    yHOME = p_strcpy(ySiteDir);
  }
  if (hnm) p_free(hnm);
}

/* convert path name which may be relative, or begin with ., .., ~, or
 * an environment variable, into an absolute pathname */
char *
YExpandName(const char *name)
{
  char *path, *head = 0, *tail0 = 0;
  char *tail = (char *)name;  /* I promise not to write to the original... */
  int freeHead = 0;
  int nRemove = 0;   /* count number of leading ..s */

  if (!name) return 0;

  /* try to take care of simple environment variable in first position */
  if (tail[0]=='$') {
    char *env;
    char delim = *(++tail);
    if (delim=='(') { delim = ')'; tail++; }
    else if (delim=='{') { delim='}'; tail++; }
    else delim = DIR_SEP;
    env = tail;
    while (*tail && *tail!=delim) tail++;
    head = Ygetenv( (env = p_strncat(0, env, tail-env)) );
    p_free(env);
    if (*tail && delim!=DIR_SEP) tail++;
    tail = tail0 = p_strncat(head, tail, 0);
    p_free(head);
    head = 0;
  }

  /* handle paths beginning with . or ~ or relative pathnames */
  if (tail[0]=='.') {
    head = yCWD;
    if (!tail[1]) tail++;
    else if (tail[1]==DIR_SEP) tail += 2;
  } else if (tail[0]=='~') {
    char *user = tail++;
    while (tail[0]) if ((tail++)[0] == DIR_SEP) break;
    user = p_strncat(0, user, tail-user);
    head = p_native(user);
    freeHead = 1;
    p_free(user);
  } else if (!YIsAbsolute(tail)) {
    head = yCWD;
  }

  /* count number of leading ..s */
  while (tail[0]=='.') {
    if (tail[1]=='.') {
      if (!tail[2]) {
        tail += 2;
        nRemove++;
      } else if (tail[2]==DIR_SEP) {
        tail += 3;
        nRemove++;
      }
    } else if (tail[1]==DIR_SEP) {
      tail += 2;
    } else if (!tail[1]) {
      tail++;
    } else {
      break;
    }
  }

  /* strip nRemove parent directories (but stop at root) */
  if (nRemove && head) {
    char *old = head;
    head = p_strcpy(head);
    if (freeHead) p_free(old);
    else freeHead = 1;
    path = head+strlen(head)-1;   /* guaranteed to point to DIR_SEP */
    do {
      while (path>head && *(--path)!=DIR_SEP);
    } while (--nRemove);
    path[1] = '\0';
  }

  path = p_strncat(head, tail, 0);
  if (freeHead) p_free(head);
  if (tail0) p_free(tail0);
  return path;
}

/* strip leading directory names from a pathname */
char *
YNameTail(const char *name)
{
  const char *nm = name;
  if (!nm) return 0;
  nm += strlen(nm);
  while (nm>name && *nm!=DIR_SEP) nm--;
  if (*nm!=DIR_SEP) return p_strcpy(nm);
  else return p_strcpy(nm+1);
}

/* return leading directory names of a pathname, including trailing /
 * returns 0 if no occurrences of / in pathname */
char *
YNameHead(const char *name)
{
  const char *nm = name;
  if (!nm) return 0;
  nm += strlen(nm);
  while (nm>name && *nm!=DIR_SEP) nm--;
  if (*nm!=DIR_SEP) return 0;
  else return p_strncat(0, name, nm-name+1);
}

/* ensure that a name ends in /, so it can be used as a path prefix */
void
YNameToHead(char **name)
{
  char *head = *name;
  long n = head? strlen(head) : 0;
  if (n<1 || head[n-1]!=DIR_SEP) {
    head = p_strncat(head, DIR_SEP_S, 0);
    p_free(*name);
    *name = head;
  }
}

int
YIsAbsolute(const char *name)
{
  if (name[0]==DIR_SEP || name[0]=='~') return 1;
  /* handle MS Windows a:/blahblah */
  if (name[0] && name[1]==':' && name[2]==DIR_SEP &&
      ((name[0]>='A' && name[0]<='Z') || (name[0]>='a' && name[0]<='z')))
    return 1;
  /* handle MS Windows \\server\path */
  if (name[0]=='\\' && name[1]=='\\') return 1;
  return 0;
}

int
YIsDotRelative(const char *name)
{
  return name[0]=='.' &&
    (!name[1] || (name[1]==DIR_SEP ||
                  (name[1]=='.' && (!name[2] || name[2]==DIR_SEP))));
}
