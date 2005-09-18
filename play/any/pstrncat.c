/*
 * $Id: pstrncat.c,v 1.1 2005-09-18 22:05:43 dhmunro Exp $
 * strncat that p_mallocs its destination
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"
#include "pstdlib.h"
#include <string.h>

char *
p_strncat(const char *s1, const char *s2, size_t n)
{
  if (s2) {
    size_t n1 = strlen(s2);
    char *d;
    if (!n) n = n1;
    else if (n1<n) n = n1;
    n1 = s1? strlen(s1) : 0;
    d = p_malloc(n1+n+1);
    if (s1) strcpy(d, s1);
    else d[0] = '\0';
    if (n) strncat(d+n1, s2, n);
    return d;
  } else {
    return p_strcpy(s1);
  }
}
