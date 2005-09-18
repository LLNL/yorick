/*
 * $Id: pmemcpy.c,v 1.1 2005-09-18 22:05:43 dhmunro Exp $
 * memcpy that p_mallocs its destination
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"
#include "pstdlib.h"
#include <string.h>

void *
p_memcpy(const void *s, size_t n)
{
  if (s) {
    void *d = p_malloc(n);
    if ( ! (((char *)s-(char *)0) & (sizeof(size_t)-1)) ) {
      /* some versions of memcpy miss this obvious optimization */
      const size_t *sl=s;
      size_t *dl=d;
      while (n>=sizeof(size_t)) {
        *(dl++)= *(sl++);
        n -= sizeof(size_t);
      }
    }
    if (n) memcpy(d, s, n);
    return d;
  } else {
    return 0;
  }
}
