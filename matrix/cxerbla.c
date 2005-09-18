/*
 * $Id: cxerbla.c,v 1.1 2005-09-18 22:04:39 dhmunro Exp $
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
extern void cblas_xerbla(int p, char *rout, char *form, ...);
void cblas_xerbla(int p, char *rout, char *form, ...)
{
   va_list argptr;

   va_start(argptr, form);
   if (p)
      fprintf(stderr, "Parameter %d to routine %s was incorrect\n", p, rout);
   vfprintf(stderr, form, argptr);
   va_end(argptr);
   exit(-1);
}

extern int CBLAS_CallFromC;
int CBLAS_CallFromC= 0;

extern void xerbla_(char *srname, void *vinfo);
void xerbla_(char *srname, void *vinfo)
{
  return;
}
