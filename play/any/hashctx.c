/*
 * $Id: hashctx.c,v 1.1 2005-09-18 22:05:44 dhmunro Exp $
 * generic pointer<->context association functions
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"
#include "phash.h"

static p_hashtab *ctx_table = 0;

void
p_setctx(void *ptr, void *context)
{
  if (!ctx_table) ctx_table = p_halloc(64);
  p_hinsert(ctx_table, P_PHASH(ptr), context);
}

void *
p_getctx(void *ptr)
{
  return ctx_table? p_hfind(ctx_table, P_PHASH(ptr)) : 0;
}
