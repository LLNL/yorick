/*
 * $Id: phash.h,v 1.1 2005-09-18 22:05:31 dhmunro Exp $
 * portability layer unique key hashing functions
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "plugin.h"

typedef struct p_hashtab p_hashtab;
typedef unsigned long p_hashkey;

/* randomize the low order 32 bits of an address or integer
 *   such that P_IHASH(x)==P_IHASH(y) if and only if x==y */
#define P_IHASH(x) ((x)^p_hmasks[(((p_hashkey)(x))>>4)&0x3f])
#define P_PHASH(x) P_IHASH((char*)(x)-(char*)0)

BEGIN_EXTERN_C

PLUG_API p_hashkey p_hmasks[64];  /* for P_IHASH, P_PHASH macros */

/* unique key hash tables are basis for all hashing */
PLUG_API p_hashtab *p_halloc(p_hashkey size);
PLUG_API void p_hfree(p_hashtab *tab, void (*func)(void *));
PLUG_API int p_hinsert(p_hashtab *tab, p_hashkey hkey, void *value);
PLUG_API void *p_hfind(p_hashtab *tab, p_hashkey hkey);
PLUG_API void p_hiter(p_hashtab *tab,
                    void (*func)(void *val, p_hashkey key, void *ctx),
                    void *ctx);

/* global name string to id number correspondence
 *   p_id returns id number of an existing string, or 0
 *   p_idmake returns id number valid until matching p_idfree, never 0
 *   p_idstatic returns id number for statically allocated input name
 *     - name not copied, subsequent calls to p_idfree will be ignored
 *   p_idfree decrements the use counter for the given id number,
 *     freeing the number if there are no more uses
 *     - p_idmake increments use counter if name already exists
 *   p_idname returns 0 if no such id number has been made */
PLUG_API p_hashkey p_id(const char *name, int len);
PLUG_API p_hashkey p_idmake(const char *name, int len);
PLUG_API p_hashkey p_idstatic(char *name);
PLUG_API void p_idfree(p_hashkey id);
PLUG_API char *p_idname(p_hashkey id);

/* global pointer-to-pointer correspondence
 *   p_getctx returns context of a pointer or 0
 *   p_setctx sets context of a pointer, or deletes it if 0 */
PLUG_API void p_setctx(void *ptr, void *context);
PLUG_API void *p_getctx(void *ptr);

END_EXTERN_C
