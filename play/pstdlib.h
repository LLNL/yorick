/*
 * $Id: pstdlib.h,v 1.1 2005-09-18 22:05:31 dhmunro Exp $
 * portability layer basic memory management interface
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include <stdlib.h>

#include "plugin.h"

BEGIN_EXTERN_C

PLUG_API void *(*p_malloc)(size_t);
PLUG_API void  (*p_free)(void *);
PLUG_API void *(*p_realloc)(void *, size_t);

/* above data loaded to system malloc, free, and realloc
 * -- call p_mminit to get mm version
 */
#ifdef P_DEBUG
#define p_mminit p_mmdebug
PLUG2_API int p_mmcheck(void *p);
PLUG2_API void p_mmguard(void *b, unsigned long n);
PLUG2_API long p_mmextra, p_mmoffset;
#endif
PLUG_API void p_mminit(void);

/* make trivial memory statistics globally available
 * -- counts total number of allocations, frees, and
 *    current number of large blocks */
PLUG_API long p_nallocs;
PLUG_API long p_nfrees;
PLUG_API long p_nsmall;
PLUG_API long p_asmall;

/* define this to get control when mm functions fail
 * -- if it returns, must return 0 */
PLUG_API void *(*p_mmfail)(unsigned long n);

/* temporary space */
#define P_WKSIZ 2048
typedef union {
  char c[P_WKSIZ+8];
  int i[P_WKSIZ/8];
  long l[P_WKSIZ/8];
  double d[P_WKSIZ/8];
} p_twkspc;
PLUG_API p_twkspc p_wkspc;

/* similar to the string.h functions, but p_malloc destination
 * - 0 src is acceptable */
PLUG_API void *p_memcpy(const void *, size_t);
PLUG_API char *p_strcpy(const char *);
PLUG_API char *p_strncat(const char *, const char *, size_t);

/* expand leading env var, ~, set / or \ separators, free with p_free */
PLUG_API char *p_native(const char *unix_name);

/* dont do anything critical if this is set -- signal an error */
PLUG2_API volatile int p_signalling;

/* dynamic linking interface
 * dlname is filename not including .so, .dll, .dylib, etc. suffix
 * symbol is name in a C program
 * type is 0 if expecting a function, 1 if expecting data
 * paddr is &addr where addr is void* or void(*)(),
 * p_dlsym return value is 0 on success, 1 on failure */
PLUG_API void *p_dlopen(const char *dlname);
PLUG_API int p_dlsym(void *handle, const char *symbol, int type, void *paddr);

/* interface for synchronous subprocess
 * (p_popen, p_spawn in pstdio.h for asynchronous)
 */
PLUG_API int p_system(const char *cmdline);

END_EXTERN_C
