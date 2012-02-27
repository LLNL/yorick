/*
 * $Id: udl.c,v 1.1 2005-09-18 22:05:41 dhmunro Exp $
 * UNIX version of play dynamic linking operations
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"

#include "playu.h"
#include "pstdlib.h"
#include <string.h>

#ifdef PLUG_HEADER
#include PLUG_HEADER
#endif

#if defined(PLUG_LIBDL)

#ifndef PLUG_SUFFIX
# define PLUG_SUFFIX ".so"
#endif

#ifndef PLUG_HEADER
#include <dlfcn.h>
#endif

#ifndef PLUG_FLAGS
# define PLUG_FLAGS RTLD_LAZY | RTLD_GLOBAL
#endif

void *
p_dlopen(const char *dlname)
{
  void *handle = 0;
  if (dlname && dlname[0]) {
    char *name = p_strncat(u_pathname(dlname), PLUG_SUFFIX, 0);
    handle = dlopen(name, PLUG_FLAGS);
    p_free(name);
  }
  return handle;
}

int
p_dlsym(void *handle, const char *symbol, int type, void *paddr)
{
  void **addr = paddr;
  addr[0] = dlsym(handle, symbol);
  /* correct way to detect failure if a==0 were legal:
   *   const char *msg = dlerror();
   *   if (msg) return 0;
   */
  return !addr[0];
}



#elif defined(PLUG_HPUX)

#define PLUG_SUFFIX ".sl"

#ifndef PLUG_HEADER
#include <dl.h>
#endif
#include <errno.h>

#ifndef PLUG_FLAGS
# define PLUG_FLAGS BIND_DEFERRED
#endif

void *
p_dlopen(const char *dlname)
{
  void *handle = 0;
  if (dlname && dlname[0]) {
    char *name = p_strncat(u_pathname(dlname), PLUG_SUFFIX, 0);
    handle = (void *)shl_load(name, PLUG_FLAGS, 0);
    p_free(name);
  }
  return handle;
}

int
p_dlsym(void *handle, const char *symbol, int type, void *paddr)
{
  void **addr = paddr;
  short expect = type? ((type&1)? TYPE_DATA : TYPE_UNDEFINED) : TYPE_PROCEDURE;
  int notok = shl_findsym(&handle, symbol, expect, paddr);
  return notok || !addr[0];
}



#elif defined(PLUG_MACOSX)
/* Apple now deprecates this.  At Mac OS X 10.4 and beyond, use PLUG_LIBDL.
 * see https://developer.apple.com/library/mac/#qa/qa1180/_index.html
 */

#define PLUG_SUFFIX ".so"

#ifndef PLUG_HEADER
#include <mach-o/dyld.h>
#endif

void *
p_dlopen(const char *dlname)
{
  void *handle = 0;
  if (dlname && dlname[0]) {
    char *name = p_strncat(u_pathname(dlname), PLUG_SUFFIX, 0);
    NSObjectFileImage file_image;
    if (NSCreateObjectFileImageFromFile(name, &file_image) ==
        NSObjectFileImageSuccess) {
      handle = (void *)NSLinkModule(file_image, name, 
                                    NSLINKMODULE_OPTION_RETURN_ON_ERROR
                                    | NSLINKMODULE_OPTION_PRIVATE);
      NSDestroyObjectFileImage(file_image);
    }
    p_free(name);
  }
  return handle;  /* actually type NSModule */
}

int
p_dlsym(void *handle, const char *symbol, int type, void *paddr)
{
  void **addr = paddr;
  addr[0] = 0;
  if (symbol && symbol[0]) {
    char *symname = p_strncat("_", symbol, 0);
    NSSymbol sym = NSLookupSymbolInModule(handle, symname);
    if (sym) addr[0] = NSAddressOfSymbol(sym);
    p_free(symname);
  }
  return !addr[0];
}



#elif defined(PLUG_MACOSX_DYLIB)
/*
  this is fossil code - it is correct, but I couldn't get the linker
  to build a correct optimized .dylib for the cerfc example in extend/

  a more careful reading of the Mach-O documentation (the tech note on
  porting to UNIX in particular) suggests that plugins are supposed
  to be implemented with the -bundle switch to the compiler, not .dylib
  the required MACOSX_DEPLOYMENT_TARGET env var to libtool exists only
  from 10.3 onward, so -bundle is the only possibility for <=10.2
*/

#define PLUG_SUFFIX ".dylib"

#ifndef PLUG_HEADER
#include <mach-o/dyld.h>
#endif

#ifndef PLUG_FLAGS
# define PLUG_FLAGS NSLOOKUPSYMBOLINIMAGE_OPTION_BIND
#endif

void *
p_dlopen(const char *dlname)
{
  void *handle = 0;
  if (dlname && dlname[0]) {
    char *name = p_strncat(u_pathname(dlname), PLUG_SUFFIX, 0);
    handle = (void *)NSAddImage(name, NSADDIMAGE_OPTION_RETURN_ON_ERROR);
    p_free(name);
  }
  return handle;  /* actually type (struct mach_header *) */
}

int
p_dlsym(void *handle, const char *symbol, int type, void *paddr)
{
  void **addr = paddr;
  addr[0] = 0;
  if (symbol && symbol[0]) {
    char *symname = p_strncat("_", symbol, 0);
    if (NSIsSymbolNameDefinedInImage(handle, symname)) {
      NSSymbol nss =
        NSLookupSymbolInImage(handle, symname, PLUG_FLAGS |
                              NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
      if (nss) addr[0] = NSAddressOfSymbol(nss);
    }
    p_free(symname);
  }
  return !addr[0];
}



#else

/* ARGSUSED */
void *
p_dlopen(const char *dlname)
{
  return 0;
}

/* ARGSUSED */
int
p_dlsym(void *handle, const char *symbol, int type, void *paddr)
{
  void **addr = paddr;
  addr[0] = 0;
  return 1;
}

#endif
