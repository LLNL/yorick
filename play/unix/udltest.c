/*
 * $Id: udltest.c,v 1.1 2005-09-18 22:05:41 dhmunro Exp $
 * test of play dynamic link functions (with config.c TEST_DL branch)
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#if defined(TEST_SHARED)

/* main() that dlopens this is TEST_DL branch of config.c */
extern void fun(int x);
extern int dat[2];
int dat[2] = { -1, -2 };
void fun(int x) {
  extern int testcall(int);
  if (dat[0]>0 && testcall(x)) {
    dat[1] = dat[0];
    dat[0] = x;
  }
}



#else

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

extern void *test_dlopen(void);
extern int test_dlsym(void *handle, int dat, void *paddr);

void *
test_dlopen(void)
{
  return dlopen("./udltest" PLUG_SUFFIX, PLUG_FLAGS);
}

int
test_dlsym(void *handle, int dat, void *paddr)
{
  void **addr = paddr;
  addr[0] = dlsym(handle, (dat? "dat" : "fun"));
  return addr[0]? 3 : 0;
}



#elif defined(PLUG_HPUX)

#define PLUG_SUFFIX ".sl"

#ifndef PLUG_HEADER
#include <dl.h>
#endif

#ifndef PLUG_FLAGS
# define PLUG_FLAGS BIND_DEFERRED
#endif

extern void *test_dlopen(void);
extern int test_dlsym(void *handle, int dat, void *paddr);

void *
test_dlopen(void)
{
  return (void *)shl_load("./udltest" PLUG_SUFFIX, PLUG_FLAGS);
}

int
test_dlsym(void *handle, int dat, void *paddr)
{
  void **addr = paddr;
  int notok = shl_findsym(&handle, (dat? "dat" : "fun"), TYPE_UNDEFINED, addr);
  return (notok || !addr[0])? 0 : 3;
}



#elif defined(PLUG_MACOSX)

#define PLUG_SUFFIX ".so"

#ifndef PLUG_HEADER
#include <mach-o/dyld.h>
#endif

void *
test_dlopen(void)
{
  void *handle = 0;
  NSObjectFileImage file_image;
  if (NSCreateObjectFileImageFromFile("./udltest" PLUG_SUFFIX, &file_image) ==
      NSObjectFileImageSuccess) {
    handle = (void *)NSLinkModule(file_image, "./udltest" PLUG_SUFFIX,
                                  NSLINKMODULE_OPTION_RETURN_ON_ERROR
                                  | NSLINKMODULE_OPTION_PRIVATE);
    NSDestroyObjectFileImage(file_image);
  }
  return handle;  /* actually type NSModule */
}

int
test_dlsym(void *handle, int dat, void *paddr)
{
  void **addr = paddr;
  NSSymbol sym = NSLookupSymbolInModule(handle, (dat? "_dat" : "_fun"));
  if (sym) addr[0] = NSAddressOfSymbol(sym);
  return addr[0]? 3 : 0;
}



#elif defined(PLUG_MACOSX_DYLIB)
# do not use this branch, see notes in udl.c

#define PLUG_SUFFIX ".dylib"

#ifndef PLUG_HEADER
#include <mach-o/dyld.h>
#endif

#ifndef PLUG_FLAGS
# define PLUG_FLAGS NSLOOKUPSYMBOLINIMAGE_OPTION_BIND
#endif

extern void *test_dlopen(void);
extern int test_dlsym(void *handle, int dat, void *paddr);

void *
test_dlopen(void)
{
  return (void *)NSAddImage("./udltest" PLUG_SUFFIX,
                            NSADDIMAGE_OPTION_RETURN_ON_ERROR);
}

int
test_dlsym(void *handle, int dat, void *paddr)
{
  void **addr = paddr;
  addr[0] = 0;
  if (NSIsSymbolNameDefinedInImage(handle, (dat? "_dat" : "_fun"))) {
    NSSymbol nss =
      NSLookupSymbolInImage(handle, (dat? "_dat" : "_fun"), PLUG_FLAGS |
                            NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
    if (nss) addr[0] = NSAddressOfSymbol(nss);
  }
  return (!addr[0])? 0 : 3;
}



#else

#error one of PLUG_LIBDL, PLUG_HPUX, or PLUG_MACOSX must be set

#endif
#endif
