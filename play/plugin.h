/*
 * $Id: plugin.h,v 1.1 2005-09-18 22:05:31 dhmunro Exp $
 * provide extern attribute for MSWindows DLLs
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef PLUGIN_H
#define PLUGIN_H

# if !defined(_WIN32) && !defined(__CYGWIN__)
/* non-MSWindows platforms have more sophistocated dynamic linkers */
#define PLUG_EXPORT extern
#define PLUG_IMPORT extern
# else
#  ifdef __GNUC__
#define PLUG_EXPORT extern __attribute__((dllexport))
#define PLUG_IMPORT extern __attribute__((dllimport))
#  else
#define PLUG_EXPORT extern __declspec(dllexport)
#define PLUG_IMPORT extern __declspec(dllimport)
#  endif
# endif

# ifndef PLUG_IN
/* yorick/play source code uses this branch */
#define PLUG_API PLUG_EXPORT
# else
/* yorick/play plugins (MSWindows DLLs) use this branch */
#define PLUG_API PLUG_IMPORT
# endif

#define PLUG2_API PLUG_API

/* get this ugliness out of important header files */
#ifndef BEGIN_EXTERN_C
# if defined(__cplusplus) || defined(c_plusplus)
#  define BEGIN_EXTERN_C extern "C" {
#  define END_EXTERN_C }
# else
#  define BEGIN_EXTERN_C
#  define END_EXTERN_C
# endif
#endif

#endif
