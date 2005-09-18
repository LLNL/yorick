/*
 * $Id: pstdio.h,v 1.1.1.1 2005-09-18 22:05:31 dhmunro Exp $
 * portability layer I/O wrappers
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* filesystem services (mostly ANSI or POSIX)
 * - necessary to implement UNIX-like filenaming semantics universally
 * - p_free frees result of p_getcwd, p_frall for p_lsdir result */

#ifndef PSTDIO_H
#define PSTDIO_H

#include "plugin.h"

typedef struct p_file p_file;
typedef struct p_dir p_dir;

BEGIN_EXTERN_C

PLUG_API p_file *p_fopen(const char *unix_name, const char *mode);
PLUG_API p_file *p_popen(const char *command, const char *mode);

PLUG_API unsigned long p_fsize(p_file *file);
PLUG_API unsigned long p_ftell(p_file *file);
PLUG_API int p_fseek(p_file *file, unsigned long addr);

PLUG_API char *p_fgets(p_file *file, char *buf, int buflen);
PLUG_API int p_fputs(p_file *file, const char *buf);
PLUG_API unsigned long p_fread(p_file *file,
                               void *buf, unsigned long nbytes);
PLUG_API unsigned long p_fwrite(p_file *file,
                                const void *buf, unsigned long nbytes);

PLUG_API int p_feof(p_file *file);
PLUG_API int p_ferror(p_file *file);
PLUG_API int p_fflush(p_file *file);
PLUG_API int p_fclose(p_file *file);

PLUG_API int p_remove(const char *unix_name);
PLUG_API int p_rename(const char *unix_old, const char *unix_new);

PLUG_API int p_chdir(const char *unix_name);
PLUG_API int p_rmdir(const char *unix_name);
PLUG_API int p_mkdir(const char *unix_name);
PLUG_API char *p_getcwd(void);

PLUG_API p_dir *p_dopen(const char *unix_name);
PLUG_API int p_dclose(p_dir *dir);
/* returned filename does not need to be freed, but
 * value may be clobbered by dclose, next dnext, or p_wkspc use
 * . and .. do not appear in returned list */
PLUG_API char *p_dnext(p_dir *dir, int *is_dir);

END_EXTERN_C

#endif
