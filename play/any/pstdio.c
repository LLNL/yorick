/*
 * $Id: pstdio.c,v 1.1 2009-06-18 05:19:12 dhmunro Exp $
 *
 * implement virtual pstdio.h interface
 */
/* Copyright (c) 2009, David H. Munro
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"
#include "pstdio.h"

struct pv_file {
  p_file_ops *ops;
};

unsigned long
p_fsize(p_file *file)
{
  p_file_ops *ops = ((struct pv_file *)file)->ops;
  return ops->v_fsize(file);
}

unsigned long
p_ftell(p_file *file)
{
  p_file_ops *ops = ((struct pv_file *)file)->ops;
  return ops->v_ftell(file);
}

int
p_fseek(p_file *file, unsigned long addr)
{
  p_file_ops *ops = ((struct pv_file *)file)->ops;
  return ops->v_fseek(file, addr);
}

char *
p_fgets(p_file *file, char *buf, int buflen)
{
  p_file_ops *ops = ((struct pv_file *)file)->ops;
  return ops->v_fgets(file, buf, buflen);
}

int
p_fputs(p_file *file, const char *buf)
{
  p_file_ops *ops = ((struct pv_file *)file)->ops;
  return ops->v_fputs(file, buf);
}

unsigned long
p_fread(p_file *file, void *buf, unsigned long nbytes)
{
  p_file_ops *ops = ((struct pv_file *)file)->ops;
  return ops->v_fread(file, buf, nbytes);
}

unsigned long
p_fwrite(p_file *file, const void *buf, unsigned long nbytes)
{
  p_file_ops *ops = ((struct pv_file *)file)->ops;
  return ops->v_fwrite(file, buf, nbytes);
}

int
p_feof(p_file *file)
{
  if (file) {
    p_file_ops *ops = ((struct pv_file *)file)->ops;
    return ops->v_feof(file);
  } else {
    return 1;
  }
}

int
p_ferror(p_file *file)
{
  if (file) {
    p_file_ops *ops = ((struct pv_file *)file)->ops;
    return ops->v_ferror(file);
  } else {
    return 1;
  }
}

int
p_fflush(p_file *file)
{
  p_file_ops *ops = ((struct pv_file *)file)->ops;
  return ops->v_fflush(file);
}

int
p_fclose(p_file *file)
{
  if (file) {
    p_file_ops *ops = ((struct pv_file *)file)->ops;
    return ops->v_fclose(file);
  } else {
    return 0;
  }
}
