/*
 * $Id: files.c,v 1.2 2009-05-22 04:02:26 dhmunro Exp $
 * MS Windows version of plib file operations
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"
#include "pstdio.h"
#include "pstdlib.h"
#include "playw.h"

#include <stdio.h>
#include <io.h>
#include <fcntl.h>

struct p_file {
  p_file_ops *ops;
  FILE *fp;  /* use FILE* for text file operations */
  int fd;    /* use file descriptor for binary file operations */
  int binary;
};

static unsigned long pv_fsize(p_file *file);
static unsigned long pv_ftell(p_file *file);
static int pv_fseek(p_file *file, unsigned long addr);

static char *pv_fgets(p_file *file, char *buf, int buflen);
static int pv_fputs(p_file *file, const char *buf);
static unsigned long pv_fread(p_file *file,
                              void *buf, unsigned long nbytes);
static unsigned long pv_fwrite(p_file *file,
                               const void *buf, unsigned long nbytes);

static int pv_feof(p_file *file);
static int pv_ferror(p_file *file);
static int pv_fflush(p_file *file);
static int pv_fclose(p_file *file);

static p_file_ops w_file_ops = {
  &pv_fsize, &pv_ftell, &pv_fseek,
  &pv_fgets, &pv_fputs, &pv_fread, &pv_fwrite,
  &pv_feof, &pv_ferror, &pv_fflush, &pv_fclose };

p_file *
p_fopen(const char *unix_name, const char *mode)
{
  FILE *fp = fopen(w_pathname(unix_name), mode);
  p_file *f = fp? p_malloc(sizeof(p_file)) : 0;
  if (f) {
    f->ops = &w_file_ops;
    f->fp = fp;
    f->fd = _fileno(fp);
    for (; mode[0] ; mode++) if (mode[0]=='b') break;
    f->binary = (mode[0]=='b');
    /* problem:
     *   in _O_TEXT mode, ftell only works properly if the file
     *   really is a DOS file with CRLF newlines; if it is a UNIX
     *   file (like all of the distribution .i files), ftell fails
     * on the other hand, using _O_BINARY makes fgets fail to remove
     *   the CR and fputs not insert CR
     * (may depend on Windows flavor, this is Win2k */
    if (!f->binary) _setmode(f->fd, _O_BINARY);
  }
  return f;
}

p_file *
p_popen(const char *command, const char *mode)
{
  /* WARNING- for non-console programs, returned FILE* causes
   * program to hang forever (according to msdn documentation) */
  FILE *fp = _popen(command, mode);
  p_file *f = fp? p_malloc(sizeof(p_file)) : 0;
  if (f) {
    f->ops = &w_file_ops;
    f->fp = fp;
    f->fd = fileno(fp);
    for (; mode[0] ; mode++) if (mode[0]=='b') break;
    f->binary = (mode[0]=='b') | 2;
  }
  return f;
}

static int
pv_fclose(p_file *file)
{
  int flag = (file->binary&2)? _pclose(file->fp) : fclose(file->fp);
  p_free(file);
  return flag;
}

static char *
pv_fgets(p_file *file, char *buf, int buflen)
{
  char *b = fgets(buf, buflen, file->fp);
  if (b) {
    /* remove CR if line ends with CRLF */
    int n;
    for (n=0 ; n<buflen-2 ; n++) {
      if (!buf[n]) break;
      if (buf[n]=='\015' && buf[n+1]=='\n' && buf[n+2]=='\0')
        buf[n] = '\n', buf[n+1] = '\0';
    }
  }
  return b;
}

static int
pv_fputs(p_file *file, const char *buf)
{
  int n, dn, i;
  char b[1026];
  for (n=0 ;;) {
    for (i=0 ; i<1024 ; i++,buf++) {
      if (!buf[0]) break;
      if (buf[0] == '\n') b[i++] = '\015';
      b[i] = buf[0];
    }
    b[i] = '\0';
    dn = fputs(b, file->fp);
    if (dn < 0) return dn;
    n += dn;
    if (!buf[0]) break;
  }
  return n;
}

static unsigned long
pv_fread(p_file *file, void *buf, unsigned long nbytes)
{
  if (file->binary&1) {
    int fd = fileno(file->fp);
    return _read(fd, buf, nbytes);
  } else {
    return fread(buf, 1, nbytes, file->fp);
  }
}

static unsigned long
pv_fwrite(p_file *file, const void *buf, unsigned long nbytes)
{
  if (file->binary&1) {
    int fd = fileno(file->fp);
    return _write(fd, buf, nbytes);
  } else {
    return fwrite(buf, 1, nbytes, file->fp);
  }
}

static unsigned long
pv_ftell(p_file *file)
{
  if (file->binary&1)
    return _tell(file->fd);
  else /* broken in _O_TEXT mode, see p_fopen */
    return ftell(file->fp);
}

static int
pv_fseek(p_file *file, unsigned long addr)
{
  if (file->binary&1)
    return -(_lseek(file->fd, addr, SEEK_SET)==-1L);
  else
    return fseek(file->fp, addr, SEEK_SET);
}

static int
pv_fflush(p_file *file)
{
  return fflush(file->fp);
}

static int
pv_feof(p_file *file)
{
  return feof(file->fp);
}

static int
pv_ferror(p_file *file)
{
  int flag = ferror(file->fp);
  clearerr(file->fp);
  return flag;
}

static unsigned long
pv_fsize(p_file *file)
{
  return _filelength(file->fd);
}

int
p_remove(const char *unix_name)
{
  return -(!DeleteFile(w_pathname(unix_name)));
}

int
p_rename(const char *unix_old, const char *unix_new)
{
  char old[P_WKSIZ+1];
  old[0] = '\0';
  strncat(old, w_pathname(unix_old), P_WKSIZ);
  return -(!MoveFile(old, w_pathname(unix_new)));
}

char *
p_native(const char *unix_name)
{
  return p_strcpy(w_pathname(unix_name));
}
