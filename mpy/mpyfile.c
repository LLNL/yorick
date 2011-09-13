/*
 * $Id: mpyfile.c,v 1.2 2011-02-11 05:25:42 dhmunro Exp $
 * virtual distributed on_include callback
 */
/* Copyright (c) 2009, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "mpy.h"
#include "play.h"
#include "pstdlib.h"
#include "yapi.h"
#include "ydata.h"
#include "yio.h"
#include <string.h>

/* used in custom main.c (see Makefile) */
PLUG_API int mpy_on_launch(int argc, char *argv[]);

static p_file *mpy_on_include(const char *filename,
                              int fullparse);  /* yio.h */
static void mpy_eval_auto(Operand *op);  /* see autold.c */
static void (*mpy0_eval_auto)(Operand *op) = 0;

static char *mpy_fake_argv[3] = {"--no-paths", "-batch", 0};

#define STRLEN_P_1(s) ((s)?strlen(s):0)+1
#define STRCPY_TXT(s) if(s)for(i=0;(*(txt++)=(s)[i]);i++);else *(txt++)='\0'
#define STRGET_TXT(s,t) if(!t[0])t++,s=0;else s=p_strcpy(t),t+=strlen(t)+1

/* modified main calls mpy_on_launch instead of on_launch */
int
mpy_on_launch(int argc, char *argv[])
{
  /* codger creates yinit.c:on_launch, which calls std0.c:y_launch */
  char *txt;
  int ret;
  mpy_initialize(&argc, &argv);
  if (!mpy_size) return on_launch(argc, argv);

  /* on_launch is collective operation, because it queries filesystem
   * to find path for .i files
   */
  if (!mpy_rank) {
    long i;
    /* broadcast yLaunchDir, ySiteDir, yHomeDir, defaultPath */
    long dims[2];
    int batflag = (argc>1) && !strcmp(argv[1],"-batch");
    /* lie about arguments for startup, will put back command line later */
    ret = on_launch(batflag?2:(argc?1:0), argv);
    if (batflag) {
      if (argc > 2) {
        /* mpy_initialize already saved -batch script name */
        for (i=3 ; i<argc ; i++) argv[i-2] = argv[i];
        argc -= 2;
      } else {
        argc = 1;
      }
    }
    /* now that on_launch/y_launch done, put back true command line */
    ym_argc = argc;
    ym_argv = argv;
    dims[0] = 1;
    dims[1] = STRLEN_P_1(yLaunchDir) + STRLEN_P_1(ySiteDir)
      + STRLEN_P_1(yHomeDir) + STRLEN_P_1(y_user_dir)
      + STRLEN_P_1(y_gist_dir) + STRLEN_P_1(y_home_pkg);
    ypush_check(1);  /* we are outside any interpreted function */
    txt = ypush_c(dims);
    STRCPY_TXT(yLaunchDir);
    STRCPY_TXT(ySiteDir);
    STRCPY_TXT(yHomeDir);
    STRCPY_TXT(y_user_dir);
    STRCPY_TXT(y_gist_dir);
    STRCPY_TXT(y_home_pkg);
    mpy_bcast(1);

  } else {
    extern void Y_set_site(int);
    /* need to call on_launch to initialize interpreter for mpy_bcast */
    ret = on_launch(2, mpy_fake_argv);
    mpy_bcast(1);
    txt = ygeta_c(0, (long *)0, (long *)0);
    STRGET_TXT(yLaunchDir, txt);
    STRGET_TXT(ySiteDir, txt);
    STRGET_TXT(yHomeDir, txt);
    STRGET_TXT(y_user_dir, txt);
    STRGET_TXT(y_gist_dir, txt);
    STRGET_TXT(y_home_pkg, txt);
    Y_set_site(0);
  }
  yarg_drop(1);
  /* after on_launch, virtual machine initialized, startup .i files
   * (including mpy.i) are queued by name but not yet opened, and
   * batch mode has been set if -batch argv
   */

  /* replace on_include with parallel version, set parallel flag */
  ycall_on_include(mpy_on_include);
  mpy_parallel = 1;

  /* disable autoload */
  if (!mpy0_eval_auto) {
    mpy0_eval_auto = auto_ops.Eval;
    auto_ops.Eval = mpy_eval_auto;
  }

  return ret;
}

static void
mpy_eval_auto(Operand *op)
{
  autoload_t *autl = op->value;
  y_errorq("mpy cannot autoload (symbol = %s), call require function",
           globalTable.names[autl->isymbol]);
}

/* include files only need a subset of the virtual file operations */
static char *mpy_fgets(p_file *file, char *buf, int buflen);
static int mpy_feof(p_file *file);
static int mpy_ferror(p_file *file);
static int mpy_fclose(p_file *file);

static p_file_ops mpy_fopen_ops = {
  0, 0, 0, &mpy_fgets, 0, 0, 0, &mpy_feof, &mpy_ferror, 0, &mpy_fclose };

typedef struct mpy_fopen_t mpy_fopen_t;
struct mpy_fopen_t {
  p_file_ops *ops;
  Array *array;
  long addr;
};

static char *
mpy_fgets(p_file *file, char *buf, int buflen)
{
  mpy_fopen_t *f = (mpy_fopen_t *)file;
  char *txt = f->array->value.c + f->addr;
  long jeof = f->array->type.number - f->addr;
  char c='\0';
  int i, j;
  if (buflen <= 0) return 0;
  for (i=j=0 ; i<buflen-1 && c!='\n' ; i++,j++) {
    if (j >= jeof) break;
    if (!txt || !txt[j]) {
      c = '\n';
    } else if (txt[j] == '\r') {
      if (j<jeof-1 && txt[j+1]=='\n') j++;
      c = '\n';
    } else {
      c = txt[j];
    }
    buf[i] = c;
  }
  buf[i] = '\0';
  f->addr += j;
  return buf;
}

static int
mpy_feof(p_file *file)
{
  mpy_fopen_t *f = (mpy_fopen_t *)file;
  return (f->addr >= f->array->type.number);
}

static int mpy_ferror(p_file *file) { return 0; }

static int
mpy_fclose(p_file *file)
{
  mpy_fopen_t *f = (mpy_fopen_t *)file;
  Array *array = f->array;
  f->array = 0;
  Unref(array);
  p_free(file);
  return 0;
}

static p_file *
mpy_on_include(const char *filename, int fullparse)
{
  mpy_fopen_t *f = 0;
  long len = 0;

  if (!mpy_parallel)
    return p_fopen(filename, "r");

  /* refuse to reopen file for debug line numbers when parallel */
  if (!fullparse) return 0;

  if (mpy_rank == 0) {
    p_file *file = p_fopen(filename, "r");
    long sz = file? p_fsize(file) : 0;
    char *txt;
    long dims[2];
    if (file) len = strlen(filename);
    dims[0] = 1;
    dims[1] = len + 1 + sz;
    ypush_check(1);  /* may be called outside any interpreted function */
    txt = ypush_c(dims);
    if (len) {
      strcpy(txt, filename);
      txt += len + 1;
      if (p_fread(file, txt, sz) != sz) {
        yarg_drop(1);
        y_errorq("rank 0 cannot read include file %s", filename);
        return 0;
      }
    } else {
      txt[0] = '\0';
    }
  }

  mpy_bcast(1);  /* broadcast top of stack to all ranks */
  if (mpy_rank > 0) {
    /* set filename, file contents on all non-0 ranks */
    long sz = 0;
    filename = ygeta_c(0, &sz, (long *)0);
    len = strlen(filename);
  }
  mpy_bcast(0);  /* send acknowledgement back to rank 0 */
  if (!len) return 0;

  f = p_malloc(sizeof(mpy_fopen_t));
  f->ops = &mpy_fopen_ops;
  f->array = yget_use(0);
  f->addr = len + 1;
  yarg_drop(1);

  return (p_file *)f;
}
