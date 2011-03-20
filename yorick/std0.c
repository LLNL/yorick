/*
 * $Id: std0.c,v 1.17 2010-07-06 10:39:34 thiebaut Exp $
 * Define various standard Yorick built-in functions declared in std.i
 *
 *  See std.i for documentation on the functions defined here.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "bcast.h"
#include "yio.h"
#include "defmem.h"
#include "pstdlib.h"
#include "play.h"
#include <string.h>
#include <stdio.h>

#include <errno.h>

extern char *yVersion;
#ifndef Y_VERSION
#include "yversion.h"
#endif
char *yVersion= Y_VERSION;

#ifndef PATH_SEP
#define PATH_SEP ":"
#endif

extern BuiltIn Y_yorick_init, Y_set_path, Y_reshape, Y_array, Y_structof,
  Y_dimsof, Y_orgsof, Y_sizeof, Y_numberof, Y_typeof, Y_nameof, Y_identof,
  Y_is_array, Y_is_scalar, Y_is_vector, Y_is_matrix, Y_is_func, Y_is_void,
  Y_is_range, Y_is_struct, Y_is_stream, Y_is_integer, Y_is_real, Y_is_complex, 
  Y_is_numerical, Y_is_string, Y_is_pointer, Y_am_subroutine, Y_unref, Y_swap,
  Y_sin, Y_cos, Y_tan, Y_asin, Y_acos, Y_atan, Y_sinh, Y_cosh, Y_tanh,
  Y_exp, Y_log, Y_log10, Y_sqrt, Y_ceil, Y_floor, Y_abs, Y_sign, Y_conj,
  Y_min, Y_max, Y_sum, Y_avg, Y_allof, Y_anyof, Y_noneof, Y_nallof, Y_where,
  Y_get_cwd, Y_get_home, Y_cd, Y_get_env, Y_get_argv, Y_use_origins;

extern BuiltIn Y_set_site, Y_timer, Y_get_path, Y_get_pkgnames;
extern BuiltIn Y_round, Y_lround;

/*--------------------------------------------------------------------------*/

extern void InitStructDefs(void);  /* opsv.c */

extern Operand *FormOperandDB(Symbol *owner, Operand *op);
extern DataBlock *ForceToDB(Symbol *s);

extern void BuildDimList(Symbol *stack, int nArgs);  /* ops3.c */

extern int ym_argc;      /* set in main.c */
extern char **ym_argv;

/*--------------------------------------------------------------------------*/

static void YInit(char *home, char *site, y_pkg_t **pkgs);

extern int YGetLaunchDir(char *argv0);  /* sysdep.c */

char *yLaunchDir=0, *ySiteDir=0, *yHomeDir=0, *defaultPath = 0;
char *y_user_dir=0, *y_gist_dir=0, *y_home_pkg=0;

extern int y_on_idle(void);
extern void y_on_stdin(char *input_line);
extern void y_on_exception(int signal, char *errmsg);
extern int y_on_quit(void);
extern void (*g_on_keyline)(char *);
extern void (*g_stdout)(char *);

extern int yBatchMode;  /* may be set with -batch, see std0.c */

static void *y_mmfail(unsigned long n);
static void y_on_keyline(char *msg);

static int get_type(Symbol *s);
/* Returns the type identifier (typeID) of stack symbol S, taking care of
   L-values and of following references.  Keyword symbols yield -1. */

int
y_launch(int argc, char *argv[],
         char *home, char *site, y_pkg_t **pkgs)
{
  ym_argc= argc;
  ym_argv= argv;
  p_mmfail = y_mmfail;
  YInit(home, site, pkgs);
  if (!yBatchMode) {
    p_stdinit(y_on_stdin);
    g_on_keyline = y_on_keyline;
    g_stdout = p_stdout;
  }
  p_idler(y_on_idle);
  p_handler(y_on_exception);
  p_quitter(y_on_quit);
  return 0;
}

static void
y_on_keyline(char *msg)
{
  p_stdout(msg);
  p_stdout("\n");
  y_on_stdin(msg);
}

static void *
y_mmfail(unsigned long n)
{
  if (p_signalling) p_abort();
  if (!n) YError("***SEVERE*** memory manager failed during free");
  sprintf(p_wkspc.c, "memory manager failed to allocate %ld bytes", n);
  YError(p_wkspc.c);
  return 0;
}

void Y_yorick_init(int nArgs)
{
  Drop(nArgs);
  y_pkg_link(0);
}

static void
YInit(char *home, char *site, y_pkg_t **pkgs)
{
  /* initialize Yorick timer and user HOME directory */
  Y_timer(-1);
  YGetHOME();

  /* initialize basic data types */
  InitStructDefs();                /* opsv.c */

  /* add all statically linked packages, and link interpreter to them */
  while (pkgs[0]) y_pkg_add(*pkgs++);
  y_pkg_link(0);

  /* set startup include file search path:
        yLaunchDir:ySiteDir/i0:ySiteDir/contrib

     the switch to the normal include file search path is done in stdx.i
     -- create the default path here:
        .:~/.yorick:~/yorick:~/Yorick:ySiteDir/i:ySiteDir/contrib  */
  if (ym_argc<1 || strcmp(ym_argv[0],"--no-paths")) {
    yLaunchDir = p_strcpy(ym_argc>0? ym_argv[0] : 0);
    if (yLaunchDir) {
      int i;
      for (i=0 ; yLaunchDir[i] ; i++);
      while (i>0 && yLaunchDir[i]!='/') i--;
      if (yLaunchDir[i]=='/') yLaunchDir[i+1] = '\0';
    }
    ySiteDir = site? p_strcpy(site) : p_strcpy("/");
    YNameToHead(&ySiteDir);
    yHomeDir = home? p_strcpy(home) : p_strcpy("/");
    YNameToHead(&yHomeDir);
    Y_set_site(-1);
  } else {
    /* yLaunchDir, ySiteDir, yHomeDir, defaultPath will be set elsewhere
     * - also y_user_dir, y_gist_dir
     */
    Y_set_site(-2);
  }

  /* set up to read i-start customizations
     If the first two arguments are -batch alternate.i, use alternate.i
     -- useful for scripts which must NOT risk user customization.  */
  if (ym_argc>1 && !strcmp(ym_argv[1],"-batch")) {
    yBatchMode= 1;  /* data loaded to zero in task.c */
    if (ym_argc==2) {
      ym_argc= 1;
    } else {
      int i= 3;
      YpPush(ym_argv[2]);
      while (i<ym_argc) { ym_argv[i-2]= ym_argv[i]; i++; }
      ym_argc-= 2;
    }
  }

  /* final include file is always stdx.i, which returns to defaultPath */
  YpPush("stdx.i");

  /* next include all the statically loaded packages */
  y_pkg_include(0, 0);

  /* first include file is always paths.i, which may override ySiteDir */
  YpPush("paths.i");
}

void Y_set_path(int nArgs)
{
  char *path;
  if (nArgs<1) path= defaultPath;
  else if (nArgs==1) path= YGetString(sp);
  else { YError("set_path takes at most one argument"); path= 0; }
  YpSetPaths(path);
}

void Y_get_path(int nArgs)
{
  char *path, *p;
  Array *rslt;
  long i, len = 1;
  if (nArgs!=1 || !yarg_nil(0)) YError("get_path takes one nil argument");
  for (i=0 ; i<nYpPrefixes ; i++) len += strlen(ypPrefixes[i])+1;
  rslt = (Array *)PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  rslt->value.q[0] = path = p_malloc(len);
  for (i=0 ; i<nYpPrefixes ; i++) {
    p = ypPrefixes[i];
    while (p[0]) *path++ = *p++;
    *path++ = ':';
  }
  if (nYpPrefixes) path--;
  path[0] = '\0';
}

void
Y_get_pkgnames(int nargs)
{
  int i, npkg;
  Array *rslt;
  Dimension *tmp = tmpDims;
  if (nargs!=1) YError("get_pkgnames takes exactly one argument");
  npkg = y_pkg_count(yarg_si(0));
  if (npkg<=0) YError("get_pkgnames -- no packages loaded yet");
  tmpDims = 0;
  FreeDimension(tmp);
  tmpDims = NewDimension((long)npkg, 1L, (Dimension *)0);
  rslt = (Array *)PushDataBlock(NewArray(&stringStruct, tmpDims));
  for (i=0 ; i<npkg ; i++)
    rslt->value.q[i] = p_strcpy(y_pkg_name(i));
}

static char *y_make_ipath(char *ylaunch, char *ysite, char *yhome);
static void y_set_globvar(const char *globname, const char *value, int tohead);

void
Y_set_site(int nArgs)
{
  /* This function should ONLY be called from paths.i, or by YInit  */
  char *path;

  if (nArgs>2) YError("expecting zero, one, or two arguments");

  /* record Y_LAUNCH in global variable */
  y_set_globvar("Y_LAUNCH", yLaunchDir, 0);

  if (nArgs >= 1) {
    if (ySiteDir) p_free(ySiteDir);
    ySiteDir = p_strcpy(YGetString(sp-nArgs+1));
    YNameToHead(&ySiteDir);
    if (nArgs==2) {
      if (yHomeDir) p_free(yHomeDir);
      yHomeDir = p_strcpy(YGetString(sp));
      YNameToHead(&yHomeDir);
    }
  } else if (nArgs == -1) {
#ifndef NO_FALLBACK_TEST
    /* always look for relocatable installation first */
    p_file *test = 0;
    long i = yLaunchDir? strlen(yLaunchDir) : 0;
    long iparent = 0;
    if (i > 1) {
      /* first try Y_SITE = parent of Y_LAUNCH */
      for (i-=2 ; i>0 ; i-=2)
        if (yLaunchDir[i-1]!='/' || yLaunchDir[i]!='.') break;
      for (; i>0 ; i--) if (yLaunchDir[i]=='/') break;
      /* yLaunchDir[i] is / at end of parent of Y_LAUNCH directory */
      path = p_malloc(i+10);
      strncpy(path, yLaunchDir, i+1);
      strcpy(path+i+1, "i0/std.i");
      test = p_fopen(path, "r");
      if (!test) {
        /* then check for Y_LAUNCH/Y_HOME.txt file */
        p_free(path);
        path = p_strncat(yLaunchDir, "Y_HOME.txt", 0);
        test = p_fopen(path, "r");
        if (test) {  /* get Y_HOME from Y_HOME.txt file */
          p_wkspc.c[P_WKSIZ] = '\0';
          if (p_fgets(test, p_wkspc.c, P_WKSIZ)) {
            iparent = i+1;  /* yLaunchDir length to parent / */
            for (i=0 ; p_wkspc.c[i] ; i++) if (p_wkspc.c[i] == '\n') break;
            if (i>0 && p_wkspc.c[i-1]!='/') p_wkspc.c[i++] = '/';
            p_wkspc.c[i] = '\0';
            i--;
            path = p_strncat(p_wkspc.c, "i0/std.i", 0);
            test = p_fopen(path, "r");
          }
        }
      }
      if (test) {
        p_fclose(test);
        path[i+1] = '\0';
        ySiteDir = path;
        /* Y_HOME == Y_SITE for all relocatable installations */
        yHomeDir = p_strcpy(ySiteDir);
        if (iparent) {
          y_home_pkg = p_strcpy(yLaunchDir);
          y_home_pkg[iparent] = '\0';
        }
      } else {
        p_free(path);
      }
    }
    /* if !test, this is not relocatable installation
     * we have no choice but to assume ySiteDir and yHomeDir are correct
     */
#endif
  }

  /* record Y_VERSION in global variable */
  y_set_globvar("Y_VERSION", yVersion, 0);

  /* record Y_HOME in global variable */
  y_set_globvar("Y_HOME", yHomeDir, 1);

  /* record Y_SITE in global variable */
  y_set_globvar("Y_SITE", ySiteDir, 1);

  y_set_globvar("Y_HOME_PKG", y_home_pkg, 1);

  /* note: by default, this is never called from paths.i at all */

  if (nArgs == -1) {
    /* set the initial include path for paths.i
     * -- if nArgs>=0, this call is from paths.i, just set defaultPath
     */
    path = y_make_ipath(yLaunchDir, ySiteDir, yHomeDir);
    YpSetPaths(path);
    p_free(path);
  }

  if (nArgs > -2) {
    defaultPath = y_make_ipath(0, ySiteDir, yHomeDir);
    if (nArgs >= 0) YpSetPaths(defaultPath);  /* from paths.i */
  }
  y_set_globvar("Y_USER", y_user_dir, 1);
  y_set_globvar("Y_GISTDIR", y_gist_dir, 0);  /* used in paths.i */
}

static void
y_set_globvar(const char *globname, const char *value, int tohead)
{
  /* record Y_HOME in global variable */
  long index = Globalize(globname, 0L);
  DataBlock *oldDB =
    globTab[index].ops==&dataBlockSym? globTab[index].value.db : 0;
  Array *dirName = NewArray(&stringStruct, (Dimension *)0);
  globTab[index].value.db = (DataBlock *)dirName;
  globTab[index].ops = &dataBlockSym;
  Unref(oldDB);
  dirName->value.q[0] = p_strcpy(value);
  if (tohead && value) YNameToHead(dirName->value.q);
}

static char *
y_make_ipath(char *ylaunch, char *ysite, char *yhome)
{
  /*~/.yorick:~/yorick:~/Yorick:Y_SITE/i:Y_SITE/contrib:Y_SITE/i0:Y_HOME/lib*/\
  char *path1 = 0, *path2 = 0;
  if (!y_user_dir) {
    char *yuser = "~/.yorick/";      /* preferred user directory */
    p_dir *yudir = p_dopen("~/.yorick");
    if (!yudir) {
      yudir = p_dopen("~/yorick");   /* unhidden user directory */
      if (yudir) yuser = "~/yorick/";
      else {
        yudir = p_dopen("~/Library/Yorick");             /* Mac OS X */
        if (yudir) yuser = "~/Library/Yorick/";
        else {
          yudir = p_dopen("~/Application Data/Yorick");  /* Windows */
          if (yudir) yuser = "~/Application Data/Yorick/";
          else {
            yudir = p_dopen("~/Yorick");  /* obsolete choice */
            if (yudir) yuser = "~/Yorick/";
          }
        }
      }
    }
    if (yudir) p_dclose(yudir);
    y_user_dir = p_strcpy(yuser);
  }
  YNameToHead(&y_user_dir);
  if (!y_gist_dir) {
    p_dir *yudir;
    y_gist_dir = p_strncat(y_user_dir, "gist", 0);
    yudir = p_dopen(y_gist_dir);
    if (!yudir) {  /* for backwards compatibility */
      char *yuser = "~/.gist";
      yudir = p_dopen(yuser);
      if (!yudir) {
        yuser = "~/gist";
        yudir = p_dopen(yuser);
        if (!yudir) {
          yuser = "~/Gist";
          yudir = p_dopen(yuser);
        }
      }
      if (yudir) {
        p_dclose(yudir);
        p_free(y_gist_dir);
        y_gist_dir = p_strcpy(yuser);
      }
    }
  }
  if (ylaunch && ylaunch[0]) {
    /* prepend Y_LAUNCH for paths.i only */
    long len = strlen(ylaunch);
    path2 = (ylaunch[len-1]=='/')? p_strncat(0, ylaunch, len-1) :
      p_strcpy(ylaunch);
    path1 = p_strncat(path2, PATH_SEP, 0);
    p_free(path2);
  } else {
    path1 = p_strcpy("." PATH_SEP);
  }
  path2 = p_strncat(path1, y_user_dir, 0);
  if (path1) p_free(path1);
  if (path2[strlen(path2)-1] == '/') path2[strlen(path2)-1] = '\0';
  path1 = p_strncat(path2, PATH_SEP, 0);
  if (y_home_pkg) {
    p_free(path2);
    path2 = p_strncat(path1, y_home_pkg, 0);
    p_free(path1);
    path1 = p_strncat(path2, "i" PATH_SEP, 0);
    p_free(path2);
    path2 = p_strncat(path1, y_home_pkg, 0);
    p_free(path1);
    path1 = p_strncat(path2, "i0" PATH_SEP, 0);
    p_free(path2);
    path2 = p_strncat(path1, y_home_pkg, 0);
    p_free(path1);
    path1 = p_strncat(path2, "lib" PATH_SEP, 0);
  }
  p_free(path2);
  path2 = p_strncat(path1, ysite, 0);
  p_free(path1);
  path1 = p_strncat(path2, "i" PATH_SEP, 0);
  p_free(path2);
  path2 = p_strncat(path1, ysite, 0);
  p_free(path1);
  path1 = p_strncat(path2, "contrib" PATH_SEP, 0);
  p_free(path2);
  path2 = p_strncat(path1, ysite, 0);
  p_free(path1);
  path1 = p_strncat(path2, "i0" PATH_SEP, 0);
  p_free(path2);
  path2 = p_strncat(path1, yhome, 0);
  p_free(path1);
  path1 = p_strncat(path2, "lib", 0);
  p_free(path2);
  return path1;
}

/*--------------------------------------------------------------------------*/

void Y_reshape(int nArgs)
{
  Symbol *arg= sp-nArgs+1;
  long index;
  DataBlock *db;
  if (nArgs<1 || arg->ops!=&referenceSym)
    YError("first argument to reshape must be variable reference");

  index= arg->index;
  arg++;
  nArgs--;
  db= globTab[index].value.db;  /* might not be meaningful... */

  if (!nArgs) {
    /* reshape, var
       same as var=[], but works for LValues as well */
    globTab[index].value.db= RefNC(&nilDB);
    if (globTab[index].ops==&dataBlockSym) { Unref(db); }
    else globTab[index].ops= &dataBlockSym;
    Drop(1);

  } else {
    StructDef *base= 0;
    void *address= 0;
    Array *owner= 0;
    LValue *result;
    DataBlock *adb= 0;

    if (arg->ops==&referenceSym) ReplaceRef(arg);
    if (arg->ops==&dataBlockSym) {
      adb= arg->value.db;
      if (adb->ops==&structDefOps) {
        /* reshape, var, ->type, dimlist
           uses address of the current value of var */
        base= (StructDef *)adb;
        db= ForceToDB(&globTab[index]);
        if (db->ops==&lvalueOps) {
          LValue *lvalue= (LValue *)db;
          if (lvalue->type.base->file)
            YError("cannot reshape a disk-resident variable");
          address= lvalue->address.m;
          owner= lvalue->owner;
        } else if (db->ops->isArray) {
          Array *array= (Array *)db;
          address= array->value.c;
          owner= array;
        } else {
          YError("cannot reshape non-array object");
        }

      } else if ((adb->ops==&pointerOps ||
                  adb->ops==&intOps || adb->ops==&longOps) &&
                 !((Array *)adb)->type.dims) {
        /* reshape, var, ->address, type, dimlist */
        Array *array= (Array *)adb;
        if (adb->ops==&pointerOps) {
          address= array->value.p[0];
          owner= Pointee(address);
        } else if (adb->ops==&intOps) {
          address= (char *)0 + array->value.i[0];
          owner= 0;
        } else {
          address= (char *)0 + array->value.l[0];
          owner= 0;
        }

      } else {
      badd:
        YError("reshape address must be scalar pointer, int, or long");
      }
    } else if (arg->ops==&intScalar) {
      /* reshape, var, ->address, type, dimlist */
      address= (char *)0 + arg->value.i;
      owner= 0;
    } else if (arg->ops==&longScalar) {
      /* reshape, var, ->address, type, dimlist */
      address= (char *)0 + arg->value.l;
      owner= 0;
    } else {
      goto badd;
    }

    arg++;
    nArgs--;
    if (!base) {
      if (!nArgs) YError("no data type specified for reshape");
      /* reshape, var, address, ->type, dimlist */
      if (arg->ops==&referenceSym) ReplaceRef(arg);
      if (arg->ops==&dataBlockSym && arg->value.db->ops==&structDefOps)
        base= (StructDef *)arg->value.db;
      else
        YError("bad data type specified for reshape");
      arg++;
      nArgs--;
    }

    BuildDimList(arg, nArgs);
    result= PushDataBlock(NewLValueM(owner, address, base, tmpDims));

    if (owner) {
      char *end= owner->value.c +
        owner->type.number*owner->type.base->size;
      char *last= result->address.m +
        result->type.number*result->type.base->size;
      if (last>end) {
        Drop(1);
        YError("reshape beyond bounds of array owner");
      }
    }

    PopTo(&globTab[index]);
  }
}

/*--------------------------------------------------------------------------*/

void Y_array(int nArgs)
{
  Symbol *stack= sp-nArgs+1;
  StructDef *base;
  Operand op;
  Dimension *dims;
  void *value;
  Array *result;
  if (nArgs < 1) YError("too few arguments to array() function");

  /* get type or value argument */
  if (!stack->ops) goto barf;
  stack->ops->FormOperand(stack, &op);
  if (op.ops==&structDefOps) {
    dims= 0;
    base= op.value;
    value= 0;
  } else if (op.ops->isArray) {
    dims= op.type.dims;
    base= op.type.base;
    value= op.value;
  } else {
  barf:
    YError("first argument to array() must be either type or value");
    dims= 0;
    base= 0;
    value= 0;
  }

  BuildDimList(stack+1, nArgs-1);
  if (dims) {
    if (tmpDims) {
      Dimension *d= tmpDims;
      while (d->next) d= d->next;
      d->next= Ref(dims);
    } else {
      tmpDims= Ref(dims);
    }
  }

  /* push result Array, then fill it with either value or 0 */
  result= PushDataBlock(NewArray(base, tmpDims));
  if (value) {
    Broadcast(result->value.c, tmpDims, value, dims, base);
  } else if (base->Copy==&CopyX) {
    memset(result->value.c, 0, result->type.number*result->type.base->size);
  }
}

/*--------------------------------------------------------------------------*/

static Member type;

/* sets static type variable and returns 0, or sets type.base==0 and
   returns DataBlock* to non-Array object */
static DataBlock *GetInfo(Symbol *s);

static DataBlock *GetInfo(Symbol *s)
{
  DataBlock *db= 0;
  for (;;) {
    if (s->ops==&doubleScalar) {
      type.base= &doubleStruct;
      type.dims= 0;
      type.number= 1;
      break;
    } else if (s->ops==&longScalar) {
      type.base= &longStruct;
      type.dims= 0;
      type.number= 1;
      break;
    } else if (s->ops==&intScalar) {
      type.base= &intStruct;
      type.dims= 0;
      type.number= 1;
      break;
    } else if (s->ops==&dataBlockSym) {
      db= s->value.db;
      if (db->ops==&lvalueOps) {
        LValue *lvalue= (LValue *)db;
        type.base= lvalue->type.base;
        type.dims= lvalue->type.dims;
        type.number= lvalue->type.number;
      } else if (db->ops->isArray) {
        Array *array= (Array *)db;
        type.base= array->type.base;
        type.dims= array->type.dims;
        type.number= array->type.number;
      } else {
        type.base= 0;
        type.dims= 0;
        type.number= 0;
      }
      break;
    } else if (s->ops==&referenceSym) {
      s= &globTab[s->index];
    } else {
      YError("unexpected keyword argument");
    }
  }
  return type.base? 0 : db;
}

void Y_structof(int nArgs)
{
  DataBlock *db;
  if (nArgs != 1) YError("structof takes exactly one argument");
  db= GetInfo(sp);
  if (!db) PushDataBlock(Ref(type.base));
  else PushDataBlock(RefNC(&nilDB));
}

Dimension *dimsofDims= 0;

void Y_dimsof(int nArgs)
{
  DataBlock *db;
  Symbol *stack= sp-nArgs+1;
  Dimension *tmp= dimsofDims;
  dimsofDims= 0;
  FreeDimension(tmp);
  if (nArgs < 1) YError("dimsof requires at least one argument");

  db= GetInfo(stack++);
  nArgs--;
  dimsofDims= Ref(type.dims);

  while (nArgs--) {
    db= GetInfo(stack++);
    if (db) break;
    if (Conform(dimsofDims, type.dims) & 4) {
      db= (DataBlock *)&charStruct;  /* anything non-0 */
      break;
    }
    tmp= dimsofDims;
    dimsofDims= 0;
    FreeDimension(tmp);
    dimsofDims= Ref(tmpDims);
  }

  if (!db) {
    int nDims= CountDims(dimsofDims);
    long *l;
    Array *result;
    tmp= tmpDims;
    tmpDims= 0;
    FreeDimension(tmp);
    tmpDims= NewDimension((long)(nDims+1), 1L, (Dimension *)0);
    result= PushDataBlock(NewArray(&longStruct, tmpDims));
    l= result->value.l;
    *l= nDims;
    l+= nDims;
    tmp= dimsofDims;
    while (tmp) {
      *l--= tmp->number;
      tmp= tmp->next;
    }
  } else {
    PushDataBlock(RefNC(&nilDB));
  }
}

void Y_orgsof(int nArgs)
{
  DataBlock *db;
  if (nArgs != 1) YError("orgsof takes exactly one argument");
  db= GetInfo(sp);
  if (!db) {
    int nDims= CountDims(type.dims);
    long *l;
    Array *result;
    Dimension *tmp= tmpDims;
    tmpDims= 0;
    FreeDimension(tmp);
    tmpDims= NewDimension((long)(nDims+1), 1L, (Dimension *)0);
    result= PushDataBlock(NewArray(&longStruct, tmpDims));
    l= result->value.l;
    *l= nDims;
    l+= nDims;
    while (type.dims) {
      *l--= yForceOrigin? 1L : type.dims->origin;
      type.dims= type.dims->next;
    }
  } else {
    PushDataBlock(RefNC(&nilDB));
  }
}

void Y_sizeof(int nArgs)
{
  DataBlock *db;
  long size;
  if (nArgs != 1) YError("sizeof takes exactly one argument");
  db= GetInfo(sp);
  if (!db) {
    size= type.number*type.base->size;
  } else if (db->ops==&structDefOps) {
    size= ((StructDef *)db)->size;
  } else if (db->ops==&streamOps) {
    /* return length of file in bytes */
    IOStream *file= (IOStream *)db;
    IOOperations *ioOps= file->ioOps;
    long address;
    if (file->history) file= file->history->child;
    ioOps= file->ioOps;
    address= ioOps->Tell(file, 0L);
    ioOps->SeekEnd(file, 0L);
    size= ioOps->Tell(file, 0L);
    ioOps->Seek(file, address);
  } else {
    size= 0;
  }
  Drop(2);
  PushLongValue(size);
}

void Y_numberof(int nArgs)
{
  DataBlock *db;
  long number;
  if (nArgs != 1) YError("numberof takes exactly one argument");
  db= GetInfo(sp);
  if (!db) number= type.number;
  else number= 0;
  Drop(2);
  PushLongValue(number);
}

void Y_typeof(int nArgs)
{
  DataBlock *db;
  char *typeName;
  Array *result;
  if (nArgs != 1) YError("typeof takes exactly one argument");
  db= GetInfo(sp);
  if (!db) typeName= type.base->dataOps->typeName;
  else typeName= db->ops->typeName;
  Drop(2);
  result= PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  result->value.q[0]= p_strcpy(typeName);
}

void Y_nameof(int nArgs)
{
  DataBlock *db;
  char *name= 0;
  if (nArgs != 1) YError("nameof takes exactly one argument");

  db= GetInfo(sp);
  if (!db) name= 0;
  else if (db->ops==&structDefOps) {
    name= StructName((StructDef *)db);
  } else {
    long index;
    if (db->ops==&functionOps) index= ((Function *)db)->code[0].index;
    else if (db->ops==&builtinOps) index= ((BIFunction *)db)->index;
    else if (db->ops==&auto_ops) index= ((autoload_t *)db)->isymbol;
    else index= -1;
    if (index>=0) name= globalTable.names[index];
    else name= 0;
  }

  Drop(2);
  if (!name) {
    PushDataBlock(RefNC(&nilDB));
  } else {
    Array *result= PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
    result->value.q[0]= p_strcpy(name);
  }
}

/*--------------------------------------------------------------------------*/

void
Y_is_array(int nargs)
{
  int result;
  if (nargs != 1) YError("is_array takes exactly one argument");
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  if (sp->ops==&dataBlockSym) {
    Operations *ops= sp->value.db->ops;
    result = (ops==&lvalueOps || ops->isArray);
  } else if (sp->ops==&intScalar || sp->ops==&longScalar ||
             sp->ops==&doubleScalar) {
    result = 1;
  } else {
    result = 0;
  }
  PushIntValue(result);
}

void
Y_is_scalar(int nargs)
{
  int result = 0;
  if (nargs != 1) YError("is_scalar takes exactly one argument");
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  if (sp->ops == &dataBlockSym) {
    Operations *ops =  sp->value.db->ops;
    if (ops->isArray)
      result = (((Array *)sp->value.db)->type.dims == (Dimension *)0);
    else if (ops == &lvalueOps)
      result = (((LValue *)sp->value.db)->type.dims == (Dimension *)0);
  } else if (sp->ops==&intScalar || sp->ops==&longScalar ||
             sp->ops==&doubleScalar) {
    result = 1;
  }
  PushIntValue(result);
}

void
Y_is_vector(int nargs)
{
  int result = 0;
  if (nargs != 1) YError("is_vector takes exactly one argument");
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  if (sp->ops == &dataBlockSym) {
    Dimension *dims;
    Operations *ops =  sp->value.db->ops;
    if (ops->isArray) dims = ((Array *)sp->value.db)->type.dims;
    else if (ops == &lvalueOps) dims = ((LValue *)sp->value.db)->type.dims;
    else dims = 0;
    result = (dims && !dims->next);
  }
  PushIntValue(result);
}

void
Y_is_matrix(int nargs)
{
  int result = 0;
  if (nargs != 1) YError("is_matrix takes exactly one argument");
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  if (sp->ops == &dataBlockSym) {
    Dimension *dims;
    Operations *ops =  sp->value.db->ops;
    if (ops->isArray) dims = ((Array *)sp->value.db)->type.dims;
    else if (ops == &lvalueOps) dims = ((LValue *)sp->value.db)->type.dims;
    else dims = 0;
    result = (dims && dims->next && !dims->next->next);
  }
  PushIntValue(result);
}

void
Y_is_func(int nArgs)
{
  if (nArgs != 1) YError("is_func takes exactly one argument");
  PushIntValue(yarg_func(0));
}

void Y_is_void(int nArgs)
{
  Symbol *s= sp;
  int isVoid;
  if (nArgs != 1) YError("is_void takes exactly one argument");

  for (;;) {
    if (s->ops==&dataBlockSym) {
      Operations *ops= s->value.db->ops;
      isVoid= (ops==&voidOps || ops==&auto_ops);
      break;
    } else if (s->ops!=&referenceSym) {
      isVoid= 0;
      break;
    }
    s= &globTab[s->index];
  }

  PushIntValue(isVoid);
}

void
Y_is_range(int nArgs)
{
  int result = 0;
  if (nArgs != 1) YError("is_range takes exactly one argument");
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  if (sp->ops==&dataBlockSym) result = (sp->value.db->ops==&rangeOps);
  PushIntValue(result);
}

void
Y_is_struct(int nArgs)
{
  int result = 0;
  if (nArgs != 1) YError("is_struct takes exactly one argument");
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  if (sp->ops==&dataBlockSym) result= (sp->value.db->ops==&structDefOps);
  PushIntValue(result);
}

void
Y_is_stream(int nArgs)
{
  int result = 0;
  if (nArgs != 1) YError("is_stream takes exactly one argument");
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  if (sp->ops==&dataBlockSym) result = (sp->value.db->ops==&streamOps);
  PushIntValue(result);
}

void Y_is_integer(int nargs)
{
  int type;
  if (nargs != 1) YError("is_integer takes exactly one argument");
  type = get_type(sp);
  PushIntValue(type >= T_CHAR && type <= T_LONG);
}

void Y_is_real(int nargs)
{
  int type;
  if (nargs != 1) YError("is_real takes exactly one argument");
  type = get_type(sp);
  PushIntValue(type == T_DOUBLE || type == T_FLOAT);
}

void Y_is_complex(int nargs)
{
  if (nargs != 1) YError("is_complex takes exactly one argument");
  PushIntValue(get_type(sp) == T_COMPLEX);
}

void Y_is_numerical(int nargs)
{
  int result;
  if (nargs != 1) YError("is_numerical takes exactly one argument");
  switch (get_type(sp)) {
  case T_CHAR:
  case T_SHORT:
  case T_INT:
  case T_LONG:
  case T_FLOAT:
  case T_DOUBLE:
  case T_COMPLEX:
    result = 1;
    break;
  default:
    result = 0;
  }
  PushIntValue(result);
}

void Y_is_string(int nargs)
{
  if (nargs != 1) YError("is_string takes exactly one argument");
  PushIntValue(get_type(sp) == T_STRING);
}

void Y_is_pointer(int nargs)
{
  if (nargs != 1) YError("is_pointer takes exactly one argument");
  PushIntValue(get_type(sp) == T_POINTER);
}

void Y_identof(int argc)
{
  long type;
  if (argc != 1) YError("identof takes exactly one argument");
  type = get_type(sp);
  if (type == -1) YError("unexpected keyword argument");
  PushLongValue(type);
}

static int get_type(Symbol *s)
{
  for (;;) {
    if (s->ops == &dataBlockSym) {
      DataBlock *db = s->value.db;
      if (db->ops == &lvalueOps) {
	return ((LValue *)db)->type.base->dataOps->typeID;
      } else {
        return db->ops->typeID;
      }
    } else if (s->ops == &referenceSym) {
      s = &globTab[s->index];
    } else if (s->ops == &doubleScalar) {
      return T_DOUBLE;
    } else if (s->ops == &longScalar) {
      return T_LONG;
    } else if (s->ops == &intScalar) {
      return T_INT;
    } else {
      /* Must be a keyword. */
      return -1;
    }
  }
}

/*--------------------------------------------------------------------------*/

void Y_unref(int argc)
{
  if (argc != 1) YError("unref takes exactly one argument");
  if (sp->ops == &referenceSym) {
    /* Replace reference without augmenting the reference
       count of the data block object if it is an array. */
    Symbol *ref = &globTab[sp->index];
    OpTable *ops = ref->ops;
    if (ops == &dataBlockSym) {
      DataBlock *db = ref->value.db;
      if (db && db->ops->isArray) {
	/* Replace symbol in global table by nil. */
	ref->value.db = RefNC(&nilDB);
	sp->value.db = db; /* no Ref */
      } else {
	sp->value.db = Ref(db);
      }
    } else {
      sp->value = ref->value;
    }
    sp->ops = ops; /* change ops only AFTER value updated */
  }
}

void Y_swap(int argc)
{
  SymbolValue a_val, b_val;
  OpTable *a_ops, *b_ops;
  volatile Symbol *a_sym, *b_sym;
  if (argc != 2) YError("swap takes exactly 2 arguments");
  a_sym = sp;
  b_sym = sp-1;
  if (a_sym->ops != &referenceSym || b_sym->ops != &referenceSym)
    YError("arguments must be simple variable references");
  a_sym = &globTab[a_sym->index];
  a_ops = a_sym->ops;
  a_val = a_sym->value;
  a_sym->ops = &intScalar;
  b_sym = &globTab[b_sym->index];
  b_ops = b_sym->ops;
  b_val = b_sym->value;
  b_sym->ops = &intScalar;
  b_sym->value = a_val;
  a_sym->value = b_val;
  Drop(2);
  b_sym->ops = a_ops;
  a_sym->ops = b_ops;
}

/*--------------------------------------------------------------------------*/

extern VMaction DropTop;
void Y_am_subroutine(int nArgs)
{
  Symbol *stack= sp;
  Instruction *pcRet= 0;
  while (stack>spBottom) {
    if (stack->ops==&returnSym) {
      pcRet= stack->value.pc;
      break;
    }
    stack--;
  }
  if (!pcRet) YError("am_subroutine lost return pc -- corrupt stack?");
  PushIntValue(pcRet->Action==&DropTop);
}

/*--------------------------------------------------------------------------*/

extern void *BuildResultU(Operand *op, StructDef *base);
extern void *BuildResult2(Operand *l, Operand *r);
extern void PopToI(Symbol *s);
extern void PopToL(Symbol *s);
extern void PopToD(Symbol *s);

static void PopToX(Symbol *s, int typeID);

static void PopToX(Symbol *s, int typeID)
{
  if (typeID==T_INT) PopToI(s);
  else if (typeID==T_LONG) PopToL(s);
  else if (typeID==T_DOUBLE) PopToD(s);
  else PopTo(s);
}

static Array *Force1D(Operand *op);

typedef void Looper(double *dst, double *src, long n);

static void UnaryTemplate(int nArgs, Looper *DLooper, Looper *ZLooper);

static void UnaryTemplate(int nArgs, Looper *DLooper, Looper *ZLooper)
{
  Operand op;
  int promoteID;
  if (nArgs != 1) YError("expecting exactly one argument");
  sp->ops->FormOperand(sp, &op);
  promoteID= op.ops->promoteID;
  if (promoteID<=T_DOUBLE) {
    if (promoteID<T_DOUBLE) op.ops->ToDouble(&op);
    errno = 0;
    DLooper(BuildResultU(&op, &doubleStruct), op.value, op.type.number);
    promoteID = errno;
    PopToD(sp-2);
  } else {
    if (promoteID>T_COMPLEX) YError("expecting numeric argument");
    errno = 0;
    ZLooper(BuildResultU(&op, &complexStruct), op.value, 2*op.type.number);
    promoteID = errno;
    PopTo(sp-2);
  }
  if (promoteID) YError("mathlib function signals error");
  Drop(1);
}

/* ANSI standard math.h functions */
extern double sin(double);
extern double cos(double);
extern double tan(double);
extern double asin(double);
extern double acos(double);
extern double atan(double);
extern double atan2(double, double);
extern double sinh(double);
extern double cosh(double);
extern double tanh(double);
extern double exp(double);
extern double log(double);
extern double log10(double);
extern double sqrt(double);
extern double ceil(double);
extern double floor(double);
#ifdef HAVE_ROUND
extern double round(double);
#endif
#ifdef HAVE_LROUND
extern long lround(double);
#endif

/* function either present in math library or implemented in nonc.c */
extern double hypot(double, double);

/* complex equivalents are defined in nonc.c */
extern void sinZ(double z[2], double x[2]);
extern void cosZ(double z[2], double x[2]);
extern void tanZ(double z[2], double x[2]);
extern void asinZ(double z[2], double x[2]);
extern void acosZ(double z[2], double x[2]);
extern void atanZ(double z[2], double x[2]);
extern void sinhZ(double z[2], double x[2]);
extern void coshZ(double z[2], double x[2]);
extern void tanhZ(double z[2], double x[2]);
extern void expZ(double z[2], double x[2]);
extern void logZ(double z[2], double x[2]);
extern void sqrtZ(double z[2], double x[2]);

extern void signZ(double z[2], double x[2]);

/* ----- pi ----- */

double y_pi;   /* value filled in by interpreter */

/* ----- sin ----- */

static Looper sinLoop, sinZLoop;

static void sinLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= sin(src[i]); }
static void sinZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i+=2) sinZ(&dst[i], &src[i]); }

void Y_sin(int nArgs) { UnaryTemplate(nArgs, &sinLoop, &sinZLoop); }

/* ----- cos ----- */

static Looper cosLoop, cosZLoop;

static void cosLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= cos(src[i]); }
static void cosZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i+=2) cosZ(&dst[i], &src[i]); }

void Y_cos(int nArgs) { UnaryTemplate(nArgs, &cosLoop, &cosZLoop); }

/* ----- tan ----- */

static Looper tanLoop, tanZLoop;

static void tanLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= tan(src[i]); }
static void tanZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i+=2) tanZ(&dst[i], &src[i]); }

void Y_tan(int nArgs) { UnaryTemplate(nArgs, &tanLoop, &tanZLoop); }

/* ----- asin ----- */

static Looper asinLoop, asinZLoop;

static void asinLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i++) dst[i]= asin(src[i]); }
static void asinZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i+=2) asinZ(&dst[i], &src[i]); }

void Y_asin(int nArgs) { UnaryTemplate(nArgs, &asinLoop, &asinZLoop); }

/* ----- acos ----- */

static Looper acosLoop, acosZLoop;

static void acosLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i++) dst[i]= acos(src[i]); }
static void acosZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i+=2) acosZ(&dst[i], &src[i]); }

void Y_acos(int nArgs) { UnaryTemplate(nArgs, &acosLoop, &acosZLoop); }

/* ----- atan ----- */

static Looper atanLoop, atanZLoop;

static void atanLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= atan(src[i]); }
static void atanZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i+=2) atanZ(&dst[i], &src[i]); }

void Y_atan(int nArgs)
{
  if (nArgs<2) UnaryTemplate(nArgs, &atanLoop, &atanZLoop);
  else if (nArgs>2) YError("atan takes one or two arguments");
  else {
    Operand opy, opx;
    double *y, *x, *dst;
    Operations *ops;
    long i;
    sp->ops->FormOperand(sp, &opx);
    if (opx.ops->promoteID<T_DOUBLE) opx.ops->ToDouble(&opx);
    (sp-1)->ops->FormOperand(sp-1, &opy);
    ops= opy.ops->Promote[opx.ops->promoteID](&opy, &opx);
    if (!ops ||
        ops->promoteID>T_DOUBLE) YError("illegal data type to atan(y, x)");
    dst= BuildResult2(&opy, &opx);
    if (!dst) YError("operands not conformable in binary atan");
    y= opy.value;
    x= opx.value;
    errno = 0;
    for (i=0 ; i<opy.type.number && !errno ; i++) dst[i]= atan2(y[i], x[i]);
    if (errno) YError("mathlib atan2() function signals error");
    PopToD(sp-3);
    Drop(2);
  }
}

/* ----- sinh ----- */

static Looper sinhLoop, sinhZLoop;

static void sinhLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i++) dst[i]= sinh(src[i]); }
static void sinhZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i+=2) sinhZ(&dst[i], &src[i]); }

void Y_sinh(int nArgs) { UnaryTemplate(nArgs, &sinhLoop, &sinhZLoop); }

/* ----- cosh ----- */

static Looper coshLoop, coshZLoop;

static void coshLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i++) dst[i]= cosh(src[i]); }
static void coshZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i+=2) coshZ(&dst[i], &src[i]); }

void Y_cosh(int nArgs) { UnaryTemplate(nArgs, &coshLoop, &coshZLoop); }

/* ----- tanh ----- */

static Looper tanhLoop, tanhZLoop;

static void tanhLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= tanh(src[i]); }
static void tanhZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i+=2) tanhZ(&dst[i], &src[i]); }

void Y_tanh(int nArgs) { UnaryTemplate(nArgs, &tanhLoop, &tanhZLoop); }

/* ----- exp ----- */

static Looper expLoop, expZLoop;

static void expLoop(double *dst, double *src, long n)
{
  long i;
  for (i=0 ; i<n ; i++) {
    dst[i]= exp(src[i]);
    if (errno) { if (errno==ERANGE && !dst[i]) errno=0; else break; }
  }
}
static void expZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i+=2) expZ(&dst[i], &src[i]); }

void Y_exp(int nArgs) { UnaryTemplate(nArgs, &expLoop, &expZLoop); }

/* ----- log ----- */

static Looper logLoop, logZLoop;

static void logLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i++) dst[i]= log(src[i]); }
static void logZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i+=2) logZ(&dst[i], &src[i]); }

void Y_log(int nArgs) { UnaryTemplate(nArgs, &logLoop, &logZLoop); }

/* ----- log10 ----- */

static Looper log10Loop, log10ZLoop;

static void log10Loop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i++) dst[i]= log10(src[i]); }
static void log10ZLoop(double *dst, double *src, long n)
{ YError("log10(complex) not implemented, use log"); }

void Y_log10(int nArgs) { UnaryTemplate(nArgs, &log10Loop, &log10ZLoop); }

/* ----- sqrt ----- */

static Looper sqrtLoop, sqrtZLoop;

static void sqrtLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n && !errno ; i++) dst[i]= sqrt(src[i]); }
static void sqrtZLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i+=2) sqrtZ(&dst[i], &src[i]); }

void Y_sqrt(int nArgs) { UnaryTemplate(nArgs, &sqrtLoop, &sqrtZLoop); }

/* ----- ceil ----- */

static Looper ceilLoop, ceilZLoop;

static void ceilLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= ceil(src[i]); }
static void ceilZLoop(double *dst, double *src, long n)
{ YError("ceil(complex) not defined"); }

void Y_ceil(int nArgs) { UnaryTemplate(nArgs, &ceilLoop, &ceilZLoop); }

/* ----- floor ----- */

static Looper floorLoop, floorZLoop;

static void floorLoop(double *dst, double *src, long n)
{ long i; for (i=0 ; i<n ; i++) dst[i]= floor(src[i]); }
static void floorZLoop(double *dst, double *src, long n)
{ YError("floor(complex) not defined"); }

void Y_floor(int nArgs) { UnaryTemplate(nArgs, &floorLoop, &floorZLoop); }

/* ----- round ----- */

void Y_round(int nArgs)
{
  Operand op;
  long number, i;
  const double *src;
  double *dst;
  int promoteID, inPlace, errCode;

  if (nArgs != 1) YError("round takes exactly one argument");
  sp->ops->FormOperand(sp, &op);
  if ((promoteID = op.ops->promoteID) > T_DOUBLE)
    YError("expecting non-complex numeric argument");
  if (promoteID < T_DOUBLE) {
    op.ops->ToDouble(&op);
    if (promoteID <= T_LONG) {
      PopToD(sp - 1);
      return;
    }
    inPlace = 1;
    dst = op.value;
  } else {
    inPlace = 0;
    dst = BuildResultU(&op, &doubleStruct);
  }
  src = op.value;
  number = op.type.number;
  errno = 0;
  for (i = 0; i < number; ++i) {
#ifdef HAVE_ROUND
    dst[i] = round(src[i]);
#else
    dst[i] = floor(src[i] + 0.5);
#endif
  }
  errCode = errno;
  if (inPlace) {
    PopToD(sp - 1);
  } else {
    PopToD(sp - 2);
    Drop(1);
  }
  if (errCode != 0) YError("mathlib function signals error");
}

/* ----- lround ----- */

void Y_lround(int nArgs)
{
  Operand op;
  long number, i;
  const double *src;
  long *dst;
  int promoteID, errCode;

  if (nArgs != 1) YError("lround takes exactly one argument");
  sp->ops->FormOperand(sp, &op);
  if ((promoteID = op.ops->promoteID) > T_DOUBLE)
    YError("expecting non-complex numeric argument");
  if (promoteID <= T_LONG) {
    if (promoteID < T_LONG) {
      op.ops->ToLong(&op);
    }
    PopToL(sp - 1);
    return;
  }
  if (promoteID < T_DOUBLE) {
    /* FIXME: we could use lroundf and/or roundf */
    op.ops->ToDouble(&op);
  }
  src = op.value;
  dst = BuildResultU(&op, &longStruct);
  number = op.type.number;
  errno = 0;
  for (i = 0; i < number; ++i) {
#ifdef HAVE_LROUND
    dst[i] = lround(src[i]);
#else
# ifdef HAVE_ROUND
    dst[i] = (long)round(src[i]);
# else
    dst[i] = (long)floor(src[i] + 0.5);
# endif
#endif
  }
  errCode = errno;
  PopToL(sp - 2);
  Drop(1);
  if (errCode != 0) YError("mathlib function signals error");
}

/* ----- abs ----- */

typedef void AbsLooper(void *, void *, long);
static AbsLooper absCLoop, absSLoop, absILoop, absLLoop,
  absFLoop, absDLoop, absZLoop;

static void absCLoop(void *d, void *s, long n)
{ long i; unsigned char *dst= d; unsigned char *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]; }
static void absSLoop(void *d, void *s, long n)
{ long i; short *dst= d; short *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -src[i] : src[i]; }
static void absILoop(void *d, void *s, long n)
{ long i; int *dst= d; int *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -src[i] : src[i]; }
static void absLLoop(void *d, void *s, long n)
{ long i; long *dst= d; long *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -src[i] : src[i]; }
static void absFLoop(void *d, void *s, long n)
{ long i; float *dst= d; float *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -src[i] : src[i]; }
static void absDLoop(void *d, void *s, long n)
{ long i; double *dst= d; double *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -src[i] : src[i]; }
static void absZLoop(void *d, void *s, long n)
{ long i; double *dst= d; double *src= s;
  for (i=0 ; i<n ; i++) dst[i]= hypot(src[2*i], src[2*i+1]); }

static AbsLooper *absLoop[]= { &absCLoop, &absSLoop, &absILoop, &absLLoop,
                               &absFLoop, &absDLoop };

void Y_abs(int nArgs)
{
  if (nArgs<2) {
    Operand op;
    int promoteID;
    long n;
    if (nArgs != 1) YError("abs requires at least one argument");
    sp->ops->FormOperand(sp, &op);
    promoteID= op.ops->promoteID;
    n= op.type.number;
    if (promoteID>T_COMPLEX) YError("abs requires numeric argument");
    if (promoteID<T_COMPLEX) {
      absLoop[promoteID](BuildResultU(&op, op.type.base), op.value, n);
      PopToX(sp-2, promoteID);
    } else {
      absZLoop(BuildResultU(&op, &doubleStruct), op.value, n);
      PopToD(sp-2);
    }
    Drop(1);

  } else {
    Operand opy, opx;
    double *y, *x, *dst;
    Operations *ops;
    long i;
    while (--nArgs) {
      sp->ops->FormOperand(sp, &opx);
      if (opx.ops->promoteID<T_DOUBLE) opx.ops->ToDouble(&opx);
      (sp-1)->ops->FormOperand(sp-1, &opy);
      ops= opy.ops->Promote[opx.ops->promoteID](&opy, &opx);
      if (!ops || ops->promoteID>T_DOUBLE) {
        if (!ops ||
            ops->promoteID>T_COMPLEX) YError("illegal data type to abs(x,y)");
        else YError("no abs(complex,complex), take abs of argument(s)");
      }
      dst= BuildResult2(&opy, &opx);
      if (!dst) YError("operands not conformable in binary abs");
      y= opy.value;
      x= opx.value;
      for (i=0 ; i<opy.type.number ; i++) dst[i]= hypot(y[i], x[i]);
      PopToD(opy.owner);
      Drop(1);
    }
    PopToD(sp-1);
  }
}

/* ----- sign ----- */

static AbsLooper signCLoop, signSLoop, signILoop, signLLoop,
  signFLoop, signDLoop, signZLoop;

static void signCLoop(void *d, void *s, long n)
{ long i; char *dst= d;
  for (i=0 ; i<n ; i++) dst[i]= 1; }
static void signSLoop(void *d, void *s, long n)
{ long i; short *dst= d; short *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -1 : 1; }
static void signILoop(void *d, void *s, long n)
{ long i; int *dst= d; int *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -1 : 1; }
static void signLLoop(void *d, void *s, long n)
{ long i; long *dst= d; long *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -1 : 1; }
static void signFLoop(void *d, void *s, long n)
{ long i; float *dst= d; float *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -1.f : 1.f; }
static void signDLoop(void *d, void *s, long n)
{ long i; double *dst= d; double *src= s;
  for (i=0 ; i<n ; i++) dst[i]= src[i]<0? -1 : 1; }
static void signZLoop(void *d, void *s, long n)
{ long i; double *dst= d; double *src= s;
  if (dst!=src) for (i=0 ; i<n ; i++) dst[i]= src[i];
  for (i=0 ; i<n ; i+=2) signZ(&dst[i], &src[i]); }

static AbsLooper *signLoop[]= { &signCLoop, &signSLoop, &signILoop,
                                &signLLoop, &signFLoop, &signDLoop,
                                &signZLoop };

void Y_sign(int nArgs)
{
  Operand op;
  int promoteID;
  long n;
  if (nArgs != 1) YError("sign function requires exactly one argument");
  sp->ops->FormOperand(sp, &op);
  promoteID= op.ops->promoteID;
  n= op.type.number;
  if (promoteID>T_COMPLEX) YError("sign requires numeric argument");
  if (promoteID==T_COMPLEX) n*= 2;
  signLoop[promoteID](BuildResultU(&op, op.type.base), op.value, n);
  PopToX(sp-2, promoteID);
  Drop(1);
}

/* ----- conj ----- */

void Y_conj(int nArgs)
{
  Operand op;
  int promoteID;
  long i, n;
  double *dst, *src;
  if (nArgs != 1) YError("conj requires exactly one argument");
  sp->ops->FormOperand(sp, &op);
  promoteID= op.ops->promoteID;
  if (promoteID>T_COMPLEX) YError("conj requires numeric argument");
  if (promoteID<T_COMPLEX) {
    PopTo(sp-1);
    return;
  }
  n= 2*op.type.number;
  src= op.value;
  dst= BuildResultU(&op, &complexStruct);
  for (i=0 ; i<n ; i+=2) {
    dst[i]= src[i];
    dst[i+1]= -src[i+1];
  }
  PopTo(sp-2);
  Drop(1);
}

/*--------------------------------------------------------------------------*/

extern RangeFunc RFmin, RFmax, RFsum, RFavg;

static Array *Force1D(Operand *op)
{
  Array *array;
  if (!op->type.dims) return 0;
  if (op->owner->value.db->ops==&lvalueOps)
    array= FetchLValue(op->owner->value.db, op->owner);
  else
    array= (Array *)op->owner->value.db;
  if (op->type.dims->next) {
    /* need to copy the array to a single dimensional version */
    Dimension *dim= tmpDims;
    tmpDims= 0;
    FreeDimension(dim);
    tmpDims= NewDimension(op->type.number, 1L, (Dimension *)0);
    if (op->references) {
      array= PushDataBlock(NewArray(op->type.base, tmpDims));
      op->type.base->Copy(op->type.base, array->value.c, op->value,
                          op->type.number);
      PopTo(op->owner);
      op->value= array->value.c;
    } else {
      array->type.dims= Ref(tmpDims);
      FreeDimension(op->type.dims);
    }
    op->type.dims= tmpDims;
  }
  return array;
}

/* ----- min ----- */

extern BinaryOp MinC, MinS, MinI, MinL, MinF, MinD;

static void MinError(void);
static void MinError(void)
{ YError("operands not conformable in binary min"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]<=rv[i]? lv[i] : rv[i]; \
    Popper(l->owner); \
  } else MinError(); }

OPERATION(MinC, char, PopTo)
OPERATION(MinS, short, PopTo)
OPERATION(MinI, int, PopToI)
OPERATION(MinL, long, PopToL)
OPERATION(MinF, float, PopTo)
OPERATION(MinD, double, PopToD)

static BinaryOp *MinOps[]= { &MinC, &MinS, &MinI, &MinL, &MinF, &MinD };

void Y_min(int nArgs)
{
  if (nArgs<2) {
    Operand op;
    Array *array;
    int promoteID;
    if (nArgs != 1) YError("min requires at least one argument");
    sp->ops->FormOperand(sp, &op);
    promoteID= op.ops->promoteID;
    if (promoteID>=T_COMPLEX) YError("min requires numeric argument");
    array= Force1D(&op);
    if (!array) {  /* scalars are trivial */
      PopTo(sp-1);
      return;
    }
    RFmin(array, 0);
    PopToX(sp-2, promoteID);  /* min does not change data type */
    Drop(1);

  } else {
    Operand opy, opx;
    Operations *ops;
    int promoteID= -1;
    while (--nArgs) {
      sp->ops->FormOperand(sp, &opx);
      (sp-1)->ops->FormOperand(sp-1, &opy);
      ops= opy.ops->Promote[opx.ops->promoteID](&opy, &opx);
      promoteID= ops? ops->promoteID : T_COMPLEX;
      if (promoteID>T_DOUBLE) YError("illegal data type to min(x,y)");
      MinOps[promoteID](&opy, &opx);
      Drop(1);
    }
    PopToX(sp-1, promoteID);
  }
}

/* ----- max ----- */

extern BinaryOp MaxC, MaxS, MaxI, MaxL, MaxF, MaxD;

static void MaxError(void);
static void MaxError(void)
{ YError("operands not conformable in binary max"); }

#undef OPERATION
#define OPERATION(opname, typd, Popper) \
void opname(Operand *l, Operand *r) \
{ typd *dst= BuildResult2(l, r); \
  if (dst) { \
    long i, n= l->type.number; \
    typd *lv= l->value, *rv= r->value; \
    for (i=0 ; i<n ; i++) dst[i]= lv[i]>=rv[i]? lv[i] : rv[i]; \
    Popper(l->owner); \
  } else MaxError(); }

OPERATION(MaxC, char, PopTo)
OPERATION(MaxS, short, PopTo)
OPERATION(MaxI, int, PopToI)
OPERATION(MaxL, long, PopToL)
OPERATION(MaxF, float, PopTo)
OPERATION(MaxD, double, PopToD)

static BinaryOp *MaxOps[]= { &MaxC, &MaxS, &MaxI, &MaxL, &MaxF, &MaxD };

void Y_max(int nArgs)
{
  if (nArgs<2) {
    Operand op;
    Array *array;
    int promoteID;
    if (nArgs != 1) YError("max requires at least one argument");
    sp->ops->FormOperand(sp, &op);
    promoteID= op.ops->promoteID;
    if (promoteID>=T_COMPLEX) YError("max requires numeric argument");
    array= Force1D(&op);
    if (!array) {  /* scalars are trivial */
      PopTo(sp-1);
      return;
    }
    RFmax(array, 0);
    PopToX(sp-2, promoteID);  /* max does not change data type */
    Drop(1);

  } else {
    Operand opy, opx;
    Operations *ops;
    int promoteID= -1;
    while (--nArgs) {
      sp->ops->FormOperand(sp, &opx);
      (sp-1)->ops->FormOperand(sp-1, &opy);
      ops= opy.ops->Promote[opx.ops->promoteID](&opy, &opx);
      promoteID= ops? ops->promoteID : T_COMPLEX;
      if (promoteID>T_DOUBLE) YError("illegal data type to max(x,y)");
      MaxOps[promoteID](&opy, &opx);
      Drop(1);
    }
    PopToX(sp-1, promoteID);
  }
}

/* ----- sum ----- */

void Y_sum(int nArgs)
{
  Operand op;
  Array *array;
  int promoteID;
  if (nArgs != 1) YError("sum requires exactly one argument");
  sp->ops->FormOperand(sp, &op);
  promoteID= (op.ops->typeID!=T_STRING)? op.ops->promoteID : T_STRING;
  if (promoteID>T_COMPLEX && promoteID!=T_STRING)
    YError("sum requires numeric argument");
  array= Force1D(&op);
  if (!array) {                 /* scalars are trivial */
    PopTo(sp-1);
    return;
  }
  RFsum(array, 0);
  /* sum range function may change data type */
  PopToX(sp-2, (promoteID!=T_STRING)?sp->value.db->ops->promoteID:T_STRING);
  Drop(1);
}

/* ----- avg ----- */

void Y_avg(int nArgs)
{
  Operand op;
  Array *array;
  int promoteID;
  if (nArgs != 1) YError("avg requires exactly one argument");
  sp->ops->FormOperand(sp, &op);
  promoteID= op.ops->promoteID;
  if (promoteID>T_COMPLEX) YError("avg requires numeric argument");
  array= Force1D(&op);
  if (!array) {                 /* scalars are trivial */
    PopTo(sp-1);
    return;
  }
  RFavg(array, 0);
  /* avg range function may change data type */
  PopToX(sp-2, sp->value.db->ops->promoteID);
  Drop(1);
}

/*--------------------------------------------------------------------------*/

extern VMaction True, Not;

static void AllofWorker(VMaction *TrueOrNot, int trueIfFinishes)
{
  Operand op;
  int *src;
  long i;
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  TrueOrNot();   /* converts top of stack to 0-or-1 int array */
  sp->ops->FormOperand(sp, &op);
  src= op.value;
  for (i=0 ; i<op.type.number ; i++) if (src[i]) break;
  Drop(2);
  PushIntValue(trueIfFinishes? i>=op.type.number : i<op.type.number);
}

/* ----- allof ----- */

void Y_allof(int nArgs)
{
  if (nArgs != 1) YError("allof requires exactly one argument");
  AllofWorker(&Not, 1);
}

/* ----- anyof ----- */

void Y_anyof(int nArgs)
{
  if (nArgs != 1) YError("anyof requires exactly one argument");
  AllofWorker(&True, 0);
}

/* ----- noneof ----- */

void Y_noneof(int nArgs)
{
  if (nArgs != 1) YError("noneof requires exactly one argument");
  AllofWorker(&True, 1);
}

/* ----- nallof ----- */

void Y_nallof(int nArgs)
{
  if (nArgs != 1) YError("nallof requires exactly one argument");
  AllofWorker(&Not, 0);
}

/* ----- where ----- */

void Y_where(int nArgs)
{
  Operand op;
  int *src;
  long i, n;
  if (nArgs!=1) YError("where takes exactly one argument");
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  True();   /* converts top of stack to 0-or-1 int array */
  sp->ops->FormOperand(sp, &op);
  src= op.value;

  /* first pass counts number of non-zero points */
  n= 0;
  for (i=0 ; i<op.type.number ; i++) if (src[i]) n++;

  if (n && op.type.dims) {
    long j;
    Array *array;
    long origin= 1L;
    Dimension *dims= tmpDims;
    tmpDims= 0;
    FreeDimension(dims);
    tmpDims= NewDimension(n, 1L, (Dimension *)0);
    array= PushDataBlock(NewArray(&longStruct, tmpDims));
    dims= op.type.dims;
    if (dims) {
      while (dims->next) dims= dims->next;
      origin= yForceOrigin? 1L : dims->origin;
    } else {
      origin= 1L;
    }
    j= 0;
    for (i=0 ; i<op.type.number ; i++)
      if (src[i]) array->value.l[j++]= i+origin;

  } else if (n) {
    /* Return scalar index for scalar input.  This provides maximum
       performance when used in conjunction with merge (std1.c).  */
    PushLongValue(1L);

  } else {
    /* Return R_NULLER index range.  This prevents bogus results if
       the caller should use the result of where to extract a subset
       of an array without checking whether anything was returned.  */
    PushDataBlock(NewRange(0L, 0L, 1L, R_NULLER));
  }

  PopTo(sp-2);
  Drop(1);
}

/*--------------------------------------------------------------------------*/

/* ARGSUSED */
void Y_get_cwd(int nArgs)
{
  Array *result= PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  result->value.q[0]= p_strcpy(yCWD);
}

/* ARGSUSED */
void Y_get_home(int nArgs)
{
  Array *result= PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  result->value.q[0]= p_strcpy(yHOME);
}

void Y_cd(int nArgs)
{
  char *q;
  char *name;
  int notOK;
  int amSubroutine= CalledAsSubroutine();
  if (nArgs!=1) YError("cd function takes exactly one argument");
  q= YGetString(sp);
  if (!q) YError("argument to cd must be a non-nil scalar string");
  name= YExpandName(q);
  notOK= YSetCWD(name);
  p_free(name);
  if (notOK) {
    if (amSubroutine) YError("cd failed -- no such directory");
    PushDataBlock(RefNC(&nilDB));
  } else if (!amSubroutine) {
    Y_get_cwd(nArgs);
  }
}

/* Use p_free to get rid of return value from Ygetenv (wraps getenv).  */
char *Ygetenv(const char *name)
{
  return p_strcpy(p_getenv(name));
}

void Y_get_env(int nArgs)
{
  Array *result;
  char *q;
  if (nArgs!=1) YError("getenv function takes exactly one argument");
  q= YGetString(sp);
  result= PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  if (q) result->value.q[0]= Ygetenv(q);
}

int ym_argc= 0;
char **ym_argv= 0;

/* ARGSUSED */
void Y_get_argv(int nArgs)
{
  Array *result;
  Dimension *dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  if (ym_argc>0) tmpDims= NewDimension((long)ym_argc, 1L, (Dimension *)0);
  result= PushDataBlock(NewArray(&stringStruct, tmpDims));
  if (ym_argc>0) {
    int i;
    for (i=0 ; i<ym_argc ; i++) result->value.q[i]= p_strcpy(ym_argv[i]);
  }
}

/*--------------------------------------------------------------------------*/

/* The OriginStatus stores yForceOrigin -- when it is destroyed, this is
   automatically copied back to the yForceOrigin global variable.  */
typedef struct OriginStatus OriginStatus;
struct OriginStatus {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  int force;
};

extern OriginStatus *NewOrg(int force);
extern void FreeOrg(void *ob);  /* ******* Use Unref(bm) ******* */

static UnaryOp PrintOB;

Operations orgsOps = {
  &FreeOrg, T_OPAQUE, 0, T_STRING, "origin_status",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &EvalX, &SetupX, &GetMemberX, &MatMultX, &PrintOB
};

static MemryBlock orgBlock= {0, 0, sizeof(OriginStatus),
                                16*sizeof(OriginStatus)};

void Y_use_origins(int nArgs)
{
  int nf;
  if (nArgs!=1) YError("use_origins takes exactly one argument");
  nf= (int)YGetInteger(sp);
  PushDataBlock(NewOrg(yForceOrigin));
  yForceOrigin= !nf;
}

OriginStatus *NewOrg(int force)
{
  OriginStatus *ob= NextUnit(&orgBlock);
  ob->references= 0;
  ob->ops= &orgsOps;
  ob->force= force;
  return ob;
}

void FreeOrg(void *ob)
{
  OriginStatus *oblk= ob;
  yForceOrigin= oblk->force;
  FreeUnit(&orgBlock, ob);
}

static void PrintOB(Operand *op)
{
  OriginStatus *ob= op->value;
  char line[80];
  sprintf(line, "origin_status: index origins %s",
          ob->force? "force" : "default");
  ForceNewline();
  PrintFunc(line);
  ForceNewline();
}

/*--------------------------------------------------------------------------*/
