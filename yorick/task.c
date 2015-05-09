/*
 * $Id: task.c,v 1.16 2010-07-04 23:07:06 dhmunro Exp $
 * Implement Yorick virtual machine.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "yapi.h"
#include "ydata.h"
#include "yio.h"
#include "pstdlib.h"
#include "play.h"

#include <string.h>

/* packages that need to clean up before Yorick exits should supply
   CleanUpForExit, then call the one they found */
extern void (*CleanUpForExit)(void);
void (*CleanUpForExit)(void)= 0;

extern int YpParse(void *func);  /* argument non-zero for YpReparse */

/* The offsetof macro may be defined in <stddef.h> (it is ANSI standard).  */
/* #define offsetof(structure, member)  ((long)&(((structure*)0)->member))
   (does not work on Crays) */
#ifndef offsetof
#define offsetof(structure, member) \
  ((long)((char *)&(((structure*)0)->member) - (char *)0))
#endif

/*--------------------------------------------------------------------------*/

extern BuiltIn Y_quit, Y_include, Y_require, Y_help, Y_exit, Y_error, Y_batch;
extern BuiltIn Y_current_include, Y_get_includes;
extern BuiltIn Y_plug_in, Y_plug_dir, Y_maybe_prompt, Y_suspend, Y_resume;
extern BuiltIn Y_after, Y__after_func, Y_include1, Y_vopen, Y_vclose;

extern ybuiltin_t Y_prompt_marker;

extern void YRun(void);
extern void YHalt(void);

extern int CheckForTasks(int wait);

extern VMaction Eval, Return, PushVariable;

extern void ClearTasks(void);
extern int DoTask(void);

extern Function *FuncContaining(Instruction *pc);

/* If yAutoDebug!=0, debug mode will be entered automatically whenever
   a runtime error occurs.  Otherwise, you must type "debug".  */
extern int yAutoDebug;
int yAutoDebug= 0;

int yDebugLevel= 0;

/* most recent error message was built in yErrorMsg */
char yErrorMsg[192+12];
char yWarningMsg[192];

static int inYError= 0;

/* from fnctn.c */
extern Instruction *ClearStack(void);
extern Instruction *AbortReturn(void);

extern long ypBeginLine;

extern TextStream *NewTextStream(char *fullname,
                                 void *stream, int permissions,
                                 long line, long pos);

typedef struct DebugBlk DebugBlk;
extern DebugBlk *NewDebugBlk(Function *f, long pcerr, char *errFile,
                             long lnum);

/* stuff to implement catch */
extern BuiltIn Y_catch;
typedef struct Catcher Catcher;
struct Catcher {
  Instruction *task;
  Instruction *pc;  /* of conditional branch instruction */
  long isp;         /* sp-spBottom of returnSym for calling function */
  int category;     /* of error to be caught */
};
static long n_catchers= 0;
static long max_catchers= 0;
static Catcher *catchers= 0;
extern void YCatchDrop(long isp);        /* used in fnctn.c */
extern long ispCatch;                    /* used in fnctn.c */
long ispCatch= 0;
extern int y_catch_category;
int y_catch_category= 0x08;
static Catcher *CatchScan(const char *msg, int category);
static Catcher *CatchNew(void);
static int caughtTask= 0;

/* stuff to implement set_idler */
extern BuiltIn Y_set_idler;
Function *y_idler_function= 0;
static int y_idler_fault = 0;

/*--------------------------------------------------------------------------*/

Instruction *pc= 0;

static int ym_state= 0;
static int ym_fatal = 0;
extern int ym_dbenter;
int ym_dbenter = 0;
#define Y_QUITTING 1
#define Y_RUNNING 2
#define Y_SUSPENDED 4
#define Y_PENDING 8

void
YRun(void)
{
  register VMaction *Action;
  int run_state = ym_state & Y_RUNNING;
  ym_state |= Y_RUNNING;
  ym_dbenter = 0;

  P_SOFTFPE_TEST;

  while (!p_signalling) {
    Action = (pc++)->Action;
    Action();
  }
  if (p_signalling==-1 && !(ym_state&Y_RUNNING))
    p_signalling = 0;      /* p_signalling set by YHalt (?? see below) */

  /* reset Y_RUNNING to value on entry -- allows YRun to recurse */
  ym_state = (ym_state & ~Y_RUNNING) | run_state;

  P_SOFTFPE_TEST;

  if (p_signalling)
    p_abort();             /* p_signalling set by real signal */
}

extern void ym_escape(void);
void
ym_escape(void)
{
  Y_quit(0);
  if (p_signalling==-1) p_signalling = 0;
  p_abort();
}

void
YHalt(void)
{
  ym_state &= ~Y_RUNNING;
  /* this may not be quite right -- a real signal might go unnoticed --
   * but may only be possible for SIGINT when machine is halting anyway
   * -- but if several other tasks queued, behavior might not be right */
  if (!p_signalling) p_signalling = -1;
}

/* if read() interrupted by uncaught error, have a serious problem */
extern char *y_read_prompt;
extern Instruction *ym_suspend(void);
extern void ym_resume(Instruction *pc);

static Instruction ym_stopper;

Instruction *
ym_suspend(void)
{
  Instruction *ipc = pc;
  if (caughtTask || (ym_state&(Y_SUSPENDED|Y_PENDING)))
    YError("ym_suspend called while suspended or in catch");
  ym_state |= Y_PENDING;
  ym_stopper.Action = YHalt;
  pc = &ym_stopper;
  return ipc;
}

void
ym_resume(Instruction *ipc)
{
  int was_suspended = (ym_state&Y_SUSPENDED);
  ym_state &= ~(Y_SUSPENDED|Y_PENDING);
  if (!was_suspended)
    YError("ym_resume called while not suspended");
  if (!ipc) YError("(BUG) ym_resume from null pc");
  pc = ipc;
  caughtTask = 1;
  /* protect against following sequence:
   * (1) task suspends during include file, causing y_on_idle to return 0
   *     this marks idler_eligible==0 in p_on_idle (alarms.c)
   * (2) event arrives (e.g.- expose) and is handled as a pending event
   *     before calling p_timeout; this event calls ym_resume
   * (3) p_timeout is called, which should resume execution, but cannot
   *     because idler_eligible has never been reset
   */
  p_timeout();
}

/*--------------------------------------------------------------------------*/

static Function **tasks= 0;
static int nTasks= 0;
static int maxTasks= 0;

void ClearTasks(void)
{
  while (nTasks) { nTasks--;  Unref(tasks[nTasks]); }
  if (maxTasks>16) {
    maxTasks= 0;
    p_free(tasks);
    tasks= 0;
  }
}

void
PushTask(Function *task)
{
  if (p_signalling) p_abort();
  if (nTasks>=maxTasks) {
    int newSize = maxTasks+16;
    tasks = p_realloc(tasks, sizeof(Function *)*newSize);
    maxTasks = newSize;
  }
  tasks[nTasks++] = Ref(task);  /* WARNING-- this is the reference for when
                                 * DoTask pushes the task onto the stack--
                                 * you are still giving your reference to the
                                 * Function* away when you call PushTask */
}

/* The task pseudo-code MUST be static, since YError may p_abort out
   of the DoTask procedure.  */
static Instruction taskCode[4];
static int taskCodeInit= 0;

static Instruction *ym_suspc = 0;
extern int yg_blocking;         /* graph.c */

int DoTask(void)
{
  extern Operations debugOps;
  if (caughtTask) {
    caughtTask= 0;
  } else if (nTasks>0) {
    Function *task;
    if (p_signalling) p_abort();
    task = tasks[--nTasks];

    CheckStack(1);
    (sp+1)->ops= &dataBlockSym;
    (sp+1)->value.db= (DataBlock *)task;  /* use owned by stack */
    sp++;

    pc= taskCode;     /* note that original pc is clobbered */
  }
  YRun();
  if (yg_blocking == 4) {  /* resume has been called */
    yg_blocking = 0;
    if (ym_suspc) {
      Instruction *suspc = ym_suspc;
      ym_suspc = 0;
      ym_resume(suspc);
    }
  }
  if (ym_state & Y_PENDING) {
    ym_state &= ~Y_PENDING;
    ym_state |= Y_SUSPENDED;
  } else if (sp->ops!=&dataBlockSym || sp->value.db->ops!=&debugOps) {
    /* actually, something is terribly wrong if this is not *main* */
    Drop(1);
  }
  return nTasks;
}

/*--------------------------------------------------------------------------*/

extern int y_on_idle(void);
extern void y_on_exception(int signal, char *errmsg);
extern int y_on_quit(void);

extern void y_cleanup(void);

extern int nYpInputs;   /* from yinput.c */
extern int yImpossible;
int yImpossible= 0;

extern int yBatchMode;  /* may be set with -batch, see std0.c */
int yBatchMode= 0;

static int y_was_idle= 0;

/* prompt marker after prompts simplifies writing yorick controllers */
static char *prompt_marker = 0;
void
Y_prompt_marker(int argc)
{
  char *marker = (argc==1)? ygets_q(0) : 0;
  if (argc > 1) y_error("prompt_marker expecting single string argument");
  if (marker && !marker[0]) marker = 0;
  if (prompt_marker) {
    char *old = prompt_marker;
    prompt_marker = 0;
    p_free(old);
  }
  if (marker)
    prompt_marker = p_strcpy(marker);
}

static void ym_prompter(void);
extern char *y_read_prompt;     /* ascio.c */
/* yp_did_prompt reset by y_on_stdin */
extern int yp_did_prompt;
int yp_did_prompt= 0;

static void
ym_prompter(void)
{
  if (!yp_did_prompt) {
    if (y_read_prompt) {
      if (y_read_prompt[0]) {
        p_stdout(y_read_prompt);
        if (prompt_marker) p_stdout(prompt_marker);
        yp_did_prompt = 1;
      }
    } else {
      extern void y_do_prompt(void);  /* yorick.c */
      if (!yg_blocking) {
        y_do_prompt();
        if (prompt_marker) p_stdout(prompt_marker);
        yp_did_prompt = 1;
      }
    }
  }
}

void
Y_maybe_prompt(int nargs)
{
  if (nargs) YError("maybe_prompt accepts no arguments");
  if (!yp_did_prompt && !y_read_prompt) ym_prompter();
}

/* suspend/resume is very primitive - would be nice to permit a
 * queue of suspended tasks, but needs a bigger interpreted API
 */
void
Y_suspend(int nargs)
{
  if (ym_suspc || yg_blocking || y_read_prompt)
    YError("suspend: already suspended, paused, or waiting for input");
  ym_suspc = ym_suspend();
  yg_blocking = 3;     /* for y_on_stdin */
}

void
Y_resume(int nargs)
{
  /* cannot call ym_resume directly, signal DoTask to do that */
  if (ym_suspc && yg_blocking==3) yg_blocking = 4;
}

static void ym_after(void *context);
static void ym_after_dq(int i);

typedef struct ym_after_t ym_after_t;
struct ym_after_t {
  void *f, *d;     /* yget_use function and data handles */
  long fndx, dndx;  /* same as closure object, see oxy.c */
  int next;
};
static ym_after_t *ym_after_list = 0;
static int ym_after_n = 0;  /* length of list, not number active */
static int ym_after_i = -1; /* index of first active list item */
static int ym_after_j = -1; /* index of first unused list item */
static int ym_after_k = -1; /* index of last active list item */
#define YM_AFTER_MAX 1024

void Y__after_func(int argc)
{
  extern void FormEvalOp(int nargs, Operand *obj);
  ym_after_t *ao;
  long dref;
  void *p = 0;
  Operand op;
  if (ym_after_i<0 || ym_after_i>=ym_after_n)
    y_error("(BUG?) bad link in _after_func");
  ao = ym_after_list + ym_after_i;
  ym_after_i = ao->next;
  if (ym_after_i == -1) ym_after_k = -1;
  ao->next = ym_after_j;
  ym_after_j = ao - ym_after_list;
  if (argc!=0 || !yarg_subroutine() || yarg_func(0)!=2)
    y_error("_after_func must only be called by _after_work");
  yarg_drop(1);
  ypush_check(3);
  dref = ao->dndx;
  p = ao->f;
  ao->f = 0;
  if (ao->fndx >= 0) {
    ypush_global(ao->fndx);
    if (yarg_func(0)) dref = -1L;
  } else {
    ypush_use(p);
  }
  p = ao->d;
  ao->d = 0;
  if (dref >= -1L) {
    if (dref < 0) {
      ypush_use(p);
    } else {  /* object(member,...) semantics */
      sp[1].ops = &referenceSym;
      sp[1].index = dref;
      sp++;
    }
    argc = 1;
  } else {
    argc = 0;
  }
  FormEvalOp(argc, &op);
  op.ops->Eval(&op);
}

static Function *ym_after_work = 0;  /* yget_use handle to _after_work */

void
Y_after(int argc)
{
  long range[3];
  int flags = yget_range(argc-1, range);
  double secs = -1.0;
  long fndx=-1L, dndx=-1L;
  if (!ym_after_work) {  /* this is first call */
    long w = yfind_global("_after_work", 0);
    if (w > 0) {
      ypush_global(w);
      if (yarg_func(0) == 1) ym_after_work = yget_use(0);
      yarg_drop(1);
    }
    if (!ym_after_work) y_error("(BUG) missing _after_work function");
  }
  if (flags==(Y_PSEUDO | Y_MIN_DFLT | Y_MAX_DFLT) && range[2]==1) {
    if (argc == 1) {
      ym_after_dq(-1);  /* cancel everything, just like error */
      return;
    }
  } else {
    secs = ygets_d(argc-1);
    if (secs < 0.0) secs = 0.0;
    flags = 0;  /* if not, previous line raised error */
  }
  if (argc!=2 && argc!=3)
    y_error("after called with illegal number of arguments");
  flags = yarg_func(argc-2);
  if (!flags) {
    yo_ops_t *ops;
    if (yo_get(argc-2, &ops)) {
      flags = -1;
    } else if (yarg_string(argc-2)==1) {
      char *name = ygets_q(argc-2);
      if (name[0]=='o' && name[1]==':') name+=2, flags=-1;
      else flags = -2;
      fndx = yget_global(name, 0L);
    } else {
      y_error("unrecognized second argument to after");
    }
  }
  if (argc == 3) {
    dndx = yget_ref(0);
    if (flags>0 || dndx<0) {
      if (yarg_typeid(0) >= 100)
        y_error("(BUG?) unrecognized third argument to after");
      dndx = -1L;
    }
  } else {
    dndx = -2L;  /* no data argument */
  }

  yexec_after(secs, fndx, argc-2, dndx, argc-3);
}

void
yexec_after(double secs, long fndx, int farg, long dndx, int darg)
{
  int i;

  if (secs < 0.0) { /* dequeue anything which matches */
    /* first check active list */
    void *f = (fndx>=0)? 0 : yget_use(farg);
    void *d = (dndx!=-1L)? 0 : yget_use(darg);
    int j = -1;
    if (f) ydrop_use(f);
    if (d) ydrop_use(d);
    for (i=ym_after_i ; i>=0 ; j=i,i=ym_after_list[i].next) {
      if (ym_after_list[i].f==f && ym_after_list[i].fndx==fndx) {
        if (dndx==-2L ||
            (ym_after_list[i].d==d && ym_after_list[i].dndx==dndx)) {
          if (j >= 0) ym_after_list[j].next = ym_after_list[i].next;
          else ym_after_i = ym_after_list[i].next;
          if (i == ym_after_k) ym_after_k = j;
          ym_after_dq(i);
        }
      }
    }
    /* then check any waiting items (no list of these) */
    for (i=0 ; i<ym_after_n ; i++) {
      if (ym_after_list[i].next != -2) continue;
      if (ym_after_list[i].f==f && ym_after_list[i].fndx==fndx) {
        if (dndx==-2L &&
            (ym_after_list[i].d==d && ym_after_list[i].dndx==dndx)) {
          ym_after_dq(i);
        }
      }
    }
    return;
  }

  if (ym_after_j < 0) {  /* need to lengthen list */
    int j, k;
    if (ym_after_n+ym_after_n > YM_AFTER_MAX)
      y_error("runaway queue of after functions");
    if (!ym_after_n) {
      j = 0;
      k = 4;
    } else {
      j = ym_after_n;
      k = j+j;
    }
    ym_after_list = p_realloc(ym_after_list, sizeof(ym_after_t)*k);
    for (i=j ; i<k ; i++) {
      ym_after_list[i].f = ym_after_list[i].d = 0;
      ym_after_list[i].fndx = ym_after_list[i].dndx = -1L;
      ym_after_list[i].next = (i<k-1)? i+1 : -1;
    }
    ym_after_j = j;
    ym_after_n = k;
  }
  i = ym_after_j;
  ym_after_j = ym_after_list[i].next;
  ym_after_list[i].next = -2;

  ym_after_list[i].f = (fndx>=0)? 0 : yget_use(farg);
  ym_after_list[i].fndx = fndx;
  ym_after_list[i].d = (dndx!=-1L)? 0 : yget_use(darg);
  ym_after_list[i].dndx = dndx;

  p_set_alarm(secs, ym_after, i+(char*)0);
}

static void
ym_after(void *context)
{
  long i = (char *)context - (char*)0;
  if (i>=0 && i<ym_after_n) {
    if (ym_after_list[i].next != -2) return; /* cancelled */
    if (ym_after_k>=0) ym_after_list[ym_after_k].next = i;
    else ym_after_i = i;
    ym_after_k = i;
    ym_after_list[i].next = -1;
    PushTask(ym_after_work);
  } else {
    y_error("(BUG) bad task.c:ym_after call");
  }
}

static void
ym_after_dq(int i)
{
  if (i<0) {
    for (i=ym_after_n-1 ; i>=0 ; i--) ym_after_dq(i);
    ym_after_i = ym_after_k = -1;
  } else {
    void *p = ym_after_list[i].f;
    if (p) {
      ym_after_list[i].f = 0;
      ydrop_use(p);
    }
    p = ym_after_list[i].d;
    if (p) {
      ym_after_list[i].d = 0;
      ydrop_use(p);
    }
    ym_after_list[i].next = ym_after_j;
    ym_after_j = i;
  }
}

static int startup_done = 0;

int
y_on_idle(void)
{
  int more_work = 0;
  int pending_stdin = 0;
  int idler_fault = 0;

  if (!taskCodeInit) {
    taskCode[0].Action = &Eval;
    taskCode[1].count = 0;
    taskCode[2].Action = &YHalt;
    taskCode[3].index = 0;
    taskCodeInit = 1;
  }

  p_fpehandling(2);  /* be sure yorick FPE handling set properly */

  if (ym_state & Y_QUITTING) {
  die_now:
    y_cleanup();
    p_quit();
    return 0;
  }
  if (!(nTasks+caughtTask)) {
    extern int y_pending_stdin(void);  /* yinput.c */
    if (nYpIncludes || nYpInputs) {
      YpParse((void *)0);
    } else {
      if (startup_done) {
        pending_stdin = y_pending_stdin();
      } else {
        /* make sure idler gets called once before any stdin
         * - without this, starting with shell here-document makes
         *   lines passed as here-document execute before custom.i,
         *   not what user expects
         */
        if (!y_idler_function) pending_stdin = y_pending_stdin();
        startup_done = 1;
      }
      if (!(nTasks+caughtTask || nYpIncludes || nYpInputs) &&
          y_idler_function) {
        Function *f = y_idler_function;
        y_idler_function = 0;
        /* this does not really detect when the after_error function
         * itself has completed - a catch inside after_error can get
         * complicated and probably defeat this loop detection attempt
         */
        idler_fault = y_idler_fault;
        PushTask(f);
        Unref(f);
      }
    }
  }

  if (nTasks+caughtTask) DoTask();
  if (ym_state & Y_QUITTING) goto die_now;
  if (idler_fault) y_idler_fault = 0;
  more_work = (nTasks+caughtTask || nYpIncludes || nYpInputs ||
               pending_stdin || y_idler_function);

  /* non-0 return means we want to run again
   * -- need to prompt if nothing left to do, but want to check
   *    for more input events before deciding
   * thus, first time we are out of tasks return non-0 anyway,
   * but prompt and return 0 second consecutive time we are out */
  if (more_work)
    {
      y_was_idle = 0;

      /* probably incorrect -- any tasks created before the suspend
       * but not yet run will be blocked
       * or maybe that's correct behavior?  what are the implied
       * dependencies of one task on another?
       */

      /* block on window,wait=1 or mouse() from #include file */
      /* if (yg_blocking) more_work=0; */
      /* block whenever suspended for any reason from #include file */
      if (ym_state&Y_SUSPENDED) more_work = 0;
      /* deliver prompt on read() or rdline() from #include file */
      if (y_read_prompt) ym_prompter();
    }
  else if (y_was_idle)
    y_was_idle=0, ym_prompter();
  else
    y_was_idle = more_work = !yBatchMode;
  if (!(nTasks+caughtTask)) {
    extern void yg_before_wait(void);
    yg_before_wait();
  }
  return more_work;
}

void Y_quit(int nArgs)
{
  ym_state|= Y_QUITTING;
  ResetStack(0);
  YHalt();
}

static int did_cleanup = 0;

int
y_on_quit(void)
{
  if (!did_cleanup) y_cleanup();
  return ym_fatal;
}

static volatile int detectRecursion = 0;
static IOStream *yclean_file = 0;

void
y_cleanup(void)
{
  if (detectRecursion) {
    if (detectRecursion < 2) {
      detectRecursion = 3;
      RemoveIOLink(yBinaryFiles, yclean_file);
    } else {
      yBinaryFiles = 0;
    }
    detectRecursion = 0;
  }
  /* attempt to close all binary files properly */
  while (yBinaryFiles) {
    yclean_file = yBinaryFiles->ios;
    yclean_file->references = 0;
    detectRecursion = 1;
    Unref(yclean_file);
    detectRecursion = 2;
    RemoveIOLink(yBinaryFiles, yclean_file);
  }

  if (CleanUpForExit) CleanUpForExit();
  did_cleanup = 1;
}

void RunTaskNow(Function *task)
{
  Instruction *pcHere = pc;
  int t0 = nTasks;
  if (ym_state & Y_SUSPENDED)
    YError("RunTaskNow called while suspended waiting for event");
  PushTask(Ref(task));
  while (nTasks>t0 && !(ym_state&(Y_QUITTING|Y_SUSPENDED))) DoTask();
  if (ym_state & Y_SUSPENDED)
    YError("(read, pause, wait=1, etc.) suspended during RunTaskNow");
  pc = pcHere;           /* may have been clobbered by DoTask */
  if (ym_state&Y_QUITTING) YHalt();
}

void IncludeNow(void)
{
  Instruction *pcHere= pc;
  int i0= nYpIncludes;
  int t0= nTasks;
  if (ym_state & Y_SUSPENDED)
    YError("IncludeNow called while suspended waiting for event");
  for (;;) {
    while (nTasks<=t0 && (nYpIncludes>i0 || (nYpIncludes==i0 &&
           ypIncludes[i0-1].file))) YpParse((void *)0);
    while (nTasks>t0 && !(ym_state&(Y_QUITTING|Y_SUSPENDED))) DoTask();
    if ((ym_state&(Y_QUITTING|Y_SUSPENDED)) || nYpIncludes<i0 ||
        !ypIncludes[i0-1].file) break;
  }
  if (ym_state & Y_SUSPENDED)
    YError("(read, pause, wait=1, etc.) suspended during IncludeNow");
  pc= pcHere;           /* may have been clobbered by DoTask */
  if (ym_state&Y_QUITTING) YHalt();
}

static char *y_include_arg(p_file **file);

void
Y_include(int nArgs)
{
  long now = 0;
  char *name;
  p_file *file = 0;
  if (nArgs!=1 && nArgs!=2)
    YError("include function takes exactly one or two arguments");
  if (nArgs > 1) {
    now = YGetInteger(sp);
    Drop(1);
  }
  name = y_include_arg(&file);
  if (name[0]) {  /* name=="" special hack cleans out pending includes */
    if (now >= 0) {
      if (file) {
        y_push_include(file, name);
      } else if (!YpPushInclude(name)) {
        if (!(now&2))
          YError("missing include file specified in include function");
        now = 0;
      }
    } else {
      if (file) YError("include: now<0 only possible with filename argument");
      YpPush(name);          /* defer until all pending input parsed */
    }
  }
  Drop(1);
  if (now>0) IncludeNow(); /* parse and maybe execute file to be included
                            * -- without now, this won't happen until the
                            * next line is parsed naturally */
}

void
yexec_include(int iarg, int now)
{
  ypush_use(yget_use(iarg));
  ypush_int(now);
  Y_include(2);
}

void
ytask_push(int iarg)
{
  if (iarg>=0 && yarg_func(iarg)==1) {
    Function *f = (Function *)sp[-iarg].value.db;
    PushTask(Ref(f));
  } else {
    YError("can only run interpreted functions as tasks");
  }
}

void
ytask_run(int iarg)
{
  if (iarg>=0 && yarg_func(iarg)==1) {
    Function *f = (Function *)sp[-iarg].value.db;
    RunTaskNow(f);
  } else {
    YError("can only push interpreted functions onto task stack");
  }
}

void
Y_include1(int nArgs)
{
  int i0 = nYpIncludes;
  int t0 = nTasks;
  char *name;
  p_file *file = 0;
  if (nArgs != 1) YError("include1 function takes exactly one argument");
  name = y_include_arg(&file);
  if (file) y_push_include(file, name);
  else if (!YpPushInclude(name))
    YError("missing include file specified in include1 function");
  Drop(nArgs);
  if (ym_state & Y_SUSPENDED)
    YError("include1 called while suspended waiting for event");
  while (nTasks<=t0 && (nYpIncludes>i0 || (nYpIncludes==i0 &&
         ypIncludes[i0-1].file))) YpParse((void *)0);
  if (nTasks == t0+1) {
    (sp+1)->ops = &dataBlockSym;
    (sp+1)->value.db = (DataBlock *)tasks[--nTasks];  /* use owned by stack */
    sp++;
  } else if (nTasks <= t0) {
    PushDataBlock(RefNC(&nilDB));
  } else {
    YError("include1 created more than one task (impossible?)");
  }
}

static p_file *ynew_vopen(Array *array, int binary);
static char *y_vopen_name = "(vopen file)";

static char *
y_include_arg(p_file **file)
{
  Operand op;
  char *name = y_vopen_name;
  if (!sp->ops) YError("unexpected keyword argument in include or include1");
  sp->ops->FormOperand(sp, &op);
  if (op.ops->typeID == T_STRING) {
    if (!op.type.dims) {
      char **q = op.value;
      if (!q[0]) YError("string(0) filename to include or include1");
      name = q[0];
    } else {
      *file = ynew_vopen((Array *)sp->value.db, 0);
      name = y_vopen_name;
    }
  } else if (op.ops->typeID == T_CHAR) {
    *file = ynew_vopen((Array *)sp->value.db, 0);
    name = y_vopen_name;
  } else if (op.ops == &textOps) {
    /* first few members of TextStream same as IOStream */
    name = ((IOStream *)op.value)->fullname;
  } else {
    if (op.ops == &streamOps)
      YError("include or include1 cannot accept binary file handle");
    YError("include or include1 cannot convert argument to text file handle");
  }
  return name;
}

/* ----- begin vopen implementation ----- */

static unsigned long yv_fsize(p_file *file);
static unsigned long yv_ftell(p_file *file);
static int yv_fseek(p_file *file, unsigned long addr);

static char *yv_fgets(p_file *file, char *buf, int buflen);
static int yv_fputs(p_file *file, const char *buf);
static unsigned long yv_fread(p_file *file,
                              void *buf, unsigned long nbytes);
static unsigned long yv_fwrite(p_file *file,
                               const void *buf, unsigned long nbytes);

static int yv_feof(p_file *file);
static int yv_ferror(p_file *file);
static int yv_fflush(p_file *file);
static int yv_fclose(p_file *file);

static p_file_ops y_vopen_ops = {
  &yv_fsize, &yv_ftell, &yv_fseek,
  &yv_fgets, &yv_fputs, &yv_fread, &yv_fwrite,
  &yv_feof, &yv_ferror, &yv_fflush, &yv_fclose };

typedef struct y_vopen_t y_vopen_t;
struct y_vopen_t {
  p_file_ops *ops;
  Array *array;
  long addr, maxaddr, offset;
  int binary;
};

static p_file *
ynew_vopen(Array *array, int binary)
{
  y_vopen_t *file = p_malloc(sizeof(y_vopen_t));
  file->ops = &y_vopen_ops;
  file->array = Ref(array);
  file->addr = file->offset = 0;
  file->maxaddr = (binary&2)? 0 : ((y_vopen_t *)file)->array->type.number;
  file->binary = binary;
  return (p_file *)file;
}

void *
y_vopen_file(void *stream)
{
  y_vopen_t *file = stream;
  return (file->ops==&y_vopen_ops)? file->array : 0;
}

void
Y_vopen(int argc)
{
  Operand op;
  p_file *file = 0;
  int binary = 0, wrt = 0;
  if (argc == 2) {
    if (!sp[-1].ops) YError("vopen: unexpected keyword argument");
    binary = (YGetInteger(sp) != 0);
    Drop(1);
  } else if (argc != 1) {
    YError("vopen takes one or two arguments");
  }
  sp->ops->FormOperand(sp, &op);
  if (op.ops->typeID == T_VOID) {
    Dimension *tmp = tmpDims;
    tmpDims = 0;
    FreeDimension(tmp);
    tmpDims = NewDimension(binary? 16384L : 1024L, 1L, (Dimension *)0);
    Drop(1);
    if (binary) PushDataBlock(NewArray(&charStruct, tmpDims));
    else PushDataBlock(NewArray(&stringStruct, tmpDims));
    sp->ops->FormOperand(sp, &op);
    binary |= (wrt = 2);
  }
  if (op.ops->typeID!=T_STRING && op.ops->typeID!=T_CHAR)
    YError("vopen argument must be string or char array");
  file = ynew_vopen((Array *)sp->value.db, binary);
  if (binary&1)
    PushDataBlock(NewIOStream(p_strcpy(y_vopen_name), file, 9|wrt));
  else
    PushDataBlock(NewTextStream(p_strcpy(y_vopen_name), file, 1|wrt, 0L, 0L));
}

void
Y_vclose(int argc)
{
  long index = -1;
  Operand op;
  if (argc != 1) YError("vclose takes exactly one argument");
  if (sp->ops == &referenceSym) index = sp->index;
  sp->ops->FormOperand(sp, &op);
  if (op.ops==&textOps || op.ops==&streamOps) {
    IOStream *ios = op.value;  /* first few members match TextStream */
    y_vopen_t *file = ios->stream;
    if (file && file->ops==&y_vopen_ops) {
      long len;
      len = file->maxaddr;
      if (!len) {
        PushDataBlock(RefNC(&nilDB));
      } else {
        if (file->binary & 2) {
          if (file->binary & 1) {
            if (ios->CloseHook) {
              ios->CloseHook(ios);
              ios->CloseHook = 0;
              len = file->maxaddr;
            }
          }
          if (file->array->type.number > len) {
            /* shrink array to elements actually used */
            file->array->type.number =
              file->array->type.dims->number = len;
            if (!(file->binary & 1)) len *= sizeof(char*);
            len += (char *)file->array->value.q - (char *)file->array;
            file->array = p_realloc(file->array, len);
          }
        }
        PushDataBlock(Ref(file->array));
        if ((file->binary & 3) == 3) {
          ios->ioOps->Close(ios);
          ios->stream = 0;
        }
      }
      if (index >= 0) {
        /* set reference argument to nil */
        Symbol *s = &globTab[index];
        if (s->ops==&dataBlockSym && s->value.db==op.value) {
          s->ops = &intScalar;
          Unref(s->value.db);
          s->value.db = RefNC(&nilDB);
          s->ops = &dataBlockSym;
        }
      }
      return;
    }
  }
  YError("vclose: already closed, not vopen handle, or not a file handle");
}

static unsigned long
yv_fsize(p_file *file)
{
  if (((y_vopen_t *)file)->binary & 2) return ((y_vopen_t *)file)->maxaddr;
  else return ((y_vopen_t *)file)->array->type.number;
}

static unsigned long
yv_ftell(p_file *file)
{
  return ((y_vopen_t *)file)->addr;
}

static int
yv_fseek(p_file *file, unsigned long addr)
{
  long len = ((y_vopen_t *)file)->array->type.number;
  if (((y_vopen_t *)file)->binary & 2) {
    if (((y_vopen_t *)file)->binary & 1) {
      if (addr > len) {
        long j, n = 2*((y_vopen_t *)file)->array->type.number;
        long nhead = (char *)((y_vopen_t *)file)->array->value.c -
          (char *)((y_vopen_t *)file)->array;
        while (n < addr) n += n;
        ((y_vopen_t *)file)->array = p_realloc(((y_vopen_t *)file)->array,
                                               nhead+n);
        for (j=len ; j<n ; j++) ((y_vopen_t *)file)->array->value.c[j] = '\0';
        ((y_vopen_t *)file)->array->type.number =
          ((y_vopen_t *)file)->array->type.dims->number = n;
      }
      len = addr;
    } else {
      len = ((y_vopen_t *)file)->maxaddr;
    }
  }
  if (addr<0 || addr>len) return -1;
  ((y_vopen_t *)file)->addr = addr;
  ((y_vopen_t *)file)->offset = 0;
  if (addr > ((y_vopen_t *)file)->maxaddr)
    ((y_vopen_t *)file)->maxaddr = addr;
  return 0;
}

static char *
yv_fgets(p_file *file, char *buf, int buflen)
{
  int strng = (((y_vopen_t *)file)->array->ops->typeID == T_STRING);
  char *txt, c='\0';
  long jeof;
  int i, j;
  if (!strng) {
    txt = ((y_vopen_t *)file)->array->value.c + ((y_vopen_t *)file)->addr;
    jeof = ((y_vopen_t *)file)->array->type.number - ((y_vopen_t *)file)->addr;
  } else {
    txt = ((y_vopen_t *)file)->array->value.q[((y_vopen_t *)file)->addr];
    txt += ((y_vopen_t *)file)->offset;
    jeof = 0L;
  }
  if (buflen <= 0) return 0;
  for (i=j=0 ; i<buflen-1 && c!='\n' ; i++,j++) {
    if (!strng && j>=jeof) break;
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
  if (!strng) {
    ((y_vopen_t *)file)->addr += j;
  } else if (i>=buflen-1 && txt && txt[j] && j>0) {
    /* handle case where fgets stopped because buflen too short to hold line */
    ((y_vopen_t *)file)->offset += j;
  } else {
    ((y_vopen_t *)file)->addr++;
    ((y_vopen_t *)file)->offset = 0;
  }
  return buf;
}

static int
yv_fputs(p_file *file, const char *buf)
{
  if (!buf) return 0;
  if (((y_vopen_t *)file)->binary == 2) {
    long n, addr = ((y_vopen_t *)file)->addr;
    long len = ((y_vopen_t *)file)->array->type.number;
    long nhead = (char *)((y_vopen_t *)file)->array->value.q -
      (char *)((y_vopen_t *)file)->array;
    char *line;
    for (;;) {
      if (addr == len) {
        n = nhead + (len+len)*sizeof(char*);
        ((y_vopen_t *)file)->array = p_realloc(((y_vopen_t *)file)->array, n);
        for (n=0 ; n<len ; n++) ((y_vopen_t *)file)->array->value.q[len+n] = 0;
        ((y_vopen_t *)file)->array->type.number =
          ((y_vopen_t *)file)->array->type.dims->number = len + len;
      }
      for (n=0 ; buf[n] && buf[n]!='\n' ; n++);
      line = ((y_vopen_t *)file)->array->value.q[addr];
      if (n) {
        ((y_vopen_t *)file)->array->value.q[addr] = p_strncat(line, buf, n);
        if (line) p_free(line);
      } else if (!line) {
        ((y_vopen_t *)file)->array->value.q[addr] = p_strcpy("");
      }
      if (buf[n] == '\n') addr++, n++;
      if (!buf[n]) break;
      buf += n;
    }
    ((y_vopen_t *)file)->addr = addr;
    if (addr > ((y_vopen_t *)file)->maxaddr)
      ((y_vopen_t *)file)->maxaddr = addr;
  } else {
    YError("p_fputs to binary or read-only vopen file handle");
  }
  return 0;
}

static unsigned long
yv_fread(p_file *file, void *buf, unsigned long nbytes)
{
  int strng = (((y_vopen_t *)file)->array->ops->typeID == T_STRING);
  char *cbuf=buf, *txt=0;
  unsigned long j, jeof=0;
  if (!strng) {
    txt = ((y_vopen_t *)file)->array->value.c + ((y_vopen_t *)file)->addr;
    jeof = ((y_vopen_t *)file)->array->type.number - ((y_vopen_t *)file)->addr;
  } else {
    YError("p_fread from text vopen file handle");
  }
  if (!nbytes) return 0L;
  for (j=0 ; j<nbytes ; j++) {
    if (!strng && j>=jeof) break;
    cbuf[j] = txt[j];
    if (strng && !txt[j]) break;
  }
  if (!strng) ((y_vopen_t *)file)->addr += j;
  else ((y_vopen_t *)file)->addr++;
  return j;
}

static unsigned long
yv_fwrite(p_file *file, const void *buf, unsigned long nbytes)
{
  if (((y_vopen_t *)file)->binary == 3) {
    long len = ((y_vopen_t *)file)->array->type.number;
    long i = ((y_vopen_t *)file)->addr;
    if (i+nbytes > len) {
      /* double array size if at eof */
      long j, n = len + len;
      long nhead = (char *)((y_vopen_t *)file)->array->value.c -
        (char *)((y_vopen_t *)file)->array;
      while (n < i+nbytes) n += n;
      ((y_vopen_t *)file)->array = p_realloc(((y_vopen_t *)file)->array,
                                             nhead + n);
      for (j=len ; j<n ; j++) ((y_vopen_t *)file)->array->value.c[j] = '\0';
      ((y_vopen_t *)file)->array->type.number =
        ((y_vopen_t *)file)->array->type.dims->number = n;
    }
    if (nbytes) memcpy(((y_vopen_t *)file)->array->value.c+i, buf, nbytes);
    ((y_vopen_t *)file)->addr = (i += nbytes);
    if (i > ((y_vopen_t *)file)->maxaddr) ((y_vopen_t *)file)->maxaddr = i;
  } else {
    YError("p_fwrite to text or read-only vopen file handle");
  }
  return nbytes;
}

static int
yv_feof(p_file *file)
{
  return (((y_vopen_t *)file)->addr
          >= ((y_vopen_t *)file)->array->type.number);
}

static int yv_ferror(p_file *file) { return 0; }
static int yv_fflush(p_file *file) { return 0; }

static int
yv_fclose(p_file *file)
{
  Array *array = ((y_vopen_t *)file)->array;
  ((y_vopen_t *)file)->array = 0;
  Unref(array);
  p_free(file);
  return 0;
}

/* ----- end vopen implementation ----- */

static char **yplug_path = 0;

void
Y_plug_dir(int nArgs)
{
  Dimension *dims = 0;
  char **d = (nArgs==1)? YGet_Q(sp, 1, &dims) : 0;
  long nd = d? TotalNumber(dims) : 0;
  int i;
  if (nArgs > 1)
    YError("plug_dir function takes at most one argument");
  if (nArgs && !CalledAsSubroutine()) {
    for (i=0 ; yplug_path && yplug_path[i] ; i++);
    if (i > 1) {
      int n = i - 1;
      Array *rslt;
      Dimension *tmp = tmpDims;
      tmpDims = 0;
      FreeDimension(tmp);
      tmpDims = NewDimension((long)n, 1L, (Dimension *)0);
      rslt = (Array *)PushDataBlock(NewArray(&stringStruct, tmpDims));
      for (i=0 ; i<n ; i++)
        rslt->value.q[i] = p_strcpy(yplug_path[i]);
    } else {
      PushDataBlock(RefNC(&nilDB));
    }
    if (!nd) return;
  }
  if (yplug_path) {
    for (i=0 ; yplug_path[i] ; i++) {
      p_free(yplug_path[i]);
      yplug_path[i] = 0;
    }
    p_free(yplug_path);
    yplug_path = 0;
  }
  yplug_path = p_malloc(sizeof(char *)*(nd+2));
  for (i=0 ; i<=nd ; i++) {
    if (i < nd) {
      yplug_path[i] = YExpandName(d[i]);
      YNameToHead(&yplug_path[i]);
    } else {
      yplug_path[i] = p_strncat(yHomeDir, "lib/", 0L);
    }
  }
  yplug_path[nd+1] = 0;
}

struct y_package_t {
  char *name;
  y_pkg_t *init;
  char **ifiles;
  BuiltIn **code;
  void **data;
  char **varname;
};
static struct y_package_t *y_pkg_list = 0;
static int y_npkg = 0;
static int y_n0pkg = 0;

char *
y_pkg_name(int i)
{
  return (i<0 || i>=y_npkg || !y_pkg_list)? 0 : y_pkg_list[i].name;
}

int
y_pkg_count(int i)
{
  return i? y_npkg : y_n0pkg;
}

y_pkg_t *
y_pkg_lookup(char *name)
{
  int i;
  for (i=0 ; i<y_npkg ; i++)
    if (!strcmp(y_pkg_list[i].name, name))
      return y_pkg_list[i].init;
  return 0;
}

void
y_pkg_add(y_pkg_t *init)
{
  char **ifiles, **varname;
  BuiltIn **code;
  void **data;
  char *pkgname = init(&ifiles, &code, &data, &varname);
  if (!y_pkg_list)
    y_pkg_list = p_malloc(sizeof(struct y_package_t)*8);
  else if (!(y_npkg & 7))
    y_pkg_list = p_realloc(y_pkg_list, sizeof(struct y_package_t)*(y_npkg+8));
  y_pkg_list[y_npkg].name = pkgname;
  y_pkg_list[y_npkg].init = init;
  y_pkg_list[y_npkg].ifiles = ifiles;
  y_pkg_list[y_npkg].code = code;
  y_pkg_list[y_npkg].data = data;
  y_pkg_list[y_npkg].varname = varname;
  y_npkg++;
}

static void y_pkg_0link(char **varname, BuiltIn **code, void **data);

void
y_pkg_link(char *name)
{
  /* use name==0 to relink every package */
  int i;
  if (!y_n0pkg) y_n0pkg = y_npkg;
  for (i=0 ; i<y_npkg ; i++) {
    if (!name || !strcmp(y_pkg_list[i].name, name)) {
      y_pkg_0link(y_pkg_list[i].varname,
                  y_pkg_list[i].code, y_pkg_list[i].data);
      if (name) break;
    }
  }
}

static void
y_pkg_0link(char **varname, BuiltIn **code, void **data)
{
  long index;
  DataBlock *db;

  /* initialize built-in functions */
  if (code) while (*code) {
    index = Globalize(*varname++, 0L);
    db = globTab[index].ops==&dataBlockSym? globTab[index].value.db : 0;
    if (!db || db->ops!=&builtinOps ||
        ((BIFunction *)db)->function!=*code) {
      globTab[index].value.db = (DataBlock *)NewBIFunction(*code, index);
      globTab[index].ops = &dataBlockSym;
      Unref(db);
    }
    code++;
  }

  /* initialize compiled variables */
  if (data) while (*data) {
    index = Globalize(*varname++, 0L);
    db = globTab[index].ops==&dataBlockSym? globTab[index].value.db : 0;
    if (!db || db->ops!=&lvalueOps || ((LValue *)db)->type.base->file ||
        ((LValue *)db)->owner || ((LValue *)db)->address.m!=*data) {
      /* note: everything starts out as a scalar char at the correct address
       * - subsequent reshape will set true data type and dimensions
       */
      globTab[index].value.db =
        (DataBlock *)NewLValueM((Array *)0, *data++,
                                &charStruct, (Dimension *)0);
      globTab[index].ops = &dataBlockSym;
    }
    Unref(db);
  }
}

void
y_pkg_include(char *name, int now)
{
  char **ifiles, *msg;
  int i;
  /* use name==0 to relink every package */
  if (now) {
    for (i=0 ; i<y_npkg ; i++) {
      if (!name || !strcmp(y_pkg_list[i].name, name)) {
        ifiles = y_pkg_list[i].ifiles;
        if (ifiles) while (ifiles[0]) {
          if (YpPushInclude(*ifiles++)) {
            IncludeNow();
          } else {
            msg= p_strncat("missing include file ", ifiles[-1], 0);
            YWarning(msg);
            p_free(msg);
          }
        }
        if (name) break;
      }
    }
  } else {
    /* when not immediate (e.g.- at startup) push in reverse order */
    for (i=y_npkg-1 ; i>=0 ; i--) {
      if (!name || !strcmp(y_pkg_list[i].name, name)) {
        ifiles = y_pkg_list[i].ifiles;
        if (ifiles) {
          while (ifiles[0]) ifiles++;
          for (ifiles-- ; ifiles>=y_pkg_list[i].ifiles ; ifiles--)
            YpPush(ifiles[0]);
        }
        if (name) break;
      }
    }
  }
}

void
Y_plug_in(int nArgs)
{
  char *pname, *pkgname;
  y_pkg_t *init = 0;
  void *plug;
  int i;
  if (nArgs!=1) YError("plug_in function takes exactly one argument");
  pkgname = YGetString(sp-nArgs+1);
  if (!pkgname || !pkgname[0]) 
    YError("plug_in: package name argument is null");
  for (i=1 ; pkgname[i] ; i++);
  for (pname=pkgname+i-1 ; pname>pkgname ; pname--)
    if (pname[0]=='/' || pname[0]=='\\') break;

  /* become a no-op if pkgname already plugged in */
  if (y_pkg_lookup(pname)) return;

  if (!yplug_path) Y_plug_dir(0);

  plug = p_dlopen(pkgname);
  if (!plug && !YIsAbsolute(pkgname)) {
    /* check in plug_dir and Y_HOME/lib before giving up */
    char *tmp;
    for (i=0 ; yplug_path[i] ; i++) {
      tmp = p_strncat(yplug_path[i], pkgname, 0L);
      plug = p_dlopen(tmp);
      p_free(tmp);
      if (plug) break;
    }
  }
  if (plug) {
    char *tmp = p_strncat("yk_", pname, 0);
    int failed = p_dlsym(plug, tmp, 0, &init);
    p_free(tmp);
    if (failed || !init)
      YError("plug_in: dynamic library missing yk_<pkgname> function");
  }
  if (init) {
    y_pkg_add(init);
    y_pkg_link(pname);
  } else {
    YError("plug_in: unable to find dynamic library file");
  }
}

void Y_require(int nArgs)
{
  char *full, *name, *tail= 0;
  long i;
  if (nArgs!=1) YError("require function takes exactly one argument");
  full= YGetString(sp);
  name= YNameTail(full);
  for (i=0 ; i<sourceTab.nItems ; i++) {
    tail= YNameTail(sourceTab.names[i]);
    if (name && tail && strcmp(tail, name)==0) break;
    p_free(tail);
    tail= 0;
  }
  p_free(name);
  p_free(tail);
  if (i>=sourceTab.nItems && !YpPushInclude(full))
    YError("missing include file specified in require function");
  Drop(nArgs);
  if (i>=sourceTab.nItems)
    IncludeNow();   /* parse and maybe execute file to be included */
}

void Y_current_include(int argc)
{
  if (argc != 1 || YNotNil(sp))
    y_error("current_include takes exactly one nil argument");
  if (nYpIncludes > 0 && ypIncludes[nYpIncludes-1].filename != NULL) {
    *ypush_q(NULL) = p_strcpy(ypIncludes[nYpIncludes-1].filename);
  } else {
    ypush_nil();
  }
}

void Y_get_includes(int argc)
{
  if (argc != 1 || YNotNil(sp))
    YError("get_includes takes exactly one nil argument");
  if (sourceTab.nItems > 0) {
    long i, n;
    long dims[2];
    char **s;
    dims[0] = 1L;
    dims[1] = (n = sourceTab.nItems);
    s = ypush_q(dims);
    for (i = 0; i < n; ++i) {
      s[i] = p_strcpy(sourceTab.names[i]);
    }
  } else {
    ypush_nil();
  }
}

/*--------------------------------------------------------------------------*/

static int findingFunc= 0;
/* error handling hacks (for mpy) set up with set_idler
 * bit
 *  1  do not print error messages (like .SYNC.)
 *  2  include [pc] after func name in error messages
 *  4  call after_error in dbug mode (rather than full stack reset)
 *     - after_error responsible for calling dbexit
 *  8  reserved for use by y_errhook
 */
PLUG_API int yerror_flags;
int yerror_flags = 0;

Function *FuncContaining(Instruction *pc)
{
  Function *func= 0;

  if (!findingFunc && pc) {
    long i = -1;
    if (pc>=taskCode && pc<=taskCode+4) return 0;
    findingFunc = 1;
    for (;; i++) {
      while (pc[i].Action) i++;
      if (pc[i-1].Action==&Return) break;
    }
    i++;
    /* Now pc[i] is the Instruction generated by following line
       in parse.c (YpFunc):
          vmCode[nextPC].index= codeSize= nPos+nKey+nLocal+ nextPC;
       (nextPC does NOT include the parameters or locals)
     */
    i -= pc[i].index;
    if (i<0) {
      /* see also Pointee function in ydata.c */
      func = (Function *)((char *)(pc+i) - offsetof(Function, code));
      findingFunc = 0;
    }
  }

  if (findingFunc) {
    /* may get here after a disaster causing an interrupt above, as well
       as after scanning from a garbled initial pc */
    int no_pf = ((yerror_flags&1) != 0);
    findingFunc = 0;
    if (!no_pf) YputsErr("(BUG) lost function produced following error:");
  }
  return func;
}

/*--------------------------------------------------------------------------*/

void ResetStack(int hard)
{
  Instruction *pcRet;
  while ((pcRet= AbortReturn())) if (pcRet==&taskCode[2] && !hard) break;
}

/*--------------------------------------------------------------------------*/

static int y_do_not_abort = 0;

void
y_on_exception(int signal, char *errmsg)
{
  /* signal==PSIG_SOFT for call to p_abort, otherwise this is real signal */
  if (signal != PSIG_SOFT) {
    y_do_not_abort = 1;
    if (signal==PSIG_INT)
      {
        y_catch_category= 0x04;
        YError("Keyboard interrupt received (SIGINT)");
      }
    else if (signal==PSIG_FPE)
      {
        y_catch_category= 0x01;
        YError("Floating point interrupt (SIGFPE)");
      }
    else if (signal==PSIG_SEGV)
      YError("Segmentation violation interrupt (SIGSEGV)");
    else if (signal==PSIG_ILL)
      YError("Illegal instruction interrupt (SIGILL)");
    else if (signal==PSIG_BUS)
      YError("Misaligned address interrupt (SIGBUS)");
    else if (signal==PSIG_IO)
      YError((errmsg&&errmsg[0])? errmsg : "I/O interrupt (SIGIO)");
    else
      YError((errmsg&&errmsg[0])? errmsg :
             "Unrecognized signal delivered to y_on_exception");
  }
  if (ym_state&Y_QUITTING) {
    y_cleanup();
    p_quit();
  }
}

void YWarning(const char *msg)
{
  strcpy(yWarningMsg, "WARNING ");
  strncat(yWarningMsg, msg, 120);
  YputsErr(yWarningMsg);
}

static char *includeFile= 0;
static long mainIndex= -1;
Instruction *yErrorPC= 0;   /* for dbup function in debug.c */

static void yset_catchmsg(char *tmsg);
static long after_index = -1;

PLUG_API int (*y_errhook)(const char *msg, long *after);
int (*y_errhook)(const char *msg, long *after) = 0;

void
YError(const char *msg)
{
  extern void yg_got_expose(void);
  long beginLine= ypBeginLine;
  Instruction *pcDebug= pc;
  Function *func;
  char *name;
  DebugBlk *dbg;
  Instruction *pcUp=yErrorPC, *pcue;
  int category;
  int no_abort = y_do_not_abort;
  int no_print = 0, no_pf = ((yerror_flags&1) != 0);
  int no_reset = ((yerror_flags&4) != 0);

  int recursing= inYError;
  inYError++;
  yErrorPC= 0;
  y_do_not_abort = 0;

  category= y_catch_category;
  y_catch_category= 0x08;

  ym_state &= ~Y_PENDING;
  ym_dbenter = 0;

  if (recursing>8 || yImpossible>8) {
    if (!no_pf) YputsErr("****FATAL**** YError looping -- quitting now");
    ym_state|= Y_QUITTING;
    ym_fatal = 3;
    if (!no_abort) p_abort();
    return;
  }
  yImpossible++;  /* zeroed only by GetNextLine and after CatchScan */

  if (!caughtTask && CatchScan(msg, category)) {
    /* resume at catch PC if this error has been caught --
     * is this really proof against catastrophic looping? */
    inYError= 0;
    yImpossible= 0;
    caughtTask= 1;
    if (!no_abort) p_abort();
    return;
  } else if (caughtTask) {
    caughtTask= 0;
    if (!no_pf) YputsErr("****OOPS**** error on read resume or throw/catch");
  }

  if (y_idler_function) {
    /* remove any idler on error - after_error can reset if desired */
    Function *f= y_idler_function;
    y_idler_function= 0;
    Unref(f);
  }
  if (after_index < 0) after_index = yget_global("after_error", 0L);
  if (!y_idler_fault
      && globTab[after_index].ops == &dataBlockSym
      && globTab[after_index].value.db->ops == &functionOps) {
    /* if after_error function present, make it the idler */
    y_idler_function = (Function *)Ref(globTab[after_index].value.db);
    y_idler_fault = 1;
  } else {
    y_idler_fault = 0;
  }

  /* this is a nasty hack for mpy and after_error */
  no_print = no_pf || (msg && !strcmp(msg, ".SYNC."));

  for (;;) {
    func = (((ym_state&Y_RUNNING)||no_abort) && !recursing &&
            pcUp!=&taskCode[2])? FuncContaining(pcDebug) : 0;
    if (!func || !func->errup) break;
    ClearStack();
    pcue = AbortReturn();
    if (!pcue || pcue==&taskCode[2]) break;
    pcDebug = pc = pcue;
    func = 0;
  }
  name = func? globalTable.names[func->code[0].index] : "VM idle or lost";

  /* Clear out include stack, but remember current include file name.
     If the error happened while executing a main program which came from
     the include file, then includeFile will be the filename, and
     ypBeginLine will be the line number at which the errant main
     program began.  (No other line number information will be available
     for a main program?)  */
  if (!recursing) {
    char *tmp= includeFile;
    includeFile= 0;
    p_free(tmp);
    if (nYpIncludes)
      includeFile= p_strcpy(ypIncludes[nYpIncludes-1].filename);
  }
  YpClearIncludes();

  /* Clean up any Array temporaries (used for data format conversions).  */
  if (recursing<2) ClearTmpArray();

  /* Clear out any pending keyboard input or tasks.  */
  if (recursing<3) {
    p_qclear();
    ClearTasks();
  } else if (nTasks) {
    if (!no_pf) YputsErr("WARNING unable to free task pointers in YError");
    nTasks = 0;
  }

  /* Print error message, with name of current Yorick function prepended:
        ERROR (yorick_function) msg passed to YError
        ERROR (VM idle or lost) msg passed to YError
     The second form is used when the virtual machine is idle at the
     time of the error.  */
  if (!pcUp || recursing)
    strcpy(yErrorMsg, "ERROR (");
  else
    strcpy(yErrorMsg, "Up to (");
  strncat(yErrorMsg, name, 40);
  if (func && (yerror_flags&2)!=0) {
    char relpc[32];
    sprintf(relpc, "[%ld]", (long)(pcDebug-func->code));
    strncat(yErrorMsg, relpc, 12);
  }
  strcat(yErrorMsg, ") ");
  if (!pcUp || recursing)
    strncat(yErrorMsg, msg, 140);
  if (y_errhook) {
    long index;
    int hook = y_errhook(yErrorMsg, &index);
    if (no_print != no_pf) no_pf = (hook & 1);
    else no_print = no_pf = (hook & 1);
    if ((hook & 2)
        && globTab[index].ops == &dataBlockSym
        && globTab[index].value.db->ops == &functionOps) {
      /* if alternate after_error function present, make it the idler */
      y_idler_function = (Function *)Ref(globTab[index].value.db);
      no_reset = ((hook & 4) != 0);
    }
  }
  if (!no_print) YputsErr(yErrorMsg);

  if (recursing) {
    func= 0;
    if (!no_print) YputsErr("WARNING aborting on recursive calls to YError");
  } else if (ym_state&Y_SUSPENDED) {
    func= 0;
    if (y_read_prompt) {
      if (!no_print) YputsErr("WARNING aborting on YError"
                              " during keyboard read()");
    } else if (ym_suspc) {
      if (!no_print) YputsErr("WARNING aborting on YError during suspend");
    } else {
      if (!no_print) YputsErr("WARNING aborting on YError"
                              " after mouse() pause() or wait=1");
    }
  }
  ym_suspc = 0;
  if (yg_blocking==3 || yg_blocking==4) yg_blocking = 0;

  if (func && !no_print) {
    /* Try to find the source code for this function.  */
    long index= func->code[0].index;
    if (mainIndex<0) mainIndex= Globalize("*main*", 0L);
    if (index==mainIndex) {
      name= includeFile;
    } else {
      char *mess= YpReparse(func);
      if (nTasks) ClearTasks();
      if (mess[0]!='*') {
        /* reparse succeeded, skip to filename */
        name= 0;
        while (mess[0] && mess[0]!=':') mess++;
        if (mess[0]) do { mess++; } while (mess[0] && mess[0]!=':');
        if (mess[0]) {
          mess++;
          if (mess[0]==' ') mess++;
          if (mess[0]) name= mess;
        }
      } else {
        /* reparse failed */
        name= 0;
      }
      beginLine= 0;  /* used only for *main* from includeFile */
    }

    /* Push debug info (function and code index) onto stack.  */
    ClearStack();
    CheckStack(2);
    dbg= NewDebugBlk(func, pcDebug-func->code, name, beginLine);
    if (dbg) {
      PushDataBlock(dbg);
    } else {
      ResetStack(0);
      YputsErr("Function corrupted, cannot enter debug mode.");
    }

    if (y_idler_function) {
      /* special after_error function will get control */
      if (no_reset && yDebugLevel<=2 && !y_read_prompt) {
        if (yDebugLevel>1) ResetStack(0);
      } else {
        ResetStack(1);
        yr_reset();
      }
      yg_got_expose();
    } else if (!yBatchMode && !pcUp && (!yAutoDebug || yDebugLevel>1)) {
      if (yDebugLevel>1) {
        YputsErr(" To enter recursive debug level, type <RETURN> now");
      } else {
        YputsErr(" To enter debug mode, type <RETURN> now"
                 " (then dbexit to get out)");
      }
      ym_dbenter = 1;
    }

  } else {
    /* Clear the stack back to the most recent debugging level,
     * or completely clear if aborting a read() operation.  */
    if (recursing<5) {
      ResetStack(y_read_prompt!=0);
    } else {
      if (!no_pf) YputsErr("****SEVERE**** YError unable to reset stack -- "
                           "probably lost variables");
      sp= spBottom;
    }
    ym_state &= ~Y_SUSPENDED;
    if (y_read_prompt) yr_reset();
    yg_got_expose();  /* in case window,wait=1 or pause */
  }
  p_clr_alarm(0, 0);
  ym_after_dq(-1);

  if (ym_state&Y_QUITTING) {
    if (!no_pf)
      YputsErr("****TERMINATING**** on error after main loop exit");
    if (!no_abort) p_abort();
    return;
  }

  if ((yBatchMode && !y_idler_function) || !strncmp(msg,"(FATAL)",7)) {
    if (!no_pf) {
      if (yBatchMode) YputsErr("yorick: quitting on error in batch mode");
      else YputsErr("yorick: quitting on fatal error");
    }
    ym_fatal = yBatchMode? 1 : 2;
    ResetStack(0);
    ym_state|= Y_QUITTING;
  }

  /* set catch_message variable for after_error function */
  if (y_idler_function) yset_catchmsg(yErrorMsg);

  /* Go back to the main loop.  */
  inYError= 0;
  if (!no_abort) p_abort();
}

/*--------------------------------------------------------------------------*/

static Function *help_worker= 0;

void Y_help(int nArgs)
{
  Symbol *stack= sp-nArgs+1;
  long index, worker_arg, isrc=-1;
  int nAbove;
  p_file *file;

  worker_arg= Globalize("help_topic", 0L);

  while (stack<=sp && !stack->ops) stack+=2;  /* skip any keywords */
  nAbove= sp-stack;
  if (nAbove>=0) {
    /* a legal argument has been supplied */
    if (stack->ops==&referenceSym) {
      index= stack->index;
      ReplaceRef(stack);
    } else {
      index= -1;
    }
    if (stack->ops==&dataBlockSym) {
      DataBlock *db= stack->value.db;
      Operations *ops= db->ops;
      if (ops==&functionOps) {
        Function *f= (Function *)db;
        index= f->code[0].index;
        isrc = f->isrc;
      } else if (ops==&structDefOps) {
        StructDef *base= (StructDef *)db;
        while (base->model) base= base->model;
        index= Globalize(yStructTable.names[base->index], 0L);
      } else if (ops==&builtinOps) {
        BIFunction *f= (BIFunction *)db;
        index= f->index;
      }
    }
    Drop(nAbove);
    nArgs-= nAbove+1;
  } else {
    /* no legal arguments, help function itself is target */
    BIFunction *f= (BIFunction *)(sp-nArgs)->value.db;
    index= f->index;
    PushDataBlock(RefNC(&nilDB));
  }

  /* move help topic argument off stack into help_topic extern variable */
  PopTo(&globTab[worker_arg]);  /* help_topic */
  Drop(nArgs);  /* only argument of any conceivable value just saved */

  if (!help_worker) {
    long help_index= Globalize("help_worker", 0L);
    if (globTab[help_index].ops!=&dataBlockSym ||
        (help_worker= (Function *)Ref(globTab[help_index].value.db))->ops
        !=&functionOps)
      YError("(BUG) help_worker function not found -- help unavailable");
  }

  /* create help_file extern variable */
  if (index>=0 && (file= OpenSource(index, isrc)))
    PushDataBlock(NewTextStream(p_strcpy(ypIncludes[nYpIncludes-1].filename),
                                file, 1, ypBeginLine-1, p_ftell(file)));
  else
    PushDataBlock(RefNC(&nilDB));
  worker_arg= Globalize("help_file", 0L);
  PopTo(&globTab[worker_arg]);  /* help_file */

  RunTaskNow(help_worker);
}

/*--------------------------------------------------------------------------*/

void Y_exit(int nArgs)
{
  char *msg= 0;
  if (nArgs>1) YError("exit takes exactly zero or one argument");
  if (nArgs==1 && YNotNil(sp)) msg= YGetString(sp);
  if (msg) YputsOut(msg);
  else YputsOut("EXIT called, back to main loop");
  ResetStack(0);
  p_abort();
}

void Y_error(int nArgs)
{
  char *msg= 0;
  if (nArgs>1) YError("error takes exactly zero or one argument");
  if (nArgs==1 && YNotNil(sp)) msg= YGetString(sp);
  y_catch_category= 0x10;
  if (msg) YError(msg);
  else YError("<interpreted error function called>");
}

/* FIXME: this should also turn off on_stdin event handling */
void Y_batch(int nArgs)
{
  int flag= 2;
  if (nArgs>1) YError("batch takes exactly zero or one argument");
  if (nArgs==1 && YNotNil(sp)) {
    flag= (YGetInteger(sp)!=0);
    Drop(1);
  }
  PushIntValue(yBatchMode);
  if (flag!=2) yBatchMode= flag;
}

/*--------------------------------------------------------------------------*/

static Catcher *CatchNew(void)
{
  if (n_catchers>=max_catchers) {
    catchers= p_realloc(catchers, (max_catchers+16)*sizeof(Catcher));
    max_catchers+= 16;
  }
  catchers[n_catchers].task= 0;
  catchers[n_catchers].pc= 0;
  catchers[n_catchers].isp= 0;
  catchers[n_catchers].category= 0;
  return &catchers[n_catchers++];
}

void YCatchDrop(long isp)
{
  while (n_catchers>0 && catchers[n_catchers-1].isp>=isp) {
    n_catchers--;
    catchers[n_catchers].category= 0;
  }

  if (n_catchers>0) ispCatch= catchers[n_catchers-1].isp;
  else ispCatch= 0;

  if ((max_catchers>>6) > n_catchers) {
    /* attempt to limit damage from runaway catch calls */
    catchers= p_realloc(catchers, (max_catchers>>6)*sizeof(Catcher));
    max_catchers>>= 6;
  }
}

static Catcher *CatchScan(const char *msg, int category)
{
  long i= n_catchers-1;
  while (i>=0 && !(category&catchers[i].category)) {
    if (catchers[i].task != &taskCode[2]) i= 0;
    i--;
  }

  if (i>=0) {
    char tmsg[144];
    Instruction *pcRet;
    Symbol *spCatch= spBottom + catchers[i].isp;
    catchers[i].category= 0;  /* disable this catcher */

    /* note: msg itself might be on stack! */
    strncpy(tmsg, msg, 140);
    tmsg[140]= '\0';

    for (;;) {
      ClearStack();
      if (spCatch >= sp) break;
      pcRet= AbortReturn();
      if (!pcRet || pcRet==&taskCode[2])
        YError("catch does not work outside immediate include or require");
    }
    if (spCatch!=sp) YError("(BUG) impossible catch or corrupt stack");
    pc= catchers[i].pc;
    PushIntValue(1);

    /* set catch_message variable (after stack cleared) */
    yset_catchmsg(tmsg);

    return &catchers[i];

  } else {
    return 0;
  }
}

static void
yset_catchmsg(char *tmsg)
{
  Array *array;
  long cmsg = Globalize("catch_message", 0L);
  if (globTab[cmsg].ops==&dataBlockSym) {
    globTab[cmsg].ops = &intScalar;
    Unref(globTab[cmsg].value.db);
  }
  array=  NewArray(&stringStruct, (Dimension *)0);
  globTab[cmsg].value.db = (DataBlock *)array;
  globTab[cmsg].ops = &dataBlockSym;
  array->value.q[0] = p_strcpy(tmsg);
}

extern VMaction BranchFalse, BranchTrue;

void Y_catch(int nArgs)
{
  Catcher *catcher= 0;
  long i= n_catchers-1;
  long isp= (sp-2) - spBottom;
  int category;
  if (nArgs!=1) YError("catch takes exactly one argument");
  category= YGetInteger(sp);
  if ((sp-2)->ops != &returnSym ||
      (pc->Action!=&BranchFalse && pc->Action!=&BranchTrue))
    YError("catch() must be the condition in an if or while statement");

  while (i>=0 && catchers[i].task==&taskCode[2] && catchers[i].isp==isp) {
    if (catchers[i].pc==pc) {
      catcher= &catchers[i];
      break;
    }
    i--;
  }
  if (!catcher) catcher= CatchNew();
  catcher->task= &taskCode[2];
  catcher->pc= pc;
  catcher->isp= ispCatch= isp;
  catcher->category= category;

  PushIntValue(0);
}

/*--------------------------------------------------------------------------*/

void Y_set_idler(int nArgs)
{
  Function *f;
  if (nArgs>1) {
    /* second argument is hack to add error handling features */
    if (nArgs==2 && sp[-1].ops) yerror_flags = YGetInteger(sp);
    else YError("set_idler function takes zero or one arguments");
  }

  if (nArgs>0 && YNotNil(sp-nArgs+1)) {
    f = (Function *)sp[1-nArgs].value.db;
    if (sp[1-nArgs].ops!=&dataBlockSym || f->ops!=&functionOps)
      YError("set_idler expecting function as argument");
    y_idler_function = Ref(f);

  } else if (y_idler_function) {
    f = y_idler_function;
    y_idler_function = 0;
    Unref(f);
  }
}

/*--------------------------------------------------------------------------*/
