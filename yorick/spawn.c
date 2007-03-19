/*
 * $Id: spawn.c,v 1.3 2007-03-19 18:38:06 dhmunro Exp $
 * yorick spawn process command
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "pstdlib.h"
#include "play.h"
#include "ydata.h"
#include "yio.h"

#include <string.h>

/* FIXME -- things not in any .h file */
extern void (*CleanUpForExit)(void);
extern VMaction PushVariable, PushString, Eval, DropTop, PushNil, Return;

extern BuiltIn Y_spawn;

typedef struct spawn_proc spawn_proc;
struct spawn_proc {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  char *argv0;         /* name used to exec process */
  p_spawn_t *proc;
  long callout;        /* process stdout callback globTab index */
  long callerr;        /* process stderr callback globTab index */
  spawn_proc *next;
};
static spawn_proc *spawn_list = 0;

static void spawn_free(void *vproc);  /* for Unref only */
static char spawn_desc[] = "spawn-process";
static void spawn_eval(Operand *op);
static void spawn_print(Operand *op);
static void spawn_callback(void *vproc, int err);
static void spawn_cleanup(void);
static void (*spawn_prevclean)(void) = 0;
static int spawn_setclean = 0;

static Operations spawn_ops = {
  &spawn_free, T_OPAQUE, 0, T_STRING, spawn_desc,
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, spawn_eval, &SetupX, &GetMemberX, &MatMultX, spawn_print
};

void
Y_spawn(int nargs)
{
  Dimension *dims = 0;
  char **argv = yarg_q(nargs-1, &dims);
  long argc = 1;
  Operand op;
  Operand *pop = yarg_op(nargs-2, &op);
  long callout=-1, callerr=-1;
  spawn_proc *proc;

  if (nargs<2 || nargs>3)
    YError("spawn: accepts precisely two or three arguments");
  if (dims) {
    if (dims->next)
      YError("spawn: first argument must be string or 1D array of strings");
    argc = dims->number;
  }
  if (!argv || !argv[0] || !argv[0][0])
    YError("spawn: first element of first argument must be process name");
  if (!pop || (op.ops!=&functionOps && op.ops!=&builtinOps))
    YError("spawn: second argument must be callback function");
  if (op.ops==&builtinOps) callout = ((BIFunction *)op.value)->index;
  else callout = ((Function *)op.value)->code[0].index;
  if (nargs == 3) {
    pop = yarg_op(0, &op);
    if (!pop || (op.ops!=&functionOps && op.ops!=&builtinOps))
      YError("spawn: third argument must be callback function");
    if (op.ops==&builtinOps) callerr = ((BIFunction *)op.value)->index;
    else callerr = ((Function *)op.value)->code[0].index;
  }

  if (argv[argc-1]) {
    /* must construct a 0-terminated argv list */
    typedef struct tmpobj {
      int references;
      Operations *ops;
      void (*zapper)(void *to);
      char *argv[1];
    } tmpobj;
    tmpobj *tmp;
    long i;
    /* CheckStack(2); fnctn.c guarantees at least 2 free stack slots */
    tmp = PushDataBlock(y_new_tmpobj(sizeof(tmpobj)+argc*sizeof(char*),
                                     p_free));
    for (i=0 ; i<argc ; i++) tmp->argv[i] = argv[i];
    tmp->argv[i] = 0;
    argv = tmp->argv;
    /* tmp will be cleaned up when stack cleared */
  }

  /* push result object onto stack */
  proc = p_malloc(sizeof(spawn_proc));
  proc->references = 0;
  proc->ops = &spawn_ops;
  proc->proc = p_spawn(argv[0], argv, spawn_callback, proc, callerr>=0);
  proc->argv0 = p_strcpy(argv[0]);
  proc->callout = callout;
  proc->callerr = callerr;
  proc->next = spawn_list;
  spawn_list = proc;
  PushDataBlock(proc);
  if (!proc->proc) {
    Drop(1);
    PushDataBlock(RefNC(&nilDB));
  }

  if (!spawn_setclean) {
    spawn_setclean = 1;
    spawn_prevclean = CleanUpForExit;
    CleanUpForExit = spawn_cleanup;
  }
}

void
spawn_cleanup(void)
{
  spawn_proc *list = spawn_list;
  while (list) {
    if (list->proc) {
      p_spawf(list->proc, 1);
      list->proc = 0;
    }
    list = list->next;
  }
  if (spawn_prevclean) spawn_prevclean();
}

static void
spawn_free(void *vproc)
{
  if (vproc) {
    spawn_proc *proc = vproc;
    spawn_proc *list = spawn_list;
    char *argv0 = proc->argv0;
    p_spawn_t *pp = proc->proc;
    proc->argv0 = 0;
    proc->proc = 0;
    p_free(argv0);
    if (pp) {
      p_send(pp, (char *)0, -9);
      p_spawf(pp, 0);
    }
    if (list == proc) {
      spawn_list = proc->next;
    } else while (list && list->next) {
      if (list->next == proc) {
        list->next = proc->next;
        break;
      }
      list = list->next;
    }
    p_free(proc);
  }
}

static void
spawn_print(Operand *op)
{
  spawn_proc *proc = op->value;
  ForceNewline();
  PrintFunc("spawned process: ");
  if (!proc) PrintFunc("<no longer exists>");
  else if (proc->argv0) PrintFunc(proc->argv0);
  else PrintFunc("<name lost>");
  ForceNewline();
}

/* send message or signal to process */
static void
spawn_eval(Operand *op)
{
  spawn_proc *proc = op->value;
  if (proc->proc) {
    int nargs = op->references;  /* (sic) misuse explained in ops3.c */
    Operand arg;
    Range *range;
    if (nargs!=1 || !yarg_op(0,&arg))
      YError("spawned process requires exactly one argument");
    range = (arg.ops==&rangeOps)? arg.value : 0;
    if (arg.ops!=&stringOps && arg.ops->typeID>T_LONG &&
        (!range || range->nilFlags!=(R_PSEUDO+R_MINNIL+R_MAXNIL)))
      YError("spawned process argument must be string or integer or -");
    if (arg.type.dims)
      YError("spawned process argument must be scalar");
    if (!proc->proc)
      YError("spawned process no longer exists");

    if (arg.ops == &stringOps) {
      /* send text message to process via its stdin */
      char *msg = *(char **)arg.value;
      if (msg && p_send(proc->proc, msg, strlen(msg)))
        YError("spawned process write to stdin failed");

    } else if (range) {
      /* suspend yorick virtual machine until callback from process */
      YError("(BUG) spawned process suspend not implemented");

    } else {
      /* send signal to process */
      long signum;
      arg.ops->ToLong(&arg);
      signum = *(long *)arg.value;
      p_send(proc->proc, (char *)0, -signum);
    }

  } else {
    YError("spawned process has no stdin");
  }

  Drop(1);
}

/* do interpreted callback to process message from process */
static void
spawn_callback(void *vproc, int err)
{
  spawn_proc *proc = vproc;
  char *msg = 0;
  Array *msga;
  Instruction code[12];
  Symbol *ctable;
  long callback = -1;
  if (err != 2) {
    long nbytes = 0;
    if (proc && proc->proc) callback = err? proc->callerr : proc->callout;
    if (callback<0) {
      /* probably a bug, but unclear that calling YError
       * would prevent the possible fault loop
       */
      return;
    }

    /* read the message from process
     * - this just reads all available bytes, up to 2048
     */
    msg = p_malloc(2048);
    nbytes = p_recv(proc->proc, msg, 2048);
    if (nbytes <= 0) {
      p_free(msg);
      /* can get this when an unrelated process finishes, just ignore */
      /* YError("spawn process read error in callback"); */
      /* also could get this if the fd gave POLLERR, possibly should
       * find some way to figure out what's going on
       */
      return;
    }
    msg = p_realloc(msg, nbytes+1);
    msg[nbytes] ='\0';
  } else {
    if (!proc || proc->callout<0) return;
    callback = proc->callout;
    proc->callout = -1;
    p_spawf(proc->proc, 1);
    proc->proc = 0;
  }

  if (callback < 0) return;

  /* task constant table contains only message string */
  msga = NewArray(&stringStruct, (Dimension *)0);
  msga->value.q[0] = msg;
  ctable = p_malloc(sizeof(Symbol));
  ctable->ops = &dataBlockSym;
  ctable->value.db = (DataBlock *)msga;

  /* fill in function code */
  code[0].Action = &PushVariable;
  code[1].index = callback;
  code[2].Action = &PushString;
  code[3].constant = ctable;
  code[4].Action = &Eval;
  code[5].count = 1;
  code[6].Action = &DropTop;
  code[7].Action = &PushNil;
  code[8].Action = &Return;
  code[9].Action = 0;
  code[10].index = 11;
  /* NewFunction moves this to beginning */
  code[11].index = Globalize("*callback*", 0L);
  PushTask(NewFunction(ctable, 1, 0, 0, 0, 0, 2, code, 11));
}
