/*
 *$Id: uspawn.c,v 1.3 2010-02-28 21:32:23 dhmunro Exp $
 * play spawn process command
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"
#include "pstdlib.h"
#include "pstdio.h"
#include "playu.h"
#include "play.h"

#include <sys/types.h>
#ifndef NO_PROCS
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#endif

/* opaque, instance returned by spawn, freed by spawf */
struct p_spawn_t {
  pid_t pid;
  int fdin, fdout, fderr, ready;
  void (*callback)(void *ctx, int err);
  void *ctx;
  p_spawn_t *next;  /* u_chldlist of all child processes */
};

#ifndef NO_PROCS
static p_spawn_t *u_chldlist = 0;

static int u_prechld(void *dummy);
static int u_setpre = 0;

static void u_spawf0(p_spawn_t *proc, int nocallback);
static void u_callout(void *vproc);
static void u_callerr(void *vproc);
#endif

/* p_popen in files.c also creates a process */
int
p_system(const char *cmdline)
{
#ifndef NO_PROCS
  return system(cmdline);
#else
  return -1;
#endif
}

p_spawn_t *
p_spawn(char *name, char **argv, void (*callback)(void *ctx, int err),
        void *ctx, int err)
{
#ifndef NO_PROCS
  int pipe_in[2], pipe_out[2], pipe_err[2];
  pid_t pid;

  /* open pipes for stdin, stdout, stderr */
#ifndef O_NONBLOCK
#  define O_NONBLOCK O_NDELAY
#endif
  if (pipe(pipe_in) < 0) return 0;
  if (pipe(pipe_out) < 0) {
    close(pipe_in[0]), close(pipe_in[1]);
    return 0;
  }
  fcntl(pipe_in[1], F_SETFL, O_NONBLOCK);
  fcntl(pipe_out[0], F_SETFL, O_NONBLOCK);
  if (err) {
    if (pipe(pipe_err) < 0) {
      close(pipe_in[0]), close(pipe_in[1]);
      close(pipe_out[0]), close(pipe_out[1]);
      return 0;
    }
    fcntl(pipe_err[0], F_SETFL, O_NONBLOCK);
  } else {
    pipe_err[0] = pipe_err[1] = -1;
  }

  /* spawn the process */
  pid = fork();
  if (pid) {
    /* parent, program that called p_spawn */
    p_spawn_t *proc = p_malloc(sizeof(p_spawn_t));

    proc->callback = callback;
    proc->ctx = ctx;
    proc->pid = pid;
    setpgid(pid, pid);  /* put child in its own process group */

    proc->fdin = pipe_in[1],  close(pipe_in[0]);
    proc->fdout = pipe_out[0],  close(pipe_out[1]);
    proc->fderr = -1;
    if (err) proc->fderr = pipe_err[0],  close(pipe_err[1]);
    proc->ready = 0;

    proc->next = u_chldlist;
    u_chldlist = proc;

    if (!u_setpre) u_prepoll(u_prechld, &u_setpre), u_setpre = 1;
    u_event_src(proc->fdout, u_callout, proc);
    if (err) u_event_src(proc->fderr, u_callerr, proc);
    return proc;

  } else if (pid == 0) {
    /* child, i.e.- spawned process */
    pid = getpid();
    setpgid(pid, pid);  /* put child in its own process group */

    /* turn off play signal handling (see play/unix/handler.c) */
    signal(SIGALRM, SIG_IGN);  /* clear play alarms */
    signal(SIGALRM, SIG_DFL);
    signal(SIGFPE, SIG_DFL);
    signal(SIGINT, SIG_DFL);
    signal(SIGILL, SIG_DFL);
    signal(SIGSEGV, SIG_DFL);
    signal(SIGPIPE, SIG_DFL);
#ifdef SIGBUS
    signal(SIGBUS, SIG_DFL);
#endif

    /* connect process stdin, stdout, stderr to yorick */
    if (pipe_in[0] != 0) dup2(pipe_in[0], 0), close(pipe_in[0]);
    close(pipe_in[1]);
    if (pipe_out[1] != 1) dup2(pipe_out[1], 1), close(pipe_out[1]);
    close(pipe_out[0]);
    if (err) {
      if (pipe_err[1] != 2) dup2(pipe_err[1], 2), close(pipe_err[1]);
      close(pipe_err[0]);
    }

    u_closeall();

    execvp(name, argv);
    _exit(127);

  } else {
    /* fork failed */
    close(pipe_in[0]), close(pipe_in[1]);
    close(pipe_out[0]), close(pipe_out[1]);
    if (err) close(pipe_err[0]), close(pipe_err[1]);
  }
#endif
  return 0;
}

void
p_spawf(p_spawn_t *proc, int nocallback)
{
#ifndef NO_PROCS
  if (proc) {
    u_spawf0(proc, nocallback);
    p_free(proc);
  }
#endif
}

#ifndef NO_PROCS
static void
u_spawf0(p_spawn_t *proc, int nocallback)
{
  if (proc) {
    p_spawn_t *list = u_chldlist;
    int in=proc->fdin, out=proc->fdout, err=proc->fderr;

    if (err>=0) u_event_src(err, 0, proc);
    if (out>=0) u_event_src(out, 0, proc);
    if (proc->pid) {
      int status;
      pid_t pid = waitpid(proc->pid, &status, WNOHANG);
      if (pid == 0) {  /* not dead, no error, so kill it */
        if (!kill(proc->pid, SIGKILL))
          waitpid(proc->pid, &status, 0);
        /* gentler to close stdin and wait to see if process exits? */
      }
      if (proc->callback  && !nocallback)
        proc->callback(proc->ctx, 2);
    }
    proc->pid = 0;
    proc->callback = 0;

    proc->fdin = -1;   if (in>=0) close(in);
    proc->fdout = -1;  if (out>=0) close(out);
    proc->fderr = -1;  if (err>=0) close(err);
    proc->ready = 0;

    if (list == proc) {
      u_chldlist = proc->next;
    } else {
      while (list && list->next!=proc) list = list->next;
      if (list) list->next = proc->next;
    }

    if (!u_chldlist) {
      u_setpre = 0;
      u_prepoll(0, &u_setpre);
    }
  }
}

/* prepoller makes final callback to terminated processes */
/* ARGSUSED */
static int
u_prechld(void *dummy)
{
  /* deliver final callback for terminated processes */
  int serviced = 0;
  p_spawn_t *list = u_chldlist;
  int status;
  pid_t pid;

  while (list) {
    if (list->pid) {
      pid = waitpid(list->pid, &status, WNOHANG);
      if (list->pid == pid) list->pid = 0;
    }
    list = list->next;
  }
  list = u_chldlist;

  while (list) {
    if (!list->pid) {
      list->callback(list->ctx, 2);
      u_spawf0(list, 1);  /* should this be done before callback? */
      serviced = 1;
      break;
    }
    list = list->next;
  }

  /* turn off prepolling if no active processes */
  if (!u_chldlist) {
    u_setpre = 0;
    u_prepoll(0, &u_setpre);  /* note that serviced == 1 always */
  }

  return serviced;
}

/* event callbacks to process stdout and stderr from process */
static void
u_callout(void *vproc)
{
  p_spawn_t *proc = vproc;
  if (proc->fdout>=0) proc->ready |= 1;
  proc->callback(proc->ctx, 0);
}
static void
u_callerr(void *vproc)
{
  p_spawn_t *proc = vproc;
  if (proc->fderr>=0) proc->ready |= 2;
  proc->callback(proc->ctx, 1);
}
#endif

int
p_send(p_spawn_t *proc, char *msg, long len)
{
  int result = 2;
#ifndef NO_PROCS
  if (proc && proc->pid) {
    result = 1;
    if (len > 0) {
      if (msg) result = write(proc->fdin, msg, len)<0;
    } else if (len < 0) {
      if (len == -9) u_spawf0(proc, 0);
      else kill(proc->pid, (int)(-len));
    }
  }
#endif
  return result;
}

long
p_recv(p_spawn_t *proc, char *msg, long len)
{
#ifndef NO_PROCS
  if (proc && proc->pid && len>0) {
    if (proc->ready&1) {
      proc->ready &= ~1;
      return read(proc->fdout, msg, len);
    } else if (proc->ready&2) {
      proc->ready &= ~2;
      return read(proc->fderr, msg, len);
    }
  }
#endif
  return -1;
}
