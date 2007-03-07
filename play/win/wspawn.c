/*
 * $Id: wspawn.c,v 1.3 2007-03-07 00:50:24 dhmunro Exp $
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
#include "playw.h"

#include <string.h>

/* p_popen in files.c also creates a process */
int
p_system(const char *cmdline)
{
  return system(cmdline);
}

#define SPAWN_MAXMSG 2048

typedef struct spawn_rd_t spawn_rd_t;
struct spawn_rd_t {
  p_spawn_t *parent;
  HANDLE pipe;      /* stdout or stderr from child process */
  HANDLE ready;     /* event set when buf has been filled from pipe */
  HANDLE accepted;  /* event set when buf has been used */
  int n;            /* actual number of bytes available, -1 on broken pipe */
  char buf[SPAWN_MAXMSG+4];        /* i/o buffer for pipe */
};

/* opaque, instance returned by spawn, freed by spawf */
struct p_spawn_t {
  DWORD pid;
  HANDLE in;
  void (*callback)(void *ctx, int err);
  void *ctx;      /* context parameter for callback */
  int flags;      /* uses + 4 stderr, 8 p_recv in callerr, 16 error */
  spawn_rd_t o[2];
};

static int spawn_mkpipe(HANDLE *rdend, HANDLE *wrend, int wrdup);
static void w_callout(void *vproc);
static void w_callerr(void *vproc);
static DWORD WINAPI spawn_reader(LPVOID lpvThreadParam);

#define SPAWN_HCLOSE(h) if (h) CloseHandle(h), h = 0

static int
spawn_mkpipe(HANDLE *rdend, HANDLE *wrend, int wrgive)
{
  SECURITY_ATTRIBUTES sa;
  ZeroMemory(&sa,sizeof(SECURITY_ATTRIBUTES));
  sa.nLength= sizeof(SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  if (CreatePipe(rdend, wrend, &sa, 0)) {
    /* turn off inheritance for parent end of this pipe */
    SetHandleInformation(wrgive?*rdend:*wrend, HANDLE_FLAG_INHERIT, 0);
    return 0;
  }
  *rdend = *wrend = 0;
  return 1;
}

p_spawn_t *
p_spawn(char *name, char **argv, void (*callback)(void *ctx, int err),
        void *ctx, int err)
{
  p_spawn_t *proc;
  HANDLE ht1;
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  long len, cmdlen;
  char *cmdline;
  int nargs = 0;

  if (!name[0]) return 0;
  /* count number of characters required to hold command line */
  cmdlen = len = strcspn(name, " \t");
  if (name[len]) cmdlen += 2 + strlen(name+len);
  if (argv[0]) for (nargs=1 ; argv[nargs] ; nargs++) {
    len = strcspn(argv[nargs], " \t");
    if (name[len] || !len) len += 2 + strlen(argv[nargs]+len);
    cmdlen += 1 + len;
  }
  /* construct the command line from name, argv */
  if (cmdlen > 32767) return 0;  /* mswindows limitation */

  proc = p_malloc(sizeof(p_spawn_t));
  if (!proc) return 0;
  ZeroMemory(proc, sizeof(p_spawn_t));
  proc->flags = 1;  /* this threads use */
  proc->o[0].parent = proc->o[1].parent = proc;

  ZeroMemory(&si, sizeof(STARTUPINFO));
  si.cb = sizeof(STARTUPINFO);
  si.dwFlags = STARTF_USESTDHANDLES;

  /* open pipes for stdin, stdout, stderr */
  if (spawn_mkpipe(&si.hStdInput, &proc->in, 0) ||
      spawn_mkpipe(&proc->o[0].pipe, &si.hStdOutput, 1) ||
      spawn_mkpipe(&proc->o[1].pipe, &si.hStdError, 1)) goto err0;

  /* note that name=argv[0] normally
   * CreateProcess searches PATH like execvp
   *
   * unlike execvp, CreateProcess requires argv concatentated into
   * a single pseudo command line (<32kB long)
   * constructing a properly quoted command line for all possible cases
   * is extremely difficult, and beyond the scope of this interface
   * (see emacs w32proc.c code) - just handle the common case of embedded
   * spaces in some of the arguments
   */
  cmdline = p_malloc(cmdlen+1);
  cmdlen = len = strcspn(name, " \t");
  if (name[len]) {
    cmdline[0] = '"';
    strcpy(cmdline+1, name);
    cmdlen += 2 + strlen(name+len);
    cmdline[cmdlen-1] = '"';
    cmdline[cmdlen] = '\0';
  } else {
    strcpy(cmdline, name);
  }
  if (argv[0]) for (nargs=1 ; argv[nargs] ; nargs++) {
    cmdline[cmdlen++] = ' ';
    len = strcspn(argv[nargs], " \t");
    if (name[len] || !len) {
      cmdline[cmdlen] = '"';
      strcpy(cmdline+cmdlen+1, argv[nargs]);
      len += 2 + strlen(argv[nargs]+len);
      cmdline[cmdlen+len-1] = '"';
      cmdline[cmdlen+len] = '\0';
    } else {
      strcpy(cmdline+cmdlen, argv[nargs]);
    }
    cmdlen += len;
  }
  /* start the new process and clean up CreateProcess mess */
  /* CREATE_NEW_CONSOLE CREATE_NO_WINDOW(no msdos) DETACHED_PROCESS */
  len = CreateProcess(NULL, cmdline, NULL, NULL, TRUE,
		      DETACHED_PROCESS, NULL, NULL, &si, &pi);
  /* close child side of pipes */
  SPAWN_HCLOSE(si.hStdInput);
  SPAWN_HCLOSE(si.hStdOutput);
  SPAWN_HCLOSE(si.hStdError);
  p_free(cmdline);
  if (!len) goto err0;

  proc->pid = pi.dwProcessId;
  CloseHandle(pi.hThread);
  CloseHandle(pi.hProcess);

  /* create two new threads to babysit stdout, stderr for child process */
  proc->callback = callback;
  proc->ctx = ctx;
  if (!err) proc->flags |= 4;
  proc->o[0].n = proc->o[1].n = 0;
  proc->o[0].ready = CreateEvent(0, 0, 0, 0);  /* Create(0,1,0,0) ?? */
  proc->o[0].accepted = CreateEvent(0, 0, 0, 0);
  proc->o[1].ready = CreateEvent(0, 0, 0, 0);  /* Create(0,1,0,0) ?? */
  proc->o[1].accepted = CreateEvent(0, 0, 0, 0);
  if (!proc->o[0].ready || !proc->o[0].accepted ||
      !proc->o[1].ready || !proc->o[1].accepted) goto errt0;
  ht1 = CreateThread(NULL, 0, spawn_reader, (LPVOID)&proc->o[0],
		     0, &pi.dwThreadId);
  proc->flags++;
  if (!ht1) goto errt0;
  CloseHandle(ht1);
  pi.hThread = CreateThread(NULL, 0, spawn_reader, (LPVOID)&proc->o[1],
			    0, &pi.dwThreadId);
  if (!pi.hThread) goto errt1;
  proc->flags++;
  CloseHandle(pi.hThread);

  w_add_input(proc->o[0].ready, w_callout, &proc->o[0]);
  w_add_input(proc->o[1].ready, w_callerr, &proc->o[1]);

  return proc;

 errt1:
  /* TerminateThread(ht1, 1); */
 errt0:
  /* TerminateProcess(pi.hProcess, 1);
   * too dangerous to call - assume that closing proc->in will kill process
   */
  CloseHandle(pi.hProcess);
 err0:
  SPAWN_HCLOSE(proc->in);
  SPAWN_HCLOSE(proc->o[0].ready);
  SPAWN_HCLOSE(proc->o[1].ready);
  SPAWN_HCLOSE(proc->o[0].accepted);
  SPAWN_HCLOSE(proc->o[1].accepted);
  SPAWN_HCLOSE(proc->o[0].pipe);
  SPAWN_HCLOSE(proc->o[1].pipe);
  if (--proc->flags <= 0) p_free(proc);
  return 0;
}

static DWORD WINAPI
spawn_reader(LPVOID lpvThreadParam)
{
  spawn_rd_t *chan = lpvThreadParam;
  /* read process until pipe breaks */
  while (chan->n >= 0 && chan->pipe) {
    /* ReadFile returns when
     * "a write operation completes on the write end of the pipe"
     * according to the SDK documentation of the function, independent
     * of whether the requested number of bytes have been read
     */
    if (!ReadFile(chan->pipe, chan->buf, SPAWN_MAXMSG, (DWORD*)&chan->n, 0)) {
      DWORD errcode = GetLastError();
      /* pipe related errors - first is for processes, others fifos?
       * ERROR_BROKEN_PIPE         The pipe has been ended.
       * ERROR_BAD_PIPE            The pipe state is invalid.
       * ERROR_PIPE_BUSY           All pipe instances are busy.
       * ERROR_NO_DATA             The pipe is being closed.
       * ERROR_PIPE_NOT_CONNECTED  No process is on the other end of the pipe.
       */
      if (errcode==ERROR_BROKEN_PIPE || errcode==ERROR_NO_DATA)
	chan->n = -1;
      else
	chan->n = -2;
      break;
    }
    do {
      /* this is a loop in case p_recv cannot accept all chars at once */
      if (!chan->pipe || !chan->ready || !chan->accepted) {
	chan->n = -3;
	break;
      }
      SetEvent(chan->ready);
      if (WaitForSingleObject(chan->accepted, INFINITE) != WAIT_OBJECT_0)
	chan->n = -4;
    } while (chan->n > 0);
  }
  SPAWN_HCLOSE(chan->pipe);
  if (chan->ready) SetEvent(chan->ready);
  /* slight chance following could fail due to race condition */
  if (--chan->parent->flags <= 0) p_free(chan->parent);
  return 0;
}

static void spawn_kill(p_spawn_t *proc, int nocallback);

/* ARGSUSED */
extern void
p_spawf(p_spawn_t *proc, int nocallback)
{
  if (proc) {
    spawn_kill(proc, nocallback);
    /* slight chance following could fail due to race condition */
    if (--proc->flags <= 0) p_free(proc);
  }
}

static void
spawn_kill(p_spawn_t *proc, int nocallback)
{
  if (proc) {
    if (proc->o[0].ready) w_add_input(proc->o[0].ready, 0, 0);
    if (proc->o[1].ready) w_add_input(proc->o[1].ready, 0, 0);
    SPAWN_HCLOSE(proc->in);
    if (proc->o[0].pipe && proc->o[1].pipe) {
      /* closing stdin should kill child pretty quickly, wait 1 sec */
      HANDLE handles[2];
      handles[0] = proc->o[0].ready;
      handles[1] = proc->o[1].ready;
      WaitForMultipleObjects(2, handles, TRUE, 1000);
    }
    /* note: closing proc->o[0].pipe while child blocked for stdin
     *       causes deadlock (that is, CloseHandle can block)
     * if the spawn_reader threads exit properly, these will be closed:
     * SPAWN_HCLOSE(proc->o[0].pipe);
     * SPAWN_HCLOSE(proc->o[1].pipe);
     */
    SPAWN_HCLOSE(proc->o[0].ready);
    SPAWN_HCLOSE(proc->o[1].ready);
    SPAWN_HCLOSE(proc->o[0].accepted);
    SPAWN_HCLOSE(proc->o[1].accepted);
  }
}

/* event callbacks to process stdout and stderr from process */
static void
w_callout(void *vproc)
{
  spawn_rd_t *chan = vproc;
  p_spawn_t *proc = chan->parent;
  if (!(proc->flags & 16)) {
    if (chan->n >= 0) {
      proc->flags &= ~8;   /* for p_recv */
      proc->callback(proc->ctx, 0);
    } else {
      proc->flags |= 16;
      /* on error, callback will call p_spawf with nocallback==1 */
      proc->callback(proc->ctx, 2);
    }
  }
}
static void
w_callerr(void *vproc)
{
  spawn_rd_t *chan = vproc;
  p_spawn_t *proc = chan->parent;
  if (!(proc->flags & 16)) {
    if (chan->n >= 0) {
      if ((proc->flags & 4) != 0) {
        chan->buf[chan->n] = '\0';
        chan->n = 0;
        p_stderr(chan->buf);
      } else {
        proc->flags |= 8;   /* for p_recv */
        proc->callback(proc->ctx, 1);
      }
    } else {
      proc->flags |= 16;
      /* on error, callback will call p_spawf with nocallback==1 */
      proc->callback(proc->ctx, 2);
    }
  }
}

int
p_send(p_spawn_t *proc, char *msg, long len)
{
  int result = 2;
  if (proc && proc->in) {
    result = 1;
    if (len > 0) {
      DWORD n;
      if (msg) result = !WriteFile(proc->in, msg, len, &n, 0);
    } else if (len < 0) {
      /* only implement unix process killer signals SIGINT, SIGTERM */
      if (len==-2 || len==-9)
	spawn_kill(proc, 0);
    }
  }
  return result;
}

long
p_recv(p_spawn_t *proc, char *msg, long len)
{
  if (proc && len>0) {
    int i = (proc->flags & 8) != 0;
    if (proc->o[i].n > 0) {
      long n = (len > proc->o[i].n)? proc->o[i].n : len;
      memcpy(msg, proc->o[i].buf, n);
      proc->o[i].n -= n;
      if (proc->o[i].n)
        /* len smaller than o[i].n, will loop in spawn_reader */
	memmove(proc->o[i].buf, proc->o[i].buf+n, proc->o[i].n);
      SetEvent(proc->o[i].accepted);
      return n;
    }
  }
  return -1;
}
