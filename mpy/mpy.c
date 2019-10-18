/*
 * $Id: mpy.c,v 1.9 2011-02-11 05:25:42 dhmunro Exp $
 * Provide message passing to Yorick via MPI calls.
 */
/* Copyright (c) 2010, David H. Munro
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include <mpi.h>
#define MPY_MPI_H 1
#include "mpy.h"

#include "yapi.h"
#include "pstdlib.h"
#include <string.h>

#ifdef DEBUG
#include <stdio.h>
/* p = priority, 0 for everything, 1 for less, 2 for even less, etc */
#define DBG_MSG(p,m) if (mp_dbstate>p) printf("%d: " m, mpy_rank)
#define DBG_MSG1(p,m,n) if (mp_dbstate>p) printf("%d: " m, mpy_rank,n)
#define DBG_MSG2(p,m,n1,n2) if (mp_dbstate>p) printf("%d: " m, mpy_rank,n1,n2)
#define DBG_MSG3(p,m,n1,n2,n3) if (mp_dbstate>p) printf("%d: " m, mpy_rank,n1,n2,n3)
#else
#define DBG_MSG(p,m)
#define DBG_MSG1(p,m,n1)
#define DBG_MSG2(p,m,n1,n2)
#define DBG_MSG3(p,m,n1,n2,n3)
#endif

#ifdef USE_MYPROBE
/* work around catamount bug/feature broken MPI_Probe */
#define MPI_Probe my_mpi_Probe
extern int my_mpi_Probe(int source, int tag,
                        MPI_Comm comm, MPI_Status *status);
int
my_mpi_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status)
{
  int iflag;
  int iretval;
  do {
    iretval = MPI_Iprobe(source, tag, comm, &iflag, status);
  } while (!iflag);
  return iretval;
}
#endif

/* from yio.h */
PLUG_API void YpPush(const char *filename);      /* filename will be copied */

PLUG_API ybuiltin_t Y_mp_send, Y_mp_recv, Y_mp_probe, Y_mp_exec;
PLUG_API ybuiltin_t Y_mpy_nfan, Y_mp_dbstate;

/* The interpreter uses its own communicator to avoid conflicts with
 * any compiled parallel packages.
 */
PLUG_API MPI_Comm mpy_world;
MPI_Comm mpy_world = MPI_COMM_NULL;
int mpy_size=0, mpy_rank=0, mpy_nfan=0;
int mpy_parallel = 0;

/* mpy_request  are isend requests, mpy_request[0] is control irecv
 * mpy_rhandle  are yget_use handles for isend request buffers
 */
#ifndef MPY_NFAN_0
#define MPY_NFAN_0 16
#endif
#define MAX_REQS 64
static MPI_Request mpy_request[1+MAX_REQS];
static void *mpy_rhandle[1+MAX_REQS];
#define MAX_ERR_MSG 512
static char mperr_msg[MAX_ERR_MSG];
static int mperr_from = -1;
typedef struct mpy_queue_t mpy_queue_t;
struct mpy_queue_t {
  int from;
  void *data;  /* Array* returned by yget_use */
};
static int mpy_queue_n=0, mpy_queue_size=0;
static mpy_queue_t *mpy_queue=0;
static int mpy_from = -1;  /* records previous message source */

/* tags used to identify message datatype */
#define MPY_CHAR 0
#define MPY_SHORT 1
#define MPY_INT 2
#define MPY_LONG 3
#define MPY_FLOAT 4
#define MPY_DOUBLE 5
#define MPY_COMPLEX 6
#define MPY_STRING 7
#define MPY_CONTROL 8
static MPI_Datatype mpi_types[9]= {
  MPI_BYTE, MPI_SHORT, MPI_INT, MPI_LONG,
  MPI_FLOAT, MPI_DOUBLE, MPI_DOUBLE, MPI_BYTE, MPI_BYTE };

typedef void *(*mpy_push_t)(long *dims);
static mpy_push_t mpy_pusher[8]= {
  (mpy_push_t)ypush_c, (mpy_push_t)ypush_s, (mpy_push_t)ypush_i,
  (mpy_push_t)ypush_l, (mpy_push_t)ypush_f, (mpy_push_t)ypush_d,
  (mpy_push_t)ypush_z, (mpy_push_t)ypush_q };

static long mpy_net = 0;   /* non-control messages sent minus received */

static void mpy_take_back_signals(void);
static void mperr_fatal(const char *msg);
static int mpy_recover(const char *msg, long *after);

/* int mpy_get_next(int block) declared in mpy.h */
static int mpy_push_next(int i);
static void mpy_recv(int amsub, int argc, int from);
static void mpy_send(int argc, long *to, long nto);
/* void mpy_errquiet(void) declared in mpy.h */
static int mpy_isend_wait(int nreq, int noblock);

static char *mperr_encode(char *text, long n);
static char *mperr_decode(char *text, long *pn);
static void mperr_to_boss(char *msg);
static void mperr_to_staff(char *msg);

static void mpy_quit(void);
static void mpy_abort(void);
static int mpy_aborted = 0, mpy_batch = 0;
/* The following MPI functions are used here:
  MPI_Init
  MPI_Comm_dup
  MPI_Comm_size
  MPI_Comm_rank
  MPI_Errhandler
  MPI_Finalize
  MPI_Probe
  MPI_Iprobe
  MPI_Get_count
  MPI_Recv
  MPI_Irecv
  MPI_Isend
  MPI_Waitsome
  MPI_Testsome
  MPI_Wait
  MPI_Test
  MPI_Abort
 */

PLUG_API int yerror_flags;  /* see yinput.c */
static int y0error_flags = 0;

static int mpy_initdone = 0;
static char *mpy_batscript = 0;

void
mpy_initialize(int *pargc, char **pargv[])
{
  if (!mpy_initdone) {
    int i;
    for (i=0 ; i<=MAX_REQS ; i++) {
      mpy_request[i] = MPI_REQUEST_NULL;
      mpy_rhandle[i] = 0;
    }

    mpy_initdone = (MPI_Init(pargc, pargv) == MPI_SUCCESS);

    if (!mpy_initdone
        || MPI_Comm_size(MPI_COMM_WORLD, &mpy_size) != MPI_SUCCESS
        || mpy_size < 2
        || MPI_Comm_dup(MPI_COMM_WORLD, &mpy_world) != MPI_SUCCESS
        || MPI_Comm_rank(mpy_world, &mpy_rank) != MPI_SUCCESS
        || MPI_Errhandler_set(mpy_world, MPI_ERRORS_RETURN) != MPI_SUCCESS) {
      if (mpy_initdone) MPI_Finalize(); /* may be a mistake - no return? */
      mpy_world = MPI_COMM_NULL;
      mpy_size = mpy_rank = 0;
      mpy_nfan = 1;  /* allows mp_exec to work even if no MPI */
      mpy_take_back_signals();
      /* y_error("MPI initialization sequence failed"); */
      /* at least some MPI platforms arrive here when started in serial */
      mpy_initdone = 1;
      return;
    }

    mpy_nfan = MPY_NFAN_0;
    mpy_take_back_signals();
    ycall_on_quit(&mpy_quit);

    if (pargc[0]>2 && !strcmp(pargv[0][1],"-batch"))
      mpy_batscript = pargv[0][2];

    y_errhook = mpy_recover;
    if (mpy_rank) yerror_flags |= 1;  /* mpy_recover can override this */
  }
}

/* Y_mpy_nfan must be called from mpy.i to complete initialization */
void
Y_mpy_nfan(int argc)
{
  int nfan = argc? ygets_i(argc-1) : 0;
  /* nfan < 0       --> call mpy_quit to do MPI_Finalize
   * nfan = 0 or 1  --> set mp_rank, mp_size global variables only
   * nfan > 1       --> MPI_Init sequence if not already initialized
   *                    otherwise just set abort fanout
   *  for nfan>=0, return value is abort fanout
   */

  if (nfan < 0) {
    /* mpy_init also serves as finalize from interpreted code */
    if (mpy_initdone) mpy_quit();
    return;
  }

  if (nfan > MAX_REQS)
    y_errorn("abort fanout can be at most %d", MAX_REQS);

  if (nfan > 1) mpy_nfan = nfan;

  /* set mp_size, mp_rank global variables */
  ypush_check(1);  /* may be called outside any interpreted function */
  ypush_long(mpy_rank);
  yput_global(yget_global("mp_rank", 0), 0);
  yarg_drop(1);
  ypush_long(mpy_size);
  yput_global(yget_global("mp_size", 0), 0);
  yarg_drop(1);

  ypush_long(mpy_nfan);  /* also return value */
  yput_global(yget_global("mp_nfan", 0), 0);
}

static void
mpy_abort(void)
{
  mpy_aborted = 1;
  MPI_Abort(MPI_COMM_WORLD, 1);
  y_error("called MPI_Abort");   /* unlikely to reach here */
}

extern int yBatchMode;

static int mpy_quit_count = 0;
static void
mpy_quit(void)
{
  if (mpy_aborted) return;
  if (mpy_quit_count++) mpy_abort();
  if (!mpy_rank && !mpy_parallel) {
    /* broadcast quit command */
    long cdims[2];
    char *c;
    cdims[0] = 1;  cdims[1] = 7;
    c = ypush_c(cdims);  /* VM is dead, but stack still usable */
    mpy_batch = yBatchMode;
    c[0] = mpy_batch? '\t' : ' ';  c[1] = '\0';
    c[2]='q'; c[3]='u'; c[4]='i'; c[5]='t'; c[6]='\0';
    mpy_bcast(1);  /* send quit command to all other ranks */
    mpy_bcast(0);  /* handle their synchronization */
  }
  /* presume that all processes are quitting together */
  MPI_Finalize();
}

static void
mpy_take_back_signals(void)
{
  /* try to take back important signals from MPI */
  extern void p_handler(void (*on_exception)(int signal, char *errmsg));
  extern void y_on_exception(int signal, char *errmsg);
  p_handler(y_on_exception);
}

static int mperr_quiet = 0;
void
mpy_errquiet(void)
{
  mperr_quiet = 1;
  y_errquiet();
}

static int mp_dbstate = 0;
void
Y_mp_dbstate(int argc)
{
  int n = argc>0? yarg_nil(0) : 1;
  int i = n? 0 : ygets_i(0);
  ypush_int(mp_dbstate);
  if (!n) mp_dbstate = i;
}

int
mpy_get_next(int block)
{
  MPI_Status status;
  int/*sic*/ n;
  int type, ready, source;
  long dims[2];
  void *buf;
  /* block=0  do not block
   * block=1  block if no messages pending
   * block=2  block for new message even if queue not empty
   * block=3  called during error recovery,
   *   block until next control msg, discarding all non-control messages
   * block=4  called during error recovery, like 3 but do not block
   *
   * always iprobe and move any newly arrived messages into queue
   */
  mperr_quiet = 0;
  if (block > 2) {
    while (mpy_queue_n) {
      mpy_push_next(0);
      yarg_drop(1);
    }
  }
  for (;;) {  /* loop until iprobe returns not ready, or control msg */
    if (MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpy_world, &ready, &status)
        != MPI_SUCCESS)
      mperr_fatal("MPI_Iprobe failed in mpy_get_next");
    if (!ready) {
      if (!(block&3) || (mpy_queue_n && block<2)) return 0;
      if (MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpy_world, &status)
          != MPI_SUCCESS)
        mperr_fatal("MPI_Probe failed in mpy_get_next");
    }
    type = status.MPI_TAG;
    source = status.MPI_SOURCE;
    if (type == MPY_CONTROL) {
      if (MPI_Recv(mperr_msg, MAX_ERR_MSG, MPI_BYTE, source, type,
                   mpy_world, &status) != MPI_SUCCESS)
        mperr_fatal("MPI_Recv failed in mpy_get_next");
      mperr_from = source;
      DBG_MSG1(0, "recv control msg from %d\n", source);
      if (block < 3) {
        mpy_errquiet();
      }
      return 1;
    }
    if (MPI_Get_count(&status, mpi_types[type], &n)
        != MPI_SUCCESS)
      mperr_fatal("MPI_Get_count failed in mpy_get_next");
    dims[1] = (type==MPY_COMPLEX)? n/2 : n;
    dims[0] = (dims[1]>1 && type!=MPY_STRING)? 1 : 0;

    ypush_check(1);  /* may be called outside any interpreted function */
    buf = mpy_pusher[type](dims);
    if (type == MPY_STRING) buf = ((char **)buf)[0] = p_malloc((long)n);
    if (block <= 2) {   /* else do not queue during error recovery */
      if (mpy_queue_n >= mpy_queue_size) {
        int i;
        mpy_queue = p_realloc(mpy_queue,
                              (mpy_queue_size+=16)*sizeof(mpy_queue_t));
        for (i=mpy_queue_n ; i<mpy_queue_size ; i++) {
          mpy_queue[i].from = -1;
          mpy_queue[i].data = 0;
        }
      }
      mpy_queue[mpy_queue_n].from = source;
      mpy_queue[mpy_queue_n++].data = yget_use(0);
      buf = ygeta_any(0, (long*)0, (long*)0, (int*)0);
      if (type == MPY_STRING) buf = ((char **)buf)[0] = p_malloc((long)n);
      block = 0;
    }

    DBG_MSG3(1, "recv msg from %d, type %d count %d\n", source,
             type, n);
    if (MPI_Recv(buf, n, mpi_types[type], source, type,
                 mpy_world, &status) != MPI_SUCCESS)
      mperr_fatal("MPI_Recv failed in mpy_get_next");
    mpy_net--;
    yarg_drop(1);
  }
}

static int
mpy_push_next(int i)
{
  int from;
  if (i>=mpy_queue_n || i<0) return -1;
  from = mpy_queue[i].from;
  mpy_queue[i].from = -1;
  if (mpy_queue[i].data) {
    ypush_check(1);  /* may be called outside any interpreted function */
    ypush_use(mpy_queue[i].data);
    mpy_queue[i].data = 0;
  }
  for (; i<mpy_queue_n-1 ; i++) {
    mpy_queue[i].from = mpy_queue[i+1].from;
    mpy_queue[i].data = mpy_queue[i+1].data;
  }
  mpy_queue[i].from = -1;
  mpy_queue[i].data = 0;
  mpy_queue_n--;
  return from;
}

static int mp_exec_init = 0;
static void mpy_exec0(int on);

void
Y_mp_exec(int argc)
{
  if (yarg_subroutine()) {
    extern void Y_include(int);
    if (argc) {
      int type = yarg_typeid(0);
      if (mpy_rank) y_error("mp_exec called on non-0 rank");
      if (mpy_parallel) y_error("mp_exec called in parallel mode on rank 0");
      if (argc != 1) y_error("mp_exec takes exactly one argument");
      if (type == Y_STRING) { /* do strchar convert to char */
        long i, nq, cdims[2];
        char *c, *cc, **q = ygeta_q(0, &nq, (long*)0);
        cdims[0] = 1;
        cdims[1] = 2;
        for (i=0 ; i<nq ; i++) cdims[1] += 1 + (q[i]? strlen(q[i]) : 0);
        c = ypush_c(cdims);
        /* prepend a blank line to mark batch mode (tab) or not (space) */
        mpy_batch = yBatchMode;
        *c++ = mpy_batch? '\t' : ' ';
        *c++ = '\0';
        for (i=0 ; i<nq ; i++) {
          cc = q[i];
          if (cc) while (cc[0]) *c++ = *cc++;
          *c++ = '\0';
        }
        yarg_swap(0, 1);
        yarg_drop(1);
      } else if (type != Y_CHAR) {
        y_error("mp_exec needs string or char command argument on rank 0");
      }
    } else {
      mpy_parallel = 0;
      if (!mpy_rank) {
        if (mp_exec_init) y_error("mp_exec needs one argument");
        mp_exec_init = 1;
        /* push batch script if any, will run in serial mode */
        mpy_exec0(0);
        if (mpy_batscript) YpPush(mpy_batscript);
        else YpPush("custommp.i");
        return;
      }
    }
    if (mpy_size > 1)
      mpy_bcast(1);  /* make sure everybody has char command on stack */ 
    /* invoke interpreted include,char_array */
    if (mpy_rank) {
      char *cmsg = (yarg_typeid(0)==Y_CHAR)? ygeta_c(0,0,0) : 0;
      mpy_batch = cmsg? (cmsg[0]=='\t') : 0;
      mpy_parallel = 1;
      Y_include(1);
      /* synchronize after parse, before execution (see next comment) */
      mpy_bcast(0);
    } else {
      ypush_long(1);
      mpy_exec0(1);
      /* synchronize before parse, before execution
       * - without this, syntax errors in input do bad things
       * - cannot wait until after parse, because execution is
       *   immediate inside Y_include on rank 0, and it will be
       *   a parallel task with other message passing
       */
      if (mpy_size > 1) mpy_bcast(0);
      Y_include(2);
      mpy_exec0(0);
    }

  } else {
    ypush_int(!mpy_parallel && !mpy_rank);
  }
}

static long mpy_after0 = -1, mpy_after1 = -1;
static long mpy_after2 = -1, mpy_after3 = -1;
void
mpy_exec0(int on)
{
  if (mpy_after0 < 0) {
    mpy_after0 = yget_global("after_error", 0);
    mpy_after1 = yget_global("mpy_after_error", 0);
    mpy_after2 = yget_global("mpy_on_fault", 0);
    mpy_after3 = yget_global("mpy_frank", 0);
  }
  mpy_parallel = on;
  if (mpy_size > 1) {
    if (on) {  /* after_error = mpy_after_error */
      y0error_flags = yerror_flags;
      yerror_flags |= 1;
      ypush_global(mpy_after1);
      yput_global(mpy_after0, 0);
      yarg_drop(1);
    } else {   /* after_error = [] */
      yerror_flags = y0error_flags;
      ypush_nil();
      yput_global(mpy_after0, 0);
      yarg_drop(1);
    }
  }
}

void
Y_mp_probe(int argc)
{
  int block = (argc==1 && !yarg_nil(0))? ygets_l(0) : 0;
  if (argc > 1) y_error("mp_probe takes exactly one argument");
  if (block < 0) {  /* undocumented feature? */
    ypush_long((long)mpy_from);
  } else {
    if (block > 2) y_error("mp_probe block argument must be 0, 1, or 2");
    mpy_get_next(block);
    if (mpy_queue_n) {
      int i;
      long *from, dims[2];
      dims[0] = 1;  dims[1] = mpy_queue_n;
      from = ypush_l(dims);
      for (i=0 ; i<mpy_queue_n ; i++) from[i] = mpy_queue[i].from;
    } else {
      ypush_nil();
    }
  }
}

void
Y_mp_recv(int argc)
{
  int from = argc? ygets_i(argc-1) : -1;
  if (from<0 || from>=mpy_size)
    y_errorn("mp_recv no such rank as %ld", (long)from);
  mpy_recv(yarg_subroutine(), argc-1, from);
}

void
mpy_recv(int amsub, int argc, int from)
{
  long dims[Y_DIMSIZE], *dpart, dimsdims[Y_DIMSIZE], len;
  long dest = -2, *pdims;
  int i, type;

  dims[0] = 0;
  if (!amsub) {
    /* build (leading) dimension list from arguments, if any */
    while (argc-- > 0) {
      if (yarg_nil(argc)) continue;
      dpart = ygeta_l(argc, (long *)0, dimsdims);
      if (dimsdims[0]==0 && dims[0]+1<Y_DIMSIZE) {
	dims[++dims[0]] = dpart[0];
      } else if (dimsdims[0]==1 && dpart[0]>=0 && dpart[0]<dimsdims[1]
		 && dims[0]+dpart[0]<Y_DIMSIZE) {
	for (i=1 ; i<=dpart[0] ; i++) dims[++dims[0]] = dpart[i];
      } else {
	dims[0] = Y_DIMSIZE;
	break;
      }
    }
    if (dims[0] >= Y_DIMSIZE) y_error("illegal dims list to mp_recv");
  }

  for (;;) {
    if (argc > 0) {
      dims[0] = 0;
      dest = yget_ref(--argc);
      if (dest < 0) {
        if (argc) {
          if (!yarg_nil(argc)) {
            pdims = ygeta_l(argc, (long *)0, dims);
            if (!dims[0]) dims[0] = dims[1] = 1;
            if (dims[0]==1 && dims[1]>pdims[0] && pdims[0]>=0
                && pdims[0]<Y_DIMSIZE-1) {
              for (i=0 ; i<=pdims[0] && pdims[i]>=1 ; i++)
                dims[i] = pdims[i];
              if (i <= pdims[0]) dims[0] = -1;
            } else {
              dims[0] = -1;
            }
            if (dims[0] < 0) y_error("illegal dimlist argument to mp_recv");
          }
          dest = yget_ref(--argc);
        }
        if (dest < 0) y_error("mp_recv missing output msg argument");
      }
    }

    /* block waiting for message to arrive */
    mpy_get_next(1);
    i = 0;
    if (from >= 0) {
      for (;; i++) {
        if (i >= mpy_queue_n) mpy_get_next(2);
        if (mpy_queue[i].from == from) break;
      }
    }
    from = mpy_push_next(i);
    type = yarg_typeid(0);

    if (type == Y_STRING) {
      if (dims[0]) y_error("mp_recv dimlist illegal for string messages");

    } else {
      long count;
      ygeta_any(0, &count, dimsdims, &type);
      for (i=1,len=1 ; i<=dims[0] ; i++) len *= dims[i];
      if (count % len)
        y_error("mp_recv dimlist incommensurate with actual message");
      if (count > len) {
        if (dims[0]+1 >= Y_DIMSIZE)
          y_error("mp_recv dimlist has too many dimensions for actual message");
        dims[++dims[0]] = count/len;
      }
      if (dimsdims[0] != dims[0])
        yarg_reform(0, dims);
    }

    if (dest >= 0) {
      yput_global(dest, 0);
      yarg_drop(1);
    }
    if (argc <= 0) break;
  }

  mpy_from = from;
}

void
Y_mp_send(int argc)
{
  long nto = 0;
  long *to = ygeta_l(--argc, &nto, (long *)0);
  mpy_send(argc, to, nto);
}

/* compiled analog to mp_send
 * to argument passed instead of pushed onto stack, other arguments on stack
 *   to=0, nto=0  is special case -- send to boss (noop on rank 0)
 *   to=0, nto=-1 is special case -- send to staff (noop on leaf node)
 */
static void
mpy_send(int argc, long *to, long nto)
{
  long n=0, m, dims[Y_DIMSIZE];
  int i, ytype, multi, s, dest=0, dest0=0;
  void *arg, **p=0;
  char *empty = "";
  mperr_quiet = 0;

  /* first pass through arguments checks that all arguments are legal */
  if (argc < 1) y_error("mp_send requires at least 2 arguments");
  for (i=argc-1 ; i>=0 ; i--) {
    for (m=0 ; m<nto ; m++)
      if (to[m]<0 || to[m]>=mpy_size)
        y_error("mp_send cannot send to rank<0 or >=mp_size");
    arg = ygeta_any(i, &n, dims, &ytype);
    if (ytype<0 || ytype>Y_POINTER)
      y_error("mp_send cannot handle struct or non-array data");
    if (ytype==Y_STRING && dims[0])
      y_error("mp_send cannot handle non-scalar string data");
    multi = (ytype == Y_POINTER);
    if (multi) {
      if (n != nto) {
        if (nto > 0)
          y_error("mp_send pointer data must have same length as to-list");
        else
          y_error("(BUG) mpy_send cannot handle pointer data");
      }
      p = arg;
      for (m=0 ; m<nto ; m++) {
        ypush_check(1);  /* may be called outside any interpreted function */
        ytype = ypush_ptr((arg = p[m]), &n);
        if (arg && (ytype<0 || ytype>Y_STRING))
          y_error("mp_send cannot handle ptr to structs or pointers");
        if (ytype == Y_STRING) {
          ygeta_any(0, &n, dims, &ytype);
          if (dims[0])
            y_error("mp_send cannot handle ptr to non-scalar string data");
        }
        yarg_drop(1);
      }
    }
  }

  if (!to) {
    /* handle special cases for send to boss, staff */
    if (nto == 0) {
      nto = 1;
      if (mpy_rank == 0) return;
      dest0 = (mpy_rank-1)/mpy_nfan;
    } else if (nto == -1) {
      dest0 = mpy_rank*mpy_nfan + 1;
      nto = mpy_nfan;
      if (dest0+nto > mpy_size) {
        nto = mpy_size - dest0;
        if (nto < 1) return;
      }
    } else {
      y_error("(BUG) mpy_send unrecognized nto");
    }
  }

  /* set up the control irecv */
  mpy_control_irecv(mpy_request, 0);
  /* will never return or call mpy_errquiet until mpy_request[0] NULL */

  /* second pass does the send operations */
  s = 0;
  for (i=argc-1 ; i>=0 ; i--) {                   /* outer loop on arguments */
    arg = ygeta_any(i, &n, (long*)0, &ytype);
    if (ytype == Y_COMPLEX) n += n;
    multi = (ytype == Y_POINTER);
    if (multi) p = arg;
    if (ytype == Y_STRING) {
      arg = ((char **)arg)[0];
      if (!arg) arg = empty;
      n = strlen(arg) + 1;
    }
    dest = dest0;
    for (m=0 ; m<nto ; m++,dest++) {           /* inner loop on destinations */
      if (multi) {
        ytype = ypush_ptr((arg = p[m]), &n);
        if (ytype == Y_STRING) {
          arg = ((char **)arg)[0];
          if (!arg) arg = empty;
          n = strlen(arg) + 1;
        }
      }
      if (arg) {  /* &[] means to skip sending this message */
        /* may need to wait for a previous isend to complete */
        if (++s >= 1+MAX_REQS) s = mpy_isend_wait(s, 0);
        if (mpy_request[0] == MPI_REQUEST_NULL) {
          mpy_errquiet();
        }
        /* claim a use of current message buffer */
        mpy_rhandle[s] = yget_use(multi? 0 : i);
        if (!m && !multi && n==1) /* yget_use may have invalidated arg */
          arg = ygeta_any(i, (long*)0, (long*)0, (int*)0);
        /* initiate the isend transaction */
        if (to) dest = to[m];
        DBG_MSG3(1, "isend msg to %d, type %d count %ld\n", dest, ytype, n);
        if (MPI_Isend(arg, n, mpi_types[ytype], dest, ytype,
                      mpy_world, mpy_request+s) != MPI_SUCCESS)
          mperr_fatal("MPI_Isend failed in Y_mp_send");
        mpy_net++;
      }
      if (multi) yarg_drop(1);
    }
  }

  /* wait for all isend requests to complete */
  s++;  /* one more for control irecv */
  for (;;) {
    if (s > 1) s = mpy_isend_wait(s, 0);
    if (mpy_request[0] == MPI_REQUEST_NULL) {
      mpy_errquiet();
    }
    if (s < 2) break;
  }

  /* cancel control irecv */
  mpy_control_irecv(mpy_request, 1);
}

static int
mpy_isend_wait(int nreq, int noblock)
{
  int complete[1+MAX_REQS], ncomplete, s, ss;
  void *handle;
  if (!noblock) {
    if (MPI_Waitsome(nreq, mpy_request, &ncomplete,
                     complete, MPI_STATUSES_IGNORE) != MPI_SUCCESS)
      mperr_fatal("MPI_Waitsome failed in mpy_isend_wait");
  } else {
    if (MPI_Testsome(nreq-1, mpy_request+1, &ncomplete,
                     complete, MPI_STATUSES_IGNORE) != MPI_SUCCESS)
      mperr_fatal("MPI_Testsome failed in mpy_isend_wait");
    if (!ncomplete) return 0;
  }
  if (ncomplete==MPI_UNDEFINED || !ncomplete)
    mperr_fatal("MPI_Waitsome ncomplete failed in mpy_isend_wait");
  DBG_MSG2(2, "%s, ncomplete=%d\n", (noblock?"testsome":"waitsome"),
           ncomplete);
  for (s=0 ; s<ncomplete ; s++) {
    handle = mpy_rhandle[complete[s]+noblock];
    if (!handle) continue;
    /* discard use of isend message buffer */
    DBG_MSG3(2, "%s, discarding use %d, %p\n", (noblock?"testsome":"waitsome"),
             complete[s]+noblock, handle);
    mpy_rhandle[complete[s]+noblock] = 0;
    ypush_check(1);  /* may be called outside any interpreted function */
    ypush_use(handle);
    yarg_drop(1);
  }
  /* repack active isend requests */
  for (s=ss=1 ; s<nreq ; s++) {
    if (mpy_request[s] != MPI_REQUEST_NULL) {
      if (ss < s) {
        mpy_request[ss] = mpy_request[s];
        mpy_request[s] = MPI_REQUEST_NULL;
        mpy_rhandle[ss] = mpy_rhandle[s];
        mpy_rhandle[s] = 0;
      }
      ss++;
    }
  }
  return ss;
}

void
mpy_control_irecv(MPI_Request *request, int cancel)
{
  if (!cancel) {
    /* set up the control irecv */
    if (MPI_Irecv(mperr_msg, MAX_ERR_MSG, MPI_BYTE, MPI_ANY_SOURCE, MPY_CONTROL,
                  mpy_world, request) != MPI_SUCCESS)
      mperr_fatal("MPI_Irecv failed in mpy_control_irecv");
    mperr_from = -1;  /* status ignored for control irecv */

  } else if (request[0] != MPI_REQUEST_NULL) {
    /* do self-send in order to cancel control irecv */
    char *junk = "\x40\x40";
    int s;
    mpy_request[0] = request[0];
    if (MPI_Isend(junk, 3, MPI_BYTE, mpy_rank, MPY_CONTROL,
                  mpy_world, mpy_request+1) != MPI_SUCCESS)
      mperr_fatal("MPI_Isend to self failed in mpy_control_irecv");
    while (mpy_request[0] != MPI_REQUEST_NULL) mpy_isend_wait(2, 0);
    for (s=0 ; s<3 ; s++)
      if (mperr_msg[s] != junk[s]) {
        /* control message received was not the self-sent message
         * create a receive which can only receive that message
         */
        char yuck[16];
        if (MPI_Irecv(yuck, 16, MPI_BYTE, mpy_rank, MPY_CONTROL,
                      mpy_world, mpy_request) != MPI_SUCCESS)
          mperr_fatal("MPI_Irecv of self-send failed in mpy_control_irecv");
        while (mpy_request[0] != MPI_REQUEST_NULL
               || mpy_request[1] != MPI_REQUEST_NULL) mpy_isend_wait(2, 0);
        request[0] = MPI_REQUEST_NULL;
        mpy_errquiet();
      }
    while (mpy_request[1] != MPI_REQUEST_NULL) mpy_isend_wait(2, 0);
    mperr_msg[0] = '\0';
    request[0] = MPI_REQUEST_NULL;
  }
}

/* control message contents:
 * message structure: [id, n1, n2]
 *   id =    message id
 *   n1, n2 = optional numeric arguments
 * id    n1      sent to   description
 * -1    0       any      fatal, shut down MPI
 * 0     0       self     mp_send completing control irecv
 *   - always discarded
 * 1    rank     boss     alert boss that fault has occurred
 *       n2=error message associated with fault (on rank n1)
 *   - boss will discard if id=2 already received, else pass alert along
 * 2     rank    staff    initiate recovery on all processes
 *       n1=rank of faulting process singled out by rank 0 (fault-rank)
 *   - all processes bossward of us in hierarchy in recovery
 *   - after receiving this, wait until all staff have sent id=3
 * 3    net      boss     report net message count from self and all staff
 *      n2=count of id=1 messages generated or received
 *   - this initializes net message count on rank 0
 *   - after sending this, will receive id=4 from staff or id=5 from boss
 * 4    nmsg     boss     report additional messages received
 *   - report decrement to net message count as soon as no more pending
 * 5     1       fault-rank   request fault data [optional]
 *    - sent directly from rank 0 to fault-rank
 * 5     2       0        response to fault data request [5,1]
 *       n2=func name, relative pc list
 * 5     0       staff    recovery complete
 *   - initiated by rank 0 when net message count reaches zero
 *   - after receiving this, wait for id=6 from all staff
 * 6    fatal    boss     acknowledge recovery, returning to idle state
 *   - with fatal flag, a negative acknowledgement
 * note that after rank 0 receives id=3 or id=4, all processes stop sending
 * recovery messages until a pending non-control message is delivered,
 * which guarantees continuous progress in clearing non-control messages
 *
 * notes:
 * 1. impossible to do irecv for any but control messages, because
 *    size is unlimited
 * 2. use of isend during abort lessens chance of buffers filling
 *    because receives can continue
 * does MPI offer a way to block for either isend or probe?
 */
static int mperr_recovering = 0;

/* this is called from task.c:YError as y_errhook */
int
mpy_recover(const char *emsg, long *after)
{
  int i, s, tally[MAX_REQS], ngot;
  int staff0 = mpy_rank*mpy_nfan + 1;
  int staff1 = staff0 + mpy_nfan;
  long id, id0, net, count=-1, nid1=0, mid1=0, frank=-1;
  MPI_Status status;
  char *msg;

  if (mpy_aborted) return 1;
  if (mpy_batch) mperr_fatal(emsg);

  if (staff1 >= mpy_size) staff1 = mpy_size;

  if (!mperr_quiet) {
    /* quick return if this is fault on rank 0 in non-parallel mode */
    if (!mpy_rank && !mpy_parallel) return 0;
    /* interpreted program faulted
     * construct error message in mperr_msg, because any subsequent control
     * message which clobbers it takes precedence anyway
     */
    i = mperr_encode(mperr_encode(mperr_msg, 1), mpy_rank) - mperr_msg;
    if (emsg) for (; i<MAX_ERR_MSG-1 && emsg[0] ; i++) mperr_msg[i] = *emsg++;
    mperr_msg[MAX_ERR_MSG-1] = '\0';
    mperr_from = mpy_rank;
    mid1 = 1;
  } else {
    /* unexpected MPY_CONTROL message arrived (mpy_errquiet called) */
    mperr_quiet = 0;
  }
  if (!mpy_rank && mpy_parallel) yerror_flags = y0error_flags;

  if (mperr_recovering) mperr_fatal("recursive call to mpy_recover");
  mperr_recovering = 1;
  mpy_parallel = 0;

  if (mpy_request[0] != MPI_REQUEST_NULL)
    mperr_fatal("mpy_recover began with outstanding control irecv");
  for (s=1 ; s<=MAX_REQS ; s++) if (mpy_request[s]==MPI_REQUEST_NULL) break;
  mperr_to_boss(0);
  mperr_to_staff(0);
  for (i=0 ; i<mpy_nfan ; i++) tally[i] = 0;

  /* recv all pending messages, discarding non-control messages */
  for (id=id0=0 ;;) {
    msg = mperr_decode(mperr_decode(mperr_msg, &id), &count);
    /* only id=1 (possibly generated here) or id=2 are possible initially */
    if (id == 1) nid1++;
    else if (id == 2) frank = count;
    else mperr_fatal("impossible initial control message in mpy_recover");
    if (id0 < id) id0 = id;
    if (s > 1) s = mpy_isend_wait(s, 1);
    if (!mpy_get_next(4)) break;
  }

  if (id0 == 1) {
    /* no message 2 from boss, so send message 1 to boss */
    if (mpy_rank) {
      int block = 3;
      /* non-0 rank forwards most recently received id==1 message to boss */
      mperr_to_boss(mperr_msg);
      /* and waits until id=2 message arrives */
      for (id=0 ;;) {
        if (s > 1) s = mpy_isend_wait(s, 1);
        if (!mpy_get_next(block)) break;
        msg = mperr_decode(mperr_decode(mperr_msg, &id), &count);
        if (id == 1) nid1++;
        else if (id == 2) frank = count;
        else mperr_fatal("impossible control message (2) in mpy_recover");
        if (id == 2) block = 4;
     }
    } else {
      frank = count;
    }
  } else if (!mpy_rank) {
    mperr_fatal("bad initial control message in mpy_recover");
  }

  /* id=2 message has arrived from boss
   * -- boss will send nothing more until we send id=3 to him
   * forward id=2 message to all staff
   */
  mperr_encode(mperr_encode(mperr_msg, 2), frank);
  mperr_to_staff(mperr_msg);

  /* wait for id=3 to arrive from all staff -- id=4 may also arrive */
  net = 0;
  if (staff0 < mpy_size) {
    for (ngot=0 ; ngot<staff1-staff0 ;) {
      if (s > 1) s = mpy_isend_wait(s, 1);
      mpy_get_next(3);
      if (mperr_from<staff0 || mperr_from>=staff1)
        mperr_fatal("id=3 or id=4 message from non-staff in mpy_recover");
      msg = mperr_decode(mperr_decode(mperr_msg, &id), &count);
      /* only id=3 or id=4 are possible here */
      if (id == 3) {
        if (tally[mperr_from-staff0]++)
          mperr_fatal("got multiple id=3 messages in mpy_recover");
        net += count;
        ngot++;
        msg = mperr_decode(msg, &count);
        mid1 += count;
      } else if (id == 4) {
        if (!tally[mperr_from-staff0])
          mperr_fatal("got id=4 message before id=3 in mpy_recover");
        net -= count;
      } else {
        mperr_fatal("expecting id=3 or id=4 message in mpy_recover");
      }
    }
  }
  net += mpy_net;
  /* send id=3 to boss */
  mperr_encode(mperr_encode(mperr_encode(mperr_msg, 3), net), nid1);
  mperr_to_boss(mperr_msg);

  /* wait for id=5 from boss, emitting id=4 to boss as needed */
  count = mpy_net;
  for (;;) {
    if (!mpy_rank) {
      if (!net) {
        /* print fault summary as warning */
        y_warnn("%ld ranks report fault, parallel task halted", mid1);
        /* initiate id=5 recovery complete message */
        mperr_encode(mperr_encode(mperr_msg, 5), 0);
        mperr_to_staff(mperr_msg);
        break;
      }
      if (net < 0) mperr_fatal("outstanding count < 0 in mpy_recover");
    }
    /* block until any message arrives */
    if (MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpy_world, &status)
        != MPI_SUCCESS)
      mperr_fatal("MPI_Probe failed in mpy_recover");
    /* clear out any non-control messages */
    if (s > 1) s = mpy_isend_wait(s, 1);
    for (id=4 ; mpy_get_next(4) ;) {
      /* examine control messages */
      mperr_decode(mperr_decode(mperr_msg, &id), &id0);
      if (id == 4)
        count += id0;
      else if (id != 5)
        mperr_fatal("impossible control message (4) in mpy_recover");
    }
    if (mpy_net < count) {
      /* either mpy_net decreased or id=4 arrived from staff */
      if (id == 5)
        mperr_fatal("impossible control message (5) in mpy_recover");
      if (mpy_rank) {
        /* pass along id=4 to boss */
        mperr_encode(mperr_encode(mperr_msg, 4), count-mpy_net);
        mperr_to_boss(mperr_msg);
      }
      net -= count-mpy_net;
      count = mpy_net;
    }
    if (id == 5) {
      if (!id0) break;
      mperr_fatal("mpy_recover fault-rank protocol not implemented");
    }
  }
  if (mpy_rank) { /* pass along id=5 message to staff */
    mperr_encode(mperr_encode(mperr_msg, 5), 0);
    mperr_to_staff(mperr_msg);
  }

  if (s > 1) {
    s = mpy_isend_wait(s, 1);
    if (s > 1)
      mperr_fatal("incomplete isend after recovery in mpy_recover");
  }

  /* wait until all staff acknowledge recovery complete */
  if (staff0 < mpy_size) {
    for (ngot=0 ; ngot<staff1-staff0 ;) {
      if (s > 1) s = mpy_isend_wait(s, 1);
      mpy_get_next(3);
      if (mperr_from<staff0 || mperr_from>=staff1)
        mperr_fatal("id=6 message from non-staff in mpy_recover");
      msg = mperr_decode(mperr_decode(mperr_msg, &id), &count);
      /* only id=6 possible here */
      if (id == 6) {
        if (++tally[mperr_from-staff0] > 2)
          mperr_fatal("got multiple id=6 messages in mpy_recover");
        ngot++;
      } else {
        mperr_fatal("expecting id=6 message in mpy_recover");
      }
    }
  }
  if (mpy_rank) { /* pass along id=6 message to boss */
    mperr_encode(mperr_encode(mperr_msg, 6), 0);
    mperr_to_boss(mperr_msg);
  }

  mperr_from = -1;
  mperr_recovering = 0;
  mperr_to_boss(0);
  mperr_to_staff(0);

  if (mpy_after2<0) {
    mpy_after0 = yget_global("after_error", 0);
    mpy_after2 = yget_global("mpy_on_fault", 0);
    mpy_after3 = yget_global("mpy_frank", 0);
  }
  /* set mpy_frank for mpy_on_fault */
  ypush_long(frank);
  yput_global(mpy_after3, 0);
  yarg_drop(1);

  /* note that mpy_on_fault runs with mpy_parallel=0 on any rank */
  if (mpy_rank != frank) {
    if (mpy_rank) return 1;        /* suppress error printing */
    *after = mpy_after2;
    return 3;         /* suppress printing, call mpy_on_fault */
  } else {
    if (!mpy_rank) *after = mpy_after2;
    else *after = mpy_after0;
    return 6;        /* enable printing, call mpy_after_error */
  }
}

static void
mperr_to_boss(char *msg)
{
  static char msg_boss[MAX_ERR_MSG];
  static MPI_Request req_boss;
  if (!msg && mperr_recovering) {
    req_boss = MPI_REQUEST_NULL;
    return;
  }
  if (mpy_rank) {
    int n, boss = (mpy_rank-1)/mpy_nfan;
    /* no point in sending message to boss until previous send finished */
    if (MPI_Wait(&req_boss, MPI_STATUS_IGNORE) != MPI_SUCCESS)
      mperr_fatal("MPI_Wait for send request failed in mperr_to_boss");
    if (!mperr_recovering) return;
    for (n=0 ; n<MAX_ERR_MSG-1 && msg[n] ; n++) msg_boss[n] = msg[n];
    msg_boss[n] = '\0';
    if (MPI_Isend(msg_boss, n, MPI_BYTE, boss, MPY_CONTROL,
                  mpy_world, &req_boss) != MPI_SUCCESS)
      mperr_fatal("MPI_Isend failed in mperr_to_boss");
  }
}

static void
mperr_to_staff(char *msg)
{
  static char msg_staff[32];
  static MPI_Request req_staff[MAX_REQS];
  int staff0 = mpy_rank*mpy_nfan + 1;
  int n;
  if (!msg && mperr_recovering) {
    for (n=0 ; n<MAX_REQS ; n++) req_staff[n] = MPI_REQUEST_NULL;
    return;
  }
  if (staff0 < mpy_size) {
    int nstaff = (mpy_nfan<=mpy_size-staff0)? mpy_nfan : mpy_size-staff0;
    int len, complete = 0;
    /* only id=2, id=5 messages are sent to staff
     *   also get here after recovery complete (mperr_recovering = 0)
     * for id=2, no previous req_staff requests
     * for id=5, the previous id=2 must have all completed
     * after recovery complete, all id=5 must have completed
     * ==> simple MPI_Test on any existing isends to staff must complete
     */
    for (n=0 ; n<nstaff ; n++) {
      if (req_staff[n] == MPI_REQUEST_NULL) continue;
      if (MPI_Test(req_staff+n, &complete, MPI_STATUS_IGNORE) != MPI_SUCCESS)
        mperr_fatal("MPI_Test for send request failed in mperr_to_staff");
      if (!complete)
        mperr_fatal("MPI_Test returned incomplete send in mperr_to_staff");
    }
    if (!mperr_recovering) return;
    for (len=0 ; len<31 && msg[len] ; len++) msg_staff[len] = msg[len];
    msg_staff[len] = '\0';
    for (n=0 ; n<nstaff ; n++) {
      if (MPI_Isend(msg_staff, len, MPI_BYTE, staff0+n, MPY_CONTROL,
                    mpy_world, req_staff+n) != MPI_SUCCESS)
        mperr_fatal("MPI_Isend failed in mperr_to_staff");
    }
  }
}

/* encode integer as 8-bit text string, '\0' byte never occurs
 * 128 bit set on all but final byte
 *  64 bit set in final byte if and only if negative (or zero)
 * remaining 7 bits are data (or 6 bits of final byte), msb first
 *
 * use this scheme to pass integer values as type MPI_BYTE in control
 * messages, so they can be mixed with error message strings
 */
static char *
mperr_encode(char *text, long n)
{
  char t[2*sizeof(long)];
  int i = 0;
  if (n <= 0) {
    n = -n;
    t[i] = 0x40 | (n & 0x3f);
  } else {
    t[i] = n & 0x3f;
  }
  for (n>>=6 ; n ; n>>=7) t[++i] = n & 0x7f;
  do { *text++ = t[i--]; } while (i >= 0);
  text[0] = '\0';
  return text;
}
static char *
mperr_decode(char *text, long *pn)
{
  long n = 0;
  while (text[0] & 0x80) n = (n << 7) | (*text++ & 0x7f);
  n = (n << 6) | (text[0] & 0x3f);
  *pn = (text[0] & 0x40)? -n : n;
  return text[0]? ++text : text;   /* do not go past a 0 byte */
}

static void
mperr_fatal(const char *msg)
{
  /* do not be clever here -- just kill all processes */
  if (mpy_rank <= 16) {
    if (!mpy_batch)
      y_warnn("Rank %ld raised fatal MPI communication error:", mpy_rank);
    else
      y_warnn("Rank %ld raised error in batch mode:", mpy_rank);
    y_warn(msg);
  }
  mpy_abort();
  if (!mpy_rank)
    y_warn("MPI crashed and aborted: only rank<=16 allowed to print errors");
}

/* ------------------------------------------------------------------------ */

void
mpy_bcast(int nmsgs)
{
  int m;
  if (nmsgs > 0) {
    int boss = (mpy_rank-1)/mpy_nfan;
    if (mpy_rank) for (m=0 ; m<nmsgs ; m++) mpy_recv(0, 0, boss);
    mpy_send(nmsgs, 0, -1);
  } else {
    int staff0 = mpy_rank*mpy_nfan + 1;
    for (m=staff0 ; m<mpy_size && m<staff0+mpy_nfan ; m++) {
      mpy_recv(0, 0, m);
      yarg_drop(1);
    }
    ypush_check(1);  /* may be called outside any interpreted function */
    ypush_long(1);
    mpy_send(1, 0, 0);
    yarg_drop(1);
  }
}

/* ------------------------------------------------------------------------ */
