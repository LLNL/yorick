/*
 * $Id: mpy.c,v 1.1 2005-09-18 22:04:59 dhmunro Exp $
 * Provide message passing to Yorick via MPI calls.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include <mpi.h>

#include "ydata.h"
#include "pstdlib.h"
#include "defmem.h"

#ifdef DEBUG
#include <stdio.h>
#endif

/* The interpreter uses its own communicator to avoid conflicts with
 * any compiled parallel packages.
 */

extern BuiltIn Y_mpy_init, Y_mpy_rank, Y_mpy_size, Y_mp_send, Y_mp_recv,
               Y_mp_from, Y_mpy_sync;

extern int ym_argc;      /* set in main.c */
extern char **ym_argv;

extern void (*CleanUpForExit)(void);
static void (*moreCleanUp)(void)= 0;
static void mpy_CleanUp(void);
extern void ym_escape(void);

/* mpy_from is the communication between Y_mp_recv and Y_mp_from */
static int mpy_from= -1;

/* a message pending for mp_recv may have been probed by mp_from(1)
 * in which case a second MPI_Probe call is unnecessary
 * here is the memory for that earlier probe */
static int mpy_ready= 0;
static MPI_Status mpy_status;

static void mpy_abort(void);
static void mpy_control(void);
static int mpy_probe(int blocking);
static void mpy_decode(int *type, long *count);
static int mpy_get_type(StructDef *base);
static void *mpy_message(int origin, int remaining, int *type, long *count);
static int mpy_shuffle(int *complete, int n_complete);
static void mpy_irecv(int tripped);
static void mpy_fatal(int origin, char *msg);
static int mpy_f_origin= 0;
static char *mpy_f_msg= 0;
static void mpy_quit(void);
static int mpy_iquit= 0;

static int mpy_initialized= 0;
static int mpy_rank, mpy_size;
static MPI_Comm mpy_world= MPI_COMM_NULL;

/* MAX_SIMULTANEOUS is the maximum permitted number of pending
 * MPI_Request issend handles (not including the control irecv).  */
#ifndef MAX_SIMULTANEOUS
#ifndef DEBUG
#define MAX_SIMULTANEOUS 64
#else
#define MAX_SIMULTANEOUS 2
#endif
#endif
static int max_request= 0;
static int n_request= 0;
static MPI_Request *request= 0;
static MPI_Status *status= 0;
static int n_complete= 0;
static int *complete= 0;
static int *origin= 0;
static int *remaining= 0;
static void wkspc_alloc(int mxrq);
static void wkspc_free(void);

/* this holds the number of BEGIN messages that have been received
 * each mpy_sync generates two broadcast from rank 0, so during normal
 * operation mpy_begin is 2 */
static int mpy_begin= 0;

/* state of abort -- non-zero if abort in progress */
static int mpy_alerted= 0;
static int mpy_raised= 0;

/* functions used in abort and startup sequences */
static void mpy_send_clear(int msgtyp);
static void mpy_do_abort(int rank, char *msg);
static void mpy_sclear(char *msg);
static void mpy_wait(int where);
static void mpy_rclear(void);
static int mpy_discard(void);

/* mp_send fielded subsequent control message */
static int mpy_pending = 0;

#define MAX_CONTROL_MSG 128
static char mpy_control_msg[MAX_CONTROL_MSG];
static char mpy_msg1[1], mpy_msg2[1];

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

static StructDef *mpy_bases[8]= {
  &charStruct, &shortStruct, &intStruct, &longStruct,
  &floatStruct, &doubleStruct, &complexStruct, &stringStruct };
static MPI_Datatype mpi_types[9]= {
  MPI_BYTE, MPI_SHORT, MPI_INT, MPI_LONG,
  MPI_FLOAT, MPI_DOUBLE, MPI_DOUBLE, MPI_BYTE, MPI_BYTE };

/* control messages consist of an id byte, followed by an optional
 * error message -- here are the id bytes: */
#define MPY_RAISE   '\1'
#define MPY_ABORT_1 '\2'
#define MPY_ABORT_2 '\3'
#define MPY_READY   '\4'
#define MPY_CLEAR   '\5'
#define MPY_BEGIN   '\6'
#define MPY_QUIT    '\7'
#define MPY_FATAL   '\377'
/* RAISE - sent to rank 0 by another faulting process, with error message
 * ABORT - sent by rank 0 to all others to force abort sequence
 *   ABORT_1 halts a pending mp_send
 *   ABORT_2 sometimes required to unblock mp_recv, always sent
 * READY - sent to rank 0 by non-0 rank process when it has no pending sends
 * CLEAR - sent by rank 0 to all others after READY received from all
 * BEGIN - sent by rank 0 to all others to confirm mpy_sync
 */

/*------------------------------------------------------------------------*/

/* MPICH (at least) steals SIGINT, try to take it back */
extern void SetSignals(int flags);  /* Yorick/sysdep.c */

/* mpy_init call must be first thing in mpy.i */
void Y_mpy_init(int nArgs)
{
  if (!mpy_initialized &&
      MPI_Init(&ym_argc, &ym_argv)==MPI_SUCCESS) {
    moreCleanUp= CleanUpForExit;
    CleanUpForExit= &mpy_CleanUp;

    if (MPI_Comm_size(MPI_COMM_WORLD, &mpy_size)!=MPI_SUCCESS) {
      mpy_size= mpy_rank= 0;
      YError("MPI_Comm_size failed");
    }

    if (mpy_size > 1) {
      if (MPI_Comm_dup(MPI_COMM_WORLD, &mpy_world)!=MPI_SUCCESS ||
          MPI_Comm_rank(mpy_world, &mpy_rank)!=MPI_SUCCESS ||
          MPI_Errhandler_set(mpy_world, MPI_ERRORS_RETURN)!=MPI_SUCCESS) {
        CleanUpForExit= moreCleanUp;
        MPI_Finalize();  /* may be a mistake - might never return */
        mpy_world= MPI_COMM_NULL;
        mpy_size= mpy_rank= 0;
        YError("MPI initialization sequence failed");
      }

      max_request= mpy_size-1;
      if (max_request>MAX_SIMULTANEOUS) max_request= MAX_SIMULTANEOUS;
      wkspc_alloc(max_request);

    } else {
      CleanUpForExit= moreCleanUp;
      MPI_Finalize();  /* may be a mistake - might never return */
      mpy_size= mpy_rank= 0;
    }
  }
  mpy_initialized= 1;

  /* try to take back important signals from MPI */
  {
    extern void p_handler(void (*on_exception)(int signal, char *errmsg));
    extern void y_on_exception(int signal, char *errmsg);
    p_handler(y_on_exception);
  }

  PushLongValue((long)mpy_size);
}

void Y_mpy_size(int nArgs)
{
  PushLongValue((long)mpy_size);
}

void Y_mpy_rank(int nArgs)
{
  PushLongValue((long)mpy_rank);
}

static void wkspc_alloc(int mxrq)
{
  max_request= mxrq;
  request= p_malloc(sizeof(MPI_Request)*(mxrq+1));
  status= p_malloc(sizeof(MPI_Status)*(mxrq+1));
  complete= p_malloc(sizeof(int)*(mxrq+1+2*mpy_size));
  origin= complete + mxrq+1;
  remaining= origin + mpy_size;
  while (mxrq>=0) request[mxrq--]= MPI_REQUEST_NULL;
}

static void wkspc_free(void)
{
  max_request= 0;
  p_free(request);
  p_free(status);
  p_free(complete);
}

static void mpy_CleanUp(void)
{
  if (moreCleanUp) moreCleanUp();
  if (mpy_size) {
#ifdef DEBUG
    printf("(%d) doing mpy_CleanUp\n", (int)mpy_rank);
    fflush(stdout);
#endif
    mpy_iquit= 1;
    if (mpy_f_msg) mpy_fatal(mpy_f_origin, &mpy_f_msg[1]);
    mpy_quit();
  }
}

/*------------------------------------------------------------------------*/

void Y_mp_recv(int nArgs)
{
  Array *result;
  void *buf;
  long number, count;
  int type;
  extern void BuildDimList(Symbol *stack, int nArgs);  /* ops3.c */

  if (mpy_begin!=2) YError("mp_recv before mpy_sync (mp_start)");

  /* handle any dimlist, returning result (or 0) in tmpDims */
  if (nArgs>0) {
    BuildDimList(sp-nArgs+1, nArgs);
  } else {
    Dimension *tmp= tmpDims;
    tmpDims= 0;
    FreeDimension(tmp);
  }

  mpy_from= -1;

  /* check for control message */
  mpy_control();

  /* block waiting for message to arrive */
  mpy_probe(1);
  mpy_decode(&type, &count);

  if (type==MPY_STRING && tmpDims)
    YError("mp_recv dimlist illegal for string messages");

  /* add unspecified final dimension if necessary */
  number= TotalNumber(tmpDims);
  if (type!=MPY_STRING) {
    if (count%number)
      YError("mp_recv dimlist incommensurate with actual message");
    if (count>number)
      tmpDims= NewDimension(count/number, 1L, tmpDims);
  }

  /* it's time to build the result on the stack */
  Drop(nArgs+1);
  result= PushDataBlock(NewArray(mpy_bases[type], tmpDims));
  if (type!=MPY_STRING) {
    buf= result->value.c;
  } else {
    buf= result->value.q[0]= p_malloc(count);
  }

  mpy_ready= 0;  /* MPI_Recv consumes the pending message */
  if (MPI_Recv(buf, (type==MPY_COMPLEX?2*count:count), mpi_types[type],
               mpy_status.MPI_SOURCE, mpy_status.MPI_TAG, mpy_world,
               &mpy_status)!=MPI_SUCCESS)
    mpy_fatal(mpy_rank, "MPI_Recv failed in mp_recv");
  mpy_from= mpy_status.MPI_SOURCE;
}

void Y_mp_from(int nArgs)
{
  long block;
  if (nArgs>1) YError("mp_from takes exactly one argument");
  if (mpy_begin!=2) YError("mp_from before mpy_sync (mp_start)");

  if (!mpy_pending) {
    if (nArgs>0 && YNotNil(sp) && (block=YGetInteger(sp))) {
      /* non-blocking probe for next message */
      long from= mpy_probe(block!=1)? (long)mpy_status.MPI_SOURCE : -1L;
      PushLongValue(from);
    } else {
      /* previous message */
      PushLongValue((long)mpy_from);
    }
  }

  /* take the opportunity to check for control message */
  mpy_control();
}

/* check for arrival of MPY_CONTROL message */
static void mpy_control(void)
{
  if (request[0]!=MPI_REQUEST_NULL || mpy_pending) {
    int ready = mpy_pending;
    if (!ready && MPI_Test(&request[0], &ready, &status[0])!=MPI_SUCCESS)
      mpy_fatal(mpy_rank, "MPI_Test failed in mpy_control");
    if (ready) {
      mpy_pending = 0;
#ifdef DEBUG
      printf("(%d) mpy_control completes control %d\n", (int)mpy_rank,
             (int)mpy_control_msg[0]);
#endif
      /* arrival of BEGIN control message means that rank 0 has decided
       * previous calculation was finished and begun the next
       * exit and resynchronize via mpy_sync in mp_idler in that case */
      if (status[0].MPI_SOURCE==0 && mpy_control_msg[0]==MPY_BEGIN) {
        mpy_begin++;
        YError(".SYNC.");
      }
      mpy_abort();    /* doesn't return */
    }
  }
}

static int mpy_probe(int blocking)
{
  int active=(request[0]!=MPI_REQUEST_NULL);

  if (!mpy_ready) {
    int flag;
    if (blocking) {
#ifdef DEBUG
      printf("(%d) mpy_probe about to block\n", (int)mpy_rank);
      fflush(stdout);
#endif
      flag= MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpy_world, &mpy_status);
      mpy_ready= 1;
      /* quit if control message triggered while we were waiting */
      mpy_control();
    } else {
      flag= MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpy_world,
                        &mpy_ready, &mpy_status);
    }
    if (flag!=MPI_SUCCESS)
      mpy_fatal(mpy_rank, "MPI_Probe or MPI_Iprobe failed in mpy_probe");
  }

  if (mpy_ready) {
#ifdef DEBUG
    int icount= 0;
    MPI_Get_count(&mpy_status, mpi_types[mpy_status.MPI_TAG], &icount);
    printf("(%d) mpy_probe got %d items of type %d from %d\n",
           (int)mpy_rank, icount, (int)mpy_status.MPI_TAG,
           (int)mpy_status.MPI_SOURCE);
    fflush(stdout);
#endif
    if (mpy_status.MPI_TAG<MPY_CHAR ||
        mpy_status.MPI_TAG>MPY_STRING) {
      /* if a control message arrived without tripping mpy_control,
       * we have a very serious problem */
      if (active) mpy_fatal(mpy_rank, "MPI bug detected in mpy_probe");
      /* if control irecv was not active, just ordinary abort */
      status[0]= mpy_status;
      mpy_abort();
    }
  }

  return mpy_ready;
}

static void mpy_decode(int *type, long *count)
{
  if (mpy_ready) {
    int icount;
    *type= mpy_status.MPI_TAG;

    if (MPI_Get_count(&mpy_status, mpi_types[*type], &icount)!=MPI_SUCCESS)
      mpy_fatal(mpy_rank, "MPI_Get_count failed in mpy_decode");
    *count= icount;  /* sic */
    if (*type==MPY_COMPLEX) *count/= 2;

  } else {
    *type= -1;
    *count= -1;
  }
}

/*------------------------------------------------------------------------*/

void Y_mp_send(int nArgs)
{
  long *recipient;
  Dimension *dims;
  long n_recipient, i, j, n_remaining, count;
  int n_messages= nArgs-1;
  Symbol *stack= sp-nArgs+1;
  int got_control = 0;
  MPI_Status control_status;
  int next, type, launched;
  void *buf;
  if (nArgs<2) YError("mp_send needs at least two arguments");
  if (mpy_begin!=2) YError("mp_send before mpy_sync (mp_start)");
  recipient= YGet_L(stack++, 0, &dims);
  n_recipient= TotalNumber(dims);

  /* check for control message - also after each Waitsome below */
  mpy_control();

  /* check carefully for list validity using remaining as scratch space */
  for (i=0 ; i<mpy_size ; i++) remaining[i]= 0;
  for (i=0 ; i<n_recipient ; i++) {
    if (recipient[i]<0 || recipient[i]>=mpy_size ||
        recipient[i]==mpy_rank)
      YError("mp_send to illegal destination");
    if (remaining[recipient[i]])
      YError("mp_send to lists duplicate destinations");
    remaining[recipient[i]]= -1;
  }

  /* initialize as if Waitsome has just returned all complete
   * with n_messages waiting for every recipient
   * note that request[0] is always the control recv request */
  for (i=0 ; i<n_recipient ; i++) {
    origin[i]= i;
    remaining[i]= n_messages;
    if (i<max_request) complete[i]= i+1;
  }
  n_complete= n_recipient;
  if (n_complete>max_request) n_complete= max_request;
  n_request= 1+n_complete;
  /* next properly initialized after first pass, if number of
   * recipients is larger than available number of request slots */
  next= n_recipient>=n_request? -2 : -1;

  /* send out messages in maximal batches until all are sent */
  for (n_remaining=n_recipient*n_messages ;
       n_remaining>0 ; n_remaining-=n_complete) {

    /* issue some new issends replacing those just completed */
    launched= 0;
    for (i=0 ; i<n_complete ; i++) {
      j= complete[i];   /* index in request, status (>=1) */
#ifdef DEBUG
      printf("(%d) mp_send n_remaining=%d n_complete=%d(%d)\n",
             (int)mpy_rank, (int)n_remaining,n_complete,(int)j);
      fflush(stdout);
#endif

      /* get next recipient in line for a message */
    retry:
      if (next >= n_request-1) {
        /* swap the completed request j with the next */
        count= origin[j-1];
        origin[j-1]= origin[next];
        origin[next]= count;
        count= remaining[j-1];
        remaining[j-1]= remaining[next];
        remaining[next]= count;
#ifdef DEBUG
        printf("(%d) mp_send [%d] count=%d next=%d n_recipient=%d\n",
               (int)mpy_rank, (int)n_remaining,
               (int)count, (int)next, (int)n_recipient);
        fflush(stdout);
#endif

        /* if swapped out request has no remaining messages,
         * get the last one that does, and reduce n_recipient */
        if (!count) {
          origin[next]= origin[n_recipient-1];
          remaining[next]= remaining[n_recipient-1];
          n_recipient--;
        } else {
          next++;
        }

        /* hunt for the next request */
        if (next>=n_recipient) {
          next= n_request-1;
          /* when n_recipient becomes less than n_request,
           * all remaining requests can be issued at once */
          if (next>=n_recipient) next= -1;
        }
#ifdef DEBUG
        printf("(%d) mp_send count=%d next=%d n_recipient=%d\n",
               (int)mpy_rank, (int)count, (int)next, (int)n_recipient);
        fflush(stdout);
#endif
      }

      if (remaining[j-1]) {
        remaining[j-1]--;
        /* get message from stack */
        buf= mpy_message(origin[j-1], remaining[j-1], &type, &count);
        if (buf) {
#ifdef DEBUG
          printf("(%d) mp_send(type %d[%ld]) to %d\n",
                 (int)mpy_rank, (int)type, (long)count,
                 (int)recipient[origin[j-1]]);
          fflush(stdout);
#endif
          if (MPI_Issend(buf, count, mpi_types[type],
                         (int)recipient[origin[j-1]], type,
                         mpy_world, &request[j])!=MPI_SUCCESS)
            mpy_fatal(mpy_rank, "MPI_Issend failed in mp_send");
          launched++;
        } else {
          /* possibility of nil messages complicates things a lot */
#ifdef DEBUG
          printf("(%d) mp_send <nil message> to %d, n_remaining=%d\n",
                 (int)mpy_rank, (int)recipient[origin[j-1]],
                 (int)n_remaining);
          fflush(stdout);
#endif
          n_remaining--;
          if (n_remaining) goto retry;
        }
      }
    }
    if (!n_remaining) break;

    if (got_control) {
      /* got control message during previous Waitsome, time to panic */
#ifdef DEBUG
      printf("(%d) mp_send got %d from %d\n",
             (int)mpy_rank, (int)mpy_control_msg[0],
             (int)control_status.MPI_SOURCE);
      fflush(stdout);
#endif
      mpy_abort();
    }

    /* post the permanent request to listen for abort messages -
     * this only happens once after mp_sync */
    if (request[0]==MPI_REQUEST_NULL) {
#ifdef DEBUG
      printf("(%d) mp_send posts control request\n", (int)mpy_rank);
      fflush(stdout);
#endif
      mpy_irecv(0);
    }

    /* wait for at least one message to complete */
    i= MPI_Waitsome(n_request, request, &n_complete, complete,status);
    if (i!=MPI_SUCCESS)
      mpy_fatal(mpy_rank,
                i==MPI_ERR_IN_STATUS? "MPI_Issend failed in mp_send" :
                "(BUG) bad argument list to MPI_Waitsome in mp_send?");
    if (request[0]==MPI_REQUEST_NULL) {
      /* control while waiting for mp_send indicates an abort
       * however, it is possible for the issend to complete
       * simultaneously with the arrival of a subsequent control
       * message, so we should not abort yet if the mp_send
       * has sucessfully completed
       * remove the request[0] from the completion list and set flag
       */
      n_complete--;
      for (j=0 ; j<n_complete ; j++) if (complete[j] == 0) break;
      if (complete[j] == 0)
        control_status = status[j];
      else
        mpy_fatal(mpy_rank, "MPI_Waitsome lost control message in mp_send");
      for (; j<n_complete ; j++) {
        complete[j] = complete[j+1];
        status[j] = status[j+1];
      }
      got_control = 1;
    }

    /* on first pass, initialize next properly if there are more
     * recipients than request slots */
    if (next == -2) next= n_request-1;
  }

  n_complete= n_request= 0;

  /* leave flag for next interpreted call */
  mpy_pending = got_control;
  if (mpy_pending) status[0] = control_status;
}

static int mpy_get_type(StructDef *base)
{
  int type;
  for (type=0 ; type<MPY_CONTROL ; type++)
    if (mpy_bases[type] == base) break;
  if (type>=MPY_CONTROL) YError("illegal data type for mp_send message");
  return type;
}

static void *mpy_message(int origin, int remaining, int *type, long *count)
{
  /* find the message to be sent to the destination with the given
   * origin (index in recipient list), which is on the stack, given
   * that there are remaining messages to be sent to that destination
   * after this one */
  Symbol *stack= sp - remaining;
  void *buf;
  Array *array;
  DataBlock *db;
  long number;
  if (stack->ops==&referenceSym) ReplaceRef(stack);
  if (stack->ops==&dataBlockSym) {
    db= stack->value.db;
    if (db->ops==&lvalueOps) {
      LValue *lvalue= (LValue *)db;
      if (lvalue->type.base->file ||
          lvalue->type.base->dataOps==&stringOps ||
          lvalue->type.base->dataOps==&pointerOps ||
          (lvalue->strider &&
           (lvalue->strider->next ||
            lvalue->strider->stride!=lvalue->type.base->size))) {
        db= (DataBlock *)FetchLValue(lvalue, stack);
        goto gota;
      }
      *type= mpy_get_type(lvalue->type.base);
      *count= TotalNumber(lvalue->type.dims);
      buf= lvalue->address.m;
    } else if (db->ops->isArray) {
    gota:
      array= (Array *)db;
      number= TotalNumber(array->type.dims);
      if (array->ops==&pointerOps) {
        if (number==1) {
          array= Pointee(array->value.p[0]);
        } else if (number>origin) {
          array= Pointee(array->value.p[origin]);
        } else {
          YError("more recipients than pointered messages in mp_send");
        }
        if ((DataBlock *)array == &nilDB) {
          *type= 0;
          *count= 0;
          return 0;
        }
        number= TotalNumber(array->type.dims);
      }
      *type= mpy_get_type(array->type.base);
      *count= number;
      buf= &array->value.c;
      if (*type == MPY_STRING) {
        if (array->type.dims)
          YError("only scalar string messages legal for mp_send");
        buf= array->value.q[0];
        *count= strlen(buf)+1;
      }
    } else {
      goto bada;
    }
  } else if (stack->ops==&doubleScalar) {
    *type= MPY_DOUBLE;
    *count= 1;
    buf= &stack->value.d;
  } else if (stack->ops==&longScalar) {
    *type= MPY_LONG;
    *count= 1;
    buf= &stack->value.l;
  } else if (stack->ops==&intScalar) {
    *type= MPY_INT;
    *count= 1;
    buf= &stack->value.i;

  } else {
  bada:
    YError("bad argument data type to mp_send");
    buf= 0;
  }

  return buf;
}

/*------------------------------------------------------------------------*/

/* mpy_sync is called from only the following places:
 * (1) mp_start
 *     rank 0 only, no arguments, should produce synchronization
 *     signal and wait for acknowledgements
 * (2) mp_abort
 *     (any rank) after error is caught in parallel task function
 *     only called if not ".SYNC.", argument is catch message
 *     mp_abort immediately sends ".SYNC." error
 * (3) mpy_idler after catch
 *     non-0 rank only, argument is catch message
 * (4) mpy_idler normal startup
 *     non-0 rank only, no arguments
 */
void Y_mpy_sync(int nArgs)
{
  char *msg;
  if (nArgs>1) YError("mpy_sync takes zero or one argument");
  if (nArgs && YNotNil(sp) && !mpy_alerted) msg= YGetString(sp);
  else msg= 0;

  /* if mpy_alerted continue an interrupted abort
   * if we have a message, we are expected to raise an exception */
  if (mpy_alerted || msg) {
    int redo= mpy_alerted;
    mpy_do_abort(mpy_rank, msg);
    /* interpreted code expects normal return if msg present */
    if (redo) YError("(finished interrupted MPI abort)");
    return;
  }

  if (request[0]!=MPI_REQUEST_NULL || mpy_pending) {
    /* must test outstanding control irecv request before MPI_Probe */
    int ready = mpy_pending;
    if (!ready && MPI_Test(&request[0], &ready, &status[0])!=MPI_SUCCESS)
      mpy_fatal(mpy_rank, "MPI_Test 1 failed in mpy_sync");
    if (ready) {
      mpy_pending = 0;
#ifdef DEBUG
      printf("(%d) mpy_sync 1 completes control %d from %d\n", (int)mpy_rank,
             (int)mpy_control_msg[0], (int)status[0].MPI_SOURCE);
      fflush(stdout);
#endif
      if (status[0].MPI_SOURCE==0 && status[0].MPI_TAG==MPY_CONTROL &&
          mpy_control_msg[0]==MPY_BEGIN)
        mpy_begin++;
      if (mpy_rank && mpy_control_msg[0]==MPY_QUIT) mpy_quit();
    }
  }

  /* mpy_begin=
   *  0 if this is first start
   *  2 on subsequent starts for which everyone finished
   *  3 if first MPY_BEGIN already received
   *  4 if both MPY_BEGIN already received
   * cases 3 and 4 can happen in mpy_control/mpy_probe if the rank 0
   * process decided the previous calculation was finished but just
   * left this process hanging blocked to receive its next message */
  if (mpy_begin) mpy_begin-= 2;

  /* non-0 rank receive MPY_BEGIN messages */
  if (mpy_rank) {
    int ready= 0;
    if (request[0]!=MPI_REQUEST_NULL) {
      /* if mpy_begin count has already started, the control irecv must
       * already have been tripped */
      if (mpy_begin) mpy_fatal(mpy_rank, "impossible state in mpy_sync");
      /* so if control irecv is active, we expect both BEGIN messages
       * to arrive, and the second one will trip the following probe */
    }

#ifdef DEBUG
    printf("(%d) mpsync mpy_begin=%d\n", (int)mpy_rank, (int)mpy_begin);
    fflush(stdout);
#endif

    /* expect to wait here in mp_idler until rank 0 calls mpy_sync
     * via mp_start in order to begin a parallel calculation */
    while (mpy_begin<2) {
#ifdef DEBUG
      printf("(%d) mpy_sync about to block\n", (int)mpy_rank);
      fflush(stdout);
#endif
      if (MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpy_world,
                    &mpy_status)!=MPI_SUCCESS)
        mpy_fatal(mpy_rank, "MPI_Probe failed in mpy_sync");
#ifdef DEBUG
      {
        int icount= 0;
        MPI_Get_count(&mpy_status, mpi_types[mpy_status.MPI_TAG], &icount);
        printf("(%d) mpy_sync got %d items of type %d from %d\n",
               (int)mpy_rank, icount, (int)mpy_status.MPI_TAG,
               (int)mpy_status.MPI_SOURCE);
        fflush(stdout);
      }
#endif
      if (mpy_status.MPI_SOURCE!=0 || mpy_status.MPI_TAG!=MPY_CONTROL) {
#ifdef DEBUG
        printf("(%d) mpy_sync got source=%d tag=%d\n", (int)mpy_rank,
               (int)mpy_status.MPI_SOURCE, (int)mpy_status.MPI_TAG);
        fflush(stdout);
#endif
        YError("mpy_sync expecting BEGIN message from rank 0 mpy_sync");
      }
      if (request[0]!=MPI_REQUEST_NULL) {
        /* may need to complete reading the message which arrived before
         * the one that tripped the previous probe */
        if (MPI_Test(&request[0], &ready, &status[0])!=MPI_SUCCESS)
          mpy_fatal(mpy_rank, "MPI_Test failed in mpy_sync");
#ifdef DEBUG
        if (ready) {
          printf("(%d) mpy_sync completes control %d from %d\n", (int)mpy_rank,
                 (int)mpy_control_msg[0], (int)status[0].MPI_SOURCE);
          fflush(stdout);
        }
#endif
        if (!ready) mpy_fatal(mpy_rank, "MPI bug detected in mpy_sync");
        if (status[0].MPI_SOURCE==0 && status[0].MPI_TAG==MPY_CONTROL &&
            mpy_control_msg[0]==MPY_BEGIN)
          mpy_begin++;
        if (mpy_rank && mpy_control_msg[0]==MPY_QUIT) mpy_quit();
      }
      /* go ahead and receive the message found by the probe */
      if (MPI_Recv(mpy_control_msg, MAX_CONTROL_MSG, MPI_BYTE,
                   mpy_status.MPI_SOURCE, mpy_status.MPI_TAG, mpy_world,
                   &mpy_status)!=MPI_SUCCESS)
        mpy_fatal(mpy_rank, "MPI_Recv failed in mpy_sync");
#ifdef DEBUG
      printf("(%d) mpy_sync receives control %d from %d\n", (int)mpy_rank,
             (int)mpy_control_msg[0], (int)mpy_status.MPI_SOURCE);
      fflush(stdout);
#endif
      if (mpy_control_msg[0]!=MPY_BEGIN) {
        if (mpy_control_msg[0]==MPY_QUIT) mpy_quit();
        YError("mpy_sync expecting BEGIN message from rank 0 mpy_sync!!");
      }
      mpy_begin++;
    }
  }

  /* rank 0 sends MPY_BEGIN messages */
  if (!mpy_rank) {
    mpy_send_clear(MPY_BEGIN);
    mpy_control();
    mpy_send_clear(MPY_BEGIN);
    mpy_control();
    mpy_begin= 2;
  }
}

/* ARGSUSED */
static void mpy_irecv(int tripped)
{
  if (MPI_Irecv(mpy_control_msg, MAX_CONTROL_MSG, MPI_BYTE,
                MPI_ANY_SOURCE, MPY_CONTROL, mpy_world,
                &request[0])!=MPI_SUCCESS)
      mpy_fatal(mpy_rank, "MPI_Irecv failed in mpy_sync");
#ifdef DEBUG
  if (tripped) {
    printf("(%d) mpy_irecv reposts control request\n", (int)mpy_rank);
    fflush(stdout);
  }
#endif
}

static void mpy_send_clear(int msgtyp)
{
  int i, n, next;
  if (mpy_rank) return;

  mpy_msg1[0]= msgtyp;

  /* notice that n_request includes the MPY_CONTROL receive posted
   * on request[0], and this cannot be used for sending CLEAR messages */
  for (i=1 ; i<mpy_size && i<=max_request ; i++) complete[i-1]= i;
  n_request= i;
  n_complete= i-1;

  next= 1;
  n= mpy_size-1;
  while (n > 0) {
    for (i=0 ; next<mpy_size && i<n_complete ; next++, i++) {
#ifdef DEBUG
      printf("(%d) send_clear(%d) to %d\n",
             (int)mpy_rank, (int)msgtyp, (int)next);
      fflush(stdout);
#endif
      if (MPI_Issend(mpy_msg1, 1, MPI_BYTE, next, MPY_CONTROL,
                     mpy_world, &request[complete[i]])!=MPI_SUCCESS)
        mpy_fatal(mpy_rank, "MPI_Issend failed in mpy_send_clear");
    }
    /* wait for at least one message to complete (send accepted) */
    mpy_wait(0);
    n-= n_complete;
  }
}

/* mpy_abort is called when an unexpected control message is received
 * in either mpy_control or Y_mp_send
 * it tries to resync the message passing system */
static void mpy_abort(void)
{
  /* the original MPI_Irecv message has arrived unexpectedly
   * indicating an error or resync */
  char *msg= (mpy_control_msg[0]==MPY_RAISE)? &mpy_control_msg[1] : 0;
  if (mpy_control_msg[0]==MPY_FATAL)
    mpy_fatal(status[0].MPI_SOURCE, &mpy_control_msg[1]);
  else if (mpy_control_msg[0]==MPY_QUIT) mpy_quit();
  mpy_do_abort(status[0].MPI_SOURCE, msg);

  /* mpy_abort must not return, but the problem has already been
   * handled and meaningful error messages printed as warnings on
   * rank 0
   * either
   * (1) this has been called from an mpy_sync from a caught error
   *     via the mp_abort (or mp_idler?) interpreted routines
   * (2) this has been called directly on receipt of an MPY_CONTROL
   *     message via mpy_abort
   */
  YError(".SYNC.");
}

/* need counter to mark progress of abort */
static int n_remain= 0;

static void mpy_do_abort(int rank, char *msg)
{
  int i;

#ifdef DEBUG
  printf("(%d) mpy_do_abort begins abort\n", (int)mpy_rank);
  fflush(stdout);
#endif

  if (mpy_rank) {
    /* non-0 rank can't send READY until all pending sends started
     * and two RAISE messages sent to rank 0 */
    mpy_raised= 0;

  } else {
    /* rank 0 needs to notify everybody else, so he will launch
     * a bunch of sends in addition to any that may be pending */
    if (mpy_alerted) {
      YWarning("continuing interrupted MPI abort sequence...");
    } else {
      /* origin will be used to keep track of whose alert issend is
       * pending, 0 meaning nobody
       * remaining will be used to keep tally of who has received
       * the abort alert */
      for (i=0 ; i<=max_request ; i++) origin[i]= 0;
      remaining[0]= 1;
      for (i=1 ; i<mpy_size ; i++) remaining[i]= 0;
      n_remain= mpy_size-1;
      mpy_alerted= 1;
      mpy_raised= 0;
      if (msg) {
        extern int sprintf(char *s, const char *format, ...);
        char mess[48];
        sprintf(mess, "Rank %d process raised error:", rank);
        YWarning(mess);
        YWarning(msg);
      }
      YWarning("initiating MPI abort sequence...");
    }
  }

  /* take care of completing any pending send operations */
  mpy_sclear(msg);

  if (!mpy_rank)
    YWarning("waiting for READY signal from all processes...");

  /* receive and discard any incoming messages until MPY_CLEAR */
  mpy_raised= 0;
  mpy_rclear();

  mpy_alerted= mpy_begin= 0;

  if (!mpy_rank) {
    YWarning("all processes signal READY...");
    mpy_send_clear(MPY_CLEAR);
    YWarning("all processes accept CLEAR, abort successful...");
  }
}

static void mpy_sclear(char *msg)
{
  int i, j;
  int n_extra= 0;
  char mess[MAX_CONTROL_MSG];
  long lmess= 1;
  mess[0]= MPY_RAISE;
  if (msg) {
    strcpy(&mess[1], msg);
    lmess+= strlen(msg);
  }

  /* receive and discard any messages that may have arrived */
  if (mpy_discard()) mpy_fatal(mpy_rank, "premature CLEAR in mpy_sclear");

  if (!mpy_rank) {
    /* if we are rank 0, notify everybody else of abort
     * use all free request slots for this purpose */
    mpy_msg1[0]= MPY_ABORT_1;
    mpy_msg2[0]= MPY_ABORT_2;
    for (i=1 ; i<=max_request ; i++) {
      if (request[i]!=MPI_REQUEST_NULL) {
        n_extra++;
        continue;
      }
      if (mpy_alerted>=mpy_size) {
        if (i<n_request) continue;
        else break;
      }
#ifdef DEBUG
      printf("(%d) sclear(%d A) to %d\n",
             (int)mpy_rank, (int)MPY_ABORT_1, (int)mpy_alerted);
      fflush(stdout);
#endif
      if (MPI_Issend(mpy_msg1, 1, MPI_BYTE, mpy_alerted, MPY_CONTROL,
                     mpy_world, &request[i])!=MPI_SUCCESS)
        mpy_fatal(mpy_rank, "MPI_Issend failed in mpy_sclear");
      origin[i]= mpy_alerted++;
    }
    n_request= i;

  } else {
    /* if we are non-0 rank, raise the exception for rank 0 */
    n_remain= 0;
    for (i=1 ; i<=max_request ; i++) {
      if (request[i]==MPI_REQUEST_NULL) {
#ifdef DEBUG
        printf("(%d) sclear(%d A)\n", (int)mpy_rank, (int)MPY_RAISE);
        fflush(stdout);
#endif
        if (MPI_Issend(mess, lmess, MPI_BYTE, 0, MPY_CONTROL,
                       mpy_world, &request[i])!=MPI_SUCCESS)
          mpy_fatal(mpy_rank, "MPI_Issend failed in mpy_sclear (R1)");
        mpy_raised++;
        n_remain++;
        break;
      } else {
        n_remain++;
#ifdef DEBUG
        printf("(%d) sclear wait for req#%d (max=%d)\n",
               (int)mpy_rank, i, max_request);
        fflush(stdout);
#endif
      }
    }
    if (i+1>n_request) n_request= i+1;
  }

  while (n_remain+n_extra>0) {
#ifdef DEBUG
    printf("(%d) mpy_sclear n_remain=%d n_extra=%d n_request=%d\n",
           (int)mpy_rank, n_remain, n_extra, n_request);
    fflush(stdout);
#endif
    /* wait for at least one message to complete */
    mpy_wait(1);

    if (mpy_rank) {
      /* non-0 rank just decrements pending send counter */
      n_remain-= n_complete;
      /* send second MPY_RAISE signal to rank 0 if necessary -
       * ok to just block on this one, since first was accepted */
      if (mpy_raised < 2) {
        i= complete[0];
#ifdef DEBUG
        printf("(%d) sclear(%d B)\n", (int)mpy_rank, (int)MPY_RAISE);
        fflush(stdout);
#endif
        if (MPI_Issend(mess, lmess, MPI_BYTE, 0, MPY_CONTROL,
                       mpy_world, &request[i])!=MPI_SUCCESS)
          mpy_fatal(mpy_rank, "MPI_Issend failed in mpy_sclear (R2)");
        mpy_raised++;
        n_remain++;
      }

    } else {
      /* 0 rank must continue disbursing abort notices */
      for (i=0 ; i<n_complete ; i++) {
        j= complete[i];
        if (origin[j]) {
          /* another abort notice has been received
           * we can expect the second notice to be picked up relatively
           * quickly, so just send it and wait rather than using the
           * non-blocking protocol */
          remaining[j]|= 1;
          n_remain--;
#ifdef DEBUG
          printf("(%d) sclear(%d) to %d\n",
                 (int)mpy_rank, (int)MPY_ABORT_2, (int)origin[j]);
          fflush(stdout);
#endif
          MPI_Ssend(mpy_msg2, 1, MPI_BYTE, origin[j], MPY_CONTROL, mpy_world);
        } else {
          n_extra--;
        }
        if (mpy_alerted < mpy_size) {
          /* fill any free slot with next abort notice if any remain */
#ifdef DEBUG
          printf("(%d) sclear(%d B) to %d\n",
                 (int)mpy_rank, (int)MPY_ABORT_1, (int)mpy_alerted);
          fflush(stdout);
#endif
          if (MPI_Issend(mpy_msg1, 1, MPI_BYTE, mpy_alerted, MPY_CONTROL,
                         mpy_world, &request[j])!=MPI_SUCCESS)
            mpy_fatal(mpy_rank, "MPI_Issend failed in mpy_sclear");
          origin[j]= mpy_alerted++;
        }
      }
    }

    /* receive and discard any messages that may have arrived */
    if (mpy_discard()) mpy_fatal(mpy_rank, "premature CLEAR in mpy_sclear");
#ifdef DEBUG
    for (i=j=0 ; i<=max_request ; i++) {
      if (request[i]!=MPI_REQUEST_NULL) {
        printf("(%d) sclear outstanding req #%d\n",
               (int)mpy_rank, i);
        fflush(stdout);
        j++;
      }
    }
    if (j && n_remain+n_extra<=0) {
      printf("(%d) sclear exiting with %d outstanding\n", (int)mpy_rank, j);
      fflush(stdout);
    }
#endif
  }

  if (mpy_rank) {
    mpy_msg1[0]= MPY_READY;
#ifdef DEBUG
    printf("(%d) sclear(%d)\n", (int)mpy_rank, (int)MPY_READY);
    fflush(stdout);
#endif
    MPI_Ssend(mpy_msg1, 1, MPI_BYTE, 0, MPY_CONTROL, mpy_world);
  } else {
    YWarning("all MPI tasks have received abort notice...");
  }
}

static void mpy_wait(int where)
{
  int i;
  do {
    /* launch another control irecv if current one finished */
    if (request[0]==MPI_REQUEST_NULL) mpy_irecv(1);
    i= MPI_Waitsome(n_request, request, &n_complete, complete,status);
    if (i!=MPI_SUCCESS)
      mpy_fatal(mpy_rank,
                i==MPI_ERR_IN_STATUS? "MPI_Issend failed in mpy_wait" :
                "(BUG) bad argument list to MPI_Waitsome in mpy_wait?");
    if (request[0]==MPI_REQUEST_NULL) {
#ifdef DEBUG
      printf("(%d) %s got %d from %d (+ %d more)\n",
             (int)mpy_rank, where?"mpy_sclear":"mpy_send_clear",
             (int)mpy_control_msg[0],(int)status[0].MPI_SOURCE,n_complete-1);
      fflush(stdout);
#endif
      if (mpy_control_msg[0]==MPY_FATAL) {
        mpy_fatal(status[0].MPI_SOURCE, &mpy_control_msg[1]);
      } else if (mpy_control_msg[0]==MPY_READY && !mpy_rank) {
        if (where) remaining[status[0].MPI_SOURCE]|= 2;
      } else if (mpy_rank && mpy_control_msg[0]==MPY_QUIT) {
        mpy_quit();
      }
      mpy_shuffle(complete, n_complete);
      n_complete--;
    }
    /* completion of only the control irecv doesn't count */
  } while (!n_complete);
}

/* move control irecv to end of completion list if present */
static int mpy_shuffle(int *complete, int n_complete)
{
  int i, shuffle= 0;
  for (i=0 ; i<n_complete ; i++) {
    if (complete[i]==0) {
      if (i<n_complete-1) {
        /* here is the shuffle */
        do {
          complete[i]= complete[i+1];
          i++;
        } while (i<n_complete-1);
        complete[i]= 0;
      }
      if (mpy_control_msg[0]==MPY_FATAL)
        mpy_fatal(status[0].MPI_SOURCE, &mpy_control_msg[1]);
      shuffle= 1;
    }
  }
  return shuffle;
}

static void mpy_rclear(void)
{
  int i, n_ready= 0;
  int clear= 0;

  if (request[0]!=MPI_REQUEST_NULL) {
    /* try to complete outstanding control irecv request before
     * blocking for MPI_Probe --
     * since MPI_Cancel is not always implemented, fill the request
     * by sending to ourself (explicitly allowed by MPI standard) */
    int j;
#ifdef DEBUG
    printf("(%d) mpy_rclear posts self-send\n",
           (int)mpy_rank, (int)MPY_ABORT_1, (int)mpy_alerted);
    fflush(stdout);
#endif
    mpy_msg1[0]= MPY_READY;
    if (MPI_Issend(mpy_msg1, 1, MPI_BYTE, mpy_rank, MPY_CONTROL,
                   mpy_world, &request[1])!=MPI_SUCCESS)
      mpy_fatal(mpy_rank, "MPI_Issend failed in mpy_rclear");
    do {
      j= MPI_Waitsome(2, request, &n_complete, complete,status);
      if (j!=MPI_SUCCESS)
        mpy_fatal(mpy_rank,
                  j==MPI_ERR_IN_STATUS? "MPI_Issend failed in mpy_rclear" :
                  "(BUG) bad argument list to MPI_Waitsome in mpy_rclear?");
      if (request[0]==MPI_REQUEST_NULL) {
#ifdef DEBUG
        printf("(%d) mpy_rclear got %d from %d\n",
               (int)mpy_rank, (int)mpy_control_msg[0],
               (int)status[0].MPI_SOURCE);
        fflush(stdout);
#endif
        if (status[0].MPI_SOURCE!=mpy_rank) {
          /* self-send not yet received */
          if (mpy_control_msg[0]==MPY_FATAL)
            mpy_fatal(status[0].MPI_SOURCE, &mpy_control_msg[1]);
          else if (mpy_control_msg[0]==MPY_QUIT)
            mpy_quit();
          else if ((mpy_control_msg[0]==MPY_CLEAR) &&
                   mpy_status.MPI_SOURCE==0 && mpy_rank)
            clear= 1;
          mpy_irecv(1);
        }
      }
    } while (request[0]!=MPI_REQUEST_NULL);
  }

  /* loop discarding received messages after all sends complete
   * rank 0 must check for arrival of READY messages */
  for (;;) {
    if (!mpy_rank) {
      for (i=1 ; i<mpy_size ; i++) if (remaining[i]&2) n_ready++;
      if (n_ready>=mpy_size-1) break;
    } else if (clear) {
      /* clear message may have arrived while waiting for self-send */
      if (clear) break;
    }

#ifdef DEBUG
    printf("(%d) mpy_rclear about to block\n", (int)mpy_rank);
    fflush(stdout);
#endif
    /* note that the control irecv is #not# active */
    if (MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpy_world,
                  &mpy_status)!=MPI_SUCCESS)
      mpy_fatal(mpy_rank, "MPI_Probe failed in mpy_rclear");
    if (mpy_discard()) break;
  }
}

static int mpy_discard(void)
{
  int clear= 0;
  void *buf;
  int type;
  long count;

  for (;;) {
    if (!mpy_ready) {
      if (MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpy_world,
                     &mpy_ready, &mpy_status)!=MPI_SUCCESS)
        mpy_fatal(mpy_rank, "MPI_Iprobe failed in mpy_discard");
      if (!mpy_ready) break;
    }

#ifdef DEBUG
    {
      int icount= 0;
      MPI_Get_count(&mpy_status, mpi_types[mpy_status.MPI_TAG], &icount);
      printf("(%d) mpy_discard got %d items of type %d from %d\n",
             (int)mpy_rank, icount, (int)mpy_status.MPI_TAG,
             (int)mpy_status.MPI_SOURCE);
      fflush(stdout);
    }
#endif

    /* create a buffer on stack to hold non-control message */
    mpy_decode(&type, &count);
    if (type==MPY_CONTROL) {
      buf= mpy_control_msg;
#ifdef DEBUG
      mpy_control_msg[0]= 23;  /* bogus value to check MPI_Recv */
#endif
    } else {
      Array *result;
      Dimension *tmp= tmpDims;
      tmpDims= 0;
      FreeDimension(tmp);
      tmpDims= NewDimension(count, 1L, tmpDims);
      CheckStack(1);
      result= PushDataBlock(NewArray(mpy_bases[type], tmpDims));
      if (type!=MPY_STRING) buf= result->value.c;
      else buf= result->value.q[0]= p_malloc(count);
    }

    /* pick up the message iprobe says is waiting */
    mpy_ready= 0;
    if (MPI_Recv(buf, (type==MPY_COMPLEX?2*count:count), mpi_types[type],
                 mpy_status.MPI_SOURCE, mpy_status.MPI_TAG, mpy_world,
                 &mpy_status)!=MPI_SUCCESS)
      mpy_fatal(mpy_rank, "MPI_Recv failed in mpy_discard");

    /* discard the message, noting any READY or FATAL messages */
    if (type==MPY_CONTROL) {
#ifdef DEBUG
      printf("(%d) mpy_discard receives control %d from %d\n", (int)mpy_rank,
             (int)mpy_control_msg[0], (int)mpy_status.MPI_SOURCE);
#endif
      if (mpy_control_msg[0]==MPY_FATAL) {
        mpy_fatal(mpy_status.MPI_SOURCE, &mpy_control_msg[1]);
      } else if (mpy_control_msg[0]==MPY_READY && !mpy_rank) {
        remaining[mpy_status.MPI_SOURCE]|= 2;
      } else if ((mpy_control_msg[0]==MPY_CLEAR) &&
                 mpy_status.MPI_SOURCE==0 && mpy_rank) {
        clear= 1;
      } else if (mpy_rank && mpy_control_msg[0]==MPY_QUIT) {
        mpy_quit();
      }
    } else {
      Drop(1);
    }
  }

  if (request[0]!=MPI_REQUEST_NULL) {
    int ready= 0;
    /* may need to complete reading the message which arrived before
     * the one that tripped the previous probe */
    if (MPI_Test(&request[0], &ready, &status[0])!=MPI_SUCCESS)
      mpy_fatal(mpy_rank, "MPI_Test failed in mpy_discard");
    if (ready) {
#ifdef DEBUG
      printf("(%d) mpy_discard completes control %d from %d\n", (int)mpy_rank,
             (int)mpy_control_msg[0], (int)status[0].MPI_SOURCE);
#endif
      if (mpy_control_msg[0]==MPY_FATAL)
        mpy_fatal(status[0].MPI_SOURCE, &mpy_control_msg[1]);
      if (mpy_rank) {
        if (mpy_control_msg[0]==MPY_QUIT) mpy_quit();
      } else if (mpy_control_msg[0]==MPY_READY) {
        remaining[status[0].MPI_SOURCE]|= 2;
      }
    }
  }

  return clear;
}

static void mpy_fatal(int origin, char *msg)
{
  /* shut down the interpreter first, then return here */
  if (!mpy_f_msg) {
    extern void Y_quit(int nArgs);
    long nmsg= msg? strlen(msg) : 0;
    char *mess= p_malloc(2+nmsg);
    mess[0]= MPY_FATAL;
    if (nmsg) strcpy(&mess[1], msg);
    else mess[1]= '\0';
    mpy_f_origin= origin;
    mpy_f_msg= mess;
    if (!mpy_iquit) {
#ifdef DEBUG
      printf("(%d) mpy_fatal calling ym_escape\n", (int)mpy_rank);
      fflush(stdout);
#endif
      ym_escape();
    }
  }

  if (mpy_rank) {
    /* send fatal message to rank 0, wait for completion */
    MPI_Request req;
    long nmsg= strlen(msg)+2;
    /* send it twice, in case rank 0 is blocked in MPI_Probe */
#ifdef DEBUG
    printf("(%d) fatal(%d) %s\n", (int)mpy_rank, (int)MPY_FATAL, mpy_f_msg);
    fflush(stdout);
#endif
    MPI_Issend(mpy_f_msg, nmsg, MPI_BYTE, 0, MPY_CONTROL, mpy_world, &req);
    MPI_Ssend(mpy_f_msg, nmsg, MPI_BYTE, 0, MPY_CONTROL, mpy_world);
    p_free(mpy_f_msg);
    mpy_f_msg= 0;

  } else {
    /* on rank 0, print panic and call MPI_Abort */
    char mess[48];
    extern int sprintf(char *s, const char *format, ...);
    YWarning("**************************************************");
    YWarning("***** MPI communication failure - aaaaaaaaaa *****");
    YWarning("*****    WARNING my eye, this one's fatal!   *****");
    YWarning("*** shut 'er down Clancey, she's a pumpin' mud ***");
    YWarning("**************************************************");
    sprintf(mess, "Rank %d process faulted, sending:", mpy_f_origin);
    YWarning(mess);
    YWarning(mpy_f_msg);
    MPI_Abort(MPI_COMM_WORLD, 1);
  }
}

static void mpy_quit(void)
{
  if (!mpy_iquit) {
    extern void Y_quit(int nArgs);
#ifdef DEBUG
    printf("(%d) mpy_quit calling ym_escape()\n", (int)mpy_rank);
    fflush(stdout);
#endif
    if (!mpy_rank) YWarning("MPY_QUIT received on rank 0?");
    ym_escape();
  }

  if (mpy_rank) {
#ifdef DEBUG
    printf("(%d) mpy_quit about to block\n", (int)mpy_rank);
    fflush(stdout);
#endif
    /* grab second quit message, just assume that's what we get
     * - don't bother to put this in loop checking for MPY_QUIT */
    MPI_Recv(mpy_control_msg, MAX_CONTROL_MSG, MPI_BYTE,
             0, MPY_CONTROL, mpy_world, &mpy_status);
#ifdef DEBUG
    {
      int icount= 0;
      MPI_Get_count(&mpy_status, mpi_types[mpy_status.MPI_TAG], &icount);
      printf("(%d) mpy_quit got %d items of type %d from %d (control %d)\n",
             (int)mpy_rank, icount, (int)mpy_status.MPI_TAG,
             (int)mpy_status.MPI_SOURCE, (int)mpy_control_msg[0]);
      fflush(stdout);
    }
#endif

  } else {
    /* rank 0 sends MPY_QUIT messages */
    mpy_send_clear(MPY_QUIT);
    mpy_send_clear(MPY_QUIT);
  }

  /* MPI_Finalize may not complete if the control irecv is outstanding
   * -- use the self-send trick as in mpy_rclear */
  if (request[0]!=MPI_REQUEST_NULL) {
    /* try to complete outstanding control irecv request before
     * blocking for MPI_Probe --
     * since MPI_Cancel is not always implemented, fill the request
     * by sending to ourself (explicitly allowed by MPI standard) */
    int j;
#ifdef DEBUG
    printf("(%d) mpy_quit posts self-send\n",
           (int)mpy_rank, (int)MPY_ABORT_1, (int)mpy_alerted);
    fflush(stdout);
#endif
    mpy_msg1[0]= MPY_READY;
    if (MPI_Issend(mpy_msg1, 1, MPI_BYTE, mpy_rank, MPY_CONTROL,
                   mpy_world, &request[1])!=MPI_SUCCESS)
      mpy_fatal(mpy_rank, "MPI_Issend failed in mpy_rclear");
    do {
      j= MPI_Waitsome(2, request, &n_complete, complete,status);
      if (j!=MPI_SUCCESS)
        mpy_fatal(mpy_rank,
                  j==MPI_ERR_IN_STATUS? "MPI_Issend failed in mpy_quit" :
                  "(BUG) bad argument list to MPI_Waitsome in mpy_quit?");
      if (request[0]==MPI_REQUEST_NULL) {
#ifdef DEBUG
        printf("(%d) mpy_quit got %d from %d\n",
               (int)mpy_rank, (int)mpy_control_msg[0],
               (int)status[0].MPI_SOURCE);
        fflush(stdout);
#endif
        if (status[0].MPI_SOURCE!=mpy_rank) {
          /* self-send not yet received */
          if (mpy_control_msg[0]==MPY_FATAL)
            mpy_fatal(status[0].MPI_SOURCE, &mpy_control_msg[1]);
          mpy_irecv(1);
        }
      }
    } while (request[0]!=MPI_REQUEST_NULL);
  }

#ifdef DEBUG
  printf("(%d) about to finalize\n", (int)mpy_rank);
  fflush(stdout);
#endif
  MPI_Finalize();
#ifdef DEBUG
  printf("(%d) finalize completed\n", (int)mpy_rank);
  fflush(stdout);
#endif
  mpy_size= mpy_rank= 0;
  wkspc_free();
}

/*------------------------------------------------------------------------*/
