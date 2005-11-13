/*
 * $Id: mpy.i,v 1.2 2005-11-13 23:28:49 dhmunro Exp $
 * Message passing extensions to Yorick.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* MAKE-INSTRUCTIONS
SRCS = mpy.c
LIB = mpy
DEPLIBS =
*/

/* mp_debug= 1; */
/* system,"/bin/hostname"; */

if (is_void(plug_in)) plug_in, "mpy";

local mp_rank;
local mp_size;
/* DOCUMENT mp_rank, mp_size

     set to the rank of this instance of Yorick in a message passing
     group and the size (total number of processes) of the group at
     startup.  DO NOT CHANGE THESE VALUES!
     The rank is between 0 and size-1 inclusive; note that mp_rank
     and mp_size will be nil if multiple processes are not present
     (mp_size==1 is impossible).

     The underlying notions are defined in the MPI (Message Passing
     Interface) standard.

   SEE ALSO: mp_send, mp_recv, mp_task, mp_include
 */

extern mp_send;
/* DOCUMENT mp_send, to, msg
         or mp_send, to, msg1, msg2, ...
         or mp_send, to_list, msg1, msg2, ...

     send MSG, MSG1, MSG2, ... to process whose rank is TO.  Each
     MSG must be an array (or scalar) of type char, short, int, long,
     float, double, or complex, or a scalar string.

     If the object is to send a message to all other processes, the
     binary tree broadcaster mp_bcast will probably be faster than
     mp_send.

     If TO_LIST is an array of rank numbers, then each MSG may be an
     equal length array of pointers to send a different message to
     each process in the TO_LIST, or one of the basic data types to
     send the same message to each process in TO_LIST.

     The mp_send operation blocks until the matching mp_recv operation
     begins (for every message if multiple MSG or a TO_LIST are
     specified).  However, within each mp_send, many messages may be
     launched simultaneously (but never more than one at a time to
     each process) -- the underlying MPI call is the non-blocking
     synchronous send routine MPI_Issend.

     See mp_task for how to synchronize all the processes in order to
     cleanly begin a parallel calculation.

   SEE ALSO: mp_recv, mp_bcast, mp_rank, mp_task, mp_include
 */

extern mp_recv;
/* DOCUMENT msg= mp_recv()
         or msg= mp_recv(dimlist)

     receive the next message sent by mp_send from some other process.
     The operation blocks until the next message arrives.  The mp_from
     function can be used to determine who sent the message, or to
     determine whether any message at all has arrived.

     If DIMLIST is specified, it has the same format as for the array
     function.  The arriving message must have the correct number of
     elements for the DIMLIST, or a multiple of that number; the
     result will have either those dimensions, or with an extra
     dimension tacked on the end if the arriving message is a multiple.
     (That is, you are really specifying the dimensions of the "cells"
     of which the message is to be composed.)  By default (and as a
     special case), the result of mp_recv will be either a scalar value,
     or a 1D array of the same type as the matching send.

     Each mp_recv gets one MSG from an mp_send.  Several messages from
     one process to another are guaranteed to arrive in the order of
     the mp_send calls, and within each call in the order of the
     arguments to mp_send.  However, you are responsible for considering
     how to handle the order of messages arriving from several processes.

     If you wish to process message in a different order from that in
     which they are received, you must implement the necessary queuing
     functions yourself in interpreted code.

     See mp_task for how to synchronize all the processes in order to
     cleanly begin a parallel calculation.

   SEE ALSO: mp_send, mp_rank, mp_from, mp_task, mp_include, array
 */

func mp_bcast(origin, msg, .., nfan=)
/* DOCUMENT mp_bcast, mp_rank, msg
                <on originating node>
            msg= mp_bcast(origin)
         or msg= mp_bcast(origin, dimlist)
                <on receiving nodes>

     broadcast a message MSG from one process (the ORIGIN) to all other
     processes, by means of a binary tree -- that is, each process
     passes the message along to two others.  The ORIGIN argument must
     have the same value on all processes; you cannot use mp_recv to
     receive a message broadcast using mp_bcast.  The performance of
     mp_bcast will probably be better than mp_send with a long to-list.

     For receivers, the optional DIMLIST argument works as described
     for mp_recv.  The returned msg will be scalar or 1D without it.

     All processes can use the nfan= keyword (same value everywhere) to
     have each distribute to nfan others instead of the default nfan=2.
     If nfan=1, the message is passed in a daisy chain from one to the
     next; if nfan=mp_size-1, mp_bcast reduces to a single mp_send.

   SEE ALSO: mp_send, mp_recv
 */
{
  /* roll the process rank numbers so that origin becomes 0 */
  me= (mp_rank - origin + mp_size)%mp_size;

  /* with the new numbering scheme, process me passes the message
   * along to nfan*me+1, nfan*me+2, ... nfan*me+nfan
   * (these must be rolled back to get the true process numbering) */
  if (is_void(nfan)) nfan= 2;
  i= me*nfan + 1;
  if (i<mp_size)
    to= (indgen(i:min(i-1+nfan,mp_size-1)) + origin)%mp_size;

  /* if this is not the origin, must receive the message first */
  if (me) {
    /* first collect any dimlist like the array function */
    if (!is_void(msg)) {
      if (!dimsof(msg)(1)) dims= [1, msg];
      else dims= msg(1:msg(1)+1);
    }
    while (more_args()) {
      msg= next_arg();
      if (is_void(msg)) continue;
      if (is_void(dims)) dims= [0];
      if (!dimsof(msg)(1)) {
        grow, dims, msg;
        dims(1)+= 1;
      } else {
        n= msg(1);
        if (n) grow, dims, msg(2:n+1);
        dims(1)+= n;
      }
    }

    /* receive the message */
    msg= mp_recv(dims);
  }

  /* pass the message along */
  if (numberof(to)) mp_send, to, msg;

  return msg;
}

extern mp_from;
/* DOCUMENT rank= mp_from()
         or rank= mp_from(next)

     returns the rank number of the process of the sender of the most
     recently received message (or -1 if mp_recv has not been called
     since the last mpy_sync).

     If NEXT is non-nil and non-zero, returns the rank of the sender
     of the *next* message mp_recv will produce.  If no message has
     yet been posted, returns -1 if next==1.  This feature may be used
     to test for the arrival of a message without blocking.  If next
     is a non-zero value other than 1, mp_from will block until the
     next message actually arrives.

   SEE ALSO: mp_recv, mp_send, mp_rank
 */

func mp_task(task)
/* DOCUMENT mp_task, task

     register the TASK function as a parallel processing function.
     Each process must call mp_task to declare the same set of TASK
     functions, therefore mp_task will normally be called from a
     source file included by mp_include.

     A TASK function must be have the following structure in order
     to work properly:

        func TASK(arg1, arg2, ...)
        {
          if (catch(-1)) mp_abort;
          mp_start, TASK;
          if (mp_rank) {
            ...TASK will be invoked as a subroutine with no    ...
            ...arguments in all but the rank 0 process         ...
            ...when it returns, the process becomes idle       ...
            ...so it no longer participates in the calculation ...
            ...instead of returning, a non-rank 0 task may also...
            ...choose to hang indefinitely in an mp_recv --    ...
            ...it will exit via a caught error when the next   ...
            ...registered task runs on the rank 0 process      ...
          } else {
            ...the interface to task -- it's arguments and return...
            ...value -- are relevant only to the rank 0 process  ...
            ...which must distribute startup messages and collect...
            ...results from all other processes.  when the rank 0...
            ...process decides the task is finished it returns a ...
            ...result (or has side effects) which the user wants ...
            ...the catch line is optional; if not present an     ...
            ...error in the rank 0 process will leave the other  ...
            ...processes running.  if the other processes        ...
            ...might run communicating with each other and       ...
            ...meaninglessly consuming resources, you should     ...
            ...worry about this; if the other processes will     ...
            ...halt without continuing messages from rank 0      ...
            ...you don't need to bother with this                ...
            return significant_value;
          }
        }
        mp_task, TASK;

     Only a registered task will start running on every process
     in the parallel machine.  A registered task must never be
     called during the execution of another registered task (since
     a side effect of the mp_start call is to halt and resynchronize);
     a registered task may only be called by the rank 0 process,
     invoked either interactively or as the result of some other
     interactive action (e.g.- a #include or an unregistered function
     which calls the registered TASK function).

     The mp_include, mp_cd, and mp_pool functions are initially the only
     registered tasks; other tasks are generally defined and registered
     in source files included as startup files for other packages or by
     means of mp_include.

   SEE ALSO: mp_include, mp_cd, mp_pool, mp_rank, mp_start, mp_abort, catch
 */
{
  if (!is_func(task))
    error, "cannot register non-function: "+nameof(task);
  task= nameof(task);
  if (is_void(mpy_tasks) || noneof(mpy_tasks==task))
    grow, mpy_tasks, [task];
}

func mp_start(task)
/* DOCUMENT mp_start, task
     must be the second line of any function TASK which is registered
     as a parallel task interface using mp_task.  See mp_task for the
     required form of the TASK function.

     The mp_start function resynchronizes all processes participating
     in the parallel calculation.  Called from the rank 0 process,
     mp_start arranges for TASK to be started without arguments on
     all other processes; called from any other process, mp_start
     is a no-op.

   SEE ALSO: mp_task
 */
{
  if (mp_rank) return;
  if (noneof(mpy_tasks==nameof(task)))
    error, "mp_task never registered the function "+nameof(task);

  /* resynchronize to force non-rank 0 processes back to mpy_idler */
  mpy_sync;
  /* block until mpy_idler has picked up the new task
   * since this mp_start was called by task on rank 0, and mpy_idler
   * will start task on all other ranks, task should be running
   * everywhere shortly after this mp_send returns */
  mp_send, indgen(mp_size-1), nameof(task);
}

/* this function is now never called, remove it eventually */
func mp_abort(void)
/* DOCUMENT if (catch(-1)) mp_abort
     must be the first line of any function registered with mp_task.
     This clears all pending messages, then blows up with an error,
     so it does not return to the calling task.
   SEE ALSO: mp_task, mp_start
 */
{
  if (mp_debug)
    write,"(**mp_abort) rank "+print(mp_rank)(1)+" caught "+catch_message;
  if (catch_message!=".SYNC.") mpy_sync, catch_message;
  if (mp_debug)
    write,"(**mp_abort) rank "+print(mp_rank)(1)+" done with mpy_sync";
  if (mp_rank) error, ".SYNC.";
  else error, "(bailing out of parallel task)";
}

/* temporary hack so that existing task code works properly
 * in future, catch no longer necessary in task functions
 * - potentially this breaks code that was using catch for other reasons
 */
func mp_catch(x) { return 0; }
if (is_void(_orig_catch)) { _orig_catch = catch; catch = mp_catch; }

/* The mpy_idler function is called in lieu of waiting for keyboard input
 * when Yorick is finished with all its tasks.  This idler is intended
 * for use by non-rank 0 processes.  It's purpose is to catch any errors
 * and attempt to restart the virtual machine by alerting the rank 0
 * process (which in turn will try to generate errors in the others).  */
func mpy_idler
{
  batch, 1;
  why = _mpy_catch;
  _mpy_catch = 1;
  if (why) {
    if (why == 2) {  /* this used to be mp_abort */
      if (mp_debug)
        write,"(**mp_idlera) rank "+print(mp_rank)(1)+" caught "+catch_message;
      if (catch_message!=".SYNC.") mpy_sync, catch_message;
      if (mp_debug)
        write,"(**mp_idlera) rank "+print(mp_rank)(1)+" done with mpy_sync";
      _mpy_catch = 1;
      if (mp_rank) error, ".SYNC.";
      else error, "(bailing out of parallel task)";
    }
    if (mp_debug)
      write,"(**mpy_idler) rank "+print(mp_rank)(1)+" caught "+catch_message;
    if (catch_message!=".SYNC.") mpy_sync, catch_message;
    if (mp_debug)
      write,"(**mpy_idler) rank "+print(mp_rank)(1)+" done with mpy_sync";
    set_idler, mpy_idler, 1;
    return;
  }
  if (mp_debug)
    write,"rank "+print(mp_rank)(1)+" at sync in mpy_idler";
  mpy_sync;
  /* pick up the message generated by mp_start and start that task */
  task= mp_recv();
  if (mp_debug)
    write,"rank "+print(mp_rank)(1)+" executing "+task;
  task= symbol_def(task);
  _mpy_catch = 2;
  task;
  _mpy_catch = [];
  set_idler, mpy_idler, 1;
}

func mp_include(filename)
/* DOCUMENT mp_include, filename

     include the specified FILENAME in all processes.  The FILENAME
     argument is ignored in all but the rank 0 process, which is
     assumed to be the orginal caller.  The FILENAME must be visible
     to every process (normally in Y_SITE/include, Y_SITE/contrib,
     ~/Yorick, or the current working directory - but see mp_cd in
     the latter case).

     The mp_include function is registered as a parallel task, so
     it may only be invoked interactively from the rank 0 process
     or from another file included there (see mp_task for a more
     elaborate description).

   SEE ALSO: mp_task, mp_rank, mp_cd
 */
{
  if (catch(-1)) mp_abort;
  mp_start, mp_include;
  if (mp_rank) {
    filename= mp_bcast(0);
  } else {
    if (structof(filename)!=string || strlen(filename)<1)
      error, "filename must be a non-empty string";
    mp_bcast, 0, filename;
  }
  include, filename;
}

mp_task, mp_include;

func mp_cd(dirname)
/* DOCUMENT mp_cd, dirname
         or mp_cd

     Change all processes to directory DIRNAME, or to the current
     working directory of the rank 0 process if DIRNAME is not
     specified.  Note that DIRNAME must exist for all processes.
     Note also that the processes may start in different directories.

     The mp_cd function is registered as a parallel task, so
     it may only be invoked interactively from the rank 0 process
     or from another file included there (see mp_task for a more
     elaborate description).

   SEE ALSO: mp_task, mp_rank, mp_include
 */
{
  if (catch(-1)) mp_abort;
  mp_start, mp_cd;
  if (mp_rank) {
    dirname= mp_bcast(0);
  } else {
    if (is_void(dirname)) dirname= get_cwd();
    if (structof(dirname)!=string || strlen(dirname)<1)
      error, "dirname must be a non-empty string";
    mp_bcast, 0, dirname;
  }
  cd, dirname;
}

mp_task, mp_cd;

/* ------------------------------------------------------------------------ */

func mp_pool(_p_ntasks, _mp_sow, _mp_work, _mp_reap, _mp_work0)
/* DOCUMENT mp_pool, n_tasks, sow, work, reap, work0
         or mp_pool, n_tasks, sow, work, reap

   Implement pool-of-tasks parallel calculation.  In this model,
   the master process on rank 0 has a number of tasks to be done,
   which he wants to distribute to his slaves.  The pool of tasks
   is hopefully larger than the number of slaves, so that as each
   slave finishes, it can be assigned the next task in the pool,
   achieving a simple form of load balancing.  The master can
   optionally choose to do the next task when all slaves are busy,
   or can just block until a slave finishes.

   N_TASKS is the total number of tasks in the pool; the remaining
   arguments are functions which carry out the various parts of
   the particular calculation that mp_pool is managing:

   func SOW(to, i)
     mp_send, to, <input1>, <input2>, ..., <inputQ>
     <no return value>

     The SOW function will be called by mp_pool in order to send
     the messages to a slave which will be received by the WORK
     function.  Note that SOW only runs on the master process; it
     is called by mp_pool, and has access to the local variables
     of the caller of mp_pool, which it presumably will need in
     order to figure out what to send for the i-th task.  Like
     Yorick indices, i varies from 1 to N_TASKS.  The local variables
     in the mp_pool function all have names beginning with "_p_", to
     make it very unlikely that any variables local to its caller
     will be shadowed.

   func WORK
     input1= mp_recv(dimsi1)
     input2= mp_recv(dimsi2)
     ...
     inputQ= mp_recv(dimsiQ)
     <do the task specified by the messages>
     mp_send, 0, <result1>, <result2>, ..., <resultR>
     <no return value>

     The WORK function runs on a slave, so it does NOT have access
     to any variables available to the caller of mp_pool on the
     master process.  Therefore, WORK must be completely self-contained
     so that all of the information it needs to do its job must arrive
     in the form of the messages sent by SOW (or state information
     previously sent to all slaves).  When finished, WORK sends
     its results back to the master on rank 0; it has no other return
     value.

   func REAP(i, m)
     if (m==1) {
       result1= mp_recv(dimsr1)
       <save or accumulate 1st result of i-th task>
     } else if (m==2) {
       result2= mp_recv(dimsr2)
       <save or accumulate 2nd result of i-th task>
     } else ...
     } else if (m==R) {
       resultR= mp_recv(dimsrR)
       <save or accumulate Rth result of i-th task>
     }
     return (m==R)

     The REAP function runs on the master to collect the results sent
     from a slave by a WORK function.  Like SOW, it is called by mp_pool
     on rank 0, and therefore has access to all the local variables of
     the caller of mp_pool, into which REAP must store the results.
     Because the slaves perform their tasks asynchronously, their return
     messages may be interleaved.  That is, the first argument i to
     REAP varies unpredictably in successive calls.  However, for a given
     i, the second argument m will always begin at 1 and increment by one
     on successive calls with that i.  Thus, it is possible to cope with
     situations in which one result message determines the existence of
     a subsequent message.  The return value of REAP informs mp_pool
     when that slave has completed its task and is available for another,
     by returning non-zero when m reaches the final message sent by work.
     (The mp_pool function has actually peeked at the incoming message
     to learn its origin, and therefore the corresponding task number i.
     There is no reason for REAP to call mp_from.)

   func WORK0(i)
     <do the i-th task>
     <no return value>

     If the optional WORK0 function is provided, and all slaves ever
     busy, mp_pool will call WORK0 to have the master perform a task.
     Like SOW and REAP, WORK0 always runs on rank0 and has access to the
     local variables of the caller of mp_pool.  It should combine the
     functions of SOW, WORK, and REAP without the message passing.

   SEE ALSO: mp_task, mp_partition, mp_prange
 */
{
  if (catch(-1)) mp_abort;
  mp_start, mp_pool;

  if (!mp_rank) {                                       /* MASTER */
    /* broadcast _mp_work name to slaves */
    mp_bcast, 0, nameof(_mp_work);

    /* initialize free processor list */
    _p_free= [];
    for (_p_n=mp_size-1 ; _p_n>0 ; --_p_n) _p_free= _cat(_p_n, _p_free);
    _p_working= 0;

    /* initialize the slave-->task table
     * and the number of messages counters */
    _p_table= _p_nmsg= array(0, mp_size-1);

    for (_p_n=1 ; _p_n<=_p_ntasks ; ++_p_n) {
      /* without waiting, read any results that have arrived,
       * in hopes that some slave might have finished
       * if unwilling to do any work ourself and
       * if no slaves unoccupied, wait for one to finish */
      _mp_pool_reap, is_void(_mp_work0);

      if (_p_free) {
        /* assign this task to an unoccupied slave */
        _p_to= _nxt(_p_free);
        _p_working+= 1;
        _p_table(_p_to)= _p_n;
        mp_send, _p_to, 1;    /* send start message */
        _mp_sow, _p_to, _p_n;
      } else {
        /* do this task ourselves */
        _mp_work0, _p_n;
      }
    }
    /* wait for all slaves to finish */
    _mp_pool_reap, 2;

    /* send all done message (instead of 1 message) */
    mp_send, indgen(mp_size-1), 0;

  } else {                                               /* SLAVE */
    /* get _mp_work function in original broadcast */
    _mp_work= symbol_def(mp_bcast(0));

    /* and slavishly do tasks as master dishes them out */
    while (mp_recv()) _mp_work;
  }
}

func _mp_pool_reap(wait)
{
  /* next, grab messages until one (wait==1) or all (wait==2) slaves
   * finish, or until we would block if wait==0 */
  no_free= !_p_free;
  while (!wait || ((wait==1)? no_free : _p_working)) {
    _p_n= mp_from((wait?2:1));
    if (!wait && _p_n<0) break;
    _mp_pool_handler, _p_n;
    no_free= !_p_free;
  }
}

func _mp_pool_handler(_p_n)
{
  if (!_p_n) error, "why did rank 0 send message to itself?";
  _p_nt= _p_table(_p_n);   /* translate process rank to task number */
  _p_nm= ++_p_nmsg(_p_n);  /* number of this message */
  if (_mp_reap(_p_nt, _p_nm)) {
    /* rank p slave has finished his task */
    _p_nmsg(_p_n)= _p_table(_p_n)= 0;
    _p_free= _cat(_p_n, _p_free);
    _p_working-= 1;
  }
}

mp_task, mp_pool;

func mp_partition(njobs, ntrips, master_works)
/* DOCUMENT ntasks= mp_partition(njobs, ntrips)
         or ntasks= mp_partition(njobs, ntrips, master_works)

   Partition NJOBS jobs into tasks to be distributed among slave
   processes by mp_pool.  Typically, NJOBS will be the length of an
   array of input variables, for example the number of rays to be
   traced.  Each slave is to perform a task consisting of some number
   of these individual jobs.  The idea is to pick a number of jobs
   per task large enough that the overhead of message passing and of
   carving up inputs then reassembling outputs is tolerable.
   Conversely, the number of jobs per task should be small enough
   that the pool of tasks will achieve reasonable load balancing by
   requiring each slave to come back for a new task several times.

   The NTRIPS argument is the number of tasks you would like each
   slave to perform in order to achieve load balancing, bearing in
   mind that if you choose it too large, you will increase the
   overhead by partitioning your jobs into too many tasks.  The
   minimum number of jobs for good load balancing is nslaves*NTRIPS.

   The return value is the number of tasks into which you should
   partition the NJOBS.  The mp_prange function can be used to
   compute the job index range for the i-th such task.

   SEE ALSO: mp_pool, mp_prange
 */
{
  nslaves= mp_size;
  if (!master_works) nslaves-= 1;

  /* get the maximum number of tasks any one slave should do */
  ntrips= min((njobs-1)/nslaves+1, ntrips);

  return min(nslaves*ntrips, njobs);
}

func mp_prange(i, ntasks, njobs)
/* DOCUMENT range= mp_prange(i, ntasks, njobs)
     return the job index range for the I-th of NTASKS, if the total
     number of jobs is NJOBS.  This can be used in conjunction with
     mp_partition and mp_pool.
   SEE ALSO: mp_pool, mp_partition
 */
{
  jpt= njobs/ntasks;
  rem= njobs%ntasks;
  if (rem && i<=rem) {
    jpt+= 1;
    i*= jpt;
  } else {
    i= rem + i*jpt;
  }
  return call(i-jpt+1:i);
}

/* ------------------------------------------------------------------------ */

extern mpy_sync;
/* **DOCUMENT mpy_sync
           or mpy_sync, error_message

     sends a synchronization request to the rank 0 process (if this is
     not rank 0 itself), then discards all arriving messages until the
     rank 0 process sends back a synchronization acknowledgement, at
     which point mpy_sync returns.  If any process sends an "unexpected"
     mpy_sync, all processes will halt as if by an error eventually.
     This may take some time, since the synchronization request will not
     be detected until all pending mp_send operations have been received
     and processed.

     This function should not be called by user programs; if you want
     to abort call the error function as usual; if you want to synchronize
     before beginning a calculation, use mp_start.

   SEE ALSO: mp_start, error
 */

/* Every process except the rank 0 process pauses on startup
 * until the mpy_sync command is given from the rank 0 process.
 * Also, every non-0 process runs in batch mode with the mpy_idler
 * function installed.  */
extern mpy_rank;
extern mpy_size;
extern mpy_init;

/* See process_argv - this function is called when custom.i executes. */
func get_command_line(void)
{
  { extern mp_rank, mp_size; }
  command_line= get_argv();
  if (numberof(command_line)) list= where(command_line!="-nompi");
  if (!numberof(command_line) || numberof(list)==numberof(command_line)) {
    /* Go ahead and try to start MPI.
     * The problem here is that MPI_Init calls exit if we are not running
     * as a multiprocessing task, and this error cannot be caught.
     * Furthermore, we are not guaranteed that any command line arguments
     * will be delivered to processes started by MPI, so the default must
     * be to assume that MPI_Init should be called.  */
    if (mpy_init()) {
      mp_size= mpy_size();
      mp_rank= mpy_rank();
    }
    if (mp_rank) {
      batch, 1;
      set_idler, mpy_idler;
      grow, command_line, ["-q"];
    } else if (mp_size) {
      write, "MPI started with "+print(mp_size)(1)+" processes.";
    }
  } else {
    command_line= command_line(list);
  }
  /* allow for the possibility of a second startup hook function */
  if (!is_void(mp_process_argv)) return mp_process_argv(command_line);
  else return command_line;
}
