/*
 * $Id: mpy.i,v 1.8 2011-02-11 05:25:42 dhmunro Exp $
 * Message passing extensions to Yorick.
 */
/* Copyright (c) 2009, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* if (is_void(plug_in)) plug_in, "mpy"; */
if (!is_func(mp_send)) error, "mpy built with TGT=dll, must build TGT=exe";

/*= SECTION() MPI parallel processing interface ============================*/

local mp_rank;
local mp_size;
local mp_nfan;
/* DOCUMENT mp_rank, mp_size, mp_nfan
 *   MPI rank of this process and total number of processes.
 *     0 <= mp_rank <= mp_size-1
 *   The variables are set at startup.  DO NOT CHANGE THESE VALUES!
 *   If multiple processes are not present, mp_size==1 and mp_rank=0.
 *   mp_nfan is the fanout used to broadcast messages by the mp_exec,
 *   mp_handout, and mp_handin functions.  See mpy_nfan.
 *
 * SEE ALSO: mp_send, mp_recv, mp_exec, mpy_nfan
 */

extern mp_exec;
/* DOCUMENT mp_exec, command_line
 *       or mp_exec, [command_line1, command_line2, ...]
 *       or mp_exec, char_array
 *       or is_serial = mp_exec()
 *
 *   The mp_exec function is how you launch all parallel tasks.
 *   COMMAND_LINE is a string to be parsed and executed on every
 *   rank.  It can be a single string, or an array of strings,
 *   or an array of char containing the text to be parsed.
 *
 *   Calling mp_exec on a non-0 rank process is illegal with the
 *   sole exception of the call in mpy_idler.  There, the call to
 *   mp_exec blocks until the matching call to mp_exec on rank 0
 *   broadcasts the command(s) to all ranks.  At that point, all
 *   non-zero ranks exit their idler, and execute the command;
 *   returning to the idle loop to wait for the next call to mp_exec
 *   on rank 0 to re-awaken them.
 *
 *   On rank 0, mp_exec executes the command in immediate mode,
 *   as if by include,[command_line],1.  Hence, the commands are
 *   parsed and executed before mp_exec returns.  Outside of the
 *   mp_exec function calls (and after startup), rank 0 is always
 *   in serial mode -- any activity, particularly include, require,
 *   or #include, affect only rank 0.  It is only "inside" a call
 *   to mp_exec that rank 0 is in parallel mode, where the include
 *   functions are collective operations.  The mp_exec function
 *   may only be called in serial mode (which means it cannot be
 *   called recursively).
 *
 *   However, mp_exec() may be called as a function at any time
 *   on any rank.  It returns 1 if and only if a call to mp_exec
 *   as a subroutine (launching a parallel task)  would be legal,
 *   that is, only if this is rank 0 in serial mode.
 *
 * SEE ALSO: mp_include, mp_send, mp_rank, mp_cd, mp_connect
 */

extern mp_send;
/* DOCUMENT mp_send, to, msg
 *       or mp_send, to, msg1, msg2, ...
 *       or mp_send, to_list, msg1, msg2, ...
 *
 *   send MSG, MSG1, MSG2, ... to process whose rank is TO.  Each
 *   MSG must be an array (or scalar) of type char, short, int, long,
 *   float, double, complex, or a scalar string.
 *
 *   If TO_LIST is an array of rank numbers, then each MSG may be an
 *   equal length array of pointers to send a different message to
 *   each process in the TO_LIST, or one of the basic data types to
 *   send the same message to each process in TO_LIST.
 *
 *   The mp_send function will not return until the msg variables can
 *   be discarded or reused.
 *
 *   Messages can be arrays, but their dimension information is not
 *   included in the actual message (they look like 1D arrays upon
 *   arrival).  You can use the vpack/vunpack functions to send and
 *   receive messages in a way the preserves their dimension
 *   information, and to pack several small messages together for
 *   improved message passing performance:
 *     mp_send, to, vpack(msg1, msg2, ...);
 *   which you receive as:
 *     vunpack, mp_recv(from), msg1, msg2, ...;
 *   String arrays and nil [] messages are permitted with vpack and
 *   vunpack, in addition to the array data types permitted by the
 *   raw mp_send and mp_recv functions.
 *
 *   If you need to pass pointer or struct messages, use vsave:
 *     mp_send, to, vsave(msg1, msg2, ...);
 *   which you receive as:
 *     restore, openb(mp_recv(from)), msg1, msg2, ...;
 *
 *   Use mp_handout to send messages from rank 0 to all ranks;
 *   very large TO_LIST arguments (more than a few dozen recipients)
 *   will be dramatically slower than mp_handout.
 *
 * SEE ALSO: mp_recv, mp_probe, mp_rank, mp_exec, mp_handout, vpack
 */

extern mp_recv;
/* DOCUMENT msg = mp_recv(from)
 *       or msg = mp_recv(from, dimlist)
 *       or mp_recv, from, msg1, msg2, msg3, ...;
 *
 *   receive the next message from the process whose rank is FROM.
 *   Messages from a given rank are always received in the order
 *   they are sent with mp_send.
 *
 *   The mp_recv function blocks until the next matching message
 *   arrives.  Any messages from ranks other than FROM which arrive
 *   before the message from FROM are queued internally, and will be
 *   returned by subsequent calls to mp_recv order of arrival.  The
 *   mp_probe function lets you query the state of this internal queue.
 *
 *   Array dimensions are not part of the message; if you send an array
 *   x, it will be received as x(*).  There are two ways to put back
 *   dimension information, depending on whether you want the sender
 *   to send the information, or whether you want the receiver to apply
 *   its own knowledge of what the dimensions must have been:
 *
 *   You can use the vpack/vunpack functions to send messages that
 *   contain the dimension information of arrays:
 *     mp_send, to, vpack(msg1, msg2, ...);
 *   which you receive as:
 *     vunpack, mp_recv(from), msg1, msg2, ...;
 *
 *   Or, you can pass mp_recv (on the receiving side) an explicit
 *   DIMLIST in the same format as the array function.  The arriving
 *   message must have the correct number of elements for the DIMLIST,
 *   or a multiple of that number; the result will have either the
 *   DIMLIST dimensions, or with an extra dimension tacked on the end
 *   if the arriving message is a multiple. (That is, you are really
 *   specifying the dimensions of the "cells" of which the message is
 *   to be composed.)  By default (and as a special case), the result of
 *   mp_recv will be either a scalar value, or a 1D array of the same
 *   type as the matching send.
 *
 *   Called as a subroutine, mp_recv can return multiple messages; MSG1,
 *   MSG2, MSG3, ... are simple variable references set to the result.
 *   Any or all of the MSGi may be preceded by a dimlist expression
 *   (not a simple variable reference) to specify a dimension list for
 *   that MSGi output.
 *
 *   The mp_reform function can add a DIMLIST after the mp_recv call:
 *   mp_reform(mp_recv(p),dimlist) is the same as mp_recv(p,dimlist).
 *
 * SEE ALSO: mp_probe, mp_send, mp_rank, mp_handout, mp_exec,
 *           mp_reform, vunpack
 */

extern mp_probe;
/* DOCUMENT ranks = mp_probe(block)
 *
 *   return list of the ranks of processes which have sent messages
 *   to this process that are waiting in the mp_recv queue.  If the
 *   queue is empty and BLOCK is nil or 0, mp_probe returns nil [].
 *   If BLOCK == 1 then mp_probe blocks until at least one message
 *   is queued, but returns immediately if the queue is not empty. If
 *   BLOCK >= 2 then mp_probe always blocks until the next message
 *   arrives, even if the queue was not empty.  The returned list of
 *   ranks is always in the order received, so that
 *     mp_recv(mp_probe(1)(1))
 *   returns the next message to arrive from any rank (without leaving
 *   you any way to find out what rank sent the message -- save the
 *   result of mp_probe if you need to know).
 *
 *   The mpy program always receives all available MPI messages
 *   before returning from any mp_recv, mp_send, or blocking mp_probe
 *   call, so that the MPI library message buffers are emptied as
 *   soon as possible.
 *
 * SEE ALSO: mp_recv, mp_send, mp_rank, mp_exec
 */

extern mpy_nfan;
/* DOCUMENT mp_exec, "mpy_nfan,"+print(nfan)(1);
 *   Resets the mp_size, mp_rank, and mp_nfan variables.  The NFAN
 *   argument can be 0 to restore the initial fanout, otherwise NFAN
 *   must be between 2 and 64.
 *   This is a very dangerous function, and is needed only in the
 *   very rare circumstance that the default value for mp_nfan is not
 *   good enough.  About the only legal way to invoke mpy_nfan is
 *   directly vian mp_exec.
 * SEE ALSO: mp_exec, mp_boss, mp_staff, mp_handout
 */
mpy_nfan;  /* special call finishes initializing mpy */
if (mp_size && mp_size<1) { mp_size=1;  mp_rank=0; }

/* ------------------------------------------------------------------------ */

func mp_reform(x, ..)
/* DOCUMENT mp_reform(x, dimlist)
 *   returns array X reshaped according to dimension list DIMLIST.
 *   If x is longer than dimlist, uses dimlist as the leading
 *   dimensions of x, adding one trailing dimension.  This is the
 *   same convention as the mp_recv(dimlist) function uses:
 *   mp_reform(mp_recv(),dimlist) is the same as mp_recv(dimlist).
 * SEE ALSO: reform, array, dimsof
 */
{
  local dims;
  while (more_args()) accum_dimlist, dims, next_arg();
  n = dims(1);
  for (i=len=1 ; i<=n ; ++i) len *= dims(i+1);
  if (n || numberof(x)==len) y = array(structof(x), dims);
  else y = array(structof(x), dims, numberof(x)/len);
  if (n || numberof(x)>len) y(*) = x(*);
  else y = x;
  return y;
}

func mp_boss(void)
/* DOCUMENT boss = mp_boss()
 *   get the rank of the "boss" for this process, or nil [] if this
 *   is rank 0.  The boss is the process from which fanout messages are
 *   sent to this process.
 * SEE ALSO: mp_nfan, mp_staff, mp_handout
 */
{
  if (!mp_rank) return [];
  return (mp_rank - 1) / mp_nfan;
}

func mp_staff(void)
/* DOCUMENT staff = mp_staff()
 *   get the list of ranks of the "staff" for this process, or nil [] if
 *   this is a leaf process.  The staff are the processes to which fanout
 *   messages are sent by this process.
 * SEE ALSO: mp_boss, mp_nfan, mp_handout
 */
{
  if (!mp_nfan || mp_size<2) return [];
  staff = mp_rank*mp_nfan + indgen(mp_nfan);
  return staff(where(staff < mp_size));
}

func mp_handout(args)
/* DOCUMENT mp_handout, var1, var2, ...
 *
 *   distribute VAR1, VAR2, etc. to all processes.  On rank 0, the VARi
 *   are inputs, on all other ranks the VARi are outputs.  The mp_handout
 *   operation is collective, so it must be called on all ranks.  The
 *   operation uses the same logarithmic fanout as the MPY include
 *   operation.  The VARi must be arrays of numbers or strings.
 *     if (!mp_rank) {
 *       array1 = <something>;
 *       array2 = <something else>;
 *       ...
 *     }
 *     mp_handout, array1, array2, ...;
 *
 *   The VARi are combined into a single message using vpack, so the
 *   string arrays are allowed, and array dimensions are preserved.
 *   The VARi may not be pointers or structs.
 *
 * SEE ALSO: mp_handin, mp_nfan, mp_send, mp_recv
 */
{
  if (!mp_size || mp_size<2) return;
  if (numberof(args(-))) error, "unrecognized keyword";
  n = args(0);
  if (!n) error, "requires at least one argument to hand out";
  boss = mp_boss();
  if (is_void(boss)) {
    f = vopen(,1);
    for (i=1 ; i<=n ; ++i) vpack, f, args(i);
    msg = vpack(f);
  } else {
    msg = mp_recv(boss);
  }
  staff = mp_staff();
  if (numberof(staff)) mp_send, staff, msg;
  if (mp_rank) {
    for (i=1 ; ; ++i) {
      if (args(0,i)) error, "arguments must be simple variable references";
      args, i, vunpack(msg, -);
      if (i >= n) break;
      if (vunpack(msg)) error, "boss sent too few messages";
    }
    if (!vunpack(msg)) error, "boss sent too many messages";
  }
}
wrap_args, mp_handout;

func mp_handin(part, reduce)
/* DOCUMENT mp_handin
 *       or result = mp_handin(part)
 *       or result = mp_handin(part, reduce)
 *       or result = mp_handin(part, min)
 *       or result = mp_handin(part, max)
 *       or result = mp_handin(part, sum)
 *   acknowledge completion to rank 0.  The mp_handin function must be
 *   called on all ranks; it uses the same logarithmic fanout as mp_handout,
 *   but in the reverse direction, with messages beginning at the leaf ranks
 *   and propagating to their bosses until finally reaching rank 0.
 *   In the second form, PART can be any numeric array; RESULT will be
 *   the sum of PART for this rank and all its staff.  The PART array,
 *   if present, must have the same dimensions on every rank.
 *   You may supply a REDUCE argument to get a reduction function other than
 *   REDUCE may be either one of the index range functions min, max, or
 *   sum (the default), or an interpreted function REDUCE(a,b) returning
 *   the combination of a and b you wish to return.  The default, sum,
 *   returns a+b.  REDUCE must be commutative and associative.
 * SEE ALSO: mp_handout, mp_send, mp_recv
 */
{
  if (!mp_size || mp_size<2) return part;
  if (is_void(part)) part = 0;
  if (is_void(reduce)) {
    reduce = _sum_reduce;
  } else if (is_range(reduce)) {
    reduce = print(reduce)(1);
    if (reduce == "min::") reduce = _min_reduce;
    else if (reduce == "max::") reduce = _max_reduce;
    else if (reduce == "sum::") reduce = _sum_reduce;
    else error, "illegal reduction function "+reduce;
  }
  staff = mp_staff();
  dims = dimsof(part);
  part = part(*);
  for (i=1 ; i<=numberof(staff) ; ++i) part = reduce(part, mp_recv(staff(i)));
  boss = mp_boss();
  if (!is_void(boss)) mp_send, boss, part;
  return reform(part, dims);
}

func _min_reduce(a, b) { return min(a, b); }
func _max_reduce(a, b) { return max(a, b); }
func _sum_reduce(a, b) { return a + b; }
errs2caller, _min_reduce, _max_reduce, _sum_reduce;

func mp_require(filename)
/* DOCUMENT mp_require, filename
 *   same as mp_include, but does parallel require instead of include.
 * SEE ALSO: mp_include
 */
{
  mp_exec, "require,\""+filename+"\";";
}

func mp_include(filename)
/* DOCUMENT mp_include, filename
 *   call mp_exec with "include,filename".
 *
 *   The ordinary #include directive and the include and require
 *   functions, when used as part of a parallel task (and at startup,
 *   which is effectively a parallel task), are collective operations
 *   requiring that all ranks reach them simultaneously, and in a state
 *   in which an mp_handout operation originating at rank 0 works.
 *   When rank 0 is running outside a parallel task, #include, include,
 *   and require happen only on rank 0.  The mp_include function always
 *   forces the parallel include.
 *
 *   A call to mp_include is legal only on rank 0 in serial mode.
 *
 * SEE ALSO: mp_exec, mp_require, include, mp_rank, mp_cd
 */
{
  mp_exec, "include,\""+filename+"\";";
}

func mp_cd(dirname)
/* DOCUMENT mp_cd, dirname
 *       or mp_cd
 *
 *   Change all processes to directory DIRNAME, or to the current
 *   working directory of the rank 0 process if DIRNAME is not
 *   specified.  Note that DIRNAME must exist for all processes.
 *   Note also that the processes may start in different directories.
 *
 *   The mp_cd function can only be called from rank 0 in serial mode.
 *
 * SEE ALSO: mp_handout, mp_rank, mp_include
 */
{
  if (mp_rank) error, "can only be called from rank 0";
  if (is_void(dirname)) dirname = get_cwd();
  if (structof(dirname)!=string || strlen(dirname)<1)
    error, "dirname must be a non-empty string";
  mp_exec, "cd,\""+dirname+"\";";
}

func mp_set_debug(onoff)
/* DOCUMENT mp_set_debug, onoff
 *
 *   Set mp_debug to ONOFF on all ranks.  ONOFF non-zero turns on
 *   copious debugging messages, printed on stdout, with all ranks
 *   jumbled together.
 *
 *   The mp_set_debug function is legal only from rank 0 in serial mode.
 *
 * SEE ALSO: mp_dbg, mp_handout, mp_rank, mp_include
 */
{
  if (mp_rank) error, "can only be called from rank 0";
  mp_exec, "mp_debug="+print(!(!onoff))(1)+";";
}

extern mp_dbstate;  /* compiled debug printing switch */

func mp_dbg(msg)
/* DOCUMENT mp_dbg, msg
 *   print mpy debugging message MSG if and only if mp_debug is set.
 * SEE ALSO: mp_set_debug
 */
{
  if (mp_debug) write, "@"+print(mp_rank)(1)+": "+(is_void(msg)?"":msg);
}

func mp_connect(rank, prompt=)
/* DOCUMENT mp_connect, rank
 *   connect to non-zero RANK.  Rank 0 enters a loop collecting command lines
 *   and sending them to this rank for execution.  Exit by calling mp_disconnect.
 *   Do not attempt to perform other parallel operations; you are in a parallel
 *   task in which all ranks other than 0 and RANK happen to be finished.
 *   You cannot send incomplete command lines.
 *   The prompt (on rank 0) mp_conprompt defaults to "rank%ld> ", which you
 *   can change with the prompt= keyword.  If prompt contains "%ld", the rank
 *   connected will appear in the prompt.
 * SEE ALSO: mp_exec, mp_disconnect
 */
{
  if (mp_rank) error, "called on non-0 rank";
  prank = print(rank)(1);
  if (rank<1 || rank>=mp_size) error, "no connection to rank " + prank;
  _mp_connect = _mp_connect0;  /* only called on rank 0 */
  mp_exec, "_mp_connect,"+prank;
}
func _mp_connect0(rank)
{
  /* we are still inside mp_exec, inside mp_connect */
  extern _mp_conrank;
  _mp_conrank = rank;
  if (is_void(prompt)) prompt = "rank%ld> ";
  if (strmatch(prompt, "%")) prompt = swrite(format=prompt,rank);
  /* _mp_connect1 needs to be called at idle time */
  _mpy_set_idler, _mp_connect1;

  /* need to emulate _mp_connect for call from mp_connect */
  mp_send, _mp_conrank, "_mp_conprompt=\""+prompt+"\"";
  line = mp_recv(_mp_conrank);
}
func _mp_connect1
{
  /* we have been called as idler to get a line and pass it on
   * note that rdline fails if called inside an immediate include
   *   (and mp_exec does an immediate include on rank 0)
   * note that prompt was issued by _mp_conrank on previous call
   */
  line = rdline(prompt="");
  mp_exec, "_mp_connect,"+print(_mp_conrank)(1);
}
func _mp_connect(rank)
{
  if (!mp_rank) {
    mp_send, _mp_conrank, line;  /* line is local to _mp_connect1 */
    if (mp_recv(_mp_conrank)) _mp_conrank = [];
    else _mpy_set_idler, _mp_connect1;
  } else if (mp_rank==rank) {
    extern _mp_conprompt;
    _mp_disconnect = 0n;
    /* dbexit from inside immediate include longjumps (clears stack) */
    dbexit = quit = mp_disconnect;
    include, [mp_recv(0)], 1;
    mp_send, 0, _mp_disconnect;
    if (_mp_disconnect) mp_dbexit, 0;
    else write, format="%s", _mp_conprompt;
  }
}
if (is_void(mp_dbexit)) mp_dbexit = dbexit;

func mp_disconnect(n)  /* argument to mimic dbexit */
/* DOCUMENT mp_disconnect
 *   disconnect to end an mp_connect session.
 * SEE ALSO: mp_connect
 */
{
  extern _mp_disconnect, _mp_conprompt;
  _mp_disconnect = 1n;
  _mp_conprompt = [];
}

/* ------------------------------------------------------------------------ */

/* mpy.i is included before stdx.i, which calls include_all,
 *   which uses the non-scalable function lsdir
 * define _include_all_hook that makes include_all scalable
 * also redefine customize function to avoid non-mpy custom.i
 */
func _include_all_hook(dir)
{
  if (!mp_rank) files = include_all_ls(dir);
  if (mp_size && !mp_exec()) mp_handout, files;
  return files;
}
if (mp_size) customize = noop;

func mpy_get_cmdln(void)
{
  m = _mpy_cmdln;
  _mpy_cmdln = [];
  return m;
}

func mpy_process_argv(void)
{
  if (get_command_line == process_argv) return command_line;
  extern _mpy_cmdln;
  if (is_void(get_command_line)) _mpy_cmdln = get_argv();
  else _mpy_cmdln = get_command_line();

  if (numberof(_mpy_cmdln)>=2) {
    command_line = _mpy_cmdln(2:);
    mask = (strpart(command_line, 1:2) == "-j");
    j = (command_line(0) == "-j");
    if (j) mask(0) = 0;
    list = where(mask);
    n = numberof(list);
    if (n) {
      file = strpart(command_line(list), 3:);
      i = where(file == "");
      if (numberof(i)) {
        list = list(i) + 1;
        file(i) = command_line(list);
        mask(list) = 1;
      }
    }
    if (j) mask(0) = 1;
    list = where(mask);
    if (numberof(list)) _mpy_cmdln(list+1) = string(0);
    _mpy_cmdln = _mpy_cmdln(where(_mpy_cmdln));
    q = (numberof(_mpy_cmdln) && anyof(_mpy_cmdln=="-q"));
  }

  if (!q) write, format="mpy initialized MPI on %ld processes\n", mp_size;

  for (i=numberof(file) ; i>=1 ; --i) mp_include, file(i);

  get_command_line = mpy_get_cmdln;
  m = process_argv();
  return m;
}

func mpy_idler
{
  while (_mpy_idler) {
    idler = _mpy_idler;
    _mpy_idler = [];
    idler;
  }
  if (mp_rank) {
    extern _mpy_count;
    _mpy_count = 0;
    mp_exec;  /* all parallel tasks created here */
    _mpy_set_idler, mpy_idler;
  } else if (!mp_exec()) {
    mp_exec;  /* turns off parallel mode on rank 0 */
    extern after_error;
    after_error = [];
  }
}
_mpy_count = 0;
if (mp_rank) batch, 1;

if (is_void(_mpy_set_idler)) _mpy_set_idler = set_idler;
func set_idler(idler)
{
  extern _mpy_idler;
  _mpy_idler = idler;
}
if (mp_size) _mpy_set_idler, mpy_idler, (mp_rank? 3 : 2);

func mpy_after_error
{
  extern after_error;
  if (++_mpy_count < 10) {
    _mpy_set_idler, mpy_idler;
  } else {
    after_error = [];
    batch, 1;
    _mpy_set_idler, , (mp_rank!=0);
    error,"(FATAL) rank " + print(mp_rank)(1) + " quitting on fault loop";
  }
}
/* mp_exec sets this on rank 0 as appropriate */
if (mp_size) after_error = mpy_after_error; /* reset in mpy_idler for rank 0 */

/* this is the rank 0 after_error function */
func mpy_on_fault
{
  if (!mp_rank) {
    /* enter loop sending command lines to dbug mode on mpy_frank */
    if (!mpy_dbauto) {
      write, mpy_frank,
        format="Type <RETURN> now to debug on rank %ld\n";
      line = rdline(prompt="> ");
      if (strlen(line)) {
        include, [line];
        /* following mp_exec happens before above include */
        if (!mpy_frank) dbexit, 0;
        else mp_exec, "if(mp_rank==mpy_frank)dbexit,0";
        return;
      }
    }
    if (!mpy_quiet)
      write, mpy_frank,
        format="Entering dbug mode on rank %ld, type dbexit to exit\n";
    if (mpy_frank) mp_connect, mpy_frank, prompt="dbug@%ld> ";
  }
}
/* set to faulting rank before mpy_on_fault called */
local mpy_frank;

/* ------------------------------------------------------------------------ */
