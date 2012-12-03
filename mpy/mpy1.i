/*
 * $Id: mpy1.i,v 1.4 2010-03-26 02:37:24 dhmunro Exp $
 * compatibility functions for obsolete mpy version 1
 */
/* Copyright (c) 2010, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*= SECTION() mpy version 1 support for backward compatibility =============*/

func mp_recv1(..)
/* DOCUMENT mp_recv1
   ***implements obsolete mpy1 mp_recv API***
 */
{
  local dims;
  while (more_args()) accum_dimlist, dims, next_arg();
  return mp_recv2(mp_probe(1)(1), dims);
}
if (is_void(mp_recv2)) mp_recv2 = mp_recv;

func mp1_include(filename)
/* DOCUMENT mp1_include, filename
 *   MP-include FILENAME in an mpy1 environment.  FILENAME must contain
 *   legacy version 1 MPY commands intended to be mp_included - that is,
 *   included by all processors.  These commands may include "bare"
 *   message passing calls, when FILENAME is being treated as the body
 *   of a parallel function that was never declared with mp_task, so
 *   message passing is taking place outside of any declared parallel
 *   function.
 *   You can achieve a similar effect on sections of an include file
 *   with:
 *     mp_recv = mp_recv1;
 *     ...mpy1 message passing code...
 *     mp_recv = mp_recv2;
 *   If you do this, you need to be careful to restore mp_recv if an error
 *   interrupts execution within the mpy1 section of the code.
 * SEE ALSO: mp_include
 */
{
  mp_exec, "_1include,\""+filename+"\";";
}
func _1include(filename)
{
  mp_recv = mp_recv1;
  include, filename, 1;
}

func mp_from(flag)
/* DOCUMENT mp_from
   ***obsolete mpy1 function*** (see mp_probe)
 */
{
  if (!flag) return mp_probe(-1);  /* undocumented mp_probe feature */
  f = mp_probe(flag != 1);
  return numberof(f)? f(1) : -1;
}

func mp_task(task)
/* DOCUMENT mp_task
   ***obsolete mpy1 function***
 */
{
  name = nameof(task);
  if (!is_func(task)) error, "cannot register non-function: "+name;
  if (is_void(mpy_tasks) || noneof(mpy_tasks==name)) grow, mpy_tasks, [name];
  _mpy_tasks = _cat(_mpy_tasks, task);
  /* now we are going to replace task with one that calls _mpy1_start
   * up to 8 arguments allowed, all treated as if they were output args
   * -- note that you could write a special version of this
   *    if you desperately needed to support an mpy1 function
   *    with more than 8 arguments or keywords
   */
  n = print(numberof(mpy_tasks))(1);
  include, ["func "+name+"(__a__) { __0__=__a__(0);",
            "local __1__,__2__,__3__,__4__,__5__,__6__,__7__,__8__;",
            "eq_nocopy,__1__,__a__(1);eq_nocopy,__2__,__a__(2);",
            "eq_nocopy,__3__,__a__(3);eq_nocopy,__4__,__a__(4);",
            "eq_nocopy,__5__,__a__(5);eq_nocopy,__6__,__a__(6);",
            "eq_nocopy,__7__,__a__(7);eq_nocopy,__8__,__a__(8);",
            "__0__=_mpy1_start("+n+",__0__,",
            +"__1__,__2__,__3__,__4__,__5__,__6__,__7__,__8__);",
            "__a__,1,__1__;__a__,2,__2__;__a__,3,__3__;__a__,4,__4__;",
            "__a__,5,__5__;__a__,6,__6__;__a__,7,__7__;__a__,8,__8__;",
            "return __0__;}",
            "wrap_args,"+name];
}

func _mpy1_start(__f__,__0__,
                 &__1__,&__2__,&__3__,&__4__,&__5__,&__6__,&__7__,&__8__)
{
  if (mp_rank) {
    /* start task with no arguments on non-0 rank */
    __f__ = _car(_mpy_tasks,__f__);
    mp_recv = mp_recv1;
    __f__;

  } else if (mp_exec()) {
    /* rank 0 in serial mode does mp_exec on this mpy1 task */
    if (__0__ > 8)
      error, "no support for mpy1 tasks with >8 arguments";
    mp_exec, "__0__=_mpy1_start("+print(__f__)(1)+",__0__,__1__,__2__,__3__,"+
      "__4__,__5__,__6__,__7__,__8__)";
    return __0__;

  } else {
    /* rank 0 in parallel mode called recursively from above mp_exec */
    __f__ = _car(_mpy_tasks,__f__);
    mp_recv = mp_recv1;
    if (__0__==0) __f__;
    else if (__0__==1) __0__=__f__(__1__);
    else if (__0__==2) __0__=__f__(__1__,__2__);
    else if (__0__==3) __0__=__f__(__1__,__2__,__3__);
    else if (__0__==4) __0__=__f__(__1__,__2__,__3__,__4__);
    else if (__0__==5) __0__=__f__(__1__,__2__,__3__,__4__,__5__);
    else if (__0__==6) __0__=__f__(__1__,__2__,__3__,__4__,__5__,__6__);
    else if (__0__==7) __0__=__f__(__1__,__2__,__3__,__4__,__5__,__6__,__7__);
    else if (__0__==8)
      __0__=__f__(__1__,__2__,__3__,__4__,__5__,__6__,__7__,__8__);
    return __0__;
  }
}

local mp_start;
/* DOCUMENT mp_start
   ***obsolete mpy1 function***
 */
mp_start = call;  /* work already done by _mpy1_starter */

func mp_bcast(origin, msg, .., nfan=)
/* DOCUMENT mp_bcast
   ***obsolete mpy1 function*** (see mp_handout)
 */
{
  me = (mp_rank - origin + mp_size)%mp_size;
  if (is_void(nfan)) nfan = 2;
  i = me*nfan + 1;
  if (i<mp_size)
    to = (indgen(i:min(i-1+nfan,mp_size-1)) + origin)%mp_size;
  if (me) {
    if (!is_void(msg)) {
      if (!dimsof(msg)(1)) dims = [1, msg];
      else dims = msg(1:msg(1)+1);
    }
    while (more_args()) {
      msg = next_arg();
      if (is_void(msg)) continue;
      if (is_void(dims)) dims = [0];
      if (!dimsof(msg)(1)) {
        grow, dims, msg;
        dims(1) += 1;
      } else {
        n = msg(1);
        if (n) grow, dims, msg(2:n+1);
        dims(1) += n;
      }
    }
    msg = mp_recv(dims);
  }
  if (numberof(to)) mp_send, to, msg;
  return msg;
}

func mp_pool(_p_ntasks, _mp_sow, _mp_work, _mp_reap, _mp_work0)
/* DOCUMENT mp_pool, n_tasks, sow, work, reap, work0
         or mp_pool, n_tasks, sow, work, reap

   ***obsolete mpy1 pool of tasks***

   func SOW(to, i)
     mp_send, to, <input1>, <input2>, ..., <inputQ>
     <no return value>

   func WORK
     input1= mp_recv(dimsi1)
     input2= mp_recv(dimsi2)
     ...
     inputQ= mp_recv(dimsiQ)
     <do the task specified by the messages>
     mp_send, 0, <result1>, <result2>, ..., <resultR>
     <no return value>

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

   func WORK0(i)
     <do the i-th task>
     <no return value>

   SEE ALSO: mp_task, mp_partition, mp_prange
 */
{
  mp_start, mp_pool;

  if (!mp_rank) {
    mp_send, indgen(mp_size-1), nameof(_mp_work);

    _p_free= [];
    for (_p_n=mp_size-1 ; _p_n>0 ; --_p_n) _p_free= _cat(_p_n, _p_free);
    _p_working= 0;

    _p_table= _p_nmsg= array(0, mp_size-1);

    for (_p_n=1 ; _p_n<=_p_ntasks ; ++_p_n) {
      _mp_pool_reap, is_void(_mp_work0);

      if (_p_free) {
        _p_to= _nxt(_p_free);
        _p_working+= 1;
        _p_table(_p_to)= _p_n;
        mp_send, _p_to, 1;
        _mp_sow, _p_to, _p_n;
      } else {
        _mp_work0, _p_n;
      }
    }
    _mp_pool_reap, 2;

    mp_send, indgen(mp_size-1), 0;

  } else {
    _mp_work= symbol_def(mp_recv());

    while (mp_recv()) _mp_work;
  }
}

func _mp_pool_reap(wait)
{
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
  _p_nt= _p_table(_p_n);
  _p_nm= ++_p_nmsg(_p_n);
  if (_mp_reap(_p_nt, _p_nm)) {
    _p_nmsg(_p_n)= _p_table(_p_n)= 0;
    _p_free= _cat(_p_n, _p_free);
    _p_working-= 1;
  }
}

mp_task, mp_pool;

func mp_partition(njobs, ntrips, master_works)
/* DOCUMENT ntasks= mp_partition(njobs, ntrips)
         or ntasks= mp_partition(njobs, ntrips, master_works)
         ***obsolete mpy1 function***
   SEE ALSO: mp_pool, mp_prange
 */
{
  nslaves= mp_size;
  if (!master_works) nslaves-= 1;

  ntrips= min((njobs-1)/nslaves+1, ntrips);

  return min(nslaves*ntrips, njobs);
}

func mp_prange(i, ntasks, njobs)
/* DOCUMENT range= mp_prange(i, ntasks, njobs)
     ***obsolete mpy1 function***
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
