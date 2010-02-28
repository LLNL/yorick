/*
 * $Id: mpy1.i,v 1.1 2010-02-28 21:49:21 dhmunro Exp $
 * compatibility functions for obsolete mpy version 1
 */
/* Copyright (c) 2010, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func mp_recv1(..)
/* DOCUMENT mp_recv1 ***implements obsolete mpy1 mp_recv API*** */
{
  local dims;
  while (more_args()) accum_dimlist, dims, next_arg();
  return mp_recv2(mp_probe(1)(1), dims);
}
if (is_void(mp_recv2)) mp_recv2 = mp_recv;

func mp_from(flag)
/* DOCUMENT mp_from ***obsolete mpy1 function*** (see mp_probe) */
{
  if (!flag) return mp_probe(-1);  /* undocumented mp_probe feature */
  f = mp_probe(flag != 1);
  return numberof(f)? f(1) : -1;
}

func mp_task(task)
/* DOCUMENT mp_task ***obsolete mpy1 function*** */
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
  include, ["func "+name+"(__a__) {",
            "eq_nocopy,__1__,__a__(1);eq_nocopy,__2__,__a__(2);",
            "eq_nocopy,__3__,__a__(3);eq_nocopy,__4__,__a__(4);",
            "eq_nocopy,__5__,__a__(5);eq_nocopy,__6__,__a__(6);",
            "eq_nocopy,__7__,__a__(7);eq_nocopy,__8__,__a__(8);",
            "_mpy1_start,"+n
            +",__1__,__2__,__3__,__4__,__5__,__6__,__7__,__8__;",
            "__a__,1,__1__;__a__,2,__2__;__a__,3,__3__;__a__,4,__4__;",
            "__a__,5,__5__;__a__,6,__6__;__a__,7,__7__;__a__,8,__8__;}",
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
    mp_exec, "_mpy1_start,"+print(__f__)(1)+",__0__,__1__,__2__,__3__,"+
      "__4__,__5__,__6__,__7__,__8__";

  } else {
    /* rank 0 in parallel mode called recursively from above mp_exec */
    __f__ = _car(_mpy_tasks,__f__);
    mp_recv = mp_recv1;
    if (__0__==0) __f__;
    else if (__0__==1) __f__,__1__;
    else if (__0__==2) __f__,__1__,__2__;
    else if (__0__==3) __f__,__1__,__2__,__3__;
    else if (__0__==4) __f__,__1__,__2__,__3__,__4__;
    else if (__0__==5) __f__,__1__,__2__,__3__,__4__,__5__;
    else if (__0__==6) __f__,__1__,__2__,__3__,__4__,__5__,__6__;
    else if (__0__==7) __f__,__1__,__2__,__3__,__4__,__5__,__6__,__7__;
    else if (__0__==8) __f__,__1__,__2__,__3__,__4__,__5__,__6__,__7__,__8__;
  }
}

local mp_start;
/* DOCUMENT mp_start ***obsolete mpy1 function*** */
mp_start = call;  /* work already done by _mpy1_starter */

func mp_bcast(origin, msg, .., nfan=)
/* DOCUMENT mp_bcast ***obsolete mpy1 function*** (see mp_handout) */
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
