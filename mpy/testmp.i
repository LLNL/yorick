/*
 * $Id: testmp.i,v 1.1.1.1 2005-09-18 22:05:00 dhmunro Exp $
 * Small test suite for MPY testing.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* This file must be included with mp_include to make sure that
 * the testmp function is known to all processes.  */

/* set do_fault_test non-zero to have some process fail about
 * half the time, to test mpy error recovery */
do_fault_test= 0;
if (mp_rank) random_seed, double(mp_rank)/mp_size;

func testmp(npass, nfan)
/* DOCUMENT testmp, npass, nfan

     pass a variety of message types and sizes among all processes
     in two ways: (1) direct broadcast from rank 0 to all others
     with mp_send, and (2) binary tree broadcast with mp_bcast.
     Using the optional NFAN argument (default 2), you can change the
     number of processes each one passes the message along to in the
     binary tree.

   SEE ALSO: testpool
 */
{
  if (catch(-1)) mp_abort;
  mp_start, testmp;

  mess1= "The rain in spain falls mainly in the plain.";
  mess2= *pointer(mess1);
  mess3= short(indgen(7));
  mess4= int(indgen(11));
  mess5= indgen(10000);
  mess6= float(indgen(13));
  mess7= span(0,1,200)(,-:1:50);

  elapsed= test1= test2r= test2s= test3= [0.,0.,0.];

  if (mp_rank) {
    npass= mp_recv();
    if (mp_debug) write, pr1(mp_rank)+": npass= "+pr1(npass);
    nfan= mp_recv();
    if (mp_debug) write, pr1(mp_rank)+": nfan= "+pr1(nfan);

    timer, elapsed;
    oops= 0;

    for (i=1 ; i<=npass ; ++i) {
      m1= mp_recv();
      if (mp_debug) write, pr1(mp_rank)+": got m1";
      m2= mp_recv();
      if (mp_debug) write, pr1(mp_rank)+": got m2";
      m3= mp_recv();
      if (mp_debug) write, pr1(mp_rank)+": got m3";
      m4= mp_recv(11);
      if (mp_debug) write, pr1(mp_rank)+": got m4";
      m5= mp_recv();
      if (mp_debug) write, pr1(mp_rank)+": got m5";
      m6= mp_recv();
      if (mp_debug) write, pr1(mp_rank)+": got m6";
      m7= mp_rank%2? mp_recv(dimsof(mess7)) : mp_recv(200,50);
      if (mp_debug) write, pr1(mp_rank)+": got m7";
      if (m1!=mess1 || anyof(m2!=mess2) || anyof(m3!=mess3) ||
          anyof(m4!=mess4) || anyof(m5!=mess5) ||
          anyof(m6!=mess6) || anyof(m7!=mess7) ||
          structof(m1)!=string || structof(m2)!=char ||
          structof(m3)!=short || structof(m4)!=int || structof(m5)!=long ||
          structof(m6)!=float || structof(m7)!=double) oops++;
    }

    timer, elapsed, test1;

    if (mp_debug) {
      write, pr1(mp_rank)+": m1!=mess1= "+pr1(m1!=mess1);
      write, pr1(mp_rank)+": anyof(m2!=mess2)= "+pr1(anyof(m2!=mess2));
      write, pr1(mp_rank)+": anyof(m3!=mess3)= "+pr1(anyof(m3!=mess3));
      write, pr1(mp_rank)+": anyof(m4!=mess4)= "+pr1(anyof(m4!=mess4));
      write, pr1(mp_rank)+": anyof(m5!=mess5)= "+pr1(anyof(m5!=mess5));
      write, pr1(mp_rank)+": anyof(m6!=mess6)= "+pr1(anyof(m6!=mess6));
      write, pr1(mp_rank)+": anyof(m7!=mess7)= "+pr1(anyof(m7!=mess7));
      write, pr1(mp_rank)+": structof(m1)!=string= "+pr1(structof(m1)!=string);
      write, pr1(mp_rank)+": structof(m2)!=char= "+pr1(structof(m2)!=char);
      write, pr1(mp_rank)+": structof(m3)!=short= "+pr1(structof(m3)!=short);
      write, pr1(mp_rank)+": structof(m4)!=int= "+pr1(structof(m4)!=int);
      write, pr1(mp_rank)+": structof(m5)!=long= "+pr1(structof(m5)!=long);
      write, pr1(mp_rank)+": structof(m6)!=float= "+pr1(structof(m6)!=float);
      write, pr1(mp_rank)+": structof(m7)!=double= "+pr1(structof(m7)!=double);
      write, pr1(mp_rank)+": m1= '"+m1+"'";
    }

    if (oops) error, "messages garbled in transmission test 1";

    if (do_fault_test && random()*mp_size<1.0)
      error, "***fault test triggered***";

    mp_send, 0, test1;
    if (mp_debug) write, pr1(mp_rank)+": sent test1";

    oops= 0;
    for (i=1 ; i<=npass ; ++i) {
      m1= mp_bcast_t(0, nfan=nfan);
      m2= mp_bcast_t(0, nfan=nfan);
      m3= mp_bcast_t(0, nfan=nfan);
      m4= mp_bcast_t(0, nfan=nfan);
      m5= mp_bcast_t(0, nfan=nfan);
      m6= mp_bcast_t(0, nfan=nfan);
      m7= mp_bcast_t(0, dimsof(mess7), nfan=nfan);
      if (m1!=mess1 || anyof(m2!=mess2) || anyof(m3!=mess3) ||
          anyof(m4!=mess4) || anyof(m5!=mess5) ||
          anyof(m6!=mess6) || anyof(m7!=mess7) ||
          structof(m1)!=string || structof(m2)!=char ||
          structof(m3)!=short || structof(m4)!=int || structof(m5)!=long ||
          structof(m6)!=float || structof(m7)!=double) oops++;
    }
    if (oops) error, "messages garbled in transmission test 2";

    mp_send, 0, test2r, test2s;
    if (mp_debug) write, pr1(mp_rank)+": sent test1";

  } else {
    if (is_void(npass)) npass= 1;
    if (is_void(nfan)) nfan= 2;
    mp_send, indgen(mp_size-1), npass, nfan;
    if (mp_debug) write, "0: sent npass, nfan";

    timer, elapsed;

    for (i=1 ; i<=npass ; ++i) {
      mp_send, indgen(mp_size-1), mess1,mess2,mess3,mess4,mess5,mess6,mess7;
      if (mp_debug) write, "0: sent mess1-7";
    }

    timer, elapsed, test1;

    test1a= array(test1, mp_size-1);
    done= array(0n, mp_size-1);
    while (nallof(done)) {
      m1= mp_recv();
      from= mp_from();
      if (mp_debug) write, "0: response arrived from "+pr1(from);
      if (done(from)) error, "multiple responses from one process";
      test1a(,from)= m1;
      done(from)= 1;
    }

    write,"First test is direct broadcast with mp_send";
    mbytes= 1+sizeof(mess1)+sizeof(mess2)+sizeof(mess3)+sizeof(mess4)+
      sizeof(mess5)+sizeof(mess6)+sizeof(mess7);
    write, "Total bytes sent to each process= "+pr1(mbytes);
    write, "  Number of non-rank 0 processes= "+pr1(mp_size-1);
    timer_print, "rank 0 time (send time):",test1,
      "receive time (total):",test1a(,sum),
      "receive time (worst):",test1a(,max),
      "receive time (average):",test1a(,avg);

    timer, elapsed;

    for (i=1 ; i<=npass ; ++i) {
      mp_bcast_t, 0, mess1, nfan=nfan;
      mp_bcast_t, 0, mess2, nfan=nfan;
      mp_bcast_t, 0, mess3, nfan=nfan;
      mp_bcast_t, 0, mess4, nfan=nfan;
      mp_bcast_t, 0, mess5, nfan=nfan;
      mp_bcast_t, 0, mess6, nfan=nfan;
      mp_bcast_t, 0, mess7, nfan=nfan;
    }

    test2a= array([test2r,test2s], mp_size);
    done= array(0n, 2, mp_size);
    done(,0)= 1;
    while (nallof(done)) {
      m1= mp_recv();
      from= mp_from();
      if (mp_debug) write, "0: test 2 response arrived from "+pr1(from);
      i= done(1,from)+1;
      if (done(i,from)) error, "multiple responses from one process";
      test2a(,i,from)= m1;
      done(i,from)= 1;
    }

    write,"Second test is tree broadcast with nfan= "+pr1(nfan);
    mbytes= 1+sizeof(mess1)+sizeof(mess2)+sizeof(mess3)+sizeof(mess4)+
      sizeof(mess5)+sizeof(mess6)+sizeof(mess7);
    write, "Total bytes sent to each process= "+pr1(mbytes);
    write, "  Number of non-rank 0 processes= "+pr1(mp_size-1);
    timer_print, "rank 0 time:",test2r+test2s,
      "send time (total):",test2a(,2,sum),
      "send time (worst):",test2a(,2,max),
      "send time (average):",test2a(,2,avg),
      "receive time (total):",test2a(,1,sum),
      "receive time (worst):",test2a(,1,max),
      "receive time (average):",test2a(,1,avg);
  }
}

mp_task, testmp;

func mp_bcast_t(origin, message, .., nfan=)
{
  me= (mp_rank - origin + mp_size)%mp_size;
  if (is_void(nfan)) nfan= 2;
  i= me*nfan + 1;
  if (i<mp_size)
    to= (indgen(i:min(i+nfan-1,mp_size-1)) + origin)%mp_size;

  timer, elapsed;
  if (me) {
    if (is_void(message)) dims= [0];
    else if (!dimsof(message)(1)) dims= [1, message];
    else dims= message(1:message(1)+1);
    while (more_args()) {
      message= next_arg();
      if (is_void(message)) continue;
      if (!dimsof(message)(1)) {
        grow, dims, message;
        dims(1)+= 1;
      } else {
        n= message(1);
        if (n) grow, dims, message(2:1+n);
        dims(1)+= n;
      }
    }
    message= mp_recv(dims);
  }
  timer, elapsed, test2r;

  if (numberof(to)) mp_send, to, message;
  timer, elapsed, test2s;

  return message;
}

func testpool(ntrips)
/* DOCUMENT testpool
     shake down the mp_pool function
 */
{
  if (is_void(ntrips)) ntrips= 10;
  njobs= 10000;
  input1= span(0,1,njobs);
  input2= (2.*input1)(-:1:2,);
  input2(1,)*= -1;
  result= 0.*input1;
  mask1= input1<.33;
  mask2= input1>.66;
  result0= result;
  list= where(mask1);
  result0(list)= input1(list);
  list= where((!mask2) & (!mask1));
  result0(list)= input2(1,list);
  list= where(mask2);
  result0(list)= input2(2,list);

  write, "testing mp_pool with work0 function";
  nself= 0;
  ntasks= mp_partition(njobs, ntrips, 1);
  mp_pool, ntasks, test_sow, test_work, test_reap, test_work0;
  if (anyof(result!=result0))
    write, "WARNING -- mp_pool returned wrong result";
  write, "  master did "+pr1(nself)+" of "+pr1(ntasks)+" tasks";

  write, "testing mp_pool without work0 function";
  ntasks= nt0= mp_partition(njobs, ntrips, 0);
  elapsed= time1= time2= time3= [0.,0.,0.];
  timer, elapsed;
  mp_pool, ntasks, test_sow, test_work, test_reap;
  timer, elapsed, time1;
  if (anyof(result!=result0))
    write, "WARNING -- mp_pool returned wrong result";

  timer, elapsed;
  test_pause, 500;
  timer, elapsed, time2;
  ntasks= 1;
  test_work0, 1;
  timer, elapsed, time3;

  timer_print, "actual wall timing", time1;
  timer_print, "expected wall timing", (time2*nt0)/(mp_size-1)+time3;
}

func testsync(void)
{
  if (catch(-1)) mp_abort;
  mp_start, testsync;
  if (!mp_rank) {
    line= rdline(prompt=" (1) synchronous send test -- hit RET to receive");
    msg= mp_recv();
    test_pause, 300;
    write, "(3) mp_recv on 0 from 1";
    write, "(4) sending to 1, which has paused for about 5 seconds";
    mp_send, 1, "OK";
    write, "(5) mp_send to 1 from 0 completed (after 5 sec pause)";
    test_pause, 600;
    line= rdline(prompt="hit RET to finish");
  } else if (mp_rank==1) {
    mp_send, 0, "OK";
    write, "(2) mp_send to 0 from 1 completed";
    test_pause, 5000;
    msg= mp_recv();
    test_pause, 300;
    write, "(6) mp_recv on 1 from 0";
  }
}

mp_task, testsync;

func test_sow(to, i)
{
  range= mp_prange(i, ntasks, njobs);
  mp_send, to, input1(range), input2(,range);
}

func test_work
{
  input1= mp_recv();
  input2= mp_recv(2);
  mask1= input1<.33;
  mask2= input1>.66;
  result0= 0.0*input1;
  list= where(mask1);
  if (numberof(list)) result0(list)= input1(list);
  list= where((!mask2) & (!mask1));
  if (numberof(list)) result0(list)= input2(1,list);
  list= where(mask2);
  if (numberof(list)) result0(list)= input2(2,list);

  mp_send, 0, result0;

  /* ensure some confusion in message arrival times */
  test_pause, long(1000*random());
  mp_send, 0, 1;
}

func test_pause(n)
{
  for (i=1 ; i<20*n ; ++i) x= exp(pi);  /* about like pause for HP 715 */
}

func test_reap(i, m)
{
  if (m==1) {
    range= mp_prange(i, ntasks, njobs);
    msg= mp_recv();
    result(range)= msg;
  } else if (m==2) {
    if (mp_recv()!=1) error, "garbled transmission from test_work";
  }
  return (m==2);
}

func test_work0(i)
{
  range= mp_prange(i, ntasks, njobs);
  i1= input1(range);
  i2= input2(,range);
  mask1= i1<.33;
  mask2= i1>.66;
  result0= 0.0*i1;
  list= where(mask1);
  if (numberof(list)) result0(list)= i1(list);
  list= where((!mask2) & (!mask1));
  if (numberof(list)) result0(list)= i2(1,list);
  list= where(mask2);
  if (numberof(list)) result0(list)= i2(2,list);
  result(range)= result0;
  nself+= 1;
}

if (mp_debug) write, pr1(mp_rank)+"finished including testmp.i";
