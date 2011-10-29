/* testmp.i
 * $Id: testmp.i,v 1.6 2011-02-11 05:25:42 dhmunro Exp $
 * small test suite for mpy
 */
/* Copyright (c) 2010, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*= SECTION() mpy simple parallel test suite ===============================*/

func testmp
/* DOCUMENT testmp
 *   runs testmp2, testmp3, and testmpool
 * SEE ALSO: testmp1, testmp2, testmp3, testmp4, testmpool
 */
{
  testmp2;
  testmp3;
  mp_require, "mpool.i";
  testmpool;
}

func testmp1(n)
/* DOCUMENT testmp1, n
 *   test mp_send and mp_recv between rank 0 and rank N.
 *   Other ranks will be inactive during this exchange.
 * SEE ALSO: testmp, testmp2, testmp3, testmp4, testmpool
 */
{
  if (mp_exec()) {
    if (n<1 || n>=mp_size) error, "bogus rank "+print(n)(1);
    mp_exec, "testmp1," + print(n)(1);
    return;
  }
  oops = 0;
  if (!mp_rank) {
    write, format="rank 0 begins test to rank %ld\n", n;
    for (i=0 ; i<=14 ; ++i) {
      mp_send, n, testmess(i);
      oops |= (!testmess(14-i, mp_recv(n))) << (14-i);
    }
    if (oops)
      write, format="rank 0 send/recv failed, oops=\n", oops;
    else
      write, format="rank %ld send/recv worked\n", 0;
  } else if (mp_rank == n) {
    write, format="rank %ld begins test to rank 0\n", 0;
    for (i=0 ; i<=14 ; ++i) {
      if (i%3) got = mp_recv(0);
      else mp_recv, 0, got;
      oops |= (!testmess(i, got)) << i;
      mp_send, 0, testmess(14-i);
    }
    if (oops) write, format="rank %ld send/recv failed, oops=\n",
                mp_rank, oops;
    else write, format="rank %ld send/recv worked\n", mp_rank;
  }
}

func testmp2(n)
/* DOCUMENT testmp2, n
 *   test star communication ring among all ranks. Specifically,
 *   every rank sends to rank+N.  N defaults to 1.
 *   One message of each type handled by mp_send and mp_recv is
 *   sent and received for each distance.
 * SEE ALSO: testmp1, testmp, testmp3, testmp4, testmpool
 */
{
  if (mp_exec()) {
    if (is_void(n)) n = 1;
    if (n < 0) n = mp_size - ((-n)%mp_size);
    else n = n%mp_size;
    if (!n) error, "n cannot be 0 or multiple of mp_size";
    mp_exec, "testmp2," + print(n)(1);
    return;
  }
  x = mp_size;
  y = n;
  while (y) {
    nring = y;
    y = x % nring;
    x = nring;
  }
  /* nring = gcd(n,mp_size)
   * rank 0, 1, ..., nring-1 are first points of separate rings
   * rank/nring is this rank's position in the ring
   * flag = odd position in ring
   */
  flag = (mp_rank/nring) & 1;
  t = (mp_rank + n)%mp_size;           /* send to */
  f = (mp_rank - n + mp_size)%mp_size; /* recv from */

  oops = 0;

  for (ii=0 ; ii<=14 ; ++ii) {
    /* deadlock would be possible if everyone were to send, no one recv */
    if (flag) {  /* this rank does recv then send */
      if (ii&1) got = mp_recv(f);
      else mp_recv, f, got;
      mp_send, t, testmess(ii);
    } else {     /* this rank does send then recv */
      mp_send, t, testmess(ii);
      if (ii&1) mp_recv, f, got;
      else got = mp_recv(f);
    }
    oops |= (!testmess(ii, got)) << ii;
  }
  /* test dimension arguments to mp_recv */
  z = random(4,3,5,7);
  if (flag) {  /* this rank does recv then send */
    got = mp_recv(f, 4,3);
    dims = dimsof(got);
    oops |= (dims(1)!=3 || anyof(dims!=[3,4,3,35]));
    mp_send, t, z;
    mp_recv, f, [4,4,3,5,7] ,got;
    dims = dimsof(got);
    oops |= (dims(1)!=4 || anyof(dims!=[4,4,3,5,7]));
    mp_send, t, z;
  } else {     /* this rank does send then recv */
    mp_send, t, z;
    mp_recv, f, [3,4,3,5], got;
    dims = dimsof(got);
    oops |= (dims(1)!=4 || anyof(dims!=[4,4,3,5,7]));
    mp_send, t, z;
    got = mp_recv(f, 4,[2,3,5],7);
    dims = dimsof(got);
    oops |= (dims(1)!=4 || anyof(dims!=[4,4,3,5,7]));
  }
  z = mp_handin(long(oops != 0));
  if (!mp_rank) {
    if (!z) {
      write, format="testmp2 passed on all %ld ranks\n", mp_size;
    } else {
      write, format="testmp2 failed on %ld of %ld ranks\n", z, mp_size;
    }
    if (oops)
      write, format="testmp2 rank 0 oops = %ld\n", oops;
  }
}

func testmp3
/* DOCUMENT testmp3
 *   test mp_handout and mp_handin for a variety of message types.
 * SEE ALSO: testmp1, testmp2, testmp, testmp4, testmpool
 */
{
  if (mp_exec()) {
    mp_exec, "testmp3";
    return;
  }
  oops = 0;
  if (!mpy_rank) {
    a = testmess(3);
    b = testmess(11);
    b = [b,b];
    c = testmess(5);
    d = testmess(13);
    d = [d,d,d];
    e = testmess(15);
  }
  mp_handout, a;
  mp_handout, b, c, d, e;
  if (mp_rank) {
    oops |= (!testmess(3, a));
    dims = dimsof(b);
    if (dims(1)!=2 || dims(3)!=2) {
      oops |= 1<<1;
    } else {
      oops |= (!testmess(11, b(,1))) << 2;
      oops |= (!testmess(11, b(,2))) << 2;
    }
    oops |= (!testmess(5, c)) << 3;
    dims = dimsof(d);
    if (dims(1)!=2 || dims(3)!=3) {
      oops |= 1<<4;
    } else {
      oops |= (!testmess(13, d(,1))) << 5;
      oops |= (!testmess(13, d(,2))) << 5;
      oops |= (!testmess(13, d(,3))) << 5;
    }
  }
  z = mp_handin(long(oops!=0));
  if (!mp_rank) {
    if (!z) {
      write, format="testmp3 passed on all %ld ranks\n", mp_size;
    } else {
      write, format="testmp3 failed on %ld of %ld ranks\n", z, mp_size;
    }
  }
  mp_handin;
}

func testmess(i, msg)
{
  /* v==0 scalar, v==1 array,
   * i==0-6 char, short, int, long, float, double, complex
   * i==7 string
   * msg==[] to return msg
   * otherwise return 1 if correct
   */
  t = _car(_lst(char, short, int, long, float, double, complex,
                string), (i&7)+1);
  if (t != string) {
    x = indgen(-2:3);
    if ((i&7) > 3) x += 0.5;
    x = t(x);
    if (!(i&8)) x = x(1);
  } else {
    x = "Hello, World!";
    if (i&8) x = [x, "and good night"];
  }
  if (is_void(msg)) return x;
  return (structof(msg)==t && numberof(msg)==numberof(x)
          && allof(msg==x));
}

func testmp4(n)
/* DOCUMENT testmp4, n
 *   test error recovery by causing fault on rank N, which
 *   defaults to mp_size-1.  N can be a small array to cause
 *   faults on a few ranks at once.  (If it takes more than
 *   one line to print N, all ranks will get syntax errors.)
 *   The testmp4 function has a local variable x, non-nil
 *   on the ranks which faulted.
 * SEE ALSO: testmp1, testmp2, testmp3, testmp, testmpool
 */
{
  if (mp_exec()) {
    if (is_void(n)) n = mp_size-1;
    mp_exec, "testmp4," + print(n)(1);
    return;
  }
  if (anyof(mp_rank==n)) {
    x = "testmp4 local variable to test";
    error, "this is the intentional fault";
  }
  /* without this, no calls to mp_send/recv to notice fault */
  mp_handin;
}

func testmpool(flag)
/* DOCUMENT testmpool
 *   test mpool function (and mpool_test with non-nil non-zero arg).
 * SEE ALSO: testmp1, testmp2, testmp3, testmp, testmpool
 */
{
  if (!mp_size) require, "mpool.i";
  else mp_require, "mpool.i";

  extern pool;
  if (!mp_size || flag) {
    write, "begin testing mpool_test";
    njobs = 10;
    use_vsave = 0;
    nbad = [0, 0];
    pool = mpool_test(fsow, fwork, freap);
    write, format="mpool_test finished (vpack) nerrors=%ld\n", sum(nbad);
    use_vsave = 1;
    nbad = [0, 0];
    pool = mpool_test(fsow, fwork, freap, use_vsave=1);
    write, format="mpool_test finished (vsave) nerrors=%ld\n", sum(nbad);
  }
  if (!mp_size) return;

  write, "begin testing mpool";
  njobs = 3 * min(mp_size, mpool_nmax0);
  use_vsave = 0;
  nbad = [0, 0];
  pool = mpool(fsow, fwork, freap);
  write, format="mpool finished (vpack) nerrors=%ld\n", sum(nbad);
  use_vsave = 1;
  nbad = [0, 0];
  pool = mpool(fsow, fwork, freap, use_vsave=1, self=1);
  write, format="mpool self=1 finished (vsave) nerrors=%ld\n", sum(nbad);

  mp_exec, "njobs=10;use_vsave=0;nbad=[0,0]";
  p = mpool(fsow, fwork, freap, list=[mp_size-1,mp_size-2],self=1);
  mp_exec, "nbad = mp_handin(nbad)";
  write, format="mpool list= finished (vpack) nerrors=%ld\n", sum(nbad);
}

func fsow(n, dhand)
{
  /* njobs, use_vsave local to testmpool */
  if (n > njobs) return 0;
  x = testmess((n-1)%16);
  /* idiotic in real fsow, which would use either vsave or vpack */
  if (use_vsave) vsave, dhand, use_vsave, x, njobs;
  else vpack, dhand, use_vsave, x, njobs;
  return 1;
}

func fwork(n, dhand, rhand)
{
  /* defining handle stream for vsave, char for vpack
   * (but result handle is stream for both)
   * this is idiotic in a real fwork, which would be written to
   * assume either vsave or vpack usage
   */
  v = is_stream(dhand);
  local use_vsave, x, njobs;
  if (v) restore, dhand, x, njobs, use_vsave;
  else vunpack, dhand, use_vsave, x, njobs;
  ok = testmess((n-1)%16, x);
  x = testmess(15 - (n-1)%16);
  if (v) vsave, rhand, x, ok;
  else vpack, rhand, x, ok;
  /* do a variable amount of work */
  for (r=n+(mp_rank?mp_rank:0),i=1 ; i<=8 ; ++i)
    r = 1664525*r + 1013904223; /* simple LCG from Numerical Recipes */
  r &= 1048575;  /* 2^20-1 */
  r += 1000000;    /* amount of work varies about factor of 2 */
  for (i=1 ; i<=r ; ++i);  /* reasonable fraction of a second */
}

func freap(n, rhand)
{
  local x, ok;
  /* idiotic in real freap -- would pick either vunpack or restore */
  v = is_stream(rhand);  
  if (v) restore, rhand, x, ok;
  else vunpack, rhand, x, ok;
  /* nbad local to testmpool */
  nbad += long([!ok, !testmess(15 - (n-1)%16, x)]);
}
