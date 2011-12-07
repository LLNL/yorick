/*
 * $Id: mpool.i,v 1.2 2011-02-11 05:25:42 dhmunro Exp $
 * Pool of jobs function for mpy.
 */
/* Copyright (c) 2010, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*= SECTION() mpy pool of tasks programming paradigm =======================*/

func mpool(__a__) /* fsow, fwork, freap, use_vsave=, self=, list=, nmax= */
/* DOCUMENT pool_stats = mpool(fsow, fwork, freap)
 *   perform a pool-of-jobs parallel calculation, in which a master
 *   process farms out jobs to many slave processes.  The FSOW function
 *   defines one job, which mpool sends via MPI to an idle slave.  The
 *   slave calls the FWORK function to do the job, which mpool sends
 *   back to the master.  The master calls the FREAP function to assimilate
 *   the result of one job.  The whole cycle repeats until FSOW signals
 *   there are no more jobs.  The return value of mpool is a mpool_t struct
 *   instance containing statistics about the pool.
 *
 *   The mpool function can be called in serial mode on rank 0, or in
 *   parallel mode on all participating ranks (by default that is all ranks).
 *   The FSOW, FWORK, and FREAP functions must conform to the following
 *   templates:
 *   func FSOW(njob, job_define_handle) {
 *     if (no_more_jobs) return 0;
 *     <compute parameters p1,p2,... for job number njob>
 *     vpack, job_define_handle, p1, p2, ...;
 *     return 1;
 *   }
 *   func FWORK(njob, job_define_handle, job_result_handle) {
 *     local p1, p2, ...;
 *     vunpack, job_define_handle, p1, p2, ...;
 *     <do job number njob defined by p1, p2, ...>
 *     <produce results r1, r2, ...>
 *     vpack, job_result_handle, r1, r2, ...;
 *   }
 *   func FREAP(njob, job_result_handle) {
 *     local r1, r2, ...;
 *     vunpack, job_result_handle, r1, r2, ...;
 *     <assimilate results r1,r2,... of job number njob>
 *   }
 *
 *   Several keywords are accepted by mpool:
 *   use_vsave=1   if FSOW, FWORK, and FREAP use vsave instead of vpack
 *     and restore instead of vunpack.
 *   self=1        if the FWORK function should be called on the master
 *     process when all slaves are busy.  (This is usually not a good idea,
 *     because many slaves can become idle while the master is working.)
 *   list=[master_rank, slave_rank1, slave_rank2, ...]
 *     to specify a subset of processes to participate in the pool.  If mpool
 *     is called in serial mode, any processes not in list will be idle.  If
 *     mpool is called in parallel mode, the list= keyword must be identical
 *     in every participating process, but mpool will generate no message
 *     traffic outside the listed processes.  You can use this mechanism to
 *     permit FWORK functions to themselves call mpool.
 *     Without list=, rank 0 is the master and all other ranks are slaves.
 *   nmax=   do not use more than this many processes as slaves
 *
 *   If tsow is the time to generate a job with FSOW, twork is the time to
 *   do a job with FWORK, and treap is the time to reap a job with FREAP,
 *   then mpool will employ twork/tsow slaves before the first slave finishes.
 *   In the long run, however, mpool can employ only twork/(tsow+treap) slaves
 *   in steady state, so unless treap is negligible compared to tsow, you risk
 *   starting more jobs than you can handle.  Since all slaves send messages
 *   back to the master, there is a serious risk that mpool will create a
 *   "denial of service attack" against the master process, if a large number
 *   of slaves is available.  By default, nmax=100, but even this may be too
 *   large, and you should consider carefully raising the limit.  You don't
 *   want more than a couple of dozen slaves whose job results are in the
 *   master's mp_recv queue waiting to be reaped.
 *
 *   The return value of mpool is meaningful only on the master process.
 *   Among other things, it contains the following information:
 *     pool.njobs   total number of jobs done
 *     pool.nused   maximum number of slaves employed
 *     pool.navg    average number of slaves employed = twork/(tsow+treap)
 *     pool.nqmax   largest number of jobs in mp_recv queue
 *     pool.nignore  number of non-pool messages in mp_recv queue
 *     pool.nself   number of jobs done by master
 *     pool.tsow    total FSOW time [cpu,sys,wall] (help,timer)
 *     pool.twork   total FWORK time [cpu,sys,wall]
 *     pool.treap   total FREAP time [cpu,sys,wall]
 *
 *   Algorithm:
 *     if no more jobs,
 *       if no slaves active, quit
 *       else block until message pending from slave
 *     if message pending from slave,
 *       reap pending, then sow next job to that slave
 *     else if nused<nmax
 *       sow to new slave, increment nused
 *     else if self work allowed
 *       sow to self, then reap from self
 *     else
 *       block until message pending from slave
 *
 * SEE ALSO: mpool_test, mpool_stats, mp_exec, mp_recv, vpack, vsave, timer
 */
{
  local _jobi, _jobo, _pool;
  if (mp_exec()) {
    mp_exec, "_mpool";

  } else {
    _mp_fsow = __a__(1);
    _mp_fwork = __a__(2);
    _mp_freap = __a__(3);
    if (__a__(0)!=3 || !is_func(_mp_fsow)
        || !is_func(_mp_fwork) || !is_func(_mp_freap))
      error, "expecting exactly three function-valued arguments";

    __b__ = __a__("list");
    __c__ = numberof(__b__)? __b__(1) : 0;
    if (__c__==mp_rank) __f__ = _mpool_fmaster;
    else __f__ = _mpool_fslave;
    _pool = mpool_t(master=__c__, list=&__b__, vsave=!(!__a__("use_vsave")),
                    self=!(!__a__("self")));
    __c__ = __a__("nmax");
    if (__c__) _pool.nmax = __c__;
    if (!numberof(__b__) || anyof(__b__==mp_rank)) __f__, _pool;
  }

  return _pool;
}
wrap_args, mpool;

func _mpool /* parallel task that mpool launches in serial mode */
{
  if (!mp_rank) {
    /* mpool __a__ argument available as extern on rank 0 */
    __b__ = __a__(-,1);
    if (!__b__) __b__ = nameof(__a__(1));
    __c__ = __a__(-,2);
    if (!__c__) __c__ = nameof(__a__(2));
    __c__ = "_pool=mpool("+__b__+","+__c__+",";
    __b__ = __a__(-,3);
    if (!__b__) __b__ = nameof(__a__(3));
    __c__ += __b__;
    __b__ = __a__("use_vsave");
    if (!is_void(__b__)) __c__ += ",use_vsave="+print(__b__)(1);
    __b__ = __a__("nmax");
    if (!is_void(__b__)) __c__ += ",nmax="+print(__b__)(1);
    __b__ = __a__("self");
    if (!is_void(__b__)) __c__ += ",self="+print(__b__)(1);
    __b__ = __a__("list");
    if (!is_void(__b__)) __c__ += ",list=__b__";
    __c__ += ")";
    __c__ = vpack(__b__, __c__);
  }
  mp_handout, __c__;
  vunpack, __c__, __b__, __c__;
  include, [__c__], 1;
  if (_pool.master) {
    if (mp_rank == _pool.master) mp_send, 0, 0;
    if (!mp_rank) __c__ = mp_recv(_pool.master);
  }
}

struct mpool_t {
  long master;
  pointer list;
  long nmax;
  int self, vsave;

  int sow;   /* flag set if op is sow, clear if op is reap */
  int state; /* for _mpool_active */
  long slave, njobs, nused, nself, nqmax, nignore, nactive;
  double navg, t0(3), tsow(3), twork(3), treap(3);
};

func _mpool_fmaster(_pool)
{
  /* _jobi, _jobo local to mpool */
  extern _jobi, _jobo;
  while (_mpool_active(_pool)) {
    if (_pool.sow) {
      _mpool_timer, _pool, 0;
      _jobi = _mpool_create(_pool, ++_pool.njobs);
      __c__ = _mp_fsow(_pool.njobs, _jobi);
      if (!__c__) --_pool.njobs;
      _jobi = _pool.vsave? vsave(_jobi) : vpack(_jobi);
      _mpool_timer, _pool, 1;
      if (__c__) {
        if (_pool.slave!=mp_rank) mp_send, _pool.slave, _jobi;
        else _mpool_fslave, _pool;
      } else {
        if (_pool.slave!=mp_rank) --_pool.nactive;  /* did no sow */
        _pool.state = 20;
      }
    } else {
      _mpool_timer, _pool, 0;
      if (_pool.slave!=mp_rank) mp_recv, _pool.slave, _jobo;
      __c__ = _mpool_open(_pool, _jobo);
      _mp_freap, __c__, _jobo;
      _mpool_timer, _pool, 3;
    }
  }
}

func _mpool_fslave(_pool)
{
  /* _jobi, _jobo local to mpool */
  extern _jobi, _jobo;
  while (_mpool_active(_pool)) {
    _mpool_timer, _pool, 0;
    if (_pool.master!=mp_rank) mp_recv, _pool.master, _jobi;
    __c__ = _mpool_open(_pool, _jobi);
    if (__c__ > 0) {
      _jobo = _mpool_create(_pool, __c__);
      _mp_fwork, __c__, _jobi, _jobo;
      _jobo = _pool.vsave? vsave(_jobo) : vpack(_jobo);
      if (_pool.master!=mp_rank) mp_send, _pool.master, _jobo;
    } else {
      _pool.state = 30;
    }
    _mpool_timer, _pool, 2;
  }
}

func _mpool_create(_pool, __f__)
{
  if (_pool.vsave) {
    __c__ = createb(char);
    vsave, __c__, "_mpool", __f__;
  } else {
    __c__ = vopen(,1);
    vpack, __c__, __f__;
  }
  return __c__
}

func _mpool_open(_pool, &__f__)
{
  if (_pool.vsave) {
    __f__ = openb(__f__);
    __c__ = __f__._mpool;
  } else {
    __c__ = vunpack(__f__, -);
  }
  return __c__;
}

if (!mpool_nmax0) mpool_nmax0 = 100;
func _mpool_active(_pool)
{
  if (!_pool.state) {  /* not initialized */
    if (!_pool.nmax) _pool.nmax = min(mp_size-1, mpool_nmax0);
    if (_pool.list) _pool.nmax = min(_pool.nmax, numberof(*_pool.list)-1);
    _pool.state = (mp_rank==_pool.master)? 1 : 11;
    return _mpool_active(_pool);

  } else if (_pool.state == 1) { /* just did non-self sow or self reap */
    unused = (_pool.nused < _pool.nmax);
    if (_pool.nactive &&
        _mpool_ready(_pool, !unused && !_pool.self)) {
      /* ready to reap a job */
      _pool.sow = 0;
      _pool.state = 2;
    } else {
      /* sow another job without reaping any */
      _pool.sow = 1;
      if (unused) {
        ++_pool.nactive;
        n = ++_pool.nused;
        _pool.slave = _pool.list? (*_pool.list)(1+n) : n;
      } else /* _pool.self */ {
        ++_pool.nself;
        _pool.slave = mp_rank;
        _pool.state = 12;  /* ready for fslave */
      }
    }

  } else if (_pool.state == 2) { /* just did non-self reap */
    /* sow immediately to rank just reaped */
    _pool.sow = 1;
    _pool.state = 1;

  } else if (_pool.state == 3) { /* just did self sow, work */
    _pool.sow = 0;
    _pool.state = 1;
    /* _jobo set by fwork */

  } else if (_pool.state == 11) { /* loop in fslave */

  } else if (_pool.state == 12) { /* did self sow */
    _pool.state = 13;
  } else if (_pool.state == 13) { /* did self work */
    _pool.state = 3;
    return 0;

  } else if (_pool.state == 20) { /* no more jobs */
    if (_pool.nactive) {
      _mpool_ready, _pool, 1;
      --_pool.nactive;
      _pool.sow = 0;
    } else {
      /* need to shut down all slaves */
      _jobi = _jobo = [];
      _jobi = _mpool_create(_pool, 0);
      _jobi = _pool.vsave? vsave(_jobi) : vpack(_jobi);
      /* send shutdown like mp_handout, confirm like mp_handin */
      staff = _pool.list? *_pool.list : indgen(0:(mp_size?mp_size-1:0));
      staff = (numberof(staff)>1)? staff(2:) : [];
      if (numberof(staff)) {
        mp_send, staff, _jobi;
        /* collect timing data, confirm shutdown of all slaves */
        for (i=1 ; i<=numberof(staff) ; ++i)
          _pool.twork += mp_recv(staff(i));
      }
      /* compute navg */
      _pool.navg = _mpool_navg(_pool.tsow, _pool.twork, _pool.treap);
      return 0;  /* exit fmaster */
    }

  } else if (_pool.state == 30) { /* shut down slave */
    _jobi = _jobo = [];
    /* send timing info back to boss and exit fslave */
    mp_send, _pool.master, _pool.twork;
    return 0;

  } else {
    error, "pool state corrupted";
  }
  return 1;
}

func _mpool_ready(_pool, block)
{
  /* test or wait for message from any slave */
  local list;
  eq_nocopy, list, *_pool.list;
  for (;;) {
    i = _pool.nignore;
    q = mp_probe(block && !i);
    if (numberof(q) <= i) {
      if (!block) return 0;
      q = mp_probe(2);
    }
    _pool.nqmax = max(numberof(q)-i, _pool.nqmax);
    if (is_void(list)) {
      q = q(1);
    } else {
      for (++i ; i<=numberof(q) ; ++i) if (anyof(list==q(i))) break;
      _pool.nignore = i-1;
      if (i > numberof(q)) {
        if (!block) return 0;
        continue;
      }
      q = q(i);
    }
    break;
  }

  /* message ready to reap on rank q */
  _pool.slave = q;
  return 1;
}

func _mpool_navg(tsow, twork, treap)
{
  /* compute navg = steady state number of slaves */
  t = tsow(3) + treap(3);
  return t? twork(3)/t : 0.0;
}

func mpool_test(_mp_fsow, _mp_fwork, _mp_freap,
                use_vsave=, self=, list=, nmax=)
/* DOCUMENT pool_stats = mpool_test(fsow, fwork, freap)
 *   test FSOW, FWORK, and FREAP functions by conducting an mpool
 *   on a single processor in serial mode.  This can be run in ordinary
 *   serial yorick; it does not require mpy.  The mpool_test function
 *   accepts and uses the use_vsave= keyword.  The other mpool keywords
 *   are accepted but ignored.
 * SEE ALSO: mpool, mpool_stats
 */
{
  extern vsave;
  _pool = mpool_t(vsave=!(!use_vsave), self=1);

  for (;;) {
    _mpool_timer, _pool, 0;
    _jobi = _mpool_create(_pool, ++_pool.njobs);
    __c__ = _mp_fsow(_pool.njobs, _jobi);
    if (!__c__) --_pool.njobs;
    _jobi = _pool.vsave? vsave(_jobi) : vpack(_jobi);
    _mpool_timer, _pool, 1;

    _mpool_timer, _pool, 0;
    __b__ = _mpool_open(_pool, _jobi);
    if (__c__ > 0) {
      _jobo = _mpool_create(_pool, __b__);
      _mp_fwork, __b__, _jobi, _jobo;
      _jobo = _pool.vsave? vsave(_jobo) : vpack(_jobo);
    }
    _mpool_timer, _pool, 2;
    if (!__c__) break;

    _mpool_timer, _pool, 0;
    __c__ = _mpool_open(_pool, _jobo);
    _mp_freap, __c__, _jobo;
    _mpool_timer, _pool, 3;
  }
  _pool.nself = _pool.njobs;
  _pool.navg = _mpool_navg(_pool.tsow, _pool.twork, _pool.treap);
  return _pool;
}

/* work around timer misfeature */
func _mpool_timer(_pool, __a__)
{
  if (!__a__) {
    t=_pool.t0; timer,t; _pool.t0=t;
  } else if (__a__==1) {
    t=_pool.t0; u=_pool.tsow; timer,t,u; _pool.t0=t; _pool.tsow=u;
  } else if (__a__==2) {
    t=_pool.t0; u=_pool.twork; timer,t,u; _pool.t0=t; _pool.twork=u;
  } else {
    t=_pool.t0; u=_pool.treap; timer,t,u; _pool.t0=t; _pool.treap=u;
  }
}

func mpool_stats(_pool)
/* DOCUMENT mpool_stats, pool_stats
 *   print statistics from POOL_STATS returned by mpool or mpool_test.
 * SEE ALSO: mpool, mpool_test
 */
{
  f1 = "did %ld jobs employing %ld slaves\n";
  f2 = "    %ld jobs done by master\n";
  f3 = "task   CPU (s)     SYS (s)     WALL(s)\n";
  f4 = " sow  %10.3e  %10.3e  %10.3e\n";
  f5 = "work  %10.3e  %10.3e  %10.3e\n";
  f6 = "reap  %10.3e  %10.3e  %10.3e\n";
  f7 = "could employ %.1f slaves in steady state\n";
  write, format=f1, _pool.njobs, _pool.nused;
  if (_pool.self) write, format=f2, _pool.nself;
  write, format="%s", f3;
  write, format=f4, _pool.tsow(1), _pool.tsow(2), _pool.tsow(3);
  write, format=f5, _pool.twork(1), _pool.twork(2), _pool.twork(3);
  write, format=f6, _pool.treap(1), _pool.treap(2), _pool.treap(3);
  write, format=f7, _pool.navg;
}
