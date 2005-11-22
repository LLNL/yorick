/*
 * $Id: upoll.c,v 1.2 2005-11-22 17:36:40 dhmunro Exp $
 *
 * UNIX poll function implemented with select
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*
 * WARNING
 * This is not a complete implementation of poll(), only enough for
 * using the function to make multiple input sources possible.
 * In particular, the special features of poll() relating to streams
 * are not implemented, and POLLOUT is not implemented at all.
 */

#ifndef USE_SELECT
/* normally this file will be included from uevent.c, in which
 * case USE_SELECT will be set, and these lines skipped */
#include "config.h"
#include "upoll.h"
#include "pstdlib.h"
#endif

#ifdef HAVE_SYS_SELECT_H
/* sys/types.h workaround for MacOS X 10.3 header bug */
# include <sys/types.h>
# include <sys/select.h>
#else
/* struct timeval should be in sys/time.h, select often there too */
# ifndef NO_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
# ifdef NEED_SELECT_PROTO
#  include <sys/types.h>
   extern int select(int, fd_set *, fd_set *, fd_set *, struct timeval *);
# endif
#endif

#ifdef NO_POLLING
/* if timeout, report nothing ready
 * otherwise, report stdin ready to read (if it was requested)
 * - this causes subsequent read to block, so effect is as for
 *   a non-event driven program that waits for stdin
 */
#undef select
#define select fake_select
static int fake_select(int mx, int *rd, int *wr, int *er, void *tv);
/* ARGSUSED */
static int 
fake_select(int mx, int *rd, int *wr, int *er, void *tv)
{
  int ready = (!tv && rd[0]&1);
  if (ready) {
    int i;
    mx = (mx-1)/(8*sizeof(int));
    rd[0] = 1;
    er[0] = 0;
    for (i=1 ; i<=mx ; i++) rd[i] = er[i] = 0;
  }
  return ready;
}
#endif

static unsigned int *poll_mask = 0;
static int poll_n = 0;

int
u__poll(struct pollfd *fds, unsigned long int nfds, int timeout)
{
  int i, n, bit, word;
  int maxfd = -1;
  struct timeval tm, *ptm;

  for (i=0 ; i<nfds ; i++) if (fds[i].fd>maxfd) maxfd = fds[i].fd;
  if (maxfd+1 > poll_n*(8*sizeof(unsigned int))) {
    n = poll_n? (poll_n<<1) : 8;
    poll_mask = p_realloc(poll_mask, 2*n*sizeof(unsigned int));
    poll_n = n;
  }
  n = maxfd<0? -1 : maxfd/(8*sizeof(unsigned int));
  for (i=0 ; i<=n ; i++) poll_mask[i]= poll_mask[poll_n+i]= 0;

  for (i=0 ; i<nfds ; i++) {
    if (fds[i].fd>=0) {
      word = fds[i].fd/(8*sizeof(unsigned int));
      bit = 1 << (fds[i].fd%(8*sizeof(unsigned int)));
      if (fds[i].events&(POLLIN|POLLPRI)) poll_mask[word]|= bit;
      poll_mask[poll_n+word]|= bit;
      fds[i].revents = 0;
    } else {
      fds[i].revents = POLLNVAL;
    }
  }

  if (timeout>=0) {
    ptm = &tm;
    tm.tv_sec = timeout/1000;
    tm.tv_usec = 1000*(timeout%1000);
  } else {
    ptm = 0;
  }

  /* select may return EBADF, poll cannot
   * select cannot return EAGAIN, poll may
   * the critical EINTR errno is the same for either
   * return value is same for select as poll */
  n = select(maxfd+1, (void *)poll_mask, (void *)0,
             (void *)(poll_mask+poll_n), ptm);

  if (n>0) {
    for (i=0 ; i<nfds && n>0 ; i++)
      if (fds[i].fd>=0) {
        word = fds[i].fd/(8*sizeof(unsigned int));
        bit = 1 << (fds[i].fd%(8*sizeof(unsigned int)));
        if (poll_mask[word] & bit) fds[i].revents |= POLLIN;
        if (poll_mask[poll_n+word] & bit) fds[i].revents |= POLLERR;
      }
  }

  return n;
}
