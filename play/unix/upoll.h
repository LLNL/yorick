/*
 * $Id: upoll.h,v 1.1 2005-09-18 22:05:40 dhmunro Exp $
 *
 * UNIX poll.h header, when poll() must be implemented using select
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

struct pollfd {
  int fd;         /* file descriptor */
  short events;   /* events desired */
  short revents;  /* events returned */
};

/* events or revents */
#define POLLIN          01              /* ready to read */
#define POLLPRI         02              /* urgently ready to read */
#define POLLOUT         04              /* fd is writable */

/* revents only */
#define POLLERR         010             /* error */
#define POLLHUP         020             /* hangup */
#define POLLNVAL        040             /* fd not open */

/* timeout is in milliseconds, -1 to wait forever
 * returns number of fds with non-zero revents,
 *   or -1 and sets errno to
 *     EAGAIN, EFAULT, EINTR (signal during poll), EINVAL */
extern int u__poll(struct pollfd *fds, unsigned long int nfds, int timeout);
