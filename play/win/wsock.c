/* usock.c
 * low level simple socket interface
 */
/* Copyright (c) 2015, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef NO_SOCKETS

/* must include these BEFORE windows.h, else it includes winsock version 1! */
#include <winsock2.h>
#include <ws2tcpip.h>

#include "config.h"
#include "pstdlib.h"
#include "playw.h"
#include "play.h"

/* sprintf, strtod, memset */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* socket references:
 * http://pubs.opengroup.org/onlinepubs/9699919799/nframe.html
 *   IEEE Std 1003.1, 2013 Edition   System Interfaces
 * https://msdn.microsoft.com/en-us/library/windows/desktop/ms741416%28v=vs.85%29.aspx
 *   Winsock Reference
 * FreeBSD getaddrinfo manpage
 *
 * Behavior when port "0" specified to bind:
 * - Windows bind description: For TCP/IP, service provider assigns unique
 *   port to the application from the dynamic client port range (49152-65535).
 *   - suggests only one per application?
 * - UNIX hard to find definitive statement, but getaddrinfo(0, "0", ...)
 *   suggests unique port number will be chosen at bind time
 *   http://docs.freebsd.org/44doc/psd/20.ipctut/paper.pdf
 *   http://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers
 *   http://www.unix.com/ip-networking/135087-tcp-port-0-usage.html
 */

/* matches BSD socket implementation except for callback driven recv
 * - windows does not permit sockets in MsgWaitForMultipleObjects,
 *   therefore impossible to put socket in list with other inputs
 * - like wspawn.c, require separate thread to monitor socket, which
 *   signals an event which can be part of main MsgWaitForMultipleObjects
 * - want to emulate UNIX select/poll behavior as closely as possible
 * - fundamental problem:
 *   select() (wait for socket) must happen on socket thread
 *   recv() must happen on yorick thread
 *   but undefined behavior to call both at once, so must prevent that
 *
 * situations:
 * 1. socket participates in main event loop
 * 2. request blocking recv
 * 3. recv indicates socket is closed for further reads
 * 4. closing socket
 *
 * socket thread:
 *   waitevent(released)
 *   while not finished:
 *     select()
 *     signalandwait(ready, released)
 *
 * prepoll:
 *   if not waiting:
 *     waiting = 1
 *     setevent(released)   <auto-reset when socket thread wakes up>
 *   ...proceeds to wait on ready or any other input event...
 *
 * callback:
 *   ...ready event detected and must be serviced...
 *   waiting = 0
 *   resetevent(ready)   <manual-reset>
 *   ...ready event must be manual reset because unclear what happens
 *   ...when multiple events simultaneously trigger MsgWaitForMultipleObjects
 *
 * recv:
 *   ...may be called while servicing non-ready event...
 *   if waiting:
 *     waitevent(ready)
 *     waiting = 0
 *     resetevent(ready)
 *   recv()
 *   if closed:
 *     finished = 1    ...prevent any further select() calls
 *
 * close:
 *   finished = 1
 *   closesocket()     ...hopefully returns from select if waiting
 *   setevent(released)
 *   waitforthread()   ...but no recovery possible if this fails?
 *
 * possible race conditions?
 *
 */

/* NI_MAXHOST, NI_MAXSERVE not part of POSIX 1003.1 standard
 * see linux getnameinfo manpage, these values apparently "universal"
 */
#ifndef NI_MAXHOST
#  define NI_MAXHOST 1025
#endif
#ifndef NI_MAXSERV
#  define NI_MAXSERV 32
#endif

struct psckt_t {
  SOCKET fd;
  char *peer;
  psckt_cb_t *callback;  /* int psckt_cb_t(psckt_t *sock, void *ctx); */
  void *ctx;
  /* if callback, a thread is started to wait for input on the socket
   * the wait thread and the yorick thread share ready and released events:
   * - ready: set by thread when input ready on socket (wakes up main)
   * - released: set by main when socket has been read (wakes up thread)
   */
  HANDLE ready, released, thread;
  int finished, waiting;
  psckt_t *next;
};

/* socket cleanup is crucial to release system resources */
static psckt_t *psckt_list = 0;  /* for on_quit */

static psckt_t *psckt_setup(psckt_t *sock, void *ctx, psckt_cb_t *callback);
static void psckt_stop_thread(psckt_t *sock);
static int psckt_shutdown(psckt_t *sock);
static void psckt_cb_manager(void *vsock);
static void psckt_prepoller(void *vsock);
static struct addrinfo *psckt_get_ailist(const char *addr, int port,
                                         int passive);
static int psckt_initialized = 0;

static DWORD __stdcall psckt_wait_thread(LPVOID lpvThreadParam);

static psckt_t *
psckt_setup(psckt_t *sock, void *ctx, psckt_cb_t *callback)
{
  sock->callback = callback;
  sock->ctx = ctx;
  sock->ready = sock->released = sock->thread = INVALID_HANDLE_VALUE;
  sock->finished = sock->waiting = 0;
  sock->next = psckt_list;
  psckt_list = sock;
  if (callback) {
    DWORD thread_id;
    sock->ready = CreateEvent(0, 1, 0, 0);     /* manual reset */
    sock->released = CreateEvent(0, 0, 0, 0);  /* auto reset */
    sock->thread = CreateThread(NULL, 0, psckt_wait_thread, (LPVOID)sock,
                                0, &thread_id);
    /* closing the thread handle does not terminate the thread, it merely
     * permits the thread to fully terminate whenever psckt_wait_thread
     * returns
     * however, since the thread accesses sock->finished, ready, released,
     * we cannot free sock until the thread finishes
     */
    w_add_input(sock->ready, psckt_cb_manager, sock);
    w_prepoll(psckt_prepoller, sock, 0);
  }
  return sock;
}

static void
psckt_stop_thread(psckt_t *sock)
{
  sock->finished = 1;
  if (sock->ready != INVALID_HANDLE_VALUE) {
    /* prevent yorick thread (this thread) from waiting for this socket */
    w_add_input(sock->ready, 0, sock);
    w_prepoll(psckt_prepoller, sock, 1);
    sock->callback = 0;
    sock->ctx = 0;
  }
  /* this sequence probably will cause psckt_wait_thread to complete
   * however, nothing guarantees that shutdown will cause select to return,
   * so it remains possible that thread is blocked in select after this
   * therefore wait until closesocket to reap thread
   */
  if (sock->fd!=INVALID_SOCKET && sock->released!=INVALID_HANDLE_VALUE) {
    /* wait thread may be blocked on sock->fd select or sock->released wait */
    SetEvent(sock->released);        /* wake up wait thread, first way */
    shutdown(sock->fd, SD_RECEIVE);  /* wake up wait thread, second way */
  }
}

static int
psckt_shutdown(psckt_t *sock)
{
  psckt_t *s, *prev;
  if (!sock->finished) psckt_stop_thread(sock);
  if (sock->fd != INVALID_SOCKET) {
    closesocket(sock->fd);     /* hope select() always returns after this? */
    sock->fd = INVALID_SOCKET;
  }
  if (sock->thread != INVALID_HANDLE_VALUE) {
    /* wait for half second for thread to complete, then nuke it */
    if (WaitForSingleObject(sock->thread, 500) != WAIT_OBJECT_0)
      TerminateThread(sock->thread, 6);
    CloseHandle(sock->thread);
    sock->thread = INVALID_HANDLE_VALUE;
  }
  if (sock->ready != INVALID_HANDLE_VALUE) {
    CloseHandle(sock->ready);
    sock->ready = INVALID_HANDLE_VALUE;
  }
  if (sock->released != INVALID_HANDLE_VALUE) {
    CloseHandle(sock->released);
    sock->released = INVALID_HANDLE_VALUE;
  }
  for (s=psckt_list,prev=0 ; s ; prev=s,s=s->next) if (s == sock) break;
  if (prev) prev->next = sock->next;
  else psckt_list = sock->next;
  if (sock->peer) {
    p_free(sock->peer);
    sock->peer = 0;
  }
  return -1;   /* convenience for send, recv */
}

static void
psckt_prepoller(void *vsock)
{
  psckt_t *sock = vsock;
  if (!sock->waiting) {
    sock->waiting = 1;
    SetEvent(sock->released);
  }
}

static void
psckt_cb_manager(void *vsock)
{
  psckt_t *sock = vsock;
  if (sock->callback) {
    sock->waiting = 0;
    ResetEvent(sock->ready);
    sock->callback(sock, sock->ctx);
  } else {
    psckt_stop_thread(sock);
  }
}

static DWORD __stdcall
psckt_wait_thread(LPVOID lpvThreadParam)
{
  psckt_t *sock = lpvThreadParam;
  fd_set rfds[1], xfds[1];
  int n;
  FD_ZERO(rfds);
  FD_ZERO(xfds);
  if (WaitForSingleObject(sock->released, INFINITE) != WAIT_OBJECT_0)
    return 5;
  while (!sock->finished) {
    FD_SET(sock->fd, rfds);
    FD_SET(sock->fd, xfds);
    n = select(1, rfds, 0, xfds, 0);
    if (sock->finished) break;
    if (n == SOCKET_ERROR) return 1;
    if (FD_ISSET(sock->fd, xfds)) return 2;
    if (!FD_ISSET(sock->fd, rfds)) return 3;
    if (SignalObjectAndWait(sock->ready, sock->released, INFINITE, 0)
        != WAIT_OBJECT_0) return 4;
  }
  return 0;
}

static struct addrinfo *
psckt_get_ailist(const char *addr, int port, int passive)
{
  struct addrinfo hints, *ailist;
  char sport[NI_MAXSERV];

  if (!psckt_initialized) {
    static WSADATA wsaData;
    if (WSAStartup(MAKEWORD(2,2), &wsaData)) return 0;
    psckt_initialized = 1;
  }

  /* PF_xxxx versus AF_xxxx is protocol family versus address family
   * - the PF macros are not part of POSIX 1003.1 standard
   */
  sprintf(sport, "%d", port);

  /* FreeBSD getaddrinfo manpage clearer than linux manpage */
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  if (passive) hints.ai_flags = AI_PASSIVE;  /* will call bind, not connect */
  else if (!addr) addr = "127.0.0.1";     /* more certain than "localhost"? */
  if (getaddrinfo(addr, sport, &hints, &ailist))
    return 0;
  return ailist;
}

psckt_t *
psckt_listen(int *pport, void *ctx, psckt_cb_t *callback)
{
  SOCKET lfd;
  int port = *pport;
  psckt_t *listener = p_malloc(sizeof(psckt_t));
  struct addrinfo *ai, *ailist = listener? psckt_get_ailist(0, port, 1) : 0;
  if (!ailist) {
    if (listener) p_free(listener);
    return 0;
  }

  for (ai=ailist ; ai ; ai=ai->ai_next) {
    lfd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
    if (lfd == INVALID_SOCKET) continue;
    if (!bind(lfd, ai->ai_addr, ai->ai_addrlen)) break;
    closesocket(lfd);
  }
  if (ailist) freeaddrinfo(ailist);
  if (lfd < 0) {
    p_free(listener);
    return 0;
  }

  if (port == 0) {
    struct sockaddr_storage sad;
    socklen_t lsad = sizeof(struct sockaddr_storage);
    struct sockaddr *psad = (struct sockaddr *)&sad;
    char sport[NI_MAXSERV];
    if (getsockname(lfd, psad, &lsad) ||
        getnameinfo(psad, lsad, 0, 0, sport, NI_MAXSERV, NI_NUMERICSERV)) {
      closesocket(lfd);
      p_free(listener);
      return 0;
    }
    pport[0] = port = strtol(sport, 0, 10);
  }

  if (listen(lfd, 10)) {
    closesocket(lfd);
    p_free(listener);
    return 0;
  }
  listener->fd = lfd;
  listener->peer = 0;  /* listener has no peer, could usefully be port */
  return psckt_setup(listener, ctx, callback);
}

psckt_t *
psckt_accept(psckt_t *listener, char **ppeer, void *ctx, psckt_cb_t *callback)
{
  struct sockaddr_storage sad;
  socklen_t lsad = sizeof(struct sockaddr_storage);
  struct sockaddr *psad = (struct sockaddr *)&sad;
  char host[NI_MAXHOST];
  psckt_t *sock = p_malloc(sizeof(psckt_t));
  SOCKET sfd, lfd = sock->fd;

  *ppeer = 0;
  if (listener->peer || !sock) {  /* listeners have no peer */
    if (sock) p_free(sock);
    return 0;
  }

  if (listener->callback && listener->waiting) {
    if (WaitForSingleObject(listener->ready, INFINITE) != WAIT_OBJECT_0)
      psckt_stop_thread(listener);
    listener->waiting = 0;
    if (listener->ready != INVALID_HANDLE_VALUE)
      ResetEvent(listener->ready);
  }

  sfd = accept(listener->fd, psad, &lsad);
  if (sfd==INVALID_SOCKET ||
      getnameinfo(psad, lsad, host, NI_MAXHOST, 0, 0, 0)) {
    closesocket(sfd);
    p_free(sock);
    return 0;
  }

  sock->fd = sfd;
  sock->peer = *ppeer = p_strcpy(host);
  return psckt_setup(sock, ctx, callback);
}

psckt_t *
psckt_connect(const char *addr, int port, void *ctx, psckt_cb_t *callback)
{
  SOCKET sfd;
  psckt_t *sock = p_malloc(sizeof(psckt_t));
  struct addrinfo *ai, *ailist = sock? psckt_get_ailist(addr, port, 0) : 0;
  if (!ailist) {
    if (sock) p_free(sock);
    return 0;
  }

  for (ai=ailist ; ai ; ai=ai->ai_next) {
    sfd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
    if (sfd == INVALID_SOCKET) continue;
    if (connect(sfd, ai->ai_addr, ai->ai_addrlen) != SOCKET_ERROR) break;
    closesocket(sfd);
  }
  if (!ai) {
    p_free(sock);
    sock = 0;
  }
  freeaddrinfo(ailist);
  if (!sock) return 0;

  sock->fd = sfd;
  sock->peer = p_strcpy(addr);
  return psckt_setup(sock, ctx, callback);
}

/* return value <0 for error, <len means socket recv side closed */
long
psckt_recv(psckt_t *sock, void *msg, long len)
{
  char *cmsg = msg;
  int n;
  if (len>0 && sock->callback && sock->waiting) {
    if (WaitForSingleObject(sock->ready, INFINITE) != WAIT_OBJECT_0)
      psckt_stop_thread(sock);
    sock->waiting = 0;
    if (sock->ready != INVALID_HANDLE_VALUE)
      ResetEvent(sock->ready);
  }
  while (len > 0) {
    n = recv(sock->fd, cmsg, len, 0);
    if (n < 0) return (long)psckt_shutdown(sock);
    if (n == 0) {  /* socket closed */
      /* socket may close for recv but not for send, not full shutdown yet
       * ...but remove from event sources and kill wait thread immediately
       */
      if (sock->callback) psckt_stop_thread(sock);
      break;
    }
    cmsg += n;
    len -= n;
  }
  return cmsg - (const char *)msg;
}

int
psckt_send(psckt_t *sock, const void *msg, long len)
{
  const char *cmsg = msg;
  int n;
  while (len > 0) {
    n = send(sock->fd, cmsg, len, 0);
    if (n < 0) return psckt_shutdown(sock);
    cmsg += n;
    len -= n;
  }
  return 0;
}

void
psckt_close(psckt_t *sock)
{
  if (sock) {
    psckt_shutdown(sock);
    p_free(sock);
  } else {
    while (psckt_list) psckt_shutdown(psckt_list);
    if (psckt_initialized) WSACleanup();
  }
}

#else

psckt_t *
psckt_listen(int *pport, void *ctx, psckt_cb_t *callback)
{
  return 0;
}
psckt_t *
psckt_accept(psckt_t *sock, char **peer, void *ctx, psckt_cb_t *callback)
{
  return 0;
}
psckt_t *
psckt_connect(const char *addr, int port, void *ctx, psckt_cb_t *callback)
{
  return 0;
}
long
psckt_recv(psckt_t *sock, void *msg, long len)
{
  return -1L;
}
int
psckt_send(psckt_t *sock, const void *msg, long len)
{
  return -1;
}
void
psckt_close(psckt_t *sock)
{
}

#endif
