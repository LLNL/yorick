/* usock.c
 * low level simple socket interface
 */
/* Copyright (c) 2015, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef NO_SOCKETS

#include "config.h"
#include "pstdlib.h"
#include "playu.h"
#include "play.h"

/* sprintf, strtod, memset, close */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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

/* socket and network functions used:
 *   getaddrinfo, freeaddrinfo, getnameinfo <netdb.h>
 *   socket, bind, listen, accept, connect, send, recv, getsockname
 *      <sys/socket.h>
 * socket and network structs and typedefs used:
 *   addrinfo <netdb.h>
 *   sockaddr, sockaddr_storage <sys/socket.h>
 *   socklen_t <netdb.h> and <sys/socket.h>
 * socket and network macros used:
 *   SOCK_STREAM, AF_UNSPEC <sys/socket.h>
 *   AI_PASSIVE, NI_NUMERICHOST, NI_NUMERICSERV <netdb.h>
 *   NI_MAXHOST(1025), NI_MAXSERV(32) <netdb.h>, but not POSIX
 *
 * <sys/types.h> needed in some legacy BSD implementations (linux bind manpage)
 * <arpa/inet.h> for htonl, htns, ntohl, ntohs byte swappers (unused here)
 * <netinet/in.h> for in_addr, sockaddr_in (unused here)
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

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
  int fd;
  char *peer;
  psckt_cb_t *callback;  /* int psckt_cb_t(psckt_t *sock, void *ctx); */
  void *ctx;
  psckt_t *next;
};

/* socket cleanup is crucial to release system resources */
static psckt_t *psckt_list = 0;  /* for on_quit */

static psckt_t *psckt_setup(psckt_t *sock, void *ctx, psckt_cb_t *callback);
static int psckt_shutdown(psckt_t *sock);
static void psckt_cb_manager(void *vsock);
static struct addrinfo *psckt_get_ailist(const char *addr, int port,
                                         int passive);

static psckt_t *
psckt_setup(psckt_t *sock, void *ctx, psckt_cb_t *callback)
{
  sock->callback = callback;
  sock->ctx = ctx;
  sock->next = psckt_list;
  psckt_list = sock;
  if (callback) u_event_src(sock->fd, psckt_cb_manager, sock);
  return sock;
}

static int
psckt_shutdown(psckt_t *sock)
{
  psckt_t *s, *prev;
  if (sock->fd != -1) {
    if (sock->callback) u_event_src(sock->fd, 0, sock);
    close(sock->fd);
    sock->fd = -1;
  }
  for (s=psckt_list,prev=0 ; s ; prev=s,s=s->next) if (s == sock) break;
  if (prev) prev->next = sock->next;
  else psckt_list = sock->next;
  if (sock->peer) {
    p_free(sock->peer);
    sock->peer = 0;
  }
  sock->callback = 0;
  sock->ctx = 0;
  return -1;   /* convenience for send, recv */
}

static void
psckt_cb_manager(void *vsock)
{
  psckt_t *sock = vsock;
  if (sock->callback) sock->callback(sock, sock->ctx);
}

static struct addrinfo *
psckt_get_ailist(const char *addr, int port, int passive)
{
  struct addrinfo hints, *ailist;
  char sport[NI_MAXSERV];
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
  int lfd;
  int port = *pport;
  psckt_t *listener = p_malloc(sizeof(psckt_t));
  struct addrinfo *ai, *ailist = listener? psckt_get_ailist(0, port, 1) : 0;
  if (!ailist) {
    if (listener) p_free(listener);
    return 0;
  }

  for (ai=ailist ; ai ; ai=ai->ai_next) {
    lfd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
    if (lfd == -1) continue;
    if (!bind(lfd, ai->ai_addr, ai->ai_addrlen)) break;
    close(lfd);
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
      close(lfd);
      p_free(listener);
      return 0;
    }
    pport[0] = port = strtod(sport, 0);
  }

  if (listen(lfd, 10)) {
    close(lfd);
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
  int sfd;

  *ppeer = 0;
  if (listener->peer || !sock) {  /* listeners have no peer */
    if (sock) p_free(sock);
    return 0;
  }

  sfd = accept(listener->fd, psad, &lsad);
  if (sfd==-1 || getnameinfo(psad, lsad, host, NI_MAXHOST, 0, 0, 0)) {
    close(sfd);
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
  int sfd;
  psckt_t *sock = p_malloc(sizeof(psckt_t));
  struct addrinfo *ai, *ailist = sock? psckt_get_ailist(addr, port, 0) : 0;
  if (!ailist) {
    if (sock) p_free(sock);
    return 0;
  }

  for (ai=ailist ; ai ; ai=ai->ai_next) {
    sfd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
    if (sfd == -1) continue;
    if (connect(sfd, ai->ai_addr, ai->ai_addrlen) != -1) break;
    close(sfd);
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
  while (len > 0) {
    n = recv(sock->fd, cmsg, len, 0);
    if (n < 0) return (psckt_shutdown(sock), -1L);
    if (n == 0) {  /* socket closed */
      /* socket may close for recv but not for send, not full shutdown yet */
      if (sock->callback) { /* ...but remove from event sources immediately */
        sock->callback = 0;
        u_event_src(sock->fd, 0, sock);
      }
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
