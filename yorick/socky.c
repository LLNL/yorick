/* socky.c
 * implementation of socket interface
 */
/* Copyright (c) 2015 David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "pstdlib.h"
#include "play.h"
#include "yapi.h"
#include <stdio.h>
#include <string.h>

PLUG_API int psckt_0close(void);  /* for ascio.c Y_close */

extern ybuiltin_t Y_socket, Y_socksend, Y_sockrecv;

/* other possibility is "localhost", but that requires a host lookup? */
#define YS_LOOPBACK_NAME "127.0.0.1"

static void ys_on_quit(void);
static void ys_socksr(int sending, int argc);
static void ys_callback(psckt_t *sock, void *ctx);

/* ------------------------------------------------------------------------ */
/* socket objects */

static void ys_on_free(void *uo);
static void ys_on_extract(void *uo, char *name);
static void ys_on_eval(void *uo, int nargs);
static y_userobj_t ys_uops =
  { "socket_object", ys_on_free, 0, ys_on_eval, ys_on_extract, 0 };

typedef struct ys_data_t ys_data_t;
struct ys_data_t {
  yo_ops_t *ops;
  psckt_t *socket;  /* from psckt_listen, pskt_accept, or psckt_connect */
  char *peer;       /* host at other end of socket, 0 for listener */
  int port;         /* port number for socket */
  void *use, *cb;   /* uses of socket and callback function, from yget_use */
};

static void
ys_on_free(void *uo)
{
  ys_data_t *sock = uo;
  psckt_t *socket = sock->socket;
  char *peer = sock->peer;
  void *use = sock->use;
  void *cb = sock->cb;
  sock->socket = 0;
  sock->peer = 0;
  sock->use = 0;
  sock->cb = 0;
  if (socket) psckt_close(socket);
  if (peer) p_free(peer);
  if (use) ydrop_use(use);
  if (cb) ydrop_use(cb);
}

int
psckt_0close(void)
{
  /* if top of stack is socket, close it and return 0, else return 1 */
  if (yget_obj(0,0) != ys_uops.type_name) return 1;
  ys_on_free(yget_obj(0, &ys_uops));
  return 0;
}

/* ------------------------------------------------------------------------ */

static int ys_initialized = 0;
static void
ys_on_quit(void)
{
  /* close any open sockets to release critical system resources */
  psckt_close(0);
}

void
Y_socksend(int argc)
{
  ys_socksr(1, argc);
}

void
Y_sockrecv(int argc)
{
  ys_socksr(0, argc);
}

void
Y_socket(int argc)
{
  int tid = (argc>0)? yarg_func(0) : 0;
  char *host = 0;
  int port = -1;
  ys_data_t *s = 0;
  void *cb = 0;

  if (tid) {
    if (tid == 3)
      y_error("socket() callback argument cannot be an autoload");
    cb = yget_use(0);
    yarg_drop(1);
    argc -= 1;
  }

  if (argc<1 || argc>2) {
    if (cb) ydrop_use(cb);
    y_error("socket() expecting one or two arguments");
  }
  tid = yarg_typeid(argc-1);

  if (tid == Y_RANGE) {
    long mnxs[3];
    int flag = yget_range(argc-1, mnxs);
    if (flag != (Y_PSEUDO | Y_MIN_DFLT | Y_MAX_DFLT)) {
      if (cb) ydrop_use(cb);
      y_error("socket() expects - or string as host address argument");
    }
  } else if (tid == Y_STRING) {
    host = ygets_q(argc-1);
  } else {
    if (tid<Y_CHAR || tid>Y_LONG) {
      if (cb) ydrop_use(cb);
      y_error("socket(port) expects integer port number");
    }
    port = (int)ygets_l(argc-1);
    if (port<0 || port>65535) {
      if (cb) ydrop_use(cb);
      y_error("socket(port) expects port number in range 0-65535");
    }
  }

  if (port >= 0) {
    /* create a listener */
    if (argc > 1) {
      if (cb) ydrop_use(cb);
      y_error("socket(host, port) expects - or string host address");
    }
    s = ypush_obj(&ys_uops, sizeof(ys_data_t));
    s->peer = 0;
    s->port = port;
    s->cb = cb;
    s->socket = psckt_listen(&s->port, s, cb? ys_callback : 0);
    if (!s->socket) {
      if (cb) ydrop_use(cb);
      y_errorn("socket failed to create listener on port %ld", (long)port);
    }

  } else {
    /* connect to host:port */
    if (argc < 2) {
      if (cb) ydrop_use(cb);
      y_error("socket() needs port argument to connect to host address");
    }
    tid = yarg_typeid(0);
    if (tid<Y_CHAR || tid>Y_LONG) {
      if (cb) ydrop_use(cb);
      y_error("socket(host, port) expects integer port number");
    }
    port = (int)ygets_l(0);
    if (port<0 || port>65535) {
      if (cb) ydrop_use(cb);
      y_error("socket(host, port) expects port number in range 0-65535");
    }
    s = ypush_obj(&ys_uops, sizeof(ys_data_t));
    s->peer = 0;
    s->port = port;
    s->cb = cb;
    if (!host) host = YS_LOOPBACK_NAME;
    s->socket = psckt_connect(host, port, s, cb? ys_callback : 0);
    if (!s->socket) {
      if (cb) ydrop_use(cb);
      y_errorq("socket failed to connect to host %s", host);
    }
    s->peer = p_strcpy(host);
  }
  s->use = yget_use(0);

  /* make sure all sockets closed before exit */
  if (!ys_initialized) {
    ycall_on_quit(ys_on_quit);
    ys_initialized = 1;
  }
}

static void
ys_on_eval(void *uo, int nargs)
{
  int tid = (nargs>0)? yarg_func(0) : 0;
  ys_data_t *listener = uo,  *s = 0;
  void *cb = 0;
  if (listener->peer)
    y_error("attempt to eval a data transport socket, not a listener");
  if (nargs != 1 || !(tid || yarg_nil(0)))
      y_error("listener() call expecting one nil or callback argument");
  if (tid) {
    if (tid == 3)
      y_error("socket listener() callback argument cannot be an autoload");
    cb = yget_use(0);
    yarg_drop(1);
  }
  s = ypush_obj(&ys_uops, sizeof(ys_data_t));
  s->peer = 0;
  s->port = listener->port;
  s->cb = cb;
  s->socket = psckt_accept(listener->socket, &s->peer, s, cb? ys_callback : 0);
  s->use = yget_use(0);
}

static void
ys_on_extract(void *uo, char *name)
{
  ys_data_t *s = uo;
  if (!strcmp(name, "port")) {
    ypush_long(s->port);
  } else if (!strcmp(name, "peer")) {
    char **q = ypush_q(0);
    q[0] = p_strcpy(s->peer);
  } else {
    y_errorq("socket has no such method as %s", name);
  }
}

static void
ys_socksr(int sending, int argc)
{
  ys_data_t *sock = (argc==2)? yget_obj(1, &ys_uops) : 0;
  int tid = (sock && argc>1)? yarg_typeid(0) : -1;
  void *data = 0;
  long len = 0, nbytes = -1;
  static long sizes[Y_COMPLEX-Y_CHAR+1] =
    {1, sizeof(short), sizeof(int), sizeof(long),
     sizeof(float), sizeof(double), 2*sizeof(double)};
  if (!sock || !sock->peer)
    y_error("socksend/recv expecting data transport socket and data arguments");
  if (!sock->socket)
    y_error("socksend/recv data transport socket closed?");
  if (tid<Y_CHAR || tid>Y_COMPLEX)
    y_error("socksend/recv data must be numeric or char type");
  data = ygeta_any(0, &len, 0, 0);
  len *= sizes[Y_CHAR + tid];  /* length in bytes */
  if (!sending) nbytes = psckt_recv(sock->socket, data, len);
  else nbytes = psckt_send(sock->socket, data, len)? -1L : len;
  ypush_long(nbytes);
  if (nbytes<len && yarg_subroutine()) {
    if (sending)
      y_error("socksend failed, socket shut down");
    else if (nbytes < 0)
      y_error("sockrecv failed, socket shut down");
    else
      y_error("sockrecv other end of socket closed before recv complete");
  }
}

/* ------------------------------------------------------------------------ */

static long ys_icallback=-1L, ys_isocket=-1L, ys_icaller=-1L;

static void
ys_callback(psckt_t *sock, void *ctx)
{
  ys_data_t *s = ctx;
  if (ys_icallback < 0) ys_icallback = yfind_global("_socket_callback",0);
  if (ys_isocket < 0) ys_isocket = yfind_global("_socket_socket",0);
  if (ys_icaller < 0) ys_icaller = yfind_global("_socket_caller",0);
  /* set global variables for _socket_caller */
  ykeep_use(s->cb);
  yput_global(ys_icallback, 0);
  yarg_drop(1);
  ykeep_use(s->use);
  yput_global(ys_isocket, 0);
  yarg_drop(1);
  /* run _socket_caller - must run immediately to avoid event looping */
  ypush_global(ys_icaller);
  ytask_run(0);
}

/* ------------------------------------------------------------------------ */
