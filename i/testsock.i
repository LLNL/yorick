/* testsock.i
 * Tests of socket API.
 */
/* Copyright (c) 2015, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func testsock(port)
{
  listener = socket((port?port:0), accept_cb);
  write, format="%s\n", "listening - now start client yorick, then type:";
  write, format="%s\n", "#include \"testsock.i\"";
  write, format="testclient, %ld;\n", listener.port;
}

func testclient(port, host)
{
  sock = socket((host?host:noop(-)), port);
  n = test_data(*);
  for (i=1,fail=0 ; i<=n ; ++i) {
    s = test_data(noop(i));
    if (i & 1) {
      write, format="socket client send %ld...", i;
      if (socksend(sock, s) != sizeof(s)) {
        close, sock;
        error, "socket test client died on test "+totxt(i);
      }
      write, format="%s\n", "";
    } else {
      r = s;
      r(..) *= 0;
      write, format="socket client recv %ld...", i;
      if (sockrecv(sock, r) != sizeof(r)) {
        close, sock;
        error, "socket test client died on test "+totxt(i);
      }
      write, format="%s\n", "";
      if (anyof(r != s)) {
        write, format="WARNING - socket client recv failed on test %ld\n", i;
        fail += 1;
      }
    }
  }
  write, format="socket client recv %s...", "final";
  r = array(0, 10);
  if (sockrecv(sock, r)!=sizeof(r)/2 || anyof(r(1:5)!=indgen(5))) {
    write, format="%s\n", "WARNING - socket client recv failed on final test";
    fail += 1;
  }
  write, format="%s\n", "";
  close, sock;
  if (fail) error, "socket test client recv failure count: "+totxt(fail);
  write, format="%s\n", "SUCCESS - socket test client side passed all tests";
}

test_state = save(reset, recv, step, sock, req, irep, imax, fail);
func recv(sock)   /* recv callback, do as little as possible */
{
  if (catch(-1)) {
    write, "FATAL - socket server recv callback closing sock";
    close, sock;
    return;
  }
  sockrecv, sock, use(req);
  after, 0., test_state, "step";  /* defer most work */
}
func reset(s)
{
  use, sock, req, irep, imax, fail;
  sock = s
  req = test_data(1);
  req(..) = 0;
  irep = 2;
  imax = test_data(*);
  fail = 0;
  write, format="\n%s\n", "connection established, listener shut down";
}
func step
{
  use, sock, req, irep, imax, fail;
  write, format="socket server recv %ld\n", irep-1;
  if (anyof(test_data(irep-1) != req)) {
    write, format="WARNING - socket server recv failed on test %ld\n", irep-1;
    fail += 1;
  }
  if (irep <= imax) {
    write, format="socket server send %ld...", irep;
    socksend, sock, test_data(noop(irep));
    write, format="%s\n", "";
  }
  if (irep < imax) {  /* at least one more recv to wait for */
    req = test_data(irep+1);
    irep += 2;
    return;
  }
  /* send final partial message to client */
  write, format="socket server send %s...", "final";
  socksend, sock, indgen(5);
  write, format="%s\n", "";
  close, sock;
  req = [];
  if (fail) error, "socket test server recv failure count: "+totxt(fail);
  write, format="%s\n", "SUCCESS - socket test server side passed all tests";
  maybe_prompt;  /* why doesn't this work? */
  write, format="%s", "> ";
}
sock = req = irep = imax = fail = [];
test_state = restore(test_state);

recv_cb = closure(test_state, "recv");
func accept_cb(sock) {
  test_state, reset, sock(recv_cb);
  close, sock;
}

x = indgen(0:1738);
test_data = save(,x, ,char(x), ,short(x(::-1)+x(-,)), ,int(x-x(-,)),
                 ,float(x-31), ,double(x*x(-,)), ,(x-51)-1i*(x-432));
