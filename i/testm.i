/*
 * $Id: testm.i,v 1.1 2005-09-18 22:06:12 dhmunro Exp $
 * Certify functions in fft.i and matrix.i
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func testm
{
  elapsed= old_elapsed= [0., 0., 0.];
  write, "testing fft routines...";
  timer, old_elapsed;
  force2d = 0;
  fft_test, 95;
  fft_test, 32;
  fft_test, 12;
  fft_test, 1024;
  for (i=0 ; i<32 ; i++) fft_test, 4096;
  force2d = 1;  fft_test, 128;  force2d = 0;
  timer, elapsed;
  timer_print, "FFT elapsed time", elapsed-old_elapsed;
  write, "testing LU decomposition routine...";
  timer, old_elapsed;
  testLU, 75;
  testLU, 16;
  testLU, 216;
  timer, elapsed;
  timer_print, "LU elapsed time", elapsed-old_elapsed;
  write, "testing QR decomposition routine...";
  timer, old_elapsed;
  testQR, 75, 75;
  testQR, 16, 16;
  testQR, 25, 21;
  testQR, 21, 25;
  testQR, 256, 240;
  timer, elapsed;
  timer_print, "QR elapsed time", elapsed-old_elapsed;
  write, "testing SVD routine...";
  timer, old_elapsed;
  testSVD, 75, 75;
  testSVD, 16, 16;
  testSVD, 25, 21;
  testSVD, 21, 25;
  testSV, 21;
  testSV, 64;
  testSV, 128;
  timer, elapsed;
  timer_print, "SVD elapsed time", elapsed-old_elapsed;
}

func fft_test(n)
{
  index= 2*pi*(indgen(n)-1.0)/n;
  z= sin(index*3);
  zf= fft(z, 1);
  z3= z2= array(0i, n);
  z3(4)= -0.5i*n;
  z3(-2)= 0.5i*n;
  zb= fft(z, -1);
  if (max(abs(zf-z3))>1.e-12*n || max(abs(zb-conj(z3)))>1.e-12*n)
    write, "***WARNING*** failed 1D fft test";
  if (n<=96 || force2d) {
    z*= cos(index*2)(-,);
    zf= fft(z, [0, 1]);
    z2(3)= z2(-1)= 0.5*n;
    zb= fft(z, [], [-1, 0]);
    if (max(abs(zf-sin(index*3)*z2(-,)))>1.e-12*n ||
        max(abs(zb-conj(z3)*cos(index*2)(-,)))>1.e-12*n)
      write, "***WARNING*** failed first 2D fft test";
    zf= fft(z, 1);
    zb= fft(z, -1);
    if (max(abs(zf-z3*z2(-,)))>1.e-12*n ||
        max(abs(zb-conj(z3)*z2(-,)))>1.e-12*n)
      write, "***WARNING*** failed second 2D fft test";
  }
}

func TDcheck(c, d, e, b, x, s)
{
  check= _(   d(1)*x(1)    +    e(1)*x(2),
           c(1:-1)*x(1:-2) + d(2:-1)*x(2:-1) + e(2:0)*x(3:0),
                                c(0)*x(-1)   +   d(0)*x(0)   );
  if (max(abs(check-b))>1.e-9*max(abs(b))) {
    write, "***WARNING*** "+s+" tridiagonal solution doesn't check";
    write, "   max relative error is "+pr1((max(abs(check-b)))/max(abs(b)));
  }
}

func testTD(n)
{
  c= random(n-1);
  d= random(n);
  e= random(n-1);
  b= random(n);
  TDcheck,c,d,e,b,TDsolve(c,d,e,b), "1D";
  b2= random(n);
  x= TDsolve(c,d,e,[b,b2])
  TDcheck,c,d,e,b, x(,1), "2D(1)";
  TDcheck,c,d,e,b2, x(,2), "2D(2)";
  x= TDsolve(c,d,e,transpose([b,b2]), which=2)
  TDcheck,c,d,e,b, x(1,), "2D(1)/which";
  TDcheck,c,d,e,b2, x(2,), "2D(2)/which";
}

func LUcheck(a, b, x, s)
{
  check= a(,+)*x(+);
  if (max(abs(check-b))>1.e-9*max(abs(b))) {
    write, "***WARNING*** "+s+" LUsolve solution doesn't check";
    write, "   max relative error is "+pr1((max(abs(check-b)))/max(abs(b)));
  }
}

func testLU(n)
{
  a= random(n,n);
  b= random(n);
  LUcheck,a,b,LUsolve(a,b), "1D";
  b2= random(n);
  x= LUsolve(a,[b,b2])
  LUcheck,a,b, x(,1), "2D(1)";
  LUcheck,a,b2, x(,2), "2D(2)";
  x= LUsolve(transpose(a),[b,b2])
  LUcheck,transpose(a),b, x(,1), "t2D(1)";
  LUcheck,transpose(a),b2, x(,2), "t2D(2)";
  x= LUsolve(a,transpose([b,b2]), which=2)
  LUcheck,a,b, x(1,), "2D(1)/which";
  LUcheck,a,b2, x(2,), "2D(2)/which";
  x= LUsolve(transpose(a),transpose([b,b2]), which=2)
  LUcheck,transpose(a),b, x(1,), "t2D(1)/which";
  LUcheck,transpose(a),b2, x(2,), "t2D(2)/which";
  ai= LUsolve(a);
  err= max(abs(ai(,+)*a(+,)-unit(n)))/max(abs(ai));
  if (err>1.e-9) {
    write, "***WARNING*** LUsolve inverse is fishy";
    write, "   max relative error is "+pr1(err);
  }
}

func QRcheck(a, b, x, s)
{
  check= a(,+)*x(+);
  err= max(abs(check-b))/max(abs(b));
  if (err>1.e-9 && dimsof(a)(2)<=dimsof(a)(3)) {
    write, "***WARNING*** "+s+" QRsolve solution doesn't check";
    write, "   max relative error is "+pr1(err);
  }
}

func testQR(m,n)
{
  a= random(m,n);
  b= random(m);
  QRcheck,a,b,QRsolve(a,b), "1D";
  b2= random(m);
  x= QRsolve(a,[b,b2])
  QRcheck,a,b, x(,1), "2D(1)";
  QRcheck,a,b2, x(,2), "2D(2)";
  x= QRsolve(a,transpose([b,b2]), which=2)
  QRcheck,a,b, x(1,), "2D(1)/which";
  QRcheck,a,b2, x(2,), "2D(2)/which";
}

func SVcheck(a, b, x, s)
{
  check= a(,+)*x(+);
  err= max(abs(check-b))/max(abs(b));
  if (err>1.e-9) {
    write, "***WARNING*** "+s+" SVsolve solution doesn't check";
    write, "   max relative error is "+pr1(err);
  }
}

func testSVD(m, n)
{
  a= random(m,n);
  s= SVdec(a, u, v);
  if (anyof(s(dif)>0.0))
    error, "***WARNING*** SVdec returned increasing singular values";
  achk= u(,+) * (s*v)(+,);
  sabs= max(abs(s));
  err= max(abs(a-achk))/sabs;
  if (err>1.e-9) {
    write, "***WARNING***  SVdec decomposition doesn't check";
    write, "   max relative error is "+pr1(err);
  }

  s= SVdec(a, u, v, full=1);
  if (anyof(s(dif)>0.0))
    error, "***WARNING*** SVdec returned increasing singular values";
  uu= u(,1:min(m,n));
  vv= v(1:min(m,n),);
  achk= uu(,+) * (s*vv)(+,);
  sabs= max(abs(s));
  err= max(abs(a-achk))/sabs;
  if (err>1.e-9) {
    write, "***WARNING***  SVdec decomposition doesn't check";
    write, "   max relative error is "+pr1(err);
  }
  err= max(abs(u(,+)*u(,+)-unit(m)))+max(abs(v(,+)*v(,+)-unit(n)));
  if (err>1.e-9) {
    write, "***WARNING***  SVdec decomposition not orthogonal";
    write, "   max relative error is "+pr1(err);
  }
}

func testSV(n)
{
  a= random(n,n);
  b= random(n);
  SVcheck,a,b,SVsolve(a,b), "1D";
  b2= random(n);
  x= SVsolve(a,[b,b2])
  SVcheck,a,b, x(,1), "2D(1)";
  SVcheck,a,b2, x(,2), "2D(2)";
  x= SVsolve(a,transpose([b,b2]), which=2)
  SVcheck,a,b, x(1,), "2D(1)/which";
  SVcheck,a,b2, x(2,), "2D(2)/which";
}
