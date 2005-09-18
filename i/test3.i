/*
 * $Id: test3.i,v 1.1 2005-09-18 22:06:11 dhmunro Exp $
 * Pure scalar math problem for timing Yorick.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func test3(npass)
/* DOCUMENT test3
         or test3, npass
     Computes the ratio r which solves 1 + r^2 + r^3 +...+ r^n = s,
     given n and s.  If NPASS is given, the calculation is repeated
     that many times (actually the equation is solved many times for
     each pass).  The worker routine invgeom can actually be
     vectorized; the vector version is gseries_r in series.i.  */
{
  if (is_void(npass) || npass<=0) npass= 1;

  extern s, n, answer;
  nsums= numberof(answer);
  r= 0.0*answer;

  now= split= array(0.0, 3);
  timer, now;
  m= npass;
  while (m--) {
    for (i=1 ; i<=nsums ; i++) r(i)= invgeom(s(i), n);
  }
  timer, now, split;
  timer_print, "Time per pass", split/npass, "Total time", split;

  if (anyof(abs(r-answer)>1.e-9*answer))
    write, "***WARNING*** values returned by invgeom are not correct";
}

#if 0
CPU seconds per pass:
            IDL    Yorick     Basis       DAP
HP730        -      0.11       5.08      5.00
Solbourne  0.31     0.21       11.8      13.7
    (varies by ~10%)

     forum[10] yorick
      Yorick ready.  For help type 'help'
     > #include "/home/miggle/munro/Yorick/include/test3.i"
     > test3,1
                    Timing Category     CPU sec  System sec    Wall sec
                      Time per pass       0.190       0.000       0.230
                         Total time       0.190       0.000       0.230
     > test3,51
                    Timing Category     CPU sec  System sec    Wall sec
                      Time per pass       0.206       0.000       0.207
                         Total time      10.520       0.000      10.560

     forum[6] time idl
     IDL> .rnew /home/miggle/munro/Yorick/include/test3
     % Compiled module: INVGEOM.
     % Compiled module: TEST3.
     % Compiled module: SPAN.
     % Compiled module: $MAIN$.
     IDL> test3,1
     IDL> exit
     0.5u 0.3s 0:08 11% 0+928k 0+2io 0pf+0w
     forum[7] time idl
     IDL> .rnew /home/miggle/munro/Yorick/include/test3
     % Compiled module: INVGEOM.
     % Compiled module: TEST3.
     % Compiled module: SPAN.
     % Compiled module: $MAIN$.
     IDL> test3,51
     IDL> exit
     16.0u 0.4s 0:41 39% 0+1400k 0+0io 0pf+0w

     forum[13] basis
     Initializing Basis System
     Basis 7.0
     Initializing EZCURVE/NCAR Graphics
     Ezcurve/NCAR 2.0
     Basis> echo=0
     Basis> read /home/miggle/munro/Yorick/include/test3.bas
     End of input file /home/miggle/munro/Yorick/include/test3.bas
     Resuming input from TERMINAL
     Basis> test3(1)

        CPU (sec)   SYS (sec)
           11.817       0.017

     forum[14] dap
     DAP> echo=0
     DAP> read /home/miggle/munro/Yorick/include/test3.bas
     DAP> test3(1)

        CPU (sec)   SYS (sec)
           13.700       0.033
#endif

/* invgeom(s, n)
     returns the ratio r of the finite geometric series, given the sum s:
        1 + r + r^2 + r^3 + ... + r^n = s     */
func invgeom(s, n)
{
  nn= n+1;
  if (nn<2) return s-1.0;

  /* compute an approximate result which has exact values and
     derivatives at s==1, s==n, and s->infinity */

  /* different approximations apply for s>nn and s<nn */
  if (s>nn) {
    pow= 1.0/n;
    npow= nn^pow - 1.0;
    n2r= 1.0/(nn-2.0);
    A= (2.0-nn*npow)*n2r;
    B= (2.0*npow-nn*pow)*nn*n2r;
    r= s^pow - pow + A*(nn/s)^pow + B/s;
  } else {
    sn= (s-1.0)/n;
    n2r= 1.0/(nn*nn);
    r= 1.0 - 1.0/s + n2r*sn*sn*(nn+1.0 - sn);
  }

  /* Polish the approximation using Newton-Raphson iterations.  */
  for (;;) {
    rr= r-1.0;
    rn= r^(nn-1);
    delta= rr*s - (r*rn-1.0);
    if (abs(delta)<=1.e-9*abs(rr*s)) break;
    r+= delta/(nn*rn-s);
  }
  /* try to get it to machine precision */
  if (delta) r+= delta/(nn*rn-s);

  return r;
}

/* Set up a bunch of values to compute.  */
answer= span(0.0, 2.0, 200);  /* number must be even to avoid 1.0000 */
n= 100;
s= (answer^(n+1)-1.0)/(answer-1.0);
