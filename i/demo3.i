/*
 * $Id: demo3.i,v 1.1 2005-09-18 22:05:55 dhmunro Exp $
 * Chaotic pendulum demo
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func demo3(time_limit, kick=, damp_time=)
/* DOCUMENT demo3
     Solve Lagrange's equations for a famous chaotic pendulum (as on
     exhibit at the San Francisco Exploratorium).  Run a movie of the
     result.  By default, the movie runs for 60 seconds, but if you
     supply an argument to the demo3 function, it will run for that
     many seconds.

     The kick= keyword may be used to adjsut the initial amplitude;
     kick= 1.2 is the default.

     You may also wish to supply a damp_time= keyword to see the effect
     of an ad hoc damping term.  damp_time=100 is nice.
 */
{
  require, "rkutta.i";
  a= 2.0;
  b= 3.0;
  c= 1.5;
  gravity= 1.0;
  widget_setup, a, b, c, 1.00, 0.67, gravity;

  dt= 0.05*sqrt(min(a,b,c)/gravity);

  if (is_void(damp_time)) damp_time= 1.e30;
  if (is_void(kick)) kick= 1.2;

  /* roll dice to get initial velocities */
  thetadot= kick;
  gammadot= 0.1*random();
  qdotq= [[thetadot,0.,0.,gammadot], [0.,0.,0.,0.]];

  window, wait=1;
  s= 1.1*(b+c);
  limits, -s,s,-s,s;
  display, qdotq;  /* without this, won't get axis numbers */

  require, "movie.i";
  movie, draw_frame, time_limit, 0.02;
}

func draw_frame(i)
{
  display, qdotq;
  qdotq= rkdumb(lagrange, qdotq,0., dt, 1, nosave=1);
  return 1;  /* just go until time limit expires */
}

func widget_setup(a, b, c, m_tee, m_bar, gravity)
{
  /* set up the values used by lagrange */
  extern Tdiag, Ta, Tb, Tc, Ba, Bb, Bg, Dg;

  B= 0.5*m_bar*c^2;
  C= 0.5*B*c;
  A= m_tee*(2.*a^3+b^3)/3. + m_bar*c*(2.*a^2+b^2) + C;
  Ba= B*a;
  Bb= B*b;

  Dg= (0.5*m_tee*b^2 + m_bar*b*c)*gravity;
  Bg= B*gravity;

  Tdiag= [[A,0.,0.,0.], [0.,C,0.,0.], [0.,0.,C,0.], [0.,0.,0.,C]];
  Ta= [[0.,Ba,0.,0.], [Ba,0.,0.,0.], [0.,0.,0.,0.], [0.,0.,0.,0.]];
  Tb= [[0.,0.,Ba,0.], [0.,0.,0.,0.], [Ba,0.,0.,0.], [0.,0.,0.,0.]];
  Tc= [[0.,0.,0.,Bb], [0.,0.,0.,0.], [0.,0.,0.,0.], [Bb,0.,0.,0.]];
}

func lagrange(qdotq, time/*unused*/)
{
  /* compute the time derivatives of [qdot,q]
     Lagrange's equations d(dL/dqdot)/dt - dL/dq = 0
     turn out to be T*qdotdot+L=0, with T and L computed here.  */
  theta= qdotq(1,2);
  alpha= qdotq(2,2);
  beta= qdotq(3,2);
  gamma= qdotq(4,2);
  qdot= qdotq(,1);
  t2= qdot(1)^2;
  a2= qdot(2)^2;
  b2= qdot(3)^2;
  g2= qdot(4)^2;

  amt= alpha-theta;
  bpt= beta+theta;
  gmt= gamma-theta;

  T= Tdiag + Ta*sin(amt) + Tb*sin(bpt) - Tc*cos(gmt);
  camt= cos(amt);
  cbpt= cos(bpt);
  sgmt= sin(gmt);
  L= [Dg*sin(theta) + Ba*(a2*camt+b2*cbpt)+Bb*g2*sgmt,
      Bg*sin(alpha) - Ba*t2*camt,
      Bg*sin(beta) + Ba*t2*cbpt,
      Bg*sin(gamma) - Bb*t2*sgmt];
  qdotdot= LUsolve(T, -L);

  qdotdot(,1)-= qdot/damp_time;

  return [qdotdot, qdot];
}

func display(qdotq)
{
  /* draw the widget given its current state */
  theta= qdotq(1,2);
  alpha= qdotq(2,2);
  beta= qdotq(3,2);
  gamma= qdotq(4,2);

  st= sin(theta);
  ct= cos(theta);
  p1x= -a*ct;
  p1y= -a*st;
  p2x= -p1x;
  p2y= -p1y;
  p3x= b*st;
  p3y= -b*ct;
  q1x= p1x + c*sin(alpha);
  q1y= p1y - c*cos(alpha);
  q2x= p2x + c*sin(beta);
  q2y= p2y - c*cos(beta);
  q3x= p3x + c*sin(gamma);
  q3y= p3y - c*cos(gamma);

  plg, [p1y,p2y], [p1x,p2x], marks=0,type=1,width=36,color="red";
  plg, [0.,p3y], [0.,p3x], marks=0,type=1,width=36,color="red";
  plg, [p1y,q1y], [p1x,q1x], marks=0,type=1,width=24,color="green";
  plg, [p2y,q2y], [p2x,q2x], marks=0,type=1,width=24,color="green";
  plg, [p3y,q3y], [p3x,q3x], marks=0,type=1,width=24,color="green";
}
