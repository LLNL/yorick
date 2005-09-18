/*
 * $Id: rkutta.i,v 1.1 2005-09-18 22:06:07 dhmunro Exp $
 *  4th order Runge-Kutta integrator (rk_integrate, rkutta)
 * Also Bulirsch-Stoer integrator (bs_integrate, bstoer)
 * After routines in Numerical Recipes by Press et.al.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* ------------------------------------------------------------------------ */

func rk_integrate(derivative, y1, rk_x, epsilon, dx1)
/* DOCUMENT y= rk_integrate(derivative, y1, x, epsilon, dx1)
     integrates dydx= DERIVATIVE(y,x) beginning at (X(1),Y1) and
     going to X(0) with fractional error EPSILON.  The result is
     the value of y at each value in the list X.  If non-nil, DX1
     will be used as initial guess for the first step size.
     Otherwise, X(2)-X(1) will be the first step size guess.

     The list of X values must be monotone -- strictly increasing
     or strictly decreasing; the Runge-Kutta step sizes are selected
     adaptively until the next X value would be passed, when the
     step size is adjusted to complete the step exactly.

     The external variable rk_maxits (default 10000) is the
     maximum number of steps rk_integrate will take.

     If a function rk_yscale(y,dydx,x,dx) exists, it is used
     to compute an appropriate yscale to give the EPSILON error
     criterion meaning.  Otherwise, yscale is taken to be:
        abs(y)+abs(dydx*dx)+1.e-30

     Based on odeint from Numerical Recipes (Press, et.al.).
     If the function you are trying to integrate is very
     smooth, and your X values are fairly far apart, bs_integrate
     may work better than rk_integrate.

   SEE ALSO: rkutta, bs_integrate, rk_maxits, rk_minstep,
             rk_maxstep, rk_ngood, rk_nbad, rkdumb, rk4
 */
{
  if (numberof(rk_x)<2) return array(y1, dimsof(rk_x));
  local rk_y;
  if (is_void(dx1)) dx1= rk_x(2)-rk_x(1);
  rk_nstore= -1;
  rkutta, derivative, y1, rk_x(1), rk_x(0), epsilon, dx1;
  return rk_y;
}

func rkutta(derivative, y0,x0, x1,epsilon, dx0)
/* DOCUMENT y1= rkutta(derivative, y0,x0, x1,epsilon, dx0)
     integrates dydx= DERIVATIVE(y,x) beginning at (X0,Y0) and
     going to X1 with fractional error EPSILON.  The result is
     the value of y at X1.  DX0 will be used as the initial guess
     for a step size.

     If the external variable rk_nstore is >0, rk_y and rk_x
     will contain up to rk_nstore intermediate values after the
     call to rkutta.  Consider using rk_integrate if you need
     this feature; using rk_nstore gives you the results at
     intermediate values which will tend to be closer where
     the Runge-Kutta step size was smaller, while rk_integrate
     forces you to specify precisely which x values you want.

     The external variable rk_maxits (default 10000) is the
     maximum number of steps rkutta will take.  The variable
     rk_minstep (default 0.0) is the minimum step size.  The
     variable rk_maxstep (default 1.e35) is the maximum step
     size, which you may need if you are storing intermediate
     values (particularly with bstoer).

     If a function rk_yscale(y,dydx,x,dx) exists, it is used
     to compute an appropriate yscale to give the EPSILON error
     criterion meaning.  Otherwise, yscale is taken to be:
        abs(y)+abs(dydx*dx)+1.e-30

     Based on odeint from Numerical Recipes (Press, et.al.).
     If the function you are trying to integrate is very
     smooth, bstoer will probably work better than rkutta.

   SEE ALSO: rk_integrate, bstoer, rk_nstore, rk_maxits,
             rk_minstep, rk_maxstep, rk_ngood, rk_nbad, rkdumb, rk4
 */
{
  extern rk_x, rk_y, rk_ngood, rk_nbad;
  rk_ngood= rk_nbad= 0;
  if (rk_nstore > 0) {
    if (rk_nstore<2) rk_nstore= 2;
    rk_x= array(double(x0), rk_nstore);
    rk_y= array(0.+y0, rk_nstore);
    s= [1, 1, 1];  /* see rk_store function */
  } else if (rk_nstore < 0) {
    x0= rk_x(1);
    x1= rk_x(0);
    if (anyof(rk_x(dif)*(x1-x0) <= 0.0))
      error, "given rk_x must be monotonic";
    rk_y= array(0.+y0, numberof(rk_x));
    s= 2;
  }

  dxsign= sign(x1-x0);
  dx= double(abs(dx0)*dxsign);
  x= double(x0);
  y= 0.+y0;
  for (n=1 ; n<=rk_maxits ; ++n) {
    dydx= derivative(y, x);
    if (!is_void(rk_yscale)) yscale= rk_yscale(y,dydx,x,dx);
    else yscale= abs(y)+abs(dydx*dx)+1.e-30;
    if (abs(dx) > rk_maxstep) dx= dxsign*rk_maxstep;
    if (dxsign*(x+dx-x1) > 0.0) dx= x1-x;
    if (rk_nstore<0 &&
        dxsign*(x+dx-rk_x(s)) > 0.0) dx= rk_x(s)-x;
    local dxdid, dxnxt;
    y= rkqc(y,dydx, x,dx, derivative, epsilon,yscale, dxdid,dxnxt);
    x+= dxdid;
    if (dxdid == dx) ++rk_ngood;
    else ++rk_nbad;
    if (rk_nstore>0) s= rk_store(y,x,s);
    else if (rk_nstore<0 &&
             dxsign*(x-rk_x(s))>=0.0) rk_y(..,s++)= y;
    all_done= (dxsign*(x-x1) >= 0.0);
    if (all_done) break;
    if (abs(dxnxt) < abs(rk_minstep))
      error, "required step less than rk_minstep";
    dx= dxnxt;
  }

  if (rk_nstore>0) {
    if (rk_x(s(3)) != x) {
      s(2)= 1;  /* always store final value */
      s= rk_store(y,x,s);
    }
    rk_y= rk_y(..,1:s(3));
    rk_x= rk_x(1:s(3));
  }
  if (!all_done) error, "exceeded rk_maxits iterations";
  return y;
}

local rk_nstore, rk_maxits, rk_minstep, rk_maxstep, rk_ngood, rk_nbad;
/* DOCUMENT rk_nstore, rk_maxits, rk_minstep, rk_maxstep,
            rk_ngood, rk_nbad

     rk_nstore      maximum number of y values rkutta (bstoer) will store
        after rkutta (bstoer) call, rk_y and rk_x contain stored values

     The other variables are inputs or outputs for rkutta, bstoer,
     rk_integrate, or bs_integrate:

     rk_maxits      maximum number of steps (default 10000)
     rk_minstep     minimum step size (default 0.0)
     rk_maxstep     maximum step size (default 1.e35)
     rk_ngood       number of good steps taken
     rk_nbad        number of failed (but repaired) steps taken
 */
rk_maxits= 10000;
rk_minstep= 0.0;
rk_maxstep= 1.0e35;
rk_nstore= 0;

func rk_store(y,x,s)
{
  /* s= [step number, step increment, store index] */
  i= ++s(1);
  if (! ((i-1)%s(2))) {
    i= ++s(3);
    if (i > rk_nstore) {
      y2= rk_y(..,1:0:2);
      x2= rk_x(1:0:2);
      i= numberof(x2);
      rk_y(..,1:i)= y2;
      rk_x(1:i)= x2;
      s(3)= ++i;
      s(2)*= 2;
    }
    rk_y(..,i)= y;
    rk_x(i)= x;
  }
  return s;
}

func rkdumb(derivative, y0,x0, x1,nsteps, nosave=)
/* DOCUMENT y_of_x= rkdumb(derivative, y0,x0, x1,nsteps)
     integrates dydx= DERIVATIVE(y,x) beginning at (X0,Y0) and
     going to X1 in NSTEPS 4th order Runge-Kutta steps.  The
     result is dimsof(Y0)-by-(NSTEPS+1) values of y at the points
     span(X0, X1, NSTEPS+1).
     If the nosave= keyword is non-zero, the returned value will
     simply be the final y value.
 */
{
  dx= (x1-x0)/nsteps;
  ++nsteps;
  if (!nosave) y= array(0.+y0, nsteps);
  for (i=2 ; i<=nsteps ; ++i) {
    y0= rk4(y0,derivative(y0,x0), x0,dx, derivative);
    x0+= dx;
    if (!nosave) y(..,i)= y0;
  }
  return nosave? y0 : y;
}

func rkqc(y,dydx, x,dx, derivative, epsilon,yscale, &dxdid,&dxnxt)
{
  x0= x;
  y0= y;

  for (;;) {
    x= x0+dx;
    if (x==x0) error, "integration step crash";
    /* first take two half steps... */
    dx2= 0.5*dx;
    x2= x0+dx2;
    y2= rk4(y0,dydx, x0,dx2, derivative);
    y2= rk4(y2,derivative(y2,x2), x2,dx2, derivative);
    /* ...then compare with one full step... */
    y1= rk4(y0,dydx, x0,dx, derivative);
    /* ...to estimate error */
    y1= y2-y1;
    err= max(abs(y1/yscale))/epsilon;
    if (err <= 1.0) {
      dxdid= dx;
      dxnxt= (err>6.e-4)? 0.9*dx*err^-0.20 : 4.*dx;
      break;
    }
    dx*= 0.9*err^-0.25;
  }
  return y2 + y1/15.;
}

func rk4(y,dydx, x,dx, derivative)
/* DOCUMENT y_at_x_plus_dx= rk4(y,dydx, x,dx, derivative)
     takes a single 4th order Runge-Kutta step from X to X+DX.
     DERIVATIVE(y,x) is a function returning dydx; the input DYDX
     is DERIVATIVE(y,x) at the input (X,Y).  This fourth evaluation
     of DERIVATIVE must be performed by the caller of rk4.
 */
{
  dx2= 0.5*dx;
  x2= x+dx2;
  dydxp= derivative(y+dydx*dx2, x2);   /* slope at 1st trial midpoint */
  dydxm= derivative(y+dydxp*dx2, x2);  /* slope at 2nd trial midpoint */
  dydxp+= dydxm;
  dydxm= derivative(y+dydxm*dx, x+dx); /* slope at trial endpoint */
  return y + (dydx+dydxp+dydxp+dydxm)*(dx/6.0);
}

/* ------------------------------------------------------------------------ */

func bs_integrate(derivative, y1, rk_x, epsilon, dx1)
/* DOCUMENT y= bs_integrate(derivative, y1, x, epsilon, dx1)
     Bulirsch-Stoer integrator, otherwise identical to rk_integrate
     routine. All of the options for rk_integrate work here as well.

     Based on odeint from Numerical Recipes (Press, et.al.).
     If the function you are trying to integrate is not very
     smooth, or your X values are closely spaced, rk_integrate
     will probably work better than bs_integrate.

   SEE ALSO: bstoer, rk_integrate, rk_maxits, rk_minstep,
             rk_maxstep, rk_ngood, rk_nbad, rkdumb, rk4
 */
{
  if (numberof(rk_x)<2) return array(y1, dimsof(rk_x));
  local rk_y;
  if (is_void(dx1)) dx1= rk_x(2)-rk_x(1);
  rk_nstore= -1;
  bstoer, derivative, y1, rk_x(1), rk_x(0), epsilon, dx1;
  return rk_y;
}

func bstoer(derivative, y0,x0, x1,epsilon, dx0)
/* DOCUMENT y1= bstoer(derivative, y0,x0, x1,epsilon, dx0)
     Bulirsch-Stoer integrator, otherwise identical to rkutta routine.
     All of the options for rkutta (rk_nstore, etc.) work here as well.

     If the function you are trying to integrate is not very
     smooth, rkutta will probably work better than bstoer.

   SEE ALSO: rkutta, rk_nstore, rk_maxits, rk_minstep, rk_maxstep,
             rk_ngood, rk_nbad
 */
{
  extern _rzextr_x, _rzextr_d;
  rkqc= bsstep;
  _rzextr_x= array(0.0, numberof(_bs_nseq));
  _rzextr_d= array(0.+y0, 7);
  return rkutta(derivative, y0,x0, x1,epsilon, dx0);
}

func bsstep(y,dydx, x,dx, derivative, epsilon,yscale, &dxdid,&dxnxt)
{
  x0= x;
  y0= y;

  for (;;) {
    for (i=1 ; i<=numberof(_bs_nseq) ; ++i) {
      n= _bs_nseq(i);
      y= rzextr(i, (dx/n)^2, mod_midpt(y0,dydx, x0,dx, derivative, n),
                yerr, 7);
      err= max(abs(yerr/yscale))/epsilon;
      if (err < 1.0) {
        dxdid= dx;
        if (i==7) dxnxt= 0.95*dx;
        else if (i==6) dxnxt= 1.2*dx;
        else dxnxt= (16./*_bs_nseq(6)*/*dx)/n;
        return y;
      }
    }
    /* step failed, claimed to be unusual */
    dx*= 0.0625;  /* related to numberof(_bs_nseq) */
    if (x+dx == x) error, "integration step crash";
  }
}

_bs_nseq= [2, 4, 6, 8, 12, 16, 24, 32, 48, 64, 96];

func mod_midpt(y,dydx, x,dx, derivative, nstep)
{
  dx/= nstep;
  ym= y;
  y+= dydx*dx;
  x+= dx;
  dydx= derivative(y,x);
  dx2= 2.*dx;
  for (--nstep ; nstep ; --nstep) {
    swap= ym + dydx*dx2;
    ym= y;
    y= swap;
    x+= dx;
    dydx= derivative(y,x);
  }
  return 0.5*(ym+y+dydx*dx);
}

func rzextr(iest, xest, yest, &yerr, nuse)
{
  _rzextr_x(iest)= xest;
  if (iest==1) {
    _rzextr_d(..,1)= yest;
    yerr= yest;
    return yest;
  } else {
    m1= ((iest<nuse)? iest : nuse) - 1;
    fx= _rzextr_x(iest-1 : iest-m1 : -1)/xest;  /* no more than nuse-1 */
    yy= yest;
    v= _rzextr_d(..,1);
    _rzextr_d(..,1)= c= yy;
    for (k=1 ; k<=m1 ; ++k) {
      b1= fx(k)*v;
      b= b1-c;
      ok= double(b!=0.0);
      bad= 1.0-ok;
      b= ((c-v)/(b+bad))*ok;
      ddy= c*b + bad*v;
      c= b1*b + bad*c;
      if (k!=m1) v= _rzextr_d(..,k+1);
      _rzextr_d(..,k+1)= ddy;
      yy+= ddy;
    }
    yerr= ddy;
    return yy;
  }
}

/* ------------------------------------------------------------------------ */
