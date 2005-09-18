/*
 * $Id: random.i,v 1.1 2005-09-18 22:06:05 dhmunro Exp $
 * Random numbers with various distributions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* Contents:

   random_x   - avoids the 2.e9 bins of the random function at a
                cost of calling random twice, can be used as a
                drop-in replacement for random
   random_u   - convenience routine to give uniform deviate on an
                interval other than (0,1)
   random_n   - return gaussian deviate
   random_ipq - arbitrary piecewise linear deviate, with optional
                power law or exponential tails
   random_rej - generic implementation of rejection method, can be
                used either in conjunction with a piecewise linear
                bounding function, or an arbitrary bounding function
                (in the latter case, the inverse of the integral of
                the bounding function must be supplied as well)
   poisson    - return poisson deviate

   In all cases, these routines accept a dimlist or arguments to
   determine the dimensions of the returned random deviates.
   Furthermore, in all cases you will get back the same sequence
   of deviates no matter what the dimensionality of the calls --
   for example, if at some point the call to random_n(5) returns
   [.3,-.1,-.8,1.1,-.9], then if instead random_n(3) followed by
   random_n(2) had been called, the return values would have been
   [.3,-.1,-.8] and [1.1,-.9].
 */

/* ------------------------------------------------------------------------ */

func build_dimlist(&dimlist, arg)
/* DOCUMENT build_dimlist, dimlist, next_argument
     build a DIMLIST, as used in the array function.  Use like this:

     func your_function(arg1, arg2, etc, dimlist, ..)
     {
       while (more_args()) build_dimlist, dimlist, next_arg();
       ...
     }

     After this, DIMLIST will be an array of the form
     [#dims, dim1, dim2, ...], compounded from the multiple arguments
     in the same way as the array function.  If no DIMLIST arguments
     given, DIMLIST will be [] instead of [0], which will act the
     same in most situations.  If that possibility is unacceptible,
     you may add
       if (is_void(dimlist)) dimlist= [0];
     after the while loop.
 */
{
  if (is_void(dimlist)) dimlist= [0];
  else if (!dimsof(dimlist)(1)) dimlist= [1,dimlist];
  if (is_void(arg)) return;
  if (!dimsof(arg)(1)) {
    grow, dimlist, arg;
    dimlist(1)+= 1;
  } else {
    n= arg(1);
    grow, dimlist, arg(2:1+n);
    dimlist(1)+= n;
  }
}

/* ------------------------------------------------------------------------ */

func random_x(dimlist, ..)
/* DOCUMENT random_x(dimlist)

     same as random(DIMLIST), except that random_x calls random
     twice at each point, to avoid the defect that random only
     can produce about 2.e9 numbers on the interval (0.,1.) (see
     random for an explanation of these bins).

     You may set random=random_x to get these "better" random
     numbers in every call to random.

     Unlike random, there is a chance in 1.e15 or so that random_x
     may return exactly 1.0 or 0.0 (the latter may not be possible
     with IEEE standard arithmetic, while the former apparently is).
     Since cosmic rays are far more likely, you may as well not
     worry about this.  Also, because of rounding errors, some bit
     patterns may still be more likely than others, but the 0.5e-9
     wide bins of random will be absent.

   SEE ALSO: random
 */
{
  while (more_args()) build_dimlist, dimlist, next_arg();
  if (is_void(dimlist)) {
    dimlist= [1,2];
  } else if (!dimsof(dimlist)(1)) {
    dimlist= [2,2,dimlist];
  } else {
    dimlist= grow([dimlist(1)+1],dimlist);
    dimlist(2)= 2;
  }
  r= random_0(dimlist);
  return r(1,..) + (r(2,..)-0.5)/2147483562.
}

if (is_void(random_0)) random_0= random;

/* ------------------------------------------------------------------------ */

func random_u(a, b, dimlist, ..)
/* DOCUMENT random_u(a, b, dimlist)

     return uniformly distributed random numbers between A and B.
     (Will never exactly equal A or B.)  The DIMLIST is as for the
     array function.  Same as (b-a)*random(dimlist)+a.  If A==0,
     you are better off just writing B*random(dimlist).

   SEE ALSO: random, random_x, random_n, random_ipq, random_rej
*/
{
  while (more_args()) build_dimlist, dimlist, next_arg();
  return (b-a)*random(dimlist)+a;
}

/* ------------------------------------------------------------------------ */

func random_n(dimlist, ..)
/* DOCUMENT random_n(dimlist)

     returns an array of normally distributed random double values with
     the given DIMLIST (see array function, nil for a scalar result).
     The mean is 0.0 and the standard deviation is 1.0.
     The algorithm follows the Box-Muller method (see Numerical Recipes
     by Press et al.).

   SEE ALSO: random, random_x, random_u, random_ipq, random_rej, poisson
*/
{
  while (more_args()) build_dimlist, dimlist, next_arg();
  a= array(0.0, dimlist);
  scalar= !dimsof(a)(1);
  if (scalar) a= [a];  /* work around long-standing Yorick bug */

  /* crucial feature of this algorithm is that the same sequence
   * of random numbers is returned independent of the number requested
   * each time, just like random itself */
  na= numberof(a);
  np= numberof(_random_n_prev);
  n= (na-np+1)/2;
  nr= 2*n;
  if (n) {
    x= random(2,n);
    r= sqrt(-2.0*log(x(1,)));
    theta= 2.0*pi*x(2,);
    x(1,)= r*cos(theta);
    x(2,)= r*sin(theta);
    a(np+1:na)= x(1:na-np);
  }
  if (np) {
    a(1)= _random_n_prev;
    _random_n_prev= [];
  }
  if (na-np<nr) {
    _random_n_prev= x(nr);
  }

  if (scalar) a= a(1);  /* work around long-standing Yorick bug */
  return a;
}

/* storage for odd random_n values */
_random_n_prev= [];

/* ------------------------------------------------------------------------ */

func random_ipq(model, dimlist, ..)
/* DOCUMENT random_ipq(ipq_model, dimlist)

     returns an array of double values with the given DIMLIST (see array
     function, nil for a scalar result).  The numbers are distributed
     according to a piecewise linear function (possibly with power law
     or exponential tails) specified by the IPQ_MODEL.  The "IPQ" stands
     for "inverse piecewise quadratic", which the type of function
     required to transform a uniform random deviate into the piecewise
     linear distribution.  Use the ipq_setup function to compute
     IPQ_MODEL.

   SEE ALSO: random, random_x, random_u, random_n, random_rej, ipq_setup
 */
{
  while (more_args()) build_dimlist, dimlist, next_arg();
  ymax= (*model(4))(1);
  return ipq_compute(model, ymax*random(dimlist));
}

func random_rej(target, bound, dimlist, ..)
/* DOCUMENT random_rej(target_dist, ipq_model, dimlist)
         or random_rej(target_dist, bounding_dist, bounding_rand, dimlist)

     returns an array of double values with the given DIMLIST (see array
     function, nil for a scalar result).  The numbers are distributed
     according to the TARGET_DIST function:
        func target_dist(x)
     returning u(x)>=0 of same number and dimensionality as x, normalized
     so that the integral of target_dist(x) from -infinity to +infinity
     is 1.0.  The BOUNDING_DIST function must have the same calling
     sequence as TARGET_DIST:
        func bounding_dist(x)
     returning b(x)>=u(x) everywhere.  Since u(x) is normalized, the
     integral of b(x) must be >=1.0.  Finally, BOUNDING_RAND is a
     function which converts an array of uniformly distributed random
     numbers on (0,1) -- as returned by random -- into an array
     distributed according to BOUNDING_DIST:
        func bounding_rand(uniform_x_01)
     Mathematically, BOUNDING_RAND is the inverse of the integral of
     BOUNDING_DIST from -infinity to x, with its input scaled to (0,1).

     If BOUNDING_DIST is not a function, then it must be an IPQ_MODEL
     returned by the ipq_setup function.  In this case BOUNDING_RAND is
     omitted -- ipq_compute will be used automatically.

   SEE ALSO: random, random_x, random_u, random_n, random_ipq, ipq_setup
 */
{
  if (is_func(bound)) {
    brand= dimlist;
    dimlist= more_args()? next_arg() : [];
  }
  while (more_args()) build_dimlist, dimlist, next_arg();
  if (!is_func(target) || (!is_void(brand) && !is_func(brand)) ||
      (!is_func(bound) && structof(bound)!=pointer))
    error, "improper calling sequence, try help,random_rej";
  if (!is_func(bound)) ymax= (*bound(4))(1);

  /* build result to requested shape */
  x= array(0.0, dimlist);
  nreq= nx= numberof(x);
  ix= 1;

  do {
    /* get 25% more pairs of random numbers than nreq in order
     * to allow for some to be rejected -- should actually go for
     * integral(bounding_dist) times nreq, but don't know what
     * that is -- could refine the estimate as each pass gets a
     * better notion of the fraction rejected, but don't bother */
    r= random(2, max(nreq+nreq/4,10));

    /* first get xx distributed according to bounding_rand,
     * then accept according to the second random number
     * continue until at least one is accepted */
    for (xx=[] ; !numberof(xx) ; xx=xx(list)) {
      if (is_func(bound)) {
        xx= brand(r(1,..));
        list= where(bound(xx)*r(2,..) <= target(xx));
      } else {
        xx= ipq_compute(bound, ymax*r(1,..));
        list= where(ipq_function(bound,xx)*r(2,..) <= target(xx));
      }
    }
    nxx= numberof(xx);
    if (nxx>nreq) {
      xx= xx(1:nreq);
      nxx= nreq;
    }

    nreq-= nxx;
    x(ix:nx-nreq)= xx;
    ix+= nxx;
  } while (nreq);

  return x;
}

func ipq_setup(x,u,power=,slope=)
/* DOCUMENT model= ipq_setup(x, u)
         or model= ipq_setup(x, u, power=[pleft,prght])
         or model= ipq_setup(x, u, power=[pleft,prght], slope=[sleft,srght])

     compute a model for the ipq_compute function, which computes the
     inverse of a piecewise quadratic function.  This function occurs
     when computing random numbers distributed according to a piecewise
     linear function.  The piecewise linear function is u(x), determined
     by the discrete points X and U input to ipq_setup.  None of the
     values of U may be negative, and X must be strictly increasing,
     X(i)<X(i+1).

     If U(1) or U(0) is not zero, you may want to model the "tail" of
     the distribution, which is not well modeled by a piecewise linear
     function.  You can specify a power law tail using the power=
     keyword; the left tail has initial slope (U(2)-U(1))/(X(2)-X(1)),
     and decays to the left as 1/X^PLEFT.  Similarly for the right tail.
     If U(1)==0, PLEFT is ignored, as is PRGHT when U(0)==0.  Use the
     slope= keyword to specify an alternative value for the slope.
     Note that PLEFT and PRGHT must each be greater than 1.0, and that
     SLEFT>0 while SRGHT<0.  If either power is greater than or equal to
     100, an exponential tail will be used.  As a convenience, you may
     also specify PLEFT or PRGHT of 0 to get an exponential tail.

     Note: ipq_function(model, xp) returns the function values u(xp) at
     the points xp, including the tails (if any).  ipq_compute(model, yp)
     returns the xp for which (integral from -infinity to xp) of u(x)
     equals yp; i.e.- the inverse of the piecewise quadratic.

   SEE ALSO: random_ipq, random_rej
 */
{
  x= double(x);
  u= double(u);
  if (dimsof(x)(1)!=1 || numberof(x)<2 || dimsof(u)(1)!=1 ||
      numberof(u)!=numberof(x) || anyof(u<0.)) error, "bad U or X arrays";

  /* compute the integral of u(x), starting from x(1),
   * both at the given points x and at the midpoints of the intervals
   * integ(u,x,xx) is the basic piecewise quadratic function */
  bins= (u(zcen)*x(dif))(cum);
  cens= x(pcen);  /* right shape, wrong values */
  yc= integ(u,x, x(zcen));
  dy= bins(dif);
  /* note that these cens are constrained to lie between -1 and +1 */
  cens(2:-1)= 4.*(yc-bins(1:-1))/(dy+!dy) - 2.;

  ymax= bins(0);

  if (!is_void(power)) {
    if (dimsof(power)(1)!=1 || numberof(power)!=2 ||
        anyof(power<=1.&power!=0.)) error, "illegal power= keyword";
    if (!power(1) || !u(1)) power(1)= 100.;
    if (!power(0) || !u(0)) power(0)= 100.;

    if (is_void(slope))
      slope= [(u(2)-u(1))/(x(2)-x(1)), (u(0)-u(-1))/(x(0)-x(-1))];
    if (dimsof(slope)(1)!=1 || numberof(slope)!=2 ||
        slope(1)<0. || slope(0)>0.)
      error, "illegal slope= keyword, or upward slope at endpoint";

    cens(1)= u(1)? slope(1)/u(1) : 1000./(x(2)-x(1));
    cens(0)= u(0)? -slope(0)/u(1) : 1000./(x(0)-x(-1));
    yi= u(1)/cens(1);
    if (power(1)<100.) yi*= power(1)/(power(1)-1.);
    ymax+= yi;
    bins+= yi;
    yi= u(0)/cens(0);
    if (power(0)<100.) yi*= power(0)/(power(0)-1.);
    ymax+= yi;

  } else {
    power= [100.,100.];
    cens(1)= 1000./(x(2)-x(1));
    cens(0)= 1000./(x(0)-x(-1));
  }

  parm= [ymax, power(1), power(0)];

  return [&bins, &x, &cens, &parm, &u];
}

/* ------------------------------------------------------------------------ */

func ipq_compute(model, y)
{
  /*
   * model= [&bins, &vals, &cens, &parm] where:
   * bins   values of y, a piecewise quadratic function of x
   * vals   values of x that go with bins
   * cens   4*(yc-y0)/(y1-y0) - 2 where yc is value of y at
   *             x=vals(pcen), except for first and last points
   *             which are du/dx / u0 for the extrapolation model
   * parm   [ymax, left_power, right_power]
   *             maximum possible value of y, and
   *             [left,right] powers (>1.0) of x for extrap. model
   */
  local bins, vals, cens, parm;
  eq_nocopy, bins, *model(1);
  eq_nocopy, vals, *model(2);
  eq_nocopy, cens, *model(3);
  eq_nocopy, parm, *model(4);

  i= digitize(y, bins);

  mask0= (i>1);
  list= where(mask0);
  if (numberof(list)) {
    yy= y(list);
    ii= i(list);
    mask= (ii<=numberof(bins));
    list= where(mask);
    if (numberof(list)) {
      /* handle piecewise quadratic part */
      j= ii(list);
      yb= bins(j);
      xb= vals(j);
      aa= cens(j);   /* 4*(yc-y0)/(y1-y0) - 2 */
      j-= 1;
      ya= bins(j);
      xa= vals(j);
      bb= 0.5*(1.+aa);
      yq= (yy(list)-ya)/(yb-ya);
      xq= bb+sqrt(bb*bb-aa*yq);
      xq= xa + (xb-xa)*( yq/(xq+!xq) );
    }

    list= where(!mask);
    if (numberof(list)) {
      /* handle right tail */
      ymax= parm(1);
      if (ymax > bins(0)) {
        yy= (ymax - yy(list))/(ymax - bins(0));
        xa= vals(0);
        aa= cens(0);    /* du/dx / u0 */
        pp= parm(2);    /* power */
        yy0= yy<=0.0;
        yy= max(yy,0.0)+yy0;
        if (pp>=100.) xt= -log(yy)/aa;
        else xt= (pp/aa) * (yy^(1./(1.-pp)) - 1.0);
        xt+= xa + 1.e9*yy0*(xa-vals(1));
      } else {
        xt= array(vals(0), numberof(list));
      }
    }

    xq= merge(xq, xt, mask);
  }

  list= where(!mask0);
  if (numberof(list)) {
    /* handle left tail */
    if (bins(1)) {
      yy= y(list)/bins(1);
      xa= vals(1);
      aa= cens(1);    /* du/dx / u0 */
      pp= parm(3);    /* power */
      yy0= yy<=0.0;
      yy= max(yy,0.0)+yy0;
      if (pp>=100.) xt= log(yy)/aa;
      else xt= (pp/aa) * (1.0 - yy^(1./(1.-pp)));
      xt+= xa - 1.e9*yy0*(vals(0)-xa);
    } else {
      xt= array(vals(1), numberof(list));
    }
  } else {
    xt= [];
  }

  return merge(xq, xt, mask0);
}

func ipq_function(model, x)
{
  /*
   * model= [&bins, &vals, &cens, &parm, &valu] where:
   * bins   values of y, a piecewise quadratic function of x
   * vals   values of x that go with bins
   * cens   4*(yc-y0)/(y1-y0) - 2 where yc is value of y at
   *             x=vals(pcen), except for first and last points
   *             which are du/dx / u0 for the extrapolation model
   * parm   [ymax, left_power, right_power]
   *             maximum possible value of y, and
   *             [left,right] powers (>1.0) of x for extrap. model
   * valu   values of u that go with x
   */
  local vals, cens, parm, valu;
  eq_nocopy, vals, *model(2);
  eq_nocopy, cens, *model(3);
  eq_nocopy, parm, *model(4);
  eq_nocopy, valu, *model(5);

  i= digitize(x, vals);

  mask0= (i>1);
  list= where(mask0);
  if (numberof(list)) {
    xx= x(list);
    ii= i(list);
    mask= (ii<=numberof(vals));
    list= where(mask);
    if (numberof(list)) {
      /* handle piecewise linear part */
      uq= interp(valu, vals, xx(list));
    }

    list= where(!mask);
    if (numberof(list)) {
      /* handle right tail */
      if (valu(0)) {
        xx= xx(list) - vals(0);
        aa= cens(0);    /* du/dx / u0 */
        pp= parm(2);    /* power */
        if (pp>=100.) ut= valu(0)*exp(-aa*xx);
        else ut= valu(0) / (1. + (aa/pp)*xx)^pp;
      } else {
        ut= array(0.0, numberof(list));
      }
    }

    uq= merge(uq, ut, mask);
  }

  list= where(!mask0);
  if (numberof(list)) {
    /* handle left tail */
    if (valu(1)) {
      xx= vals(1) - x(list);
      aa= cens(1);    /* du/dx / u0 */
      pp= parm(3);    /* power */
      if (pp>=100.) ut= valu(1)*exp(-aa*xx);
      else ut= valu(1) / (1. + (aa/pp)*xx)^pp;
    } else {
      ut= array(0.0, numberof(list));
    }
  } else {
    ut= [];
  }

  return merge(uq, ut, mask0);
}

/* ------------------------------------------------------------------------ */

_poiprev = 0;

func poisson(navg)
/* DOCUMENT poisson(navg)

     returns a Poisson distributed random value with mean NAVG.
     (This is the integer number of events which actually occur
      in some time interval, when the probability per unit time of
      an event is constant, and the average number of events during
      the interval is NAVG.)
     The return value has the same dimensions as the input NAVG.
     The return value is an integer, but its type is double.
     The algorithm is taken from Numerical Recipes by Press, et. al.

   SEE ALSO: random, random_n
 */
{
  if (!_poiprev) require, "gamma.i";
  navg = double(navg);
  is_scalar = !dimsof(navg)(1);
  if (is_scalar) navg = [navg];

  mask = navg < 12;

  list = where(mask);
  if (numberof(list)) {
    n = exp(-navg(list));
    rlo = 0.*n;
    master = indgen(numberof(n));
    t = random(numberof(n));
    for (;;) {
      list = where(t > n);
      if (!numberof(list)) break;
      t = t(list) * random(numberof(list));
      n = n(list);
      master = master(list);
      rlo(master) += 1.;
    }
  }

  list = where(!mask);
  if (numberof(list)) {
    r = navg = double(navg(list));
    master = indgen(numberof(r));
    sq = sqrt(2.*navg);
    alxm = log(navg);
    g = navg*alxm - lngamma(navg+1.);
    n = navg;
    for (;;) {
      nn = n;
      rr = y = array(0., numberof(master));
      for (sub=indgen(numberof(rr)) ;; sub=sub(list)) {
        y(sub) = yy = tan(pi*random(numberof(sub)));
        rr(sub) = rs  =sq(sub)*yy + nn;
        list = where(rs < 0.);
        if (!numberof(list)) break;
        nn = nn(list);
      }
      r(master) = rr = floor(rr);
      t = 0.9*(1.+y*y)*exp(rr*alxm-lngamma(rr+1.)-g);
      list = where(random(numberof(t)) > t);
      if (!numberof(list)) break;
      master = master(list);
      n = n(list);
      sq =sq(list);
      alxm = alxm(list);
      g = g(list);
    }
  }

  r = merge(rlo, r, mask);
  if (is_scalar) r = r(1);
  return r;
}

/* ------------------------------------------------------------------------ */
