/*
 * $Id: elliptic.i,v 1.2 2007-11-10 20:03:49 dhmunro Exp $
 * elliptic functions
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* Abramowitz and Stegun, sections 16.4, 17.2.17-19, 17.6 */

local elliptic;
/* DOCUMENT elliptic, ell_am, ell_f, ell_e, dn_, ellip_k, ellip_e

     The elliptic integral of the first kind is:

        u = integral[0 to phi]( dt / sqrt(1-m*sin(t)^2) )

     The functions ell_f and ell_am compute this integral and its
     inverse:

        u   = ell_f(phi, m)
        phi = ell_am(u, m)

     The Jacobian elliptic functions can be computed from the
     "amplitude" ell_am by means of:

        sn(u|m) = sin(ell_am(u,m))
        cn(u|m) = cos(ell_am(u,m))
        dn(u|m) = dn_(ell_am(u,m)) = sqrt(1-m*sn(u|m)^2)

     The other nine functions are sc=sn/cn, cs=cn/sn, nd=1/dn,
     cd=cn/dn, dc=dn/cn, ns=1/sn, sd=sn/dn, nc=1/cn, and ds=dn/sn.
     (The notation u|m does not means yorick's | operator; it is
     the mathematical notation, not valid yorick code!)

     The parameter M is given in three different notations:
       as M, the "parameter",
       as k, the "modulus", or
       as alpha, the "modular angle",
     which are related by: M = k^2 = sin(alpha)^2.  The yorick elliptic
     functions in terms of M may need to be written
       ell_am(u,k^2) or ell_am(u,sin(alpha)^2)
     in order to agree with the definitions in other references.
     Sections 17.2.17-19 of Abramowitz and Stegun explains these notations,
     and chapters 16 and 17 present a compact overview of the subject of
     elliptic functions in general.

     The parameter M must be a scalar; U may be an array.  The
     exceptions are the complete elliptic integrals ellip_k and
     ellip_e which accept an array of M values.

     The ell_am function uses the external variable ell_m if M is
     omitted, otherwise stores M in ell_m.  Hence, you may set ell_m,
     then simply call ell_am(u) if you have a series of calls with
     the same value of M; this also allows the dn_ function to work
     without a second specification of M.

     The elliptic integral of the second kind is:

        u = integral[0 to phi]( dt * sqrt(1-m*sin(t)^2) )

     The function ell_e computes this integral:

        u   = ell_e(phi, m)

     The special values ell_f(pi/2,m) and ell_e(pi/2,m) are the complete
     elliptic integrals of the first and second kinds; separate functions
     ellip_k and ellip_e are provided to compute them.

     Note that the function ellip_k is infinite for M=1 and for large
     negative M.  The "natural" range for M is 0<=M<=1; all other real
     values can be "reduced" to this range by various transformations;
     the logarithmic singularity of ellip_k is actually very mild, and
     other functions such as ell_am are perfectly well-defined there.

     Here are the sum formulas for elliptic functions:

       sn(u+v) = ( sn(u)*cn(v)*dn(v) + sn(v)*cn(u)*dn(u) ) /
                 ( 1 - m*sn(u)^2*sn(v)^2 )
       cn(u+v) = ( cn(u)*cn(v) - sn(u)*dn(u)*sn(v)*dn(v) ) /
                 ( 1 - m*sn(u)^2*sn(v)^2 )
       dn(u+v) = ( dn(u)*dn(v) - m*sn(u)*cn(u)*sn(v)*cn(v) ) /
                 ( 1 - m*sn(u)^2*sn(v)^2 )

     And the formulas for pure imaginary values:

       sn(1i*u,m) = 1i * sc(u,1-m)
       cn(1i*u,m) = nc(u,1-m)
       dn(1i*u,m) = dc(u,1-m)

   SEE ALSO: ell_am, ell_f, ell_e, dn_, ellip_k, ellip_e
 */

func ell_am(u,m)
/* DOCUMENT ell_am(u)
         or ell_am(u,m)

     returns the "amplitude" (an angle in radians) for the Jacobi
     elliptic functions at U, with parameter M.  That is,
        phi = ell_am(u,m)
     means that
        u = integral[0 to phi]( dt / sqrt(1-m*sin(t)^2) )

     Thus ell_am is the inverse of the incomplete elliptic function
     of the first kind ell_f.  See help,elliptic for more.

   SEE ALSO: elliptic
 */
{
  if (structof(u)==complex || structof(m)==complex)
    error, "elliptic function not valid for complex";
  /* set up the arithmetic-geometric mean scale */
  extern ell_m, _agm_m, _agm_n, _agm_coa, _agm_a, _agm_sn;
  if (is_void(m)) m= ell_m;
  if (m != ell_m) {
    ell_m= m= double(m);
    if (m<0.) {
      _agm_sn= 1./(1.-m);
      m*= -_agm_sn;
      _agm_sn= sqrt(_agm_sn);
    } else if (m>1.) {
      m= 1./m;
      _agm_sn= sqrt(m);
    }
    _agm_m= m;
    _agm_coa()= 0.;
    _agm_n= 0;
    _agm_a= 1.;
    if (m!=1.) {
      b= sqrt(1.-m);
      for (;;) {         /* maximum of 8 passes for 64-bit double */
        c= 0.5*(_agm_a-b);
        if (!c) break;
        am= _agm_a-c;         /* arithmetic mean */
        _agm_coa(++_agm_n)= c/am;
        if (am==_agm_a) break;
        b= sqrt(_agm_a*b);    /* geometric mean */
        _agm_a= am;
      }
      _agm_a*= 2.^_agm_n;
    }
  }

  phi= _agm_a*u;
  if (m<0. || m>1.) phi/= _agm_sn;
  if (m!=1.)
    for (n=_agm_n ; n>0 ; n--) phi= 0.5*(phi+asin(_agm_coa(n)*sin(phi)));
  else
    phi= atan(tanh(phi), sech(phi));
  if (m<0.) {
    cn= cos(phi);
    phi= sin(phi);
    nd= 1./sqrt(1.-m*phi*phi);
    phi= atan(_agm_sn*phi*nd, cn*nd);
  } else if (m>1.) {
    phi= sin(phi);
    phi= atan(_agm_sn*phi, sqrt(1.-_agm_m*phi*phi));
  }
  return phi;
}

_agm_coa= array(0.,16);
_agm_n= 0;
_agm_m= 0.0;
_agm_a= 1.0;
_agm_sn= 1.0;
ell_m= 0.0;

func dn_(phi, m)
/* DOCUMENT dn_(ell_am(u,m))

     return the Jacobian elliptic function dn(u|m).  The external
     variable ell_m must be set properly before calling dn_.

   SEE ALSO: elliptic, ell_am
 */
{
  if (is_void(m)) m= ell_m;
  phi= sin(phi);
  return sqrt(1.-m*phi*phi);
}

func ell_f(phi,m)
/* DOCUMENT ell_f(phi,m)

     returns the incomplete elliptic integral of the first kind F(phi|M).
     That is,
        u = ell_f(phi,m)
     means that
        u = integral[0 to phi]( dt / sqrt(1-m*sin(t)^2) )

     See help,elliptic for more.

   SEE ALSO: elliptic, ell_e
 */
{
  if (structof(phi)==complex || structof(m)==complex)
    error, "elliptic integral not valid for complex";
  orig_m= m= double(m);
  if (m>1.) {
    scale= sqrt(m);
    phi= asin(scale*sin(phi));
    m= 1./m;
  } else if (m<0.) {
    scale= sqrt(1.-m);
    phi= 0.5*pi - phi;
    m/= m-1.;
  }

  if (m==1.) {
    phi= atanh(sin(phi));
    a= 0.;
  } else {          /* compute using arithmetic-geometric mean */
    sgn= sign(phi);
    phi= abs(phi);
    a= 1.;
    b= sqrt(1.-m);
    twon= 1.0;
    pi2= 0.5*pi;
    for (;;) {      /* maximum of 8 passes for 64-bit double */
      c= 0.5*(a-b);
      if (!c) break;
      phase= (phi+pi2)/pi;
      cycle= floor(phase);
      phi*= 1. + 1.e-15*(cycle==phase);
      phi+= atan((b/a)*tan(phi)) + pi*cycle;
      twon*= 2.0;
      am= a-c;
      if (am==a) break;
      b= sqrt(a*b);
      a= am;
    }
    phi/= twon*a*sgn;
  }

  if (orig_m>1.) {
    phi/= scale;
  } else if (orig_m<0.) {
    phi= (0.5*pi/a - phi)/scale;
  }

  return phi;
}

func ell_e(phi,m)
/* DOCUMENT ell_e(phi,m)

     returns the incomplete elliptic integral of the second kind E(phi|M).
     That is,
        u = ell_e(phi,m)
     means that
        u = integral[0 to phi]( dt * sqrt(1-m*sin(t)^2) )

     See help,elliptic for more.

   SEE ALSO: elliptic, ell_f
 */
{
  if (structof(phi)==complex || structof(m)==complex)
    error, "elliptic integral not valid for complex";
  orig_m= m= double(m);
  if (m>1.) {
    scale= sqrt(m);
    phi= asin(scale*sin(phi));
    m= 1./m;
  } else if (m<0.) {
    scale= sqrt(1.-m);
    phi= 0.5*pi - phi;
    m/= m-1.;
  }

  if (m==1.) {
    per= floor((phi+0.5*pi)/pi);
    phi= sin(phi)*sign(0.5-abs(per%2.)) + 2.*per;
    a= 0.;
  } else {          /* compute using arithmetic-geometric mean */
    sgn= sign(phi);
    phi= abs(phi);
    a= 1.;
    b= sqrt(1.-m);
    eok= 1.-0.5*m;
    cs= 0.;
    twon= 1.0;
    pi2= 0.5*pi;
    for (;;) {      /* maximum of 8 passes for 64-bit double */
      c= 0.5*(a-b);
      if (!c) break;
      phi+= atan((b/a)*tan(phi)) + pi*floor((phi+pi2)/pi);
      cs+= c*sin(phi);
      eok-= twon*c*c;
      twon*= 2.0;
      am= a-c;
      if (am==a) break;
      b= sqrt(a*b);
      a= am;
    }
    f= sgn*phi/(twon*a);
    phi= eok*f + sgn*cs;
  }

  if (orig_m>1.) {
    phi= (phi-(1.-m)*f)/scale;
  } else if (orig_m<0.) {
    phi= (eok*0.5*pi/a - phi)*scale;
  }

  return phi;
}

func ellip_k(m)
/* DOCUMENT ellip_k(m)

     returns the complete elliptic integral of the first kind K(M):
        K(M) = integral[0 to pi/2]( dt / sqrt(1-M*sin(t)^2) )

     See help,elliptic for more.

   SEE ALSO: elliptic, ellip_e, ell_f
 */
{
  if (anyof(m>=1.)) error, "ellip_k(m) not computed for m>=1";

  m= double(m);
  mask= (m>=0.);
  list= where(mask);
  if (numberof(list)) scale= array(0.5*pi,numberof(list));
  list= where(!mask);
  if (numberof(list)) {
    sm= 1./(1.-m(list));
    m(list)*= -sm;
    sm= 0.5*pi*sqrt(sm);
  }
  scale= merge(scale,sm,mask);

  a= array(1.,dimsof(m));
  b= sqrt(1.-m);
  for (;;) {
    c= 0.5*(a-b);
    am= a-c;
    if (allof(am==a)) break;
    b= sqrt(a*b);
    a= am;
  }

  return scale/a;
}

func ellip_e(m)
/* DOCUMENT ellip_e(m)

     returns the complete elliptic integral of the second kind E(M):
        E(M) = integral[0 to pi/2]( dt * sqrt(1-M*sin(t)^2) )

     See help,elliptic for more.

   SEE ALSO: elliptic, ellip_k, ell_e
 */
{
  if (anyof(m>1.)) error, "ellip_e(m) not computed for m>1";

  m= double(m);
  mask= (m>=0.);
  list= where(mask);
  if (numberof(list)) scale= array(0.5*pi,numberof(list));
  list= where(!mask);
  if (numberof(list)) {
    sm= 1.-m(list);
    m(list)/= -sm;
    sm= 0.5*pi*sqrt(sm);
  }
  scale= merge(scale,sm,mask);

  mask= (m!=1.);
  list= where(mask);
  if (numberof(list)) {
    m= m(list);
    a= array(1.,numberof(m));
    b= sqrt(1.-m);
    e= 1.-0.5*m;
    twon= 1.0;
    for (n=0 ;;) {  /* maximum of 8 passes for 64-bit double */
      c= 0.5*(a-b);
      am= a-c;
      if (allof(am==a)) break;
      e-= twon*c*c;
      twon*= 2.0;
      b= sqrt(a*b);
      a= am;
    }
    e/= a;
  }
  list= where(!mask);
  if (numberof(list)) em= array(2./pi,numberof(list));

  return scale*merge(e,em,mask);
}

func ellip2_k(m)
/* DOCUMENT ellip2_k(m)

     returns the complete elliptic integral of the first kind K(M):
        K(M) = integral[0 to pi/2]( dt / sqrt(1-M*sin(t)^2) )
     accurate to 2e-8 for 0<=M<1

   SEE ALSO: elliptic, ellip_e, ell_f
 */
{
  m= 1.-m;
  return poly(m,1.38629436112,0.09666344259,0.03590092383,
              0.03742563713,0.01451196212) + 
         log(1./m)*poly(m,0.5,0.12498593597,0.06880248576,
                        0.03328355346,0.00441787012);
}

func ellip2_e(m)
/* DOCUMENT ellip2_e(m)

     returns the complete elliptic integral of the second kind E(M):
        E(M) = integral[0 to pi/2]( dt * sqrt(1-M*sin(t)^2) )
     accurate to 2e-8 for 0<=M<=1

   SEE ALSO: elliptic, ellip_k, ell_e
 */
{
  m= 1.-m;
  return poly(m,1.,0.44325141463,0.06260601220,
              0.04757383546,0.01736506451) +
         log(1./m)*poly(m,0.,0.24998368310,0.09200180037,
                        0.04069697526,0.00526449639);
}
