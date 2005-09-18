/*
 * $Id: filter.i,v 1.1 2005-09-18 22:05:56 dhmunro Exp $
 * analog signal processing routines
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func filter(filt, dt, signal, pad=, shift=)
/* DOCUMENT filter(filt, dt, signal)

     apply the filter FILT to the input SIGNAL, which is sampled
     at times spaced by DT.  The filter is assumed to be normalized
     to an angular frequency (e.g.- radians per second), unless
     DT<0, in which case FILT is assumed to be normalized to a
     circular frequency (e.g.- Hz or GHz).

     The result will have the same length as SIGNAL; be sure to pad
     SIGNAL if you need the response to go beyond that time, or
     you can use the pad=n keyword to force the returned result to
     have N samples more than SIGNAL.

     If the shift= keyword is non-nil and non-0, then the result
     is shifted backward in time by the filter group delay at
     zero frequency.

     The impulse response of the FILT is also assumed to be shorter
     than the duration of signal, and SIGNAL is assumed to be sampled
     finely enough to resolve the FILT impulse response.

     FILT is an array of double, which represents a filter with
     a particular finite list of zeroes and poles.  See the specific
     functions to construct filters from poles and zeroes (fil_make),
     or classic Bessel, Butterworth, Chebyshev, inverse Chebyshev, or
     Cauer (elliptic) designs.  With fil_analyze, you can find the
     poles and zeroes of a FILT.  The format for FILT is:

     FILT is an array of double with the following meanings:
       FILT(1) = np = number of poles  (integer >= 0)
       FILT(2) = nz = number of zeroes (integer >= 0)
       FILT(3) = reserved
       FILT(4:4+nz) = coefficients for numerator
                 = [a0, a1, a2, a3, ..., anz]
       FILT(5+nz:4+nz+np) = coefficents for denominator (if np>0)
                 = [b1, b2, b3, ..., bnp]

     The Laplace transform (s-transform) of the filter response is

       L[FILT] = (a0 + a1*s + a2*s^2 + a3*s^3 + ...) /
                 ( 1 + b1*s + b2*s^2 + b3*s^3 + ...)

   SEE ALSO: filter, fil_bessel, fil_butter, fil_cheby1, fil_cheby2,
             fil_cauer, fil_response, fil_make, fil_analyze,
             to_db, to_phase
 */
{
  m= numberof(signal);
  if (is_void(pad)) pad= 0;
  n= fft_good(max(2*m,m+pad));
  signal= fft(grow(double(signal), array(0.,n-m)))/n;
  if (dt>0.) w= (2*pi/n/dt)*roll(indgen(n/2+1-n:n/2),n/2+1);
  else       w= (-1./n/dt)*roll(indgen(n/2+1-n:n/2),n/2+1);
  signal= double(fft(signal*fil_response(filt,w),-1));
  if (shift) {
    d= long(fil_delay(filt,dt<0.)/abs(dt));
    signal= signal(d:0);
    if (numberof(signal)<m+pad)
      grow, signal, array(0.,m+pad-numberof(signal));
  }
  return signal(1:m+pad);
}

func fil_response(filt, w)
/* DOCUMENT fil_response(filt, w)

     return the complex response of FILT at the frequencies W.
     The frequency scale for W depends on how FILT has been scaled;
     filters are rational functions in W.

     The to_db and to_phase functions may be useful for extracting
     the attenuation and phase parts of the complex response.

   SEE ALSO: filter, fil_butter, fil_bessel, fil_cheby1, fil_cheby2,
             fil_delay, to_db, to_phase
 */
{
  np= long(filt(1));
  nz= long(filt(2));
  w*= 1i;
  y= np? fil_poly(filt(5+nz:4+nz+np),w) : 0.0;
  return fil_poly(filt(4:4+nz),w)/(1.+y*w);
}

func fil_delay(filt, hz)
/* DOCUMENT fil_delay(filt)
         or fil_delay(filt, 1)

     return the group delay d(phase)/dw at w=0 (zero frequency) for
     filter FILT.  By default, FILT is assumed to be normalized 
     to an angular frequency (e.g.- radians per second), but if
     the 2nd parameter is non-nil and non-0 FILT is assumed to be
     normalized to a circular frequency (e.g.- Hz or GHz).

   SEE ALSO: filter, fil_butter, fil_bessel, fil_cheby1, fil_cheby2,
             fil_response, to_db, to_phase
 */
{
  np= long(filt(1));
  nz= long(filt(2));
  dpdw= np? filt(5+nz) : 0.0;
  if (nz) dpdw-= filt(5)/filt(4);
  if (hz) dpdw/= 2.*pi;
  return dpdw;
}

func fil_poly(c, x)
/* DOCUMENT fil_poly(c, x)
     return c(1) + c(2)*x + c(3)*x^2 + c(4)*x^3 + ...
 */
{
  n= numberof(c);
  y= array(c(n), dimsof(x));
  while(--n) y= y*x+c(n);
  return y;
}

func fil_make(poles, zeroes)
/* DOCUMENT filt= fil_make(poles, zeroes)

     given the complex POLES and ZEROES, return a FILT.  The real
     parts of POLES must all be negative to make a stable FILT.
     Both POLES and ZEROES must occur in conjugate pairs in order to
     make a real filter (the returned filter is always real).

     The returned filter always has a0=1 (its DC gain is 1).

   SEE ALSO: filter, fil_analyze
 */
{
  np= numberof(poles);
  nz= numberof(zeroes);
  filt= array(0., 4+nz+np);
  filt(1:4)= [np,nz,0,1];
  if (nz) {
    a= array(0i, nz+1);
    a(0)= 1.0;
    for (i=1 ; i<=nz ; i++) a(1:-1)-= a(2:0)*zeroes(i);
    a/= double(a(1));
    filt(4:4+nz)= a;              /* discards imaginary part */
  }
  if (np) {
    a= array(0i, np+1);
    a(0)= 1.0;
    for (i=1 ; i<=np ; i++) a(1:-1)-= a(2:0)*poles(i);
    a= a(2:0)/double(a(1));
    filt(5+nz:4+nz+np)= a;        /* discards imaginary part */
  }
  return filt;
}

func fil_analyze(filt, &poles, &zeroes)
/* DOCUMENT fil_analyze, filt, poles, zeroes

     given a FILT, return the complex POLES and ZEROES, sorted in
     order of increasing imaginary part.  The real parts of POLES will
     all be negative if the FILT is stable.

   SEE ALSO: filter, fil_make
 */
{
  require, "zroots.i";
  np= long(filt(1));
  nz= long(filt(2));
  poles= zeroes= [];
  if (nz>0) zeroes= zroots(filt(4:4+nz), imsort=1);
  if (np>0) poles= zroots(grow([1.],filt(5+nz:4+nz+np)), imsort=1);
}

func fil_bessel(np, wc, db, natural=)
/* DOCUMENT filt= fil_bessel(np, wc, db)

     returns the lowpass Bessel filter with NP poles, normalized
     such that at angular frequency WC, the attenuation is DB decibels.
     (That is, the attenuation factor is 10^(.05*DB) at WC,
      so that to_db(response(filt,WC))==DB.)

     A Bessel filter has the most nearly constant group delay time
     d(phase)/dw of any filter of the same order.  It minimizes pulse
     distortion, but does not cut off very rapidly in frequency.

     If WC is nil or zero, it defaults to 1.0.

     If DB is nil, the filter is normalized such that both the s^0
     and s^NP terms are 1, unless the natural= keyword is non-zero,
     in which case the filter is normalized such that the group delay
     d(phase)/dw is -1 at w=0.

   SEE ALSO: filter, fil_analyze
 */
{
  filt= array(0., 4+np);
  filt(1:4)= [np,0,0,1];
  if (np) {
    ca= cb= array(0., np+1);
    ca(1)= 1.;
    cb(1:2)= c= [1.,1.];
    for (i=2 ; i<=np ; i++,ca=cb,cb=c) c= (2.*i-1.)*cb + roll(ca,2);
    /* several different normalizations are used:
     * (0) note c(1) = (2n-1)!! (product of odd numbers <= 2*np-1)
     * (1) c/= c(1) normalizes to a group delay -d(phase)/dw = 1.0
     *              for all orders (coefficient c(2)=c(1)=1)
     * (2) c/= c(1)^span(1.,0.,np+1) normalizes both leading and trailing
     *              polynomial coefficients to 1.0
     *              corresponds to w->w*((2n-1)!!)^(1/n)
     *                limit as n->big is w->w*(2*n/e^2), so
     *                phase delay becomes proportional to order n
     * (3) scale w so that polynomial value at w=1 is sqrt(0.5) (3 db)
     *              (or some other value) independent of order
     */
    if (!natural) c/= c(1)^span(1.,0.,np+1);
    else          c/= c(1);
    filt(4:4+np)= c;
  }
  return fil_normalize(filt, wc, db);
}

func fil_butter(np, wc, db)
/* DOCUMENT filt= fil_butter(np, wc, db)

     returns the lowpass Butterworth filter with NP poles, normalized
     such that at angular frequency WC, the attenuation is DB decibels.
     (That is, the attenuation factor is 10^(.05*DB) at WC,
      so that to_db(response(filt,WC))==DB.)

     A Butterworth filter is the best Taylor series approximation to
     the ideal lowpass filter (a step in frequency) response at both
     w=0 and w=infinity.

     For wc=1 and db=10*log10(2), the square of the Butterworth frequency
     response is 1/(1+w^(2*np)).

     If WC is nil or zero, it defaults to 1.0.

     If DB is nil, the filter is normalized "naturally", which is the
     same as DB=10*log10(2).

   SEE ALSO: filter, fil_analyze, butter
 */
{
  filt= array(0., 4+np);
  filt(1:4)= [np,0,0,1];
  /* poles: 1i*exp(0.5i*pi/np*(2*indgen(np)-1)) */
  a= pi/(2*np);
  b= cos(a*indgen(0:np-1))/sin(a*indgen(np));
  for (i=1,a=1.0 ; i<=np ; i++) filt(4+i)= a= a*b(i);
  if (db) {
    if (!wc) wc= 1.0;
    wc*= (10.^(0.1*db)-1.)^(-0.5/np);
  }
  if (wc) filt(5:4+np)/= wc^indgen(np);
  return filt;
}

func fil_cheby1(np, ripple, wc, db)
/* DOCUMENT filt= fil_cheby1(np, ripple, wc, db)

     returns the lowpass Chebyshev type I filter with NP poles, and
     passband ripple RIPPLE decibels, normalized such that at
     angular frequency WC, the attenuation is DB decibels.
     (That is, the attenuation factor is 10^(.05*DB) at WC,
      so that to_db(response(filter,WC))==DB.)

     A Chebyshev type I filter gives the smallest maximum error over the
     passband for any filter that is a Taylor series approximation to
     the ideal lowpass filter (a step in frequency) response at
     w=infinity.  It has NP/2 ripples of amplitude RIPPLE in its passband,
     and a smooth stopband.

     For wc=1 and db=ripple, the square of the Chebyshev frequency
     response is 1/(1+eps2*Tnp(w)), where eps2 = 10^(ripple/10)-1,
     and Tnp is the np-th Chebyshev polynomial, cosh(np*acosh(x)) or
     cos(np*acos(x)).

     If WC is nil or zero, it defaults to 1.0.

     If DB is nil, the filter is normalized "naturally", which is the
     same as DB=RIPPLE.

   SEE ALSO: filter, fil_analyze, cheby1
 */
{ /* passband ripple */
  eps21= 10.^(0.1*ripple);
  reps= 1./sqrt(eps21-1.);
  ripple= asinh(reps)/np;
  poles= 1i*cos(0.5*pi/np*(2*indgen(np)-1) - 1i*ripple);
  filt= fil_make(poles);
  if (!(np%2)) filt(4)/= sqrt(eps21);
  if (db) {
    if (!wc) wc= 1.0;
    wc*= sech(acosh(reps*sqrt(10.^(0.1*db)-1.))/np);
  }
  if (wc) filt(5:4+np)/= wc^indgen(np);
  return filt;
}

func fil_cheby2(np, atten, wc, db)
/* DOCUMENT filt= fil_cheby2(np, atten, wc, db)

     returns the lowpass Chebyshev type II filter with NP poles, and
     stopband attenuation ATTEN decibels, normalized such that at
     angular frequency WC, the attenuation is DB decibels.
     (That is, the attenuation factor is 10^(.05*DB) at WC,
      so that to_db(response(filter,WC))==DB.)

     This is also called an inverse Chebyshev filter, since its poles
     are the reciprocals of a Chebyshev type I filter.  It has NP zeroes
     as well as NP poles.

     A Chebyshev type II filter gives the smallest maximum leakage over
     the stopband for any filter that is a Taylor series approximation to
     the ideal lowpass filter (a step in frequency) response at
     w=0.  It has NP/2 ripples of amplitude ATTEN in its stopband,
     and a smooth passband.

     For wc=1 and db=ripple, the square of the inverse Chebyshev frequency
     response is 1 - 1/(1+eps2*Tnp(1/w)), where eps2 = 10^(ripple/10)-1 =
     1/(10^(atten/10)-1) and Tnp is the np-th Chebyshev polynomial,
     cosh(np*acosh(x)) or cos(np*acos(x)).

     If WC is nil or zero, it defaults to 1.0.

     If DB is nil, the filter is normalized "naturally", which is the
     same as DB=ATTEN.

   SEE ALSO: filter, fil_analyze, cheby2
 */
{ /* stopband attenuation (ripple) */
  reps= sqrt(10.^(0.1*atten)-1.);
  atten= asinh(reps)/np;
  wk= 0.5*pi/np*(2*indgen(np)-1);
  poles= -1i/cos(wk - 1i*atten);
  zeroes= 1i/cos(wk);
  if (np==1) zeroes= [];
  else if (np%2) zeroes= grow(zeroes(1:(np-1)/2),zeroes((np+3)/2:0));
  filt= fil_make(poles, zeroes);
  if (db) {
    if (!wc) wc= 1.0;
    wc*= cosh(acosh(reps/sqrt(10.^(0.1*db)-1.))/np);
  }
  if (wc) {
    nz= numberof(zeroes);
    if (nz) filt(5:4+nz)/= wc^indgen(nz);
    filt(5+nz:4+nz+np)/= wc^indgen(np);
  }
  return filt;
}

func fil_cauer(np, ripple, atten, wc, db)
/* DOCUMENT filt= fil_cauer(np, ripple, atten, wc, db)
         or filt= fil_cauer(np, ripple, -skirt, wc, db)

     returns the lowpass Cauer (elliptic) filter with NP poles, passband
     ripple RIPPLE and stopband attenuation ATTEN decibels, normalized
     such that at angular frequency WC, the attenuation is DB decibels.
     (That is, the attenuation factor is 10^(.05*DB) at WC,
      so that to_db(response(filter,WC))==DB.)

     If the third parameter is negative, its absolute value is SKIRT,
     the ratio of the frequency at which the stopband attenuation is
     first reached to the frequency at which the passband ends (where
     the attenuation is RIPPLE).  The closer to 1.0 SKIRT is, the
     smaller the equivalent ATTEN would be.  The external variable
     cauer_other is set to ATTEN if you provide SKIRT, and to SKIRT
     if you provide ATTEN.

     The Cauer filter has NP zeroes as well as NP poles.

     Consider the four parameters: (1) filter order, (2) transition
     ("skirt") bandwidth, (3) passband ripple, and (4) stopband ripple.
     Given any three of these, the Cauer filter minimizes the fourth.

     If WC is nil or zero, it defaults to 1.0.

     If DB is nil, the filter is normalized "naturally", which is the
     same as DB=RIPPLE.

   SEE ALSO: filter, fil_analyze, cauer
 */
{
  extern cauer_other;
  eps2= 10.^(0.1*ripple)-1.;
  if (atten > 0.) {
    mb= eps2/(10.^(0.1*atten)-1.);
    ma= _cauer_parameter(np, mb);
    cauer_other= 1./sqrt(ma);
  } else {
    ma= 1./(atten*atten);
    mb= _cauer_parameter(1./np, ma);
    cauer_other= 10.*log10(1.+eps2/mb);
  }
  ekb= ellip_k(mb);
  eka= ellip_k(ma);
  rat= np*ekb/eka;  /* equals ellip_k(1-mb)/ellip_k(1-ma) */

  if (np>1) {
    zeroes= indgen(np-1:1:-2)(-:1:2,)(*);
    zeroes(1:0:2)*= -1.;
    zeroes= 1i/(sqrt(ma)*sin(ell_am(zeroes*eka/np,ma)));
  }
  a= ell_f(atan(1./sqrt(eps2)),1.-mb)/rat;
  b= (2.*indgen(np)-(np+1))*eka/np;
  phi= ell_am(b,ma);
  d= dn_(phi);
  s= sin(phi);
  c= cos(phi);
  phi= ell_am(a,1.-ma);
  dp= dn_(phi);
  sp= sin(phi);
  cp= cos(phi);
  poles= (-c*d*sp*cp + 1i*s*dp)/(cp*cp+ma*(s*sp)^2);

  filt= fil_make(poles, zeroes);
  if (db) {
    if (!wc) wc= 1.0;
    wc*= dn_(ell_am(ell_f(asin(sqrt((1.-eps2/(10.^(0.1*db)-1.))/(1.-mb))),
                          1.-mb) / rat, 1.-ma));
  }
  if (wc) {
    nz= numberof(zeroes);
    if (nz) filt(5:4+nz)/= wc^indgen(nz);
    filt(5+nz:4+nz+np)/= wc^indgen(np);
  }
  return filt;
}

func _cauer_parameter(n, mb)
{
  /*   K'(mb)/K'(ma) = n*K(mb)/K(ma)
   * where the rational Chebyshev polynomial is
   *   sn( n*K(mb)/K(ma) * asn(w|ma) | mb )
   * <asn means inverse of sn>
   *
   * the passband ripple is determined by the additional
   * parameter e, and the stopband (w>1) ripple is e/mb > e
   *
   * now K'(ma)/K(ma) = K'(mb)/K(mb) * (1/n)
   * and q(m) = exp(-pi*K'(m)/K(m)) is called the "nome" of m,
   *   m ~ 16*q for small q
   */
  require, "elliptic.i";
  if (!mb)    return 0.;
  if (mb==1.) return 1.;
  if (mb < 1.e-4) u= -log(poly(mb/16., 0., 1., 8., 84., 992.))/pi;
  else            u= ellip_k(1.-mb)/ellip_k(mb);
  u/= n;                   /* K'(ma)/K(ma) */
  if (!u) return 1.0;
  recip= (u<1.);
  if (recip) u= 1./u;
  q= qn= exp(-pi*u);       /* < 1/20 */
  q2n= num= 1.;
  den= 0.5;
  for (;;) {
    den+= q2n*qn;
    q2n*= qn*qn;
    numx= num+q2n;
    if (numx==num) break;
    num= numx;
    qn*= q;
  }
  ma= q*(num/den)^4;
  return recip? (1.-ma) : ma;
}

func fil_normalize(filt, wc, db)
/* xxDOCUMENT filt= fil_normalize(filt, wc, db)
     normalize the all pole FILT to give DB decibels attenuation
     at frequency WC, by rescaling the frequency.
     Assumes w=1 is a good starting point for search for initial filt;
     probably should not be used except for cases in this file.
 */
{
  if (filt(1)>1.5 && !filt(2)) {
    np= long(filt(1));
    if (db) {
      c= filt(4:4+np);             /* polynomial (denominator) */
      c1= c(1);
      c(1)= 1.0;
      c/= c1;
      d= c(2:0)*indgen(np);          /* derivative */
      db= 10.^(0.05*db);             /* y value to be found */
      x= 1.0;                        /* initial guess */
      for (i=1 ; i<100 ; i++) {
        py= fil_poly(c, 1i*x);
        y= abs(py);
        dydx= -(fil_poly(d, 1i*x)*conj(py)).im/y;
        dx= (db-y)/dydx;
        if (abs(dx)<1.e-12*x) break;
        x+= dx;
      }
      if (i>=100) error, "failed to converge to specified dB level";
      if (!wc) wc= 1.0;
      wc/= x;
    }
    if (wc) filt(4:4+np)/= wc^indgen(0:np);
  }
  return filt;
}

/* ------------------------------------------------------------------------ */

func to_db(signal, ref)
/* DOCUMENT to_db(signal, ref)
         or to_db(signal)

     return 20.*log10(abs(SIGNAL)/REF), the number of decibels
     corresponding to the input SIGNAL.  REF defaults to 1.0.

   SEE ALSO: fil_response, to_phase
 */
{
  if (is_void(ref)) ref= 1.0;
  return 20.*log10(max(abs(signal)/ref,1.e-37));
}

func to_phase(signal, degrees)
/* DOCUMENT to_phase(signal)
         or to_phase(signal, 1)

     return atan(SIGNAL.im,SIGNAL.re), the phase of the input SIGNAL.
     If the second argument is present and non-0, the phase will be in
     degrees; by default the phase is in radians.

     To_phase attempts to unroll any jumps from -180 to +180 degrees
     or vice-versa; zero phase will be taken somewhere near the middle
     of the signal.  The external variable to_phase_eps controls the
     details of this unrolling; you can turn off unrolling by setting
     to_phase_eps=0.0 (initially it is 0.3).

   SEE ALSO: fil_response, to_phase
 */
{
  p= atan(signal.im, signal.re+!abs(signal));
  np= numberof(p);
  if (np>2) {
    p0= p(1:-1);
    p1= p(2:0);
    eps= (1.00000001-to_phase_eps)*pi;
    up= where((p0>=eps)&(p1<=-eps));
    dn= where((p0<=-eps)&(p1>=eps));
    adj= 0.*p0;
    if (numberof(up)) adj(up)= 2.*pi;
    if (numberof(dn)) adj(dn)= -2.*pi;
    p+= adj(cum) - adj(np/2+1);
  }
  if (degrees) p*= 180/pi;
  return p;
}

to_phase_eps= 0.3;

/* ------------------------------------------------------------------------ */
/* the frequency responses of the Butterworth, Chebyshev, and Cauer
 * filters have simple analytic forms
 * - note the frequency scaling rules are also simple */

func butter(np,w,wc,db)
/* DOCUMENT butter(np, w)
         or butter(np, w, wc, db)

     return frequency response (amplitude) for Butterworth filter;
     the parameters are the same as for fil_butter.

   SEE ALSO: fil_butter
 */
{
  if (db) w*= (10.^(0.1*db)-1.)^(0.5/np);
  if (wc) w/= wc;
  return 1./sqrt(1.+w^(2*np));
}

func cheby1(np,ripple,w,wc,db)
/* DOCUMENT cheby1(np, ripple, w)
         or cheby1(np, ripple, w, wc, db)

     return frequency response (amplitude) for Chebyshev filter;
     the parameters are the same as for fil_cheby1.

   SEE ALSO: fil_cheby1
 */
{
  eps2= 10.^(0.1*ripple)-1.;
  if (db) w*= cosh(acosh(sqrt((10.^(0.1*db)-1.)/eps2))/np);
  if (wc) w/= wc;
  /* 1/sqrt(1+eps2*Tn(w)^2) where Tn is nth Chebyshev polynomial */
  mask= (w<=1.0);
  list= where(mask);
  if (numberof(list)) cn= 1./(1.+eps2*cos(np*acos(w(list)))^2);
  list= where(!mask);
  if (numberof(list)) {
    cnw= sech(np*acosh(w(list)))^2;
    cnw= cnw/(cnw+eps2);
  }
  return sqrt(merge(cn,cnw,mask));
}

func cheby2(np,atten,w,wc,db)
/* DOCUMENT cheby2(np, atten, w)
         or cheby2(np, atten, w, wc, db)

     return frequency response (amplitude) for inverse Chebyshev filter;
     the parameters are the same as for fil_cheby2.

   SEE ALSO: fil_cheby2
 */
{
  eps2= 1./(10.^(0.1*atten)-1.);
  if (db) w*= sech(acosh(1./sqrt((10.^(0.1*db)-1.)*eps2))/np);
  if (wc) w/= wc;
  /* sqrt(1-1/(1+eps2*Tn(1/w)^2)) where Tn is nth Chebyshev polynomial */
  w= (1.+(!w)*1.e99)/(w+(!w));
  mask= (w<=1.0);
  list= where(mask);
  if (numberof(list)) cn= 1./(1.+eps2*cos(np*acos(w(list)))^2);
  list= where(!mask);
  if (numberof(list)) {
    cnw= sech(np*acosh(w(list)))^2;
    cnw= cnw/(cnw+eps2);
  }
  return sqrt(1.-merge(cn,cnw,mask));
}

func cauer(n, ripple, atten, w, wc, db)
/* DOCUMENT cauer(np, ripple, atten, w)
         or cauer(np, ripple, atten, w, wc, db)

     return frequency response (amplitude) for Cauer filter;
     the parameters are the same as for fil_cauer.

   SEE ALSO: fil_cauer
 */
{
  eps2= 10.^(0.1*ripple)-1.;
  if (atten > 0.) {
    mb= eps2/(10.^(0.1*atten)-1.);
    ma= _cauer_parameter(n, mb);
  } else {
    ma= 1./(atten*atten);
    mb= _cauer_parameter(1./n, ma);
  }
  ekb= ellip_k(mb);
  rat= n*ekb/ellip_k(ma);
  if (db)
    w/= dn_(ell_am(ell_f(asin(sqrt((1.-eps2/(10.^(0.1*db)-1.))/(1.-mb))),
                         1.-mb) / rat, 1.-ma));
  if (wc) w/= wc;

  w= abs(w);
  mask= (w<=1.);
  list= where(mask);
  if (numberof(list)) {
    x= rat*ell_f(asin(w(list)),ma);
    if (!(n%2)) x+= ekb;
    f1= sin(ell_am(x,mb));
  }
  list= where(!mask);
  if (numberof(list)) {
    w= 1./w(list);
    ka= sqrt(ma);
    mask2= (w>ka);
    list= where(mask2);
    if (numberof(list)) {
      x= w(list);
      x= rat*ell_f(asin(sqrt((1.-x*x)/(1.-ma))),1.-ma);
      f21= 1./dn_(ell_am(x,1.-mb));
    }
    list= where(!mask2);
    if (numberof(list)) {
      x= rat*ell_f(asin(w(list)/ka),ma);
      if (!(n%2)) x+= ekb;
      f22= 1./(sqrt(mb)*sin(ell_am(x,mb)));
    }
    f2= merge(f21,f22,mask2);
  }
  f= merge(f1,f2,mask);

  return 1./sqrt(1.+eps2*f*f);
}

/* ------------------------------------------------------------------------ */
/* also in convol.i */

func fft_good(n)
/* DOCUMENT fft_good(n)

     returns the smallest number of the form 2^x*3^y*5^z greater
     than or equal to n.  An fft of this length will be much faster
     than a number with larger prime factors; the speed difference
     can be an order of magnitude or more.

     For n>100, the worst cases result in a little over a 11% increase
     in n; for n>1000, the worst are a bit over 6%; still larger n are
     better yet.  The median increase for n<=10000 is about 1.5%.

   SEE ALSO: fft, fft_setup, convol
 */
{
  if (n<7) return max(n,1);
  logn= log(n);
  n5= 5.^indgen(0:long(logn/log(5.) + 1.e-6));  /* exact integers */
  n3= 3.^indgen(0:long(logn/log(3.) + 1.e-6));  /* exact integers */
  n35= n3*n5(-,);             /* fewer than 300 numbers for n<5e9 */
  n35= n35(where(n35<=n));
  n235= 2^long((logn-log(n35))/log(2.) + 0.999999) * long(n35);
  return min(n235);
}

/* ------------------------------------------------------------------------ */
