/*
 * $Id: dawson.i,v 1.2 2007-11-10 20:03:49 dhmunro Exp $
 * Dawson's integral and error functions after SLATEC
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func dawson(x)
/* DOCUMENT dawson(x)
 *   return Dawson's integral, exp(-x^2)*integral[0 to x](exp(t^2)*dt)
 *   maximum is dawson(0.9241388730) = 0.5410442246
 *   inflection point is dawson(1.5019752682) = 0.4276866160
 * SEE ALSO: erf, erfc
 */
{
  { local mask1,mask2,mask3,xx,yy; }
  if (structof(x)==complex) error, "dawson function not valid for complex";
  y = abs(x);
  if (any_in(,y,1., mask1, yy, x,xx))
    d1 = xx * (0.75 + _cheby_eval(_dawson_1, 2.*yy*yy-1.));
  if (any_in(1.,y,4., mask2, yy, x,xx))
    d2 = xx * (0.25 + _cheby_eval(_dawson_2, 0.125*yy*yy-1.));
  if (any_in(4.,y,, mask3, yy, x,xx))
    d3 = (0.50 + _cheby_eval(_dawson_3, 32./(yy*yy)-1.)) / xx;
  return merge_n(d1,mask1, d2,mask2, d3,mask3);
}

_dawson_1 = [-.006351734375145949, -.229407147967738690,  .022130500939084764,
             -.001549265453892985,  .000084973277156849, -.000003828266270972,
              .000000146285480625, -.000000004851982381,  .000000000142146357,
             -.000000000003728836,  .000000000000088549, -.000000000000001920,
              .000000000000000038];
_dawson_2 = [-.056886544105215527, -.318113469961681310,  .208738454136422370,
             -.124754099137791310,  .067869305186676777, -.033659144895270940,
              .015260781271987972, -.006348370962596214,  .002432674092074852,
             -.000862195414910650,  .000283765733363216, -.000087057549874170,
              .000024986849985481, -.000006731928676416,  .000001707857878557,
             -.000000409175512264,  .000000092828292216, -.000000019991403610,
              .000000004096349064, -.000000000800324095,  .000000000149385031,
             -.000000000026687999,  .000000000004571221, -.000000000000751873,
              .000000000000118931, -.000000000000018116,  .000000000000002661,
             -.000000000000000377,  .000000000000000051];
_dawson_3 = [ .01690485637765704,  .00868325227840695,  .00024248640424177,
              .00001261182399572,  .00000106645331463,  .00000013581597947,
              .00000002171042356,  .00000000286701050, -.00000000019013363,
             -.00000000030977804, -.00000000010294148, -.00000000000626035,
              .00000000000856313,  .00000000000303304, -.00000000000025236,
             -.00000000000042106, -.00000000000004431,  .00000000000004911,
              .00000000000001235, -.00000000000000578, -.00000000000000228,
              .00000000000000076,  .00000000000000038, -.00000000000000011,
             -.00000000000000006,  .00000000000000002];

func erf(x)
/* DOCUMENT erf(x)
 *   return erf(x), 2./sqrt(pi) * integral[0 to x](exp(-t^2)*dt)
 * SEE ALSO: erfc, dawson
 */
{
  { local mask1,mask2,mask3,mask4,xx,yy,zz; }
  if (structof(x)==complex) error, "erf function not valid for complex";
  y = abs(x);
  if (any_in(,y,1., mask1, yy, x,xx))
    e1 = xx * (1.0 + _cheby_eval(_erf_1, 2.*yy*yy-1.));
  if (any_in(1.,y,, mask2, yy, x,xx)) {
    z = yy*yy;
    yy = exp(-z)/yy;
    if (any_in(,z,4., mask3, zz, yy,y))
      e3 = y * (0.5 + _cheby_eval(_erfc_1, (8./zz-5.)/3.));
    if (any_in(4.,z,, mask4, zz, yy,y))
      e4 = y * (0.5 + _cheby_eval(_erfc_2, 8./zz-1.));
    e2 = sign(xx) * (1.0 - merge_n(e3,mask3, e4,mask4));
  }
  return merge_n(e1,mask1, e2,mask2);
}

func erfc(x)
/* DOCUMENT erfc(x)
 *   return erfc(x), 2./sqrt(pi) * integral[x to infinity](exp(-t^2)*dt)
 * SEE ALSO: erf, dawson
 */
{
  { local mask1,mask2,mask3,mask4,xx,yy,zz; }
  if (structof(x)==complex) error, "erfc function not valid for complex";
  y = abs(x);
  if (any_in(,y,1., mask1, yy, x,xx))
    e1 = 1.0 - xx * (1.0 + _cheby_eval(_erf_1, 2.*yy*yy-1.));
  if (any_in(1.,y,, mask2, yy, x,xx)) {
    z = yy*yy;
    yy = exp(-z)/yy;
    if (any_in(,z,4., mask3, zz, yy,y))
      e3 = y * (0.5 + _cheby_eval(_erfc_1, (8./zz-5.)/3.));
    if (any_in(4.,z,, mask4, zz, yy,y))
      e4 = y * (0.5 + _cheby_eval(_erfc_2, 8./zz-1.));
    e2 = 2.*(xx<0.) + sign(xx)*merge_n(e3,mask3, e4,mask4);
  }
  return merge_n(e1,mask1, e2,mask2);
}

_erf_1 = [-.049046121234691808, -.142261205103713640,  .010035582187599796,
          -.000576876469976748,  .000027419931252196, -.000001104317550734,
           .000000038488755420, -.000000001180858253,  .000000000032334215,
          -.000000000000799101,  .000000000000017990, -.000000000000000371,
           .000000000000000007];
_erfc_1 = [-.069601346602309501, -.041101339362620893,  .003914495866689626,
           -.000490639565054897,  .000071574790013770, -.000011530716341312,
            .000001994670590201, -.000000364266647159,  .000000069443726100,
           -.000000013712209021,  .000000002788389661, -.000000000581416472,
            .000000000123892049, -.000000000026906391,  .000000000005942614,
           -.000000000001332386,  .000000000000302804, -.000000000000069666,
            .000000000000016208, -.000000000000003809,  .000000000000000904,
           -.000000000000000216,  .000000000000000052];
_erfc_2 = [ .071517931020292500, -.026532434337606719,  .001711153977920853,
           -.000163751663458512,  .000019871293500549, -.000002843712412769,
            .000000460616130901, -.000000082277530261,  .000000015921418724,
           -.000000003295071356,  .000000000722343973, -.000000000166485584,
            .000000000040103931, -.000000000010048164,  .000000000002608272,
           -.000000000000699105,  .000000000000192946, -.000000000000054704,
            .000000000000015901, -.000000000000004729,  .000000000000001432,
           -.000000000000000439,  .000000000000000138, -.000000000000000048];

func _cheby_eval(cs, x)
{
  n = numberof(cs);
  b0 = b1 = 0.;
  x *= 2.;
  for (i=0 ; i<n ; ++i) {
    b2 = b1;
    b1 = b0;
    b0 = x*b1 - b2 + cs(n-i);
  }
  return 0.5*(b0-b2);
}

func any_in(left,x,right, &mask, &xx, a,&aa, b,&bb, c,&cc)
/* DOCUMENT any_in(left,x,right, mask, xx, a,aa, b,bb, c,cc
 *   return the number of elements of the array X which are in the
 *   interval LEFT < X <= RIGHT.  Also return MASK, which has the
 *   shape of X and is 1 where X is in the interval and 0 otherwise,
 *   and XX = X(where(MASK)).  Up to three optional arrays A, B, and C
 *   of the same shape as X may be supplied; the arrays AA, BB, and CC
 *   analogous to XX are returned.  LEFT or RIGHT may be [] for the
 *   interval to extend to infinity on the corresponding side.
 *   LEFT and/or RIGHT may be arrays as long as they are conformable
 *   with X.
 * SEE ALSO: merge_n, merge
 */
{
  if (is_void(left)) mask = array(1n, dimsof(x));
  else mask = (x>left);
  if (!is_void(right)) mask &= (x<=right);
  list = where(mask);
  n = numberof(list);
  if (n) {
    xx = x(list);
    if (!is_void(a)) {
      aa = a(list);
      if (!is_void(b)) {
        bb = b(list);
        if (!is_void(c)) cc = c(list);
      }
    }
  }
  return n;
}

func merge_n(val,mask, ..)
/* DOCUMENT merge_n(val1,mask1, val2,mask2, ...)
 *   return array with shape of MASKi (which must all have same shape)
 *   and values VALi where MASKi is true.  Unspecified values will be
 *   zero; the data type of the result is the data type of the first
 *   non-nil VALi.  Each VALi must be a 1D array of length sum(MASKi!=0).
 * SEE ALSO: any_in, merge
 */
{
  rslt = dimsof(mask);
  while (is_void(val)) {
    val = next_arg();
    mask = next_arg();
    if (is_void(mask)) return array(0., rslt);
  }
  rslt = array(structof(val), rslt);
  for (;;) {
    rslt(where(mask)) = val;
    do {
      val = next_arg();
      mask = next_arg();
      if (is_void(mask)) return rslt;
    } while (is_void(val));
  }
}
