/*
 * $Id: legndr.i,v 1.1 2005-09-18 22:06:00 dhmunro Exp $
 * Compute Legendre polynomials and associated Legendre functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* After Numerical Recipes, by Press, et. al., section 6.6.  */

func legndr(l,m, x)
/* DOCUMENT legndr(l,m, x)
     return the associated Legendre function Plm(x).  The X may
     be an array (-1<=x<=1), but L and M (0<=M<=L) must be scalar
     values.  For m=0, these are the Legendre polynomials Pl(x).
     Relation of Plm(x) to Pl(x):
       Plm(x) = (-1)^m (1-x^2)^(m/2) d^m/dx^m(Pl(x))
     Relation of Plm(x) to spherical harmonics Ylm:
       Ylm(theta,phi)= sqrt((2*l+1)(l-m)!/(4*pi*(l+m)!)) *
                           Plm(cos(theta)) * exp(1i*m*phi)
   SEE ALSO: ylm_coef
 */
{
  m+= 0;
  l+= 0;
  if (structof(m)!=long || structof(l)!=long ||
      m<0 || m>l) error, "l, m integers with 0<=m<=l for Plm";

  if (m > 0) {
    /* sqrt blows up if x out of range */
    pmm= ((m%2? -1:1) * exp(sum(log(indgen(1:2*m:2))))) *
      (sqrt((1.-x)*(1.+x))^m);
  } else {
    if (anyof(abs(x)>1.0)) error, "-1<=x<=1 for Plm";
    pmm= array(1.0, dimsof(x));
  }

  if (l==m) return pmm;

  pmmp1= (2*m+1)*x*pmm;
  if (l==m+1) return pmmp1;

  for (ll=m+2 ; ; ++ll) {
    c1= double(ll-m);
    c= (ll+m-1)/c1;
    c1= (2*ll-1)/c1;
    pll= c1*x*pmmp1 - c*pmm;
    if (ll==l) return pll;
    pmm= pmmp1;
    pmmp1= pll;
  }
}

func ylm_coef(l,m)
/* DOCUMENT ylm_coef(l,m)
     return sqrt((2*l+1)(l-m)!/(4*pi*(l+m)!)), the normalization
     coefficient for spherical harmonic Ylm with respect to the
     associated Legendre function Plm.  In this implementation,
     0<=m<=l; use symmetry for m<0, or use sines and cosines
     instead of complex exponentials.  Unlike Plm, array L and M
     arguments are permissible here.
     WARNING: These get combinitorially small with large L and M;
     probably Plm is simultaneously blowing up and should be
     normalized directly in legndr if what you want is Ylm.  But
     I don't feel like working all that out -- if you need large
     L and M results, you should probably be working with some
     sort of asymptotic form anyway...
   SEE ALSO: legndr
 */
{
  m+= 0;
  l+= 0;
  if (structof(m)!=long || structof(l)!=long ||
      anyof(m<0) || anyof(m>l))
    error, "l, m integers with 0<=m<=l; use symmetry for m<0";

  lm= l-m;
  not_scalar= dimsof(lm)(1);
  if (not_scalar) result= array(1.0, dimsof(lm));
  else result= [1.0];
  iactive= indgen(numberof(lm));
  for (list=where(lm) ; numberof(list) ; list=where(lm)) {
    lm= lm(list);
    iactive= iactive(list);
    result(iactive)*= lm;
    lm-= 1;
  }

  lm= l+m;
  iactive= indgen(numberof(lm));
  for (list=where(lm) ; numberof(list) ; list=where(lm)) {
    lm= lm(list);
    iactive= iactive(list);
    result(iactive)/= lm;
    lm-= 1;
  }

  result= sqrt(((2*l+1)/(4.*pi)) * result);
  return not_scalar? result : result(1);
}
