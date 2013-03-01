/* ylm.i
 * functions to compute spherical harmonics
 * http://www.ppsloan.org/publications/StupidSH26.pdf
 *   Stupid Spherical Harrmonics (SH) Tricks
 *   Peter Pike-Sloan, Microsoft Corporation
 *     ylm only, sec. A1, A2 give quick recursion, polynomial table thru L=5
 * http://azufre.quimica.uniovi.es/articles/Theochem419-19-ov-BF97-rotation-matrices.pdf
 *   Evaluation of the rotation matrices in the basis of real spherical harmonics
 *   Miguel A. Blanco, M. Florez, M. Bermejo
 *     using recurrences for both ylm and dlmm
 * http://muri.lci.kent.edu/Publications/Papers_NYU/Gimbutas_JCommPhys228_2009.pdf
 *   A fast and stable method for rotating spherical harmonic expansions
 *   Z. Gimbutas, L. Greengard
 *     limitations of recurrences, algorithm that works for very high L
 * http://www.math.univ-toulouse.fr/Archive-MIP/publis/files/06.30.pdf
 *   Rotation matrices for real spherical harmonics:
 *     general rotations of atomic orbitals in space-fixed axes
 *   Didier Pinchon, Philip E. Hoggan
 *     slick representation of dlmm, why real better than complex for this
 */
/* Copyright (c) 2012, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func ylm(lmax, x, y, z, cplx=)
/* DOCUMENT ylm(lmax, theta, phi)
 *       or ylm(lmax, x, y, z)
 *   return 1+LMAX by 1+LMAX by dimsof(THETA,PHI) or dimsof(X,Y,Z) array
 *   of all real spherical harmonics through L=LMAX.  The (X,Y,Z) are
 *   projected onto the unit sphere to get (sin(THETA)*cos(PHI),
 *   sin(THETA)*sin(PHI), cos(THETA)).  The inverse relation is
 *   THETA = acos(Z), PHI = atan(Y,X).  The point X=Y=Z=0 is mapped to
 *   THETA=0 or (0,0,1).
 *
 *   Supply the cplx=1 keyword to return complex spherical harmonics.
 *
 *   The real spherical harmonics y[l,m] are defined in terms of the complex
 *   spherical harmonics Y[l,m] by:
 *     y[l,m] = sqrt(2) * Re(Y[l,m])       for m>0
 *            = Y[l,0]                     for m=0
 *            = sqrt(2) * Im(Y[l,abs(m)])  for m<0
 *   With the Condon-Shortly phase convention for m<0, the Y[l,m] are:
 *     Y[l,m] = sqrt(0.5) * (y[l,m] + 1i*y[l,-m])           for m>0
 *            = y[l,0]                                      for m=0
 *            = (-1)^m * sqrt(0.5) * (y[l,-m] - 1i*y[l,m])  for m<0
 *     so that conj(Y[l,m]) = (-1)^m Y[l,-m]
 *   (This is the Y[l,m] used by Mathematica and most quantum mechanics
 *    texts, but not necessarily in other disciplines.)
 *
 *   The layout of the Y[l,m] in the first two dimensions of ylm is:
 *     y[l,m] = y(1+l, 1+l-m)   for m>=0
 *            = y(1+l+m, 1+l)   for m<=0
 *     y(i,j) = y[max(i,j)-1, i-j
 *   so  y(,1) = y[0, 0]  y[1, 1]  y[2, 2]  y[3, 3] ...
 *       y(,2) = y[1,-1]  y[1, 0]  y[2, 1]  y[3, 2]
 *       y(,3) = y[2,-2]  y[2,-1]  y[2, 0]  y[3, 1]
 *       y(,4) = y[3,-3]  y[3,-2]  y[3,-1]  y[3, 0] ...
 *                 ...                        ...
 *   You can use the auxilliary function ylm_lm to extract or set particular
 *   [l,m] values in such an array.
 *
 *   Note: Will produce SIGFPE due to overflow for LMAX>150 or so.  The
 *   overflow occurs in an intermediate calculation, but I believe the
 *   domain of the function could not be extended too much further before
 *   overflow would occur in the result.
 *
 * SEE ALSO: ylm_lm, ylm_fit, ylm_eval, legpol
 */
{
  if (is_void(z)) {
    x += array(0., dimsof(x, y));
    r = sin(x);
    z = cos(x);
    x = r * cos(y);
    y = r * sin(y);
  } else {
    r = abs(x, y, z);
    list = where(!r);
    if (numberof(list)) {
      z += array(0., dimsof(r));
      z(list) = r(list) = 1.;
    }
    r = 1./r;
    x *= r;  y *= r;  z *= r;
    /* r = abs(x, y); */
  }

  /* xm = r^m cos(m phi)  ym = r^m sin(m phi) */
  xm = array(1.+0.i, 1+lmax, dimsof(z));
  ym = x + 1i*y;
  if (lmax) xm(2,..) = sqrt(2.)*ym*xm(1,..);
  for (m=2 ; m<=lmax ; ++m) xm(1+m,..) = ym*xm(m,..);
  ym = xm.im;
  xm = xm.re;

  y_lm = array(0., 1+lmax, 1+lmax, dimsof(z));
  local a, b, c, n;
  r = 1.;  /* include r^m factor in xm, ym above */
  for (l=0 ; l<=lmax ; ++l) {
    m1 = 1+lmax-l;
    y_lm(1+l:,l+1,..) = _ylm_plm(l) * (nc=n*c) * xm(1:m1,..);
    if (l<lmax) y_lm(1+l,l+2:,..) = nc(2:)*a(2:,..) * ym(2:m1,..);
  }

  if (cplx) {
    if (lmax) {
      m = indgen(0:lmax)(,-:0:lmax) - indgen(0:lmax)(-:0:lmax,);
      y_lm(where(m)) *= sqrt(0.5);
      ilm = transpose(y_lm, [1,2]);
      y_lm(where(m<0)) *= -1.;
      ilm(where(!m)) = 0.;
      y_lm += 1i*ilm;
      y_lm(where((m<0) & (m&1))) *= -1.;  /* (-1)^m */
    } else {
      y_lm += 0i;
    }
  }
  return y_lm;
}

func ylm_lm(y, l, m, v)
/* DOCUMENT lm = ylm_lm(lmax)
 *       or list = ylm_lm(lmax, l, m)
 *       or lm = ylm_lm(y)
 *       or y_subset = ylm_lm(y, l, m)
 *       or y_old = ylm_lm(y, l, m, y_values)
 *   manipulates an array of values Y returned by the ylm function,
 *   for which the first two indices represent the l and m values of
 *   the spherical harmonic, y[l,m] = y(1+l-min(0,m), 1+l-max(0,m)).
 *   (See help,ylm for a complete description of the indexing of Y.)
 *   Note that for real spherical harmonics, M>0 are the cosine
 *   components and M<0 are the sin components.
 
 *   The first argument to ylm_lm can be either a scalar LMAX, or an
 *   array Y returned by ylm, whose first two dimensions, 1+lmax by
 *   1+lmax, determine lmax.
 *
 *   Called with a single argument, ylm_lm returns the
 *   1+LMAX by 1+LMAX by 2 array of [l,m] values corresponding to the
 *   first two dimensions of a result returned by ylm.
 *
 *   If present, the second and third arguments L and M to ylm_lm must
 *   be conformable arrays of L and M values.  When the first argument
 *   is the scalar LMAX, ylm_lm returns the index list, with dimsof(L,M),
 *   of indices into a 1+LMAX by 1+LMAX array corresponding to those
 *   values of L and M.  For example:
 *      y = ylm(lmax, z);
 *      list = ylm_lm(lmax, l, m);
 *      y_subset = y(list,1,..);  // extracts subset of y
 *
 *   If you only want the index list returned by ylm_lm(lmax,l,m) in
 *   order to extract or to set a subset of a single array with leading
 *   dimensions 1+lmax by 1+lmax, you may pass that array as the first
 *   argument Y to ylm_lm, and ylm_lm will return the extracted values
 *   y_subset as in the previous example, rather than the index list.
 *   Thus, the following extracts the same subset as in the previous
 *   example:
 *      y = ylm(lmax, z);
 *      y_subset = ylm_lm(y, l, m);
 *
 *   Finally, if you wish to set the subset of Y, you may supply a
 *   fourth argument Y_VALUES, in which case ylm_lm returns the previous
 *   values of the subset before changing them in the input Y:
 *      y_old = ylm_lm(y, l, m, y_values);
 *
 *   Note that y_subset (or y_old) has the two leading dimensions of Y
 *   replaced by dimsof(L,M), and that Y_VALUES must be conformable with
 *   y_subset for the purposes of the assignment.
 *
 * SEE ALSO: ylm, ylm_fit
 */
{
  d = dimsof(y);
  if (!numberof(d)) error, "unrecognized first argument";
  scalar = !d(1);
  if (scalar)
    lmax = y;
  else if (d(1)<2 || d(2)!=d(3))
    error, "first argument must be LMAX or 1+LMAX by 1+LMAX array";
  else
    lmax = d(2) - 1;

  if (is_void(l)) {
    l = indgen(0:lmax)(,-:0:lmax);
    m = transpose(l);
    return [max(l,m), l-m];
  }

  list = 1+l+min(m,0) + (1+lmax)*(l-max(m,0));
  if (scalar) return list;
  if (!am_subroutine()) yold = y(list,1,..);
  if (!is_void(v)) y(list,1,..) = v;
  return yold;
}

/* Here are the primary recursions for calculating ylm.  The associated
 * Legendre recursion and a recursion for the normalization coefficients
 * are carried out simultaneously.
 *
 * Ylm = Nlm Plm exp(i m phi)         conj(Ylm) = (-1)^m Yl-m
 * Nlm = sqrt( (2l+1) (l-m)! / (l+m)! / (4pi) )
 * Plm(x) = (-1)^m/(2^l l!) (1-x^2)^(m/2) * d^(l+m)/dx^(l+m) (x^2-1)^l
 *   (-1)^m is Condon-Shortly phase, included in Mathematica LegendreP
 *     and in QM generally (simplifies raising, lowering operators)
 *   in either case   P[l,-m] = (-1)^m (l-m)!/(l+m)! P[l,m]
 *
 *   recurrence (physics+Mathematica sign convention)
 * P[0,0] = 1
 * P[m,m] = -(2m-1) sqrt(1-x^2) P[m-1,m-1]
 * P[m+1,m] = (2m+1) x P[m,m]
 * P[l,m] = ( (2l-1) x P[l-1,m] - (l+m-1) P[l-2,m] )/(l-m)   for l>m+1
 *   permits calculation for all m>=0
 *
 * Note that the ratio of largest to smallest values of Plm is very
 * large, so probably does not extend the range much to work with
 * the product Nlm*Plm?  Anyway, this algorithm hurls chunks for L above
 * 150 or so.  Probably could restructure it to get slightly larger LMAX,
 * but not worthwhile?
 */
func _ylm_plm(dm)
{
  extern z, r, lmax, a, b, c, n;  /* r = sqrt(1-z^2) */
  if (!dm) {       /* compute P[m,m] */
    n = double(indgen(1:2*lmax+1:2))
    p = array(2.-n, dimsof(z));
    k = 1.;
    c = array(k, 1+lmax);
    for (m=1,q=1.,mm=2. ; m<=lmax ; ++m,mm+=2.) {
      q = p(1+m,..) *= q*r;
      c(1+m) = k *= sqrt(1./(mm*(mm-1.)));  /* c hits zero around m=40... */
    }
    n = sqrt(n * 0.25/pi);
  } else {
    m = double(indgen(0:lmax-dm));
    if (dm == 1) { /* compute P[m+1,m] */
      b = a(1:-1,..);
      p = (m+m+1.)*b*z(-:dm:lmax,..);
      c = c(1:-1) * sqrt(1. / (m+m+1.));
    } else {       /* compute P[m+dm,m] */
      l = double(indgen(dm:lmax));
      q = b(1:-1,..);
      b = a(1:-1,..);
      p = ((l+l-1.)*b*z(-:dm:lmax,..) - (l+m-1.)*q) * (1./dm);
      c = c(1:-1) * sqrt(dm / (l+m));
    }
    n = n(2:0);
  }
  return a = p;
}

func ylm_fit(lmax, v, x, y, z, domega=)
/* DOCUMENT alm = ylm_fit(lmax, v, theta, phi)
 *       or alm = ylm_fit(lmax, v, x, y, z)
 *   returns least squares fit of spherical harmonic coefficients alm
 *   through l=LMAX to the function V specified at points (THETA,PHI)
 *   on the surface of a sphere.  The points on the sphere may also be
 *   specified by three coordinates (X,Y,Z), which are projected onto
 *   the unit sphere.  The V, THETA, PHI (or X, Y, Z) arrays should all
 *   have the same dimensions, although the coordinate arrays THETA
 *   and PHI (or X, Y, Z) will be broadcast to the dimensions of V if
 *   necessary.  The returned ALM is a 1+LMAX by 1+LMAX array of real
 *   numbers for a real spherical harmonic expansion.  The correspondence
 *   between the dimensions of ALM and the values of l and m is detailed
 *   in the help,ylm documentation.
 *
 *   The ylm_fit function returns the ALM which minimize
 *      sum( ((ALM * ylm(lmax, THETA,PHI))(sum,sum,..) - V)^2 )
 *   using the regress function.  See  help,regress for details.
 *
 *   This fit requires that the points (THETA,PHI) be uniformly
 *   distributed over the sphere.  If not, you may supply a domega=
 *   keyword, where domega has the same dimensions as V, and specifies
 *   the solid angle (or effective solid angle) to be associated with
 *   each point V.  The domega= keyword should always be used when
 *   (THETA,PHI) are the centers of a spherical mesh.
 *
 *   Note that numberof(V) must be at least (1+LMAX)^2.
 *
 * SEE ALSO: ylm_eval, ylm, ylm_lm
 */
{
  if (is_void(domega)) domega = 4.*pi/numberof(v);
  domega += 0.*v;
  x = transpose(ylm(lmax, x+0.*v, y, z), -1)(..,1:(1+lmax)^2,1);
  alm = array(0., 1+lmax,1+lmax);
  alm(*) = regress(v, x, sigy=1./sqrt(domega));  /* weights are 1/sigy^2 */
  return alm;
}

func ylm_eval(alm, x, y, z)
/* DOCUMENT v = ylm_eval(alm, theta, phi)
 *       or v = ylm_eval(alm, x, y, z)
 *   evaluates the spherical harmonic expansion ALM at points (THETA,PHI)
 *   or (X,Y,Z), where (X,Y,Z) will be projected onto the unit sphere.
 *   The return value has dimsof(THETA,PHI), or dimsof(X,Y,Z).  The input
 *   ALM is arranged as a 1+lmax by 1+lmax array, as described in detail
 *   in the help,ylm documentation, and as returned by ylm_fit.
 *
 * SEE ALSO: ylm_fit, ylm, ylm_lm
 */
{
  d = dimsof(alm);
  if (d(1)!=2 || d(2)!=d(3)) error, "alm must be 1+lmax by 1+lmax array";
  return (alm * ylm(d(2)-1, x, y, z))(sum,sum,..);
}

func legpol(lmax, z)
/* DOCUMENT legpol(lmax, z)
 *   return Legendre polynomials Pl(z) through order LMAX evaluated at Z,
 *   as a 1+LMAX by dimsof(Z) array.
 *     P[0] = 1
 *     P[1] = z
 *     l P[l] = (2l-1) z P[l-1] - (l-1) P[l-2,m]
 *   integral[-1,1]{ dz P[l](z)^2 } = 2/(2l+1)
 * SEE ALSO: legeval, legint, legfit, legser, ylm
 */
{
  p = array(1., 1+lmax, dimsof(z));
  p0 = p(1,..);
  p(2,..) = p1 = double(z);
  for (l=2 ; l<=lmax ; ++l,p0=p1,p1=q)
    p(1+l,..) = q = ((l+l-1)*z*p1 - (l-1)*p0) / l;
  return p;
}

func legint(lmax, z)
/* DOCUMENT legint(lmax, z)
 *   Return integrals of Legendre polynomials Pl(z) through order LMAX
 *   evaluated at Z, as a 1+LMAX by dimsof(Z) array.
 *     integral[1,z]{ dz P[l](z) } = (P[l+1](z) - P[l-1](z)) / (2l+1)
 *   Note that the lower limit of integration is z=+1.  If you want
 *   the integral from some other z=z0, subtract legint(lmax,z0).
 * SEE ALSO: legpol, legbins, legeval, legfit, legser
 */
{
  p = legpol(lmax+1, z);
  p(3:0,..) -= p(1:-2,..);  /* p(2,) = z already correct */
  return 1./indgen(1:lmax+lmax+1:2) * p(2:0,..);
}

func legser(a, flag)
/* DOCUMENT b = legser(a)
 *       or b = legser(f, lmax)
 *       or b = legser(cfit, lmax)
 *       or matrix = legser(lmax)
 *   return coefficients B of Legendre series, given coefficients A of
 *   Chebyshev series for the same function.
 *   In the second form, F is a function F(x) on -1<=x<=1, and LMAX is the
 *   maximum order of the Legendre series.  This first performs a Chebyshev
 *   fit of order 2*LMAX to F(x), then truncates the Chebyshev fit to LMAX.
 *   In the third form, CFIT is returned by cheby_fit, and will be truncated
 *   to order LMAX.  (The interval for CFIT should be [-1,1] to make sense.)
 *   You may specify LMAX=-1 to use the full order of CFIT, but unless CFIT
 *   has already been truncated, that will risk inaccuracy at high order.
 *   In the fourth form, returns the MATRIX such that B = A(+)*MATRIX(+,).
 *
 *   ACM algorithm 473, Robert Piessens, Comm ACM vol 17 no 1, Jan 1974
 *   Tn(x) = Chebyshev polynomials, Pl(x) = Legendre polynomials
 *   f(x) = sum[n=0,N]{An Tn(x)}
 *   Bl = (2l+1)/2 integral[-1,1]{dx Pl(x) f(x)}
 *      ~ (2l+1)/2 sum[k=0,N]{Ak Ilk}
 *   where Ilk = integral[-1,1]{dx Pl(x) Tk(x)}
 *     I[l,k] = 0 for k<l
 *     I[l,l] = 2^(2l)(l!)^2/(2l+1)!
 *     I[l,l+1] = 0
 *     I[l,k+2] = recurrence to I[l,k]
 *   The Bl are exact if and only if f(x) is a polynomial of degree <= N
 *
 * SEE ALSO: legeval, legfit, legpol, cheby_fit
 */
{
  if (flag) {  /* a returned by cheby_fit, truncated from higher order */
    if (is_func(a)) a = cheby_fit(a, [-1.,1.], 2*flag)(1:3+flag);
    else if (flag>0) a = a(1:3+flag);
    flag = a(1:2);
    a = a(3:0);
  }
  if (dimsof(a)(1)==0 && structof(a+0)==long) {
    n = a+1;
    if (n < 2) return [[0.5]];
    a = flag = [];
  } else {
    n = numberof(a);   /* = lmax+1 */
    if (n < 2) return 0.5*a;
  }

  d = array(0., n, n);
  /* begin with recurrence for diagonal
   * d(0,0) = 1
   * d(l,l) = 4*l^2/((2l+1)(2l)) * d(l-1,l-1)
   *   -4*l^2/((2l+1)(2l)) * d(l-1,l-1) + d(l,l) = 0
   */
  c = double(indgen(1:n-1));       /* l for l>0 */
  c = -4.*c*c / ((c+c+1.)*(c+c));  /* subdiagonal of recursion */
  b = array(0.,n);  b(1) = 1.;     /* rhs of recursion */
  d(1:n*n:n+1) = TDsolve(c,array(1.,n),array(0.,n-1), b);

  /* l=0 is special case */
  l = double(indgen(2:n-1:2));
  d(3:n:2,1) = -2./(l*l - 1.);

  /* l>1 computed by recursion on k>=l starting from d(l,l)
   * d(k+2,l) = ((k-1)k - (l+1)l) / ((k+3)(k+2) - (l+1)l) (k+2)/k d(k,l)
   */
  l = double(indgen(0:n-1));
  l *= l + 1.;
  for (k=k0=1 ; k+2<n ; k+=1,k0=3-k0) {
    lk = l(k0+1:k+1:2);
    d(k+3,k0+1:k+1:2) = (k+2.)/k * (l(k)-lk)/(l(k+3)-lk) * d(k+1,k0+1:k+1:2);
  }
  d *= (double(indgen(0:n-1)) + 0.5)(-,);

  return numberof(a)? a(+)*d(+,) : d;
}

func legfit(y, x, lmax, dx=)
/* DOCUMENT legfit(y, x, lmax)
 *   return coefficients of Legendre polynomials Pl(z) through order LMAX
 *   which give least squares best fit of truncated Legendre series to
 *   the given points (X,Y).  The points X should be uniformly distributed
 *   in the interval [-1,1].  If not, you should specify the dx= keyword
 *   giving the dx associated with each point Y (dx must be conformable
 *   with Y).
 * SEE ALSO: legeval, legbins, legser, legpol
 */
{
  if (is_void(dx)) dx = 1./numberof(y);
  dx += 0.*y;
  x = transpose(legpol(lmax, x), 0);
  return regress(y, x, sigy=1./sqrt(dx));  /* weights are 1/sigy^2 */
}

func legbins(y, xb, lmax, dy=)
/* DOCUMENT legbins(y, xb, lmax)
 *   Return coefficients of Legendre polynomials Pl(z) through order LMAX
 *   which give least squares best fit of truncated Legendre series to
 *   the given values y integrated over bins xb.  numberof(y) must equal
 *   numberof(xb)-1.  The returned coefficients are of the density
 *   function represented by y; that is, the value of the Legendre series
 *   is y/xb(dif).
 *   Use the keyword dy= to specify standard deviations of y.  If not
 *   specified, all y will be equally weighted in the fit.
 * SEE ALSO: legeval, legfit, legser, legpol, legint
 */
{
  z = legint(lmax, xb)(,dif);  /* integral of P[l](x) over bin */
  return regress(y, transpose(z), sigy=dy);
}

func legeval(al, x)
/* DOCUMENT legeval(al, x)
 *   evaluate the Legendre series with coefficients AL = [a0,a1,a2,...]
 *   at points X.
 * SEE ALSO: legfit, legser, legpol, ylm
 */
{
  return al(+) * legpol(numberof(al)-1, x)(+,..);
}

#if 0
/* verified with a = (2.*(1.-sqrt(0.75)))^indgen(0:20)/sqrt(0.75)
 * which is Chebyshev expansion of 1./(2.-x)
 *
 * B0 = (2*0+1)/2 * sum[k=0;k<=N;k+=2]{ Dk0 * Ak }
 *   D00 = 1
 *   Dk0 = -2/(k^2-1)
 * Bl = (2l+1)/2 * sum[k=l;k<=N;k+=2]{ Dkl * Ak }
 *   Dll = Cl
 *   C[1] = 2/3
 *   C[l] = 4*l^2/((2l+1)(2l)) * C[l-1]
 *     Dll = (2^l l!)^2 / (2l+1)!      note: this works for D00 = 1 as well
 *   D[k+2,l] = ((k-1)k - (l+1)l) / ((k+3)(k+2) - (l+1)l) (k+2)/k D[k,l]
 *   D[k+2,0] = -2/(k^2-1) D[k,0]
 *   D[0,0] = 1
 */
func acm473(a)
{ /* more-or-less direct translation of original fortran */
  b = 0.*a;
  n1 = numberof(a);
  b(1) = 0.5*a(1);
  for (k=3,ak=0. ; k<=n1 ; k+=2) {
    ak += 2.;
    b(1) -= a(k)/(ak*ak-1.);
  }
  c = 2./3.;
  for (l=2,al=0. ; l<=n1 ; ++l) {
    ll = l + 2;
    al += 1.;
    bb = c*a(l);
    if (ll <= n1) {
      d = c;
      l2 = al*(al+1.);
      for (k=ll,ak=al ; k<=n1 ; k+=2,ak+=2.) {
        d *= ((ak-1.)*ak - l2)*(ak+2.) / (((ak+3.)*(ak+2.) - l2)*ak);
        bb += a(k)*d;
      }
    }
    c *= 4.*(al+1.)^2/((al+al+3.)*(al+al+2.));
    b(l) = (al+0.5)*bb;
  }
  return b;
}
#endif

/* If X[l,alpha] = rotation about z by alpha, a (2l+1)x(2l+1) non-zero for
 *   X[l,alpha](i,i) = cos((i-1-l)alpha)
 *   X[l,alpha](2l+2-i,i) = sin((i-1-l)alpha)  (i not equal l+1)
 * then rotation by Euler angles (alpha,beta,gamma) is composition of
 * five operations:
 *   Euler[l,alpha,beta,gamma] = X[l,alpha] J[l] X[l,beta] J[l] X[l,gamma]
 * where:
 * J[l] is (2l+1)x(2l+1) matrix representing exchange of y and z
 *   on real spherical harmonic coefficients with quantum number l
 *   note J[0] = 1
 *   only ((l+1)^2+1)/2 non-zero values in J[l] matrix
 *
 * compute J[l] by recursion on l using following relations:
 * Gy[l] and Gz[l] are (2l+3)x(2l+1) matrices
 * J[l+1] Gz[l] = Gy[l] J[l]
 *
 * J[l](i,j) = J[l](j,i)   that is, J[l] is symmetric
 * corner values are (l>=1):
 *   J[l](1,1) = J[l](1,2l+1) = J[l](2l+1,1) = 0
 *   J[l](2l+1,2l+1) = 2^(1-l)
 *
 * Gz[l](k+1, k) = sqrt( k(2l+2-k) / ((2l+1)(2l+3)) )
 *   for 1 <= k <= 2l+1, are only non-0 elements of Gz[l] (for l>=1)
 *
 * Gy[l](2l+2-k, k) = -Gy[l](2+k, 2l+2-k) =
 *   0.5*sqrt( k(k+1) / ((2l+1)(2l+3) )   for 1 <= k <= l-1
   Gy[l](l+2,l) = 0.5*sqrt( 2l(l+1) / ((2l+1)(2l+3)) )
 * Gy[l](k, 2l+2-k) = -Gy[l](2l+4-k, k) =
 *   -0.5*sqrt( (2l+2-k)(2l+3-k) / ((2l+1)(2l+3) )   for 1 <= k <= l
   Gy[l](l+1,l+1) = -0.5*sqrt( 2(l+1)(l+2) / ((2l+1)(2l+3)) )
 * are only non-0 elements of Gy[l] (for l>=1)
 * Gy[l](i,j) != 0 if i+j=2l+2  or  i+j=2l+4
 *   i+j=2l+2, i=1,..,l,l+3,..,2l+1 while j=2l+1,..,l+2,l-1,..,1
 *     s*0.5*sqrt( j(j+1) / ((2l+1)(2l+3) ) where s=-1 for i<l+1, +1 for i>i+2
 *     -sqrt( 2j(j+1) / ((2l+1)(2l+3) ) for i=l+1, j=l+1
 *     sqrt( 2j(j+1) / ((2l+1)(2l+3) ) for i=l+2, j=l
 *   i+j=2l+4, i=3,..l+1,l+4,..,2l+3 while j=2l+1,..,l+3,l,..,1
 *     s*sqrt( (i-1)(i-2) / ((2l+1)(2l+3) ) where s=-1 for i<l+2, +1 for i>l+3
 *
 * J[l+1] Gz[l] (j,k) = sum[q]{ J[l+1](j,q) * Gz[l](q,k) }
 *                    = J[l+1](j,k+1) Gz[l](k+1, k)
 *
 * Gy[l] J[l] (j,k) = sum[q]{ Gy[l](j,q) J[l](q,k) }  q=1,2q+1
 *   non-zero terms: (q=2l+2-j) and (q=2l+4-j except q=l+1 and q=l+2)
 *
 */
