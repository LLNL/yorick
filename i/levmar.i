/* levmar.i
 * Non-linear least squares fitting by Levenberg-Marquardt algorithm
 */
/* Copyright (c) 2009, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func levmar(y, x, f, a0, &avar, &acovar, wgt=, fit=, amin=, amax=, lu=)
/* DOCUMENT a = levmar(y, x, f, a0, avar, acovar)
 *   perform a Levenberg-Marquardt non-linear least squares fit
 *   to data values Y, which are functions of X.  The dimensions
 *   of X need bear no particular relationship to the dimensions
 *   of Y, but Y must be a 1D array.  The function F maps X into Y,
 *   as a function of some additional parameters A, according to
 *     Y = F(X, A)
 *   Again, the dimensions of A bear no particular relation to
 *   the dimensions of either X or Y.  The input A0 is the initial
 *   estimate of the parameter values, which must be made with some
 *   care for the algorithm to converge.  Note that it may not converge
 *   to the expected relative minimum in many situations.
 *
 *   The parameters Y and A must be 1D arrays, X can be anything.
 *
 *   AVAR and ACOVAR are return arguments; ACOVAR is the covariance
 *   matrix of the fit parameters A, and AVAR is its diagonal.  Hence
 *   AVAR has the same dimensions as A0, while ACOVAR is a 2D symmetric
 *   matrix, whose diagonal is AVAR.  Multiply AVAR or ACOVAR by
 *   levmar_chi2 if you did not use wgt= and want to use the quality
 *   of the fit to estimate the variances of the parameters.
 *   ACOVAR is what is returned by regress_cov, see help,regress_cov.
 *
 *   You may be able to improve the performance of levmar by supplying
 *   analytic derivatives.  To support this improvement, levmar permits
 *   two different "prototypes" for the function F:
 *     func F(x, a)
 *       if you cannot compute analytic dfda
 *       approximate dfda will be computed by levmar_partial (see help)
 *     func F(x, a, &dfda)
 *       if you can return analytic dfda
 *       if levmar will use dfda, dfda=1 on input
 *       if levmar will not use dfda, dfda=[] on input
 *     An obsolete third prototype is for backward compatibility with
 *     the old lmfit function.  Do not use it in new code:
 *     func F(x, a, &dfda, deriv=)
 *       if levmar will use dfda, it sets deriv=1, else deriv=0
 *       (this form is for backward compatibility with original lmfit)
 *   When F is an interpreted function, levmar automatically detects
 *   which of these prototypes F matches.  Otherwise (if F is a closure
 *   or a built-in function, for example), levmar assumes the second
 *   prototype, F(x, a, &dfda).  Use the levmar0 function if you want
 *   to skip automatic prototype detection and assumes the F(x,a) form.
 *   Similarly, levmar1 skips autodetection and assumes F(x,a,&dfda).
 *   In all cases, dfda is defined as:
 *     dfda(i,j) = partial[Y(i)] / partial[A(j)]
 *   Note that the external variable fit (the fit= keyword to levmar)
 *   is available to F, indicating that only a subset of the partial
 *   derivatives must be computed.  Set unused j indices to zero.
 *
 *  EXTERNAL VARIABLES:
 *      outputs:
 *    levmar_chi2             final value of chi2
 *    levmar_chi20            initial value of chi2 (for a0)
 *    levmar_lambda           final value of lambda
 *    levmar_neval            number of calls to F
 *      inputs:
 *    levmar_itmax = 100      maximum number of gradient recalculations
 *    levmar_tol = 1.e-7      stop when chi2 changes by less than this
 *    levmar_lambda0 = 0.001  initial value of lambda
 *    levmar_lambda1 = 1.e12  maximum permitted value of lambda
 *    levmar_gain = 10.       factor by which to change lambda
 *    levmar_aabs, levmar_arel, levmar_ada -- see levmar_partial
 *
 *  KEYWORDS:
 *    fit=   index list into a0 of parameters to be varied
 *      the returned model will equal a0 for parameters not varied,
 *      avar and acovar will be 0.0 for parameters not varied
 *    amin=  minimum values for parameters, same size as a0
 *    amax=  maximum parameter values, same size as a0
 *      the function F will not be called with parameters outside
 *      these specified ranges
 *    wgt=   same size as Y, weightings for each point
 *      if sigma_y(i) is standard deviation of i-th point, then
 *      wgt=1./sigma_y^2 is the appropriate weight.
 *    lu=    set non-zero to use LUsolve instead of SVsolve
 *      LUsolve will be faster, which will only be an issue if the
 *      number of parameters is large
 *
 *  SEE ALSO: regress, levmar_partial, levmar0, levmar1
 */
{
  /* automatically detect input function prototype */
  if (is_func(f)==1 && is_void(_levmar01_flags)) {
    args = strtok(print(f)(1),"(");  delim = " ,)";
    keyd = strmatch(args(2), "deriv=");
    for (i=1;i<=3;++i) args = strtok(args(2),delim);
    hasd = strpart(args(1),1:1) == "&";
    if (keyd) {
      if (!hasd) error, "deriv= prototype needs third argument to be output";
      args = strtok(args(2),delim);
    }
    if (args(2)) error, "unrecognized fitting function prototype";
  } else {
    keyd = 0;
    hasd = is_void(_levmar01_flags)? 1 : (_levmar01_flags & 1);
    /* make _levmar01_flags use deprecated prototype */
    if (hasd && _levmar01_flags && (_levmar01_flags & 2)) keyd = 1;
  }
  if (!hasd) ff = _levmar_f;
  else if (keyd) ff = _levmar_g;
  else ff = f;
  _from_levmar = 1;

  a0 = double(a0);
  aa = a0;
  a = a0(fit);  /* possible subset of a0 */
  nfit = numberof(a);
  if (!is_void(amin)) { an0 = amin += 0.*a0;  amin = amin(fit); }
  if (!is_void(amax)) { ax0 = amax += 0.*a0;  amax = amax(fit); }

  if (is_void(wgt)) wgt = 1.0;
  else if (anyof(wgt < 0.0)) error, "bad weights";
  wgt += 0.0*y;
  nfree = sum(wgt > 0.0) - nfit;
  if (nfree <= 0) error, "not enough data points";

  extern levmar_neval, levmar_lambda, levmar_chi20, levmar_chi2;
  levmar_neval = 0;
  conv = 0.0;

  amult = array(1.0, nfit, nfit);
  diag = indgen(1:nfit*nfit:1+nfit);
  levmar_lambda = levmar_lambda0;
  solver = lu? LUsolve : SVsolve;
  for (niter=0 ; niter<levmar_itmax ; ++niter) {
    dfda = 1;
    dy = y - ff(x, aa, dfda);
    levmar_neval++;
    dfda = dfda(,fit);

    beta = wgt * dy;
    if (!niter) levmar_chi20 = levmar_chi2 = sum(beta * dy);
    beta = dfda(+,) * beta(+);
    alpha = (wgt * dfda)(+,) * dfda(+,);

    /* lambda >> 1 is steepest descents,
     *   moving down gradient by an amount that goes like 1/lambda
     * lambda << 1 is linearized least squares solution
     */
    if (!levmar_chi2) break;
    chi2prev = levmar_chi2;
    for (a1=a ;;) {  /* try to step away from a1 = initial a */
      amult(diag) = 1.0 + levmar_lambda;
      a = a1 + solver(alpha*amult, beta);
      if (!is_void(amin)) a = max(a, amin);
      if (!is_void(amax)) a = min(a, amax);
      aa(fit) = a;
      dy = y - ff(x, aa);
      levmar_neval++;
      levmar_chi2 = sum(wgt*dy*dy);
      if (levmar_chi2<1.0000000001*chi2prev || allof(a1==a)) break;
      /* attempt to step with this lambda made things worse,
       * increase lambda to take more conservative steepest descents step
       */
      levmar_lambda *= levmar_gain;
      if (levmar_lambda > levmar_lambdax)
        error, "lambda exceeds levmar_lambdax";
    }
    conv = 2.0*(chi2prev-levmar_chi2)/(chi2prev+levmar_chi2);
    if (conv <= levmar_tol) break;
    levmar_lambda /= levmar_gain;
  }
  if (niter >= levmar_itmax)
    write, "WARNING: levmar hit iteration limit "+print(niter)(1);

  levmar_chi20 /= nfree;
  levmar_chi2 /= nfree;
  avar = 0.0*a0;
  acovar = avar(-,) + avar;
  if (levmar_chi2) {
    acovar(fit,fit) = lu? LUsolve(alpha) : SVsolve(alpha,unit(numberof(diag)));
    avar(fit) = acovar(fit,fit)(diag);
  }

  return aa;
}
levmar_lambda0 = 0.001; /* initial value of lambda */
levmar_lambdax = 1.e12; /* maximum permitted value of lambda */
levmar_gain = 10.;      /* factor by which to change lambda */
levmar_itmax = 100;     /* maximum number of gradient recalculations */
levmar_tol = 1.e-7;     /* stop when chi2 changes by less than this */

func levmar0(y, x, f, a0, &avar, &acovar, wgt=, fit=, amin=, amax=, lu=)
/* DOCUMENT a = levmar0(y, x, f, a0, avar, acovar)
 *   Same as levmar, except assumes F(x,a) prototype.
 *   See help for levmar_partial for important information about how the
 *   unsupplied partial derivatives dfda will be estimated.
 * SEE ALSO: levmar, levmar_partial
 */
{
  _levmar01_flags = 0;
  return levmar(y, x, f, a0, avar, acovar, wgt=wgt, fit=fit,
                amin=amin, amax=amax, lu=lu);
}

func levmar1(y, x, f, a0, &avar, &acovar, wgt=, fit=, amin=, amax=, lu=)
/* DOCUMENT a = levmar1(y, x, f, a0, avar, acovar)
 *   Same as levmar, except assumes F(x,a,&dfda) prototype.
 * SEE ALSO: levmar, levmar_partial
 */
{
  _levmar01_flags = 1;
  return levmar(y, x, f, a0, avar, acovar, wgt=wgt, fit=fit,
                amin=amin, amax=amax, lu=lu);
}

func _levmar_g(x, a, &dfda) { return f(x, a, dfda, deriv=!is_void(dfda)); }
func _levmar_f(x, a, &dfda) {
  return levmar_partial(f, x, a, dfda, fit=fit, amin=an0, amax=ax0);
}

func levmar_partial(f, x, a, &dfda, fit=, amin=, amax=)
/* DOCUMENT y = levmar_partial(f, x, a, dfda)
 *   return y=F(X,A) and DFDA(i,j) = partial[F(X,A)(i)] / partial[A(j)]
 *   by finite differences.  Accepts fit=, amin=, amax= keywords with
 *   same meaning as levmar.  DFDA is only computed if DFDA=1 on input;
 *   if DFDA=[] or 0, the (expensive) DFDA calculation is skipped.
 *
 *   Uses levmar_arel and levmar_aabs to compute the step sizes
 *   use to compute the patial derivatives as follows:
 *     da = levmar_aabs + levmar_arel*abs(A)
 *   (Hence levmar_aabs, levmar_arel can be arrays with the same length
 *   as the parameter array A.)  By default, levmar_arel = 1.e-6, and
 *   levmar_aabs = 0.  If levmar_aabs = 0 and A(i)=0, then da(i)=1.e-9.
 *   You can also supply a function, levmar_ada(A) returning da>0 by
 *   whatever formula you wish.
 * SEE ALSO: levmar
 */
{
  y = f(x, a);
  if (dfda) {
    dfda = array(0., numberof(y), numberof(a));
    da = levmar_ada? levmar_ada(a) : (levmar_aabs + levmar_arel*abs(a));
    list = where(!da);
    if (numberof(list)) da(list) = 1.e-9;
    if (!is_void(amin)) {
      list = where(a+da < amin);
      if (numberof(list)) da(list) = -da(list);
    }
    if (!is_void(amax)) {
      list = where(a+da > amax);
      if (numberof(list)) da(list) = -da(list);
    }
    list = array(0, numberof(a));
    list(fit) = 1;
    list = where(list);
    for (k=1 ; k<=numberof(list) ; ++k) {
      j = list(k);
      aa = a;
      aa(j) += da(j);
      dfda(,j) = (f(x, aa) - y)/da(j);
      if (_from_levmar) levmar_neval++;
    }
  }
  return y;
}
levmar_aabs = 0.0;
levmar_arel = 1.e-6;

#if 0
func levmar_test1(x, a, &dfda)
{
  /* linear model used by regress, should give same result */
  y = x(,+) * a(+);
  if (dfda) dfda = x;
  return y;
}
func levmar_test2(x, a)
{
  /* linear model used by regress, should give same result */
  return x(,+) * a(+);
}
#endif
