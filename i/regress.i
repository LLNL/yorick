/*
  * $Id: regress.i,v 1.1 2005-09-18 22:06:18 dhmunro Exp $
  * linear regression and related statistical functions
  * after Numerical Recipes (Press et. al.) chapter 14
  */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func regress(y, x, &axes, &vals, &chi2, sigy=, rcond=)
/* DOCUMENT model = regress(y, [x1,x2,x3,...])
  *       or model = regress(y, [x1,x2,x3,...], axes, vals, chi2)
  *
  *   Performs linear regression analysis, that is, a linear least squares
  *   fit of the parametric model
  *      y = model(1)*x1 + model(2)*x2 + model(3)*x3 + ...
  *        = model(+) * [x1,x2,x3,...](..,+)
  *   to the supplied data Y, [X1,X2,X3,...].  Y is an array, and the
  *   Xi arrays should have the same dimensions as Y, or more accurately
  *   [X1,X2,X3,...] must have one trailing dimension beyond the dimensions
  *   of Y; the length of the trailing dimension is the number of parameters
  *   in the model.
  *
  *   Use the sigy= keyword to pass in the standard deviations of the Y
  *   values; sigy must be conformable with Y.  The returned model has
  *   minimum chi2, where
  *     chi2 = sum( (y - model(+) * [x1,x2,x3,...](..,+))^2 / sigy^2 )
  *   The default sigy=1, that is, all points have equal weight in chi2.
  *
  *   The optional AXES, VALS, and CHI2 arguments are outputs.  AXES are
  *   the axes of the error ellipsoid of the fitted parameters, and VALS
  *   are the corresponding singular values.  AXES(i,) is the vector in
  *   the space of model() corresponding to VALS(i).  The VALS are
  *   non-negative and arranged in descending order.  CHI2 is the
  *   chi2 value for the returned model, divided by the number of
  *   degrees of freedom in the fit, numberof(y)-sum(vals>rcond*vals(1)).
  *   AXES(i,) are mutually orthogonal unit vectors; you should arrange
  *   the magnitudes of X and Y so this makes sense - their scale factors
  *   may matter.  Usually this means arranging that the magnitudes of
  *   the elements of the returned model not differ too wildly.
  *
  *   Often and unexpectedly, the data do not permit a definitive choice
  *   of the model(); there may be entire subspaces of the model space
  *   which produce indistinguishably good fits to the data.  The signature
  *   of this problem is that some of the VALS may be zero or very small.
  *   The rcond= keyword permits you to specify how small a singular
  *   value, relative to the largest, VALS(1), you are willing to consider.
  *   The AXES(i,) corresponding to VALS(i) smaller than rcond*VALS(1)
  *   will be given zero contribution to the returned model().  The default
  *   value is rcond=1.e-9.  If any of the returned VALS is less than
  *   rcond*VALS(1) you should either change the scales of Y or some X,
  *   or remove some of the X (the ones which aren't contributing) to
  *   eliminate the problem.  The regress function reverses the sign of
  *   any VALS which are less than rcond*VALS(1), so you can quickly
  *   identify them.
  *
  *   Examples:
  *     ab = regress(y, [1,x]);
  *       ab(1)+ab(2)*x   is best fit line to y(x)
  *     ab = regress(y, [1,x,x^2,x^3]);
  *       poly(x,ab(1),ab(2),ab(3))   is best fit cubic to y(x)
  *     ab = regress(y, [cos(x),sin(x)]);
  *       ab(1)*cos(x)+ab(2)*sin(x)   is best fit period 2*pi sine wave to y(x)
  *
  * SEE ALSO: regress_cov
  */
{
   y = double(y(*));
   x = double(x(*,));
   dims = dimsof(x);
   n = numberof(y);
   if (dims(1)!=2 || dims(2)!=n) error, "bad x dimensions";
   if (is_void(sigy)) sigy = 1. + 0.*y;
   else sigy = 1./sigy(*);
   if (is_void(rcond)) rcond = 1.e-9;
   else rcond = min(max(rcond,1.e-13),0.5);
   x *= sigy;
   y *= sigy;
   local u;
   vals = SVdec(x, u, axes);
   u = y(+) * u(+,);
   mask = vals > rcond*vals(1);
   np = sum(mask);
   rvals = mask / (vals + !mask);
   if (np < numberof(mask)) vals(where(!mask)) *= -1.;
   u *= rvals;
   model = u(+) * axes(+,);
   chi2 = model(+)*x(,+) - y;
   chi2 = sum(chi2*chi2) / max(n - np, 1);
   return model;
}

func regress_cov(axes, vals)
/* DOCUMENT cov = regress_cov(axes, vals)
  *       or cov = chi2 * regress_cov(axes, vals)
  *
  *   Return the covariance matrix for the model returned by regress,
  *   where AXES and VALS are the values returned by regress.  This
  *   is a symmetric matrix representing
  *     covariance(model(i),model(j))
  *   that is, the variances (on the diagonal i=j) and correlations
  *   of the model parameters.
  *
  *   If you did not specify the sigy= keyword in the original call to
  *   regress, then the absolute magnitudes of the covariance matrix
  *   elements do not mean much.  In case you wish to use the quality
  *   of the fit itself as an estimate of the errors sigy in the
  *   original Y values, assuming they are all equal, you can multiply
  *   by the CHI2 returned by regress, as in the second form above,
  *   in order to produce an estimated error in the fit.  See Numerical
  *   Recipes for caveats, but this is what commercial statistics
  *   software generally does.
  *
  *   cov(1::numberof(vals)+1) are the variances of the model parameters
  *   themselves.
  *
  * SEE ALSO: regress
  */
{
   mask = vals > 0.;
   axes *= mask / (vals + !mask);
   return axes(+,) * axes(+,);
}
