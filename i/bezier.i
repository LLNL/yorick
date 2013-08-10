/* bezier.i
 * compute points on Bezier curve
 */
/* Copyright (c) 2013, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.github.com).
 * Read the accompanying LICENSE file for details.
 */

func bezier(p, s, n, w=)
/* DOCUMENT bezier(p, s)
 *       or bezier(p, s, n)
 *
 *  Return points on a Bezier curve defined by the array of control
 *  points P, at parameter values S.  The Bezier curve is of order N,
 *  which defaults to 3.  P is a d-by-(m*N+1) dimensional array
 *  of control points in d-dimensions.  (That is, d=2 is a planar curve,
 *  d=3 is a curve in 3D space, and so on.)  The points 1, 1+N, 1+2*N,
 *  1+3*N, etc. are on the curve, while the N-1 intermediate points
 *  are Bezier control points.  The array S may have any dimensionality;
 *  the return value will have dimensions d-by-dimsof(S).  The values
 *  of S must lie between 0 and m, where S=0 represents P(,1), S=1
 *  represents P(,2), S=2 represents P(,3), and so on, and intermediate
 *  values of S are on the Bezier curve connecting those integer points
 *  defined by the intermediate control points.
 *
 *  As a shorthand, if S is an integer scalar greater than 1, bezier
 *  gives you S equally spaced points from 0 to m, S = span(0,m,S).
 *
 *  If the endpoints of one interval are P[0] and P[N], and the control
 *  points are P[1], P[2], ..., P[N-1], then the Bezier curve of order N
 *  is defined by:
 *     P(s) = sum[k=0,N]{ P[k] * (N k)*(1-s)^(N-k)*s^k }
 *  where (N k) = N!/((N-1)!*k!) is the binomial coefficient, where s
 *  goes from 0 to 1 takes P(s) from P[0] to P[N] along the Bezier curve
 *  of order N.
 *
 *  The default case n=3 for d=1 produces a piecewise cubic curve, which
 *  you can also produce using the spline or splinef function.
 *
 *  The w= keyword can be used to supply weights W to produce a rational
 *  Bezier curve with control points P and weights W:
 *    q = bezier(P, s, w=W);
 *  is equivalent to:
 *    pw = W(-:1:dimsof(P)(2)+1,);
 *    pw(1:-1,..) *= P;
 *    q = bezier(pw, s);
 *    q = q(1:-1,..) / q(0:0,..);
 *  Rational Bezier curves of order 2 exactly represent ellipses,
 *  parabolas, or hyperbolas, when the weights w are 1 for points on the
 *  curve and w<1, w=1, or w>1, respectively, for intermediate points.
 *  For example, bezier([[0,1],[1,1],[1,0]],w=[1,sqrt(.5),1], 100, 2) is
 *  100 points along the first quadrant of the unit circle.
 *
 * SEE ALSO: span, spanarc, spline, splinef, interp
 */
{
  if (is_void(n)) n = 3;
  d = dimsof(p);
  m = (numberof(d) && d(1)==2)? (d(3)-1)/n : -1;
  if (m<=0 || d(3)!=m*n+1)
    error, "wrong number of control points, need npts%order = 1";
  if (!is_void(w)) {
    if (numberof(w)!=d(3) || dimsof(w)(1)!=1)
      error, "wrong number of point weights w= for rational bezier";
    w = w(-:1:d(2)+1,..);
    w(1:-1,) *= p;
    p = w;
    w = 1;
  }
  if (numberof(s)==1 && structof(s+0)==long) s = span(0, m, s);
  i = min(long(max(s,0.)), m-1);
  s = (s-i)(-,-,..);
  /* iterated linear interpolation, modified De Casteljau's algorithm */
  p = p(, indgen(n+1) + n*i(-,..));
  for (k=0 ; k<n ; ++k) p = p(,1:-1,..) + p(,dif,..) * s;
  p = p(,1,..);
  if (w) p = p(1:-1,..) / p(0:0,..);
  return p;
}
