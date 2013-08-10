/* spanarc.i
 * Compute equally spaced points along a circular arc.
 */
/* Copyright (c) 2013, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func spanarc(a, b, c, n, cont=)
/* DOCUMENT spanarc(a, b, c, n)
 *   return N points equally spaced along circular arc ABC.  Each of A,
 *   B, and C is a point in M>=2 dimensional space.  That is, A = [x,y]
 *   or [x,y,z], etc.  A, B, and C may also have trailing dimensions in
 *   order to generate points along several arcs simultaneously.  The
 *   result has dimensions dimsof(A,B,C)-by-N.
 *   With non-nil, non-zero cont= keyword, the points will fall along the
 *   circle from C to A continuing the arc ABC and completing the circle.
 * SEE ALSO: span, spanl, bezier
 */
{
  if (cont) {  /* invert b through midpoint of ac */
    m = 0.5*(a + c);
    bm = m - b;
    am = m - a;
    b = ((am*am)(-,sum,..) / (bm*bm)(-,sum,..)) * bm + m;
    m = a;  a = c;  c = m;  /* swap a and c */
  }
  abc = [a, b, c] - [b, c, a];
  s = (abc*abc)(-,sum,..);
  /* get p = vector perpendicular to ac toward b
   * and q = exterior angle from ab to bc
   */
  ac = abc(..,3);
  p = abc(..,2);
  q = (abc(..,1)*p)(-,sum,..);
  q = acos(q / sqrt(s(..,1)*s(..,2)));
  s = s(..,3);
  p -= (p*ac)(-,sum,..)/s * ac;
  /* arc starts in direction q at a, finishes in direction -q at b */
  q -= (((q+q)/(n-1.))(..,-:1:n-1))(..,cum)(..,zcen);  /* step angles */
  cs = [cos(q), sin(q)](..,cum,);
  s = sqrt(s);
  cs *= s / cs(..,0,1);
  ac *= 1./s;
  p *= 1./sqrt((p*p)(-,sum,..));
  abc = a + ac*cs(..,1) + p*cs(..,2);
  abc(..,0) = c;  /* abc(..,1) = a already guaranteed */
  return abc;
}
