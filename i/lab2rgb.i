/* lab2rgb.i
 * conversions among RGB, CIEXYZ, CIELAB, CIELUV color spaces
 *
 * See Wikipedia articles
 *   http://en.wikipedia.org/wiki/International_Commission_on_Illumination
 *   http://en.wikipedia.org/wiki/SRGB
 * and the CIEXYZ, CIELAB, and CIELUV links therein.
 */
/* Copyright (c) 2013, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* sRGB IEC 61966-2-1 is RGB/255 on canonical monitor
 * lRGB is physically linear (red, green, blue)
 *   lRGB = ((sRGB + 0.055)/1.055)^2.4  for sRGB > sRGB0
 *          sRGB * lRGB0/sRGB0          for sRGB <= sRGB0
 * where sRGB0 = 5./7.*0.055
 *       lRGB0 = (12/7.*0.055/1.055)^2.4
 *       (solves y = x * dy/dx, so the line is a tangent to the power law)
 *   sRGB = 1.055*lRGB^(5./12.) - 0.055 for lRGB > lRGB0
 *          lRGB * sRGB0/lRGB0          for lRGB <= lRGB0
 *
 * CIEXYZ is a linear transform of lRGB
 *
 * CIELAB perceptual color space based on CIEXYZ color space:
 *   L = 116 * f(y/yn) - 16
 *   a = 500 * (f(x/xn) - f(y/yn))
 *   b = 200 * (f(y/yn) - f(z/zn))
 * where:
 *   f(t) = t^(1/3)               if  t > (6/29)^3
 *        = 1/3(29/6)^2 t + 4/29  otherwise
 *   t(f) = f^3                   if f > 6/29
 *        = 3(6/29)^2 (f - 4/29)  otherwise
 * and
 *   [xn,yn,zn] is an XYZ reference white color
 *   here D65 white [0.3127, 0.3290, 0.3583], scaled to Y=1
 * 0<=L<=100, abs(a,b) up to of order 100
 *
 * CIELUV color space, typically 0<=L<=100, abs(u), abs(v) <= 100
 * L = (29/3)^3 y/yn          if y/yn <= (6/29)^3
 *   = 116 (y/yn)^(1/3) - 16  otherwise
 * u = 13 (u' - un') L
 * v = 13 (v' - vn') L
 *   u' = 4x/(x+15y+3z)
 *   v' = 9y/(x+15y+3z)
 *   and (un',vn') corespond to the reference white [xn,yn,zn)
 * u' = u/(13L) + un'
 * v' = v/(13L) + vn'
 * y/yn = (3/29)^3 L          if L <= 8
 *      = ((L + 16)/116)^3    otherwise
 * x = y (9u')/(4v')
 * z = y (12 - 3u' -20v')/(4v')
 */

func rgb_l2s(rgb, cmax=)
/* DOCUMENT srgb = rgb_l2s(rgb_linear)
 *  Returns sRGB, the IEC 61966-2-1 is RGB on a canonical monitor, given
 *  the physically linear RGB_LINEAR = [rlin, glin, blin], which is
 *  always normalized to lie in [0.,1.].  By default, rgb_l2s returns
 *  a char [r,g,b] (of the same dimensions as RGB_LINEAR, with the
 *  color index last), normalized to [0,255], but you can specify a
 *  different maximum color value using the cmax= keyword.  In particular,
 *  cmax=1 results in a double array normalized to lie in [0.,1.] like
 *  the input RGB_LINEAR.
 *
 *  The input RGB_LINEAR is clipped to the interval [0,1].  By default,
 *  the clipping just chops any component value above 1 to 1 and below
 *  0 to 0.  By setting the external variable l2s_clipper to a function
 *  you can use a more sophistocated clipping algorithm.  In particular,
 *  l2s_clipper = lrgb_clip gives you a constant hue clipper.  In any
 *  event, rgb_l2s sets the external variable srgb_clip to be an array
 *  with the same dimensions as LINEAR_RGB which is 1 for any clipped
 *  color component, and 0 for unclipped components.
 *
 *  You can also set the external variable lrgb_gamma to use a pure
 *  power law gamma for the conversion instead of the sRGB IEC 61966-2-1
 *  canonical monitor function.  Finally, if you set the 1 bit of the
 *  external variable lrgb_skip, rgb_l2s becomes a no-op.  If you set
 *  the 2 bit of lrgb_skip, the rgb_s2l function becomes a no-op.  Do
 *  not set lrgb_skip as a global variable; make it local to the function
 *  requiring this non-standard behavior.
 *
 * SEE ALSO: rgb_s2l, rgb2xyz, rgb2lab, rgb2luv, lrgb_clip
 */
{
  if (lrgb_skip && (lrgb_skip&1)) return rgb;
  extern srgb_clip;
  srgb_clip = ((rgb<-0.0001) | (rgb>1.0001));
  rgb = is_func(l2s_clipper)? l2s_clipper(rgb) : min(max(rgb, 0.), 1.);
  if (!lrgb_gamma) {
    u = 1.055*rgb^(5./12.) - 0.055;
    v = _srgb_0/_lrgb_0 * rgb;
    hi = double(rgb > _lrgb_0);
    return _rgb_scale(hi*u + (1.-hi)*v, cmax);
  } else {
    return rgb ^ lrgb_gamma;
  }
}

func rgb_s2l(rgb, g, b, cmax=)
/* DOCUMENT rgb_linear = rgb_s2l([r, g, b])
 *       or rgb_linear = rgb_s2l(r, g, b)
 *
 *  Returns physically linear red-green-blue, given RGB on a canonical
 *  monitor (the IEC 61966-2-1 sRGB).
 *  Return value has same dimensions as input (in first form).
 *  If input rgb are real, they are assumed normalized to lie in [0.,1.].
 *  If input are integers, they are assumed to lie in [0,255].
 *  You can specify a different maximum color value using the cmax= keyword.
 *
 *  You can also set the external variable lrgb_gamma to use a pure
 *  power law gamma for the conversion instead of the sRGB IEC 61966-2-1
 *  canonical monitor function.  Finally, if you set the 2 bit of the
 *  external variable lrgb_skip, rgb_s2l becomes a no-op.
 *
 * SEE ALSO: rgb_l2s, rgb2xyz, rgb2lab, rgb2luv
 */
{
  if (!is_void(g)) rgb = [rgb, g, b];
  if (lrgb_skip && (lrgb_skip&2)) return rgb;
  if (is_void(cmax)) cmax = (structof(rgb+0)==long)? 255. : 1.0;
  rgb *= 1./cmax;
  if (!lrgb_gamma) {
    u = ((rgb + 0.055)/1.055)^2.4;
    v = _lrgb_0/_srgb_0 * rgb;
    hi = double(rgb > _srgb_0);
    return hi*u + (1.-hi)*v;
  } else {
    return rgb ^ (1./lrgb_gamma);
  }
}

_srgb_0 = 5./7.*0.055;
_lrgb_0 = (12./7.*0.055/1.055)^2.4;

func _rgb_scale(rgb, cmax)
{
  if (is_void(cmax)) cmax = 255;
  if (cmax == 1) cmax = 1.0;
  rgb *= cmax;
  if (structof(cmax+0) == long) {
    rgb = long(rgb + 0.5);
    if (max(cmax) < 256) rgb = char(rgb);
  }
  return rgb;
}

func xyz2rgb(xyz, cmax=)
/* DOCUMENT srgb = xyz2rgb(xyz)
 *  Returns sRGB, the IEC 61966-2-1 is RGB on a canonical monitor, given
 *  the CIEXYZ.  By default, xyz2rgb returns a char [r,g,b] (of the same
 *  dimensions as XYZ, with the color index last), normalized to [0,255],
 *  but you can specify a different maximum color value using the cmax=
 *  keyword.  In particular, cmax=1 results in a double array normalized
 *  to lie in [0.,1.].
 *  XYZ colors may not be representable in rgb.  You can check the
 *  external variable srgb_clip after a call to xyz2rgb to find out
 *  if any colors have been clipped; it has the same dimensions as the
 *  input and is 1 where a color is clipped, otherwise 0.
 *  External variables lrgb_skip, lrgb_gamma, or lrgb_clipper affect this
 *  function; see rgb_l2s for details.
 * SEE ALSO: rgb2xyz, lab2rgb, rgb2lab, rgb2luv, rgb_s2l
 */
{
  return rgb_l2s(xyz(..,+) * _rgb_xyz(,+), cmax=cmax);
}

func rgb2xyz(rgb, g, b, cmax=)
/* DOCUMENT xyz = rgb2xyz([r, g, b])
 *       or xyz = rgb2xyz(r, g, b)
 *  Returns CIEXYZ, given RGB on a canonical monitor (the IEC 61966-2-1 sRGB).
 *  Return value has same dimensions as input (in first form).
 *  If input rgb are real, they are assumed normalized to lie in [0.,1.].
 *  If input are integers, they are assumed to lie in [0,255].
 *  You can specify a different maximum color value using the cmax= keyword.
 *  External variables lrgb_skip, lrgb_gamma, or lrgb_clipper affect this
 *  function; see rgb_l2s for details.
 * SEE ALSO: xyz2rgb, lab2rgb, rgb2lab, rgb2luv, rgb_l2s
 */
{
  if (!is_void(g)) rgb = [rgb, g, b];
  if (structof(rgb+0) == long) rgb *= 1./255.;
  return rgb_s2l(rgb, cmax=cmax)(..,+) * _xyz_rgb(,+);
}

_xyz_rgb = [[0.4124, 0.2126, 0.0193],
            [0.3576, 0.7152, 0.1192],
            [0.1805, 0.0722, 0.9505]];
_rgb_xyz = LUsolve(_xyz_rgb);

func lab2rgb(lab, cmax=)
/* DOCUMENT srgb = lab2rgb(lab)
 *  Returns sRGB, the IEC 61966-2-1 is RGB on a canonical monitor, given
 *  the CIELAB.  By default, lab2rgb returns a char [r,g,b] (of the same
 *  dimensions as LAB, with the color index last), normalized to [0,255],
 *  but you can specify a different maximum color value using the cmax=
 *  keyword.  In particular, cmax=1 results in a double array normalized
 *  to lie in [0.,1.].
 *
 *  The required reference white value is D65 white, which gives
 *  [255,255,255] and coresponds to LAB=[100,0,0].  The L coordinate is
 *  perceived luminance, and the angle in the (A,B) plane is hue.  The
 *  red-green component A is negative for green, positive for magenta,
 *  while the blue-yellow component B is negative for blue and positive
 *  for yellow.  Euclidean distance in 3D LAB space represents the
 *  perceptual difference between two colors.
 *
 *  LAB colors may not be representable in rgb.  You can check the
 *  external variable srgb_clip after a call to lab2rgb to find out
 *  if any colors have been clipped; it has the same dimensions as the
 *  input and is 1 where a color is clipped, otherwise 0.
 *
 *  Notes: chroma = abs(B, A)  hue = atan(B, A)  saturation = chroma/L
 *
 *  External variables lrgb_skip, lrgb_gamma, or lrgb_clipper affect this
 *  function; see rgb_l2s for details.
 * SEE ALSO: rgb2lab, rgb2xyz, xyz2rgb, rgb2luv, luv2rgb, rgb_s2l
 */
{
  xyz = double(lab);
  xyz(*,) *= 1./[116., -500., -200.](-,);
  l = xyz(..,1) + 16./116.;
  xyz(..,3) += l;
  xyz(..,1) = l - xyz(..,2);
  xyz(..,2) = l;
  xyz = _f_bal(xyz);
  xyz(*,) *= xyz_white(-,);
  return rgb_l2s(xyz(..,+) * _rgb_xyz(,+), cmax=cmax);
}

func rgb2lab(rgb, g, b, cmax=)
/* DOCUMENT xyz = rgb2lab([r, g, b])
 *       or xyz = rgb2lab(r, g, b)
 *  Returns CIEXYZ, given RGB on a canonical monitor (the IEC 61966-2-1 sRGB).
 *  Return value has same dimensions as input (in first form).
 *  If input rgb are real, they are assumed normalized to lie in [0.,1.].
 *  If input are integers, they are assumed to lie in [0,255].
 *  You can specify a different maximum color value using the cmax= keyword.
 *
 *  The required reference white value is D65 white, which gives
 *  [255,255,255] and coresponds to LAB=[100,0,0].  The L coordinate is
 *  perceived luminance, and the angle in the (A,B) plane is hue.  The
 *  red-green component A is negative for green, positive for magenta,
 *  while the blue-yellow component B is negative for blue and positive
 *  for yellow.  Euclidean distance in 3D LAB space represents the
 *  perceptual difference between two colors.
 *
 *  Notes: chroma = abs(B, A)  hue = atan(B, A)  saturation = chroma/L
 *
 *  External variables lrgb_skip, lrgb_gamma, or lrgb_clipper affect this
 *  function; see rgb_l2s for details.
 * SEE ALSO: lab2rgb, rgb2xyz, xyz2rgb, rgb2luv, luv2rgb, rgb_l2s
 */
{
  if (!is_void(g)) rgb = [rgb, g, b];
  if (structof(rgb+0) == long) rgb *= 1./255.;
  xyz = rgb_s2l(rgb, cmax=cmax)(..,+) * _xyz_rgb(,+);
  xyz(*,) *= 1./xyz_white(-,);
  lab = _f_lab(xyz);
  l = lab(..,2);
  lab(..,2:3) -= lab(..,1:2);
  lab(..,1) = l - 16./116.;
  lab(*,) *= [116., -500., -200.](-,);
  return lab;
}

/* default is D65 white, scaled so the y component = 1
 * Apparently the _rgb_xyz and _xyz_rgb matrices assume this,
 * so you will probably break things if you change it without
 * also fixing lRGB <--> XYZ the matrices.  See Wikipedia articles
 * http://en.wikipedia.org/wiki/Standard_illuminant and
 * http://en.wikipedia.org/wiki/SRGB for more.
 */
d65_white = [0.3127, 0.3290, 0.3583];  /* CIE 1931 2 degree XYZ */
xyz_white = [0.9505, 1., 1.089] /* = d65_white / d65_white(2); */

func _f_lab(x) {
  hi = double(x > (6./29.)^3);
  return hi*abs(x)^(1./3.)*sign(x) + (1.-hi)*((29./6.)^2/3.*x + 4./29.);
}

func _f_bal(x) {
  hi = double(x > 6./29.);
  return hi*x*x*x + 3.*(6./29.)^2*(1.-hi)*(x - 4./29.);
}

func luv2rgb(luv, cmax=)
/* DOCUMENT srgb = luv2rgb(luv)
 *  Returns sRGB, the IEC 61966-2-1 is RGB on a canonical monitor, given
 *  the CIELUV.  By default, luv2rgb returns a char [r,g,b] (of the same
 *  dimensions as LUV, with the color index last), normalized to [0,255],
 *  but you can specify a different maximum color value using the cmax=
 *  keyword.  In particular, cmax=1 results in a double array normalized
 *  to lie in [0.,1.].
 *
 *  The required reference white value is D65 white, which gives
 *  [255,255,255] and coresponds to LUV=[100,0,0].  The L component is
 *  perceptual luminance, abs(v,u) is perceptual chroma, and atan(v,u)
 *  is perceptual hue.  Euclidean distance in LUV is perceived color
 *  color difference.
 *
 *  LUV colors may not be representable in rgb.  You can check the
 *  external variable srgb_clip after a call to luv2rgb to find out
 *  if any colors have been clipped; it has the same dimensions as the
 *  input and is 1 where a color is clipped, otherwise 0.  You can also
 *  set the external variable l2s_clipper (to the function lrgb_clip,
 *  for example) to get more sophistocated clipping for colors outside
 *  the RGB gamut.  The lrgb_clip function maintains Luv luminance and
 *  hue and desaturating the color until it fits within the gamut.
 *
 *  The Luv luminance (L component) is identical to the Lab L component,
 *  and is a function only of the XYZ Y component.  The (u,v) components,
 *  like the (a,b) components are (0,0) for gray colors, so that hue
 *  varies with angle around (a,b).  The difference is, that the (u,v)
 *  coordinates are chosen so that lines of constant Luv hue (slope v/u)
 *  are straight lines in XYZ (or lRGB) space.  The lines of constant
 *  Lab hue (slope b/a), on the other hand, are curves in XYZ space.
 *
 *  Notes: chroma = abs(V, U)  hue = atan(V, U)  saturation = chroma/L
 *
 *  External variables lrgb_skip, lrgb_gamma, or lrgb_clipper affect this
 *  function; see rgb_l2s for details.
 * SEE ALSO: rgb2luv, rgb2xyz, xyz2rgb, rgb2lab, lab2rgb, rgb_s2l, lrgb_clip
 */
{
  uv = _uv_white();
  l = luv(..,1);
  zero = double(!l);
  d = 1./(13.*l+zero);
  u = uv(1) + luv(..,2)*d;
  v = uv(2) + luv(..,3)*d;
  xyz = double(luv);
  hi = double(l > 8.);
  xyz(..,2) = y = xyz_white(2) * (hi*((l+16.)/116.)^3 + (3./29.)^3*(1.-hi)*l);
  zero = double(!v);
  d = 0.25*(1.-zero)/(v+zero);
  xyz(..,1) = 9.*u*d * y;
  xyz(..,3) = (12.-3.*u-20.*v)*d * y;
  return rgb_l2s(xyz(..,+) * _rgb_xyz(,+), cmax=cmax);
}

func _uv_white(void)
{
  d = xyz_white(+) * [1.,15.,3.](+);
  return [4.*xyz_white(1), 9.*xyz_white(2)] / d;
}

func rgb2luv(rgb, g, b, cmax=)
/* DOCUMENT xyz = rgb2luv([r, g, b])
 *       or xyz = rgb2luv(r, g, b)
 *  Returns CIEXYZ, given RGB on a canonical monitor (the IEC 61966-2-1 sRGB).
 *  Return value has same dimensions as input (in first form).
 *  If input rgb are real, they are assumed normalized to lie in [0.,1.].
 *  If input are integers, they are assumed to lie in [0,255].
 *  You can specify a different maximum color value using the cmax= keyword.
 *
 *  The required reference white value is D65 white, which gives
 *  [255,255,255] and corresponds to LUV=[100,0,0].  The L component is
 *  perceptual luminance, abs(v,u) is perceptual chroma, and atan(v,u)
 *  is perceptual hue.  Euclidean distance in LUV is perceived color
 *  color difference.
 *
 *  Notes: chroma = abs(V, U)  hue = atan(V, U)  saturation = chroma/L
 *
 *  External variables lrgb_skip, lrgb_gamma, or lrgb_clipper affect this
 *  function; see rgb_l2s for details.
 * SEE ALSO: luv2rgb, rgb2xyz, xyz2rgb, rgb2lab, lab2rgb, rgb_l2s
 */
{
  if (!is_void(g)) rgb = [rgb, g, b];
  if (structof(rgb+0) == long) rgb *= 1./255.;
  xyz = rgb_s2l(rgb, cmax=cmax)(..,+) * _xyz_rgb(,+);
  y = xyz(..,2) / xyz_white(2);
  hi = double(y > (6./29.)^3);
  luv = double(xyz);
  luv(..,1) = l = hi*(116*abs(y)^(1./3.) - 16.) + (29./3.)^3*(1.-hi)*y;
  d = xyz(..,+) * [1., 15., 3.](+);
  zero = double(!d);
  d = (1.-zero)/(d+zero);
  uv = _uv_white();
  luv(..,2:3) = 13. * l * [4.*xyz(..,1)*d - uv(1), 9.*xyz(..,2)*d - uv(2)];
  return luv;
}

func lrgb_clip(rgb)
/* DOCUMENT clipped_lrgb = lrgb_clip(lrgb)
 *
 *   With input LRGB = physically linear [r,g,b] as real values (final
 *   index is length 3), clip any colors with one or more components
 *   outside the interval [0,1] along a line from lrgb to the gray
 *   with the same luminance.  The clipped LRGB will always have at
 *   least one component either 0 or 1.  This procedure maintains the
 *   CIE Luv luminance and hue, simply desaturating the color until it
 *   fits inside the gamut.  (Note that CIE Lab hue is not maintained.)
 *
 *   You can set the external variable lrgb_gray to any value you want
 *   in order to define the "gray with the same luminance" as something
 *   other than the CIE Luv luminance.  The gray value will be
 *   LRGB(..,+)*lrgb_gray(+)/sum(lrgb_gray).  The default is equivalent
 *   to lrgb_gray = _xyz_rgb(2,), the CIE Y value.
 *
 *   You can force rgb_l2s to clip using lrgb_clip by setting the
 *   external variable l2s_clipper = lrgb_clip.
 *
 * SEE ALSO: rgb_l2s, luv2rgb
 */
{
  rgb0 = rgb;
  rgb = min(max(rgb, 0.), 1.);
  list = where((rgb0(..,max)>1.0001) | (rgb0(..,min)<-0.0001));
  if (numberof(list)) {  /* dont bother with microscopic clips */
    rgb = transpose(rgb, 2);
    rgb0 = transpose(rgb0, 2)(,list);
    if (is_void(lrgb_gray)) gray = _xyz_rgb(2,);      /* Y = default gray */
    else gray = lrgb_gray / sum(lrgb_gray);
    w = gray(-:1:3,+) * rgb0(+,);  /* gray level, assume gray(sum) = 1.0 */
    /* find first point between [w,w,w] and rgb0 where one of rgb is 0 or 1 */
    x = rgb0 - w;
    zero = double(!x);
    x = ((1.-zero) * [-w, 1.-w] / (x + zero));
    x += (x<=0.)*1.e30;                   /* get rid of solutions with x<=0 */
    rgb0 = w + (rgb0 - w)*x(-,min,..,min); /* take minimum of x>0 solutions */
    rgb0 = min(max(rgb0, 0.), 1.);  /* happens when w<0 or w>1 */
    rgb(,list) = rgb0;
    rgb = transpose(rgb, 0);
  }
  return rgb;
}
