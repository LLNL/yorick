/* hsv2rgb.i
 * conversions among RGB, HSV, HSL, CMYK color representations
 *
 * See Wikipedia article "HSL and HSV"
 */
/* Copyright (c) 2013, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func rgb2hsv(r, g, b, cmax=)
/* DOCUMENT hsv = rgb2hsv([r, g, b])
 *       or hsv = rgb2hsv(r, g, b)
 *  Returns hue-saturation-value given red-green-blue.
 *  Return value has same dimensions as input (in first form).
 *  If input rgb are real, they are assumed normalized to lie in [0.,1.].
 *  If input are integers, they are assumed to lie in [0,255].
 *  You can specify a different maximum color value using the cmax=
 *  keyword.
 *  Note that the HSV and HSL systems share a common definition of
 *  hue H, but that saturation S is a very different thing.
 * SEE ALSO: rgb2hsl, hsv2rgb, hsl2rgb, rgb2cmyk, cmyk2rgb, rgb2hsi
 */
{
  local rgb, V, m, C, L, H;
  _2hs;
  S = C / max(V, 1.e-20);  /* hsv saturation */
  return [H, S, V];
}

func rgb2hsl(r, g, b, cmax=)
/* DOCUMENT hsl = rgb2hsl([r, g, b])
 *       or hsl = rgb2hsl(r, g, b)
 *  Returns hue-saturation-lightness given red-green-blue.
 *  Return value has same dimensions as input (in first form).
 *  If input rgb are real, they are assumed normalized to lie in [0.,1.].
 *  If input are integers, they are assumed to lie in [0,255].
 *  You can specify a different maximum color value using the cmax=
 *  keyword.
 *  Note that the HSV and HSL systems share a common definition of
 *  hue H, but that saturation S is a very different thing.
 * SEE ALSO: rgb2hsv, hsv2rgb, hsl2rgb, rgb2cmyk, cmyk2rgb, rgb2hsi
 */
{
  local rgb, V, m, C, L, H;
  _2hs;
  S = C / max(1.-abs(2.*L-1.), 1.e-20);  /* hsl saturation */
  return [H, S, L];
}

func hsv2rgb(H, S, V, cmax=)
/* DOCUMENT rgb = hsv2rgb([h, s, v])
 *       or rgb = hsv2rgb(h, s, v)
 *  Returns red-green-blue given hue-saturation-value.
 *  Return value has same dimensions as input (in first form).
 *  Output rgb are type char by default, with range [0,255].
 *  You can specify a different maximum color value using the cmax=
 *  keyword.  With cmax=1, return value will be type double, with
 *  range [0.,1.]; otherwise, the return value will be double if cmax
 *  is real, char if cmax is an integer <=255, otherwise long.
 *  Note that the HSV and HSL systems share a common definition of
 *  hue H, but that saturation S is a very different thing.
 * SEE ALSO: hsl2rgb, rgb2hsv, rgb2hsl, rgb2cmyk, cmyk2rgb, rgb2hsi
 */
{
  if (is_void(S)) {
    S = H(..,2);
    V = H(..,3);
    H = H(..,1);
  }
  C = V*S;
  return _2rgb(H, C, V-C, cmax=cmax);
}

func hsl2rgb(H, S, L, cmax=)
/* DOCUMENT rgb = hsl2rgb([h, s, l])
 *       or rgb = hsl2rgb(h, s, l)
 *  Returns red-green-blue given hue-saturation-lightness.
 *  Return value has same dimensions as input (in first form).
 *  Output rgb are type char by default, with range [0,255].
 *  You can specify a different maximum color value using the cmax=
 *  keyword.  With cmax=1, return value will be type double, with
 *  range [0.,1.]; otherwise, the return value will be double if cmax
 *  is real, char if cmax is an integer <=255, otherwise long.
 *  Note that the HSV and HSL systems share a common definition of
 *  hue H, but that saturation S is a very different thing.
 * SEE ALSO: hsv2rgb, rgb2hsl, rgb2hsv, rgb2cmyk, cmyk2rgb, rgb2hsi
 */
{
  if (is_void(S)) {
    S = H(..,2);
    L = H(..,3);
    H = H(..,1);
  }
  C = (1.-abs(2.*L-1.)) * S;
  return _2rgb(H, C, L-0.5*C, cmax=cmax);
}

func _2rgb(H, C, m, cmax=)
{
  H *= 1./60.;
  H -= 6.*floor(H/6.);  /* permit illegal hue >360 or <0 */
  n = array(0, dimsof(H));
  nc = numberof(H);
  n(*) = indgen(nc);
  rgb = array(0., dimsof(H), 3);
  i = digitize(H, indgen(5)) % 6;
  j = i / 2;
  i = (j + [2,1]((i&1)+1)) % 3;
  rgb(n+j*nc) = C;
  rgb(n+i*nc) = C * (1. - abs(H%2. - 1.));
  rgb += m;
  if (is_void(cmax)) cmax = 255;
  if (cmax == 1) cmax = 1.0;
  rgb *= cmax;
  if (structof(cmax+0) == long) {
    rgb = long(rgb + 0.5);
    if (max(cmax) < 256) rgb = char(rgb);
  }
  return rgb;
}

func _2hs
{
  extern rgb, V, m, C, L, H;
  rgb = is_void(g)? r : [r, g, b];
  if (is_void(cmax)) cmax = (structof(r+0)==long)? 255. : 1.0;
  rgb *= 1./cmax;
  r = rgb(..,1);
  g = rgb(..,2);
  b = rgb(..,3);
  V = rgb(..,max); /* value */
  m = rgb(..,min);
  if (max(V) > 1.) error, "need explicit cmax= specification";
  if (min(m) < 0.) error, "rgb color component less than zero";
  C = V - m;       /* chroma */
  L = 0.5*(V+m);   /* lightness */
  n = array(0, dimsof(r));
  nr = numberof(r);
  n(*) = indgen(nr);
  i = (V == rgb)(..,mxx) - 1;
  H = ((rgb(n+((i+1)%3)*nr) - rgb(n+((i+2)%3)*nr))/max(C,1.e-20) + 2.*i+6.)%6.;
  H *= 60.;        /* hue */
}

func rgb2cmyk(r, g, b, cmax=)
/* DOCUMENT cmyk = rgb2cmyk([r, g, b])
 *       or cmyk = rgb2cmyk(r, g, b)
 *  Returns cyan-magenta-yellow-black given red-green-blue.  The
 * returned [c, m, y, k] are normalized to lie in [0., 1.].
 *  Return value has same dimensions as input (in first form).
 *  If input rgb are real, they are assumed normalized to lie in [0.,1.].
 *  If input are integers, they are assumed to lie in [0,255].
 *  You can specify a different maximum color value using the cmax=
 *  keyword.
 *  In this simple conversion scheme, the complement of the largest
 *  component of [r,g,b] will always be 0.0.  This is the choice with
 *  the largest possible k component.
 * SEE ALSO: cmyk2rgb, rgb2hsv, hsv2rgb, rgb2hsl, hsl2rgb, rgb2hsi
 */
{
  rgb = is_void(g)? r : [r, g, b];
  if (is_void(cmax)) cmax = (structof(r+0)==long)? 255. : 1.0;
  rgb *= 1./cmax;
  V = rgb(..,max);    /* value */
  zero = double(!V);
  rgb *= (1. + zero) / (V + zero);
  c = 1. - rgb(..,1);
  m = 1. - rgb(..,2);
  y = 1. - rgb(..,3);
  k = 1. - V;
  return [c, m, y, k];
}

func cmyk2rgb(c, m, y, k, cmax=)
/* DOCUMENT rgb = cmyk2rgb([c, m, y, k])
 *       or rgb = cmyk2rgb(c, m, y, k)
 *  Returns red-green-blue given cyan-magenta-yellow-black.  The
 *  C, M, Y, K values must be between 0.0 and 1.0 inclusive.
 *  Return value has same dimensions as input (in first form).
 *  Output rgb are type char by default, with range [0,255].
 *  You can specify a different maximum color value using the cmax=
 *  keyword.  With cmax=1, return value will be type double, with
 *  range [0.,1.]; otherwise, the return value will be double if cmax
 *  is real, char if cmax is an integer <=255, otherwise long.
 * SEE ALSO: rgb2cmyk, hsv2rgb, rgb2hsv, hsl2rgb, rgb2hsl, rgb2hsi
 */
{
  rgb = 1. - (is_void(m)? c : [c, m, y, k]);
  rgb = rgb(,1:3) * rgb(4);
  if (is_void(cmax)) cmax = 255;
  if (cmax == 1) cmax = 1.0;
  rgb *= cmax;
  if (structof(cmax+0) == long) {
    rgb = long(rgb + 0.5);
    if (max(cmax) < 256) rgb = char(rgb);
  }
  return rgb;
}

/* ------------------------------------------------------------------------ */
