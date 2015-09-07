/* cmap.i
 * Large enhancement to palette function, including dozens of additional
 * pre-defined palette choices: ColorBrewer, Matplotlib (python), Gnuplot,
 * and GMT palettes included.
 */
/* Copyright (c) 2013, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */
require, "hsv2rgb.i";
require, "lab2rgb.i";

func cmap(p, n, hist=, hsv=, hsl=, rev=, gamma=, lgamma=, usehue=, nmax=)
/* DOCUMENT cmap, p, n
 *       or rgb = cmap(p, n)
 *   returns a list of colors 0xrrggbb corresponding to the colormap
 *   defined by parameters P.  The N argument is optional; it is the
 *   the number of colors you want in the palette.  N defaults to the
 *   number of colors in the palette for the current window (usually 200).
 *   If you specify N<0, that many colors will be appended to the
 *   existing palette, enabling you to stack multiple shortened palettes.
 *   Called as a subroutine, actually sets the palette; called as a
 *   function, returns [r,g,b] as Nx3 char array.
 *
 *   Recommendations: Read http://www.sandia.gov/~kmorel/documents/ColorMaps/
 *   Most color maps here are rather poor choices.  Some reasonably good ones
 *   are: "gray", "coolwarm", "bone", "copper", and most of the ColorBrewer
 *   maps, such as "Blues" or "PuOr".  The function cmap_test makes a
 *   picture you can use to test palettes.  The "dblue", "dred", and "dgreen"
 *   maps are a matched set designed to be distinguishable by colorblind
 *   people.  Use them if you need multiple maps in one picture.
 *
 *   See also http://bids.github.io/colormap/ for a discussion of colormap
 *   design.  The excellent "viridis", "magma", "inferno", and "plasma"
 *   colormaps are available with cmap.  Besides "gray", these are the best
 *   choices for sequential colormaps; "coolwarm" is the best choice for a
 *   diverging colormap.  All of these are colorblind-friendly, perceptually
 *   uniform, and the sequential maps are also uniform in brightness (good
 *   for being reproduced in gray scale).
 *
 *   Keywords:
 *     hist=1  if P is a simple list of colors, creates a stepped
 *             palette instead of interpolating between colors
 *     hist=2  like hist=1, but the first and last colors get only a
 *             single map index instead of a full width step
 *     rev=1   reverses order of the colors (also "_r" when P is name)
 *     hsv=1   interpret P values as hue, saturation, value, not rgb
 *     hsl=1   interpret P values as hue, saturation, lightness, not rgb
 *     gamma=  adjust the value (after transforming to HSV) by V^gamma
 *             gamma=1 does nothing, gamma>1 darkens the colors, and
 *             gamma<1 brightens the colors
 *     lgamma= adjust the lightness (after transforming to HSL) by L^gamma
 *             gamma=1 does nothing, gamma>1 darkens the colors, and
 *             gamma<1 lightens the colors
 *     usehue=1  if P is a simple list of colors, interpolates in hsv
 *             instead of rgb, sometimes a nicer option (try with "accent")
 *
 *   Specifiy the rev=, gamma=, or lgamma= keywords with no other parameters
 *   to adjust the existing palette.  In the case of gamma corrections,
 *   cmap remembers the initial rgb values, so that you can accurately
 *   apply multiple gamma corrections interactively.  The total gamma is
 *   kept in the external variable _cm_gamma (the product of all the
 *   applied gammas).  Hence, a sequence of cmap,gamma=.8 commands (say)
 *   can be used to brighten the colors preogressively, and a sequence of
 *   cmap,gamma=1.25 commands will darken it.
 *
 *   P can be:
 *   name
 *     a scalar string naming a named palette
 *     append "_r" to name to reverse it, "_N" to name, where N is a
 *     number, to select a map of that name with the specified number
 *     of colors and select hist=N
 *   [rgb1, rgb2, ...]
 *     a list of equally spaced colors as 0xrrggbb; all three components
 *     will be linearly interpolated between these given colors
 *   [[x1,rgb1], [x2,rgb2], ..., [xN,rgbN]]
 *     a list of unequally spaced colors as 0xrrggbb; all three components
 *     will be linearly interpolated between these given colors,  The
 *     x values are linearly scaled so that x1 is mapped to the first
 *     returned color and xN is mapped to the last returned color.
 *   [[r1,g1,b1], [r2,g2,b2], ...]
 *     equally spaced colors as rgb triples, range 0.0 to 1.0 if real,
 *     0 to 255 if integers
 *   [[x1,r1,g1,b1], [x2,r2,g2,b2], ...]
 *     unequally spaced colors as rgb triples, range 0.0 to 1.0 if real,
 *     0 to 255 if integers
 *   [&[[xr1,r1], [xr2,r2], ...], &[[xg1,g1], [xg2,g2], ...],
 *    &[[xb1,b1], [xb2,b2], ...]]
 *     a list of unequally spaced color components rN, gN, bN, which are
 *     in the range from 0.0 to 1.0 inclusive (mapped to 0 and 255 as char
 *     color component values for the returned palette).  Again, the
 *     xrN, xgN, and xbN values are linearly scaled so that the first color
 *     in the returned list gets the first value in the list and the last
 *     color gets the last value.  Matplotlib calls this a "linearly
 *     segmented color map".
 *   [[rindex, gindex, bindex]]
 *     gnuplot function indices for each component, 0 to 36 inclusive
 *     trailing 1-length dimension distinguishes from [rgb1, rgb2, ...].
 *   [[[p1, p2, p3, p4]]]
 *     cmap_nonlin function parameterizing a few power laws and trig
 *     functions:
 *       p1*sin((p2*x+p3)*pi) + p4       but
 *       abs(p1*sin((p2*x+p3)*pi) + p4)  if p1<0 and p2<0
 *       p1*x^p3 + p4                    if p2==0
 *   [r, g, b]
 *     sets palette directly, ignoring n parameter unless hist=1, but
 *     accepting hsv=, hsl=, and rev= keywords.  (r, g, b must have more
 *     than 4 colors, but no more than 240.)
 *   save(p=params,hist=hist,...)
 *     an object containing cmap parameters and keywords permits any
 *     input to cmap (except n) to be return value of another function
 *
 *  Available named color maps are the yorick palette names (less ".gp"),
 *  the ColorBrewer color maps (help, cb_choices), Matplotlib (python)
 *  color maps, and GMT (Generic Mapping Tools) color maps.  Some of the
 *  names conflict; you can use the functions gistct, mplct, and gmtct
 *  to break any conflicts (the ColorBrewer names are all unique).  Also
 *  featured are several of the diverging color maps devised by Kenneth
 *  Moreland.  See help,mshct for more on those.  Finally, a sequential
 *  map design tool I invented, seqct, is available.
 *
 *  The Gnuplot and IDL programs use a scheme of indexed palettes; the
 *  functions gplct and idlct allow you to set those palettes.  Some of
 *  the Gnuplot palettes are named Matplotlib palettes, including all
 *  the ones mentioned in the Gnuplot user manual.  The cubehelix function
 *  lets you set palettes by parameters; you can get the default parameters
 *  with cmap, "cubehelix".
 *
 * SEE ALSO: mshct, seqct, gplct, mplct, gmtct, gistct, idlct, cubehelix,
 *           cb_choices, cmap_rd, cmap_test
 */
{
  if (!is_void(lgamma)) { gamma = lgamma; lgamma = 1; }
  if (is_void(p)) {
    local r, g, b;
    palette, query=1, r, g, b;
    if (!numberof(r)) return [];
    p = transpose([r, g, b]);
    if (!rev && !gamma && !n) return p;
    if (is_void(n)) n = numberof(r);
  }
  if (is_obj(p)) {
    if (is_void(hist)) hist = p.hist;
    if (is_void(usehue)) usehue = p.usehue;
    if (is_void(rev)) rev = 0;
    rev = (rev!=0) ^ (p.rev!=0);
    hsv = p.hsv;
    hsl = p.hsl;
    p = p.p;
  }
  if (!is_void(n) && (n<2 || n>240))
    error, "illegal number of colors for palette, must be between 2 and 240";
  n0 = n;
  if (structof(p) == string) {  /* name */
    name = p = cmap_name(p, n, rev);
    if (is_func(cmap_user_map)) p = cmap_user_map(name, n);
    else p = [];
    if (!is_void(cmap_alias)) {
      list = where(name == cmap_alias(1,));
      if (numberof(list)) name = cmap_alias(2,list(1));
    }
    if (is_void(p) && name=="cubehelix") {
      rgb = cubehelix(n);
      if (rev) rgb = rgb(::-1,);
      if (am_subroutine()) palette, rgb(,1), rgb(,2), rgb(,3);
      return rgb;
    }
    if (is_void(p)) {
      local type;
      p = cb_map(name, ((n && n<13)?n:[]), type);
      if (!is_void(p)) {
        if ((n != n0) && is_void(hist)) hist = 1;
        n = n0;
        if (is_void(hist) && type==3) hist = 1;
      }
    }
    if (is_void(p)) {
      i = mshct(name, , n0);
      if (numberof(i)) p = transpose(i);
    }
    if (is_void(p)) {
      i = seqct(name, n0);
      if (numberof(i)) p = transpose(i);
    }
    if (is_void(p)) {
      i = where(mpl_names == name);
      if (numberof(i)) p = *mpl_maps(i(1));
    }
    if (is_void(p)) {
      i = where(gmt_names == name);
      if (numberof(i)) {
        p = *gmt_maps(i(1));
        flags = gmt_flags(i(1));
        if (flags == 1) hist = 1;
        else if (flags == 2) hsv = 1;
      }
    }
    if (is_void(p)) {
      i = where(gist_names == name);
      if (numberof(i)) {
        p = *gist_maps(i(1));
        if (name == "earth") hsv = 1;
      }
    }
    if (is_void(p)) {
      p = _cmap_gist_scan(name)
    }
    if (is_void(p)) error, "unrecognized palette name "+name;
  }
  if (is_void(n)) {
    n = cmap_ncolors();  /* avoids calling it twice */
  } else if (n < 0) {
    palette, query=1, r, g, b;
    rgb0 = [r, g, b];   /* for append mode */
    n = -n;
  }
  d = dimsof(p);
  if (structof(p) == pointer) {     /* [&[[xr1,r1], [xr2,r2], ...], ...] */
    r = *p(1);
    g = *p(2);
    b = *p(3);
    scale = cmap_get_scale(r, g, b);
  } else if (d(1) == 2) {
    if (d(3) == 1) {         /* [[rindex, gindex, bindex]] */
      rgb = gnu_map(p(1), p(2), p(3), n);
      r = g = b = span(0.,1.,numberof(rgb)/3)(-:1:2,);
      r(2,) = rgb(,1);
      g(2,) = rgb(,2);
      b(2,) = rgb(,3);
      if (hsv || hsl) r *= 360.;
      scale = 255.;
    } else if (d(2) == 2) {  /* [[x1,rgb1], [x2,rgb2], ... */
      r = g = b = p;
      p = p(2,);
      r(2,) = char(p >> 16);
      g(2,) = char(p >> 8);
      b(2,) = char(p);
      scale = 1.;
    } else if (d(2) == 3) {  /* [[r1,g1,b1], [r2,g2,b2], ... */
      r = g = b = structof(p+0)(indgen(numberof(p)/3))(-:1:2,);
      r(2,) = p(1,);
      g(2,) = p(2,);
      b(2,) = p(3,);
      scale = cmap_get_scale(r, g, b);
      histok = 1;
    } else if (d(2) == 4) {  /* [[x1,r1,g1,b1], [x2,r2,g2,b2], ... */
      r = g = b = p(1,-:1:2,);
      r(2,) = p(2,);
      g(2,) = p(3,);
      b(2,) = p(4,);
      scale = cmap_get_scale(r, g, b);
    } else if (d(3) == 3) {  /* [r, g, b] */
      if (rev) p = p(::-1,);
      if (hsv) p = hsv2rgb(p);
      else if (hsl) p = hsl2rgb(p);
      if (structof(p+0) != long) p = long(255*p+0.5);
      if (hist) {
        x = span(0., 1., n);
        x = digitize(x, span(0., 1., numberof(p)/3+1)(2:-1));
        p = p(x,);
      }
      if (am_subroutine()) palette, char(p(,1)), char(p(,2)), char(p(,3));
      return p;
    } else {
      error, "unrecognized color map parameterization";
    }
  } else if (d(1)==3 && d(2)==4 && d(3)==3 && d(4)==1) {
    x = span(0., 1., n);
    r = g = b = x(-:1:2,);
    r(2,) = cmap_nonlin(p(,1,1), x);
    g(2,) = cmap_nonlin(p(,2,1), x);
    b(2,) = cmap_nonlin(p(,3,1), x);
    scale = 255.;
  } else if (structof(p+0) == long) {  /* [rgb1, rgb2, ... */
    r = g = b = indgen(numberof(p))(-:1:2,);
    r(2,) = (p >> 16) & 0xff;
    g(2,) = (p >> 8) & 0xff;
    b(2,) = p & 0xff;
    scale = 1.;
    histok = 1;
  } else {
    error, "unrecognized color map parameterization";
  }
  if (nmax) {
    if (am_subroutine()) error, "too many colors to set";
    n = 256;
  }
  x = span(0., 1., n);
  if (rev) x = 1. - x;
  if (hist) {
    if (!histok) error, "need simple list of colors for hist=1";
    if (hist != 2) x0 = span(0., 1., numberof(r)/2+1)(2:-1);
    else x0 = span(1.e-6, 1.-1.e-6, numberof(r)/2-1);
    x = digitize(x, x0);
    r = r(2,x);
    g = g(2,x);
    b = b(2,x);
  } else {
    cminterp = cmap_interp;
    if (usehue) {
      if (!histok) error, "need simple list of colors for usehue=1";
      if ((!hsv) & (!hsl)) hsv = 1;
      rgb = (hsv? rgb2hsv : rgb2hsl)(r(2,), g(2,), b(2,));
      r = double(r);  g = double(g);  b = double(b);
      r(2,) = rgb(,1);  g(2,) = rgb(,2);  b(2,) = rgb(,3);
      scale = 255.;
      cminterp = hue_interp;
    }
    r = cminterp(r, x);
    g = cmap_interp(g, x);
    b = cmap_interp(b, x);
  }
  if (hsv || hsl) {
    if (scale != 255.) error, "expecting real values for hsv or hsl";
    rgb = (hsv? hsv2rgb : hsl2rgb)(r, g, b);
  } else {
    rgb = char(scale*[r, g, b] + 0.5);
  }
  if (numberof(rgb0)) rgb = grow(rgb0, rgb);
  /* remember initial rgb to permit accurate multiple gamma corrections */
  if (numberof(rgb)!=numberof(_cm_rgb) || nallof(rgb==_cm_rgb)) {
    extern _cm_gamma, _cm_rgb0;
    _cm_gamma = _cm_lgamma = _cm_rgb0 = _cm_rgb = [];
  }
  if (gamma && gamma!=1.) {
    cmg = (lgamma? _cm_lgamma : _cm_gamma);
    if (cmg) {
      rgb = _cm_rgb0;
      gamma *= cmg;
    }
    _cm_rgb0 = rgb;
    if (lgamma) {
      _cm_lgamma = gamma;
      _cm_gamma = [];
    } else {
      _cm_gamma = gamma;
      _cm_lgamma = [];
    }
    rgb = (lgamma? rgb2hsl : rgb2hsv)(rgb);
    rgb(,3) = rgb(,3)^gamma;
    rgb = _cm_rgb = (lgamma? hsl2rgb : hsv2rgb)(rgb);
  }
  if (!am_subroutine()) return rgb;
  palette, rgb(,1), rgb(,2), rgb(,3);
}

cmap_alias = [["france", "polar"]];

func cmap_name(name, &n, &rev)
{
  if (strpart(name,-1:0) == "_r") {
    rev = !rev;
    name = strpart(name, 1:-2);
  }
  i = strgrep("(.+)_([0-9]+)", name, sub=[1,2]);
  if (i(4) > i(3)) {
    n = long(tonum(strpart(name, i(3:4))));
    if (n<2 || n>240)
      error, "illegal number of colors for palette, must be between 2 and 240";
    name = strpart(name, i(1:2));
  }
  return name;
}

func cmap_get_scale(r, g, b)
{
  if (structof(r(1)+g(1)+b(1)+0)==long && max(r(2,max),g(2,max),b(2,max))>1)
    return 1.;
  else
    return 255.;
}

func cmap_ncolors(void)
{
  local r, g, b;
  palette, query=1, r, g, b;
  return (numberof(r) < 2)? 200 : numberof(r);
}

func cmap_clip(x)
{
  return min(max(x, 0.), 1.);
}

func cmap_nonlin(p, x)
{
  if (!p(1)) return (2.*x-1.)^2;   /* gnuplot function 12 */
  if (!p(2)) x = p(1)*x^p(3) + p(4);
  else x = p(1)*sin((p(2)*x + p(3)) * pi) + p(4);
  if (p(1)<0. && p(2)<0.) x = abs(x);
  return cmap_clip(x);
}

func cmap_interp(p, x)
{
  xp = p(1,);
  xp = (xp - xp(1)) / double(xp(0) - xp(1));
  list = where(!xp(dif));
  if (numberof(list)) { /* arrange for discontinuous jump */
    xp(list+1) += 1.e-6;
    if (xp(0) > 1.) {
      xp(-1) -= 1.e-6;
      xp(0) = 1.;
    }
  }
  return interp(p(2,), xp, x);
}

func hue_interp(p, x)
{
  p = p;
  h = p(2,) / 360.;
  h -= floor(h);
  dh = h(dif);
  list = where(abs(dh) > 0.5);
  if (numberof(list)) dh(list) -= sign(dh(list));
  h = h(1) + dh(cum);
  p(2,) = 360. * h;
  return cmap_interp(p, x);
}

func cmap_test
/* DOCUMENT cmap_test
 *   Create a picture to help test color maps for artifacts and common
 *   misfeatures.  After calling this, call cmap or one of the *ct
 *   functions to change the color map.
 * SEE ALSO: cmap, mshct, seqct, mplct, gplct, gmtct, idlct, cb_choices
 */
{
  x=span(0,1,400)(,-:1:400);  y=transpose(x);
  fma;  limits;
  pli, (1.02-y)*sin(8*pi*x);
}

func gistct(name)
/* DOCUMENT cm = gistct(name)
 *       or gistct, name
 *   return color map (ct = "color table") specification for yorick
 *   map NAME (where NAME.gp is the name of the palette file).
 *   You may append "_r" to the name to reverse the colors.
 *   Called as a subroutine, sets the current palette.
 * SEE ALSO: cmap, mplct, gplct, gmtct, idlct
 */
{
  local n, rev;
  name = cmap_name(name, n, rev);
  i = where(name == gist_names);
  if (!numberof(i)) error, name+" is not a gist color map";
  p = *gist_maps(i(1));
  if (name == "earth") hsv = 1;
  if (!am_subroutine())
    return (hsv || rev)? save(p=p, hsv=hsv, rev=rev) : p;
  cmap, p, rev=rev, hsv=hsv;
}

/* these are good approximations to the gist palettes of the same names */
gist_names = ["earth", "heat", "rainbow", "gray", "yarg", "stern", "ncar"];

/* heat, rainbow, stern, and ncar parameterizations by
 *   Reinier Heeres for matplotlib
 * earth is hsv
 */
gist_maps = [&[[1,0,0,0],[1,240,1,.135],[7,240,1,.455],[57,180,.629,.5],
               [109,93.9,.512,.65],[157,40.67,.472,.753],[200,0,0,1]],
             &[&[[0,0], [7,1], [10,1]], &[[0,0], [19,0], [40,1]],
               &[[0,0], [3,0], [4,1]]],
             &[[1,0xff0029],[8,0xff0000],[52,0xffff00],[97,0x00ff00],
               [141,0x00ffff],[185,0x0000ff],[229,0xff00ff],[240,0xff00bf]],
             &[0x000000,0xffffff], &[0xffffff,0x000000],
             &[&[[0,0], [0.0547,1], [0.25,.027], [0.25,.25], [1,1]],
               &[[0,0], [1,1]], &[[0,0], [.5,1], [0.735,0], [1,1]]],
             &[&[[0,0],[0.3098,0],[0.3725,0.3993],[0.4235,0.5003],[0.5333,1],
                 [0.7922,1],[0.8471,0.6218],[0.8980,0.9235],[1,0.9961]],
               &[[0,0],[0.0510,0.3722],[0.1059,0],[0.1569,0.7202],
                 [0.1608,0.7537],[0.1647,0.7752],[0.2157,1],[0.2588,0.9804],
                 [0.2706,0.9804],[0.3176,1],[0.3686,0.8081],[0.4275,1],
                 [0.5216,1],[0.6314,0.7292],[0.6863,0.2796],[0.7451,0],
                 [0.7922,0],[0.8431,0.1753],[0.8980,0.5],[1,0.9725]],
               &[[0.0,0.5020],[0.0510,0.0222],[0.1098,1],[0.2039,1],
                 [0.2627,0.6145],[0.3216,0],[0.4157,0],[0.4745,0.2342],
                 [0.5333,0],[0.5804,0],[0.6314,0.0549],[0.6902,0],[0.7373,0],
                 [0.7922,0.9738],[0.8000,1],[0.8431,1],[0.8980,0.9341],
                 [1,0.9961]]]];

func cmap_rd(f, &hsv)
/* DOCUMENT rgb = cmap_rd(filename, hsv)
 *       or cmap_rd, filename
 *  Read a color map from file FILENAME.  Called as a subroutine, passes
 *  the map to cmap to install it as the current palette.  Accepts yorick
 *  (gist) .gp format or GMT .cpt format.  The HSV argument is an output,
 *  non-nil if and only if this is to be interpreted in HSV color space.
 * SEE ALSO: cmap
 */
{
  hsv = [];
  lines = text_lines(f);
  n = strgrep("^[ \t]*ncolors[ \t]*=[ \t]*([0-9]+)", lines, sub=1);
  list = where(n(1,) < n(2,));
  if (numberof(list)) {  /* this is .gp format */
    nc = 0;
    sread, strpart(lines(list(1)), n(,list(1))), nc;
    lines = strpart(lines, strgrep("^[ \t]*([0-9]+[ \t]*[0-9]+[ \t]*[0-9]+)",
                                   lines, sub=1));
    lines = lines(where(lines));
    if (numberof(lines) != nc)
      error, "ncolors does not match rgb list in "+f;
    rgb = array(char, 3, nc);
    if (sread(lines, rgb) != 3*nc)
      error, "problem reading .gp file "+f;
  } else {
    lines = strtrim(lines);
    n = strgrep("^#[ \t]*COLOR_MODEL[ \t]*=.*(RGBA?|HSV|CMYK)[ \t]*",
                lines, sub=1);
    m = strpart(lines, n);
    m = m(where(m))(1);
    if (m == "HSV") hsv = 1;
    else if (m == "CMYK") cmyk = 1;
    else if (m == "RGBA") rgba = 1;
    else if (m == "RGB") rgb = 1;
    else if (m) error, "unrecognized COLOR_MODEL in "+f;
    lines = strpart(lines, strgrep("^[-+. \t0-9]+", lines));
    lines = lines(where(lines));
    rgb = array(0., 4+(cmyk||rgba), 2, numberof(lines));
    if (sread(lines, rgb) != numberof(rgb))
      error, "problem reading color table in "+f;
    if (cmyk) error, "no support for CMYK color table in "+f;
    rgb = rgb(1:4,*);
    if (!hsv) {
      if (allof(rgb == long(rgb))) rgb = long(rgb);
      else if (max(rgb(2:4,)) > 1.) rgb(2:4,) /= 255.;
    }
  }
  if (!am_subroutine()) return rgb;
  cmap, rgb, hsv=hsv;
}

/* ------------------------------------------------------------------------ */
/* msh colormaps, http://www.sandia.gov/~kmorel/documents/ColorMaps/
 * devised by Kenneth Moreland
 * The coolwarm Matplotlib map below is apparently a botched version of these.
 * Coolwarm, blutan, and redgrn appear in Moreland's paper.  The others are
 * random fiddling with the mshct rgb1 and rgb2.
 */

msh_names = ["coolwarm", "blutan", "ornpur", "grnred",
             "purple", "blue", "green", "red", "brown"];
msh_maps = [[[ 59, 76,192], [180,  4, 38]], [[ 55,133,232], [172,125, 24]],
            [[179, 88,  6], [ 84, 39,136]], [[ 22,135, 51], [193, 54, 59]],
            [[ 63,  0,125], [221,221,221]], [[  8, 48,107], [221,221,221]],
            [[ 30, 90,  0], [221,221,221]], [[180,  0, 30], [221,221,221]],
            [[103,  0, 13], [221,221,221]]]; 

func mshct(rgb1, rgb2, n, wht=, msh=, cmax=)
/* DOCUMENT cm = mshct(name)
 *       or cm = mshct(rgb1, rgb2)
 *       or mshct, name
 *       or mshct, rgb1, rgb2
 *   return diverging color map going from RGB1 to RGB2 through white.  If
 *   either color is too close to white, produces a sequential map between
 *   the two.  If one of RGB1 or RGB2 is nil, it is taken to be white.
 *   If the two colors are too close in hue, also just produces a
 *   sequential map.  The algorithm is from Kenneth Moreland,
 *   http://www.sandia.gov/~kmorel/documents/ColorMaps/ and references
 *   therein.  A few reasonable RGB1 and RGB2 values have been given names.
 *   The name "coolwarm" produces Moreland's recommended color table.
 *   Called as a subroutine, sets the current palette.
 * SEE ALSO: cmap, seqct, gmtct, gplct, gistct, idlct
 */
{
  if (structof(rgb1) == string) {
    /* some predefined diverging and sequential maps */
    list = where(rgb1 == msh_names);
    if (!numberof(list)) {
      if (!am_subroutine()) return [];
      error, "unknown mshct name "+rgb1;
    }
    rgb1 = msh_maps(,1,list(1));
    rgb2 = msh_maps(,2,list(1));
    msh = [];
  }

  /* pushing wht higher than about 93 gives white band */
  if (is_void(wht)) wht = 88.;
  if (msh) {
    msh1 = rgb1;  msh2 = rgb2;
  } else {
    msh1 = rgb2msh(rgb1);
    if (!is_void(rgb2)) msh2 = rgb2msh(rgb2);
  }
  if (is_void(msh2)) msh2 = [max(msh1(1), wht), 0., 0.];
  else if (is_void(msh1)) msh1 = [max(msh2(1), wht), 0., 0.];
  wht = max(msh1(1), msh2(1), wht);
  if (is_void(n)) n = cmap_ncolors();
  x = span(0., 1., abs(n));
  if (n < 0) {
    if (msh == 1) x = 2.*x(where(x < 0.5));
    else x = 2.*x(where(x >= 0.5)) - 1.;
  }
  s1 = msh1(2);
  s2 = msh2(2);
  dh = abs(msh1(3) - msh2(3)) % (2.*pi);
  dh = min(dh, 2.*pi-dh);
  if (s1>0.5 && s2>0.05 && dh>pi/3.) {
    /* if rgb1, rgb2 are saturated and distinct, put white at x=0.5
     * (otherwise, this is just a fragment going from rgb1 to rgb2)
     * neither recursion can take this branch
     */
    rgb1 = transpose(mshct(msh1, , -n, cmax=cmax, wht=wht, msh=1));
    rgb2 = transpose(mshct(, msh2, -n, cmax=cmax, wht=wht, msh=2));
    rgb = transpose(grow(rgb1, rgb2));
  } else {
    /* avoid perceptual kink near white */
    if (s1<0.5 && s2>0.05) msh1(3) = _adjust_hue(msh2, msh1(1));
    else if (s2<0.5 && s1>0.05) msh2(3) = _adjust_hue(msh1, msh2(1));
    /* then just interpolate polar coordinates
     * why isn't this broken for h1, h2 separated across 0,2*pi?
     */
    msh = x(..,-:1:3);
    x = x(*);
    msh(*,) = (1.-x)*msh1(-,) + x*msh2(-,);
    rgb = msh2rgb(msh, cmax=cmax);
  }
  if (!am_subroutine()) return rgb;
  cmap, rgb;
}

func rgb2msh(rgb, g, b)
{
  msh = rgb2lab(rgb, g, b);
  l = msh(..,1);  a = msh(..,2);  b = msh(..,3);  c = abs(b, a);
  msh(..,3) = atan(b, a+!c);
  msh(..,1) = a = abs(c, l);
  msh(..,2) = atan(c, l+!a);
  return msh;
}

func msh2rgb(msh, cmax=)
{
  s = msh(..,2);  h = msh(..,3);
  lab = double(msh);
  lab(..,1) = cos(s);
  lab(..,2:3) = sin(s) * [cos(h), sin(h)];
  return lab2rgb(msh(..,1)*lab, cmax=cmax);
}

func _adjust_hue(msh, mu)
{ /* only needs to work for scalar m,s,h and mu */
  m = msh(1);  s = msh(2);  h = msh(3);
  if (m >= mu) return h;
  hspin = s * sqrt(mu*mu - m*m) / (m * sin(s));
  return (h > -pi/3.)? h + hspin : h - hspin;  /* -pi/3 seems odd?? */
}

/* ------------------------------------------------------------------------ */
/* Sequential colormaps built on similar principles to Moreland */

/* named cm_seq map parameters are h (degrees), l1 (bright L), l2 (dark L)
 * where the L values can be a negative integer to use -L as the index
 * into seq_params of another named map for the L and saturation
 *
 * dblue, dred, dgreen are three matched maps carefully designed to
 *   be distinguishable to dichromats (colorblind people)
 */
seq_names = ["dblue", "dred", "dgreen"];
seq_params = [[228, -2, -3], [345, 90, -3], [65, -2, 20]];

/* lRGB hues in degrees, r=0, g=120, b=240
 * LMS color directions:
 *   Vischeck L=351.23, M=156.73, S=253.69   (180+156.73 = 336.73)
 *   Meyer    L=355.21, M=344.47, S=255.34   (344.47-180 = 164.47)
 * Dichromat simulator primaries:
 *                Vischeck         Meyer
 *   protan    227.99  49.13   228.92  47.02
 *   deuteran  227.99  49.13   221.03  34.76
 *   tritan    202.14 352.67   184.16   0.36
 */

func seqct(h, n, l1, l2, cmax=)
/* DOCUMENT cm = seqct(name)
 *       or cm = seqct(h, n, l1, l2)
 *       or seqct, h
 *   Return a sequential colormap based on hue H, running from lighter
 *   shades to darker shades in equal steps of luminance.  A few good
 *   choices of hue have names; you can also pass such a NAME.  The
 *   remaining arguments are optional: N is the number of colors to
 *   return, which defaults to the number in the currently installed
 *   colormap (usually 200).  L1 is the luminance for the lightest (first)
 *   shade, which defaults to 90, and L2 is the luminance of the darkest
 *   (last) shade, which defaults to 20.  L1 or L2 may also be RGB triples,
 *   which causes both the endpoint luminance and chroma to match that
 *   given color.  The hue H may be specified as an RGB triple, or as an
 *   angle in degrees in lRGB color space, with r=0, g=120, and b=240.
 *   The returned map is based on Luv luminance and hue; all points will
 *   have the same (u,v) hue, and therefore the same lRGB hue.  By default,
 *   the color will be maximally saturated at the endpoints, but you can
 *   desaturate it by specifying unsaturated RGB triples for L1 or L2.
 *   Typically, you pick L1 or L2 to be the endpoint of another seqct
 *   map you wish to match, or rgb_saturate on another hue.
 *   The colormap follows a second degree Bezier curve in lRGB space,
 *   with the control point between the endpoints located on the fully
 *   saturated edge of the RGB color cube, at the point with the given hue,
 *   guaranteeing the whole curve lies inside the cube.  The points are
 *   evenly spaced in luminance along this curve.
 *
 *   The named seqct maps are "dblue", "dred", and "dgreen".  These maps
 *   are matched to have equal luminance and comparable saturation, so that
 *   you can use them for situations in which you use all three maps
 *   in the same figure.  Furthermore, the three hues are carefully chosen
 *   to be distinguishable by dichromats (colorblind people).
 * SEE ALSO: cmap, mshct, gmtct, gplct, gistct, idlct
 */
{
  if (is_void(n)) n = cmap_ncolors();
  if (structof(h) == string) {
    list = where(h == seq_names);
    if (!numberof(list)) {
      if (!am_subroutine()) return [];
      error, "unknown seqct name "+h;
    }
    h = seq_params(,list(1));
    if (h(2) >= 0.) l1 = h(2);
    else l1 = rgb_saturate(seq_params(1,-h(2)), seq_params(2,-h(2)), cmax=1);
    if (h(3) >= 0.) l2 = h(3);
    else l2 = rgb_saturate(seq_params(1,-h(3)), seq_params(3,-h(3)), cmax=1);
    h = h(1);
  }
  rgb = _lrgbsat(cm_hue2lrgb(h))
  lmid = _lrgb2luv(rgb)(1);
  gmid = _luv2lrgb([lmid,0.,0.])(1);  /* lrgb gray of same luminance as rgb */

  s1 = _cm_get_sat(l1, 90.);
  s2 = _cm_get_sat(l2, 20.);
  if (l1<0 || l2>100 || abs(l1-l2)<20) error, "bad endpoint luminances";
  /* select gray levels with equal luminance steps */
  g = _luv2lrgb([span(l1, l2, n),0.,0.])(,1);
  g1 = g(1);  g2 = g(0);
  /* move endpoints to fully saturated colors of given hue */
  rgb1 = (g1<=gmid)? rgb*g1/gmid : 1.-(1.-rgb)*(1.-g1)/(1.-gmid);
  rgb2 = (g2<=gmid)? rgb*g2/gmid : 1.-(1.-rgb)*(1.-g2)/(1.-gmid);
  /* desaturate endpoints if requested */
  if (numberof(s2)) rgb2 = _cm_desat(rgb2, s2);
  if (numberof(s1)) rgb1 = _cm_desat(rgb1, s1);
  /* move midpoint to mean of endpoint luminances or lrgb grays
   * get brighter colors with mean gray if gmid not between g1 and g2,
   * otherwise with mean luminance
   * -- this switch leads to slight discontinuity as function of hue
   */
  if ((g1-gmid)*(gmid-g2)<0.) gm = 0.5*(g1 + g2);
  else gm = _luv2lrgb([0.5*(l1+l2),0.,0.])(1);
  mask = double(gmid >= gm);
  rgb = mask*gm/gmid*rgb + (1.-mask)*(1. - (1.-rgb)*(1.-gm)/(1.-gmid));
  gmid = gm;

  /* [g,g,g] is gray value for points in colormap
   * colormap is order 2 Bezier curve [rgb1,rgb,rgb2]
   * first find s Bezier parameter which gives yl
   */
  s = (g - g1) / (g2 - g1);
  sm = (gmid - g1) / (g2 - g1);
  s /= sm + sqrt(sm*sm + (1.-sm-sm)*s);
  s = s(-:1:3,);  t = 1. - s;
  rgb = rgb1*t*t + rgb*2.*t*s + rgb2*s*s;
  rgb = transpose(rgb_l2s(rgb, cmax=cmax));
  if (!am_subroutine()) return rgb;
  cmap, rgb;
}

func _lrgb2luv(rgb) { lrgb_skip=3; return rgb2luv(rgb); }
func _luv2lrgb(luv) { lrgb_skip=3; return luv2rgb(luv); }
func _lrgbsat(rgb) {
  rgb -= rgb(min);
  g = rgb(max);
  return rgb / (g + !g); /* on maximal saturation edge of cube */
}
func _cm_get_sat(&l, l0) {
  if (is_void(l)) l = l0;
  if (numberof(l) == 3) { s = rgb_s2l(l); l = rgb2luv(l)(1); }
  return s;
}
func _cm_desat(rgb, s) {
  luv = _lrgb2luv(rgb);  c0 = abs(luv(2), luv(3));
  s = _lrgb2luv(s);      s = abs(s(2), s(3));
  if (s > 1.01*c0) error, "L1 or L2 more saturated than max for this hue";
  else if (s >= c0) return rgb;
  return _luv2lrgb(luv * [c0,s,s]/c0);
}

func cm_hue2lrgb(h)
{
  if (numberof(h) == 3) {
    return rgb_s2l(h);
  } else {
    h *= pi/180.;  /* h is lRGB hue */
    return [2.,-1.,-1.]/sqrt(6.)*cos(h) + [0.,1.,-1.]/sqrt(2.)*sin(h);
  }
}

func rgb_saturate(rgb, l, &uvc, cmax=)
/* DOCUMENT rgbsat = rgb_saturate(rgb, l, uv)
 *          rgbsat = rgb_saturate(hue, l, uv)
 *   Return [r,g,b] with same Luv hue as RGB, saturated at luminosity L.
 *   Optionally return the uv chroma, sqrt(u^2+v^2) for this color.
 *   In the second form, HUE is the lRGB hue in degrees, lRGB=rgb_s2l(rgb),
 *   the angle in the lRGB color cube of the plane determined by lRGB
 *   and the gray diagonal of the cube.
 *   With the cmax= keyword, rgbsat is normalized to cmax.
 * SEE ALSO: seqct
 */
{
  gl = _luv2lrgb([l,0.,0.])(1);      /* lrgb gray of luminance l */
  rgb = _lrgbsat(cm_hue2lrgb(rgb));  /* hue of input rgb on edge of cube */
  g = _luv2lrgb([_lrgb2luv(rgb)(1),0.,0.])(1);
  mask = double(gl <= g);  /* luminance l below rgb */
  rgb = mask*gl/g*rgb + (1.-mask)*(1. - (1.-rgb)*(1.-gl)/(1.-g));
  luv = _lrgb2luv(rgb);
  uvc = abs(luv(2), luv(3));
  return rgb_l2s(rgb, cmax=cmax);
}

/* ------------------------------------------------------------------------ */
/* matplotlib colormaps, plus four additional names for gnuplot maps */

func mplct(name)
/* DOCUMENT cm = mplct(name)
 *       or mplct, name
 *   return color map (ct = "color table") specification for Matplotlib
 *   map NAME.  You may append "_r" to the name to reverse the colors.
 *   Called as a subroutine, sets the current palette.
 * SEE ALSO: cmap, mshct, seqct, gmtct, gplct, gistct, idlct
 */
{
  local n, rev;
  name = cmap_name(name, n, rev);
  i = where(name == mpl_names);
  if (!numberof(i)) error, name+" is not a known Matplotlib color map";
  p = *mpl_maps(i(1));
  if (!am_subroutine())
    return p;
  cmap, p;
}

mpl_names = ["binary", "gray", "cool", "bwr", "brg",
             "autumn", "spring", "summer", "winter",
             "seismic", "spectral", "coolwarm", "pink", "terrain",
             "hsv", "bone", "copper", "hot", "jet", "flag", "prism",
             "gnuplot", "gnuplot2", "ocean", "rainbow", "afmhot",
             "gnuplot3", "gnu_hot", "gnuplot4", "gnuplot5"];
/* gnuplot is traditional pm3d map, BkBlRdYl
 * gnuplot2 is gray-printable, BkBuViYlWt
 * following are all mentioned in gnuplot user manual,
 * gnuplot3 and later not in matplotlib
 */

/* matplotlib color maps begin with equally spaced color lists */
mpl_maps = [&[0xffffff,0x000000], &[0x000000,0xffffff], &[0x00ffff,0xff00ff],
            &[0x0000ff,0xffffff,0xff0000], &[0x0000ff, 0xff0000, 0x00ff00],
            &[0xff0000, 0xffff00], &[0xff00ff, 0xffff00],
            &[0x008066, 0xffff66], &[0x0000ff, 0x00ff80],
            &[0x00004d, 0x0000ff, 0xffffff, 0xff0000, 0x800000],
            &[0x000000,0x770088,0x880099,0x0000aa,0x0000dd,0x0077dd,0x0099dd,
              0x00aaaa,0x00aa88,0x009900,0x00bb00,0x00dd00,0x00ff00,0xbbff00,
              0xeeee00,0xffcc00,0xff9900,0xff0000,0xdd0000,0xcc0000,0xcccccc],
            &[0x1d2660,0x212d66,0x26336b,0x2b3a70,0x304074,0x364778,0x3b4d7b,
              0x40527d,0x46577e,0x4c5c7f,0x51607f,0x56647e,0x5c677c,0x616a7a,
              0x656c76,0x6a6d72,0x6e6e6e,0x726c68,0x756962,0x78665c,0x7a6256,
              0x7b5d50,0x7b5849,0x7b5243,0x7a4c3d,0x784637,0x753f31,0x72382b,
              0x6e3026,0x6a2721,0x651e1c,0x5f1417,0x590113],
            &[0x1e0000,0x321a1a,0x402525,0x4b2d2d,0x553434,0x5e3b3b,0x664040,
              0x6e4545,0x754a4a,0x7b4f4f,0x825353,0x885757,0x8d5b5b,0x935f5f,
              0x986262,0x9d6666,0xa26969,0xa76c6c,0xac6f6f,0xb07272,0xb57575,
              0xb97878,0xbd7b7b,0xc27e7e,0xc38481,0xc58a83,0xc79086,0xc99588,
              0xca9a8b,0xcc9f8d,0xcea490,0xcfa992,0xd1ae94,0xd3b297,0xd4b799,
              0xd6bb9b,0xd8bf9d,0xd9c3a0,0xdbc7a2,0xdccba4,0xdecfa6,0xdfd3a8,
              0xe1d7aa,0xe2daac,0xe4deae,0xe5e1b0,0xe7e5b2,0xe8e8b4,0xeaeab9,
              0xebebbf,0xededc4,0xeeeec9,0xf0f0ce,0xf1f1d3,0xf3f3d8,0xf4f4dd,
              0xf5f5e1,0xf7f7e6,0xf8f8ea,0xfafaee,0xfbfbf3,0xfcfcf7,0xfefefb,
              0xffffff],
            /* followed by piecewise linear unequally spaced color lists */
            &[[0,0x333399], [3,0x0099ff], [5,0x00cc66], [10,0xffff99],
              [15,0x805c54], [20,0xffffff]],
            &[[0, 0xff0000], [10,0xffef00], [11,0xf7ff00], [21,0x08ff00],
              [22,0x00ff10], [32,0x00ffff], [42,0x0010ff], [43,0x0800ff],
              [53,0xf700ff], [54,0xff00ef], [63,0xff0018]],
            /* followed by piecewise linear individual color components */
            &[&[[0,0], [47,47./72], [63,1]],
              &[[0,0], [23,23./72], [47,56./72], [63,1]],
              &[[0,0], [23,32./72], [63,1]]],
            &[&[[0,0], [17,1], [21,1]],
              &[[0,0], [21,482./617]],
              &[[0,0], [21,199./400]]],
            &[&[[0,26./625], [23,1], [63,1]],
              &[[0,0], [23,0], [47,1], [63,1]],
              &[[0,0], [47,0], [63,1]]],
            &[&[[0,0], [.35,0], [.66,1], [.89,1], [1,.5]],
              &[[0,0], [.125,0], [.375,1], [.64,1], [.91,0], [1,0]],
              &[[0,.5], [.11,1], [.34,1], [.65,0], [1,0]]],
            /* followed by cmap_nonlin individual color components */
            &[[[0.75, 31.5, 0.25, 0.5], [1, 31.5, 0, 0],
              [0.75, 31.5, -0.25, 0.5]]],
            &[[[0.75, 20.9, 0.25, 0.67], [0.75, 20.9, -0.25, 0.33],
              [-1.1, 20.9, 0, 0]]],
            /* followed by gnuplot color component enumerated functions */
            &[[7, 5, 15]], &[[30, 31, 32]], &[[23, 28, 3]], &[[33, 13, 10]],
            &[[34, 35, 36]], &[[3, 11, 6]], &[[21, 22, 23]], &[[21, 23, 3]],
            &[[3, 23, 21]]];

func cubehelix(n, gamma=, s=, r=, h=)
/* DOCUMENT cubehelix, n
 *       or cubehelix(n)
 *   Set cubehelix palette, designed by D.A. Green, adapted from matplotlib.
 *   Optional argument N is number of colors in palette, defaults to 200.
 *   If called as function, returns [r,g,b] without setting palette.
 *   Optional keyword arguments:
 *   Keyword     Description
 *   gamma       gamma factor to emphasise either low intensity values
 *               (gamma < 1), or high intensity values (gamma > 1);
 *               defaults to 1.0.
 *   s           the start color; defaults to 0.5 (i.e. purple).
 *   r           the number of r,g,b rotations in color that are made
 *               from the start to the end of the color scheme; defaults
 *               to -1.5 (i.e. -> B -> G -> R -> B).
 *   h           the hue parameter which controls how saturated the
 *               colors are. If this parameter is zero then the color
 *               scheme is purely a greyscale; defaults to 1.0.
 */
{
  if (is_void(n)) n = cmap_ncolors();
  if (is_void(gamma)) gamma = 1.0;
  if (is_void(s)) s = 0.5;
  if (is_void(r)) r = -1.5;
  if (is_void(h)) h = 1.0;
  x = span(0., 1., (n?n:200));
  rgb = [cubehelix_gcf(x, -0.14861, 1.78277),
         cubehelix_gcf(x, -0.29227, -0.90649),
         cubehelix_gcf(x, 1.97294, 0.0)];
  rgb = char(255*rgb);
  if (!am_subroutine()) return rgb;
  cmap, rgb;
}

func cubehelix_gcf(x, p0, p1)
{
  xg = x^gamma;
  a = h * xg * (1.-xg) / 2.;
  phi = 2.*pi * (s/3. + r*x);
  return xg + a * (p0*cos(phi) + p1*sin(phi));
}

/* ------------------------------------------------------------------------ */
/* gnuplot colormaps specified by rgbformulae */

func gplct(r, g, b)
/* DOCUMENT cm = gplct(r, g, b)
 *       or gplct, r, g, b
 *   return color map (ct = "color table") specification for gnuplot
 *   function indices (R,G,B), suitable to pass to cmap.  This is
 *   equivalent to:
 *     cm = [[r, g, b]]
 *   Each of R, G, B must be between 0 and 36 inclusive.  A negative
 *   number reverses the sense of the function.
 *   Called as a subroutine, sets the current palette.
 * SEE ALSO: cmap, gmtct, mplct, gistct, idlct
 */
{
  rgb = [[r, g, b]];
  if (max(abs(rgb)) > 36)
    error, "known gnuplot colormap functions are 0 through 36";
  if (!am_subroutine()) return rgb;
  cmap, rgb;
}

func gnu_map(r, g, b, n)
{
  if (is_void(n)) n = cmap_ncolors();
  x = span(0., 1., n);
  return [gnu_func(r,x), gnu_func(g,x), gnu_func(b,x)];
}

func gnu_func(n, x)
{
  rev = (n < 0);
  if (rev) n = -n;
  if (n > 36) return [];
  p = *_gnu_funcs(n+1);
  if (rev) x = 1. - x;
  return ((dimsof(p)(1) == 2)? cmap_interp : cmap_nonlin)(p, x);
}

_gnu_funcs = [&[[0,0], [1,0]], &[[0,0.5], [1,0.5]], &[[0,1], [1,1]],
              &[[0,0], [1,1]], &[1, 0, 2, 0], &[1, 0, 3, 0], &[1, 0, 4, 0],
              &[1, 0, 0.5, 0], &[1, 0, 0.25, 0],
              &[1, 0.5, 0, 0], &[1, 0.5, 0.5, 0],
              &[[0,0.5], [1,0], [2,0.5]], &[0.],
              &[1., 1, 0, 0], &[-1, -1, -0.5, 0],
              &[1., 2, 0, 0], &[1, 2, 0.5, 0],
              &[-1., -2, 0, 0], &[-1, -2, -0.5, 0],
              &[-1., -4, 0, 0], &[-1, -4, -0.5, 0],
              &[[0,0], [1,1], [3,1]], &[[0,0], [1,0], [2,1], [3,1]],
              &[[0,0], [2,0], [3,1]], &[[0,1], [1,0], [2,1], [3,1]],
              &[[0,1], [1,1], [2,0], [3,1]],
              &[[0,0], [1,0], [3,1]], &[[0,0], [2,0], [3,0.5]],
              &[[0,0.5], [1,0], [3,1]], &[[0,1], [2,0], [3,0.5]],
              &[[0,0], [25,0], [57,1], [100,1]],
              &[[0,0], [42,0], [92,1], [100,1]],
              &[[0,0], [25,1], [42,1], [92,0], [100,1]],
              &[[0,0.5], [1,0], [3,1], [4,1]], &[[0,0], [1,1], [2,1]],
              &[[0,0], [1,0], [3,1], [4,1]], &[[0,0], [1,0], [2,1]]];

/* ------------------------------------------------------------------------ */
/* The following colors and names are repackaged from GMT,
 * Generic Mapping Tools, distributed under the following license:
 *
 * Copyright (c) 1991-2013, P. Wessel & W. H. F. Smith
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 or any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Contact info: http://gmt.soest.hawaii.edu
 *
 * The palette files are in the share/cpt/ subdirectory of the
 * GMT source distribution.
 */

func gmtct(name)
/* DOCUMENT cm = gmtct(name)
 *       or gmtct, name
 *   return color map (ct = "color table") specification for GMT
 *   map NAME.  You may append "_r" to the name to reverse the colors.
 *   Called as a subroutine, sets the current palette.
 *   Note that the cmap_rd function can read GMT color map files.
 * SEE ALSO: cmap, mplct, gplct, gistct, cmap_rd
 */
{
  local n, rev;
  name = cmap_name(name, n, rev);
  i = where(name == gmt_names);
  if (!numberof(i)) error, name+" is not a known GMT color map";
  p = *gmt_maps(i(1));
  flags = gmt_flags(i(1));
  if (flags == 1) hist = 1;
  else if (flags == 2) hsv = 1;
  if (!am_subroutine())
    return (hist || hsv)? save(p=p, hsv=hsv, hist=hist) : p;
  cmap, p, hist=hist, hsv=hsv;
}

gmt_names = ["cool","copper","cyclic","drywet","gebco","globe","gray",
             "haxby","hot","jet","nighttime","no_green","ocean","paired",
             "panoply","polar","rainbow","red2green","relief","sealand",
             "seis","split","topo","wysiwyg"];

/* flags is 1 for hist=1, 2 for hsv=1 */
gmt_flags = [0,0,2,0,0,0,0,1,0,0,2,1,0,1,1,0,2,0,0,2,0,0,2,1];

gmt_maps = [&[0x00ffff,0xff00ff], &[[0,0x000000],[51,0xff9f66],[64,0xffc880]],
            &[[0.,1,1],[360,1,1]],
            &[0x86612a,0xeec764,0xb4ee87,0x32eeeb,0x0c78ee,0x2601b7,0x083371],
            &[[0,0x00f0ff],[10,0x00f0ff],[10,0x23ffff],[20,0x23ffff],
              [20,0x5affff],[30,0x5affff],[30,0x8cffe6],[40,0x8cffe6],
              [40,0xa5ffd7],[50,0xa5ffd7],[50,0xc3ffd7],[60,0xc3ffd7],
              [60,0xd2ffd7],[65,0xd2ffd7],[65,0xe6fff0],[68,0xe6fff0],
              [68,0xebffff],[70,0xebffff]],
            &[[-100,0x9900ff],[-95,0x9900ff],[-90,0x9900ff],[-85,0x9900ff],
              [-85,0x8811ff],[-80,0x8811ff],[-80,0x7722ff],[-75,0x7722ff],
              [-75,0x6633ff],[-70,0x6633ff],[-70,0x5544ff],[-65,0x5544ff],
              [-65,0x4455ff],[-60,0x4455ff],[-60,0x3366ff],[-55,0x3366ff],
              [-55,0x2277ff],[-50,0x2277ff],[-50,0x1188ff],[-45,0x1188ff],
              [-45,0x0099ff],[-40,0x0099ff],[-40,0x1ba4ff],[-35,0x1ba4ff],
              [-35,0x36afff],[-30,0x36afff],[-30,0x51baff],[-25,0x51baff],
              [-25,0x6cc5ff],[-20,0x6cc5ff],[-20,0x86d0ff],[-15,0x86d0ff],
              [-15,0xa1dbff],[-10,0xa1dbff],[-10,0xbce6ff],[-5,0xbce6ff],
              [-5,0xd7f1ff],[-2,0xd7f1ff],[-2,0xf1fcff],[0,0xf1fcff],
              [0,0x336600],[1,0x33cc66],[1,0x33cc66],[2,0xbbe492],
              [2,0xbbe492],[5,0xffdcb9],[5,0xffdcb9],[10,0xf3ca89],
              [10,0xf3ca89],[15,0xe6b858],[15,0xe6b858],[20,0xd9a627],
              [20,0xd9a627],[25,0xa89a1f],[25,0xa89a1f],[30,0xa49019],
              [30,0xa49019],[35,0xa28613],[35,0xa28613],[40,0x9f7b0d],
              [40,0x9f7b0d],[45,0x9c7107],[45,0x9c7107],[50,0x996600],
              [50,0x996600],[55,0xa25959],[55,0xa25959],[60,0xb27676],
              [60,0xb27676],[65,0xb79393],[65,0xb79393],[70,0xc2b0b0],
              [70,0xc2b0b0],[75,0xcccccc],[75,0xcccccc],[80,0xe5e5e5],
              [80,0xe5e5e5],[85,0xf2f2f2],[85,0xf2f2f2],[90,0xffffff],
              [90,0xffffff],[95,0xffffff],[95,0xffffff],[100,0xffffff]],
            &[0x000000,0xffffff],
            &[0x0a0079,0x280096,0x1405af,0x000ac8,0x0019d4,0x0028e0,
              0x1a66f0,0x0d81f8,0x19afff,0x32beff,0x44caff,0x61e1f0,0x6aebe1,
              0x7cebc8,0x8aecae,0xacf5a8,0xcdffa2,0xdff58d,0xf0ec79,0xf7d768,
              0xffbd57,0xffa045,0xf4754b,0xee504e,0xff5a5a,0xff7c7c,0xff9e9e,
              0xf5b3ae,0xffc4c4,0xffd7d7,0xffebeb,0xffffff],
            &[[0,0x000000],[3,0xff0000],[6,0xffff00],[8,0xffffff]],
            &[[0,0x00007f],[1,0x0000ff],[3,0x00ffff],[5,0xffff7f],
              [7,0xff0000],[8,0x7f0000]],
            &[[0,260,1,0.1],[1,195,0.55,0.55],[1,65,0.55,0.55],[2,0,0.1,1]],
            &[0x2060ff,0x209fff,0x20bfff,0x00cfff,0x2affff,0x55ffff,0x7fffff,
              0xaaffff,0xffff54,0xfff000,0xffbf00,0xffa800,0xff8a00,0xff7000,
              0xff4d00,0xff0000],
            &[0x000000,0x000519,0x000a32,0x00507d,0x0096c8,0x56c5b8,0xacf5a8,
              0xd3fad3,0xfaffff],
            &[0xa6cee3,0x1f78b4,0xb2df8a,0x33a02c,0xfb9a99,0xe31a1c,0xfdbf6f,
              0xff7f00,0xcab2d6,0x6a3d9a,0xffff99,0xb15928],
            &[0x040ed8,0x2050ff,0x4196ff,0x6dc1ff,0x86d9ff,0x9ceeff,0xaff5ff,
              0xceffff,0xfffe47,0xffeb00,0xffc400,0xff9000,0xff4800,0xff0000,
              0xd50000,0x9e0000],
            &[0x0000ff,0xffffff,0xff0000], &[[300.,1,1],[0,1,1]],
            &[0xff0000,0xffffff,0x00ff00],
            &[[-80,0x000000],[-70,0x000519],[-60,0x000a32],[-50,0x00507d],
              [-40,0x0096c8],[-30,0x56c5b8],[-20,0xacf5a8],[-10,0xd3fad3],
              [0,0xfaffff],[0,0x467832],[5,0x786432],[10,0x927e3c],
              [20,0xc6b250],[30,0xfae664],[30,0xfae664],[40,0xfaea7e],
              [40,0xfaea7e],[50,0xfcee98],[50,0xfcee98],[60,0xfcf3b1],
              [60,0xfcf3b1],[70,0xfdf9d8],[70,0xfdf9d8],[80,0xffffff]],
            &[[0,255,0.6,1],[1,240,0.6,1],[2,225,0.6,1],[3,210,0.6,1],
              [4,195,0.6,1],[5,180,0.6,1],[6,165,0.6,1],[7,150,0.6,1],
              [8,135,0.6,1],[9,120,0.6,1],[10,105,0.6,1],[11,90,0.6,1],
              [12,75,0.6,1],[12,60,0.35,1],[13,40,0.35,1],[13,40,0.35,1],
              [14,20,0.35,1],[14,20,0.35,1],[15,0,0.35,1],[15,360,0.35,1],
              [16,345,0.3,1],[16,345,0.3,1],[17,330,0.25,1],[17,330,0.25,1],
              [18,315,0.2,1]],
            &[0xaa0000,0xff0000,0xff5500,0xffaa00,0xffff00,0xffff00,0x5aff1e,
              0x00f06e,0x0050ff,0x0000cd],
            &[0x8080ff,0x000080,0x000000,0x800000,0xff8080],
            &[[-70,290,0.45,0.85],[-65,265,0.45,0.85],[-65,265,0.4,0.9],
              [-60,240,0.4,0.9],[-55,220,0.4,0.9],[-50,199,0.4,0.9],
              [-45,175,0.4,0.95],[-40,150,0.45,0.95],[-35,125,0.45,0.95],
              [-30,99,0.45,0.95],[-25,75,0.45,0.95],[-20,50,0.45,0.95],
              [-15,25,0.45,0.95],[-5,10,0.35,0.85],[-5,0,0.25,0.85],
              [0,0,0.25,0.8],[0,195,0.35,0.7],[2,160,0.4,0.7],
              [2,160,0.4,0.7],[4,125,0.45,0.7],[4,125,0.45,0.7],
              [6,99,0.45,0.8],[6,99,0.45,0.8],[10,75,0.45,0.8],
              [10,75,0.45,0.8],[15,50,0.35,0.9],[15,50,0.35,0.9],
              [35,25,0.1,1],[35,25,0.05,1],[70,0,0,1]],
            &[0x400040,0x4000c0,0x0040ff,0x0080ff,0x00a0ff,0x40c0ff,0x40e0ff,
              0x40ffff,0x40ffc0,0x40ff40,0x80ff40,0xc0ff40,0xffff40,0xffe040,
              0xffa040,0xff6040,0xff2040,0xff60c0,0xffa0ff,0xffe0ff]];

/* ------------------------------------------------------------------------ */
/* The following colors and names are repackaged from a spreadsheet
 * obtained from http://colorbrewer.org bearing the following
 * open source license:
 *
 * Copyright (c) 2002 Cynthia Brewer, Mark Harrower,
 *                    and The Pennsylvania State University.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you
 * may not use this file except in compliance with the License.  You may
 * obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */

/* Unlike the other color maps defined in this file, these sets of
 * colors are carefully selected individually.  They are intended to
 * be used to color maps with a small number of colors (12 or less).
 * Each name corresponds to a family of these sets with increasing
 * numbers of colors.
 *
 * By default, yorick palettes all have 200 colors, usually varying
 * smoothly, intended to represent continuous variables.  By default,
 * therefore, we interpret the ColorBrewer palette names as the
 * largest set of colors availble with that name, and regard these
 * as equally spaced, linearly interpolating the r, g, b components
 * to fill in the 200 (or other large number) of smoothly varying
 * colors.  This interpolation works very well for the sequential
 * and diverging ColorBrewer sets, but is a bit odd for the qualitative
 * sets, which are specifically chosen not to appear in any natural
 * sequence.
 *
 * You can specify a "histogram" colormap with cmap, which will
 * produce a palette having only the specific colors in the ColorBrewer
 * color sets.  To do that, append "_N" to the name, where N is the
 * number of distinct colors you want.  Thus, "Blues" will give the
 * smoothly interpolated table, while "Blues_7" gives a palette with
 * seven steps corresponding exactly to the 7 level ColorBrewer Blues.
 */

cb_names = ["Accent","Blues","BrBG","BuGn","BuPu","Dark2","GnBu","Greens",
            "Greys","Oranges","OrRd","Paired","Pastel1","Pastel2","PiYG",
            "PRGn","PuBu","PuBuGn","PuOr","PuRd","Purples","RdBu","RdGy",
            "RdPu","Reds","RdYlBu","RdYlGn","Set1","Set2","Set3","Spectral",
            "YlGn","YlGnBu","YlOrBr","YlOrRd"];

/* 1=sequential, 2=divergent, 3=qualitative */
cb_types = [3,1,2,1,1,3,1,1,1,1,1,3,3,3,2,2,1,1,2,1,1,2,2,1,1,2,2,3,3,3,2,
            1,1,1,1];

/* here are the 379 unique ColorBrewer colors as 0xrrggbb */
cb_colors =
  [0x000000,0x003c30,0x00441b,0x004529,0x005824,0x005a32,0x006837,0x006d2c,
   0x008837,0x014636,0x016450,0x01665e,0x016c59,0x018571,0x023858,0x02818a,
   0x034e7b,0x045a8d,0x053061,0x0570b0,0x0571b0,0x081d58,0x08306b,0x084081,
   0x084594,0x08519c,0x08589e,0x0868ac,0x0c2c84,0x1a1a1a,0x1a9641,0x1a9850,
   0x1b7837,0x1b9e77,0x1c9099,0x1d91c0,0x1f78b4,0x2166ac,0x2171b5,0x225ea8,
   0x238443,0x238b45,0x252525,0x253494,0x276419,0x2b83ba,0x2b8cbe,0x2c7bb6,
   0x2c7fb8,0x2ca25f,0x2d004b,0x313695,0x3182bd,0x31a354,0x3288bd,0x33a02c,
   0x35978f,0x3690c0,0x377eb8,0x386cb0,0x3f007d,0x40004b,0x404040,0x41ab5d,
   0x41ae76,0x41b6c4,0x4292c6,0x4393c3,0x43a2ca,0x4575b4,0x49006a,0x4a1486,
   0x4d004b,0x4d4d4d,0x4d9221,0x4dac26,0x4daf4a,0x4eb3d3,0x525252,0x542788,
   0x54278f,0x543005,0x5aae61,0x5ab4ac,0x5e3c99,0x5e4fa2,0x636363,0x662506,
   0x666666,0x66a61e,0x66bd63,0x66c2a4,0x66c2a5,0x67000d,0x67001f,0x67a9cf,
   0x6a3d9a,0x6a51a3,0x6baed6,0x6e016b,0x737373,0x74a9cf,0x74add1,0x74c476,
   0x756bb1,0x7570b3,0x762a83,0x78c679,0x7a0177,0x7b3294,0x7bccc4,0x7f0000,
   0x7f2704,0x7f3b08,0x7fbc41,0x7fbf7b,0x7fc97f,0x7fcdbb,0x800026,0x8073ac,
   0x807dba,0x80b1d3,0x80cdc1,0x810f7c,0x878787,0x88419d,0x8856a7,0x8c2d04,
   0x8c510a,0x8c6bb1,0x8c96c6,0x8da0cb,0x8dd3c7,0x8e0152,0x91003f,0x91bfdb,
   0x91cf60,0x92c5de,0x969696,0x980043,0x984ea3,0x990000,0x99000d,0x993404,
   0x9970ab,0x998ec3,0x999999,0x99d594,0x99d8c9,0x9e0142,0x9e9ac8,0x9ebcda,
   0x9ecae1,0xa1d76a,0xa1d99b,0xa1dab4,0xa50026,0xa50f15,0xa63603,0xa65628,
   0xa6611a,0xa6761d,0xa6bddb,0xa6cee3,0xa6d854,0xa6d96a,0xa6dba0,0xa8ddb5,
   0xabd9e9,0xabdda4,0xaddd8e,0xae017e,0xaf8dc3,0xb10026,0xb15928,0xb2182b,
   0xb2abd2,0xb2df8a,0xb2e2e2,0xb30000,0xb35806,0xb3b3b3,0xb3cde3,0xb3de69,
   0xb3e2cd,0xb8e186,0xbababa,0xbae4b3,0xbae4bc,0xbc80bd,0xbcbddc,0xbd0026,
   0xbdbdbd,0xbdc9e1,0xbdd7e7,0xbeaed4,0xbebada,0xbf5b17,0xbf812d,0xbfd3e6,
   0xc2a5cf,0xc2e699,0xc51b7d,0xc51b8a,0xc6dbef,0xc7e9b4,0xc7e9c0,0xc7eae5,
   0xc994c7,0xca0020,0xcab2d6,0xcb181d,0xcbc9e2,0xcbd5e8,0xcc4c02,0xcccccc,
   0xccebc5,0xccece6,0xce1256,0xd01c8b,0xd0d1e6,0xd1e5f0,0xd4b9da,0xd53e4f,
   0xd6604d,0xd7191c,0xd7301f,0xd73027,0xd7b5d8,0xd8b365,0xd8daeb,0xd94701,
   0xd94801,0xd95f02,0xd95f0e,0xd9d9d9,0xd9ef8b,0xd9f0a3,0xd9f0d3,0xdadaeb,
   0xdd1c77,0xdd3497,0xde2d26,0xde77ae,0xdecbe4,0xdeebf7,0xdf65b0,0xdfc27d,
   0xe08214,0xe0e0e0,0xe0ecf4,0xe0f3db,0xe0f3f8,0xe31a1c,0xe34a33,0xe41a1c,
   0xe5c494,0xe5d8bd,0xe5f5e0,0xe5f5f9,0xe6550d,0xe66101,0xe6ab02,0xe6f598,
   0xe6f5c9,0xe6f5d0,0xe7298a,0xe78ac3,0xe7d4e8,0xe7e1ef,0xe9a3c9,0xec7014,
   0xece2f0,0xece7f2,0xedf8b1,0xedf8e9,0xedf8fb,0xef3b2c,0xef6548,0xef8a62,
   0xefedf5,0xeff3ff,0xf0027f,0xf03b20,0xf0f0f0,0xf0f9e8,0xf16913,0xf1a340,
   0xf1b6da,0xf1e2cc,0xf1eef6,0xf2f0f7,0xf2f2f2,0xf46d43,0xf4a582,0xf4cae4,
   0xf5f5f5,0xf6e8c3,0xf6eff7,0xf768a1,0xf781bf,0xf7f4f9,0xf7f7f7,0xf7fbff,
   0xf7fcb9,0xf7fcf0,0xf7fcf5,0xf7fcfd,0xfa9fb5,0xfb6a4a,0xfb8072,0xfb9a99,
   0xfbb4ae,0xfbb4b9,0xfc4e2a,0xfc8d59,0xfc8d62,0xfc9272,0xfcae91,0xfcbba1,
   0xfcc5c0,0xfccde5,0xfcfbfd,0xfd8d3c,0xfdae61,0xfdae6b,0xfdb462,0xfdb863,
   0xfdbb84,0xfdbe85,0xfdbf6f,0xfdc086,0xfdcc8a,0xfdcdac,0xfdd0a2,0xfdd49e,
   0xfddaec,0xfddbc7,0xfde0dd,0xfde0ef,0xfe9929,0xfeb24c,0xfec44f,0xfecc5c,
   0xfed976,0xfed98e,0xfed9a6,0xfee08b,0xfee090,0xfee0b6,0xfee0d2,0xfee391,
   0xfee5d9,0xfee6ce,0xfee8c8,0xfeebe2,0xfeedde,0xfef0d9,0xff7f00,0xffd92f,
   0xffed6f,0xffeda0,0xfff2ae,0xfff5eb,0xfff5f0,0xfff7bc,0xfff7ec,0xfff7f3,
   0xfff7fb,0xffff33,0xffff99,0xffffb2,0xffffb3,0xffffbf,0xffffcc,0xffffd4,
   0xffffd9,0xffffe5,0xffffff];

/* indices into cb_colors for each color map */
cb_maps =
  [&[&[117,196,332],&[117,196,332,371],&[117,196,332,371,60],
     &[117,196,332,371,60,283],&[117,196,332,371,60,283,198],
     &[117,196,332,371,60,283,198,89]],
   &[&[246,153,53],&[282,195,99,39],&[282,195,99,53,26],
     &[282,205,153,99,53,26],&[282,205,153,99,67,39,25],
     &[304,246,205,153,99,67,39,25],&[304,246,205,153,99,67,39,26,23]],
   &[&[230,297,84],&[161,248,123,14],&[161,248,297,123,14],
     &[129,230,298,208,84,12],&[129,230,298,297,208,84,12],
     &[129,199,248,298,208,123,57,12],&[129,199,248,298,297,208,123,57,12],
     &[82,129,199,248,298,208,123,57,12,2],
     &[82,129,199,248,298,297,208,123,57,12,2]],
   &[&[260,149,50],&[277,179,92,42],&[277,179,92,50,8],&[277,218,149,92,50,8],
    &[277,218,149,92,65,42,5],&[308,260,218,149,92,65,42,5],
    &[308,260,218,149,92,65,42,8,3]],
   &[&[251,152,127],&[277,183,131,126],&[277,183,131,127,124],
    &[277,200,152,131,127,124],&[277,200,152,131,130,126,100],
    &[308,251,200,152,131,130,126,100],&[308,251,200,152,131,130,126,124,73]],
   &[&[34,234,106],&[34,234,106,267],&[34,234,106,267,90],
    &[34,234,106,267,90,263],&[34,234,106,267,90,263,162],
    &[34,234,106,267,90,263,162,89]],
   &[&[252,168,69],&[286,189,111,47],&[286,189,111,69,28],
    &[286,217,168,111,69,28],&[286,217,168,111,78,47,27],
    &[306,252,217,168,111,78,47,27],&[306,252,217,168,111,78,47,28,24]],
   &[&[259,155,54],&[276,188,104,42],&[276,188,104,54,8],
    &[276,207,155,104,54,8],&[276,207,155,104,64,42,6],
    &[307,259,207,155,104,64,42,6],&[307,259,207,155,104,64,42,8,3]],
   &[&[285,193,87],&[303,216,139,79],&[303,216,139,87,43],
    &[303,236,193,139,87,43],&[303,236,193,139,101,79,43],
    &[379,285,236,193,139,101,79,43],&[379,285,236,193,139,101,79,43,1]],
   &[&[354,326,261],&[357,330,324,232],&[357,330,324,261,159],
    &[357,335,326,324,261,159],&[357,335,326,324,287,233,128],
    &[364,354,335,326,324,287,233,128],&[364,354,335,326,324,287,233,159,113]],
   &[&[355,329,255],&[358,333,316,227],&[358,333,316,255,180],
    &[358,336,329,316,255,180],&[358,336,329,316,279,227,142],
    &[367,355,336,329,316,279,227,142],&[367,355,336,329,316,279,227,180,112]],
   &[&[164,37,178],&[164,37,178,56],&[164,37,178,56,312],
    &[164,37,178,56,312,254],&[164,37,178,56,312,254,331],
    &[164,37,178,56,312,254,331,359],&[164,37,178,56,312,254,331,359,211],
    &[164,37,178,56,312,254,331,359,211,97],
    &[164,37,178,56,312,254,331,359,211,97,371],
    &[164,37,178,56,312,254,331,359,211,97,371,175]],
   &[&[313,183,217],&[313,183,217,245],&[313,183,217,245,347],
    &[313,183,217,245,347,375],&[313,183,217,245,347,375,258],
    &[313,183,217,245,347,375,258,337],&[313,183,217,245,347,375,258,337,293]],
   &[&[185,334,214],&[185,334,214,296],&[185,334,214,296,265],
    &[185,334,214,296,265,363],&[185,334,214,296,265,363,290],
    &[185,334,214,296,265,363,290,216]],
   &[&[271,303,154],&[220,289,186,76],&[220,289,303,186,76],
    &[203,271,340,266,154,75],&[203,271,340,303,266,154,75],
    &[203,244,289,340,266,186,115,75],&[203,244,289,340,303,266,186,115,75],
    &[134,203,244,289,340,266,186,115,75,45],
    &[134,203,244,289,340,303,266,186,115,75,45]],
   &[&[173,303,116],&[110,201,167,9],&[110,201,303,167,9],
    &[107,173,269,239,116,33],&[107,173,269,303,239,116,33],
    &[107,145,201,269,239,167,83,33],&[107,145,201,269,303,239,167,83,33],
    &[62,107,145,201,269,239,167,83,33,3],
    &[62,107,145,201,269,303,239,167,83,33,3]],
   &[&[274,163,47],&[291,194,102,20],&[291,194,102,47,18],
    &[291,221,163,102,47,18],&[291,221,163,102,58,20,17],
    &[369,274,221,163,102,58,20,17],&[369,274,221,163,102,58,20,18,15]],
   &[&[273,163,35],&[299,194,96,16],&[299,194,96,35,13],
    &[299,221,163,96,35,13],&[299,221,163,96,58,16,11],
    &[369,273,221,163,96,58,16,11],&[369,273,221,163,96,58,16,13,10]],
   &[&[288,303,146],&[262,328,177,85],&[262,328,303,177,85],
    &[181,288,350,231,146,80],&[181,288,350,303,231,146,80],
    &[181,249,328,350,231,177,120,80],&[181,249,328,350,303,231,177,120,80],
    &[114,181,249,328,350,231,177,120,80,51],
    &[114,181,249,328,350,303,231,177,120,80,51]],
   &[&[270,209,241],&[291,229,247,219],&[291,229,247,241,140],
    &[291,223,209,247,241,140],&[291,223,209,247,267,219,135],
    &[302,270,223,209,247,267,219,135],&[302,270,223,209,247,267,219,140,95]],
   &[&[281,191,105],&[292,213,151,98],&[292,213,151,105,81],
    &[292,240,191,151,105,81],&[292,240,191,151,121,98,72],
    &[323,281,240,191,151,121,98,72],&[323,281,240,191,151,121,98,81,61]],
   &[&[280,303,96],&[210,295,138,21],&[210,295,303,138,21],
    &[176,280,338,222,96,38],&[176,280,338,303,222,96,38],
    &[176,225,295,338,222,138,68,38],&[176,225,295,338,303,222,138,68,38],
    &[95,176,225,295,338,222,138,68,38,19],
    &[95,176,225,295,338,303,222,138,68,38,19]],
   &[&[280,379,147],&[210,295,187,63],&[210,295,379,187,63],
    &[176,280,338,250,147,74],&[176,280,338,379,250,147,74],
    &[176,225,295,338,250,187,125,74],&[176,225,295,338,379,250,187,125,74],
    &[95,176,225,295,338,250,187,125,74,30],
    &[95,176,225,295,338,379,250,187,125,74,30]],
   &[&[339,309,204],&[356,314,300,172],&[356,314,300,204,109],
    &[356,321,309,300,204,109],&[356,321,309,300,242,172,109],
    &[368,339,321,309,300,242,172,109],&[368,339,321,309,300,242,172,109,71]],
   &[&[351,318,243],&[353,319,310,212],&[353,319,310,243,158],
    &[353,320,318,310,243,158],&[353,320,318,310,278,212,143],
    &[365,351,320,318,310,278,212,143],&[365,351,320,318,310,278,212,158,94]],
   &[&[316,374,136],&[226,325,169,48],&[226,325,374,169,48],
    &[228,316,349,253,136,70],&[228,316,349,374,253,136,70],
    &[228,294,325,349,253,169,103,70],&[228,294,325,349,374,253,169,103,70],
    &[157,228,294,325,349,253,169,103,70,52],
    &[157,228,294,325,349,374,253,169,103,70,52]],
   &[&[316,374,137],&[226,325,166,31],&[226,325,374,166,31],
    &[228,316,348,237,137,32],&[228,316,348,374,237,137,32],
    &[228,294,325,348,237,166,91,32],&[228,294,325,348,374,237,166,91,32],
    &[157,228,294,325,348,237,166,91,32,7],
    &[157,228,294,325,348,374,237,166,91,32,7]],
   &[&[256,59,77],&[256,59,77,141],&[256,59,77,141,359],
    &[256,59,77,141,359,370],&[256,59,77,141,359,370,160],
    &[256,59,77,141,359,370,160,301],&[256,59,77,141,359,370,160,301,147]],
   &[&[93,317,132],&[93,317,132,268],&[93,317,132,268,165],
    &[93,317,132,268,165,360],&[93,317,132,268,165,360,257],
    &[93,317,132,268,165,360,257,182]],
   &[&[133,373,197],&[133,373,197,311],&[133,373,197,311,122],
    &[133,373,197,311,122,327],&[133,373,197,311,122,327,184],
    &[133,373,197,311,122,327,184,322],&[133,373,197,311,122,327,184,322,236],
    &[133,373,197,311,122,327,184,322,236,190],
    &[133,373,197,311,122,327,184,322,236,190,217],
    &[133,373,197,311,122,327,184,322,236,190,217,361]],
   &[&[316,374,148],&[226,325,170,46],&[226,325,374,170,46],
    &[224,316,348,264,148,55],&[224,316,348,374,264,148,55],
    &[224,294,325,348,264,170,93,55],&[224,294,325,348,374,264,170,93,55],
    &[150,224,294,325,348,264,170,93,55,86],
    &[150,224,294,325,348,374,264,170,93,55,86]],
   &[&[305,171,54],&[375,202,108,41],&[375,202,108,54,7],
    &[375,238,171,108,54,7],&[375,238,171,108,64,41,6],
    &[378,305,238,171,108,64,41,6],&[378,305,238,171,108,64,41,7,4]],
   &[&[275,118,49],&[375,156,66,40],&[375,156,66,49,44],
    &[375,206,118,66,49,44],&[375,206,118,66,36,40,29],
    &[377,275,206,118,66,36,40,29],&[377,275,206,118,66,36,40,44,22]],
   &[&[366,343,235],&[376,346,341,215],&[376,346,341,235,144],
    &[376,352,343,341,235,144],&[376,352,343,341,272,215,128],
    &[378,366,352,343,341,272,215,128],&[378,366,352,343,341,272,215,144,88]],
   &[&[362,342,284],&[372,344,324,254],&[372,344,324,284,192],
    &[372,345,342,324,284,192],&[372,345,342,324,315,254,174],
    &[375,362,345,342,324,315,254,174],&[375,362,345,342,324,315,254,192,119]]];

func cb_map(name, nlevs, &type)
{
  if (structof(name) == string) {
    maps = where(strcase(0,name) == strcase(0,cb_names));
    if (numberof(maps) != 1) return [];
    m = maps(1);
  } else {
    m = name;
    if (m<1 || m>numberof(cb_maps)) return [];
  }
  eq_nocopy, maps, *cb_maps(m);
  type = cb_types(m);
  if (!nlevs) return cb_colors(*maps(0));
  for (i=1 ; i<=numberof(maps) ; ++i) if (nlevs == numberof(*maps(i))) break;
  if (i > numberof(maps))
    error, cb_names(m)+" has no maps with "+totxt(nlevs)+" levels";
  return cb_colors(*maps(i));
}

func cb_choices(n, what)
/* DOCUMENT cb_choices, n, what
 *       or cb_choices, name
 *   list available ColorBrewer color map choices with N colors.  The
 *   WHAT argument is "sequential", "diverging", or "qualitative", which
 *   can be abbreviated to any smaller number of characters.  The N and
 *   WHAT arguments are both optional, and may occur in either order.
 *   Called as a function, cb_choices returns a list of palette names;
 *   if the N argument is specified, "_N" will be appended to the
 *   returned names.
 *   In the second form, prints (or displays) a list of the different
 *   numbers of colors available for color map NAME.
 */
{
  if (structof(n) == string) {
    tmp = n;
    n = what;
    what = tmp;
  }
  list = msort(cb_types, cb_names);
  names = cb_names(list);
  types = cb_types(list);
  type = ["sequential", "diverging", "qualitative"];
  if (!is_void(what)) {
    i = where(strcase(0, what) == strcase(0, names));
    if (!numberof(i)) {
      i = where(strglob(what+"*", type));
      if (numberof(i) != 1) error, "unrecognized color map type "+what;
      flag = i(1);
      i = where(types == i);
      list = list(i);
      names = names(i);
      types = types(i);
    } else {
      list = list(i(1));
      names = names(i(1));
      types = types(i(1));
      flag = -1;
    }
  }
  lens = array(int, 20, numberof(list));
  maps = cb_maps(list);
  for (i=1 ; i<=numberof(maps) ; ++i) {
    m = *maps(i);
    for (j=1 ; j<=numberof(m) ; ++j) lens(numberof(*m(j)), i) = 1;
  }
  if (flag == -1) {
    lens = where(lens);
    if (am_subroutine()) {
      write, names(1)+" maps available with";
      print, lens;
      write, "colors";
    }
    return lens;
  }
  if (n) {
    if (n<1 || n>20) error, "bad color map size "+totxt(n);
    i = where(lens(n,));
    if (!numberof(i)) {
      if (am_subroutine())
        write, "No "+(flag?type(flag)+" ":"")+"maps with "+totxt(n)+" colors";
      return [];
    }
    list = list(i);
    names = names(i);
    types = types(i);
    sfx = "_"+totxt(n);
    if (am_subroutine())
      write, "Available maps with "+totxt(n)+" colors:";
  } else {
    sfx = string(0);
  }
  if (!am_subroutine()) return names+sfx;
  i = where(types == 1);
  if (numberof(i)) write, grow("Sequential maps:",names(i));
  i = where(types == 2);
  if (numberof(i)) write, grow("Diverging maps:",names(i));
  i = where(types == 3);
  if (numberof(i)) write, grow("Qualitative maps:",names(i));
}

/* ------------------------------------------------------------------------ */
/* Many published figures use color tables from the IDL (or PVWAVE)
 * programming language.  See
 *   http://www.exelisvis.com/language/en-us/productsservices/idl.aspx
 *   http://en.wikipedia.org/wiki/IDL_%28programming_language%29
 */

func idlct_rd(filename, &names)
/* DOCUMENT tables = idlct_rd(filename, names)
 *   read an IDL color table file, returning TABLES and NAMES.
 *   TABLES is a 256-by-3-by-N array of N [r,g,b] color mappings, and
 *   NAMES is a corresponding list of N descriptions.
 *   The first index of tables corresponds to color index values 0:255.
 *
 *   The default IDL color tables file is colors1.tbl in the
 *   resource/colors subdirectory of the IDL distribution.  Copies
 *   and modified copies can be found on the Web, for example:
 *     http://www.bnl.gov/x26a/download/colors1.zip
 *   The IDLVM virtual machine is available without charge and
 *   includes this file.
 *
 *   This function will also read the png files posted at
 *     http://docs.idldev.com/vis/color/default-colors.png
 *     (see http://docs.idldev.com/vis/color/vis_loadct.html)
 *   (Assumes you have the yorick-z package installed.)
 * SEE ALSO: idlct
 */
{
  if (strpart(filename, -3:0) != ".png") {
    f = open(filename, "rb");
    ntab = char(0);
    _read, f, 0, ntab;
    if (!ntab) error, filename+" has no color tables";
    if (sizeof(f) < 1+800*ntab) error, filename+" too short";
    if (sizeof(f) > 1+800*ntab) write, "idlct: "+filename+" not truncated?";
    names = array(char, 32, ntab);
    _read, f, 1+768*ntab, names;
    names = strtrim(strchar(names));
    tables = array(char, 256, 3, ntab);
    _read, f, 1, tables;
    tables = transpose(tables, [1,2]);
  } else {
    tables = png_read(filename)(1:3, 1:256, 7::13);
    names = [];
  }
  return tables;
}

func idlct(n, ncols)
/* DOCUMENT idlct, n
 *       or idlct, tables
 *   set palette to IDL color table N.  You must first call the function
 *   with the TABLES returned by idlct_rd.  A second parameter is interpreted
 *   as the number of colors to use; the default is the size of the current
 *   palette or 200 if there is none.
 *   Hint: You can set _idlctables to a string containing the name of
 *     a file readable by idlct_rd to automatically load the tables the
 *     first time idlct is called.
 * SEE ALSO: idlct_rd, cmap
 */
{
  d = dimsof(n);
  if (d(1)) {
    if (d(1)!=3 || d(2)!=3 || structof(n)!=char)
      error, "IDL tables array not in format returned by idlct_rd";
    extern _idlctables;
    _idlctables = n;
  } else {
    if (structof(_idlctables) == string)
      _idlctables = idlct_rd(_idlctables);
    if (is_void(_idlctables))
      error, "IDL color tables not installed, use idlct,idlct_rd(file)";
    if (n<0 || n>=dimsof(_idlctables)(4))
      error, "invalid color table index, "+totxt(d(4))+" tables installed";
    ct = _idlctables(,,1+n);
    if (!am_subroutine()) return ct;
    cmap, ct, ncols;
  }
}

/* look for .gp file on gist path */
func _cmap_gist_scan(name)
{
  if (strpart(name,-2:0) != ".gp") name += ".gp";
  if (numberof(_cmap_gist_cache)) {
    i = where(_cmap_gist_cache == name);
    if (numberof(i)) return *_cmap_gist_cache_p(i(1));
  }
  if (open(name, "r", 1)) {
    fname = name;
  } else {
    require, "pathfun.i";
    gpath = pathsplit(set_gpath());
    list = where(strpart(gpath, 0:0) != "/");
    if (numberof(list)) gpath(list) += "/";  /* need to repair gpath */
    fname = find_in_path(name, takefirst=1, path=pathform(gpath));
    if (!fname) return [];
  }
  f = open(fname);
  r = g = b = array(char, 1024);
  for (;;) {
    n = read(f, r, g, b);
    if (n) break;
  }
  if (n%3 || !n) return [];
  rgb = transpose([r,g,b](1:n/3,));
  grow, _cmap_gist_cache, [name];
  grow, _cmap_gist_cache_p, [&rgb];
  return rgb;
}
