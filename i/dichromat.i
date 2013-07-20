/* dichromat.i
 * Color transforms to help visualize what colorblind people see.
 *
 * Based on
 * Brettel, Hans, Vienot, Francoise, and Mollon, John D.,
 * "Computerized simulation of color appearance for dichromats,"
 * J. Opt. Soc. Am. A Vol. 14, No.10(October), p. 2647-2655 (1997)
 *   http://perso.telecom-paristech.fr/~brettel
 *
 * Meyer, Gary W., and Greenberg, Donald P.,
 * "Color-Defective Vision and Computer Graphics Displays,"
 * IEEE Computer Graphics & Applications, September 1988, p. 28-40
 *
 * See vischeck.com to download tools for java.
 * See daltonize.org for more tools, discussion.
 * Good background reading at http://www.cvrl.org/
 */
/* Copyright (c) 2013, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */
require, "lab2rgb.i";

func dichromat(type, rgb, g, b, cmax=)
/* DOCUMENT drgb = dichromat(type, rgb)
 *       or drgb = dichromat(type, r, g, b)
 *
 *   Return colors DRGB perceived by a dichromat of TYPE corresponding
 *   to screen colors RGB = [r,g,b], or R, G, B.  The return value has
 *   the same dimensions as RGB or [R,G,B].  There are three classes
 *   of dichromats (colorblind people): Protrans, Deuterans, and
 *   Tritans, according to whether they lack long, medium, or short
 *   wavelength cone cells (or which type of cell is damaged).  For
 *   good background material, see
 *   http://en.wikipedia.org/wiki/Color_blindness or
 *   http://cvrl.ioo.ucl.ac.uk/ and for details of the algorithm
 *   implemented here see Brettel et al., JOSA 14, 2647 (1997),
 *   http://perso.telecom-paristech.fr/~brettel.  By default, DRGB is
 *   a char array, but with cmax=1, you can get a double array.  The
 *   input RGB can be a char array, or any integer datatype with a
 *   maximum of 255, or a floating point array with maximum 1.0.  Both
 *   input and output are sRGB values, that is, the RGB of your
 *   monitor; see the rgb_s2l and rgb_l2s functions for conversions to
 *   physically linear ("gamma corrected") RGB values.
 *
 *   TYPE is 1 for protran, 2 for deuteran, or 3 for tritan dichromatism.
 *   You may also pass any string beginning with "p", "d", or "t",
 *   respectively.  The dichromat function projects the three dimensional
 *   RGB onto a two dimensional subspace, so that there are only two primary
 *   colors instead of three.  Note that tritan dichromatism is quite rare,
 *   while some male in a room of 20 probably suffers from either
 *   deuteran or (less likely) protan dichromacy.
 *
 *   Use the dichromap function to quickly check a pseudocolor picture
 *   made with pli, plf, or plfc to see how it appears to a dichromat.
 *
 * SEE ALSO: dichromap, dichromat_setup, rgb_s2l, rgb_l2s
 */
{
  rgb = rgb_s2l(rgb, g, b);
  if (structof(type) == string)
    type = where(strcase(0,strpart(type,1:1)) == ["p", "d", "t"]);
  type = (numberof(type)==1)? type(1) : 0;
  if (type<1 || type>3) error, "type must be one of 1,2,3 or p,d,t";
  proj = _dichromatp(,,,type);  /* select projection operators */
  test = _dichromatt(,type);    /* select projection test */
  test = double(rgb(..,+) * test(+) >= 0.);
  rgb = rgb(..,+) * proj(,+,);  /* perform both projections */
  rgb = test*rgb(..,1) + (1.-test)*rgb(..,2);  /* pick proper projection */
  return rgb_l2s(rgb, cmax=cmax);
}

func dichromap(type)
/* DOCUMENT dichromap, type
 *       or dichromap
 *   Modify the current colormap to see how the plot (pli, plf, or
 *   plfc) would appear to a dichromat of the given TYPE, which is 1,
 *   2, 3 (or "p", "d", "t") to simulate protan, deuteran, or tritan
 *   vision.  See help, dichromat for more.  Omitting TYPE toggles the
 *   original color map with the modified one provided the current
 *   colormap is still the one installed by dichromap.
 * SEE ALSO: dichromat, cmap
 */
{
  require, "cmap.i";
  map = cmap();
  if (is_void(map)) error, "no current colormap";
  if (numberof(map)==numberof(_dichro_map))
    match = allof(map==_dichro_map) | (allof(map==_dichro_orig)<<1);
  if (is_void(type)) { 
    if (!match) error, "current map not installed by dichromap";
  } else {
    if (match == 1) map = _dichro_orig;
    match = 2;
    _dichro_orig = map;
    _dichro_map = transpose(dichromat(type, transpose(map)));
  }
  if (match == 1) cmap, _dichro_orig;
  else if (match == 2) cmap, _dichro_map;
}

func dichromat_setup(xylms, xypri, &test, recurse=)
/* DOCUMENT dichromat_setup, xylms, xypri
 *   Set up the dichromatic projection matrices and selection tests for
 *   the dichromat function.  XYLMS is either a 2x3 or 3x3 array of the
 *   CIE xy or XYZ color coordinates of the L, M, and S spectral responses
 *   of the three retinal cone cells responsible for color vision.  There
 *   is some dispute about exactly what colors these are, since the
 *   experiments to find them are difficult.  XYPRI is either a 2x2x3 or
 *   3x2x3 of the xy or XYZ color coordinates of the pairs of primary
 *   trichromatic colors which will be used to represent what the dichromat
 *   sees.  The third index is [protan,deuteran,tritan].  These should be
 *   chosen so that the colors returned by the dichromat function are
 *   indistinguishable from the input colors by that particular type of
 *   dichromat.
 * SEE ALSO: dichromat
 */
{
  if (!recurse) {
    d = dimsof(xylms);
    if (d(2) == 2) {
      d = array(0., d+[0,1,0]);
      d(1:2,) = xylms;  d(3,) = 1. - xylms(sum,);
      xylms = d;
    }
    d = dimsof(xypri);
    if (d(2) == 2) {
      d = array(0., d+[0,1,0,0]);
      d(1:2,,) = xypri;  d(3,,) = 1. - xypri(sum,,);
      xypri = d;
    }
    local t1, t2, t3;
    p1 = dichromat_setup(xylms, xypri(,,1), t1, recurse=1);
    p2 = dichromat_setup(xylms(,[2,3,1]), xypri(,,2), t2, recurse=1);
    p3 = dichromat_setup(xylms(,[3,1,2]), xypri(,,3), t3, recurse=1);
    extern _dichromatp, _dichromatt;
    _dichromatp = [p1, p2, p3];
    _dichromatt = [t1, t2, t3];

  } else {
    /* xylms permuted so that missing cone is xylms(,1),
     * and xypri is the pair of primaries to use for the result
     * both have full XYZ color coordinates
     * also need xyz_white from lab2rgb.i as neutral color
     *   (sRGB monitor definition requires this to be D65 white)
     */
    /* xyz = xylms(,+) * lms(+) */
    lmsxyz = LUsolve(xylms);       /* lms = lmsxyz(,+) * xyz(+) */
    p3 = xylms(,+:2:3) * lmsxyz(+:2:3,);  /* project out L axis */
    /* adding anything times xylms(,1) imperceptible to dichromat */
    /* first, we project white into the MS plane - this line in
     * MS is the test for which of the two dichromat primaries
     * we will use; colors that project to one side of the line
     * get one primary, while those projecting to the other side
     * get the other color
     */
    lmsw = lmsxyz(,+) * xyz_white(+);
    test = lmsw(3)*lmsxyz(2,) - lmsw(2)*lmsxyz(3,);
    t1 = test(+) * xypri(+,1);
    t2 = test(+) * xypri(+,2);
    if (t1*t2 >= 0.) error, "bad xypri, two primaries on same side of white";
    /* now we have to figure out how much xylms(,1) to add to p3:
     * in LMS space, want to add back L = some linear combination of M,S
     * L = a*M + b*S, where a and b are chosen to make white and one
     * primary map to themselves:
     *   lmsw(1) = lmsw(+:2:3) * [a,b](+)
     *   lmspri(1,i) = lmspri(+:2:3,i) * [a,b](+)
     */
    lmspri = lmsxyz(,+) * xypri(+,);
    ab1 = LUsolve(transpose([lmsw(2:3),lmspri(2:3,1)]), [lmsw(1),lmspri(1,1)]);
    ab2 = LUsolve(transpose([lmsw(2:3),lmspri(2:3,2)]), [lmsw(1),lmspri(1,2)]);
    p1 = p3 + xylms(,1) * (ab1(-,+) * lmsxyz(+:2:3,));
    p2 = p3 + xylms(,1) * (ab2(-,+) * lmsxyz(+:2:3,));
    /* finally, transform everything to lRGB coordinates from XYZ
     * rgb = _rgb_xyz(,+) * xyz(+)
     * xyz = _xyz_rgb(,+) * rgb(+)
     */
    test = test(+) * _xyz_rgb(+,);
    p1 = (_rgb_xyz(,+) * p1(+,))(,+) * _xyz_rgb(+,);
    p2 = (_rgb_xyz(,+) * p2(+,))(,+) * _xyz_rgb(+,);
    return (t1 > 0.)? [p1, p2] : [p2, p1];
  }
}

/* Brettel has suggestions for xypri: For both protan and deuteran
 * dichromats, he chooses monochromatic 475 (blue) and 575 (yellow) as
 * the two colors, while for tritan dichromats, he chooses 485 (green)
 * and 660 (red).
 *
 * Meyer says protan axis is 473 nm to D65 to 574 nm,
 * deuteran axis is 477 to D65 to 578, tritan axis is 490 to D65 to 610.
 *
 * VC vischeck.com uses somewhat different choices
 */
xypri_brettel = [[[0.109594,0.0868425], [0.478775,0.520202]],
                 [[0.109594,0.0868425], [0.478775,0.520202]],
                 [[0.0687059,0.200723], [0.729969,0.270031]]];
xypri_meyer = [[[0.116091,0.073853], [0.47197,0.526967]],
               [[0.103188,0.102897], [0.49913,0.499907]],
               [[0.0453907,0.294976],[0.665764,0.334011]]];
xypri_vc = [[[0.123165,0.088141], [0.476373,0.542367]],
            [[0.123165,0.088141], [0.476373,0.542367]],
            [[0.111112,0.207644], [0.840774,0.202248]]];

/* Meyer "Color-Defective Vision and Computer Graphics Displays" (1988)
 * Hunt-Pointer-Estevez (HPE) used in RLAB color appearance model
 * Bradford used in CIE CAT02, CAT97s, CNCCAT97
 *  spectral sharpening does not change xylms
 *  normalizing to D65 white does not change xylms
 * VC = VisCheck, vischeck.com
 * Daltonize = daltonize.org, very close to Meyer
 */
xylms_meyer =    [[0.735,0.265], [ 1.14, -0.14], [0.171,-0.003]];
xylms_hpe =      [[0.837,0.163], [ 2.30, -1.30], [0.168, 0]];
xylms_bradford = [[0.700,0.306], [-0.286,1.196], [0.137, 0.043]];
xylms_vc = [[0.883854,0.161895], [-13.6498,13.7753], [0.153684,-0.0288546]];
xylms_daltonize = [[0.7465, 0.2535], [1.4, -0.4], [0.1748, 0.]];

dichromat_setup, xylms_vc, xypri_vc;

/* after daltonize.py from daltonize.org and http://moinmo.in/AccessibleMoin */
func daltonize(type, rgb, g, b, cmax=)
{
  rgb = rgb_s2l(rgb, g, b);
  lrgb_skip = 1;
  err = rgb - dichromat(type, rgb);
  lrgb_skip = [];
  rgb += err(..,+) * daltonizer(+,);
  return rgb_l2s(min(max(rgb, 0.), 1.), cmax=cmax);
}
daltonizer = [[0,0,0], [0.7,1,0], [0.7,0,1]];
