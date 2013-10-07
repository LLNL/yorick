/* cie.i
 * CIE standard illuminants and RGB and XYZ color matching functions.
 *
 * See http://en.wikipedia.org/wiki/Standard_illuminant
 * and http://en.wikipedia.org/wiki/CIE_1931_color_space
 * Good background reading at http://www.cvrl.org/
 */
/* Copyright (c) 2013, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* CIE A illuminant exactly matches tabulated A illuminant
 * http://en.wikipedia.org/wiki/Standard_illuminant
 */
func cie_a(lambda)
/* DOCUMENT cie_a(lambda)
 *   Return CIE A illuminant (incandescent lighting) as a function of
 *   wavelength LAMBDA in nm.  (2856 K Plankian)
 *   Chromaticity coordinates are xy = [0.44758, 0.40745].
 * SEE ALSO: cie_d
 */
{
  c = 1.435e7 / 2848.;  /* 2848 K with 1931 value of hc/k */
  return 100. * (560./lambda) * expm1(c/560.)/expm1(c/lambda);
}

/* CIE daylight components S0(lambda), S1(lambda), S2(lambda),
 * from 1931 tables at lambda = 300 to 830 nm in steps of 10 nm
 * available from wwww.cie.co.at
 * http://files.cie.co.at/204.xls  is tabulated in steps of 5 nm,
 *   but the S0,S1,S2 values at odd multiples of 5 nm are simply the
 *   linear interpolates of the 10 nm points here
 */
cie_s0 = [0.04,6,29.6,55.3,57.3,61.8,61.5,68.8,63.4,65.8,94.8,104.8,105.9,
          96.8,113.9,125.6,125.5,121.3,121.3,113.5,113.1,110.8,106.5,108.8,
          105.3,104.4,100,96,95.1,89.1,90.5,90.3,88.4,84,85.1,81.9,82.6,
          84.9,81.3,71.9,74.3,76.4,63.3,71.7,77,65.2,47.7,68.6,65,66,61,
          53.3,58.9,61.9];
cie_s1 = [0.02,4.5,22.4,42,40.6,41.6,38,42.4,38.5,35,43.4,46.3,43.9,37.1,36.7,
          35.9,32.6,27.9,24.3,20.1,16.2,13.2,8.6,6.1,4.2,1.9,0,-1.6,-3.5,-3.5,
          -5.8,-7.2,-8.6,-9.5,-10.9,-10.7,-12,-14,-13.6,-12,-13.3,-12.9,-10.6,
          -11.6,-12.2,-10.2,-7.8,-11.2,-10.4,-10.6,-9.7,-8.3,-9.3,-9.8];
cie_s2 = [0,2,4,8.5,7.8,6.7,5.3,6.1,3,1.2,-1.1,-0.5,-0.7,-1.2,-2.6,-2.9,-2.8,
          -2.6,-2.6,-1.8,-1.5,-1.3,-1.2,-1,-0.5,-0.3,0,0.2,0.5,2.1,3.2,4.1,
          4.7,5.1,6.7,7.3,8.6,9.8,10.2,8.3,9.6,8.5,7,7.6,8,6.7,5.2,7.4,6.8,
          7,6.4,5.5,6.1,6.5];

func cie_d(lambda, x, y)
/* DOCUMENT cie_d(lambda)
 *       or cie_d(lambda, T)
 *       or cie_d(lambda, xD, yD)
 *       or cie_d(lambda, xyD)
 *       or cie_d(, T)
 *   Return CIE D illuminant (daylight) as a function of wavelength LAMBDA
 *   in nm.  In the first form, return the D65 illuminant (noon daylight).
 *   In the second form, T is the temperature in K (roughly 6500 for D65).
 *   T must be >=4000 and <=25000, except as special cases you may pass
 *   T = 50, 55, 65, or 75 to get D50, D55, D65, or D75, respectively.
 *   In the third and fourth forms, (xD,yD) or xyD=[xD,yD] are chromaticity
 *   coordinates (roughly [0.31271,0.32902] for D65).
 *   In the fifth form, return the xyD chromaticity coordinates.
 *
 *   This function implements the formulas given in
 *     http://en.wikipedia.org/wiki/Standard_illuminant
 *   combined with the exact CIE tabulated S0,S1,S2 daylight components.
 *   The CIE tabulated D65 illuminant (from http://files.cie.co.at/204.xls
 *   the same source as the daylight components) differs slightly.  In its
 *   single argument form, cie_d returns the best fit to the D65 table
 *   from CIE, which corresponds to T=6502.849 K.  The maximum disagreement
 *   with the table is less than 0.00081, while the table itself shows
 *   the 0.0001 digit.
 * SEE ALSO: cie_a
 */
{
  if (is_void(y)) {
    if (is_void(x)) x = 6502.849;  /* best match to CIE D65 table */
    if (numberof(x) == 2) {
      y = x(2);
      x = x(1);
    } else if (x == 50) {
      x = 0.34567;  y = 0.35850;
    } else if (x == 55) {
      x = 0.33242;  y = 0.34743;
    } else if (x == 65) {
      x = 0.31271;  y = 0.32902;
    } else if (x == 75) {
      x = 0.29902;  y = 0.31485;
    } else {
      if (x<4000 || x>25000) error, "D temperature outside 4000 to 25000 K";
      if (x<=7000) x = poly(1000./x, 0.244063, 0.09911, 2.9678, -4.6070);
      else         x = poly(1000./x, 0.237040, 0.24748, 1.9018, -2.0064);
      y = poly(x, -0.275, 2.870, -3.000);
    }
    if (is_void(lambda)) return [x, y];
  }
  m1 = m2 = 1./(0.0241 + 0.2562*x - 0.7341*y);
  m1 *= -1.3515 -  1.7703*x +  5.9114*y;
  m2 *=  0.0300 - 31.4424*x + 30.0717*y;
  return interp(cie_s0+m1*cie_s1+m2*cie_s2, indgen(300:830:10), lambda);
}

/* CIE 1931 standard colorimetric observer (2 degree field of view)
 * from http://files.cie.co.at/204.xls 380 to 780 nm in 5 nm steps
 */
cie_lambda = double(indgen(380:780:5));
cie_x =
  [0.001368,0.002236,0.004243,0.00765,0.01431,0.02319,0.04351,0.07763,0.13438,
   0.21477,0.2839,0.3285,0.34828,0.34806,0.3362,0.3187,0.2908,0.2511,0.19536,
   0.1421,0.09564,0.05795,0.03201,0.0147,0.0049,0.0024,0.0093,0.0291,0.06327,
   0.1096,0.1655,0.22575,0.2904,0.3597,0.43345,0.51205,0.5945,0.6784,0.7621,
   0.8425,0.9163,0.9786,1.0263,1.0567,1.0622,1.0456,1.0026,0.9384,0.85445,
   0.7514,0.6424,0.5419,0.4479,0.3608,0.2835,0.2187,0.1649,0.1212,0.0874,
   0.0636,0.04677,0.0329,0.0227,0.01584,0.011359,0.008111,0.00579,0.004109,
   0.002899,0.002049,0.00144,0.001,0.00069,0.000476,0.000332,0.000235,
   0.000166,0.000117,8.3e-05,5.9e-05,4.2e-05];
cie_y =
  [3.9e-05,6.4e-05,0.00012,0.000217,0.000396,0.00064,0.00121,0.00218,0.004,
   0.0073,0.0116,0.01684,0.023,0.0298,0.038,0.048,0.06,0.0739,0.09098,0.1126,
   0.13902,0.1693,0.20802,0.2586,0.323,0.4073,0.503,0.6082,0.71,0.7932,0.862,
   0.91485,0.954,0.9803,0.99495,1,0.995,0.9786,0.952,0.9154,0.87,0.8163,0.757,
   0.6949,0.631,0.5668,0.503,0.4412,0.381,0.321,0.265,0.217,0.175,0.1382,0.107,
   0.0816,0.061,0.04458,0.032,0.0232,0.017,0.01192,0.00821,0.005723,0.004102,
   0.002929,0.002091,0.001484,0.001047,0.00074,0.00052,0.000361,0.000249,
   0.000172,0.00012,8.5e-05,6e-05,4.2e-05,3e-05,2.1e-05,1.5e-05];
cie_z =
  [0.00645,0.01055,0.02005,0.03621,0.06785,0.1102,0.2074,0.3713,0.6456,
   1.03905,1.3856,1.62296,1.74706,1.7826,1.77211,1.7441,1.6692,1.5281,1.28764,
   1.0419,0.81295,0.6162,0.46518,0.3533,0.272,0.2123,0.1582,0.1117,0.07825,
   0.05725,0.04216,0.02984,0.0203,0.0134,0.00875,0.00575,0.0039,0.00275,
   0.0021,0.0018,0.00165,0.0014,0.0011,0.001,0.0008,0.0006,0.00034,0.00024,
   0.00019,0.0001,5e-05,3e-05,2e-05,1e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0];

/* xyz = _cie_xyz(+,) * rgb(+)
 * rgb = _cie_rgb(+,) * xyz(+)
 *   This is 1931 CIE RGB, based on (700, 546.1, 435.8) nm primaries.
 * The RGB for a monitor are different; the standard monitor RGB
 * space is the lRGB space (see lab2rgb.i), which is defined by
 *   xyz = _xyz_rgb(,+) * lrgb(+)
 *   lrgb = _rgb_xyz(,+) * xyz(+)
 */
_cie_xyz = [[0.49, 0.31, 0.20], [0.17697, 0.81240, 0.01063],
            [0., 0.01, 0.99]] / 0.17697;
_cie_rgb = LUsolve(_cie_xyz);

func cie_xyz(lambda, xy=)
/* DOCUMENT xyz = cie_xyz(lambda)
 *   Return the 1931 CIE standard observer color matching functions at
 *   wavelength LAMBDA (nm).  LAMBDA may be an array; the return value
 *   has an additional trailing dimension of length 3, corresponding to
 *   the [X, Y, Z] color matching functions.  If you have a spectrum
 *   as a 1D array with the same length as LAMBDA, and if the spectrum
 *   is negligible outside the range of LAMBDA, then the [X,Y,Z] color
 *   space coordinates for that spectrum are the integral:
 *     ((spectrum * cie_xyz(lambda))(zcen) * lambda(dif))(sum,)
 *
 *   With the xy=1 keyword, returns only the CIE [x,y] chromaticity
 *   coordinates instead of the full [X,Y,Z] color space coordinates.
 *   With LAMBDA=span(380,780,n), this gives you n points around the
 *   curved monochromatic locus of the CIE 1931 chromaticity diagram.
 * SEE ALSO: cie_rgb, cie_a, cie_d
 */
{
  x = interp(cie_x, cie_lambda, lambda);
  y = interp(cie_y, cie_lambda, lambda);
  z = interp(cie_z, cie_lambda, lambda);
  if (xy) return [x, y] / (x+y+z);
  return [x, y, z];
}

func cie_rgb(lambda)
/* DOCUMENT xyz = cie_xyz(lambda)
 *   Return the 1931 CIE RGB color matching functions at wavelength
 *   LAMBDA (nm).  LAMBDA may be an array; the return value has an
 *   additional trailing dimension of length 3, corresponding to
 *   the [R, G, B] color matching functions.  The [R,G,B] values are
 *   the (scaled) brightnesses of monochromatic primaries of wavelengths
 *   700 nm (R), 546.1 nm (G), and 435.8 nm (B) (convenient to produce
 *   in 1931, otherwise arbitrary), required to exactly match a fourth
 *   monochromatic color of wavelength LAMBDA.  The R, G, B components
 *   can be negative (especially the R component), meaning that the
 *   primary must be added to the LAMBDA wavelength instead of to the
 *   other primaries to achieve a match.
 * SEE ALSO: cie_rgb, cie_a, cie_d
 */
{
  xyz = cie_xyz(lambda);
  return xyz(..,+) * _cie_rgb(+,);
}
