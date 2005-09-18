/*
 * $Id: fermi.i,v 1.1 2005-09-18 22:06:15 dhmunro Exp $
 * Fermi-Dirac integrals and inverses of orders -1/2, 1/2, 3/2, 5/2
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

local fermi;
/* DOCUMENT #include "fermi.i"
 *   Fermi-Dirac integrals and inverses of orders -1/2, 1/2, 3/2, 5/2
 *
 *   Antia, H. M., Aph.J. 84, p.101-108 (1993)
 *
 * SEE ALSO: fdm12, fd12, fd32, fd52, ifdm12, ifd12, ifd32, ifd52
 */

func fdm12(x)
/* DOCUMENT fdm12(x)
 *   return Fermi-Dirac integral of order -1/2,
 *      fdm12(x) = integral[0 to inf]{ dt * t^-0.5 / (exp(t-x)+1) }
 *   accurate to about 1e-12
 * SEE ALSO: fd12, fd32, fd52, ifdm12, ifd12, ifd32, ifd52
 */
{
  mask = x < 2.;
  list = where(mask);
  if (numberof(list)) {
    a = exp(x(list));
    a = a*poly(a, 1.71446374704454e7,    3.88148302324068e7,
               3.16743385304962e7,    1.14587609192151e7,
               1.83696370756153e6,    1.14980998186874e5,
               1.98276889924768e3,    1.0) /
      poly(a, 9.67282587452899e6,    2.87386436731785e7,
           3.26070130734158e7,    1.77657027846367e7,
           4.81648022267831e6,    6.13709569333207e5,
           3.13595854332114e4,    4.35061725080755e2);
  }
  list = where(!mask);
  if (numberof(list)) {
    x = x(list);
    b = 1./(x*x);
    b = sqrt(x)*poly(b, -4.46620341924942e-15, -1.58654991146236e-12,
                     -4.44467627042232e-10, -6.84738791621745e-8,
                     -6.64932238528105e-6,  -3.69976170193942e-4,
                     -1.12295393687006e-2,  -1.60926102124442e-1,
                     -8.52408612877447e-1,  -7.45519953763928e-1,
                     2.98435207466372e0,    1.0) /
      poly(b, -2.23310170962369e-15, -7.94193282071464e-13,
           -2.22564376956228e-10, -3.43299431079845e-8,
           -3.33919612678907e-6,  -1.86432212187088e-4,
           -5.69764436880529e-3,  -8.34904593067194e-2,
           -4.78770844009440e-1,  -4.99759250374148e-1,
           1.86795964993052e0,    4.16485970495288e-1);
  }
  return merge(a, b, mask);
}

func fd12(x)
/* DOCUMENT fd12(x)
 *   return Fermi-Dirac integral of order 1/2,
 *      fd12(x) = integral[0 to inf]{ dt * t^0.5 / (exp(t-x)+1) }
 *   accurate to about 1e-12
 * SEE ALSO: fdm12, fd32, fd52, ifdm12, ifd12, ifd32, ifd52
 */
{
  mask = x < 2.;
  list = where(mask);
  if (numberof(list)) {
    a = exp(x(list));
    a = a*poly(a, 5.75834152995465e6,   1.30964880355883e7,
               1.07608632249013e7,   3.93536421893014e6,
               6.42493233715640e5,   4.16031909245777e4,
               7.77238678539648e2,   1.0) /
      poly(a, 6.49759261942269e6,   1.70750501625775e7,
           1.69288134856160e7,   7.95192647756086e6,
           1.83167424554505e6,   1.95155948326832e5,
           8.17922106644547e3,   9.02129136642157e1);
  }
  list = where(!mask);
  if (numberof(list)) {
    x = x(list);
    b = 1./(x*x);
    b = x*sqrt(x)*poly(b, 4.85378381173415e-14, 1.64429113030738e-11,
                       3.76794942277806e-9,  4.69233883900644e-7,
                       3.40679845803144e-5,  1.32212995937796e-3,
                       2.60768398973913e-2,  2.48653216266227e-1,
                       1.08037861921488e0,   1.91247528779676e0, 1.0) /
      poly(b, 7.28067571760518e-14, 2.45745452167585e-11,
           5.62152894375277e-9,  6.96888634549649e-7,
           5.02360015186394e-5,  1.92040136756592e-3,
           3.66887808002874e-2,  3.24095226486468e-1,
           1.16434871200131e0,   1.34981244060549e0,
           2.01311836975930e-1, -2.14562434782759e-2);
  }
  return merge(a, b, mask);
}

func fd32(x)
/* DOCUMENT fd32(x)
 *   return Fermi-Dirac integral of order 3/2,
 *      fd32(x) = integral[0 to inf]{ dt * t^1.5 / (exp(t-x)+1) }
 *   accurate to about 1e-12
 * SEE ALSO: fdm12, fd12, fd52, ifdm12, ifd12, ifd32, ifd52
 */
{
  mask = x < 2.;
  list = where(mask);
  if (numberof(list)) {
    a = exp(x(list));
    a = a*poly(a, 4.32326386604283e4,   8.55472308218786e4,
               5.95275291210962e4,   1.77294861572005e4,
               2.21876607796460e3,   9.90562948053193e1, 1.0) /
      poly(a, 3.25218725353467e4,   7.01022511904373e4,
           5.50859144223638e4,   1.95942074576400e4,
           3.20803912586318e3,   2.20853967067789e2,
           5.05580641737527e0,   1.99507945223266e-2);
  }
  list = where(!mask);
  if (numberof(list)) {
    x = x(list);
    b = 1./(x*x);
    b = sqrt(x)*poly(b, 2.80452693148553e-13, 8.60096863656367e-11,
                     1.62974620742993e-8,  1.63598843752050e-6,
                     9.12915407846722e-5,  2.62988766922117e-3,
                     3.85682997219346e-2,  2.78383256609605e-1,
                     9.02250179334496e-1,  1.0) /
      (b*poly(b, 7.01131732871184e-13, 2.10699282897576e-10,
              3.94452010378723e-8,  3.84703231868724e-6,
              2.04569943213216e-4,  5.31999109566385e-3,
              6.39899717779153e-2,  3.14236143831882e-1,
              4.70252591891375e-1, -2.15540156936373e-2,
              2.34829436438087e-3));
  }
  return merge(a, b, mask);
}

func fd52(x)
/* DOCUMENT fd52(x)
 *   return Fermi-Dirac integral of order 5/2,
 *      fd52(x) = integral[0 to inf]{ dt * t^2.5 / (exp(t-x)+1) }
 *   accurate to about 1e-12
 * SEE ALSO: fdm12, fd12, fd32, ifdm12, ifd12, ifd32, ifd52
 */
{
  mask = x < 2.;
  list = where(mask);
  if (numberof(list)) {
    a = exp(x(list));
    a = a*poly(a, 6.61606300631656e4,   1.20132462801652e5,
               7.67255995316812e4,   2.10427138842443e4,
               2.44325236813275e3,   1.02589947781696e2, 1.0) /
      poly(a, 1.99078071053871e4,   3.79076097261066e4,
           2.60117136841197e4,   7.97584657659364e3,
           1.10886130159658e3,   6.35483623268093e1,
           1.16951072617142e0,   3.31482978240026e-3);
  }
  list = where(!mask);
  if (numberof(list)) {
    x = x(list);
    b = 1./(x*x);
    b = x*sqrt(x)*poly(b, 8.42667076131315e-12, 2.31618876821567e-9,
                       3.54323824923987e-7,  2.77981736000034e-5,
                       1.14008027400645e-3,  2.32779790773633e-2,
                       2.39564845938301e-1,  1.24415366126179e0,
                       3.18831203950106e0,   3.42040216997894e0, 1.0) /
      (b*poly(b, 2.94933476646033e-11, 7.68215783076936e-9,
              1.12919616415947e-6,  8.09451165406274e-5,
              2.81111224925648e-3,  3.99937801931919e-2,
              2.27132567866839e-1,  5.31886045222680e-1,
              3.70866321410385e-1,  2.27326643192516e-2));
  }
  return merge(a, b, mask);
}

func ifdm12(x)
/* DOCUMENT ifdm12(y)
 *   return x = inverse of Fermi-Dirac integral of order -1/2,
 *      y = integral[0 to inf]{ dt * t^-0.5 / (exp(t-x)+1) }
 *   accurate to about 1e-8
 * SEE ALSO: ifd12, ifd32, ifd52, fdm12, fd12, fd32, fd52
 */
{
  mask = x < 4.;
  list = where(mask);
  if (numberof(list)) {
    a = x(list);
    a = log(a*poly(a, -1.570044577033e4,   1.001958278442e4,
                   -2.805343454951e3,   4.121170498099e2,
                   -3.174780572961e1,   1.0) /
            poly(a, -2.782831558471e4,   2.886114034012e4,
                 -1.274243093149e4,   3.063252215963e3,
                 -4.225615045074e2,   3.168918168284e1,
                 -1.008561571363e0));
  }
  list = where(!mask);
  if (numberof(list)) {
    x = x(list);
    b = 1./(x*x);
    b = poly(b, 2.206779160034e-8,  -1.437701234283e-6,
             6.103116850636e-5,  -1.169411057416e-3,
             1.814141021608e-2,  -9.588603457639e-2, 1.0) /
      (b*poly(b, 8.827116613576e-8,  -5.750804196059e-6,
              2.429627688357e-4,  -4.601959491394e-3,
              6.932122275919e-2,  -3.217372489776e-1,
              3.124344749296e0));
  }
  return merge(a, b, mask);
}

func ifd12(x)
/* DOCUMENT ifd12(y)
 *   return x = inverse of Fermi-Dirac integral of order 1/2,
 *      y = integral[0 to inf]{ dt * t^0.5 / (exp(t-x)+1) }
 *   accurate to about 1e-8
 * SEE ALSO: ifdm12, ifd32, ifd52, fdm12, fd12, fd32, fd52
 */
{
  mask = x < 4.;
  list = where(mask);
  if (numberof(list)) {
    a = x(list);
    a = log(a*poly(a, 1.999266880833e4,   5.702479099336e3,
                   6.610132843877e2,   3.818838129486e1, 1.0) /
            poly(a, 1.771804140488e4,  -2.014785161019e3,
                 9.130355392717e1,  -1.670718177489e0));
  }
  list = where(!mask);
  if (numberof(list)) {
    x = x(list);
    b = x^(-2./3.);
    b = poly(b, -1.277060388085e-2,  7.187946804945e-2, 
             -4.262314235106e-1,  4.997559426872e-1,
             -1.285579118012e0,  -3.930805454272e-1, 1.0) /
      (b*poly(b, -9.745794806288e-3,  5.485432756838e-2,
              -3.299466243260e-1,  4.077841975923e-1,
              -1.145531476975e0,  -6.067091689181e-2));
  }
  return merge(a, b, mask);
}

func ifd32(x)
/* DOCUMENT ifd32(y)
 *   return x = inverse of Fermi-Dirac integral of order 3/2,
 *      y = integral[0 to inf]{ dt * t^1.5 / (exp(t-x)+1) }
 *   accurate to about 1e-8
 * SEE ALSO: ifdm12, ifd12, ifd52, fdm12, fd12, fd32, fd52
 */
{
  mask = x < 4.;
  list = where(mask);
  if (numberof(list)) {
    a = x(list);
    a = log(a*poly(a, 1.715627994191e2,   1.125926232897e2,
                   2.056296753055e1,   1.0) /
            poly(a, 2.280653583157e2,   1.193456203021e2,
                 1.167743113540e1,  -3.226808804038e-1,
                 3.519268762788e-3));
  }
  list = where(!mask);
  if (numberof(list)) {
    x = x(list);
    b = x^(-0.4);
    b = poly(b, -6.321828169799e-3, -2.183147266896e-2,
             -1.057562799320e-1, -4.657944387545e-1,
             -5.951932864088e-1,  3.684471177100e-1, 1.0) /
      (b*poly(b, -4.381942605018e-3, -1.513236504100e-2,
              -7.850001283886e-2, -3.407561772612e-1,
              -5.074812565486e-1, -1.387107009074e-1));
  }
  return merge(a, b, mask);
}

func ifd52(x)
/* DOCUMENT ifd52(y)
 *   return x = inverse of Fermi-Dirac integral of order 5/2,
 *      y = integral[0 to inf]{ dt * t^2.5 / (exp(t-x)+1) }
 *   accurate to about 1e-8
 * SEE ALSO: ifdm12, ifd12, ifd32, fdm12, fd12, fd32, fd52
 */
{
  mask = x < 4.;
  list = where(mask);
  if (numberof(list)) {
    a = x(list);
    a = log(a*poly(a, 2.138969250409e2,   3.539903493971e1, 1.0) /
            poly(a, 7.108545512710e2,   9.873746988121e1,
                 1.067755522895e0,  -1.182798726503e-2));
  }
  list = where(!mask);
  if (numberof(list)) {
    x = x(list);
    b = x^(-2./7.);
    b = poly(b, -3.312041011227e-2,  1.315763372315e-1, 
             -4.820942898296e-1,  5.099038074944e-1,
             5.495613498630e-1, -1.498867562255e0, 1.0) /
      (b*poly(b, -2.315515517515e-2,  9.198776585252e-2,
              -3.835879295548e-1,  5.415026856351e-1,
              -3.847241692193e-1,  3.739781456585e-2, 
              -3.008504449098e-2));
  }
  return merge(a, b, mask);
}
