/*
 * $Id: kepler.i,v 1.1 2005-09-18 22:06:15 dhmunro Exp $
 * yorick solar system model
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func kepler(orbit, time, &ma, &ta, &norb, full=)
/* DOCUMENT xyz = kepler(orbit, time)
         or xyz = kepler(orbit, time, ma, ta, norb)

     return 3-dimsof(orbit(1,..))-by-dimsof(time) XYZ coordinates
     corresponding to the orbit(s) ORBIT and time(s) TIME.  Optionally
     return mean anomaly MA, true anomaly TA, and integer number of
     orbits, each a dimsof(orbit(1,..))-by-dimsof(time) array.  The
     MA and TA are in radians.  The x-axis is along the line of the
     vernal equinox, the z-axis is ecliptic north.

     ORBIT has leading dimension 12: [angle from perihelion, mean daily
     motion, semi-major axis, d/dt(semi-major axis), eccentricity,
     d/dt(eccentricity), longitude of ascending node, d/dt(ascending
     node), angle from ascending node to perihelion, d/dt(perihelion),
     inclination, d/dt(inclination)]
     (Six pairs of a quantity and its time derivative.)
     The angles are in degrees; d/dt units must match TIME units.

     Mean anomaly is not an angle in real space; it is the quantity
     proportional to time in Kepler's equation.  True anomaly is the
     angle from perihelion to planet.

     With a non-nil, non-zero full= keyword, return XYZUVW -- that is,
     six coordinates including velocities as well as positions.

   SEE ALSO: sch_planets, jpl_planets, sch_moon, moon, solar_system
 */
{
  /* result will be dimsof(orbit(1,..))-by-dimsof(time) */
  time = [orbit(1,..)*0.+1.](..,+) * [time](..,+);

  /* ma = mean anomaly = time from perihelion to planet, as an angle
   * a = semi-major axis
   * e = eccentricity
   */
  ma = (orbit(1,..) + orbit(2,..)*time) * (pi/180.);
  a = (orbit(3,..) + orbit(4,..)*time);
  e = (orbit(5,..) + orbit(6,..)*time);

  /* reduce ma to interval (-pi,pi] */
  norb = ceil((ma-pi)/(2.*pi));
  ma -= 2.*pi*norb;

  /* ea = eccentric anomaly, ma = ea-e*sin(ea) is kepler's equation of time
   * solve for ea by newton iteration */
  ea = ma + e*sin(ma)*(1.+e*cos(ma));
  do {
    sea = sin(ea);
    cea = cos(ea);
    dea = (ea-e*sea - ma)/(1.-e*cea);
    ea -= dea;
  } while (anyof(abs(dea) > 1.e-8));

  /* ta = true anomaly = angle from perihelion to planet */
  ta = atan(sea, cea-e);

  boa = sqrt(1.-e*e);
  b = a * boa;  /* semi-minor axis */
  xyz = array(0., 3,dimsof(cea));
  xyz(1,..) = a*(cea-e);
  xyz(2,..) = b*sea;

  local rotdot;
  rot = to_ecliptic(orbit,time,full=full);
  rxyz = (rot * xyz(-,..))(,sum,..);

  if (full) {
    adot = orbit(4,..);
    edot = orbit(6,..);
    bdot = adot*boa - e*edot/boa;
    eadot = (orbit(2,..)*(pi/180.) + edot*sea) / (1.-e*cea);
    uvw = array(0., 6,dimsof(cea));
    uvw(4,..) = adot*(cea-e) - a*(sea*eadot-edot);
    uvw(5,..) = bdot*sea + b*cea*eadot;
    uvw(4:6,..) = (rotdot * xyz(-,..) + rot * uvw(-,4:6,..))(,sum,..);
    uvw(1:3,..) = rxyz;
    return uvw;
  }

  return rxyz;
}

func kepler2(orbit, xyz, &time, &ma, &ta)
/* DOCUMENT xyz = kepler2(orbit, xyz0)
         or xyz = kepler2(orbit, xyz0, time, ma, ta)

     return dimsof(xyz0) XYZ coordinates corresponding to the orbit(s)
     ORBIT and direction(s) XYZ0.  The dimensions of ORBIT beyond the
     first, if any, must match those of XYZ0, although XYZ0 may have
     any number of trailing dimensions.

     Optionally return TIME, mean anomaly MA, and true anomaly TA,
     each a dimsof(orbit(1,..))-by-dimsof(time) array.  The MA and TA
     are in radians.  The x-axis is along the line of the vernal
     equinox, the z-axis is ecliptic north.  The XYZ0 direction is first
     projected into the plane of the orbit; then XYZ will be proportional
     to XYZ0.  The time derivatives of the ORBIT elements are ignored.

     ORBIT has leading dimension 12: [angle from perihelion, mean daily
     motion, semi-major axis, d/dt(semi-major axis), eccentricity,
     d/dt(eccentricity), longitude of ascending node, d/dt(ascending
     node), angle from ascending node to perihelion, d/dt(perihelion),
     inclination, d/dt(inclination)]
     (Six pairs of a quantity and its time derivative.)
     The angles are in degrees; d/dt units must match TIME units.

     Mean anomaly is not an angle in real space; it is the quantity
     proportional to time in Kepler's equation.  True anomaly is the
     angle from perihelion to planet.

   SEE ALSO: sch_planets, jpl_planets, sch_moon, moon, solar_system
 */
{
  /* rotate xyz0 into plane of orbit, with x along perihelion */
  transform = to_ecliptic(orbit,0.0);
  xyz = (transform * xyz(,-,..))(sum,..);
  xyz(3,..) = 0.0;  /* project xyz0 into orbital plane */

  /* a = semi-major axis
   * e = eccentricity
   */
  a = orbit(3,..); /* + orbit(4,..)*time */
  e = orbit(5,..); /* + orbit(6,..)*time */
  b = a*sqrt(1.-e*e);

  /* solve for ea = eccentric anomaly by newton iteration */
  cea = xyz(1,..);
  sea = xyz(2,..);
  ay = a*sea;
  bx = b*cea;
  for (done=0 ;;) {
    rr = 1./abs(cea,sea);
    cea *= rr;
    sea *= rr;
    if (done) break;
    rr = (ay*(cea-e)-bx*sea)/(bx*cea+ay*sea);
    cea -= sea*rr;
    sea += cea*rr;
    done = allof(abs(rr) < 1.e-8);
  }
  xyz(1,..) = a*(cea - e);
  xyz(2,..) = b*sea;

  ma = atan(sea, cea) - e*sea;
  ta = atan(sea, cea-e);
  time = ((180./pi)*ma - orbit(1,..))/orbit(2,..);

  return (transform * xyz(-,..))(,sum,..);
}

func to_ecliptic(orbit,time,full=)
{
  /* ascn = longitude of ascending node (from vernal equinox at epoch)
   * argp = argument of perihelion = angle from ascn to perihelion
   * incl = inclination of orbit from ecliptic
   */
  ascn = (orbit(7,..) + orbit(8,..)*time) * (pi/180.);
  argp = (orbit(9,..) + orbit(10,..)*time) * (pi/180.);
  incl = (orbit(11,..) + orbit(12,..)*time) * (pi/180.);

  can = cos(ascn);
  san = sin(ascn);
  car = cos(argp);
  sar = sin(argp);
  cn = cos(incl);
  sn = sin(incl);

  /* [x-axis (perihelion vector) in ecliptic coords,
   *  y-axis (perp to perihelion) in ecliptic coords,
   *  z-axis (of orbit) in ecliptic coords]
   */
  axes = [[ can*car-san*cn*sar,  san*car+can*cn*sar, sn*sar],
          [-can*sar-san*cn*car, -san*sar+can*cn*car, sn*car],
          [         san*sn,             -can*sn,     cn]];
  axes = transpose(axes, 3);  /* make matrix indices first */

  if (full) {
    ascndot = orbit(8,..) * (pi/180.);
    argpdot = orbit(10,..) * (pi/180.);
    incldot = orbit(12,..) * (pi/180.);
    dcan = -san*ascndot;
    dsan = can*ascndot;
    dcar = -sar*argpdot;
    dsar = car*argpdot;
    dcn = -sn*incldot;
    dsn = cn*incldot;
    extern rotdot;
    rotdot = [[ can*dcar+dcan*car-san*cn*dsar-san*dcn*sar-dsan*cn*sar,
                san*dcar+dsan*car+can*cn*dsar+can*dcn*sar+dcan*cn*sar,
                sn*dsar+dsn*sar],
              [-can*dsar-dcan*sar-san*cn*dcar-san*dcn*car-dsan*cn*car,
               -san*dsar-dsan*sar+can*cn*dcar+can*dcn*car+dcan*cn*car,
                sn*dcar+dsn*car],
              [ san*dsn+dsan*sn,
               -can*dsn-dcan*sn, 
               dcn]];
    rotdot = transpose(rotdot, 3);
  }

  return axes;
}

func geocentric(xyz, time)
{
  if (!time) time = 0.0;
  s = (23.4393 - 3.563e-7 * time) * (pi/180.);
  c = cos(s);
  s = sin(s);
  xyze = xyz;
  xyze(2:3,..) = [[c,-s],[s,c]](+,) * xyz(+:2:3,..);
  return xyze;  /* xyze in geocentric coords, xyz in ecliptic coords */
}

local sch_planets, sch_moon;
/* DOCUMENT sch_planets, sch_moon
 * from "How to compute planetary positions",
 * by Paul Schlyter of Stockholm, Sweden
 * http://hotel04.ausys.se/pausch
 * "Please note that the orbital elements of Uranus and Neptune as given
 * here are somewhat less accurate.  They include a long period perturbation
 * between Uranus and Neptune.  The period of the perturbation is about
 * 4200 years."
 * After corrections in the case of the moon, jupiter, saturn, and uranus,
 * these are claimed to be accurate to under 1 arc minute for the inner
 * planets, about 1 arc minute for the outer planets, and 2 arc minutes
 * for the moon.
 */
sch_planets = [
  [168.6562,4.0923344368, 0.387098,0., 0.205635,5.59e-10,
   48.3313,3.24587e-5, 29.1241,1.01444e-5, 7.0047,5.00e-8],     /* mercury */
  [48.0052,1.6021302244, 0.723330,0., 0.006773,-1.302e-9,
   76.6799,2.46590e-5, 54.8910,1.38374e-5, 3.3946,2.75e-8],     /* venus */
  [356.0470,0.9856002585, 1.,0., 0.016709,-1.151e-9,
   0.,0., 102.9404,4.70935e-5, 0.,0.],                          /* earth */
  [18.6021,0.5240207766, 1.523688,0., 0.093405,2.516e-9,
   49.5574,2.11081e-5, 286.5016,2.92961e-5, 1.8497,-1.78e-8],   /* mars */
  [19.8950,0.0830853001, 5.20256,0., 0.048498,4.469e-9,
   100.4542,2.76854e-5, 273.8777,1.64505e-5, 1.3030,-1.557e-7], /* jupiter */
  [316.9670,0.0334442282, 9.55475,0., 0.055546,-9.499e-9,
   113.6634,2.38980e-5, 339.3939,2.97661e-5, 2.4886,-1.081e-7], /* saturn */
  [142.5905,0.011725806, 19.18171,-1.55e-8, 0.047318,7.45e-9,
   74.0005,1.3978e-5, 96.6612,3.0565e-5, 0.7733,1.9e-8],        /* uranus */
  [260.2471,0.005995147, 30.05826,3.313e-8, 0.008606,2.15e-9,
   131.7806,3.0173e-5, 272.8461,-6.027e-6, 1.7700,-2.55e-7],    /* neptune */
  [239.,0.00397, 39.5,0., 0.249,0.,
   110.,0., 114.,0., 17.1,0.]                                   /* pluto */
];
sch_moon = [
  [115.3654,13.0649929509, 60.2666,0., 0.054900,0.,
   125.1228,-0.0529538083, 318.0634,0.1643573223, 5.1454,0.], /* moon */
  [356.0470,0.9856002585, 1.,0., 0.016709,-1.151e-9,
   0.,0., 282.9404,4.70935e-5, 0.,0.]                         /* sun */
];

func moon(time, full=)
/* DOCUMENT xyz = moon(time)
     return position XYZ of the moon relative to center of earth
     at TIME; the XYZ has leading dimension 3; x is along the vernal
     equinox, z is ecliptic north.  The corrections to the lunar orbit
     are from Schlyter (see sch_moon).  Claimed accurate to 2 arc minutes
     over some reasonable time.  TIME is in days since 0/Jan/00 (that is,
     0000 UT 31/Dec/99).  This is 1.5 days earlier than the J2000 epoch.
   SEE ALSO: solar_system, sch_moon, kepler
 */
{
  local mm;
  xyz = kepler(sch_moon(,1), time, mm, full=full);
  rxy = abs(xyz(2,..),xyz(1,..));
  lon = atan(xyz(2,..), xyz(1,..));
  lat = atan(xyz(3,..), rxy);
  r = rr = abs(xyz(3,..), rxy);

  ms = (sch_moon(1,2) + sch_moon(2,2)*time) * (pi/180.);
  nm = (sch_moon(7,1) + sch_moon(8,1)*time) * (pi/180.);
  wm = (sch_moon(9,1) + sch_moon(10,1)*time) * (pi/180.);
  ws = (sch_moon(9,2) + sch_moon(10,2)*time) * (pi/180.);
  lm = mm+wm+nm;
  ls = ms+ws;
  d = lm-ls;
  f = lm-nm;

  lon += (-1.274*sin(mm-2.*d)   +0.658*sin(2.*d)      -0.186*sin(ms)
          -0.059*sin(2.*mm-2.*d)-0.057*sin(mm-2.*d+ms)+0.053*sin(mm+2.*d)
          +0.046*sin(2.*d-ms)   +0.041*sin(mm-ms)     -0.035*sin(d)
          -0.031*sin(mm+ms)     -0.015*sin(2.*f-2.*d) +0.011*sin(mm-4.*d)) *
    (pi/180.);
  lat += (-0.173*sin(f-2.*d)-0.055*sin(mm-f-2.*d)-0.046*sin(mm+f-2.*d)
          +0.033*sin(f+2.*d)+0.017*sin(2.*mm+f)) * (pi/180.);
  r += (-0.58*cos(mm-2.*d)-0.46*cos(2.*d));

  if (full) {
    dmm = sch_moon(2,1) * (pi/180.);
    drxy = (xyz(1:2,..)*xyz(4:5,..))(sum,..)/rxy
    dlon = (xyz(1,..)*xyz(5,..)-xyz(2,..)*xyz(4,..))/(rxy*rxy);
    dlat = (rxy*xyz(6,..)-xyz(3,..)*drxy)/(rr*rr);
    dr = (xyz(1:3,..)*xyz(4:6,..))(sum,..)/rr;

    dms = sch_moon(2,2) * (pi/180.);
    dnm = sch_moon(8,1) * (pi/180.);
    dwm = sch_moon(10,1) * (pi/180.);
    dws = sch_moon(10,2) * (pi/180.);
    dlm = dmm+dwm+dnm;
    dls = dms+dws;
    dd = dlm-dls;
    df = dlm-dnm;

    dlon += (-1.274*cos(mm-2.*d)*(dmm-2.*dd)   +0.658*cos(2.*d)*(2.*dd)
             -0.186*cos(ms)*(dms)
             -0.059*cos(2.*mm-2.*d)*(2.*dmm-2.*dd)-
             0.057*cos(mm-2.*d+ms)*(dmm-2.*dd+dms)+
             0.053*cos(mm+2.*d)*(dmm+2.*dd)
             +0.046*cos(2.*d-ms)*(2.*dd-dms)   +0.041*cos(mm-ms)*(dmm-dms)
             -0.035*cos(d)*(dd)
             -0.031*cos(mm+ms)*(dmm+dms)  -0.015*cos(2.*f-2.*d)*(2.*df-2.*dd)
             +0.011*cos(mm-4.*d)*(dmm-4.*dd)) *
      (pi/180.);
    dlat += (-0.173*cos(f-2.*d)*(df-2.*dd)-0.055*cos(mm-f-2.*d)*(dmm-df-2.*dd)
             -0.046*cos(mm+f-2.*d)*(dmm+df-2.*dd)
             +0.033*cos(f+2.*d)*(df+2.*dd)+0.017*cos(2.*mm+f)*(2.*dmm+df)) *
      (pi/180.);
    dr += (+0.58*sin(mm-2.*d)*(dmm-2.*dd)+0.46*sin(2.*d)*(2.*dd));

    xyz(4,..) = (dr*cos(lon)*cos(lat) - r*sin(lon)*cos(lat)*dlon -
                 r*cos(lon)*sin(lat)*dlat);
    xyz(5,..) = (dr*sin(lon)*cos(lat) + r*cos(lon)*cos(lat)*dlon -
                 r*sin(lon)*sin(lat)*dlat);
    xyz(6,..) = (dr*sin(lat) + r*cos(lat)*dlat);
  }

  xyz(1,..) = r*cos(lon)*cos(lat);
  xyz(2,..) = r*sin(lon)*cos(lat);
  xyz(3,..) = r*sin(lat);
  return xyz;
}

func solar_system(time)
/* DOCUMENT xyz = moon(time)
     return position XYZ of the moon relative to center of earth
     at TIME; the XYZ has leading dimension 3; x is along the vernal
     equinox, z is ecliptic north.  Corrections due to Schlyter (see
     sch_planets) are applied.  Claimed accurate to under 1 arc minute
     over some reasonable time.  TIME is in days since 0/Jan/00 (that is,
     0000 UT 31/Dec/99).  This is 1.5 days earlier than the J2000 epoch.
   SEE ALSO: solar_system, sch_moon, kepler
 */
{
  local mu;
  xyz = kepler(sch_planets, time, mu);

  jsu = xyz(,5:7,..);
  lon = atan(jsu(2,..), jsu(1,..));
  lat = atan(jsu(3,..), abs(jsu(2,..),jsu(1,..)));
  r = abs(jsu(3,..),jsu(2,..),jsu(1,..));

  mj = mu(5,..);
  ms = mu(6,..);
  mu = mu(7,..);

  /* note: 4220 year great uranus-neptune term
   * included in orbital elements for uranus and neptune */
  pio180 = pi/180;
  lon(1,..) += (-0.332*sin(2.*mj-5.*ms-67.6*pio180)
                -0.056*sin(2.*mj-2.*ms+21.*pio180)
                +0.042*sin(3.*mj-5.*ms+21.*pio180)
                -0.036*sin(mj-2.*ms)
                +0.022*cos(mj-ms)
                +0.023*sin(2.*mj-3.*ms+52.*pio180)
                -0.016*sin(mj-5.*ms-69*pio180)) * pio180;
  lon(2,..) += (+0.812*sin(2.*mj-5.*ms+67.6*pio180)
                -0.229*cos(2.*mj-4.*ms-2.*pio180)
                +0.119*sin(mj-2.*ms-3.*pio180)
                +0.046*sin(2.*mj-6.*ms-69.*pio180)
                +0.014*sin(mj-3.*ms+32.*pio180)) * pio180;
  lat(2,..) += (-0.020*cos(2.*mj-4.*ms-2.*pio180)
                +0.018*sin(2.*mj-6.*ms-49.*pio180)) * pio180;
  lon(3,..) += (+0.040*sin(ms-2.*mu+6.*pio180)
                +0.035*sin(ms-3.*mu+33.*pio180)
                -0.015*sin(mj-mu+20.*pio180)) * pio180;

  xyz(1,5:7,..) = r*cos(lon)*cos(lat);
  xyz(2,5:7,..) = r*sin(lon)*cos(lat);
  xyz(3,5:7,..) = r*sin(lat);

  /* schlyter's pluto model */
  s = (50.03+0.033459652*time)*pio180;
  p = (238.95+0.003968789*time)*pio180;
  lon = (238.9508 + 0.00400703*time
         -19.799*sin(p)   +19.848*cos(p)
         +0.897*sin(2.*p) -4.956*cos(2.*p)
         +0.610*sin(3.*p) +1.211*cos(3.*p)
         -0.341*sin(4.*p) -0.190*cos(4.*p)
         +0.128*sin(5.*p) -0.034*cos(5.*p)
         -0.038*sin(6.*p) +0.031*cos(6.*p)
         +0.020*sin(s-p)  -0.010*cos(s-p)) * pio180;
  lat = (-3.9082
         -5.453*sin(p)   -14.975*cos(p)
         +3.527*sin(2.*p) +1.673*cos(2.*p)
         -1.051*sin(3.*p) +0.328*cos(3.*p)
         +0.179*sin(4.*p) -0.292*cos(4.*p)
         +0.019*sin(5.*p) +0.100*cos(5.*p)
         -0.031*sin(6.*p) -0.026*cos(6.*p)
         +0.011*cos(s-p)) * pio180;
  r = (40.72
       +6.68*sin(p) +6.90*cos(p)
       -1.18*sin(2.*p) -0.03*cos(2.*p)
       +0.15*sin(3.*p) -0.14*cos(3.*p));
  
  xyz(1,9,..) = r*cos(lon)*cos(lat);
  xyz(2,9,..) = r*sin(lon)*cos(lat);
  xyz(3,9,..) = r*sin(lat);

  return xyz;
}

func day2000(y,m,d,ut)
{
  /* zero for 2000 Jan 0 0000 UT (1999 Dec 31 0000 UT) */
  y = long(y);  m = long(m);  d = long(d);
  if (is_void(ut)) ut = 0.0;
  return 367*y - 7*(y+(m+9)/12)/4 + 275*m/9 + d - 730530 + ut/24.0;
}

func earth_tilt(d)
{
  return 23.4393 - 3.563e-7*d;  /* degrees axis to ecliptic */
}

local jpl_planets;
/* DOCUMENT jpl_planets
 * orbital elements from http://ssd.jpl.nasa.gov/elem_planets.html
 * "Mean orbit solutions from a 250 yr. least squares fit of the
 * DE200 planetarty ephemeris to a Keplerian orbit where each element
 * is allowed to vary linearly with time.  This solution fits the
 * terrestrial planets to 25" or better, but achieves only 600" for
 * Saturn.  Elements are referenced to mean ecliptic and equinox
 * of J2000 at the J2000 epoch (2451545.0 JD)."
 * J2000 = 2000 January 1.5
 *
 * WARNING: these elements are 1.5 JD later than sch_planets
 *
 * definitions:
 * argument of perihelion = longitude of perihelion -
 *                          longitude of ascending node
 * mean anomaly = mean longitude - longitude of perihelion
 * 1 Julian century = 36525 days
 */
jpl_planets = [
  [174.79439,4.092334433949,0.38709893,1.806982e-11,0.20563069,6.918549e-10,
    48.33167,-3.394174e-06,29.12478, 7.75626e-06,7.00487,-1.7880e-07],
  [ 50.44675,1.602131301696,0.72333199,2.518823e-11,0.00677323,-1.351951e-09,
    76.68069,-7.581489e-06,54.85229, 6.75405e-06,3.39471,-2.1751e-08],
  [357.51716,0.985599987452,1.00000011,-1.368925e-12,0.01671022,-1.041478e-09,
   -11.26064,-1.386284e-04,114.20783,1.47742e-04,0.00005,-3.5699e-07],
  [ 19.41248,0.524021165108,1.52366231,-1.977002e-09,0.09341233,3.258590e-09,
    49.57854,-7.758689e-06,286.46230,1.96286e-05,1.85061,-1.9370e-07],
  [ 19.65053,0.083080374325,5.20336301,1.662888e-08,0.04839266,-3.526352e-09,
   100.55615, 9.256750e-06,274.19770,-2.86896e-06,1.30530,-3.1561e-08],
  [317.51238,0.033485450148,9.53707032,-8.255441e-08,0.05415060,-1.006489e-08,
   113.71504,-1.210016e-05,338.71690,-2.72142e-06,2.48446, 4.6467e-08],
  [142.26794,0.011721311354,19.19126393,4.162218e-08,0.04716771,-5.242984e-09,
    74.22988,-1.278728e-05,96.73436,2.27695e-05,0.76986,-1.5895e-08],
  [259.90868,0.005987479200,30.06896348,-3.427680e-08,0.00858587,6.872005e-10,
   131.72169,-1.150278e-06,273.24966,-5.27173e-06,1.76917,-2.7683e-08],
  [ 14.86205,0.003976577306,39.48168677,-2.105736e-08,0.24880766,1.770021e-09,
   110.30347,-2.838999e-07, 113.76329,-7.21880e-07, 17.14175, 8.4189e-08]
];

func lon_lat(xyz)
{
  lat = atan(xyz(3,..),abs(xyz(2,..),xyz(1,..)));
  lon = atan(xyz(2,..),xyz(1,..));
  return transpose([lon,lat], 2);
}

func lon_subtract(obj, ref)
{
  objp = obj(1,..);
  objp -= ref(1,..);
  return atan(sin(objp),cos(objp));
}
