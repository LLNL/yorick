/*
 * $Id: test2.i,v 1.1 2005-09-18 22:06:11 dhmunro Exp $
 * Highly vectorizing physics problem for timing Yorick.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func test2(npass)
/* DOCUMENT test2
         or test2, npass
     Given a slab divided into zones by parallel planes, and given a
     set of photon group boundary energies, compute the contribution of
     each zone to the radiation flux emerging from one surface of the
     slab.  If NPASS is given, the calculation is repeated that many
     times.  The zoning, photon group structure, opacities, and source
     functions are all computed arbitrarily, but the number of zones
     and groups are taken to be representative of a typical 1-D
     radiation transport calculation.  */
{
  if (is_void(npass) || npass<=0) npass= 1;

  now= split= exponential= array(0.0, 3);
  timer, now;
  n= npass;
  while (n--) esc= escout(zt, akap, srcfun);
  timer, now, split;
  timer_print, "Time per pass", split/npass, "Total time", split,\
    "Computing exponentials", exponential;

  if (esc(0,mxx)!=29 || !approx_eq(esc(0,max),0.001875213417) ||
      !approx_eq(esc(0,1),0.001694423767) ||
      !approx_eq(esc(0,0),5.434635527e-05) ||
      esc(1,mxx)!=39 || !approx_eq(esc(1,max),0.000440460566))
    write, "***WARNING*** values returned by escout are not correct";

  if (flxout(0,mxx)!=34 || !approx_eq(flxout(0,max),0.06615472064) ||
      !approx_eq(flxout(0,1),0.003187516911) ||
      !approx_eq(flxout(0,0),0.005280842058) ||
      flxout(1,mxx)!=29 || !approx_eq(flxout(1,max),0.001805157164))
    write, "***WARNING*** values of flxout are not correct";

  if (!approx_eq(min(tau(:-1,)),6.982090961e-05) ||
      !approx_eq(max(tau),45.80160946))
    write, "***WARNING*** values of tau are not correct";
}

func approx_eq(x, y)
{
  return (abs(x-y)/(abs(x+y)+1.e-35))<1.e-6;
}

#if 0
CPU seconds per pass:
            IDL    Yorick     Basis       DAP     FORTRAN(-O)
HP730        -      0.60       2.04      1.84       0.27  (0.60 -g)
Solbourne  2.81     1.90       6.02      5.90       1.00
    (varies by ~10%)

Using forum (Solbourne) on 8/Dec/92:
     forum[18] time /usr/local/lib/idl/bin.sunos.4.1.sun4/idl
     IDL. Version 2.3.0 (sunos sparc).
     Copyright 1989-1992, Research Systems, Inc.
     All rights reserved.  Unauthorized reproduction prohibited.
     Site: 1491.
     Licensed for use by: LLNL - X Division
     IDL> .rnew /home/miggle/munro/Yorick/include/test2
     % Compiled module: ESCOUT.
     % Compiled module: APPROX_EQ.
     % Compiled module: TEST2.
     % Compiled module: BNU.
     % Compiled module: OPACSET.
     % Compiled module: SPAN.
     % Compiled module: SPANL.
     % Compiled module: $MAIN$.
     IDL> test2,1
     IDL> exit
     3.2u 0.4s 0:12 29% 0+2496k 1+2io 1pf+0w
     forum[19] time time /usr/local/lib/idl/bin.sunos.4.1.sun4/idl
     IDL. Version 2.3.0 (sunos sparc).
     Copyright 1989-1992, Research Systems, Inc.
     All rights reserved.  Unauthorized reproduction prohibited.
     Site: 1491.
     Licensed for use by: LLNL - X Division
     IDL> .rnew /home/miggle/munro/Yorick/include/test2
     % Compiled module: ESCOUT.
     % Compiled module: APPROX_EQ.
     % Compiled module: TEST2.
     % Compiled module: BNU.
     % Compiled module: OPACSET.
     % Compiled module: SPAN.
     % Compiled module: SPANL.
     % Compiled module: $MAIN$.
     IDL> test2,11
     IDL> exit
     31.3u 0.9s 0:58 54% 0+3544k 0+0io 0pf+0w


     forum[20] time yorick
      Yorick ready.  For help type 'help'
     > #include "/home/miggle/munro/Yorick/include/test2.i"
     > test2,1
                    Timing Category     CPU sec  System sec    Wall sec
                      Time per pass       1.840       0.300       2.270
                         Total time       1.840       0.300       2.270
             Computing exponentials       0.600       0.120       0.760
      -----Total Elapsed Times-----       2.510       0.490      27.500
     > test2,10
                    Timing Category     CPU sec  System sec    Wall sec
                      Time per pass       1.932       0.050       1.986
                         Total time      19.320       0.500      19.860
             Computing exponentials       6.360       0.080       6.440
      -----Total Elapsed Times-----      21.860       1.020      53.080
     > quit
     21.9u 1.0s 0:59 38% 0+3560k 31+0io 85pf+0w


     forum[21] basis
     Basis    (basis, Version 921125)
     Run at 13:02:49 on 12/08/92 on the forum    machine, suffix 10797x
     Initializing Basis System
     Basis 7.0
     Initializing EZCURVE/NCAR Graphics
     Ezcurve/NCAR 2.0
     Basis> echo=0
     Basis> read /home/miggle/munro/Yorick/include/test2.bas
     End of input file /home/miggle/munro/Yorick/include/test2.bas
     Resuming input from TERMINAL
     Basis> test2(1)


        CPU (sec)   SYS (sec)
            6.017       0.300
     Basis> test2(10)

        CPU (sec)   SYS (sec)
           60.233       0.017
     Basis> end

        CPU (sec)   SYS (sec)
           68.617       0.917
     68.6u 0.9s 2:07 54% 0+6144k 19+1io 213pf+0w


     forum[22] dap
     DAP> read /home/miggle/munro/Yorick/include/test2.bas
     DAP> test2(1)

        CPU (sec)   SYS (sec)
            5.883       0.400
     DAP> test2(10)

        CPU (sec)   SYS (sec)
           59.533       0.000
     DAP> end
     69.1u 1.5s 1:47 65% 0+6488k 20+1io 218pf+0w


Yorick results (test2.bas) on tonto (HP730) 21:53 4/Dec/92
top showed SIZE/RES  764K/244K  at prompt
                    3636K/3052K after test2,1
                    4628K/4012K after test2,20
     tonto[9] yorick
      Yorick ready.  For help type 'help'
     > #include "/home/miggle/munro/Yorick/include/test2.i"
     > test2,1
                    Timing Category     CPU sec  System sec    Wall sec
                      Time per pass       0.550       0.100       0.670
                         Total time       0.550       0.100       0.670
             Computing exponentials       0.240       0.030       0.270
      -----Total Elapsed Times-----       0.930       0.220      34.950
     > test2,20
                    Timing Category     CPU sec  System sec    Wall sec
                      Time per pass       0.599       0.001       0.906
                         Total time      11.980       0.020      18.110
             Computing exponentials       5.030       0.000       7.270
      -----Total Elapsed Times-----      12.920       0.240      83.120

Basis results (test2.bas) on tonto (HP730) 21:53 4/Dec/92
top showed SIZE/RES 5356K/540K  at prompt
                    8960K/4108K after test2(1)
                    8960K/4112K after test2(20)
     tonto[8] basis
     Basis    (basis, Version 921125)
     Run at 21:45:42 on 12/04/92 on the tonto    machine, suffix 27713x
     Initializing Basis System
     Basis 7.0
     Initializing EZCURVE/NCAR Graphics
     Ezcurve/NCAR 2.0
     Basis> echo=0
     Basis> read /home/miggle/munro/Yorick/include/test2.bas
     End of input file /home/miggle/munro/Yorick/include/test2.bas
     Resuming input from TERMINAL
     Basis> test2(1)

        CPU (sec)   SYS (sec)
            1.990        .120
     Basis> test2(20)

        CPU (sec)   SYS (sec)
           40.800        .000

DAP results (test2.bas) on tonto (HP730) 21:53 4/Dec/92
top showed SIZE/RES 6796K/844K   at prompt
                    10360K/4352K after test2(1)
                    10360K/4352K after test2(20)
     tonto[11] dap
     DAP> echo=0
     DAP> read /home/miggle/munro/Yorick/include/test2.bas
     DAP> test2(1)

        CPU (sec)   SYS (sec)
            1.840        .080
     DAP> test2(20)

        CPU (sec)   SYS (sec)
           36.830        .000
#endif

/* This routine computes the optical depth through each zone at every
   frequency and then uses that to compute the radiation emitted in
   each zone that escapes from the problem, assuming plane parallel
   geometry.
   The returned result is an nzones-by-ngroups array of
   power per unit photon energy per unit area.  */
func escout(zt,      /* npoints zone boundary positions (cm) */
            akap,    /* nzones-by-ngroups opacities (1/cm) */
            srcfun)  /* nzones-by-ngroups source (specific intensity units) */
{
  extern flxout;     /* (output) nzones-by-ngroups outgoing fluxes */
  extern dtau;       /* (output) nzones-by-ngroups optical depths (ODs) */
  extern tau;        /* (output) npoints-by-ngroups cumulative ODs */

  extern mu, wmu;    /* Gauss-Legendre cos(theta) and weight arrays
                        for integration over escape angles */

  /* compute tau, the optical depth to each zone along the zt-direction */
  dtau= akap*zt(dif);
  tau= array(0.0, dimsof(zt), dimsof(akap)(3));
  tau(2:,)= dtau(psum,);
  /* consider the outward going radiation, and thus use tau
     measured from the right boundary */
  tau= tau(0,)(-,) - tau;

  /* compute exf, the fraction of the srcfun which escapes from
     each zone in each bin at each angle mu relative to the zt direction */
  enow= array(0.0, 3);
  timer, enow;  /* most of the actual work is computing these exponentials */
  exf= exp(-tau(,,-)/mu(-,-,));
  timer, enow, exponential;

  /* compute the escaping flux per unit surface area in each bin
     contributed by each zone -- units are 10^17 W/kev/cm^2  */
  /* Note:
       dI/dtau = -I + S   (S is srcfun, which is Bnu) implies
       I = integral of ( S * exp(tau) ) dtau from tau -infinity to tau 0
       Hence, the contribution of a single zone to this integral is
       S12 * (exp(tau1) - exp(tau2)), which explains the exf(dif,,) below.
       The wmu are Gauss-Legendre integration weights, and the mu is the
       cos(theta) to project the specific intensity onto the direction
       normal to the surface (zt).  */
  esfun= 2*pi*exf(dif,,)*srcfun(,,-)*(mu*wmu)(-,-,);

  /* Also compute what the total flux WOULD have been if each successive
     zone were at the surface.  This is the same as the "one-sided"
     flux directed outward across each zone boundary.  */
  fuzz= 1.0e-10;
  flxout= (esfun(psum,,)/(exf(2:,,)+fuzz))(,,sum);

  return esfun(,,sum);
}

/* This function is used to set a dummy opacity to be used by the
   transport calculation  */
func opacset(tmp, rho, gav, gb)
{
  extern srcfun;  /* (output) nzones-by-ngroups Bnu, LTE source function */
  extern akap;    /* (output) nzones-by-ngroups opacities (1/cm) */

  /* the opacity is proportional to the density to the rhopow
     power and the temperature to the tempow power  */
  rhopow= 2;
  tempow= 3;
  factr= (tmp/1.0)^tempow*(rho/1.0)^rhopow;

  /* set the constants in front of the terms for the two edges */
  con0= 1.0e+4;
  cona= 2.5e+2;
  conb= 5.0e+0;
  /* set the energies of the two edges */
  edg0= 0.1;
  edga= 0.5;
  edgb= 2.0;

  /* set up arrays that are zero below the edges and one above them  */
  vala= double(gav>edga);
  valb= double(gav>edgb);

  /* frequency dependence is the same for all zones, so calculate it once */
  frval= con0*(edg0/gav)^3+cona*vala*(edga/gav)^3+conb*valb*(edgb/gav)^3;

  /* set the opacity */
  akap= factr(,-)*frval(-,);
  /* the source function is always a blackbody */
  srcfun= bnu(tmp, gb);
}

/* Make a blackbody using the analytic formula --
   the units are 10^17 W/kev/g/ster */
func bnu(tmp, freqb)
{
  /* Note that tmp and freqb are one dimensional and the output
     array should be 2D  */
  /* compute the derivative using the exact black body,
     not the average over the bin  */
  exf= exp(-(freqb(zcen)(-,)/tmp));
  return 0.0504*(freqb(zcen)^3)(-,)*exf/(1.0-exf);
}

/* set up default params. etc. */

/* set the number of spatial zones and photon groups */
npoints= 100;
ngroups= 64;
nzones= npoints-1;

/* set the zone boundary positions */
zt= span(0.0, 0.0050, npoints);

/* set the frequency bins */
gb= spanl(0.1, 4.0, ngroups+1);
gav= gb(zcen);

/* set the density and temperature */
rho= spanl(1.0, 1.0, nzones);
tmp= spanl(1.0, 1.0, nzones);

/* set the opacity akap and source function srcfun */
opacset, tmp, rho, gav, gb;

/* set up for a Gauss-Legendre angular quadrature */
xmu= [0.1488743389, 0.4333953941, 0.6794095682, 0.8650633666, 0.9739065285];
mu= -xmu(::-1);
grow, mu, xmu;
xmu= [0.2955242247, 0.2692667193, 0.2190863625, 0.1494513491, 0.0666713443];
wmu= xmu(::-1);
grow, wmu, xmu;
/* correct the Gauss-Legendre abcissas-- interval is only 0 to 1 */
mu= 0.5 + 0.5*mu;
