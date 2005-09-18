/*
 * $Id: test1.i,v 1.1 2005-09-18 22:06:10 dhmunro Exp $
 * Poorly vectorizing physics problem for timing Yorick.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* WARNING -- this code assumes 1-origin indexing is the default */

func test1(npass)
/* DOCUMENT test1
         or test1, npass
     Track a mock "ablation front" as it propagates through a mesh.
     If NPASS is given, the calculation is repeated that many
     times.  The zoning, densities, temperatures, pressures, and
     velocities are all computed arbitrarily, but the number of zones
     and groups are taken to be representative of a typical 1-D
     ablation calculation.  */
{
  if (is_void(npass) || npass<=0) npass= 1;

  now= split= array(0.0, 3);
  timer, now;
  n= npass;
  while (n--) {
    prevScale= prevAbl= 37;
    for (i=1 ; i<=ntimes ; i++) accum, i;
  }
  timer, now, split;
  timer_print, "Time per pass", split/npass, "Total time", split;

  if (prevAbl!=15 || prevScale!=8 ||
      nallof(approx_eq(rorab,
                       [2.673480885e-03,2.566922597e-03,2.422313787e-03,
                        2.211817497e-03,1.870390096e-03])) ||
      nallof(approx_eq(times,1.5)) ||
      nallof(approx_eq(rordx,3.109107238e-03)) ||
      nallof(approx_eq(rorsx,2.946068988e-03)) ||
      nallof(approx_eq(rorhx,2.916081687e-03)) ||
      nallof(approx_eq(dmax,3.034383698e-01)) ||
      nallof(approx_eq(zs,5.973585920e-03)) ||
      nallof(approx_eq(vel,-1.662706089e-01)) ||
      nallof(approx_eq(acc,-3.148890163e-03)) ||
      nallof(approx_eq(scl,2.061472601e-03)))
    write, "***WARNING*** values returned by accum are not correct";
}

func approx_eq(x, y)
{
  return (abs(x-y)/(abs(x+y)+1.e-35))<1.e-6;
}

#if 0
CPU seconds per pass:
            IDL    Yorick     Basis       DAP
HP730        -      0.41       8.21      8.15
Solbourne  0.57     1.00       20.1      22.4  (1st trial)
Solbourne  0.57     0.90       19.5      20.9  (2nd trial)
Note:  Times on Solbourne fluctuate by ~10%

Run on forum (Solbourne) 8/Dec/92
     forum[4] time /usr/local/lib/idl/bin.sunos.4.1.sun4/idl
     IDL. Version 2.3.0 (sunos sparc).
     Copyright 1989-1992, Research Systems, Inc.
     All rights reserved.  Unauthorized reproduction prohibited.
     Site: 1491.
     Licensed for use by: LLNL - X Division
     IDL> .rnew /home/miggle/munro/Yorick/include/test1
     % Compiled module: PEAK.
     % Compiled module: DERIV.
     % Compiled module: ACCUM.
     % Compiled module: APPROX_EQ.
     % Compiled module: TEST1.
     % Compiled module: INTERP.
     % Compiled module: UNCP38.
     % Compiled module: $MAIN$.
     IDL> test1,1
     IDL> exit
     1.1u 0.3s 0:28 5% 0+1264k 4+0io 5pf+0w
     forum[5] !!
     time /usr/local/lib/idl/bin.sunos.4.1.sun4/idl
     IDL. Version 2.3.0 (sunos sparc).
     Copyright 1989-1992, Research Systems, Inc.
     All rights reserved.  Unauthorized reproduction prohibited.
     Site: 1491.
     Licensed for use by: LLNL - X Division
     IDL> .rnew /home/miggle/munro/Yorick/include/test1
     % Compiled module: PEAK.
     % Compiled module: DERIV.
     % Compiled module: ACCUM.
     % Compiled module: APPROX_EQ.
     % Compiled module: TEST1.
     % Compiled module: INTERP.
     % Compiled module: UNCP38.
     % Compiled module: $MAIN$.
     IDL> test1,21
     IDL> exit
     12.5u 0.2s 0:30 42% 0+1432k 0+0io 0pf+0w


     forum[6] yorick
      Yorick ready.  For help type 'help'
     > #include "/home/miggle/munro/Yorick/include/test1.i"
     > test1
                    Timing Category     CPU sec  System sec    Wall sec
                      Time per pass       1.010       0.000       1.020
                         Total time       1.010       0.000       1.020
      -----Total Elapsed Times-----       1.670       0.140      20.270
     > test1,21
                    Timing Category     CPU sec  System sec    Wall sec
                      Time per pass       1.001       0.001       1.004
                         Total time      21.020       0.030      21.080
      -----Total Elapsed Times-----      22.710       0.180      51.760
     > test1
                    Timing Category     CPU sec  System sec    Wall sec
                      Time per pass       0.990       0.000       0.990
                         Total time       0.990       0.000       0.990
      -----Total Elapsed Times-----      23.720       0.190     163.440
     > test1
                    Timing Category     CPU sec  System sec    Wall sec
                      Time per pass       0.990       0.010       1.000
                         Total time       0.990       0.010       1.000
      -----Total Elapsed Times-----      24.740       0.200     171.340
     > quit
     24.7u 0.2s 3:35 11% 0+1088k 18+0io 64pf+0w


     forum[7] basis
     Basis    (basis, Version 921125)
     Run at 10:06:44 on 12/08/92 on the forum    machine, suffix 10470x
     Initializing Basis System
     Basis 7.0
     Initializing EZCURVE/NCAR Graphics
     Ezcurve/NCAR 2.0
     Basis> echo=0
     Basis> read /home/miggle/munro/Yorick/include/test1.bas
     End of input file /home/miggle/munro/Yorick/include/test1.bas
     Resuming input from TERMINAL
     Basis> test1(1)

        CPU (sec)   SYS (sec)
           20.150       0.000
     Basis> end

        CPU (sec)   SYS (sec)
           23.117       0.467
     23.1u 0.4s 1:12 32% 0+2408k 20+0io 229pf+0w


     forum[8] dap
     DAP> read /home/miggle/munro/Yorick/include/test1.bas
     DAP> test1(1)

        CPU (sec)   SYS (sec)
           22.383       0.017
     DAP> end
     26.7u 1.1s 1:07 41% 0+2784k 17+0io 209pf+0w


Run on tonto (HP730) 7/Dec/92
     tonto[8] basis
     Basis    (basis, Version 921125)
     Run at 16:06:31 on 12/07/92 on the tonto    machine, suffix 2545x
     Initializing Basis System
     Basis 7.0
     Initializing EZCURVE/NCAR Graphics
     Ezcurve/NCAR 2.0
     Basis> echo=0
     Basis> read /home/miggle/munro/Yorick/include/test1.bas
     End of input file /home/miggle/munro/Yorick/include/test1.bas
     Resuming input from TERMINAL
     Basis> test1(1)

        CPU (sec)   SYS (sec)
            8.260        .010
     Basis> test1(10)

        CPU (sec)   SYS (sec)
           82.090        .020
     91.7u 0.1s 4:07 37%


     tonto[9] dap
     DAP> echo=0
     DAP> read /home/miggle/munro/Yorick/include/test1.bas
     DAP> test1(1)

        CPU (sec)   SYS (sec)
            8.130        .000
     DAP> test1(10)

        CPU (sec)   SYS (sec)
           81.520        .050
     DAP> end
     91.5u 0.6s 3:02 50%


     tonto[10] yorick
      Yorick ready.  For help type 'help'
     > #include "/home/miggle/munro/Yorick/include/test1.i"
     > test1
                    Timing Category     CPU sec  System sec    Wall sec
                      Time per pass       0.400       0.000       0.410
                         Total time       0.400       0.000       0.410
      -----Total Elapsed Times-----       0.720       0.070      14.810
     > test1,20
                    Timing Category     CPU sec  System sec    Wall sec
                      Time per pass       0.412       0.000       0.599
                         Total time       8.240       0.000      11.980
      -----Total Elapsed Times-----       8.960       0.070      32.350
#endif

/* ------------------- Ablation front tracker ---------------------- */

/* The ablation front location (rorab) is defined here by the condition that
   the matter temperature reaches a particular fraction of the current drive
   temperature.  Five different hypotheses about this fraction (.4 thru .8)
   are calculated simultaneously.  These can be compared with the ablation
   front location (rorhx) defined by the location of peak phot (X-ray
   absorption), or defined by the steepest gradient (rorsx), or to the
   location of maximum density (rordx).  Other useful parameters are also
   recorded for later perusal.  */
func accum(irec)
{
  /* Input arrays are external -- they would ordinarily be read from
     successive time history dumps of a hydro program.  */
  /* time-independent input quantities */
  extern kmax, lmax;   /* number of points for mesh arrays */
  extern fraction;     /* list of fractions for front definition */
  extern ror;          /* column density coordinate */

  /* Since the direction of the ablation is known, the various searches
     begin at the location of the fron on the previous call to accum;
     these are stored in the following two external variables, which
     must be initialized to 37 before the loop calling accum begins.  */
  extern prevScale, prevAbl;

  /* time-dependent input quantities */
  extern time;         /* current problem time */
  extern zt;           /* position (point centered mesh array) */
  extern v;            /* velocity (point centered mesh array) */
  extern s;            /* radiation temperature (zone centered mesh array) */
  extern e;            /* matter temperature (zone centered mesh array) */
  extern d;            /* density (zone centered mesh array) */
  extern p;            /* pressure (zone centered mesh array) */
  extern h;            /* heating (zone centered mesh array) */

  /* output quantities */
  extern times;        /* list of problem times */
  extern rorab;        /* nfraction-by-ntimes ablation depths (rho*r) */
  extern rordx;        /* ntimes rho*r at peak density */
  extern rorsx;        /* ntimes rho*r at steepest density gradient */
  extern rorhx;        /* ntimes rho*r at peak phot (heating rate) */
  extern dmax;         /* ntimes peak densities */
  extern zs;           /* nfraction-by-ntimes ablation front positions */
  extern vel;          /* ntimes unablated slab velocities */
  extern acc;          /* ntimes unablated slab accelerations */
  extern scl;          /* ntimes density scale lengths at front */

  mid = (kmax+1)/2;    /* slab is ablating in l-direction -- calculate
                          front for k-zone in middle */
  nfr= numberof(fraction);
  rorzc= ror(zcen);

  /* save time */
  times(irec) = time;

  /* get choices for ablation temperatures */
  abltemp= fraction*s(mid,lmax);
  epc= e(mid,2:lmax-1);

  i= prevAbl-1;  /* epc indices 1 less than e indices */
  for (j=nfr ; j>0 ; j--) {
    ablt= abltemp(j);
    while (epc(i)>ablt) i--;  /* find first zone with lower temperature */
    if (j==nfr) prevAbl= i+1;
    rorab(j,irec)= rorzc(i) +
      (ablt-epc(i))*(rorzc(i+1)-rorzc(i))/(epc(i+1)-epc(i));
  }

  /* get peak density and its location */
  dmax(irec)= d(mid, max);
  rordx(irec)= peak(rorzc, d(mid,2:));

  /* get location of peak phot heating */
  rorhx(irec) = peak(rorzc(1:35), h(mid,2:36))

  /* record position, velocity, and acceleration of zone at peak density */
  i= d(mid, mxx);
  zs(irec)= 0.5*(zt(mid,i)+zt(mid,i-1));
  vel(irec)= 0.5*(v(mid,i)+v(mid,i-1));
  acc(irec)= (0.5*(p(mid,i+1)+p(mid,i-1))-p(mid,i))/(ror(i)-ror(i-1));

  /* get density scale height, location of steepest gradient */
  i= min(36, prevScale+4);
  rscl= deriv(d(mid,2:i), rorzc(1:i-1));
  scl(irec)= 1.0/(max(rscl)+1.e-35);
  rorsx(irec) = peak(ror(2:i-1), rscl);
  prevScale= rscl(mxx)+1;

  return;
}

/* Mock up realistic ablation problem.  */

ntimes= 84;
kmax= 11;
lmax= 38;  /* this cannot be changed */

time= 1.5;

fraction= [.4,.5,.6,.7,.8];  /* MUST be in increasing order */

/* the following values represent a very rough fit to an "actual"
   ablating slab problem (that is, the overall shapes of the curves
   are realistic, although the numerical values are not consistent) */
x= span(1,38,38);
y= 4.e-4*atan(x/8) + 1.7e-3*atan((x-16)/3);
y= (2./pi)*(y - y(1));    /* rho*r coordinate */
ror= y(37)-y;             /* ror measured from right (ablating) side */

/* v and p were originally fit on a uniform rho*r grid */
yeven= span(min(y), max(y), 38);
tmp= array(0.0, 38);
tmp(1:35)= -0.2 + 0.4*(x(1:35)-x(1))/(x(35)-x(1));
tmp(35:38)= 0.2 + 0.3*(x(35:38)-x(35))/(x(38)-x(35));
v= interp(tmp, yeven, y)(-:1:kmax,);

/* original fits to density, pressure, temperature, and heating were for
   point centered data -- the strict uncp algorithm will not work here,
   however, since the input was not actually formed as pairwise averages
   of another array... */
func uncp38(xpc)
{
  x= array(0.0, kmax, 38);
  /* here is "true" uncp algorithm:
     x(2,2)= xpc(1);
     for (i=3 ; i<=38 ; i++) x(2,i)= 2*xpc(i-1)-x(2,i-1);
     x(3:,)= x(2,-,);
   */
  x(2:,2:)= xpc(-,zcen);
  return x;
}

dpc= exp(-40./(x+1)^2 - 50./(x-41)^2 + 1.0-0.22*x);
d= uncp38(dpc);

tmp= -0.01/(x+7)^2 - 0.0001/(x-41)^2;
tmp= tmp-tmp(1) + (tmp(1)-tmp(38))*(x-x(1))/(x(38)-x(1));
tmp(2:10)= tmp(2) + (tmp(10)-tmp(2))*(x(2:10)-x(2))/(x(10)-x(2));
ppc= interp(tmp, yeven, y);
p= uncp38(ppc);

tmp= ppc/dpc;
tmp(19:38)= tmp(19)*(1.0 + 0.1*(x(19:38)-x(19))/(x(38)-x(19)));
tmp(1:5)= max(tmp)/25. + (tmp(5)-max(tmp)/25.)*(x(1:5)-x(1))/(x(5)-x(1));
e= s= uncp38(tmp);

tmp(1:9)= 3.6*(x(1:9)-x(1))/(x(9)-x(1));
tmp(9:20)= 3.6 - 2.6*(x(9:20)-x(9))/(x(20)-x(9));
tmp(21:33)= tmp(20);
tmp(33:38)= 1.0 - (x(33:38)-x(33))/(x(38)-x(33));
h= uncp38(tmp);

/* declare arrays which will hold results of the calculation */
times= array(0.0, ntimes);
rorab= array(0.0, numberof(fraction), ntimes);
rordx= rorhx= rorsx= acc= vel= scl= zs= dmax= 0.0*times;

/* work backwards from d and rhor*r to get zt coordinate */
zt= array(0.0, kmax, 38);
zt(,2:)= (y(dif)/d(2,2:))(-,psum);

/* compute parabolic maximum of a curve
     peak = value of xin where yin is maximum   */
func peak(xin,yin)
{
  imax= yin(mxx);
  if (imax==1 || imax==numberof(yin)) return double(imax);
  ymax= yin(imax);
  dy1= ymax-yin(imax-1);
  dy2= yin(imax+1)-ymax;
  xmax= xin(imax);
  dx1= xmax-xin(imax-1);
  dx2= xin(imax+1)-xmax;
  return xmax - (dy2*dx1*dx1+dy1*dx2*dx2)/(dy2*dx1-dy1*dx2);
}

/*   deriv(y,x) = derivative of y wrt x
  example:
    acc = deriv( deriv(position,time) , time)
       puts acc equal to the second derivative of position wrt time  */
func deriv(y,x)
{
  return y(dif)/x(dif);
}
