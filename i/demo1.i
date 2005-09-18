/*
 * $Id: demo1.i,v 1.1 2005-09-18 22:05:55 dhmunro Exp $
 * 1-D hydro code written in Yorick language
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* ------------- Set a few parameters --------------------------- */

/* Set initial density and temperature.  */
rho0= 1.1845e-3;             /* g/cc dry air at NTP */
RT0= 298.16;                       /* Kelvin at NTP */

/* Set number of zones for hydro calculation, and
   total length of column.  */
n_zones= 200;
column_length= 100.0/*cm*/;

/* A conversion factor.  */
R_gas= 8.314e7;    /* erg/K/mole ideal gas constant */

/* Gamma-law gas equation of state parameters.  */
gamma= 1.4;                 /* Cp/Cv for air is 7/5 */
gammaM1= gamma - 1;
A_bar= 1.1845e-3/*g/cc dry air*/ * 24.5e3/*cc/mole*/;
K_to_v2= 1.e-6*R_gas/A_bar;     /* (cm/ms)^2/Kelvin
                                   for dry air      */
/* Local temperature units are (cm/ms)^2. --
   Note dependence of temperature units on A_bar.  */
RT0*= K_to_v2;

/* ------------- Set initial conditions --------------------------- */

/* Set initial conditions for hydro calculation.  */
func reset
{
  extern RT, z, v, M, p;

  RT= array(RT0, n_zones);           /* temperature */
  z= span(0, column_length, n_zones+1);     /* zone
                               boundary coordinates */
  v= array(0.0, dimsof(z)); /* zone bndy velocities */

  /* The column consists initially of n_zones zones
     of equal size containing equal amounts of gas. */
  M= rho0*abs(z(2)-z(1));         /* mass/area/zone */

  /* The pressure array includes a pressure before and
     after the column, p(0) and p(n_zones+1), which
     will be used to set pressure boundary conditions.
     (Velocity boundary conditions will be applied by
     setting v(0) and v(n_zones).)
     Note that the units of this pressure are
          g*(cm/ms)^2/cc.  */
  p= array(rho0*RT0, n_zones+2);
}

/* ------------- Set boundary conditions --------------------------- */

func sound
/* DOCUMENT sound
     Set up the initial conditions for evolve to launch a weak sound wave.

   SEE ALSO: shock, evolve
 */
{
  extern bc0_v, bc0_time, bc0_p, bc0_Z;
  bc0_v= 0.1*sin(span(0, 2*pi, 100));
  bc0_time= span(0, 1.0, 100);
  bc0_p= bc0_Z= [];
  reset;
}

func shock
/* DOCUMENT sound
     Set up the initial conditions for evolve to launch a strong wave, which
     steepens into a shock as it propagates.

   SEE ALSO: sound, evolve
 */
{
  extern bc0_v, bc0_time, bc0_p, bc0_Z;
  bc0_v= 10.0*sin(span(0, 2*pi, 100));
  bc0_time= span(0, 1.0, 100);
  bc0_p= bc0_Z= [];
  reset;
}

sound;

func nobc {
  extern bcN_v, bcN_p, bcN_Z;
  bcN_v= bcN_p= bcN_Z= [];
}

func hardbc {
  extern bcN_v, bcN_p, bcN_Z;
  bcN_v= 0;
  bcN_p= bcN_Z= [];
}

func softbc {
  extern bcN_v, bcN_p, bcN_Z;
  bcN_p= rho0*RT0;
  bcN_v= bcN_Z= [];
}

func matchbc {
  extern bcN_v, bcN_p, bcN_Z;
  softbc;
  bcN_Z= rho0*sqrt((gammaM1+1)*RT0);
}

/* ------------- Define the main function --------------------------- */
/* The DOCUMENT comment will be printed in response to:  help, evolve */

func evolve(time1, time0)
/* DOCUMENT evolve, time1
         or evolve, time1, time0
     Step the hydro calculation forward to TIME1,
     starting with the initial conditions in the
     RT, z, and v arrays at time TIME0 (default 0.0
     if omitted).  The calculation also depends on
     the constants M (mass/area/zone) and gammaM1
     (gamma-1 for the gamma-law equation of state).
     The pressure array p is updated in addition to
     the primary state arrays RT, z, and v.

     Boundary conditions are specified by setting
     either a boundary pressure or a boundary
     velocity at each end of the fluid column.
     bc0_v   - Boundary velocity at z(0), or []
               if z(0) has pressure BC.
     bc0_p   - Boundary pressure beyond z(0).
     bc0_time  - If bc0_v or bc0_p is an array,
                 bc0_time is an array of the same
                 length specifying the corresponding
                 times for time dependent BCs.
     bc0_Z   - Acoustic impedance at z(0) if bc0_v
               is nil (default is 0).
     bcN_v, bcN_p, bcN_time, and bcN_Z have the same
     meanings for the z(n_zones) boundary.

     The worker routines OutputResults and
     TakeStep must be supplied.
 */
{
  if (is_void(time0)) time0= 0.0;

  for (time=time0 ; ; time+=dt) {
    dt= GetTimeStep();
    SetBoundaryValues, time, dt;
    OutputResults, time, dt;
    if (time >= time1) break;
    TakeStep, dt;
  }
}

/* ----------------- compute time step --------------------- */

func GetTimeStep(dummy)
{
  dz= abs(z(dif));
  dv= abs(v(dif));
  cs= sqrt((gammaM1+1)*RT);
  return min( dz / max(courant*cs, accuracy*dv) ) * dt_multiplier;
}
/* Set reasonable default values for courant and
   accuracy parameters.  */
courant= 2.0;  /* number of cycles for sound signal
                  to cross one zone -- must be >=2.0
                  for numerical stability */
accuracy= 3.0; /* number of cycles for zone volume to
                  change by a factor of ~2 -- must be
                  >1.0 to avoid possible collapse to
                  zero volume */
dt_multiplier= 1.0;

/* ----------------- set boundary conditions ------------------ */

func SetBoundaryValues(time, dt)
{
  vtime= time + 0.5*dt;  /* velocity is 1/2 step
                            ahead of pressure, z */

  /* boundary at z(0) */
  if (!is_void(bc0_v)) {
    /* velocity BC */
    v(1)= BCinterp(bc0_v, bc0_time, vtime);

  } else if (!is_void(bc0_p)) {
    /* pressure BC */
    p(1)= BCinterp(bc0_p, bc0_time, time);
    /* acoustic impedance BC */
    if (!is_void(bc0_Z))
      p(1)-= sign(z(2)-z(1))*bc0_Z*v(1);
  }

  /* boundary at z(n_zones) (written here as z(0)) */
  if (!is_void(bcN_v)) {
    v(0)= BCinterp(bcN_v, bcN_time, vtime);

  } else if (!is_void(bcN_p)) {
    p(0)= BCinterp(bcN_p, bcN_time, time);
    if (!is_void(bcN_Z))
      p(0)+= sign(z(0)-z(-1))*bcN_Z*v(0);
  }
}

func BCinterp(values, times, time)
{
  if (numberof(times)<2) return values(1);
  else return interp(values, times, time);
}

/* ----------------- produce output ----------------------- */

func OutputVPlot(time, dt)
{
  extern cycle_number;
  if (time==time0) cycle_number= 0;
  else cycle_number++;
  if (!(cycle_number%output_period)) {
    fma;
    plg, v, z;
    zx= sqrt((gammaM1+1)*RT0)*time;
    if (zx>max(z)) {
      /* make a dotted "ruler" at the location a sound wave
         would have reached */
      zx= 2*max(z)-zx;
      if (zx<min(z)) zx= 2*min(zx)-zx;
    }
    pldj, zx, min(bc0_v), zx, max(bc0_v), type="dot";
  }
}
output_period= 8;  /* about 50 frames for wave to
                      transit 200 zones */

/* OutputResults can be switched among several possibilities */
OutputResults= OutputVPlot;

/* ----------------- 1-D hydro worker ----------------------- */

func TakeStep1(dt)
{
  /* velocities 1/2 step ahead of coordinates */
  z+= dt * v;
  dv= v(dif);
  dz= z(dif);

  /* Compute artificial viscosity.  */
  q= array(0.0, n_zones+2);
  q(2:-1)= q_multiplier * M * max(-dv/dz, 0.0)*abs(dv);

  /* Apply 1st law of thermodynamics to compute
     temperature change from work p*v(dif) done
     on zone.  Note that p is not time-centered
     properly here.  */
  RT-= (dt * gammaM1/M) * (p+q)(2:-1) * dv;

  /* Update the pressures from the updated densities
     M/z(dif) and temperatures RT.  Note that p(0)
     and p(-1) updated by SetBoundaryValues.  */
  p(2:-1)= M * RT/dz;

  /* Apply Newton's 2nd law to update velocities.  */
  v-= (dt/M) * (p+q)(dif);
}
q_multiplier= 1.0;

TakeStep= TakeStep1;

/* ----------------- simple interface ----------------------- */

func demo1
/* DOCUMENT demo1
     run the 1-D hydrocode demonstration.  An X window should pop up
     in which a movie of a wave propagating down a shock tube appears.
     Use the 'sound' and 'shock' commands to set two different interesting
     initial conditions; the default is 'sound'.

   SEE ALSO: sound, shock, evolve
 */
{
  window, 0, wait=1;
  reset;
  evolve, 5;
}

/* ----------------- end of demo1.i ----------------------- */
