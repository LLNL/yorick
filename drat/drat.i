/*
 * $Id: drat.i,v 1.3 2006-10-19 04:29:18 dhmunro Exp $
 * Yorick interface definitions for Drat transport equation solver.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*= SECTION() 2D transport equation solver =================================*/

if (!is_void(plug_in)) plug_in, "drat";

/* Note: Constructions of the following form --
   { extern a, b, c; }
   -- throughout this file prevent Codger from seizing an extern
   or struct statement that is not intended for it.  In one case,
   a local statement is similarly protected for the mkdoc function.  */

/* ------------------------------------------------------------------------ */

/* First section defines the highest level user interface routines:

   streak  -- return complete solution given dump file containing
              the mesh, opacity, and source functions
   snap    -- return time integrated solution given dump file
              containing the mesh, opacity, and source functions
   streak_save    -- like streak, but places result directly in
                     an output file

   guess_symmetry -- given dump file containing the mesh
 */

func streak(f, rays, slimits)
/* DOCUMENT streak(f, rays)
         or streak(f, rays, slimits)
     returns the transparency and self-emission as functions of time for
     the rad-hydro problem dumped in file F, on the specified RAYS, with
     the specified limits SLIMITS on the transport integrals.

     The first dimension of RAYS may be length 3, 5, or 6 to represent
     the ray(s) in TDG/DIRT coordinates (x,y,theta), "best" coordinates
     (x,y,z,theta,phi), or internal coordinates (cos,sin,y,z,x,r),
     respectively.  The remaining dimensions of RAYS, if any, will be
     called "nrays" below.

     The SLIMITS parameter, if present, is the value of the s-coordinate
     -- position along the ray -- at which to start and stop the
     integration of the transport equation.  SLIMITS may be nil, a 1-D
     array of length 2, or a 2-by-nrays array.  Each component of SLIMITS
     is [s_start, s_stop]; if s_stop<s_start, the integration will run
     from minus infinity to plus infinity, which is the default.  The
     s-coordinate is measured from the reference point if RAYS is given
     in "best" or internal format, or from the point of closest approach
     to the origin r=z=0 if RAYS is given in TDG/DIRT format.

     The variables rt, zt, akap, and ekap must be present in the
     history file F, and represent, respectively, cylindrical coordinate,
     axial coordinate, absorption opacity, and source function.  The mesh
     (rt,zt) is a kmax-by-lmax array.  The opacity akap must be in units
     of inverse length (e.g.- 1/rt or 1/zt); its dimensions should be
     kmax-by-lmax-by-ngroup.  The source function ekap must be in units
     of specific intensity -- power per unit area per unit solid angle
     per unit spectrum, or "B nu units"; ekap must have the same dimensions
     as akap.  (The akap and ekap arrays may be padded so that they are
     actually (longer than kmax*lmax)-by-ngroup to handle outdated storage
     formats.)  The names "rt", "zt", "akap", and "ekap" may be changed
     by setting the drat_rt, drat_zt, drat_akap, and drat_ekap options.
     The ireg array, if present in F, is also used.

     The result has dimension ngroup-by-2-by-nrays-by-ntimes, where
     ntimes is the number of history records in the file F, nrays is
     related to the dimensions of RAYS as above, and NGROUP is
     the number of groups in akap and ekap.  The first component of
     the length-2 second dimension is the transparency (between 0 and 1),
     and the second component is the self-emission (which has the same
     units as ekap, specific intensity).

     Many external variables modify the behavior of streak.  Each of
     these options is separately documented.  The drat_glist option
     can be used to select a subset of ngroup, which can obviously
     reduce the processing time.  The drat_compress option can be used
     to reduce the size of the result, so that the amount of data
     saved at each time is less than ngroup-by-2-by-nrays.

     If you set any options, be careful.  The reset_options functions
     clears all options back to their default values;
   OPTIONS: drat_rt, drat_zt, drat_ireg, drat_akap, drat_ekap,
            drat_ireg, drat_isymz, drat_khold, drat_lhold,
            drat_amult, drat_emult, drat_omult, drat_ireg_adj,
            drat_start, drat_stop, drat_symmetry, drat_glist,
            drat_linear, drat_nomilne,
            drat_integrate, drat_compress, drat_quiet

     The include file multi.i defines an extension to the streak function
     for allowing the opacities and emissivities to be read from several
     history files.  Include multi.i, then get help on multi_streak.

   SEE ALSO: reset_options, snap, streak_save, integ_flat, integ_linear
             streak_times, form_rays, best_rays, dirt_rays, internal_rays,
             apply_funcs
 */
{
  dummy= use_origins(0);

  /* put rays in internal format now to avoid conversion
     in integ_flat or integ_linear at every time */
  irays= internal_rays(rays);

  vars= get_vars(f);

  /* check to see whether ireg region number array is present */
  { extern drat_ireg; }    /* option */
  ireg= [];
  if (drat_ireg && is_present(vars, drat_ireg)) has_ireg= 1;
  else has_ireg= 0;
  has_time= (vars(2) && anyof(*vars(2)=="time"));

  result= [];

  /* initialize the mesh structure */
  mesh= alloc_mesh(f, vars);

  /* loop over problem times, each record in f corresponds to one time */
  { local times; }   /* complete list of times available to snap_worker
                        or other drat_compress as an extern */
  times= streak_times(f);
  n= numberof(times);
  if (!drat_quiet)
    write, "Processing "+print(n)(1)+" records, now on record:";
  for (i=1 ; i<=n ; i++) {
    if (!drat_quiet) {
      write, format=" %3ld", i;
      if (i%19 == 0) write, format="%s\n", "";
    }
    jt, f, times(i);
    time= has_time? f.time : times(i);  /* times(i) may not be exact */

    /* update the mesh */
    if (has_ireg) ireg= adjust_ireg(get_member(f, drat_ireg));
    update_mesh, mesh, get_member(f, drat_rt), get_member(f, drat_zt), ireg;

    /* integrate the transport equation */
    instant= drat_integrate(f, mesh, time, irays, slimits);

    /* compress result if requested */
    { extern drat_compress; }     /* option */
    if (!is_void(drat_compress))
      instant= drat_compress(instant(,1,..), instant(,2,..), time);

    /* accumulate result */
    if (!is_void(instant)) {
      if (is_void(result))
        result= array(structof(instant), dimsof(instant), n);
      result(..,i)= instant;
    }
  }
  if (!drat_quiet && i%19 != 0) write, format="%s\n", "";

  return result;
}

func default_integrate(f, mesh, time, irays, slimits)
/* DOCUMENT atten_emit= default_integrate(f, mesh, time, irays, slimits)
     is the default drat_integrate routine.
     On entry, file F is positioned at TIME, from which MESH has already
     been read.  IRAYS and SLIMITS are the rays coordinates (in internal
     format) and integration limits.
     The result should be ngroup-by-2-by-raydims, where the second index
     is 1 for the attenuation factor, 2 for the self-emission (specific
     intensity due to emission along the ray).
   OPTIONS: drat_linear, drat_ocompute, drat_oadjust,
            drat_emult, drat_amult, drat_omult, drat_nomilne,
            drat_ekap, drat_akap, drat_glist
   SEE ALSO: streak
 */
{
  /* read in the opacities and source functions for this time
     -- also applies opacity multipliers and point centers source
     function if necessary */
  { local opac, source; }   /* returned by get_kaps */
  get_kaps, f, mesh, time;

  /* do transport integral for these rays cutting through current mesh */
  integrator= drat_linear? integ_linear : integ_flat;
  return integrator(opac, source, irays, mesh, slimits);
}

func snap(f, rays, slimits)
/* DOCUMENT snap(f, rays)
         or snap(f, rays, slimits)
     returns the time-integrated specific intensity for the rad-hydro
     problem dumped in file F, on the specified RAYS, with the
     specified limits SLIMITS on the transport integrals.

     The first dimension of RAYS may be length 3, 5, or 6 to represent
     the ray(s) in TDG/DIRT coordinates (x,y,theta), "best" coordinates
     (x,y,z,theta,phi), or internal coordinates (cos,sin,y,z,x,r),
     respectively.  The remaining dimensions of RAYS, if any, will be
     called "nrays" below.

     The SLIMITS parameter, if present, is the value of the s-coordinate
     -- position along the ray -- at which to start and stop the
     integration of the transport equation.  SLIMITS may be nil, a 1-D
     array of length 2, or a 2-by-nrays array.  Each component of SLIMITS
     is [s_start, s_stop]; if s_stop<s_start, the integration will run
     from minus infinity to plus infinity, which is the default.  The
     s-coordinate is measured from the reference point if RAYS is given
     in "best" or internal format, or from the point of closest approach
     to the origin r=z=0 if RAYS is given in TDG/DIRT format.

     The variables rt, zt, akap, and ekap must be present in the
     history file F, and represent, respectively, cylindrical coordinate,
     axial coordinate, absorption opacity, and source function.  The mesh
     (rt,zt) is a kmax-by-lmax array.  The opacity akap must be in units
     of inverse length (e.g.- 1/rt or 1/zt); its dimensions should be
     kmax-by-lmax-by-ngroup.  The source function ekap must be in units
     of specific intensity -- power per unit area per unit solid angle
     per unit spectrum, or "B nu units"; ekap must have the same dimensions
     as akap.  (The akap and ekap arrays may be padded so that they are
     actually (longer than kmax*lmax)-by-ngroup to handle outdated storage
     formats.)  The names "rt", "zt", "akap", and "ekap" may be changed
     by setting the drat_rt, drat_zt, drat_akap, and drat_ekap options.

     The result has dimension ngroup-by-nrays, where nrays is
     related to the dimensions of RAYS as above, and NGROUP is
     the number of groups in akap and ekap.

     Many external variables modify the behavior of streak.  Each of
     these options is separately documented.  The drat_glist option
     can be used to select a subset of ngroup, which can obviously
     reduce the processing time.  The drat_backlight and drat_gate
     options allow you to specify backlighter and gating functions,
     respectively.

     If you set any options, be careful.  The reset_options functions
     clears all options back to their default values;
   OPTIONS: drat_rt, drat_zt, drat_akap, drat_ekap,
            drat_ireg, drat_isymz, drat_khold, drat_lhold,
            drat_gb, drat_gav,
            drat_amult, drat_emult, drat_omult, drat_ireg_adj,
            drat_start, drat_stop, drat_symmetry, drat_glist,
            drat_linear, drat_nomilne,
            drat_backlight, drat_gate, drat_channel
   SEE ALSO: reset_options, streak, streak_save, integ_flat, integ_linear
             streak_times, form_rays, best_rays, dirt_rays, internal_rays
 */
{
  { local snap_result; }       /* used by snap_worker
                                  -- must NOT be local in streak */
  { local snap_dt, snap_i; }   /* also used by snap_worker */

  /* be sure that the gb and gav variables in the original file are
     available as an extern by any optional backlighter, gate, or channel
     functions */
  { local gb, gav; }
  vars= get_vars(f);
  if (drat_gb && is_present(vars, drat_gb)) gb= get_member(f, drat_gb);
  if (drat_gav && is_present(vars, drat_gav)) gav= get_member(f, drat_gav);

  /* swap out drat_compress and call streak */
  drat_compress= snap_worker;
  streak, f, rays, slimits;

  return snap_result;
}

func snap_worker(transp, selfem, time)
/* DOCUMENT snap_worker(transp, selfem, time)
     The snap function actually works by replacing the drat_compress
     with snap_worker.  See the source for snap in drat.i for details.
 */
{
  intens= apply_funcs(transp, selfem, time);

  { extern times; }                          /* local to streak */
  { extern snap_result, snap_dt, snap_i; }   /* local to snap */
  if (is_void(snap_i)) {
    /* this is the first record */
    snap_dt= drat_gate(times);
    snap_i= 1;  /* use_origins in streak is in effect */
    snap_result= intens*snap_dt(snap_i);

  } else {
    snap_result+= intens*snap_dt(++snap_i);
  }

  return [];  /* discard streak result entirely */
}

func streak_save(outname, f, rays, slimits)
/* DOCUMENT streak_save, outname, f, rays
         or streak_save, outname, f, rays, slimits
         or streak_save, outfile, f, rays, slimits
     is the same as the streak function, except that the results of
     the transport calculation are placed into a PDB file called
     OUTNAME, instead of being accumulated in memory.  All of the
     options for the streak function are available, except for
     drat_compress (which is set to streak_saver).

     If the first argument is OUTFILE, a file variable instead of a
     file name, then that file is used for output.  You can create
     OUTFILE and add static variables to it with save (but do NOT call
     add_record) which streak_save otherwise wouldn't know about.

     The output file has history records at the same times as the
     input file.  Each record contains "time" (a double scalar),
     and the two arrays "transp", the transparency (between 0 and 1),
     and "selfem", the self emission (which has the same units as
     ekap in the file F).  The dimensions of transp and selfem
     are ngroup-by-2-by-nrays (where nrays represents zero or more
     dimensions, copied from the RAYS input array).  The RAYS and
     SLIMITS inputs are placed into the output file as non-record
     variables, and any variables in the drat_static option are
     copied form F to the output file.  The gb and gav variables
     are copied from F into the output file as well.  If the drat_glist
     option is present, that is stored in the output file also.

   OPTIONS: all options available for streak except drat_compress,
            drat_gb, drat_gav, drat_static
   SEE ALSO: streak, snap
 */
{
  dummy= use_origins(0);

  /* create the output file */
  { local f_save; }    /* used in streak_saver
                          -- must NOT be local variable in streak */
  if (is_stream(outname)) f_save= outname;
  else f_save= createb(outname);

  /* save rays and slimits parameters to output file */
  if (is_void(slimits)) save, f_save, rays;
  else save, f_save, rays, slimits;

  /* copy drat_static from input file to output file */
  { extern drat_static; }
  vars= get_vars(f);
  if (drat_gb && is_present(vars, drat_gb)) list= [drat_gb];
  else list= [];
  if (drat_gav && is_present(vars, drat_gav)) grow, list, [drat_gav];
  grow, list, drat_static;
  n= numberof(list);
  for (i=1 ; i<=n ; i++) {
    name= list(i);
    if (!is_present(vars, name))
      error, "\""+name+"\" specified in drat_static option not present";
    value= get_member(f, name);
    add_variable, f_save, -1, name, structof(value), dimsof(value);
    get_member(f_save, name)= value;
  }

  /* save drat_glist option if present */
  { extern drat_glist; }
  if (!is_void(drat_glist)) save, f_save, drat_glist;

  /* swap out drat_compress and call streak */
  drat_compress= streak_saver;
  streak, f, rays, slimits;
}

func streak_saver(transp, selfem, time)
/* DOCUMENT streak_saver(transp, selfem, time)
     The streak_save function actually works by replacing the drat_compress
     with streak_saver.  See the source for streak_saver in drat.i for
     details.
 */
{
  { extern f_save; }   /* output file is local to streak_save */
  add_record, f_save, time;
  save, f_save, time, transp, selfem;
  return [];   /* discard streak result entirely */
}

func apply_funcs(transp, selfem, time)
/* DOCUMENT apply_funcs(streak_result)
         or apply_funcs(transp, selfem)
         or apply_funcs(transp, selfem, time)
         or apply_funcs(transp, selfem, times)
     applies the drat_backlight and drat_channel options (if any)
     to the input streak_result.  This destroys the separate
     transparency and self-emission information returned by streak.
     transp= streak_result(,1,..) and selfem= streak_result(,2,..).
     If time is not given, time=0.0 is passed to the functions.
     If times is a vector, it must match the final dimension of
     transp and selfem.
 */
{
  dummy= use_origins(0);

  { extern drat_backlight, drat_channel; }

  if (is_void(selfem)) {
    selfem= transp(,2,..);
    transp= transp(,1,..);
  }

  if (is_void(time)) time= 0.0;

  t= time(1);

  /* get the backlighter, if any, attenuate it, and add the self emission */
  if (is_func(drat_backlight)) intens= drat_backlight(t)*transp+selfem;
  else if (!is_void(drat_backlight)) intens= drat_backlight*transp+selfem;
  else intens= selfem;

  /* apply channel function, if any */
  if (is_func(drat_channel)) intens= drat_channel(intens, t);
  else if (!is_void(drat_channel)) intens= drat_channel(..,+)*intens(+,..);

  if (dimsof(time)(1)) {
    /* handle time array the lazy way -- recurse once only */
    n= numberof(time);
    intens= intens(..,-:1:n);
    for (i=2 ; i<=n ; i++)
      intens(..,i)= apply_funcs(transp(..,i), selfem(..,i), time(i));
    return intens;
  }

  return intens;
}

func adjust_ireg(ireg)
/* DOCUMENT adjust_ireg(ireg)
     returns the input IREG with the regions specified in drat_ireg_adj
     zeroed.  Beware-- the ireg array is actually modified.
   SEE ALSO: drat_ireg_adj
 */
{
  n= numberof(drat_ireg_adj);
  if (n && !is_void(ireg)) {
    dummy= use_origins(0);
    for (i=1 ; i<=n ; i++) {
      list= where(ireg==drat_ireg_adj(i));
      if (numberof(list)) ireg(list)= 0;
    }
  }
  return ireg;
}

local drat_rt, drat_zt, drat_ireg, drat_akap, drat_ekap;
local drat_isymz, drat_khold, drat_lhold, drat_gb, drat_gav;
/* DOCUMENT drat_rt, drat_zt, drat_ireg,
            drat_akap, drat_ekap,
            drat_isymz,
            drat_khold, drat_lhold,
            drat_gb, drat_gav
     can be set to strings other than "rt", "zt", etc. (their default
     values) to force the streak, snap, and streak_save routines to use
     alternative names to look up these quantites in the history file.

     The following 4 variables are NOT optional:
     (rt, zt) must be a 2-D mesh in cylindrical coordinates
     akap is a mesh-by-ngroup array of absorption opacities, in units
          of reciprocal length (1/rt or 1/zt)
     ekap is a mesh-by-ngroup array of source functions, in (arbitrary)
          specific intensity units
     The akap and ekap arrays must be zone centered; the first row and
     column of akap and ekap will be ignored.

     The remaining variables are all optional -- set the drat_.. variable
     to [] to ignore them completely.  Otherwise, they will be ignored if
     they are not present in the history file, and used as follows
     otherwise:
     ireg is a mesh-size region number array (zone centered as akap and
          ekap).  Zones where ireg==0 do not exist.
     isymz is non-zero if the problem has reflection symmetry about z=0,
          zero otherwise.  The drat_symmetry option overrides this value.
     khold and lhold are mesh indices specifying "hold lines" --
          khold is an index into the first dimension of (rt,zt), and
          lhold is an index into the second dimension of (rt,zt).
          These are used only if the drat_linear option is specified.
     gb and gav are, respectively, the group boundary energies and group
          center energies.  These are used by the snap and streak_save
          functions.

   SEE ALSO: streak, snap, streak_save, drat_symmetry, drat_linear
 */

local drat_amult, drat_emult, drat_omult, drat_ireg_adj;
/* DOCUMENT drat_amult, drat_emult, drat_omult
     are optional opacity multipliers used by the streak, snap, and
     streak_save functions.  The multipliers are applied to the
     opacity and source functions before the transport equation is
     integrated.  Setting them to [] is the same as setting them
     to 1.0.

     DRAT_EMULT     - multiply the emissivity by this factor,
                         without affecting the absorption
            opac   <- opac
            source <- source*DRAT_EMULT
     DRAT_AMULT   - multiply the absorption opacity by this
                         factor, without affecting the emissivity
            opac   <- opac*(DRAT_AMULT+1.e-20)
            source <- source/(DRAT_AMULT+1.e-20)
     DRAT_OMULT      - multiply BOTH the absorption opacity and the
                         emissivity by this factor
            opac   <- opac*DRAT_OMULT
            source <- source

     DRAT_IREG_ADJ   - list of region numbers to be zeroed.  This
                       has the same effect as a zero DRAT_OMULT in
                       the corresponding zones, but is more efficient.

     Since opac and source are mesh-by-ngroup (where mesh is usually
     kmax-by-lmax), DRAT_EMULT, DRAT_AMULT, DRAT_OMULT can
     be scalars, mesh arrays, 1-by-1-by-ngroup arrays, or
     kmax-by-lmax-by-ngroup arrays.  If DRAT_GLIST is non-nil, ngroup
     should be numberof(DRAT_GLIST), not the total number of groups.

  SEE ALSO: drat_glist, adjust_ireg
 */

local drat_start, drat_stop;
/* DOCUMENT drat_start, drat_stop
     if non-nil, specify the minimum and maximum dump times which will
     be considered by the streak, snap, or streak_save functions.
 */

local drat_symmetry;
/* DOCUMENT drat_symmetry
     set to 2 to force spherical symmetry, 1 to force reflection symmetry
     about the z=0 plane, 0 to force no symmetry, [] (the default) to
     use the guess_symmetry function to compute problem symmetry.
     Special value drat_symmetry=2+khold where k=khold is a hold-line
     causes ray to reflect at the hold line.  This doesn't mean anything
     physically (in fact, it is wrong), but may give qualitatively useful
     pictures in problems that are polar wedges.
 */

local drat_glist;
/* DOCUMENT drat_glist
     if non-nil, an index list into the final dimension of akap and ekap.
     Only these groups will be read from disk and used in the transport
     calculation.  All other options which depend on "ngroup" or "gav"
     should use numberof(DRAT_GLIST) or gav(DRAT_GLIST) instead.  The
     "gb" group boundary array is not well-defined in this case, since
     the group boundaries need not be contiguous.  The best strategy is
     to save drat_glist and the original gb array.

     DRAT_GLIST must be a 1-D, 1-origin index list.  (1-origin even if
     gav and gb are not 1-origin, since use_origins(0) will be in effect
     when DRAT_GLIST is used.)  The streak function will be most efficient
     if DRAT_GLIST is strictly increasing.

   SEE ALSO: drat_channel
 */

local drat_linear, drat_nomilne;
/* DOCUMENT drat_linear, drat_nomilne
     Set DRAT_LINEAR to 1 in order to use integ_linear to perform the
     transport integration instead of the default integ_flat.
     The DRAT_NOMILNE option, if non-nil, is a list of "norad"
     edges in the (rt,zt) mesh (other than the khold and lhold lines),
     which is required for the source function point centering operation.
     DRAT_NOMILNE is a 2 or 3-D array with the format:
        [[k1,l1], [k2,l2]]
     or an array of elements of that form, where either k1==k2 or
     l1==l2.  (Where k is the first index of rt or zt and l is the second.)

     DRAT_NOMILNE must always be a 1-origin index list into the (rt,zt)
     mesh, independent of the index origins of rt and/or zt.

   SEE ALSO: integ_linear, integ_flat
 */

local drat_static;
/* DOCUMENT drat_static
     if non-nil, a list of strings representing variable names in the
     input file which the streak_save function should copy to the
     output file.
   SEE ALSO: streak_save
 */

local drat_compress;
/* DOCUMENT func drat_compress(transp, selfem, time)
         or drat_compress= <such a function>
     supplies a compression algorithm to the streak function.
     The drat_compress can return anything, as long as it returns the
     same shape array (or nil) at each time.  The snap_worker and
     streak_saver routines are examples of compression algorithms.
 */

local drat_backlight;
/* DOCUMENT func drat_backlight(time) { extern gb, gav;  ... }
         or drat_backlight= <such a function>
         or drat_backlight= <array (time-independent backlighter)>
     supplies a backlighter for the snap function.

     Given ngroup-by-nrays transparency fraction transp and self-emission
     selfem (in specific intensity units), snap applies the backlighter
     using:
        result= backlighter*transp + selfem;
     where backlighter is drat_backlight(time), if drat_backlight is
     a function, or drat_backlight itself, if drat_backlight is an
     array.

     Note that the result (or value) of backlighter_func must be
     conformable with transp and selfem.  Most commonly, drat_backlight
     will be a vector of length ngroup -- a Planckian backlighter at
     temperature Tr would be
        drat_backlight= B_nu(gav, Tr);
     -- but that a scalar, 1-by-nrays, or ngroup-by-nrays are all
     possible.

     Note also that if drat_backlight is a function, the gb and gav
     arrays read from the history file are available as external
     variables.
   SEE ALSO: snap, drat_channel, drat_gate, apply_funcs
 */

local drat_channel;
/* DOCUMENT func drat_channel(time) { extern gb, gav;  ... }
         or drat_channel= <such a function>
         or drat_channel= <array (time-independent channel response)>
     supplies a channel response for the snap function.

     Use the drat_glist option to select a subset of the groups;
     drat_channel can be used in addition to drat_glist.

     Given ngroup-by-nrays specific intensity, snap applies the
     channel response using:
        result= drat_channel(..,+)*specific_intensity(+,..);
     if drat_channel is an array, or
        result= drat_channel(specific_intensity, time);
     if drat_channel is a function.

     Note that if drat_channel is an array, its final dimension must
     be of length ngroup.  A multidimensional drat_channel represents
     more than one channel response function.  Most drat_channel
     arrays will be proportional to the bin widths gb(dif).  The
     correct way to interpolate a filter function transmission
     fraction known at photon enrgies efa is:
        drat_channel= integ(ffa, efa, f.gb)(dif)

     If you have more than one channel, the first dimension of
     drat_channel should be the channel number.

     The best way to generate a filter response function is to
     use Yorick's cold opacity library.  To do this:
        #include "coldopac/xray.i"
     This will define the functions cold_opacity and cold_reflect,
     which you can use to build up channel response functions from
     filter materials and thicknesses and mirror compositions.

     Note also that if drat_channel is a function, the gb and gav
     arrays read from the history file are available as external
     variables.
   SEE ALSO: drat_glist, snap, drat_backlight, drat_gate, apply_funcs
 */

local drat_gate;
/* DOCUMENT func drat_gate(times) { extern gb, gav;  ... }
         or drat_gate= <such a function>
     supplies a gate (to make gated images) for the snap function.

     For a simple gate, the drat_start and drat_stop options will
     be more efficient than drat_gate.

     The input to drat_gate is the list of dump times; the output
     should be the "effective dt" for each of these dumps.  This is
     the product of the actual time interval and the gate transparency;
     the sum of the return vector is the gate time.  See the default_gate
     and gaussian_gate functions for examples.

     Note that the gb and gav arrays read from the history file are
     available as external variables, in case the gate transparency is
     frequency dependent.
   SEE ALSO: snap, drat_backlight, drat_channel, apply_funcs
             drat_start, drat_stop, gaussian_gate, default_gate
 */

local drat_ocompute, drat_oadjust;
/* DOCUMENT func drat_ocompute(file, time) { extern opac, source; ...}
         or drat_ocompute= <such a function>
        and func drat_oadjust(file, time) { extern opac, source; ...}
         or drat_oadjust= <such a function>
     supply opacities from a source other than the file.drat_akap and
     file.drat_ekap, or adjust these values.  You need to be cognizant of
     the drat_glist option (see get_kaps source code).
     DRAT_OCOMPUTE must set opac and source entirely on its own;
     DRAT_OADJUST will be called afterwards.
     The default DRAT_OCOMPUTE (default_ocompute) reads drat_akap and
     drat_ekap from FILE, optionally extracting drat_glist, and places
     them in opac and source.
     DRAT_OADJUST is free to modify opac and source them at will; the
     default DRAT_OADJUST is nil, which means no adjustment.
     Any opacity or emissivity multipliers will be applied after
     DRAT_OADJUST, as will the point centering operation if necessary
     (DRAT_OADJUST should return zone centered opacities).
 */

local drat_integrate;
/* DOCUMENT func drat_integrate(file, mesh, time, irays, slimits) { ... }
         or drat_integrate= <such a function>
     integrate the transport equation.  FILE is positioned to TIME, and
     MESH has already been read.  IRAYS are the rays in internal format
     and SLIMITS is the integration limits.  The return value should be
     ngroup-by-2-by-raydims (where irays is 6-by-raydims).  The default
     integrator is default_integrate, which handles the drat_ocompute,
     drat_oadjust, drat_amult, drat_emult, drat_omult, drat_akap,
     drat_ekap, drat_glist, drat_linear, and drat_nomilne options.
     Reasons to replace the default routine include: (1) Some or all of
     the opacities come from a source other than the FILE, e.g.- a second
     post processing file.  (2) The total number of zones times number of
     groups is debilitatingly large, even though the number of rays times
     the number of groups is not.
 */

local drat_quiet;
/* DOCUMENT drat_quiet
     By default, Drat prints the total number of records it will process,
     and the number of the record it is currently processing.  If drat_quiet
     is non-nil and non-zero, the printout is supressed.
 */

func default_ocompute(f, time)
/* DOCUMENT default_ocompute(f, time)
     initial value of drat_ocompute.  Extracts drat_akap and drat_ekap
     from file F, possibly using the subset drat_glist.  TIME is unused.
 */
{
  { extern opac, source; }           /* outputs -- local to caller */
  { extern drat_akap, drat_ekap; }   /* options */
  { extern drat_glist; }             /* option */
  if (is_void(drat_glist)) {
    opac= get_member(f, drat_akap);
    source= get_member(f, drat_ekap);
  } else {
    /* repeat get_member calls here -- potentially reduces disk traffic */
    opac= get_member(f, drat_akap)(..,drat_glist);
    source= get_member(f, drat_ekap)(..,drat_glist);
  }

  /* if akap and ekap have other than the expected shape, fix them */
  dims= dimsof(get_member(f,drat_rt));
  kmax= dims(2);
  lmax= dims(3);
  dims= dimsof(opac);
  ndims= dims(1);
  if (ndims<3 || dims(2)!=kmax || dims(3)!=lmax) {
    /* unfortunately, there are two possible ndims==2 cases:
       either both are spatial dimensions, or one is space and the
       other is energy -- only the latter case is handled here */
    o= opac;    opac= [];
    s= source;  source= [];
    ngroup= (ndims>1? dims(0) : 1);
    opac= array(structof(o), kmax, lmax, ngroup);
    source= array(structof(s), kmax, lmax, ngroup);
    if (ndims<3) {
      nzones= dims(2);
      if (nzones==(kmax-1)*(lmax-1)) {
        tmp= array(0.0, kmax-1, lmax-1, ngroup);
        tmp(*,)= o;
        opac(2:0,2:0,)= tmp;
        tmp(*,)= s;
        source(2:0,2:0,)= tmp;
      } else if (has_ireg) {
        list= where(ireg);
        if (numberof(list)==nzones) {
          opac(list,1,)= o;
          source(list,1,)= s;
        }
      }
    } else if (dims(2)==kmax-1 && dims(3)==lmax-1) {
      opac(2:0,2:0,)= o;
      source(2:0,2:0,)= s;
    }
  }
}

func default_gate(times)
/* DOCUMENT default_gate(times)
     initial value of drat_gate.  Refer to the source code
     to learn how to write your own gate function, making proper use
     of drat_start and drat_stop options in addition to the input times.
   SEE ALSO: gauss_gate, drat_gate
 */
{
  dummy= use_origins(0);
  /* append start and stop times to ends of times list
     -- if none specified, use first and last times */
  if (!is_void(drat_start)) t= [drat_start];
  else t= [times(1)];
  if (!is_void(drat_stop)) tn= [drat_stop];
  else tn= [times(0)];
  grow, t, times, tn;

  /* associate half of the previous interval and half of the next interval
     with each dump, except first and last */
  t(1) -= t(2)-t(1);  t(0) += t(0)-t(-1);
  return t(dif)(zcen);
}

func gauss_gate(times)
/* DOCUMENT gauss_gate(times)
     gate function used by gaussian_gate.  Refer to the source code
     to learn how to write your own gate function, making proper use
     of drat_start and drat_stop options in addition to the input times.
   SEE ALSO: gaussian_gate, drat_gate
 */
{
  /* append start and stop times to ends of times list
     -- if none specified, use first and last times */
  if (!is_void(drat_start)) t= [drat_start];
  else t= [times(1)];
  if (!is_void(drat_stop)) tn= [drat_stop];
  else tn= [times(0)];
  grow, t, times, tn;

  /* The correct approach is to interpolate into the time integral of
     the gate transparency.  If there are n times, there are n+2 values
     of the t array just constructed.  Each of the n+1 intervals should
     be associated half with the preceding time and half with the
     following time.  */
  t(1) -= t(2)-t(1);  t(0) += t(0)-t(-1);
  return gauss_int(t)(dif)(zcen);
}

func gauss_int(t)
/* DOCUMENT gauss_int(t)
     returns time integral of Gaussian specified in call to gaussian_gate.
 */
{
  /* Algorithm from Numerical Recipes (Press, et.al.) -- fractional
     error less than 1.2e-7.  */
  { extern gauss_t0, gauss_tsigma, gauss_norm; }
  tn= (t-gauss_t0)/gauss_tsigma;
  z= abs(tn);
  x= 1.0/(1.0+0.5*z);
  erfc= x*exp(-z*z+poly(x,-1.26551223, 1.00002368, 0.37409196, 0.09678418,
                        -0.18628806, 0.27886807, -1.13520398, 1.48851587,
                        -0.82215223, 0.17087277));
  x= sign(tn);
  erfc= (1.0-x)+x*erfc;
  return gauss_norm*(1.-0.5*erfc);
}

func gaussian_gate(t0, tsigma, max_trans)
/* DOCUMENT gaussian_gate(t0, tsigma, max_trans)
     sets the drat_gate for the snap function to be a Gaussian
     centered at time T0, with sigma TSIGMA, and maximum transmission
     fraction MAX_TRANS.
   SEE ALSO: snap, drat_gate
 */
{
  { extern gauss_t0, gauss_tsigma, gauss_norm; }
  gauss_t0= t0;
  gauss_tsigma= tsigma*sqrt(2.0);
  gauss_norm= sqrt(2.0*pi)*tsigma*max_trans;
  drat_gate= gauss_gate;
}

func reset_options(only)
/* DOCUMENT reset_options
         or reset_options, 1
     resets all options for the streak, snap, and streak_save functions
     to their default values.  With a non-zero, non-nil argument, only
     resets options which are currently nil, but have non-nil defaults.
 */
{
  { extern drat_rt, drat_zt, drat_ireg, drat_akap, drat_ekap; }
  { extern drat_isymz, drat_khold, drat_lhold, drat_gb, drat_gav; }
  { extern drat_amult, drat_emult, drat_omult, drat_ireg_adj; }
  { extern drat_start, drat_stop; }
  { extern drat_symmetry, drat_glist, drat_linear, drat_nomilne; }
  { extern drat_compress, drat_backlight, drat_channel, drat_gate; }
  { extern drat_static, drat_ocompute, drat_oadjust, drat_integrate; }
  { extern drat_quiet; }
  if (!only || is_void(drat_rt)) drat_rt= "rt";
  if (!only || is_void(drat_zt)) drat_zt= "zt";
  if (!only || is_void(drat_ireg)) drat_ireg= "ireg";
  if (!only || is_void(drat_akap)) drat_akap= "akap";
  if (!only || is_void(drat_ekap)) drat_ekap= "ekap";
  if (!only || is_void(drat_isymz)) drat_isymz= "isymz";
  if (!only || is_void(drat_khold)) drat_khold= "khold";
  if (!only || is_void(drat_lhold)) drat_lhold= "lhold";
  if (!only || is_void(drat_gb)) drat_gb= "gb";
  if (!only || is_void(drat_gav)) drat_gav= "gav";
  if (!only) drat_amult= drat_emult= drat_omult= drat_ireg_adj= drat_start=
    drat_stop= drat_symmetry= drat_glist= drat_linear= drat_nomilne=
    drat_compress= drat_backlight= drat_channel= drat_static= [];
  if (!only || is_void(drat_gate)) drat_gate= default_gate;
  if (!only || is_void(drat_ocompute)) drat_ocompute= default_ocompute;
  if (!only) drat_oadjust= [];
  if (!only || is_void(drat_integrate)) drat_integrate= default_integrate;
  if (!only) drat_quiet= [];
}
reset_options, 1;

func is_present(vars, name)
/* DOCUMENT is_present(get_vars(f), name)
     returns 1 if variable NAME is present in file F, 0 if not.
 */
{
  return (anyof(*vars(1)==name) || anyof(*vars(2)==name));
}

func streak_times(f)
/* DOCUMENT streak_times(f)
     returns the times from file F whic lie between the optional
     drat_start and drat_stop.
   SEE ALSO: drat_start, drat_stop
 */
{
  { extern drat_start, drat_stop; }   /* options */
  times= get_times(f);
  if (!is_void(drat_start)) times= times(where(times>=drat_start));
  if (!is_void(drat_stop)) times= times(where(times<=drat_stop));
  return times;
}

func get_std_limits(rays, slimits)
/* DOCUMENT get_std_limits(rays, slimits)
     returns slimits suitable for internal routines: 2-by-nrays,
     with s=0 at point of closest approach to origin
 */
{
  dummy= use_origins(0);

  dims= dimsof(rays);
  if (is_void(slimits)) {
    slims= array([1.0,-1.0], numberof(rays)/dims(2));
  } else {
    mask= (slimits(1,..)<slimits(2,..));
    slims= slimits - (mask*get_s0(rays))(-,);
  }
  return slims;
}

func guess_symmetry(f, vars)
/* DOCUMENT guess_symmetry, f
         or guess_symmetry(f)
     guesses the symmetry of the problem in the dump file F based on
     the variables f.isymz, f.rt, and f.zt.
     If called as a subroutine, prints one of:
     "no symmetry", "z=0 reflection symmetry", or "spherical symmetry"
     If called as a function, returns 0, 1, or 2, respectively.
 */
{
  dummy= use_origins(0);
  if (is_void(vars)) vars= get_vars(f);
  if (drat_isymz && is_present(vars, drat_isymz))
    zsym= (get_member(f, drat_isymz)!=0);
  else
    zsym= 0;
  { extern drat_rt, drat_zt; }              /* options */
  rt= get_member(f, drat_rt)(,1);
  if (numberof(rt)==2 &&
      !max(abs(rt, get_member(f,drat_zt)(,1)))) zsym= 2;

  if (!am_subroutine()) return zsym;

  if (zsym==0) write, "no symmetry";
  else if (zsym==1) write, "z=0 reflection symmetry";
  else write, "spherical symmetry";
}

func B_nu(hnu, kt)
/* DOCUMENT B_nu(hnu, kt)
     returns the specific intensity emitted by a black surface at
     photon energy HNU and temperature KT.  The units of HNU and KT
     must be the same.  The units of the result are determined by
     the variable B_nu_scale, which must be consistent with the units
     of HNU and KT.  B_nu_scale is the Stefan-Boltzmann constant
     (sigma in sigma*T^4) times 15/pi^5.  By default, B_nu_scale is
     set to 0.05040366 ((jrk/sh)/(cm^2 ster))/keV^4.  (1 jrk/sh =
     10^17 W)
     HNU and KT may be arrays, provided they are conformable.

   SEE ALSO: B_nu_bar
 */
{
  { extern B_nu_scale; }
  boltz= exp(-hnu/kt);
  return B_nu_scale*hnu^3*boltz/(1.0-boltz);
}
B_nu_scale= 0.05040366 /* ((jrk/sh)/(cm^2 ster))/keV^4, according to
                           the 1986 least-squares adjustment, Physics
                           Today, August 1988, p. 9.  The last digit
                           is uncertain.  */

func B_nu_bar(hnub, kt)
/* DOCUMENT B_nu_bar(hnub, kt)
     returns the specific intensity emitted by a black surface at
     temperature KT in the energy bins whose boundary energies are
     HNUB.  HNUB must be a 1-D array of bin boundary energies; the
     units of KT must match the units of KT.  Both are in keV, by
     default; see B_nu for a discussion of units.
     The result will have dimensions (numberof(HNUB)-1)-by-dimsof(KT).

     The algorithm has an accuracy of 0.2 percent.  The idea is to
     difference an analytic approximation to the integral of B_nu.

   SEE ALSO: B_nu
 */
{
  dummy= use_origins(0);
  { extern B_nu_scale; }
  /* Integrate of B_nu over each bin analytically,
     then divide by the bin width.  */
  dbdt= 1./(kt + 1.e-40);
  v= hnub*dbdt(-,..);
  rdv= 1.0/hnub(dif);
  bnu= -rdv*(kt^4)(-,..)*
    (exp(-v)*(((((0.07713864107538*v+0.5194172986679)*v+2.161761553097)*v+
                    5.570970415031)*v+8.317008834543)*v+6.493939402267)/
     ((0.07713864107538*v+0.2807339758744)*v+1.0))(dif,..);
  /* The preceding expression is inaccurate at small values of v (hnu/kt),
     where the B_nu is parabolic.  Fill in a better result there.  */
  vsq= (hnub^3)(dif)*rdv/3.0;
  mask= (v(2:0,..) > 0.001);
  bnu= mask*bnu + (!mask)*vsq*kt(-,..);
  return B_nu_scale*bnu;
}

func alloc_mesh(f, vars)
{
  dummy= use_origins(0);
  { extern drat_symmetry; }   /* option */
  if (is_void(drat_symmetry)) zsym= guess_symmetry(f, vars);
  else zsym= drat_symmetry;

  /* note that khold and lhold must be presented to form_mesh in a manner
     that is independent of the origin of the rt and zt arrays --
     here, use 1 origin indexing */
  { extern drat_khold, drat_lhold; }   /* options */
  { extern drat_rt; }                  /* option */
  if (drat_khold && is_present(vars, drat_khold))
    khold= get_member(f, drat_khold);
  else
    khold= 0;
  if (drat_lhold && is_present(vars, drat_lhold))
    lhold= get_member(f, drat_lhold);
  else
    lhold= 0;

  return form_mesh(zsym, khold, lhold);
}

func get_kaps(f, mesh, time)
{
  /* outputs -- local to caller */
  { extern opac, source; }

  /* retrieve from file -- optionally only those groups in drat_glist */
  { extern drat_ocompute, drat_oadjust; }  /* options */
  drat_ocompute, f, time;
  if (!is_void(drat_oadjust)) drat_oadjust, f, time;

  /* apply absorption and emission multipliers */
  { extern drat_amult, drat_emult, drat_omult; }   /* options */
  if (!is_void(drat_emult)) source*= drat_emult;
  if (!is_void(drat_amult)) {
    opac*= drat_amult+1.e-20;
    source/= drat_amult+1.e-20;
  }
  if (!is_void(drat_omult)) opac*= drat_omult;

  /* point center the source function (in place) if requested */
  { extern drat_linear, drat_nomilne; }   /* options */
  if (drat_linear) {
    if (structof(source)!=double) source= double(source);
    pcen_source, opac, source, mesh, drat_nomilne;
  }
}

/* ------------------------------------------------------------------------ */
/* Low level interface */

extern form_mesh;
/* DOCUMENT form_mesh(zsym, khold, lhold)
     returns an opaque "mesh" object, which will hold rt, zt, ireg,
     and a boundary edge list.  This opaque mesh object is required
     as an input to the integ_flat and integ_linear routines.

     ZSYM is 2 for spherical symmetry, 1 for z=0 reflection symmetry,
          or 0 for no symmetry

     KHOLD and LHOLD are the 1-origin indices of "hold" lines in the
          mesh, or 0 if none.  This information is used only during the
          pcen_source operation before integ_linear is called.
   SEE ALSO: update_mesh, integ_flat, integ_linear
 */

extern update_mesh;
/* DOCUMENT update_mesh, mesh, rt, zt
         or update_mesh, mesh, rt, zt, ireg
     updates the opaque MESH object to reflect a new RT, ZT, and
     (optionally) IREG.  The boundary edges are recomputed and stored
     in MESH, as well.
   SEE ALSO: form_mesh, integ_flat, integ_linear
 */

extern find_boundary;
/* DOCUMENT boundary= find_boundary(mesh)
         or boundary= find_boundary(mesh, region, sense)
     returns an array of 4 pointers representing the boundary of the
     MESH, or of a particular REGION of the MESH, with a particular
     SENSE -- 0 for counterclockwise in the (r,z)-plane, 1 for
     clockwise.  The returned arrays are:
        *boundary(1)   zone index list -- always 1-origin values
        *boundary(2)   side list 0, 1, 2, or 3
                       side 0 is from point zone to zone-1, side 1 is
                       from zone-1 to zone-imax-1
        *boundary(3)   z values of boundary points
        *boundary(4)   r values of boundary points
   SEE ALSO: form_mesh, update_mesh
 */

func integ_flat(opac, source, rays, mesh, slimits)
/* DOCUMENT integ_flat(opac, source, rays, mesh, slimits)
         or integ_flat(opac, source, ray_paths)
     returns ngroup-by-2-by-nrays result, where result(,1,..) is
     the transparency factors, and result(,2,..) is the self-emission
     for each group on each ray.  The input OPAC and SOURCE are the
     opacity (an inverse length) and the source function (a specific
     intensity).  They are mesh-by-ngroups zone centered arrays.  The
     result has the same units as SOURCE.
     In the second form, RAY_PATHS was returned by the track_rays
     function.
   SEE ALSO: integ_linear, track_rays, form_mesh, streak, snap
 */
{
  /* get various dimensions */
  dims= dimsof(opac);
  ngroup= dims(0);
  kxlm= numberof(opac)/ngroup;
  if (anyof(dims!=dimsof(source)) ||
      (!is_void(mesh) && kxlm<_get_msize(mesh)))
    error, "opac and source must be kmax-by-lmax-by-ngroup arrays";
  dims= dimsof(rays);
  nrays= numberof(rays)/dims(2);

  /* form result array */
  if (--dims(1)) dims(2:-1)= dims(3:0);
  result= array(double, ngroup, 2, dims);

  if (!is_void(mesh)) {
    /* convert slimits to standard s-coordinate used internally, based
       on point of closest approach of ray to origin, rather than
       ray reference point */
    slims= get_std_limits(rays, slimits);
    return _raw1_flat(opac, source, kxlm, ngroup, internal_rays(rays),
                      nrays, mesh, slims, result);

  } else {
    /* the routine internally checks that rays.zone never gets
       larger than kxlm */
    return _raw2_flat(opac, source, kxlm, ngroup, rays, nrays, result);
  }
}

func integ_linear(opac, source, rays, mesh, slimits)
/* DOCUMENT integ_linear(opac, source, rays, mesh, slimits)
         or integ_linear(opac, source, ray_paths)
     returns ngroup-by-2-by-nrays result, where result(,1,..) is
     the transparency factors, and result(,2,..) is the self-emission
     for each group on each ray.  The input OPAC and SOURCE are the
     opacity (an inverse length) and the source function (a specific
     intensity).  They are mesh-by-ngroups arrays; OPAC is zone centered,
     while SOURCE is point centered (using pcen_source).  The result
     has the same units as SOURCE.
     In the second form, RAY_PATHS was returned by the track_rays
     function.
     The integ_linear routine assumes that the SOURCE function varies
     linearly between the entry and exit points from each zone.  This
     assumption is poor near the turning point, and causes the result
     to be a discontinuous function of the ray coordinates, unlike the
     integ_flat result.
   SEE ALSO: pcen_source, integ_flat, track_rays, form_mesh, streak, snap
 */
{
  /* get various dimensions */
  dims= dimsof(opac);
  ngroup= dims(0);
  kxlm= numberof(opac)/ngroup;
  if (anyof(dims!=dimsof(source)) ||
      (!is_void(mesh) && kxlm<_get_msize(mesh)))
    error, "opac and source must be kmax-by-lmax-by-ngroup arrays";
  dims= dimsof(rays);
  nrays= numberof(rays)/dims(2);

  /* form result array */
  if (--dims(1)) dims(2:-1)= dims(3:0);
  result= array(double, ngroup, 2, dims);

  if (!is_void(mesh)) {
    /* convert slimits to standard s-coordinate used internally, based
       on point of closest approach of ray to origin, rather than
       ray reference point */
    slims= get_std_limits(rays, slimits);
    return _raw1_linear(opac, source, kxlm, ngroup, internal_rays(rays),
                        nrays, mesh, slims, result);

  } else {
    /* the routine internally checks that rays.zone never gets
       larger than kxlm */
    return _raw2_linear(opac, source, kxlm, ngroup, rays, nrays, result);
  }
}

func pcen_source(opac, source, mesh, drat_nomilne)
/* DOCUMENT pcen_source, opac, source, mesh, drat_nomilne
     point centers the SOURCE array (in place) using a complicated
     algorithm involving the OPAC and MESH (from form_mesh and update_mesh).
     If non-nil, DRAT_NOMILNE must have the same format as the
     drat_nomilne option.
 */
{
  /* get various dimensions */
  dims= dimsof(opac);
  ngroup= dims(0);
  kxlm= numberof(opac)/ngroup;
  if (anyof(dims!=dimsof(source)) || kxlm<_get_msize(mesh))
    error, "opac and source must be kmax-by-lmax-by-ngroup arrays";
  if (!is_void(drat_nomilne)) {
    dims= dimsof(drat_nomilne);
    if (dims(1)<2 || dims(2)!=2 || dims(3)!=2)
      error, "drat_nomilne argument to pcen_source must be 2x2xn_edges";
    nomilne= drat_nomilne-1;    /* _raw_pcens needs 0-origin */
    nedges= numberof(nomilne)/4;
  } else {
    nomilne= 0;
    nedges= 0;
  }
  _raw_pcens, opac, source, kxlm, ngroup, mesh, nomilne, nedges;
}

/* The Ray_Path struct is duplicated from drat.c -- if you change it
   here, you must change it there as well.  */
struct Ray_Path {
  pointer zone;   /* list of zones (1-origin) cut by the ray */
  pointer ds;     /* list of path lengths in above zones */
  double fi, ff;  /* fraction of 1st and last ds, respectively, outside
                     the specified slimits */
  pointer pt1, pt2;  /* lists of endpoints of edges cut by ray -- ray cuts
                        directed edge pt1->pt2 from right to left
                        Like zone, always 1-origin values.  */
  pointer f;         /* list of fractions -- (f+0.5) is the fraction of
                        distance from pt1 to pt2 where ray cuts edge */
}

func track_rays(rays, mesh, slimits)
/* DOCUMENT ray_paths= track_rays(rays, mesh, slimits)
     returns array of Ray_Path structs representing the progress of
     RAYS through the MESH between the given SLIMITS.
   SEE ALSO: Ray_Path, integ_flat, get_ray-path
 */
{
  return _raw_track(numberof(rays)/dimsof(rays)(2), internal_rays(rays),
                    mesh, get_std_limits(rays,slimits));
}

func get_ray_path(path, rt, zt)
/* DOCUMENT ray_info= get_ray_path(path, rt, zt)
     where PATH is one element of an array returned by track_rays,
     returns the points where the ray cut the edges of the mesh (ZT, RT).
     The returned RAY_INFO has two components: RAY_INFO(,1) is the z
     coordinates and RAY_INFO(,2) is the r coordinates.
   SEE ALSO: track_rays
 */
{
  pt1= *path.pt1;
  if (is_void(pt1)) return [];
  pt2= *path.pt2;
  f= *path.f;
  info= array(0.0, numberof(pt1), 2);
  info(,1)= zt(pt1)*(0.5-f) + zt(pt2)*(0.5+f);
  info(,2)= rt(pt1)*(0.5-f) + rt(pt2)*(0.5+f);
  if ((f=path.fi)>0.0) info(1,)= f*info(2,)+(1.-f)*info(1,);
  if ((f=path.ff)>0.0) info(0,)= f*info(-1,)+(1.-f)*info(0,);
  return info;
}

extern set_tolerances;
/* DOCUMENT set_tolerances()
         or old_tols= set_tolerances([tol1, tol2, lost_tol])
     returns the current tolerances for the ray tracking.  Initially,
     these are [1.e-3, 1.e-6, 0.0].  In the second form, sets new
     tolerances.  If any of TOL1, TOL2, or LOST_TOL is zero, that
     tolerance is restored to its default value.  If TOL1 is less
     than zero, the root polishing operation which requires TOL1
     and TOL2 is not done at all.
   SEE ALSO: track_rays, integ_flat, integ_linear, streak, snap
 */

extern _raw_track;
/* _raw_track(nrays, rays, mesh, slimits)
   expects rays in internal format, and slimits as returned from
   get_std_limits
 */

extern _raw1_flat;
/* xxPROTOTYPE
   void IntegFlat(double array opac, double array source,
                  long kxlm, long ngroup, double array rays, long nrays,
                  opaque mesh, double array slimits, double array result)
 */

extern _raw2_flat;
/* xxPROTOTYPE
   void I2flat(double array opac, double array source,
               long kxlm, long ngroup, Ray_Path array rays,
               long nrays, double array result)
 */

extern _raw1_linear;
/* xxPROTOTYPE
   void IntegLinear(double array opac, double array source,
                    long kxlm, long ngroup, double array rays, long nrays,
                    opaque mesh, double array slimits, double array result)
 */

extern _raw2_linear;
/* xxPROTOTYPE
   void I2linear(double array opac, double array source,
                 long kxlm, long ngroup, Ray_Path array rays,
                 long nrays, double array result)
 */

extern _raw_pcens;
/* xxPROTOTYPE
   void DoPtCenter(double array opac, double array source,
                   long kxlm, long ngroup, opaque mesh,
                   long array nomilne, long nedges)
 */

extern _get_msize;
/* xxPROTOTYPE
   long _get_msize(opaque mesh)
 */

/* The initialization routine sets the Drat memory management routines
   to the Yorick memory management routines, and locates the Ray_Path
   data structure for track_rays.  */
extern _init_drat;
if (!is_void(_init_drat)) _init_drat;

require, "rays.i";
