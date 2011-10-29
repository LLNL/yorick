/*
 * $Id: multi.i,v 1.1 2005-09-18 22:06:02 dhmunro Exp $
 * Drat/DSP routines defining and implementing a standard for
 * retrieving opacity/emissivity data from multiple files and
 * combining it for a single problem.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*
   Design for a Drat interface which handles opacity/emissivity
   input from several post-processing files.  Key features:

   (1) One file is the "master file", which contains all of the
       zones (rt, zt, ireg from the original calculation), at all
       of the times that were dumped.  This file may or may not
       contain its own gb, ekap, and akap arrays.

   (2) All other files may contain only subsets of the zones or
       times of the master file.  If they contain only a subset of
       the times, the transport calculation will be done only at
       the times which are common to every file.  Any zone may
       get contributions to its opacity and emissivity from zero,
       one, or more of the files.

   (3) Every file (except possibly the master file) contains its own
       gb, ekap, and akap arrays.  Optionally, it may contain a gexist
       array, which is one shorter than gb, and is 0 in any bins which
       do not exist in that file.  The group dimension of ekap and akap
       should be the number of non-zero elements of gexist.  This allows
       calculations with non-contiguous coverage of photon energy.
       Alternatively, codes which have no concept of photon energy bins
       may write a gav array, and omit the gb array.  A suitable gb will
       be inferred, and gexist is meaningless for this case.
       ===>> In no case may gb or gexist (or gav) change either as a
             function of time, or from zone to zone.
       However, the group structures of the various files need not match.
       You can either specify an overall group structure, or have the
       code generate one for you before you perform the transport
       calculation; in either case, the code will interpolate from
       the group structure in each file onto the common group structure
       where the transport calculation is performed.

   (4) Zones are determined by a separate list of "master" zone indices
       for each slave file (and optionally for the master file, too).
       If a zone in some slave file is marked in its zone list, that
       file will contribute both opacity and emissivity to the total
       for that zone, otherwise it will not contribute.

   (5) The quantities currently dumped, akap (1/length) and
       ekap (power/photon energy/solid angle/area) may not be appropriate
       for another opacity code, because the ekaps from several sources
       cannot simply be added.  Therefore, the slave files may optionally
       dump the variable emiss (power/photon energy/solid angle/volume)
       instead of ekap.  The emiss and akap from several different files
       simply add.

   (6) Some codes may need to dump the opacity arrays with the
       frequency as the first index. The freqfirst flag handles this.
 */

/*= SECTION() multiple mesh support for drat package ======================*/

func multio(filename, opac=, emiss=, srcf=, oscale=, escale=,
            gb=, gav=, gexist=, gscale=, zonelist=, zoneuse=,
            tscale=, noextrap=, freqfirst=)
/* DOCUMENT mf= multio(filename)
         or mf= multio(file)
     opens file FILENAME for use with the multi_streak function.
     The file MUST be subsequently closed using multic, since
     this function produces a hidden reference to the file.  The function
     multif can be used to return an ordinary file pointer, given the
     returned MF structure.  If the argument is already a stream FILE,
     that file will be used.  The call still produces a hidden copy of
     FILE, so you may set your copy of the FILE variable to [], but do
     not close the file.

     The following keywords can be used to allow for variations in the
     variable names or units, and to specify the correspondence between
     the zones in this file, and the zones in the master file:

     zonelist=index_list
     -or- zonelist=zonelist_name
       is an index list into the (rt,zt) mesh arrays of the master file.
       If ireg is the region number array (having the same dimensions as
       rt or zt, and with its first row and column all 0), and if FILENAME
       contains opacity data only for zones with region numbers 1 and 2,
       you could open the file using:
          mf= multio(filename, zonelist=where(ireg==1 | ireg==2))
       The zonelist should be nil only if the spatial dimensions of the
       opacity and emissivity in this file exactly match those of rt or
       zt in the master file.
       If zonelist is a string, it replaces the default name for the
       zonelist variable stored in the file (see multi_zonelist).

     zoneuse=index_list
       The zonelist specifies how the zones in this file correspond
       with those in the master file.  The zoneuse list allows you
       to specify that only some of the zones actually present in the
       opacity and emissivity arrays of this file are to contribute
       to the total.  This might be necessary to avoid double counting
       in a region covered by more than one file.  Hence zoneuse is
       a list of indices into the spatial dimension(s) of the opacity
       and emissivity arrays in this file.  If nil, all zones in this
       file will contribute.  If present, and if zonelist is supplied
       as an array (rather than out of the file), zonelist should
       have the same length as zoneuse.
       As a special case, if zoneuse is a scalar 0, no opacity or
       emissivity will come from this file; this makes sense only if
       this is the master file.

     opac=oname, emiss=ename, srcf=sname
       specify non-default names for the opacity, emissivity, and
       osource function arrays.  The defaults are given by the global
       variables mutli_opac, multi_emiss, and multi_srcf (see help).
       If the emissivity array is present in the file, it is preferred
       to the source function array, which will then be ignored.

     oscale=opacity_unit, escale=emissivity_unit
       are optional conversion factors to bring the units of the
       opac and emiss (or srcf) arrays into agreement among the various
       files which are to be used in a single run.  The default value
       is 1.0 (i.e.- all files are expected to have the same units).

     gb=gbname, gav=gavname, gexist=gexistname
     -or- gexist=group_existence_map
       specify non-default names for the group boundary, group energy,
       and group existence arrays.  The defaults are given by the global
       variables mutli_gb, multi_gav, and multi_gexist (see help).
       If the group boundary array is present in the file, it is preferred
       to the group energy array, which will then be ignored.  The file
       should specify group boundaries if its opacity and emissivity are
       averaged over finite width bins; group energies if its opacity
       and emissivity are computed at points.  The group existence map,
       if present, allows several disjoint spectral regions to exist in
       a single file.  If the data type of gexist is not "string", it
       should be an array of length one less than gb, if gb is present,
       or gav, otherwise.  By this means you can ignore spectral regions
       which are present in the file.

     gscale=photon_energy_unit
       is an optional conversion factor to bring the units of the
       gb (or gav) arrays into agreement among the various files
       which are to be used in a single run.  The default value
       is 1.0 (i.e.- all files are expected to have the same units).

     tscale=time_unit
       is an optional conversion factor to bring the units of the
       time into agreement among the various files which are to be used
       in a single run.  The default value is 1.0 (i.e.- all files are
       expected to have the same units).

     noextrap=1
       if present and non-zero prevents the opacity and emissivity
       data from this file from being extrapolated as 1/hnu^3 in
       master bins at energies above the highest energy bin in this
       file.

     freqfirst=0
       if present and non-zero means the frequency index is first
       for the opacity and emissivity arrays, instead of the
       default of frequency index last.

   SEE ALSO: multic, multif, multi_streak, MultiFile
             multi_opac, multi_emiss, multi_srcf, multi_gb, multi_gav,
             multi_zonelist
 */
{
  if (!is_stream(filename)) {
    f= openb(filename);
    if (!is_stream(f)) error, "couldn't openb "+filename;
  } else {
    f= filename;
  }

  /* create a symbol and stick a hidden reference to the file in it --
     this is as close as Yorick gets to a pointer to a file variable
     :::should re-implement this with lists for Yorick 1.2::: */
  extern _multi_symbols;
  if (is_void(_multi_symbols)) _multi_symbols= array(string, 8);
  list= where(!_multi_symbols);
  if (!numberof(list)) {
    grow, _multi_symbols, array(string, 8);
    list= where(!_multi_symbols);
  }
  i= list(1);
  _multi_symbols(i)= name= "_m_f_"+pr1(i);
  symbol_set, name, f;

  mf= MultiFile(f=name);
  if (!is_void(zonelist) && structof(zonelist)!=string) {
    mf.zones= &zonelist;
    zonelist= [];
  }
  if (!is_void(zoneuse)) mf.zuse= &zoneuse;
  if (!is_void(opac)) mf.oname= opac;
  if (!is_void(emiss)) mf.ename= emiss;
  if (!is_void(srcf)) mf.sname= srcf;
  if (!is_void(escale)) mf.escale= escale;
  if (!is_void(oscale)) mf.oscale= oscale;
  if (!is_void(noextrap)) mf.noextrap= noextrap;
  if (!is_void(freqfirst)) {
    mf.freqfirst= freqfirst;
  } else {
    /* default is zero so q-files will work */
    mf.freqfirst= 0;
  }
  _multi_names, mf, gbname=gb, gavname=gav, gexistname=gexist,
                    gscale=gscale, zonesname=zonelist;

  if (!is_void(tscale) && tscale>0.0) mf.tscale= tscale;
  else mf.tscale= 1.0;

  return mf;
}

_multi_symbols= [];

func multic(mf)
/* DOCUMENT multic, mf
         or multic, [mf1, mf2, mf3, ...]
     closes a MultiFile created with multif.
     Presented with an array of multifiles, closes them all.
   SEE ALSO: multio, multif
 */
{
  n= numberof(mf);
  for (i=1 ; i<=n ; i++) {
    name= mf(i).f;
    mf(i).f= "?";    /* prevent future references */
    if (!is_void(_multi_symbols)) {
      list= where(_multi_symbols==name);
      if (numberof(list)) _multi_symbols(list)= string(0);
    }
    /* discard hidden file reference */
    symbol_set, name, [];
    /* free associated storage as well */
    mf.zones= mf.gb= mf.gav= mf.gexist= mf.gx= mf.nu= &[];
  }
}

func multif(mf)
/* DOCUMENT multif(mf)
     returns an ordinary file pointer for the MultiFile MF.
     Do not use close to close this pointer; just set it to [] when
     you are done.  Use multic to properly close the MF.
   SEE ALSO: multio, multic
 */
{
  return symbol_def(mf.f);
}

func multi_streak(mf, rays, slimits, gb=)
/* DOCUMENT result= multi_streak(mf, rays, slimits, gb=common_bins)
     like the streak function, but allows opacity to be built up from
     "slave files", in addition to the "master file" MF(1).  The MF
     parameter is an array of MultiFiles, each created by multif.
     The master file MF(1) contains the mesh, and the master list of dump
     times.  Only dump times which are present in this master list, and
     in every slave file, will be processed.
     The master file MF(1) need not contain any opacity or emissivity data
     at all; each of the slave files MF(2:0) must contain data for at
     least one zone.

     The emissivities and opacities from each file are interpolated onto
     a common group structure.  This common group structure can be
     provided via the GB keyword to multi_streak.  If it is not provided,
     GB is computed by examining the group boundary (or center) arrays
     from the master and every slave file, and building a group structure
     which is at least as fine as every component group structure, at every
     point in the spectrum.

     Example:
       File family "prob_p00" contains the mesh and opacities and
       emissivities for all zones.  Family "pp_h00" contains post
       processed opacities and emissivities on a much finer spectral
       mesh, but only for zones in regions 1 and 2 of the original
       problem.  You want to transport the emission from the
       inner regions 1 and 2 through the overlying material:

         restore, openb("prob_p00"), ireg;
         master= multif("prob_p00", zoneuse=where(ireg>2));
         slave= multif("pp_h00", zonelist=where(ireg==1|ireg==2));
         rays= ...
         slimits= ...
         drat_start= ...
         drat_stop= ...
         result= multi_streak([master,slave], rays, slimits);
         multic, master;
         multic, slave;

   SEE ALSO: multio, multic, multif, MultiFile
             multi_opac, multi_emiss, multi_srcf, multi_gb, multi_gav,
             multi_zonelist, multi_times, multi_bins
 */
{
  f= multif(mf(1));       /* master file is first one */

  /* initialize local variables which are hidden arguments to
     _multi_... functions */
  _multi_files= mf;
  _multi_gb= gb;
  _multi_nchunks= 0;     /* set in _multi__init */

  /* set up hook functions for streak */
  streak_times= multi_times;
  _multi_init= _multi__init;
  drat_integrate= _multi_integrate;
  jt= _multi_jt;

  return streak(f, rays, slimits);
}

func multi_streak_save(outname, mf, rays, slimits, gb=)
/* DOCUMENT multi_streak_save, outname, mf, rays, slimits, gb=common_bins
         or multi_streak_save, outfile, mf, rays, slimits, gb=common_bins
     like the streak function, but allows opacity to be built up from
     "slave files", in addition to the "master file" MF(1) and 
     saves the streak in a PDB history file.  The MF parameter
     is an array of MultiFiles, each created by multif.
     The master file MF(1) contains the mesh, and the master list of dump
     times.  Only dump times which are present in this master list, and
     in every slave file, will be processed.
     The master file MF(1) need not contain any opacity or emissivity data
     at all; each of the slave files MF(2:0) must contain data for at
     least one zone.

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

     The emissivities and opacities from each file are interpolated onto
     a common group structure.  This common group structure can be
     provided via the GB keyword to multi_streak.  If it is not provided,
     GB is computed by examining the group boundary (or center) arrays
     from the master and every slave file, and building a group structure
     which is at least as fine as every component group structure, at every
     point in the spectrum.

     Example:
       File family "prob_p00" contains the mesh and opacities and
       emissivities for all zones.  Family "pp_h00" contains post
       processed opacities and emissivities on a much finer spectral
       mesh, but only for zones in regions 1 and 2 of the original
       problem.  File "prob_strk" contains the streak history. 
       You want to transport the emission from the
       inner regions 1 and 2 through the overlying material:

         restore, openb("prob_p00"), ireg;
         master= multif("prob_p00", zoneuse=where(ireg>2));
         slave= multif("pp_h00", zonelist=where(ireg==1|ireg==2));
         fout= openb("prob_strk");
         save, fout, kmax, lmax;
         rays= ...
         slimits= ...
         drat_start= ...
         drat_stop= ...
         result= multi_streak_save(fout, [master,slave], rays, slimits);
         multic, master;
         multic, slave;

   SEE ALSO: multio, multic, multif, MultiFile, multi_streak
             multi_opac, multi_emiss, multi_srcf, multi_gb, multi_gav,
             multi_zonelist, multi_times, multi_bins
 */
{
  dummy= use_origins(0);

  /* create the output file */
  { local f_save; }    /* used in streak_saver
                          -- must NOT be local variable in streak */
  if (is_stream(outname)) f_save= outname;
  else f_save= createb(outname);

  f= multif(mf(1));       /* master file is first one */

  /* initialize local variables which are hidden arguments to
     _multi_... functions */
  _multi_files= mf;
  _multi_gb= gb;
  _multi_nchunks= 0;     /* set in _multi__init */

  /* set up hook functions for streak */
  streak_times= multi_times;
  _multi_init= _multi__init;
  drat_integrate= _multi_integrate;
  jt= _multi_jt;

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
    { extern drat_compress; }     /* option */
  drat_compress= streak_saver;
  streak, f, rays, slimits;
}

func multi_bins(mf)
/* DOCUMENT multi_bins(mf)
     The MF parameter is an array of MultiFiles, each created by multif.
     Automatically generates the bin structure which will be used by
     multi_streak (if the GB keyword is not specified).
 */
{
  /* get the group structure from the master file, if any */
  mfi= mf(1);
  if (mfi.gb) gb= *mfi.gb;
  else if (mfi.gav) gb= multi_bav(*mfi.gav);

  /* build an appropriate bin boundary array */
  n= numberof(mf);
  for (i=2 ; i<=n ; i++) {
    mfi= mf(i);
    if (mfi.gb) gbf= *mfi.gb;
    else if (mfi.gav) gbf= multi_bav(*mfi.gav);
    else error, "no gb or gav arrays in slave file "+pr1(i);
    if (is_void(gb)) gb= gbf;
    else gb= _multi_bins(0, gb, gbf);
  }

  return gb;
}

func _multi_jt(f, time)
{
  _jt, f, time;
  n= numberof(_multi_files);
  for (i=2 ; i<=n ; i++) {
    mfi= _multi_files(i);
    _jt, multif(mfi), time/mfi.tscale;
  }
}

/* streak_times is "swapped out" in multi_streak */
_orig_streak_times= streak_times;

func multi_times(f)
/* DOCUMENT times= multi_times(mf)
     returns the list of times which will be used by multi_streak.  This
     is the subset of streak_times(mf(1)) which occur in all of the slave
     files.  The drat_start and drat_stop times work as usual.
 */
{
  if (!is_stream(f)) {
    mf= f;
    f= multif(mf(1));
  } else {
    mf= _multi_files;  /* if called as streak_times from streak function,
                          _multi_files is local to multi_streak */
  }
  if (!is_void(mf) &&
      abs(mf(1).tscale-1.0)>1.e-6) error, "tscale illegal in master file";
  times= _orig_streak_times(f);

  n= numberof(mf);
  for (i=2 ; i<=n ; i++) {
    mfi= mf(i);
    times= _multi_times(times, mfi.tscale*get_times(multif(mfi)));
  }

  /* give the multi package a chance to initialize just before the main
     loop over time is entered in the streak function */
  if (!is_void(_multi_init)) _multi_init;

  return times;
}

local multi_memory;
/* DOCUMENT multi_memory
     amount of memory used to determine size of spectral chunks.
     Default is 2000000, which keeps the memory required per chunk
     to under a few megabytes.
 */
multi_memory= 2000000;

func _multi__init
/* xxDOCUMENT multi_init
     is called immediately before the streak function enters its loop
     over time.  It is responsible for computing all of the photon group
     interpolation weights for the master and slave files.
 */
{
  extern _multi_files, _multi_gb;  /* local to multi_streak */

  /* build an appropriate bin boundary array if necessary */
  if (is_void(_multi_gb)) _multi_gb= multi_bins(_multi_files);

  /* compute a reasonable number of chunks, trying to limit memory
     usage to a few megabytes */
  extern _multi_nchunks;    /* local to multi_streak */
  kmx= dimsof(get_member(multif(_multi_files(1)), drat_rt));
  lmx= kmx(3);
  kmx= kmx(2);
  ngroup= numberof(_multi_gb)-1;
  _multi_nchunks= min(max(multi_memory/(8*8*kmx*lmx), 1), ngroup);
  if (ngroup%_multi_nchunks) _multi_nchunks= ngroup/_multi_nchunks + 1;
  else _multi_nchunks= ngroup/_multi_nchunks;

  /* compute the various indices and other data required for the
     interpolation and splitting into chunks */
  n= numberof(_multi_files);
  for (i=1 ; i<=n ; i++)
    _multi_chunk, i, _multi_gb, _multi_nchunks;
}

func _multi_integrate(f, mesh, time, irays, slimits)
/* DOCUMENT atten_emit= _multi_integrate(f, mesh, time, irays, slimits)
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
   SEE ALSO: streak, multi_streak
 */
{
  /* build arrays which will hold the opacity and emissivity chunks */
  ngroup= numberof(_multi_gb)-1;
  size= ngroup/_multi_nchunks;
  nlong= ngroup%_multi_nchunks;
  if (nlong) size++;
  /* local here, external to _multi_accum */
  opac= emiss= array(0.0, dimsof(get_member(f,drat_rt)), size);

  /* select the appropriate integrator */
  integrator= drat_linear? integ_linear : integ_flat;

  /* loop over spectral chunks */
  nfiles= numberof(_multi_files);
  n2= 0;
  for (i=1 ; i<=_multi_nchunks ; i++) {
    n1= n2+1;
    n2+= size;

    /* accumulate opacities and emissivities from all files */
    for (j=1 ; j<=nfiles ; j++)
      _multi_accum, _multi_files(j), _multi_gb(n1:n2+1), i;

    /* do transport integral for these rays cutting through current mesh */
    emiss/= opac+1.e-99;  /* do it here to reduce number of temporaries */

    /* apply absorption and emission multipliers */
    { extern drat_amult, drat_emult, drat_omult; }   /* options */
    if (!is_void(drat_emult)) emiss*= drat_emult;
    if (!is_void(drat_amult)) {
      opac*= drat_amult+1.e-20;
      emiss/= drat_amult+1.e-20;
    }
    if (!is_void(drat_omult)) opac*= drat_omult;

    /* point center the source function (in place) if requested */
    { extern drat_linear, drat_nomilne; }   /* options */
    if (drat_linear) {
      if (structof(emiss)!=double) emiss= double(emiss);
      pcen_source, opac, emiss, mesh, drat_nomilne;
    }

    chunk= integrator(opac, emiss, irays, mesh, slimits);

    if (i==1) {
      /* result is same as chunk except in spectral dimension,
         which is length ngroup instead of chunk size */
      dims= dimsof(chunk);
      dims(2)= ngroup;
      instant= array(0.0, dims);
    }
    instant(n1:n2,..)= chunk;
    chunk= [];  /* free memory ASAP */

    if (i==nlong) {
      /* ngroup%_multi_nchunks chunks are length ngroup/_multi_nchunks+1,
         the rest are length ngroup/_multi_nchunks
         Adjust size for the next pass -- note that nlong<_multi_nchunks
         by construction, so this can never happen on the last pass; it
         never happens at all if nlong==0.  */
      size--;
      dims= dimsof(opac);
      dims(0)= size;
      opac= emiss= [];  /* destroy before recreating */
      opac= emiss= array(0.0, dims);
    } else {
      opac(..)= emiss(..)= 0.0;  /* initialize for next pass */
    }
  }

  return instant;
}

/* ------------------------------------------------------------------------ */

struct MultiFile {
  string f;         /* name of the file variable for this file
                       -- no such thing as address of a file variable */

  string oname;     /* name of the opacity variable in f
                       -- default is "opac" if present in the file */
  string ename;     /* name of the emissivity variable in f, or 0 if
                       source function used instead
                       -- default is "emiss" if present in the file */
  string sname;     /* name of the source function variable in f,
                       ignored if ename!=0
                       -- default is "srcf" if present in the file */

  double oscale;    /* scale factor for correcting opacity units */
  double escale;    /* scale factor for correcting emissivity units
                       -- will be used to correct srcf*opac if srcf used */

  pointer zuse;     /* pointer to list of zone indices to use in this file
                       -- If &[], all zones in this file are used.
                       -- As a special case, if &array(0), use no zones.  */

  pointer zones;    /* list of master file zone indices for the zones which
                       are present in this post-processing file
                       -- The length of this list must be the same as the
                       product of the non-spectral dimensions of the opacity
                       and emissivity arrays, or the same length as *zuse,
                       if that array is present.
                       -- zones==0 means that all zones in the master file
                       (including zeroes in the first row and column) are
                       present in this file.  */

  /* This standard interface supports two spectral models:
     (1) Zone-centered model: Opacities and emissivities are defined as
         integrals over photon energy bins, so that the group structure
         is specified by a bin boundary energy list gb.
     (2) Point-centered model: Opacities and emissivities are defined at
         particular discrete photon energies, so that the group structure
         is specified by a photon energy list gav.
     In case (1), gav==0, and in case (2), gb==0.
     In either case, gb or gav must be a strictly increasing list of
     photon energies.  However, a single file may contain data in several
     disjoint spectral regions.  This information, together with spectral
     chunking information, is stored in the index array, nu.
   */
  pointer gb;       /* bin boundary array list for this file */
  pointer gav;      /* photon energy array list for this file */
  pointer gexist;   /* group existence map (if gb!=0) or connection map
                       (if gav!=0), or 0 if all groups exist or all
                       photon energies are connected */

  pointer gx;
  /* (*gx) is a pointer to a n_spectral_regions -by- 2 array
     of extreme values of each spectral region, used to limit the
     interpolation boundaries.  (*gx)(,1) is the lower boundary,
     while (*gx)(,2) is the upper boundary.  */

  pointer nu;
  /* (*nu) is 6 -by- n_spectral_regions -by- n_chunks index array:
     (*nu)(, spectral-region, chunk)
     can be written [i1,i2,i3,i4,i5,i6] where:

     gb(i1:i2+1) or gav(i1:i2) are the bins required to compute this
          chunk in this spectral region.
          i1==0 if, and only if, this file make no contribution at all
          to this chunk in this spectral region.
     multi_gb(i3:i4) are the master bins affected by this chunk
     If i6!=0, opacity(i4+1:i5) will be extrapolated as 1/nu^3 from the
     value at gb(i6:i6+1) or gav(i6).  This will be done even if i1==0.  */

  double tscale;    /* scale factor for correcting time units */

  int noextrap;  /* 1/nu^3 extrapolation will be done unless non-zero */

  int freqfirst;  /* frequency index is last unless non-zero */
}

func _multi_chunk(i, gb, nchunks)
/* xxDOCUMENT _multi_chunk, i, gb, nchunks
     computes the nu and gi members of the MultiFile struct _multi_files(I),
     so that the master bin boundary array GB can be computed in NCHUNKS
     chunks.
 */
{
  mf= _multi_files(i);
  /* get group structure out of file */
  gbf= *mf.gb;
  gavf= *mf.gav;
  gexist= *mf.gexist;
  if (is_void(gbf) && is_void(gavf)) return;

  /* compute chunk boundaries */
  nb= numberof(gb);
  ngroup= nb-1;
  if (nchunks>ngroup) nchunks= ngroup;
  chunk= array(ngroup/nchunks, nchunks);
  n= ngroup%nchunks;
  if (n) chunk(1:n)+= 1;
  chunk= chunk(cum);
  cbot= chunk(1:-1)+1;
  ctop= chunk(2:0);

  /* get j1= indices of beginnings of spectral regions,
         j2= indices of ends of spectral regions
     in file bins */
  j1= _multi_spectreg(gexist, gbf, gavf);
  j2= j1(,2);
  j1= j1(,1);

  /* get master bin numbers containing j1 and j2
     -- after this, bot and top are bin number containing the
        bottom and top of each of the spectral regions
     -- 1 and ngroup+2 are still possible if the region partially
        hangs over the master bin structure */
  if (!is_void(gbf)) {
    gf= gbf;
    nf= max(j2)-1;   /* top group index in file */
  } else {
    gf= gavf;
    nf= max(j2);
  }
  gbot= gf(j1);
  gtop= gf(j2+(!is_void(gbf)));
  bot= digitize(gbot, gb)-1;
  top= digitize(-gtop, -gb)-1;  /* if one of the gtop is exactly equal to
                                   one of the gb, want the smaller bin
                                   for top (see digitize help) */

  /* get the master group indices of the minimum i3 and maximum i4 group
     which can be affected by each spectral region for each chunk
     -- note that i3>=1 and i4<=ngroup are guaranteed after this, so
        gb(i3) and gb(i4+1) below will never blow up */
  i3= max(bot, cbot(-,));
  i4= min(top, ctop(-,));

  /* get minimum i1 and maximum i2 bin indices in file
     which must be extracted for each spectral region and each chunk
     -- note that i3>i4 in empty spectral regions, which will be
        removed later */
  i1= digitize(gb(i3), gf)-1;
  i2= digitize(-gb(i4+1), -gf);  /* see digitize comment above */
  if (!is_void(gbf)) i2--;  /* top group one less than top bin boundary */
  /* can't retrieve anything beyond min(gf) and max(gf) */
  i1= max(1, i1);
  i2= min(nf, i2);

  /* zero empty spectral region/chunks -- they haven't been computed
     correctly anyway */
  list= where(i3>i4);
  if (numberof(list)) i1(list)= i2(list)= i3(list)= i4(list)= 0;

  /* find spectral regions where extrapolation is necessary */
  cbot-= 1;
  cbot= cbot(-:1:numberof(j2),);
  i5= i6= 0*i1;
  list= where(top<ctop(-,));  /* see definition of i4 above */
  if (!mf.noextrap && numberof(list)) {
    i6(list)= j2(,-:1:numberof(ctop))(list);
    i5(list)= ctop(-:1:numberof(j2),)(list);
    i4e= i4(list);
    l= where(i4e==0);
    if (numberof(l)) {
      /* fix any chunks which require extrapolation, but which are
         completely above the spectral region being extrapolated */
      i4e(l)= cbot(list)(l);
      i4(list)= i4e;
    }
  }

  /* adjust i3, i4, and i5 to apply to the current chunk, rather
     than to the entire gb array */
  list= where(i3);
  if (numberof(list)) i3(list)-= cbot(list);
  list= where(i4);
  if (numberof(list)) i4(list)-= cbot(list);
  list= where(i5);
  if (numberof(list)) i5(list)-= cbot(list);

  /* install index lists into mf */
  _multi_files(i).nu= &transpose([i1,i2,i3,i4,i5,i6],2);

  /* generate the interpolation bin limits for each spectral region
     and install into mf */
  _multi_files(i).gx= &[gbot, gtop];
}

func _multi_spectreg(gexist, gbf, gavf)
/* xxDOCUMENT _multi_spectreg(gexist, gbf, gavf)
     crack GBF (if non-nil) or GAVF (if GBF nil) into spectral regions
     according to GEXIST.  Returns 2-by-n_spectral_regions array of
     minimum and maximum group indices for each spectral region.
     In the case of GBF, GEXIST is a bin existence map, dimension one
     less than GBF, non-zero in bins which exist.
     In the case of GAVF, GEXIST is a photon connectivity array, dimension
     one less than GAVF, zero between consecutive elements of GAVF which
     are not connected.  Single, isolated points are removed entirely.
     In either case, GAVF or GBF must be a strictly increasing list.
 */
{
  n= numberof(gbf);
  if (is_void(gexist)) {
    j1= [1];
    j2= [(n? n-1 : numberof(gavf))];
  } else {
    gexist= grow([0], (gexist!=0), [0]);
    gexist= gexist(dif);
    j1= where(gexist>0);  /* off to on transitions */
    j2= where(gexist<0);  /* on to off transitions */
    if (numberof(j2)) {
      if (n) {
        j2--;   /* bin index, not boundary index */
      } else if (anyof(j2==j1+1)) {
        list= where(j2>j1+1);
        j1= j1(list);
        j2= j2(list);
      }
    }
    if (numberof(j2)<1) error, "group existence map for file is void";
  }
  return [j1,j2];
}

func _multi_accum(mf, gb, chunk)
/* xxDOCUMENT _multi_accum, mf, gb, chunk
     increments externals opac, emiss from MultiFile MF, chunk number
     CHUNK.  GB is the master bin boundary array.
 */
{
  /* unpack the mf MultiFile struct */
  nu= mf.nu;
  if (!nu) return;         /* detect no-op quickly */
  nu= (*nu)(,,chunk);
  if (noneof(nu)) return;  /* detect no-op quickly */
  f= multif(mf);
  oname= mf.oname;
  ename= mf.ename;
  sname= mf.sname;
  oscale= mf.oscale;
  escale= mf.escale;
  freqfirst= mf.freqfirst;
  zuse= *mf.zuse;   /* if scalar 0, nu==&[] above, and won't get here */
  zones= *mf.zones;
  gbf= *mf.gb;
  gavf= *mf.gav;
  gx= (*mf.gx);
  gbot= gx(,1);
  gtop= gx(,2);

  n= dimsof(nu)(3);
  for (i=1 ; i<=n ; i++) {
    /* loop over disjoint spectral regions in this file
       -- this is a little inefficient, as presumably at most two
          adjacent ones will have non-zero i1 */
    i1= nu(1,i);
    i2= nu(2,i);
    i3= nu(3,i);
    i4= nu(4,i);
    i5= nu(5,i);
    i6= nu(6,i);

    if (i1) {
      /* compute integral of opacity and emissivity over this chunk,
         interpolated onto the master bin structure */
      gbsub= gb(i3:i4+1);
      gbm= min(max(gbsub,gbot(i)),gtop(i));
      /* NOTE: oraw is two-dimensional */
      if(freqfirst) {
        oraw= transpose(get_member(f,oname)(i1:i2,*));
      } else {
        oraw= get_member(f,oname)(*,i1:i2);
      }
      if (!is_void(gbf)) {
        /* zone-centered spectral model */
        g12= gbf(i1:i2+1);
        dg12= g12(-,dif);
        if (is_void(gavf)) o= interp((oraw*dg12)(,cum), g12, gbm, 2);
        else o= integ(oraw, gavf(i1:i2), gbm, 2);
        if (ename) {
          oraw= [];
          if(freqfirst) {
            e= transpose(get_member(f,ename)(i1:i2,*));
            e= interp((e*dg12)(,cum), g12, gbm, 2);
          } else {
            e= interp((get_member(f,ename)(*,i1:i2)*dg12)(,cum), g12, gbm, 2);
          }
        } else {
          if(freqfirst) {
            e= transpose(oraw*get_member(f,sname)(i1:i2,*));
            e= interp((e*dg12)(,cum), g12, gbm, 2);
          } else {
            e= interp((oraw*get_member(f,sname)(*,i1:i2)*dg12)(,cum),
                      g12, gbm, 2);
          }
          oraw= [];
        }
      } else {
        /* point-centered spectral model */
        g12= gavf(i1:i2);
        dg12= g12(-,dif);
        o= integ(oraw, g12, gbm, 2);
        if (ename) {
          oraw= [];
          if(freqfirst) {
            e= transpose(get_member(f,ename)(i1:i2,*));
            e= integ(e, g12, gbm, 2);
          } else {
            e= integ(get_member(f,ename)(*,i1:i2), g12, gbm, 2);
          }
        } else {
          if(freqfirst) {
            e= transpose(oraw*get_member(f,sname)(i1:i2,*));
            e= integ(e, g12, gbm, 2);
          } else {
            e= integ(oraw*get_member(f,sname)(*,i1:i2), g12, gbm, 2);
          }
          oraw= [];
        }
      }

      /* differentiate and scale */
      dgb= gbsub(-,dif);
      o= o(,dif)/(dgb/oscale);
      e= e(,dif)/(dgb/escale);

      /* accumulate into opac and emiss externals */
      if (!is_void(zuse)) {
        /* only a subset of the zones present in this file contribute */
        o= o(zuse,);
        e= e(zuse,);
      }
      if (is_void(zones)) {
        /* all zones represented in this file */
        opac(*,i3:i4)+= o;
        emiss(*,i3:i4)+= e;
      } else {
        /* subset of zones in this file */
        opac(zones,1,i3:i4)+= o;
        emiss(zones,1,i3:i4)+= e;
      }
    }

    if (i6) {
      /* must extrapolate opacity as 1/hnu^3 and emissivity as
         exp(-hnu/kT) */
      /* first, get gb3, proportional to the integral of 1/hnu^3 across
         the master bins gb (assumed external) */
      gb3= gb(i4+1:i5+1);
      gb3= gb3(zcen)/(gb3(1:-1)*gb3(2:0))^2;
      if (!is_void(gbf)) gf= avg(gbf(i6:i6+1));
      else gf= gavf(i6);
      gb3*= gf^3;

      /* next, get the opacity and emissivity in bin i6 of this file */
      if(freqfirst) {
        o= transpose(get_member(f,oname)(i6,*));
      } else {
        o= get_member(f,oname)(*,i6);
      }
      if (ename) {
        if(freqfirst) {
          e= transpose(get_member(f,ename)(i6,*));
        } else {
          e= get_member(f,ename)(*,i6);
        }
      } else {
        if(freqfirst) {
          e= transpose(o*get_member(f,sname)(i6,*));
        } else {
          e= o*get_member(f,sname)(*,i6);
        }
      }

      if (!is_void(zuse)) {
        /* only a subset of the zones present in this file contribute */
        o= o(zuse);
        e= e(zuse);
      }
      if (is_void(zones)) {
        /* all zones represented in this file */
        opac(*,i4+1:i5)+= o*(gb3(-,)*oscale);
        emiss(*,i4+1:i5)+= e*(gb3(-,)*escale);
      } else {
        /* subset of zones in this file */
        opac(zones,1,i4+1:i5)+= o*(gb3(-,)*oscale);
        emiss(zones,1,i4+1:i5)+= e*(gb3(-,)*escale);
      }
    }

    /* free temporary storage before next pass */
    o= e= [];

  }  /* end of loop over spectral regions */
}

local multi_opac, multi_emiss, multi_srcf;
/* DOCUMENT multi_opac, multi_emiss, multi_srcf
     are the default names of the opacity, emissivity, and source function
     arrays in post-processing files.  By default, they are "opac",
     "emiss", and "srcf", respectively.  If none of these are present,
     drat_akap and drat_ekap will also be tried for the opacity and
     source function, respectively.  If emissivity is present, source
     function will be ignored.
     The units of opacity are inverse length, of emissivity power per
     photon energy per sterradian per volume, and source function
     power per photon energy per sterradian per area.
 */

local multi_gb, multi_gav, multi_gexist;
/* DOCUMENT multi_gb, multi_gav, multi_gexist
     are the default names of the group boundary, group center, and group
     existence arrays in post-processing files.  By default, they are
     "gb", "gav", and "gexist".  If neither is present, drat_gb and
     drat_gav will also be tried.
     If present, gb is the photon bin boundary array, and gexist (if
     present) has one fewer element and is non-zero in bins which exist.
     Otherwise, gav is the photon energy array, and gexist (if present)
     has one fewer element and is zero between elements of gav which are
     not connected.  Isolated points in gav are removed entirely.
     Either gb or gav must be strictly increasing, and has units of
     photon energy.
 */

local multi_zonelist;
/* DOCUMENT multi_zonelist
     is the default name of the variable which is a list of 1-origin
     zone indices in the mesh of the master file.
 */

func _multi_names(mf, gbname=, gavname=, gexistname=, gscale=, zonesname=)
/* xxDOCUMENT _multi_names, mf, gbname=..., gavname=...,
                            gexistname=..., gscale=..., zonesname=...
     find all relevant variable names in the file specified by MF.f.
     Reads in the gb (or gav) and gexist arrays, setting MF.gb, MF.gav,
     and MF.gexist appropriately, multiplying the values read from the
     file by GSCALE if supplied.
     Reads in MF.zones if not already set and if zonelist is present in
     the file.
     If MF.oscale or MF.escale is 0.0, it is set to 1.0; if less than
     zero, it is set to 0.0.
     If the routine fails, mf.oname will be string(0) on output.
 */
{
  f= multif(mf);
  vars= get_vars(f);

  zuse= *mf.zuse;
  contrib= (is_void(zuse) || zuse(1));

  if (contrib) {

    if (!mf.zones) {
      if (is_void(zonesname)) zonesname= multi_zonelist;
      if (is_present(vars, zonesname)) zones= get_member(f, zonesname);
      if (!is_void(zones) && !is_void(zuse)) zones= zones(zuse);
      mf.zones= &zones;
    }

    if (mf.oname && !is_present(vars, mf.oname)) mf.oname= string(0);
    if (!mf.oname) {
      if (is_present(vars, multi_opac)) mf.oname= multi_opac;
      else if (is_present(vars, drat_akap)) mf.oname= drat_akap;
    }

    if (mf.ename && !is_present(vars, mf.ename)) mf.ename= string(0);
    if (mf.sname && !is_present(vars, mf.sname)) mf.sname= string(0);
    if (!mf.ename && !mf.sname) {
      if (is_present(vars, multi_emiss)) mf.ename= multi_emiss;
      else if (is_present(vars, multi_srcf)) mf.sname= multi_srcf;
      else if (is_present(vars, drat_ekap)) mf.sname= drat_ekap;
      else mf.oname= string(0);
    }

    if (!mf.gb && !mf.gav && mf.oname) {
      if (is_void(gbname) && is_void(gavname)) {
        if (is_present(vars, multi_gb)) gbname= multi_gb;
        else if (is_present(vars, drat_gb)) gbname= drat_gb;
        if (is_present(vars, multi_gav)) gavname= multi_gav;
        else if (is_present(vars, drat_gav)) gavname= drat_gav;
      } else {
        if (!is_void(gbname) && !is_present(vars, gbname))
          error, "gbname= "+gbname+" is not present in file";
        if (!is_void(gavname) && !is_present(vars, gavname))
          error, "gavname= "+gavname+" is not present in file";
      }
      if (is_void(gexistname)) {
        if (is_present(vars, multi_gexist)) gexistname= multi_gexist;
      } else {
        if (!is_void(gexistname) && !is_present(vars, gexistname))
          error, "gexistname= "+gexistname+" is not present in file";
      }
      if (!gscale) gscale= 1.0;

      if (!is_void(gexistname)) mf.gexist= &get_member(f, gexistname);
      if (!is_void(gbname)) mf.gb= &(gscale*get_member(f, gbname));
      if (!is_void(gavname)) mf.gav= &(gscale*get_member(f, gavname));
      if (!mf.gb && !mf.gav) mf.oname= mf.ename= string(0);
    }

    if (mf.oscale==0.0) mf.oscale= 1.0;
    else if (mf.oscale<0.0) mf.oscale= 0.0;
    if (mf.escale==0.0) mf.escale= 1.0;
    else if (mf.escale<0.0) mf.escale= 0.0;

  } else {
    mf.oname= string(0);
    mf.gb= mf.gav= &[];
  }
}

/* ------------------------------------------------------------------------ */
/* procedures to assist in combining different sets of photon bins, or
   otherwise generating photon group structure */

func multi_bav(gav)
/* DOCUMENT multi_bav(gav)
     returns bin boundaries for the bin centers gav.
     The bin boundaries are taken at the geometric means between
     consecutive gav(i), with the endpoints extended slightly beyond
     the endpoints of gav.
 */
{
  gb= log(gav + 1.e-99)(pcen);
  gb(1)-= gb(2)-gb(1);
  gb(0)+= gb(0)-gb(-1);
  return exp(gb);
}

func _multi_bins(nfinal, ..)
/* DOCUMENT gb= _multi_bins(nfinal, gb1, gb2, ...)
     returns NFINAL+1 boundaries of NFINAL bins constructed by combining
     the input bin structures GB1, GB2, etc.
     Use NFINAL=0 to get at least the resolution in the finest GBi in
     every region of the spectrum.

     This is done by constructing a total bin density function
     (#bins/energy width), as the maximum of the bin density of each
     component.  This total bin density function is integrated, and
     the integral is divided into NFINAL equal parts; the points in
     energy at which this division must be made are the returned bin
     boundaries.

     In the returned bin structure, the density of bins is everywhere
     proportional to the densest bins in any of the GBi.
 */
{
  boundary= density= [];
  while (more_args()) {
    /* get next input boundary array */
    gb= next_arg();
    if (is_void(gb)) continue;
    gd= 1./gb(dif);  /* bin density is zone centered -- constant
                        between the bin boundaries */
    nb= numberof(boundary);

    if (!nb) {
      /* first pass */
      boundary= gb;
      density= gd;

    } else {
      /* subsequent passes */
      /* combine lists of bin boundaries, sort into increasing order,
         and eliminate any duplicates */
      combine= grow(boundary, gb);
      combine= combine(sort(combine));
      combine= multi_no_dups(combine);

      /* get lists of indices of the center of each of the new bins in
         the old boundary and gb arrays */
      comcen= combine(zcen);
      list1= digitize(comcen, boundary);
      list2= digitize(comcen, gb);

      /* add guard zones for points beyond boundary or gb separately */
      density= grow([0.], density, [0.]);
      gd= grow([0.], gd, [0.]);

      /* the new density is the maximum of the two component densities,
         and the new boundary is the combination of the two component
         boundaries */
      density= max(density(list1), gd(list2));
      boundary= combine;
    }
  }
  if (is_void(boundary)) return [];

  /* now have a histogram density(boundary) of the maximum bin density
     between every energy mentioned in any of the gb -- the integral of
     this histogram is the piecewise linear function total(boundary): */
  total= (density*boundary(dif))(cum);

  /* note that the last point, total(0) is the "natural" number of bins
     required to get at least the spectral resolution in any of the
     input gb */
  if (nfinal<1) nfinal= long(total(0)+0.99);

  /* should this be enhanced to allow the caller to specify endpoints
     other than boundary(1) and boundary(0)? */
  return interp(boundary, total/total(0), span(0.,1.,nfinal+1));
}

func multi_line(nbins, hnu0, dhnu, dhnu_min)
/* DOCUMENT gb= multi_line(nbins, hnu0, dhnu, dhnu_min)
     returns 2*NBINS+1 bin boundary energies for 2*NBINS bins
     cenetered around a spectral line at HNU0 of width DHNU.  The
     result begins at HNU0-DHNU and ends at HNU0+DHNU.  The finest
     two bins (nearest HNU0) has width DHNU_MIN, and the remaining
     bins have equal ratio widths as you move away from HNU0.
 */
{
  dhnu= double(dhnu);
  r= 0.;
  if (dhnu_min && dhnu_min>0. && dhnu_min<dhnu) {
    require, "series.i";
    r= gseries_r(dhnu/dhnu_min, nbins);
  }
  if (r>1.) widths= dhnu_min*exp(log(r)*indgen(0:nbins-1));
  else widths= dhnu/nbins;
  widths= grow(widths(0:1:-1), widths);
  return hnu0-dhnu + widths(cum);
}

/* ------------------------------------------------------------------------ */
/* random worker routines */

func multi_no_dups(x)
/* DOCUMENT xnd= multi_no_dups(x)
     returns its input vector X with any duplicate values removed.
     X must be non-decreasing and of length at least two.
 */
{
  return x(grow([1], where(x(dif)>0.0)+1));
}

func _multi_times(tmaster, tslave, terror=)
/* xxDOCUMENT times_list= _multi_times(tmaster, tslave)
     return the subset of TMASTER which occurs in TSLAVE.  Times in
     TSLAVE which do not appear in TMASTER are ignored.
     The optional TERROR keyword determines the accuracy with which
     TSLAVE must match TMASTER in order to be included.  TERROR may
     be a scalar, or an array of length TMASTER.  By default TERROR
     is 0.01 of the spacing between successive elements of TMASTER
     (see code for the exact expression).
 */
{
  /* handle single points and other common annoying cases */
  if (numberof(tslave)<1) return [];
  if (dimsof(tslave)(1)!=1) tslave= tslave(*);
  if (numberof(tslave)>1 && anyof(tslave(dif)<=0.))
    tslave= multi_no_dups(tslave(sort(tslave)));
  if (dimsof(tmaster)(1)!=1) tmaster= tmaster(*);
  if (numberof(tmaster)<2) {
    if (numberof(tmaster)<1) return [];
    if (is_void(terror)) terror= max(1.e-6, 1.e-6*abs(tmaster));
    if (anyof(abs(tslave-tmaster)<=terror)) return tmaster;
    else return [];
  }

  /* create default terror widths if not supplied */
  if (is_void(terror)) {
    dt= tmaster(pcen)(dif);
    dt(1)*= 2.0;
    dt(0)*= 2.0;  /* never same as dt(1) since at least two times */
    terror= 0.01*dt;
  }

  /* get list of indices of largest and smallest possible match
     of tslave in tmaster --
     the required points are where these are equal */
  list1= digitize(tslave, tmaster+terror);
  list2= digitize(tslave, tmaster-terror)-1;
  list= where(list1==list2);
  if (numberof(list)) return tmaster(list1(list));
  else return [];
}

/* ------------------------------------------------------------------------ */
