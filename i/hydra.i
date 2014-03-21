/*
 * $Id: hydra.i,v 1.6 2010-08-29 16:24:44 dhmunro Exp $
 * functions to access hydra-generated Silo/PDB files
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

_h_i_version = 1.01;

local hydra;
/* DOCUMENT hydra.i
   defines several functions useful for examining and extracting
   data from hydra-generated Silo/PDB dump files:

   h_openb     -- use instead of openb for hydra files
   hydra_xyz   -- extracts xyz and boundary arrays
   h_data      -- extracts data nodal or zonal arrays
   h_array     -- extracts data nodal or zonal arrays for one ublk
   h_mix       -- extracts zonal data for mixed zones
   h_iparm     -- extracts integer parameter values
   h_gblk      -- extracts information relating hblks to
                  user blocks
   h_collect   -- loops on h_array over all times

   SEE ALSO: h_openb, hydra_xyz, h_data, h_array, h_mix,
             h_iparm, h_fparm, h_gblk
 */

/*
 * NOTES:
 * (1) legacy format
 *     support for legacy file format does not allow distributed (.root)
 *     files, but in principle allows time history families
 * (2) ndims
 *     assume that dimensionality of /hblk0/hydro_mesh_coord0 variable
 *     in the first file of the family is ndims
 * (3) assume the times in a history family are in alphabetical order
 *     of their filenames, only include files with names alphabetically
 *     greater than the filename passed to h_openb
 */

/* ------------------------------------------------------------------------ */

func h_openb(name, one=)
/* DOCUMENT f = h_openb(filename)

     open a hydra dump file, including 2D families of distributed
     history files.

     The return value is a list (see _lst function) containing the
     currently opened file and the non-PDB data required to navigate
     through each file and the entire family.

     With one=1 keyword, only one file of a history family is opened.

   SEE ALSO: h_close, hydra_xyz, h_data, h_jt, h_jr, h_get_times
 */
{
  f = _h_openb(name);
  tmp = print(f);
  dir = strtok(strtok(tmp(1:2),":")(2,))(1,);
  name = dir(1);  /* non-directory part of name */
  dir = dir(2);   /* directory name */
  if (numberof(tmp)>2) {
    tmp = tmp(3:0);
    dir = strpart(dir, 1:-1);
    if (numberof(tmp)>1) tmp(1:-1) = strpart(tmp(1:-1), 1:-1);
    for (n=1 ; n<=numberof(tmp) ; ++n) dir += tmp(n);
  }
  tmp = [];
  root = (strpart(name,-4:0) == ".root");
  if (root) name = strpart(name,1:-5);

  _h_legacy = 0;  /* 1 to use obsolete hydra format */
  if (!root && _h_autodetect)
    _h_legacy = noneof((*get_vars(f)(1))=="/Global/");
  _h_mapfmt = 0;
  if (!_h_legacy) {
    vars = *(get_vars(f)(1));
    if (anyof(strmatch(vars, "/Decomposition/Block_Extents")))
      _h_mapfmt = 1;
    else if (anyof(strmatch(vars, "/Decomposition/Block_Decomposition")))
      _h_mapfmt = 2;
    if (anyof(strmatch(vars, "/Decomposition/Domain_Extents")))
      _h_mapfmt += 2;
    else if (anyof(strmatch(vars, "/Decomposition/Domain_Decomposition")))
      _h_mapfmt += 4;
    if (_h_mapfmt && _h_mapfmt<3)
      error, "unrecognized umap/gmap format";
  }

  sfx = strpart(name,-3:0);
  n = 0;
  if (sread(sfx,n) && swrite(format="%04ld",n)==sfx) {
    /* filename (excluding any .root) ends in four digits */
    pfx = strpart(name,1:-5);  /* suffix is either 4 or 5 digits */
    n = strlen(pfx);
    fifth = strpart(name,n+1:n+1);
    has_five = ((fifth>="0") && (fifth<="9"));
    if (!has_five) {
      /* if this is obsolete naming style, assume pbnm has 4 characters */
      if (n!=3) pfx = strpart(name,1:3);
    } else {
      for (;;) {
        fifth = strpart(pfx, 0:0);
        if ((fifth<"0") || (fifth>"9")) break;
        ++has_five;
        pfx = strpart(pfx, 1:-1);
        --n;
      }
      /* has_five= number of digits beyond 4, pfx= non-digit prefix */
    }
    if (n) {
      list = lsdir(dir);
      if (root && numberof(list)) {
        list = list(where(strpart(list,-4:0)==".root"));
        if (numberof(list)) list = strpart(list,1:-5);
      }
      if (numberof(list)) {
        list = list(where(strpart(list,1:strlen(pfx)) == pfx));
        if (has_five) list = list(where(strlen(list) >= n+4+has_five));
        else list = list(where(strlen(list) == n+5));
      }
      if (numberof(list) > 1) {
        list = list(msort(strlen(list),list));
        final = strpart(list,n+1:n+1);
        if (has_five) {
          /* all new files have >=five digit numeric suffix, fixed prefix */
          list = list(where((final>="0") & (final<="9")));
        } else {
          /* this is obsolete naming convention */
          ok = (final==fifth);
          if (nallof(ok)) {
            fdigit = (final>="0") & (final<="9");
            /* make non-digit fifth character files come first in list */
            last = list(where( (!ok) & fdigit ));
            list = list(where(ok));
            grow, list, last;
            /* note that "g" or "opc" or restart have already been
             * weeded out by the fact that the filename length
             * would be 1 or 3 characters longer than a restart dump */
          }
        }
        if (numberof(list)) {
          ok = array(1n, numberof(list));
          nmax = has_five? n+4+has_five : n+5;
          len = strlen(list);
          for (i=n+2 ; i<=nmax ; i++) {
            final = strpart(list, i:i);
            ok &= ((final>="0") & (final<="9")) | (len<i);
          }
          list = list(where(ok));
        }
        if (numberof(list) > 1) {
          i = where(list==name);
          if (numberof(i)) {
            /* this file is member of a history family */
            if (!one)
              name = list(i(1):0);  /* only take this file and later */
            else
              name = list(i(1));
          }
        }
      }
    }
  }
  /* name is now either the original scalar name (excluding any directory)
   * or an array of names in a history family
   * if name ended in ".root", that is not included in name value(s)
   */
  name = dir+name;

  /* scan through files to get cycle and time, umap and gmap
   * this collects everything necessary to find variables except for
   * the dimensionality of the problem (ndims), which cannot be easily
   * determined until the first non-root file is read
   * all the relevant information in .root files is read here,
   * they will not be opened again */
  data = array(pointer, 3, numberof(name));
  time = array(-1.e20, numberof(name));
  ncyc = array(-1, numberof(name));
  if (root) {
    sfx = ".root";
  } else {
    sfx = "";
    f0 = f;
  }
  ihist = 1;
  for (i=1 ; i<=numberof(name) ; i++) {
    if (i>1) f = _h_openb(name(i)+sfx);
    data(,i) = _h_read_maps(f, (root?dir:name(i)), root);
    if (!numberof(time)) continue;
    vars = *(get_vars(f)(1));
    if (anyof(vars=="/dtime")) time(i) = get_member(f,"/dtime")(1);
    if (anyof(vars=="/cycle")) ncyc(i) = get_member(f,"/cycle")(1);
  }
  if (numberof(time) && noneof(time > -1.1e20)) time = [];
  if (numberof(ncyc) && noneof(ncyc > -1)) ncyc = [];
  if (is_void(f0))
    f0 = _h_openb((*data(1,1))(1));
  /* is this always same as h_iparm(f0, "ndims") ?? */
  ndims = dimsof(get_member(f0, "/hblk0/hydro_mesh_coord0"))(1);

  return _lst(f0, [1,1,ndims,_h_legacy, _h_mapfmt], data, time, ncyc);
}

func h_close(&f)
/* DOCUMENT h_close, f

     close a file F opened with h_openb.

   SEE ALSO: h_openb
 */
{
  ff = _car(f,1);
  _car, f, 1, [];
  _car, f, 3, [];
  f = [];
  if (is_stream(ff)) close, ff;
}

func h_get_times(f)
/* DOCUMENT times = h_get_times(f)

     return array of times in hydra history file family F.

   SEE ALSO: h_data, h_openb, h_jt, h_jr
 */
{
  return _car(f, 4);
}

func h_jt(f, time)
/* DOCUMENT h_jt, f, time

     jump to time TIME in hydra history file family F.

   SEE ALSO: h_data, h_openb, h_get_times, h_jr, h_collect
 */
{
  if (is_stream(f)) return jt(f, time);
  times = _car(f, 4);
  if (is_void(times)) error, "no times in file";
  ihist = abs(times-time)(mnx);
  flags = _car(f, 2);
  if (flags(2) != ihist) {
    flags(2) = ihist;
    _car, f, 2, flags;
    _car, f, 1, [];
  }
}

func h_jr(f, irec)
/* DOCUMENT h_jr, f, irec
         or nrecs = h_jr(f)

     jump to record IREC in hydra history file family F.
     In second form, return total number of records in family.

   SEE ALSO: h_data, h_openb, h_get_times, h_jt, h_collect
 */
{
  if (is_stream(f)) return jr(f, irec);
  nrecs = dimsof(_car(f, 3));
  if (nrecs(1)==2) nrecs = nrecs(3);
  else nrecs = 0;
  if (!is_void(irec)) {
    if (irec>nrecs || irec<1)
      error, "no such record as "+print(irec)(1)+" in file";
    flags = _car(f, 2);
    if (flags(2) != irec) {
      flags(2) = irec;
      _car, f, 2, flags;
      _car, f, 1, [];
    }
  }
  return nrecs;
}

func h_show(f)
/* DOCUMENT h_show, f
         or varnames = h_show(f)

     prints names of variables available for h_data, h_mix, h_array.

   SEE ALSO: h_data, h_openb, h_ushow
 */
{
  { local _h_legacy; }
  vars = *get_vars(_h_get_file(f, 1, , _h_legacy))(1);
  list = where(strpart(vars,1:7)=="/hblk0/");
  if (numberof(list)) {
    /* this coding must be consistent with _h_translate */
    vars = strpart(vars(list),8:0);
    vars = vars(where(strlen(vars)>0));
    vars = vars(where(!strmatch(vars,"/")));
    list = where(vars=="Materials_matlist");
    if (numberof(list)) vars(list) = "Materials";
    mask = (vars=="hydro_mesh_coord0") + 2*(vars=="hydro_mesh_coord1") +
      3*(vars=="hydro_mesh_coord2");
    list = where(mask);
    if (numberof(list)) vars(list) = ["x","y","z"](mask(list));
    mask = strmatch(vars,"_");
    list = where(mask);
    if (numberof(list)) {
      v = strpart(vars(list), -1:-1);
      i = where(v == "_");
      if (numberof(i)) v(i) = strpart(vars(list(i)), 0:0);
      i = where((v>="0") & (v<="9"));
      if (numberof(i)) mask(list(i)) = 0;
    }
    vars = vars(where(!mask));
    vars = vars(sort(vars));
  }
  if (am_subroutine() && numberof(vars)) {
    write, vars;
    nrecs = h_jr(f);
    if (nrecs > 1) {
      nt = numberof((time=h_get_times(f)));
      if (nt)
        write, format=" Found %ld history dumps from time=%e to time=%e\n",
          nt, time(1), time(0);
      else
        write, format=" Found %ld history dumps (no times available)\n", nt;
    }
  }
  return vars;
}

func hydra_xyz(f, ublk, i0, j0, k0, face)
/* DOCUMENT mesh = hydra_xyz(f)
         or mesh = hydra_xyz(f, ublk, i0, j0, k0, face)
         or mesh = hydra_xyz(f, ublk, i0, j0, k0)

     read a 3D mesh object from the hydra PDB/Silo file F.
     The returned mesh is _lst(xyz, bound, mbnds, blks, start).

     Note that the boundary arrays are adjusted to the hex convention
     that cells with i=1, j=1, k=1 are missing, rather than the hydra
     convention that i=imax, j=jmax, k=kmax are missing.

     In the first form, the ray entry search will start on the
     first open boundary face in the mesh.  If the actual problem
     boundary is not convex, you need to identify a surface of
     constant i, j, or k in the problem which is convex, and which
     all the rays you intend to trace intersect.
     UBLK is the user block number (starting from 0),
     I0, J0, K0 are the (1-origin) logical coordinates of a
       hydra *cell*.  Note that unlike hex cells, the hydra
       cell bounded by nodes (1,1,1) and (2,2,2) is numbered (1,1,1).
       (Hex numbers it (2,2,2).)
     FACE is the face number on cell (I0,J0,K0) which you want a
       ray to enter.  0 means the -I face, 1 the +I face, 2 the -J
       face, 3 the +J face, 4 the -K face, and 5 the +K face.
       As you step from this cell to its neighbors, then to their
       neighbors, and so on, this face must trace out a convex
       surface for the ray entry search.  Rays not intersecting
       this surface will not enter the problem; the ray trace
       will begin at this surface, not at -infinity.

     If FACE==-1 or is omitted (as in the third form), then the
     given points on the rays are assumed to lie inside the mesh,
     and a pseudo ray from the centroid of cell (I0, J0, K0) will be
     tracked to the given point on each ray; the ray will be launched
     into the cell containing that point.

     You can set a hydra_bnd_hook function before calling hydra_xyz
     if the boundary conditions for hex need to be different than
     for hydra.

   SEE ALSO: hydra_bnd_hook, h_data, h_openb,
             hydra_aux_data, hydra_mix_data
 */
{
  { local mdims, mlens, bnum, _h_legacy, ndims, gmap; }
  nblk = h_blocks(f, mdims, mlens, _h_legacy, ndims, gmap);
  dim2 = dim3 = call(2:0);
  if (ndims < 3) {
    dim3 = 1;
    if (ndims < 2) dim2 = 1;
  }
  ncoords = 3-(ndims<3);

  /* compute strides and global offsets for all the mesh blocks */
  blo = array(0, 4, nblk);
  blo(2:4,) = mdims;
  lscratch = hydra_blks(nblk, blo);

  /* allocate the global mesh array and read it from disk
   * read bc and bnd information at same time to avoid multiple
   * loops in the case of multiple files */
  if (nblk>1) xyz = array(0., ncoords, sum(mlens));
  else        xyz = array(0., ncoords, grow([ndims],mdims(,1)));
  off = 0;
  binfo = array(0, 4, nblk);
  bndtmp = array(pointer, nblk);
  bctmp = array(pointer, 2, nblk);

  /* set up for concurrent read of auxilliary data */
  extern hydra_aux_data, hydra_mix_data;
  naux = numberof(hydra_aux_names);
  if (naux) {
    aux_names = _h_translate(hydra_aux_names);
    hydra_aux_data = array(pointer, numberof(aux_names));
  }
  nmix = numberof(hydra_mix_names);
  if (nmix) {
    mix_names = _h_xtranslate(hydra_mix_names);
    hydra_mix_data = array(pointer, numberof(mix_names)+1);
    qdata = array(pointer, nblk);
    ncmix = array(0, nblk);
    cmix = lmix = array(pointer, nblk);
    if (nblk>1) matlist = array(0., sum(mlens));
    else        matlist = array(0., grow([ndims],mdims(,1)));
  }

  for (i=1 ; i<=nblk ; i++) {
    ff = _h_get_file(f, i, bnum);
    off0 = off+1;
    off += mlens(i);
    xyz(1,off0:off) =
      get_member(ff,swrite(format="/hblk%ld/hydro_mesh_coord0",bnum))(*);
    xyz(2,off0:off) =
      get_member(ff,swrite(format="/hblk%ld/hydro_mesh_coord1",bnum))(*);
    if (ncoords>2)
      xyz(3,off0:off) =
        get_member(ff,swrite(format="/hblk%ld/hydro_mesh_coord2",bnum))(*);
    /* read [nbc, nbnd, jp, kp] */
    binfo(,i) = get_member(ff,_h_varname(bnum,"hydrodati"))([34,37,21,22]);
    n = binfo(2,i);
    strides = [1,binfo(3,i),binfo(4,i)];
    if (n > 0) {
      tmp = array(pointer, 3, n);
      bndtmp(i) = &tmp;
      for (j=1 ; j<=n ; j++) {
        prefix = _h_bndname(bnum,j-1);
        pn = get_member(ff,prefix+"pn")(1);
        list = where(abs(pn)==strides);
        if (!numberof(list)) continue;
        pn = sign(pn)*list(1);
        tmp(1,j) = &[i, get_member(ff,prefix+"len_nsend1")(1),
                     pn, get_member(ff,prefix+"blk_send")(1),
                     get_member(ff,prefix+"bndnbc")(1)];
        tmp(2,j) = &long(get_member(ff,prefix+"ndx_send"));
        tmp(3,j) = &long(get_member(ff,prefix+"ndx_recv"));
      }
    }
    n = binfo(1,i);
    if (n > 0) {
      tmp = array(pointer, n);
      tmp2 = array(0, 2, n);
      bctmp(1,i) = &tmp;
      bctmp(2,i) = &tmp2;
      for (j=1 ; j<=n ; j++) {
        prefix = _h_bcname(bnum,j-1);
        pn = get_member(ff,prefix+"pn")(1);
        list = where(abs(pn)==strides);
        if (!numberof(list)) continue;
        pn = sign(pn)*list(1);
        tmp2(1,j) = pn;
        jj = get_member(ff,prefix+"len")(1);  /* unnecessary?? */
        tmp(j) = &long(get_member(ff,prefix+"ndx")(1:jj));
        tmp2(2,j) = get_member(ff,prefix+"rtype")(1);
      }
    }
    /* collect any requested auxilliary data in hydra_aux_data */
    if (naux) _h_data_internal, aux_names, hydra_aux_data;
    /* collect any requested mix data in qdata, ncmix, cmix, lmix */
    if (nmix) _h_mix_internal, mix_names;
  }

  if (nmix) {
    mixdat = _h_mix_work();
    hydra_mix_data = array(pointer, nmix+5);
    nx = data = pbdat = bdat = cmix = ncmix = lmix = [];
    eq_nocopy, nx, *mixdat(3);
    hydra_mix_data(nmix+1:nmix+4) = mixdat;
    hydra_mix_data(nmix+5) = &matlist;
    matlist = mixdat = [];
    off = 0;
    for (i=1 ; i<=nblk ; i++) {
      if (!nx(i)) continue;
      off0 = off+1;
      off += nx(i);
      eq_nocopy, pbdat, *qdata(i);
      for (j=1 ; j<=nmix ; j++) {
        eq_nocopy, bdat, *pbdat(j);
        eq_nocopy, data, *hydra_mix_data(j);
        if (is_void(data)) {
          data = array(structof(bdat), sum(nx));
          hydra_mix_data(j) = &data;
        }
        data(off0:off) = bdat(*);
      }
    }
    nx = data = pbdat = bdat = qdata = [];
  }

  bnd_off= binfo(2,cum);
  nbnd= max(bnd_off(0),1);
  bnd_blk= bnd_len= bnd_pn= bnd_r= bnd_ri= array(0, nbnd);
  bnd_ndxs= bnd_ndxr= array(pointer, nbnd);
  jj= 0;
  for (i=1 ; i<=nblk ; i++) {
    n= binfo(2,i);
    strides= [1,binfo(3,i),binfo(4,i)];
    eq_nocopy, tmp, *bndtmp(i);
    for (j=1 ; j<=n ; j++) {
      jj++;
      eq_nocopy, tmp2, *tmp(1,j);
      if (!numberof(tmp2)) continue;
      bnd_blk(jj)= tmp2(1);
      bnd_len(jj)= tmp2(2);
      bnd_pn(jj)= tmp2(3);
      bnd_r(jj)= tmp2(4);
      bnd_ri(jj)= tmp2(5);
      bnd_ndxs(jj)= tmp(2,j);
      bnd_ndxr(jj)= tmp(3,j);
    }
  }
  bndtmp = tmp = tmp2 = [];
  bnd_r++;
  bnd_ri++;

  /* allocate the block bound array and construct it from disk data */
  bound= array(0, dimsof(xyz));
  scratch= array(0, lscratch);
  /* number of bnd nodes is sum(bnd_len), which overestimates number
   * of faces; take that as a safe length to allocate */
  n= bnd_len(sum:1:jj);
  mbnds= n? array(HX_blkbnd, n) : [];
  for (i=1,ibnd=0 ; i<=jj ; i++) {
    if (!bnd_pn(i)) continue;
    n= bnd_len(i);
    s= bnd_blk(i);
    r= bnd_r(i);
    ri= bnd_off(r) + bnd_ri(i);
    bnds= [bnd_pn(i),binfo(3,s),binfo(4,s)];
    bndr= [bnd_pn(ri),binfo(3,r),binfo(4,r)];
    jbnd= hydra_bnd(ibnd, bound, scratch, blo(,s), blo(,r),
                    bnds, bndr, n, *bnd_ndxs(i), bnd_ndxr(ri), &mbnds, r-1);
    if (jbnd<0) {
      if (hydra_bnd_check) error, "illegal bnd data";
    } else {
      ibnd = jbnd;
    }
  }
  nbnds= ibnd;
  bnd_ndxs= bnd_ndxr= [];

  /* remains only to fill in the bcs in bound */
  start= -1;
  bndr= blor= [0];  /* unused */
  for (i=1,off=0 ; i<=nblk ; off+=mlens(i++)) {
    n= binfo(1,i);
    if (n <= 0) continue;
    strides= bnds= [1,binfo(3,i),binfo(4,i)];
    blos= blo(,i);
    tbound= array(0, 3,mdims(1,i),mdims(2,i),mdims(3,i));
    tcheck= array(0, 2,n);
    eq_nocopy, tmp, *bctmp(1,i);
    eq_nocopy, tmp2, *bctmp(2,i);
    for (j=1 ; j<=n ; j++) {
      local ndxs;
      eq_nocopy, ndxs, *tmp(j);
      if (!numberof(ndxs)) continue;
      pn = tmp2(1,j);
      bnds(1) = pn;
      rtype = tmp2(2,j);
      if (rtype==0) ibnd= -1;             /* open bc */
      else if (rtype==1) ibnd= -2;  /* reflecting bc */
      else if (rtype==3) ibnd= -3;   /* zero area bc */
      else error, "unknown rtype in bc data";
      if (!is_void(hydra_bnd_hook))
        ibnd= hydra_bnd_hook(ibnd, pn, i, j, ndxs);
      jj = numberof(ndxs);
      jj= hydra_mrk(ibnd, tbound, blos, bnds, jj, ndxs);
      if (jj < 0) error, "illegal bc data";
      tcheck(1,j)= pn;
      tcheck(2,j)= jj;
    }
    jj= hydra_adj(bound, tbound, blos, n, tcheck);
    tbound= [];
    if (jj>=0 && start<0) start= jj;
  }
  bctmp = tmp = tmp2 = [];

  blks= array(HX_block, nblk);
  blks.length= blo(2:4,);
  blks.stride(1,)= 1;
  blks.stride(2:3,)= blo(2:3,);
  blks.first= blo(1,);
  blks.final= blo(1,)+blo(4,);

  if (is_void(ublk)) {
    if (start<0) error, "mesh must have at least one open bc";
  } else {
    list= where(gmap(1,)==ublk);
    if (!numberof(list)) error, "mesh has no user block #"+pr1(ublk);
    gmap= gmap(,list);
    lst= where((gmap(2,)<=i0) & (gmap(3,)>i0) &
               (gmap(4,)<=j0) & (gmap(5,)>j0) &
               (gmap(6,)<=k0) & (gmap(7,)>k0));
    if (!numberof(lst))
      error, "mesh ublk #"+pr1(ublk)+" has no point "+pr1([i0,j0,k0]);
    if (numberof(lst)>1)
      error, "mesh ublk #"+pr1(ublk)+" has multiple points "+pr1([i0,j0,k0]);
    /* could read ireg and check that this cell exists, but don't bother */
    lst= lst(1);
    /* get 0-origin hex cell ijk within this block, convert to 1D index */
    start= [i0,j0,k0] - gmap(2:6:2,lst) + 1;
    block= blks(list(lst));
    start= block.first + sum(start*block.stride);
    if (!is_void(face) && face!=-1) {
      if (face<0 || face>5) error, "face must be between 0 and 5 inclusive";
      start= 6*start + face;
    } else {
      start= -1 - start;
    }
  }

  if (nbnds) mbnds= mbnds(1:nbnds);

  /* form the mesh */
  return _lst(xyz, bound, mbnds, blks, start);
}
if (is_void(h_xyz)) h_xyz = hydra_xyz;
if (!is_func(hydra_blks)) {
  autoload, "hex.i", hydra_blks, hydra_bnd, hydra_mrk, hydra_adj;
}

local hydra_aux_data;
local hydra_aux_names;
/* DOCUMENT hydra_aux_names = [name1, name2, ...];
            mesh = hydra_mesh(f, ...);
            eq_nocopy, var1, *hydra_aux_data(1);
            eq_nocopy, var2, *hydra_aux_data(2);
            ...
     Set hydra_aux_names to a list of names (see h_data) in order to
     have hydra_mesh retrieve those variables concurrently as it reads
     the mesh.  When the mesh is spread over many files, this avoids
     reopening and reclosing all the files, as happens if you call
     hydra_mesh and h_data separately.

   SEE ALSO: hydra_mesh, hydra_xyz, h_data, hydra_mix_data
 */

local hydra_mix_data;
local hydra_mix_names;
/* DOCUMENT hydra_mix_names = [name1, name2, ...];
            mesh = hydra_mesh(f, ...);
            eq_nocopy, var1, *hydra_mix_data(1);
            eq_nocopy, var2, *hydra_mix_data(2);
            ...
            eq_nocopy, mixn, *hydra_mix_data(nn+1);
            eq_nocopy, mixcell, *hydra_mix_data(nn+2);
            eq_nocopy, mixnmat, *hydra_mix_data(nn+3);
            eq_nocopy, mixhist, *hydra_mix_data(nn+4);
            eq_nocopy, matlist, *hydra_mix_data(nn+5);
     Set hydra_mix_names to a list of names (see h_mix) in order to
     have hydra_mesh retrieve those variables concurrently as it reads
     the mesh.  When the mesh is spread over many files, this avoids
     reopening and reclosing all the files, as happens if you call
     hydra_mesh and h_mix separately.
     In the example, nn=numberof(hydra_mix_names).  See h_mix for a
     description of mixn, mixcell, mixnmat, mixhist, and matlist.

   SEE ALSO: hydra_mesh, hydra_xyz, h_data, hydra_aux_data
 */

func h_data(f, name)
/* DOCUMENT name_array = h_data(f, name)
         or pname_arrays = h_data(f, [name1,name2,...,nameN])
              eq_nocopy, name_array1, *pname_arrays(1)
              ...
              eq_nocopy, name_arrayN, *pname_arrays(N)

     reads variable NAME from the hydra file F.  If F is a multiblock
     file, NAME_ARRAY will be 1-D; for single block problems it will
     be 3-D.  If NAME=="matlist", you get the "Materials_matlist"
     array.  Coordinates can be obtained using the names x, y or z.

     In the second form, NAME1, ..., NAMEN are retrieved simultaneously,
     which is useful when F is a large family of files.

     Note that zone centered arrays are adjusted to the hex convention
     that cells with i=1, j=1, k=1 are missing, rather than the hydra
     convention that i=imax, j=jmax, k=kmax are missing.

   SEE ALSO: hydra_xyz, h_mix, h_array, h_show, hydra_aux_data
 */
{
  { local mdims, mlens, bnum, _h_legacy, ndims; }
  nblk = h_blocks(f, mdims, mlens, _h_legacy, ndims);
  dim2 = dim3 = call(2:0);
  if (ndims < 3) {
    dim3 = 1;
    if (ndims < 2) dim2 = 1;
  }

  /* allocate the global data array and read it from disk */
  name = _h_translate(name);
  dims = dimsof(name);
  naux = numberof(name);
  pdata = array(pointer, dims);
  off = 0;
  for (i=1 ; i<=nblk ; i++) {
    off0 = off+1;
    off += mlens(i);
    ff = _h_get_file(f, i, bnum);
    data = _h_data_internal(name, pdata);
  }

  return dims(1)? pdata : data;
}

func _h_data_internal(name, pdata)
{
  data = [];
  for (j=1 ; j<=naux ; j++) {
    bdat = get_member(ff,swrite(format="/hblk%ld/%s",bnum,name(j)));
    if (numberof(bdat)!=mlens(i)) {
      if (dimsof(bdat)(1)==1) {
        tmp = array(structof(bdat), grow([ndims],mdims(,i)-1));
        tmp(*) = bdat;
        bdat = tmp;
      }
      tmp = array(structof(bdat), grow([ndims],mdims(,i)));
      tmp(2:0,dim2,dim3) = bdat;
      bdat=tmp;  tmp=[];
    }
    eq_nocopy, data, *pdata(j);
    if (is_void(data)) {
      if (nblk>1) data = array(structof(bdat), sum(mlens));
      else        data = array(structof(bdat), grow([ndims],mdims(,1)));
      pdata(j) = &data;
    }
    data(off0:off) = bdat(*);
  }
  return data;
}

func h_mix(f, &matlist, name, &mixdat)
/* DOCUMENT mixdat = h_mix(f, matlist)
              eq_nocopy, mixn, *mixdat(1)
              eq_nocopy, mixcell, *mixdat(2)
              eq_nocopy, mixnmat, *mixdat(3)
              eq_nocopy, mixhist, *mixdat(4)
         or mix_array = h_mix(f, mixdat, name)
         or pmix_array = h_mix(f, matlist, [name1,...,nameN], mixdat)
              eq_nocopy, mix_array1, *pmix_array(1)
              ...
              eq_nocopy, mix_arrayN, *pmix_array(N)

     In first form, returns MIXDAT and MATLIST for the hydra file F.
     MIXDAT consists of two arrays: MIXN is a list of the number of
     mixed cells for each block, and MIXCELL is an index array
     into any hex global cell array (as returned by h_data),
     MIXNMAT is the number of mix "zones" within each cell,
     and MIXHIST is the list required in order to use the
     histogram function on a mix array.

     In the second form, reads the mix data for the variable NAME
     in the hydra file F; the MIXDAT argument must have been returned
     by a previous call to h_mix using the first form.

     In the third form, MATLIST and MIXDAT are both returned along
     with the set of variables NAME1, ..., NAMEN, so that a number of
     variables can be retrieved in one call (useful when F is a large
     family of files).

     For example, to compute the temperature in each cell, using
     a mass weighted average in mixed zones, you would do this:
       den = h_data(f,"den");
       tmat = h_data(f,"tmat");
       mixdat = h_mix(f, matlist);
       local mixcell, mixhist;
       eq_nocopy, mixcell, *mixdat(2);
       eq_nocopy, mixhist, *mixdat(4);
       denx = h_mix(f, mixdat, "den");
       tmatx = h_mix(f, mixdat, "tmat");
       vf = h_mix(f, mixdat, "vf");
       tavg = tmat;
       tavg(mixcell) = histogram(mixhist, tmatx*denx*vf)/den(mixcell);

   SEE ALSO: hydra_xyz, h_data, h_array, h_show
 */
{
  { local mdims, mlens, bnum, _h_legacy, ndims; }
  nblk = h_blocks(f, mdims, mlens, _h_legacy, ndims);
  dim2 = dim3 = call(2:0);
  if (ndims < 3) {
    dim3 = 1;
    if (ndims < 2) dim2 = 1;
  }
  dims = dimsof(name);

  if (is_void(name) || dims(1)) {
    /* may as well collect matlist as long as we have to read it */
    if (nblk>1) matlist = array(0, sum(mlens));
    else        matlist = array(0, grow([ndims],mdims(,1)));

    if (!is_void(name)) {
      name = _h_xtranslate(name);
      nmix = numberof(name);
      pdata = array(pointer, dims);
      qdata = array(pointer, nblk);
    }

    /* scan blocks for mix data */
    ncmix = array(0, nblk);
    cmix = lmix = array(pointer, nblk);
    off = 0;
    for (i=1 ; i<=nblk ; i++) {
      ff = _h_get_file(f, i, bnum);
      off0 = off+1;
      off += mlens(i);
      _h_mix_internal, name;
    }

    mixdat = _h_mix_work();
    if (is_void(qdata)) return mixdat;
    eq_nocopy, nx, *mixdat(3);
    if (is_void(nx)) return pdata;

  } else {
    nx = *matlist(3);
  }

  if (is_void(nx)) return [];
  if (is_void(qdata)) {
    name = _h_xtranslate(name);
    nmix = 1;
  }
  data = pbdat = [];
  off = 0;
  for (i=1 ; i<=nblk ; i++) {
    if (!nx(i)) continue;
    off0 = off+1;
    off += nx(i);
    if (!is_void(qdata)) eq_nocopy, pbdat, *qdata(i);
    for (j=1 ; j<=nmix ; j++) {
      if (is_void(qdata)) {
        ff = _h_get_file(f, i, bnum);
        bdat = get_member(ff,swrite(format="/hblk%ld/%s",bnum,name));
      } else {
        eq_nocopy, bdat, *pbdat(j);
        eq_nocopy, data, *pdata(j);
      }
      if (is_void(data)) {
        data = array(structof(bdat), sum(nx));
        if (!is_void(qdata)) pdata(j) = &data;
      }
      data(off0:off) = bdat(*);
    }
  }
  return is_void(qdata)? data : pdata;
}

func _h_mix_internal(name)
{
  prefix = swrite(format="/hblk%ld/",bnum);
  xdata = get_member(ff, prefix+"Materials_matlist");
  dims = grow([ndims],mdims(,i));
  tmp = array(0, dims-[0,1,1,1]);
  tmp(*) = xdata(*);
  xdata = array(0, dims);
  xdata(2:0,dim2,dim3) = tmp;  tmp = [];
  matlist(off0:off) = xdata(*);

  cells = where(xdata<0);    /* cells with mixed materials */
  if (numberof(cells)) {
    xtart = -xdata(cells);   /* initial indices into mix_next (1-origin) */
    mix_next = get_member(ff, prefix+"Materials_mix_next");
    /* mix_next is a packed set of sequences of indices into vf,
     *   with each sequence terminated by a zero
     * - apparently, only the zero markers are used by the hydra
     * - mix_next is the same shape as the vf or *_mix variables
     *
     * xtart is guaranteed to be a monotonically increasing
     *   (hydra creates it by incrementing through all zones),
     *   so each contiguous block of vf or the *_mix variables
     *   represents the next mixed cell in zone order */
    xtop = where(mix_next<=0);
    nx = xtop - xtart + 1;  /* number of materials per mixed zone */
    /* convert cells to global hex cell indices */
    cells += off0-1;
  } else {
    cells = nx = [];
  }
  cmix(i) = &cells;
  lmix(i) = &nx;
  ncmix(i) = numberof(cells);
  if (nmix && numberof(cells)) {
    xdata = array(pointer, nmix);
    qdata(i) = &xdata;
    for (j=1 ; j<=nmix ; j++)
      xdata(j) = &get_member(ff,swrite(format="/hblk%ld/%s",bnum,name(j)));
  }
}

func _h_mix_work(void)
{
  nx = sum(ncmix);
  if (nx) {
    cells = nx = array(0, nx);
    offs = ncmix(cum);
    for (i=1 ; i<=nblk ; i++) if (ncmix(i)) {
      cells(offs(i)+1:offs(i+1)) = *cmix(i);
      nx(offs(i)+1:offs(i+1)) = *lmix(i);
    }
    imix = nx(cum);
    marks = array(0, imix(0));
    marks(imix(1:-1)+1) = 1;
    marks = marks(psum);       /* nx(1) copies of 1, nx(2) copies of 2
                                * nx(3) copies of 3, etc. */
    nx = imix(offs+1)(dif);
  } else {
    cells = nx = marks = [];
  }
  return [&ncmix, &cells, &nx, &marks];
}

func h_array(f, ublk, name)
/* DOCUMENT name_array = h_array(f, ublk, name)
         or pname_arrays = h_array(f, ublk, [name1,name2,...,nameN])
              eq_nocopy, name_array1, *pname_arrays(1)
              ...
              eq_nocopy, name_arrayN, *pname_arrays(N)

     reads variable array NAME for user block UBLK from the hydra file F.  
     If NAME=="matlist", you get the "Materials_matlist" array.
     Coordinates can be obtained using the names x, y or z.

     Ublk numbering starts at 0.  You can omit the UBLK argument and it
     will default to zero, which is useful for problems with only a single
     user block.

     Note that here zone centered arrays are given using the hydra convention
     so that i=imax, j=jmax, k=kmax are missing.  Thus in order to use the 
     Yorick plc and plf functions correctly you should index the plotted
     variable i.e. for a 2D array.
     plf, den(1:-1,1:-1), y, x

   SEE ALSO: hydra_xyz, h_data, h_mix, h_show, h_collect
 */
{
  { local mdims, mlens, bnum, _h_legacy, ndims, gmap, umap; }
  gnblk = h_blocks(f, mdims, mlens, _h_legacy, ndims, gmap, umap);

  unblk = dimsof(umap)(0);
  if (is_void(name) && structof(ublk)==string) {
    name = ublk;
    ublk = 0;
  }
  ublk++;

  if (ublk > unblk) {
    write, format="Error bad ublk number specified - only %ld user blocks,"+
      " numbering starts from 0\n", unblk;
    return ;
  }

  /* allocate the global data array and read it from disk */
  name = _h_translate(name);
  dims = dimsof(name);
  nn = numberof(name);
  pdata = array(pointer, dims);
  data = [];
  off= 0;
  ulens = umap(3:7:2,ublk) - umap(2:6:2,ublk) + 1;
  udims = grow([ndims],ulens)(1:1+ndims);
  for (i=1 ; i<=gnblk ; i++) {
    if ((gmap(1,i)+1) != ublk) continue;
    ff = _h_get_file(f, i, bnum);
    for (j=1 ; j<=nn ; j++) {
      bdat = get_member(ff,swrite(format="/hblk%ld/%s",bnum,name(j)));

      eq_nocopy, data, *pdata(j);
      if (is_void(data)) {
        data = array(structof(bdat), udims);
        pdata(j) = &data;
      }

      dm = dimsof(bdat);
      ix = dm(2);
      ioff = gmap(2,i)-1;
      if (ndims < 2) {
        data(ioff+1:ioff+ix) = bdat(*);
      } else {
        jx = dm(3);
        joff = gmap(4,i)-1;
        if (ndims < 3) {
          data(ioff+1:ioff+ix,joff+1:joff+jx) = bdat;
        } else {
          kx = dm(4);
          koff = gmap(6,i)-1;
          data(ioff+1:ioff+ix,joff+1:joff+jx,koff+1:koff+kx) = bdat;
        }
      }
    }
  }

  return dims(1)? pdata : data;
}

func h_collect(f, ublk, name)
/* DOCUMENT vart = h_collect(f, ublk, name)

     returns an array of the variable NAME (a string) from user block
     UBLK of hydra file family F.  The return value has the leading
     dimensions of h_array(f,ublk,name), with a trailing dimension
     representing all the times in the family.

   SEE ALSO: h_array, h_show
 */
{
  nrecs = h_jr(f);
  if (nrecs < 1) return h_array(f, ublk, name);
  ihist = _car(f, 2)(2);
  h_jr, f, 1;
  result = array(h_array(f, ublk, name), nrecs);
  for (i=1 ; i<=nrecs ; i++) {
    h_jr, f, i;
    result(..,i) = h_array(f, ublk, name);
  }
  h_jr, f, ihist;
  return result;
}

func h_iparm(f, name)
/* DOCUMENT value = h_iparm(f, name)
         or names = h_iparm(f)

     returns value of hydra parameter NAME from file F,
     or a list of all names in F if NAME is not supplied.

     If NAME is not a string, returns that parameter
     or parameters (NAME is index in the returned list of names),
     for example h_iparm(f,1:0) returns all parameters.

   SEE ALSO: hydra_xyz, h_fparm, h_parm, h_uparm
 */
{
  return h_parm(f, name, iorf="i");
}

func h_fparm(f, name)
/* DOCUMENT value = h_fparm(f, name)
         or names = h_fparm(f)

     returns value of hydra parameter NAME from file F,
     or a list of all names in F if NAME is not supplied.

     If NAME is not a string, returns that parameter
     or parameters (NAME is index in the returned list of names),
     for example h_fparm(f,1:0) returns all parameters.

   SEE ALSO: hydra_xyz, h_iparm, h_parm, h_uparm
 */
{
  return h_parm(f, name, iorf="f");
}

func h_parm(f, name, iorf=)
/* DOCUMENT value = h_parm(f, name)
         or names = h_parm(f)

     returns value of hydra parameter NAME from file F,
     or a list of all names in NAME is not supplied.

   SEE ALSO: hydra_xyz, h_fparm, h_iparm, h_uparm
 */
{
  { local _h_legacy, lparm, mparm; }
  f = _h_get_file(f, 1, , _h_legacy);

  p = is_void(iorf)? "f" : iorf;
  parmn0 = get_member(f,_h_globname(p+"parmn")); /* dims may be wrong :(*/
  parmn = _h_parm_fix(parmn0, lparm);
  n1 = numberof(parmn) / lparm;
  if (is_void(iorf)) {
    parmn0 = get_member(f,_h_globname("iparmn")); /* dims may be wrong :(*/
    parmm = _h_parm_fix(parmn0, mparm);
    n2 = numberof(parmm)/mparm;
    parmn0 = array(char, max(lparm,mparm), n1+n2);
    parmn0(1:lparm,1:n1) = parmn;
    parmn0(1:mparm,n1+1:n1+n2) = parmm;
    parmn = parmn0;
    lparm = max(lparm,mparm);
  }
  if (!is_void(name)) {
    if (structof(name)==string) {
      name = *pointer(name);
      n = numberof(name);
      if (n>lparm) name = name(1:(n=lparm));
      i = where(!(parmn(1:n,)!=name)(sum,));
      if (!numberof(i)) error, "no such parameter name as "+string(&name);
      i = i(1);
    } else {
      i = name;
    }
    if (is_range(i) || i<=n1) return get_member(f,_h_globname(p+"parmv"))(i);
    else return get_member(f,_h_globname("iparmv"))(i-n1);

  } else {
    n = numberof(parmn0)/lparm;
    parmn0 = array(string, n);
    for (i=1 ; i<=n ; i++) parmn0(i) = string(&parmn(,i));
    return parmn0;
  }
}

func _h_parm_fix(parms, &lparm)
{
  /* silo bug reverses order of dimensions of parameter names
   * /Global/wordlength if present gives true length of leading dim
   * as an easier way to maintain backward compatibility, and
   * also to allow for the silo bug being fixed someday,
   * hydra guarantees that (1) the first parameter name is <=8 chars,
   * and (2) the second parameter name does not begin with <=' '
   */
  dims = dimsof(parms);
  lparm = numberof(parms);
  if (lparm<9 || dims(min:2:0)==1) return [parms(*)];
  lparm = 7 + min(where(parms(9:lparm)>' '));
  if (dims(2) != lparm) {
    dims = array(structof(parms), lparm, numberof(parms)/lparm);
    dims(*) = parms(*);
    parms = dims;
  }
  return parms;
}

func h_ushow(f)
/* DOCUMENT h_ushow, f
         or varnames = h_ushow(f)

     prints names of user defined variables (def cards) in hydra file F.
     As a function, returns array of def names.  The index of each name
     in the returned list is 1 plus its index in the file F.  (Missing
     indices in F will have string(0) in varnames -- shouldn't happen.)

   SEE ALSO: h_uparm, h_show, h_openb
 */
{
  gpdat = "gpdat";
  prefix = "/Global/"+gpdat;
  vars = *get_vars(_h_get_file(f, 1))(1);
  vars = vars(where(strpart(vars,-3:0) == "/sym"));
  if (!numberof(vars)) return vars;
  vars = vars(where(strpart(vars,1:strlen(prefix)) == prefix));
  n = numberof(vars)
  if (!n) return vars;
  vars = strpart(vars, strlen(prefix)+1:-4);
  ndxs = array(-1, n);
  names = array(string, n);
  ndx = 0;
  for (i=1 ; i<=n ; ++i) {
    if (sread(vars(i),ndx)!=1 || ndx<0) continue;
    ndxs(i) = ndx;
    names(i) = string(&h_global(f, gpdat+vars(i)+"/sym"));
  }
  list = where(ndxs >= 0);
  ndxs = ndxs(list);
  names = names(list);
  if (!numberof(list)) return names;
  if (am_subroutine()) {
    write, names(sort(names));
  } else {
    list = sort(ndxs);
    ndxs= ndxs(list);
    names = names(list);
    n = ndxs(0) + 1;
    list = array(0, n);
    list(ndxs+1) = 1;
    vars = array(string, n);
    vars(where(list)) = names;
    return vars;
  }
}

func h_uparm(f, name, isstr=, isint=)
/* DOCUMENT values = h_uparm(f, names)

     returns values of user defined variables (def cards) in hydra file F.
     NAMES is a name or array of names; the returned values have the
     same dimensions as NAMES.  All values are type double.

     Note that integer valued variables can be returned as integers
     with long(h_uparm(f, names)).

     User defined variables are stored as strings.  You can retrieve
     the strings instead of the numeric values with the isstr=1
     keyword.  Without isstr=1, any string which cannot be converted
     to a number will appear as -1.e99.

   SEE ALSO: h_ushow, h_uparm, h_fparm, h_parm
 */
{
  names = h_ushow(f);
  if (!isstr) {
    dims = dimsof(name);
    scalar = !dims(1);
    value = scalar? [0.] : array(0., dims);
  } else {
    value = name;
  }
  n = numberof(value);
  d = 0.0;
  for (i=1 ; i<=n ; ++i) {
    list = where(name(i) == names);
    if (!numberof(list)) error, "unknown user variable name "+name;
    v = h_global(f, swrite(format="gpdat%ld/val", list(1)-1));
    if (!isstr) {
      if (sread(string(&v), d) != 1) d = -1.e99;
      value(i) = d;
    } else {
      value(i) = string(&v);
    }
  }
  if (!isstr) {
    if (scalar) value = value(1);
    if (isint) value = long(value);  /* better for caller to convert */
  }
  return value;
}

func h_global(f, name, prefix=)
/* DOCUMENT value = h_global(f, name)
         or names = h_global(f)
     returns value of hydra Global variable NAME from file F.
     Without NAME returns a list of all valid names.
   SEE ALSO: hydra_xyz, h_iparm
 */
{
  { local _h_legacy; }
  f = _h_get_file(f, 1, , _h_legacy);
  if (!prefix) prefix = "/Global/";
  if (is_void(name)) {
    v = *get_vars(f)(1);
    n = strlen(prefix);
    v = v(where(strpart(v,1:n) == prefix));
    if (numberof(v)) v = strpart(v,n+1:);
    return v;
  }
  return get_member(f, prefix+name);
}

func h_gblk(f)
/* DOCUMENT gblk = h_gblk(f)

     return global block information from the hydra file F (see h_openb).

     Each hblk in the mesh corresponds to a particular imin:imax,
     jmin:jmax, kmin:kmax in a particular gblk.  The return value is
     a 2D long array 7-by-numberof(h blocks):

     gblk(1,) =   user block number for this hblk
     gblk(2:3,) = gblk [imin,imax] of this hblk
     gblk(4:5,) = gblk [jmin,jmax] of this hblk
     gblk(6:7,) = gblk [kmin,kmax] of this hblk

   SEE ALSO: hydra_xyz, h_data, h_openb
 */
{
  { local gmap; }
  _h_unpack_file, f, , , , , , gmap;
  return gmap(1:7,);  /* gmap(8,)=hblk number in distributed file */
}

func h_ublk(f, unew)
/* DOCUMENT ublk = h_ublk(f)
         or ublk = h_ublk(f, unew)

     return user block information from the hydra file F (see h_openb).

     Each ublk in the mesh has a particular size.  The return value is
     a 2D long array 7-by-numberof(u blocks):

     ublk(1,) =   user block number for this ublk
     ublk(2:3,) = ublk [imin,imax] of this ublk
     ublk(4:5,) = ublk [jmin,jmax] of this ublk
     ublk(6:7,) = ublk [kmin,kmax] of this ublk

     Normally, imin=jmin=kmin=1, and the only information in the return
     value is imax, jmax, kmax.

     In the second form, sets the ublk to UNEW, which is useful for
     resetting imin, jmin, and kmin for each block so that it describes
     a packing of the user blocks into an overall global block
     structure.

   SEE ALSO: hydra_xyz, h_data, h_openb
 */
{
  { local umap; }
  _h_unpack_file, f, , , , , , , umap;
  if (!is_void(unew)) {
    flags = _car(f, 2);
    ndims = flags(3);
    dimn = dimsof(unew);
    if (dimn(1)!=2) error, "supplied umap must be a 2D array";
    dimo = dimsof(umap);
    if (dimn(0)!=dimo(0))
      error, "wrong number of user blocks in supplied umap";
    if (dimn(2)==3 && ndims==1) {
      tmp = 0*umap - 1;
      tmp(1:3,) = unew;
      unew = tmp;
    } else if (dimn(2)==5 && ndims==2) {
      tmp = 0*umap - 1;
      tmp(1:5,) = unew;
      unew = tmp;
    } else if (dimn(2)!=7) {
      error, "supplied umap has wrong number of elements in first dimension";
    }
    if (anyof((unew(3:7:2,)-unew(2:6:2,))!=(umap(3:7:2,)-umap(2:6:2,))))
      error, "supplied umap has wrong block sizes";
    ihist = flags(2);
    data = _car(f, 3);
    *data(3,ihist) = unew;
  }
  return umap;
}

/* aliases by popular request */
hmesh = hydra_mesh;
hdata = h_data;
hjt = h_jt;
hjr = h_jr;
ha = h_array;
hp = h_parm;

/* ------------------------------------------------------------------------ */

/* should this be demoted to an _h internal routine? */

func h_blocks(f, &mdims, &mlens, &legacy, &ndims, &gmap, &umap)
/* DOCUMENT gnblk = h_blocks(f, mdims, mlens)

     returns number of blocks GNBLK, block dimensions MDIMS, and
     block lengths MLENS for the hydra mesh in file F.
     MDIMS is 3-by-NBLK, MLENS is GNBLK elements.

   SEE ALSO: hydra_xyz, h_iparm
 */
{
  _h_unpack_file, f, , , ndims, legacy, , gmap, umap;

  mdims = gmap(3:7:2,) - gmap(2:6:2,) + 1;
  if (ndims < 3) {
    mdims(3,) = 1;
    if (ndims < 2) mdims(2,) = 1;
  }
  mlens = mdims(1,) * mdims(2,) * mdims(3,);

  return numberof(mlens);
}

/* ------------------------------------------------------------------------ */

_h_autodetect = 1;  /* set to 0 to skip legacy detection */

func _h_openb(name)
{
  yPDBopen = 1;  /* open102=1 */
  f = open(name, "rb");
  if (_not_pdb(f, 0)) {
    close, f;
    error, name+" missing or not a PDB file";
  }
  return f;
}

func _h_get_file(f, i, &bnum, &legacy)
{
  { local idistrib, ihist, names, gmap;}
  ff = _h_unpack_file(f, idistrib, ihist, , legacy, names, gmap);
  name = names(i);
  if (!ff || !idistrib || names(idistrib)!=name) {
    ff = _h_openb(name);
    _car, f, 1, ff;
  }
  bnum = gmap(8, i);
  flags = _car(f, 2);
  flags(1) = i;
  _car, f, 2, flags;
  return ff;
}

func _h_unpack_file(f, &idistrib, &ihist, &ndims, &legacy,
                    &names, &gmap, &umap, &time, &ncyc)
{
  ff = _car(f, 1);
  flags = _car(f, 2);
  idistrib = flags(1);
  ihist = flags(2);
  ndims = flags(3);
  legacy = flags(4);
  data = _car(f, 3);
  eq_nocopy, names, *data(1,ihist);
  eq_nocopy, gmap, *data(2,ihist);
  eq_nocopy, umap, *data(3,ihist);
  time = _car(f,4);
  ncyc = _car(f,5);
  return ff;
}

func _h_translate(name)
{
  nm = array(string, dimsof(name));
  for (i=1 ; i<=numberof(nm) ; i++) {
    nami = name(i);
    if (nami=="matlist") nm(i) = "Materials_matlist";
    else if (nami=="x")  nm(i) = "hydro_mesh_coord0";
    else if (nami=="y")  nm(i) = "hydro_mesh_coord1";
    else if (nami=="z")  nm(i) = "hydro_mesh_coord2";
    else                 nm(i) = nami+"_data";
  }
  return nm;
}

func _h_xtranslate(name)
{
  nm = array(string, dimsof(name));
  for (i=1 ; i<=numberof(nm) ; i++) {
    nami = name(i);
    if (nami=="vf")       nm(i) = "Materials_mix_vf";
    else if (nami=="mat") nm(i) = "Materials_mix_mat";
    else                  nm(i) = nami+"_mix";
  }
  return nm;
}

func _h_varname(n,suffix)
{
  if (_h_legacy) return swrite(format="/BDATA/hblk%ld/"+suffix,n);
  return swrite(format="/hblk%ld/bdata/"+suffix,n);
}
func _h_bndname(n,b)
{
  if (_h_legacy) return swrite(format="/BDATA/hblk%ld/bnd%ld/",n,b);
  return swrite(format="/hblk%ld/bdata/bnd%ld/",n,b);
}
func _h_bcname(n,b)
{
  if (_h_legacy) return swrite(format="/BDATA/hblk%ld/bc%ld/",n,b);
  return swrite(format="/hblk%ld/bdata/bc%ld/",n,b);
}
func _h_globname(suffix)
{
  if (_h_legacy) return "/BDATA/"+suffix;
  return "/Global/"+suffix;
}
func _h_gmapname(n)
{
  if (_h_legacy) return swrite(format="/BDATA/gmap%ld",n);
  return swrite(format="/Global/Decomposition/gmap%ld/gmap",n);
}
func _h_umapname(n)
{
  if (_h_legacy) return swrite(format="/BDATA/umap%ld",n);
  return swrite(format="/Global/Decomposition/umap%ld/umap",n);
}

/*
 * ublk
 *   gblk (may be several gblks per ublk each gblk is in one ublk)
 *
 * distributed file
 *   gblk (may be several gblks per file)
 *
 * history family
 *   either distributed file or not
 * names[gnblk]     file names (may be repeats)
 * blocks[gnblk]    block number in file --> append to gmap
 * gmap[7,gnblk]    (user block and how it fits)
 * umap[7,unblk]    (user block sizes)
 */

func _h_read_maps(f, name, root)
{
  list = [1,4,7,5,8,6,9];
  if (root) {
    unblk = get_member(f,"/Decomposition/NumBlocks")(1);
    umap = array(0, 7, unblk);
    if (!_h_mapfmt) {
      for (i=1 ; i<=unblk ; i++) {
        nm = swrite(format="/Decomposition/umap%ld/umap",i-1);
        umap(,i) = get_member(f, nm)(list);
      }
    } else {
      umap = _h_rdmap(f, "", 0, unblk);
    }
    gnblk = get_member(f,"/Decomposition/NumDomains")(1);
    names = array(string, gnblk);
    toks = [string(0), string(&get_member(f,"/hydro_mesh_meshnames"))];
    gmap = array(0, 8, gnblk);
    for (i=1 ; i<=gnblk ; i++) {
      toks = strtok(toks(2),";");
      names(i) = strtok(toks(1),":")(1);
    }
    if (!_h_mapfmt) {
      for (i=1 ; i<=gnblk ; i++) {
        nm = swrite(format="/Decomposition/gmap%ld/gmap",i-1);
        gmap(1:7,i) = get_member(f, nm)(list);
      }
    } else {
      gmap = _h_rdmap(f, "", 1, gnblk);
    }
    gmap(8,) = get_member(f,"/DomainFiles")(1:gnblk);
    names = name+names;

  } else {
    iparmnv= get_member(f, _h_globname("iparmn")); /* dims may be wrong :(*/
    iparmn= _h_parm_fix(iparmnv);
    iparmv = get_member(f, _h_globname("iparmv"));
    pname = *pointer("unblk");
    i = where(!(iparmn(1:6,)!=pname)(sum,))(1);
    unblk = iparmv(i);
    if (!_h_mapfmt) {
      umap = array(0, 7, unblk);
      for (i=1 ; i<=unblk ; i++)
        umap(,i) = get_member(f, _h_umapname(i-1))(list);
    } else {
      umap = _h_rdmap(f, "/Global", 0, unblk);
    }
    pname(1) = 'g';  /* "gnblk" */
    i = where(!(iparmn(1:6,)!=pname)(sum,))(1);
    gnblk = iparmv(i);
    if (!_h_mapfmt) {
      gmap = array(0, 8, gnblk);
      for (i=1 ; i<=gnblk ; i++) {
        nblk = get_member(f,_h_varname(i-1,"hydrodati"))(36);
        gmap(1:7,i) = get_member(f, _h_gmapname(nblk))(list);
      }
    } else {
      gmap = _h_rdmap(f, "/Global", 1, gnblk);
    }
    gmap(8,) = indgen(0:gnblk-1);
    names = array(name, gnblk);
  }

  /* adjust umap, gmap for ghost nodes, 0 vs 1-origin */
  umap(2:7,) -= 1;
  gmap(2:7,) -= 1;

  return [&names, &gmap, &umap];
}

func _h_rdmap(f, prefix, gmap, nblk)
{
  list = [1,2,3,4,5,6];
  nm0 = prefix + "/Decomposition/";
  nm = nm0 + (gmap? "Domain" : "Block");
  map = indgen(0:nblk-1)(-:1:7+gmap,);
  if (gmap) fmt = (_h_mapfmt-3)&2;
  else fmt = (_h_mapfmt-3)&1;
  if (!fmt) {
    map(2:7,) = get_member(f, nm+"_Extents")(list);
  } else {
    nm += "_Decomposition_";
    nn = get_member(f, nm+"nneighbors");
    nodes = get_member(f, nm+"nodelists");
    map(2:7,) = nodes(list + 15*nn(cum)(-,1:-1));
  }
  if (gmap) map(1,) = get_member(f, nm0+"Domains_BlockNums");
  return map;
}

struct HX_block {   /* must match hex.h */
  long stride(3);
  long length(3);
  long first;
  long final;
}

struct HX_blkbnd {  /* must match hex.h */
  long block;
  long cell;
  int orient;
}

/* ------------------------------------------------------------------------ */
