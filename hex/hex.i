/*
 * $Id: hex.i,v 1.1 2005-09-18 22:05:48 dhmunro Exp $
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*= SECTION() 3D transport equation solver =================================*/

if (!is_void(plug_in)) plug_in, "hex";

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

extern hex_mesh;
/* DOCUMENT mesh= hex_mesh(xyz, bound, nbnds, &mbnds, nblk, &blks, start)

     create a 3D mesh object from the multiblock mesh parameters
     XYZ   is NBLK 3 x Ni x Nj x Nk coordinate arrays packed together
     BOUND is NBLK 3 x Ni x Nj x Nk face boundary markers packed
     NBNDS is length of MBNDS
     MBNDS is HX_blkbnd describing each internal block boundary face
     NBLK  is number of blocks
     BLKS  is NBLK HX_block objects describing the block structure
     START is 0-origin 6*cell+face index of first boundary face/cell
            or -1-cell to trace from centroid of that cell to point
            p on ray to begin tracking

   SEE ALSO: hex5_track, hydra_mesh, hex_startflag
 */

extern hex_query;
/* DOCUMENT start= hex_query(mesh, xyz, bound, mbnds, blks)

     query a mesh created by hex_mesh, returning the arrays
     passed to that function (these are not copies -- be careful
     not to clobber them)
     function return value is the start index

   SEE ALSO: hex5_track, hydra_mesh
 */

extern hex5_track;
extern hex24f_track;
extern hex24b_track;
/* DOCUMENT c= hex5_track(mesh, rays, s)
            c= hex24f_track(mesh, rays, s)
            c= hex24b_track(mesh, rays, s)

     track 3 x Nrays x 2 RAYS through the 3D MESH.  RAYS(,,1) are
     points on the rays, while RAYS(,,2) are normalized ray directions.

     The c return value and the S parameter are a long and double
     array respectively, with number of elements equal to the total
     number of intersections of all the RAYS with faces of the MESH,
     plus one for any RAY which misses MESH entirely.  The values of
     c are:
       [#hits,cell1,cell2,cell3,..., #hits,cell1,cell2,cell3,..., ...]
     where each #hits is followed by the list of cell indices (assuming
     i=1, j=1, and k=1 are present but meaningless in cell arrays --
     that is, assuming zone centered arrays have the same dimensions
     as XYZ rather than one less in each direction).  Rays which miss
     the mesh entirely have #hits=1, all others have #hits>=2 since they
     must exit.  #hits<0 means a ray reentered the mesh for abs(#hits)
     more face crossings, but this currently cannot happen.  The values
     of S correspond to c:
       [s0,s1,s2,s3,..., s0,s1,s2,s3,..., ...]
     which are the distances along the ray measured from RAYS(,,1) in
     the direction of RAYS(,,2) where the ray pierces a cell face.  For
     rays which miss the mesh, the value of s0 is a diagnostic telling
     why they missed (see compiled code).

     Function hex5_track uses the 5-tet decomposition for hexes,
     which is not unique when the quad faces are non-planar.  You may
     be able to get an idea of this effect by setting hex_triang the
     opposite way and redoing the trace.

     Functions hex24f_track and hex24b_track use the face and body
     centered 24-tet decompositions for hexes.  These are unique;
     however, hex_triang may in rare cases change the trace slightly,
     since the entry search algorithm still involves triangulating
     the surface quads.

   SEE ALSO: hydra_mesh, hex_triang, reg_track, track_reduce, c_adjust,
     pic3_rays, conv3_rays
 */

extern reg_track;
/* DOCUMENT c= reg_track(x, y, z, rays, s)

     track RAYS through regular mesh defined by the 1D coordinate
     arrays X, Y, and Z.  Return values S and C are as for
     hex5_track, where the mesh is numberof(X) by numberof(Y) by
     numberof(Z).

   SEE ALSO: hex5_track, track_reduce
 */

func pic3_rays(xpict, ypict, ray, q_up)
/* DOCUMENT rays= pic3_rays(xpict, ypict, ray)
         or rays= pic3_rays(xpict, ypict, ray, q_up)

     Like picture_rays, but returns rays in the [p,q] representation
     appropriate for hex5_track.

     (XPICT,YPICT) are 2D arrays of pixel corners in the image plane;
     RAY is the central ray (0,0) in (XPICT,YPICT) coordinates, given
     in [p,q] representation (i.e. RAY is a 3-by-2 array).  The
     optional Q_UP is a 3-vector specifying the orientation of the
     y-axis in the picture plane (see theta_up, phi_up in picture_rays
     for a description of default orientation).  Q_UP must not be
     parallel to RAY(,2).

   SEE ALSO: hex5_track, conv3_rays, picture_rays
 */
{
  if (!is_void(q_up)) {
    q_up= conv3_rays([[0.,0.,0.],q_up]);
    theta_up= q_up(4);
    phi_up= q_up(5);
  }
  ray= conv3_rays(ray);
  return conv3_rays(picture_rays(xpict,ypict,ray,theta_up,phi_up));
}

func conv3_rays(rays)
/* DOCUMENT conv3_rays(rays)

     convert [p,q] representation to or from best_rays representation.
     If the first dimension of RAYS is 3, returns 5-by-raydims array
     of best_rays; if first dimension of RAYS is 5, returns 3-by-raydims-
     by-2 [p,q] for use with hex5_track.

   SEE ALSO: hex5_track, pic3_rays, best_rays
 */
{
  dims= dimsof(rays);
  if (dims(2)==5) {
    theta= rays(4,..);
    phi= rays(5,..)
    rays= q= rays(1:3,..);
    q(3,..)= cos(theta);
    theta= sin(theta);
    q(2,..)= theta*sin(phi);
    q(1,..)= theta*cos(phi);
    return [rays,q];
  } else {
    qz= rays(..,2);
    qx= qz(1,..);
    qy= qz(2,..);
    qz= qz(3,..);
    qxy= abs(qx,qy);
    r= array(0., 5,dimsof(qz));
    r(1:3,..)= rays(..,1);
    r(4,..)= atan(qxy, qz);
    r(5,..)= atan(qy, qx+!qxy);
    return r;
  }
}

func track_reduce(&c, &s, rays, slimits, flip=)
/* DOCUMENT nlist= track_reduce(c, s)
         or nlist= track_reduce(c, s, rays, slimits)

     compresses the C and S returns from the tracking routines (see
     hex5_track) to the following form:
       [cell1,cell2,cell3,..., cell1,cell2,cell3,..., ...]
       [s1-s0,s2-s1,s3-s2,..., s1-s0,s2-s1,s3-s2,..., ...]
     returning nlist as
       [#hits, #hits, ...]

     In this form, any negative #hits are combined with the preceding
     positive values, and #hits=1 (indicating a miss) appear as #hits=0
     in nlist.  Hence, nlist always has exactly Nrays elements.

     If RAYS is supplied, it is used to force the dimensions of the
     returned nlist to match the dimensions of RAYS (the value of RAYS
     is never used).  The RAYS argument need not have the trailing 2
     dimension, so if you specified RAYS as [P,Q] if the call to
     hex5_track, you can use just P or Q as the RAYS argument to
     track_reduce.

     If SLIMITS is supplied, it should be [smin,smax] or [smin,smax]-
     by-dimsof(nlist) in order to reject input S values outside the
     specified limits.  The C list will be culled appropriately, and
     the first and last returned ds values adjusted.

     With a non-zero flip= keyword, the order of the elements of
     C and S within each group of #hits is reversed, so that a
     subsequent track_solve will track the ray backwards.  If you
     use this, both the ray direction input to the tracking routine
     and any SLIMITS argument here should refer to the reverse of
     the ray you intend to track.

   SEE ALSO: hex5_track, c_adjust, track_solve, track_integ,
             bi_dir, track_combine
 */
{
  n= _ray_reduce(numberof(c), c, s, &[], &[]);
  if (is_void(rays)) {
    nlist= array(0, n);
  } else {
    dims= dimsof(rays);
    if      (numberof(rays)==6*n) dims(1)-= 2;
    else if (numberof(rays)==3*n) dims(1)-= 1;
    else error, "rays argument inconsistent with c argument";
    if (dims(1)) dims(2:-1)= dims(3:0);
    nlist= array(0, dims);
  }
  if (!is_void(slimits)) {
    slimits= double(slimits);
    if (numberof(slimits)==2) slimits= array(slimits, dimsof(nlist));
    else if (numberof(slimits)!=2*numberof(nlist))
      error, "slimits dimensions inconsistent with rays dimensions";
  }
  n= numberof(c);
  if (flip) n= -n;
  _ray_reduce, n, c, s, &nlist, &slimits;
  n= sum(nlist);
  if (n) {
    c= c(1:n);
    s= s(1:n);
  } else {
    c= s= [];
  }
  return nlist;
}

func bi_dir(tracker, mesh, rays, slimits, &c, &s)
/* DOCUMENT nlist = bi_dir(tracker, mesh, rays, slimits, c, s)

     Perform hexX_track and track_reduce on a ray that enters
     the problem at the given point on the ray.  This requires
     tracking the ray in both directions from the given point,
     hence this function name indicating bi-directional tracking.
     This is unnecessary when the entry point search was over
     the problem boundary, or when the SLIMITS for the rays
     always lie in one direction relative to the starting point.

     TRACKER is the function used to track the rays, normally
       one of hex5_track, hex_24f_track, or hex24b_track.
     MESH is the problem mesh returned by hex_mesh or hydra_mesh;
       it should be generated using the entry option that finds
       the cell containing the given point on the ray.
     RAYS is the 3-by-nrays-by-2 array of rays, as for hex5_track
     SLIMITS is nil or the ray tracking limits as for track_reduce
     C, S, together with NLIST are the output arrays, as for
       track_reduce

   SEE ALSO: track_reduce, hex5_track, hex24f_track, hex24b_track,
             track_combine
 */
{
  /* copy rays and ensure they are normalized */
  rays += 0.;
  rays(..,2) /= abs(rays(1,..,2),rays(2,..,2),rays(3,..,2))(-,..);

  /* track in positive direction, then in negative direction flipped */
  local sp, sm;
  cp = tracker(mesh, rays, sp);
  if (is_void(slimits)) slimits = [-1.e35, 1.e35];
  np = track_reduce(cp, sp, rays, max(slimits, 0.));
  rays(..,2) = -rays(..,2);
  slimits = -slimits(2:1:-1,..);
  cm = tracker(mesh, rays, sm);
  nm = track_reduce(cm, sm, rays, max(slimits, 0.), flip=1);

  return track_combine(nm,cm,sm, np,cp,sp, c, s);
}

func track_combine(nm, cm, sm, np, cp, sp, &c, &s)
/* DOCUMENT nlist = track_combine(nm,cm,sm, np,cp,sp, c, s)

     combine two track_reduce results NM,CM,SM, and NP,CP,SP,
     which represent the first and second halves of a set of
     rays.  See bi_dir for a typical application.  The returned
     NLIST is NM+NP, or NM+NP-1 for those rays where the
     final CM is identical to the initial CP.

     C, S, together with NLIST are the output arrays, as for
     track_reduce.

   SEE ALSO: track_reduce, bi_dir
 */
{
  if (!numberof(cm)) {
    c = cp;  s = sp;
    return np;
  }
  if (numberof(cp)) {
    nm = nm(*);  np = np(*);
    last = nm(psum);
    firstx = np(cum) + 1;
    first = firstx(1:-1);
    mask = (cp(min(first,numberof(cp)))==cm(max(last,1))) & (np>0) & (nm>0);
    list = where(mask);
    if (numberof(list)) {
      sm = sm;  sp = sp;  cp = cp;
      /* combine first p-cell with last m-cell when they are same */
      first = first(list);
      sm(last(list)) += sp(first);
      keep = array(1n, numberof(sp));
      keep(first) = 0n;
      keep = where(keep);
      sp = sp(keep);
      cp = cp(keep);
      np(list) -= 1;
      firstx = np(cum) + 1;
      first = firstx(1:-1);
    }
  }
  if (!numberof(cp)) {
    c = cm;  s = sm;
    return nm;
  }
  mrkm = histogram(1+last)(1:-1);
  mrkm = mrkm(psum)+1;  /* nm(1) 1's, nm(2) 2's, nm(3) 3's, etc */
  mrkm = indgen(numberof(cm)) + (first-1)(mrkm);
  mrkp = histogram(firstx(2:0))(1:-1);
  mrkp = mrkp(psum)+1;  /* np(1) 1's, np(2) 2's, np(3) 3's, etc */
  mrkp = indgen(numberof(cp)) + last(mrkp);
  c = array(long, numberof(cm)+numberof(cp));
  s = array(double, numberof(cm)+numberof(cp));
  c(mrkm) = cm;
  s(mrkm) = sm;
  c(mrkp) = cp;
  s(mrkp) = sp;
  return nm+np;
}

func c_adjust(&c, mesh, how)
/* DOCUMENT c_adjust, c, mesh
         or c_adjust, c, mesh, 1
         or c= c_adjust(c, mesh, how)

     adjust the cell number array C returned by track_reduce to
     allow for a different layout of cell arrays than the one assumed
     by the tracking routines.  Two HOW values are currently
     supported: 0 (or nil) if the cell arrays are the same shape as
     the nodal arrays, but the non-existent cell is at the end of
     each row rather than at the beginning.  And 1 if the cell arrays
     are smaller by one along each dimension than the nodal arrays.

     If you call c_adjust as a subroutine, the input C array
     is modified; if you call it as a function, the input C is
     unchanged and the new values returned.

   SEE ALSO: track_reduce, hex5_track, cs_adjust
 */
{
  { local xyz, bound, bnds, blks; }
  hex_query, mesh, xyz, bound, bnds, blks;
  dims= dimsof(xyz);
  if (dims(1)!=4) error, "no support for multiblock meshes here yet";
  ix= dims(3);
  jx= dims(4);
  ijx= ix*jx;

  cc= c - 1 - ix - ijx;
  if (how) {
    cc-= 1;
    ic= cc%ix;
    jc= (cc/ix)%jx;
    kc= cc/ijx;
    ix-= 1;
    cc= 1 + ic + ix*jc + (ijx-jx-ix)*kc;
  }

  if (am_subroutine()) c= cc;
  else                 return cc;
}

func cs_adjust(nlist, &c, &s, ireg)
/* DOCUMENT nlist= cs_adjust(nlist, c, s, ireg)

     adjust NLIST, C, S returned from track_reduce to remove transits
     of cells for which IREG == 0.  Can be called before or after
     c_adjust, depending on layout of IREG.

   SEE ALSO: c_adjust
 */
{
  ireg= where(ireg(c)!=0);
  if (numberof(ireg) < numberof(c)) {
    scalar= !dimsof(nlist)(1);
    if (scalar) nlist= [nlist];
    else nlist+= 0;
    n= histogram(nlist(*)(cum)+1)(psum:1:-1);
    n= n(ireg);
    c= c(ireg);
    s= s(ireg);
    nlist(*)= histogram(n,top=numberof(nlist));
    if (scalar) nlist= nlist(1);
  }
  return nlist;
}

func track_integ(nlist, transp, selfem, last)
/* DOCUMENT result= track_integ(nlist, transp, selfem, last)

     integrates a transport equation by doing the sums:

        transparency(i) = transparency(i-1) * TRANSP(i)
        emissivity(i) = emissivity(i-1) * TRANSP(i) + SELFEM(i)

     returning only the final values transparency(n) and emissivity(n).
     The NLIST is a list of n values, so that many transport integrals
     can be performed simultaneously; sum(NLIST) = numberof(TRANSP) =
     numberof(SELFEM).  The result is 2-by-dimsof(NLIST).

     If TRANSP is nil, result is dimsof(NLIST) sums of SELFEM.
     If SELFEM is nil, result is dimsof(NLIST) products of TRANSP.

     TRANSP and SELFEM may by 2D to do multigroup integrations
     simultaneously.  By default, the group dimension is first, but
     if LAST is non-nil and non-zero, the group dimension is second.
     In either case, the result will be ngroup-by-2-by-dimsof(NLIST).

     track_solve is the higher-level interface.

   SEE ALSO: track_reduce, track_solve, track_solve
 */
{
  ntot= sum(nlist);
  if (is_void(transp))      dims= dimsof(selfem);
  else if (is_void(selfem)) dims= dimsof(transp);
  else                      dims= dimsof(transp, selfem);
  if (!is_void(dims)) {
    if (!is_void(transp) &&
        (dims(1)!=dimsof(transp)(1) || anyof(dimsof(transp)!=dims) ||
         structof(transp)!=double))
      transp= double(transp)+array(0., dims);
    if (!is_void(selfem) &&
        (dims(1)!=dimsof(selfem)(1) || anyof(dimsof(selfem)!=dims) ||
         structof(selfem)!=double))
      selfem= double(selfem)+array(0., dims);
  }
  if (last) last= 2;
  else      last= 1;
  if (is_void(dims) || dims(1)<1 || dims(1)>2 ||
      (dims(1)==1? dims(2) : dims(4-last))!=ntot)
    error, "nlist, trans, and selfem are inconsistent";
  if (dims(1)==1) ng= 1;
  else            ng= dims(1+last);
  if (ng>1 && last==2) ng= -ng;
  if (is_void(transp) || is_void(selfem)) {
    if (dims(1)==1) result= array(0., dimsof(nlist));
    else            result= array(0., abs(ng), dimsof(nlist));
  } else {
    if (dims(1)==1) result= array(0., 2, dimsof(nlist));
    else            result= array(0., abs(ng), 2, dimsof(nlist));
  }
  _ray_integ, numberof(nlist), nlist, ng, &transp, &selfem, result;
  return result;
}

func track_solve(nlist, c, s, akap, ekap, last)
/* DOCUMENT result= track_solve(nlist, c, s, akap, ekap, last)

     integrates a transport equation for NLIST, C, and S returned
     by track_reduce (and optionally c_adjust).  The RAYS argument
     is used only to set the dimensions of the result.  AKAP and
     EKAP are mesh-sized arrays of opacity and emissivity, respectively.
     They may have an additional group dimension, as well.  The
     units of AKAP are 1/length (where length is the unit of S),
     while EKAP is (spectral) power per unit area (length^2), where
     the power is what ever units you want the result in.  The
     emission per unit volume of material is EKAP*AKAP; an optically
     thick block of material emits EKAP per unit surface.

     The NLIST is a list of n values, so that many transport integrals
     can be performed simultaneously; sum(NLIST) = numberof(AKAP) =
     numberof(EKAP).  The result is 2-by-dimsof(NLIST), where the
     first element of the first index is the transmission fraction
     through the entire ray path, and the second element of the
     result is the self-emission along the ray, which has the same
     units as EKAP.

     If EKAP is nil, result is dimsof(NLIST) -- exactly the same as
     the transparency (1st element of result) when both EKAP and AKAP
     are specified.

     If AKAP is nil, result is dimsof(NLIST).  In this case, EKAP
     must have units of emission per unit volume instead of per unit
     area; the result will be the sum of EKAP*S along each ray.

     AKAP and EKAP may by 2D to do multigroup integrations
     simultaneously.  By default, the group dimension is first, but
     if LAST is non-nil and non-zero, the group dimension is last.
     In either case, the result will be ngroup-by-2-by-dimsof(NLIST).

     To use in conjuction with hex5_track, one might do this:

        c= hex5_track(mesh, rays, s);
        nlist= track_reduce(c, s, rays);
        c_adjust, c, mesh;  // if necessary
        result= track_solve(nlist, c, s, akap, ekap);

   SEE ALSO: track_reduce, hex5_track
 */
{
  if (!is_void(akap)) {
    dims= dimsof(akap);
    if (!is_void(ekap) && anyof(dims!=dimsof(ekap)))
        error, "dimensions of akap and ekap do not match";
  } else {
    dims= dimsof(ekap);
  }
  ndims= dims(1);
  if (ndims<1 || ndims>4)
    error, "akap and ekap must be 1, 2, 3 or 4 dimensional arrays";
  if (ndims>2) {
    if (ndims==3 || last) {
      if (!is_void(akap)) akap= akap(c,1,1,);
      if (!is_void(ekap)) ekap= ekap(c,1,1,);
    } else {
     if (!is_void(akap)) akap= akap(,c,1,1);
     if (!is_void(ekap)) ekap= ekap(,c,1,1);
     s= s(-,..);
   }
  } else {
    if (ndims==1 || last) {
      if (!is_void(akap)) akap= akap(c,);
      if (!is_void(ekap)) ekap= ekap(c,);
    } else {
      if (!is_void(akap)) akap= akap(,c);
      if (!is_void(ekap)) ekap= ekap(,c);
      s= s(-,..);
    }
  }
  if (!is_void(akap)) {
    if (!is_void(ekap)) ekap= -ekap*expm1(-akap*s, akap);
    else                akap= exp(-akap*s);
  } else {
    ekap= ekap*s;
  }
  return track_integ(nlist, akap, ekap, last);
}

func make_sphere(radius, ijk_max, phi12, theta12)
/* DOCUMENT make_sphere(radius, [imax,jmax,kmax],
                        [phi1, phi2], [theta1, theta2])

     return a mesh (see hex_mesh) representing the given section
     of the sphere of given RADIUS.  IMAX, JMAX, and KMAX are the
     number of nodes (cells+1) in the radial, longitude (phi), and
     colatitude (theta) directions, respectively.  Note that for
     a right handed coordinate system, phi1<phi2 but theta1>theta2.

   SEE ALSO: hex_mesh
 */
{
  /* note that theta should be given in *decreasing* order
   * to get a right-handed coordinate mesh */
  if (is_void(phi12))   phi12= [0., 2*pi];
  if (is_void(theta12)) theta12= [pi, 0.];
  periodic= (abs(phi12(2)-phi12(1)) > 2*pi-1.e-5);
  r= span(0., radius, ijk_max(1));
  phi= span(phi12(1), phi12(2), ijk_max(2))(-,);
  theta= span(theta12(1), theta12(2), ijk_max(3))(-,-,);
  xyz= array(0., grow([4,3],ijk_max));
  xyz(3,,,)= r*cos(theta);
  r*= sin(theta);
  xyz(2,,,)= r*sin(phi);
  xyz(1,,,)= r*cos(phi);
  bndy= [2,1, 2+periodic,2+periodic, 2,2];
  return hex_mesh2(xyz, bndy);
}

extern _ray_reduce;
/* PROTOTYPE
   long ray_reduce(long len, long array c, double array s, pointer nlist,
                   pointer slims)
 */

extern _ray_integ;
/* PROTOTYPE
   void ray_integ(long nr, long array nlist, long ng,
                  pointer transp, pointer selfem, double array result)
 */

extern hex_triang;
/* PROTOTYPE
   int hex_triang(int flag)
 */
/* DOCUMENT old_flag= hex_triang(new_flag)

     possibly set flag to NEW_FLAG, always return OLD_FLAG, where
     flag value is 0 for default mesh triangulation, 1 for opposite
     triangulation, and 2 on input to signal not to change the
     current value.  The triangulation value can affect the result
     of hex5_track if the quad faces of the mesh are not planar.

   SEE ALSO: hex5_track
 */

extern hex_startflag;
/* PROTOTYPE
   int hex_startflag(int flag)
 */
/* DOCUMENT old_flag= hex_startflag(new_flag)

     possibly set flag to NEW_FLAG, always return OLD_FLAG, where
     flag value is 0 (default) to begin search for new entry point
     at previous entry point, 1 to begin search for new entry point
     from mesh start face for every ray.  Any other value of NEW_FLAG
     returns OLD_FLAG without changing it.

   SEE ALSO: hex_mesh
 */

extern hydra_blks;
/* PROTOTYPE
   long hydra_blks(long nblks, long array blo)
 */

extern hydra_bnd;
/* PROTOTYPE
   long hydra_bnd(long ibnd, long array bound, long array scratch,
                  long array blos, long array blor, long array bnds,
                  long array bndr, long n, long array ndxs,
                  pointer ndxr, pointer mbnds, long rblock)
 */

extern hydra_mrk;
/* PROTOTYPE
   long hydra_mrk(int ibnd, long array tbound, long array blo,
                  long array bnd, long n, long array ndx)
 */

extern hydra_adj;
/* PROTOTYPE
   long hydra_adj(long array bound, long array tbound, long array blo,
                  long n, long array tcheck)
 */

/* ------------------------------------------------------------------------ */

func hydra_mesh(f, ublk, i0, j0, k0, face)
/* DOCUMENT mesh= hydra_mesh(f)
         or mesh= hydra_mesh(f, ublk, i0, j0, k0, face)
         or mesh= hydra_mesh(f, ublk, i0, j0, k0)

     read a 3D mesh object from the hydra PDB/Silo file F.

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

   SEE ALSO: hex_query, hex5_track, h_data, h_openb
 */
{
  mesh= hydra_xyz(f, ublk, i0, j0, k0, face);

  { local xyz, bound, mbnds, blks; }
  eq_nocopy, xyz, _car(mesh);
  eq_nocopy, bound, _car(mesh,2);
  eq_nocopy, mbnds, _car(mesh,3);
  eq_nocopy, blks, _car(mesh,4);
  start= _car(mesh,5);
  nbnds= numberof(mbnds);
  nblk= numberof(blks);

  /* form the HX_mesh */
  return hex_mesh(xyz, bound, nbnds, &mbnds, nblk, &blks, start);
}

func hex_mesh2(xyz, bounds)
/* DOCUMENT mesh= hex_mesh2(xyz, bounds)
     old interface for hex_mesh

     create a 3D mesh object from the 3 x Ni x Nj x Nk coordinate
     array XYZ and the list of 6 BOUNDS:
       BOUNDS(1), BOUNDS(2)  for the i=1,Ni boundaries
       BOUNDS(3), BOUNDS(4)  for the j=1,Nj boundaries
       BOUNDS(5), BOUNDS(6)  for the k=1,Nk boundaries
     The BOUNDS values are:
       1   if this is a problem boundary
       2   if this is a reflecting boundary
       3   if this is a periodic boundary

   SEE ALSO: hydra_mesh
 */
{
  xyz= double(xyz);
  dims= dimsof(xyz);
  if (dims(1)!=4 || dims(2)!=3 || anyof(dims(3:5)<2))
    error, "bad XYZ parameter";

  if (numberof(bounds)!=6 || min(bounds<0) ||
      max(bounds>3)) error, "bad BOUNDS parameter";
  bound= array(0, dims);
  nbnds= 0;
  ni= dims(3);  nj= dims(4);  nk= dims(5);

  if (is_void(bounds)) bounds= array(1, 6);
  bounds+= 0;
  mask= bounds==3;
  list= where(mask);
  if (numberof(list)) {
    if (anyof(mask(1:5:2)!=mask(2:6:2))) error, "bad BOUNDS periodic values";
    bounds(list)= 0;
    njk= mask(1)? (nj-1)*(nk-1) : 0;
    nki= mask(3)? (nk-1)*(ni-1) : 0;
    nij= mask(5)? (ni-1)*(nj-1) : 0;
    nbnds= 2*(njk+nki+nij);
    bnds= array(HX_blkbnd, nbnds);
    bnds.block= 0;
    bnds.orient= 0;
    nbnds= 0;
    ni1= ni-1;
    nj1= (nj-1)*ni;
    nk1= (nk-1)*nj*ni;
    if (njk) {
      cells= (indgen(ni:nj1:ni)+indgen(ni*nj:nk1:ni*nj)(-,))(*);
      n0= nbnds+1;  nbnds+= njk;
      bound(1,1,2:0,2:0)= reform(indgen(n0:nbnds),nj-1,nk-1);
      bnds(n0:nbnds).cell= cells + ni1;
      n0= nbnds+1;  nbnds+= njk;
      bound(1,0,2:0,2:0)= reform(indgen(n0:nbnds),nj-1,nk-1);
      bnds(n0:nbnds).cell= cells;
    }
    if (nki) {
      cells= (indgen(1:ni1)+indgen(ni*nj:nk1:ni*nj)(-,))(*);
      n0= nbnds+1;  nbnds+= nki;
      bound(2,2:0,1,2:0)= reform(indgen(n0:nbnds),ni-1,nk-1);
      bnds(n0:nbnds).cell= cells + nj1;
      n0= nbnds+1;  nbnds+= nki;
      bound(2,2:0,0,2:0)= reform(indgen(n0:nbnds),ni-1,nk-1);
      bnds(n0:nbnds).cell= cells;
    }
    if (nij) {
      cells= (indgen(1:ni1)+indgen(ni:nj1:ni)(-,))(*);
      n0= nbnds+1;  nbnds+= nij;
      bound(3,2:0,2:0,1)= reform(indgen(n0:nbnds),ni-1,nj-1);
      bnds(n0:nbnds).cell= cells + nk1;
      n0= nbnds+1;  nbnds+= nij;
      bound(3,2:0,2:0,0)= reform(indgen(n0:nbnds),ni-1,nj-1);
      bnds(n0:nbnds).cell= cells;
    }
  }

  for (i=1 ; i<=6 ; i++) {
    if (!bounds(i)) continue;
    j= 1+(i-1)/2;
    if (j==1) bound(1,i&1,2:0,2:0)= -bounds(i);
    else if (j==2) bound(2,2:0,i&1,2:0)= -bounds(i);
    else bound(3,2:0,2:0,i&1)= -bounds(i);
  }

  list= where(bounds==1);
  if (!numberof(list)) error, "mesh must have at least one open bc";
  list= list(1);
  start= ([0,1,2,3,4,5] +
          6*([0,ni-2,0,ni*(nj-2),0,ni*nj*(nk-2)]+1+ni+ni*nj))(list);

  blks= array(HX_block);
  blks.length= [ni, ni*nj, ni*nj*nk];
  blks.stride= [1, ni, ni*nj];
  blks.first= 0;
  blks.final= ni*nj*nk;

  return hex_mesh(xyz, bound, nbnds, &bnds, 1, &blks, start);
}

func hydra_start(&mesh, start)
/* DOCUMENT hydra_start, mesh, start

     change the starting cell of the hydra MESH (returned by hydra_mesh)
     to START.  If called as a function, returns old start value.

   SEE ALSO: hydra_mesh, h_data
 */
{
  { local xyz, bound, bnds, blks; }
  old= hex_query(mesh, xyz, bound, bnds, blks);
  if (!is_void(start)) {
    nbnds= numberof(bnds);
    nblk= numberof(blks);
    mesh= hex_mesh(xyz, bound, nbnds, &bnds, nblk, &blks, start);
  }
  return old;
}

/* much of the hydra functionality is available for any yorick code */
if (!is_func(h_openb)) autoload, "hydra.i", h_openb, hydra_xyz;
