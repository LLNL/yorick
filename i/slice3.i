/*
 * $Id: slice3.i,v 1.1 2005-09-18 22:06:09 dhmunro Exp $
 * find 2D slices of a 3D hexahedral mesh
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*
 * Caveats:
 * (A) Performance is reasonably good, but may still be a factor of
 *     several slower than what could be achieved in compiled code.
 * (B) Only a simple in-memory mesh model is implemented here.
 *     However, hooks are supplied for more interesting possibilities,
 *     such as a large binary file resident mesh data base.
 * (C) There is a conceptual difficulty with _walk3 for the case
 *     of a quad face all four of whose edges are cut by the slicing
 *     plane.  This can only happen when two opposite corners are
 *     above and the other two below the slicing plane.  There are
 *     three possible ways to connect the four intersection points in
 *     two pairs: (1) // (2) \\ and (3) X  There is a severe problem
 *     with (1) and (2) in that a consistent decision must be made
 *     when connecting the points on the two cells which share the
 *     face - that is, each face must carry information on which way
 *     it is triangulated.  For a regular 3D mesh, it is relatively
 *     easy to come up with a consistent scheme for triangulating faces,
 *     but for a general unstructured mesh, each face itself must carry
 *     this information.  This presents a huge challenge for data flow,
 *     which I don't believe is worthwhile.  Because the X choice is
 *     unique, and I don't see why we shouldn't use it here.
 *     For contouring routines, we reject the X choice on esthetic
 *     grounds, and perhaps that will prove to be the case here as
 *     well - but I believe we should try the simple way out first.
 *     In this case, we are going to be filling these polygons with
 *     a color representing a function value in the cell.  Since the
 *     adjacent cells should have nearly the same values, the X-traced
 *     polygons will have nearly the same color, and I doubt there will
 *     be an esthetic problem.  Anyway, the slice3 implemented
 *     below produces the unique X (bowtied) polygons, rather than
 *     attempting to choose between // or \\ (non-bowtied) alternatives.
 *     Besides, in the case of contours, the trivial alternating
 *     triangulation scheme is just as bad esthetically as every
 *     zone triangulated the same way!
 */

/* ------------------------------------------------------------------------ */

func plane3(normal, point)
/* DOCUMENT plane3(normal, point)
         or plane3([nx,ny,nz], [px,py,pz])

     returns [nx,ny,nz,pp] for the specified plane.

   SEE ALSO: slice3, mesh3
 */
{
  /* the normal doesn't really need to be normalized, but this
   * has the desirable side effect of blowing up if normal==0 */
  normal/= abs(normal(1),normal(2),normal(3));
  return grow(normal,normal(+)*point(+));
}

/* ------------------------------------------------------------------------ */

func mesh3(x,y,z,..)
/* DOCUMENT mesh3(x,y,z)
         or mesh3(x,y,z, f1,f2,...)
         or mesh3(xyz, f1,f2,...)
         or mesh3(nxnynz, dxdydz, x0y0z0, f1,f2,...)

     make mesh3 argument for slice3, xyz3, getv3, etc., functions.
     X, Y, and Z are each 3D coordinate arrays.  The optional F1, F2,
     etc. are 3D arrays of function values (e.g. density, temperature)
     which have one less value along each dimension than the coordinate
     arrays.  The "index" of each zone in the returned mesh3 is
     the index in these cell-centered Fi arrays, so every index from
     one through the total number of cells indicates one real cell.
     The Fi arrays can also have the same dimensions as X, Y, or Z
     in order to represent point-centered quantities.

     If X has four dimensions and the length of the first is 3, then
     it is interpreted as XYZ (which is the quantity actually stored
     in the returned cell list).

     If X is a vector of 3 integers, it is interpreted as [nx,ny,nz]
     of a uniform 3D mesh, and the second and third arguments are
     [dx,dy,dz] and [x0,y0,z0] respectively.  (DXDYDZ represent the
     size of the entire mesh, not the size of one cell, and NXNYNZ are
     the number of cells, not the number of points.)

   SEE ALSO: slice3, xyz3, getv3, getc3
 */
{
  /* other sorts of meshes are possible -- a mesh which lives
   * in a binary file is an obvious example -- which would need
   * different workers for xyz3, getv3, getc3, and iterator3
   * iterator3_rect may be more general than the other three;
   * as long as the cell dimensions are the car of the list
   * which is the 2nd car of m3, it will work */
  virtuals= _lst(xyz3_rect, getv3_rect, getc3_rect, iterator3_rect);

  dims= dimsof(x);
  if (dims(1)==4 && dims(2)==3 && min(dims)>=2) {
    xyz= &double(x);
    n= 2*(!is_void(y))+(!is_void(z));
    dims= grow([3], dims(3:5));
  } else if (dims(1)==1 && dims(2)==3 && structof(0+x)==long && min(x)>0 &&
             dimsof(y)(1)==1 && dimsof(z)(1)==1 &&
             numberof(y)==3 && numberof(z)==3) {
    xyz= &double([y/double(x), z]);
    dims= grow([3], 1+x);
    n= 0;
    _car, virtuals, 1, xyz3_unif;
  } else {
    if (dims(1)!=3 || min(dims)<2 ||
        dimsof(y)(1)!=3 || anyof(dimsof(y)!=dims) ||
        dimsof(z)(1)!=3 || anyof(dimsof(z)!=dims))
      error, "X,Y,Z are not viable 3D coordinate mesh arrays";
    xyz= array(0.0, 3, dimsof(x));
    xyz(1,..)= x;
    xyz(2,..)= y;
    xyz(3,..)= z;
    xyz= &xyz;
    n= 0;
  }

  dim_cell= dims;
  dim_cell(2:4)-= 1;
  m3= _lst(virtuals, _lst(dim_cell(2:4), *xyz));
  last= _cdr(m3);

  while (n || more_args()) {
    if (n) {
      if (n&2) {
        f= y;
        n&= 1;
      } else {
        f= z;
        n= 0;
      }
    } else {
      f= next_arg();
    }
    if (dimsof(f)(1)!=3 ||
        (anyof(dimsof(f)!=dims) && anyof(dimsof(f)!=dim_cell))) {
      error, "F"+pr1(i)+" is not a viable 3D cell value";
    }
    _cdr, last, 1, _lst(f);
    last= _cdr(last);
  }

  return m3;
}

/* ------------------------------------------------------------------------ */

/* Ways that a list of polygons can be extracted:
 * Basic idea:
 *   (1) At each *vertex* of the cell list, a function value is defined.
 *       This is the "slicing function", perhaps the equation of a plane,
 *       perhaps some other vertex-centered function.
 *   (2) The slice3 routine returns a list of cells for which the
 *       function value changes sign -- that is, cells for which some
 *       vertices have positive function values, and some negative.
 *       The function values and vertex coordinates are also returned.
 *   (3) The slice3 routine computes the points along the *edges*
 *       of each cell where the function value is zero (assuming linear
 *       variation along each edge).  These points will be vertices of
 *       the polygons.  The routine also sorts the vertices into cyclic
 *       order.
 *   (4) A "color function" can be used to assign a "color" or other
 *       value to each polygon.  If this function depends only on the
 *       coordinates of the polygon vertices (e.g.- 3D lighting), then
 *       the calculation can be done elsewhere.  There are two other
 *       possibilities:  The color function might be a cell-centered
 *       quantity, or a vertex-centered quantity (like the slicing
 *       function) on the mesh.  In these cases, slice3 already
 *       has done much of the work, since it "knows" cell indices,
 *       edge interpolation coefficients, and the like.
 *
 * There are two particularly important cases:
 * (1) Slicing function is a plane, coloring function is either a
 *     vertex or cell centered mesh function.  Coloring function
 *     might also be a *function* of one or more of the predefined
 *     mesh functions.  If you're eventually going to sweep the whole
 *     mesh, you want to precalculate it, otherwise on-the-fly might
 *     be better.
 * (2) Slicing function is a vertex centered mesh function,
 *     coloring function is 3D shading (deferred).
 *
 * fslice(m3, vertex_list)
 * vertex_list_iterator(m3, vertex_list, mesh3)
 * fcolor(m3, vertex_list, fslice_1, fslice_2)
 *   the coloring function may need the value of fslice at the vertices
 *   in order to compute the color values by interpolation
 * two "edge functions": one to detect edges where sign of fslice changes,
 *   second to interpolate for fcolor
 *
 * slice3(m3, fslice, &nverts, &xyzverts, <fcolor>)
 */

func slice3(m3, fslice, &nverts, &xyzverts, fcolor, nointerp, value=)
/* DOCUMENT slice3, m3, fslice, nverts, xyzverts
         or color_values= slice3(m3, fslice, nverts, xyzverts, fcolor)
         or color_values= slice3(m3, fslice, nverts, xyzverts, fcolor, 1)

     slice the 3D mesh M3 using the slicing function FSLICE, returning
     the lists NVERTS and XYZVERTS.  NVERTS is the number of vertices
     in each polygon of the slice, and XYZVERTS is the 3-by-sum(NVERTS)
     list of polygon vertices.  If the FCOLOR argument is present, the
     values of that coloring function on the polygons are returned as
     the value of the slice3 function (numberof(color_values) ==
     numberof(NVERTS) == number of polygons).

     If the slice function FSLICE is a function, it should be of the
     form:
        func fslice(m3, chunk)
     returning a list of function values on the specified chunk of the
     mesh m3.  The format of chunk depends on the type of m3 mesh, so
     you should use only the other mesh functions xyz3 and getv3 which
     take m3 and chunk as arguments.  The return value of fslice should
     have the same dimensions as the return value of getv3; the return
     value of xyz3 has an additional first dimension of length 3.

     If FSLICE is a list of 4 numbers, it is taken as a slicing plane
     with the equation FSLICE(+:1:3)*xyz(+)-FSLICE(4), as returned by
     plane3.

     If FSLICE is a single integer, the slice will be an isosurface for
     the FSLICEth variable associated with the mesh M3.  In this case,
     the keyword value= must also be present, representing the value
     of that variable on the isosurface.

     If FCOLOR is nil, slice3 returns nil.  If you want to color the
     polygons in a manner that depends only on their vertex coordinates
     (e.g.- by a 3D shading calculation), use this mode.

     If FCOLOR is a function, it should be of the form:
        func fcolor(m3, cells, l, u, fsl, fsu, ihist)
     returning a list of function values on the specified cells of the
     mesh m3.  The cells argument will be the list of cell indices in
     m3 at which values are to be returned.  l, u, fsl, fsu, and ihist
     are interpolation coefficients which can be used to interpolate
     from vertex centered values to the required cell centered values,
     ignoring the cells argument.  See getc3 source code.
     The return values should always have dimsof(cells).

     If FCOLOR is a single integer, the slice will be an isosurface for
     the FCOLORth variable associated with the mesh M3.

     If the optional argument after FCOLOR is non-nil and non-zero,
     then the FCOLOR function is called with only two arguments:
        func fcolor(m3, cells)

   SEE ALSO: mesh3, plane3, xyz3, getv3, getc3, slice2, plfp
 */
{
  nverts= xyzverts= [];

  /* handle the different possibilities for fslice */
  if (!is_func(fslice)) {
    if (is_void(value) &&
        dimsof(fslice)(1)==1 && numberof(fslice)==4) {
      normal= fslice(1:3);
      projection= fslice(4);
      fslice= _plane_slicer;
    } else if (dimsof(fslice)(1)==0 && structof(0+fslice)==long) {
      if (is_void(value))
        error, "value= keyword required when FSLICE is mesh variable";
      iso_index= fslice;
      fslice= _isosurface_slicer;
    } else {
      error, "illegal form of FSLICE argument, try help,slice3";
    }
  }

  /* will need cell list if fcolor function to be computed */
  need_clist= !is_void(fcolor);

  /* test the different possibilities for fcolor */
  if (need_clist && !is_func(fcolor)) {
    if (dimsof(fcolor)(1)!=0 || structof(0+fcolor)!=long) {
      error, "illegal form of FCOLOR argument, try help,slice3";
    }
  }

  /* chunk up the m3 mesh and evaluate the slicing function to
   * find those cells cut by fslice==0
   * chunking avoids potentially disasterously large temporaries
   */
  _xyz3_save= 1;      /* flag for xyz3 */
  got_xyz= 0;
  ntotal= nchunk= 0;
  results= [];
  for (chunk=iterator3(m3) ;
       !is_void(chunk) ;
       chunk=iterator3(m3,chunk)) {

    /* get the values of the slicing function at the vertices of
     * this chunk
     */
    _xyz3= [];        /* if fslice calls xyz3, xyz saved here */
    fs= fslice(m3, chunk);

    /* will need cell list if fslice did not compute xyz */
    got_xyz= !is_void(_xyz3);
    need_clist|= !got_xyz;

    /* If the m3 mesh is totally unstructured, the chunk should be
     * arranged so that fslice returns an 2-by-2-by-2-by-ncells array
     * of vertex values of the slicing function.
     * On the other hand, if the mesh vertices are arranged in a
     * rectangular grid (or a few patches of rectangular grids), the
     * chunk should be the far less redundant rectangular patch.
     */
    dims= dimsof(fs);
    irregular= numberof(dims)>4;
    if (irregular) {
      /* fs is a 2-by-2-by-2-by-ncells array
       * here is the fastest way to generate the required cell list
       */
      clist= where((fs<0.0)(sum:1:8,1,1,) & 7);

    } else {
      /* fs is an ni-by-nj-by-nk array
       * result of the zcen is 0, 1/8, 2/8, ..., 7/8, or 1
       */
      clist= double(fs<0.0)(zcen,zcen,zcen);
      clist= where(clist>.1 & clist<.9);
      /* alternative possibilities which run about equally quickly are:
       *    clist!=floor(clist)
       *    clist%1.0
       * these both rely on the fact that 0.5*(1.0+1.0)==1.0 exactly
       * they also both call slow libm functions (floor and amod)
       */
    }

    if (numberof(clist)) {
      ntotal+= numberof(clist);
      /* we need to save:
       * (1) the absolute cell indices of the cells in clist
       * (2) the corresponding 2-by-2-by-2-by-ncells list of fslice
       *     values at the vertices of these cells
       */
      if (irregular) {
        fs= fs(,,,clist);
        if (got_xyz) _xyz3= _xyz3(,,,,clist);
      } else {
        list= to_corners3(clist, dims(2), dims(3));
        fs= fs(list);
        if (got_xyz) _xyz3= _xyz3(,list);
      }
      /* here, the iterator converts to absolute cell indices without
       * incrementing the chunk */
      if (need_clist) clist= iterator3(m3, chunk, clist);
      else clist= [];
      if (!(nchunk&127)) grow, results, array(pointer, 3, nchunk+128);
      ++nchunk;
      results(1,nchunk)= &clist;
      results(2,nchunk)= &fs;
      results(3,nchunk)= &_xyz3;
    }
  }
  _xyz3= [];  /* free memory */

  /* collect the results of the chunking loop */
  if (!ntotal) return [];
  if (need_clist) clist= array(0, ntotal);
  fs= array(0.0, 2,2,2,ntotal);
  if (got_xyz) xyz= array(0.0, 3, dimsof(fs));
  for (i=1,k=0 ; i<=nchunk ; ++i) {
    j= k+1;
    k+= numberof(*results(1,i));
    if (need_clist) clist(j:k)= *results(1,i);
    fs(,,,j:k)= *results(2,i);
    if (!is_void(xyz)) xyz(,,,,j:k)= *results(3,i);
  }
  results= [];  /* free memory */
  if (!got_xyz) xyz= xyz3(m3, clist);

  /* produce the lists of edge intersection points
   * -- generate 2x2x3x(nsliced) array of edge mask values
   * (mask non-zero if edge is cut by plane) */
  below= fs<0.0;
  mask= array(0n, 2,2,3,ntotal);
  mask(-,,,1,)= abs(below(dif,,,));
  mask(,-,,2,)= abs(below(,dif,,));
  mask(,,-,3,)= abs(below(,,dif,));
  list= where(mask);
  edges= list-1;
  cells= edges/12;      /* 0-origin momentarily */
  edges= (edges%12)+1;
  /* construct edge endpoint indices in fs, xyz arrays
   * the numbers are the endpoint indices corresponding to
   * the order of the 12 edges in the mask array */
  lower= [1,3,5,7, 1,2,5,6, 1,2,3,4](edges) + 8*cells;
  upper= [2,4,6,8, 3,4,7,8, 5,6,7,8](edges) + 8*cells;
  /* restore the cells array to 1-origin */
  cells+= 1;

  /* interpolate to find edge intersection points */
  fsl= fs(lower)(-,);
  fsu= fs(upper)(-,);
  /* following denominator guaranteed non-zero */
  xyz= (xyz(,lower)*fsu-xyz(,upper)*fsl)/(fsu-fsl);

  /* The xyz array is now the output xyzverts array,
   * but for the order of the points within each cell.  */

  /* give each sliced cell a "pattern index" between 0 and 255
   * (non-inclusive) representing the pattern of its 8 corners
   * above and below the slicing plane */
  pattern= below(*,)(+,) * (1<<indgen(0:7))(+);
  if (slice3_stats) {
    extern poly_patterns;
    poly_patterns= histogram(pattern, top=254);
  }
  /* broadcast the cell's pattern onto each of its sliced edges */
  pattern= pattern(-:1:12,)(list);

  /* To each pattern, there corresponds a permutation of the
   * twelve edges so that they occur in the order in which the
   * edges are to be connected.  Let each such permuation be
   * stored as a list of integers from 0 to 11 such that sorting
   * the integers into increasing order rearranges the edges at
   * the corresponding indices into the correct order.  (The position
   * of unsliced edges in the list is arbitrary as long as the sliced
   * edges are in the proper order relative to each other.)
   * Let these permutations be stored in a 12-by-254 array
   * poly_permutations (see next comment for explanation of 48): */
  pattern= poly_permutations(12*(pattern-1)+edges) + 48*cells;
  order= sort(pattern);
  xyz= xyz(,order);
  edges= edges(order);
  pattern= pattern(order);
  /* cells(order) is same as cells by construction */

  /* There remains only the question of splitting the points in
   * a single cell into multiple disjoint polygons.
   * To do this, we need one more precomputed array: poly_splits
   * should be another 12-by-254 array with values between 0 and 3
   * 0 for each edge on the first part, 1 for each edge on the
   * second part, and so on up to 3 for each edge on the fourth
   * part.  The value on unsliced edges can be anything, say 0.
   * With a little cleverness poly_splits can be combined with
   * poly_permutations, by putting poly_permutations =
   * poly_permutations(as described above) + 12*poly_splits
   * (this doesn't change the ordering of poly_permutations).
   * I assume this has been done here: */
  pattern/= 12;
  /* now pattern jumps by 4 between cells, smaller jumps within cells
   * get the list of places where a new value begins, and form a
   * new pattern with values that increment by 1 between each plateau */
  pattern= pattern(dif);
  list= grow([1], where(pattern)+1);
  pattern= (pattern!=0)(cum) + 1;

  nverts= histogram(pattern);
  xyzverts= xyz;

  /* finally, deal with any fcolor function */
  if (is_void(fcolor)) return [];

  /* if some polys have been split, need to split clist as well */
  if (numberof(list)>numberof(clist)) clist= clist(cells(list));
  if (!nointerp) {
    if (is_func(fcolor))
      return fcolor(m3, clist, lower, upper, fsl(1,), fsu(1,), cells);
    else
      return getc3(fcolor, m3, clist, lower, upper, fsl(1,), fsu(1,), cells);
  } else {
    if (is_func(fcolor)) return fcolor(m3, clist);
    else return getc3(fcolor, m3, clist);
  }
}

func _isosurface_slicer(m3, chunk)
{
  return getv3(iso_index, m3, chunk)-value;
}

func _plane_slicer(m3, chunk)
{
  return xyz3(m3, chunk)(+,..)*normal(+) - projection;
}

func to_corners3(list, ni, nj)
/* DOCUMENT to_corners(list, ni, nj)
     convert a LIST of cell indices in an (NI-1)-by-(NJ-1)-by-(nk-1)
     logically rectangular grid of cells into the list of
     2-by-2-by-2-by-numberof(LIST) cell corner indices in the
     corresponding NI-by-NJ-by-nk list of vertices.
 */
{
  ninj= ni*nj;
  list-= 1;
  ii= list/(ni-1);
  list+= ii + ni*(ii/(nj-1));
  return ([1,2]+[0,ni](-,)+[0,ninj](-,-,)) + list(-,-,-,);
}

/* The iterator3 function combines three distinct operations:
 * (1) If only the M3 argument is given, return the initial
 *     chunk of the mesh.  The chunk will be no more than
 *     chunk3_limit cells of the mesh.
 * (2) If only M3 and CHUNK are given, return the next CHUNK,
 *     or [] if there are no more chunks.
 * (3) If M3, CHUNK, and CLIST are all specified, return the
 *     absolute cell index list corresponding to the index list
 *     CLIST of the cells in the CHUNK.
 *     Do not increment the chunk in this case.
 *
 * The form of the CHUNK argument and return value for cases (1)
 * and (2) is not specified, but it must be recognized by the
 * xyz3 and getv3 functions which go along with this iterator3.
 * (For case (3), CLIST and the return value are both ordinary
 *  index lists.)
 */
func iterator3(m3, chunk, clist)
{
  return _car(_car(m3),4)(m3, chunk, clist);
}

/* biggest temporary is 3 doubles times this,
 * perhaps 4 or 5 doubles times this is most at one time */
chunk3_limit= 10000;

func iterator3_rect(m3, chunk, clist)
{
  if (is_void(chunk)) {
    dims= _car(_car(m3,2));  /* [ni,nj,nk] cell dimensions */
    ni= dims(1);
    nj= dims(2);
    nk= dims(3);
    ninj= ni*nj;
    if (chunk3_limit <= ni) {
      /* stuck with 1D chunks */
      ci= (ni-1)/chunk3_limit + 1;
      cj= ck= 0;
    } else if (chunk3_limit <= ninj) {
      /* 2D chunks */
      ci= ck= 0;
      cj= (ninj-1)/chunk3_limit + 1;
    } else {
      /* 3D chunks */
      ci= cj= 0;
      ck= (ninj*nk-1)/chunk3_limit + 1;
    }
    chunk= [[ci==0,cj==0,ck==0],
            [ni*((cj+ck)!=0),nj*(ck!=0)+(ci!=0),!ck],
            [ci,cj,ck],[ni,nj,nk]];

  } else {
    chunk= *chunk;
    ni= chunk(1,4);  nj= chunk(2,4);  nk= chunk(3,4);
    ninj= ni*nj;

    if (!is_void(clist)) {
      /* add offset for this chunk to clist and return */
      return [1,ni,ninj](+)*(chunk(,1)-1)(+) + clist;
    }
  }

  /* increment to next chunk */
  xi= chunk(1,2);  xj= chunk(2,2);  xk= chunk(3,2);

  if ((np= chunk(1,3))) {
    /* 1D chunks */
    if (xi==ni) {
      if (xj==nj) {
        if (xk==nk) return [];
        xk+= 1;
        xj= 1;
      } else {
        xj+= 1;
      }
      xi= 0;
    }
    ci= xi+1;
    step= ni/np;
    frst= ni%np;   /* first frst steps are step+1 */
    if (xi < (step+1)*frst) step+= 1;
    xi+= step;
    chunk(,1)= [ci,xj,xk];
    chunk(,2)= [xi,xj,xk];

  } else if ((np= chunk(2,3))) {
    if (xj==nj) {
      if (xk==nk) return [];
      xk+= 1;
      xj= 0;
    }
    cj= xj+1;
    step= nj/np;
    frst= nj%np;   /* first frst steps are step+1 */
    if (xj < (step+1)*frst) step+= 1;
    xj+= step;
    chunk(2:3,1)= [cj,xk];
    chunk(2:3,2)= [xj,xk];

  } else {
    if (xk==nk) return [];
    ck= xk+1;
    np= chunk(3,3);
    step= nk/np;
    frst= nk%np;   /* first frst steps are step+1 */
    if (xk < (step+1)*frst) step+= 1;
    xk+= step;
    chunk(3,1)= ck;
    chunk(3,2)= xk;
  }

  /* return pointer to make chunk easy to recognize for xyz3, getv3 */
  return &chunk;
}

func xyz3(m3, chunk)
/* DOCUMENT xyz3(m3, chunk)

     return vertex coordinates for CHUNK of 3D mesh M3.  The CHUNK
     may be a list of cell indices, in which case xyz3 returns a
     3x2x2x2x(dimsof(CHUNK)) list of vertex coordinates.  CHUNK may
     also be a mesh-specific data structure used in the slice3
     routine, in which case xyz3 may return a 3x(ni)x(nj)x(nk)
     array of vertex coordinates.  For meshes which are logically
     rectangular or consist of several rectangular patches, this
     is up to 8 times less data, with a concomitant performance
     advantage.  Use xyz3 when writing slicing functions or coloring
     functions for slice3.

   SEE ALSO: slice3, mesh3, getv3, getc3
 */
{
  xyz= _car(_car(m3),1)(m3, chunk);
  extern _xyz3;
  /* the cost of the following copy operation could be saved
   * by making _xyz3 a pointer, but then we would be relying on
   * the fslice caller of xyz3 not using the returned array for
   * scratch space - leave it as is for now */
  if (_xyz3_save) _xyz3= xyz;
  return xyz;
}

func xyz3_rect(m3, chunk)
{
  m3= _car(m3,2);
  if (structof(chunk)==pointer) {
    c= *chunk;
    return _car(m3,2)(,c(1,1):1+c(1,2),c(2,1):1+c(2,2),c(3,1):1+c(3,2));
  } else {
    dims= _car(m3);
    return _car(m3,2)(,to_corners3(chunk,dims(1)+1,dims(2)+1));
  }
}

func xyz3_unif(m3, chunk)
{
  m3= _car(m3,2);
  n= _car(m3,2);
  dxdydz= n(,1);
  x0y0z0= n(,2);
  if (structof(chunk)==pointer) {
    c= *chunk;
    i= c(,1)-1;
    n= c(,2)+1-i;
    xyz= array(x0y0z0+i*dxdydz, n(1), n(2), n(3));
    xyz(1,..)+= span(0.,dxdydz(1),n(1));
    xyz(2,..)+= span(0.,dxdydz(2),n(2))(-,);
    xyz(3,..)+= span(0.,dxdydz(3),n(3))(-,-,);
    return xyz;
  } else {
    dims= _car(m3);
    ni= dims(1);
    nj= dims(2);
    ninj= ni*nj;
    ijk= (chunk-1)(-:1:3,-,-,-,..);
    ijk(3,..)/= ninj;
    ijk(2,..)%= ninj;
    ijk(2,..)/= ni;
    ijk(1,..)%= ni;
    ijk+= [[[[0,0,0],[1,0,0]],[[0,1,0],[1,1,0]]],
           [[[0,0,1],[1,0,1]],[[0,1,1],[1,1,1]]]];
    return x0y0z0 + ijk*dxdydz;
  }
}

func getv3(i, m3, chunk)
/* DOCUMENT getv3(i, m3, chunk)

     return vertex values of the Ith function attached to 3D mesh M3
     for cells in the specified CHUNK.  The CHUNK may be a list of
     cell indices, in which case getv3 returns a 2x2x2x(dimsof(CHUNK))
     list of vertex coordinates.  CHUNK may also be a mesh-specific data
     structure used in the slice3 routine, in which case getv3 may
     return a (ni)x(nj)x(nk) array of vertex values.  For meshes which
     are logically rectangular or consist of several rectangular
     patches, this is up to 8 times less data, with a concomitant
     performance advantage.  Use getv3 when writing slicing functions
     for slice3.

   SEE ALSO: slice3, mesh3, getc3, xyz3
 */
{
  return _car(_car(m3),2)(i, m3, chunk);
}

func getv3_rect(i, m3, chunk)
{
  fi= _cdr(m3,2);
  m3= _car(m3,2);
  if (i<1 || i>_len(fi)) error, "no such mesh function as F"+pr1(i);
  dims= _car(m3);
  if (allof(dimsof(_car(fi,i))(2:4)==dims)) {
    error, "mesh function F"+pr1(i)+" is not vertex-centered";
  }
  if (structof(chunk)==pointer) {
    c= *chunk;
    return _car(fi,i)(c(1,1):1+c(1,2),c(2,1):1+c(2,2),c(3,1):1+c(3,2));
  } else {
    return _car(fi,i)(to_corners3(chunk,dims(1)+1,dims(2)+1));
  }
}

func getc3(i, m3, chunk, l, u, fsl, fsu, cells)
/* DOCUMENT getc3(i, m3, chunk)
         or getc3(i, m3, clist, l, u, fsl, fsu, cells)

     return cell values of the Ith function attached to 3D mesh M3
     for cells in the specified CHUNK.  The CHUNK may be a list of
     cell indices, in which case getc3 returns a (dimsof(CHUNK))
     list of vertex coordinates.  CHUNK may also be a mesh-specific data
     structure used in the slice3 routine, in which case getc3 may
     return a (ni)x(nj)x(nk) array of vertex values.  There is no
     savings in the amount of data for such a CHUNK, but the gather
     operation is cheaper than a general list of cell indices.
     Use getc3 when writing colorng functions for slice3.

     If CHUNK is a CLIST, the additional arguments L, U, FSL, and FSU
     are vertex index lists which override the CLIST if the Ith attached
     function is defined on mesh vertices.  L and U are index lists into
     the 2x2x2x(dimsof(CLIST)) vertex value array, say vva, and FSL
     and FSU are corresponding interpolation coefficients; the zone
     centered value is computed as a weighted average of involving these
     coefficients.  The CELLS argument is required by histogram to do
     the averaging.  See the source code for details.
     By default, this conversion (if necessary) is done by averaging
     the eight vertex-centered values.

   SEE ALSO: slice3, mesh3, getv3, xyz3
 */
{
  return _car(_car(m3),3)(i, m3, chunk, l, u, fsl, fsu, cells);
}

func getc3_rect(i, m3, chunk, l, u, fsl, fsu, cells)
{
  fi= _cdr(m3,2);
  m3= _car(m3,2);
  if (i<1 || i>_len(fi)) error, "no such mesh function as F"+pr1(i);
  dims= _car(m3);
  if (allof(dimsof(_car(fi,i))(2:4)==dims)) {
    if (structof(chunk)==pointer) {
      c= *chunk;
      return _car(fi,i)(c(1,1):c(1,2),c(2,1):c(2,2),c(3,1):c(3,2));
    } else {
      return _car(fi,i)(chunk);
    }
  } else {
    if (structof(chunk)==pointer) {
      c= *chunk;
      return _car(fi,i)(zcen:c(1,1):1+c(1,2),zcen:c(2,1):1+c(2,2),
                        zcen:c(3,1):1+c(3,2));
    } else {
      corners= _car(fi,i)(to_corners3(chunk,dims(1)+1,dims(2)+1));
      if (is_void(l)) {
        return 0.125*corners(sum:1:8,1,1,..);
      } else {
        /* interpolate corner values to get edge values */
        corners= (corners(l)*fsu-corners(u)*fsl)/(fsu-fsl);
        /* average edge values (vertex values of polys) on each poly */
        return histogram(cells,corners)/histogram(cells);
      }
    }
  }
}

/* ------------------------------------------------------------------------ */

/* There are 254 possible combinations of 8 vertices above and below
 * the slicing plane (not counting all above or all below).  */
func _construct3(void)
{
  /* construct the edge list for each of the 254 possibilities */
  i= indgen(254);
  below= transpose([[[i&1,i&2],[i&4,i&8]],[[i&16,i&32],[i&64,i&128]]],0);
  below= (below!=0);
  mask= array(0n, 2,2,3,254);
  mask(-,,,1,)= abs(below(dif,,,));
  mask(,-,,2,)= abs(below(,dif,,));
  mask(,,-,3,)= abs(below(,,dif,));

  /* walking the edges requires some connectivity information */
  start_face= [3,4,3,4, 1,2,1,2, 1,2,1,2];
  face_edges= [[5,11,7,9], [6,10,8,12],
               [1,9,3,10], [2,12,4,11],
               [1,6,2,5],  [3,7,4,8]];
  edge_faces= [[3,5],[4,5],[3,6],[4,6],[1,5],[2,5],
               [1,6],[2,6],[1,3],[2,3],[1,4],[2,4]];

  permute= array('\0', 12, 254);
  for (i=1 ; i<=254 ; ++i) permute(,i)= _walk3(mask(*,i));
  return permute;
}

func _walk3(mask)
{
  permute= splits= array(0, 12);
  split= 0;
  list= where(mask);
  edge= min(list);
  face= start_face(edge);

  for (i=0 ; i<numberof(list)-1 ; ++i) {
    /* record this edge */
    permute(edge)= i;
    splits(edge)= split;
    mask(edge)= 0;

    /* advance to next edge */
    try= face_edges(,face);
    now= abs(try-edge)(mnx);
    /* test opposite edge first (make X, never // or \\) */
    edge= try((now+1)%4+1);
    if (!mask(edge)) {
      /* otherwise one or the other opposite edges must be set */
      edge= try(now%4+1);
      if (!mask(edge)) {
        edge= try((now+2)%4+1);
        if (!mask(edge)) {
          /* we must have closed a loop */
          split+= 1;
          edge= min(where(mask));
        }
      }
    }
    try= edge_faces(,edge);
    now= try(1);
    face= face==now? try(2) : now;
  }
  /* last pass through loop */
  permute(edge)= i;
  splits(edge)= split;
  mask(edge)= 0;

  return permute+12*splits;
}

poly_permutations= _construct3();

/* ------------------------------------------------------------------------ */

func slice2x(plane, &nverts, &xyzverts, &values, &nvertb, &xyzvertb, &valueb)
/* DOCUMENT slice2, plane, nverts, values, xyzverts

     Slice a polygon list, retaining only those polygons or
     parts of polygons on the positive side of PLANE, that is,
     the side where xyz(+)*PLANE(+:1:3)-PLANE(4) > 0.0.
     The NVERTS, VALUES, and XYZVERTS arrays serve as both
     input and output, and have the meanings of the return
     values from the slice3 function.

   SEE ALSO: slice2, slice2_precision
 */
{
  _slice2x= 1;
  return slice2(plane, nverts, xyzverts, values, nvertb, xyzvertb, valueb);
}

func slice2(plane, &nverts, &xyzverts, &values, &nvertb, &xyzvertb, &valueb)
/* DOCUMENT slice2, plane, nverts, xyzverts
         or slice2, plane, nverts, xyzverts, values

     Slice a polygon list, retaining only those polygons or
     parts of polygons on the positive side of PLANE, that is,
     the side where xyz(+)*PLANE(+:1:3)-PLANE(4) > 0.0.
     The NVERTS, XYZVERTS, and VALUES arrays serve as both
     input and output, and have the meanings of the return
     values from the slice3 function.  It is legal to omit the
     VALUES argument (e.g.- if there is no fcolor function).

     In order to plot two intersecting slices, one could
     slice (for example) the horizontal plane twice (slice2x) -
     first with the plane of the vertical slice, then with minus
     that same plane.  Then, plot first the back part of the
     slice, then the vertical slice, then the front part of the
     horizontal slice.  Of course, the vertical plane could
     be the one to be sliced, and "back" and "front" vary
     depending on the view point, but the general idea always
     works.

   SEE ALSO: slice3, plane3, slice2x, slice2_precision
 */
{
  have_values= !is_void(values);

  /* get the list of indices into nverts (or values) for each of
   * the points in xyzverts */
  ndxs= histogram(1+nverts(psum))(1:-1);
  ndxs(1)+= 1;
  ndxs= ndxs(psum);

  /* form dot products of all the points with the given plane */
  dp= xyzverts(+,)*plane(+:1:3) - plane(4);

  /* separate into lists of unclipped and partially clipped polys */
  if (!slice2_precision) {
    /* if precision is not set, slice exactly at dp==0.0, with
     * any points exactly at dp==0.0 treated as if they had dp>0.0 */
    keep= (dp>=0.0);
  } else {
    /* if precision is set, polygons are clipped to +-precision,
     * so that any poly crossing +precision is clipped to dp>=+precision,
     * any poly crossing -precision is clipped to dp<=-precision, and
     * any poly lying entirely between +-precision is discarded entirely */
    keep= (dp>=slice2_precision);
  }
  nkeep= long(histogram(ndxs, keep));
  mask0= (nkeep==nverts);
  mask1= (nkeep!=0 & !mask0);
  list1= where(mask1);
  if (numberof(list1)) {
    nvertc= nverts(list1);
    if (have_values) valuec= values(list1);
    list= where(mask1(ndxs));
    xyzc= xyzverts(, list);
  }
  if (_slice2x) {
    if (!slice2_precision) {
      mask2= !nkeep;
      nvertc0= nvertc;
      valuec0= valuec;
      xyzc0= xyzc;
    } else {
      keep2= (dp>-slice2_precision);
      nkeep2= long(histogram(ndxs, keep2));
      mask2= (nkeep!=0 & nkeep<nverts);
      list2= where(mask2);
      if (numberof(list2)) {
        nvertc0= nverts(list2);
        if (have_values) valuec0= values(list2);
        listc= where(mask2(ndxs));
        xyzc0= xyzverts(, listc);
      }
      mask2= !nkeep2;
    }
    list2= where(mask2);
    if (numberof(list2)) {
      nvertb= nverts(list2);
      if (have_values) valueb= values(list2);
      xyzvertb= xyzverts(, where(mask2(ndxs)));
    } else {
      nvertb= valueb= xyzvertb= [];
    }
  }
  list0= where(mask0);
  if (numberof(list0)<numberof(nverts)) {
    nverts= nverts(list0);
    if (have_values) values= values(list0);
    xyzverts= xyzverts(, where(mask0(ndxs)));
  }

  /* done if no partially clipped polys */
  if (!numberof(list1) && !numberof(listc)) return;
  if (!numberof(list1)) goto skip;

  /* get dot products and keep list for the clipped polys */
  dp= dp(list);
  if (slice2_precision) dp-= slice2_precision;
  keep= (dp>=0.0);

  /* get the indices of the first and last points in each clipped poly */
  last= nvertc(psum);
  frst= last - nvertc + 1;

  /* get indices of previous and next points in cyclic order */
  prev= next= indgen(0:numberof(keep)-1);
  prev(frst)= last;
  next(prev)= indgen(numberof(keep));

  _slice2_part;

  grow, nverts, nvertc;
  if (have_values) grow, values, valuec;
  grow, xyzverts, xyzc;

  if (_slice2x) {
  skip:
    nvertc= nvertc0;
    valuec= valuec0;
    xyzc= xyzc0;
    if (!slice2_precision) {
      keep= !keep;
    } else {
      dp= dp(listc)+slice2_precision;
      keep= (dp>=0.0);
    }

    _slice2_part;

    grow, nvertb, nvertc;
    if (have_values) grow, valueb, valuec;
    grow, xyzvertb, xyzc;
  }
}

local slice2_precision;
/* DOCUMENT slice2_precision= precision
     Controls how slice2 (or slice2x) handles points very close to
     the slicing plane.  PRECISION should be a positive number or zero.
     Zero PRECISION means to clip exactly to the plane, with points
     exactly on the plane acting as if they were slightly on the side
     the normal points toward.  Positive PRECISION means that edges
     are clipped to parallel planes a distance PRECISION on either
     side of the given plane.  (Polygons lying entirely between these
     planes are completely discarded.)

     Default value is 0.0.

   SEE ALSO: slice2, slice2x
 */
slice2_precision= 0.0;

func _slice2_part
{
  extern nvertc, xyzc;

  /* find the points where any edges of the polys cut the plane */
  mask0= (!keep) & keep(next);   /* off-->on */
  list0= where(mask0);
  if (numberof(list0)) {
    list= next(list0);
    dpl= dp(list0)(-,);
    dpu= dp(list)(-,);
    xyz0= (xyzc(,list0)*dpu-xyzc(,list)*dpl)/(dpu-dpl);
  }
  mask1= (!keep) & keep(prev);   /* on-->off */
  list1= where(mask1);
  if (numberof(list1)) {
    list= prev(list1);
    dpl= dp(list1)(-,);
    dpu= dp(list)(-,);
    xyz1= (xyzc(,list1)*dpu-xyzc(,list)*dpl)/(dpu-dpl);
  }

  /* form an index list xold which gives the indices in the original
   * xyzc list at the places in the new, sliced xyzc list */
  mask= keep+mask0+mask1;  /* 0, 1, or 2 */
  list= mask(psum);  /* index values in new list */
  xold= array(0, list(0));
  mlist= where(mask);
  xold(list(mlist))= mlist;
  dups= where(mask==2);
  if (numberof(dups)) xold(list(dups)-1)= dups;

  /* form the new, sliced xyzc vertex list */
  xyzc= xyzc(,xold);
  if (numberof(list0)) xyzc(,list(list0))= xyz0;
  if (numberof(list1)) xyzc(,list(list1)-mask(list1)+1)= xyz1;

  /* get the list of indices into nvertc (or valuec) for each of
   * the points in xyzc */
  ndxs= histogram(1+last)(1:-1);
  ndxs(1)+= 1;
  ndxs= ndxs(psum);
  /* compute the new number of vertices */
  nvertc= histogram(ndxs(xold));
}

/* ------------------------------------------------------------------------ */

func pl3surf(nverts, xyzverts, values, cmin=, cmax=)
/* DOCUMENT pl3surf, nverts, xyzverts
         or pl3surf, nverts, xyzverts, values

     Perform simple 3D rendering of an object created by slice3
     (possibly followed by slice2).  NVERTS and XYZVERTS are polygon
     lists as returned by slice3, so XYZVERTS is 3-by-sum(NVERTS),
     where NVERTS is a list of the number of vertices in each polygon.
     If present, the VALUES should have the same length as NVERTS;
     they are used to color the polygon.  If VALUES is not specified,
     the 3D lighting calculation set up using the light3 function
     will be carried out.  Keywords cmin= and cmax= as for plf, pli,
     or plfp are also accepted.  (If you do not supply VALUES, you
     probably want to use the ambient= keyword to light3 instead of
     cmin= here, but cmax= may still be useful.)

   SEE ALSO: pl3tree, slice3, slice2, rot3, light3
 */
{
  require, "pl3d.i";
  if (_draw3) {
    list= nverts;
    nverts= _nxt(list);
    xyzverts= _nxt(list);
    values= _nxt(list);
    cmin= _nxt(list);
    cmax= _nxt(list);

    local x, y, z, list, vlist;
    get3_xy, xyzverts, x, y, z, 1;
    if (is_void(values)) {
      xyzverts(1,..)= x;
      xyzverts(2,..)= y;
      xyzverts(3,..)= z;
      values= get3_light(xyzverts, nverts);
    }
    sort3d, z, nverts, list, vlist;
    nverts= nverts(list);
    values= values(list);
    x= x(vlist);
    y= y(vlist);

    plfp, values,y,x,nverts, cmin=cmin,cmax=cmax, legend=string(0);
    return;
  }

  nverts+= 0;
  xyzverts+= 0.0;

  if (numberof(xyzverts)!=3*sum(nverts) || anyof(nverts<3) ||
      structof(nverts)!=long)
    error, "illegal or inconsistent polygon list";
  if (!is_void(values) && numberof(values)!=numberof(nverts))
    error, "illegal or inconsistent polygon color values";

  if (!is_void(values)) values+= 0.0;

  clear3;
  set3_object, pl3surf, _lst(nverts,xyzverts,values,cmin,cmax);
}

/* ------------------------------------------------------------------------ */

func pl3tree(nverts, xyzverts, values, plane, cmin=, cmax=)
/* DOCUMENT pl3tree, nverts, xyzverts
         or pl3tree, nverts, xyzverts, values, plane

     Add the polygon list specified by NVERTS (number of vertices in
     each polygon) and XYZVERTS (3-by-sum(NVERTS) vertex coordinates)
     to the currently displayed b-tree.  If VALUES is specified, it
     must have the same dimension as NVERTS, and represents the color
     of each polygon.  If VALUES is not specified, the polygons
     are assumed to form an isosurface which will be shaded by the
     current 3D lighting model; the isosurfaces are at the leaves of
     the b-tree, sliced by all of the planes.  If PLANE is specified,
     the XYZVERTS must all lie in that plane, and that plane becomes
     a new slicing plane in the b-tree.  

     Each leaf of the b-tree consists of a set of sliced isosurfaces.
     A node of the b-tree consists of some polygons in one of the
     planes, a b-tree or leaf entirely on one side of that plane, and
     a b-tree or leaf on the other side.  The first plane you add
     becomes the root node, slicing any existing leaf in half.  When
     you add an isosurface, it propagates down the tree, getting
     sliced at each node, until its pieces reach the existing leaves,
     to which they are added.  When you add a plane, it also propagates
     down the tree, getting sliced at each node, until its pieces
     reach the leaves, which it slices, becoming the nodes closest to
     the leaves.

     This structure is relatively easy to plot, since from any
     viewpoint, a node can always be plotted in the order from one
     side, then the plane, then the other side.

     This routine assumes a "split palette"; the colors for the
     VALUES will be scaled to fit from color 0 to color 99, while
     the colors from the shading calculation will be scaled to fit
     from color 100 to color 199.  (If VALUES is specified as a char
     array, however, it will be used without scaling.)
     You may specifiy a cmin= or cmax= keyword to affect the
     scaling; cmin is ignored if VALUES is not specified (use the
     ambient= keyword from light3 for that case).

   SEE ALSO: pl3surf, slice3, slice2, rot3, light3, split_palette
 */
{
  require, "pl3d.i";
  if (_draw3) {
    /* avoid overhead of local variables for _pl3tree and _pl3leaf */
    local x,y,z;
    local nverts, xyzverts, values, list, vlist, cmin, cmax;
    local nv, vv, xx, yy, zz, cmin, cmax;
    _pl3tree, nverts;
    return;
  }

  nverts= 0+nverts(*);
  xyzverts= double(xyzverts(,*));
  if (!is_void(values)) values= double(values(*));
  if (!is_void(plane)) plane= double(plane);

  if (numberof(xyzverts)!=3*sum(nverts) || anyof(nverts<3) ||
      structof(nverts)!=long)
    error, "illegal or inconsistent polygon list";
  if (!is_void(values) && numberof(values)!=numberof(nverts))
    error, "illegal or inconsistent polygon color values";
  if (!is_void(plane) && (dimsof(plane)(1)!=1 || numberof(plane)!=4))
    error, "illegal plane format, try plane3 function";

  leaf= _lst(_lst(nverts, xyzverts, values, cmin, cmax));

  /* retrieve current b-tree (if any) from 3D display list */
  tree= _cdr(_draw3_list, _draw3_n);
  if (is_void(tree) || _car(tree)!=pl3tree) {
    tree= _lst(plane, [], leaf, []);
  } else {
    tree= _car(tree,2);
    _pl3tree_add, leaf, plane, tree;
  }

  clear3;
  set3_object, pl3tree, tree;
}

func split_palette(name)
/* DOCUMENT split_palette
         or split_palette, "palette_name.gp"
     split the current palette or the specified palette into two
     parts; colors 0 to 99 will be a compressed version of the
     original, while colors 100 to 199 will be a gray scale.
   SEE ALSO: pl3tree, split_bytscl
 */
{
  if (!is_void(name)) palette, name;
  local r,g,b;
  palette,query=1, r,g,b;
  n= numberof(r);
  if (n<100) {
    palette, "viridis.gp";
    palette,query=1, r,g,b;
    n= numberof(r);
  }
  r= char(interp(r,indgen(n),span(1,n,100)));
  g= char(interp(g,indgen(n),span(1,n,100)));
  b= char(interp(b,indgen(n),span(1,n,100)));
  grow, r, char(span(0,255,100));
  grow, g, char(span(0,255,100));
  grow, b, char(span(0,255,100));
  palette, r,g,b;
}

func split_bytscl(x, upper, cmin=,cmax=)
/* DOCUMENT split_bytscl(x, 0)
         or split_bytscl(x, 1)
     as bytscl function, but scale to the lower half of a split
     palette (0-99, normally the color scale) if the second parameter
     is zero or nil, or the upper half (100-199, normally the gray
     scale) if the second parameter is non-zero.
   SEE ALSO: split_palette
 */
{
  x= bytscl(x,cmin=cmin,cmax=cmax,top=99);
  if (upper) x+= char(100);
  return x;
}

func _pl3tree(tree)
{
  /* avoid overhead of local variables for _pl3tree and _pl3leaf */
  extern x,y,z;

  /* tree is a 4-element list like this:
   * _lst(plane, back_tree, inplane_leaf, front_tree)
   * plane= _car(tree)  is void if this is just a leaf
   *                    in which case, only inplane_leaf is not void
   * back_tree= _car(tree,2)    is the part behind plane
   * inplane_leaf= _car(tree,3) is the part in the plane itself
   * front_tree= _car(tree,4)   is the part in front of plane
   */
  if (is_void(tree)) return;
  if (is_void(_car(tree))) {
    /* only the leaf is non-nil (but not a plane) */
    _pl3leaf, _car(tree,3), 1;
    return;
  }

  /* apply the 3D coordinate transform to two points along the
   * normal of the splitting plane to judge which is in front */
  get3_xy, [[0.,0.,0.],_car(tree)(1:3)], x,y,z, 1;

  /* plot the parts in order toward the current viewpoint */
  if (z(2) >= z(1)) {
    _pl3tree, _car(tree,2);
    _pl3leaf, _car(tree,3), 0;
    _pl3tree, _car(tree,4);
  } else {
    _pl3tree, _car(tree,4);
    _pl3leaf, _car(tree,3), 0;
    _pl3tree, _car(tree,2);
  }
}

func _pl3leaf(leaf, not_plane)
{
  /* avoid overhead of local variables for _pl3tree and _pl3leaf */
  extern x,y,z;
  extern nverts, xyzverts, values, list, vlist;

  /* count number of polys, number of vertices */
  nverts= xyzverts= 0;
  _map, _pl3tree_count, leaf;

  /* accumulate polys and vertices into a single polygon list */
  values= array(char, nverts);
  nverts= array(0, nverts);
  x= y= array(0.0, xyzverts);
  if (not_plane) z= x;
  else z= [];
  list= vlist= 1;
  _map, _pl3tree_accum, leaf;

  /* sort the single polygon list */
  if (not_plane) {
    sort3d, z, nverts, list, vlist;
    nverts= nverts(list);
    values= values(list);
    x= x(vlist);
    y= y(vlist);
  }

  plfp, values,y,x,nverts, legend=string(0);
}

func _pl3tree_count(item)
{
  nverts+= numberof(_nxt(item));
  xyzverts+= numberof(_nxt(item))/3;
}

func _pl3tree_accum(item)
{
  /* avoid overhead of local variables for _pl3tree and _pl3leaf */
  extern x,y,z;
  extern nverts, xyzverts, values, list, vlist;
  extern nv, vv, xx, yy, zz, cmin, cmax;

  nv= _nxt(item);
  xyzverts= _nxt(item);
  vv= _nxt(item);
  cmin= _nxt(item);
  cmax= _nxt(item);

  if (is_void(vv)) {
    /* this is an isosurface to be shaded */
    get3_xy, xyzverts, xx, yy, zz, 1;
    xyzverts(1,..)= xx;
    xyzverts(2,..)= yy;
    xyzverts(3,..)= zz;
    vv= get3_light(xyzverts, nv);
    vv= split_bytscl(vv, 1, cmin=0.0, cmax=cmax);
  } else {
    /* this is to be pseudo-colored */
    if (not_plane) get3_xy, xyzverts, xx, yy, zz, 1;
    else get3_xy, xyzverts, xx, yy;
    if (structof(vv)!=char)
      vv= split_bytscl(vv, 0, cmin=cmin, cmax=cmax);
  }

  /* accumulate nverts and values */
  item= numberof(nv)-1;
  nverts(list:list+item)= nv;
  values(list:list+item)= vv;
  list+= item+1;

  /* accumulate x, y, and z */
  item= numberof(xx)-1;
  x(vlist:vlist+item)= xx;
  y(vlist:vlist+item)= yy;
  if (not_plane) z(vlist:vlist+item)= zz;
  vlist+= item+1;
}

func _pl3tree_add(leaf, plane, tree)
{
  if (!is_void(_car(tree))) {
    /* tree has slicing plane, slice new leaf or plane and descend */
    back= _pl3tree_slice(_car(tree), leaf);
    if (back) {
      if (_car(tree,2)) _pl3tree_add, back, plane, _car(tree,2);
      else _car, tree, 2, _lst([], [], back, []);
    }
    if (leaf) {
      if (_car(tree,4)) _pl3tree_add, leaf, plane, _car(tree,4);
      else _car, tree, 4, _lst([], [], leaf, []);
    }

  } else if (!is_void(plane)) {
    /* tree is just a leaf, but this leaf has slicing plane */
    _car, tree, 1, plane;
    leaf= _car(tree, 3, leaf);  /* swap new leaf with original leaf */
    back= _pl3tree_slice(plane, leaf);   /* slice old leaf by plane */
    if (back) _car, tree, 2, _lst([], [], back, []);
    if (leaf) _car, tree, 4, _lst([], [], leaf, []);

  } else {
    /* tree is just a leaf and this leaf has no slicing plane */
    _cdr, leaf, 1, _car(tree, 3, leaf);
  }
}

func _pl3tree_slice(plane, &leaf)
{
  back= frnt= [];
  for ( ; leaf ; leaf=_cdr(leaf)) {
    ll= _car(leaf);
    /* each item in the leaf list is itself a list */
    nvf= nvb= _nxt(ll);
    xyzf= xyzb= _nxt(ll);
    valf= valb= _nxt(ll);
    slice2x, plane, nvf, xyzf, valf, nvb, xyzb, valb;
    if (numberof(nvf))
      frnt= _cat(_lst(_cat(nvf,xyzf,_lst(valf),ll)),frnt);
    if (numberof(nvb))
      back= _cat(_lst(_cat(nvb,xyzb,_lst(valb),ll)),back);
  }
  leaf= frnt;
  return back;
}

#if 0
func pl3tree_prt(void)
{
  tree= _cdr(_draw3_list, _draw3_n);
  if (is_void(tree) || _car(tree)!=pl3tree) {
    write, "<current 3D display not a pl3tree>";
  } else {
    tree= _car(tree,2);
    _pl3tree_prt,tree,0
  }
}

func _pl3tree_prt(tree,depth)
{
  if (is_void(tree)) return;
  indent= strpart("                               ",1:1+2*depth);
  indent= strpart(indent, 1:-1);
  write, indent+"+DEPTH= "+pr1(depth);
  if (_len(tree)!=4) write, indent+"***error - not a tree";
  write, indent+"plane= "+pr1(_car(tree));
  back= _car(tree,2);
  list= _car(tree,3);
  frnt= _car(tree,4);
  if (is_void(back)) write, indent+"back= []";
  else _pl3tree_prt, back, depth+1;

  while (list) {
    leaf= _nxt(list);
    write, indent+"leaf length= "+pr1(_len(leaf));
    write, indent+"npolys= "+pr1(numberof(_car(leaf)))+
      ", nverts= "+pr1(sum(_nxt(leaf)));
    write, indent+"nverts= "+pr1(numberof(_nxt(leaf))/3)+
      ", nvals= "+pr1(numberof(_nxt(leaf)));
  }

  if (is_void(frnt)) write, indent+"frnt= []";
  else _pl3tree_prt, frnt, depth+1;
  write, indent+"-DEPTH= "+pr1(depth);
}
#endif

/* ------------------------------------------------------------------------ */
