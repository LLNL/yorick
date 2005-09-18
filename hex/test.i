/*
 * $Id: test.i,v 1.1 2005-09-18 22:05:50 dhmunro Exp $
 * set various skip==1 before including to skip tests
 * set skip_all=1 to skip all tests
 * set timing_only==1 before including to do big timing test
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

require, "hex.i";

func one_by_one(refl)
{
  extern xyz, bndy, mesh;
  x= span(0,1,2)(,-:1:2,-:1:2);
  y= span(0,1,2)(-:1:2,,-:1:2);
  z= span(0,1,2)(-:1:2,-:1:2,);
  xyz= transpose([x,y,z],2);
  if (refl) {
    bndy= [2,1,2,1,2,1];
    xyz+= 1.;
  } else {
    bndy= [1,1,1,1,1,1];
  }
  mesh= hex_mesh2(xyz,bndy);
}

func indiv_test
{
  /* test individual rays */
  extern xyz, bndy, mesh, rays, svals, topos, s, topo;

  one_by_one;

  p= [0.49,0.53,0.57](,-:1:6);
  q= 0.*p;
  q(,2:0:2)= -(q(,1:-1:2)= unit(3));
  rays= [p,q];

  topos= [2,8](,-:1:6);
  svals= array(1., 2, 6);
  svals(1,)= transpose([-p(,1),p(,1)-1.])(*);
  svals= svals(psum,);

  for (i=1 ; i<=6 ; i++) {
    topo= hex5_track(mesh, rays(,i,), s);
    if (numberof(topo)!=2 || numberof(s)!=2)
                                  error, "(1) single ray, face "+pr1(i);
    if (topo(1)!=2 || topo(2)!=8) error, "(2) single ray, face "+pr1(i);
    if (anyof(abs(s-svals(,i))>1.e-12))
                                  error, "(3) single ray, face "+pr1(i);
  }
  write, "finished indiv_test";

  topos= topos(*);
  svals= svals(*);
}

func multi_test
{
  /* test multiple rays in single call */
  extern xyz, bndy, mesh, rays, svals, topos, s, topo;

  one_by_one;

  p= [0.49,0.53,0.57](,-:1:6);
  q= 0.*p;
  q(,2:0:2)= -(q(,1:-1:2)= unit(3));
  rays= [p,q];

  topos= [2,8](,-:1:6)(*);
  svals= array(1., 2, 6);
  svals(1,)= transpose([-p(,1),p(,1)-1.])(*);
  svals= svals(psum,)(*);

  if (!flog_only) {
    topo= hex5_track(mesh, rays, s);
    if (numberof(topo)!=12 || numberof(s)!=12)
                                    error, "(1) multi rays";
    if (anyof(topo!=topos))         error, "(2) multi rays";
    if (anyof(abs(s-svals)>1.e-12)) error, "(3) multi rays";
    write, "finished multi_test";
  }

  flog, xyz, bndy, rays, topos, svals, "multi rays";
}

func refl_test
{
  /* test some reflections */
  extern xyz, bndy, mesh, rays, svals, topos, s, topo;

  one_by_one, 1;

  p= [0.49,0.53,0.57](,-:1:6);
  q= 0.*p;
  q(,2:0:2)= -(q(,1:-1:2)= unit(3));
  rays= [p,q];

  topos= [3,8,8](,-:1:6)(*);
  svals= array(1., 3, 6);
  svals(1,)= transpose([-p(,1),p(,1)-2.])(*);
  svals= svals(psum,)(*);

  if (!flog_only) {
    topo= hex5_track(mesh, rays, s);
    if (numberof(topo)!=18 || numberof(s)!=18)
                                    error, "(1) reflected multi rays";
    if (anyof(topo!=topos))         error, "(2) reflected multi rays";
    if (anyof(abs(s-svals)>1.e-12)) error, "(3) reflected multi rays";
    write, "finished refl_test";
  }

  flog, xyz, bndy, rays, topos, svals, "reflected multi rays";
}

func rotmat(axis, angle)
{
  axis/= abs(axis(1),axis(2),axis(3));
  proj= axis*axis(-,);
  cros= array(0., 3,3);
  cros([8,3,4])= -(cros([6,7,2])= axis);
  return proj + (unit(3)-proj)*cos(angle) + cros*sin(angle);
}

func flog(xyz, bndy, rays, topos, svals, text)
{
  /* repeat a calculation with 64 different random orientations */
  extern xyz0, ray0, mesh, s, topo;
  if (flog_off) return;
  ns= numberof(topos);
  i= is_void(flog_i)? 1 : flog_i;
  for ( ; i<=64 ; i++) {
    matrix= rotmat(2.*random(3)-1.+1.e-12, pi*random());
    xyz0= matrix(,+)*xyz(+,..);
    ray0= matrix(,+)*rays(+,..);
    mesh= hex_mesh2(xyz0,bndy);
    topo= hex5_track(mesh, ray0, s);
    if (numberof(topo)!=ns || numberof(s)!=ns)
                                    error, "(1) flogging "+text+" "+pr1(i);
    if (anyof(topo!=topos))         error, "(2) flogging "+text+" "+pr1(i);
    if (anyof(abs(s-svals)>1.e-12)) error, "(3) flogging "+text+" "+pr1(i);
  }
  write, "finished flogging "+text;
}

func two_by_two(refl)
{
  extern xyz, bndy, mesh;
  x= span(0,2,3)(,-:1:3,-:1:3);
  y= span(0,2,3)(-:1:3,,-:1:3);
  z= span(0,2,3)(-:1:3,-:1:3,);
  xyz= transpose([x,y,z],2);
  bndy= refl? [1,2,1,2,1,2] : [1,1,1,1,1,1];
  mesh= hex_mesh2(xyz,bndy);
}

func fake_track(mesh, rays, &s)
{
  p= rays(,-,*,1);
  q= rays(,-,*,2);
  //local xyz;
  //bndy= hex_mesh2(mesh, xyz);

  x= xyz(1,,1,1);
  if (bndy(2)==2) x= grow(x,2*x(0)-x(-1:1:-1));
  else if (bndy(1)==2) x= grow(2*x(1)-x(0:2:-1),x);
  y= xyz(2,1,,1);
  if (bndy(4)==2) y= grow(y,2*y(0)-y(-1:1:-1));
  else if (bndy(3)==2) y= grow(2*y(1)-y(0:2:-1),y);
  z= xyz(3,1,1,);
  if (bndy(6)==2) z= grow(z,2*z(0)-z(-1:1:-1));
  else if (bndy(5)==2) z= grow(2*z(1)-z(0:2:-1),z);
  //xyz= [];
  nx= numberof(x);
  ny= numberof(y);
  nz= numberof(z);
  nxy= nx*ny;
  ntot= nx+ny+nz;

  nr= numberof(p)/3;
  cell= array(0, ntot, nr);
  s= double(cell);

  qd= q+(!q)*1.e-30;
  s(1:nx,)= ss= (x - p(1,,))/qd(1,,);
  sn= min(ss(1:1,),ss(0:0,));
  sx= max(ss(1:1,),ss(0:0,));
  s(nx+1:nx+ny,)= ss= (y - p(2,,))/qd(2,,);
  get_extremes, ss, sn, sx;
  s(nx+ny+1:ntot,)= ss= (z - p(3,,))/qd(3,,);
  get_extremes, ss, sn, sx;
  ss= qd= [];

  s= sc= s(sort(s));
  sc(2:0,)= s(zcen,);
  xyz0= p + q*sc(-,,);
  cell= digitize(xyz0(1,,), x);
  if (bndy(2)==2) {
    n= (nx+1)/2;
    list= where(cell>n);
    if (numberof(list)) cell(list)= 2*n+1-cell(list);
  } else if (bndy(1)==2) {
    n= (nx+1)/2;
    cell-= n-1;
    list= where(cell<2);
    if (numberof(list)) cell(list)= 3-cell(list);
  } else {
    n= nx;
  }
  y= digitize(xyz0(2,,), y);
  if (bndy(4)==2) {
    ny= (ny+1)/2;
    list= where(y>ny);
    if (numberof(list)) y(list)= 2*ny+1-y(list);
    nxy= n*ny;
  } else if (bndy(3)==2) {
    ny= (ny+1)/2;
    y-= ny-1;
    list= where(y<2);
    if (numberof(list)) y(list)= 3-y(list);
    nxy= n*ny;
  }
  cell+= n*(y-1);
  z= digitize(xyz0(3,,), z);
  if (bndy(6)==2) {
    n= (nz+1)/2;
    list= where(z>n);
    if (numberof(list)) z(list)= 2*n+1-z(list);
  } else if (bndy(5)==2) {
    n= (nz+1)/2;
    z-= n-1;
    list= where(z<2);
    if (numberof(list)) z(list)= 3-z(list);
  }
  cell+= nxy*(z-1);
  x= y= z= p= q= [];

  mask= (s>=sn) & (s<=sx);
  sn= sx= [];
  n= mask(sum,);
  list= where(!n);
  if (numberof(list)) {
    mask(1,list)= 1;  /* mark rays that missed */
    s(1,list)= 1;
    n(list)= 1;
  }
  cell(mask(mxx,)+indgen(0:numberof(mask)-1:ntot))= n;
  list= where(mask);
  s= s(list);
  return cell(list);
}

func get_extremes(s, &sn, &sx)
{
  sx= min(max(s(1:1,),s(0:0,)),sx);
  sn= max(min(s(1:1,),s(0:0,)),sn);
}

func indiv2_test
{
  /* test individual rays */
  extern xyz, bndy, mesh, rays, svals, topos, s, topo;

  two_by_two;

  p= [0.49,0.53,0.57](,-:1:6);
  q= 0.*p;
  q(,2:0:2)= -(q(,1:-1:2)= unit(3));
  rays= [p,q];

  topos= fake_track(mesh, rays, svals);

  for (i=1 ; i<=6 ; i++) {
    topo= hex5_track(mesh, rays(,i,), s);
    if (numberof(topo)!=3 || numberof(s)!=3)
                                   error, "(1) single ray 2, face "+pr1(i);
    range= call(3*i-2:3*i);
    if (anyof(topo!=topos(range))) error, "(2) single ray 2, face "+pr1(i);
    if (anyof(abs(s-svals(range))>1.e-12))
                                   error, "(3) single ray 2, face "+pr1(i);
  }
  write, "finished indiv2_test";
}

func multi2_test
{
  /* test multiple rays in single call */
  extern xyz, bndy, mesh, rays, svals, topos, s, topo;

  two_by_two;

  p= [0.49,0.53,0.57](,-:1:6);
  q= 0.*p;
  q(,2:0:2)= -(q(,1:-1:2)= unit(3));
  rays= [p,q];

  svals= [];
  topos= fake_track(mesh, rays, svals);

  if (!flog_only) {
    topo= hex5_track(mesh, rays, s);
    if (numberof(topo)!=18 || numberof(s)!=18)
                                    error, "(1) multi2 rays";
    if (anyof(topo!=topos))         error, "(2) multi2 rays";
    if (anyof(abs(s-svals)>1.e-12)) error, "(3) multi2 rays";
    write, "finished multi2_test";
  }

  flog, xyz, bndy, rays, topos, svals, "multi2 rays";
}

func refl2_test
{
  /* test some reflections */
  extern xyz, bndy, mesh, rays, svals, topos, s, topo;

  two_by_two, 1;

  p= [0.49,0.53,0.57](,-:1:6);
  q= 0.*p;
  q(,2:0:2)= -(q(,1:-1:2)= unit(3));
  rays= [p,q];

  svals= [];
  topos= fake_track(mesh, rays, svals);

  if (!flog_only) {
    topo= hex5_track(mesh, rays, s);
    if (numberof(topo)!=30 || numberof(s)!=30)
                                    error, "(1) reflected multi2 rays";
    if (anyof(topo!=topos))         error, "(2) reflected multi2 rays";
    if (anyof(abs(s-svals)>1.e-12)) error, "(3) reflected multi2 rays";
    write, "finished refl2_test";
  }

  flog, xyz, bndy, rays, topos, svals, "reflected multi2 rays";
}

func fake_mesh2(xyz,bndy)
{
  mesh = _orig_hex_mesh2(xyz,bndy);
  hex_query, mesh, xyz, bound, mbnds, blks;
  length = blks(1).length;
  cells = array(1n, length(1), length(2)/length(1), length(3)/length(2));
  cells(1,,) = 0n;
  cells(,1,) = 0n;
  cells(,,1) = 0n;
  starts = -where(cells);
  i = long(random()*numberof(starts)) + 1;
  return hex_mesh(xyz, bound, numberof(mbnds), &mbnds,
                  numberof(blks), &blks, starts(i));
  
}

_orig_hex_mesh2 = hex_mesh2;

func refl2p_test
{
  /* test some reflections */
  extern xyz, bndy, mesh, rays, svals, topos, s, topo;

  hex_mesh2 = fake_mesh2;
  two_by_two, 1;

  p= [0.49,0.53,0.57](,-:1:6);
  q= 0.*p;
  q(,2:0:2)= -(q(,1:-1:2)= unit(3));
  p(,4:6) += 2.0;
  rays= [p,q];

  svals= [];
  topos= fake_track(mesh, rays, svals);
  for (i=j=1 ; i<=numberof(topos) ; i+=n,j+=k) {
    n = topos(i);
    for (k=n-1 ; k>0 ; --k)
      if (svals(i+k) < 0.) break;
    k = n-k;
    topos(j) = k;
    if (k > 1) {
      topos(j+1:j+k-1) = topos(i+n-k+1:i+n-1);
      svals(j:j+k-1) = svals(i+n-k:i+n-1);
    }
  }
  --j;
  topos = topos(1:j);
  svals = svals(1:j);

  if (!flog_only) {
    topo= hex5_track(mesh, rays, s);
    if (numberof(topo)!=j || numberof(s)!=j)
                                    error, "(1p) reflected multi2 rays";
    if (anyof(topo!=topos))         error, "(2p) reflected multi2 rays";
    if (anyof(abs(s-svals)>1.e-12)) error, "(3p) reflected multi2 rays";
    topos = topo;
    svals = s;
    write, "finished refl2p_test";
  }

  flog, xyz, bndy, rays, topos, svals, "reflected multi2 rays, alt start";
}

func simple_angle
{
  /* test rays incident at angle */
  extern xyz, bndy, mesh, rays, svals, topos, s, topo;

  one_by_one, 1;

  q= [1.,2.,3.]/sqrt(14.);
  p= transpose([grow(span(.11,2.24,5),1.74),.85,0.]);
  rays= [p,q];

  svals= [];
  topos= fake_track(mesh, rays, svals);
  n= numberof(topos);

  if (!flog_only) {
    topo= hex5_track(mesh, rays, s);
    if (numberof(topo)!=n || numberof(s)!=n)
                                    error, "(1) simple angle rays";
    if (anyof(topo!=topos))         error, "(2) simple angle rays";
    if (anyof(abs(s-svals)>1.e-12)) error, "(3) simple angle rays";
    write, "finished simple_angle";
  }

  flog, xyz, bndy, rays, topos, svals, "simple angle rays";
}

func fire_away
{
  /* test lots of ray directions */
  extern xyz, bndy, mesh, rays, svals, topos, s, topo;

  two_by_two, 1;

  p= 5*random(3, 100);  /* some should miss */
  q= 2*random(3, 100)-1.;
  q/= abs(q(1,),q(2,),q(3,))(-,);
  rays= [p,q];

  svals= [];
  topos= fake_track(mesh, rays, svals);
  sred= svals;
  tred= topos;
  nlist= track_reduce(tred, sred);
  write,"Histogram of intersection counts: "+pr1(histogram(nlist+1));

  topo= hex5_track(mesh, rays, s);
  n= numberof(topos);
  if (numberof(topo)!=n || numberof(s)!=n)
                                  error, "(1) fire_away rays";
  if (anyof(topo!=topos))         error, "(2) fire_away rays";
  if (anyof(abs(s-svals)>1.e-11)) error, "(3) fire_away rays";
  write, "finished fire_away";
}

func pathology
{
  /* test pathologies */
  extern xyz, bndy, mesh, rays, svals, topos, s, topo;

  two_by_two, 1;

  p= [1.,1.,1.];
  q= [0.,0.,1.];
  rays= [p,q];
  topo= hex5_track(mesh, rays, s);
  if (topo(1)!=5 || anyof(abs(s-span(-1,3,5))>1.e-12))
    error, "(1) pathology failed";

  q= [1.,1.,1.]/sqrt(3.);
  rays= [p,q];
  topo= hex5_track(mesh, rays, s);
  svals= span(-1,3,5)(-:1:3,)(*)(3:-2)*sqrt(3);
  if (topo(1)!=11 || anyof(abs(s-svals)>1.e-12))
    error, "(2) pathology failed";

  p= [2.,2.,2.];
  q= [0.,0.,1.];
  rays= [p,q];
  topo= hex5_track(mesh, rays, s);
  if (topo(1)!=5 || anyof(abs(s-span(-2,2,5))>1.e-12))
    error, "(3) pathology failed";

  p= [2.,2.,2.];
  q= [0.,0.,-1.];
  rays= [p,q];
  topo= hex5_track(mesh, rays, s);
  if (topo(1)!=5 || anyof(abs(s-span(-2,2,5))>1.e-12))
    error, "(4) pathology failed";

  p= [1.,1.,1.];
  q= [0.,0.,-1.];
  rays= [p,q];
  topo= hex5_track(mesh, rays, s);
  if (topo(1)!=5 || anyof(abs(s-span(-3,1,5))>1.e-12))
    error, "(5) pathology failed";

  write, "finished pathology";
}

func big_mesh(n_big)
{
  /* time performance on a large mesh */
  extern xyz, bndy, mesh, rays, svals, topos, s, topo;

  if (!is_void(n_big)) n= n_big;
  else n= 11;
  xyz= array(0., 3, n,n,n);
  xyz(1,,,)= span(0,1,n);
  xyz(2,,,)= span(0,1,n)(-,);
  xyz(3,,,)= span(0,1,n)(-,-,);
  bndy= [1,2,1,2,1,2];
  mesh= hex_mesh2(xyz,bndy);
  write,format=" ...constructed %ldx%ldx%ld mesh + reflections all axes\n",
    n-1,n-1,n-1;

  rays= array(0., 3,100,100,2);
  rays(,,,2)= [1.,1.,1.]/sqrt(3.);
  xy= span(-sqrt(2.),sqrt(2.),100);
  z= span(-1.,1.,100);
  rays(1,,,1)= xy;
  rays(2,,,1)= -xy;
  rays(3,,,1)= z(-,);
  nrays= numberof(rays)/6;
  write, "...constructed 100x100 ray set";

  elapsed1= elapsed2= elapsed3= elapsed4= time= [0.,0.,0.];

  timer, time;
  topo= hex5_track(mesh, rays, s);
  timer, time, elapsed2;
  write,format="hex5 tracked %ld rays, which cut %ld faces\n",nrays,
    numberof(topo);

  /* have pity on fake_track by doing rays in 25 groups */
  timer, time;
  if (!skip_check) {
    sptr= tptr= array(pointer,25);
    nptr= 0;
    write, format=" %s","checking";
    for (i=1 ; i<=25 ; ++i) {
      topos= fake_track(mesh, rays(,,4*i-3:4*i,), svals);
      nptr+= numberof(topos);
      tptr(i)= &topos;
      sptr(i)= &svals;
      topos= svals= [];
      write,format="%s",".";
    }
    write, format="%s\n","";
    topos= array(0, nptr);
    svals= array(0., nptr);
    for (i=1,j=0 ; i<=25 ; ++i,j+=nptr) {
      nptr= numberof(*tptr(i));
      topos(j+1:j+nptr)= *tptr(i);
      svals(j+1:j+nptr)= *sptr(i);
    }
    tptr= sptr= [];
  } else if (skip_check==2) {
    x= y= z= span(0,2,2*n-1);
    topos= reg_track(x, y, z, rays, svals);
  }
  timer, time, elapsed1;

  if (!skip_check || skip_check==2) {
    nm= skip_check? "reg" : "fake";
    nfaces= numberof(topos);
    write,format="%s tracked %ld rays, which cut %ld faces\n",nm,nrays,nfaces;
    svals(where(topos==1))= 1.0;
    if (numberof(topo)!=nfaces || numberof(s)!=nfaces) {
      write,format="%s\n","WARNING tracking does not check";
    } else if (anyof(abs(s-svals)>1.e-12)) {
      write,format="WARNING max tracking error = %g\n",max(abs(s-svals));
    }
  }
  timer, time, elapsed3;

  if (!skip_reduce) nlist= track_reduce(topo, s);
  timer, time, elapsed4;

  write, format=" hex? crossed %ld cells\n", numberof(topo);

  if (skip_check && skip_check==2) nm= "reg_track time";
  else nm= "fake_track time";
  timer_print, "hex?_track time", elapsed2, nm, elapsed1,
    "comparison time", elapsed3, "track_reduce time", elapsed4;
}

func do_tests
{
  write, format="\nTesting function %s\n",pr1(hex5_track);

  if (!skip_indiv && !timing_only) indiv_test;
  if (!skip_multi && !timing_only) multi_test;
  if (!skip_refl && !timing_only) refl_test;
  if (!skip_indiv2 && !timing_only) indiv2_test;
  if (!skip_multi2 && !timing_only) multi2_test;
  if (!skip_refl2 && !timing_only) refl2_test;
  if (!skip_refl2 && !timing_only) refl2p_test;
  if (!skip_simple && !timing_only) simple_angle;
  if (!skip_fire && !timing_only) fire_away;
  if (!skip_pathology && !timing_only) pathology;
  if (!skip_degen && !timing_only) degen_test,3,,1;

  if (timing_only) big_mesh, n_big;
}

func do24f_tests
{
  hex5_track= hex24f_track;
  do_tests;
}

func do24b_tests
{
  hex5_track= hex24b_track;
  do_tests;
}

func doreg_tests
{
  write, format="\nTesting function %s\n",pr1(reg_track);

  x= y= z= [0., 1.];

  p= [0.49,0.53,0.57](,-:1:6);
  q= 0.*p;
  q(,2:0:2)= -(q(,1:-1:2)= unit(3));
  rays= [p,q];

  topos= [2,8](,-:1:6)(*);
  svals= array(1., 2, 6);
  svals(1,)= transpose([-p(,1),p(,1)-1.])(*);
  svals= svals(psum,)(*);

  topo= reg_track(x, y, z, rays, s);
  if (numberof(topo)!=12 || numberof(s)!=12)
                                  error, "(1) reg multi rays";
  if (anyof(topo!=topos))         error, "(2) reg multi rays";
  if (anyof(abs(s-svals)>1.e-12)) error, "(3) reg multi rays";
  write, "finished reg multi_test";

  two_by_two;
  x= xyz(1,,1,1);
  y= xyz(2,1,,1);
  z= xyz(3,1,1,);
  svals= [];
  topos= fake_track(mesh, rays, svals);

  topo= reg_track(x, y, z, rays, s);
  if (numberof(topo)!=18 || numberof(s)!=18)
                                  error, "(1) reg multi2 rays";
  if (anyof(topo!=topos))         error, "(2) reg multi2 rays";
  if (anyof(abs(s-svals)>1.e-12)) error, "(3) reg multi2 rays";
  write, "finished reg multi2_test";

  q= [1.,2.,3.]/sqrt(14.);
  p= transpose([grow(span(.11,2.24,5),1.74),.85,0.]);
  rays= [p,q];

  svals= [];
  topos= fake_track(mesh, rays, svals);
  n= numberof(topos);

  topo= reg_track(x, y, z, rays, s);
  if (numberof(topo)!=n || numberof(s)!=n)
                                  error, "(1) reg simple angle rays";
  if (anyof(topo!=topos))         error, "(2) reg simple angle rays";
  s(where(topo==1))= 1.0;
  if (anyof(abs(s-svals)>1.e-12)) error, "(3) reg simple angle rays";
  write, "finished reg simple_angle";

}

func sphere(ix, jx, kx, nparts, refl)
{
  extern xyz, bndy, mesh;
  if (!nparts) nparts= 6;
  nparts= 2./nparts;
  r= span(0,1.0, ix);
  if (refl) refl= 0.5*pi;
  else      refl= 0.;
  theta= span(refl,pi, kx);
  phi= span(0,nparts*pi, jx);
  rst= r*sin(theta(-,-,));
  xyz= array(0., 3,ix,jx,kx);
  xyz(1,,,)= rst*cos(phi(-,));
  xyz(2,,,)= rst*sin(phi(-,));
  xyz(3,,,)= -r*cos(theta(-,-,));
  b= 2 + (nparts>1.999);
  bndy= [2,1,b,b,2,2];
  mesh= hex_mesh2(xyz,bndy);
}

func degen_test(n, tracker, flag)
{
  extern rays, p, q, mesh;
  if (is_void(tracker)) tracker= hex5_track;
  if (!flag) write, format="\nTesting function %s\n",pr1(tracker);

  if (is_void(n)) n= is_void(n_big)? 11 : n_big;
  sphere, n,n,n,4,1;

  if (!flag)
    write,format=" ...constructed %ldx%ldx%ld octant+reflections all axes\n",
      n-1,n-1,n-1;

  time= elapsed1= elapsed2= [0.,0.,0.];

  if (!skip_random) {
    if (!flag) write, format="%s", "Testing 2500 random rays...";
    p= 0.5*(random(3, 2500)-0.5);
    q= random(3, 2500)-0.5;
    timer, time;
    topo= tracker(mesh, [p,q], s);
    timer, time, elapsed1;
    if (!flag) write, "done, reducing to check for misses";
    nlist= track_reduce(topo,s);
    if (!flag) write, format=" hex? crossed %ld cells\n", numberof(topo);
    if (sum(!nlist))
      write, "WARNING -- some rays (=[p,q]) erroniously miss mesh";
  }

  if (!skip_image) {
    if (!flag) write, format="%s", "Testing 2500 image-like parallel rays...";
    rays= array(0., 3,50,50,2);
    rays(,,,2)= [1.,1.,1.]/sqrt(3.);
    xy= 0.15*span(-sqrt(2.),sqrt(2.),50);
    z= 0.15*span(-1.,1.,50);
    rays(1,,,1)= xy;
    rays(2,,,1)= -xy;
    rays(3,,,1)= z(-,);
    timer, time;
    topo= tracker(mesh, rays, s);
    timer, time, elapsed2;
    if (!flag) write, "done, reducing to check for misses";
    nlist= track_reduce(topo,s);
    if (!flag) write, format=" hex? crossed %ld cells\n", numberof(topo);
    if (sum(!nlist))
      write, "WARNING -- some rays erroniously miss mesh";
  }

  if (!flag) timer_print, "random rays", elapsed1, "image rays", elapsed2;
  else write, "finished degen_test";
}

func integ_test
{
  self= double(indgen(5));
  tran= array(0.5, 5);
  self= [self,256.*self](*);
  tran= [tran,tran](*);
  rslt= track_integ([5,5], tran, self, 0);
  correct= [0.03125, 5.+4.*.5+3.*.25+2.*.125+1.*.0625];
  correct= [correct,correct];
  correct(2,2)*= 256.;
  if (anyof(abs(rslt-correct)>1.e-12)) error, "(1) track_integ failed";
  rslt= track_integ([5,5], tran, self, 1);
  if (anyof(abs(rslt-correct)>1.e-12)) error, "(2) track_integ failed";
  rslt= track_integ([5,5], [tran,tran], [self,self], 1);
  correct= transpose([correct,correct],2);
  if (anyof(abs(rslt-correct)>1.e-12)) error, "(3) track_integ failed";
  rslt= track_integ([5,5], transpose([tran,tran]), transpose([self,self]), 0);
  if (anyof(abs(rslt-correct)>1.e-12)) error, "(4) track_integ failed";

  rslt= track_integ([5,5], transpose([tran,tran]), , 0);
  if (anyof(abs(rslt-correct(,1,..))>1.e-12)) error, "(5) track_integ failed";
  rslt= track_integ([5,5], [tran,tran], , 1);
  if (anyof(abs(rslt-correct(,1,..))>1.e-12)) error, "(6) track_integ failed";

  correct= [15.,256.*15];
  correct= transpose([correct,correct]);
  rslt= track_integ([5,5], , transpose([self,self]), 0);
  if (anyof(abs(rslt-correct)>1.e-12)) error, "(7) track_integ failed";
  rslt= track_integ([5,5], , [self,self], 1);
  if (anyof(abs(rslt-correct)>1.e-12)) error, "(8) track_integ failed";

  write, format="\n%s\n", "Finished testing track_integ";
}

func full_test
{
  extern xyz,mesh,rays,c,s,nlist,akap,ekap,answ,result,resid;

  x= span(-3,3,4)(,-:1:4,-:1:4);
  y= span(-3,3,4)(-:1:4,,-:1:4);
  z= span(-3,3,4)(-:1:4,-:1:4,);
  xyz= transpose([x,y,z],2);
  mesh= hex_mesh2(xyz, array(1,6));

  x=span(-3,3,4)(,-:1:4);
  y=transpose(x);
  rays= pic3_rays(x,y,[[0,0,0],[0,-1,0]],[0,0,1]);

  c= hex5_track(mesh, rays, s);
  nlist= track_reduce(c, s, rays);

  ekap= akap= array(0., 4,4,4,2);
  ekap(2:4,2:4,2:4,1)= 1.0;
  ekap(2:4,2:4,2:4,2)= 10.0;
  akap(2:4,2:4,2:4,1)= -0.5*log(0.5);
  akap(2:4,2:4,2:4,2)= -0.5*log(0.1);
  ekap(3,3,3,1)= 2.0;
  ekap(3,3,3,2)= 20.0;
  akap(3,3,3,1)= -log(0.5);
  akap(3,3,3,2)= -log(0.1);

  answ= array(0., 2,2,3,3);
  answ(1,1,,)= 0.125;
  answ(2,1,,)= 0.001;
  answ(1,2,,)= 0.5 + 0.25 + 0.125;
  answ(2,2,,)= 9. + 0.9 + 0.09;
  answ(1,1,2,2)= 0.5*0.125;
  answ(2,1,2,2)= 0.1*0.001;
  answ(1,2,2,2)= 0.5 + 3.*0.25 + 0.5*0.125;
  answ(2,2,2,2)= 9. + 2.*0.99 + 0.1*0.09;

  answ1= answ(,2,,);
  answ1(1,,)= 2.*3.;
  answ1(2,,)= 2.*30.;
  answ1(1,2,2)= 2.*4.;
  answ1(2,2,2)= 2.*40.;

  akap0= akap;  ekap0= ekap;  answ0= answ;  rays0= rays;

  /* basic test */

  result= track_solve(nlist, c, s, akap, ekap, 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(1) track_solve failed";
  answ= answ(,1,,);
  result= track_solve(nlist, c, s, akap, , 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(2) track_solve failed";
  answ= answ1;
  result= track_solve(nlist, c, s, , ekap, 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(3) track_solve failed";

  akap= transpose(akap, 2);
  ekap= transpose(ekap, 2);
  answ= answ0;

  result= track_solve(nlist, c, s, akap, ekap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(4) track_solve failed";
  answ= answ(,1,,);
  result= track_solve(nlist, c, s, akap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(5) track_solve failed";
  answ= answ1;
  result= track_solve(nlist, c, s, , ekap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(6) track_solve failed";

  /* single group */

  akap= akap0(..,1);
  ekap= ekap0(..,1);
  answ= answ0(1,..);

  result= track_solve(nlist, c, s, akap, ekap, 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(1a) track_solve failed";
  answ= answ(1,,);
  result= track_solve(nlist, c, s, akap, , 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(2a) track_solve failed";
  answ= answ1(1,..);
  result= track_solve(nlist, c, s, , ekap, 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(3a) track_solve failed";

  answ= answ0(1,..);

  result= track_solve(nlist, c, s, akap, ekap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(4a) track_solve failed";
  answ= answ(1,,);
  result= track_solve(nlist, c, s, akap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(5a) track_solve failed";
  answ= answ1(1,..);
  result= track_solve(nlist, c, s, , ekap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(6a) track_solve failed";

  /* with cell adjustment */

  answ= answ0;
  akap= roll(akap0,[-1,-1,-1]);
  ekap= roll(ekap0,[-1,-1,-1]);
  c0= c;   s0= s;  nlist0= nlist;
  c_adjust, c, mesh;

  result= track_solve(nlist, c, s, akap, ekap, 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(1b) track_solve failed";
  answ= answ(,1,,);
  result= track_solve(nlist, c, s, akap, , 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(2b) track_solve failed";
  answ= answ1;
  result= track_solve(nlist, c, s, , ekap, 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(3b) track_solve failed";

  akap= transpose(akap, 2);
  ekap= transpose(ekap, 2);
  answ= answ0;

  result= track_solve(nlist, c, s, akap, ekap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(4b) track_solve failed";
  answ= answ(,1,,);
  result= track_solve(nlist, c, s, akap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(5b) track_solve failed";
  answ= answ1;
  result= track_solve(nlist, c, s, , ekap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(6b) track_solve failed";

  /* with body centered cell adjustment */

  answ= answ0;
  akap= akap0(2:0,2:0,2:0,);
  ekap= ekap0(2:0,2:0,2:0,);
  c= c0;
  c_adjust, c, mesh, 1;

  result= track_solve(nlist, c, s, akap, ekap, 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(1c) track_solve failed";
  answ= answ(,1,,);
  result= track_solve(nlist, c, s, akap, , 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(2c) track_solve failed";
  answ= answ1;
  result= track_solve(nlist, c, s, , ekap, 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(3c) track_solve failed";

  akap= transpose(akap, 2);
  ekap= transpose(ekap, 2);
  answ= answ0;

  result= track_solve(nlist, c, s, akap, ekap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(4c) track_solve failed";
  answ= answ(,1,,);
  result= track_solve(nlist, c, s, akap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(5c) track_solve failed";
  answ= answ1;
  result= track_solve(nlist, c, s, , ekap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(6c) track_solve failed";

  /* single ray */

  akap= akap0;
  ekap= ekap0;
  answ= answ0(,,2,2);
  rays0= rays;
  rays= rays0(,2,2,);
  c= hex5_track(mesh, rays, s);
  nlist= track_reduce(c, s, rays);

  result= track_solve(nlist, c, s, akap, ekap, 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(1d) track_solve failed";
  answ= answ(,1);
  result= track_solve(nlist, c, s, akap, , 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(2d) track_solve failed";
  answ= answ1(,2,2);
  result= track_solve(nlist, c, s, , ekap, 1);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(3d) track_solve failed";

  akap= transpose(akap, 2);
  ekap= transpose(ekap, 2);
  answ= answ0(,,2,2);

  result= track_solve(nlist, c, s, akap, ekap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(4d) track_solve failed";
  answ= answ(,1);
  result= track_solve(nlist, c, s, akap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(5d) track_solve failed";
  answ= answ1(,2,2);
  result= track_solve(nlist, c, s, , ekap);
  resid= abs(result-answ);
  if (max(resid)>1.e-10) error, "(6d) track_solve failed";

  write,format="%s\n\n","Finished full_test";
}

//skip_indiv= 1;
//skip_multi= 1;
//skip_refl= 1;
//skip_indiv2= 1;
//skip_multi2= 1;
//skip_refl2= 1;
//skip_simple= 1;
//skip_fire= 1;
//skip_pathology= 1;
//skip_degen= 1;
//skip_integ= 1;
//skip_full= 1;

//flog_off= 1;
//flog_only= 1;
//flog_i= 1;

//timing_only= 1;
skip_check= 2;

//skipreg= 1;
//skip5= 1;
//skip24f= 1;
//skip24b= 1;

if (!skip_all) {
  if (!skipreg && !timing_only) doreg_tests;
  if (!skip5) do_tests;
  if (!skip24f) do24f_tests;
  if (!skip24b) do24b_tests;
  if (!skip_integ && !timing_only) integ_test;
  if (!skip_full && !timing_only) full_test;
}
