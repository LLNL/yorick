/*
 * $Id: ylmdec.i,v 1.1 2005-09-18 22:05:46 dhmunro Exp $
 * decompose a radiation pattern into spherical harmonics
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*

Idea: Use plato.i:bucky to generate 20*4^n nearly equally spaced
points on the surface of a sphere.  From each of these points, many
rays are launched into 2pi sterradians randomly, in order to find the
total radiation flux incident at that point.  These fluxes are used in
turn to compute the coefficients of the Ylm for the radiation pattern.

The flux=integral(domega*mu*specific_intensity) is
  sum(mu*specific_intensity) * 2*pi/numberof(rays)
because the rays are uniformly distributed over 2pi sterradians.

flux = sum(alm*Ylm), where
alm = integral(domega*flux*Ylm)
    = sum(flux*(Ylm*domega))

Up to l=n, there are (n+1)^2 Ylm; however, since the functions we need
to transform are real, only need (n+1)*(n+2)/2 of these (the others
are their complex conjugates).  This is manageable for <=5000 points
and n<=12, but rapidly gets nasty for more points or more l-values.

 */

/*= SECTION() compute Ylm coefficients using hex ray tracer ===============*/

#include "plato.i"
#include "legndr.i"

/* bucky(n,1) is 20*4^n ready-to-use pts */

func get_ylm(pts, lmax, &list)
/* DOCUMENT get_ylm(pts, lmax, list)
     returns values of Ylm at the 3-by-anything array of unit vectors
     PTS, for all l and m up to LMAX.  The return value is
     (lmax+1)*(lmax+2)/2-by-anything.  The LIST is also returned; its
     value is [1, 2,2, 3,3,3, 4,4,4,4, ..., LMAX+1,LMAX+1,...,LMAX+1]
     -- i copies of each integer i up to LMAX+1.  This can be useful
     in conjunction with the histogram function; its values are l+1
     for each element of the returned array's first index.  The m
     values are [0, 0,1, 0,1,2, 0,1,2,3, 0,1,2,3,4, ...].  Negative m
     values are omitted, since they would be the conjugates of m>0.
       llist= list-1;
       mlist= (!list(dif))(cum)-(list-2)*(list-1)/2;
 */
{
  mu= pts(3,..);
  phi= atan(pts(2,..),pts(1,..));
  ephi= exp(1i*phi);
  lmax= ((lmax+1)*(lmax+2))/2;
  ylm= array(0i, lmax, dimsof(pts(1,..)));
  for (n=1,l=0 ; n<=lmax ; l++)
    for (m=0,mphi=1. ; m<=l ; m++,n++,mphi*=ephi)
      ylm(n,..)= ylm_coef(l,m)*legndr(l,m,mu)*mphi;
  list= long(sqrt(indgen(1:2*lmax:2))-0.5)+1;
  return ylm;
}

func ylm_test(n)
{
  local domega, list;
  lmax= 12;
  pts= bucky(n,1,domega);
  ylm= get_ylm(pts,lmax,list);
  dylm= (ylm*domega(-,..))(,*);
  domega= [];
  write, format="Ylm constructed: %ld lm values at %ld points\n",
    dimsof(dylm)(2), dimsof(dylm)(3);

  p0= ylm(1,*).re;
  p1= ylm(2,*).re;
  p2= ylm(4,*).re;
  p3= ylm(7,*).re;
  p4= ylm(11,*).re;
  p44= ylm(15,*).re;
  p6= ylm(22,*).re;
  p10= ylm(56,*).re;
  p12= ylm(79,*).re;

  signal= p0 - p1 + p2 - p3 + p4 - p44 + p6 - p10 + p12;
  junk= indgen(numberof(alm));
  junk([1,2,4,7,11,15,22,56,79])= 0;
  junk= where(junk);

  write, "\n(1) raw fidelity test";
  noise= 0.;
  _ylm_test;

  write, "\n(2) noise robustness test, 10% full width per point";
  noise= 0.10*max(abs(signal))*(random(numberof(signal))-0.5);
  _ylm_test;

  write, "\n(3) noise robustness test, 3% full width per point";
  noise= 0.03*max(abs(signal))*(random(numberof(signal))-0.5);
  _ylm_test;

  write, "\n(4) noise robustness test, 1% full width per point";
  noise= 0.01*max(abs(signal))*(random(numberof(signal))-0.5);
  _ylm_test;
}

func _ylm_test(void)
{
  alm= dylm(,+)*(signal+noise)(+);
  write, format="  Y00 component (+1) re= %g im= %g\n", alm(1).re, alm(1).im;
  write, format="  Y10 component (-1) re= %g im= %g\n", alm(2).re, alm(2).im;
  write, format="  Y20 component (+1) re= %g im= %g\n", alm(4).re, alm(4).im;
  write, format="  Y30 component (-1) re= %g im= %g\n", alm(7).re, alm(7).im;
  write, format="  Y40 component (+1) re= %g im= %g\n", alm(11).re, alm(11).im;
  write,format="  Y44 component (-1/2) re= %g im= %g\n",alm(15).re,alm(15).im;
  write, format="  Y60 component (+1) re= %g im= %g\n", alm(22).re, alm(22).im;
  write,format="  Y10 0 component (-1) re= %g im= %g\n",alm(56).re,alm(56).im;
  write,format="  Y12 0 component (+1) re= %g im= %g\n",alm(79).re,alm(79).im;
  aalm= abs(alm)(junk);
  write, format="  Other components: max= %g, avg=%g\n",max(aalm),avg(aalm);
}

func generays(pts, nrpp)
{
  c= pts(3,..);
  dims= dimsof(c);

  /* (ux,uy,uz) uniformly distributed on hemisphere z>0 */
  uz= random(nrpp,dims);
  s= sqrt(1.-uz*uz);
  uy= 2.*pi*random(nrpp,dims);
  ux= s*cos(uy);
  uy= s*sin(uy);

  px= pts(1,..);
  py= pts(2,..);
  pxy= abs(px,py);
  r= 1./abs(c,pxy);
  c*= r;
  s= pxy*r;
  c= c(-:1:nrpp,..);
  s= s(-:1:nrpp,..);

  /* turn u=(0,0,1) to lie along pts direction */
  q= array(0.,3,nrpp,dims);
  q(3,..)= uz*c - ux*s;     /* rotate z-axis in (z,x) to pts theta */
  ux=      ux*c + uz*s;
  axis= !pxy;
  r= 1./(pxy+axis);
  c= (px*r + axis)(-:1:nrpp,..);    /* rotate about z to pts (x,y) */
  s= (py*r)(-:1:nrpp,..);
  q(2,..)= uy*c + ux*s;
  q(1,..)= ux*c - uy*s;

  return [pts(,-,..),q];
}

rays_per_batch= 13000;
ray_tracker= hex24f_track;

func accumulate(n, f, mesh, rcap, rtrack, &pts, &dylm, &flux, &nrpp, quiet=)
/* DOCUMENT alm= accumulate(n, f, mesh, rcap, rtrack, pts, dylm, flux, nrpp)

     Do N batches (approximately rays_per_batch each, default 13000) of
     rays from file F, MESH = hydra_mesh(F), analyzing the symmetry of
     a capsule of radius RCAP (all rays tangent to this sphere),
     tracing the rays only down to radius RTRACK (>1.001*RCAP).

     This routine is designed to be called several times successively
     to get better statistics.  PTS, DYLM, FLUX, and NRPP are all updated.
     PTS = unit vectors in directions of points on capsule surface
           defaults to bucky(3,1): 1280 points with an intrinsic
           fidelity of about 1% out to l=12
           use bucky(4,1,domega), dylm= get_ylm(pts,lmax,list)*domega(-,..)
           to get 5120 points and intrinsic fidelity of about 0.2% at l=12
     DYLM = ylm*domega for PTS
     FLUX = accumulated flux at each of PTS; units are power/area
     NRPP = accumulated number of rays per point of capsule surface
     ALM = 91 coefficients of Ylm up to l=12 corresponding to FLUX
           l= [0, 1,1, 2,2,2, 3,3,3,3, 4,4,4,4,4, ...]
           m= [0, 0,1, 0,1,2, 0,1,2,3, 0,1,2,3,4, ...]

 */
{
  local list;
  if (is_void(pts)) {
    lmax= 12;
    pts= bucky(3,1,domega);
    dylm= get_ylm(pts,lmax,list);
    dylm= (dylm*domega(-,..))(,*);
    domega= [];
  } else if (dimsof(dylm)(1)>2) {
    dylm= dylm(,*);
  }
  if (is_void(flux) || !nrpp) {
    flux= 0.*pts(1,..);
    nrpp= 0;
  }
  nrpp0= 0;
  while (n-- > 0) {
    flx= do_batch(f, mesh, pts, rcap, rtrack, nrpp0);
    flux= flux*nrpp + flx*nrpp0;
    nrpp+= nrpp0;
    flux/= double(nrpp);
  }
  return dylm(,+)*(flux(*))(+);
}

/* pts= bucky(ndivs,1) is appropriate input to do_batch */

func do_batch(f, mesh, pts, rcap, rtrack, &nrpp)
{
  np= numberof(pts)/3;
  nrpp= (rays_per_batch-1)/np + 1;
  rays= generays(rcap*pts,nrpp);
  mu= (pts(,-,..)*rays(..,2))(sum,..);
  rtrack= (rtrack/double(rcap))^2 - 1.;
  slimits= (rcap*(sqrt(mu*mu+rtrack)-mu))(-:1:2,..);
  slimits(2,..)= 1.e35;

  local s;
  c= ray_tracker(mesh, rays, s);  /* hex24f_track or hex5track */
  nlist= track_reduce(c, s, rays, slimits, flip=1);
  nlost= sum(nlist==0);
  if (nlost)
    write, format="WARNING: lost %ld rays out of %ld\n",nlost,numberof(nlist);

  mu*= 2.*pi/nrpp;
  dhnu= gb(dif);
  flux= 0.*pts(1,..);
  if (one_group) {
    /* the full akap and ekap arrays may be too big for memory,
     * in which case use this loop to do one group at a time
     * if many batches are done, the same akap and ekap are recomputed
     * many times -- this is a lot of exps */
   for (i=1 ; i<=numberof(dhnu) ; i++) {
      akap(unmixed)= lookup_akap(opac, i, io, fd, ft);
      ekap(unmixed)= B_nu_bar(gb(i:i+1), tmat)(1,..);
      if (numberof(mixed)) {
        ak= lookup_akap(opac, i, iox, fdx, ftx)*vf;
        akap(mixed)= akx= histogram(mixhist, ak);
        ekap(mixed)= histogram(mixhist,
                               B_nu_bar(gb(i:i+1), tmatx)(1,..)*ak)/akx;
        ak= akx= [];
      }
      weight= dhnu(i)*mu;
      flux+= (track_solve(nlist,c,s,akap,ekap)(2,..)*weight)(sum,..);
    }
  } else {
    /* assume full akap, ekap are in memory instead of interpolation
     * parameters io,fd,ft,unmixed,mixed,iox,fdx,ftx,vf,tmat,tmatx */
    flux+= (track_solve(nlist,c,s,akap,ekap)(+,2,..)*dhnu(+)*mu)(sum,..);
  }

  return flux;
}

/* for multiple file problems, this routine unfortunately opens
 * and closes all files three times -- hence in principle it could be
 * made to go three times faster, but that would require combining
 * hydra_mesh, h_mix, and h_data into one giant routine */
func read_state(f, rho, te, gb, opac, &mesh, args)
{
  /* read and load mesh into memory */
  local hydra_aux_data;
  hydra_aux_names = ["ireg", "den", "tmat"];
  if (is_void(args)) mesh = hydra_mesh(f);
  else mesh = hydra_mesh(f, args(1), args(2), args(3), args(4), args(5));

  /* read and load 7 mixed-mesh size arrays required for opacity lookup */
  extern unmixed;  /* index list into work for unmixed zones */
  extern tmat;     /* temperature for unmixed zones */
  extern io;       /* ll index into opacity table (unmixed zones) */
  extern fd;       /* density interpolation fraction (unmixed zones) */
  extern ft;       /* temperature interpolation fraction (unmixed zones) */
  extern mixed;    /* index list into work for mixed zones */
  extern mixhist;  /* zone index list for mixed subzones */
  extern vf;       /* volume fraction for mixed subzones */
  extern tmatx;    /* temperature for mixed subzones */
  extern iox;      /* ll index into opacity table (mixed subzones) */
  extern fdx;      /* density interpolation fraction (mixed subzones) */
  extern ftx;      /* temperature interpolation fraction (mixed subzones) */

  extern akap, ekap;

  irho = double(indgen(numberof(rho)));
  irho(0) -= 1.e-6; /* guarantee no overreach */
  ite = double(indgen(numberof(te)));
  ite(0) -= 1.e-6;  /* guarantee no overreach */

  /* extract mixed zone material (ireg),
   * and position in opacity table for each mixed subzone */
  local matlist, mixdat;
  pdata = h_mix(f, matlist, ["vf", "ireg", "den", "tmat"], mixdat);
  eq_nocopy, mixed, *mixdat(2);
  eq_nocopy, mixhist, *mixdat(4);
  if (numberof(mixed)) {
    eq_nocopy, vf, *pdata(1);
    eq_nocopy, iox, *pdata(2);
    eq_nocopy, fdx, *pdata(3);
    eq_nocopy, tmatx, *pdata(4);
    fdx = interp(irho,rho, log(fdx));
    ftx = interp(ite,te, log(tmatx));
    distill_interp, iox, fdx, ftx;
  } else {
    vf = iox = fdx = tmatx = ftx = [];
  }
  pdata = [];

  /* extract unmixed zone material (ireg),
   * and position in opacity table for each unmixed zone */
  /* pdata = h_data(f, ["ireg", "den", "tmat"]); */
  eq_nocopy, io, *hydra_aux_data(1);
  eq_nocopy, fd, *hydra_aux_data(2);
  eq_nocopy, tmat, *hydra_aux_data(3);
  if (numberof(mixed)) io(mixed)= 0;        /* zero out mixed zones */
  unmixed = where(io);
  io = io(unmixed);
  fd = interp(irho,rho, log(fd(unmixed)));
  tmat = tmat(unmixed);
  ft = interp(ite,te, log(tmat));
  distill_interp, io, fd, ft;

  hex_query, mesh, akap;  /* akap set to mesh xyz temporarily */
  if (!is_void(opac)) {
    dims = dimsof(akap);
    dims(2) = ngrp = dimsof(opac)(2);
    akap = ekap = array(0.,dims);
    for (i=1 ; i<=ngrp ; i++) {
      akap(i,unmixed) = lookup_akap(opac, i, io, fd, ft);
      ekap(i,unmixed) = B_nu_bar(gb(i:i+1), tmat)(1,..);
      if (!numberof(mixed)) continue;
      /* interpolation formulas for mixed zones assume all
       * materials in mixture are thin within each zone
       * -- opacity may be huge underestimate if one component thick
       *    possibly could recover slightly by breaking into track_solve
       *    and computing expm1 over each component separately... */
      ak = lookup_akap(opac, i, iox, fdx, ftx)*vf;
      akap(i,mixed) = akx = histogram(mixhist, ak);
      ekap(i,mixed) = histogram(mixhist,
                                B_nu_bar(gb(i:i+1), tmatx)(1,..)*ak)/akx;
      ak = akx = [];
    }
    /* opacity interpolation is done, can discard the raw interpolation
     * arrays now -- for fewer than 10 or 20 groups, should certainly
     * take this branch; for more there is some question */
    io=fd=ft=iox=fdx=ftx=vf=tmat=tmatx=mixed=unmixed=mixhist = [];
  } else {
    akap = ekap = akap(1,..);
  }
}

func distill_interp(&ir, &id, &it)
{
  lid = long(id);
  lit = long(it);
  /* index of ll corner of interpolation box in opacity table */
  ir = lid + numberof(rho)*(lit + numberof(te)*ir - (numberof(te)+1));
  id -= lid;
  it -= lit;
}

func lookup_akap(opac, igrp, io, fd, ft)
{
  /* if igrp is a range instead of a scalar, fd and ft need
   * to have a leading - index added before calling this */
  lo = opac(igrp,io);
  lo += (opac(igrp,1+io)-lo)*fd;
  nd = dimsof(opac)(3);
  hi = opac(igrp,nd+io);
  hi += (opac(igrp,1+nd+io)-hi)*fd;
  return exp(lo + (hi-lo)*ft);
}

func read_otable(filename, &gb,&rho,&te,&opac)
{
  f = open(filename);

  dtab0 = deltab0 = 0.;
  nftp = nkappa = ntime = nmat = ngrp = nrho = nte = nd = 0;
  read,f, dtab0, deltab0;
  read,f, nftp,nmat,nkappa,ngrp,nrho,nte,ntime;

  if ((nftp!=-3 && nftp!=-2) || nkappa==2 || ntime>1)
    error, "unrecognized type of opacity table: "+filename;

  if (nftp==-3)
    error, "cannot use a time dependent opacity table: "+filename;

  gb = array(0., ngrp+1);
  rho = array(0., nrho);
  te = array(0., nte);
  read,f, gb;
  read,f, rho;  /* actually log density */
  read,f, te;   /* actually log temperature */

  read,f,nd;

  opac = array(0., ngrp,nrho,nte,nmat);

  read,f, opac;   /* actually log opacity */

  opac += rho(-,);  /* convert from log(cm^2/g) to log(1/cm) */
}
