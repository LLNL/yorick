/*
 * $Id: ydrat.c,v 1.1 2005-09-18 22:04:57 dhmunro Exp $
 * High level interface routines for Yorick Drat (interpreter dependent).
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "drat.h"
#include "ydata.h"
#include "yio.h"
#include "pstdlib.h"

extern BuiltIn Y_form_mesh, Y_update_mesh, Y_find_boundary, Y__raw_track,
  Y__raw1_flat, Y__raw2_flat, Y__raw1_linear, Y__raw2_linear,
  Y__raw_pcens, Y__init_drat, Y_set_tolerances, Y__get_msize;

/* Implement Drat FullMesh as a foreign Yorick data type.  */
typedef struct DratMesh DratMesh;
struct DratMesh {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  FullMesh mesh;
};

extern DratMesh *NewDratMesh(int zsym, long khold, long lhold);
extern void FreeDratMesh(void *dm);  /* ******* Use Unref(dm) ******* */

extern DratMesh *YGetDMesh(Symbol *s, int nilOK);

/*--------------------------------------------------------------------------*/

void Y_form_mesh(int nArgs)
{
  if (nArgs!=3) YError("form_mesh takes exactly three arguments");
  PushDataBlock(NewDratMesh((int)YGetInteger(sp-2),
                            YGetInteger(sp-1)-1, YGetInteger(sp)-1));
}

void Y_update_mesh(int nArgs)
{
  DratMesh *dm;
  double *rt, *zt;
  int *ireg;
  Dimension *rdims, *zdims, *idims;
  long kmax, lmax;
  Array *array;

  if (nArgs!=3 && nArgs!=4)
    YError("update_mesh takes exactly three or four arguments");

  dm= YGetDMesh(sp-nArgs+1, 1);
  rt= YGet_D(sp-nArgs+2, 0, &rdims);
  zt= YGet_D(sp-nArgs+3, 0, &zdims);
  if (nArgs>3 && YNotNil(sp)) ireg= YGet_I(sp, 0, &idims);
  else { ireg= 0; idims= 0; }

  /* check that rt dimensions are credible */
  kmax= dm->mesh.mesh.kmax;
  lmax= dm->mesh.mesh.lmax;
  if (kmax==0) {
    /* this is the first time this mesh has been updated */
    if (!rdims) goto badd;
    lmax= rdims->number;
    rdims= rdims->next;
    if (!rdims || rdims->next) goto badd;
    kmax= rdims->number;
    if (kmax<2 || lmax<2) goto badd;
    dm->mesh.mesh.kmax= kmax;
    dm->mesh.mesh.lmax= lmax;

  } else {
    /* don't allow dimensions to change after first time */
    if (!rdims) goto badd;
    if (rdims->number!=lmax) goto badc;
    rdims= rdims->next;
    if (!rdims || rdims->next) goto badd;
    if (rdims->number!=kmax) goto badc;
  }

  /* check zt and ireg dimensions */
  if (!zdims || zdims->number!=lmax || !(zdims= zdims->next) ||
      zdims->number!=kmax || zdims->next ||
      (ireg &&
       (!idims || idims->number!=lmax || !(idims= idims->next) ||
        idims->number!=kmax || idims->next)))
    YError("dimensions of zt and ireg (if given) must match rt");

  /* argument list is good -- proceed */

  /* discard old rt, zt references */
  if (dm->mesh.mesh.z) {
    Array *old= Pointee(dm->mesh.mesh.z);
    dm->mesh.mesh.z= 0;
    Unref(old);
  }
  if (dm->mesh.mesh.r) {
    Array *old= Pointee(dm->mesh.mesh.r);
    dm->mesh.mesh.r= 0;
    Unref(old);
  }

  /* install new rt, zt -- the DratMesh owns a reference */
  array= Pointee(zt);
  dm->mesh.mesh.z= Ref(array)->value.d;
  array= Pointee(rt);
  dm->mesh.mesh.r= Ref(array)->value.d;

  /* update the rest of the FullMesh structure (boundaries, etc) */
  UpdateMesh(&dm->mesh, ireg);

  return;

  /* deliver rdims error messages */
 badd:
  YError("rt must be 2D with at least 2 points along each dimension");
 badc:
  YError("rt changed shape since previous update_mesh call");
}

/* The find_boundary function may produce temporary pointers which are
   not recorded elsewhere.  Guard against unexpected interrupts by
   putting these in static memory.  Then, whenever find_booundary is
   called, it first checks to see if the previous call was interrupted,
   orphaning these pointers.  */
static int liveBoundary= 0;  /* set when boundary is active */
static Boundary boundary;

void Y_find_boundary(int nArgs)
{
  DratMesh *dm;
  int region, sense;
  Array *array;

  if (liveBoundary) {
    liveBoundary= 0;
    EraseBoundary(&boundary);
  }

  if (nArgs<1 || nArgs>3)
    YError("update_mesh takes exactly one, two, or three arguments");

  dm= YGetDMesh(sp-nArgs+1, 0);

  if (nArgs>1) {
    if (YNotNil(sp-nArgs+2)) region= (int)YGetInteger(sp-nArgs+2);
    else region= 0;
    if (nArgs>2 && YNotNil(sp)) sense= (int)YGetInteger(sp);
    else sense= 0;
  } else {
    /* here are the values used in UpdateMesh call to FindBoundaryPoints */
    region= 0;
    sense= 1;
  }

  boundary.zsym= dm->mesh.boundary.zsym;
  if (region!=0 || sense!=1) {
    /* must calculate boundary now */
    boundary.nk= boundary.nl= boundary.npoints= 0;
    boundary.zone= 0;
    boundary.side= 0;
    boundary.z= boundary.r= 0;
    liveBoundary= 1;
    FindBoundaryPoints(&dm->mesh.mesh, region, sense, &boundary,
                       dm->mesh.work);

  } else {
    /* boundary already calculated */
    boundary.nk= dm->mesh.boundary.nk;
    boundary.nl= dm->mesh.boundary.nl;
    boundary.npoints= dm->mesh.boundary.npoints;
    boundary.zone= dm->mesh.boundary.zone;
    boundary.side= dm->mesh.boundary.side;
    boundary.z= dm->mesh.boundary.z;
    boundary.r= dm->mesh.boundary.r;
  }

  /* form result -- 1-origin array of 4 pointers */
  array=
    (Array *)PushDataBlock(NewArray(&pointerStruct,
                                    NewDimension(4L, 1L, (Dimension *)0)));
  array->type.dims->references--;
  if (boundary.npoints>0) {
    long npoints= boundary.npoints;
    long i;
    void **result= array->value.p;
    long *zone;
    int *side;
    double *z, *r;

    Dimension *dims= tmpDims;
    tmpDims= 0;
    FreeDimension(dims);
    tmpDims= NewDimension(npoints, 1L, (Dimension *)0);

    result[0]= zone= (NewArray(&longStruct, tmpDims))->value.l;
    result[1]= side= (NewArray(&intStruct, tmpDims))->value.i;
    result[2]= z= (NewArray(&doubleStruct, tmpDims))->value.d;
    result[3]= r= (NewArray(&doubleStruct, tmpDims))->value.d;

    /* convert zone to 1-origin index list */
    for (i=0 ; i<npoints ; i++) {
      zone[i]= boundary.zone[i]+1;
      side[i]= boundary.side[i];
      z[i]= boundary.z[i];
      r[i]= boundary.r[i];
    }
  }

  if (liveBoundary) EraseBoundary(&boundary);
}

/*--------------------------------------------------------------------------*/

/* The Ray_Path struct is duplicated from drat.i -- if you change it
   here, you must change it there as well.  */
typedef struct Ray_Path Ray_Path;
struct Ray_Path {
  void *zone;   /* list of zones cut by the ray */
  void *ds;     /* list of path lengths in above zones */
  double fi, ff;  /* fraction of 1st and last ds, respectively, within
                     the specified slimits */
  void *pt1, *pt2;  /* lists of endpoints of edges cut by ray */
  void *f;          /* list of fractions (f+0.5) is fraction of distance
                        from pt1 to pt2 where ray cuts edge */
};

/* set in Y__init_drat */
static StructDef *sdRay_Path= 0;

/* Again, try not to orphan any pointers on unexpected interrupts.  */
static RayPath path= {0,0, 0,0,0, 0,0};

void Y__raw_track(int nArgs)
{
  long nrays;
  Ray *rays;
  DratMesh *dm;
  double *slimits;
  Array *array;
  Ray_Path *result;
  long ncuts, i;
  long *zone, *pt1, *pt2;
  double *ds, *f;

  EraseRayPath(&path);

  if (nArgs!=4) YError("_raw_track takes exactly four arguments");

  /* no validity checking -- leave it to interpreted wrapper */
  nrays= YGetInteger(sp-3);
  rays= (Ray *)YGet_D(sp-2, 0, (Dimension **)0);
  dm= YGetDMesh(sp-1, 0);
  slimits= YGet_D(sp, 0, (Dimension **)0);

  /* push result array onto stack */
  array=
    (Array *)PushDataBlock(NewArray(sdRay_Path,
                                    NewDimension(nrays, 1L, (Dimension *)0)));
  array->type.dims->references--;
  result= (Ray_Path *)array->value.c;

  while (nrays>0) {
    TrackRay(&dm->mesh, rays, slimits, &path);
    result->fi= path.fi;
    result->ff= path.ff;
    ncuts= path.ncuts;
    if (ncuts>1) {
      Dimension *dims= tmpDims;
      tmpDims= 0;
      FreeDimension(dims);
      tmpDims= NewDimension(ncuts, 1L, (Dimension *)0);
      result->zone= zone= (NewArray(&longStruct, tmpDims))->value.l;
      result->ds= ds= (NewArray(&doubleStruct, tmpDims))->value.d;
      result->pt1= pt1= (NewArray(&longStruct, tmpDims))->value.l;
      result->pt2= pt2= (NewArray(&longStruct, tmpDims))->value.l;
      result->f= f= (NewArray(&doubleStruct, tmpDims))->value.d;
      /* convert zone, pt1, and pt2 to 1-origin index lists */
      for (i=0 ; i<ncuts ; i++) {
        zone[i]= path.zone[i]+1;
        ds[i]= path.ds[i];
        pt1[i]= path.pt1[i]+1;
        pt2[i]= path.pt2[i]+1;
        f[i]= path.f[i];
      }
    }

    rays++;
    slimits+= 2;
    result++;
    nrays--;
  }

  EraseRayPath(&path);
}

void Y_set_tolerances(int nArgs)
{
  double *tols, t1, t2, t3;
  Dimension *dims;
  Array *array;

  if (nArgs!=1) YError("set_tolerances takes exactly one argument");

  tols= YGet_D(sp, 1, &dims);
  if (tols && (!dims || dims->number!=3 || dims->next))
    YError("argument to set_tolerances must be nil or array(double,3)");

  /* get current tolerances */
  if (polishRoot) {
    t1= polishTol1;
    t2= polishTol2;
  } else {
    t1= t2= -1.0;
  }
  t3= findRayTol;

  if (tols) {
    /* set tolerances to new values */
    if (tols[0]>=0.0) {
      if (tols[0]>0.0) polishTol1= tols[0];
      else polishTol1= 1.0e-3;   /* default value as set in track.c */
      if (tols[1]>0.0) polishTol2= tols[1];
      else polishTol2= 1.0e-6;   /* default value as set in track.c */
      polishRoot= 1;
    } else {
      polishRoot= 0;
    }
    if (tols[2]>0.0) findRayTol= tols[2];
    else findRayTol= 0.0;   /* default value as set in track.c */
  }

  dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  tmpDims= NewDimension(3L, 1L, (Dimension *)0);
  array= (Array *)PushDataBlock(NewArray(&doubleStruct, tmpDims));
  tols= array->value.d;
  tols[0]= t1;
  tols[1]= t2;
  tols[2]= t3;
}

/*--------------------------------------------------------------------------*/
/* These routines could almost be made to work with the standardized
   Codger wrappers, but for the opaque mesh argument, which wants a
   call to YGetDMesh.  */

#undef D0
#define D0 (Dimension **)0

void Y__raw1_flat(int nArgs)
{
  if (nArgs!=9) YError("_raw1_flat takes exactly 9 arguments");
  IntegFlat(YGet_D(sp-8,0,D0), YGet_D(sp-7,0,D0), (long)YGetInteger(sp-6),
            (long)YGetInteger(sp-5), (Ray *)YGet_D(sp-4,0,D0),
            (long)YGetInteger(sp-3), &YGetDMesh(sp-2,0)->mesh,
            YGet_D(sp-1,0,D0), YGet_D(sp,0,D0));
}

void Y__raw1_linear(int nArgs)
{
  if (nArgs!=9) YError("_raw1_linear takes exactly 9 arguments");
  IntegLinear(YGet_D(sp-8,0,D0), YGet_D(sp-7,0,D0), (long)YGetInteger(sp-6),
              (long)YGetInteger(sp-5), (Ray *)YGet_D(sp-4,0,D0),
              (long)YGetInteger(sp-3), &YGetDMesh(sp-2,0)->mesh,
              YGet_D(sp-1,0,D0), YGet_D(sp,0,D0));
}

void Y__raw_pcens(int nArgs)
{
  if (nArgs!=7) YError("_raw_pcens takes exactly 7 arguments");
  DoPtCenter(YGet_D(sp-6,0,D0), YGet_D(sp-5,0,D0), (long)YGetInteger(sp-4),
             (long)YGetInteger(sp-3), &YGetDMesh(sp-2,0)->mesh,
             YGet_L(sp-1,0,D0), (long)YGetInteger(sp));
}

/*--------------------------------------------------------------------------*/

static void raw2_worker(int nArgs, int linear);

void Y__raw2_flat(int nArgs)
{
  raw2_worker(nArgs, 0);
}

void Y__raw2_linear(int nArgs)
{
  raw2_worker(nArgs, 1);
}

static void raw2_worker(int nArgs, int linear)
{
  double *opac, *source;
  long kxlm, ngroup;
  Operand op;
  Ray_Path *paths;
  long nrays;
  double *result;
  long ncuts, i;
  long *zone, *pt1, *pt2;
  double *ds, *f;

  EraseRayPath(&path);

  if (nArgs!=7)
    YError("_raw2_flat or _raw2_linear takes exactly 7 arguments");

  opac= YGet_D(sp-6, 0, (Dimension **)0);
  source= YGet_D(sp-5, 0, (Dimension **)0);
  kxlm= YGetInteger(sp-4);
  ngroup= YGetInteger(sp-3);
  if (!(sp-2)->ops) YError("unexpected keyword argument to _raw2_flat");
  (sp-2)->ops->FormOperand(sp-2, &op);
  if (!StructEqual(op.type.base, sdRay_Path))
    YError("rays must be an array of Ray_Path structs in _raw2_flat");
  paths= op.value;
  nrays= YGetInteger(sp-1);
  result= YGet_D(sp, 0, (Dimension **)0);

  while (--nrays) {
    zone= paths->zone;
    ds= paths->ds;
    pt1= paths->pt1;
    pt2= paths->pt2;
    f= paths->f;
    if (zone) {
      Array *array= Pointee(zone);
      ncuts= array->type.number;
    } else {
      ncuts= 0;
    }

    if (ncuts) {
      if (ncuts>path.maxcuts)
        ExtendRayPath(&path, 256*(1+(ncuts-path.maxcuts-1)/256));
      path.ncuts= ncuts;
      path.fi= paths->fi;
      path.ff= paths->ff;
      /* convert zone, pt1, and pt2 back to 0-origin index lists */
      for (i=0 ; i<ncuts ; i++) {
        path.zone[i]= zone[i]-1;
        path.ds[i]= ds[i];
        /* actually only use these if linear... */
        path.pt1[i]= pt1[i]-1;
        path.pt2[i]= pt2[i]-1;
        path.f[i]= f[i];
      }
      if (!linear)
        FlatSource(opac, source, kxlm, ngroup, &path,
                   result, result+ngroup, IntegWorkspace(ncuts));
      else
        LinearSource(opac, source, kxlm, ngroup, &path,
                     result, result+ngroup, IntegWorkspace(ncuts));
    }

    paths++;
    result+= 2*ngroup;
    nrays--;
  }

  EraseRayPath(&path);
  IntegClear();
}

/*--------------------------------------------------------------------------*/

void Y__get_msize(int nArgs)
{
  DratMesh *dm;
  if (nArgs!=1) YError("_get_msize takes exactly one argument");
  dm= YGetDMesh(sp, 0);
  PushLongValue(dm->mesh.mesh.klmax);
}

/*--------------------------------------------------------------------------*/

void Y__init_drat(int nArgs)
{
  /* be sure that Ray_Path structure in drat.i has been read and
     matches C struct defined in this file */
  if (!HashFind(&yStructTable, "Ray_Path", 0L))
    YError("(BUG) Ray_Path struct not found in _init_drat");
  sdRay_Path= yStructList[hashIndex];
  if (sdRay_Path->size != sizeof(Ray_Path)) {
    sdRay_Path= 0;
    YError("(BUG) Ray_Path wrong size in _init_drat");
  }
}

/*--------------------------------------------------------------------------*/
/* Define opaque mesh object DratMesh as a foreign Yorick data type.  */

extern PromoteOp PromXX;
extern UnaryOp ToAnyX, NegateX, ComplementX, NotX, TrueX;
extern BinaryOp AddX, SubtractX, MultiplyX, DivideX, ModuloX, PowerX;
extern BinaryOp EqualX, NotEqualX, GreaterX, GreaterEQX;
extern BinaryOp ShiftLX, ShiftRX, OrX, AndX, XorX;
extern BinaryOp AssignX, MatMultX;
extern UnaryOp EvalX, SetupX, PrintX;
extern MemberOp GetMemberX;

static UnaryOp PrintDM;

Operations meshOps = {
  &FreeDratMesh, T_OPAQUE, 0, T_STRING, "Drat-Mesh",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &EvalX, &SetupX, &GetMemberX, &MatMultX, &PrintDM
};

DratMesh *NewDratMesh(int zsym, long khold, long lhold)
{
  DratMesh *dm= (DratMesh *)p_malloc(sizeof(DratMesh));
  dm->references= 0;
  dm->ops= &meshOps;

  dm->mesh.mesh.kmax= dm->mesh.mesh.lmax= dm->mesh.mesh.klmax= 0;
  dm->mesh.mesh.z= dm->mesh.mesh.r= 0;
  dm->mesh.mesh.ireg= 0;
  dm->mesh.mesh.zsym= zsym;

  dm->mesh.boundary.zsym= -1;  /* see UpdateMesh (drat.h) */
  dm->mesh.boundary.nk= dm->mesh.boundary.nl= 0;
  dm->mesh.boundary.npoints= 0;
  dm->mesh.boundary.zone= 0;
  dm->mesh.boundary.side= 0;
  dm->mesh.boundary.z= dm->mesh.boundary.r= 0;

  dm->mesh.work= 0;

  dm->mesh.khold= khold;
  dm->mesh.lhold= lhold;

  return dm;
}

void FreeDratMesh(void *dmesh)
{
  DratMesh *dm= dmesh;
  double *z= dm->mesh.mesh.z;
  double *r= dm->mesh.mesh.r;
  DiscardMesh(&dm->mesh);
  if (z) {
    Array *array= Pointee(z);
    Unref(array);
  }
  if (r) {
    Array *array= Pointee(r);
    Unref(array);
  }
  p_free(dmesh);
}

static void PrintDM(Operand *op)
{
  DratMesh *dm= op->value;
  char line[96];
  int zsym= dm->mesh.mesh.zsym;
  if (zsym<0 || zsym>2) zsym= -1;
  ForceNewline();
  sprintf(line, "Drat mesh: %ld-by-%ld, zsym=%d",
          dm->mesh.mesh.kmax, dm->mesh.mesh.lmax, zsym);
  PrintFunc(line);
  ForceNewline();
}

DratMesh *YGetDMesh(Symbol *s, int nilOK)
{
  DratMesh *dm;
  if (s->ops==&referenceSym) ReplaceRef(s);
  if (s->ops!=&dataBlockSym || s->value.db->ops!=&meshOps)
    YError("expecting Drat-Mesh argument");
  dm= (DratMesh *)s->value.db;
  if (!nilOK && dm->mesh.mesh.kmax<2)
    YError("mesh has not yet been updated -- call update_mesh");
  return (DratMesh *)s->value.db;
}

/*--------------------------------------------------------------------------*/
