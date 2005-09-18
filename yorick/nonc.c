/*
 * $Id: nonc.c,v 1.1 2005-09-18 22:04:08 dhmunro Exp $
 * Implement a few math functions not in the standard C library:
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*
    hypot   - optional hypotenuse for systems which don't have it

    sqrtZ   - square root of complex
    expZ    - exponential of complex
    logZ    - logarithm of complex
    powZZ   - complex raised to complex power

    powLL   - long raised to long power
    powDL   - double raised to long power
    powZL   - complex raised to long power

    sinZ, cosZ, tanZ, asinZ, acosZ, atanZ, sinhZ, coshZ, tanhZ
            - complex version of other math.h functions
    signZ   - closest point on unit circle (+1 for 0)

    $Id: nonc.c,v 1.1 2005-09-18 22:04:08 dhmunro Exp $
 */

/* NOTE: All complex function algorithms work when the destination
   array (z) is identical to the source array(s) (x).  */

/*--------------------------------------------------------------------------*/

#include <errno.h>

/* functions defined in this file */
extern void sqrtZ(double z[2], double x[2]);
extern void expZ(double z[2], double x[2]);
extern void logZ(double z[2], double x[2]);
extern void powZZ(double z[2], double x[2], double y[2]);
extern long powLL(long b, long p);
extern double powDL(double b, long p);
extern void powZL(double z[2], double b[2], long p);

extern void sinZ(double z[2], double x[2]);
extern void cosZ(double z[2], double x[2]);
extern void tanZ(double z[2], double x[2]);
extern void asinZ(double z[2], double x[2]);
extern void acosZ(double z[2], double x[2]);
extern void atanZ(double z[2], double x[2]);
extern void sinhZ(double z[2], double x[2]);
extern void coshZ(double z[2], double x[2]);
extern void tanhZ(double z[2], double x[2]);
extern void signZ(double z[2], double x[2]);

static void SquareZ(double z[2], double x[2]);
static void CubeZ(double z[2], double x[2]);
static void MultZ(double z[2], double x[2]);
static void RecipZ(double z[2]);
static void SideZ(double z[2], double x[2]);

/* definitely in standard math library */
extern double sqrt(double);
extern double exp(double);
extern double log(double);
extern double sin(double);
extern double cos(double);
extern double atan2(double, double);

extern double asin(double);
extern double acos(double);
extern double atan(double);
extern double sinh(double);
extern double cosh(double);
extern double tanh(double);
/* may be in standard math library, maybe not */
extern double hypot(double, double);

/*--------------------------------------------------------------------------*/

#ifdef NO_HYPOT
/* hypot(x,y) is sqrt(x*x+y*y)
   The problem is that without some care, the domain of x and y will be only
   half (square root) of the complete range of floating point numbers.  */
double hypot(double x, double y)
{
  if (x<0) x= -x;
  if (y<0) y= -y;
  if (x<y) x/= y;
  else if (x==0) return 0.0;
  else {
    register double tmp= x;
    x= y/x;
    y= tmp;
  }
  return y*sqrt(1.0+x*x);
}
#endif

/*--------------------------------------------------------------------------*/

void sqrtZ(double z[2], double x[2])
{
  register double x0= 0.5*x[0];
  register double x1= 0.5*x[1];
  register double r= x1? hypot(x0, x1) : (x0>0.0? x0 : -x0);
  if (x0 > 0.0) {
    z[0]= sqrt(r+x0);
    z[1]= x1/z[0];
  } else if ( (r-= x0) ) {
    z[1]= x1>=0.0? sqrt(r) : -sqrt(r);
    z[0]= x1/z[1];
  } else {
    z[0]= z[1]= 0.0;
  }
}

void expZ(double z[2], double x[2])
{
  register double r= exp(x[0]);
  if (errno) { if (errno==ERANGE && !r) errno=0; else return; }
  z[0]= r*cos(x[1]);
  z[1]= r*sin(x[1]);
}

void logZ(double z[2], double x[2])
{
  register double x0= x[0];
  z[0]= log(hypot(x0, x[1]));
  z[1]= atan2(x[1], x0);
}

void powZZ(double z[2], double x[2], double y[2])
{
  double logx[2], z0;
  logZ(logx, x);
  z0=   y[0]*logx[0] - y[1]*logx[1];
  z[1]= y[0]*logx[1] + y[1]*logx[0];
  z[0]= z0;
  expZ(z, z);
}

long powLL(long b, long p)
{
  if (p==0) return b? 1 : 1/powLL(0L,1L);
  else if (p==1) return b;
  else if (p==2) return b*b;
  else if (p==3) return b*b*b;
  else if (p>0) {
    register long c, q= (p&3);
    p>>= 2;
    if (p==1) c= b;
    else if (p==2) c= b*b;
    else if (p==3) c= b*b*b;
    else c= powLL(b, p);      /* recurse for large powers (p>15) */
    c*= c;
    if (q&2) c*= b;
    if (q&1) return c*c*b;
    else return c*c;
  } else /* (p<0) */ {
    if (b==1) return 1;
    else if (b==-1) return (p&1)? -1 : 1; /* assumes 2s complement */
    else return 0;  /* incorrect if b==0... */
  }
}

double powDL(double b, long p)
{
  if (p==0) return b? 1.0 : 1.0/powDL(0.0, 1L);
  else if (p==1) return b;
  else if (p==2) return b*b;
  else if (p==3) return b*b*b;
  else if (p>0) {
    register double c;
    register long q= (p&3);
    p>>= 2;
    if (p==1) c= b;
    else if (p==2) c= b*b;
    else if (p==3) c= b*b*b;
    else c= powDL(b, p);      /* recurse for large powers (p>15) */
    c*= c;
    if (q&2) c*= b;
    if (q&1) return c*c*b;
    else return c*c;
  } else /* (p<0) */ {
    if (p==-p) p+= 1;        /* p=1<<31 (or 63) is <0, but p==-p */
    return 1.0/powDL(b, -p);
  }
}

static void SquareZ(double z[2], double x[2])
{
  register double x0= x[0];
  z[0]= x0*x0 - x[1]*x[1];
  z[1]= 2.0*x0*x[1];
}

static void CubeZ(double z[2], double x[2])
{
  register double x0= x[0];
  z[0]= x0*x0*x0 - 3.0*x0*x[1]*x[1];
  z[1]= 3.0*x0*x0*x[1] - x[1]*x[1]*x[1];
}

static void MultZ(double z[2], double x[2])
{
  register double x0= x[0];
  register double z0= z[0];
  z[0]= x0*z0 - x[1]*z[1];
  z[1]= x[1]*z0 + x0*z[1];
}

static void RecipZ(double z[2])
{
  register double z0= z[0];
  register double z1= z[1];
  register double nrm;
  if ((z0>0?z0:-z0)>(z1>0?z1:-z1)) {
    nrm= z0;  z0= 1.0;  z1/= nrm;  nrm*= 1.0+z1*z1;
  } else {
    nrm= z1;  z0/= nrm;  z1= 1.0;  nrm*= 1.0+z0*z0;
  }
  z[0]= z0/nrm;
  z[1]= -z1/nrm;
}

void powZL(double z[2], double b[2], long p)
{
  if (p==0) { z[0]= (b[0]||b[1])? 1.0 : 1.0/powDL(0.0, 1L);  z[1]= 0.0; }
  else if (p==1) { z[0]= b[0];  z[1]= b[1]; }
  else if (p==2) { SquareZ(z, b); return; }
  else if (p==3) { CubeZ(z, b); return; }
  else if (p>0) {
    double c[2];
    register long q= (p&3);
    p>>= 2;
    if (p==1) { c[0]= b[0];  c[1]= b[1]; }
    else if (p==2) SquareZ(c, b);
    else if (p==3) { CubeZ(c, b); }
    else powZL(c, b, p);      /* recurse for large powers (p>15) */
    SquareZ(c, c);
    if (q&2) MultZ(c, b);
    SquareZ(c, c);
    if (q&1) MultZ(c, b);
    z[0]= c[0];
    z[1]= c[1];
  } else /* (p<0) */ {
    powZL(z, b, -p);
    RecipZ(z);
  }
}

/*--------------------------------------------------------------------------*/

void sinZ(double z[2], double x[2])
{
  register double sinx= sin(x[0]);
  register double cosx= cos(x[0]);
  z[0]= sinx*cosh(x[1]);
  z[1]= cosx*sinh(x[1]);
}

void cosZ(double z[2], double x[2])
{
  register double sinx= sin(x[0]);
  register double cosx= cos(x[0]);
  z[0]= cosx*cosh(x[1]);
  z[1]= -sinx*sinh(x[1]);
}

void tanZ(double z[2], double x[2])
{
  register double tanh2y= tanh(2*x[1]);
  register double sech2y= sqrt(1.0-tanh2y*tanh2y); /* cosh can overflow */
  register double denom= 1.0 + cos(2*x[0])*sech2y;
  z[0]= sin(2*x[0])*sech2y/denom;
  z[1]= tanh2y/denom;
}

void sinhZ(double z[2], double x[2])
{
  register double sinhx= sinh(x[0]);
  register double coshx= cosh(x[0]);
  z[0]= sinhx*cos(x[1]);
  z[1]= coshx*sin(x[1]);
}

void coshZ(double z[2], double x[2])
{
  register double sinhx= sinh(x[0]);
  register double coshx= cosh(x[0]);
  z[0]= coshx*cos(x[1]);
  z[1]= sinhx*sin(x[1]);
}

void tanhZ(double z[2], double x[2])
{
  register double tanh2x= tanh(2*x[0]);
  register double sech2x= sqrt(1.0-tanh2x*tanh2x); /* cosh can overflow */
  register double denom= 1.0 + cos(2*x[1])*sech2x;
  z[0]= tanh2x/denom;
  z[1]= sin(2*x[1])*sech2x/denom;
}

static void SideZ(double z[2], double x[2])
{
  register double x0= x[0];
  z[0]= 1.0 - (x0*x0 - x[1]*x[1]);
  z[1]= -2.0*x0*x[1];
  sqrtZ(z, z);    /* sqrt(1-x^2) */
}

void asinZ(double z[2], double x[2])
{
  double side[2];
  SideZ(side, x);
  side[0]-= x[1];
  side[1]+= x[0];
  logZ(side, side);
  z[0]= side[1];
  z[1]= -side[0];
}

void acosZ(double z[2], double x[2])
{
  register double tmp;
  double side[2];
  SideZ(side, x);
  tmp= side[0];
  side[0]= x[0]-side[1];
  side[1]= x[1]+tmp;
  logZ(side, side);
  z[0]= side[1];
  z[1]= -side[0];
}

void atanZ(double z[2], double x[2])
{
  double side[2];
  z[0]= -x[0];
  z[1]= 1.0-x[1];
  RecipZ(z);
  side[0]= x[0];
  side[1]= 1.0+x[1];
  MultZ(side, z);
  logZ(side, side);
  z[0]= -0.5*side[1];
  z[1]= 0.5*side[0];
}

void signZ(double z[2], double x[2])
{
  register double r= hypot(x[0], x[1]);
  if (r!=0.0) {
    z[0]/= r;
    z[1]/= r;
  } else {
    z[0]= 1.0;
    z[1]= 0.0;
  }
}

/*--------------------------------------------------------------------------*/
