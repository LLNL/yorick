/*
 * $Id: hack103.c,v 1.1 2005-09-18 22:05:45 dhmunro Exp $
 * fix bugs in sqrt, tanh, sinh shipped with MacOS X 10.3
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* note: asinh, acosh, and atanh have this same SIGFPE bug, but
 *       yorick does not reference them (they are not ANSI functions)
 */

extern double sqrt_hack(double);
extern double (*sqrt_phack)(double);
extern double sinh_hack(double);
extern double tanh_hack(double);

static double __sqrt_hack(double x);
static double sqrt0_hack(double x);
double (*sqrt_phack)(double) = sqrt0_hack;

/* is a true function sqrt_hack ever really necessary, or is
 * (*sqrt_phack) identical in every possible context in C?
 */
double
sqrt_hack(double x)
{
  return sqrt_phack(x);
}

/* the libm (/usr/lib/LibSystem.dylib) version of sqrt magically
 *  recognizes whether the processor has the fsqrt instruction
 *  (present on G5, absent on G4 and earlier CPUs), and arranges
 *  to use this hardware sqrt if present at runtime
 * in order to emulate this as closely as possible, we call
 *  sqrt0_hack the first time sqrt_hack (or sqrt_phack) is called;
 *  it either calls the hardware sqrt (which is not buggy, by
 *  definition), or the repaired software sqrt, __sqrt_hack
 * this cannot be quite as fast as the libm sqrt on a G5,
 *  but best we can do, and it should work for any hardware
 */
static double
sqrt0_hack(double x)
{
  extern double sqrt(double);
  union {
    double (*f)(double);
    int *i;
  } u;
  u.f = &sqrt;
  /* 0xfc20082c is "fsqrt f1,f1" instruction, want to use it */
  sqrt_phack = (u.i[0]==0xfc20082c)? u.f : __sqrt_hack;
  return sqrt_phack(x);
}


/*------------------------------------------------------------------------
 * original from www.opensource.apple.com/darwinsource/10.3
 * in Libm-47, ppc.subproj directory, files sqrt.c, shchth.c
 * changed function names, modified lines marked FIXED SIGFPE BUG
 * removed comments and inserted .h files
 *------------------------------------------------------------------------*/

/*
 * Copyright (c) 2002 Apple Computer, Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 *
 * Copyright (c) 1999-2003 Apple Computer, Inc.  All Rights Reserved.
 *
 * This file contains Original Code and/or Modifications of Original Code
 * as defined in and that are subject to the Apple Public Source License
 * Version 2.0 (the 'License'). You may not use this file except in
 * compliance with the License. Please obtain a copy of the License at
 * http://www.opensource.apple.com/apsl/ and read it before using this
 * file.
 *
 * The Original Code and all software distributed under the License are
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.
 * Please see the License for the specific language governing rights and
 * limitations under the License.
 *
 * @APPLE_LICENSE_HEADER_END@
 */

#ifdef      __APPLE_CC__
#if         __APPLE_CC__ > 930

/* ----------------------------------------------------------- from math.h */
extern double expm1(double);

/* --------------------------------------------------- from fenv_private.h */
#ifndef __FENV_PRIVATE__
#define      FEGETENVD(x)         asm volatile ("mffs %0" : "=f" (x));
#define      FESETENVD(x)         asm volatile ("mtfsf 255,%0" : : "f" (x));
#define      SET_INVALID          0x01000000
#endif

/* ------------------------------------------------- from ppc_intrinsics.h */
#ifndef _PPC_INTRINSICS_H_
static inline double
 __fmadd (double a, double c, double b) __attribute__((always_inline));
static inline double
__fmadd (double  a, double c, double b)
{
  double result;
  __asm__ ("fmadd %0, %1, %2, %3"
           /* outputs:  */ : "=f" (result)
           /* inputs:   */ : "f" (a), "f" (c), "f" (b));
  return result;
}
static inline double
 __fnmsub (double a, double c, double b) __attribute__((always_inline));
static inline double
__fnmsub (double  a, double c, double b)
{
  double result;
  __asm__ ("fnmsub %0, %1, %2, %3"
           /* outputs:  */ : "=f" (result)
           /* inputs:   */ : "f" (a), "f" (c), "f" (b));
  return result;
}
#endif

/* ----------------------------------------------------- from fp_private.h */
#ifndef __FP_PRIVATE__
double   fabs ( double x );
double   nan  ( const char *string );
#define __FMADD __fmadd
#define __FNMSUB __fnmsub
#define __FABS(x) fabs(x)

#define __ORI_NOOP \
({ \
    asm volatile ( "ori r0, r0, 0" ); /* NOOP */ \
})

#define __ENSURE(x, y, z) \
({ \
    double __value, __argx = (x), __argy = (y), __argz = (z); \
    asm volatile ("fmadd %0,%1,%2,%3" : "=f" (__value): "f" (__argx), "f" (__argy), "f" (__argz)); \
    __value; \
})

#define __PROD(x, y) \
({ \
    double __value, __argx = (x), __argy = (y); \
    asm volatile ("fmul %0,%1,%2" : "=f" (__value): "f" (__argx), "f" (__argy)); \
    __value; \
})
#define __PROG_INEXACT( x ) (void)__PROD( x, x )
#define __PROG_UF_INEXACT( x ) (void)__PROD( x, x )
#define __PROG_OF_INEXACT( x ) (void)__PROD( x, x )

typedef union {
       long int       lval;
       float          fval;
} hexsingle;

#if defined(__BIG_ENDIAN__)
  typedef union {
    struct {
      unsigned long hi;
      unsigned long lo;
    } i;
    double d;
  } hexdouble;
# define HEXDOUBLE(hi, lo) { { hi, lo } }

#elif defined(__LITTLE_ENDIAN__)
  typedef union {
    struct {
      unsigned long lo;
      unsigned long hi;
    } i;
    double d;
  } hexdouble;
# define HEXDOUBLE(hi, lo) { { lo, hi } }

#else
# error Unknown endianness
#endif

#endif
/* ------------------------------------------------------ end fp_private.h */

#define      twoTo512           1.34078079299425971e154
#define      twoToMinus256      8.636168555094444625e-78
#define      upHalfOfAnULP      0.500000000000000111

#define      SQRT_NAN           "1"

__private_extern__
const unsigned short SqrtTable[256] = {
  27497,27752,28262,28517,28772,29282,29537,29792,30302,30557,31067,31322,
  31577,32088,32343,32598,33108,33363,33618,34128,34384,34639,35149,35404,
  35659,35914,36425,36680,36935,37446,37701,37956,38211,38722,38977,39232,
  39487,39998,40253,40508,40763,41274,41529,41784,42040,42550,42805,43061,
  43316,43571,44082,44337,44592,44848,45103,45358,45869,46124,46379,46635,
  46890,47401,47656,47911,48167,48422,48677,48933,49443,49699,49954,50209,
  50465,50720,50976,51231,51742,51997,52252,52508,52763,53019,53274,53529,
  53785,54296,54551,54806,55062,55317,55573,55828,56083,56339,56594,56850,
  57105,57616,57871,58127,58382,58638,58893,59149,59404,59660,59915,60170,
  60426,60681,60937,61192,61448,61703,61959,62214,62470,62725,62981,63236,
  63492,63747,64003,64258,64514,64769,65025,65280,511,  510,  764,  1018,
  1272, 1526, 1780, 2034, 2288, 2542, 2796, 3050, 3305, 3559, 3813, 4067,
  4321, 4576, 4830, 5084, 5338, 5593, 5847, 6101, 6101, 6356, 6610, 6864,
  7119, 7373, 7627, 7882, 8136, 8391, 8391, 8645, 8899, 9154, 9408, 9663,
  9917, 10172,10172,10426,10681,10935,11190,11444,11699,11699,11954,12208,
  12463,12717,12972,13226,13226,13481,13736,13990,14245,14245,14500,14754,
  15009,15264,15518,15518,15773,16028,16282,16537,16537,16792,17047,17301,
  17556,17556,17811,18066,18320,18575,18575,18830,19085,19339,19339,19594,
  19849,20104,20104,20359,20614,20868,21123,21123,21378,21633,21888,21888,
  22143,22398,22653,22653,22907,23162,23417,23417,23672,23927,23927,24182,
  24437,24692,24692,24947,25202,25457,25457,25712,25967,25967,26222,26477,
  26732,26732,26987,27242 };

static const hexdouble Two911   = HEXDOUBLE( 0x07000000, 0x00000000 );
static const hexdouble infinity = HEXDOUBLE( 0x7ff00000, 0x00000000 );
static const double twoTo128    = 0.340282366920938463463e39;
static const double twoToM128   = 0.293873587705571876992e-38;

static double
__sqrt_hack(double x)
{
  register int index;
  hexdouble xInHex, yInHex, gInHex;
  register double OldEnvironment, g, y, y2, d, e;
  register unsigned long int xhead, ghead, yhead;

  register double FPR_z, FPR_Two911, FPR_TwoM128, FPR_Two128,
    FPR_inf, FPR_HalfULP;

  xInHex.d = x;					FPR_z = 0.0;
  FPR_inf = infinity.d;				FPR_Two911 = Two911.d;
  FPR_TwoM128 = twoToM128;			FPR_Two128 = twoTo128;
  gInHex.i.lo = 0UL;				yInHex.i.lo = 0UL;
  FPR_HalfULP = upHalfOfAnULP;

  FEGETENVD( OldEnvironment );
  FESETENVD( FPR_z );    /* FIXED SIGFPE BUG */

  __ORI_NOOP;
  __ENSURE( FPR_z, FPR_inf, FPR_Two911 );
  __ENSURE( FPR_TwoM128, FPR_Two128, FPR_HalfULP );

  FESETENVD( FPR_z );

  __ORI_NOOP;
  if (  FPR_TwoM128 < x &&  x < FPR_Two128 ) {
    hexsingle GInHex, YInHex;
    register unsigned long GPR_t, GPR_foo;

    xhead = xInHex.i.hi;
    index = ( xhead >> 13 ) & 0xffUL;
    GPR_t = SqrtTable[index];

    ghead = ( ( xhead + 0x07f00000 + 0x07f00000 - 0x3ff00000 ) << 2 )
      & 0x7f800000;
    GInHex.lval = ghead + ( ( 0xff00UL & GPR_t ) << 7 );

    asm volatile ( "add %0, %1, %2" : "=r" (GPR_foo) : "b" (&GInHex), "b" (&YInHex) : "memory" );
    g = GInHex.fval;

    yhead = 0x7e000000UL - ghead;
    YInHex.lval = yhead + ( ( 0xffUL & GPR_t ) << 15 );

    asm volatile ( "add %0, %1, %2" : "=r" (GPR_foo) : "b" (&GInHex), "b" (&YInHex) : "memory" );
    __ORI_NOOP;
    __ORI_NOOP;
    y = YInHex.fval;

    d = __FNMSUB( g, g, x );		y2 = y + y;
    g = __FMADD(y, d, g );
    e = __FNMSUB( y, g, FPR_HalfULP );	d = __FNMSUB( g, g, x );
    y = __FMADD( e, y2, y );
    y2 = y + y;				g = __FMADD(y, d, g);
    e = __FNMSUB( y, g, FPR_HalfULP );	d = __FNMSUB( g, g, x );
    y = __FMADD( e, y2, y );
    y2 = y + y;                  		g = __FMADD(y, d, g );
    e = __FNMSUB( y, g, FPR_HalfULP );	d = __FNMSUB( g, g, x );
    y = __FMADD( e, y2, y );

    FESETENVD( OldEnvironment );

    __ORI_NOOP;
    return (  __FMADD( y, d, g ) );
  }

  if ( FPR_z < x && x < FPR_inf ) {
    if ( FPR_Two911 < x ) {
      register unsigned long GPR_t;

      xhead = xInHex.i.hi;
      index = ( xhead >> 13 ) & 0xffUL;
      GPR_t = SqrtTable[index];

      ghead = ( ( xhead + 0x3ff00000 ) >> 1 ) & 0x7ff00000;
      yhead = 0x7fc00000UL - ghead;

      gInHex.i.hi = ghead + ( ( 0xff00UL & GPR_t ) << 4 );
      g = gInHex.d;

      yInHex.i.hi = yhead + ( ( 0xffUL & GPR_t ) << 12 );
      y = yInHex.d;

      d = __FNMSUB( g, g, x );			y2 = y + y;
      g = __FMADD(y, d, g );
      e = __FNMSUB( y, g, FPR_HalfULP );	d = __FNMSUB( g, g, x );

      y = __FMADD( e, y2, y );
      y2 = y + y;				g = __FMADD(y, d, g);
      e = __FNMSUB( y, g, FPR_HalfULP );	d = __FNMSUB( g, g, x );
      y = __FMADD( e, y2, y );
      y2 = y + y;                  		g = __FMADD(y, d, g );
      e = __FNMSUB( y, g, FPR_HalfULP );	d = __FNMSUB( g, g, x );
      y = __FMADD( e, y2, y );

      FESETENVD( OldEnvironment );
      return ( __FMADD( y, d, g ) );

    } else {
      xInHex.d = x * twoTo512;
      xhead = xInHex.i.hi;

      ghead = ( ( xhead + 0x3ff00000 ) >> 1) & 0x7ff00000;
      index = ( xhead >> 13) & 0xffUL; // table index
      yhead = 0x7fc00000UL - ghead;
      gInHex.i.hi = ghead + ( ( 0xff00UL & SqrtTable[index] ) << 4 );
      yInHex.i.hi = yhead + ( ( 0xffUL & SqrtTable[index] ) << 12 );
      x = xInHex.d;
      g = gInHex.d;
      y = yInHex.d;

      d = __FNMSUB( g, g, x );			y2 = y + y;
      g = __FMADD(y, d, g );
      e = __FNMSUB( y, g, FPR_HalfULP );	d = __FNMSUB( g, g, x );
      y = __FMADD( e, y2, y );
      y2 = y + y;				g = __FMADD(y, d, g);
      e = __FNMSUB( y, g, FPR_HalfULP );	d = __FNMSUB( g, g, x );
      y = __FMADD( e, y2, y );
      y2 = y + y;                  		g = __FMADD(y, d, g );
      e = __FNMSUB( y, g, FPR_HalfULP );	d = __FNMSUB( g, g, x );
      d *= twoToMinus256;			g *= twoToMinus256;
      y = __FMADD( e, y2, y );

      FESETENVD( OldEnvironment );
      return ( __FMADD( y, d, g ) );
    }
  }

  if ( x < FPR_z && x == x  ) {
    hexdouble env;
    x = nan ( SQRT_NAN );
    env.d = OldEnvironment;
    env.i.lo |= SET_INVALID;
    FESETENVD( env.d );
    return ( x );
  } else {
    /* FIXED SIGFPE BUG -- moved line to avoid restoring twice above */
    FESETENVD( OldEnvironment );
    return ( x );
  }
}

static const hexdouble SqrtNegEps = HEXDOUBLE(0x3e400000, 0x00000000);
static const hexdouble Huge       = HEXDOUBLE(0x7ff00000, 0x00000000);
static const double kMinNormal = 2.2250738585072014e-308;


static const hexdouble Log2        = HEXDOUBLE(0x3FE62E42, 0xFEFA39EF);
static const double kMaxNormal = 1.7976931348623157e308;

double
sinh_hack(double x)
{
  register double PositiveX;

  register double result, FPR_env, FPR_z, FPR_kMinNormal, FPR_half, FPR_one,
    FPR_ln2, FPR_sqreps, FPR_kMaxNormal, FPR_inf;

  PositiveX = __FABS ( x );
  FPR_z = 0.0;				FPR_half = 0.5;
  FPR_one = 1.0;			FPR_sqreps = SqrtNegEps.d;
  FPR_inf = Huge.d;			FPR_kMinNormal = kMinNormal;
  FPR_ln2 = Log2.d;			FPR_kMaxNormal = kMaxNormal;

  FEGETENVD ( FPR_env);
  FESETENVD ( FPR_z );    /* FIXED SIGFPE BUG */
  __ENSURE( FPR_z, FPR_one, FPR_inf );
  __ENSURE( FPR_half, FPR_sqreps, FPR_kMinNormal );
  __ENSURE( FPR_z, FPR_kMaxNormal, FPR_ln2 );
  FESETENVD ( FPR_z );

  if ( PositiveX > FPR_sqreps ) {
    result = expm1 ( PositiveX );
    if ( result != FPR_inf )
      result = FPR_half * ( result + result / ( FPR_one + result ) );
  } else {
    result = PositiveX;
  }

  FESETENVD ( FPR_env );

  if ( result != result)
    ; /* NOTHING */
  else if ( result == FPR_z )
    result = x;
  else if ( result < FPR_kMinNormal )
    __PROG_UF_INEXACT( FPR_kMinNormal );
  else if ( result < FPR_inf )
    __PROG_INEXACT( FPR_ln2 );
  else if ( PositiveX < FPR_inf )
    __PROG_OF_INEXACT( FPR_kMaxNormal );

  if ( x < FPR_z)
    result = -result;

  return result;
}

double
tanh_hack(double x)
{
  register double PositiveX;

  register double result, FPR_env, FPR_z, FPR_kMinNormal, FPR_two, FPR_negTwo,
    FPR_ln2, FPR_sqreps, FPR_kMaxNormal, FPR_inf, FPR_t;

  PositiveX = __FABS ( x );
  FPR_z = 0.0;				FPR_inf = Huge.d;
  FPR_two = 2.0;			FPR_negTwo = -2.0;
  FPR_sqreps = SqrtNegEps.d;		FPR_kMinNormal = kMinNormal;
  FPR_ln2 = Log2.d;			FPR_kMaxNormal = kMaxNormal;

  if ( PositiveX == FPR_inf )
    return (x >= FPR_z ? 1.0 : -1.0);

  FEGETENVD ( FPR_env );
  FESETENVD ( FPR_z );    /* FIXED SIGFPE BUG */
  __ENSURE( FPR_z, FPR_inf, FPR_two );
  __ENSURE( FPR_negTwo, FPR_sqreps, FPR_kMinNormal );
  __ENSURE( FPR_z, FPR_kMaxNormal, FPR_ln2 );
  FESETENVD ( FPR_z );

  if ( PositiveX > FPR_sqreps) {
    FPR_t = expm1 ( FPR_negTwo * PositiveX ); /* call exp1 once   */
    result = - FPR_t / ( FPR_two + FPR_t );
  } else {
    result = PositiveX;
  }

  FESETENVD ( FPR_env );

  if ( result != result)
    ; /* NOTHING */
  else if ( result == FPR_z )
    result = x;
  else if ( result < FPR_kMinNormal )
    __PROG_UF_INEXACT( FPR_kMinNormal );
  else if ( result < FPR_inf )
    __PROG_INEXACT( FPR_ln2 );
  else if ( PositiveX < FPR_inf )
    __PROG_OF_INEXACT( FPR_kMaxNormal );

  if ( x < FPR_z)
    result = -result;

  return result;
}

#else       /* __APPLE_CC__ version */
#warning A higher version than gcc-932 is required.
#endif      /* __APPLE_CC__ version */
#endif      /* __APPLE_CC__ */
