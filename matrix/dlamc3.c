/*
 * $Id: dlamc3.c,v 1.1 2005-09-18 22:04:48 dhmunro Exp $
 * LAPACK routine to add in non-optimized way -- compile this -g always.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "dg.h"

extern void dlamc3_worker( double a, double b );
extern double dlamc3_sum;

double dlamc3_sum= 0.0;

double   dlamc3( double a, double b )
{
  /**
   *  -- LAPACK auxiliary routine (version 1.1) --
   *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
   *     Courant Institute, Argonne National Lab, and Rice University
   *     October 31, 1992
   */
  /**     ..
   *
   *  Purpose
   *  =======
   *
   *  DLAMC3  is intended to force  A  and  B  to be stored prior to doing
   *  the addition of  A  and  B ,  for use in situations where optimizers
   *  might hold one of these in a register.
   *
   *  Arguments
   *  =========
   *
   *  A, B    (input) DOUBLE PRECISION
   *          The values A and B.
   *
   * =====================================================================
   *
   *     .. Executable Statements ..
   **/
  /*-----implicit-declarations-----*/
  /*-----end-of-declarations-----*/
  dlamc3_worker(a,b);

  return dlamc3_sum;
  /**
   *     End of DLAMC3
   **/
}
/**
************************************************************************
**/
