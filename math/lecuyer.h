/*
 * $Id: lecuyer.h,v 1.1 2005-09-18 22:03:44 dhmunro Exp $
 * random number generator interface
 * L'Ecuyer, Mathematics of Computation, 65, pp 203-213 (96)
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* __cplusplus is for version 2.0, c_plusplus for version 1.2 */
#ifdef __cplusplus
extern "C" {
#endif

/* get random numbers between 0. and 1. one or n at a time */
extern double le_random(unsigned long *generator);
extern void le_nrandom(unsigned long *generator, long n, double *r);

/* get underlying random integer between 1 and 2^32-1 (4,294,967,295) */
extern unsigned long le_next(unsigned long *generator);

/* seed the sequence with either double or long
 * -- 0 seed means reinitialize to default sequence
 * -- note that the complete state of the generator requires
 *    three numbers, not one, so "seeding" can't reproduce
 *    an arbitrary state -- copy the generator to do that */
extern void le_rseed(unsigned long *generator, double seed);
extern void le_iseed(unsigned long *generator, unsigned long seed);

/* above can all take generator==0, in which case, they use this one
 * -- if you create your own generator, none of the three values
 *    can be 0; best to call one of the seed routines to initialize */
extern unsigned long le_generator[3];

#ifdef __cplusplus
}
#endif
