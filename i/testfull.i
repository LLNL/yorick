/*
 * $Id: testfull.i,v 1.1 2005-09-18 22:06:11 dhmunro Exp $
 * This file runs a full set of tests on the Macintosh version 
 * of yorick, although it should be also useful for any other version.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* run all tests in testp.i */
skip_testb= 0;
skip_test1= 0;
skip_test2= 0;
skip_test3= 0;

/* use the same pass count for all tests */
npass= 20;

/* run the parser test */
#include "testp.i"

/* Run the tests of the math library routines */
#include "testm.i"
testm;

/* Run the linpack benchmark */
#include "testlp.i"
testlp, 200;
write, "";

/* Run the tests of the graphics package */
include, "testg.i"
lissajous,0;
lissajous,1;
testg;

#include "demo2.i"
demo2;

grtest, quick=1;
txtest, quick=1;
if (batch()) winkill;
