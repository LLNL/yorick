/*
 * $Id: custommp.i,v 1.1 2010-02-28 21:49:21 dhmunro Exp $
 * default version mpy replacement for custom.i
 */
/* Copyright (c) 2010, David H. Munro
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* Place a copy of this file in your ~/yorick directory and add
 * any customizations here.  This script runs in serial mode on
 * rank 0 only.
 */

/* This should be the final line in your custommp.i file. */
command_line = mpy_process_argv();
