/*
 * $Id: custommp.i,v 1.2 2011-02-11 05:25:42 dhmunro Exp $
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
command_line = mp_size? mpy_process_argv() : process_argv();
