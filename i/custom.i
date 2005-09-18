/*
 * $Id: custom.i,v 1.1 2005-09-18 22:05:54 dhmunro Exp $
 * Default version of user customization file --
 * read after std.i and all package initalization files.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* With yorick-1.6, you should usually put customizations in
 * ~/yorick/i-start/ (or ~/Yorick/i-start/).  All files whose names
 * end in ".i" in that directory will be included at startup, in
 * alphabetical order.  Consider using autoload in any i-start files.
 * (see help,autoload and look in Y_SITE/i-start for examples.)
 * Do not have a ~/yorick/custom.i unless you need to change the
 * default command line processing.
 */

/* Place your own customizations here.

   Be careful!  You can break Yorick in a personalized way, so that only
   you will be affected -- this makes it difficult to get anyone else to
   believe there is a problem!

   Examples:

   // Read in my_special_functions.i (in ~/Yorick), which I always need.
   #include "my_special_functions.i"

   // Use ugly boxed graphics and waste screen real estate by default.
   pldefault, style="boxed.gs", dpi=100;

 */

/* This should be the final line in your custom.i file-- it implements
   the default command line processing (see help for process_argv).  */
command_line= process_argv();
