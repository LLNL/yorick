/*
 * $Id: paths.i,v 1.3 2009-10-19 04:37:51 dhmunro Exp $
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* Y_LAUNCH, Y_SITE, and Y_HOME are directories which yorick needs
 * to find to be completely functional.  The paths.i include file
 * may be edited to assist yorick in locating Y_SITE and Y_HOME.
 *
 *    Y_SITE       the directory containing the standard startup
 *                 files Yorick needs to be able to find.
 *    Y_LAUNCH     the directory containing the executable file
 *                 which is now running.  This may be Yorick itself,
 *                 or a custom version of Yorick.  On UNIX systems,
 *                 Yorick will not be fooled by an "executable"
 *                 which is a soft link to a file in another
 *                 directory; the directory containing the actual
 *                 executable file will be found.
 *    Y_HOME       the directory containing the tools required to
 *                 build custom versions of yorick.
 *
 * This file, paths.i, is the first file included.  Ordinarily, paths.i
 * will be in Y_SITE/i0, but if it is not (because the Y_SITE does not
 * exist or is not readable), may call the set_site function, which sets
 * the interpreted variables Y_SITE and Y_HOME, and adjusts the initial
 * include path for .i files accordingly.  The initial include path is:
 *
 *   ~/yorick:~/Yorick:Y_SITE/i:Y_SITE/contrib:Y_SITE/i0:Y_HOME/lib
 *
 * The paths.i file is the one exception to this include path; if paths.i
 * is not found anywhere else, yorick looks for Y_LAUNCH/paths.i.
 *
 * The to-be-installed values of Y_SITE and Y_HOME are stored into
 * the yorick executable at compile time.  At runtime, these directories
 * may not exist -- intentionally in the case of a relocatable yorick
 * installation -- in which case, yorick searches for them relative to
 * the Y_LAUNCH directory (which is always computed when yorick starts).
 * Yorick cannot start at all until it finds at least Y_SITE.
 *
 * ----------------------------------------------------------------------
 *
 * The Gist style sheets, palettes, and PostScript template must lie
 * in a place known to the Gist library routines in order for Yorick's
 * graphics functions to work.  If the Gist library was compiled with
 * an incorrect value of GISTPATH, set the variable GISTPATH to the
 * correct value below.
 *
 * The hcp_out command invokes the gist CGM browser and pipes its
 * PostScript output into lpr.  If the binaries have been ported, gist's
 * compiled-in GISTPATH may not be correct.  Furthermore, gist itself may
 * not be on the user's execution path.  You may be able to remedy these
 * defects by setting the variable GIST_FORMAT here.  The variable
 * LPR_FORMAT provides a similar service for PostScript files generated
 * directly by Yorick.
 */
/*= SECTION() file system paths =====================================*/

/* ------------------------------------------------------------------------ */

extern Y_SITE;
extern Y_HOME;
extern Y_LAUNCH;
/* DOCUMENT Y_LAUNCH       the directory containing the Yorick executable
            Y_SITE         Yorick's "site directory" (platform independent)
            Y_HOME         Yorick's "home directory" (platform dependent)
     Y_LAUNCH is set by compiled code when Yorick starts and should never
              be modified.
     Y_SITE is set to a default built-in value by compiled code at startup,
              but may be modified in the file Y_LAUNCH/paths.i to allow for
              ports of Yorick binary executables.
       contains subdirectories i0/, i/, i-start/, and others
     Y_HOME is set to a default built-in value by compiled code at startup,
              but may be modified in the file Y_LAUNCH/paths.i to allow for
              ports of Yorick binary executables.
       contains file Make.cfg, subdirectories i-start/, lib/, and others
     if the compiled-in value of Y_SITE is incorrect (i0/std.i missing),
     then both Y_SITE and Y_HOME are reset to Y_LAUNCH/.., which allows
     relocatable yorick distributions
 */

/* set_site, "Y_SITE_DIR", "Y_HOME_DIR"; */

/* Y_USER, Y_GISTDIR now set by set_site
 * Y_USER is ~/.yorick unless ~/yorick or (obsolete) ~/Yorick exists,
 *   or ~/Library/Yorick or ~/Application Data/Yorick for Mac or Windows
 * Y_GISTDIR is Y_USER/gist unless ~/.gist, ~/gist, or ~/Gist exists
 */

/* ------------------------------------------------------------------------ */

/* The first component of the GISTPATH should always be ~/Gist
   -- you need this line only if the value compiled into the Gist library
      libgist.a is incorrect.  */
/* GISTPATH= "~/Gist:"+"GIST_SITE_DIR"; */
GISTPATH = Y_GISTDIR+":"+Y_SITE+"g";

extern GIST_FORMAT, LPR_FORMAT;
/* DOCUMENT GIST_FORMAT
   NOTE: gist is deprecated; postscript output is now the default

     is used by the hcp_out function to generate the system call which
     invokes the gist CGM browser and pipes its output to lpr.  This
     format should contain a single %s specification; after this %s is
     replaced by the name of the CGM file, Yorick invokes the system
     command on the resulting string.

     LPR_FORMAT is also used by hcp_out to process PostScript files
     made directly by Yorick.

     The default values are:

        GIST_FORMAT= "gist %s -f | lpr";
        LPR_FORMAT= "lpr %s";

   SEE ALSO: hcp_out
 */

GIST_FORMAT= /*GIST_HOME_DIR*/ "gist %s -f | lpr";
LPR_FORMAT= "lpr %s";

/* Here is how to remedy (1) gist not being on the user's execution path
   and (2) gist being compiled at a different site with the wrong
   compiled-in GISTPATH:
GIST_FORMAT= "env GISTPATH=/env/Gist /usr/local/Gist/gist/gist %s -f | lpr";
 */

/* ------------------------------------------------------------------------ */
