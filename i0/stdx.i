/*
 * $Id: stdx.i,v 1.1.1.1 2005-09-18 22:05:25 dhmunro Exp $
 * Perform any post-initialization tasks.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*
    When Yorick starts, std.i is included, then any pkg.i files for
    compiled-in packages, then this file stdx.i, and finally
    the user customization file custom.i.
    For now, the only thing which must be done here is the critical job
    changing from the startup search path YORICK_PATH to the normal
    include file search path (which, among other things, allows custom.i
    to be found).  This path can be overridden from custom.i, which
    runs after this.
*/

set_path;   /* set compiled-in default include path */

/* with yorick-1.6, add the i-start directories
 * these are mostly intended for files containing autoloads,
 * but other initialization code could appear there as well
 * - optional packages should place autoload files in these directories
 *   (Y_SITE for interpreted only packages, Y_HOME for plugin packages)
 * - note that these all come before custom.i
 * - eventually, this mechanism may replace custom.i
 */
include_all, Y_SITE+"i-start";
if (Y_HOME!=Y_SITE) include_all, Y_HOME+"i-start";
if (!batch()) include_all, "~/yorick/i-start", "~/Yorick/i-start";
