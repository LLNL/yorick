/*
 * pathfun.i
 * $Id: pathfun.i,v 1.1 2006-05-12 03:35:48 dhmunro Exp $
 * manipulate path names and file names
 */
/* Copyright (c) 2006, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func dirname(path)
/* DOCUMENT dir = dirname(pathname)
     returns the directory part of PATHNAME (like GNU dirname), which
     can be an array of names

     Examples:
       pathname         dirname(pathname)
       /                /
       /path/to/file    /path/to
       /path/to/dir/    /path/to
       path/to/file     path/to
       file             .
       c:               c:
       c:/              c:

   SEE ALSO: basename
 */
{
  path = strtrim(path, 2, blank="/");
  dos = strglob("[A-Za-z]:", path);  /* DOS drive letter */
  root = !strlen(path);
  list = strfind("/", path, back=1)(2:1:-1,..);
  c = list(1,..);
  list(1,..) = 0;
  if (anyof(dos)) list(2,where(dos)) = 2;
  path = strpart(path, list);
  list = where(root | (c==1));
  if (numberof(list)) path(list) = "/";
  root |= dos;
  list = where((!root) & (c==-1));
  if (numberof(list)) path(list) = ".";
  return path;
}

func basename(path)
/* DOCUMENT file = basename(pathname)
     returns the file part of PATHNAME (like GNU basename), which
     can be an array of names

     Examples:
       pathname         basename(pathname)
       /                /
       /path/to/file    file
       /path/to/dir/    dir
       path/to/file     file
       file             file
       c:               c:
       c:/              c:

   SEE ALSO: dirname
 */
{
  path = strtrim(path, 2, blank="/");
  list = max(strfind("/", path, back=1)(2:1:-1,..), 0);
  list(2,..) = strlen(path);
  path = strpart(path, list);
  list = where(!strlen(path));
  if (numberof(list)) path(list) = "/";
  return path;
}

func pathform(list, delim=)
/* DOCUMENT path = pathform(list)
     combine a LIST (that is, an array) of strings into a single colon
     delimited path.  For example,
       pathform(["one","two","three"]) --> "one:two:three"
     The delim= keyword can be used to specify an alternative separator
     for the components of the path, delim=":" is the default.

   SEE ALSO: pathsplit, set_path, get_path, plug_dir
 */
{
  if (is_void(delim)) delim = ":";
  if (numberof(list) <= 1) return numberof(list)? list(1) : "";
  d = array(string, dimsof(list));
  d(1:numberof(list)-1) = delim;
  return sum(list+d);
}

func pathsplit(path, delim=)
/* DOCUMENT list = pathsplit(path)
     split a colon delimited PATH into a list of strings.  For example,
       pathsplit("one:two:three") --> ["one","two","three"]
     The delim= keyword can be used to specify an alternative separator
     for the components of the path, delim=":" is the default.

   SEE ALSO: pathform, set_path, get_path, plug_dir
 */
{
  if (is_void(delim)) delim = ":";
  path = path(1);
  i = strfind(delim, path, n=1024);
  i = i(1:min(where(i<0))-1);  /* blow up if more than 1023 items */
  if (numberof(i) <= 1) return [path];
  return strpart(path, grow([0],i));
}

func add_y_home_y_site(y_home,y_site) {
/* DOCUMENT add_y_home_y_site, y_home, y_site;
         or add_y_home_y_site, y_both;

     Set all Yorick paths to take into account y_home as a new (additional)
     root for architecture-dependent files and y_site as the corresponding
     root for architecture-independent files.  Note that separating
     architecture-independent and architecture-dependent files is a
     deprecated practice.  If you work on two architectures, maintain two
     copies of y_home and y_site and keep your sanity.

     The paths taken care of are:
       - the main path for .i files (see set_path(), get_path();
       - the plug_dir path for compiled add-ons (see plug_dir());
       - GISTPATH (.gp and .gs files).
       
     In addition, we perform an include_all (which see) on y_home/i-start and
     y_site/i-start (unless we are not in batch mode and y_home or is
     "~/yorick" or "~/Yorick", in which case this is done by i0/stdx.i).

     "add_y_home_y_site, y_both" is the same as setting y_site=y_home.
     
     See pkg_mngr.i for more.
 */
  if (!y_home) return;
  if (!y_site) y_site=y_home;
  extern Y_SITES;
  extern Y_HOMES;
  extern GISTPATH;
  local user_path, sys_path;

  if (strpart(y_home,0:0)!="/") y_home += "/";
  if (strpart(y_site,0:0)!="/") y_site += "/";

  // Keep track of what we're doing. Used by the package manager
  Y_HOMES = grow(y_home, Y_HOMES);
  Y_SITES = grow(y_site, Y_SITES);

  // path for interpreted routines (.i)
  i_dirs = grow(y_site+["i/","contrib/","i0/"], y_home+"lib/");
  get_user_sys_path, pathsplit(get_path()), user_path, sys_path;
  set_path, pathform(grow(user_path, i_dirs, sys_path));

  // path for compiled plugins
  plug_dirs = [y_home+"lib/"];
  get_user_sys_path, plug_dir(), user_path, sys_path;
  plug_dir, grow(user_path, plug_dirs, sys_path);
  // plug_dir,Y_HOMES+"lib/";

  // include_all part
  // "~/.yorick", "~/yorick/i-start/" and "~/Yorick/i-start/" are
  // include_all'ed by stdx.i unless in batch mode.
  // If they are defined as Y_HOMES or Y_SITES, we want them include_all'ed in
  // any case, but only once.
  if (batch() |
      noneof(y_home==grow(get_home()+[".y","y","Y"]+"orick/",
                          "~/"+[".y","y","Y"]+"orick/")))
    include_all, y_home+"i-start/";
  if ((y_site!=y_home) &
      (batch() |
      noneof(y_home==grow(get_home()+[".y","y","Y"]+"orick/",
                          "~/"+[".y","y","Y"]+"orick/"))))
    include_all, y_site+"i-start/";

  // GISTPATH
  get_user_sys_path, pathsplit(GISTPATH), user_path, sys_path;
  GISTPATH = pathform(grow(user_path, y_site+"g/", sys_path));
}

func get_user_sys_path(path, &user_path, &sys_path) {
/* xxDOCUMENT get_user_sys_path, path, user_path, sys_path

    Splits an array of strings PATH into two subarrays such that
    PATH==grow(USER_PATH,SYS_PATH). This is intended for splitting a path into
    its "user" and "system" parts.

    We first try to determine the user part by looking for matches of either
    get_env("HOME") or "~" in each element of the array. The first element
    that does not match is considered to be the beginning of the system part.

    If all elements match, we assume that Yorick is installed in the user's
    directory. In that case, will try to match each element against Y_HOME or
    Y_SITE. The last element that does not match is consider as the last
    element of the user part.

    This function is not intended to be used outside of the script in which it
    is defined.
 */
  user_path = sys_path = [];
  if (!numberof(path)) return;
  induser = where(strmatch(path, strtrim(get_home(),2,blank="/")) |
                  strmatch(path, "~"));
  if (numberof(induser)==numberof(path)) {
    // Yorick seems to be installed in user directory
    induser = where(!strmatch(path,Y_SITE) & !strmatch(path,Y_HOME));
  }
  if (numberof(induser)) {
    indusermax = max(induser);
    user_path = path(1:indusermax);
    if (indusermax < numberof(path)) sys_path = path(indusermax+1:);
  } else {
    user_path = [];
    sys_path = path;
  }
}
