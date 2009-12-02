/*
 * pathfun.i
 * $Id: pathfun.i,v 1.6 2009-12-02 05:20:19 dhmunro Exp $
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
  path = strpart(path, grow([0],i));
  if (delim == ":") {
    /* msdos drive letter repair */
    list = where(strlen(path) == 1);
    if (numberof(list)) {
      if (list(0) == numberof(path)) {
        if (numberof(list) < 2) return path;
        list = list(1:-1);
      }
      i = strcase(0, path(list));
      list = list(where((i>="a") & (i<="z")));
      if (numberof(list)) {
        path(list+1) = path(list) + ":" + path(list+1);
        mask = array(1n, numberof(path));
        mask(list) = 0;
        path = path(where(mask));
      }
    }
  }
  return path;
}

func add_y_home(y_home,y_site) {
/* DOCUMENT add_y_home, y_home, y_site;
         or add_y_home, y_both;

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
     y_site/i-start (unless we are not in batch mode and y_home or y_site is
     "~/yorick" or "~/Yorick", in which case this is done by i0/stdx.i).

     "add_y_home, y_both" is the same as setting y_site=y_home.
     
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
  // Y_USER/i-start is
  // include_all'ed by stdx.i unless in batch mode.
  // If it is defined as Y_HOMES or Y_SITES, we want it include_all'ed in
  // any case, but only once.

  // first put these three strings in a canonical form
  test_y_user=Y_USER;
  test_y_home=y_home;
  test_y_site=y_site;
  if (strpart(test_y_user,0:0)!="/") test_y_user += "/";
  if (strpart(test_y_user,1:2) == "~/") streplace,test_y_user,[0,2],get_home();
  if (strpart(test_y_home,1:2) == "~/") streplace,test_y_home,[0,2],get_home();
  if (strpart(test_y_site,1:2) == "~/") streplace,test_y_site,[0,2],get_home();
  if (batch() | test_y_home!=test_y_user)
    include_all, y_home+"i-start/";
  
  if ((y_site!=y_home) &
      (batch() | test_y_site!=test_y_user))
    include_all, y_site+"i-start/";

  // GISTPATH
  get_user_sys_path, pathsplit(set_gpath()), user_path, sys_path;
  set_gpath, pathform(grow(user_path, y_site+"g/", sys_path));
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

func find_in_path(filename,takefirst=,path=) {
/* DOCUMENT find_in_path(filename,takefirst=,path=)
     returns the full path (including filename) where filename has been found.

     Rules:
      - If filename has not been found, [] is returned
      - if filename has been found at several locations, a string vector
        with all locations is returned (unless takefirst is set, see below)

     If takefirst=1, will return when the first occurence of filename is
      found (returns a string scalar).
     If path= is set, it will be used instead of the default yorick path as
      returned by get_path(). Note that path must use the same syntax as
      the result of get_path(), i.e. a single string with ":" delimiters
      between the directories (and entries MUST end with a "/"), e.g.:
      path="/usr/local/share/yao/:/home/frigaut/Yorick/"
     
   SEE ALSO: get_path, dirname, basename
 */
  local lpath,valid;

  findpath = get_path();
  if (path!=[]) findpath=path;

  lpath = pathsplit(findpath,delim=":");
  valid = array(0,numberof(lpath));

  for (i=1;i<=numberof(lpath);i++) {
    if (open(lpath(i)+filename,"r",1)) valid(i)=1;
    if (takefirst&&valid(i)) return lpath(i)+filename;
  }
  if (noneof(valid)) return;
  return lpath(where(valid))+filename;
}
