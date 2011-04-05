/*
 * $Id: make.i,v 1.2 2006-04-09 01:05:45 dhmunro Exp $
 * create or update a Makefile for a yorick package (compiled extension)
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func make(path, template=)
/* DOCUMENT make
 *       or make, path
 *       or make, path, template=1
 *       or make, path, template="Makefile_name"
 *  creates or updates the Makefile for a compiled package (plugin).
 *  If PATH is given, it is the directory containing the source code
 *  for the package; by default, PATH is ".", the current working
 *  directory.
 *
 *  With the template= keyword, a template Makefile is written, which
 *  you can use as a starting point, filling in all the necessary
 *  information by hand.  In this case, make makes no further checks
 *  on the contents of the directory.  In the first form, the name of
 *  the Makefile template is "Makefile", but if the template= keyword
 *  is a string, that name is used instead.  The template Makefile
 *  will not function at all until you edit it.
 *
 *  Alternatively, without the template= keyword, make looks at the
 *  contents of the directory, and fills in as much of the Makefile
 *  as it can using what it sees.  There are two cases:
 *  1. A Makefile (or makefile) already exists.  Provided the file
 *     contains a Y_MAKEDIR= line, make merely updates Y_MAKEDIR,
 *     Y_EXE, and Y_EXE_PKGS to reflect this running yorick, and
 *     attempts no other modifications of the existing Makefile.
 *  2. No such Makefile exists.  Then make writes a template Makefile,
 *     and reads all the .i interpreted source files, .c and .h C source
 *     files, and .f or .F or .m Fortran source files in the directory,
 *     filling in as much of the Makefile as possible.  In simple cases,
 *     the resulting Makefile will be perfectly adequate.  (If there
 *     is any Fortran source, however, it will never be adequate.)
 *
 *  In case 2, the following steps are taken:
 *  A. Any .i files containing lines of the form
 *      plug_in, "pkname"
 *    or
 *      if (!is_void(plug_in)) plug_in, "pkgname"
 *    are identified as the "package include file(s)" which define
 *    the interpreted API for the package.  The make function takes
 *    the name of the package from the "pkgname" in these lines.
 *    At least one .i file must contain at least one plug_in line;
 *    at most one "pkgname" must occur in all the plug_in lines in
 *    all the .i files in the directory.  This information is used
 *    to fill in the PKG_NAME macro and the PKG_I macro in the
 *    Makefile (the latter becomes a list of every .i file containing
 *    a plug_in statement).
 *  B. All .c, .f, .F, or .m files are assumed to be required compiled
 *    source, and the OBJS macro in the Makefile is set to a list of
 *    all corresponding .o object files.
 *  C. If there are any .h C header files in the directory, every .c
 *    C source file is scanned for an include directive which reads
 *    the header.  An approriate dependency line for each .o file which
 *    is discovered to depend on local .h files in this manner.
 *  These steps will be sufficient to produce a finished Makefile in
 *  many simple cases; an example is given in the extend/ directory of
 *  the yorick distribution (a compiled complementary error function).
 *
 *  These same steps will be attempted for C++ source code.  The gcc
 *  compiler recognizes the file extensions .cc, .cp, .cxx, .cpp, .c++,
 *  .C, and .CPP as C++ source code, and so does this make function.
 *  Similarly, .hh and .H are recognized as C++ header files, in addition
 *  to .h.  Microsoft Visual C++ recognizes only the .cpp and .cxx
 *  extensions, and several other popular Windows compilers recognize
 *  only .cpp (which is the extension used by Trolltech's Qt library).
 *  All of this is by way of warning you to be prepared for portability
 *  problems if you attempt to use C++.  The make function will emit
 *  rules for building C++ objects (cribbed from gmake); see the comments
 *  at that point in the Makefile if these don't work for you.  Be sure
 *  that any C++ compiled functions which will be called by yorick are
 *  declared as extern "C" in your C++ source.
 *
 *  However, it is impossible for yorick to automatically discover
 *  complicated Makefile rules your package may require.  The most
 *  common example is dependency on a third party library, which
 *  requires a -l loader option, and very possibly a -L option as
 *  well.  You need to add these flags to PKG_DEPLIBS in the Makefile
 *  by hand.  If you want such a package to be buildable by non-experts,
 *  you will need to provide a configure script in order to fill in
 *  the required information across a wide variety of platforms.  This
 *  can be exceptionally challenging, which is why yorick itself has
 *  no dependencies beyond what is guaranteed to be present on every
 *  system where it can run (libc, libm, and, on Unix systems, libX11).
 *
 *  Once the Makefile has been built it becomes part of the source code
 *  of your package, and should not be removed.  (You may want it to
 *  have an include directive to get the results of any configure script
 *  which must be run, however.)  Thereafter, use
 *    yorick -batch make.i
 *    (or simply make in a running yorick)
 *  to set Y_MAKEDIR, Y_EXE, and Y_EXE_PKGS appropriately for the
 *  particular platform where you are building (case 1 above).
 *
 *  Read Y_HOME/Makepkg for a description of the various make targets
 *  available.  In a nutshell:
 *    make debug          builds a debugging version (never a plugin)
 *    make                builds a release version (a plugin if possible)
 *    make install        installs the release version (may require root)
 *    make install2       installs to relocate subdirectory
 *    make clean          removes all results, leaving only original source
 *
 * SEE ALSO: write_home, plug_in, autoload, cd
 */
{
  if (!path) path = ".";
  else if (strglob("*/", path)) path = strpart(path, 1:-1);
  files = lsdir(path);
  path = path + "/";

  mkfile = make_file;
  make_subst, mkfile;
  if (!is_void(template) && template) {
    if (structof(template)==string && strlen(template)) name = template;
    else name = "Makefile";
    if (anyof(files == name)) {
      for (i=0 ; ; i++) {
        bakname = i? swrite(format=name+"-%ld.bak",i) : name+".bak";
        if (noneof(files==bakname)) break;
      }
      rename, path+name, path+bakname;
    }
    write, create(path+name), format="%s\n", linesize=65535, mkfile;
    write, path+name, format="created template %s\n";
    return;
  }

  /* check for existing Makefile
   * if present and not obsolete, just update Y_MAKEDIR, Y_EXE, and quit
   * else move to .obs and proceed to create a new Makefile
   */
  name = anyof(files == "Makefile");
  if (name || anyof(files == "makefile")) {
    name = (name? "Makefile" : "makefile");
    line = rdfile(path+name);
    i = make_subst(line);
    if (!numberof(i)) {
      write, name, name, format=
        "***WARNING*** %s is obsolete, moving to %s.obs\n";
      if (anyof(files == name+".obs")) {
        for (i=1 ; ; i++)
          if (noneof(files==swrite(format=name+"-%ld.obs",i))) break;
        rename, path+name+".obs", swrite(format=path+name+"-%ld.obs",i);
      }
      rename, path+name, path+name+".obs";
    } else {
      for (i=0 ; ; i++) {
        bakname = i? swrite(format=name+"-%ld.bak",i) : name+".bak";
        if (noneof(files==bakname)) break;
      }
      rename, path+name, path+bakname;
      write, create(path+name), format="%s\n", linesize=65535, line;
      remove, path+bakname;
      write, path+name, format="updated %s\n";
      return;
    }
  } else {
    name = "Makefile";
  }

  /* look for plug_in command to identify package include files
   * get the package name from any plug_in command(s) found, plus
   * the PKG_I list
   */
  i = files(where(strglob("*.i", files)));
  if (numberof(i)) i = i(where(i!="check.i"));
  if (numberof(i)) {
    ipkg = array(string, numberof(i));
    for (k=1 ; k<=numberof(i) ; k++) {
      line = open(path+i(k), "r", 1);
      if (is_void(line)) continue;
      line = rdfile(line);
      list = strfind("plug_in", line);
      line = line(where(list(2,) > 0));
      if (numberof(line)) {
        optif = "^[ \t]*(if[ \t]*\\(.+\\)[ \t]*)?";
        list = strgrep(optif+"plug_in[ \t]*,[ \t]*[\"](.+)[\"]", line, sub=2);
        line = strpart(line, list);
        line = line(where(line));
        if (!numberof(line) || anyof(line(1)!=line))
          write, i(k), format=
            "***WARNING*** skipping %s: multiple plug_in commands\n";
        else
          ipkg(k) = line(1);
        /* could look for PROTOTYPE FORTRAN or EXTERNAL FORTRAN
         * in order to judge if fortran code present, but this is
         * not definitive since it could be called from C
         */
      }
    }
    list = where(ipkg);
    i = i(list);
    ipkg = ipkg(list);
    if (numberof(i)) {
      if (numberof(ipkg)>1 && anyof(ipkg(1)!=ipkg)) {
        write, ipkg(1), format=
            "***WARNING*** defining package %s, but plug_in commands\n";
        write, sum(" "+ipkg(2:0)), format=
            "***WARNING*** also use packages%s\n";
      }
      ipkg = ipkg(1);
    }
  }
  if (!numberof(i))
    error,
      "***FATAL*** no plug_in command in any .i file in this directory";

  mkfile(where(strglob("PKG_NAME=*",mkfile))) = pnm = "PKG_NAME="+ipkg;
  mkfile(where(strglob("PKG_I=*",mkfile))) = inm =
    "PKG_I="+strpart(sum(i+" "),1:-1);

  /* look for #include statements in the .c files
   * which refer to .h files in this directory,
   * and generate .o file dependencies accordingly
   */
  c = files(where(strglob("*.c", files)));
  if (numberof(c)) c = c(where((c!="yinit.c")&(c!="ywrap.c")));
  cxx_list = [];
  exts = numberof(c)? strpart(c, -1:0) : [];
  for (k=1 ; k<=numberof(_make_cxx_exts) ; k++) {
    list = where(strglob("*"+_make_cxx_exts(k), files));
    if (numberof(list)) {
      grow, cxx_list, [_make_cxx_exts(k)];
      grow, exts, array(_make_cxx_exts(k), numberof(list));
      grow, c, files(list);
    }
  }

  h = files(where(strglob("*.h", files)));
  o = strpart(c,1:-1) + "o";
  if (numberof(cxx_list)) {
    for (i=1 ; i<=numberof(_make_hxx_exts) ; i++) {
      list = where(strglob("*"+_make_hxx_exts(i), files));
      if (numberof(list)) grow, h, files(list);
    }
    len = strlen(exts);
    list = where(len > 2);
    for (k=1 ; k<=numberof(list) ; k++)
      o(list(k)) = strpart(o(list(k)), 1:1-len(list(k))) + "o";
  }
  if (numberof(h)) {
    h = h(sort(h));
    hh = indgen(numberof(h));
    if (numberof(c)) cdep = array(pointer, numberof(c));
    for (k=1 ; k<=numberof(c) ; k++) {
      line = open(path+c(k), "r", 1);
      if (is_void(line)) continue;
      line = rdfile(line);
      list = strgrep("#[ \t]*include[ \t]*[<\"](.+)[\">]", line, sub=1);
      line = strpart(line, list);
      list = where(line);
      if (!numberof(list)) continue;
      line = line(list);
      line = line(sort(line));
      if (numberof(list) > 1) {
        list = grow([string(0)], line);
        line = line(where(list(2:0) != list(1:-1)));
      }
      ii = grow(array(0,numberof(line)), hh);
      line = grow(line, h);
      list = sort(line);
      line = line(list);
      ii = ii(list);
      list = where(line(2:0) == line(1:-1));
      if (!numberof(list)) continue;
      cdep(k) = &h( max(ii(list), ii(list+1)) );
    }
    list = where(cdep);
    chdep = c(list);
    cdep = cdep(list);
    if (numberof(chdep)) {
      for (k=1 ; k<=numberof(chdep) ; k++)
        chdep(k) = o(k) + ":" + sum(" "+(*cdep(k)));
    }
    mkfile = grow(mkfile(1:-1), chdep, [""], mkfile(0:0));
  }

  if (!no_fort) {
    fort = files(where(strglob("*.[fFm]", files)));
    if (numberof(fort) && !fortran_supported) {
      write, format="***WARNING*** fortran source not fully supported%s","\n";
      grow, exts, strpart(fort, -1:0);
      grow, c, fort;
    }
  }
  if (!numberof(c))
    error,
      "***FATAL*** no C, C++, or Fortran source files in this directory";

  /* generate the OBJS= line, assuming all source files are
   * to become part of the package
   */
  objnm = o+" ";
  if (!dimsof(objnm)(1)) objnm = [objnm];
  objs = array(string, numberof(objnm));
  for (k=1 ; ; k++) {
    lobjs = strlen(objnm)(psum);
    j = max(sum(lobjs<70), 1);
    objs(k) = objnm(sum:1:j);
    if (j == numberof(objnm)) break;
    objnm = objnm(j+1:0);
  }
  objs = objs(1:k);
  objs(1) = "OBJS="+objs(1);
  if (k > 1) {
    objs(2:k) = "  "+objs(2:k);
    objs(1:k-1) += "\\";
  }
  objs(0) = strpart(objs(0),1:-1);

  k = where(strglob("OBJS=*", mkfile))(1);
  mkfile = grow(mkfile(1:k-1), objs, mkfile(k+1:0));

  if (numberof(cxx_list)) {
    cxx_lines = _make_cxx_lines(1:-1);
    grow, cxx_lines, swrite(format=_make_cxx_rule, cxx_list);
    grow, cxx_lines, _make_cxx_lines(0:0);
    k = where(strglob("Y_SITE=*", mkfile))(1);
    mkfile = grow(mkfile(1:k+1), cxx_lines, mkfile(k+1:0));
  }

  write, create(path+name), format="%s\n", linesize=65535, mkfile;
  write, ((path!="./")?path:"")+name, format="created %s\n";
  write, "automatically generated make macros and dependencies:";
  write, format="%s\n", pnm;
  write, format="%s\n", inm;
  write, format="%s\n", objs;
  if (!is_void(chdep)) write, format="%s\n", chdep;
  write, "edit "+name+" by hand to provide PKG_DEPLIBS or other changes";
}

/* default C++ source extensions recognized by make() */
_make_cxx_exts = [".cxx", ".cpp", ".cc", ".c++", ".C", ".CPP"];
_make_hxx_exts = [".hh", ".hxx", ".H"];  /* .h also checked */
_make_cxx_rule =
  "%s.o:\n\t$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c -o $@ $<";
_make_cxx_lines =
 ["# ------------begin C++ source hacks",
  "# must use C++ to load yorick with this C++ package",
  "# this assumes make default CXX macro points to C++ compiler",
  "CXXFLAGS=$(CFLAGS)",
  "LD_DLL=$(CXX) $(LDFLAGS) $(PLUG_SHARED)",
  "LD_EXE=$(CXX) $(LDFLAGS) $(PLUG_EXPORT)",
  "",
  "# C++ has no standard file extension, supply default make rule(s)",
  "# --------------end C++ source hacks"
  ];
/* typical standalone link rule
LINK.cc = $(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_ARCH)
.cc:
	$(LINK.cc) $^ $(LOADLIBES) $(LDLIBS) -o $@
*/

func make_subst(&line)
{
  i = where(strglob("Y_MAKEDIR=*", line));
  if (numberof(i)) {
    j = i(1);
    i = where(strglob("Y_EXE=*", line));
    if (numberof(i)) {
      i = i(1);
    } else {  /* file probably corrupt */
      i = j+1;
      line = grow(line(1:j), string(0), line(i:0));
    }
    k = where(strglob("Y_EXE_PKGS=*", line));
    if (numberof(k)) {
      k = k(1);
    } else {  /* file probably corrupt */
      k = i+1;
      line = grow(line(1:i), string(0), line(k:0));
    }
    pkgs = get_pkgnames(0);
    pkgs = pkgs(where(pkgs!="yor"));
    if (!numberof(pkgs)) pkgs = "";
    else pkgs = strpart(sum(pkgs+" "), 1:-1);
    exename = get_argv()(1);
    if (strglob("*.EXE", exename)) strcase,0, exename;
    line(j) = "Y_MAKEDIR=" + make_despace(strpart(Y_HOME,1:-1));
    line(i) = "Y_EXE=" + make_despace(exename);
    line(k) = "Y_EXE_PKGS=" + pkgs;
    i = k;
    k = where(strglob("Y_EXE_HOME=*", line));
    if (numberof(k)) {
      k = k(1);
    } else {  /* file probably corrupt */
      k = i+1;
      line = grow(line(1:i), string(0), line(k:0));
    }
    line(k) = "Y_EXE_HOME=" + make_despace(strpart(Y_HOME,1:-1));
    i = k;
    k = where(strglob("Y_EXE_SITE=*", line));
    if (numberof(k)) {
      k = k(1);
    } else {  /* file probably corrupt */
      k = i+1;
      line = grow(line(1:i), string(0), line(k:0));
    }
    line(k) = "Y_EXE_SITE=" + make_despace(strpart(Y_SITE,1:-1));
    i = k;
    k = where(strglob("Y_HOME_PKG=*", line));
    if (numberof(k)) {
      k = k(1);
    } else {  /* file probably corrupt */
      k = i+1;
      line = grow(line(1:i), string(0), line(k:0));
    }
    y = Y_HOME_PKG? make_despace(strpart(Y_HOME_PKG,1:-1)) : "";
    line(k) = "Y_HOME_PKG=" + y;
  }
  return i;
}

func make_despace(name)
{
  /* backspace escape blanks in path names */
  return streplace(name, strfind(" ",name,n=256), "\\ ");
}

/* here is the yorick package Makefile template
 * fill in PKG_NAME, PKG_I, OBJS, and optionally
 * PKG_DEPLIBS, PKG_CFLAGS, EXTRA_PKGS, PKG_CLEAN, etc.
 *
 *   yorick -batch make.i
 * fills in Y_MAKEDIR, Y_EXE
 * or creates a Makefile if none is present
 */
make_file =
 ["# these values filled in by    yorick -batch make.i",
  "Y_MAKEDIR=###  Y_HOME (directory containing Makepkg)",
  "Y_EXE=###      Y_LAUNCH/yorick (path to yorick executable)",
  "Y_EXE_PKGS=### packages statically loaded into Y_EXE",
  "Y_EXE_HOME=### Y_HOME for Y_EXE",
  "Y_EXE_SITE=### Y_SITE for Y_EXE",
  "Y_HOME_PKG=### Y_HOME_PKG for Y_EXE",
  "",
  "# ----------------------------------------------------- optimization flags",
  "",
  "# options for make command line, e.g.-   make COPT=-g TGT=exe",
  "COPT=$(COPT_DEFAULT)",
  "TGT=$(DEFAULT_TGT)",
  "",
  "# ------------------------------------------------ macros for this package",
  "",
  "PKG_NAME=###   name of this package",
  "PKG_I=###      pkg.i file(s) defining interpreted API for this package",
  "",
  "OBJS=###       list of .o files (library modules) for this package",
  "",
  "# change to give the executable a name other than yorick",
  "PKG_EXENAME=yorick",
  "",
  "# PKG_DEPLIBS=-Lsomedir -lsomelib   for dependencies of this package",
  "PKG_DEPLIBS=",
  "# set compiler (or rarely loader) flags specific to this package",
  "PKG_CFLAGS=",
  "PKG_LDFLAGS=",
  "",
  "# list of additional package names you want in PKG_EXENAME",
  "# (typically $(Y_EXE_PKGS) should be first here)",
  "EXTRA_PKGS=$(Y_EXE_PKGS)",
  "",
  "# list of additional files for clean",
  "PKG_CLEAN=",
  "",
  "# autoload file for this package, if any",
  "PKG_I_START=",
  "# non-pkg.i include files for this package, if any",
  "PKG_I_EXTRA=",
  "",
  "# -------------------------------- standard targets and rules (in Makepkg)",
  "",
  "# set macros Makepkg uses in target and dependency names",
  "# DLL_TARGETS, LIB_TARGETS, EXE_TARGETS",
  "# are any additional targets (defined below) prerequisite to",
  "# the plugin library, archive library, and executable, respectively",
  "PKG_I_DEPS=$(PKG_I)",
  "Y_DISTMAKE=distmake",
  "",
  "include $(Y_MAKEDIR)/Make.cfg",
  "include $(Y_MAKEDIR)/Makepkg",
  "include $(Y_MAKEDIR)/Make$(TGT)",
  "",
  "# override macros Makepkg sets for rules and other macros",
  "# see comments in Y_HOME/Makepkg for a list of possibilities",
  "",
  "# if this package built with mpy: 1. be sure mpy appears in EXTRA_PKGS,",
  "# 2. set TGT=exe, and 3. uncomment following two lines",
  "# Y_MAIN_O=$(Y_LIBEXE)/mpymain.o",
  "# include $(Y_MAKEDIR)/Makempy",
  "",
  "# configure script for this package may produce make macros:",
  "# include output-makefile-from-package-configure",
  "",
  "# reduce chance of yorick-1.5 corrupting this Makefile",
  "MAKE_TEMPLATE = protect-against-1.5",
  "",
  "# ------------------------------------- targets and rules for this package",
  "",
  "# simple example:",
  "#myfunc.o: myapi.h",
  "# more complex example (also consider using PKG_CFLAGS above):",
  "#myfunc.o: myapi.h myfunc.c",
  "#	$(CC) $(CPPFLAGS) $(CFLAGS) -DMY_SWITCH -o $@ -c myfunc.c",
  "",
  "# -------------------------------------------------------- end of Makefile"
  ];

if (batch()) {
  if (numberof(get_argv()) > 1)
    error, "yorick -batch make.i no longer accepts any parameters";
  make;
  quit;
}

func write_home(dir)
/* DOCUMENT write_home, dir
     writes file Y_HOME.txt to directory DIR (current working directory
     by default).  This file contains the path of Y_HOME, used by yorick
     to find its public installation when started as a private pacakge.
 */
{
  if (!dir) dir = "";
  else if (strpart(dir,0:0) != "/") dir += "/";
  write,create(dir+"Y_HOME.txt"),format="%s\n", Y_HOME;
}
