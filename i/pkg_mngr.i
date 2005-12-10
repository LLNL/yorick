/*
 * pkg_mngr.i
 * $Id: pkg_mngr.i,v 1.9 2005-12-10 12:41:36 frigaut Exp $
 * Yorick package manager
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

PKG_MNGR_VERSION = 0.6;

local pkg_mngr;
/* DOCUMENT pkg_mngr.i
 *
 * Yorick Package Manager. Main functions
 *
 * pkg_setup               Set up pkg_mngr parameters (see below for details)
 * pkg_sync                Sync the local package repository with server
 * pkg_list                List all available packages (list install status
 *                         ("i" marks installed packages), the package name,
 *                         the last available version, the installed version
 *                         (when applicable) and a short description.
 * pkg_install,"pkg_name"  Install package "pkg_name"
 * pkg_remove,"pkg_name"   Remove package "pkg_name"
 * pkg_info,"pkg_name"     Print detail info about package "pkg_name"
 * pkg_save                Saves PKG global variables in default file,
 *                          which will be re-read each time pkg-mngr.i
 *                          is included. Called by pkg_setup.
 * pkg_reset               Delete all tarballs (in case of problem).
 * 
 *  --- INTRODUCTION TO THE YORICK PACKAGE INSTALLER
 * 
 *  --- TYPICAL USE
 * 
 *  FIRST TIME USE:
 * 
 *  > pkg_setup
 *  ... will ask for parameters for your OS/installation
 *  ... just set the OS, most of the other defaults should be OK.
 *
 *   PKG_OS              : OS. "macosx","linux","windows"
 *   PKG_SERVER          : URL of central server with info files
 *   PKG_FETCH_CMD       : system cmd to fetch a file through an
 *                          internet connection [ex: curl]
 *   PKG_GUNTAR_CMD      : system cmd to gunzip and untar a tgz file
 *                          [ex; "tar -zxf"]
 *   PKG_TMP_DIR         : temporary directory. Normaly Y_HOME/packages/tmp
 *   PKG_VERBOSE         : verbose level
 *   PKG_ASK_CONFIRM     : ask for confirmation in critical operations
 *   PKG_RUN_CHECK       : run check after install
 *
 *  > pkg_sync
 *  ...to sync your local info file repository with the server
 *  ...this should be done from time to time.
 *
 *  ---
 * 
 *  > pkg_list
 *  ...fetch all packages .info files on the server, and prints out
 *  the available & installed packages, with their version.
 *
 *  > pkg_install,"pkgname"
 *  ... will install package "pkgname"
 *
 *  FURTHER USE:
 * pkg_list, pkg_install, and pkg_remove are the only 3 functions
 * that should be necessary for further use.
 *
 *  --- IN CASE OF PROBLEMS:
 *  In case something went wrong and you downloaded a bad tarball:
 *  Go in Y_HOME/packages/tarballs
 *  and remove the offensive tgz file. In doubt, you can just wipe
 *  up the whole directory content. tarballs will be downloaded again
 *  if pkg_mngr does not find existing local ones.
 *
 *  --- SERVER / CLIENT ORGANIZATION
 * 
 *  The yorick package manager (pkg_mngr.i) is organized with several
 *  central servers (sourceforge, maumae), being repositories of binary
 *  packages. These packages are self-contained plugins for yorick, and
 *  include yorick include files, autoload files and libraries. Necessary
 *  dependencies have been linked in. Other yorick package dependecies
 *  are cared for, and dependent packages are installed automatically.
 * 
 *  Locally, the directory package is the home of the packager. It contains
 *  3 subdirectories:
 *  packages/info: home of the info files that describe all available
 *                 packages.
 *  packages/installed: info files for installed packages.
 *  packages/tarballs:  tarballs for installed packages.
 * 
 *  --- PACKAGE STRUCTURE:
 * 
 *  A standard package includes (once tar zxvf'd):
 * 
 * 
 *  root/
 *  root/pkg.info         : package info file
 *  root/preflight.i      : if present, will be run, from Y_HOME/packages,
 *                           before the copying of the other files (but
 *                           after the tar -zxvf or equivalent).
 *  root/postflight.i     : if present, will be run, from Y_HOME/packages,
 *                           after the copying of the other files (but
 *                           before the dist tree cleanup).
 *  root/dist/            : distribution root
 *  root/dist/y_site/     : all the files under this will be copied
 *                           recursively into Y_SITE.
 *  root/dist/y_home/     : all the files under this will be copied
 *                           recursively into Y_HOME.
 * 
 *  I recommend to include and distribute a copy of the package source:
 *  It is (1) educational, (2) easier to rebuild if needed and (3) that's
 *  a good place to store a check.i and other example for the package (and
 *  perhaps a doc). A good place to store the source is Y_SITE/contrib/pkg.
 * 
 *  Here is an example for the HDF5 package:
 *  
 *  poliahu:tarballs% tar zxvf hdf5-0.5.tgz
 *  hdf5/
 *  hdf5/dist/
 *  hdf5/dist/check.i
 *  hdf5/dist/y_home/
 *  hdf5/dist/y_home/i-start/
 *  hdf5/dist/y_home/i-start/yhdf5.i
 *  hdf5/dist/y_home/lib/
 *  hdf5/dist/y_home/lib/hdf5.so
 *  hdf5/dist/y_site/
 *  hdf5/dist/y_site/contrib/
 *  hdf5/dist/y_site/contrib/hdf5/
 *  hdf5/dist/y_site/contrib/hdf5/check.i
 *  hdf5/dist/y_site/contrib/hdf5/hdf5.c
 *  hdf5/dist/y_site/contrib/hdf5/hdf5.i
 *  hdf5/dist/y_site/contrib/hdf5/Makefile
 *  hdf5/dist/y_site/contrib/hdf5/yhdf5.i
 *  hdf5/dist/y_site/i0/
 *  hdf5/dist/y_site/i0/hdf5.i
 *  hdf5/hdf5.info          
 * 
 *  --- INSTALLER MECHANICS
 * 
 *  The operation of installing a packages goes through two main parts:
 * 
 *  part 1. Get the tarball from the server.
 * 
 *  part 2. Once the tarball is local, it is installed. All the libraries
 *    and other files are put where they belong. Usually:
 *    libraries (*.so) are put in Y_HOME/lib
 *    include files (*.i) are put in Y_SITE/i0
 *    autoload files are put in Y_HOME/i-start
 *    However, this is not mandatory, and depends on how the maintainer
 *    has arrange the files in the tarball.
 *    The installer also scans for a preflight and postflight include files,
 *    and run them (include them) if present.
 * 
 *  In more details:
 * 
 *  pkgname example = hdf5;
 * 
 *  I.  check for depenency tree. Announce dependencies, possibly
 *      ask for confirm.
 *  II. Start by lowest dependency:
 *     1.  check if already on disk locally [to come]
 *     2.  if yes, check that local version is the last one [to come]
 *     3.  if not, offer the choice to re-install local version
 *           or fetch and install new one [to come]
 *     4.  fetch tarball (if necessary)
 *     5.  md5 it [to come]
 *     6.  gunzip + untar it (e.g. tar -zxvf)
 *     7.  cd in pkgname directory
 *     8.  check if preflight exist. If it does, execute it (include it).
 *     9.  recursively copy the distributed files in Y_SITE and Y_HOME
 *     10. copy the info file in the installed directory, with a list
 *           of the installed files.
 *     11. check if postflight exist. If it does, execute it (include it).
 *     12. clean up after ourselves (temp dist tree). Keep tarball.
 *           
 *  III. Update installed pkg. This is done automatically by putting
 *       the info file of the installed package in package/installed
 * 
 * 
 *  --- .INFO FILE
 * 
 *  Here is an example of a .info file (from the hdf5 package):
 * 
 *  Package: hdf5
 *  Kind: plugin
 *  Version: 0.5
 *  Revision: 1
 *  Description: Hierarchical Data Format 5 interface
 *  License: GPL
 *  Maintainer: Francois Rigaut <frigaut@users.sf.net>
 *  OS: macosx
 *  Depends: yorick(>=1.6.02)
 *  Source: http://www.maumae.net/yorick/packages/%o/tarballs/hdf5-%v.tgz
 *  Source-MD5: 6f8038cd09f72f4ff060e2d278256b6f
 *  Source-Directory: contrib/hdf5
 *  DocFiles: README NEWS doc/*.doc doc/*.pdf doc/*.ps doc/*.tex
 *  Homepage: http://www.maumae.net/yorick/doc/plugins.php
 *  DescDetail: <<
 *  HDF5 is the yorick interface plugin to the NCSA Hierarchical Data Format
 *  version 5. It includes function for reading, writing, updating, getting
 *  information on HDF5 files.
 *  <<
 *  DescUsage: <<
 *  See i/hdf5_tests.i for a test suite. Type 
 *  "yorick -batch hdf5_tests.i" in a terminal to run it.
 *  <<
 *  DescPort: <<
 *  This package will compile Yorick only on MacOSX 10.3.4 or later, because
 *  of a bug in the system math library libm (part of /usr/lib/LibSystem.dylib)
 *  in earlier versions of MacOSX 10.3.
 *  <<
 * 
 *  Right now, the only important (used) keywords are:
 *  Version, Description, OS, Depends, Source
 * 
 *  If you want to contribute a package, you will have to generate a
 *  .info file:
 * 
 *  Instructions and tips:
 *  - Keep the description short (< 45 characters), you can expand ad lib in
 *    the DescDetail
 *  - Depends can include several members, separated by a comma (e.g.
 *    yorick(>=1.6.02),imutil(>0.4)
 *  - OS has to be "macosx","linux" or "windows" for now. lowercase pls.
 *  - Source if the URL where the tarball can be fetched. In the near future,
 *    I'm planning to make that compatible with a vector (several URL that
 *    will be tried after another in case a link is down) but for now,
 *    keep that a single URL.
 *  - Version can only contain integer (e.g. 2.1.4-r2 does *not* work).
 *  
 * 
 *  --- TO DO
 * 
 *  * Allow installations in other locations that the Y_SITE and Y_HOME.
 *    Some users may not have access to these and still want to install
 *    packages for their personnal use.
 * 
 *  * At one point, we should have, to complement this installer, a 
 *    utility to check for symbols conflicts (e.g. same function names
 *    in 2 different packages)
 * 
 */

require,"string.i";

struct pkginfo_str{
  string name;
  string kind;
  string vers;
  string revision;
  string desc;
  string license;
  string maintainer;
  string os;
  string depends;
  string depends_pkg(20);
  string depends_vers(20);
  string depends_rel(20);
  string source;
  string md5;
  string dir;
  string docs;
  string homepage;
  string desc_details;
  long   version(20);
};

setup_done=0;

// read packages/pkg_setup.i if it exists
if (open(Y_HOME+"packages/pkg_setup.i","r",1)) {
  require,Y_HOME+"packages/pkg_setup.i";
  setup_done=1;
}


func pkg_save
/* DOCUMENT pkg_save
   Save the packager parameters in the file
   Y_HOME/packages/pkg_setup.i
   SEE ALSO: pkg_setup
 */
{
  if (noneof(lsdir(Y_HOME)=="packages")) 
    mkdir,Y_HOME+"packages";
  if (noneof(lsdir(Y_HOME)=="packages")) 
    error,"Can't create Y_HOME/packages (permissions?)";
  
  f = open(Y_HOME+"packages/pkg_setup.i","w",1);
  if (!f) error,"Can't create Y_HOME/packages/pkg_setup.i (permissions?)";
  write,f,format="PKG_OS = \"%s\";\n",PKG_OS;
  write,f,format="PKG_FETCH_CMD = \"%s\";\n",PKG_FETCH_CMD;
  write,f,format="PKG_SERVER = \"%s\";\n",PKG_SERVER;
  write,f,format="PKG_GUNTAR_CMD = \"%s\";\n",PKG_GUNTAR_CMD;
  write,f,format="PKG_TMP_DIR = \"%s\";\n",PKG_TMP_DIR;
  write,f,format="PKG_VERBOSE = %d;\n",PKG_VERBOSE;
  write,f,format="PKG_ASK_CONFIRM = %d;\n",PKG_ASK_CONFIRM;
  write,f,format="PKG_RUN_CHECK = %d;\n",PKG_RUN_CHECK;
  if (PKG_SYNC_DONE)
    write,f,format="PKG_SYNC_DONE = \"%s\";\n",PKG_SYNC_DONE;
  close,f;

  if (PKG_VERBOSE)
    write,format="%s\n","Parameters saved in Y_HOME/packages/pkg_setup.i";
}



func pkg_sync(server,verbose=)
/* DOCUMENT pkg_sync(server)
   sync the info tree with the central server
   server: overrides the PKG_SERVER variable definition
   SEE ALSO: pkg_mngr, pkg_list, pkg_install
 */
{
  extern PKG_SYNC_DONE;
  if (!setup_done) pkg_setup,first=1;
  
  if (!server) server = PKG_SERVER;
  if (verbose==[]) verbose=PKG_VERBOSE;

  if (noneof(lsdir(Y_HOME)=="packages")) 
    mkdir,Y_HOME+"packages";
  if (noneof(lsdir(Y_HOME)=="packages")) 
    error,"Can't create Y_HOME/packages (permissions?)";

  if (noneof(lsdir(Y_HOME+"packages/")=="info"))
    mkdir,Y_HOME+"packages/info";
  if (noneof(lsdir(Y_HOME+"packages/")=="installed"))
    mkdir,Y_HOME+"packages/installed";
  if (noneof(lsdir(Y_HOME+"packages/")=="tarballs"))
    mkdir,Y_HOME+"packages/tarballs";
  if (noneof(lsdir(Y_HOME+"packages/")=="tmp"))
    mkdir,Y_HOME+"packages/tmp";


  if (verbose) write,format="%s","Syncing with server";
  if (verbose>=3) write,"";


  pkg_fetch_url,server+PKG_OS+"/info/",PKG_TMP_DIR+"info.html",
    verbose=verbose;
  
  if (verbose<3) write,format="%s",".";

  ctn = rdfile(PKG_TMP_DIR+"info.html");
  files = strpart(ctn,strgrep("[a-zA-Z0-9\_\.\+\-]+\\.info",ctn));
  files = files(where(files));
  
  
  for (i=1;i<=numberof(files);i++) {
    pkg_fetch_url,server+PKG_OS+"/info/"+files(i),      \
      Y_HOME+"packages/info/"+files(i),verbose=verbose;
    if (verbose<3) write,format="%s",".";
  }

  if (verbose<3) write,format="done (%d info files fetched)\n",numberof(files);
  PKG_SYNC_DONE = getdate();
  pkg_save;
}

func get_international_date(date)
/* DOCUMENT get_intern_date(date)
   meant to convert date returned by getdate() into something
   clear and understandable by people from europe and US
   SEE ALSO:
 */
{
  if (!date) return;
  d = strtok(date,"/",3);
  n = 0;
  sread,d(2),n;
  month = ["Jan","Feb","Mar","Apr","May","Jun",
           "Jul","Aug","Sep","Oct","Nov","Dec"];
  return d(1)+" "+month(n)+" 20"+d(3); //valid for 95 years
}

func pkg_list(server,sync=,verbose=)
/* DOCUMENT pkg_list,server,nosync=
   Print out a list of available packages, including
   version number, whether it is installed (installed
   version), and a short description.
   server: overrides the PKG_SERVER variable definition
   nosync= : if set, prevent sync with server
   SEE ALSO: pkg_mngr, pkg_sync, pkg_install
 */
{
  extern PKG_SYNC_DONE;
  if (!setup_done) pkg_setup,first=1;
  if (!PKG_SYNC_DONE) pkg_sync;

  if (sync) {
    pkg_sync,server,verbose=verbose;
  } else {
    write,format="last sync %s (\"pkg_sync\" to sync)\n",get_international_date(PKG_SYNC_DONE);
  }
  if (verbose==[]) verbose=PKG_VERBOSE;
  
  infodir = Y_HOME+"packages/info/";
  instdir = Y_HOME+"packages/installed/";

  infoname = get_avail_pkg();
  if (numberof(infoname)==0) error,"No info file found in "+infodir;

  instname = get_inst_pkg();
  if (instname==0) instname="";
  
  for (i=1;i<=numberof(infoname);i++) {
    // for all info file, parse file:
    pkg = parse_info_file(infodir+infoname(i)+".info");

    // is this package installed locally?
    w = where(instname==infoname(i));
    // is yes, what version?
    if (numberof(w)) {
      is_inst = "i";
      instvers = (parse_info_file(instdir+instname(w(1))+".info")).vers;
    } else {
      is_inst = "";
      instvers = "-";
    }
    
    write,format="%1s %-12s %-8s %-8s %-45s\n", is_inst,infoname(i),
      pkg.vers,instvers,pkg.desc;
  }
  
}



func pkg_info(pkgname)
/* DOCUMENT pkg_info,pkgname
   Prints out more information about package "pkgname" (string).
   Does nothing else.
   SEE ALSO:
 */
{
  if (!setup_done) pkg_setup,first=1;
  if (!PKG_SYNC_DONE) pkg_sync;

  infodir = Y_HOME+"packages/info/";
  
  pkg = parse_info_file(infodir+pkgname+".info");

  write,format="Package    : %s\n",pkg.name;
  write,format="Version    : %s\n",pkg.vers;
  write,format="Maintainer : %s\n",pkg.maintainer;
  write,format="Depends    : %s\n",pkg.depends;
  write,format="Homepage   : %s\n",pkg.homepage;
  write,format="Description: \n%s\n",pkg.desc_details;
  write,format="\n%s\n","\"pkg_list[,nosync=1]\" to see installed status";
}

func pkg_setup(first=)
/* DOCUMENT pkg_setup
   Simply print out the global variables relative to this
   package manager.
   SEE ALSO: pkg_mngr, pkg_save.
 */
{
  extern setup_done;
  if (first) {
    write,format="%s\n\n",
    "pkg_mngr was not setup! You need to go through this set-up once";
  }
  write,format="%s\n","Enter set-up parameters for pkg_mngr ([] = default)";

  if (!PKG_OS) PKG_OS="macosx";
  if (!PKG_FETCH_CMD) PKG_FETCH_CMD="curl -s";
  if (!PKG_GUNTAR_CMD) PKG_GUNTAR_CMD="tar zxf";
  if (!PKG_SERVER) PKG_SERVER="http://www.maumae.net/yorick/packages/";
  if (!PKG_TMP_DIR) PKG_TMP_DIR=Y_HOME+"packages/tmp/";
  if (PKG_VERBOSE==[]) PKG_VERBOSE=1;
  if (PKG_ASK_CONFIRM==[]) PKG_ASK_CONFIRM=1;
  if (PKG_RUN_CHECK==[]) PKG_RUN_CHECK=0;
    
  PKG_OS = kinput("PKG_OS (\"linux\"|\"macosx\"|\"windows\")",PKG_OS);
  PKG_FETCH_CMD = kinput("PKG_FETCH_CMD",PKG_FETCH_CMD);
  PKG_SERVER = kinput("PKG_SERVER",PKG_SERVER);
  PKG_GUNTAR_CMD = kinput("PKG_GUNTAR_CMD",PKG_GUNTAR_CMD);
  //  PKG_TMP_DIR = kinput("PKG_TMP_DIR",PKG_TMP_DIR);
  PKG_VERBOSE = kinput("PKG_VERBOSE (0|1|2|3)",PKG_VERBOSE);
  PKG_ASK_CONFIRM = kinput("PKG_ASK_CONFIRM (0|1)",PKG_ASK_CONFIRM);
  PKG_RUN_CHECK = kinput("PKG_RUN_CHECK",PKG_RUN_CHECK);

  write,format="\nPKG_OS = %s\n",PKG_OS;
  write,format="PKG_FETCH_CMD = %s\n",PKG_FETCH_CMD;
  write,format="PKG_SERVER = %s\n",PKG_SERVER;
  write,format="PKG_GUNTAR_CMD = %s\n",PKG_GUNTAR_CMD;
  //  write,format="PKG_TMP_DIR = %s\n",PKG_TMP_DIR;
  write,format="PKG_VERBOSE = %d\n",PKG_VERBOSE;
  write,format="PKG_ASK_CONFIRM = %d\n",PKG_ASK_CONFIRM;
  write,format="PKG_RUN_CHECK = %d\n",PKG_RUN_CHECK;

  setup_done = 1;
  pkg_save;
}


func pkg_install(pkgnames,verbose=,check=,force=,_recur=,_version=,_vrel=)
/* DOCUMENT pkg_install,pkgname,force=,check=,verbose=
   Install package "pkgname" (string vector)
   Grabs the tarball from a central server, untar it, copy the files
   according to the directory structure specified in the untared
   package, possibly run a preflight and postflight include files (for
   special needs), and place the package info file and a list of
   installed files in package/installed/.

   pkgname can be a string scalar, vector, and contains wildcard to
   install multiple packages in one call.
   examples: pkg_install,"y*" or pkg_install,["soy","yao"] or pkg_install,"*"
   
   Keywords:
   force: force installation
   check: run checks after install
   verbose: 0 (silent), 1 (some messages), 2 (chatty), 3 (more chatty)
   
   SEE ALSO: pkg_mngr, pkg_remove.
 */
{
  extern _pkg_recur_tree; // to avoid recursion
  if (!PKG_SYNC_DONE) pkg_sync;

  if (!_recur) { // first call in possible recursion
    // the following is to expand the possible wildcards
    // in elements of the vector pkgnames
    // e.g. a call can be pkg_install,["y*","soy"]
    instpkg = get_avail_pkg();
    allpkg = [];
    for (np=1;np<=numberof(pkgnames);np++) {
      w = where(strglob(pkgnames(np),instpkg));
      if (numberof(w)==0) continue;
      grow,allpkg,instpkg(w);
    }
    
    pkgnames = allpkg;
    if (numberof(pkgnames)==0) return 0;

    if (PKG_ASK_CONFIRM) {
      write,format="%s\n","Packages to install :";
      write,pkgnames;
      if (kinput("OK? ","y")!="y") return 0;
    }
  }

  for (np=1;np<=numberof(pkgnames);np++) {

    pkgname = pkgnames(np);
    
    if (!setup_done) pkg_setup,first=1;

    if (!_recur)      _pkg_recur_tree=[];
    if (verbose==[])  verbose=PKG_VERBOSE;
    if (_version==[]) _version="0.0";
    if (_vrel==[])    _vrel=">=";
    if (check==[])    check=PKG_RUN_CHECK;
    if (pkgname==[])  error,"Must specify a string-type package name";

    infodir = Y_HOME+"packages/info/";
    instdir = Y_HOME+"packages/installed/";
    tarbdir = Y_HOME+"packages/tarballs/";

    if (anyof(_pkg_recur_tree) && anyof(strmatch(_pkg_recur_tree,pkgname)))
      //we just installed it, to avoid recursing, we should exit.
      continue;

    // is this package already installed with a version > requested version ?
    ins = lsdir(instdir);
    if (!force && anyof(ins)) {
      w = strgrep("([a-zA-Z0-9\_\.\+\-]*)(.info$)",ins,sub=[1]);
      instname = strpart(ins,w);
      w = where(instname==pkgname);
      if (numberof(w)!=0) {
        // this package has been installed. check version:
        ins=parse_info_file(instdir+pkgname+".info");
        if (vers_cmp(ins.vers,_vrel,_version)) {
          if (verbose && !_recur) {
            write,format="Package %s already installed (%s, needed %s)\n", \
              pkgname,ins.vers,_version;
          }
          continue;
        }
      }
    }
  
    // upgrade: here, check if last version of package already installed
    
    pkg=parse_info_file(infodir+pkgname+".info");
    
    // works out dependencies:
    for (i=1;i<=numberof(where(pkg.depends_pkg));i++) {
      
      // yorick version number:
      if (pkg.depends_pkg(i)=="yorick") {
        if (!vers_cmp(Y_VERSION,pkg.depends_rel(i),pkg.depends_vers))   \
          error,"This package needs yorick version "+
            pkg.depends_rel(i)+" "+pkg.depends_vers(i);
        continue;
      }

      // here work out the possible recursion. use _pkg_uptree
      // other dependencies:
      pkg_install,pkg.depends_pkg(i),verbose=verbose,force=force,
        _recur=1,_version=pkg.depends_vers(i),_vrel=pkg.depends_rel(i);
      
      // add the name to tree to avoid recursion
      grow,_pkg_recur_tree,pkg.depends_pkg(i);
    }

    pkgtgz = strtok(pkg.source,"/",10);
    pkgtgz = pkgtgz(where(pkgtgz))(0);
  
    cd,tarbdir;

    // fetch the tarball
    localtarballs = lsdir(".");
    if ((noneof(localtarballs))|| // no tarballs OR
        (noneof(strmatch(localtarballs,pkgtgz)))|| //no match in loc. tarballs
        (force) ) { // we were asked to force the install anyway
      // this package does not exist locally.
      if (verbose==1) {
        write,format="%-20s: %s",pkgname,"Fetching tarball..";
      } else if (verbose>=2) {
        write,format="%s\n","--------------------------";
        write,format="%s: %s\n",pkgname,"Fetching tarball";
      }
      pkg_fetch_url,pkg.source,tarbdir+pkgtgz,verbose=verbose;
    } else {
      if (verbose==1) {
        write,format="%-20s: %s",pkgname,"Using local tarball..";
      } else if (verbose>=2) {
        write,format="%s\n","--------------------------";
        write,format="%s: %s\n",pkgname,"Using local tarball";
      }
      // the tarball exist locally, we'll use it (unless force=1)
    }

    // gunzip and untar:
    if (verbose==1) {
      write,format="%s","Inflating..";
    } else if (verbose>=2) {
      write,format="%s: %s\n",pkgname,"Inflating tarball";
    }
    pkg_sys,PKG_GUNTAR_CMD+" "+tarbdir+pkgtgz,verbose=verbose;
    
    // run preflight.i
    if (open(pkgname+"/preflight.i","r",1)) {
      if (verbose) write,format="\n%s\n","Running preflight.i";
      include,pkgname+"/preflight.i",1;
    }
    
    list1=list2=[];
    
    // copy to Y_SITE:
    if (anyof(lsdir(pkgname+"/dist/y_site/"))) {
      recursive_rename,pkgname+"/dist/y_site",Y_SITE,
        list1,verbose=verbose,init=1;
    }
    
    // copy to Y_HOME:
    if (anyof(lsdir(pkgname+"/dist/y_home/"))) {
      recursive_rename,pkgname+"/dist/y_home",Y_HOME,
        list2,verbose=verbose,init=1;
    }
    
    list = _(list1,list2);
    
    if (verbose>=2) {
      write,format="%s\n","Installed files:";
      write,format="  + %s\n",list;
    }
    
    // copy info file:
    rename,pkgname+"/"+pkgname+".info",instdir+"/"+pkgname+".info";
    if (verbose>=3) write,format="Executing rename,%s,%s\n",
      pkgname+"/"+pkgname+".info",instdir+"/"+pkgname+".info";
    
    f = open(instdir+"/"+pkgname+".flist","w");
    write,f,format="%s\n",list;
    close,f;
    
    if (open(pkgname+"/postflight.i","r",1)) {
      if (verbose) write,format="%s\n","Running postflight.i";
      include,pkgname+"/postflight.i",1;
    }
    
    //run check if requested and pkgname/check.i file present
    if (check && open(pkgname+"/check.i","r",1)) {
      if (verbose==1) {
        write,format="%s","Checking..";
      } else if (verbose>=2) {
        write,format="%s\n","Checking package";
      }
      cd,pkgname;
      if (anyof(pkgname==["imutil","curses","yorz","yutils","yao"])) {
        include,"check.i";
      } else {
        include,"check.i",1;
      }
    }
    
    // clean up after ourselves
    recursive_rmdir,tarbdir+pkgname+"/dist",verbose=verbose;
    
    // if we got there, it ought to be OK
    if (verbose==1) {
      write,format="%s","installed\n";
    } else if (verbose>=2) {
      write,format="%s installed sucessfully\n",pkgname;
    }
    
  }
  return 0;
}


func pkg_reset(verbose=)
/* DOCUMENT pkg_reset(verbose=)
   Brute force solution: if a bad tarball has been downloaded,
   pkg_mngr will try and try using it and fail. One can remove
   it by hand in Y_HOME/packages/tarballs/ or use this routine
   to remove them all.
   SEE ALSO:
 */
{
  if (verbose==[]) verbose=PKG_VERBOSE;

  rep=kinput("Delete all pkg_mngr tarballs and start from scratch","n");
  if (rep!="y") return
  
  tarbdir = Y_HOME+"packages/tarballs/";
  recursive_rmdir,tarbdir,verbose=verbose;
  mkdir,tarbdir;
}


func pkg_remove(pkgnames,verbose=)
/* DOCUMENT pkg_remove,pkgname,verbose=
   Remove package "pkgname" (string)
   Remove all files (libraries, include files, autoload)
   that were installed by the installer.

   pkgname can be a string scalar, vector, and contains wildcard to
   remove multiple packages in one call.
   examples: pkg_remove,"y*" or pkg_remove,["soy","yao"] or pkg_remove,"*"
   
   help,pkg_mngr for more details.
   
   SEE ALSO: pkg_mngr, pkg_install
 */
{
  if (!setup_done) pkg_setup,first=1;
  if (verbose==[]) verbose=PKG_VERBOSE;
  if (!PKG_SYNC_DONE) pkg_sync;
  if (pkgnames==[]) error,"Must specify a string-type package name";

  instpkg = get_inst_pkg();
  if (noneof(instpkg)) return;
  
  allpkg = [];
  for (np=1;np<=numberof(pkgnames);np++) {
    w = where(strglob(pkgnames(np),instpkg));
    if (numberof(w)==0) {
      write,format="No package corresponding to %s\n",pkgnames(np);
      continue;
    }
    grow,allpkg,instpkg(w);
  }

  pkgnames = allpkg;
  if (numberof(pkgnames)==0) return 0;

  if (PKG_ASK_CONFIRM) {
    write,format="%s\n","Packages to remove :";
    write,pkgnames;
    if (kinput("OK? ","y")!="y") return 0;
  }
  
  instdir = Y_HOME+"packages/installed/";
    
  for (np=1;np<=numberof(pkgnames);np++) {
    pkgname = pkgnames(np);

    if (verbose==1) {
      write,format="%-20s: Removing...",pkgname;
    } else if (verbose>=2) {
      write,format="%s\n","--------------------------";
      write,format="Removing package %s\n",pkgname;
    }
    
    instpkg = get_inst_pkg();
    if (noneof(strmatch(instpkg,pkgname))) {
      if (verbose) write,format="\nPackage %s is not installed\n",pkgname;
      continue;
    }
    
    files = rdfile(instdir+pkgname+".flist");

    for (i=1;i<=numberof(files);i++) {
      if (verbose>=2) write,format=" - %s\n",files(i);
      remove,files(i);
    }

    file = instdir+pkgname+".info";
    if (verbose>=2) write,format=" - %s\n",file;
    remove,file;

    file = instdir+pkgname+".flist";
    if (verbose>=2) write,format=" - %s\n",file;
    remove,file;

    // if we got there, we ought to be OK
    if (verbose==1) {
      write,format="%s\n","done";
    } else if (verbose>=2) {
      write,format="%s removed successfully\n",pkgname;
    }
  }
  return 0;
}


/********************************************\
 *           UTILITARY FUNCTIONS            *
\********************************************/

func get_avail_pkg(void)
{
  infodir = Y_HOME+"packages/info/";

  all = lsdir(infodir);
  if (anyof(all)) {
    w = strgrep("([a-zA-Z0-9\_\.\+\-]*)(.info$)",all,sub=[1]);
    infoname = strpart(all,w);
    infoname = infoname(where(infoname));
  }
  return infoname;
}

func get_inst_pkg(void)
{
  instdir = Y_HOME+"packages/installed/";

  all = lsdir(instdir);
  if (anyof(all)) {
    w = strgrep("([a-zA-Z0-9\_\.\+\-]*)(.info$)",all,sub=[1]);
    instname = strpart(all,w);
    instname = instname(where(instname));
  }
  return instname;
}

func recursive_rmdir(dir,verbose=)
/* DOCUMENT recursive_rmdir,dir,verbose=
   Recursively delete all files and directories under dir, dir
   included.
   Equivalent to the linux/unix command
   rm -rf dir
   Note that recursive_rmdir,"." will fail removing "." (for
   some reason).
   SEE ALSO:
 */
{
  if (strpart(dir,0:0)!="/") dir+="/";
  if (verbose==[]) verbose=PKG_VERBOSE;

  if (allof(lsdir(dir)==0)) return 0;

  orig_dir = dir;
  
  do {

    f = lsdir(dir,subdirs);

    if ( (noneof(f)) && (noneof(subdirs)) ) {
      // no files, no subdirs, ok to remove dir:
      if (verbose>=3) write,format="Removing directory %s\n",dir;
      rmdir,dir;
      if (lsdir(dir)!=0) error,"Can't remove directory "+dir;
      // and exit (cul-de-sac):
      break;
    }

    if (anyof(f)) {
      // some files in here, remove them
      for (i=1;i<=numberof(f);i++) {
        if (verbose>=3) write,format="Removing %s\n",dir+f(i);
        remove,dir+f(i);
      }
    }

    // no more files. If no subdirs, remove dir and exit:
    if (noneof(subdirs)) {
      if (verbose>=3) write,format="Removing directory %s\n",dir;
      rmdir,dir;
      break;
    } else {
      // else go down one level
      dir += subdirs(1)+"/";
    } 
  } while (1);
  
  recursive_rmdir,orig_dir,verbose=verbose;
}



func recursive_rename(dir1,dir2,&list,verbose=,init=)
/* DOCUMENT recursive_rename,dir1,dir2,&list,verbose=,init=
   As it says. This routine will move the whole content of
   dir1 to dir2, recursively.
   Absolute equivalent to the linux/unix command
   cp -pr dir1 dir1
   Except that the files get moved, not copied.
   SEE ALSO:
 */
{
  local sub1;
  
  if (init) list=[];
  if (verbose==[]) verbose=PKG_VERBOSE;

  if (verbose>=3) write,format="Recursive rename %s to %s\n",dir1,dir2;
  
  // make sure dir ends by slash
  if (strpart(dir1,0:0)!="/") dir1+="/";
  if (strpart(dir2,0:0)!="/") dir2+="/";

  // get files and subdirs
  f1 = lsdir(dir1,sub1);

  // make sure destination dir exists:
  f2 = lsdir(dir2);

  // if not, create:
  if (allof(f2)==0) {
    mkdir,dir2;
    if (lsdir(dir2)==0) error,"Creating "+dir2+" failed (check permission)";
    if (verbose>=2) write,format="%s\n","Created "+dir2;
  }

  // finally, move the files:
  for (i=1;i<=numberof(f1);i++) {
    if (verbose>=3)
      write,format="Executing rename,%s,%s\n",dir1+f1(i),dir2+f1(i);
    rename,dir1+f1(i),dir2+f1(i);
    grow,list,dir2+f1(i);
  }

  for (i=1;i<=numberof(sub1);i++) {
    recursive_rename,dir1+sub1(i),dir2+sub1(i),list,verbose=verbose;
  }
}



func vers_cmp(v1,op,v2)
/* DOCUMENT vers_cmp(v1,op,v2)
   Compare (software) version, using operator op.
   Returns 0 (fail) or 1 (pass).
   Operators allowed are >=, >, ==, < and <=
   Will possibly fails for subversion # > 99 (will exit in error if
   case arises)
   SEE ALSO:
 */
{

  ndown=5;
  
  tok1 = strtok(v1,".",ndown);
  tok2 = strtok(v2,".",ndown);

  n1=n2=0l;
  
  for (i=1;i<=ndown;i++) {
    n=0;
    sread,tok1(i),n;
    if (n>99) error,
      swrite(format="n>99 (%d): possible failure mode!",n);
    n1+=n*100^(ndown-i); //limits version numbering to *.99.*
  }

  for (i=1;i<=ndown;i++) {
    n=0;
    sread,tok2(i),n;
    if (n>99) error,
      swrite(format="n>99 (%d): possible failure mode!",n);
    n2+=n*100^(ndown-i); //limits version numbering to *.99.*
  }

  if (op==">=") if (n1>=n2) return 1;
  if (op==">")  if (n1>n2) return 1;
  if (op=="<=") if (n1<=n2) return 1;
  if (op=="<")  if (n1<n2) return 1;
  if (op=="==") if (n1==n2) return 1;

  return 0;
}



func parse_info_file(file)
/* DOCUMENT parse_info_file(file)
   Parse a .info file (file describing a package in the
   yorick package manager, similar to fink/debian info files)
   Return a structure with elements = parsed keywords values
   SEE ALSO:
 */
{
  if (!open(file,"r",1)) error,"Can not find (package exist?)"+file;

  text = rdfile(file);

  pkg = pkginfo_str();

  pkg.version -=99;
  
  pkg.name = pkg_get_keyword_value(text,"Package");
  pkg.kind = pkg_get_keyword_value(text,"Kind");
  pkg.vers = pkg_get_keyword_value(text,"Version");
  pkg.revision = pkg_get_keyword_value(text,"Revision");
  pkg.desc = pkg_get_keyword_value(text,"Description");
  pkg.license = pkg_get_keyword_value(text,"License");
  pkg.maintainer = pkg_get_keyword_value(text,"Maintainer");
  pkg.os = pkg_get_keyword_value(text,"OS");
  pkg.depends = pkg_get_keyword_value(text,"Depends");
  pkg.source = pkg_get_keyword_value(text,"Source");
  pkg.md5 = pkg_get_keyword_value(text,"Source-MD5");
  pkg.dir = pkg_get_keyword_value(text,"Source-Directory");
  pkg.docs = pkg_get_keyword_value(text,"DocFiles");
  pkg.homepage = pkg_get_keyword_value(text,"Homepage");
  pkg.desc_details = pkg_get_keyword_value(text,"DescDetail");


  // deals with source
  pkg.source = streplace(pkg.source,strgrep("%v",pkg.source),pkg.vers);
  pkg.source = streplace(pkg.source,strgrep("%v",pkg.source),pkg.vers);
  pkg.source = streplace(pkg.source,strgrep("%o",pkg.source),pkg.os);
  pkg.source = streplace(pkg.source,strgrep("%o",pkg.source),pkg.os);

  // parse version
  i=1; tmp = pkg.vers;
  do {
    tmp = strtok(tmp,".");
    sread,tmp(1),format="%d",pkg.version(i);
    tmp = tmp(2);
    i++;
  } while (tmp);    

  // parse depends
  i=1; tmp = pkg.depends;
  do {
    tmp = strtok(tmp,",");
    if (strmatch(tmp(1),"(")) {
      tmp2 = strtok(tmp(1),"(");
      pkg.depends_pkg(i) = strtrim(tmp2(1));
      pkg.depends_vers(i) = strpart(tmp2(2),strgrep("[0-9.]+",tmp2(2)));
      pkg.depends_rel(i) = strpart(tmp2(2),strgrep("[><=]+",tmp2(2)));
    } else {
      pkg.depends_pkg(i) = strtrim(tmp(1));
    }
    tmp = tmp(2);
    i++;
  } while (tmp);    
  
      
  return pkg;
}



func pkg_get_keyword_value(text,keyw)
/* DOCUMENT pkg_get_keyword_value(text,keyw)
   return parsed value of keyword read from a .info
   file, in the form:
   keyword: value
   or
   keyword: >>
   multi
   line value
   >>
   SEE ALSO:
 */
{
  w = where(strgrep("^"+keyw,text)(2,)!=-1);
  if (numberof(w)==0) {
    write,format="%s\n","WARNING: Keyword "+keyw+" does not exist";
    return "";
  }

  res = strtrim(strtok(text(w(1)),":")(2));

  if (res == "<<") { // multi line keyword
    res = "";
    w = w(1)+1;
    do {
      res += text(w)+"\n";
      w++;
    } while (strtrim(text(w))!="<<");
    res = strpart(res,1:-1); //suppress last \n
  }
  
  return res;
}



func pkg_sys(cmd,verbose=)
/* DOCUMENT pkg_sys(cmd,verbose=)
   simple interface to the system command.
   Allows echoing of commands output to system
   SEE ALSO:
 */
{
  if (verbose==[]) verbose=PKG_VERBOSE;

  if (verbose>=3) write,format="Spawning %s\n",cmd;

  system,cmd;

  return 0;
}


func pkg_fetch_url(url,dest,verbose=)
/* DOCUMENT pkg_fetch_url(url,dest,verbose=)
   wrap the url fetch command (e.g. curl) with error checking
   SEE ALSO:
 */
{
  // fetch the page/file:
  pkg_sys,PKG_FETCH_CMD+" "+url+" > "+dest,verbose=verbose;

  if (!(f=open(dest,"r",1))) {
    write,"";
    error,"No file fetched (connection down?)";
  }

  ctn = rdline(f,30);

  if (numberof(where(ctn))==0) {
    write,"";
    error,"Zero length file (connection down?)";
  }

  if (anyof(strmatch(ctn,"<title>Error 404: Page Not Found"))) {
    write,"";
    error,"Page not found (error 404)";
  }

  return 0;
}


func kinput(prompt,default)
{
  if (typeof(default)=="string") {
    s = swrite(format=prompt+" [\"%s\"]: ",default);
    sres = rdline(,1,prompt=s)(1);
    if (sres == "") return default;
    res = sres;
  } else if ((typeof(default)=="long")||(typeof(default)=="int")) {
    s = swrite(format=prompt+" [%d]: ",long(default));
    sres = rdline(,1,prompt=s)(1);
    if (sres == "") return default;
    res = 1l;
    sread,sres,res;
  } else if ((typeof(default)=="double")||(typeof(default)=="float")) {
    s = swrite(format=prompt+" [%f]: ",double(default));
    sres = rdline(,1,prompt=s)(1);
    if (sres == "") return default;
    res = 1.0;
    sread,sres,res;
  } else error,"type not supported";

  return res;
}
