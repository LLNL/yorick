#!/usr/bin/yorick -batch
/*
  $Id: pkginst.i,v 1.3 2007-12-11 16:58:06 paumard Exp $
  To be used in Debian packages for Yorick plug-ins

  It is considered obsolete to call this script as documented below.
  Call dh_installyorick instead, and refer to man dh_installyorick.
  
  yorick -batch debian/pkginst.i $DESTDIR [pkg.info]

  For now, will install under DESTDIR/Y_HOME, then move the
  architecture-independent directories at the right place under
  DESTDIR/usr/share and create the right symlinks in DESTDIR/Y_HOME.

  Anything installed in contrib will be moved to i.

  If the name of a .info file is provided on the command line, it will be
  installed in the relevant directory.
  
*/
quiet=1; // avoid printing out install directories
#include "debian/instdirs.i"
extern VERBOSE, NO_ACT;
if (anyof(options=="-v" | options=="--verbose") | get_env("DH_VERBOSE")=="1")
 VERBOSE=1;
if (anyof(options=="--no-act") | get_env("DH_NO_ACT")=="1") NO_ACT=1;

func syscall(command) {
  if (VERBOSE) write, format="\t%s\n",command;
  if (!NO_ACT) {
    status=long;
    fh=popen(command+" >/dev/null && echo 0",0);
    status=rdline(fh);
    close,fh;
    if (status != "0") error,"\""+command+"\" failed";
  }
}

func mkdirpv (dir) {
  if (VERBOSE) write, format="\tmkdir -p %s\n", dir;
  if (!NO_ACT) mkdirp, dir;
}

func write_snipet (package, stage) {
  fname=DEBIAN+package+"."+stage+".debhelper";
  if (VERBOSE) write, format="\tupdating %s\n", fname;
  if(!NO_ACT) {
    f=open(fname,"a");
    if (stage=="postinst") action="configure";
    else if (stage=="postrm") action="remove";
    write, f, format="%s\n",
      ["# Automatically added by dh_installyorick",
       "if [ \"$1\" = \""+action+"\" ] && "+
       "[ -x \"`which update-yorickdoc 2>/dev/null`\" ]; then",
       "	update-yorickdoc --auto",
       "fi",
       "# End automatically added section"];
    close, f;
  }
}

// single package special case:
// start with "make DESTDIR=debian/package/ install"
DIRS=["i-start","include","i0","i","g","doc","packages"];//,"contrib"];
if (noneof(options=="--no-make-install") & numberof(packages)==1) {
  syscall,"make DESTDIR="+DESTDIR+" install";
  
  for (d=1;d<=numberof(DIRS);d++) {
    from=DEPDIR+DIRS(d);
    to=INDEPDIR+DIRS(d);
    if (lsdir(DEPDIR+DIRS(d))!=0) {
      mkdirpv, INDEPDIR;
      syscall,"mv "+from+" "+to;
    }
  }
  
  if (lsdir(DEPDIR+"contrib")!=0) {
    mkdirpv,INDEPDIR+"i";
    syscall,"mv "+DEPDIR+"contrib/* "+INDEPDIR+"i/";
    syscall,"rmdir "+DEPDIR+"contrib";
  }

  temp=DEPDIR;
  while (is_void(lsdir(temp)) & temp!=DESTDIR+"/") {
    if (VERBOSE) write, format="\trmdir%s\n";
    if (!NO_ACT) remove,temp;
    temp=dirname(temp);
  }
}

// General case: for each packages,
//  1) install various types of files listed in debian/package.*
//  2) add snipets to postinst.debhelper etc...
debian_files=lsdir(DEBIAN,dirs);
for (packn=1;packn<=numberof(packages);packn++) {
  package=packages(packn);
  if (package==MAINPACKAGE) ISMAIN=1; else ISMAIN=0;
  if (!ISMAIN & numberof(packages)>=2) {
    DESTDIR=DEBIAN+package;
    DEPDIR=DESTDIR+Y_HOME;
    if (Y_SITE==Y_HOME) INDEPDIR=
         DESTDIR+streplace(Y_HOME,strfind("usr/lib/",Y_HOME),"usr/share/");
    else INDEPDIR=DESTDIR+Y_SITE;
  }
  // HTML doc building files
  exts=["packinfo","aliases","keywords"];
  for (i=1;i<=numberof(exts);i++) {
    ext=exts(i);
    if (ISMAIN & anyof(debian_files==ext)) file=DEBIAN+ext;
    else file=DEBIAN+package+"."+ext;
    if (f=open(file,"r",1)) {
      line=rdline(f);
      close,f;
      if (strgrep("."+ext+"$",line)(2)!=-1 & !strmatch(line," "))
        file=line;
      mkdirpv,DESTDIR+"/usr/share/yorick-doc";
      syscall,"cp "+file+" "+DESTDIR+"/usr/share/yorick-doc/";
    }
  }
  // package info file
  ext="ynfo";
  if (ISMAIN & anyof(debian_files=="ynfo")) file=DEBIAN+"ynfo";
  else file=DEBIAN+package+".ynfo";
  if (ISMAIN & !is_void(INFILE)) file=INFILE;
  if (f=open(file,"r",1)) {
    line=rdline(f);
    close,f;
    if (strgrep("."+ext+"$",line)(2)!=-1 & !strmatch(line," "))
        file=line;
    if (strgrep(".info$",line)(2)!=-1 & !strmatch(line," "))
        file=line;
    outfile=basename(streplace(file,strfind(".ynfo",file),".info"));
    mkdirpv,INDEPDIR+"packages/installed";
    syscall,"cp "+file+" "+INDEPDIR+"packages/installed/"+outfile;
  }
  // platform independent files
  exts=["i","i0","i-start","g"];
  for (i=1;i<=numberof(exts);i++) {
    ext=exts(i);
    if (ISMAIN & anyof(debian_files==ext)) file=DEBIAN+ext;
    else file=DEBIAN+package+"."+ext;
    if (open(file,"r",1)) {
      line=hdoc_read_file(file);
      mkdirpv,INDEPDIR+ext;
      for (j=1;j<=numberof(line);j++) syscall,"cp "+line(j)+" "+INDEPDIR+ext;
    }
  }
  // platform dependent files
  exts=["lib"];
  for (i=1;i<=numberof(exts);i++) {
    ext=exts(i);
    if (ISMAIN & anyof(debian_files==ext)) file=DEBIAN+ext;
    else file=DEBIAN+package+"."+ext;
    if (open(file,"r",1)) {
      line=hdoc_read_file(file);
      mkdirpv,DEPDIR+ext;
      for (j=1;j<=numberof(line);j++) syscall,"cp "+line(j)+" "+DEPDIR+ext;
    }
  }
  // add maintainer script snipets
  if (noneof(options=="-n")) {
    write_snipet, package, "postinst";
    write_snipet, package, "postrm";
  }
}
