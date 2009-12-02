#!/usr/bin/yorick -batch
/*
  $Id: pkginst.i,v 1.7 2009-12-02 16:01:03 paumard Exp $
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
extern VERBOSE, NO_ACT, DESTDIR;
quiet=1; // avoid printing out install directories
#include "debian/instdirs.i"
if (anyof(options=="-v" | options=="--verbose") | get_env("DH_VERBOSE")=="1")
 VERBOSE=1;
if (anyof(options=="--no-act") | get_env("DH_NO_ACT")=="1") NO_ACT=1;

func make_link(target, link) {
  starget=pathsplit(target,delim="/");
  slink=pathsplit(link,delim="/");
  shortest=min(numberof(starget),numberof(slink));
  fdif=(starget(1:shortest)==slink(1:shortest))(mnx);
  target=pathform(starget(fdif:0),delim="/");
  if (pathform(starget(1:fdif-1),delim="/")+"/"==DESTDIR) {
    target="/"+target;
  } else {
    for (i=1;i<=numberof(slink)-fdif;i++) target="../"+target;
  }
  mkdirpv,dirname(link);
  syscall,"ln -s "+target+" "+link;
}

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
DIRS=["i-start","include","i0","i","g","doc","packages",
      "python","glade","data"];
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

  if (INFILE) {
    dest=INDEPDIR+"packages/installed";
    mkdirpv,dest;
    syscall,"cp "+INFILE+" "+dest;
  }
}

// General case: for each packages,
//  1) install various types of files listed in debian/package.*
//  2) add snipets to postinst.debhelper etc...
debian_files=lsdir(DEBIAN,dirs);
sy_site=pathsplit(Y_SITE,delim="/");
dy_site=numberof(sy_site); // depth of Y_SITE
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

  ext="ynstall";
  if (ISMAIN & anyof(debian_files==ext)) file=DEBIAN+ext;
  else file=DEBIAN+package+"."+ext;
  if (open(file,"r",1)) {
    line=hdoc_read_file(file);
    for (j=1;j<=numberof(line);j++) {
      words=strpart(line(j),strword(line(j),,3));
      file=words(1);
      if (!file) continue;
      if (strpart(file,[0,1])=="#") continue;
      is_exec=0;
      if (words(2)) {
        // we've got a destination.
        dest=words(2);
        sdest=pathsplit(dest,delim="/");
        if (numberof(sdest)>=dy_site && sdest(1:dy_site)==sy_site){
          sdest=grow(["Y_SITE"],sdest(dy_site+1:));
          dest=pathform(sdest,delim="/");
        }
        if (anyof(sdest(1)==DIRS)) dest=INDEPDIR+dest;
        else if (anyof(sdest(1)==["lib","bin"])) dest=DEPDIR+dest;
        else {
          if (sdest(1)=="Y_HOME") dest=DEPDIR+pathform(sdest(2:),delim="/");
          else if (sdest(1)=="Y_SITE") {
            dest=pathform(sdest(2:),delim="/");
            make_link,INDEPDIR+dest+"/"+basename(file),
              DEPDIR+dest+"/"+basename(file);
            dest=INDEPDIR+dest;
            mkdirpv,dest;
          } else dest=DESTDIR+"/"+dest;
        }
      } else {
        // guess destination
        ext=pathsplit(file,delim=".")(0);
        if (ext=="i") dest=INDEPDIR+"i"; // so i0 and i-sart must be specified
        else if (anyof(ext==["gs","gp"])) dest=INDEPDIR+"g";
        else if (ext=="info") dest=INDEPDIR+"packages/installed";
        else if (ext=="py") dest=INDEPDIR+"python";
        else if (ext=="glade") dest=INDEPDIR+"glade";
        else if (ext=="so") dest=DEPDIR+"lib";
        else if (anyof(ext==["packinfo","keywords","aliases"]))
          dest=DESTDIR+"/usr/share/yorick-doc";
      }
      mkdirpv,dest;
      syscall,"cp "+file+" "+dest;
      if (words(3)) {
        // we've got a link.
        link=pathsplit(words(3),delim="/");
        if (link(1)=="Y_HOME") link=DEPDIR+pathform(link(2:),delim="/");
        else if (link(1)=="Y_SITE") link=INDEPDIR+pathform(link(2:),delim="/");
        else if (anyof(link(1)==DIRS)) link=INDEPDIR+words(3);
        else if (anyof(link(1)==["lib","bin"])) link=DEPDIR+words(3);
        else link=DESTDIR+"/"+words(3);
        make_link,dest+"/"+basename(file),link;
        if (anyof(basename(dirname(link))==["bin","sbin"])) is_exec=1;
      }
      if (anyof(basename(dest)==["bin","sbin","python"])) is_exec=1;
      if (is_exec) syscall,"chmod a+x "+dest+"/"+basename(file);
    }       
  }
  // add maintainer script snipets
  if (noneof(options=="-n") & anyof(options=="-m")) {
    write_snipet, package, "postinst";
    write_snipet, package, "postrm";
  }
}
