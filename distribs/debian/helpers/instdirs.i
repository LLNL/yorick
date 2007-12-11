#!/usr/bin/yorick -batch
/*
  $Id: instdirs.i,v 1.2 2007-12-11 12:56:54 paumard Exp $
  To be used in Debian packages for Yorick add-ons

  yorick -batch debian/instdirs.i [$DESTDIR]

  Will output the directories for architecture dependent and independent
  files.
  
*/
if (batch()) b=1; else b=0;
batch,0;
#include "htmldoc.i"
batch,b;

if (is_void(argv)) argv=get_argv();
ind=where(strgrep("^-",argv)(2,)==-1);
if (numberof(ind)>=2) {
  DESTDIR=argv(ind(2));
  MAINPACKAGE=package=basename(DESTDIR);
  packages=[package];
  DEBIAN=dirname(DESTDIR)+"/";
}
if (is_void(DEBIAN)) DEBIAN="debian/";
if (numberof(ind)>=3) INFILE= argv(ind(3));

ind=where(strgrep("^-",argv)(2,)!=-1);
if (numberof(ind)) options=argv(ind);
options=grow(get_env("DH_OPTIONS"), options);

if (is_void(packages)) {
  file=hdoc_read_file("debian/control");
  ind=where(strgrep("^Package: ",file)(2,)!=-1);
  packages=strpart(file(ind),strlen("Package: ")+1:0);
  ind=where(strgrep("^Architecture: ",file)(2,)!=-1);
  archs=strpart(file(ind),strlen("Architecture: ")+1:0);
  MAINPACKAGE=packages(1);
}
  
if (anyof(strgrep("^-p",options)(2,)!=-1)) packages=[];
if (anyof(strgrep("^--package=",options)(2,)!=-1)) packages=[];

if (anyof(options=="-i" | options=="--indep"))
  packages=packages (where(archs=="all")); 
if (anyof(options=="-a" | options=="--arch"))
  packages=packages (where(archs!="all")); 
if (anyof(options=="-s" | options=="--same-arch")) {
  f=popen("dpkg-architecture",0);
  line=rdline(f,12);
  close,f;
  arch=pathsplit(line(where(strmatch(line,"DEB_BUILD_ARCH"))),delim="=")(2);
  packages=packages(where(strmatch(archs,arch) | strmatch(archs,"any")));
}
  
for (i=1;i<=numberof(options);i++) {
  option=options(i);
  if (strpart(option,1:strlen("--tmpdir="))=="--tmpdir=") {
    DESTDIR=strpart(option,strlen("--tmpdir=")+1:0);
    continue;
  }
  if (strpart(option,1:strlen("-P"))=="-P") {
    DESTDIR=strpart(option,strlen("-P")+1:0);
    continue;
  }
  if (strpart(option,1:strlen("--infofile="))=="--infofile=") {
    INFILE=strpart(option,strlen("--infofile=")+1:0);
    continue;
  }
  if (strpart(option,1:strlen("--mainpackage="))=="--mainpackage=") {
    MAINPACKAGE=strpart(option,strlen("--mainpackage=")+1:0);
    continue;
  }
  if (strpart(option,1:strlen("--package="))=="--package=") {
    grow,packages,strpart(option,strlen("--package=")+1:0);
    continue;
  }
  if (strpart(option,1:strlen("-p"))=="-p") {
    grow,packages,strpart(option,strlen("-p")+1:0);
    continue;
  }
  if (strpart(option,1:strlen("--no-package="))=="--no-package=") {
    packages=packages(where(packages!=strpart(option,strlen("--no-package=")+1:0)));
    continue;
  }
  if (strpart(option,1:strlen("-N"))=="-N") {
    packages=packages(where(packages!=strpart(option,strlen("-N")+1:0)));
    continue;
  }
  if (option=="-q" | option=="--quiet") {
    quiet=1;
    continue;
  }
}

if (numberof(packages)==1) package=packages(1); else package=MAINPACKAGE;

if (is_void(DESTDIR)) {
  DESTDIR="debian/"+package;
}

DEPDIR=DESTDIR+Y_HOME;
if (Y_SITE==Y_HOME) INDEPDIR=DESTDIR+streplace(Y_HOME,strfind("usr/lib/",Y_HOME),"usr/share/"); else INDEPDIR=DESTDIR+Y_SITE;

if (!quiet) {
  write,format="%s\n",DEPDIR;
  write,format="%s\n",INDEPDIR;
}
