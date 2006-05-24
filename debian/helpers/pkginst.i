/*
  $Id: pkginst.i,v 1.1 2006-05-24 06:13:26 paumard Exp $
  To be used in Debian packages for Yorick plug-ins

  yorick -batch debian/pkginst.i $DESTDIR [pkg.info]

  For now, will install under DESTDIR/Y_HOME, then move the
  architecture-independent directories at the right place under
  DESTDIR/usr/share and create the right symlinks in DESTDIR/Y_HOME.

  Anything installed in contrib will be moved to i.

  If the name of a .info file is provided on the command line, it will be
  installed in the relevant directory.
  
*/
#include "debian/instdirs.i"

func syscall(command) {
  status=long;
  fh=popen(command+" >/dev/null ; echo $PIPESTATUS",0);
  status=rdline(fh);
  close,fh;
  if (status != "0") error,"\""+command+"\" failed"; 
}

DIRS=["i-start","include","i0","i","g","doc","packages"];//,"contrib"];

syscall,"make DESTDIR="+DESTDIR+" install";

for (d=1;d<=numberof(DIRS);d++) {
  from=DEPDIR+DIRS(d);
  to=INDEPDIR+DIRS(d);
  if (lsdir(DEPDIR+DIRS(d))!=0) {
    mkdirp, INDEPDIR;
    syscall,"mv "+from+" "+to;
  }
}

if (lsdir(DEPDIR+"contrib")!=0) {
  mkdirp,INDEPDIR+"i";
  syscall,"mv "+DEPDIR+"contrib/* "+INDEPDIR+"i/";
  syscall,"rmdir "+DEPDIR+"contrib";
}

temp=DEPDIR;
while (is_void(lsdir(temp)) & temp!=DESTDIR+"/") {
  remove,temp;
  temp=dirname(temp);
}

if (!is_void(INFILE)) {
  mkdirp,DEPDIR+"packages/installed";
  syscall,"cp "+INFILE+" "+INDEPDIR+"packages/installed";
}
