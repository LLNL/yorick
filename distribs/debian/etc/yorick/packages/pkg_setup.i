/*
  /etc/yorick/packages/pkg_setup.i
  linked to by /usr/lib/yorick/packages/pkg_setup.i

  Debian configuration for the Yorick package manager, pkg_mngr.i

  See /usr/share/doc/yorick/README.Debian.packages
  Author: Thibaut Paumard <paumard@users.sourceforge.net>

  The preferred way to customize the configuration is through pkg_setup
  (within Yorick). The customized configuration will be saved in 
  /usr/local/lib/yorick/packages/pkg_setup.i , which is read from here.

 */

localtree="/usr/local/lib/yorick/";

PKG_OS = "linux-x86";
PKG_FETCH_CMD = "curl -s";
PKG_SERVER = "http://www.maumae.net/yorick/packages/";
PKG_GUNTAR_CMD = "tar zxf";
PKG_TMP_DIR = localtree+"packages/tmp/";
PKG_VERBOSE = 1;
PKG_ASK_CONFIRM = 1;
PKG_RUN_CHECK = 0;
PKG_VAR_STATE = localtree+"packages/";
PKG_Y_HOME = localtree;
PKG_Y_SITE = localtree;
PKG_OTHER_INSTALLED = Y_SITE+"packages/installed/";

// read packages/pkg_setup.i if it exists
PKG_SETUP=localtree+"packages/pkg_setup.i";
if (open(PKG_SETUP,"r",1)) {
  require,PKG_SETUP;
  sync_done=1;
} else {
  sync_done=0;
}

write,format="\n"+
  " \n"+
  " System-wide configuration for the pkg_mngr loaded.\n"+
  " \n"+
  " Local Y_HOME tree: %s\n"+
  " Local Y_SITE tree: %s\n"+
  " \n"+
  " See /usr/share/doc/yorick/README.Debian.packages.gz for details.\n"+
  " \n",PKG_Y_HOME,PKG_Y_SITE;

f=popen("test -w "+localtree+" ; echo $?",0);
line=rdline(f);
close,f;
if (line=="0" & !sync_done)
  write,format="%s\n","Please run pkg_sync.\nIf your architecture is not i386, please run pkg_setup before.\n";
