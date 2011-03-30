#!/bin/sh
# $Id: instally.sh,v 1.1 2005-09-18 22:03:40 dhmunro Exp $

# set Y_SITE, Y_HOME environment variables
if test -r Make.cfg; then
  Y_SITE=`grep '^Y_SITE=' Make.cfg | sed -e 's/^Y_SITE=//'`
  Y_HOME=`grep '^Y_HOME=' Make.cfg | sed -e 's/^Y_HOME=//'`
  PLAY_DIRS=`grep '^PLAY_DIRS=' Make.cfg | sed -e 's/^PLAY_DIRS=//'`
  YWIN_LIB=`grep '^YWIN_LIB=' Make.cfg | sed -e 's/^YWIN_LIB=//'`
else
  echo instally.sh: Make.cfg missing -- cannot install before make config
  exit 1
fi

home_only=no
un_install=no
case "$1" in
  +home) home_only=yes ;;
  -home) home_only=yes; un_install=yes ;;
  +both) ;;
  -both) un_install=yes ;;
  *) echo "instally.sh: FATAL, damaged Makefile"; exit 1 ;;
esac

if test -n "$2"; then
  Y_SITE="$2$Y_SITE"
  Y_HOME="$2$Y_HOME"
fi

if test -n "$3"; then
  Y_BINDIR="$3"
else
  Y_BINDIR="$Y_HOME/bin"
fi

if test -n "$4"; then
  Y_DOCDIR="$4"
  if test "$Y_DOCDIR" = /dev/null; then Y_DOCDIR=""; fi
else
  Y_DOCDIR="$Y_SITE/doc"
fi

EXE_SFX=`grep '^EXE_SFX=' Make.cfg | sed -e 's/^EXE_SFX=//'`

if test $un_install = yes; then
echo "********************* uninstalling architecture-dependent files from"
echo Y_HOME="$Y_HOME"
rm -f "$Y_HOME"/junk.tst
touch ./junk.tst
if test -f "$Y_HOME"/junk.tst; then
  for sub in include lib bin; do rm -rf "$Y_HOME"/$sub; done
  rm -f "$Y_HOME"/Make* "$Y_HOME"/install*
else
  rm -rf "$Y_HOME"
fi
rm -rf "$Y_BINDIR/yorick$EXE_SFX"
rm -rf "$Y_BINDIR/gist$EXE_SFX"
rm -f ./junk.tst

if test $home_only = yes; then exit 0; fi
echo "********************* uninstalling architecture-independent files from"
echo Y_SITE="$Y_SITE"
rm -f "$Y_SITE/junk.tst"
touch ./junk.tst
if test -f "$Y_SITE/junk.tst"; then
  rm -rf "$Y_SITE/man"
else
  rm -rf "$Y_SITE"
fi
rm -f ./junk.tst
# note: this does not uninstall non-default Y_DOCDIR

else
echo "********************* installing architecture-dependent files to"
echo Y_HOME="$Y_HOME"
if test -r ysite.grp; then
  YGP=`cat ysite.grp`
fi
if test -z "$YGP"; then YGP="-"; fi
YNSTALL=yorick/install.sh
$YNSTALL $YGP "$Y_HOME"
$YNSTALL $YGP "$Y_HOME/i-start"
$YNSTALL $YGP "$Y_HOME/include"
$YNSTALL $YGP "$Y_HOME/lib"
if test -n "$3"; then
  $YNSTALL $YGP "$Y_BINDIR"
else
  $YNSTALL $YGP "$Y_HOME/bin"
fi
rm -f "$Y_HOME/lib/install.grp"
echo $YGP > "$Y_HOME/lib/install.grp"
if test "$PLAY_DIRS" = "win"; then
  hconfig=play/win/config.h
else
  hconfig=play/unix/config.h
fi
$YNSTALL $YGP $hconfig play/*.h gist/*.h yorick/*.h "$Y_HOME/include"
touch ./junk.tst
if test -f "$Y_HOME/junk.tst"; then
  :
else
  $YNSTALL $YGP LICENSE.md Make.cfg Makepkg Makeexe Makedll "$Y_HOME"
fi
rm -f ./junk.tst
$YNSTALL $YGP $YNSTALL yorick/libdep.sh "$Y_HOME/lib"
$YNSTALL $YGP yorick/libyor.a yorick/main.o yorick/codger$EXE_SFX "$Y_HOME/lib"
if test -n "$YWIN_LIB"; then
  $YNSTALL $YGP yorick/$YWIN_LIB "$Y_HOME/lib"
fi
RANLIB=`grep '^RANLIB=' Make.cfg | sed -e 's/^RANLIB=//'`
$RANLIB "$Y_HOME/lib/libyor.a"
if test -r drat/libdrat.a; then
  cp -f drat/libdrat.a "$Y_HOME/lib"
  $RANLIB "$Y_HOME/lib/libdrat.a"
fi
if test -r hex/libhex.a; then
  cp -f hex/libhex.a "$Y_HOME/lib"
  $RANLIB "$Y_HOME/lib/libhex.a"
fi
PLUG_SFX=`grep '^PLUG_SFX=' Make.cfg | sed -e 's/^PLUG_SFX=//'`
if test -r drat/drat$PLUG_SFX; then
  $YNSTALL $YGP drat/drat$PLUG_SFX "$Y_HOME/lib"
fi
if test -r hex/hex$PLUG_SFX; then
  $YNSTALL $YGP hex/hex$PLUG_SFX "$Y_HOME/lib"
fi
$YNSTALL $YGP yorick/yorick$EXE_SFX "$Y_BINDIR"
$YNSTALL $YGP gist/gist$EXE_SFX "$Y_BINDIR"

if test $home_only = yes; then exit 0; fi
echo "********************* installing architecture-independent files to"
echo Y_SITE="$Y_SITE"

$YNSTALL $YGP "$Y_SITE"
$YNSTALL $YGP "$Y_SITE/i-start"
$YNSTALL $YGP "$Y_SITE/i0"
$YNSTALL $YGP "$Y_SITE/i"
$YNSTALL $YGP "$Y_SITE/g"
if test -n "$Y_DOCDIR"; then
  $YNSTALL $YGP "$Y_DOCDIR"
fi
rm -f "$Y_SITE/junk.tst"
touch ./junk.tst
if test -f "$Y_SITE/junk.tst"; then
  :
else
  $YNSTALL $YGP i/*.i i/README "$Y_SITE/i"
  $YNSTALL $YGP i0/*.i i0/README "$Y_SITE/i0"
  $YNSTALL $YGP i-start/*.i i-start/README "$Y_SITE/i-start"
  $YNSTALL $YGP g/*.gs g/*.gp g/ps.ps g/README "$Y_SITE/g"
  if test -n "$Y_DOCDIR"; then
    xd="doc/FILE_FORMATS doc/README"
    $YNSTALL $YGP doc/*.tex doc/*.pdf $xd doc/*.doc "$Y_DOCDIR"
  fi
fi
rm -f ./junk.tst

fi
