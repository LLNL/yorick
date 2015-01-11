#! /bin/sh
# $Id: yconfig.sh,v 1.1 2005-09-18 22:04:15 dhmunro Exp $

echo ""
echo "  ============ begin yorick configuration ============="
echo ""

rm -f cfg*
# debug=yes to keep all cfg.* stdout and stderr files
debug=no

fatality=0

here=`cd ..;pwd`
# remove any /tmp_mnt automounter garbage from name of current directory
if echo "$here" | grep '^/tmp_mnt'; then
  tmp=`echo "$here" | sed -e s%\^/tmp_mnt%%`
  if test -d "$tmp"; then
    here="$tmp";
  else
    tmp=`echo "$tmp" | sed -e s%\^/[\^/]*%%`
    if test -d "$tmp"; then
      here="$tmp";
    fi
  fi
fi

VERSION=`head -1 ../VERSION`
. ../ysite.sh

if test -z "$Y_SITE"; then
  Y_SITE="$here"
else
  if test "$Y_SITE" = "."; then
    Y_SITE="$here"
  fi
fi
if test "$Y_HOME" = "."; then Y_HOME=""; fi
if test "$Y_PLATFORM" = "."; then Y_PLATFORM=""; fi
if test -z "$Y_HOME"; then
  if test -z "$Y_PLATFORM"; then
    Y_PLATFORM=`uname -s`-`uname -m`
    Y_PLATFORM=`echo $Y_PLATFORM | sed -e "s/ /_/g"`
  fi
  Y_HOME="$Y_SITE/$Y_PLATFORM"
fi
if test -z "$Y_BINDIR"; then
  Y_BINDIR='$(Y_EXE_HOME)/bin'
fi

Y_VERSION="$VERSION".`tail -n 1 ../VERSION`
echo "Y_VERSION=$Y_VERSION" >>../Make.cfg
echo "Y_SITE=$Y_SITE" >>../Make.cfg
echo "Y_HOME=$Y_HOME" >>../Make.cfg
echo "Y_BINDIR=$Y_BINDIR" >>../Make.cfg
echo "Y_CFG_SITE=$Y_SITE" >>../Make.cfg
echo "Y_CFG_HOME=$Y_HOME" >>../Make.cfg

# extract global variables from ../Make.cfg
CC=`grep '^CC=' ../Make.cfg | sed -e s/CC=//`
COPTIONS=`grep '^Y_CFLAGS=' ../Make.cfg | sed -e s/Y_CFLAGS=//`
LDOPTIONS=`grep '^Y_LDFLAGS=' ../Make.cfg | sed -e s/Y_LDFLAGS=//`
MATHLIB=`grep '^MATHLIB=' ../Make.cfg | sed -e s/MATHLIB=//`
FPELIB=`grep '^FPELIB=' ../Make.cfg | sed -e s/FPELIB=//`
if test -z "$FPELIB"; then
  LOWLIBS="$MATHLIB"
else
  LOWLIBS="$MATHLIB" "$FPELIB"
fi
X11LIB=`grep '^X11LIB=' ../Make.cfg | sed -e s/X11LIB=//`
RANLIB=`grep '^RANLIB=' ../Make.cfg | sed -e s/RANLIB=//`
AR=`grep '^AR=' ../Make.cfg | sed -e s/AR=//`

if test "$HACK103" = "yes"; then
  echo "will emulate hypot to avoid system sqrt (HACK103)"
  NO_HYPOT="-DNO_HYPOT"
else
cat >cfg.c <<EOF
int main(int argc, char *argv[])
{
  double x=hypot(3.,4.);
  return (x<4.99999)||(x>5.00001);
}
EOF
if $CC $COPTIONS -o cfg cfg.c $LDOPTIONS $LOWLIBS >cfg.00 2>&1; then
  echo "using hypot found in libm"
  NO_HYPOT=""
else
  echo "libm does not contain hypot, will emulate"
  NO_HYPOT="-DNO_HYPOT"
fi
fi
echo "NO_HYPOT=$NO_HYPOT" >>../Make.cfg

cat >cfg.c <<EOF
int main(int argc, char *argv[])
{
  double x=exp10(3.);
  return (x<999.999)||(x>1000.001);
}
EOF
if $CC $COPTIONS -o cfg cfg.c $LDOPTIONS $LOWLIBS >cfg.00 2>&1; then
  echo "using exp10 found in libm"
  NO_EXP10=""
else
  echo "libm does not contain exp10, will emulate"
  NO_EXP10="-DNO_EXP10"
fi
echo "NO_EXP10=$NO_EXP10" >>../Make.cfg

if test -z "$FC" && make echofc >/dev/null 2>&1; then
  FC=`cat cfg.tmp`
  if test -z "$FC"; then FC=g77; fi
fi
rm -f cfg.tmp
FC=f77
FORTRAN_LIBS=
if test -z "$FORTRAN_LINKAGE"; then
  FORTRAN_LINKAGE=-Df_linkage_
fi
CXX=CC
echo "FORTRAN_LINKAGE=$FORTRAN_LINKAGE" >>../Make.cfg

#----------------------------------------------------------------------
# clean up, issue warning if compiler gave fishy output
rm -f cfg cfg.c cfg.o
for f in cfg.[0-9]*; do
  if grep ... $f >/dev/null 2>&1; then   # or use test -s $f ?
    if test $debug = yes; then echo "WARNING - check compiler message in $f"
    else rm -f $f; fi
  else # remove empty files
    rm -f $f
  fi
done

echo "appended to ../Make.cfg"
echo ""
echo "  ============= end yorick configuration =============="

exit $fatality
