#!/bin/sh
# $Id: libdep.sh,v 1.1 2005-09-18 22:04:17 dhmunro Exp $

# Usage: libdep.sh Y_HOME Y_HOME_ALT pkname1 pkgname2 ...
# returns -L and -l options for given list of libpkgname.a libraries
#   Y_HOME_ALT environment variable used if not given

# remove escaped blanks from Y_HOME
yhome=`echo $1 | sed -e 's/\\\\ / /g'`
shift
yalt=`echo $2 | sed -e 's/\\\\ / /g'`
shift
if test "$#" -lt 1; then
  # no extra packages
  exit 0
fi
if test ! -d "$yhome/lib"; then
  echo +++ libdep.sh missing $yhome/lib +++
  exit 1
fi
if test -z "$yalt"; then
  yalt="$Y_HOME_ALT"
  if test -n "$yalt"; then
    if test ! -d "$yhome/lib"; then
      yalt=""
    fi
  fi
fi
opts=
usealt=no
while test "$#" -gt 0; do
  if test -r "$yhome/lib/lib$1.a"; then
    dep="$yhome/lib/lib$1.a.dep"
  elif test -r "$yalt/lib/lib$1.a"; then
    dep="$yalt/lib/lib$1.a.dep"
    usealt=yes
  else
    dep="/-/-/-"
  fi
  if test -r "$dep"; then
    opt=`cat $dep`
    opts="$opt $opts"
  fi
  opts="-l$1 $opts"
  shift
done
if test $usealt = yes; then
  opts="-L$yalt $opts"
fi
echo $opts
