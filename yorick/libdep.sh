#!/bin/sh
# $Id: libdep.sh,v 1.1 2005-09-18 22:04:17 dhmunro Exp $

# Usage: libdep.sh Y_HOME pkname1 pkgname2 ...
# returns -L and -l options for given list of libpkgname.a libraries

# remove escaped blanks from Y_HOME
yhome=`echo $1 | sed -e 's/\\\\ / /g'`
shift
if test "$#" -lt 1; then
  # no extra packages
  exit 0
fi
if test ! -d "$yhome/lib"; then
  echo +++ libdep.sh missing $yhome/lib +++
  exit 1
fi
opts=
while test "$#" -gt 0; do
  dep="$yhome/lib/lib$1.a.dep"
  if test -r "$dep"; then
    opt=`cat $dep`
    opts="$opt $opts"
  fi
  opts="-l$1 $opts"
  shift
done
echo $opts
