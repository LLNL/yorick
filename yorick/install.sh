#!/bin/sh
# $Id: install.sh,v 1.1 2005-09-18 22:04:17 dhmunro Exp $

# Usage: install.sh [- | group] [file1 [file2 ...]] dir
# if dir does not exist, creates it
# when group is not -, assumes group sharable setup, and
#   gives group write and set group id permission
# when group is -, group permissions untouched
# in either case, every file in the directory is affected
#   also, sets u+w to handle read-only sourcecode control systems

group="$1"
if test "$group" = "-"; then
  group=""
fi
shift
if test "$#" -lt 1; then
  echo "install.sh: bad command line"
  exit 1
fi
has_files=
fils=
while true; do
  if test "$#" -eq 1; then break; fi
  fils="$fils $1"
  has_files=yes
  shift
done

if test ! -d "$1"; then
  mkdir -p "$1";
fi
if test -n "$group"; then
  chgrp $group "$1"; chmod g+w "$1"; chmod g+s "$1"
fi
if test -n "$has_files"; then
  cp -f $fils "$1"; chmod u+w "$1"/*
  if test -n "$group"; then
    chgrp $group "$1"/*; chmod g+w "$1"/*
  fi
fi
