#!/bin/sh
prefix="."
Y_PLATFORM="."
Y_SITE="."
Y_HOME="relocate"

# -------------- DO NOT CHANGE THIS LINE OR BELOW -----------------------
# ysite.sh -- $Id: ysite.sh,v 1.4 2010-02-28 21:52:28 dhmunro Exp $
# the install.sh script sources this script in order to set the
# Y_SITE and Y_HOME variables that determine where the architecture
# independent and dependent parts of yorick will be installed

# you can:

# (0) set Y_HOME to an absolute or relative pathname, Y_SITE="."
#     to get
#       Y_SITE=$Y_HOME
#     The recommended value is Y_HOME="relocate" which makes a
#     relocatable version of yorick.

# (1) set Y_SITE, Y_HOME, and Y_PLATFORM to ".",
#     and prefix to an absolute pathname (usually /usr or /usr/local)
#     to get
#       Y_SITE=$prefix/share/yorick/$Y_VERSION
#       Y_HOME=$prefix/lib/yorick/$Y_VERSION

# (2) set Y_SITE and Y_HOME to ".",
#     prefix to an absolute pathname, and Y_PLATFORM to an
#     architecture name (e.g.- linux86, compaq, sunos, etc)
#     to get
#       Y_SITE=$prefix/yorick/$Y_VERSION
#       Y_HOME=$prefix/yorick/$Y_VERSION/$Y_PLATFORM

# (3) set Y_HOME and prefix to ".",
#     Y_SITE to an absolute pathname or to ".", and Y_PLATFORM to an
#     architecture name (e.g.- linux86, compaq, sunos, etc)
#     to get
#       Y_HOME=$Y_SITE/$Y_PLATFORM

# (4) set both Y_SITE and Y_HOME to absolute pathnames

# an "absolute pathname" begins with / and does NOT include a trailing /

# the remainder of this script sets the values of Y_SITE and Y_HOME
# according to (0-4) above so it can be sourced by install.sh

if test "$Y_SITE" = "."; then
  if test "$Y_HOME" = "."; then
    if test "$prefix" != "."; then
      # prefix meaningful only if neither Y_SITE nor Y_HOME set
      if test "$Y_PLATFORM" = "."; then
        Y_SITE=$prefix/share/yorick/$VERSION
        Y_HOME=$prefix/lib/yorick/$VERSION
      else
        Y_SITE=$prefix/yorick/$VERSION
      fi
    fi
  else
    Y_SITE=$Y_HOME
  fi
else
  if test "$Y_HOME" = "."; then
    if test "$Y_PLATFORM" = "."; then
      Y_HOME=$Y_SITE
    fi
  fi
fi
# when Y_PLATFORM set, ignore any other definition of Y_HOME
if test "$Y_PLATFORM" != "."; then
  Y_HOME=$Y_SITE/$Y_PLATFORM
fi
