# Makefile for MPY = Message Passing Yorick based on the MPI standard
# $Id: Makefile,v 1.1.1.1 2005-09-18 22:04:57 dhmunro Exp $

Y_MAKEDIR=..
Y_EXE=../yorick/yorick
Y_EXE_PKGS=
Y_EXE_HOME=
Y_EXE_SITE=

#Y_MAKEMPY=$(Y_MAKEDIR)/Makempy
Y_MAKEMPY=./Makempy

# ----------------------------------------------------- optimization flags

COPT=$(COPT_DEFAULT)
#TGT=$(DEFAULT_TGT)
TGT=exe

# ------------------------------------------------ macros for this package

PKG_NAME=mpy
PKG_I=mpy.i

OBJS=mpy.o

PKG_EXENAME=mpy

# PKG_DEPLIBS=-Lsomedir -lsomelib   for dependencies of this package
PKG_DEPLIBS=
# set compiler flags specific to this package
PKG_CFLAGS=$(MPI_CFLAGS)
PKG_LDFLAGS=

EXTRA_PKGS=$(Y_EXE_PKGS)

PKG_CLEAN=

PKG_I_START=
PKG_I_EXTRA=testmp.i

# -------------------------------- standard targets and rules (in Makepkg)

# set macros Makepkg uses in target and dependency names
# DLL_TARGETS, LIB_TARGETS, EXE_TARGETS
# are any additional targets (defined below) prerequisite to
# the plugin library, archive library, and executable, respectively
PKG_I_DEPS=$(PKG_I)

include $(Y_MAKEDIR)/Make.cfg
include $(Y_MAKEDIR)/Makepkg
include $(Y_MAKEDIR)/Make$(TGT)

# override macros Makepkg sets for rules and other macros

# this overrides CC and FC macros, provides MPI_CFLAGS
include $(Y_MAKEMPY)

# Y_HOME and Y_SITE in Make.cfg may not be correct (e.g.- relocatable)
#Y_HOME=$(Y_EXE_HOME)
#Y_SITE=$(Y_EXE_SITE)

# ------------------------------------- targets and rules for this package

install:: Makempy
	$(YNSTALL) Makempy "$(Y_HOME)"

uninstall::
	rm -f "$(Y_HOME)/Makempy"

distclean::
	rm -f Makempy

# -------------------------------------------------------- end of Makefile
