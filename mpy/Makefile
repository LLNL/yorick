# Makefile for MPY = Message Passing Yorick based on the MPI standard

Y_MAKEDIR=
Y_EXE=
Y_EXE_PKGS=
Y_EXE_HOME=
Y_EXE_SITE=
Y_HOME_PKG=

# note: to be fully scalable, mpy cannot use dll packages; all
# packages should be statically linked via Y_EXE_PKGS or EXTRA_PKGS

# mpy needs custom main.c, assume this directory is a
# sibling of the play/ directory in the distribution
Y_MAIN_SRC=../play/$(Y_MAIN_C)
Y_MAIN_SRC=$(Y_EXE_HOME)/../play/$(Y_MAIN_C)

#Y_MAKEMPY=$(Y_MAKEDIR)/Makempy
Y_MAKEMPY=./Makempy

# ----------------------------------------------------- optimization flags

COPT=$(COPT_DEFAULT)
#TGT=$(DEFAULT_TGT)
TGT=exe

# ------------------------------------------------ macros for this package

PKG_NAME=mpy
PKG_I=mpy.i

OBJS=mpy.o mpyfile.o

PKG_EXENAME=mpy

# PKG_DEPLIBS=-Lsomedir -lsomelib   for dependencies of this package
PKG_DEPLIBS=
# set compiler flags specific to this package
PKG_CFLAGS=$(MPI_CFLAGS)
PKG_LDFLAGS=$(MPI_LDFLAGS)

EXTRA_PKGS=$(Y_EXE_PKGS)

PKG_CLEAN=

PKG_I_START=
PKG_I_EXTRA=custommp.i testmp.i mpool.i mpy1.i

# -------------------------------- standard targets and rules (in Makepkg)

# set macros Makepkg uses in target and dependency names
# DLL_TARGETS, LIB_TARGETS, EXE_TARGETS
# are any additional targets (defined below) prerequisite to
# the plugin library, archive library, and executable, respectively
PKG_I_DEPS=$(PKG_I)
Y_DISTMAKE=distmake
EXE_TARGETS=mpymain.o paths.i

include $(Y_MAKEDIR)/Make.cfg
include $(Y_MAKEDIR)/Makepkg
include $(Y_MAKEDIR)/Makeexe

# override macros Makepkg sets for rules and other macros
Y_MAIN_O=mpymain.o

# this overrides CC and FC macros, provides MPI_CFLAGS
include $(Y_MAKEMPY)

# above appropriate for TGT=dll, below because this is TGT=exe
Y_BINDIR=$(Y_EXE_HOME)/bin
Y_LIBEXE=$(Y_EXE_HOME)/lib
Y_INCLUDE=$(Y_EXE_HOME)/include
DEST_Y_SITE=$(DESTDIR)$(Y_EXE_SITE)
DEST_Y_HOME=$(DESTDIR)$(Y_EXE_HOME)

# reduce chance of yorick-1.5 corrupting this Makefile
MAKE_TEMPLATE = /usr/lib/yorick/1.5/protect-against-1.5

# ------------------------------------- targets and rules for this package

# override on_launch callback with scalable MPI version
D_ON_LAUNCH=-Don_launch=mpy_on_launch
# always compile main.c with -g so new packages can be debugged
mpymain.o: mpymain.c
	$(CC) $(CPPFLAGS) $(D_ON_LAUNCH) -g $(Y_IDIR) -o $@ -c mpymain.c
mpymain.c: $(Y_MAIN_SRC)
	cp $(Y_MAIN_SRC) mpymain.c

# need paths.i to be able to run TGT=exe in this directory
# after install it is not needed
paths.i:
	echo >paths.i 'set_site,"$(Y_EXE_SITE)","$(Y_EXE_HOME)";include,"$(Y_EXE_SITE)/i0/paths.i",1;'

install:: Makempy mpy.h mpymain.o
	$(YNSTALL) Makempy "$(DEST_Y_HOME)"
	$(YNSTALL) mpy.h "$(DEST_Y_HOME)/include"
	$(YNSTALL) mpymain.o "$(DEST_Y_HOME)/lib"

uninstall::
	rm -f "$(DEST_Y_HOME)/Makempy"
	rm -f "$(DEST_Y_HOME)/include/mpy.h"
	rm -f "$(DEST_Y_HOME)/lib/mpymain.o"

clean::
	rm -f mpymain.* paths.i

distclean::
	rm -f Makempy

# -------------------------------------------------------- end of Makefile
