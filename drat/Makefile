# $Id: Makefile,v 1.2 2007-03-25 15:36:32 dhmunro Exp $
# similar to any yorick package Makefile

COPT=$(COPT_DEFAULT)
TGT=$(DEFAULT_TGT)

# ------------------------------------------------ macros for this package
Y_MAKEDIR=..
Y_EXE=../yorick/yorick$(EXE_SFX)
PKG_NAME=drat
PKG_EXENAME=yorick
PKG_I=drat.i

OBJS=bound.o track.o trans.o drat.o ydrat.o

PKG_CLEAN=../i0/drat.i

# in ordinary yorick packages, these dependencies unnecessary
# they appear here because this package is part of baseline yorick
H_DEPS=../play/plugin.h
H_YDATA=../yorick/ydata.h ../yorick/binio.h ../yorick/hash.h
H_YWRAP=../play/play.h $(H_YDATA) $(H_DEPS)
H_YDRAT=../yorick/yio.h $(H_YDATA) ../play/pstdlib.h ../play/pstdio.h $(H_DEPS)

# ------------------------------------ standard targets and rules (in Makepkg)

# set macros Makepkg uses in target and dependency names
PKG_I_DEPS=$(PKG_I)

include $(Y_MAKEDIR)/Make.cfg
include $(Y_MAKEDIR)/Makepkg
include $(Y_MAKEDIR)/Make$(TGT)

# override macros set by Makepkg used in rules (because yorick not installed)
Y_IDIR=-I. -I../play -I../yorick
Y_LIBEXE=../yorick
Y_EXE=../yorick/yorick$(EXE_SFX)

# ------------------------------------- targets and rules for this package

bound.o: bound.h
track.o: track.h   bound.h
trans.o: trans.h   track.h bound.h
drat.o: drat.h   trans.h track.h bound.h
ydrat.o: drat.h   trans.h track.h bound.h  $(H_YDRAT)

../i0/drat.i: drat.i
	cp -f drat.i ../i0/drat.i

# if drat and hex are to be statically linked,
# their static libraries must be built before yorick
preexe: $(PKG_LIB) ../i0/drat.i
predll:
	@:

# if drat and hex are to be dynamically linked,
# their dynamic libraries must be built after yorick
postdll: $(PKG_DLL) ../i0/drat.i
postexe:
	@:

#-------------------------------------- end of Makefile
