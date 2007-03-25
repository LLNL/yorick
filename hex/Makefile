# $Id: Makefile,v 1.2 2007-03-25 15:36:32 dhmunro Exp $
# similar to any yorick package Makefile

COPT=$(COPT_DEFAULT)
TGT=$(DEFAULT_TGT)

# ------------------------------------------------ macros for this package
Y_MAKEDIR=..
Y_EXE=../yorick/yorick$(EXE_SFX)
PKG_NAME=hex
PKG_EXENAME=yorick
PKG_I=hex.i

OBJS=yhex.o hex5.o hex24.o hex.o regul.o tools.o store.o hydram.o

PKG_CLEAN=../i0/hex.i ../i/ylmdec.i

# in ordinary yorick packages, these dependencies unnecessary
# they appear here because this package is part of baseline yorick
H_DEPS=../play/plugin.h
H_YDATA=../yorick/ydata.h ../yorick/binio.h ../yorick/hash.h
H_YWRAP=../play/play.h $(H_YDATA) $(H_DEPS)
H_YHEX=../yorick/yio.h $(H_YDATA) ../play/pstdlib.h ../play/pstdio.h $(H_DEPS)

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

yhex.o: hex.h  tools.h
hex5.o: hex.h  tools.h
hex.o: hex.h  tools.h
tools.o: tools.h
store.o: tools.h
hydram.o: hydram.h hex.h  tools.h

../i0/hex.i: hex.i
	cp -f hex.i ../i0/hex.i
../i/ylmdec.i: ylmdec.i
	cp ylmdec.i ../i/ylmdec.i

# if drat and hex are to be statically linked,
# their static libraries must be built before yorick
preexe: $(PKG_LIB) ../i0/hex.i ../i/ylmdec.i
predll:
	@:

# if drat and hex are to be dynamically linked,
# their dynamic libraries must be built after yorick
postdll: $(PKG_DLL) ../i0/hex.i ../i/ylmdec.i
postexe:
	@:

hex.doc:
	echo 'mkdoc,["hex.i","hydra.i"],"hex.doc";quit' | $(Y_EXE) -imkdoc.i

#-------------------------------------- end of Makefile
