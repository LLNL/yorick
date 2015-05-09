# $Id: Makefile,v 1.6 2010-08-13 09:46:16 thiebaut Exp $
# similar to any yorick package Makefile

# ------------------------------------------------ macros for yor base package
COPT=$(COPT_DEFAULT)
Y_MAKEDIR=..
Y_EXE=./yorick$(EXE_SFX)
PKG_NAME=yor
PKG_EXENAME=yorick
PKG_I=std.i matrix.i fft.i graph.i
PKG_I_DEPS=../i0/std.i ../i0/matrix.i ../i0/fft.i ../i0/graph.i
PKG_I_DIR=-I../i0

OBJS = opsv.o task.o yorick.o yinput.o parse.o ydata.o yapi.o yio.o fnctn.o \
  array.o range.o ops.o ops0.o ops1.o ops2.o ops3.o nonc.o debug.o \
  std0.o std1.o std2.o ascio.o defmem.o yhash.o  yrdwr.o bcast.o binio.o \
  binobj.o binstd.o cache.o convrt.o binpdb.o clog.o ystr.o graph.o fwrap.o \
  graph0.o style.o list.o pathfun.o autold.o funcdef.o spawn.o fortrn.o oxy.o \
  mdigest.o socky.o

PKG_CLEAN=libyor main.* prmtyp.h codger$(EXE_SFX) lib$(PKG_NAME).a $(PKG_EXENAME) yorapi*

PLAPI=../play/plugin.h
PLAYONLY=../play/play.h $(PLAPI)

DIST_LIBS=-L../drat -ldrat -L../hex -lhex
DIST_DIRS=../drat ../hex
# Make.cfg sets IF_TGT according to DEFAULT_TGT
IF_EXE=drat hex
IF_DLL=

# ------------------------------------ standard targets and rules (in Makepkg)

# set macros Makepkg uses in target and dependency names
H_YWRAP=$(PLAYONLY) ydata.h yapi.h binio.h hash.h
CODGER_DEP=codger$(EXE_SFX)
EXE_TARGETS=main.o $(YWIN_LIB) prepkg
LIB_TARGETS=libyor
ALL_TARGETS=postpkg
TGT=exe

include $(Y_MAKEDIR)/Make.cfg
include $(Y_MAKEDIR)/Makepkg
include $(Y_MAKEDIR)/Make$(TGT)

# override macros set by Makepkg used in rules (because yorick not installed)
Y_MAIN_O=main.o
Y_IDIR=-I. -I../play -I../gist
Y_LIBEXE=.
Y_LDIR=-L../drat -L../hex
PKG_L_OPT=-L.
ARFLAGS=r
EXTRA_LIBS=$(ALT_LIBS)
EXTRA_PKGS=$(IF_TGT)

# hack combines gist, matrix, and fft libs into a single libyor.a
# (this does not get inter-directory dependencies absolutely correct)
BUILTIN_PKGS=../gist/libgist.a ../regexp/libyor ../matrix/libyor ../fft/libyor
libyor: $(BUILTIN_PKGS)
	@rm -f $(PKG_LIB)
	cp ../gist/libgist.a $(PKG_LIB)
	cd ../regexp; $(MAKE) "COPT=$(COPT)" oinsert
	cd ../matrix; $(MAKE) "COPT=$(COPT)" oinsert
	cd ../fft; $(MAKE) "COPT=$(COPT)" oinsert
	touch $@
../gist/libgist.a:
	@cd ../gist; $(MAKE) "COPT=$(COPT)" libgist.a
../regexp/libyor:
	@cd ../regexp; $(MAKE) "COPT=$(COPT)" libyor
../matrix/libyor:
	@cd ../matrix; $(MAKE) "COPT=$(COPT)" libyor
../fft/libyor:
	@cd ../fft; $(MAKE) "COPT=$(COPT)" libyor

# MSWindows and AIX need yorapi.def for plugins to work (see ../yorapi.sh)
yorapi.def:
	cd ..; ./yorapi.sh; mv yorapi.def yorick/

# if drat and hex are to be statically linked,
# their static libraries must be built before yorick
M_V1="TGT=exe" pre$(DISTRIB_TGT)
prepkg:
	@for d in $(DIST_DIRS); do ( cd $$d; $(MAKE) "COPT=$(COPT)" $(M_V1); ); done

# if drat and hex are to be dynamically linked,
# their dynamic libraries must be built after yorick
M_V2="TGT=dll" post$(DISTRIB_TGT)
postpkg: $(PKG_EXE)
	@for d in $(DIST_DIRS); do ( cd $$d; $(MAKE) "COPT=$(COPT)" $(M_V2); ); done

# two colons means these rules appended to Makepkg rule
clean::
	@for d in $(DIST_DIRS); do ( cd $$d; $(MAKE) TGT=exe clean; ); done
distclean::
	@for d in $(DIST_DIRS); do ( cd $$d; $(MAKE) TGT=exe distclean; ); done

# ------------------------------------- targets and rules for yor base package
# ydata.h: yapi.h binio.h hash.h
# binio.h: hash.h
# yio.h: hash.h pstdio.h
# defmem.h: pstdlib.h
# bcast.h: ydata.h   binio.h hash.h
HSHAPI=hash.h ../play/plugin.h
HSH=hash.h
YDATA_H=ydata.h yapi.h binio.h $(HSHAPI)
YDATA_HP=ydata.h yapi.h binio.h $(HSH)
HGIST=../gist/gist.h ../gist/hlevel.h
PSLIB=../play/pstdlib.h $(PLAPI)
PSLIO=../play/pstdlib.h ../play/pstdio.h $(PLAPI)
PSPLAY=../play/pstdlib.h ../play/play.h $(PLAPI)
PLAYALL=../play/play.h ../play/pstdlib.h ../play/pstdio.h $(PLAPI)

array.o: $(YDATA_H)
ascio.o: $(YDATA_HP) yio.h defmem.h $(PLAYALL)
autold.o: $(YDATA_HP) yio.h defmem.h $(PSLIO)
bcast.o: bcast.h   $(YDATA_H)
binio.o: $(YDATA_HP) yio.h defmem.h $(PSLIO)
binobj.o: bcast.h defmem.h $(PSLIB)   $(YDATA_HP)
binpdb.o: $(YDATA_HP) yio.h $(PSLIO)
binstd.o: binio.h $(PSLIB)   $(HSH)
cache.o: binio.h $(PSLIO)   $(HSH)
clog.o: $(YDATA_HP) yio.h $(PSLIO)
convrt.o: binio.h $(PSLIB)   $(HSH)
debug.o: $(YDATA_HP) yio.h $(PLAYALL)
defmem.o: defmem.h   $(PSLIB)
fnctn.o: $(YDATA_H) $(PLAYONLY)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_USE_SOFTFPE) -o $@ -c fnctn.c
fortrn.o: fortrn.c yasync.h $(PSLIB)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(FORTRAN_LINKAGE) -o $@ -c fortrn.c
graph.o: graph.c $(YDATA_HP) yio.h   $(PLAYALL) $(HGIST)
graph0.o: $(YDATA_H)
yhash.o: $(HSH) defmem.h $(PSLIB)
list.o: $(YDATA_H) defmem.h
mdigest.o: mdigest.h
nonc.o: nonc.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(NO_HYPOT) -o $@ -c nonc.c
ops.o: bcast.h $(PSPLAY)   ydata.h binio.h $(HSH)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_USE_SOFTFPE) -o $@ -c ops.c
ops0.o: $(YDATA_H)
ops1.o: $(YDATA_H)
ops2.o: $(YDATA_HP)   $(PSLIB)
ops3.o: bcast.h $(PSPLAY)   $(YDATA_HP)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_USE_SOFTFPE) -o $@ -c ops3.c
opsv.o: $(YDATA_HP) $(PSLIB) prmtyp.h
oxy.o: $(YDATA_HP) $(PSLIB) ../play/phash.h
parse.o: parse.h $(YDATA_HP) $(PSLIB)
pathfun.o: yio.h $(PSLIO)   $(HSH)
range.o: $(YDATA_HP) $(PSLIB)
socky.o: socky.c $(PSPLAY) yapi.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_NO_SOCKETS) -o $@ -c socky.c
std0.o: std0.c bcast.h yio.h defmem.h $(PLAYALL)   ydata.h binio.h $(HSH)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_Y_VERSION) -o $@ -c std0.c
std1.o: $(YDATA_HP) $(PSPLAY)
std2.o: mdigest.h $(YDATA_HP) yio.h $(PLAYALL)
style.o: style.c $(YDATA_HP) $(PSLIB) $(HGIST) ../gist/draw.h
task.o: $(YDATA_HP) yio.h yapi.h $(PLAYALL)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_USE_SOFTFPE) -o $@ -c task.c
yapi.o: $(YDATA_HP) yio.h bcast.h $(PSLIB)
ydata.o: bcast.h defmem.h $(PSLIB)   $(YDATA_HP)
yinput.o: yio.h parse.h $(HSH) $(PLAYALL)
yio.o: yio.h defmem.h $(YDATA_HP) $(PLAYALL)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_USE_SOFTFPE) -o $@ -c yio.c
yorick.o: yorick.c parse.h $(PSPLAY)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_NO_STRTOUL) -o $@ -c yorick.c
yrdwr.o: bcast.h $(PSLIB)   $(YDATA_HP)
YREH=../regexp/yfnmatch.h ../regexp/yregexp.h
ystr.o: ystr.c $(YREH) $(PSLIB) $(YDATA_HP)
	$(CC) $(CPPFLAGS) $(CFLAGS) -I../regexp -o $@ -c ystr.c

# codger code generator computes ywrap.c and yinit.c source files
CDG_FLAGS=$(CFLAGS)
codger$(EXE_SFX): codger.c
	$(CC) $(CPPFLAGS) $(CDG_FLAGS) $(FORTRAN_LINKAGE) -o codger$(EXE_SFX) codger.c

# numfmt computes binary number formats, results in prmtyp.h
prmtyp.h: ../play/any/numfmt ../play/any/numfmt.c
	../play/any/numfmt prmtyp.h
../play/any/numfmt:
	@cd ../play; $(MAKE) numfmt

# always compile main.c with -g so new packages can be debugged
main.o: main.c $(PLAYONLY)
	$(CC) $(CPPFLAGS) -g -I../play -o $@ -c main.c
main.c: ../play/$(Y_MAIN_C)
	cp ../play/$(Y_MAIN_C) main.c

config:
	./yconfig.sh
