# $Id: Makefile,v 1.1 2005-09-18 22:04:19 dhmunro Exp $
SHELL=/bin/sh
MAKE=make
X11OBJS=xfancy.o xbasic.o
include ../Make.cfg

CFLAGS=$(COPTIONS) -I. -I../play
CCLOAD=$(CC) $(LDOPTIONS) $(LDFLAGS)
SYS_LIBS=$(X11LIB) $(MATHLIB) $(FPELIB)
COPTIONS=$(COPT) $(Y_CFLAGS)
LDOPTIONS=$(COPT) $(Y_LDFLAGS)
COPT=$(COPT_DEFAULT)

D_GISTPATH='-DGISTPATH="~/gist:~/Gist:$(Y_SITE)/g"'

OBJS=gist.o tick.o tick60.o engine.o gtext.o draw.o draw0.o clip.o \
  gread.o gcntr.o hlevel.o ps.o cgm.o $(X11OBJS)
BOBJS=browser.o cgmin.o eps.o

all: gist

# always check to see whether ../play/libplay.a needs to be rebuilt
libgist:
	@cd ../play; $(MAKE) "COPT=$(COPT)"
	@$(MAKE) "COPT=$(COPT)" libgist.a

../play/libplay.a:
	@cd ../play; $(MAKE) "COPT=$(COPT)" libplay

libgist.a: ../play/libplay.a $(OBJS)
	cp ../play/libplay.a $@
	$(AR) r $@ $(OBJS)
	$(RANLIB) $@

gist: libgist.a $(BOBJS) main.o
	$(CCLOAD) -o $@ main.o $(BOBJS) -L. -lgist $(SYS_LIBS)

# libpyg target is for pygist
libpyg: libgist
	@$(MAKE) "COPT=$(COPT)" ../libpyg.a

../libpyg.a: libgist.a
	cp libgist.a ../libpyg.a
	$(RANLIB) ../libpyg.a

bench: libgist bench.o main.o
	$(CCLOAD) -o $@ main.o bench.o -L. -lgist $(SYS_LIBS)

main.c: ../play/$(Y_MAIN_C)
	cp ../play/$(Y_MAIN_C) main.c

clean::
	rm -f *~ '#'* *.o *.a core* *.core a.out main.c gist bench

distclean:: clean
	rm -f .gdb*

config:
	@:

# ------------- dependencies -------------

# cgm.h: gist.h engine.h
# draw.h: gist.h
# engine.h: gist.h
# gtext.h: gist.h
# hlevel.h: gist.h
# ps.h: gist.h engine.h
# xbasic.h: gist.h engine.h play.h
# xfancy.h: xbasic.h

PLUGEXT=../play/plugin.h
PLAY_H=../play/play.h $(PLUGEXT)
PSLIB=../play/pstdlib.h $(PLUGEXT)
PSLIO=../play/pstdlib.h ../play/pstdio.h $(PLUGEXT)
PLAYIO=../play/play.h ../play/pstdio.h $(PLUGEXT)
PSPLAY=../play/pstdlib.h ../play/play.h $(PLUGEXT)
PLAYALL=../play/play.h ../play/pstdlib.h ../play/pstdio.h $(PLUGEXT)

cgm.o: cgm.h gtext.h $(PLAYALL)  gist.h engine.h
clip.o: clip.h $(PSLIB)
draw.o: draw.c draw.h gtext.h $(PSLIB)  gist.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(NO_EXP10) -c draw.c
draw0.o: draw.h gtext.h $(PSPLAY)  gist.h
engine.o: engine.h gist.h draw.h $(PSLIB)
gcntr.o: gist.h
gist.o: gist.h engine.h clip.h $(PSPLAY)
gread.o: gread.c gist.h $(PLAYALL)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_GISTPATH) -c gread.c
gtext.o: gtext.h  gist.h
hlevel.o: hlevel.h xbasic.h engine.h $(PLAYIO)  gist.h
ps.o: ps.h gtext.h $(PLAYIO)  gist.h engine.h
tick.o: tick.c gist.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(NO_EXP10) -c tick.c
tick60.o: gist.h
xbasic.o: xbasic.h gtext.h $(PLAYIO)  gist.h engine.h
xfancy.o: xfancy.c xfancy.h draw.h  gist.h xbasic.h engine.h $(PLAY_H)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(NO_EXP10) -c xfancy.c

browser.o: xbasic.h cgm.h cgmin.h eps.h   gist.h engine.h $(PLAYALL)
cgmin.o: cgmin.h engine.h $(PLAYALL)   gist.h
eps.o: eps.h ps.h gtext.h $(PSLIO)   gist.h engine.h

bench.o: hlevel.h xfancy.h $(PLAYALL)  gist.h xbasic.h engine.h

# Always compile main.c with -g so new packages can be debugged.
main.o: main.c
	$(CC) $(CPPFLAGS) -I../play -g -c main.c
