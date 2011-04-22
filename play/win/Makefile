# $Id: Makefile,v 1.1 2005-09-18 22:05:38 dhmunro Exp $
# for cygwin mingw

SHELL=/bin/sh
MAKE=make
include ../../Make.cfg

CFLAGS=$(COPTIONS) -I. -I..
COPTIONS=$(COPT) $(Y_CFLAGS)
LDOPTIONS=$(COPT) $(Y_LDFLAGS)
COPT=$(COPT_DEFAULT)

OBJS=dir.o files.o handler.o sigansi.o pathnm.o conterm.o timeu.o \
  timew.o usernm.o wpoll.o wdl.o wstdio.o cygapp.o cygmain.o \
  clips.o cursors.o ellipse.o feep.o getdc.o pals.o pcell.o pfill.o plines.o \
  points.o prect.o pscr.o ptext.o pwin.o wspawn.o

all: libplay

../playwin.h: playwin.h
	rm -f ../playwin.h
	cp playwin.h ..

libplay: $(OBJS) ../playwin.h
	$(AR) rc ../libplay.a $(OBJS)
	$(RANLIB) ../libplay.a
	touch $@

config:
	cp Make.win ../../Make.cfg

CLEANUP_LIST=cfg* config.0* fputest core* *.core a.out wdltest.??*
clean::
	rm -f *~ '#'* *.o *.a $(CLEANUP_LIST) libplay

distclean:: clean


PLUGEXT=../plugin.h

conterm.o: playw.h   ../play.h $(PLUGEXT)
cygapp.o: playw.h ../pstdlib.h   ../play.h $(PLUGEXT)
#cygmain.o:
dir.o: config.h playw.h ../pstdio.h ../pstdlib.h   ../play.h $(PLUGEXT)
files.o: config.h playw.h ../pstdio.h ../pstdlib.h   ../play.h $(PLUGEXT)
handler.o: playw.h ../pstdlib.h   ../play.h $(PLUGEXT)
pathnm.o: playw.h ../pstdlib.h   ../play.h $(PLUGEXT)
sigansi.o: playw.h ../pstdlib.h   ../play.h $(PLUGEXT)
timeu.o: ../play.h $(PLUGEXT)
timew.o: ../play.h $(PLUGEXT)
usernm.o: ../play.h ../pstdlib.h $(PLUGEXT)
wdl.o: config.h playw.h ../pstdlib.h   ../play.h $(PLUGEXT)
wpoll.o: playw.h ../pstdlib.h   ../play.h $(PLUGEXT)
wstdio.o: playw.h ../pstdlib.h   ../play.h $(PLUGEXT)

clips: playw.h   ../play.h $(PLUGEXT)
cursors: playw.h   ../play.h $(PLUGEXT)
ellipse.o: playw.h   ../play.h $(PLUGEXT)
feep.o: playw.h   ../play.h $(PLUGEXT)
getdc.o: playw.h ../pstdlib.h   ../play.h $(PLUGEXT)
pals.o: playw.h ../pstdlib.h   ../play.h $(PLUGEXT)
pcell.o: playw.h   ../play.h $(PLUGEXT)
pfill.o: playw.h   ../play.h $(PLUGEXT)
plines.o: playw.h   ../play.h $(PLUGEXT)
points.o: playw.h   ../play.h $(PLUGEXT)
prect.o: playw.h   ../play.h $(PLUGEXT)
pscr.o: playw.h ../pstdlib.h   ../play.h $(PLUGEXT)
ptext.o: playw.h ../pstdlib.h   ../play.h $(PLUGEXT)
pwin.o: playw.h ../pstdlib.h   ../play.h $(PLUGEXT)

#	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_UEVENT) -c uevent.c

# this is the DL config test used in config.sh
YORICK_EXE=./cfg
YORICK_DEF=wdltest.def
LDEXE_OPTS=$(LDOPTIONS) $(LDFLAGS) $(PLUG_EXPORT) -I.
CCSO_OPTS=$(CPPFLAGS) $(CFLAGS) $(PLUG_PIC)
LDSO_OPTS=$(LDOPTIONS) $(LDFLAGS) $(PLUG_SHARED)
wdltest: wdlmain.c wdltest.c Makefile
	$(CC) $(CPPFLAGS) $(CFLAGS) -c wdlmain.c
	$(CC) $(LDEXE_OPTS) -o cfg.exe wdlmain.o wdlmain.def
	$(CC) $(CCSO_OPTS) -c wdltest.c
	$(CC) $(LDSO_OPTS) -o wdltest$(PLUG_SFX) wdltest.o $(PLUG_DEF)
	./cfg
