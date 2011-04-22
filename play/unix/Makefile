# $Id: Makefile,v 1.3 2010-02-28 21:32:23 dhmunro Exp $
SHELL=/bin/sh
MAKE=make
include ../../Make.cfg

COPT=$(COPT_DEFAULT)
CFLAGS=$(COPT) $(Y_CFLAGS) -I. -I..
COPTIONS=$(COPT) $(Y_CFLAGS)
LDOPTIONS=$(COPT) $(Y_LDFLAGS)

OBJS=dir.o files.o fpuset.o handler.o pathnm.o slinks.o stdinit.o timeu.o \
  timew.o udl.o uevent.o ugetc.o uinbg.o usernm.o umain.o uspawn.o

all: libplay

libplay: $(OBJS)
	$(AR) rc ../libplay.a $(OBJS)
	$(RANLIB) ../libplay.a
	touch $@

config:
	CC="$(CC)" CFLAGS="$(Y_CFLAGS)" LDFLAGS="$(Y_LDFLAGS)" MATHLIB="$(MATHLIB)" ./config.sh

CLEANUP_LIST=cfg* config.0* fputest core* *.core a.out udltest.??*
clean::
	rm -f *~ '#'* *.o *.a $(CLEANUP_LIST) libplay

distclean:: clean
	rm -f config.h



PLUGEXT=../plugin.h

dir.o: config.h ../play.h playu.h ../pstdlib.h ../pstdio.h dir.c $(PLUGEXT)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_DIR1) $(D_DIR2) -c dir.c
files.o: config.h ../pstdio.h ../pstdlib.h playu.h files.c $(PLUGEXT)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_NO_PROCS) -c files.c
fpuset.o: config.h playu.h fpuset.c $(PLUGEXT)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_FPUSET) -c fpuset.c
handler.o: config.h ../play.h playu.h ../pstdlib.h $(PLUGEXT)
pathnm.o: config.h ../play.h playu.h ../pstdlib.h pathnm.c $(PLUGEXT)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_USERNM) -c pathnm.c
slinks.o: config.h playu.h ../pstdlib.h ../pstdio.h $(PLUGEXT)
stdinit.o: config.h ../play.h ../pstdlib.h ../pstdio.h playu.h $(PLUGEXT)
timeu.o: config.h ../play.h timeu.c $(PLUGEXT)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_TIMEU) -c timeu.c
timew.o: config.h ../play.h timew.c $(PLUGEXT)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_TIMEW) -c timew.c
udl.o: config.h ../pstdlib.h udl.c $(PLUGEXT)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(PLUG_UDL) -c udl.c
uevent.o: config.h upoll.h upoll.c ../play.h playu.h ../pstdlib.h uevent.c $(PLUGEXT)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_UEVENT) -c uevent.c
ugetc.o: config.h playu.h ugetc.h ../pmin.h $(PLUGEXT)
uinbg.o: config.h playu.h uinbg.c $(PLUGEXT)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_UINBG1) $(D_UINBG2) -c uinbg.c
usernm.o: config.h ../play.h usernm.c $(PLUGEXT)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_USERNM) -c usernm.c
umain.o: config.h ../play.h playu.h ../pstdlib.h $(PLUGEXT)
uspawn.o: config.h ../pstdio.h ../pstdlib.h ../play.h playu.h uspawn.c $(PLUGEXT)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(D_NO_PROCS) -c uspawn.c

# this is the DL config test used in config.sh
YORICK_EXE=./cfg
LDEXE_OPTS=$(LDOPTIONS) $(LDFLAGS) -DTEST_PLUG $(PLUG_EXPORT) $(PLUG_UDL) -I.
CCSO_OPTS=$(CPPFLAGS) $(CFLAGS) $(PLUG_PIC)
LDSO_OPTS=$(LDOPTIONS) $(LDFLAGS) $(PLUG_SHARED)
udltest: config.c udltest.c Makefile
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDEXE_OPTS) -o cfg config.c $(PLUG_LIB)
	$(CC) $(CCSO_OPTS) -DTEST_SHARED -c udltest.c
	$(CC) $(LDSO_OPTS) -o udltest$(PLUG_SFX) $(PLUG_DEF) udltest.o
	./cfg
