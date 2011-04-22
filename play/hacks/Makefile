# $Id: Makefile,v 1.1 2005-09-18 22:05:45 dhmunro Exp $
SHELL=/bin/sh
MAKE=make
include ../../Make.cfg

CFLAGS=$(COPTIONS) -I. -I.. -I../unix -I../x11
CCLOAD=$(CC) $(LDOPTIONS) $(LDFLAGS)

OBJS=$(HACK_OBJS)

all: $(OBJS)

clean::
	rm -f *~ '#'* *.o *.a core* *.core ../hack103.h

distclean:: clean

hack103.o: ../hack103.h hack103.c
	gcc -o hack103.o -c hack103.c
	$(AR) rc ../libplay.a $(OBJS)
	$(RANLIB) ../libplay.a

../hack103.h: hack103.h
	rm -f ../hack103.h
	cp hack103.h ..
