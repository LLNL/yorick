# $Id: Makefile,v 1.1 2005-09-18 22:06:22 dhmunro Exp $
SHELL=/bin/sh
MAKE=make
include ../Make.cfg

CFLAGS=$(COPTIONS) -I../play -I../yorick
COPTIONS=$(COPT) $(Y_CFLAGS)
COPT=$(COPT_DEFAULT)

OBJS=yfnmatch.o yregexp.o

all: $(OBJS)

libyor: $(OBJS)
	touch $@

oinsert: $(OBJS) ../yorick/libyor.a
	$(AR) r ../yorick/libyor.a $(OBJS)

config:
	@:

clean::
	rm -f *~ '#'* *.o *.a core* *.core a.out libyor

distclean:: clean


yfnmatch.o: yfnmatch.h ../play/plugin.h
yregexp.o: yregexp.h ../play/pstdlib.h ../play/plugin.h
