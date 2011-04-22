# $Id: Makefile,v 1.2 2009-05-22 04:02:26 dhmunro Exp $
SHELL=/bin/sh
MAKE=make
include ../../Make.cfg

COPT=$(COPT_DEFAULT)
CFLAGS=$(COPT) $(Y_CFLAGS) -I. -I.. $(PLAY_INCS) $(HACK_FLAGS)
CCLOAD=$(CC) $(COPT) $(Y_LDFLAGS)

OBJS=hash.o hash0.o hashctx.o hashid.o mm.o mminit.o \
  alarms.o pmemcpy.o pstrcpy.o pstrncat.o p595.o \
  bitrev.o bitlrot.o bitmrot.o pstdio.o

all: libplay

libplay: $(OBJS)
	$(AR) rc ../libplay.a $(OBJS)
	$(RANLIB) ../libplay.a
	touch $@

EXE_FILES=numfmt test2d hashtest
clean::
	rm -f *~ '#'* *.o *.a main.c core* *.core a.out $(EXE_FILES) libplay

distclean:: clean
	rm -f .gdb*



numfmt: numfmt.c
	$(CC) -g -o $@ numfmt.c

test2d: main.o test2d.o ../libplay.a
	$(CCLOAD) -o $@ main.o test2d.o -L.. -lplay $(X11LIB)

test: test2d

hashtest: hashtest.o ../libplay.a
	$(CCLOAD) -o $@ hashtest.o -L.. -lplay

PLUGEXT=../plugin.h

hash.o: $(PLAY_CFG)/config.h ../phash.h ../pstdlib.h $(PLUGEXT)
hash0.o: $(PLAY_CFG)/config.h ../phash.h $(PLUGEXT)
hashctx.o: $(PLAY_CFG)/config.h ../phash.h $(PLUGEXT)
hashid.o: $(PLAY_CFG)/config.h ../phash.h ../pstdlib.h $(PLUGEXT)

mm.o: $(PLAY_CFG)/config.h ../pstdlib.h $(PLUGEXT)
mminit.o: $(PLAY_CFG)/config.h ../pstdlib.h $(PLUGEXT)
pfrall.o: $(PLAY_CFG)/config.h ../pstdlib.h $(PLUGEXT)

alarms.o: $(PLAY_CFG)/config.h ../play.h ../pstdlib.h $(PLUGEXT)

pmemcpy.o: $(PLAY_CFG)/config.h ../pstdlib.h $(PLUGEXT)
pstrcpy.o: $(PLAY_CFG)/config.h ../pstdlib.h $(PLUGEXT)
pstrncat.o: $(PLAY_CFG)/config.h ../pstdlib.h $(PLUGEXT)

bitrev.o: $(PLAY_CFG)/config.h ../play.h $(PLUGEXT)
bitlrot.o: $(PLAY_CFG)/config.h ../play.h $(PLUGEXT)
bitmrot.o: $(PLAY_CFG)/config.h ../play.h $(PLUGEXT)
p595.o: ../play.h $(PLUGEXT)

pstdio.o: $(PLAY_CFG)/config.h ../pstdio.h $(PLUGEXT)

test2d.o: ../play.h ../pstdlib.h ../pstdio.h $(PLUGEXT)
hashtest.o: ../phash.h ../pstdlib.h ../pstdio.h $(PLUGEXT)

main.o: main.c ../play.h $(PLUGEXT)
main.c: ../$(Y_MAIN_C)
	cp ../$(Y_MAIN_C) main.c
