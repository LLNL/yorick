# $Id: Makefile,v 1.1 2005-09-18 22:05:33 dhmunro Exp $
SHELL=/bin/sh
MAKE=make
include ../../Make.cfg

COPT=$(COPT_DEFAULT)
CFLAGS=$(COPT) $(Y_CFLAGS) -I. -I.. -I../unix $(XINC)

OBJS=colors.o connect.o cursors.o errors.o events.o feep.o fills.o \
  fonts.o images.o lines.o pals.o pwin.o resource.o rgbread.o textout.o \
  rect.o ellipse.o clips.o points.o

PLUGEXT=../plugin.h

STD_DEPS=../unix/config.h ../play.h ../phash.h playx.h $(PLUGEXT)

all: libplay

../playwin.h: playwin.h
	rm -f ../playwin.h
	cp playwin.h ..

libplay: $(OBJS) ../playwin.h
	$(AR) rc ../libplay.a $(OBJS)
	$(RANLIB) ../libplay.a
	touch $@

COPTIONS=$(COPT) $(Y_CFLAGS)
LDOPTIONS=$(COPT) $(Y_LDFLAGS)
config:
	CC="$(CC)" CFLAGS="$(Y_CFLAGS)" LDFLAGS="$(Y_LDFLAGS)" ./xconfig.sh

clean::
	rm -f *~ '#'* *.o *.a core* *.core a.out cfg* libplay

distclean:: clean



connect.o: $(STD_DEPS) ../pstdlib.h
resource.o: $(STD_DEPS)
pwin.o: $(STD_DEPS) ../pstdlib.h
errors.o: $(STD_DEPS) ../unix/playu.h ../pstdlib.h
events.o: $(STD_DEPS) ../unix/playu.h
clips.o: $(STD_DEPS)
feep.o: $(STD_DEPS)

colors.o: $(STD_DEPS)
pals.o: $(STD_DEPS) ../pstdlib.h
cursors.o: $(STD_DEPS)
fonts.o: $(STD_DEPS) ../pstdlib.h

points.o: $(STD_DEPS)
fills.o: $(STD_DEPS)
rect.o: $(STD_DEPS)
ellipse.o: $(STD_DEPS)
images.o: $(STD_DEPS) ../pstdlib.h
lines.o: $(STD_DEPS)
textout.o: $(STD_DEPS) ../pstdlib.h
rgbread.o: $(STD_DEPS)
