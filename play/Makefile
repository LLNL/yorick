# $Id: Makefile,v 1.1 2005-09-18 22:05:31 dhmunro Exp $
SHELL=/bin/sh
MAKE=make
include ../Make.cfg

SUBDIRS=hacks $(PLAY_DIRS) any
ALLDIRS=hacks win unix x11 any

all: libplay

# the library is actually built from subdirectories (in order)
libplay:
	@for d in $(SUBDIRS); do ( cd $$d; $(MAKE); ); done

config:
	@for d in $(PLAY_DIRS); do ( cd $$d; $(MAKE) TGT=$(TGT) config; ); done

numfmt: libplay
	cd any; $(MAKE) numfmt

test2d: libplay
	cd any; $(MAKE) test2d

clean:: cleantop
	@for dir in $(ALLDIRS); do ( cd $$dir; $(MAKE) clean; ); done

distclean:: distcleantop
	@for dir in $(ALLDIRS); do ( cd $$dir; $(MAKE) distclean; ); done

cleantop:
	rm -f *~ '#'* *.o *.a core* *.core a.out hack*.h playwin.h

distcleantop: cleantop
