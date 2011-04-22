# $Id: Makefile,v 1.4 2010-02-28 21:32:23 dhmunro Exp $
# see README for usage

SHELL=/bin/sh

ALLDIRS=play gist yorick regexp matrix fft doc
CONFIGDIRS=play yorick

all: yorexe gistexe docs

yorexe: libyor
	@cd yorick; $(MAKE)

gistexe: libgist
	@cd gist; $(MAKE) gist

docs: yorexe
	@cd doc; $(MAKE) docs

# libraries are built in a fixed order:
# libplay, then libgist, then libyor
#   - the libraries are accumulated, that is,
#     libyor.a contains libgist.a contains libplay.a

libyor: libgist
	@cd regexp; $(MAKE) libyor
	@cd matrix; $(MAKE) libyor
	@cd fft; $(MAKE) libyor
	@cd yorick; $(MAKE) libyor.a

libgist: libplay
	@cd gist; $(MAKE) libgist.a

libplay: Make.cfg
	@cd play; $(MAKE) libplay

LD_STATIC=
Make.cfg:
	LD_STATIC=$(LD_STATIC) ./configure

config: distclean
	@$(MAKE) "LD_STATIC=$(LD_STATIC)" Make.cfg

prefix=.
Y_PLATFORM=.
Y_SITE=.
Y_HOME=.
ysite:
	if S="s?^prefix=.*?prefix=\"$(prefix)\"?;\
	s?^Y_PLATFORM=.*?Y_PLATFORM=\"$(Y_PLATFORM)\"?;\
	s?^Y_SITE=.*?Y_SITE=\"$(Y_SITE)\"?;\
	s?^Y_HOME=.*?Y_HOME=\"$(Y_HOME)\"?";\
	sed -e "$$S" ysite.sh >ysite.sh1; then mv ysite.sh1 ysite.sh; fi
	@if test -r ysite.grp; then chmod g+w ysite.sh; fi

reloc:
	$(MAKE) Y_HOME=relocate ysite

clean::
	@rm -f Make.del yorapi.def
	@if test ! -r Make.cfg; then touch Make.cfg Make.del; fi
	@for d in $(ALLDIRS); do ( cd $$d; $(MAKE) TGT=exe clean; ); done
	@if test -r Make.del; then rm -f Make.cfg Make.del; fi
	rm -f *~ '#'* *.o cfg* ysite.sh? core* *.core a.out
	rm -f i/*~ i0/*~ i-start/*~ g/*~ extend/*~
	rm -rf relocate

distclean::
	@touch Make.cfg
	@for d in $(ALLDIRS); do ( cd $$d; $(MAKE) TGT=exe distclean; ); done
	rm -f *~ '#'* *.o cfg* Make.* ysite.sh? core* *.core a.out
	rm -f i/*~ i0/*~ i-start/*~ g/*~ extend/*~
	rm -rf relocate

siteclean: distclean
	@rm -f ysite.grp
	@$(MAKE) prefix=. Y_PLATFORM=. Y_SITE=. Y_HOME=relocate ysite


check:
	@cd yorick; $(MAKE) check

INSTALL_ROOT=
Y_BINDIR=
Y_DOCDIR=
install: yorexe gistexe docs
	./instally.sh +both "$(INSTALL_ROOT)" "$(Y_BINDIR)" "$(Y_DOCDIR)"

install1: yorexe gistexe
	./instally.sh +home "$(INSTALL_ROOT)" "$(Y_BINDIR)" "$(Y_DOCDIR)"

uninstall:
	./instally.sh -both "$(INSTALL_ROOT)" "$(Y_BINDIR)" "$(Y_DOCDIR)"

uninstall1:
	./instally.sh -home "$(INSTALL_ROOT)" "$(Y_BINDIR)" "$(Y_DOCDIR)"

dist: siteclean
	W=`pwd`;N=`basename "$$W"`;R=`tail -n 1 VERSION`;cd ..;\
	tar cvf - $$N|gzip - >$$N.$$R.tgz;

# Usage: make YGROUP=altgrp sharable
# default group is "yorick", affects instally.sh
YGROUP=yorick
sharable:
	@rm -f ysite.grp
	echo "$(YGROUP)" >ysite.grp
	chgrp -R $(YGROUP) .
	chmod -R g+w .
	find . -type d | xargs chmod g+s

relocatable: siteclean
	@rm -rf relocate
	$(MAKE) Y_HOME=relocate ysite
	$(MAKE) install
	cp install.rel relocate/README
	cp emacs/yorick.el relocate
	mkdir relocate/contrib
	W=`pwd`;N=`basename "$$W"`;R=`tail -n 1 VERSION`;\
	mv relocate $$N-$$R;tar cvf - $$N-$$R|gzip - >$$N-$$R.tgz;\
	rm -rf $$N-$$R

dumpconfig:
	@cd yorick; $(MAKE) dumpconfig

# targets for ./configure
echocc:
	echo "$(CC)" >cfg.tmp
echorl:
	echo "$(RANLIB)" >cfg.tmp
echoar:
	echo "$(AR)" >cfg.tmp
echoml:
	echo "$(MATHLIB)" >cfg.tmp
pkgconfig:
	@for d in $(CONFIGDIRS); do ( cd $$d; $(MAKE) config; ); done
