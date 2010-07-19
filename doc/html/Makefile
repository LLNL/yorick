# $Id: Makefile,v 1.7 2010-07-19 08:08:33 thiebaut Exp $
# This file is part of Yorick and shares its BSD-type copyright.
# For the "manual" target, you need a recent texi2html that accepts
# "--split node". Mac users: The one in fink (1.64) is too old, get one from
# http://www.nongnu.org/texi2html/
include ../../Make.cfg
YORICK = ../../yorick/yorick$(EXE_SFX)
DESTDIR = /usr/local/share/doc/yorick
TEXI2HTML = texi2html -init_file html/texi2html.init -output yorick
#select mac or win version of ../README.html
PLATFORM=mac
BUILDDIR=compiled-doc/

all: html-doc

html-doc: html_xref manual refcard README.html README-install.html images css

css: $(BUILDDIR)doc/skull.css $(BUILDDIR)doc/style.css
$(BUILDDIR)doc/skull.css $(BUILDDIR)doc/style.css:
	mkdir -p $(BUILDDIR)doc/
	cp skull.css style.css $(BUILDDIR)doc/

images: $(BUILDDIR)doc/images/triangle3.jpg
$(BUILDDIR)doc/images/triangle3.jpg:
	mkdir -p $(BUILDDIR)doc/images
	cp -p images/*.* $(BUILDDIR)doc//images/

README-install.html: $(BUILDDIR)README.html
$(BUILDDIR)README.html:
	$(YORICK) -batch make-doc.i --to=$(BUILDDIR)doc/ ../README.html-$(PLATFORM)

html_xref: $(BUILDDIR)doc/html_xref/packages.html $(BUILDDIR)doc/html_xref/keywords-z.html
$(BUILDDIR)doc/html_xref/packages.html $(BUILDDIR)doc/html_xref/keywords-z.html:
	$(YORICK) -batch make-doc.i --to=$(BUILDDIR)doc/ html_xref

README.html: $(BUILDDIR)doc/README.html
$(BUILDDIR)doc/README.html:
	$(YORICK) -batch make-doc.i --to=$(BUILDDIR)doc/ extract_embedded

texi2html.init:
	$(YORICK) -batch make-doc.i texi2html.init

../refs.pdf:
	make -C .. refs.pdf
refcard: $(BUILDDIR)doc/refcard/index.html
$(BUILDDIR)doc/refcard/index.html: ../refs.pdf
	$(YORICK) -batch make-doc.i --to=$(BUILDDIR)doc/ refcard
	cp ../refs.pdf $(BUILDDIR)doc/refcard/refs.pdf

../yorick.pdf:
	make -C .. yorick.pdf
manual: $(BUILDDIR)doc/manual/yorick.html
$(BUILDDIR)doc/manual/yorick.html: ../yorick/yorick.html ../yorick.pdf
	mkdir -p $(BUILDDIR)doc/manual
	cp ../yorick/* ../yorick.pdf $(BUILDDIR)doc/manual

../yorick/yorick.html: texi2html.init
	cd ..; $(TEXI2HTML) yorick.tex

install: html-doc
	install -d $(DESTDIR)
	cp -a $(BUILDDIR)* $(DESTDIR)

clean:
	rm -Rf compiled-doc texi2html.init ../yorick/ *~

distclean:: clean

.PHONY: all html-doc clean distclean install html_xref manual refcard README.html README-install.html images css
