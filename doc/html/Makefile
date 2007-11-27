# $Id: Makefile,v 1.2 2007-11-27 19:19:30 paumard Exp $
YORICK = ../../yorick/yorick
HTMLDIR = /usr/local/share/doc/yorick
TEXI2HTML = texi2html -init_file html/texi2html.init

all: html-doc

html-doc: html_xref manual refcard README.html

html_xref: html_xref/html_xref/packages.html
html_xref/html_xref/packages.html:
	$(YORICK) -batch make-doc.i html_xref

README.html:
	$(YORICK) -batch make-doc.i extract_embedded

texi2html.init:
	$(YORICK) -batch make-doc.i texi2html.init

../refs.pdf:
	cd ..; make refs.pdf
refcard: refcard/index.html
refcard/index.html: ../refs.pdf
	$(YORICK) -batch make-doc.i refcard
	cp ../refs.pdf refcard/refs.pdf

../yorick.pdf:
	cd ..; make yorick.pdf
manual: manual/yorick.html
manual/yorick.html: ../yorick/yorick.html ../yorick.pdf
	mkdir -p manual
	cp ../yorick/* ../yorick.pdf manual

../yorick/yorick.html: texi2html.init
	cd ..; $(TEXI2HTML) yorick.tex

install: html-doc
	install -d $(HTMLDIR)
	install -m 644  README.html *.css images html_xref manual refcard $(HTMLDIR)

clean:
	rm -Rf refcard html_xref manual index-raw.html texi2html.init README.html ../yorick/ *~ copyright.html

distclean:: clean
