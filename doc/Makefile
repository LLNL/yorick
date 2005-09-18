# $Id: Makefile,v 1.1 2005-09-18 22:05:08 dhmunro Exp $
# you need the makeinfo and texi2dvi programs from gnu.org
# and the tex typesetting system from tug.org
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
TEXI2PDF = texi2pdf
TEX = tex
PDFTEX = pdftex
YORICK = ../yorick/yorick

all: yorick.info docs

docs:
	$(YORICK) -batch maked.i

yorick.info: yorick.tex
	$(MAKEINFO) yorick.tex

yorick.dvi: yorick.tex
	$(TEXI2DVI) yorick.tex

yorick.pdf: yorick.tex
	$(TEXI2PDF) yorick.tex

refs.dvi: refs.tex
	$(TEX) refs.tex

refs.pdf: refs.tex
	$(PDFTEX) refs.tex

TEX_AUX=*.aux *.log *.toc *.cp *.fn *.ky *.pg *.tp *.vr *.tmp
clean::
	rm -f *~ '#'* *.inf* *.dvi *.doc $(TEX_AUX)

distclean:: clean
