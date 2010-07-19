# $Id: Makefile,v 1.8 2010-07-19 08:08:33 thiebaut Exp $
# you need the makeinfo and texi2dvi programs from gnu.org
# and the tex typesetting system from tug.org
# For the html-doc target, you need a recent texi2html that accepts
# "--split node". Mac users: The one in fink (1.64) is too old, get one from
# http://www.nongnu.org/texi2html/
include ../Make.cfg
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
#TEXI2PDF = texi2pdf
TEXI2PDF = texi2dvi --pdf
TEX = tex
PDFTEX = pdftex
YORICK = ../yorick/yorick$(EXE_SFX)

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

html-doc:
	cd html; make html-doc

TEX_AUX=*.aux *.log *.toc *.cp *.fn *.ky *.pg *.tp *.vr *.tmp
clean::
	rm -f *~ '#'* *.inf* *.dvi *.doc $(TEX_AUX)
	rm -Rf yorick/
	cd html; make clean

distclean:: clean
