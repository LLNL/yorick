# $Id: Makefile,v 1.3 2007-11-26 20:57:35 paumard Exp $
# you need the makeinfo and texi2dvi programs from gnu.org
# and the tex typesetting system from tug.org
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
#TEXI2PDF = texi2pdf
TEXI2PDF = texi2dvi --pdf
TEXI2HTML = texi2html -init_file html/texi2html.init
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

yorick.html: yorick/yorick.html
yorick/yorick.html:
	$(TEXI2HTML) yorick.tex

html-doc: yorick/yorick.html
	cd html; make html-doc

TEX_AUX=*.aux *.log *.toc *.cp *.fn *.ky *.pg *.tp *.vr *.tmp
clean::
	rm -f *~ '#'* *.inf* *.dvi *.doc $(TEX_AUX)
	rm -Rf yorick
	cd html; make clean

distclean:: clean
