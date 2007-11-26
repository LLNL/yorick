YORICK = ../../yorick/yorick
HTMLDIR = /usr/local/share/doc/yorick

all: html-doc

html-doc:
	$(YORICK) -i make-doc.i
	cp ../yorick/* manual

install:
	install -d $(HTMLDIR)
	install -m 644  index.html *.css images html_xref manual refcard $(HTMLDIR)

clean:
	rm -Rf refcard html_xref manual index-raw.html
