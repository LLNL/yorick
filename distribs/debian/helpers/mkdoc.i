#include "doc/html/htmldoc.i"
// Overwrite ugly defaults
extern std_head;
f=open("debian/helpers/header.html");
std_head=rdline(f,100);
close,f;
std_head=std_head(where(std_head));
func _hdoc_head (g, title, table=) {
   wryte, g, "<html> <head> <title>";
   wryte, g, title;
   wryte, g, "</title>";
   write, g, std_head;
}
func _hdoc_tail (g, table=) {
  wryte, g, "</div>";
  _hdoc_skip, g, 3;
  wryte, g, "</body>";
  wryte, g, "</html>";
}

// Build cross-referenced function documentation
mkhtmldoc,from=["build/share/yorick/i","build/share/yorick/i0"],
  to="build/doc/",keywords="doc/html/keywords.txt",
  packinfo="doc/html/packinfo.txt";

// Replace header/footer of reference card
inDIR="doc/refs-html/";
title="Yorick Reference Card";
outDIR="build/doc/refcard/";
mkdirp,outDIR;
files=lsdir("doc/refs-html/");
for (n=1;n<=numberof(files);n++) {
  f=open(inDIR+files(n),"r");
  text=rdline(f,200);
  close,f;
  bl=where(strmatch(text,"id=\"content\""))(1)+1;
  el=max(where(strmatch(text,"</div>"))-1);
  body=text(bl:el);
  f=open(outDIR+files(n),"w");
  _hdoc_head,f,title,table=1;
  write,f,body;
  _hdoc_tail,f,table=1;
  close,f;
}

quit;
