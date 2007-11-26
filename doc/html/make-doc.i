// build cross-referenced function documentation
#include "htmldoc.i"
hdoc_read_template,"template.html";
mkhtmldoc;

// Replace header/footer of reference card
inDIR="../refs-html/";
title="Yorick Reference Card";
outDIR="./refcard/";
mkdirp,outDIR;
files=lsdir("../refs-html/");
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
  _hdoc_tail,f,title,table=1;
  close,f;
}

quit;
