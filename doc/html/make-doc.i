batch,0;
#include "htmldoc.i"
batch,1;
hdoc_read_template,"template.html";
TARGETS=get_argv();
for (t=2;t<=numberof(TARGETS);t++) {
  target=TARGETS(t);
  if (target=="html_xref") {
    mkhtmldoc;
    continue;
  }
  if (target=="texi2html.init") {
    mktexi2html_init,,"texi2html.init";
    continue;
  }
  if (target=="refcard") {
    inDIR="../refs-html/";
    title="Yorick Reference Card";
    outDIR="./refcard/";
    mkdirp,outDIR;
    files=lsdir("../refs-html/");
    files=files(sort(files))(0:1:-1);
    for (n=1;n<=numberof(files);n++) {
      files(n);
      text=hdoc_read_file(inDIR+files(n));
      bl=where(strmatch(text,"id=\"content\""))(1)+1;
      el=max(where(strmatch(text,"</div>"))-1);
      body=text(bl:el);
      f=open(outDIR+files(n),"w");
      _hdoc_head,f,title,table=1,doc="qref";
      write,f,body;
      _hdoc_tail,f,title,table=1,doc="qref";
      close,f;
    }
    continue;
  }
  if (target=="extract_embedded") {
    hdoc_extract_embedded;
    continue;
  }
}
