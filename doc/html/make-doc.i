batch,0;
#include "htmldoc.i"
batch,1;
hdoc_read_template,"template.html";
TARGETS=get_argv();
TARGETS;
for (t=2;t<=numberof(TARGETS);t++) {
  target=TARGETS(t);
  if (strpart(target,1:5)=="--to=") {
    to=strpart(target,6:0);
    if (strpart(to,0:0)!="/") to=to+"/";
    write,format="Destination directory: %s\n",to;
    mkdirp,to;
    continue;
  }
  if (target=="html_xref") {
    mkhtmldoc,to=to;
    continue;
  }
  if (target=="texi2html.init") {
    mktexi2html_init,,"texi2html.init";
    continue;
  }
  if (target=="../README.html-mac") {
    hdoc_headtail,"README-install.html.content",to+"../README.html", 0,title="Yorick: Installation Instructions",toroot="doc/", doc="installmac";
    continue;
  }
  if (target=="../README.html-win") {
    hdoc_headtail,"README-install.html.content",to+"../README.html", 0,title="Yorick: Installation Instructions",toroot="doc/", doc="installwin";
    continue;
  }
  if (target=="refcard") {
    inDIR="../refs-html/";
    title="Yorick Reference Card";
    outDIR=to+"refcard/";
    mkdirp,outDIR;
    files=lsdir(inDIR);
    files=files(where(strgrep(".html$",files)(2,)!=-1));
    files=files(sort(files))(0:1:-1);
    for (n=1;n<=numberof(files);n++) {
      files(n);
      text=hdoc_read_file(inDIR+files(n));
      bl=where(strmatch(text,"id=\"content\""))(1)+1;
      el=max(where(strmatch(text,"</div>"))-1);
      body=text(bl:el);
      f=open(outDIR+files(n),"w");
      hdoc_head,f,title,table=1,doc="qref";
      write,f,body;
      hdoc_tail,f,title,table=1,doc="qref";
      close,f;
    }
    continue;
  }
  if (target=="extract_embedded") {
    hdoc_extract_embedded,to=to;
    continue;
  }
}
