/*  ************************** htmldoc.i *****************   */

/* DOCUMENT htmldoc.i 
   html documentation tools. By default the function mkhtmldoc constructs 
   html pages from the i0, i and contrib directories. 
   Document comments are extracted, and cross-refrenced to each other and to 
   the original function definitions. Crude indexing is performed by matching
   a list of keywords to the document comments.
   Functions mkhtmldoc.
 */



/*
author: Robert Cannon, rcc1@soton.ac.uk, 15th May 98
revised by David Munro 19/Apr/01
*/




func mkhtmldoc(from=, to=, keywords=, packinfo=, nosrc=, nofunc=, warn=)
/* DOCUMENT mkhtmldoc         generate html documentation tree
    
            mkhtmldoc, from=, to=, keywords=, packinfo= nosrc=, nofunc= 
   generates html documentation from yorick files in selected directories.
   Without any arguments the subdirectories i0, i, and contrib
   of Y_SITE are scanned for function definitions, and the documentation is 
   created in subdirectories of the current directory.
   If specified, the  'from' keyword should be a string array of 
   directories to scan. The 'to' keyword can be used to set a 
   destination directory other than the current directory.
   A keyword keywords= can be used to specify a file containing a list 
   of keywords from which to create a crude index. If not specified, and 
   if there is a file keywords.txt in the current directory, then that is 
   used. Likewise, the packinfo= can be used to specify a file containing 
   further information on some or all of the files in the source directories.
   It defaults to packinfo.txt if not specified. 
   The keywords nosrc and nofunc, if non null cut out the slowest parts 
   of the document creation process - crossreferencing the source files, 
   and creating function pages. They can be useful when checking the 
   format of a source file without recreating the whole document set.
   When source files do not match the formats mkhtmldoc is expecting, 
   warnings are printed to standard output, or to a file specified by
   the warn keyword. Note that non-compliance with the expected format
   is not necessarily an indication of errors in the source files - simply
   that mkhtmldoc can't make sense of them. Generally, however, it is 
   far easier to make one's own files follow the format of the 
   yorick i0 files more closely than it is to modify mkhtmldoc 
   to cope with them.


   The documentation tree is generated in seven stages.
   1 - read through all the source files, extracting function names 
       extern declarations of builtin functions, and document comments
   2 - reread the source files writing them out as preformatted html with 
       anchors for each function declaration and links to the (as yet 
       unwritten) function documentation
   3 - for each source file, compile a series of html pages of the document 
       comments for the functions in that file. One html file is generated 
       for each first letter.  
   4 - compile a series of html pages for all the functions together, again
       grouped into pages according to first letters.
   5 - if a keywords file is available, match keywords in the document 
       comments, and compile a keyword index pointing to all the matched 
       functions.
   6 - if a packinfo file is available, match source file names with 
       the packinfo file and compile a package list with the corresponding 
       descriptions. Alternatively, if a document comments appears near 
       the top of a source files, unattached to a function, this will be used
       instead. 



   By default, mkhtmldoc, will create links to the yorick manual, but 
   it does nothing to the manual itself. To get the manual in the right place, 
   run texi2html on the texinfo file yorick.tex putting the files in
   the 'manual' directory. To put an index bar at the top of each page
   and set up a margin and background colours, run hdoc_wrap on the manual 
   directory 
   eg, if you are creating the documentation in the current directory, 
   and Y_SITE is where yorick i installed, then 
   >   cp Y_SITE/doc/yorick.tex  manual
   >   cd manual
   >   texi2html yorick.tex

   then in yorick, 
   >  hdoc_wrap, manual
     
   So as not to overwrite carefully modified pages, the "home" link 
   on the indexbar points to index.html in the documentation directory, but 
   mkhtmldoc writes its template to index-raw.html.
   This should be moved to index.html and edited by hand as necessary

   KEYWORDS: keywords, packinfo, from, to, nosrc, nofunc

   SEE ALSO:  mkdoc, tagscan, srcanchor 
 */

{
   if (is_void (from)) {
     if (Y_VERSION!="1.4")
       from = Y_SITE + ["i0", "i"];
     else
       from = Y_SITE + ["startup", "include", "contrib"];
   }
   if (is_void (to)) to = get_cwd();
   if (strpart (to, strlen(to):) != "/") to += "/";
   if (is_void (keywords)) {
      if (open("keywords.txt","r",1)) keywords = "keywords.txt";
   }
   if (is_void (packinfo)) {
      if (open("packinfo.txt","r",1)) packinfo = "packinfo.txt";
   }
   nwarn = 0;
   if (warn) {
      fwarn = open (warn, "w");
   } 


   /* directories for html files: html for alphabetical function listings,
      html_i for converted yorick code, and create the directories 
      manual and html to hold the manual, and pages to be created by hand.
      (for now will put simple placeholder files in them)
      */ 
   
   dnames = ["manual","html_i","html","images"];
   // make these directories under the 'to' directory if they don't exist;
   for (i = 1; i <= numberof (dnames); i++) {
      todir = to + dnames(i);
      if (!open(todir,"r",1)) system, "mkdir " + todir;
      if (!open(todir,"r",1)) {
	 exit, "cant create directory " + todir;
      }
   }

   // the _lsm1 function is unix xpecific - it gets a list of the files 
   // in its arguement;
   ifiles = _lsm1 (from, ext = ".i", to=to);


   /* STAGE 1 - get a list of all function declarations, their names, 
    file, directory, document comment and line in the file
    based on the MKDOC function in mkdoc.i
    */

   maxtags = 256;
   tags = array(string, 6, maxtags);
   ntags = 0;
   write, "scanning files for function definitions";
   for (i=1 ; i <= numberof(ifiles) ; i++) {
      write, format = "%s           \n", ifiles(i) ;
      newtags = tagscan(ifiles(i));
      nt = numberof(newtags)/6;
      if (nt) {
        if (nt > maxtags-ntags) {
          no = maxtags;
          do {
            maxtags *= 2;
          } while (nt > maxtags-ntags);
          grow, tags, array(string, 6, maxtags-no);
        }
        tags(,ntags+1:ntags+nt) = newtags;
        ntags += nt;
      }
   }
   if (ntags) {
     tags = tags(,1:ntags);
     tags = tags(,where(strlen(tags(6,))>6));
   }
   write, "";

   /* STAGE 2 - copy the files name.i to tmp_name_i.html in the directories 
      under 'to', html quoting the characters "<" and ">" and adding
      anchors and cross references.  
      */
   // this part is slow - set nosrc to 1 to skip it;
   if (never_do && !nosrc) {
      write, "anchoring and linking source files";
      for (i=1 ; i <= numberof(ifiles) ; i++) {
	 name= [".", ifiles(i)];
	 do {
	    rtdir = name(1);
	    rtname = name(2);
	    name= strtok(name(2), "/\:");
         } while (name(2));
	 if (strpart(rtname, -1:0)==".i") rtname= strpart(rtname, 1:-2);
	 
	 dest = to + "html_i/tmp_" + rtname +"_i.html";
	 finaldest = to + "html_i/" + rtname +"_i.html";
	 write, format = "%s           \n", ifiles(i) ;      
	 srcanchor, ifiles(i), dest, tags;

	 _hdoc_headtail, dest, finaldest, title = rtdir + "/" + rtname;
      }
      write, "";
   }

   if (!nofunc) {
      // STAGE 3 - function listings by file in which they are defined;

     mask = strpart(tags(3,),-4:-1) == "test";
     tags = tags(,where(!mask));
     mask = strpart(tags(3,),-6:0) == "teststr";
     tags = tags(,where(!mask));
     mask = strpart(tags(3,),-5:0) == "testlp";
     tags = tags(,where(!mask));
     mask = strpart(tags(3,),-5:0) == "txpath";
     tags = tags(,where(!mask));
     mask = strpart(tags(1,),1:7) == "fitsAdd";
     mask |= strpart(tags(1,),1:6) == "fitsRe";
     mask |= tags(1,) == "fitsFixHeader";
     mask |= tags(1,) == "fitsWrite";
     mask |= tags(1,) == "fits_coordinate";
     mask |= strpart(tags(1,),1:7) == "_numfmt";
     mask |= tags(1,) == "system_orig";
     mask |= tags(1,) == "rez_style";
     mask |= strpart(tags(1,),1:5) == "_png_";
     tags = tags(,where(!mask));

     tags = tags(,sort(strcase(0,tags(1,))));

     mask = (strpart(tags(3,),-2:0)=="fft") |
       (strpart(tags(3,),-5:0)=="matrix");
     list = where(mask);
     tags(3,list) = "math";
     write, format = "writing index and doc pages for %s      \n", "math";
     hdoc_funcindex, "math", tags(,list), to;
     hdoc_funcdocs, "math", tags(,list), tags, to;

     mask = (strpart(tags(3,),-2:0)=="png") |
       (strpart(tags(3,),-3:0)=="mpeg") |
       (strpart(tags(3,),-3:0)=="jpeg") |
       (strpart(tags(3,),-3:0)=="zlib");
     list = where(mask);
     tags(3,list) = "yorz";
     write, format = "writing index and doc pages for %s      \n", "yorz";
     hdoc_funcindex, "yorz", tags(,list), to;
     hdoc_funcdocs, "yorz", tags(,list), tags, to;
      tags = tags(,sort(tags(3,)));
      rtndx = (tags(3,2:0)!=tags(3,1:-1))(cum)+1;
      rtname = array(string, rtndx(0));
      rtname(rtndx) = tags(3,);
      tags = tags(,sort(strcase(0,tags(1,))));
      mask = (rtname=="fft") | (rtname=="matrix") | (rtname=="png") |
        (rtname=="mpeg") | (rtname=="jpeg") | (rtname=="zlib");
      rtname = rtname(where(!mask));
      for (i = 1; i <= numberof(rtname); i++) {
	 write, format = "writing index and doc pages for %s      \n", 
           rtname(i);
         w = where(tags(3,)==rtname(i));
         hdoc_funcindex, rtname(i), tags(,w), to;
         hdoc_funcdocs, rtname(i), tags(,w), tags, to;
      }
      write, "";
   }


   if (!nofunc) {
      // STAGE 4 - function listings for all builtin and extern functions;
      // w = where (tags(5,) !=  "local");
     w = [];
      write, "writing global index and doc pages";
      hdoc_funcindex, "global", tags(,w), to;
      // hdoc_funcdocs, "global", tags(,w), tags, to;
   }


   // STAGE 5 - package listing;
   write, "making package listing";
   hdoc_packagelist, from, tags, packinfo=packinfo, to=to;


   //STAGE 6 - keywords
   if (keywords) {
      write, "making keyword index ";
      hdoc_keywordindex, tags, keywords, to;
   }

   //STAGE 7 - miscellaneous;
   hdoc_toptemplate, to;


   // put a default background image in the images directory;
   // f = open (to + "images/ydocbg.gif", "wb");
   // a = 
// [0x47,0x49,0x46,0x38,0x37,0x61,0xb7,0x04,0x01,0x00,0xf0,0x00,0x00,0x00,0x56,
//  0x00,0xff,0xea,0xc2,0x2c,0x00,0x00,0x00,0x00,0xb7,0x04,0x01,0x00,0x00,0x02,
//  0x2b,0x84,0x8f,0xa9,0xcb,0xed,0x0f,0xa3,0x9c,0x31,0xd8,0x8b,0xb3,0xde,0xbc,
//  0xfb,0x0f,0x86,0xe2,0x48,0x96,0xe6,0x89,0xa6,0xea,0xca,0xb6,0xee,0x0b,0xc7,
//  0xf2,0x4c,0xd7,0xf6,0x8d,0xe7,0xfa,0xce,0xf7,0xfe,0x0f,0x0c,0xee,0x0a,0x00,
// 0x3b];
   // _write, f, 0, char(a);
   // close, f;
}



func _lsm1 (from, ext=, to=) {
   if (is_void (ext)) ext = "";
   if (is_void (to)) to = "./";

   ndir = numberof (from);
   system, "ls -1 " + from(1) + "/*" +  ext + "  > " + to + "tmpflist.dat";
   for (i = 2; i <= ndir; i++) {
      system, "ls -1 " + from(i) + "/*" + ext + " >> " + to + "tmpflist.dat";
   }
   f = open (to + "tmpflist.dat");
   ifiles = rdline(f, 1000);
   ifiles = ifiles(where(ifiles));
   close, f;
   remove, to + "tmpflist.dat";
   if (ext==".i" && numberof(ifiles)) {
     mask = strpart(ifiles,-7:0)!="collec.i";
     mask &= strpart(ifiles,-7:0)!="custom.i";
     mask &= strpart(ifiles,-6:0)!="legal.i";
     mask &= strpart(ifiles,-7:0)!="prmtyp.i";
     mask &= strpart(ifiles,-6:0)!="readn.i";
     mask &= strpart(ifiles,-5:0)!="show.i";
     mask &= strpart(ifiles,-5:0)!="stdx.i";
     mask &= strpart(ifiles,-9:0)!="testfull.i";
     ifiles = ifiles(where(mask));
   }
   return ifiles;
}



func tagscan (fnm) {
  /* DOCUMENT  
            tags = tagscan, filename
    scan file filename for function declarations/definitions.       
    Returns a (6, nfunc) string array containing, for each function,
    the function name, then the directory name, file name, and line
    where it appears, its DOCUMENT comment and type. As with the mkdoc 
    function, if subsequent extern lines precede the DOCUMENT 
    comment, generate a cross-reference SEE ... to the first extern
    of the group.
    based on the mkdoc function in mkdoc.i

    SEE ALSO: mkhtmldoc, mkdoc, srcanchor
   */

   /* strip off non-directory part of filename */
   name= [".", fnm];
   do {
      rtdir = name(1);
      rtfnm = name(2);
      name= strtok(name(2), "/\:");
   } while (name(2));
   if (strpart(rtfnm, -1:0)==".i") rtfnm= strpart(rtfnm, 1:-2);
   /* scan the file to accumulate lists of function/variable/keyword names
       and the corresponding document strings */

   tags = [];
   ntags = maxtags = 0;
   f = open (fnm);
   pfunclev = funclev = 0;
   iline = 0;
   while (line= _myrdline(f)) {
      split= strtok(line);
      doctext= "";
      gottag = 0;
      if (pfunclev == 0 && (split(1)=="func" || 
			    split(1)=="extern" || 
			    split(1) == "local")) {
	 gottag = 1;
	 mytype = split(1);
	 name= strtok(split(2), " \t(,;");
         if (name(1)=="junk" || name(1)=="op") continue;
	 if (split(1)!="local") crossref= [];
	 else crossref= _hdoc_cross(1, name(2), []);
	 name= name(1);
	 myline = iline;

	 // beginning of function definition;
	 count= 15;        /* like help_worker function defined in std.i */
	 while ((line= _myrdline(f)) && count--) {

	    split= strtok(line);
	    if (!split(1) || split(1) == "}") break;
	    if (strmatch(line, "/* DOCUMENT")) {
	       do {
		  doctext += line + "\n";
		  if (strmatch(line, "*/")) break;
	       } while (line= _myrdline(f));
	    } else if (funclev == 0 && (split(1)=="extern" ||
                                        split(1)=="func")) {
	       crossref= _hdoc_cross(0, split(2), crossref);
	       if (count==9) count= 10;
	    } else if (funclev == 0 && split(1)=="local") {
	       crossref= _hdoc_cross(1, split(2), crossref);
	       if (count==9) count= 10;
	    }
	 }

      } else if (pfunclev == 0 && split(1)=="struct") {
	 gottag = 1;
	 mytype = split(1);
	 name= strtok(split(2), " \t(,;")(1);
	 myline = iline;
	 gotopen= 0;
	 do {
	    doctext += line + "\n";
	    if (!gotopen) gotopen= strmatch(line, "{");
	    if (gotopen && strmatch(line, "}")) break;
	 } while (line= _myrdline(f));
	 crossref= [];
      }

      // ignore things looking like tags but with one or fewer characters;
      if (gottag && strlen (name) > 1) {
	 sline = strtrim(swrite (myline));
         if (ntags == maxtags)
           grow, tags, array(string, 6, (maxtags+=100));
         ntags++;
         tags(,ntags) = [name, rtdir, rtfnm, sline, mytype, doctext];

	 n= numberof(crossref);
	 for (i=1 ; i<=n ; i++) {
	    doctext = "/* SEE "+name+"     */";
	    xname = crossref(i);
            if (ntags == maxtags)
              grow, tags, array(string, 6, (maxtags+=100));
            ntags++;
	    tags(,ntags) = [xname, rtdir, rtfnm, sline, mytype, doctext];
	 }
      }
   }
   close, f;
   if (ntags) tags = tags(,1:ntags);
   return tags;
}


func _myrdline(f) {
  /* keep a record of whether we are between curly braces or not
    so as to avoid picking up extern declarations of variables at 
    the top level.
    */
   iline++;
   line = rdline(f);
   extern pfunclev;
   pfunclev = funclev; 
   if (strmatch (line, "{")) funclev++;
   if (strmatch (line, "}")) funclev--;
   if (strpart (line, 1:1) == "}") funclev = 0;
   return line;
}


func _hdoc_cross(loc, names, crossref)
{
   split= strtok(names, " \t,;()");
   cross= crossref;
   while (split(1) && !strmatch(split(1), "/*")) {
      grow, cross, split(1:1);
      if (!loc) break;
      split= strtok(split(2), " \t,;");
   }
   return cross;
}






func srcanchor (infile, outfile, tags) {
  /* DOCUMENT  
            srcanchor, infile, outfile, tags
    convert yorick source to html
    Copy infile to outfile quoting any html special characters, inserting 
    anchors at function definitions/declarations, and cross-referencing
    function calls to definitions.  Tags should be a two dimensional string 
    array containing in tags (1,) the function names, and in tags(2,),
    tags(3,) and tags(4,) the directory, file, and line where each functions 
    is defined/declared. 

    SEE ALSO: mkhtmldoc, tagscan, mkdoc
   */


   tnames = tags(1,);
   ttypes = tags (5,);
   tnamesttypes = tnames + ttypes;
   nfn = numberof (tnames);

   f = open (infile);
   g = open (outfile, "w");

   wryte, g, "<pre>";
   wryte, g, "<font SIZE=2 color=\"#000000\">";

   funclev = 0;
   nwsf = 0;
   while (line = rdline(f)) {

      // convert ">" and "<" to their html equivalents &lt; and &gt;
      while (strmatch (line, ">")) {
	 split = strtok (line, ">");
	 line = split(1) + "&gt;" + split(2);
      }
      while (strmatch (line, "<")) {
	 split = strtok (line, "<");
	 line = split(1) + "&lt;" + split(2);
      }

      split = strtok (line);
      type = split(1);
      if (funclev == 0 && (type=="func" || 
			   type=="extern" || 
			   type=="local") ) {
	 name = (strtok(split(2), " \t(,;"))(1); 
	 rest = strpart (split(2), strlen(name)+1:);

	 w = where (tnamesttypes == name + type);
	 if (numberof (w) != 1) {
	    if (strlen(name) > 1 && name != "junk" && name != "op") {
	       // silently ignore failed one character matches;
	       if (nwsf == 0) write, format = "\n %s", "";
	       nwsf++;
	       write, fwarn, format = 
		      "warning: %i tag matches for %s \n",
	               numberof(w), name;
	       nwarn++;
	    }
            wryte, g, line;
	 } else {
	    tagdat = tags(,w(1));
	    
	    /* for the definition/declaration itself, put in an anchor and 
               a link back to the documentation tree */
	    wryte, g, ("<b>" + split(1) + " <a name = " + tagdat(1) + 
		       " href = ../html/" + tagdat(3) + 
		       "-doc.html" + "#" + tagdat(1) + 
		       ">" + tagdat(1) + "</a></b> " + rest);  
	 }
      } else {
	 /* look for tags within this line and put in links to the
            source tree */
	 
	 // this is going to be pretty slow....;
	 rest = line;
	 newline = "";
	 while (rest && rest != "") {
	    tok = (strtok (rest, " ,+-*/=\(\)!&^:;"))(1);
	    if (!tok) {
	       newline += rest;
	       rest = string(0);
	    } else {
	       c = (*pointer(tok))(1);
	       ss= *pointer(rest);
	       i0 = (where(ss == c))(1);
	       if (i0 > 1) newline += strpart(rest, 1:i0-1);
	       rest = strpart (rest, i0 + strlen(tok):);

	       w = where (tnames == tok);
	       if (numberof (w) == 1) {
		  tagdat = tags (,w(1));
		  
		  newline += ("<A HREF = ../html_i/" + tagdat(3) +
			      "_i.html#" + tagdat(1) +">" + tagdat(1) + 
			      "</A>");
	       } else {
		  newline += tok;
	       }
	    }
	 }

	 wryte, g, newline;
      }
      if (strmatch (line, "{")) funclev++;
      if (strmatch (line, "}")) funclev--;
      /* just in case, reset block counting for closing braces at start
         of line. As far as I'm aware, the above only fails once, on the line
         if (anyof(['{','}',',','=']==cs)) {
         in string.i */
      if (strpart (line, 1:1) == "}") funclev = 0;
   }
   wryte, g, "</pre>";
   close, f;
   close, g;
}



func _alphabsuffix (name) {
  /* make a suffix from the first letter of  name. 
     to cope with case insensitive systems, map eg "A" to "ac" and 
     "a" to "as" (for capital and small);
     */  
  if (strpart(name, 1:1) == "_") {
     suf = "-uscr"; 
  } else {
     ia = int (*pointer("a"))(1);
     iA = int (*pointer("A"))(1);
     ic = int (*pointer(name))(1);
     icl = (ic >= ia ? ic : ic + ia - iA);
     suf = "-" + string(&char(icl)) + (ic >= ia ? "s" : "c");
  }
   return suf;
}


func hdoc_funcindex(rtname, tags, to) {
   if (is_void(to)) to = ".";

   name_list = tags(1:3:2,);
   modnl = [];
   n = numberof(name_list)/2;
   fp = [];
   for (i = 1; i <= n; i++) {
      aa = name_list(,i);
      if ((nfp = strcase(0,strpart (aa(1), 1:1))) != fp) {
	 grow, modnl, [[strcase(1,nfp) + ".", string(0)]];
	 fp = nfp;
      }
      grow, modnl, aa;
   }

   name_list = modnl(1,);
   modnl = modnl(2,);

   idxbg1 = "\"#bbddff\"";
   idxbg2 = "\"#bbddff\"";
   idxfg = "\"#000000\"";
   idxbg0 = "\"#ffffff\"";


   f = open (to + "html/" + rtname + "-index.html", "w");

   if (rtname == "global") {
     _hdoc_head, f, "Yorick routines defined in all files", table=1; 
     wryte, f, "<center><h1>";
      wryte, f,  "all routines";
   } else {
     _hdoc_head,f, "Yorick routines defined in file " + rtname + ".i",table=1; 
     wryte, f, "<center><h1>";
      wryte, f,  "Yorick routines defined in file " + rtname + ".i";
   }
   wryte, f, "</h1></center>";
   _hdoc_skip, f, 2;

   n = numberof(name_list);
   ncol = min(1+(n-1)/10, 4);
   nrow = (n + ncol-1) / ncol;

   wryte, f, "<table cellpadding=2 cellspacing = 0>";

   for (ir = 1; ir <= nrow; ir++) {
      wryte, f, "<tr>";
      for (ic = 1; ic <= ncol; ic++) {
	 ielt = (ic-1) * nrow + ir;
	 ok = (ielt <= n);
	 s = (ok ? name_list(ielt) : "    ");
	 lab = (strlen (s) == 2 && strpart(s, 2:2) == ".");

	 if (lab) {
	    wryte, f, "<td BGCOLOR=" + idxbg2 + ">";
	    wryte, f, "<font color=" + idxfg + "> " + 
		          strpart(s,1:1) + " </font>";
	    wryte, f, "</td>"; 
	 } else {
	    wryte, f, "<td></td>";
	 }
	 wryte, f, "<td>"; 
	 if (ok && !lab) {
	    wryte, f, "<a href = " + modnl(ielt) + "-doc.html\#" +
                      s + ">" + s + "</a>";
	 } else if (ok) {
	    wryte, f, "<font color =" + idxbg1 + "> ------- </font>";
	 } else {
	    wryte, f, "<font color =" + idxbg0 + "> ------- </font>";
	 }
	 wryte, f,  "</td>";
      }
      wryte, f, "</tr>";
   }
   wryte, f, "</table>";
   _hdoc_tail, f, table=1;
}



func hdoc_funcdocs (rtname, tags, atags, to) {
   if (is_void (to)) to = "./"; 
   name_list =tags(1,);
   doc_list = tags(6,);
   anames = atags(1,);
   nwsf = 0;
   split_doc_list, name_list, doc_list, doc_def, doc_body, doc_see;
   aprev = "";
   f = [];
   n = numberof (name_list);

	 f = open(to+"html/"+rtname+"-doc.html","w");
         _hdoc_head, f, "section " + aprev + " of routines in " +
           rtname + ".i", table=1
	 wryte, f, "<center><h1>";
	 if (rtname == "global") {
	    wryte, f, " all functions  - " + aprev;
	 } else {
	    wryte, f,  "functions in " + rtname + ".i  - " + aprev;
	 }
	 wryte, f, "</h1></center>";

   for (i = 1; i <= n; i++) {
      name = name_list(i);
      _hdoc_skip, f, 3;
      wryte, f, "<table cellpadding = 0 border = 0 cellspacing = 0>";
      wryte, f, "<tr>";
      wryte, f, "<td valign = top width = 140 rowspan = 2>";
      wryte, f, "<big><b>";
      wryte, f, "<a name = " + name_list(i) + ">" + name_list(i) + "</a> <p>";
//      wryte, f, name_list(i);
      wryte, f, "</b></big>";
      wryte, f, "</td>";     

      dd = *(doc_def(i));
      db = *(doc_body(i));
      dsa = *(doc_see(i));

      myname = name_list(i);
      w = where (anames == myname);
      if (numberof(w) == 1) {
	 igl = w(1);
	 tagtyp = tags(5,i);
	 if (tagtyp == "extern") {
	    sdef = "builtin function, documented at ";
	 } else if (tagtyp == "func") {
	    sdef = "interpreted function, defined at ";
	 } else if (tagtyp == "struct") {
	    sdef = "structure, defined at ";
	 } else if (tagtyp == "local") {
	    sdef = "keyword,  defined at ";
	 } else {
	    sdef = "unknown tag ";
	 }

	 sdef = (sdef  + atags(2, igl) + "/" +
		 atags(3, igl) + ".i  <a href = ../html_i/" + 
		 atags(3, igl) + "_i.html#" + myname + ">" + 
		 " line " + strtrim (swrite(atags(4, igl))) + "</a>");
	 /* grow, db, sdef; */
      } else {
	 if (nwsf == 0) write, format = "\n %s", "";
	 print, "zero or multiple names ", w, myname;
	 nwsf++;
      }

      if (!is_void (dd) || !is_void (db)) {
	 
	 wryte, f, "<td colspan = 2>";
	 wryte, f, "<pre>";
	 wryte, f, ("<font SIZE=2 color=\"#000000\">");
	 if (!is_void (dd)) {
	    write, f, format = "<b> %s  </b>\n", dd;
	    wryte, f, " ";
	 }
	 if (!is_void (db)) {
	    write, f, format = "%s  \n", db;
	 }
	 wryte, f, "</pre>";
	 wryte, f, "</font>";
	 wryte, f, "</td></tr>";
      }

      if (!is_void (dsa)) {
	 nsa = numberof (dsa);
	 nsac = 5;
	 nsar = (nsa - 1 + nsac-1) / nsac;  

	 wryte, f, "<tr>";
	 wryte, f, "<td  width = 100 >";
	 wryte, f, ("<font SIZE=2 color=\"#00000\">");
	 wryte, f, dsa(1);
	 wryte, f, "</font>";
	 wryte, f, "</td>";     
	 
	 wryte, f, "<td width = 300>";
	 wryte, f, ("<font SIZE=3 color=\"#000000\">");
	 nchar = 0;
	 for (k = 2; k <= nsa; k++) {
	    ssa = dsa(k);
            if (ssa == "get_ray-path") ssa = "get_ray";
	    nchar += strlen (ssa)+2;
	    if (nchar > 46) {
	       wryte, f, "<br>";
	       nchar = strlen(ssa);
	    }
	    defroot = "";
	    w = where (anames == ssa);
	    if (numberof(w) == 1) {
	       igl = w(1);
	       defroot = "../html/" + atags(3, igl);
	    } else {
	       if (nwsf == 0) write, format = "\n %s", "";
	       write, fwarn, format = " warning: %i tag matches for  %s \n", 
	             numberof (w), ssa;
	       nwsf++;
	    }
	    if (defroot != "") {
	       aaa = strpart (ssa, 1:1);
	       write, f, (" <a href = " + defroot + 
			   "-doc.html#"+ssa + ">" + ssa + "</a>"+ 
			  ((k < nsa) ? ", &nbsp; " : " &nbsp; ")); 
	    } else {
	       write, f, (ssa + ((k < nsa) ? ", &nbsp; " : " &nbsp; ")); 
	    }
	 }
	 wryte, f, "</td>";
	 wryte, f, "</tr>";
      }
      wryte, f, "</table>";
   }
   _hdoc_tail, f, table=1;
   close, f;
}



func split_doc_list (name_list, doc_list, &doc_def, &doc_body, &doc_see)
{
   n = numberof (doc_list);
   doc_see = doc_body = doc_def = array (&[], n);

   for (i = 1; i <= n; i++) {
      sdoc = doc_list(i);
      doc = [];
      do {
	 split = strtok (sdoc, "\n");
	 grow, doc, [split(1)];
	 sdoc = split(2);
      } while (sdoc && sdoc != "");
      nl = numberof (doc);

      dd = [];
      db = [];
      dsa = [];

      k = 1;
      if (nl > 0) {
	 fl = doc(k);
	 if ((ind = strmatch (fl, "DOCUMENT")) > 0) {
	   grow, dd, "           " + strpart(fl, 12:strlen(fl));
	   k++;
	   fl = doc(k);
           ta = strtok(fl);
           if (ta(1)=="*") ta = strtok((fl = "  "+ta(2)));
	   while (k <= nl && (ta(1) == "or" || 
			      strpart(fl,1:11) == "           ")) {
	      grow, dd, fl;
	      k++;
              fl = doc(k);
              ta = strtok(fl);
              if (ta(1)=="*") ta = strtok((fl = "  "+ta(2)));
	   }
	 }

	 while (k <= nl) {
	    fl = doc(k);
	    if (strmatch (fl, "SEE ALSO:")) {
	       dsa = ["SEE ALSO:"];

	       for (j = k+1; j <= nl; j++) {
		  fl = fl + " " + doc(j);
	       }
	       ta = strtok (fl, " .,\n\t*");
	       while (ta(1) != string(0)) {
		  if (ta(1) != "SEE" && ta(1) != "ALSO:" &&  
		      ta(1) != "/*" && ta(1) != "*/" && ta(1) != "*") {
                    if (strpart(ta(1),1:1)!="(" && strpart(ta(1),0:0)!=")")
                      grow, dsa, ta(1);
		  }
		  ta = strtok (ta(2), " .,\n\t");
	       }
	       k = nl+1;	       

	    } else if (strmatch (fl, " SEE ")) {
	       dsa = ["SEE"];
	       ta = strtok (fl, " .,\n\t");
	       while (ta(1) != string(0)) {
		  if (ta(1) != "SEE" && ta(1) != "/*" && 
                      ta(1) != "*/" && ta(1) != "*") {
		     grow, dsa, ta(1);
		  }
		  ta = strtok (ta(2));
	       }
	       k = nl+1;
	    } else {
	       ta = strtok (fl);
               if (ta(1) == "*") fl = "  "+ta(2);
	       if (ta(1) != "*/") grow, db, fl;
	    }
	    k++;
	 }
      }
      if (is_void(dd)) dd = name_list(i);
      doc_def(i) = &dd;
      doc_body(i) = &db;
      doc_see(i) = &dsa;
   }
}



func hdoc_toptemplate (to) {
   _hdoc_indexbartags, htags, hfiles;
   // if index.html exists, don't overwrite it - put the template
   // in index-raw.html instead

   if (f = open(to + "index.html", "r", 1)) {
      close, f;
      f = open (to + "index-raw.html", "w");
   } else {
      f = open(to + "index.html", "w");
   }
   _hdoc_head, f, "yorick reference", table=1;


   wryte, f, "<h1><center>Yorick</center></h1>";
   _hdoc_skip, f, 2;

   wryte, f, "<blockquote>";
   wryte, f, "<h1>";
   wryte, f, "<ul>";
   ntag = numberof (htags);
   for (i = 2; i <= ntag; i++) {
      wryte, f, "<li><a href = " + hfiles(i) + ">" + htags(i) + "</a>";
   }
   wryte, f, "</ul></h1>";
   _hdoc_skip, f, 2;
   wryte, f, "for a complete local copy of this documentation tree, download one of ";
   wryte, f, "<pre>";
   wryte, f, "        <a href = ../../ydoc.tgz>ydoc.tgz</a>"; 
   wryte, f, "        <a href = ../../ydoc.zip>ydoc.zip</a>"; 
   wryte, f, "</pre>";

   _hdoc_skip, f, 2;

   wryte, f, "<h2> Other sources of yorick information </h2>";
   f = f;
   wryte, f, "<ul>";
   wryte, f, "<li>";
   wryte, f, "<a href = ftp://ftp-icf.llnl.gov/pub/Yorick/yorick-ad.html>yorick's home</a>";

   wryte, f, "<li>";
   wryte, f, "a  copy of <a href=\"../yorick-ad.html\">the same page</a>";
   wryte, f, "in case that link is slow";

   wryte, f, "<li>";
   wryte, f, "the <a href = ftp://ftp-icf.llnl.gov/pub/munro/yorickfaq.html>yorick faq</a>";
   wryte, f, "</ul>";

   wryte, f, "<hrule>";
   _hdoc_skip, f, 2;
   wryte, f, "This documentation was generated from the manual, README files, ";
   wryte, f, "and code documents of Yorick written by David H. Munro. <p>";

   wryte, f, "Yorick is free software, <a href=\"../copyright.html\">copyright</a>";
   wryte, f, "of the Regents of the University of California";

   wryte, f, "<hrule>";

   _hdoc_tail, f, table=1;

   f = open (to + "copyright.html", "w");
   _hdoc_head, f, "yorick copyright", table=1;
   wryte, f, "<center><h1>Yorick</h1></center>";
   _hdoc_skip, f, 4;
   _hdoc_copyright, f;
   _hdoc_tail, f, table=1;
}




func hdoc_packagelist (from, tags, packinfo=, to=) {

   name_list = tags(1,);
   dir_list = tags (2,);

   ifiles = _lsm1 (from, ext=".i", to=to);

   /* read the first few lines from each file to extract any package 
     description comments - DOCUMENT comments preceding the first 
     function definition */ 
    
   n = numberof (ifiles);
   filedoc = array (string, 3, n);
   for (i = 1; i <= n; i++) {
      fnm = ifiles(i);
      name= [".", fnm];
      do {
	 rtdir = name(1);
	 rtfnm = name(2);
	 name= strtok(name(2), "/\:");
      } while (name(2));
      if (strpart(rtfnm, -1:0)==".i") rtfnm= strpart(rtfnm, 1:-2);

      filedoc(1,i) = rtdir;
      filedoc(2,i) = rtfnm;
      doc = "";
      f = open (fnm);
      count= 10;        /* like help_worker function defined in std.i */
      done = 0;
      while ((line= rdline(f)) && count-- && !done) {
	 split= strtok(line);
	 if (!split(1) || split(1) == "func" || split(1) == "extern") done=1;
	 if (strmatch(line, "/* DOCUMENT")) {
	    while (line = rdline(f) && !done) {
	       if (strmatch(line, "*/")) {
		  done = 1;
	       }  else { 
		  doc += line + "<br>";
	       }
	    }
	 }
      }      
      filedoc(3,i) = doc;
      close, f;
   }


   // if a packinfo file was provided, extract name-document pairs
   // from it
   pkey = [];
   ptext = [];
   if (!is_void(packinfo)) {
      f = open (packinfo, "r");
      while ((line = rdline (f))) {
	 if (strpart(line, 1:1) == ":") {
	    grow, pkey, [strtrim (strpart (line, 2:strlen(line)))];

	    about = "";
	    while ((line = rdline (f)) && strpart(line,1:1) != ":") {
              //ss = strtrim (line);
              ss = line;
	       if (ss != "") about += ss;
	    }
	    grow, ptext, [about];
	    backup, f;
	 }
      }
   }

   f = open (to + "html/packages.html", "w");
   _hdoc_head, f, "Yorick packages", table=1;

   was_entry = 0;
       wryte, f, ("<table cellspacing=0 border=0" +
                  " cellpadding=2 width=100\%>");
       nf = numberof(pkey);
       wryte, f, "<tr><td width = 40 rowspan = " + swrite(nf+2) + ">";
       wryte, f, "</td>";
       wryte, f, ("<td colspan = 2> <h3> " + ptext(1) + " </h3>");
       wryte, f, "</td></tr>";
   for (i=2 ; i<=numberof(pkey) ; i++) {
     if (pkey(i)=="newsection") {
       //if (i>1) wryte, f, "</table> &nbsp;<br>&nbsp;<br>&nbsp;<br>";
       //wryte, f, ("<table cellspacing=0 border=0" +
       //           " cellpadding=2 width=10\%>");
       //nf = where(pkey(i+1:0)=="newsection");
       //nf = numberof(nf)? nf(1) : numberof(pkey)+1-i;
       //wryte, f, "<tr><td width = 40 rowspan = " + swrite(nf+1) + ">";
       //wryte, f, "</td>";
       //wryte, f, ("<td colspan = 2> <h3> " + ptext(i) + " </h3>");
       //wryte, f, "</td></tr>";
       af = "&nbsp;<br>&nbsp;<br><h3> " + ptext(i);
       wryte, f, ("<tr><td colspan = 2> " + af + " </h3>");
       wryte, f, "</td></tr>";
       was_entry = 0;
       continue;
     } else if (pkey(i)=="newsubsection") {
       af = ptext(i);
       if (was_entry) af = "&nbsp;<br>&nbsp;<br>"+af;
       wryte, f, ("<tr><td colspan = 2><em>" +  af + 
                  "</em></td></tr>");
       continue;
     }
       was_entry = 1;

       af = pkey(i);
       fstr = ("<tr><td valign = top>" + 
               "<a href = %s-index.html> %s</a> </td>\n");
       write, f, format = fstr, af, af;

       wryte, f, "<td>";
       wryte, f, ptext(i);
       wryte, f, "</td>";
       wryte, f, "</tr>";
   }
   wryte, f, "</table> &nbsp;<br>&nbsp;<br>&nbsp;<br>";

   _hdoc_tail, f, table=1;
   close, f;
}






func hdoc_keywordindex (tags, keywords, to) {

   // read the list if keywords - assumed to be one per line, with 
   // nothing else in the file
   f = open (keywords);
   kwl = [];
   while ((line = rdline(f)) != string(0)) {
      s = strtrim (line);
      if (s != "") grow, kwl, [s];
   }
   s = sort (kwl);
   kwl = kwl(s);

   f = open (to + "html/keywords.html", "w");
   _hdoc_head, f, "Yorick keyword index", table=1;

   wryte, f, ("<table cellspacing=2 border=0" +
		  " cellpadding=4 width=100\%>");
   wryte, f, "<tr>";
   for (i = 0; i < 26; i++) {
      ch = string (&(char('a' + i)));
      w = where (strpart (kwl, 1:1) == ch);
      if (!numberof(w)) continue;
      wryte, f, "<td>"; 
      wryte, f, "<a href = ../html/keywords-"+ch+".html>"+ch+"</a>";
      wryte, f, "</td>" 
   }
   wryte, f, "</tr></table>";
   _hdoc_skip, f, 2;

   nw = numberof (kwl);
   ncol = 3;
   nrow = (nw+ncol-1) / ncol;
   wryte, f, ("<table cellspacing=0 border=0" +
		  " cellpadding=1 width=100\%>");
   for (i = 1; i <= nrow; i++) {
      wryte, f, "<tr>";
      for (j = 1; j <= ncol; j++) {
	 irc = (j-1) * nrow + i;
	 if (irc <= nw) {
	    myw = kwl(irc);
	    suff = strcase(0, strpart (myw, 1:1));
	    wryte, f, "<td>";
	    wryte, f, ("<a href = ../html/keywords-" + suff +
		       ".html#" + strcomp(myw) + ">" + myw + "</a>");
	    wryte, f, "</td>";
	 } else {
	    wryte, f, "<td> </td>";
	 }
      }
      wryte, f, "</tr>";
   }
   wryte, f, "</table>";
   _hdoc_tail, f, table=1;
   close, f;

   for (i = 0; i < 26; i++) {
      ch = string (&(char('a' + i)));
      w = where (strpart (kwl, 1:1) == ch);
      if (!numberof(w)) continue;
      fnm = "html/keywords-" + ch + ".html";
      write, format = "\n %s   ", fnm;
      hdoc_keywordref, fnm, kwl(w), tags;
   }
   write, "";
}



func hdoc_keywordref (fnm, kwl, tags) {
   name_list = tags(1,);
   file_list = tags (3,);
   doc_list = tags (6,);

   f = open (fnm, "w");
   _hdoc_head, f, fnm, table=1; 

   wryte, f, ("<table cellspacing=2 border=0" +
		  " cellpadding=2 width=100\%>");
   wryte, f, "<tr>";
   for (i = 0; i < 26; i++) {
      ch = string (&(char(int(*pointer ("a"))(1) + i)));
      wryte, f, "<td>"; 
      wryte, f, "<a href = ../html/keywords-" + ch + ".html>"+ch+"</a>";
      wryte, f, "</td>" 
   }
   wryte, f, "</td></tr></table>";
   _hdoc_skip, f, 2;

   nr = numberof (kwl);
   wryte, f, ("<table cellspacing=0 border=0" +
		  " cellpadding=1 width=100\%>");
   for (i = 1; i <= nr; i++) {
      myk = kwl(i);
      wryte, f, "<tr>";
      wryte, f, "<td valign=top width=150><FONT face=\"Arial,Helvetica\"><b>"; 
      wryte, f, "<a name=\""+strcomp(myk)+"\"></a>" +  myk;
      wryte, f, "</b></FONT></td><td>";

      w = where (name_list == myk);
      if (numberof (w) > 0) {
	 defroot = "../html/" + file_list(w(1));
	 wryte, f, (" <b><a href=\"" + defroot +
		    "-doc.html#"+myk + "\">" + 
		    myk + "</a></b> &nbsp;&nbsp;&nbsp;");
      }

      rel = array (1, numberof (name_list));
      wd = myk;
      while ((wd = strtok(wd))(1)) {
	 rel = (rel & strmatch(doc_list, " " + wd(1) + " ", 1));
	 wd = wd(2);
      }
      w = where (rel);
      if (numberof (w) > 0) {
	 nsa = numberof (w);
	 for (j = 1; j <= nsa; j++) {
	    ssa = name_list(w(j));
	    defroot = "../html/" + file_list(w(j));
	    wryte, f, (" <a href=\"" + defroot +
		        "-doc.html#"+ssa + "\">" + ssa + "</a>"+ 
		       ((j < nsa) ? ", &nbsp; " : " &nbsp; ")); 
	 }
      }

      wryte, f, "</td></tr>";
   }
   wryte, f, "</table>";
   _hdoc_tail, f, table=1;
   close, f;
}


func strcomp (s) {
   a = "";
   while ((s = strtok(s))(1)) {
      a += s(1);
      s = s(2);
   }
   return a;
}



/* various chunks of html which are used repeatedly */
func _hdoc_startbody (f)
{
  wryte, f, "<body text=\"#000000\" bgcolor=\"#ffffff\" link=\"#0000ff\" "+
    "vlink=\"#800080\" alink=\"#ff0000\">";
}


/* a simple page can be made with 
       _hdoc_head, f, title;
       ... write some html;
       _hdoc_tail, f;

       with the 'table' option set, it puts a margin on the left and 
       the text is put inside a table - the left column is the margin, 
       the right column contains evverything else
 */

func wryte(f, line)
{
  write,format="%s\n", f, line;
}

func _hdoc_head (g, title, table=) {
   wryte, g, "<html> <head> <title>";
   wryte, g, title;
   wryte, g, "</title> </head>";
   _hdoc_startbody, g;
   if (table) {
     wryte, g, "<TABLE border=\"0\" cellpadding=\"5\" cellspacing=\"0\" "+
       "width=\"100%\">";
     wryte, g, "<TR><TD valign=\"TOP\" width=\"150\" bgcolor=\"#bbddff\"><BR>";
     wryte, g, "<IMG src=\"../images/ybanng.jpg\" border=\"0\" "+
       "hspace=\"0\" vspace=\"0\"";
     wryte, g, " alt=\"yorick banner\">";
     wryte, g, "  <FONT face=\"Arial,Helvetica\">";
     wryte, g, "  <P><B><A href=\"../index.html\">Home</A></B></P>";
     wryte, g, "  <P><B><A href=\"../manual/yorick.html\">Manual</A></B></P>";
     wryte, g, "  <P><B><A href=\"../html/packages.html\">"+
       "Packages</A></B></P>";
     wryte, g, "  <P><B><A href=\"../html/global-index.html\">"+
       "Global Index</A></B></P>";
     wryte, g, "  <P><B><A href=\"../html/keywords.html\">"+
       "Keywords</A></B></P>";
     wryte, g, "  <P><B><A href=\"../refcard/index.html\">"+
       "Quick Reference</A></B></P>";
     wryte, g, "  </FONT>";
     wryte, g, "</TD><TD valign=\"TOP\">";
   } else {
     _hdoc_indexbar, g;
     _hdoc_skip, g, 3;
   }
}

func _hdoc_tail (g, table=) {
   if (do_disclaimer) {
     wryte, g, "<P><HR><a href=\"http://www.llnl.gov/disclaimer.html\">";
     wryte, g, "<small>LLNL Disclaimers</small></a></P>";
   }
   if (table) {
      wryte, g, "</td></tr>";
      wryte, g, "</table>";
   }
   _hdoc_skip, g, 3;
   wryte, g, "</body>";
   wryte, g, "</html>";
}

func _hdoc_headtail (src, dest, title= ) {
   f = open (src);
   g = open (dest, "w");
   if (is_void(title)) title = dest
   _hdoc_head, g, title, table=1;
   line = rdline(f,50000);
   line = line(where(line));
   wryte, g, line;
   _hdoc_tail, g, table=1;
   close, f;
   close, g;
   remove, src;
}


func _hdoc_skip (f, n) {
   if (n) {
      for (i = 1; i <= n; i++) wryte, f, "&nbsp;<br>";
   } else {
      wryte, f, "&nbsp;<br>&nbsp;<br>";
      wryte, f, "&nbsp;<br>&nbsp;<br>";
   }
}



func _hdoc_indexbartags (&htags, &hfiles) {
   htags = ["home", "manual",  "packages", "index", "keywords"];
   hfiles = ["../index.html", "../manual/yorick.html", 
	     "../html/packages.html", 
	     "../html/global-index.html", "../html/keywords.html"];
}

func _hdoc_indexbar(f) {
   _hdoc_indexbartags, htags, hfiles;
   hbg1 = "\"#bbddff\"";
   wryte, f, "<table border=0 cellpadding=5 cellspacing=0 width = 100\%>";
   wryte, f, "<tr>";
   _hdoc_margintable, f;

   for (i = 1; i <= numberof (htags); i++) {
      wryte, f, "<td valign=\"TOP\" width=\"150\" bgcolor=" +  hbg1 + ">";   
      wryte, f, "<center>";
      wryte, f, "<a href = " + hfiles(i) + ">" + htags(i) + "</a>";
      wryte, f, "</center>";
      wryte, f, "</td>";
   }
   wryte, f, "</tr>";
   wryte, f, "</table>";
}


func _hdoc_margintable (f) {
  /* the best way to make a cell in a table of a particular width, which 
     doesn't stretch or shrink depending on the size of the browser 
     window seems to be to put a table inside the cell */
   wryte, f, "<td><table width = 150><tr>"+
     "<td valign=\"TOP\" width=\"150\" bgcolor=\"#bbddff\">";
   wryte, f, "<pre>    </pre>";
   wryte, f, "</td></tr></table></td>";
}



func hdoc_wrap (dir) {
   tmpfile = "hdoc_tmp.tmp";
   fnms = _lsm1 (dir, ext = ".html");
   nfls = numberof (fnms);
   for (i = 1; i <= nfls; i++) {
      f = open (fnms(i));
      g = open ("hdoc_tmp.tmp", "w");

      done = 0;
      gotbody = 0;
      while ((line = rdline(f)) &&  !done) {
	 if (strmatch (line, "/body", 1)) {
	    done = 1;
	 } else {
	    if (gotbody) {
	       wryte, g, line;
	    } else {
	       if (strmatch (line, "<body", 1)) gotbody = 1;
	    }
	 }
      }
      close, g;
      close, f;

      if (!gotbody) {
	 write, format = "html file %s contains no <BODY> statement ", fnm;
	 write, "- nothing done";
	 
      } else {
	 _hdoc_headtail, tmpfile, fnms(i); 
      }
   }
}









func _hdoc_copyright (f) {
   wryte, f, "Copyright 1994. The Regents of the University of California";
   wryte, f, "All rights reserved";
   wryte, f, "<br><p>";

   wryte, f, "Permission to use, copy, modify, and distribute this software ";
   wryte, f, "for any purpose without fee is hereby granted, provided that ";
   wryte, f, "this entire notice is included in all copies of any software ";
   wryte, f, "which is or includes a copy or modification of this software ";
   wryte, f, "and in all copies of the supporting documentation for such ";
   wryte, f, "  software.";
   wryte, f, "<br><p>";
   
   wryte, f, "This work was produced at the University of California, ";
   wryte, f, "Lawrence Livermore National Laboratory under contract ";
   wryte, f, "no. W-7405-ENG-48 between the U.S. Department of Energy ";
   wryte, f, "and The Regents of the University of California for the ";
   wryte, f, "operation of UC LLNL. ";

   wryte, f, "<br><p>";
   wryte, f, "     DISCLAIMER  ";
   wryte, f, "<br><p>";

   wryte, f, " This software was prepared as an account of work sponsored by an ";
   wryte, f, "agency of the United States Government.  Neither the United States ";
   wryte, f, "Government nor the University of California nor any of their ";
   wryte, f, "employees, makes any warranty, express or implied, or assumes any ";
   wryte, f, "liability or responsibility for the accuracy, completeness, or ";
   wryte, f, "usefulness of any information, apparatus, product, or process ";
   wryte, f, "disclosed, or represents that its use would not infringe ";
   wryte, f, "privately-owned rights.  Reference herein to any specific commercial ";
   wryte, f, "products, process, or service by trade name, trademark, manufacturer, ";
   wryte, f, "or otherwise, does not necessarily constitute or imply its ";
   wryte, f, "endorsement, recommendation, or favoring by the United States ";
   wryte, f, "Government or the University of California.  The views and opinions of ";
   wryte, f, "authors expressed herein do not necessarily state or reflect those of ";
   wryte, f, "the United States Government or the University of California, and ";
   wryte, f, "shall not be used for advertising or product endorsement purposes. ";
   
}

if (batch()) {
  args = get_argv();
  do_disclaimer = (numberof(args)==2 && args(2)=="llnl");
  if (!do_disclaimer) write, "creating yorick html document set";
  else write, "creating yorick html documents set (LLNL version)";
  mkhtmldoc;
}
