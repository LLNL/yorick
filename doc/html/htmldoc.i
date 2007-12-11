/*  ************************** htmldoc.i *****************   */
// $Id: htmldoc.i,v 1.8 2007-12-11 10:22:19 paumard Exp $

/* DOCUMENT htmldoc.i
   
   html documentation tools. By default the function mkhtmldoc constructs 
   html pages from the installed i0 and i directories. 
   Document comments are extracted, and cross-referenced to each other and to 
   the original function definitions. Crude indexing is performed by matching
   a list of keywords to the document comments.

   Batch mode:
    yorick -batch htmldoc.i [--quiet|-q] [--nosrc|-s] [--nofunc|-f] \
      [--from=dir1:dir2,...] [--to=destdir] [--xref-dir=html_xref] \
      [--keywords=keywords.txt] [--packinfo=packinfo.txt] \
      [--aliases=aliases.txt] [--template=template.html] [--warn=warnfile]
   
   In batch mode, mkhtmldoc() is automatically called. Each
   mkhtmldoc() keyword has an equivalent long option form. The boolean
   options also have a short option form.

   SEE ALSO: mkhtmldoc, hdoc_read_template, hdoc_extract_embedded,
             mktexi2html_init, hdoc_head, hdoc_tail, hdoc_headtail
 */



/*
author: Robert Cannon, rcc1@soton.ac.uk, 15th May 98
revised by David Munro 19/Apr/01, T. Paumard Nov/07
Yorick's BSD license applies:
Copyright (c) 2005-2007, The Regents of the University of California.
*/


func mkhtmldoc(from=, to=, xref_dir=,
               keywords=, packinfo=, template=, aliases=, warn=,
               nosrc=, nofunc=, quiet=)
/* DOCUMENT mkhtmldoc         generate html documentation tree
    
            mkhtmldoc, from=, to=, xref_dir=,
                       keywords=, packinfo=, aliases=, template=,
                       nosrc=, nofunc=, quiet=,
                       warn=
   generates html documentation from yorick files in selected directories.
   Without any arguments the subdirectories i0 and i
   of Y_SITE are scanned for function definitions, and the documentation is 
   created in subdirectories of the current directory.
   
   The page layout is defined in a template file, which defaults to
   "template.html" if this file exists. A builtin fallback is used if
   no file is provided. The template can also be set using
   hdoc_read_template.

   If specified, the  'from' keyword should be a string array of 
   directories to scan. The 'to' keyword can be used to set a 
   destination directory other than the current directory.
   The cross-reference DOCUMENT comments are extracted into
   TO/XREF_DIR, where XREF_DIR defaults to "html_xref".

   A keyword keywords= can be used to specify a file containing a list 
   of keywords from which to create a crude index. If not specified, and 
   if there is a file keywords.txt in the current directory, then that is 
   used. Likewise, the packinfo= can be used to specify a file containing 
   further information on some or all of the files in the source directories.
   It defaults to packinfo.txt if not specified. An "aliases" file can  also be
   specified to merge the functions from several .i files in a single .html
   file. It defaults to aliases.txt in the current directory if this file
   exists.
   
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


   The documentation tree is generated in five stages.
   1 - read through all the source files, extracting function names 
       extern declarations of builtin functions, and document comments
   2 - for each source file, compile a series of html pages of the document 
       comments for the functions in that file. One html file is generated 
       for each first letter.  
   3 - compile a series of html pages for all the functions together, again
       grouped into pages according to first letters.
   4 - if a keywords file is available, match keywords in the document 
       comments, and compile a keyword index pointing to all the matched 
       functions.
   5 - if a packinfo file is available, match source file names with 
       the packinfo file and compile a package list with the corresponding 
       descriptions. Alternatively, if a document comments appears near 
       the top of a source files, unattached to a function, this will be used
       instead. 


   Unless QUIET is set to 1, mkhtmldoc outputs a lot of information,
   including warnings. These warnings can be redirected to a file
   using the WARN keyword (it is the only way to get them if QUIET is
   set).
   
   KEYWORDS: keywords, packinfo, aliases, template, from, to, nosrc, nofunc,
             quiet, warn, xref_dir

   SEE ALSO:  mkdoc, tagscan, srcanchor, hdoc_read_template,
              mktexi2html_init, hdoc_extract_embedded
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
   if (is_void(xref_dir)) xref_dir="html_xref/";
   if (strpart (xref_dir, strlen(xref_dir):) != "/") xref_dir += "/";
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

   if (is_void(template)) template="template.html";
   if (open(template,"r",1)) hdoc_read_template,template;

   /* directories for html files: html_xref for alphabetical function
      listings, create the directories manual and refcard to hold the
      manual and reference cards.  (for now will put simple
      placeholder files in them)
      */ 
   
   dnames = [xref_dir];
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
   if (!quiet) write, "scanning files for function definitions";
   for (i=1 ; i <= numberof(ifiles) ; i++) {
      if (!quiet) write, format = "%s           \n", ifiles(i) ;
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
   if (!quiet) write, "";

   /* STAGE 2 - copy the files name.i to tmp_name_i.html in the directories 
      under 'to', html quoting the characters "<" and ">" and adding
      anchors and cross references.  
      */
   // this part is slow - set nosrc to 1 to skip it;
   if (never_do && !nosrc) {
      if (!quiet) write, "anchoring and linking source files";
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
	 if (!quiet) write, format = "%s           \n", ifiles(i) ;      
	 srcanchor, ifiles(i), dest, tags, quiet=quiet, xref_dir=xref_dir;

	 hdoc_headtail, dest, finaldest, title = rtdir + "/" + rtname;
      }
      if (!quiet) write, "";
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

     if (is_void(aliases)) aliases="aliases.txt";
     if (fal=open(aliases,"r",1)) {
       while (line=rdline(fal)) {
         found=0;
         if (strlen(line)==0) continue;
         aka=pathsplit(line,delim=" ");
         rname=aka(1);
         if (strgrep("^#",rname)(2)==1) continue;
         for (i=1;i<=numberof(aka);i++) {
           list=where(strgrep(aka(i)+"$",tags(3,))(2,)!=-1);
           if (numberof(list)) {
             tags(3,list)=rname;
             found=1;
             grow,rtx,aka(i);
           }
         }
         if (found) {
           list=where(tags(3,)==rname);
           if (!quiet) write, format = "writing index and doc pages for %s \n", rname;
           hdoc_funcindex, rname, tags(,list), to, xref_dir;
           hdoc_funcdocs, rname, tags(,list), tags, to, xref_dir, quiet=quiet;
         }
       }
       close,fal;
         // math fft matrix
         // yorz png mpeg jpeg zlib
     }

      tags = tags(,sort(tags(3,)));
      rtndx = (tags(3,2:0)!=tags(3,1:-1))(cum)+1;
      rtname = array(string, rtndx(0));
      rtname(rtndx) = tags(3,);
      tags = tags(,sort(strcase(0,tags(1,))));
      for (i=1;i<=numberof(rtx);i++) rtname = rtname(where(rtname!=rtx(i)));
      for (i = 1; i <= numberof(rtname); i++) {
	 if (!quiet) write, format = "writing index and doc pages for %s      \n", 
           rtname(i);
         w = where(tags(3,)==rtname(i));
         hdoc_funcindex, rtname(i), tags(,w), to, xref_dir;
         hdoc_funcdocs, rtname(i), tags(,w), tags, to, xref_dir, quiet=quiet;
      }
      if (!quiet) write, "";
   }


   if (!nofunc) {
      // STAGE 4 - function listings for all builtin and extern functions;
      // w = where (tags(5,) !=  "local");
     w = [];
      if (!quiet) write, "writing global index and doc pages";
      hdoc_funcindex, "global", tags(,w), to, xref_dir;
      // hdoc_funcdocs, "global", tags(,w), tags, to, xref_dir;
   }


   // STAGE 5 - package listing;
   if (!quiet) write, "making package listing";
   hdoc_packagelist, from, tags, packinfo=packinfo, to=to, xref_dir=xref_dir;


   //STAGE 6 - keywords
   if (keywords) {
      if (!quiet) write, "making keyword index ";
      hdoc_keywordindex, tags, keywords, to, xref_dir;
   }

   //STAGE 7 - miscellaneous;
   //hdoc_toptemplate, to;

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
   ifiles = hdoc_read_file(to + "tmpflist.dat");
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






func srcanchor (infile, outfile, tags, quiet=, xref_dir=) {
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
	       if (nwsf == 0 & !quiet) write, format = "\n %s", "";
	       nwsf++;
	       if (!quiet|!is_void(warn)) write, fwarn, format = 
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
		       " href = ../" + xref_dir + tagdat(3) + 
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


func hdoc_funcindex(rtname, tags, to, xref_dir) {
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

   doc="xref";
   f = open (to + xref_dir + rtname + "-index.html", "w");

   if (rtname == "global") title="Yorick routines defined in all files";
   else title="Yorick routines defined in file " + rtname + ".i";
   hdoc_head, f, title, table=1, doc=doc; 
   wryte, f, "<center><h1>";
   if (rtname == "global") wryte, f,  "all routines";
   else wryte, f,  title;
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
   hdoc_tail, f, title, table=1, doc=doc;
}



func hdoc_funcdocs (rtname, tags, atags, to, xref_dir, quiet=) {
   if (is_void (to)) to = "./"; 
   name_list =tags(1,);
   doc_list = tags(6,);
   anames = atags(1,);
   nwsf = 0;
   split_doc_list, name_list, doc_list, doc_def, doc_body, doc_see;
   aprev = "";
   f = [];
   n = numberof (name_list);
   doc="xref";
	 f = open(to+xref_dir+rtname+"-doc.html","w");
         title = "section " + aprev + " of routines in " + rtname + ".i";
         hdoc_head, f, title, table=1, doc=doc;
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
	 if (nwsf == 0 & !quiet) write, format = "\n %s", "";
	 if (!quiet) print, "zero or multiple names ", w, myname;
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
	       defroot = "../" + xref_dir + atags(3, igl);
	    } else {
	       if (nwsf == 0 & !quiet) write, format = "\n %s", "";
	       if (!quiet|!is_void(warn)) write, fwarn, format = " warning: %i tag matches for  %s \n", 
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
   hdoc_tail, f, title, table=1, doc=doc;
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
   _hdoc_indexbartags, htags, hfiles, toroot="";
   // if index.html exists, don't overwrite it - put the template
   // in index-raw.html instead

   if (f = open(to + "index.html", "r", 1)) {
      close, f;
      f = open (to + "index-raw.html", "w");
   } else {
      f = open(to + "index.html", "w");
   }
   title = "yorick reference";
   doc="index";
   hdoc_head, f, title, table=1, toroot="", doc=doc;


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
   wryte, f, "<a href = http://yorick.sourceforge.net>the official yorick homepage</a>";

   wryte, f, "<li>";
   wryte, f, "<a href = http://www.maumae.net/yorick/doc/index.php>";
   wryte, f, "the unofficial yorick homepage</a>";

   wryte, f, "<li>";
   wryte, f, "the <a href = http://yorick.sourceforge.net/yorickfaq.php>yorick faq</a>";
   wryte, f, "</ul>";

   wryte, f, "<hrule>";
   _hdoc_skip, f, 2;
   wryte, f, "This documentation was generated from the manual, README files, ";
   wryte, f, "and code documents of Yorick written by David H. Munro. <p>";

   wryte, f, "Yorick is free software, <a href=\"copyright.html\">copyright</a>";
   wryte, f, "of the Regents of the University of California";

   wryte, f, "<hrule>";

   hdoc_tail, f, title, table=1, toroot="", doc=doc;

   f = open (to + "copyright.html", "w");
   title = "yorick copyright";
   hdoc_head, f, title, table=1, toroot="";
   wryte, f, "<center><h1>Yorick</h1></center>";
   _hdoc_skip, f, 4;
   _hdoc_copyright, f;
   hdoc_tail, f, title, table=1, toroot="";
}




func hdoc_packagelist (from, tags, packinfo=, to=, xref_dir=) {

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
	    while ((line = rdline(f)) && !done) {
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

   if (!is_void(ptext)) {
   f = open (to + xref_dir + "packages.html", "w");
   title = "Yorick packages";
   doc="xref";
   hdoc_head, f, title, table=1, doc=doc;

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

   hdoc_tail, f, title, table=1, doc=doc;
   close, f;
   }
}


func hdoc_extract_embedded (template_file,to=) {
  /* DOCUMENT hdoc_extract_embedded [,template_file, to=to]
     
      extract documents embedded in the HTML documentation template.

      If TEMPLATE_FILE is specified, load this file as a template
      first.

      A template may have one or two line identical to
      "%content%". Everything above the first one is used as a header
      (see hdoc_head), everything after the last one as a footer (see
      hdoc_tail). Between these two line, it is possible to embed the
      content of one or several files. This function extract these
      files, setting their header and footer accordingly to the
      template.

      The definition of each of these files starts with a line of the
      form:
        %embedded:file_name:doc:toroot%Nice Long Title
      and finishes with the beginning of the next embedded file or
      with the last %content% line.
        
      The first '%' character must be the first character of the
      line. FILE_NAME is the actual filename to which the file will be
      extracted (as usual, TO is prepended to FILE_NAME. DOC is used
      for %onlydoc:doc% lines (see hdoc_read_template). TOROOT is the
      path from FILE_NAME to the root of the documentation tree.

      Except for this special first line, each line of the embedded
      document is parsed for occurrences of e.g. %title% or %toroot%
      using hdoc_parse and written fo FILE_NAME.

     SEE ALSO: hdoc_read_template, mktexi2html_init
       low level: hdoc_head, hdoc_tail, hdoc_parse
  */
  if (!is_void(template_file)) hdoc_read_template(template_file);
  ind=where(_hdoc_template=="%content%");
  if (is_void(to)) to="";
  if ((max(ind)-min(ind))<=1) return;
  scont=min(ind);
  contents=_hdoc_template(scont+1:max(ind)-1);
  ind=where(strgrep("^%embedded:",contents)(2,) != -1);
  if (numberof(ind)>1) indf=grow(ind(2:0)-1,numberof(contents));
  else indf=[numberof(contents)];
  for (n=1;n<=numberof(ind);n++) {
    col=strfind("%",contents(ind(n)),2)(1);
    title=strpart(contents(ind(n)),col+2:0);
    control=strpart(contents(ind(n)),2:col);
    control=pathsplit(control);
    fname=control(2);
    fname;
    doc=control(3);
    toroot=control(4);
    f=open(to+fname,"w");
    hdoc_head, f, title, table=1, toroot=toroot, doc=doc;
    hdoc_parse, f, scont+ind(n)+1:scont+indf(n), toroot=toroot,title=title,doc=doc;
    hdoc_tail, f, title, table=1, toroot=toroot, doc=doc;
    close, f;
  }
}



func hdoc_keywordindex (tags, keywords, to, xref_dir) {

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

   f = open (to + xref_dir + "keywords.html", "w");
   doc="xref";
   title = "Yorick keyword index";
   hdoc_head, f, title, table=1, doc=doc;

   wryte, f, ("<table cellspacing=2 border=0" +
		  " cellpadding=4 width=100\%>");
   wryte, f, "<tr>";
   for (i = 0; i < 26; i++) {
      ch = string (&(char('a' + i)));
      w = where (strpart (kwl, 1:1) == ch);
      if (!numberof(w)) continue;
      wryte, f, "<td>"; 
      wryte, f, "<a href = ../"+xref_dir+"keywords-"+ch+".html>"+ch+"</a>";
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
	    wryte, f, ("<a href = ../"+xref_dir+"keywords-" + suff +
		       ".html#" + strcomp(myw) + ">" + myw + "</a>");
	    wryte, f, "</td>";
	 } else {
	    wryte, f, "<td> </td>";
	 }
      }
      wryte, f, "</tr>";
   }
   wryte, f, "</table>";
   hdoc_tail, f, title, table=1, doc=doc;
   close, f;

   for (i = 0; i < 26; i++) {
      ch = string (&(char('a' + i)));
      w = where (strpart (kwl, 1:1) == ch);
      if (!numberof(w)) continue;
      fnm = to+xref_dir+"keywords-" + ch + ".html";
      if (!quiet) write, format = "\n %s   ", fnm;
      hdoc_keywordref, fnm, kwl(w), tags, xref_dir;
   }
   if (!quiet) write, "";
}



func hdoc_keywordref (fnm, kwl, tags, xref_dir) {
   name_list = tags(1,);
   file_list = tags (3,);
   doc_list = tags (6,);

   f = open (fnm, "w");
   doc="xref";
   hdoc_head, f, fnm, table=1, doc=doc; 

   wryte, f, ("<table cellspacing=2 border=0" +
		  " cellpadding=2 width=100\%>");
   wryte, f, "<tr>";
   for (i = 0; i < 26; i++) {
      ch = string (&(char(int(*pointer ("a"))(1) + i)));
      wryte, f, "<td>"; 
      wryte, f, "<a href = ../"+xref_dir+"keywords-" + ch + ".html>"+ch+"</a>";
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
	 defroot = "../"+xref_dir + file_list(w(1));
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
	    defroot = "../" + xref_dir + file_list(w(j));
	    wryte, f, (" <a href=\"" + defroot +
		        "-doc.html#"+ssa + "\">" + ssa + "</a>"+ 
		       ((j < nsa) ? ", &nbsp; " : " &nbsp; ")); 
	 }
      }

      wryte, f, "</td></tr>";
   }
   wryte, f, "</table>";
   hdoc_tail, f,fnm,  table=1, doc=doc;
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
       hdoc_head, f, title;
       ... write some html;
       hdoc_tail, f, title;

       with the 'table' option set, it puts a margin on the left and
       the text is put inside a table - the left column is the margin,
       the right column contains evverything else. The layout can be
       customized using hdoc_read_template().
 */

func wryte(f, line)
{
  write,format="%s\n", f, line;
}

func hdoc_read_file(fname,block) {
  // returns the content of a text file FNAME in a string array.
  // Read BLOCK lines at a time (default=1024).
  if (is_void(block)) block=1024;
  f=open(fname,"r");
  lines=rdline(f,1024);
  while (lines(0)) grow,lines,rdline(f,1024);
  return lines(where(lines));
}

func hdoc_read_template(fname) {
  /* DOCUMENT hdoc_read_template, fname

      load an HTML template for htmldoc.i functions.

      FNAME is the name of an HTML file, from which the layout of the
      documentation (navigation bars etc.) will be determined.

      The functions in htmldoc.i treat certain strings in the template
      in a special way. The first few of them must e alone on the
      line, wihtout heading or trailing blank:
            
        Either one or two lines must contain exactly the text
        "%content%". The content of each page will be placed there,
        replacing anything between the first and last line equal to
        "%content%". The space in-between can be used to embed
        documents, see hdoc_extract_embedded for details.

        %startbody% will be replaced using _hdoc_startbody. This
        special line should be used at most once.

        %indexbar% produces a simple index bar using _hdoc_indexbar.

        %skip% skips a line (replaced with "&nbsp;<br>").

      Certain patterns are recognized when they are found at the
      beginning of a line:

        %onlydoc:doc% where DOC is a short document ID: the remaining
        of the line will appear only in files pertaining to the
        document DOC. Certain IDs are predefined: "index" for the main
        index and "xref" for most of the pages produced by
        mkhtmldoc. In addition, "manual" is used for the Yorick Manual
        (see mktexi2html_init) and "qref" is reserved for the
        reference card. You can use other IDs for use e.g. with your
        custom embedded documents (see hdoc_extract_embedded).

        %embedded:....% is used to define embedded documents, see
        hdoc_extract_embedded.

      Finally, a few patterns are recognized anywhere in the document:

        %title% is replaced with the (long) document title. It should
        probably be used at least once, between <title> and </title>.

        %toroot% is replaced with the path from the current file to
        the HTML root. It is often "../" and sometimes empty.
      
     SEE ALSO: hdoc_extract_embedded, mktexi2html_init
  */
  extern _hdoc_template;
  _hdoc_template=hdoc_read_file(fname);
}
 
extern _hdoc_template;
_hdoc_template=
["<html> <head> <title>",
 "%title%",
 "</title> </head>",
 "%startbody%",
 "<TABLE border=\"0\" cellpadding=\"5\" cellspacing=\"0\" width=\"100%\">",
 "<TR><TD valign=\"TOP\" width=\"150\" bgcolor=\"#bbddff\"><BR>",
 "<IMG src=\"../images/ybanng.jpg\" border=\"0\" hspace=\"0\" vspace=\"0\"",
 " alt=\"yorick banner\">",
 "  <FONT face=\"Arial,Helvetica\">",
 "  <P><B><A href=\"../index.html\">Home</A></B></P>",
 "  <P><B><A href=\"../manual/yorick.html\">Manual</A></B></P>",
 "  <P><B><A href=\"../html_xref/packages.html\">Packages</A></B></P>",
 "  <P><B><A href=\"../html_xref/global-index.html\">Global Index</A></B></P>",
 "  <P><B><A href=\"../html_xref/keywords.html\">Keywords</A></B></P>",
 "  <P><B><A href=\"../refcard/index.html\">Quick Reference</A></B></P>",
 "  </FONT>",
 "</TD><TD valign=\"TOP\">",
 "%content%",
 "</td></tr>",
 "</table>",
 "&nbsp;<br>",
 "&nbsp;<br>",
 "&nbsp;<br>",
 "</body>",
 "</html>"];

func hdoc_parse (g, spec, toroot=,title=,doc=,text=) {
  /* DOCUMENT hdoc_parse, g, spec, title=title [, toroot=toroot, doc=docid, text=text]

      Parse lines specified by index specification SPEC from the
      string array TEXT (defaults to currently loaded template) write
      them to file stream G.

      TITLE has no default and must be set, TOROOT defaults to "../",
      DOC is the doc ID for %onlydoc:doc% matching: a line starting
      with "%onlydoc:line_doc_id" will be output only if LINE_DOC_ID
      == DOCID.

      This function implements some of the special strings detailed in
      hdoc_read_template: %startbody%, %indexbar%, %skip%,
      %onlydoc:...%, %title% and %toroot%.

     SEE ALSO: hdoc_read_template
  */
  if (is_void(toroot)) toroot = "../";
  if (is_void(doc)) doc = "";
  if (is_void(text)) lines=_hdoc_template(spec);
  else lines=text(spec);
  for (i=1;i<=numberof(lines);i++) {
    line=lines(i);
    if (line=="%startbody%") _hdoc_startbody, g;
    else {
      if (line=="%indexbar%") _hdoc_indexbar, g;
      else {
        if (line=="%skip%") _hdoc_skip, g, 1;
        else {
          if (strgrep("^%onlydoc:",line)(2)!=-1) {
            if (strgrep("^%onlydoc:"+doc+"%",line)(2)==-1) continue;
            line=strpart(line,strgrep("^%onlydoc:"+doc+"%",line)(2)+1:0);
          }
          line=streplace(line,strgrep("%title%",line),title);
          line=streplace(line,strgrep("%toroot%",line),toroot);
          wryte, g, line;
        }
      }
    }
  }
}

func hdoc_head (g, title, table=, toroot=, doc=) {
   if (table) {
     last_line=min(where(_hdoc_template=="%content%"))-1;
     hdoc_parse, g, 1:last_line, toroot=toroot, title=title, doc=doc;
   } else {
     wryte, g, "<html> <head> <title>";
     wryte, g, title;
     wryte, g, "</title>";
     _hdoc_startbody, g;
     _hdoc_indexbar, g;
     _hdoc_skip, g, 3;
   }
}

func hdoc_tail (g, title, table=, toroot=, doc=) {
  if (do_disclaimer) {
    wryte, g, "<P><HR><a href=\"http://www.llnl.gov/disclaimer.html\">";
    wryte, g, "<small>LLNL Disclaimers</small></a></P>";
  }
  if (table) {
    first_line=max(where(_hdoc_template=="%content%"))+1;
    hdoc_parse, g, first_line:0, toroot=toroot, title=title, doc=doc;
  } else {
    _hdoc_skip, g, 3;
    wryte, g, "</body>";
    wryte, g, "</html>";
  }
}

func hdoc_headtail (src, dest, rm, title= , toroot=, doc=) {
  /* DOCUMENT hdoc_headtail, src, dest [, rm, title= , toroot=, doc=]

      Adds HTML header and footer specified by currently loaded
      template to file SRC. Saves the result to DEST. Removes SRC
      unless "rm=0" is specified.
     
   */
  if (is_void(rm)) rm=1;
   g = open (dest, "w");
   if (is_void(title)) title = dest;
   hdoc_head, g, title, table=1, toroot=toroot, doc=doc;
   line = hdoc_read_file(src);
   hdoc_parse, g, 1:0, text=line, toroot=toroot, doc=doc, title=title;
   hdoc_tail, g, title, table=1, toroot=toroot, doc=doc;
   close, g;
   if (rm) remove, src;
}


func _hdoc_skip (f, n) {
   if (n) {
      for (i = 1; i <= n; i++) wryte, f, "&nbsp;<br>";
   } else {
      wryte, f, "&nbsp;<br>&nbsp;<br>";
      wryte, f, "&nbsp;<br>&nbsp;<br>";
   }
}



func _hdoc_indexbartags (&htags, &hfiles, toroot=) {
  // this (obsolete) routine should be customized.
  if (is_void(toroot)) toroot="../";
   htags = ["home", "manual",  "packages", "index", "keywords"];
   hfiles = toroot+["index.html", "manual/yorick.html", 
	     "html_xref/packages.html", 
	     "html_xref/global-index.html", "html_xref/keywords.html"];
}

func _hdoc_indexbar(f) {
  // this (obsolete) routine should be customized.
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



func hdoc_wrap (dir, quiet=) {
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
	 if (!quiet) write, format = "html file %s contains no <BODY> statement ", fnm;
	 if (!quiet) write, "- nothing done";
	 
      } else {
	 hdoc_headtail, tmpfile, fnms(i); 
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

func mktexi2html_init (infile,outfile) {
  /* DOCUMENT mktexi2html_init [, infile, outfile]

      create a TeXi2HTML init file using the currently loaded HTML
      template (see hdoc_read_template).

      mktexi2html_init will parse the hdoc template to fill these
      TeXi2HTML variables (see "info texi2html"):
      $EXTRA_HEAD, $AFTER_BODY_OPEN, $PRE_BODY_CLOSE.

      mktexi2html_init needs an input file INFILE (default:
      "texi2html.tpl"), which is a complete, valid TeXi2HTML init file
      except that these three variables are defined with the following
      syntax:

      $VARIABLE = <<'EOF';
      %variable%
      EOF

      Therefore, OUTFILE is a copy of INFILE with the three special
      lines
       %extra_head%
       %after_body_open%
       %pre_body_close%
      have been replaced with parts of the hdoc template. These three
      lines must appear in that order.

      mktexi2html_init works by detecting certain strings in the hdoc
      template.

        EXTRA_HEAD is everything between </TITLE> and </HEAD>
      
        AFTER_BODY_OPEN is everything between either %startbody% or
        <BODY ...>and %content%

        PRE_BODY_CLOSE is everything between the last %content% and
        </BODY>

      The case is not important for the HTML tags (but it is for the
      %...% special strings). The HTML tags must be alone on their
      line as any leading or trailing text will not be output (see
      below for a PHP trick, though). The entire <BODY ... > tag must
      be on a single line, but can contain complex definitions (these
      definitions will be ignored).

      If your template does not contain these strings, for instance
      because your HTML code is really PHP, you can try to put
      something like "%onlydoc:never%</TITLE>" where you would like
      them to be (see hdoc_read_template). As long as you dont declare
      any embedded document with "never" as a short doc ID, these
      lines will never appear in any of the produced documentation,

      Once mktexi2html_init has determined which lines to put in which
      variable, it does parse them for replacing the special strings
      such as %title%, %toroot% and %onlydoc:doc%. mktexi2html_init is
      meant for typesetting the Yorick Manual, so the doc ID it uses
      is "manual". Therefore, lines beginning with "%onlydoc:" will
      not be output, unless they actually start with
      "%onlydoc:manual%".

     SEE ALSO: hdoc_read_template, hdoc_extract_embedded
  */
  if (is_void(infile)) infile="texi2html.tpl";
  if (is_void(outfile)) outfile="texi2html.init";
  toroot="../";
  title="The Yorick Manual";
  doc="manual";
  in=hdoc_read_file(infile);
  f = open (outfile, "w");

  wryte, f, in(1:min(where(in=="%extra_head%"))-1);
  // EXTA_HEAD is everything between </TITLE> and </head>
  // find </TITLE>
  junk = strfind("</title>",_hdoc_template,case=0);
  linea = min (where(junk(2,) != -1));
    //columna = junk(2, linea);
  // find </head>
  junk = strfind("</head>",_hdoc_template,case=0);
  lineb = min (where(junk(2,) != -1));
    //columnb = junk(1, lineb);
  // write the end of line A, everything between line A and B, and the beginning of line B.
  // Warning: the bits of liine A and B are not parsed for toroot.
    //if (linea < lineb) {
    //if (columna < strlen(_hdoc_template(linea))) wryte, f, strpart(_hdoc_template(linea), columna+1:0);
  for (i=linea+1; i < lineb; i++) hdoc_parse, f, i, toroot=toroot, title=title, doc=doc;
    //if (columnb > 1) wryte, f, strpart(_hdoc_template(lineb), 1:columnb-1);
    //} else {
    // if (columna+1 < columnb) wryte, f, strpart(_hdoc_template(linea), columna+1:columnb-1);
    //}

  wryte, f, in(min(where(in=="%extra_head%"))+1:min(where(in=="%after_body_open%"))-1);
  // AFTER_BODY_OPEN is everything between %startbody% and %content%
  if (anyof(_hdoc_template == "%startbody%" )) linea = min ( where ( _hdoc_template == "%startbody%" ) );
  else linea = min ( where ( strmatch(_hdoc_template, "<body", 1 ) ));
  lineb = min ( where ( _hdoc_template == "%content%" ) );
  for (i=linea+1; i < lineb; i++) hdoc_parse, f, i, toroot=toroot, title=title, doc=doc;

  wryte, f, in(min(where(in=="%after_body_open%"))+1:min(where(in=="%pre_body_close%"))-1);
  // PRE_BODY_CLOSE is everything between %content% and </BODY>
  linea = max ( where ( _hdoc_template == "%content%" ) ) ;
  junk = strfind("</body>",_hdoc_template,case=0);
  lineb = min (where(junk(2,) != -1));
    //columnb = junk(1, lineb);
  for (i=linea+1; i < lineb; i++) hdoc_parse, f, i, toroot=toroot, title=title, doc=doc;
    //if (columnb > 1) wryte, f, strpart(_hdoc_template(lineb), 1:columnb-1);

  wryte, f, in(min(where(in=="%pre_body_close%"))+1:0);

  close, f;
}

if (batch()) {
  args = get_argv();
  junk=args(where(strpart(args,1:1)!="-"));
  do_disclaimer = (numberof(junk)==2 && junk(2)=="llnl");
  if (anyof(strpart(args,1:1)=="-")) {
    junk=args(where(strpart(args,1:1)=="-"));
    for (i=1;i<=numberof(junk);i++) {
      if (junk(i)=="--quiet" | junk(i)=="-q")
        quiet=1;
      else if (junk(i)=="--nosrc" | junk(i)=="-s")
        nosrc=1;
      else if (junk(i)=="--nofunc" | junk(i)=="-f")
        nofunc=1;
      else if (junk(i)=="--llnl" | junk(i)=="-l")
        do_disclaimer=1;
      else if (strpart(junk(i),1:strlen("--warn="))=="--warn=")
        warn=strpart(junk(i),strlen("--warn=")+1:0);
      else if (strpart(junk(i),1:strlen("--from="))=="--from=")
        from=pathsplit(strpart(junk(i),strlen("--from=")+1:0),delim=":");
      else if (strpart(junk(i),1:strlen("--to="))=="--to=")
        to=strpart(junk(i),strlen("--to=")+1:0);
      else if (strpart(junk(i),1:strlen("--template="))=="--template=")
        template=strpart(junk(i),strlen("--template=")+1:0);
      else if (strpart(junk(i),1:strlen("--keywords="))=="--keywords=")
        keywords=strpart(junk(i),strlen("--keywords=")+1:0);
      else if (strpart(junk(i),1:strlen("--aliases="))=="--aliases=")
        aliases=strpart(junk(i),strlen("--aliases=")+1:0);
      else if (strpart(junk(i),1:strlen("--packinfo="))=="--packinfo=")
        packinfo=strpart(junk(i),strlen("--packinfo=")+1:0);
      else if (strpart(junk(i),1:strlen("--xref-dir="))=="--xref-dir=")
        xref_dir=strpart(junk(i),strlen("--xref-dir=")+1:0);
      else
        continue;
    }
  }
  if (!quiet) {
    if (!do_disclaimer) write, "creating yorick html document set";
    else write, "creating yorick html documents set (LLNL version)";
  }
  mkhtmldoc,from=from, to=to, keywords=keywords, packinfo=packinfo,
    template=template, aliases=aliases, nosrc=nosrc, nofunc=nofunc,
    warn=warn, quiet=quiet, xref_dir=xref_dir;
}
