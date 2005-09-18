/*
 * $Id: mkdoc.i,v 1.1 2005-09-18 22:06:00 dhmunro Exp $
 * Alphabetize DOCUMENT comments and prepare for line printer.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func mkdoc(filename, outname, lpp)
/* DOCUMENT mkdoc, filename
         or mkdoc, filename, outname, lpp
     alphabetizes and indexes the DOCUMENT comments in FILENAME, and
     formats into "dictionary-like" pages for printing.  If OUTNAME
     is not given or nil, the output file will be FILENAME with ".doc"
     replacing the ".i".  If LPP is not given, it defaults to 58 --
     the maximum number of available lines per page of output.
     (Use 55 to be able to print with "lpr -p" style page headings.)
     FILENAME can be an array of strings to combine several include
     files into a single document.
   SEE ALSO: help
 */
{
  extern mkdoc_lpp;

  if (is_void(lpp)) mkdoc_lpp= 58;
  else mkdoc_lpp= lpp;
  name_list= doc_list= oname= [];
  inames= filename;

  for (ii=1 ; ii<=numberof(filename) ; ii++) {
    f= open(filename(ii));

    /* strip off non-directory part of filename */
    name= [string(0), filename(ii)];
    do {
      name= strtok(name(2), "/\:");
    } while (name(2));
    name= name(1);
    inames(ii)= name;

    /* get output file name */
    if (is_void(oname)) {
      if (is_void(outname)) {
        oname= name;
        if (strpart(oname, -1:0)==".i") oname= strpart(oname, 1:-2);
        oname+= ".doc";
      } else {
        oname= outname;
      }
    }

    /* scan the file to accumulate lists of function/variable/keyword names
       and the corresponding document strings */
    mkdoc_scan, f;
    close, f;
  }

  /* alphabetize the lists */
  order= sort(name_list);
  name_list= name_list(order);
  doc_list= doc_list(order);

  /* make the title page */
  f= open(oname, "w");
  mkdoc_title, f, inames, name_list;

  n= numberof(name_list);
  fragment= [];
  /* loop on output pages */
  while ((nlines= numberof(fragment)) || n) {
    nleft= mkdoc_lpp-3;  /* leave 3 lines for heading */
    if (nlines) {
      /* part of the last entry has spilled onto the new page */
      if (nlines < nleft) {
        /* ...it fits on this page */
        page= fragment;
        fragment= [];
        nleft-= nlines;
      } else {
        /* ...it fills this page completely, too --
           be sure at least 6 lines on next page */
        if (nlines < nleft+6) {
          page= fragment(1:-6);
          fragment= fragment(-5:0);
        } else {
          page= fragment(1:nleft);
          fragment= fragment(nleft+1:0);
        }
        mkdoc_page, f, name, name, page;
        continue;
      }
      fname= name;
      not_top= 1;
    } else {
      page= [];
      fname= name_list(1);
      not_top= 0;
    }

    /* loop on entries for this page */
    while (n) {
      oname= name;
      name= name_list(1);
      fragment= *doc_list(1);
      nlines= 1+numberof(fragment);
      if (nleft >= nlines+not_top) {
        /* this entire entry fits on this page */
        if (not_top) grow, page, "";
        grow, page, swrite(format="%75s", name), fragment;
        fragment= [];
        nleft-= nlines+not_top;
      } else if (nlines+not_top>7 && nleft>7 &&
                 (nlines-nleft>=6 || nlines>12)) {
        /* this entry runs over onto following pages */
        if (not_top) grow, page, "";
        nleft-= 1+not_top;
        if (nlines-nleft<6) nlines-= 6;
        else nlines= nleft;
        grow, page, swrite(format="%75s", name), fragment(1:nlines);
        fragment= fragment(nlines+1:);
        nleft= 0;
      } else {
        /* this entire entry goes on next page */
        name= oname;
        fragment= [];
        break;
      }
      if (--n) {
        name_list= name_list(2:);
        doc_list= doc_list(2:);
      }
      if (nleft<3 || numberof(fragment)) break;
      not_top= 1;
    }

    /* output this page with dictionary headings */
    mkdoc_page, f, fname, name, page;
  }

  close, f;
}

func mkdoc_scan(f)
{
  /* Add extern, local, func, and struct declaration/definition names to
     the name_list.  For extern and func, add the DOCUMENT comment to the
     doc_list.  If no DOCUMENT comment appears within 10 non-blank lines,
     skip to the next extern or func.  If subsequent extern lines precede
     the DOCUMENT comment, generate a cross-reference SEE ... to the
     first extern of the group.  For struct, the entire struct definition,
     which is presumably commented, becomes the documentation text.  */
  extern name_list, doc_list;

  while (line= rdline(f)) {

    split= strtok(line);
    doctext= [];

    if (split(1)=="func" || split(1)=="extern" || split(1)=="local") {
      name= strtok(split(2), " \t(,;");
      if (split(1)!="local") crossref= [];
      else crossref= mkdoc_cross(1, name(2), []);
      name= name(1);
      count= 10;        /* like help_worker function defined in std.i */
      while ((line= rdline(f)) && count--) {
        split= strtok(line);
        if (!split(1)) break;
        if (strmatch(line, "/* DOCUMENT")) {
          do {
            grow, doctext, [line];
            if (strmatch(line, "*/")) break;
          } while (line= rdline(f));
        } else if (split(1)=="extern") {
          crossref= mkdoc_cross(0, split(2), crossref);
          if (count==9) count= 10;
        } else if (split(1)=="local") {
          crossref= mkdoc_cross(1, split(2), crossref);
          if (count==9) count= 10;
        }
      }

    } else if (split(1)=="struct") {
      name= strtok(split(2), " \t(,;")(1);
      gotopen= 0;
      do {
        grow, doctext, [line];
        if (!gotopen) gotopen= strmatch(line, "{");
        if (gotopen && strmatch(line, "}")) break;
      } while (line= rdline(f));
      crossref= [];
    }

    if (!is_void(doctext)) {
      grow, name_list, [name];
      grow, doc_list, [&doctext];
      n= numberof(crossref);
      for (i=1 ; i<=n ; i++) {
        grow, name_list, crossref(i);
        grow, doc_list, &["     /* SEE "+name+"     */"];
      }
    }
  }
}

func mkdoc_cross(loc, names, crossref)
{
  split= strtok(names, " \t,;");
  cross= crossref;
  while (split(1) && !strmatch(split(1), "/*")) {
    grow, cross, split(1:1);
    if (!loc) break;
    split= strtok(split(2), " \t,;");
  }
  return cross;
}

func mkdoc_title(f, inames, name_list)
{
  extern mkdoc_lpp;

  write, f, format="\n\n\n"+
    "                          Yorick Documentation\n"+
    "                for functions, variables, and structures\n"+
    "                         defined in file %s\n"+
    "                   Printed: %s\n", inames(1), timestamp();

  write, f, format="\n   %s\n\n", "Contents:";

  nleft= mkdoc_lpp-10;

  n= numberof(name_list);
  ncols= 1;
  while (n/ncols > nleft-3) ncols++;
  width= 80/ncols;
  len= max(strlen(name_list));
  if (len > width-3) {
    /* Contents will overflow onto subsequent page(s).  Hopefully rare.  */
    ncols= 80/(len+3);
    width= 80/ncols;
  }

  format= (width-len)/2 + 1;
  width-= format;
  format= swrite(format="%"+print(format)(1)+"s", "");  /* leading blanks */
  format= format+"%-"+print(width)(1)+"s";           /* e.g.- "    %-12s" */

  len= (n-1)/ncols + 1;
  for (i=1 ; i<=len ; i++) {
    line= "";
    for (j=i ; j<=n ; j+= len) line+= swrite(format=format, name_list(j));
    j= strlen(line);
    while (strpart(line, j:j)==" ") j--;
    write, f, format="%s", strpart(line, 1:j)+"\n";
  }
}

func mkdoc_page(f, fname, name, page)
{
  extern mkdoc_lpp;

  write, f, format="\f\n%75s\n\n", swrite(format="FROM %s TO %s",
                                          fname, name);
  n= numberof(page);
  for (i=1 ; i<=n ; i++) write, f, format="%s\n", page(i);
}
