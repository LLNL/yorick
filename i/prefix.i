/*
 * $Id: prefix.i,v 1.1 2005-09-18 22:06:04 dhmunro Exp $
 * Functions to read and write arrays of real numbers tagged by
 * a prefix at the beginning of each line.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func prefix_find(f, prefix, delimit=)
/* DOCUMENT prefix_find(f, prefix)
     scan to the first line of text file F which begins with the
     blank delimited prefix PREFIX.  (You may specify other delimiters
     by giving prefix_find a DELIMIT keyword, whose value will be
     passed to the strtok function.)  The return value is 1 if such
     a line was found, 0 if not.  In the first case, F will be positioned
     to reread the prefixed line; in the second case, F will be at the
     end-of-file.

   SEE ALSO: prefix_read, prefix_write, strtok
 */
{
  if (is_void(delimit)) delimit= " \t\n";
  found= 0;
  while (line= rdline(f)) {
    if (strtok(line,delimit)(1)==prefix) {
      found= 1;
      backup, f;
      break;
    }
  }
  return found;
}

func prefix_read(f, prefix, delimit=, comment=)
/* DOCUMENT value_array= prefix_read(f, prefix)
     reads lines of text file F which begin with the blank delimited
     prefix PREFIX.  (You may specify other delimiters by giving
     prefix_read a DELIMIT keyword, whose value will be passed to the
     strtok function.)  Stops when a line not beginning with that
     prefix is encountered.  You may also supply a COMMENT keyword,
     which should be a function accepting a string argument and
     returning 0 to indicate that the line is not a comment line,
     and 1 to indicate that it is a comment.  By default, blank lines
     and lines beginning with "#" are taken as comments and skipped.

     The returned VALUE_ARRAY is [] if no PREFIX lines were found,
     and an array of type double and length equal to the total number
     of numbers Ni:

       prefix N1 N2 N3 N4
       prefix N5 N6
       prefix ... Nn

   SEE ALSO: prefix_find, prefix_write, prefix_comment, strtok
 */
{
  if (is_void(delimit)) delimit= " \t\n";
  if (is_void(comment)) comment= prefix_comment;
  n= l= 0;
  values= array(pointer, 100);
  target= array(0.0, 256);   /* max number of values per line */
  while (line= rdline(f)) {
    tok= strtok(line,delimit);
    if (tok(1)==prefix) {
      nn= sread(prefix_unD(tok(2)), target);
      if (nn>0) {
        n+= nn;
        if (l==numberof(values)) grow, values, array(pointer, 100);
        values(++l)= &target(1:nn);
      }
    } else if (!comment(line)) {
      backup, f;
      break;
    }
  }
  if (!n) return [];
  result= array(0.0, n);
  n= 0;
  for (i=1 ; i<=l ; i++) {
    target= *values(i);
    nn= n+numberof(target);
    result(n+1:nn)= target;
    n= nn;
  }
  return result;
}

func prefix_comment(line)
/* DOCUMENT prefix_comment(line)
     the default comment detector function for prefix_read, makes
     blank lines and any line beginning with "#" as its first non-blank
     character a comment.
 */
{
  tok= strtok(line)(1);
  return (strlen(tok)<1 || strpart(tok,1:1)=="#");
}

func prefix_unD(text)
{
  if (text) {
    c= *pointer(text);
    d= where(c=='D' | c=='d');
    if (numberof(d)) {
      c(d)= 'e';
      text= string(&c);
    }
  }
  return text;
}

func prefix_write(f, prefix, values, ndigits=, width=)
/* DOCUMENT prefix_read, f, prefix, value_array
     writes lines of text file F which begin with the prefix PREFIX:
     
       prefix N1 N2 N3 N4 N5
       prefix N6 N7 N8 N9 N10
       prefix N11 N12

     The format is %14.6e by default, but you can adjust the ".6" by
     specifying an NDIGITS keyword (6 is the default).

     Yorick will put as many numbers as fit within 79 characters by
     default, and each successive line begins with PREFIX.  You can
     change this default line width by specifying a WIDTH keyword
     (default 79).

   SEE ALSO: prefix_find, prefix_read
 */
{
  if (is_void(width)) width= 79;
  if (is_void(ndigits)) ndigits= 6;
  else if (ndigits>24) ndigits= 24;
  else if (ndigits<0) ndigits= 0;
  ntot= 8+ndigits;
  format= swrite(format="%%%ld.%lde",ntot,ndigits);
  l= (width-strlen(prefix))/ntot;
  if (l<1) l= 1;
  n= numberof(values);
  nlines= n/l;
  j= n%l;
  text= array(prefix, (j? nlines+1 : nlines));
  for (i=1 ; i<=l ; i++) {
    /* format the text one column at a time --
       some columns may be one shorter than others */
    if (i>n) break;
    column= swrite(format=format, values(i:0:l));
    if (j && i>j) text(1:nlines)+= column;
    else text+= column;
  }
  write, f, format="%s\n", text;
}
