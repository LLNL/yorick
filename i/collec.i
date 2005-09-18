/*
 * $Id: collec.i,v 1.1 2005-09-18 22:05:54 dhmunro Exp $
 * Collect variables through all times in a time history file.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func collect(f, name)
/* DOCUMENT result= collect(f, name_string)
     scans through all records of the history file F accumulating the
     variable NAME_STRING into a single array with one additional
     index varying from 1 to the number of records.

     NAME_STRING can be either a simple variable name, or a name
     followed by up to four simple indices which are either nil, an
     integer, or an index range with constant limits.  (Note that
     0 or negative indices count from the end of a dimension.)

     Examples:
        collect(f, "xle")        -- collects the variable f.xle
        collect(f, "tr(2,2:)")   -- collects f.tr(2,2:)
        collect(f, "akap(2,-1:0,)") -- collects f.akap(2,-1:0,)
                     (i.e.- akap in the last two values of its
                            second index)

   SEE ALSO: get_times
 */
{
  name= strtok(name, " \t(");
  var= name(1);
  name= name(2);

  /* this is yucky -- need a query function for current record number */
  n0= where(strmatch(print(f),"Current record"));
  n= 0;
  if (numberof(n0)) {
    n0= n0(1);
    sread, print(f)(n0), n0,n, format=" Current record is number %ld of %ld";
  } else {
    error, "no record structure for file?";
  }

  local a,b,c,d,e,name;

  if (!collect_get(a, name)) {
    jr, f, 1;
    rslt= array(get_member(f,var), n);
    for (i=2 ; i<=n ; i++) {
      jr, f, i;
      rslt(.., i)= get_member(f,var);
    }
  } else if (!collect_get(b, name)) {
    jr, f, 1;
    rslt= array(get_member(f,var)(a), n);
    for (i=2 ; i<=n ; i++) {
      jr, f, i;
      rslt(.., i)= get_member(f,var)(a);
    }
  } else if (!collect_get(c, name)) {
    jr, f, 1;
    rslt= array(get_member(f,var)(a, b), n);
    for (i=2 ; i<=n ; i++) {
      jr, f, i;
      rslt(.., i)= get_member(f,var)(a, b);
    }
  } else if (!collect_get(d, name)) {
    jr, f, 1;
    rslt= array(get_member(f,var)(a, b, c), n);
    for (i=2 ; i<=n ; i++) {
      jr, f, i;
      rslt(.., i)= get_member(f,var)(a, b, c);
    }
  } else if (!collect_get(e, name)) {
    jr, f, 1;
    rslt= array(get_member(f,var)(a, b, c, d), n);
    for (i=2 ; i<=n ; i++) {
      jr, f, i;
      rslt(.., i)= get_member(f,var)(a, b, c, d);
    }
  } else {
    error, "too many (>4) subscripts for collect";
  }

  jr, f, n0;
  return rslt;
}

func collect_get(&ndx, &name)
{
  if (!name) return 0;

  argc= *pointer(name);
  list= where(argc==',' | argc==')');
  if (numberof(list)) {
    list= list(1);
    arg= list>1? strpart(name,1:list-1) : "";
    name= list<strlen(name)? strpart(name,list+1:0) : string(0);
  } else {
    arg= name;
    name= string(0);
  }

  argc= *pointer(arg);
  list= where(argc==':');
  n= numberof(list);

  if (n==0) {
    ndx= collect_num(arg);
    return 1;

  } else {
    l= list(1);
    mn= l<2? [] : collect_num(strpart(arg, 1:l-1));
    if (n==1) m= 1;
    else m= list(2);
    mx= l>=strlen(arg)? [] : collect_num(strpart(arg, l+1:m-1));
    if (n==1) {
      ndx= collect_rng(mn:mx);
      return 2;
    } else if (n==2) {
      inc= m>=strlen(arg)? [] : collect_num(strpart(arg, m+1:0));
      ndx= collect_rng(mn:mx:inc);
      return 3;
    }
  }

  error, "index garbled or too complicated";
  return 0;
}

func collect_num(text)
{
  val= 0;
  s= string(0);
  n= sread(format="%ld%[^ \t\n]", text, val, s);
  if (n==1) return val;
  if (n==0 && !strtok(text," \t")(1)) return [];
  error, "index too complicated-- not nil, number, or const range";
  return string(0);
}

func collect_rng(x) { return x; }
