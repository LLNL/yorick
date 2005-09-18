/*
 * $Id: show.i,v 1.1 2005-09-18 22:06:08 dhmunro Exp $
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func show(f, pat)
/* DOCUMENT raw_show, f
         or raw_show, f, pat
         or raw_show, f, 1
     prints a summary of the variables contained in binary file F.
     If there are too many variables, use the second form to select
     only those variables whose first few characters match PAT.
     In the third form, continues the previous show command where it
     left off -- this may be necessary for files with large numbers of
     variables.
     The variables are printed in alphabetical order down the columns.
     The print function can be used to obtain other information about F.
   SEE ALSO: openb, jt, jc
 */
{
  /* NOTE-- codger confused if extern first on line */
  /* state variables for show function */  extern _show_more, _show_pat;
  if (typeof(pat)=="string") {
    _show_pat= pat;
    _show_more= array(0, 1:2);
  } else if (!pat || is_void(_show_pat) || is_void(_show_more)) {
    _show_pat= string(0);
    _show_more= array(0, 1:2);
  }

  both= get_vars(f);

  npass= both(2)? 2 : 1;
  for (i=1 ; i<=npass ; i++) {
    vars= *both(i);

    /* apply pattern matching if requested */
    if (strlen(_show_pat)) {
      vars= vars(where(strmatch(strpart(vars,1:strlen(_show_pat)),
                                _show_pat)));
      if (numberof(vars)) {
        keep= array(string, 1:numberof(vars) /* be sure origin is 1 */);
        keep(:)= vars;
        vars= keep;
      }
    }

    /* quit if nothing to do */
    n= numberof(vars);
    if (!n || (_show_more(i) && _show_more(i)>=n)) {
      if (i==1) {
        if (strlen(_show_pat))
          write, "<no"+(_show_more(i)?" more":"")+
            " non-record variables begin with "+_show_pat+">"
        else
          write, "<no"+(_show_more(i)?" more":"")+
            " non-record variables>";
      } else {
        write, "<no"+(_show_more(i)?" more":"")+
            " record variables begin with "+_show_pat+">"
      }
      continue;
    }

    /* put the list into alphabetical order, and make sure that both
       the variable list and the length of each name are reasonable */
    vars= vars(sort(vars));
    if (_show_more(i)) {
      n-= _show_more(i);
      keep= array(string, 1:n /* be sure origin is 1 */);
      keep(:)= vars(_show_more(i)+1:);
      vars= keep;
    }
    if (n>64) {
      keep= array(string, 1:65 /* be sure origin is 1 */);
      keep(1:64)= vars(1:64);
      keep(65)= "<<MORE>>";
      vars= keep;
    }
    longest= max(strlen(vars));
    if (longest>72) {
      keep= where(strlen(vars)>72);
      vars(keep)= strpart(vars(keep), 1:69)+"...";
      longest= 72;
    }

    /* split the vars into columns reading alphabetically down the
       columns */
    ncols= 78/(longest+5);
    nrows= 1+(numberof(vars)-1)/ncols;
    keep= array(string, 1:nrows, 1:ncols /* be sure origin is 1 */);
    keep(1:numberof(vars))= vars;
    vars= transpose(keep);

    /* list variables */
    if (i==1) write, print(n)(1)+(_show_more(1)?" more":"")+
                     " non-record variables:"
    else write, print(n)(1)+(_show_more(2)?" more":"")+
                     " record variables:"
    write, format="     %"+print(longest)(1)+"s", linesize=78, vars;
    write, "";  /* adds newline at end */
    _show_more(i)+= 64;
  }

  if (npass>1 && is_void(pat)) {
    recs= print(f);
    recs= recs(where(strmatch(recs, "  Current record")));
    if (!is_void(recs)) write, recs;
    times= get_times(f);
    if (!is_void(times)) {
      write, format="   Ranging from time= %e to time= %e\n",\
        min(times), max(times);
    } else {
      ncycs= get_ncycs(f);
      if (!is_void(ncycs)) {
        write, format="   Ranging from ncyc= %d to ncyc= %d\n",\
          min(ncycs), max(ncycs);
      }
    }
  }
}

/*--------------------------------------------------------------------------*/
