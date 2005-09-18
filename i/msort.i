/*
 * $Id: msort.i,v 1.1 2005-09-18 22:06:01 dhmunro Exp $
 * Multiple key sort routine.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func msort(x, ..)
/* DOCUMENT msort(x1, x2, x3, ...)
     returns an index list which sorts the array X1 into increasing
     order.  Where X1 values are equal, the list will sort X2 into
     increasing order.  Where both X1 and X2 are equal, X3 will be
     in increasing order, and so on.  Finally, where all of the keys
     are equal, the returned list will leave the order unchanged
     from the input keys.

     The Xi may be numbers or strings (e.g.- X1 could be an integer
     while X2 was a string, and X3 was a real).  The Xi must all be
     conformable, and each dimension of X1 must be as large as the
     corresponding dimension of any otehr Xi.

     Hence, msort(x) will return the same list as sort(x), except
     where the values of x are equal, in which case msort leaves
     the order unchanged, while sort non-deterministically permutes
     equal elements.  This feature may cost a factor of two in speed,
     so don't use it unless you really need it.  In general, msort
     will call sort up to twice per input argument.

   SEE ALSO: sort, msort_rank
 */
{
  mxrank= numberof(x)-1;
  local list;
  rank= msort_rank(x, list);
  if (max(rank)==mxrank) return list;

  norm= 1.0/(mxrank+1.0);
  if (1.0+norm == 1.0) error, pr1(mxrank+1)+" is too large an array";

  n= more_args();
  while (n--) {
    x= next_arg();
    rank+= msort_rank(x)*norm;     /* adjust rank for next key */
    rank= msort_rank(rank, list);  /* renormalize adjusted rank */
    if (max(rank)==mxrank) return list;
  }

  /* use indgen as final key guaranteed to break up any remaining
     equal values */
  return sort(rank+indgen(0:mxrank)*norm);
}

func msort_rank(x, &list)
/* DOCUMENT msort_rank(x)
            msort_rank(x, list)
     returns a list of longs the same size and shape as X, whose
     values are the "rank" of the corresponding element of X among
     all the elements of X -- the smallest element has rank 0 and
     the largest has the largest rank, which is equal to one less
     than the number of distinct values in the array X.

     If LIST is present, it is set to the order list returned by
     sort(x(*)).

   SEE ALSO: msort, sort
 */
{
  rank= array(0, dimsof(x));
  if (numberof(x)<2) return rank;
  void= use_origins(0);
  list= sort(x(*));
  x= x(list);
  x= (x(1:-1)!=x(2:0))(cum);  /* NOT dif -- x may be strings */
  rank(list)= x;
  return rank;
}
