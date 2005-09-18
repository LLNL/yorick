/*
 * $Id: readn.i,v 1.1 2005-09-18 22:06:06 dhmunro Exp $
 * Provides a handy way to read numbers from a file, skipping
 * non-numeric tokens.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func read_n(f, &n0, &n1, &n2, &n3, &n4, &n5, &n6, &n7, &n8, &n9)
/* DOCUMENT raw_read_n, f, n0, n1, n2, ...
     grabs the next numbers N0, N1, N2, ... from file F, skipping over
     any whitespace, comma, semicolon, or colon delimited tokens which
     are not numbers.  (Actually, only the first and last characters of
     the token have to look like a number -- 4xxx3 would be read as 4.)
     ***WARNING*** at most ten Ns are allowed
     The Ns can be arrays, provided all have the same dimensions.
   SEE ALSO: read, rdline
 */
{
  n= numberof(n0);
  line= array(string);
  for (i=1 ; i<=n ; i++) {
    read_n_worker, line, f, n0, i;   if (is_void(n1)) continue;
    read_n_worker, line, f, n1, i;   if (is_void(n2)) continue;
    read_n_worker, line, f, n2, i;   if (is_void(n3)) continue;
    read_n_worker, line, f, n3, i;   if (is_void(n4)) continue;
    read_n_worker, line, f, n4, i;   if (is_void(n5)) continue;
    read_n_worker, line, f, n5, i;   if (is_void(n6)) continue;
    read_n_worker, line, f, n6, i;   if (is_void(n7)) continue;
    read_n_worker, line, f, n7, i;   if (is_void(n8)) continue;
    read_n_worker, line, f, n8, i;   if (is_void(n9)) continue;
    read_n_worker, line, f, n9, i;
  }
}

func read_n_worker(&line, f, &var, i)
{
  /* indirect flag necessary because can't store back into a
     scalar using var(i)=... (sigh) */
  if (indirect=dimsof(var)(1)) value= structof(var)();
  for (;;) {
    while (line) {
      tok= strtok(line, ",;: \t");
      line= tok(2);
      len= strlen(tok(1));
      if (len && strmatch("0123456789.",strpart(tok(1), len:len)) &&
          (indirect? sread(tok(1), value) : sread(tok(1), var))) {
        if (indirect) var(i)= value;
        return;
      }
    }
    line= rdline(f);
    if (!line) error, "premature end of file in read_n";
  }
}
