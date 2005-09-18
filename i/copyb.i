/*
 * $Id: copyb.i,v 1.1 2005-09-18 22:05:54 dhmunro Exp $
 * Utility to copy binary files.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func copyb(src, dst, size=)
/* DOCUMENT copyb, src, dst
         or copyb, openb(src_name), createb(dst_name)
     Copy binary file SRC to binary file DST.
     Check for "obsolete/" subdirectory of Yorick home directory for
     extensions of the openb function to old file formats.
     Use the size= keyword to specify a non-default (4 Mbyte) size for
     the members of the output file family.
     If you habitually include basfix.i, you may want to use the
     basfix_openb function to open the src file.
   SEE ALSO: openb, createb, open102, close102
 */
{
  vars= get_vars(src);
  /* sort the variables into order of increasing address */
  addrs= get_addrs(src);

  /* declare and copy non-record variables */
  names= *vars(1);
  n= numberof(names);
  if (n) names= names(sort(*addrs(1)));
  for (i=1 ; i<=n ; i++) {
    name= names(i);
    value= get_member(src, name);
    add_variable, dst, -1, name, structof(value), dimsof(value);
    get_member(dst, name)= value;
    value= [];
  }

  names= *vars(2);
  n= numberof(names);
  if (n<1) return;
  names= names(sort(*addrs(2)));

  /* declare record variables */
  add_record, dst;
  if (size) set_filesize, dst, size;
  for (i=1 ; i<=n ; i++) {
    name= names(i);
    add_variable, dst, -1, name, structof(get_member(src, name)), \
      dimsof(get_member(src, name));
  }

  /* copy record variables for each record */
  times= get_times(src);
  ncycs= get_ncycs(src);
  time= ncyc= [];
  r= r0= 1;
  while (jt(src,-)) ++r0;
  while (jt(src)) {
    if (!is_void(times)) time= times(r);
    if (!is_void(ncycs)) ncyc= ncycs(r);
    add_record, dst, time, ncyc, -1;
    for (i=1 ; i<=n ; i++) {
      name= names(i);
      get_member(dst, name)= get_member(src, name);
    }
    r++;
  }
  if (r>1) jr, src, r0;
}
