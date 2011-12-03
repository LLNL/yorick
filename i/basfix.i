/*
 * $Id: basfix.i,v 1.3 2010-07-08 15:05:03 dhmunro Exp $
 * Fixes to handle Basis-generated PDB file weirdnesses.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*= SECTION() support for Basis-generated pdb files ========================*/

local basfix;
/* DOCUMENT #include "basfix.i"

     Provides functions obasis, baset, and baget to try to cope with
     Basis PDB file conventions.

     By default, the openb function is overloaded by the obasis
     function.  The original openb is called basfix_openb.  It is
     unclear what side effects this might have, but they are probably
     minor.  If you want to preserve the original openb function, set

          basfix= 1;

     BEFORE you include basfix.i.

   SEE ALSO: obasis, baset, baget
 */

func _obasis(filename, clogfile, at_open, open102=, one=)
{
  at_pdb_open= at_open | 12;
  return basfix_xopenb(filename, clogfile, open102=open102, one=one);
}

func obasis(filename, clogfile, update, open102=, one=)
/* DOCUMENT file= obasis(filename)
         or file= openb(filename)
         or file= openb(filename, clogfile)

     (If you typed help,openb and are reading this, then consider the
      latter two forms.  If you typed help,obasis, consider the first.
      In either case, the original openb function is called basfix_openb.)

     open the existing file FILENAME for read-only binary I/O.
     (Use updateb or createb, respectively, to open an existing file
      with read-write access or to create a new file.)
     If the CLOGFILE argument is supplied, it represents the structure
     of FILENAME in the Clog binary data description language.
     After an openb, the file variable may be used to extract variables
     from the file as if it were a structure instance.  That is, the
     expression "file.var" refers to the variable "var" in file "file".
     A complete list of the variable names present in the file may
     be obtained using the get_vars function.  If the file contains
     history records, the jt and jc functions may be used to set the
     current record -- initially, the first record is current.
     The restore function may be used to make memory copies of data
     in the file; this will be faster than a large number of
     references to "file.var".
     The openb function will recognize families of PDB or netCDF files
     by their sequential names and open all files subsequent to FILENAME
     in such a family as well as FILENAME itself.  You can use the one=1
     keyword to suppress this behavior and open only FILENAME.
     FILENAME may be a file handle to skip the initial open operation.
     This feature is intended to enable in-memory files created with
     vopen to be opened:
       file = openb(vopen(char_array,1));
     FILENAME may also be char_array directly, as returned by vsave.
   SEE ALSO: updateb, createb, open, vopen, cd
             show, jt, jc, restore
             get_vars, get_times, get_ncycs, get_member, has_records
             set_blocksize, dump_clog, read_clog, recover_file
             openb_hooks, open102, close102, get_addrs,
             baset, baget
 */
{
  if (update)
    return basfix_xopenb(filename,clogfile,update, open102=open102,one=one);
  else
    return _obasis(filename, clogfile, at_pdb_open, open102=open102, one=one);
}

if (is_void(basfix_openb)) basfix_openb= openb;
if (!basfix) openb= obasis;

func baset(file, varname, value)
/* DOCUMENT baset, file, varname, value
     set the (first) variable named VARNAME in FILE to VALUE.

     The obasis function opens files read-only.  If you want to update
     a PFB Basis-generated PDB file without altering its "@decorated"
     variable names, open the file with updateb, then use baset to
     modify variables.  Since you can only change the entire variable
     with baset, you may want to read it first with baget.

   SEE ALSO: obasis, baget
 */
{
  vars= *get_vars(file)(1);
  list= where(strtok(vars,"@")(1,)==varname);
  if (!numberof(list)) error, "no such variable as "+varname;
  if (numberof(list)>1) {
    write, "WARNING- the first of these variables used:";
    write, vars(list);
  }
  get_member(file,vars(list(1)))= value;
}

func baget(file, varname)
/* DOCUMENT baget(file, varname)
     read and return the (first) variable named VARNAME in FILE.

     The obasis function opens files read-only.  If you want to update
     a PFB Basis-generated PDB file without altering its "@decorated"
     variable names, open the file with updateb, then use baset to
     modify variables.  Since you can only change the entire variable
     with baset, you may want to read it first with baget.

   SEE ALSO: obasis, baset
 */
{
  vars= *get_vars(file)(1);
  list= where(strtok(vars,"@")(1,)==varname);
  if (!numberof(list)) error, "no such variable as "+varname;
  if (numberof(list)>1) {
    write, "WARNING- the first of these variables used:";
    write, vars(list);
  }
  return get_member(file,vars(list(1)));
}

func basfix_xopenb(filename,clogfile,update, open102=, one=)
{
  f = basfix_openb(filename,clogfile,update, open102=open102, one=one);
  vars = get_vars(f);
  if (vars(1) && allof((*vars(1))!="/")) return f;

  rvars = *vars(2);
  vars = grow(*vars(1), rvars);

  if (numberof(vars))
    list = where((strpart(vars,0:0)!="/") & (strpart(vars,1:1)=="/"));
  if (!numberof(list)) return f;

  vars(list) = strpart(vars(list), 2:0);
  nr = numberof(rvars);
  if (nr) {
    rvars = vars(1-nr:0);
    vars = vars(1:-nr);
  }
  set_vars, f, vars, rvars;

  if (nr && anyof(rvars=="time") && dimsof(f.time)(1)==0)
    times = collect(f,"time");
  if (nr && anyof(rvars=="ncyc") && dimsof(f.ncyc)(1)==0)
    ncycs = collect(f,"ncyc");
  if (!is_void(times) || !is_void(ncycs)) {
    edit_times, f, , times, ncycs;
    edit_times, f;
  }

  return f;
}
