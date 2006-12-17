/*
 * $Id: silo.i,v 1.2 2006-12-17 18:22:03 dhmunro Exp $
 * support for Silo files, a higher level binary format than PDB
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

if (is_void(silo_openb)) silo_openb=is_void(basfix_openb)?openb:basfix_openb;

func silo_open(filename)
/* DOCUMENT silo_open, filename
         or silo= silo_open(filename)
     open FILENAME for later use with silo_... functions

     There is a single current silo file, which is opened and set
     by the first form.  The other silo_... functions normally
     operate on this current file, but also accept a silo= keyword,
     which is a list returned by the second calling sequence for
     silo_open.

   SEE ALSO: silo_cd, silo_ls, silo_var, silo_close
 */
{
  f= silo_openb(filename);
  vars= *get_vars(f)(1);
  nvars= numberof(vars);
  mask= strpart(vars,0:0)=="/";
  list= where(mask);
  ndirs= numberof(list);
  grow, vars, vars(list)+"\377\377\377";  /* mark end of each dir */
  grow, mask, array(-1, ndirs);
  order= sort(vars);
  vars= vars(order);
  mask= mask(order);
  invrs= order;
  invrs(order)= indgen(nvars+ndirs);
  list= invrs(list);
  ends= invrs(nvars+1:nvars+ndirs);
  lens= array(0,nvars+ndirs);
  order= (mask<0)(psum);
  lens(list)= ends - list - order(ends) + order(list);
  list= where(mask>0);
  lens= lens(list);
  depth= mask(psum);
  depth(list)-= 1;
  list= where(mask>=0);
  vars= vars(list);
  depth= depth(list);
  list= where(mask(list));
  list= _lst(f, vars, depth, list, lens, "/");
  if (am_subroutine()) {
    extern silo_list;
    silo_list= list;
  } else {
    return list;
  }
}

func silo_close(void, silo=)
/* DOCUMENT silo_close
     close current silo file
   SEE ALSO: silo_cd, silo_ls, silo_var, silo_open
 */
{
  if (is_void(silo)) {
    close, _car(silo_list, 1, []);
    silo_list= [];
  } else {
    close, _car(silo, 1, []);
  }
}

func silo_cd(dir, silo=)
/* DOCUMENT silo_cd, dirname
     change current silo directory to DIRNAME, which may contain
       .. or . constructs as a UNIX pathname
     accepts silo= keyword to operate on a silo file other than the
       current silo file
   SEE ALSO: silo_ls, silo_var, silo_open, silo_close
 */
{
  list= is_void(silo)? silo_list : silo;
  if (strpart(dir,0:0)=="/") dir= strpart(dir,1:-1);
  if (strpart(dir,1:1)!="/") dir= _car(list,6) + dir;
  dir= silo_simplify(dir) + "/";
  if (dir=="//") dir= "/";
  lst= where(dir==_car(list, 2)(_car(list, 4)));
  if (!numberof(lst)) {
    if (am_subroutine()) error, "no such directory as: "+dir;
    return string(0);
  }
  _car, list, 6, dir;
  return dir;
}

func silo_var(var, silo=)
/* DOCUMENT var= silo_var(varname)
     return silo variable VARNAME
     accepts silo= keyword to operate on a silo file other than the
       current silo file
   SEE ALSO: silo_ls, silo_cd, silo_open, silo_close
 */
{
  list= is_void(silo)? silo_list : silo;
  if (strpart(var,1:1)!="/") var= _car(list,6) + var;
  return get_member(_car(list,1),silo_simplify(var));
}

func silo_ls(name, &dirname, silo=)
/* DOCUMENT silo_ls
         or silo_ls, dirname
         or itemlist= silo_ls(dirname)
         or itemlist= silo_ls(dirname, fulldirname)
     list current silo directory or DIRNAME
     if called as a function, returns a 1D array of strings beginning
       with ".", and optionally returns FULLDIRNAME, which is the
       full path name of the directory listed
       - the individual items in the list do not include the
         directory path
       - subdirectory names end with "/", so you can find them
         using strpart(itemlist,0:0)=="/"
     accepts silo= keyword to operate on a silo file other than the
       current silo file
   SEE ALSO: silo_ls, silo_cd, silo_open, silo_close
 */
{
  list= is_void(silo)? silo_list : silo;
  if (is_void(name)) name= ".";
  if (strpart(name,0:0)=="/") name= strpart(name,1:-1);
  if (strpart(name,1:1)!="/") name= _car(list,6) + name;
  name= silo_simplify(name) + "/";
  if (name=="//") name= "/";
  local vars, lens;
  eq_nocopy, vars, _car(list, 2);
  eq_nocopy, depth, _car(list, 3);
  eq_nocopy, lens, _car(list, 5);
  eq_nocopy, list, _car(list, 4);
  lst= where(name==vars(list));
  if (!numberof(lst)) {
    if (am_subroutine()) write, "no such directory as: "+name;
    return [];
  }
  lst= lst(1);
  lens= lens(lst);
  list= list(lst);
  vars= vars(list:list+lens);
  depth= depth(list:list+lens);
  dirname= vars(1);  /* full directory name */
  vars(1)= ".";
  if (numberof(vars)>1) {
    depth(1)+= 1;
    vars= vars(where(depth==depth(1)));
    lens= strlen(dirname);
    if (numberof(vars)>1) vars(2:0)= strpart(vars(2:0),lens+1:0);
  }
  if (am_subroutine()) write, format=" %s\n", vars;
  return vars;
}

func silo_simplify(dir)
{
  if (strmatch(dir,"/.")) {
    parts= array(string, 1000);
    parse= strtok(dir, "/");
    for (tok=parse(1),i=0 ; tok ; tok=parse(1)) {
      if (tok!=".") {
        if (tok=="..") i= max(0, i-1);
        else parts(++i)= tok;
      }
      parse= strtok(parse(2), "/");
    }
    dir= i? "" : "/";
    for (j=1 ; j<=i ; j++) dir+= "/"+parts(j);
  }
  return dir;
}
