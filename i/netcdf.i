/*
 * $Id: netcdf.i,v 1.3 2010-04-18 10:33:38 thiebaut Exp $
 * Yorick procedures to open a netCDF file
 *
 * The definitive reference for netCDF files is:
 * Anonymous FTP site: unidata.ucar.edu [128.117.140.3]
 * File:               pub/netcdf/netcdf.tar.Z
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* Note: I don't use netCDF files, so there are probably still some
         bugs in this code (DHM).  */

local netcdf;
/* DOCUMENT nc_open, nc_create, nc_vardef, nc_enddef, nc_addrec
     are the main routines to read and write netCDF files.

     The ordinary openb function will also open netCDF files.

     Writing a netCDF file is more problematic in Yorick, since
     you must define the entire file structure before you write
     any data.  Therefore, the nc_create call returns only a
     "token" for nc_vardef, which you use to declare variables
     in the file.  When you are done declaring variables, you
     call nc_enddef, which returns an ordinary Yorick file object.
     You can then write data to the file (with f.var=value or
     save,f,var).  To add a record, you must use nc_addrec instead
     of add_record (nc_addrec updates the record count in the file).
 */

/* ------------- netCDF interface --------------------------------- */

func nc_open(_nc_open_filename, mode)
/* DOCUMENT f= nc_open(filename, mode)
     opens a netCDF file FILENAME for reading or update as specified
     by MODE, which defaults to "rb".  Attributes and dimension names
     can be found in the three external variables nc_dims (an array of
     type NC_dim), nc_attrs (an array of type NC_attr), and nc_vars
     (an array of type NC_var) after this call.
     MODE should be either "rb" or "r+b"; nothing else makes sense.
     If FILENAME is an array of strings, exactly those files will be
     opened as a family (if possible).  Note that nc_open("myfile00")
     potentially opens myfile01, myfile02, and so on, as for openb,
     but that nc_open(["myfile00"]) opens myfile00 only.
   SEE ALSO: nc_create, nc_enddef, nc_attribute, nc_dimsof
 */
{
  if (is_void(mode)) mode= "rb";
  f= open(_nc_open_filename(1), mode);
  if (raw_not_cdf(f)) return [];
  return f;
}

func raw_not_cdf(f)
{
  i= array(char, 4);
  _read, f, 0, i;
  if (string(&i(1:3))!="CDF" || i(4)<1 || i(4)>2)
    return 1;  /* test magic number */

  _nc_version = i(4);
  xdr_primitives, f;

  numrecs= long(0);
  _read, f, 4, numrecs;

  /* Each of the major components of a netCDF file contains information
     not used by Yorick.  Specifically, dimensions are named, the file
     may have attributes, and any variable contained in the file may
     have attributes.  After a call to nc_open, this additional
     information is stored in the external variable nc_file.  */
  extern nc_file;

  pf= print(f);
  name= dir= "";
  sread, pf(where(strmatch(pf,"binary stream:"))), dir, name,
    format= "%s binary stream: %s";
  sread, pf(where(strmatch(pf,"In directory:"))), dir,
    format= " In directory: %s";

  address= 8;
  nc_dims= NC_ReadArray(f, address);
  nc_attrs= NC_ReadArray(f, address);
  nc_vars= NC_ReadArray(f, address);
  nc_file= NC_file(numrecs=numrecs, dims=&nc_dims, attrs=&nc_attrs,
                   vars=&nc_vars, filename=dir+name);

  if (_nc_declare(f, nc_file)) {
    /* check for presence of a file family */
    dims= dimsof(_nc_open_filename);
    ifile= (!is_void(dims) && dims(1));
    while (!_nc_add_next_file(f, ifile)) {
      i= array(char, 4);
      _read, f, 0, i;
      if (string(&i)!="CDF\001") break;  /* test magic number */
      _read, f, 4, numrecs;
      if (!numrecs) continue;
      nc_file.numrecs= numrecs;
      address= 8;
      dims= NC_ReadArray(f, address);
      attrs= NC_ReadArray(f, address);
      vars= NC_ReadArray(f, address);
      if (numberof(dims)!=numberof(nc_dims) ||
          anyof(dims.size!=nc_dims.size) ||
          !_nc_declare(f, nc_file, vars))
        error, "File '"+_nc_open_filename(ifile)+"' has different structure";
    }
  }

  jr, f, 1;
  return 0;
}

func _nc_add_next_file(f, &ifile)
{
  /* modified add_next file checks whether nc_open has been called
   * with an explicit list of file names
   * -- eventually this should be installed in openb generally
   */
  if (!ifile) return add_next_file(f);
  ifile+= 1;
  if (ifile>numberof(_nc_open_filename)) return 1;
  if (add_next_file(f, _nc_open_filename(ifile), 0))
    error, "File '"+_nc_open_filename(ifile)+"' not found";
  return 0;
}

func _nc_declare(f, ncf, check_vars)
{
  nc_dims= *ncf.dims;
  nc_vars= *ncf.vars;
  numrecs= ncf.numrecs;

  if (!is_void(check_vars)) {
    if (numberof(nc_vars)!=numberof(check_vars) ||
        anyof(nc_vars.name!=check_vars.name) ||
        anyof(nc_vars.type!=check_vars.type) ||
        anyof(nc_vars.len!=check_vars.len)) return 0;
    /* anyof(nc_vars.address!=check_vars.address) see below */
  }

  /* check whether there is an unlimited (history) dimension */
  if (is_void(nc_dims)) unlimited= -1;
  else {
    unlimited= (nc_dims.size==0);
    if (noneof(unlimited)) unlimited= -1;
    else unlimited= where(unlimited)(0);
  }

  /* first pass sets non-history variables */
  type_names= ["char", "char", "short", "long", "float", "double"];
  nvars= numberof(nc_vars);
  if (nvars) nonRecord= array(int, nvars);
  else nonRecord= 1;
  for (i=1 ; i<=nvars ; ++i) {
    var= nc_vars(i);
    dimlist= *var.dimlist;
    if (numberof(dimlist)) {
      dimlist+= 1;
      if (dimlist(1)==unlimited) continue;  /* record variable */
      dimlist= grow([numberof(dimlist)], nc_dims.size(dimlist(0:1:-1)));
    } else {
      dimlist= [0];
    }
    nonRecord(i)= 1;
    if (is_void(check_vars))
      add_variable, f, var.address, var.name, type_names(var.type), dimlist;
  }

  /* second pass sets history variables */
  if (nallof(nonRecord)) {
    if (is_void(check_vars))
      add_record, f;
    recList= where(!nonRecord);
    ha= min(nc_vars.address(recList));
    if (!is_void(check_vars)) {
      ha0= ha;
      ha= min(check_vars.address(recList));
      /* note that if ha0!=ha non-record variable addresses will differ,
       * which assumes they will always be read from first file of family */
      if (anyof(nc_vars.address-ha0!=check_vars.address-ha)) return 0;
    }
    time_address= 0;
    time_type= 0;
    for (i=1 ; i<=nvars ; ++i) {
      if (nonRecord(i)) continue;
      var= nc_vars(i);
      dimlist= *var.dimlist;
      ++dimlist;
      if (numberof(dimlist) > 1) {
        dimlist= grow([numberof(dimlist)-1], nc_dims.size(dimlist(0:2:-1)));
      } else {
        dimlist= [0];
        if (var.name=="time" && (var.type==5 || var.type==6)) {
          time_address=
            is_void(check_vars)? var.address : check_vars(i).address;
          time_type= var.type;
        }
      }
      if (is_void(check_vars))
        add_variable, f, var.address-ha,
                         var.name, type_names(var.type), dimlist;
    }

    /* Note: If there is exactly one record variable, then the records
       need not begin on a 4-byte boundary.  This is only an issue for
       a single record variable of type char or short.  */

    if (numberof(recList)>1 || var.type>3) {
      hs= sum(nc_vars.len(recList));
    } else {
      hs= (var.type==3? 2 : 1);
      for (i=2 ; i<=1+dimlist(1) ; ++i) hs*= dimlist(i);
    }
    if (numrecs) {
      if (time_address) {
        if (time_type==6) time= 0.0;
        else time= 0.0f;
        times= array(0.0, numrecs);
        for (i=1 ; i<=numrecs ; ++i) {
          _read, f, time_address, time;
          time_address+= hs;
          times(i)= time;
        }
        add_record, f, times, , ha+hs*indgen(0:numrecs-1);
      } else {
        add_record, f, , , ha+hs*indgen(0:numrecs-1);
      }
    }

    return 1;
  }

  return 0;
}

func nc_create(filename)
/* DOCUMENT ncf= nc_create(filename)
     creates a netCDF file FILENAME.
     After this call, use nc_vardef to declare the netCDF variables.
     Then use nc_enddef to write the netCDF self-descriptive
     information.  Only after this are you free to actually write data.

   SEE ALSO: nc_open, nc_vardef, nc_attrdef, nc_enddef, nc_addrec,
             nc_attribute, nc_dimsof
 */
{
  return NC_file(filename=filename);
}

func nc_vardef(ncf, name, type, dims, template=, record=, dimnames=)
/* DOCUMENT nc_vardef, ncf, name, type, dims, record=0/1
         or nc_vardef, ncf, name, type, record=0/1
         or nc_vardef, ncf, name, template=template, record=0/1
     define a variable in the NCF (returned by nc_create) with name
     NAME, type TYPE (as returned by typeof or structof), and dimensions
     DIMS (as returned by dimsof).  The template= keyword may be used
     instead of type and dims; the type and dims will be those of the
     TEMPLATE.  If dims is not specified, a scalar is assumed.  If the
     record= keyword is present and non-zero, the variable is a record
     variable; otherwise it is a non-record variable.

     You can use the dimnames= keyword to write specific dimension
     names into the netCDF file.  These are not useful to Yorick, but
     other codes may require them.  If two variables share a dimension
     name, the corresponding dimension must have the same length.  For
     example:
       nc_vardef, ncf, "theta", double, [1,nlat], dimnames=["latitude"]
       nc_vardef, ncf, "phi", double, [1,nlong], dimnames=["longitude"]
       nc_vardef, ncf, "elevation", double,
                  dimnames=["latitude","longitude"]
     A dimension name of "" lets Yorick invent a fake dimension name,
     as it does by default.  If dimnames= is present and the lengths
     of the dimensions have previously been defined, then the DIMS
     parameter is unnecessary, as in the "elevation" array in the example.
     You can use the nc_dimdef function to define a named dimension size
     before you define any variables with that dimension.

   SEE ALSO: nc_create, nc_attrdef, nc_enddef, nc_addrec, nc_dimdef
 */
{
  nc_vars= *ncf.vars;
  if (!is_void(nc_vars) &&
      anyof(nc_vars.name==name)) error, "duplicate netCDF name: "+name;

  if (!is_void(template)) {
    type= typeof(template);
    dims= dimsof(template);
  } else {
    if (!is_array(type)) {
      if (type==char) type= "char";
      else if (type==short) type= "short";
      else if (type==long || type==int) type= "long";
      else if (type==float) type= "float";
      else if (type==double) type= "double";
      else type= "illegal";
    }
    if (is_void(dims) && is_void(dimnames)) dims= [0];
  }
  if (type=="int") type= "long";
  list= where(type==["char","char","short","long","float","double"]);
  if (!numberof(list)) error, "illegal netCDF data type: "+type;
  type= list(1);
  len= [1, 1, 2, 4, 4, 8](type);

  if (record && numberof(dims)) {
    if (numberof(dimnames) && numberof(dimnames)==dims(1))
      grow, dimnames, "";
    grow, dims, [0];
    dims(1)+= 1;
  }

  if (numberof(dimnames) && numberof(dims) &&
      numberof(dimnames)!=dims(1))
    error, "dimnames= keyword does not match variable: "+name;

  if (!numberof(dims) && numberof(dimnames)) {
    /* use existing named dimensions */
    eq_nocopy, nc_dims, *ncf.dims;
    if (is_void(nc_dims)) error, "no dimensions yet defined";
    ncnames= nc_dims.name;
    dims= array(0, numberof(dimnames));
    for (i=1 ; i<=numberof(dims) ; ++i) {
      list= where(ncnames==dimnames(i));
      if (!numberof(list)) error, "dimension not yet defined: "+dimnames(i);
      dims(numberof(dims)-i+1)= list(1)-1;
      size= nc_dims(list(1)).size;
      if (size) len*= size;
    }

  } else if (numberof(dims)>1) {
    eq_nocopy, nc_dims, *ncf.dims;
    ndims= dims(1);
    for (i=2 ; i<=1+ndims ; ++i) {
      size= dims(i);
      if (size) len*= size;
      if (numberof(dimnames) && strlen((dimname= dimnames(i-1)))) {
        if (changed) ncf.dims= &nc_dims;
        dims(i)= nc_dimdef(ncf, dimname, size);
        eq_nocopy, nc_dims, *ncf.dims;
        continue;
      } else if (numberof(nc_dims)) {
        list= where((size==nc_dims.size) &
                    (strpart(nc_dims.name,1:2)=="D_"));
        if (numberof(list)) {
          dims(i)= list(1);
          continue;
        }
        dimname= "D_"+pr1(size);
      } else {
        dimname= "D_"+pr1(size);
      }
      grow, nc_dims, [NC_dim(name=dimname,size=size)];
      dims(i)= numberof(nc_dims);
      ncf.dims= &nc_dims;
    }
    dims= dims(0:2:-1)-1;

  } else {
    dims= [];
  }

  grow, nc_vars, [NC_var(name= name, dimlist= &dims, type= type, len= len)];
  ncf.vars= &nc_vars;
}

func nc_dimdef(ncf, dimname, size)
/* DOCUMENT nc_dimdef, ncf, dim_name, size
         or nc_dimdef, ncf, dim_name, "unlimited"
     define a named dimension.  The SIZE parameter is the length of
     the dimension, or the string "unlimited" for the unlimited
     dimension.  (The numerical value 0 is the same as "unlimited".)
     You can also define named dimensions implicitly using nc_vardef.
   SEE ALSO: nc_vardef
 */
{
  if (size=="unlimited") size= 0;
  nc_dims= *ncf.dims;
  list= numberof(nc_dims)? where(nc_dims.name==dimname) : [];
  if (numberof(list)) {
    list= list(1);
    if (nc_dims(list).size!=size)
      error, "different sizes sepcified for dimension "+dimname;
    return list;
  } else if (size || !numberof(nc_dims) || allof(nc_dims.size)) {
      grow, nc_dims, [NC_dim(name=dimname,size=size)];
      ncf.dims= &nc_dims;
      return numberof(nc_dims);
  } else {
    error, "only one unlimited dimension is allowed";
  }
}

func nc_attrdef(ncf, attr_name, var_name, value)
/* DOCUMENT nc_attrdef, ncf, attr_name, var_name, value
     sets the value of the netCDF attribute ATTR_NAME associated
     with variable VAR_NAME to VALUE (note that the data type of VALUE
     becomes the data type of the attribute).
     The NCF is the structure returned by nc_create; nc_attrdef
     must be called prior to nc_enddef, which actually writes the
     attribute data to the file.
     If VAR_NAME is omitted, ATTR_NAME refers to the whole file.

   SEE ALSO: nc_open, nc_dimsof, nc_create, nc_enddef, nc_attribute
 */
{
  attr= [NC_attr(name=attr_name, data=&value(*))];
  if (is_void(var_name)) {
    ncf.attrs= &grow(*ncf.attrs, attr);
  } else {
    local vars;
    eq_nocopy, vars, *ncf.vars;
    if (is_void(vars) ||
        !numberof((
                   attrs= where(vars.name==var_name))))
      error, "file has no such variable: "+var_name;
    attrs= attrs(0);
    vars(attrs).attrs= &grow(*vars(attrs).attrs, attr);
    /* *ncf.vars already updated, since eq_nocopy used above */
  }
}

func nc_enddef(ncf)
/* DOCUMENT f= nc_enddef(ncf)
     creates netCDF file NCF (returned by nc_create), and writes the self-
     descriptive information.  Returns the ordinary Yorick file object
     corresponding to the new file.  You are then free to write variables,
     or use the save or nc_addrec functions.

   SEE ALSO: nc_create, nc_addrec, nc_open, nc_attrdef, nc_dimsof
 */
{
  /* first, need to fill in addresses */
  vars= *ncf.vars;
  dims= *ncf.dims;
  rec= array(0, numberof(vars));
  if (!is_void(dims)) {
    list= where(dims.size==0);
    if (numberof(list)) {
      /* record variables need to be sorted separately */
      list= list(1)-1;
      for (i=1 ; i<=numberof(vars) ; ++i) {
        dimlist= *vars(i).dimlist;
        if (!is_void(dimlist) && dimlist(1)==list) rec(i)= 1;
      }
    }
  }
  lens= vars.len;
  lens= 4*((lens+3)/4);
  list= where(!rec);
  if (numberof(list)) {
    addrs= lens(list)(cum);
    lens(list)= addrs(1:-1);
    rec_addr= addrs(0);
  } else {
    rec_addr= 0;
  }
  list= where(rec);
  if (numberof(list)) lens(list)= rec_addr + lens(list)(cum)(1:-1);
  rec_addr= _nc_desc_len(ncf);  /* actually address of 1st datum */
  vars.address= rec_addr + lens;
  ncf.vars= &vars;

  f= open(ncf.filename, "w+b");
  xdr_primitives, f;

  if (!numberof(where(!rec))) {
    /* work around laziness of not computing address in nc_addrec */
    add_variable, f, rec_addr-1, "?ignore_me?", char;
  }

  _nc_declare, f, ncf;

  _write, f, 0, (*pointer("CDF\001"))(1:4);
  _write, f, 4, 0;

  address= 8;
  NC_WriteArray, f, address, dims;
  NC_WriteArray, f, address, *ncf.attrs;
  NC_WriteArray, f, address, vars;

  return f;
}

func _nc_desc_len(ncf)
{
  _write= _dummy_write;  /* overlay _write with noop */
  address= 8;
  NC_WriteArray, f, address, *ncf.dims;
  NC_WriteArray, f, address, *ncf.attrs;
  NC_WriteArray, f, address, *ncf.vars;
  return address;
}

func _dummy_write(f, a, v)
{
  return sizeof(v);
}

func nc_addrec(f, time)
/* DOCUMENT nc_addrec, f, time
         or nc_addrec, f
     adds a new record to the netCDF file F at time TIME.

   SEE ALSO: nc_create, nc_vardef, nc_enddef
 */
{
  add_record, f, time, , -1;
  files= *get_addrs(f)(4);
  _write,f,4, sum(files==files(0));
}

func nc_attribute(attr_name, var_name)
/* DOCUMENT value= nc_attribute(attr_name, var_name)
     gets the value of the netCDF attribute ATTR_NAME associated
     with variable VAR_NAME, or nil if none.  Uses the external
     variable nc_file set by nc_open.
     If VAR_NAME is omitted, ATTR_NAME refers to the whole file,
     and is retrieved (if present) from the nc_file.attrs variable.

   SEE ALSO: nc_open, nc_attrdef, nc_dimsof, nc_create, nc_enddef
 */
{
  if (is_void(var_name)) attrs= *nc_file.attrs;
  else {
    attrs= where(nc_file.vars->name==var_name);
    if (!numberof(attrs)) return [];
    attrs= *(nc_file.vars->attrs(attrs(0)));
  }
  if (is_void(attrs)) return [];
  value= where(attrs.name==attr_name);
  if (!numberof(value)) return [];
  data= *attrs(value(0)).data;
  if (numberof(data)==1) return data(1);
  else return data;
}

func nc_dimsof(var_name)
/* DOCUMENT def_string= nc_dimsof(var_name)
     returns the dimension list of a netCDF variable VAR_NAME in symbolic
     form, i.e.- using the netCDF dimension names.  This requires the
     nc_file external variable set by nc_open.

   SEE ALSO: nc_open, nc_dimsof, nc_create, nc_enddef
 */
{
  dims = (nc_file.vars->name==var_name);
  if (noneof(dims)) return [];
  dims = *(*nc_file.vars)(where(dims)(1)).dimlist;
  result = "(";
  n = numberof(dims);
  for (i=n ; i>0 ; --i) {
    result +=  nc_file.dims->name(dims(i)+1);
    if (i>1) result += ",";
  }
  return result+")";
}

/* ------------- data structures --------------------------------- */

/* nc_dims is an array of NC_dim */
struct NC_dim {
  string name;  /* represented as NC_string in file */
  long size;  /* length of dimension */
}

/* nc_attrs is an array of NC_attr */
struct NC_attr {
  string name;    /* represented as NC_string in file */
  pointer data;   /* represented as NC_array in file */
}

/* nc_vars is an array of NC_var */
struct NC_var {
  string name;         /* represented as NC_string in file */
  pointer dimlist;     /* represented as NC_iarray in file, dims index */
  pointer attrs;       /* represented as NC_array in file */
  int type;   /* netCDF data type number */
  long len;   /* bytes */
  long address;
}

struct NC_file {
  string filename;     /* for later creation */
  long numrecs;
  pointer dims, attrs, vars;
}

/* ------------- object read routines --------------------------------- */

func NC_ReadDim(f, &address)
{
  name= NC_ReadString(f, address);
  size= long(0);
  _read, f, address, size;
  address+= 4;
  return NC_dim(name=name, size=size);
}

func NC_ReadAttr(f, &address)
{
  name= NC_ReadString(f, address);
  data= &(NC_ReadArray(f, address));
  return NC_attr(name= name, data= data);
}

func NC_ReadString(f, &address)
{
  count= long(0);
  _read, f, address, count;
  address+= 4;
  if (count<=0) return string();
  result= array(char, count);
  _read, f, address, result;
  address+= 4*((count+3)/4);
  return string(&result);
}

func NC_ReadInts(f, &address)
{
  count= long(0);
  _read, f, address, count;
  address+= 4;
  if (count<=0) return &[];
  result= array(int, count);
  _read, f, address, result;
  address+= 4*count;
  return &result;
}

func NC_ReadVar(f, &address)
{
  name= NC_ReadString(f, address);
  dimlist= NC_ReadInts(f, address);
  attrs= &(NC_ReadArray(f, address));   /* must be type NC_attr */
  type= int(0);
  _read, f, address, type;
  address+= 4;
  len= addr= long(0);
  _read, f, address, len;
  address+= 4;
  _read, f, address, addr;
  address+= 4;
  if (_nc_version == 2) {
    addrlo = long(0);
    _read, f, address, addrlo;
    address+= 4;
    addr = (addr<<32) | addrlo;
  }
  return NC_var(name= name, dimlist= dimlist, attrs= attrs, type= type,
                len= len, address= addr);
}

func NC_ReadArray(f, &address)
{
  type= int(0);
  _read, f, address, type;
  address+= 4;
  count= long(0);
  _read, f, address, count;
  address+= 4;

  if (type<=0 || type>12 || count<=0)
    return (type==2 && count==0)? "" : [];
  else if (type<=2)     /* byte (1) or char (2) */
    result= NC_ReadPrim(f, address, char, 1, count);
  else if (type==3)     /* short */
    result= NC_ReadPrim(f, address, short, 2, count);
  else if (type==4)     /* long */
    result= NC_ReadPrim(f, address, long, 4, count);
  else if (type==5)     /* float */
    result= NC_ReadPrim(f, address, float, 4, count);
  else if (type==6)     /* double */
    result= NC_ReadPrim(f, address, double, 8, count);
  else if (type==7)     /* bitfield */
    return [];
  else if (type==8)     /* string */
    result= NC_ReadMulti(f, address, string, count, NC_ReadString);
  else if (type==9)     /* iarray */
    result= NC_ReadMulti(f, address, pointer, count, NC_ReadInts);
  else if (type==10)    /* dimension */
    result= NC_ReadMulti(f, address, NC_dim, count, NC_ReadDim);
  else if (type==11)    /* variable */
    result= NC_ReadMulti(f, address, NC_var, count, NC_ReadVar);
  else if (type==12)    /* attribute */
    result= NC_ReadMulti(f, address, NC_attr, count, NC_ReadAttr);

  /* presume that type char means a string */
  if (type==2) result= string(&result);

  return result;
}

func NC_ReadPrim(f, &address, type, size, count)
{
  result= array(type, count);
  _read, f, address, result;
  address+= 4*((size*count+3)/4);
  return result;
}

func NC_ReadMulti(f, &address, type, count, Reader)
{
  result= array(type, count);
  for (i=1 ; i<=count ; ++i) result(i)= Reader(f, address);
  return result;
}

/* ------------- object write routines --------------------------------- */

func NC_WriteDim(f, &address, dim)
{
  NC_WriteString, f, address, dim.name;
  size= long(0);
  _write, f, address, dim.size;
  address+= 4;
}

func NC_WriteAttr(f, &address, attr)
{
  NC_WriteString, f, address, attr.name;
  NC_WriteArray, f, address, *attr.data;
}

func NC_WriteString(f, &address, text)
{
  _write, f, address, strlen(text);
  address+= 4;
  if (strlen(text)<=0) return;
  _write, f, address, (*pointer(text))(1:-1);
  address+= 4*((strlen(text)+3)/4);
}

func NC_WriteInts(f, &address, vals)
{
  vals= *vals;
  _write, f, address, numberof(vals);
  address+= 4;
  if (numberof(vals)<=0) return;
  _write, f, address, long(vals);
  address+= 4*numberof(vals);
}

func NC_WriteVar(f, &address, var)
{
  NC_WriteString, f, address, var.name;
  NC_WriteInts, f, address, var.dimlist;
  NC_WriteArray, f, address, *var.attrs;   /* must be type NC_attr */
  _write, f, address, var.type;
  address+= 4;
  _write, f, address, var.len;
  address+= 4;
  _write, f, address, var.address;
  address+= 4;
}

func NC_WriteArray(f, &address, ary)
{
  type= nameof(structof(ary));
  if (type=="int") {
    ary= long(ary);
    type= "long";
  }
  type= where(type==["char","char","short","long","float","double", "",
                     "NC_string", "NC_iarray", "NC_dim", "NC_var", "NC_attr"]);
  if (!numberof(type)) {
    if (is_void(ary))
      type= 0;
    else if (structof(ary)==string && numberof(ary)==1)
      { ary= *pointer(ary(1));  type= 2; }
    else
      error, "illegal netCDF data type: "+nameof(structof(ary));
  } else {
    type= type(1);
  }
  _write, f, address, type;
  address+= 4;
  count= long(0);
  _write, f, address, numberof(ary);
  address+= 4;

  if (type<=0 || type>12 || !numberof(ary))
    return;
  else if (type<=2)     /* byte (1) or char (2) */
    NC_WritePrim, f, address, char, 1, ary;
  else if (type==3)     /* short */
    NC_WritePrim, f, address, short, 2, ary;
  else if (type==4)     /* long */
    NC_WritePrim, f, address, long, 4, ary;
  else if (type==5)     /* float */
    NC_WritePrim, f, address, float, 4, ary;
  else if (type==6)     /* double */
    NC_WritePrim, f, address, double, 8, ary;
  else if (type==7)     /* bitfield */
    error, "netCDF bitfield data not supported";
  else if (type==8)     /* string */
    NC_WriteMulti, f, address, string, NC_WriteString, ary;
  else if (type==9)     /* iarray */
    NC_WriteMulti, f, address, pointer, NC_WriteInts, ary;
  else if (type==10)    /* dimension */
    NC_WriteMulti, f, address, NC_dim, NC_WriteDim, ary;
  else if (type==11)    /* variable */
    NC_WriteMulti, f, address, NC_var, NC_WriteVar, ary;
  else if (type==12)    /* attribute */
    NC_WriteMulti, f, address, NC_attr, NC_WriteAttr, ary;
}

func NC_WritePrim(f, &address, type, size, data)
{
  _write, f, address, data;
  address+= 4*((size*numberof(data)+3)/4);
}

func NC_WriteMulti(f, &address, type, Writer, data)
{
  count= numberof(data);
  for (i=1 ; i<=count ; ++i) Writer, f, address, data(i);
}

/* ------------- end of netCDF routines --------------------------------- */
