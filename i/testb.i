/*
 * $Id: testb.i,v 1.2 2009-09-11 02:20:03 dhmunro Exp $
 * A comprehensive test of the native-Yorick binary I/O functions.
 *
 * Also, read back PDB files created by Stewart Brown's pdtest program,
 * and create facsimile of such files (although doesn't write PDB-style
 * pointers).
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func testb(do_stats)
/* DOCUMENT testb
         or testb, 1      (prints yorick_stats)
     Perform systematic test of all features of Yorick's binary I/O
     package.  This extends the simple test in testp.i.
 */
{
  local varCs, varCa, varSs, varSa, varIs, varIa, varLs, varLa;
  local varFs, varFa, varDs, varDa, varZs, varZa, varSIs, varSIa;
  local bundle;
  local varQs, varPs, varQa, varPa, linkedList, mixed;

  /* begin by writing a simple flat file */
  write, "  First test is to write flat files:";
  tester1, "junk", write_flat, read_flat;

  /* continue by writing a file with lots of indirections */
  write, "\n  Second test is to write files with pointers:";
  tester1, "junk", write_ptrs, read_ptrs;

  /* update the indirect files and write the flat stuff */
  write, "\n  Third test is to update pointered files with flat data:";
  tester2, "junk", write_flat, read_flat, read_ptrs;

  /* exhaustive test to and from all other primitive formats */
  write, "\n  Fourth test is exhaustive check of other primitive formats:";
  now= split= array(0.0, 3);
  timer, now;
  tester3;
  timer, now, split;
  timer_print, "Time to write/test all formats", split;

  /* Contents Log tests */
  write, "\n  Fifth test is check of Contents Log machinery:";
  tester4;

  /* History tests */
  write, "\n  Sixth test is flat history files (be patient):";
  timer, now;
  tester5;
  timer, now, split;
  timer_print, "Time to write history files", split;

  write, "\n  Seventh test is a pointered history file:";
  tester6;

  rm_hist;
  remove, "junk.clog";
  remove, "junk.pdb";  remove, "junk.pdbL";
  remove, "junkd.pdb";  remove, "junkd.pdbL";
  remove, "junkc.pdb";  remove, "junkc.pdbL";
  remove, "junks.pdb";  remove, "junks.pdbL";

  write, "\n  Eighth test is vsave in-memory files:";
  f = createb(char);
  write_flat, f, 0;
  b = vclose(f);
  read_flat, openb(b), 0;
  read_flat, openb(b), 1;

  f = createb(char);
  write_flat, f, 1;
  b = vclose(f);
  read_flat, openb(b), 0;
  read_flat, openb(b), 1;

  f = createb(char);
  write_ptrs, f, 0;
  b = vclose(f);
  read_ptrs, openb(b), 0;
  read_ptrs, openb(b), 1;

  f = createb(char);
  write_ptrs, f, 1;
  b = vclose(f);
  read_ptrs, openb(b), 0;
  read_ptrs, openb(b), 1;

  write, "\n  Ninth test is vpack in-memory files:";
  b = vpack_flat(0);
  bb = vpack_flat(1);
  if (numberof(b)!=numberof(bb) || anyof(b!=bb))
    write, "vpack_flat failed write order test";
  vunpack_flat, b, 0;
  vunpack_flat, b, 1;
}

func rm_hist
{
  for (i=0 ; i<22 ; i++) remove, swrite(format="junk%02ld.pdb", i);
  for (i=0 ; i<22 ; i++) remove, swrite(format="junk%02ld.pdbL", i);
}

func tester1(base, writer, reader)
{
  if (do_stats) "Begin1  "+print(yorick_stats())(1);
  write, "Write using native formats"
  f= createb(base+".pdb");
  if (do_stats) "Created  "+print(yorick_stats())(1);

  writer, f, 0;

  close, f;
  if (do_stats) "Closed   "+print(yorick_stats())(1);

  f= openb(base+".pdb");
  if (do_stats) "Opened   "+print(yorick_stats())(1);

  reader, f, 0;

  write, "Write using DEC primitive formats"
  g= createb(base+"d.pdb", dec_primitives);
  if (do_stats) "Created  "+print(yorick_stats())(1);

  writer, g, 1;
  reader, g, 0;

  write, "Write using Sun primitive formats"
  h= createb(base+"s.pdb", sun_primitives);
  if (do_stats) "Created  "+print(yorick_stats())(1);

  writer, h, 0;
  reader, h, 1;
  close, h;
  if (do_stats) "Closed S "+print(yorick_stats())(1);

  write, "Write using Cray primitive formats"
  h= createb(base+"c.pdb", cray_primitives);
  if (do_stats) "Created  "+print(yorick_stats())(1);

  writer, h, 1;
  reader, h, 1;

  close, h;
  if (do_stats) "Closed C "+print(yorick_stats())(1);
  close, g;
  if (do_stats) "Closed D "+print(yorick_stats())(1);
  close, f;
  if (do_stats) "Closed N "+print(yorick_stats())(1);
}

func tester2(base, writer, reader, reader2)
{
  if (do_stats) "Begin2  "+print(yorick_stats())(1);
  write, "Update using native formats"
  f= updateb(base+".pdb");
  if (do_stats) "Updating "+print(yorick_stats())(1);

  writer, f, 1;

  close, f;
  if (do_stats) "Closed   "+print(yorick_stats())(1);

  f= updateb(base+".pdb");
  if (do_stats) "Opened   "+print(yorick_stats())(1);

  reader, f, 1;
  reader2, f, 1;

  write, "Update using DEC primitive formats"
  g= updateb(base+"d.pdb", dec_primitives);
  if (do_stats) "Updating "+print(yorick_stats())(1);

  writer, g, 0;
  reader, g, 1;
  reader2, g, 1;

  write, "Update using Sun primitive formats"
  h= updateb(base+"s.pdb", sun_primitives);
  if (do_stats) "Created  "+print(yorick_stats())(1);

  writer, h, 1;
  reader, h, 0;
  reader2, h, 0;
  close, h;
  if (do_stats) "Closed S "+print(yorick_stats())(1);

  write, "Update using Cray primitive formats"
  h= updateb(base+"c.pdb", cray_primitives);
  if (do_stats) "Created  "+print(yorick_stats())(1);

  writer, h, 0;
  reader, h, 0;
  reader2, h, 0;

  close, h;
  if (do_stats) "Closed C "+print(yorick_stats())(1);
  close, g;
  if (do_stats) "Closed D "+print(yorick_stats())(1);
  close, f;
  if (do_stats) "Closed N "+print(yorick_stats())(1);
}

func tester3
{
  write, "Testing Sun format";
  test_full, "junk", sun_primitives;

  write, "Testing i86 format";
  test_full, "junk", i86_primitives;

  write, "Testing alpha format";
  test_full, "junk", alpha_primitives;

  write, "Testing sgi64 format";
  test_full, "junk", sgi64_primitives;

  write, "Testing DEC format";
  test_full, "junk", dec_primitives;

  write, "Testing Cray format";
  test_full, "junk", cray_primitives;

  write, "Testing XDR format";
  test_full, "junk", xdr_primitives;

  write, "Testing Mac format";
  test_full, "junk", mac_primitives;

  write, "Testing Mac long-double format";
  test_full, "junk", macl_primitives;

  write, "Testing IBM PC format";
  test_full, "junk", pc_primitives;

  write, "Testing VAX format";
  test_full, "junk", vax_primitives;

  write, "Testing VAX G-double format";
  test_full, "junk", vaxg_primitives;

  write, "Testing Sun-3/Sun-2 format";
  test_full, "junk", sun3_primitives;

  write, "Testing native format";
  test_full, "junk";
}

func test_full(base, primitives)
{
  write_ptrs, createb(base+".pdb", primitives), 0;
  write_flat, updateb(base+".pdb"), 0;
  read_flat, openb(base+".pdb"), 0;
  read_ptrs, openb(base+".pdb"), 0;
}

func tester4
{
  write, "Contents Log -- testing Sun format";
  test_clog, "junk", sun_primitives;

  write, "Contents Log -- testing DEC format";
  test_clog, "junk", dec_primitives;

  write, "Contents Log -- testing Cray format";
  test_clog, "junk", cray_primitives;

  write, "Contents Log -- testing VAX format";
  test_clog, "junk", vax_primitives;

  write, "Contents Log -- testing native format";
  test_clog, "junk";

  write, "Non-PDB Contents Log -- testing Sun format";
  test_clog, "junk", sun_primitives, 1;

  write, "Non-PDB Contents Log -- testing DEC format";
  test_clog, "junk", dec_primitives, 1;

  write, "Non-PDB Contents Log -- testing Cray format";
  test_clog, "junk", cray_primitives, 1;

  write, "Non-PDB Contents Log -- testing VAX format";
  test_clog, "junk", vax_primitives, 1;

  write, "Non-PDB Contents Log -- testing native format";
  test_clog, "junk",, 1;
}

func test_clog(base, primitives, nonpdb)
{
  if (!nonpdb) {
    f= createb(base+".pdb", primitives);
  } else {
    f= open(base+".pdb", "w+b");
    if (is_func(primitives)) primitives, f;
    _init_clog, f;
  }
  write_ptrs, f, 0;
  write_flat, f, 0;
  if (!nonpdb) dump_clog, f, base+".clog";
  close, f;
  if (!nonpdb) f= openb(base+".pdb", base+".clog");
  else f= openb(base+".pdb");
  read_flat, f, 0;
  read_ptrs, f, 0;
}

func tester5
{
  write, "History -- testing Sun format";
  test_hist, "junk00", sun_primitives;
  rm_hist;

  write, "History -- testing DEC format";
  test_hist, "junk00", dec_primitives;
  rm_hist;

  write, "History -- testing Cray format";
  test_hist, "junk00", cray_primitives;
  rm_hist;

  write, "History -- testing VAX format";
  test_hist, "junk00", vax_primitives;
  rm_hist;

  write, "History -- testing native format";
  test_hist, "junk00";
  rm_hist;
}

func test_hist(base, primitives)
{
  /* No non-record variables in 1st test.  */
  f= createb(base+".pdb", primitives);
  write_hist, f;
  close, f;

  f= openb(base+".pdb");
  read_hist, f;
  close, f;
}

test_records= 100;
test_filesize= 8000;   /* each record is about 1 kbyte long */

func write_hist(f)
{
  for (i=1 ; i<=test_records ; i++) {
    time= double(i-1);  ncyc= i;
    add_record, f, time, ncyc;
    if (i==1) set_filesize, f, test_filesize;
    save, f, time, ncyc;
    write_flat, f, 0;
  }
}

func read_hist(f)
{
  n= test_records/3;
  prime= 27;
  if (n%prime == 0) {
    prime= 13;
    if (n%prime == 0) prime= 0;
  }
  for (i=1 ; i<=test_records/3 ; i++) {
    if (prime) j= (i%prime) + 1;
    else j= i;
    jt, double(j-1);
    if (f.time!=double(j-1) || f.ncyc!=j)
      "time or ncyc bad at record "+print(j)(1);
    read_flat, f, 0;
  }
  for (i=test_records/3+1 ; i<=2*(test_records/3) ; i++) {
    jc, f, i;
    if (f.time!=double(i-1) || f.ncyc!=i)
      "time or ncyc bad at record "+print(i)(1);
    read_flat, f, 0;
  }
  i= 2*(test_records/3);
  do {
    restore, f, time, ncyc;
    if (f.time!=double(i-1) || f.ncyc!=i)
      "time or ncyc bad at record "+print(i)(1);
    read_flat, f, 0;
    i++;
  } while (jt(f));
  if (ncyc<test_records) "jt found only "+print(ncyc)(1)+
    " out of "+print(test_records)(1)+" records";
}

func tester6
{
  for_hist_test= 1;

  f= createb("junk00.pdb");
  write_flat, f, 1;
  for (i=1 ; i<=23 ; i++) {
    time= double(i-1);  ncyc= i;
    if (i==1) {
      /* records with pointers must be built before writing them */
      add_record, f;
      add_variable, f, -1, "varQs", string;
      add_variable, f, -1, "varQa", string, 2, 4;
      add_variable, f, -1, "varPs", pointer;
      add_variable, f, -1, "varPa", pointer, 2, 3, 2;
      add_variable, f, -1, "linkedList", pointer;
      add_variable, f, -1, "mixed", Mixed;
      save, f, Link;
    }
    add_record, f, time, ncyc;
    if (i==1) set_filesize, f, test_filesize;
    save, f, time, ncyc;
    write_ptrs, f, 0;
  }
  close, f;

  f= openb("junk00.pdb");
  for (i=1 ; i<=23 ; i++) {
    j= (17*i)%23 + 1;
    jt, double(j-1);
    if (f.time!=double(j-1) || f.ncyc!=j)
      "time or ncyc bad at record "+print(j)(1);
    read_ptrs, f, 1;
    read_flat, f, 1;
  }
  close, f;

  rm_hist;
}

func write_flat(f, how)
{
  extern varCs, varCa, varSs, varSa, varIs, varIa, varLs, varLa;
  extern varFs, varFa, varDs, varDa, varZs, varZa, varSIs, varSIa;
  extern bundle;

  /* invent a bunch of garbage to be written */
  varCs= 'A';  varSs= -37s;  varIs= 76n;  varLs= 144;
  varCa= ['Z', '\370', 'a'];
  varSa= short([[0,0,0],[0,-100,100]]);
  varIa= int([[[1,2,3,4],[-30,-20,-10,0],[-1,2,-3,4]],
              [[0,-30,20,-10],[4,3,2,1],[3,-4,1,-2]]]);
  varLa= [123456789, -987654321];
  varFs= 1.5f;  varDs= -6.022e23;  varZs= 1-1i;
  varFa= float([[0,0,0],[0,-100,100]]);
  varDa= double([[[1,2,3,4],[-30,-20,-10,0],[-1,2,-3,4]],
                 [[0,-30,20,-10],[4,3,2,1],[3,-4,1,-2]]]);
  varZa= [123456789-3.5i, -987654321+0i];

  varSIs= Simple(one='Q', two= -137., three= -37s);
  varSIa= [varSIs, Simple(one='\370', two= 1.5, three= 37s)];

  bundle= Flat(varCs=varCs, varSs=varSs, varIs=varIs, varLs=varLs,
               varCa=varCa, varSa=varSa, varIa=varIa, varLa=varLa,
               varFs=varFs, varDs=varDs, varZs=varZs, varSIs=varSIs,
               varFa=varFa, varDa=varDa, varZa=varZa, varSIa=varSIa);

  if (!how) {
    save, f, varCs, varSs, varIs, varLs, varCa, varSa, varIa, varLa;
    save, f, varFs, varDs, varZs, varSIs, varFa, varDa, varZa, varSIa;
    save, f, bundle;
  } else {
    add_variable, f, -1, "varCs", char;
    add_variable, f, -1, "varSs", "short";
    add_variable, f, -1, "varIs", int;
    add_variable, f, -1, "varLs", "long";
    add_variable, f, -1, "varCa", "char", 3;
    add_variable, f, -1, "varSa", short, 3, 2;
    add_variable, f, -1, "varIa", "int", 4, [2,3,2];
    add_variable, f, -1, "varLa", long, [1,2];

    save, f, varFs, varDs;

    add_variable, f, -1, "varZs", complex;
    f.varZs.re= varZs.re;
    f.varZs.im= varZs.im;

    add_variable, f, -1, "varFa", "float", [1,3], [1,2];
    add_variable, f, -1, "varDa", double, [2,4,3], 2;
    add_variable, f, -1, "varZa", "complex", 2;

    add_member, f, "Simple", -1, "one", char;
    add_member, f, "Simple", -1, "two", "double";
    add_member, f, "Simple", -1, "three", short;
    install_struct, f, "Simple";

    add_variable, f, -1, "varSIs", Simple;
    add_variable, f, -1, "varSIa", "Simple", 2;
    add_variable, f, -1, "bundle", Flat;

    f.varCs= varCs;
    save, f, varCa, varSs, varIs, varLs;
    f.varSa= varSa;
    f.varIa(::-1,,1)= varIa(::-1,,1);
    f.varIa(,::-1,2)= varIa(,::-1,2);
    f.varLa= varLa;
    f.varFa([3,1,5,2,4,6])= varFa([3,1,5,2,4,6]);
    f.varDa(,[3,1,5,2,4,6])= varDa(,[3,1,5,2,4,6]);
    f.varZa(1).re= varZa.re(1);
    f.varZa(1).im= varZa.im(1);
    f.varZa.re(2)= varZa(2).re;
    f.varZa.im(2)= varZa(2).im;
    f.varSIs.one= varSIs.one;
    f.varSIs.two= varSIs.two;
    f.varSIs.three= varSIs.three;
    f.varSIa.one= varSIa.one;
    f.varSIa.two= varSIa.two;
    f.varSIa(1).three= varSIa.three(1);
    f.varSIa.three(2)= varSIa(2).three;
    save, f, bundle;
  }

  if (do_stats) "Saved    "+print(yorick_stats())(1);
}

func read_flat(f, how)
{
  local varCs, varCa, varSs, varSa, varIs, varIa, varLs, varLa;
  local varFs, varFa, varDs, varDa, varZs, varZa, varSIs, varSIa;
  local bundle;

  if (!how) {
    restore, f;
  } else {
    restore, f, varFs, varCa, varDs, varSa;
    varCs= f.varCs;
    varFa= array(float, 3, 2);
    varFa([4,6,2,5,3,1])= f.varFa([4,6,2,5,3,1]);
    varSs= f.varSs;
    varDa= array(double, 4,3,2);
    varDa(4,1:3,::-1)= f.varDa(4,,::-1);
    varDa(3:1:-1,..)= f.varDa(3:1:-1,..);
    restore, f, varIs, varLs, varIa, varLa;
    varZa= array(0i, 2);
    varZa.re= f.varZa.re;
    varZa.im= f.varZa.im;
    varZs= f.varZs;
    restore, f, bundle;
    bundle.varDa= 0.0;
    bundle.varZa= 0.0;
    bundle.varDa(4,1:3,::-1)= f.varDa(4,,::-1);
    bundle.varDa(3:1:-1,..)= f.varDa(3:1:-1,..);
    bundle.varZa(1).re= f.bundle.varZa.re(1);
    bundle.varZa(1).im= f.bundle.varZa.im(1);
    bundle.varZa.re(2)= f.bundle.varZa(2).re;
    bundle.varZa.im(2)= f.bundle.varZa(2).im;
    varSIs= f.varSIs;
    varSIa= array(Simple, 2);
    varSIa(1).one= f.varSIa.one(1);
    varSIa.one(2)= f.varSIa(2).one;
    varSIa.two= f.varSIa.two;
    varSIa.three= f.varSIa.three;
  }
  if (do_stats) "Restored "+print(yorick_stats())(1);

  goofs= [varCs!='A', varSs!=-37s, varIs!=76n, varLs!=144,
          anyof(varCa!=['Z', '\370', 'a']),
          anyof(varSa!=short([[0,0,0],[0,-100,100]])),
          anyof(varIa!=int([[[1,2,3,4],[-30,-20,-10,0],[-1,2,-3,4]],
                            [[0,-30,20,-10],[4,3,2,1],[3,-4,1,-2]]])),
          anyof(varLa!=[123456789, -987654321]),
          varFs!=1.5f, abs(varDs+6.022e23)>6.022e11, varZs!=1-1i,
          anyof(varFa!=float([[0,0,0],[0,-100,100]])),
          anyof(varDa!=double([[[1,2,3,4],[-30,-20,-10,0],[-1,2,-3,4]],
                               [[0,-30,20,-10],[4,3,2,1],[3,-4,1,-2]]])),
          anyof(varZa!=[123456789-3.5i, -987654321+0i]),

          struct_neq(varSIs, Simple(one='Q', two= -137., three= -37s)),
          struct_neq(varSIa,
                     [varSIs, Simple(one='\370', two= 1.5, three= 37s)]),

          struct_neq(bundle,
                     Flat(varCs=varCs, varSs=varSs, varIs=varIs, varLs=varLs,
                          varCa=varCa, varSa=varSa, varIa=varIa, varLa=varLa,
                          varFs=varFs, varDs=varDs, varZs=varZs,
                          varFa=varFa, varDa=varDa, varZa=varZa,
                          varSIs=varSIs, varSIa=varSIa))];
  if (anyof(goofs)) {
    "read_flat failed -- goof flags are:";
    goofs;
  }

  if (do_stats) "Checked  "+print(yorick_stats())(1);
}

func vpack_flat(how)
{
  extern varCs, varCa, varSs, varSa, varIs, varIa, varLs, varLa;
  extern varFs, varFa, varDs, varDa, varZs, varZa, varQs, varQa;

  /* invent a bunch of garbage to be written */
  varCs= 'A';  varSs= -37s;  varIs= 76n;  varLs= 144;
  varCa= ['Z', '\370', 'a'];
  varSa= short([[0,0,0],[0,-100,100]]);
  varIa= int([[[1,2,3,4],[-30,-20,-10,0],[-1,2,-3,4]],
              [[0,-30,20,-10],[4,3,2,1],[3,-4,1,-2]]]);
  varLa= [123456789, -987654321];
  varFs= 1.5f;  varDs= -6.022e23;  varZs= 1-1i;
  varFa= float([[0,0,0],[0,-100,100]]);
  varDa= double([[[1,2,3,4],[-30,-20,-10,0],[-1,2,-3,4]],
                 [[0,-30,20,-10],[4,3,2,1],[3,-4,1,-2]]]);
  varZa= [123456789-3.5i, -987654321+0i];
  varQs= "";
  varQa= [["hello", string(0), "World", "", "!"],
          ["a", "b", string(0), "", "e"]];
  null = [];

  if (!how) {
    b = vpack(varCs, varSs, varIs, varLs, varCa, varSa, varIa, varLa, varQs,
              varFs, varDs, varZs, varFa, null, varQa, varDa, varZa);
  } else {
    f = vopen(,1);
    vpack, f, varCs, varSs, varIs, varLs, varCa, varSa, varIa;
    vpack, f, varLa;
    vpack, f, varQs, varFs, varDs;
    vpack, f, varZs, varFa, null, varQa, varDa, varZa;
    b = vpack(f);
  }

  if (do_stats) "Packed   "+print(yorick_stats())(1);
  return b;
}

func vunpack_flat(b, how)
{
  local varCs, varCa, varSs, varSa, varIs, varIa, varLs, varLa;
  local varFs, varFa, varDs, varDa, varZs, varZa, varQs, varQa;

  null = 1;
  if (!how) {
    neof =
      !vunpack(b, varCs, varSs, varIs, varLs, varCa, varSa, varIa, varLa,
               varQs, varFs, varDs, varZs, varFa, null, varQa, varDa, varZa);
    neof += 2*(!vunpack(b));
    vunpack, b;  /* test reset feature */
    neof += 4*vunpack(b);
  } else {
    eof1 = vunpack(b, v1, v2);
    bad1 = (v1!='A' || v2!=-37s);
    vunpack, b;  /* test reset feature */
    eof2 = vunpack(b, varCs, varSs, varIs, varLs, varCa);
    varSa = vunpack(b, -);
    eof3 = vunpack(b, varIa, varLa, varQs, varFs, varDs);
    varZs = vunpack(b, -);
    eof4 = !vunpack(b, varFa, null, varQa, varDa, varZa);
    neof = eof1 + 2*eof2 + 4*eof3 + 8*eof4 + 16*bad1;
  }
  if (do_stats) "Restored "+print(yorick_stats())(1);

  goofs= [varCs!='A', varSs!=-37s, varIs!=76n, varLs!=144,
          anyof(varCa!=['Z', '\370', 'a']),
          anyof(varSa!=short([[0,0,0],[0,-100,100]])),
          anyof(varIa!=int([[[1,2,3,4],[-30,-20,-10,0],[-1,2,-3,4]],
                            [[0,-30,20,-10],[4,3,2,1],[3,-4,1,-2]]])),
          anyof(varLa!=[123456789, -987654321]),
          varFs!=1.5f, abs(varDs+6.022e23)>6.022e11, varZs!=1-1i,
          anyof(varFa!=float([[0,0,0],[0,-100,100]])),
          anyof(varDa!=double([[[1,2,3,4],[-30,-20,-10,0],[-1,2,-3,4]],
                               [[0,-30,20,-10],[4,3,2,1],[3,-4,1,-2]]])),
          anyof(varZa!=[123456789-3.5i, -987654321+0i]),
          varQs!="", anyof(varQa != [["hello", string(0), "World", "", "!"],
                                     ["a", "b", string(0), "", "e"]]),
          !is_void(null), neof];
  if (anyof(goofs)) {
    "vunpack_flat failed -- goof flags are:";
    goofs;
  }

  if (do_stats) "Checked  "+print(yorick_stats())(1);
}

func write_ptrs(f, how)
{
  extern varQs, varPs, varQa, varPa, linkedList, mixed;

  extern varCs, varZa, varFa, varDa;  /* referenced by ptrs */
  varCs= 'A';
  varFa= float([[0,0,0],[0,-100,100]]);
  varDa= double([[[1,2,3,4],[-30,-20,-10,0],[-1,2,-3,4]],
                 [[0,-30,20,-10],[4,3,2,1],[3,-4,1,-2]]]);
  varZa= [123456789-3.5i, -987654321+0i];

  /* invent a bunch of garbage to be written */
  varQs= "Hello, world!";
  varPs= pointer(varQs);
  varQa= [["a", "bc"], ["def", "ghij"], ["klmn", "op"], [string(0), ""]];
  varPa= [[[&varCs, &varDa], [&varZa, &varDa], [&varPs, &varDa]],
          [[&varFa, &varDa], [&varDa, &varPs], [&varQa, &varQa]]];

  linkedList= &Link(name="second", index=2);
  linkedList->next= &Link(next=linkedList, name="third", index=3);
  linkedList->next->next= &Link(name="last", index=4);
  linkedList= &Link(next=linkedList, name="first", index=1);

  mixed= Mixed(varPs=varPs, s=-37s, varQa=varQa, varQs=varQs,
               links=linkedList, varPa=varPa);

  if (!how) {
    /* This should produce maximum number of duped pointers.  */
    if (!for_hist_test) save, f, complex, Link;
    save, f, varQs, varQa, varPs, varPa, linkedList, mixed;
  } else {
    /* Piecemeal writes result in some pointee rewrites.  */
    add_variable, f, -1, "varQs", string;
    add_variable, f, -1, "varQa", "string", 2, 4;
    add_variable, f, -1, "varPs", "pointer";
    add_variable, f, -1, "varPa", pointer, [3,2,3,2];
    save, f, complex, varQs;
    f.varQa(,::-1)= varQa(,::-1);
    f.varPs= varPs;
    f.varPa(2,1:3)= varPa(2,1:3);
    f.varPa(1,4:6)= varPa(1,4:6);
    add_member, f, "Link", -1, "next", pointer;
    add_member, f, "Link", -1, "name", "string";
    add_member, f, "Link", -1, "index", "long";
    install_struct, f, "Link";
    add_variable, f, -1, "linkedList", "pointer";
    save, f, mixed;
    f.linkedList= linkedList;
    f.varPa(2,4:6)= varPa(2,4:6);
    f.varPa(1,1:3)= varPa(1,1:3);
  }

  if (do_stats) "Saved ps "+print(yorick_stats())(1);
}

func read_ptrs(f, how)
{
  local varQs, varPs, varQa, varPa, linkedList, mixed;

  extern varCs, varZa, varFa, varDa;  /* referenced by ptrs */

  if (!how) {
    /* This should produce maximum number of duped pointers.  */
    restore, f, varQs, varPs, varQa, varPa, linkedList, mixed;
  } else {
    /* Piecemeal reads result in some pointee rereads.  */
    varQs= f.varQs;
    varQa= array(string, 2, 4);
    mixed= f.mixed;
    varQa(,::-1)= f.varQa(,::-1);
    varPa= array(pointer, 2, 3, 2);
    varPa(1,::-1,2)= f.varPa(1,::-1,2);
    linkedList= f.linkedList;
    varPa(2,,2)= f.varPa(2,,2);
    varPs= f.varPs;
    varPa(..,1)= f.varPa(..,1);
    mixed.varPa= &[];
    mixed.varPa(::-1,..)= f.mixed.varPa(::-1,..)
  }
  if (do_stats) "Restored "+print(yorick_stats())(1);

  goofs= [varQs!="Hello, world!",
          anyof(varQa!=[["a", "bc"], ["def", "ghij"],
                        ["klmn", "op"], [string(0), ""]]),
          string(varPs)!="Hello, world!",
          *varPa(1,1,1)!=varCs, anyof(*varPa(2,1,1)!=varDa),
          anyof(*varPa(1,2,1)!=varZa), anyof(*varPa(2,2,1)!=varDa),
          string(*varPa(1,3,1))!="Hello, world!", anyof(*varPa(2,3,1)!=varDa),
          anyof(*varPa(1,1,2)!=varFa), anyof(*varPa(2,1,2)!=varDa),
          anyof(*varPa(1,2,2)!=varDa), string(*varPa(2,2,2))!="Hello, world!",
          anyof(*varPa(1,3,2)!=varQa), anyof(*varPa(2,3,2)!=varQa)];
  if (anyof(goofs)) {
    "read_ptrs failed on simple string or pointer -- goof flags are:";
    goofs;
  }

  ll1= linkedList;
  ll3= linkedList->next->next;
  goofs= [ll1->name!="first", ll1->index!=1,
          ll1->next->name!="second", ll1->next->index!=2,
          ll3->name!="third", ll3->index!=3,
          ll3->next->name!="last", ll3->next->index!=4,
          !is_void(*ll3->next->next)];
  if (anyof(goofs)) {
    "read_ptrs failed on linked list -- goof flags are:";
    goofs;
  }

  ll1= mixed.links;
  ll3= mixed.links->next->next;
  goofs= [ll1->name!="first", ll1->index!=1,
          ll1->next->name!="second", ll1->next->index!=2,
          ll3->name!="third", ll3->index!=3,
          ll3->next->name!="last", ll3->next->index!=4,
          !is_void(*ll3->next->next),
          string(mixed.varPs)!="Hello, world!", mixed.s!=-37s,
          anyof(mixed.varQa!=[["a", "bc"], ["def", "ghij"],
                              ["klmn", "op"], [string(0), ""]]),
          mixed.varQs!="Hello, world!",
          anyof(*mixed.varPa(1,2,1)!=varZa),
          anyof(*mixed.varPa(2,2,1)!=varDa),
          string(*mixed.varPa(1,3,1))!="Hello, world!",
          anyof(*mixed.varPa(2,3,1)!=varDa),
          anyof(*mixed.varPa(1,1,2)!=varFa),
          anyof(*mixed.varPa(2,1,2)!=varDa),
          anyof(*mixed.varPa(1,2,2)!=varDa),
          string(*mixed.varPa(2,2,2))!="Hello, world!",
          anyof(*mixed.varPa(1,3,2)!=varQa),
          anyof(*mixed.varPa(2,3,2)!=varQa)];
  if (anyof(goofs)) {
    "read_ptrs failed on mixed object -- goof flags are:";
    goofs;
  }

  if (do_stats) "Checkedp "+print(yorick_stats())(1);
}

func struct_neq(x, y)
{
  members= strtok(strtok(print(structof(x))(2:-1))(2,)," (;")(1,);
  m= numberof(members);
  for (i=1 ; i<=m ; i++) {
    xm= get_member(x, members(i));
    ym= get_member(y, members(i));
    if (typeof(xm)=="struct_instance") {
      if (struct_neq(xm, ym)) return 1;
    } else {
      if (anyof(xm!=ym)) return 1;
    }
  }
  return 0;
}

struct Simple {
  char one;
  double two;
  short three;
}

struct Flat {
  int varIs, varIa(4,3,2);
  double varDs;
  char varCs;
  float varFs, varFa(3,2);
  complex varZs, varZa(2);
  short varSs, varSa(3,2);
  double varDa(4,3,2);
  Simple varSIs, varSIa(2);
  long varLs, varLa(2);
  char varCa(3);
}

struct Link {
  pointer next;
  string name;
  long index;
}

struct Mixed {
  pointer varPs;
  short s;
  string varQa(2,4), varQs;
  pointer links, varPa(2,3,2);
}

func pdcheck1(prefix)
{
  write, "Testing <- native...";
  pdtest1_check, prefix+"-nat.db1";
  write, "Testing <- cray...";
  pdtest1_check, prefix+"-cray.db1";
  write, "Testing <- dos...";
  pdtest1_check, prefix+"-dos.db1";
  write, "Testing <- mac...";
  pdtest1_check, prefix+"-mac.db1";
  write, "Testing <- mips...";
  pdtest1_check, prefix+"-mips.db1";
  write, "Testing <- sun3...";
  pdtest1_check, prefix+"-sun3.db1";
  write, "Testing <- sun4...";
  pdtest1_check, prefix+"-sun4.db1";
  write, "Testing <- vax...";
  pdtest1_check, prefix+"-vax.db1";
}

func pdcheck2
{
  write, "Testing sun_primitives db1 write...";
  pdtest1_write,"junk.pdb", sun_primitives;
  pdtest1_check,"junk.pdb";
  write, "Testing dec_primitives db1 write...";
  pdtest1_write,"junk.pdb", dec_primitives;
  pdtest1_check,"junk.pdb";
  write, "Testing cray_primitives db1 write...";
  pdtest1_write,"junk.pdb", cray_primitives;
  pdtest1_check,"junk.pdb";
  write, "Testing mac_primitives db1 write...";
  pdtest1_write,"junk.pdb", mac_primitives;
  pdtest1_check,"junk.pdb";
  write, "Testing macl_primitives db1 write...";
  pdtest1_write,"junk.pdb", macl_primitives;
  pdtest1_check,"junk.pdb";
  write, "Testing pc_primitives db1 write...";
  pdtest1_write,"junk.pdb", pc_primitives;
  pdtest1_check,"junk.pdb";
  write, "Testing sun3_primitives db1 write...";
  pdtest1_write,"junk.pdb", sun3_primitives;
  pdtest1_check,"junk.pdb";
  write, "Testing vax_primitives db1 write...";
  pdtest1_write,"junk.pdb", vax_primitives;
  pdtest1_check,"junk.pdb";
  write, "Testing vaxg_primitives db1 write...";
  pdtest1_write,"junk.pdb", vaxg_primitives;
  pdtest1_check,"junk.pdb";
  write, "Testing xdr_primitives db1 write...";
  pdtest1_write,"junk.pdb", xdr_primitives;
  pdtest1_check,"junk.pdb";
  write, "Testing sun_primitives db1 write w/PDB-style pointers...";
  pdtest1_write,"junk.pdb", sun_primitives, 1;
  pdtest1_check,"junk.pdb";
  write, "Testing dec_primitives db1 write w/PDB-style pointers...";
  pdtest1_write,"junk.pdb", dec_primitives, 1;
  pdtest1_check,"junk.pdb";
  write, "Testing cray_primitives db1 write w/PDB-style pointers...";
  pdtest1_write,"junk.pdb", cray_primitives, 1;
  pdtest1_check,"junk.pdb";
  write, "Testing native db1 write...";
  pdtest1_write,"junk.pdb";
  pdtest1_check,"junk.pdb";
}

func pdtest1_check(filename)
{
  f= openb(filename);
  vars= *get_vars(f)(1);
  if (numberof(vars)!=15) write, "Should be 15 variables in "+filename;

  local cs, ss, is, fs, ds, ca, sa, ia, fa2, da, cap, fa2_app, fs_app;
  local view, graph;
  restore, f;

  if (typeof(cs)!="char" || dimsof(cs)(1)!=0 || cs!='Q' /* 0x51 */)
    write, "variable cs bad in "+filename;
  if (typeof(ss)!="short" || dimsof(ss)(1)!=0 || ss!=-514)
    write, "variable ss bad in "+filename;
  if (typeof(is)!="int" || dimsof(is)(1)!=0 || is!=10)
    write, "variable is bad in "+filename;
  if (typeof(fs)!="float" || dimsof(fs)(1)!=0 || float_neq(fs,3.14159))
    write, "variable fs bad in "+filename;
  if (typeof(ds)!="double" || dimsof(ds)(1)!=0 || double_neq(ds,exp(1)))
    write, "variable ds bad in "+filename;

  if (typeof(ca)!="char" || dimsof(ca)(1)!=1 || dimsof(ca)(2)!=10 ||
      string(&ca)!="Hi there!")
    write, "variable ca bad in "+filename;
  if (typeof(sa)!="short" || dimsof(sa)(1)!=1 || dimsof(sa)(2)!=5 ||
      anyof(sa!=[2,1,0,-1,-2]))
    write, "variable sa bad in "+filename;
  if (typeof(ia)!="int" || dimsof(ia)(1)!=1 || dimsof(ia)(2)!=5 ||
      anyof(ia!=[-2,-1,0,1,2]))
    write, "variable ia bad in "+filename;
  if (typeof(fa2)!="float" || dimsof(fa2)(1)!=2 ||
      anyof(dimsof(fa2)!=[2,3,4]) ||
      anyof(float_neq(fa2, [[1,1,1],[2,4,8],[3,9,27],[4,16,64]])))
    write, "variable fa2 bad in "+filename;
  if (typeof(da)!="double" || dimsof(da)(1)!=1 || dimsof(da)(2)!=4 ||
      anyof(double_neq(da, exp([1,2,3,4]))))
    write, "variable da bad in "+filename;

  if (typeof(cap)!="pointer" || dimsof(cap)(1)!=1 ||  dimsof(cap)(2)!=3 ||
      typeof(*cap(1))!="char" || string(cap(1))!="lev1" ||
      typeof(*cap(2))!="char" || string(cap(2))!="lev2" ||
      typeof(*cap(3))!="char" || string(cap(3))!="tar fu blat")
    write, "variable cap bad in "+filename;

  if (typeof(fs_app)!="float" || dimsof(fs_app)(1)!=0 ||
      float_neq(fs_app,-3.14159))
    write, "variable fs_app bad in "+filename;
  if (typeof(fa2_app)!="float" || dimsof(fa2_app)(1)!=2 ||
      anyof(dimsof(fa2_app)!=[2,3,4]) ||
      anyof(float_neq(fa2_app, [[1,2,3],[1,4,9],[1,8,27],[1,16,81]])))
    write, "variable fa2_app bad in "+filename;

  if (nameof(structof(view))!="l_frame" || dimsof(view)(1)!=0 ||
      float_neq(view.x_min,0.1) || float_neq(view.x_max,1.0) ||
      float_neq(view.y_min,-0.5) || float_neq(view.y_max,0.5))
    write, "variable view bad in "+filename;

  if (nameof(structof(graph))!="plot" || dimsof(graph)(1)!=0 ||
      anyof(float_neq(graph.x_axis,[0,.1,.2,.3,.4,.5,.6,.7,.8,.9])) ||
      anyof(float_neq(graph.y_axis,[.5,.4,.3,.2,.1,0,-.1,-.2,-.3,-.4])) ||
      float_neq(graph.view.x_min,0.1) || float_neq(graph.view.x_max,1.0) ||
      float_neq(graph.view.y_min,-0.5) || float_neq(graph.view.y_max,0.5) ||
      graph.npts!=10 || string(graph.label)!="test graph")
    write, "variable graph bad in "+filename;
}

func float_neq(a, b)
{
  return abs(a-b)/(abs(a)+abs(b)+1.e-99) > 1.e-6;
}

func double_neq(a, b)
{
  return abs(a-b)/(abs(a)+abs(b)+1.e-99) > 1.e-12;
}

struct l_frame {
  float x_min, x_max, y_min, y_max;
}

struct plot {
  float x_axis(10), y_axis(10);
  int npts;
  pointer label;
  l_frame view;
}

func pdtest1_write(filename, primitives, pdbptrs)
{
  cs= 'Q';
  ss= -514s;
  is= 10n;
  fs= 3.14159;
  ds= exp(1);

  ca= *pointer("Hi there!");
  sa= [2s,1s,0s,-1s,-2s];
  ia= [-2n,-1n,0n,1n,2n];
  fa2= [[1.f,1.f,1.f],[2.f,4.f,8.f],[3.f,9.f,27.f],[4.f,16.f,64.f]];
  da= exp([1,2,3,4]);

  cap= [pointer("lev1"), pointer("lev2"), pointer("tar fu blat")];

  fs= 3.14159f;

  view= l_frame(x_min=0.1,x_max=1.0,y_min=-0.5,y_max=0.5);
  graph= plot(x_axis=[0,.1,.2,.3,.4,.5,.6,.7,.8,.9],
              y_axis=[.5,.4,.3,.2,.1,0,-.1,-.2,-.3,-.4],
              npts=10, label=pointer("test graph"),
              view=view);

  fa2_app= float([[1,2,3],[1,4,9],[1,8,27],[1,16,81]]);
  fs_app= -3.14159f;

  if (!pdbptrs) {
    save, createb(filename, primitives),\
      cs,ss,is,fs,ds, ca,sa,ia,fa2,da, cap, view,graph;
  } else {
    f= createb(filename, primitives);
    save, f, l_frame;
    add_member, f, "plot", -1, "x_axis", float, 10;
    add_member, f, "plot", -1, "y_axis", float, 10;
    add_member, f, "plot", -1, "npts", int;
    add_member, f, "plot", -1, "label", "char *";
    add_member, f, "plot", -1, "view", l_frame;
    install_struct, f, "plot";
    save, f, cs,ss,is,fs,ds, ca,sa,ia,fa2,da;
    add_variable, f, -1, "cap", "char*", 3;
    save, f, cap, view,graph;
    close, f;
  }
  save, updateb(filename), fa2_app,fs_app;
}
