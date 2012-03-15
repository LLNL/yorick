/* testoxy.i
 * $Id: testoxy.i,v 1.2 2010-07-17 22:44:56 dhmunro Exp $
 * test object extensions to yorick
 */
/* Copyright (c) 2010, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* intentionally create some items which conflict with object members */
func test1(x)
{
  goof = (data1 != 27);
  d = x(data1);
  if (d != -10) goof |= 2;
  x, test1;
  if (x("data1")!=-9 || data1!=27) goof |= 4;
  x, "test1";
  if (x(data1)!=-8 || data1!=27) goof |= 8;
  data2 = x(5);
  save, x, "data1", d, data2;
  if (x(data1)!=-10 || data1!=27) goof |= 0x10;
  if (x(*)!=6 ||
      anyof(x(*,)!=["test1", "test2", "test3", "data1", "data2", "data3"]) ||
      x(*,5)!="data2" || anyof(x(*,3::-2)!=["test3","test1"]) ||
      x(*,0)!="data3" || anyof(x(*,[-1,3,5])!=["data2","test3","data2"]) ||
      x(*,"data1")!=4 || anyof(x(*,["test1","data3","data1"])!=[1,6,4]))
    goof |= 0x20;
  d = x(data2,-,::-1);
  a = x(-1,-,::-1);
  if (anyof(dimsof(d)!=[2,1,5]) || anyof(d!=indgen(5:1:-1)(-,)) ||
      anyof(dimsof(a)!=[2,1,5]) || anyof(a!=indgen(5:1:-1)(-,)))
    goof |= 0x40;
  a = x([-1,2]);
  save,a, [], x(2:4);
  if (anyof(a(*,)!=["data2","test2","test3","data1"])) goof |= 0x80;
  save,a, , span(0,1,3), string(0), x;
  if (anyof(a(*,)!=["data2","test2","test3",
                    "data1",string(0),string(0)])) goof |= 0x100;
  if (is_obj(d) || !is_obj(a) || is_obj(a)!=3 ||
      anyof(is_obj(a,)!=[0,0,0,0,0,3]) || is_obj(a,"test2") ||
      anyof(is_obj(a,0:2:-2)!=[3,0,0]) || anyof(is_obj(a,[0,-5,6])!=[3,0,3]) ||
      anyof(is_obj(a,["test2","data1"])))
    goof |= 0x200;
  if (is_obj(d,"x",1)!=-2 || anyof(is_obj(a,[0,-5,6],1)!=[3,0,3]) ||
      is_obj(a,[0,-5,10],1)!=-1 ||is_obj(a,"oops",1)!=-1|| is_obj(a,9,1)!=-1 ||
      anyof(is_obj(a,["test2","oops","data1"],1)!=[0,-1,0]))
    goof |= 0x200;
  b = x();
  if (b(*)!=6 || anyof(b(*,)!=x(*,))) goof |= 0x400;
  b = x(0:2:-2);
  if (b(*)!=3 || anyof(b(*,)!=["data3","data1","test2"])) goof |= 0x400;
  b = x([6,-2,0]);  /* note repeat handling feature ?? */
  if (b(*)!=2 || anyof(b(*,)!=["data3","data1"])) goof |= 0x400;
  b = x(["data3","data1","data3"]);
  if (b(*)!=2 || anyof(b(*,)!=["data3","data1"])) goof |= 0x400;
  if (is_func(x(data3)) != 4) goof |= 0x800;
  b = x(data3,d,data2);
  if (anyof(vunpack(b,-)!=d) || anyof(vunpack(b,-)!=data2)) goof |= 0x800;
  b = x.data3(d,data2);
  if (anyof(vunpack(b,-)!=d) || anyof(vunpack(b,-)!=data2)) goof |= 0x800;
  b = x(4:6);
  save, b, y=cos, data1=[4.5,-1.5];
  if (b(*)!=4 || anyof(b(*,)!=["data1","data2","data3","y"]) ||
      anyof(b(data1)!=[4.5,-1.5]) || b(y)!=cos) goof |= 0x1000;
  b, data1=b(data1,2), z=tan;
  if (b(*)!=5 || anyof(b(*,)!=["data1","data2","data3","y","z"]) ||
      b(data1)!=-1.5 || b(z)!=tan) goof |= 0x1000;
  if (!goof) write, "PASSED oxy test1";
  else write, format="***FAILED oxy test1, goof=%04lx\n", goof;
}
data1 = 27;

/* begin yorick idiom for creating an object
 * first save saves the original values of testoxy members to be defined here
 * second save save original values of incidental variables used to construct
 *   the data members of the object, but not part of the object itself
 */
testoxy = save(test1, test2, test3, data1, data2, data3);
scratch = save(scratch, x, y, z);
/* now define the object members */

data1 = -10;
data2 = indgen(5);
data3 = vpack;      /* try something not an array */

func test1
{
  use, data1;
  data1++;
}

func test2(x, &data3, z)
{
  if (x==use(data1)) restore, use, data3;
  if (z==use(4)) save, use, data1=use("data1")+12;
  return save(wow=use(3:5), , use([1,2]));
}

func test3(a, .., key=)
{
  while (more_args()) save, a, string(0), next_arg();
  return (key==use(data1));
}

/* first restore puts back scratch variables (including scratch object)
 * second restore puts back original member values, creating the new
 *   object from the values we just defined
 * this completes yorick idiom for creating an object
 */
restore, scratch;
testoxy = restore(testoxy);

test1, testoxy;
y = 7;
x = testoxy(test2, -10, y, -10);
if (y!=vpack || testoxy.data1!=2 || x(*)!=3 ||
    anyof(x(*,)!=["wow","test1","test2"])) {
    write, format="***FAILED oxy test2%s\n", "";
} else {
  write, "PASSED oxy test2";
}
y = save(cos);
x = x(wow,test3, key=2, y,6,7,8);
if (!x || y(*)!=4 || y(cos)!=cos || anyof([y(2),y(3),y(4)]!=[6,7,8])) {
    write, format="***FAILED oxy test3%s\n", "";
} else {
  write, "PASSED oxy test3";
}

/* ---------------------- test closures */

func cl_assert(test, msg) { if (!test) error, msg; }
errs2caller, cl_assert;

func cl_op_extern1(data, arg)
{
  result = data;
  if (!is_void(arg)) result += arg;
  if (am_subroutine()) write, result;
  else return result;
}

func cl_op_extern2(data, arg)
{
  result = data + arg + 1;
  if (am_subroutine()) write, result;
  else return result;
}

func cl_op_extern3(data, arg)
{
  return data + arg + 1;
}

func cl_test1
{
  /* check that closure properly unreferences its "data" */
  dat1 = m = "PASSED closure called with no arguments";
  c = closure("cl_op_extern1", dat1);
  dat1 = [1,2,-5];
  c;
  cl_assert,c.data == m, "expected static data";

  /* check that closure properly solve its "function" at runtime */
  c = closure("cl_op1", 42.0);
  f = c.function;
  cl_assert, is_void(f), "expecting a nil result";
  cl_assert, is_closure(c) == 2, "expecting a runtime closure";
  cl_op1 = cl_op_extern1;
  cl_assert, is_func(c.function), "expecting a function";
  cl_assert, c(1) == 43, "unexpected result";
  cl_op1 = cl_op_extern2;
  cl_assert, c(1) == 44, "unexpected result";
  c = closure(cl_op_extern1, 42.0);
  cl_assert, c.function_name == string(0), "bad function_name";
  cl_assert, is_closure(c) == 1, "expecting an immutable closure";
  cl_assert, c.data == 42.0, "bad data extract";

  /* check closure with object argument */
  data2 = -1;
  c = closure(testoxy, data2);
  cl_assert, is_closure(c) == 1, "expecting an immutable closure";
  cl_assert, allof(c() == indgen(5)), "unexpected result";
  cl_assert, allof(c.data == -1), "expected runtime data";
  c = closure("o:testoxy", data2);
  cl_assert, is_closure(c) == 2, "expecting a runtime closure";
  cl_assert, c.data_name == "data2", "bad data name";
  cl_assert, allof(c() == indgen(5)), "unexpected result";
  cl_assert, allof(c.data == -1), "expected runtime data";
  c = closure("testoxy", data2);
  cl_assert, allof(c() == indgen(5)), "unexpected result";

  write, "PASSED all closure tests";
}

/* normally, is_func(x)==5 should be adequate (even that rarely needed) */
func is_closure(x)
{
  if (is_func(x) != 5) return 0n;
  if (x.function_name) return 2n;
  return 1n;
}

if (is_func(closure)==5) closure = closure.data;
cl_test1;

func cl_test2
{
  /* check that closure properly solve its "function" at runtime */
  local cl_op1;
  c = closure(cl_op1, 42.0);
  f = c.function;
  cl_assert, is_void(f), "expecting a nil result";
  cl_assert, is_closure(c) == 2, "expecting a runtime closure";
  cl_op1 = cl_op_extern1;
  cl_assert, is_func(c.function), "expecting a function";
  cl_assert, c(1) == 43, "unexpected result";
  cl_op1 = cl_op_extern2;
  cl_assert, c(1) == 44, "unexpected result";
  c = closure(cl_op_extern1, 42.0);
  cl_assert, c.function_name == "cl_op_extern1", "bad function_name";
  cl_assert, is_closure(c) == 2, "expecting a runtime closure";
  cl_assert, c.data == 42.0, "bad data extract";
  write, "PASSED alternate closure tests";
}

/* create an alternate form of closure for which
 * first argument simple variable reference is treated as
 *   resolve at runtime
 * - may be useful for debugging
 */
c = save(c, closure);
func closure(args)
{
  return args(1)((args(0,2)? args(2) : args(-,2)), args(3));
}
wrap_args, closure;
if (is_func(c(closure))==2) {  /* be sure c(closure) is original builtin */
  closure = c(closure, closure, c(closure));      /* you gotta love this */
  restore, c, c;
 } else { /* ... otherwise abort */
  restore, c;
}

cl_test2;

/* put back built-in closure */
closure = closure.data;

/* ---------------------- test friend and sibling calls */

func test5
{
  use, data1;
  data1++;
}
save, testoxy, test5;

func test4(&x)
{
  use, data1;
  x = data1;
  use_method, test5;
  return data1++;
}

testoxy, data1=12;
x = testoxy(noop(test4), y);
if (y!=12 || x!=13 || testoxy(data1)!=14) {
    write, format="***FAILED oxy %s test\n", "friend";
} else {
  write, "PASSED oxy friend test";
}

save, testoxy, test4;
testoxy, data1=12;
x = testoxy(test4, y);
if (y!=12 || x!=13 || testoxy(data1)!=14) {
    write, format="***FAILED oxy %s test\n", "sibling";
} else {
  write, "PASSED oxy sibling test";
}
