/*
 * $Id: yapi.h,v 1.20 2010-07-04 23:07:05 dhmunro Exp $
 * API for interfacing yorick packages to the interpreter
 *  - yorick package source should not need to include anything
 *    not here or in the play headers
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef YAPI_H
#define YAPI_H

#include "plugin.h"

typedef char *ystring_t;
typedef void *ypointer_t;
typedef void ybuiltin_t(int argc);

BEGIN_EXTERN_C
/*
The interpreter can only call compiled functions with this prototype:
    void my_builtin(int argc);
You can check whether my_builtin was invoked as a subroutine (so that
its return value is discarded) or a function (so that its return value
is used) with
*/
PLUG_API int yarg_subroutine(void);
/*
The argc arguments are on the interpreter stack.  When you return
from my_builtin, the top of the stack becomes the return value of your
function.  This API refers to elements on the stack by an index iarg;
iarg=0 means the top of the stack, iarg=1 is the second stack element,
iarg=2 is the third, and so on down to iarg=argc-1.  (The my_builtin
function itself is normally at iarg=argc when the interpreter calls
the function.)  On entry to my_builtin, its arguments are in reverse
order on the stack (in other words, the interpreter has pushed them
onto the stack in order), so that iarg=argc-1 is the first argument,
and iarg=0 is the last argument.  However, if your function accepts
keywords, the keywords will be randomly interspersed with the
positional arguments, and you must use the yarg_kw function in order
to parse the argument list.
*/
PLUG_API void yarg_kw_init(char **knames, long *kglobs, int *kiargs);
PLUG_API int yarg_kw(int iarg, long *kglobs, int *kiargs);
/*
To use yarg_kw, declare knames and kglobs as static arrays:
  static char *knames[NKEYS+1] = { "keyname1", "keyname2", ..., 0 };
  static long kglobs[NKEYS+1];
The kiargs array is an automatic (local) array
  int kiargs[NKEYS];
You must call yarg_kw_init before the first call to yarg_kw.  If
kglobs[0]=0 (as it will be with the above static definition), then
yarg_kw_init initializes it to [NKEYS, index1, index2, ...], where
indexN is the global symbol table index corresponding to the variable
named "keynameN".  If kglobs[0] != 0, yarg_kw_init ignores knames.
The yarg_kw_init function always initializes all kiargs[i] to -1.
Beginning with iarg=argc-1, you call
  iarg = yarg_kw(iarg, kglobs, kiargs);
If the return iarg>=0, it represents the first positional argument;
process it, then decrement iarg and call yarg_kw again, repeating this
process until iarg<0.  At this point you have processed all the
positional arguments, and yarg_kw has stored the iarg value for any
keyword arguments in the kiargs array.  That is, kiargs[i] will be the
iarg corresponding to keyword knames[i], or -1 if that keyword was not
present.

An alternative more primitive function yarg_key simply returns the
index of a keyword in the global symbol table, or -1 if iarg is
positional.  If the return value is not -1, then iarg-1 is the
corresponding key value, and you can continue parsing the argument
list with iarg-2.
*/
PLUG_API long yarg_key(int iarg);
/*
Several other functions are supplied to test stack elements.  The nil
test is the same as the interpreted is_void, rank returns -1 if not an
array, otherwise its dimensionality, number returns 0 if not a number,
1 if integer, 2 if floating point, or 3 if complex.  The string test
returns 0 if iarg is not a string, 1 if it is a scalar string, 2 if it
is a string array.  The func test is the same as is_func.  The typeid
returns the typeid as listed above, or some other value for internal
objects.  The scratch test returns 1 if iarg is an array which will be
destroyed when this stack element is dropped (such an array can be
reused as a return value).  The list test is the same as is_list.
The yarg_true function returns 1 whenever if(x) would be true.
*/
PLUG_API int yarg_nil(int iarg);
PLUG_API int yarg_rank(int iarg);
PLUG_API int yarg_number(int iarg);
PLUG_API int yarg_string(int iarg);
PLUG_API int yarg_func(int iarg);
PLUG_API int yarg_typeid(int iarg);
PLUG_API int yarg_scratch(int iarg);
PLUG_API int yarg_list(int iarg);
PLUG_API int yarg_true(int iarg);
/*
There are ten possible array data types, internally identified by the
typeid value 0-9.  The char, short, int, long, float, complex, string,
and pointer types are primitive; struct types are built up from
combinations of the primitive types and other structs.  A typeid
greater than 9 represents a non-array internal data type, which in
general is beyond the scope of this API for yorick packages.
*/
#define Y_CHAR 0
#define Y_SHORT 1
#define Y_INT 2
#define Y_LONG 3
#define Y_FLOAT 4
#define Y_DOUBLE 5
#define Y_COMPLEX 6
#define Y_STRING 7
#define Y_POINTER 8
#define Y_STRUCT 9
/*
The typeid of six other interpreted objects may be useful.  Note that
a Y_STREAM is a binary file, not a text file.
*/
#define Y_RANGE 10
#define Y_VOID 12
#define Y_FUNCTION 13
#define Y_BUILTIN 14
#define Y_STRUCTDEF 15
#define Y_STREAM 16
#define Y_OPAQUE 17
/*
Arrays can have from 0 (scalar) to Y_DIMSIZE-1 dimensions, except for
complex and struct arrays, which are limited to Y_DIMSIZE-2
dimensions.
*/
#define Y_DIMSIZE 11
/*
Dimension lists in this API have the same format as the interpreted
dimsof function, that is, [rank, len1, len2, ...], where rank is the
number of dimensions (zero for a scalar), and lenN is the length of
the Nth dimension, with len1 incrementing fastest in memory, len2 next
fastest, and so on.  Functions returning dims expect it to be
declared like this:
    long dims[Y_DIMSIZE];
Functions which accept dims as input, on the other hand, can accept
a dims array shorter than Y_DIMSIZE, if their rank is lower than the
maximum.

Next are a set of routines for retrieving usable data from the stack.
The ygets functions return scalar values, while ygeta functions return
array values.  The ygeta functions have two output arguments: If ntot
is non-zero, *ntot is set to the total number of array elements, and
if dims is non-zero, dims[0] is set to rank and dims[1:rank] are set
to the dimension lengths.  As noted above, dims should be dimensioned
using Y_DIMSIZE (although by checking yarg_rank first, you could
safely use a smaller dims).  Except as noted, all the yget functions
perform type conversions as necessary to force the argument to the
requested data type.  The sole exceptions are ygets_l and ygets_d,
which will not demote the type, although they will promote it.  In all
cases, if iarg cannot be convrted to the requested type, or if it is
not a scalar in a ygets, YError will be called and the function will
never return.  If you care, you need to use the query functions first.
*/
PLUG_API long ygets_l(int iarg);
PLUG_API double ygets_d(int iarg);
PLUG_API ystring_t ygets_q(int iarg);
PLUG_API ypointer_t ygets_p(int iarg);
#define ygets_c(iarg) ((char)ygets_l(iarg))
#define ygets_uc(iarg) ((unsigned char)ygets_l(iarg))
#define ygets_s(iarg) ((short)ygets_l(iarg))
#define ygets_i(iarg) ((int)ygets_l(iarg))
#define ygets_f(iarg) ((float)ygets_d(iarg))

PLUG_API char *ygeta_c(int iarg, long *ntot, long *dims);
#define ygeta_uc (unsigned char *)ygeta_c
PLUG_API short *ygeta_s(int iarg, long *ntot, long *dims);
PLUG_API int *ygeta_i(int iarg, long *ntot, long *dims);
PLUG_API long *ygeta_l(int iarg, long *ntot, long *dims);
PLUG_API float *ygeta_f(int iarg, long *ntot, long *dims);
PLUG_API double *ygeta_d(int iarg, long *ntot, long *dims);
PLUG_API double *ygeta_z(int iarg, long *ntot, long *dims);
PLUG_API ystring_t *ygeta_q(int iarg, long *ntot, long *dims);
PLUG_API ypointer_t *ygeta_p(int iarg, long *ntot, long *dims);
/*
Two ygeta functions do not return a specific data type: ygeta_dz
returns either a double (possibly promoting its argument) or a
complex, and yget_any returns any array (never converting its type).
*/
PLUG_API double *ygeta_dz(int iarg, long *ntot, long *dims, int *is_z);
PLUG_API void *ygeta_any(int iarg, long *ntot, long *dims, int *the_typeid);
/*
The ygeta_coerce function allows you to convert a numeric array to any
other numeric data type, after you have retrieved it using ygeta_any.
The arguments except for new_typeid must be returned by a previous
call to ygeta_any.
The yarg_reform function permits you to change the dimensionality
of an array without reallocating it; the new dims must have the
same total element count as the old dims.
*/
PLUG_API void *ygeta_coerce(int iarg, void *old_data, long ntot, long *dims,
                            int old_typeid, int new_typeid);
PLUG_API int yarg_reform(int iarg, long *dims);
/*
Finally, the yget_range function returns the mn:mx:step for an index
range, its return value is the union of a set of flags (or 0 if iarg
is not an index range).  MMMARK is the + matrix multiply marker, which
is almost certainly a syntax error in any other context, PSEUDO is the
- pseudo-range index, RUBBER is the .. index, RUBBER1 is the * index,
and NULLER is the result of where(0).  The Y_MIN_DFLT and Y_MAX_DFLT
are or-ed on if the minimum or maximum is defaulted; there is no flag
for a default step, so there is no way to tell the difference between
x(:) and x(::1).
*/
PLUG_API int yget_range(int iarg, long min_max_step[3]);
#define Y_MMMARK 2
#define Y_PSEUDO 3
#define Y_RUBBER 4
#define Y_RUBBER1 5
#define Y_NULLER 6
#define Y_MIN_DFLT 16
#define Y_MAX_DFLT 32
/*
There is also a family of ypush functions which create and push new
items onto the top of the stack.  Note that this changes the numbering
of the iarg; the new array will always be at iarg=0, and everything
else will have one larger iarg than before.  The interpreter always
ensures that the stack has space for at least 8 new elements before
calling any compiled builtin function.  If you need more than 8 stack
elements, you must call ypush_check to prevent stack overflow; its
argument is the number of new elements you plan to create.
*/
PLUG_API void ypush_check(int n);
/*
In order to push a nil [] or one of the three high-performance scalar
types, just call one of the four routines with the value you want to
push.
*/
PLUG_API void ypush_nil(void);

PLUG_API void ypush_int(int value);
PLUG_API void ypush_long(long value);
PLUG_API void ypush_double(double value);
/*
In order to push an array onto the stack, use one of these functions;
dims=0 is acceptable for pushing a scalar value.  The arrays are
always initialized to zero; you fill in the values after the ypush in
this case.
*/
PLUG_API char *ypush_c(long *dims);
#define ypush_uc (unsigned char *)ypush_c
PLUG_API short *ypush_s(long *dims);
PLUG_API int *ypush_i(long *dims);
PLUG_API long *ypush_l(long *dims);
PLUG_API float *ypush_f(long *dims);
PLUG_API double *ypush_d(long *dims);
PLUG_API double *ypush_z(long *dims);
PLUG_API ystring_t *ypush_q(long *dims);
PLUG_API ypointer_t *ypush_p(long *dims);

PLUG_API void ypush_range(long min_max_step[3], int flags);
/*
The ypush_ptr function allows you to push the pointee (object pointed to)
corresponding to a ypointer_t pointer onto the stack, where you can use
the yarg_* or ygeta_any functions to query its data type and dimensions.
The return value is the typeid and the number of elements is also returned,
in anticipation of at least those simple queries.
*/
PLUG_API int ypush_ptr(ypointer_t ptr, long *number);
/*
You can also create scratch space on the stack.  If your function
faults (or run long enough that the user types C-c to abort it),
anything you allocate that is not on the interpreted stack will be
lost, causing a memory leak.  The ypush_scratch function is a
protected malloc; you can optionally provide an on_free which will be
invoked to do any cleanup; the on_free function must not attempt to
free the object itself - that will happen automatically after on_free.
*/
PLUG_API void *ypush_scratch(unsigned long size, void (*on_free)(void *));
/*
Two simple stack manipulation functions, yarg_drop discarding the top
n stack elements, and yarg_swap for interchanging stack elements
(generally to set up for yarg_drop).  As long as you make sure your
return value is at the top of the stack when you return from
my_builtin, you probably will not need these.
*/
PLUG_API void yarg_drop(int n);
PLUG_API void yarg_swap(int iarg1, int iarg2);
/*
The yarg_dims function retrieves argument dimensions, optionally
checking for conformability with a cfmdims model.  It returns -1
on failure (either because iarg is not an array or because cfmdims
was specified and it is not conformable), 0 on success if cfmdims=0,
and the same return value as yarg_conform(dims, cfmdims, cfmdims)
if cfmdims was specified.  Note that cfmdims is both an input and
an output to yarg_dims (unlike yarg_conform, where it is an output).

 The yarg_conform routine checks array conformability.  The return
argument of yarg_conform is the cfmdims dimension list of the result
of a binary operation on arrays of dims1 and dims2- its return value
is -1 if dims1 and dims2 are not conformable, or the union of zero to
three of the flags Y_1_BCAST, Y_2_BCAST, Y_1_EXTEND, or Y_2_EXTEND.
If the Y_1_BCAST flags is set, it means that one or more of the dims1
dimensions has length 1 and the corresponding dims2 dimension has
length>1.  The Y_1_EXTEND flag means that the rank of dims1 is lower
than the rank of dims2; ordinarily, this also means that the Y_1_BCAST
flag will be set, but if all the trailing dimensions of dims2 have
length 1, Y_1_EXTEND could be set while Y_1_BCAST was not.  The
cfmdims array can be the same as dims1 or dims2, in which case the
original dimension list is overwritten.  If cfmdims=0, the check is
performed without returning the result dimension list.  In the case

The yarg_bcast routine broadcasts an array to a larger size. The
return value of yarg_bcast is 0 if the broadcast is possible, non-zero
on a conformability error.  (The case that dims1 or dims2 requires no
broadcast, but has lower rank than the result is a no-op.)
*/
PLUG_API int yarg_dims(int iarg, long *dims, long *cfmdims);
PLUG_API int yarg_conform(long *dims1, long *dims2, long *cfmdims);
#define Y_1_BCAST 1
#define Y_1_EXTEND 2
#define Y_2_BCAST 4
#define Y_2_EXTEND 8
#define Y_12_NOT 16
PLUG_API int yarg_bcast(int iarg, long *newdims);
/*
Interpreted variables are generally either on the stack, or in the
global symbol table (even variables local to an interpreted function
are in the global symbol table when the function is active - the
corresponding external variables are pushed onto the stack until the
function returns).  Global symbols are identified by their index in
the symbol table, which never changes and corresponds to a particular
name.  The yfind_name function returns the variable name corresponding
to an index, while yfind_global returns the index corresponding to a
given name (len=0 means the name is 0-terminated), or -1 if there is
no such variable.  The yget_global function is like yfind_global,
except it creates the variable (initializing its value to nil []) if
it does not exist.  The ypush_global function pushes a global variable
onto the stack.  The yput_global function sets a global variable from
the stack, discarding any previous value.
*/
PLUG_API char *yfind_name(long index);
PLUG_API long yfind_global(const char *name, long len);
PLUG_API long yget_global(const char *name, long len);
PLUG_API int ypush_global(long index);
PLUG_API void yput_global(long index, int iarg);
/*
You may need to use some of your arguments as output variables, like
interpreted function parameters declared as &x.  To do this, you need
to call yget_ref on the corresponding iarg before using any other
function to retrieve the associated data.  If iarg was a simple
variable reference, then yget_ref returns its global symbol table
reference; if iarg was a temporary expression, yget_ref returns -1.
Use this index in yput_global to set the output value before you
return.
*/
PLUG_API long yget_ref(int iarg);
/*
Six API functions manipulate lists:

ypush_list replaces the n elements at the top of the stack by a single
  stack element which is a list containing them, like the _lst
  interpreted function.  If n<0, -n elements are replaced but like
  the _cat interpreted function, namely, any lists are concatenated
  rather than becoming single list elements.  Finally, if n==0, ypush_list
  is the same as ypush_nil.
yarg_nlist returns the length of the list iarg.
ypush_car pushes the n-th car of iarg onto the top of the stack.
yput_car sets the n-th car of iarg to jarg.
ypush_cdr pushes the n-th cdr of iarg onto the top of the stack, if n>=0.
  If n==-1, ypush_cdr pushes a copy of the whole list, like _cpy.
yput_cdr sets the n-th cdr of iarg to jarg.

If iarg is not a list or nil, yarg_list returns -1, and ypush_car, yput_car,
ypush_cdr, and yput_cdr return non-zero.  The yput_cdr function also returns
non-zero if jarg is not a list or nil.
*/
PLUG_API void ypush_list(int n);
PLUG_API long yarg_nlist(int iarg);
PLUG_API int ypush_car(int iarg, long n);
PLUG_API int yput_car(int iarg, long n, int jarg);
PLUG_API int ypush_cdr(int iarg, long n);
PLUG_API int yput_cdr(int iarg, long n, int jarg);
/*
Many packages need to create persistent objects to store state
information for future calls.  In order to do that, you must create an
object of a new data type.  The interpreter recognizes your objects
enough to be able to store them in interpreted variables, delete them,
and pass them back to you as arguments to future calls.  You can also
supply four optional operations: on_free, on_print, on_eval, and
on_extract.
*/
typedef struct y_userobj_t y_userobj_t;
struct y_userobj_t {
  char *type_name;
  void (*on_free)(void *);
  void (*on_print)(void *);
  void (*on_eval)(void *, int);
  void (*on_extract)(void *, char *);
  void *uo_ops;
};
PLUG_API void *ypush_obj(y_userobj_t *uo_type, unsigned long size);
PLUG_API y_userobj_t *yfunc_obj(y_userobj_t *uo_type);
PLUG_API void *yget_obj(int iarg, y_userobj_t *uo_type);
PLUG_API void y_print(const char *text, int newline);
/*
You should statically initialize the uo_type structure (with
uo_ops=0).  For function-like objects, the uo_ops member must then be
initialized in a special way by calling yfunc_obj; otherwise, the
uo_ops member will be initialized on the first call to ypush_obj.
Before applying yfunc_obj or before the first call to ypush_obj, make
sure uo_ops=0, and do not touch it afterwards.  The virtual functions
for your object can be 0 to get default behavior.  Like ypush_scratch,
the on_free, if present, should not attempt to free the object itself,
just its contents.  If no special action is required to free your
object, use on_free=0.  If on_print=0, printing the object will print
a generic phrase including its type_name.  The on_eval is invoked like
a builtin function:
  object(arg1, arg2, ...)
in the interpreted code will invoke
  on_eval(object, argc)
with the argc arguments on the stack; you should leave the result on
the top of the stack, and treat this like any other my_builtin.  The
member on_extract is called as a result of interpreted code
  object.member_name
producing the compiled call
  on_extract(object, member_name)
Again, you should leave the result on the top of the stack.  If
on_eval or on_extract is zero, that operation will cause a runtime
error (the default behavior).  Note that you can use the yarg_kw_init
function (with kiargs=0) to retrieve name indices.

An object created by ypush_obj can be retrieved by yget_obj.  Passing
uo_type=0 to yget_obj returns the type_name for the object at iarg;
otherwise it returns the pointer to the object itself, as created by
ypush_obj.  For example,
y_userobj_t examp1_ops = { "example1 user object",
  &examp1_free, &examp1_print, &examp1_eval, &examp1_extract, 0 };
y_userobj_t examp2_ops = { "example2 user object",
  &examp2_free, &examp2_print, &examp2_eval, &examp2_extract, 0 };
A function which expects an example1 object at iarg=0 would simply
call yget_obj(iarg, &examp1_ops).  However, if either an example1 or
an example2 object were an acceptable iarg=0 argument, you would call
name=yget_obj(iarg,0), then check whether name==examp1_ops.type_name or
name==examp2_ops.type_name.  Note that the string itself is irrelevant --
checking the string value is not only a waste of time, but also there is no
guarantee the value is unique.  Instead, you want to check that the
address examp1_ops.type_name (or examp2_ops.type_name) is returned.  Note
that you can easily write your own yarg_examp1(iarg) or yarg_examp2(iarg)
function that retrieves specific object types with whatever semantics
you like if the actual argument is not the correct type.  Similarly, you
can write ypush_examp1 or ypush_examp2, with whatever constructor arguments
make sense.

The y_print function must be used by the on_print callback to produce
output.  Do not attempt to use y_print except in a on_print callback.
Set newline to 1 to force a newline after text; your message will
always start on a newline, and a newline will be forced after the
final text you emit (whether or not your final call to y_print sets
newline).

Yorick memory management works by reference counting.  Your compiled
code may need to own a use of a yorick object, for example, when one
user object type contains a pointer to another and both are visible
to interpreted code.  The yget_use function obtains a private use of
an object on the stack.  You can obtain a use of a yorick user defined
persistent object or array (but not a double, long, or int scalar)
by calling yget_use.  The returned handle is opaque -- you must save it
and call ypush_use to push the object back onto the stack and give up
your use.  After calling ypush_use, you must zero all your copies of
the opaque handle.  You can use the ygeta_* or yget_obj functions to
get the pointers to the actual data.  As a side effect, yget_use converts
a scalar double, long, or int stack element into a rank 0 array, since
you cannot own a use of the scalars.  This invalidates any existing pointer
you may have retrieved using ygeta_*, so call yget_use first.  The
ykeep_use function is like ypush_use, except you do not give up your
use of the object; ydrop_use discards your use (like ypush_use) without
pushing the object onto the stack.
*/
PLUG_API void *yget_use(int iarg);
PLUG_API void ypush_use(void *handle);
PLUG_API void ykeep_use(void *handle);
PLUG_API void ydrop_use(void *handle);
/*
Your compiled package may need to execute code after the interpreter
shuts down on a quit operation or after a fatal error.  This function
cannot use the interpreted stack or variables, so you must take care
to store any required information as compiled variables.  An on_quit
function must execute in a short time and be bullet-proof -- no error
recovery is possible, and yorick may be in the process of exiting after
a fatal error.  The on_quit functions are executed in reverse order
to the ycall_on_quit calls.  Use ycancel_on_quit to remove an on_quit
function.  Multiple calls to ycall_on_quit do no harm, but on_quit
will only be called once.
*/
PLUG_API void ycall_on_quit(void (*on_quit)(void));
PLUG_API void ycancel_on_quit(void (*on_quit)(void));
/*
The ypush_func function is the compiled version of the interpreted
funcdef function; it pushes the resulting anonymous function onto the
top of the stack.  If the funcdef string cannot be parsed, ypush_func
returns non-zero and pushes nil [] onto the stack.
*/
PLUG_API int ypush_func(char *funcdef);
/*
The y_error function emits a standard error message, and does not
return to the caller.  This is intended as a replacement for the
original YError API.  The y_errorn and y_errorq variants handle the
frequent cases for which the message is a format string including a
%ld or %s descriptor.  The y_errquiet variant emits no error message;
use it to transfer control to a catch or after_error handler.
*/
PLUG_API void y_error(const char *msg);
PLUG_API void y_errorn(const char *msg_format, long n);
PLUG_API void y_errorq(const char *msg_format, const char *q);
PLUG_API void y_errquiet(void);
PLUG_API void y_warn(const char *msg);
PLUG_API void y_warnn(const char *msg_format, long n);
PLUG_API void y_warnq(const char *msg_format, const char *q);

/* hook function called inside y_error
 * return value bits:
 *   1  - suppress printing error message or entering dbug mode
 *        (one time override of set_idler flag)
 *   2  - use output after as index into global symbol table of
 *        alternate after_error function
 *   4  - if bit 2 set, after_error left in dbug mode iff bit 4 set
 */
PLUG_API int (*y_errhook)(const char *fullmsg, long *after);

/* ------------------------------------------------------------------------ */
/* oxy: object-oriented extension to yorick */

/* here are the methods all oxy_objects share
 *
 * arguments and return values:
 *   obj    - the oxy_object
 *   nmemb  - number of object members
 *   mndx   - object member index (1-origin, 1 <= mndx <= nmemb)
 *   name   - object member name
 *   iname  - index into globtab corresponding to name (yfind_global)
 *            iname=-1 on input or output means unknown correspondence
 *     there are two ways to indicate an object member:
 *       by name (or iname)
 *       by 1-origin index (mndx) into the object
 *     multiple anonymous members permitted, but otherwise name is unique
 *     objects not required to assign a fixed mndx to each member,
 *       but if they do not, no anonymous members permitted
 *   iarg   - stack position (for arguments on stack, 0 = top, 1 = next, etc)
 *   flag   - 0 means success, non-0 various sorts of failure
 */
typedef struct yo_ops_t yo_ops_t;
struct yo_ops_t {
  char *type_name;
  void (*dealloc)(void *obj);    /* deallocate obj */
  long (*count)(void *obj);      /* return number of members */
  long (*find_mndx)(void *obj, const char *name, long iname);
  /*      iname=-1 if correspondence with globtab unknown
   *      find_mndx==0 permitted, means no fixed member indices
   */
  char *(*find_name)(void *obj, long mndx, long *iname);
  /*      returns iname=-1 if correspondence with globtab unknown
   *      return value owned by obj, caller must copy string
   *        next call to find_name may invalidate return value
   *      if find_mndx==0, this will only be called in a sequence
   *        from mndx=1 to mndx=count(obj) to list all names
   *        get_q or set_q may be called during listing sequence,
   *        but only with name just returned
   */
  int (*get_i)(void *obj, long mndx);
  int (*get_q)(void *obj, const char *name, long iname);
  /*      push member onto stack
   *      get_i unused (0 permitted) when find_mndx==0
   *      pass iname=-1 if correspondence with globtab unknown
   *      returns 0 on success, otherwise push nil and return:
   *        1 if no such member
   */
  int (*set_i)(void *obj, long mndx, int iarg);
  int (*set_q)(void *obj, const char *name, long iname, int iarg);
  /*      set member to value at iarg on stack
   *      set_i unused (0 permitted) when find_mndx==0
   *      both set_i==0, set_q==0 permitted if changing values unsupported
   *      pass iname=-1 if correspondence with globtab unknown
   *      returns 0 on success, otherwise return:
   *        1 if no such member and creating member not allowed
   *        2 if member is read-only
   *        3 if type or shape of iarg cannot be converted to member type
   *        4 if type not supported by this object
   */
  void (*get_atts)(void *obj);
  /*      push (possibly empty) attribute oxy_object
   *        may push nil to indicate no attributes present or writable
   *      get_atts==0 permitted if attributes unsupported
   *      attribute oxy_object may be read-only or read-write
   */
  void (*print)(void *obj);    /* print obj, print==0 uses default method */
  void (*sr_hook)(void *obj, int flags);
  /* called at end of save/restore, flags&1 == 1 if restore
   * required for back compatibilty with IOStream, which flushes to file
   * at end of save/restore operation
   */

  /* more optional methods may be added to support LValue oxy_objects */
};

/* yo_push_alloc makes obj part of data block, initialize in place
 * yo_push makes data block with pointer to obj (returning obj)
 *   yo_ops_t method functions obviously know how to distinguish
 *     - in particular, dealloc erases obj contents for yo_push_alloc,
 *       but must also free obj pointer for yo_push
 * yo_get returns obj and ops, making it possible to call method functions
 *   - returns 0 if iarg (on stack) not an oxy_object
 *   - for legacy support, returns oxy wrapper if iarg is IOStream
 *
 * note that you can use yget_use, ypush_use to pluck your own use of
 * an object off the stack, then give it back
 */
PLUG_API void *yo_push_alloc(yo_ops_t *ops, unsigned long size);
PLUG_API void *yo_push(yo_ops_t *ops, void *obj);
PLUG_API void *yo_get(int iarg, yo_ops_t **ops);

/* yo_use is the compiled equivalent of the interpreted use function
 * you should call it before reading or writing (with ypush_global
 * or yput_global) in order to be sure interpreted functions actually
 * pick up changes
 * - return value is 0 on success, 1 if index (globtab index) is not in
 *   globtab, 2 if index is not in the context object, and 3 if there is
 *   no context
 */
PLUG_API int yo_use(long index);
/* yo_get_context returns the context object for the current function
 * iarg>=0 means get the context object for the builtin function at iarg
 *           normally called with iarg=argc, the argument to the builtin
 * iarg<0  means return the context for the current interpreted func,
 *           which was called as an object method
 * allows compiled functions to examine and set context objects,
 * like interpreted functions can do with use function
 *
 * non-0 push flag pushes context (or nil) onto stack as side effect
 */
PLUG_API void *yo_get_context(int iarg, yo_ops_t **ops, int push);

/* yo_new_group pushes empty group object onto stack, returning obj,ops */
PLUG_API void *yo_new_group(yo_ops_t **ops);

/* ------------------------------------------------------------------------ */
/* closure, invented by Eric Thiebaut, see closure doc in i0/std.i */

/* create a closure on top of stack from function at farg, data at darg
 * returns 0 on success, 1 if farg is not a function or darg is not data
 */
PLUG_API int yo_closure(int farg, int darg);
PLUG_API int yo_is_closure(int iarg);

/* ------------------------------------------------------------------------ */

/* compiled interface to interpreted after function
 * secs = delay time until fndx/farg executes
 *      = 0.0 to execute on idle
 *      < 0.0 to remove existing after function from queue (after,-)
 * fndx = index into global symbol table for function or object
 *      = -1 to use farg instead of fndx (recommended)
 * farg = iarg stack position of function to call (unused if fndx>=0)
 * dndx = index into global symbol table for data (recommended for object)
 *      = -1 to use darg instead of dndx (recommended for function)
 *      = -2 to pass no argument to function
 * darg = iarg stack position of data for farg/fndx (unused if dndx!=-1)
 */
PLUG_API void yexec_after(double secs, long fndx, int farg, long dndx, int darg);
/* compiled interface to interpreted include function */
PLUG_API void yexec_include(int iarg, int now);
/* push interpreted function onto task stack as *main* */
PLUG_API void ytask_push(int iarg);
/* run interpreted function as *main* immediately, for event callbacks */
PLUG_API void ytask_run(int iarg);

/* ------------------------------------------------------------------------ */

END_EXTERN_C

#endif
