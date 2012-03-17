/*
 * $Id: ydata.h,v 1.9 2010-07-03 19:42:31 dhmunro Exp $
 * Declare structures and functions for Yorick's "private" data.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* use the functions declared in yapi.h to retrieve information
 * for use by new built-in functions - the functions and data
 * structures here are for internal use by the interpreter, and
 * are subject to change as the interpreter evolves
 */

#ifndef YDATA_H
#define YDATA_H

#include "yapi.h"

#include "hash.h"
#include "binio.h"

/* "Universal" data types -- arrays of numbers, text strings, and
   data structures compounded of these -- are declared in binio.h.
   This file contains declarations of Yorick's "private" data types
   -- interpreted functions, built-in functions, index ranges --
   the virtual function tables, Yorick's program stack, and the like.  */

/*--------------------------------------------------------------------------*/

/* The DataBlock and Symbol structures are fundamental to the
   Yorick interpreter.

   A DataBlock is a virtual base class (in C++ parlance) for storing
   information of an unknown data type.  The 2nd edition of K&R blesses
   the programming style I have adopted here in the last paragraph of
   section A8.3; namely, a struct is derived (in the sense of a C++
   derived class) from a DataBlock if its first members match the
   members of the generic DataBlock struct.  In this case, a DataBlock
   consists of a reference counter and an Operations table.  The
   Operations table is a virtual function table defining all of the
   operations whose precise meaning depends on the particular data
   type.  For example, addition of two ints is quite a different thing
   than addition of two doubles, so the Add member of the Operations
   table for an int and the Add member of the Operations table for a
   double will point to different functions.

   A Symbol represents a variable or an intermediate result in an
   expression evaluation; Yorick's global symtab and program stack
   consist of Symbols.  Symbols are distinct from DataBlocks in an
   attempt to speed up simple operations on common data types (that is,
   on ints, longs, and doubles), without drastically increasing the
   number of virtual functions.  Binary operations cause the conflict
   between table size and speed, since for a fast binary function, each
   possible ordered *pair* of operand types requires a separate function.
   Since there are only 4 different Symbol types, the OpTable virtual
   function table is not too unwieldy; with about 16 DataBlock types,
   a comparable Operations table would have to be 10 times as large.
 */

typedef struct DataBlock DataBlock;
struct DataBlock {
  int references;   /* reference counter */
  Operations *ops;  /* virtual function table */
};

typedef struct Symbol Symbol;

/* OpTable is required to define Symbol, and is itself defined below. */
typedef struct OpTable OpTable;     /* operations on a Symbol */

/* A range function takes an Array and an integer indicating which index
   of the array to operate on (0 for fastest varying, 1 for next, etc.).
   The result of the operation is placed on top of the stack, and the
   function returns 0 if the result Array (the result MUST be an Array)
   has the same rank as the input Array, 1 if the result array has
   reduced rank (the input index is missing in the result).  */
typedef int RangeFunc(Array *array, int index);  /* also in yio.h */

typedef void VMaction(void);
typedef union Instruction Instruction;
union Instruction {
  VMaction *Action;      /* do something */
  int count;             /* parameter count */
  long index;            /* index into global symbol table */
  long displace;         /* branch displacement */
  Symbol *constant;      /* pointer into function's constant table */
  RangeFunc *rf;
};

typedef union SymbolValue SymbolValue;
union SymbolValue {
  int i;            /* intScalar */
  long l;           /* longScalar */
  double d;         /* doubleScalar */
  DataBlock *db;    /* dataBlockSym */

  /* A fifth Symbol type, referenceSym, is used ONLY on the stack in
     function parameter lists, i.e.- as an argument to the Eval function
     (see array.c and fnctn.c).  On the stack, a Symbol with ops==0
     is used to mark keyword parameters and function return addresses
     as well (again, see array.c and fnctn.c).  */

  /* The offset and pc are used keywords and returnSym;
     offset is also used in referenceSym (see referenceSym below): */
  Instruction *pc;
  long offset;
};

struct Symbol {
  OpTable *ops;                             /* virtual function table */
  long index;   /* into global symtab (for replacing local variables) */
  SymbolValue value;            /* appropriate member selected by ops */
};

/*--------------------------------------------------------------------------*/

/* The structures derived from DataBlock are the objects the Yorick
   interpreter works with.

   Array, StructDef, and IOStream are DataBlocks defined in
   binio.h.  These are the types necessary for representing "universal"
   data in memory and on disk.

   Function is a list of virtual machine instructions, which results
   from parsing interpreter input.  The source file and line numbers
   corresponding to the function are recorded, although this information
   is only used for error messages.

   BIFunction points to a builtin (i.e.- compiled) function, which
   must have type BuiltIn.  A compiled function may be called from the
   Yorick interpreter by defining a wrapper of type BuiltIn which pulls
   its arguments off of the Yorick program stack, converts them to the
   appropriate types, then calls the compiled function.  The Codger
   code generator can generate simple wrapper functions automatically.
 */

/* LValue represents an object into which an Array may be stored, or
   from which an Array can be fetched.  It has a data type (StructDef),
   dimensions, and an optional list of Strider (see below) specifications
   to indicate how the data is to be extracted as a multidimensional
   subset of a larger object.  An LValue represents data on disk or in
   memory, according to whether its type is a disk or memory StructDef.

    LValues are used for 3 purposes:
    (1) As a temporary describing the result of an indexing operation.
        Further indexing, or other operations not immediately
        requiring data, may then procede without actually fetching
        the data.  This is crucial for extracting parts of complicated
        data structures (especially data in disk files), which
        often requires several member extractions and/or subarray
        specifications to get to the interesting data.
    (2) As an Array with "remote" data -- the result of a reshape
        operation.  This is Yorick's version of FORTRAN equivalence
        statements.
    (3) To point to data not owned by the Yorick interpreter --
        for example, a global data structure from a compiled routine,
        like a FORTRAN common block.
 */
typedef struct LValue LValue;
struct LValue {
  int references;   /* reference counter */
  Operations *ops;  /* virtual function table */
  Array *owner;     /* 0 if data is in disk file or unowned */
  Member type;      /* full data type of result, base x[]..[] */
  union {
    long d;         /* byte address on disk if file!=0 */
    char *m;        /* memory address if file==0 */
  } address;
  Strider *strider; /* hierachy of strides to take */
  /* If non-0, the strider MUST be set up to include a list element
     for any contiguous blocks of type->base.size;
     the Gather and Scatter routines in bcast.c, and the YRead and
     YWrite routines recognize that no loop is required for
     contiguous blocks. */
};

typedef struct Function Function;
struct Function {
  int references;     /* reference counter */
  Operations *ops;    /* pointer to virtual functions */
  Symbol *constantTable;  /* constants used by this function */
  long nConstants;        /* length of constantTable */
  int nReq;               /* worst case number of stack elements required */
  int nPos, nKey, nLocal; /* number of positionals, keywords, and locals */
  long hasPosList;        /* bit 0- 0 unless declared with .. parameter
                             bits 1-30 set if that positional parameter
                             marked as an output */
  int errup;              /* to mark func to enter caller for dbug */
  long isrc;              /* index of source file from RecordSource */
  Instruction code[1];    /* virtual machine instructions begin here */
  /* First 1+nPos+hasPosList+nKey+nLocal instructions are code[i].index
     for function name (in definition), positional parameters,
     optional .. ("*va*") parameter, keyword parameters, and local variables.
     End of code is marked by code[i].Action==&Return, code[i+1].Action==0,
     code[i+2].index==(length of code).  */
};

/* Built-in functions are called with a single argument -- the number
   of actual parameters on the top of the stack (keyword parameters
   count as two).  The function should leave its result on the top of
   the stack.  As many other items may be left on the stack as
   necessary, but if more than the input parameters plus one scratch
   plus the return value are necessary, then the built-in must call
   CheckStack.  (The built-in may clean up the stack itself, as long
   as its result is at or above sp-n when it returns.)  */
typedef void BuiltIn(int);

typedef struct BIFunction BIFunction;
struct BIFunction {
  int references;     /* reference counter */
  Operations *ops;    /* pointer to virtual functions */
  BuiltIn *function;
  long index;         /* to globTab -- shorthand for function name */
};

/* Auto-loaded functions are special interpreted functions which
   trigger parsing of their source file when first used.  */
typedef struct autoload_t autoload_t;
struct autoload_t {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  long ifile;          /* index into table of autoload files */
  long isymbol;        /* global symtab index */
  autoload_t *next;    /* linked list for each ifile */
};

/* TextStream is a "foreign" data block defined in ascio.c */
typedef struct TextStream TextStream;

/*--------------------------------------------------------------------------*/

/* Range     - A range triple min:max:inc or rf:min:max:inc

    Memory management for these objects is via a special block allocator
    for reasons of efficiency.
 */

typedef struct Range Range;
struct Range {
  int references;     /* reference counter */
  Operations *ops;    /* virtual function table */
  long min, max, inc; /* min:max:inc, inc guaranteed non-zero */
  int nilFlags;       /* + 1 if min is nil (:N)
                         + 2 if max is nil (N:)
                             Note: if inc is nil, inc==1
                         + 4 if marked index (+:min:max:inc)
                         + 8 if pseudo index (-:min:max:inc)
                         +16 if rubber index (..)
                         +32 if nullifying index, e.g.-  where(0)  */
#define R_MINNIL 1
#define R_MAXNIL 2
#define R_MARKED 4
#define R_PSEUDO 8
#define R_RUBBER 16
#define R_NULLER 32
  RangeFunc *rf;      /* possible range function,  rf:min:max:inc */
};

/*--------------------------------------------------------------------------*/

/* Operations on DataBlocks take Operand arguments -- this is an
   abbreviated sort of LValue, allowing reshaped Arrays (in LValues) or
   scalar Symbols to interact efficiently with DataBlocks.  The data
   type of an operand may mutate before it is actually used, hence
   an Operand includes the Symbol *owner, which will be updated as
   the Operand changes.  */
typedef struct Operand Operand;
struct Operand {
  Symbol *owner;
  Operations *ops;  /* NEVER &lvalueOps */
  int references;   /* 0 if owner points to temporary Array */
  Member type;      /* all 0 unless ops is an Array type */
  void *value;      /* 0 unless ops is an Array type */
};

typedef void StackOp(void);

struct OpTable {  /* virtual function table for Symbol */
  int id;  /* index into binary operations array (0-3) */
  Operand *(*FormOperand)(Symbol *owner, Operand *op);
  StackOp *ToChar, *ToShort, *ToInt, *ToLong, *ToFloat, *ToDouble, *ToComplex;
  StackOp *Negate, *Complement, *Not, *True;
  StackOp *Add[4], *Subtract[4], *Multiply[4], *Divide[4], *Modulo[4],
          *Power[4];
  StackOp *Equal[4], *NotEqual[4],
          *Greater[4], *Less[4], *GreaterEQ[4], *LessEQ[4];
  StackOp *ShiftL[4], *ShiftR[4];
  StackOp *Or[4], *And[4], *Xor[4];
};

/* Virtual function tables for the 5 Symbol types: */
PLUG_API OpTable intScalar;
PLUG_API OpTable longScalar;
PLUG_API OpTable doubleScalar;
PLUG_API OpTable dataBlockSym;
PLUG_API OpTable referenceSym;   /* referenceSym is not a "complete" Symbol
                                    in the sense that the parser ensures it
                                    never appears in a binary operation
                        ops          - &referenceSym
                        index        - to globTab entry
                        value.offset - stack offset (for Return only)  */
PLUG_API OpTable returnSym;      /* returnSym is not a "complete" Symbol
                                    in the sense that the parser ensures it
                                    never appears in a binary operation
                        ops      - &returnSym
                        index    - (unused)
                        value.pc - VM program counter (for Return only)  */
/* Keywords may also appear on the program stack--
   these are marked by ops==0.  */

/*--------------------------------------------------------------------------*/

/* Unary operators take an Operand*, perform the required operation,
   and replace the Symbol at op->owner with the result.  */
typedef void UnaryOp(Operand *op);

/* Binary operators take two Operand* representing the left and right
   operands, perform the required operation, and replace the Symbol
   at l->owner with the result.  The r->owner may be changed as well,
   if the right operand had to be type converted or broadcast.  */
typedef void BinaryOp(Operand *l, Operand *r);

/* Type promotion operators take two Operand*s, and do
   arithmetic promotion (using one stack element for protected
   scratch space).  Either the left or the right Symbol (but not both)
   is updated, and the operator returns the Operations* for the result
   type, or 0 if the required promotion operation was impossible.
   Legal types for promotion are: char, short, int, long, float, double,
   complex, for either Array or LValue (7 Array types plus LValue).
   Non-numeric types return dl->ops if the dl->ops==dr->ops, else 0.  */
typedef Operations *PromoteOp(Operand *l, Operand *r);

typedef void MemberOp(Operand *op, char *name);

struct Operations {                 /* virtual function table for DataBlock */
  void (*Free)(void *);  /* crucial member for Unref -- first in struct
                            to allow alternate Operations to be used */
  int typeID;     /* unique type ID number */
  int isArray;    /* 1 if this is one of the Array DataBlocks */
  int promoteID;  /* index into Promote array (0-7, 7 means illegal) */
  char *typeName; /* ASCII name describing this data type */
  PromoteOp *Promote[8];
  UnaryOp *ToChar, *ToShort, *ToInt, *ToLong, *ToFloat, *ToDouble, *ToComplex;
  UnaryOp *Negate, *Complement, *Not, *True;
  BinaryOp *Add, *Subtract, *Multiply, *Divide, *Modulo, *Power;
  BinaryOp *Equal, *NotEqual, *Greater, *GreaterEQ;
  BinaryOp *ShiftL, *ShiftR, *Or, *And, *Xor;
  BinaryOp *Assign;    /* WARNING- first parameter non-standard, see ops3.c */
  UnaryOp *Eval;       /* WARNING- parameter non-standard, see ops3.c */
  UnaryOp *Setup;      /* see array.c -- set up for array indexing */
  MemberOp *GetMember; /* WARNING- parameter non-standard, see ops3.c */
  BinaryOp *MatMult;   /* WARNING- non-standard semantics, see ops.c */
  UnaryOp *Print;      /* uses PrintFunc to output each line, see yio.h */
};

/* Virtual function tables for the DataBlock types: */
PLUG_API Operations charOps;
PLUG_API Operations shortOps;
PLUG_API Operations intOps;
PLUG_API Operations longOps;
PLUG_API Operations floatOps;
PLUG_API Operations doubleOps;
PLUG_API Operations complexOps;
PLUG_API Operations stringOps;
PLUG_API Operations pointerOps;
PLUG_API Operations structOps;

PLUG_API Operations rangeOps;
PLUG_API Operations lvalueOps;
PLUG_API Operations voidOps;
PLUG_API Operations functionOps;
PLUG_API Operations builtinOps;
PLUG_API Operations structDefOps;
PLUG_API Operations streamOps;
PLUG_API Operations textOps;
PLUG_API Operations listOps;
PLUG_API Operations auto_ops;

/* generic operators are needed to implement foreign objects */
PLUG_API UnaryOp ComplementX, NegateX, NotX, TrueX, ToAnyX, EvalX, SetupX;
PLUG_API BinaryOp AddX, SubtractX, MultiplyX, DivideX, ModuloX, PowerX;
PLUG_API BinaryOp GreaterX, GreaterEQX, EqualX, NotEqualX, AssignX;
PLUG_API BinaryOp OrX, AndX, XorX, ShiftLX, ShiftRX, MatMultX;
PLUG_API MemberOp GetMemberX;
PLUG_API PromoteOp PromXX;
PLUG_API UnaryOp PrintX;
PLUG_API UnaryOp y_setup_func_hack; /* do not use y_setup_func_hack */

/*--------------------------------------------------------------------------*/

PLUG_API DataBlock nilDB;       /* Nil, or [], the one instance of a void.  */

PLUG_API Instruction *pc;       /* virtual machine program counter */
PLUG_API Symbol *sp;            /* virtual machine stack pointer */
PLUG_API Symbol *spBottom;      /* current bottom of stack */
PLUG_API HashTable globalTable; /* hash table for globTab symbols */
PLUG_API Symbol *globTab;       /* global symbol table, contains any
                                   variable referenced by a Function */

PLUG_API Function *y_idler_function;  /* used by yorick-gl */

/*--------------------------------------------------------------------------*/

/* typeIDs for the basic Array data types */
#define T_CHAR 0
#define T_SHORT 1
#define T_INT 2
#define T_LONG 3
#define T_FLOAT 4
#define T_DOUBLE 5
#define T_COMPLEX 6
#define T_STRING 7
#define T_POINTER 8
#define T_STRUCT 9

/* typeIDs for the non-Array data types */
#define T_RANGE 10
#define T_LVALUE 11
#define T_VOID 12
#define T_FUNCTION 13
#define T_BUILTIN 14
#define T_STRUCTDEF 15
#define T_STREAM 16

/* typeID for data types which are opaque to Yorick */
#define T_OPAQUE 17

/*--------------------------------------------------------------------------*/

PLUG_API LValue *NewLValueD(long address, StructDef *base, Dimension *dims);
PLUG_API LValue *NewLValueM(Array *owner, void *address,
                            StructDef *base, Dimension *dims);
PLUG_API void FreeLValue(void *lvalue);     /* *** Use Unref(lvalue) *** */

PLUG_API Function *NewFunction(Symbol *consts, long nConsts,int nPos,int nKey,
                               int nLocal, long hasPL, int maxStackDepth,
                               Instruction *code, long codeSize);
PLUG_API void FreeFunction(void *func);       /* *** Use Unref(func) *** */

PLUG_API Range *NewRange(long min, long max, long inc, int nilFlags);
PLUG_API void FreeRange(void *range);        /* *** Use Unref(range) *** */

PLUG_API BIFunction *NewBIFunction(BuiltIn *bi, long index);
PLUG_API void FreeBIFunction(void *bif);    /* *** Use Unref(bif) *** */

PLUG_API int yDebugLevel;

/* ------------------------------------------------------------------------ */
/* mixed old-new functions for oxy object extension, wrap_args */
PLUG_API void *yget_obj_s(DataBlock *db);
PLUG_API void yo_cupdate(int iarg);

/*--------------------------------------------------------------------------*/
/* following are deprecated -- use alternatives in yapi.h */

PLUG_API long Globalize(const char *name, long n);
PLUG_API long GlobalizeDB(const char *name, long n, void *db);

/* CheckStack ensures that at least n more elements are available at
   the top of the virtual machine stack.  It returns 1 if the stack had
   to be copied to get more space (so that sp changed), else 0.  */
PLUG_API int CheckStack(int n);
PLUG_API void PushIntValue(int i);
PLUG_API void PushLongValue(long l);
PLUG_API void PushDoubleValue(double d);
PLUG_API int PushCopy(Symbol *s);  /* returns 1 if s is DataBlock, else 0 */
PLUG_API void *PushDataBlock(void *db);  /* returns db */
PLUG_API void Drop(int n);
PLUG_API void PopTo(Symbol *s);

PLUG_API void ReplaceRef(Symbol *stack);
PLUG_API DataBlock *ForceToDB(Symbol *s);

/* Conform sets 4 bit if not conformable, sets 1 bit if ldims==1 where
   rdims>1, sets 2 bit if rdims==1 where ldims>1.  The result dimension
   list is returned in tmpDims.  */
PLUG_API int Conform(Dimension *ldims, Dimension *rdims);
/* BinaryConform assures that the operands are conformable; either or
   both may be broadcast.  Result dimensions are left in tmpDims.
   RightConform is the same except an error is signaled if ldims would
   need to be broadcast.
   Both functions return 0 on success, 1 on failure.   */
PLUG_API int BinaryConform(Operand *l, Operand *r);
PLUG_API int RightConform(Dimension *ldims, Operand *r);

PLUG_API Array *FetchLValue(void *db, Symbol *dsts);
PLUG_API void StoreLValue(void *db, void *data);

/*--------------------------------------------------------------------------*/

PLUG_API void PushTask(Function *task);
PLUG_API void RunTaskNow(Function *task);

PLUG_API int CalledAsSubroutine(void);

/* Extract scalar integers, reals, and strings from the stack */
PLUG_API long YGetInteger(Symbol *s);
PLUG_API double YGetReal(Symbol *s);
PLUG_API char *YGetString(Symbol *s);

PLUG_API int YNotNil(Symbol *s);

/* Scan stack[0],...,stack[nArgs-1] for keywords with names in the
   0-terminated list keyNames, putting the corresponding Symbol*s in
   the array symbols, or 0 if the keyName was not found.  Returns the
   Symbol* of the first non-keyword in the input stack list, or 0 if
   none.  */
PLUG_API Symbol *YGetKeywords(Symbol *stack, int nArgs, char **keyNames,
                              Symbol **symbols);

PLUG_API IOStream *YGetFile(Symbol *stack);

/* Retrieve array arguments for foreign code wrappers,
   applying type conversion (modifies s) if necessary.
   If dims is non-zero, *dims is set to the Dimension * for the argument.
   -- Just cast YGetInteger, YGetReal for scalar arguments, and
      use YGetString for scalar strings.  */
PLUG_API char *YGet_C(Symbol *s, int nilOK, Dimension **dims);
PLUG_API short *YGet_S(Symbol *s, int nilOK, Dimension **dims);
PLUG_API int *YGet_I(Symbol *s, int nilOK, Dimension **dims);
PLUG_API long *YGet_L(Symbol *s, int nilOK, Dimension **dims);
PLUG_API float *YGet_F(Symbol *s, int nilOK, Dimension **dims);
PLUG_API double *YGet_D(Symbol *s, int nilOK, Dimension **dims);
PLUG_API double *YGet_Z(Symbol *s, int nilOK, Dimension **dims);
PLUG_API char **YGet_Q(Symbol *s, int nilOK, Dimension **dims);
PLUG_API void **YGet_P(Symbol *s, int nilOK, Dimension **dims);
/* Convenience routine YGet_dims converts a Dimension * linked list
   to a dimension list dlist --
   dlist[0] is the first dimension length, dlist[1] the second, and so on.
   The return value is the actual number of dimensions.
   No dimensions beyond dlist[maxDims-1] will be set, so dlist need have
   at most maxDims elements.
   Use the TotalNumber and CountDims functions to get just the length
   or number of dimensions of an array.  */
PLUG_API int YGet_dims(const Dimension *dims, long *dlist, int maxDims);
/* If a compiled function is to treat a parameter as an update or an
   output, call YPut_Result after you call the function.  The required
   index is obtained from YGet_Ref, which must be called BEFORE YGet_...
   gets the corresponding data:
      long index= YGet_Ref(sp);
      double *x= YGet_D(sp, 0, (Dimension **)0);
      your_function(sp);
      YPut_Result(sp, index);                */
PLUG_API long YGet_Ref(Symbol *s);
PLUG_API void YPut_Result(Symbol *s, long index);

/* generic temporary object has only a free method (zapper)
 * its data structure must begin with
 *   int references;
 *   Operations *ops;
 *   void (*zapper)(void *to);
 * pass sizeof(its structure) to y_new_tmpobj and it will
 *   be allocated and the first three elements initialized
 * you can then use PushDataBlock to put it on the stack
 *   where Drop will call your zapper
 */
PLUG_API void *y_new_tmpobj(unsigned long size, void (*zapper)(void *to));
PLUG_API Operations yo_tmpobj;

/* new improved (1.6) argument retrieval functions (no sp reference)
 * all return 0 for iarg (=number of spots below top of stack) < 0
 * need to check yarg_nil separately if required
 */
/* note that yarg_c, yarg_s, and yarg_f are nowhere referenced in yorick */
PLUG_API char *yarg_c(int iarg, Dimension **dims);
PLUG_API short *yarg_s(int iarg, Dimension **dims);
PLUG_API int *yarg_i(int iarg, Dimension **dims);
PLUG_API long *yarg_l(int iarg, Dimension **dims);
PLUG_API float *yarg_f(int iarg, Dimension **dims);
PLUG_API double *yarg_d(int iarg, Dimension **dims);
PLUG_API double *yarg_z(int iarg, Dimension **dims);
PLUG_API char **yarg_q(int iarg, Dimension **dims);
PLUG_API void **yarg_p(int iarg, Dimension **dims);
#define yarg_sc(iarg) ((char)yarg_sl(iarg))
#define yarg_ss(iarg) ((short)yarg_sl(iarg))
#define yarg_si(iarg) ((int)yarg_sl(iarg))
PLUG_API long yarg_sl(int iarg);
#define yarg_sf(iarg) ((float)yarg_sd(iarg))
PLUG_API double yarg_sd(int iarg);
PLUG_API char *yarg_sq(int iarg);
#define yarg_sp(iarg) (yarg_p(iarg,0)[0])
PLUG_API IOStream *yarg_file(int iarg);
PLUG_API Operand *yarg_op(int iarg, Operand *op);
/* yarg_keys returns next non-keyword iarg, or <0 if none */
PLUG_API int yarg_keys(int iarg, char **knames, Symbol **ksymbols);

/*--------------------------------------------------------------------------*/

typedef char *y_pkg_t(char ***ifiles, BuiltIn ***code, void ***data,
                      char ***varname);
PLUG_API void y_pkg_add(y_pkg_t *init);
PLUG_API y_pkg_t *y_pkg_lookup(char *name);
/* use name==0 to link or include all packages (static and dynamic) */
PLUG_API void y_pkg_link(char *name);
PLUG_API void y_pkg_include(char *name, int now);
PLUG_API char *y_pkg_name(int i);
PLUG_API int y_pkg_count(int i);

/* called by on_launch generated by codger in yinit.c */
PLUG_API int y_launch(int argc, char *argv[],
                      char *home, char *site, y_pkg_t **pkgs);

PLUG_API int ym_argc;
PLUG_API char **ym_argv;

#ifdef Y_FOR_YORAPI
PLUG_API y_pkg_t yk_yor;
#endif

/*--------------------------------------------------------------------------*/

#endif
