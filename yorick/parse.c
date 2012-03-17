/*
 * $Id: parse.c,v 1.2 2010-12-22 21:30:24 dhmunro Exp $
 *
 * Define functions required to parse Yorick grammar.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "parse.h"

#include "ydata.h"
#include "pstdlib.h"
#include <string.h>

extern long RecordSource(long index);  /* in yio.h */


extern long YpLineNumber(void);
extern int YpReCompare(Function *func,
                       Symbol *consts, long nConsts, int nPos, int nKey,
                       int nLocal, long hasPL, int maxStackDepth,
                       Instruction *code, long codeSize);

/* range functions */
extern RangeFunc RFmin, RFmax, RFptp, RFsum, RFavg, RFrms, RFmnx, RFmxx,
  RFpsum, RFdif, RFzcen, RFpcen, RFuncp, RFcum;

/* virtual machine instructions */
extern VMaction PushChar, PushShort, PushInt, PushLong,
  PushFloat, PushDouble, PushImaginary, PushString, Push0, Push1;
extern VMaction PushVariable, PushReference, PushNil, FormKeyword,
  FormRange, FormRangeFunc, FormRangeFlag, AddRangeFunc, AddRangeFlag;
extern VMaction Eval, Eval2, GetMember, DerefMember, Deref;
extern VMaction Address, Negate, Complement, Not, True;
extern VMaction Power, Multiply, Divide, Modulo, Add, Subtract,
  ShiftL, ShiftR, Less, Greater, LessEQ, GreaterEQ, Equal, NotEqual,
  AndBits, XorBits, OrBits, AndOrLogical;
extern VMaction Define, Assign, DupUnder, EvalUnder, DropTop;
extern VMaction BranchFalse, BranchTrue, Branch, Return;
extern VMaction OpenStruct, DeclareMember, CloseStruct;
extern VMaction MatrixMult, Build, CallShell, Print, NextArg, MoreArgs;

/* ------------------------------------------------------------------------ */

/* The literalTable allows the Yorick parser to wait until an error-free
   function definition has been parsed before adding any variable names
   to globTab.  Also, statement label names, which never reach globTab,
   appear in the literalTable, as do struct member names.  If the label
   corresponding to a goto target has not yet appeared, it is flagged as
   a "target" type, and the literalTable index is inserted into the branch
   instruction instead of the proper offset.  */
static HashTable literalTable;
static long *literalTypes= 0;  /* L_LABEL | (vmCode index << 3) for labels */
/* bit 1-- set iff variable reference exists (local, external)
   bit 2-- set iff literal is local, not external (keyword if bit 1 not set)
   bit 3-- set iff literal used as statement label
   bit 4-- set when literal used as goto target before statement label
   bit 4>> (vmCode index <<3) for statement labels

   note: if first occurrence of a symbol is as a keyword in a function
         call, then bit 2 is set, but not bit 1, indicating that the
         literal must appear in globTab, but that it is undecided whether
         it is extern or local
 */
#define L_REFERENCE 1
#define L_LOCAL 2
#define L_LABEL 4
#define L_TARGET 8

/* Each function owns an array of Symbols containing its constants.
   During parsing, this array is built in the constantTable.  */
static Symbol *constantTable= 0;
static long maxConstants= 0;
static long nConstants= 0;

/* Virtual machine code is built in a static scratch array -- this code
   is not "linked" to the global symbol table or the function's constant
   table until the parse is finished and error-free.  */
static Instruction *vmCode= 0;
static long vmCodeSize= 0;
static long nextPC= 0;

/* YpAssignOrPrint and YpCheckRef need to know previous VM instruction */
static VMaction *previousOp;

/* YpCheckDefine needs to know whether most recently discovered variable
   was undecided (as opposed to local or extern).  */
static int wasUndecided;

/* During parsing, the location of references to variables or constants
   in vmCode is recorded in the variableRefs and constantRefs arrays,
   so that the correct globTab index or Symbol* can be filled in when
   the parse is complete.  The gotoTargets array provides a similar
   service for forward-reference goto commands.  */
static long *variableRefs= 0, *constantRefs= 0, *gotoTargets= 0;
static long maxVariableRefs= 0, nVariableRefs= 0;
static long maxConstantRefs= 0, nConstantRefs= 0;
static long maxGotoTargets= 0, nGotoTargets= 0;

/* for functions, number of positional and keyword parameters, flag for
   whether .. parameter used with next_arg(), more_args() defined  */
static int nPos, nKey;
static long hasPosList;
/* number of local variables and number of referenced but undefined
   goto target labels */
static int nLocal, nTarget;
/* current and maximum required virtual machine stack depth,
   flag for whether previous push operation increased maxStackDepth */
static int stackDepth, maxStackDepth, didMaxDepth;

/* The block of code which increments a for-loop is moved aside temporarily
   while the body of the for-loop is built, so that it can later be
   moved to its logical location after the loop body.
   Before each increment block in incCode, a two instruction tag contains
   the values of nVariableRefs and nConstantRefs at the beginning of the
   block; after each block, a similar tag marks the values at the end of
   the block.  When the block moves into its final location, the PCs in
   the variableRefs and constantRefs arrays are corrected.
 */
static Instruction *incCode= 0;
static long incCodeSize= 0;
static long nextInc= 0;

static int loopDepth;   /* required to distinguish break/continue targets */

typedef struct BreakStack BreakStack;
struct BreakStack {
  int id;    /* (loopDepth<<1) & isBreak
                tells which loop and whether break or continue */
  long pc;    /* pc of branch displacement in vmCode */
};

static BreakStack *breakStack= 0;
static long breakStackSize= 0;
static long nextBSP;  /* initialized to 0 each time parser starts */

/* Matrix mulitplication syntax is handled here rather than in yyparse.  */
typedef struct MatrixMarker MatrixMarker;
struct MatrixMarker {
  long stackDepth;   /* stack depth for this mark */
  int evalled;       /* 0 if this is still index range, 1 after YpEval */
};
static MatrixMarker *matrixMarkers= 0;
static int nMatrixMarkers= 0, maxMatrixMarkers= 0;

/* Certain subroutines receive special treatment -- their actual arguments
   are "quined", that is, followed by an argument which is a string
   the parser interpreted to produce the argument.  (This will be used to
   generate automatic legends for plotting routines.)  */
static HashTable quineTable;
static int *nQuinedArgs= 0;    /* number of positional arguments to quine */

static int insideFunc; /* used to determine if extern is outside functions */

static Function *reparsing;  /* non-zero on reparse (from YpReparse) */
extern long *ypReList, nYpReList;
extern int ypReMatch;
long *ypReList= 0;      /* list of pc, linenumber pairs on reparse */
long nYpReList= 0;
int ypReMatch= 0;

static Literal currentDataType;

static int CheckCodeSpace(long n);
static int CheckConstSpace(void);
static int ConstantReference(long pc);
static int VariableReference(Literal name);
static int IsPushConst(VMaction *a);
static long ReplaceConstant(long index, SymbolValue value);
static int TargetReference(Literal label);
static long LongConstant(long l);
static long DoubleConstant(double d);
static long StringConstant(char *string);
static CodeBlock PushInteger(long l, VMaction *Action);
static CodeBlock PushReal(double d, VMaction *Action);

static void SetBranchTarget(long pc, long targetPC);
static void PushBreak(int isBreak, long pc);
static void PopBreak(long brkTarget, long cntTarget);

static void WillPushStack(void);
static void WillPopStack(long n);

static void PushMarkStack(int pushing);

static void ClearParser(void *func);

/* ------------------------------------------------------------------------ */

int YpParseInit(void *func)
{
  ypErrors= 0;
  ClearParser(func);
  return 0;
}

static void ClearParser(void *func)
{
  long i;
  extern int yp_continue;
  yp_continue = 0;
  HashClear(&literalTable);  /* sets literalTable.maxItems==0 */
  p_free(literalTypes);
  literalTypes= 0;

  reparsing= func;
  nYpReList= 0;
  p_free(ypReList);
  ypReList= 0;

  for (i=0 ; i<nConstants ; i++)
    if (constantTable[i].ops==&dataBlockSym) Unref(constantTable[i].value.db);
  nConstants= maxConstants= 0;
  p_free(constantTable);
  constantTable= 0;

  nextPC= 0;
  if (vmCodeSize > 1024L) vmCodeSize= 0;  /* force realloc */
  else memset(vmCode, 0, sizeof(Instruction)*vmCodeSize);

  previousOp= 0;

  wasUndecided= 0;

  nVariableRefs= nConstantRefs= nGotoTargets= 0;
  if (maxVariableRefs > 256L) maxVariableRefs= 0;  /* force realloc */
  if (maxConstantRefs > 256L) maxConstantRefs= 0;  /* force realloc */
  if (maxGotoTargets > 256L) maxGotoTargets= 0;  /* force realloc */

  nPos= nKey= nLocal= nTarget= 0;
  hasPosList= 0;
  stackDepth= maxStackDepth= didMaxDepth= 0;

  nextInc= 0;
  if (incCodeSize > 64L) incCodeSize= 0;  /* force realloc */

  loopDepth= 0;

  nextBSP= 0;
  if (breakStackSize > 16L) breakStackSize= 0;  /* force realloc */

  nMatrixMarkers= 0;
  if (maxMatrixMarkers > 16L) maxMatrixMarkers= 0;  /* force realloc */

  insideFunc= 0;
}

/* ------------------------------------------------------------------------ */

static int CheckCodeSpace(long n)
{
  if (nextPC+n > vmCodeSize) {
    long newSize= (vmCodeSize? vmCodeSize<<1 : 64);
    vmCode= p_realloc(vmCode, sizeof(Instruction)*newSize);
    memset(&vmCode[vmCodeSize], 0, sizeof(Instruction)*(newSize-vmCodeSize));
    vmCodeSize= newSize;
  }
  return 0;
}

static int CheckConstSpace(void)
{
  if (nConstants >= maxConstants) {
    long newSize= (maxConstants? maxConstants<<1 : 16);
    constantTable= p_realloc(constantTable, sizeof(Symbol)*newSize);
    maxConstants= newSize;
  }
  return 0;
}

static void RecordLineNumber(long pc)
{
  if ((nYpReList&0xff)==0) {
    long newSize= nYpReList+0x100;
    ypReList= p_realloc(ypReList, sizeof(long)*newSize);
  }
  ypReList[nYpReList++]= pc;              /* record pc and... */
  ypReList[nYpReList++]= YpLineNumber();  /* ...corresponding line number */
}

static int ConstantReference(long pc)
{
  if (nConstantRefs >= maxConstantRefs) {
    long newSize= (maxConstantRefs? maxConstantRefs<<1 : 16);
    constantRefs= p_realloc(constantRefs, sizeof(long)*newSize);
    maxConstantRefs= newSize;
  }
  constantRefs[nConstantRefs++]= pc;
  return 0;
}

static int VariableReference(Literal name)
{
  if (nVariableRefs >= maxVariableRefs) {
    long newSize= (maxVariableRefs? maxVariableRefs<<1 : 16);
    variableRefs= p_realloc(variableRefs, sizeof(long)*newSize);
    maxVariableRefs= newSize;
  }
  variableRefs[nVariableRefs++]= nextPC;
  vmCode[nextPC++].index= name;
  literalTypes[name]|= L_REFERENCE;
  return 0;
}

static int TargetReference(Literal label)
{
  if (nGotoTargets >= maxGotoTargets) {
    long newSize= (maxGotoTargets? maxGotoTargets<<1 : 16);
    gotoTargets= p_realloc(gotoTargets, sizeof(long)*newSize);
    maxGotoTargets= newSize;
  }
  if (!(literalTypes[label]&L_TARGET)) {
    literalTypes[label]|= L_TARGET;
    nTarget++;  /* count first forward reference to each label */
  }
  gotoTargets[nGotoTargets++]= nextPC;
  vmCode[nextPC++].index= label;
  return 0;
}

static long LongConstant(long l)
{
  long i;
  for (i=0 ; i<nConstants ; i++) {
    if (constantTable[i].ops==&longScalar &&
        constantTable[i].value.l==l) break;
  }
  if (i>=nConstants) {
    if (CheckConstSpace()) return 0;
    constantTable[nConstants].ops= &longScalar;
    constantTable[nConstants].index= 0;
    constantTable[nConstants++].value.l= l;
  } else {
    constantTable[i].index++;
  }
  return i;
}

static long DoubleConstant(double d)
{
  long i;
  for (i=0 ; i<nConstants ; i++) {
    if (constantTable[i].ops==&doubleScalar &&
        constantTable[i].value.d==d) break;
  }
  if (i>=nConstants) {
    if (CheckConstSpace()) return 0;
    constantTable[nConstants].ops= &doubleScalar;
    constantTable[nConstants].index= 0;
    constantTable[nConstants++].value.d= d;
  } else {
    constantTable[i].index++;
  }
  return i;
}

static long StringConstant(char *string)
{
  Array *array;
  long i;
  for (i=0 ; i<nConstants ; i++) {
    if (constantTable[i].ops==&dataBlockSym) {
      array= (Array*)constantTable[i].value.db;
      if (strcmp(array->value.q[0], string)==0) break;
    }
  }
  if (i>=nConstants) {
    if (CheckConstSpace()) return 0;
    array= NewArray(&stringStruct, (Dimension *)0);
    constantTable[nConstants].ops= &dataBlockSym;
    constantTable[nConstants].index= 0;
    constantTable[nConstants++].value.db= (DataBlock *)array;
    array->value.q[0]= p_strcpy(string);
  } else {
    constantTable[i].index++;
  }
  return i;
}

static long ReplaceConstant(long index, SymbolValue value)
{
  if (constantTable[index].index) {
    if (constantTable[index].ops==&longScalar)
      return LongConstant(value.l);
    else if (constantTable[index].ops==&doubleScalar)
      return DoubleConstant(value.d);
    else
      return index;
  }
  constantTable[index].value= value;
  return index;
}

/* ------------------------------------------------------------------------ */

static void WillPushStack(void)
{
  if ((stackDepth++) == maxStackDepth) {
    maxStackDepth++;
    didMaxDepth= 1;
  } else {
    didMaxDepth= 0;
  }
}

static void WillPopStack(long n)
{
  int oops= 0;
  stackDepth-= n;
  while (nMatrixMarkers &&
         matrixMarkers[nMatrixMarkers-1].stackDepth>stackDepth) {
    nMatrixMarkers--;
    oops= 1;
  }
  if (oops) YpError("misuse of matrix multiply index marker (+)");
}

static void PushMarkStack(int pushing)
{
  if (nMatrixMarkers >= maxMatrixMarkers) {
    long newSize= maxMatrixMarkers+4;
    matrixMarkers= p_realloc(matrixMarkers, sizeof(MatrixMarker)*newSize);
    maxMatrixMarkers= newSize;
  }
  matrixMarkers[nMatrixMarkers].stackDepth= stackDepth+pushing;
  matrixMarkers[nMatrixMarkers++].evalled= 0;
}

/* ------------------------------------------------------------------------ */

static CodeBlock PushInteger(long l, VMaction *Action)
{
  long initialPC= nextPC;
  if (CheckCodeSpace(2)) return initialPC;
  if (stackDepth==0 && reparsing) RecordLineNumber(nextPC);
  vmCode[nextPC++].Action= previousOp= Action;
  ConstantReference(nextPC);
  vmCode[nextPC++].index= LongConstant(l);
  WillPushStack();
  return initialPC;
}

CodeBlock YpChar(long c)
{ return PushInteger(c, &PushChar); }

CodeBlock YpShort(long s)
{ return PushInteger(s, &PushShort); }

CodeBlock YpInt(long i)
{ return PushInteger(i, &PushInt); }

CodeBlock YpLong(long l)
{ return PushInteger(l, &PushLong); }

static CodeBlock PushReal(double d, VMaction *Action)
{
  long initialPC= nextPC;
  if (CheckCodeSpace(2)) return initialPC;
  if (stackDepth==0 && reparsing) RecordLineNumber(nextPC);
  vmCode[nextPC++].Action= previousOp= Action;
  ConstantReference(nextPC);
  vmCode[nextPC++].index= DoubleConstant(d);
  WillPushStack();
  return initialPC;
}

CodeBlock YpFloat(double f)
{ return PushReal(f, &PushFloat); }

CodeBlock YpDouble(double d)
{ return PushReal(d, &PushDouble); }

CodeBlock YpImaginary(double d)
{ return PushReal(d, &PushImaginary); }

CodeBlock YpString(Quote q)
{
  long initialPC= nextPC;
  if (CheckCodeSpace(2)) return initialPC;
  if (stackDepth==0 && reparsing) RecordLineNumber(nextPC);
  vmCode[nextPC++].Action= previousOp= &PushString;
  ConstantReference(nextPC);
  vmCode[nextPC++].index= q;
  WillPushStack();
  return initialPC;
}

static int IsPushConst(VMaction *a)
{
  return (a==&PushChar || a==&PushShort || a==&PushInt || a==&PushLong ||
          a==&PushFloat || a==&PushDouble || a==&PushImaginary ||
          a==&PushString);
}

Literal YpName(char *name, long len)
{
  if (!HashAdd(&literalTable, name, len)) {
    /* this name has never been seen before */
    HASH_MANAGE(literalTable, long, literalTypes);
    literalTypes[hashIndex]= 0;
  }
  return hashIndex;
}

Quote YpQuoteConst(char *q)
{
  return (Quote)StringConstant(q);
}

CodeBlock YpQuoted(Literal name)
{
  long initialPC= nextPC;
  if (CheckCodeSpace(2)) return initialPC;
  vmCode[nextPC++].Action= previousOp= &PushString;
  ConstantReference(nextPC);
  vmCode[nextPC++].index= StringConstant(literalTable.names[name]);
  WillPushStack();
  return initialPC;
}

CodeBlock YpVariable(Literal name)
{
  long initialPC= nextPC;
  if (CheckCodeSpace(2)) return initialPC;
  if (stackDepth==0 && reparsing) RecordLineNumber(nextPC);
  vmCode[nextPC++].Action= previousOp= &PushVariable;
  /* set wasUndecided flag if this is the first reference of this variable */
  wasUndecided= !(literalTypes[name]&L_REFERENCE);
  if (wasUndecided && (literalTypes[name]&L_LOCAL))
    literalTypes[name] &= ~L_LOCAL;  /* now seen as other than keyword */
  VariableReference(name);
  WillPushStack();
  return initialPC;
}

/* YpLitName is a hook required by ScanForFunc */
char *YpLitName(Literal name)
{
  return literalTable.names[name];
}

void YpCheckRef(CodeBlock cb)
{
  long pc= cb;
  if (previousOp==&PushVariable)
    vmCode[pc].Action= previousOp= &PushReference;
}

static char *rfNames[]= { "avg", "cum", "dif", "max", "min", "mnx", "mxx",
                       "pcen", "psum", "ptp", "rms", "sum", "uncp", "zcen" };

static RangeFunc *RFs[]= { &RFavg,&RFcum,&RFdif,&RFmax,&RFmin,&RFmnx,&RFmxx,
                      &RFpcen,&RFpsum,&RFptp,&RFrms,&RFsum,&RFuncp,&RFzcen };

CodeBlock YpPushRF(int which)
{
  /* which is 0-15:
     avg cum dif max min mnx mxx pcen psum ptp rms sum uncp zcen + -   */
  return YpVariable(YpName(rfNames[which], 0L));
}

CodeBlock YpNil(void)
{
  long initialPC= nextPC;
  if (CheckCodeSpace(1)) return initialPC;
  if (stackDepth==0 && reparsing) RecordLineNumber(nextPC);
  vmCode[nextPC++].Action= previousOp= &PushNil;
  WillPushStack();
  return initialPC;
}

void YpDotDot(int which)
{
  if (CheckCodeSpace(2)) return;
  vmCode[nextPC++].Action= previousOp= &FormRangeFlag;
  vmCode[nextPC++].count= R_RUBBER | (which? R_PSEUDO : 0);
  WillPushStack();
}

void YpKeyword(Literal name, CodeBlock value)
{
  int undecided = !(literalTypes[name] & L_REFERENCE);
  if (CheckCodeSpace(2)) return;
  vmCode[nextPC++].Action= previousOp= &FormKeyword;
  VariableReference(name);
  if (undecided) {  /* default is for keywords to become local, not extern */
    literalTypes[name] &= ~L_REFERENCE;
    literalTypes[name] |= L_LOCAL;  /* note nLocal does not increment */
  }
  WillPushStack();
}

CodeBlock YpRange(CodeBlock min, int hasInc)
{
  if (CheckCodeSpace(2)) return min;
  vmCode[nextPC++].Action= previousOp= &FormRange;
  vmCode[nextPC++].count= hasInc? 3 : 2;
  WillPopStack(hasInc? 2L : 1L);
  return min;
}

CodeBlock YpRangeFunc(int which, CodeBlock range)
{
  /* which is 0-15:
     avg cum dif max min mnx mxx pcen psum ptp rms sum uncp zcen + -   */
  long initialPC;
  if (CheckCodeSpace(2)) return (range==NONE)? nextPC : range;

  if (range==NONE) {
    initialPC= nextPC;
    if (which<14) {
      vmCode[nextPC++].Action= previousOp= &FormRangeFunc;
      vmCode[nextPC++].rf= RFs[which];
    } else {
      int flag;
      if (which==14) {
        flag= R_MARKED;
        PushMarkStack(1);
      } else {
        flag= R_PSEUDO;
      }
      vmCode[nextPC++].Action= previousOp= &FormRangeFlag;
      vmCode[nextPC++].count= flag;
    }
    WillPushStack();

  } else {
    initialPC= range;
    if (which<14) {
      vmCode[nextPC++].Action= previousOp= &AddRangeFunc;
      vmCode[nextPC++].rf= RFs[which];
    } else {
      int flag;
      if (which==14) {
        flag= R_MARKED;
        PushMarkStack(0);
      } else {
        flag= R_PSEUDO;
      }
      vmCode[nextPC++].Action= previousOp= &AddRangeFlag;
      vmCode[nextPC++].count= flag;
    }
  }

  return initialPC;
}

/* ------------------------------------------------------------------------ */

void YpEvalInit(CodeBlock obj)
{
  /* could also check for specially parsed function here */
  if (nMatrixMarkers) {
    int oops= 0;
    while (nMatrixMarkers &&
           matrixMarkers[nMatrixMarkers-1].stackDepth>=stackDepth) {
      nMatrixMarkers--;
      oops= 1;
    }
    if (oops) YpError("misuse of matrix multiply index marker (+)");
  }
  /* YpCheckRef(obj);
     x(index_list)= expr
     presents a serious problem if x is a double, long, or int scalar --
     this could be handled by doing a PushReference instead of a
     PushVariable, at the cost of some efficiency.  Unfortunately, this
     is dangerous, since if the RHS changes x, the LHS would contain a
     bogus pointer...  For now, leave it so x(1)= 5, say, will not work
     if x is one of the three fast scalars.  */
}

CodeBlock YpEval(CodeBlock obj, int nArgs)
{
  long delta= nArgs;
  if (CheckCodeSpace(2)) return obj;
  vmCode[nextPC++].Action= previousOp= &Eval;
  vmCode[nextPC++].count= nArgs;
  if (nMatrixMarkers) {
    long markDepth= matrixMarkers[nMatrixMarkers-1].stackDepth;
    long objDepth= stackDepth-nArgs;
    if (markDepth>=objDepth) {
      int oops= (markDepth==objDepth);
      nMatrixMarkers--;
      if (matrixMarkers[nMatrixMarkers].evalled)
        YpError("misuse of matrix multiply index marker (+)");
      while (nMatrixMarkers &&
             matrixMarkers[nMatrixMarkers-1].stackDepth>=objDepth) {
        nMatrixMarkers--;
        oops= 1;
      }
      if (oops)
        YpError("only one index may be marked (+) for matrix multiply");
      matrixMarkers[nMatrixMarkers].stackDepth= objDepth+1;
      matrixMarkers[nMatrixMarkers++].evalled= 1;
      vmCode[nextPC-2].Action= previousOp= &Eval2;
      delta--;   /* Eval2 leaves 2 results on stack */
    }
  }
  stackDepth-= delta;   /* in lieu of WillPopStack */
  return obj;
}

CodeBlock YpNextArg(int which)
{
  long initialPC= nextPC;
  if (!(hasPosList&1)) {
    YpError("next_arg() or more_args() needs .. function parameter");
  } else {
    Literal name= YpName("*va*", 4L);
    if (CheckCodeSpace(2)) return initialPC;
    vmCode[nextPC++].Action= previousOp= which? &MoreArgs : &NextArg;
    VariableReference(name);
    WillPushStack();
  }
  return initialPC;
}

CodeBlock YpBuild(CodeBlock lop, int nArgs)
{
  if (CheckCodeSpace(2)) return lop;
  vmCode[nextPC++].Action= previousOp= &Build;
  vmCode[nextPC++].count= nArgs;
  WillPopStack(nArgs-1L);
  return lop;
}

CodeBlock YpMember(CodeBlock obj, int pointer, Literal name)
{
  if (CheckCodeSpace(2)) return obj;
  vmCode[nextPC++].Action=
    previousOp= pointer? &DerefMember : &GetMember;
  ConstantReference(nextPC);
  vmCode[nextPC++].index= StringConstant(literalTable.names[name]);
  return obj;
}

CodeBlock YpPostfix(CBorLit lhs, int assop)
{
  CodeBlock rhs;
  int isLiteral= IS_LITERAL(lhs);
  if (isLiteral) {  /* e.g.-  ++x */
    Literal name= CBL_VALUE(lhs);
    lhs= YpVariable(name);
  } else {               /* e.g.-  ++x(i) */
    lhs= CBL_VALUE(lhs);
  }
  if (CheckCodeSpace(1)) return lhs;
  rhs= nextPC;
  vmCode[nextPC++].Action= &Push1;
  WillPushStack();
  if (isLiteral) {
    if (CheckCodeSpace(1)) return lhs;
    vmCode[nextPC++].Action= previousOp= &DupUnder;
    WillPushStack();
    rhs= YpIncrement(lhs, assop, rhs);
    if (CheckCodeSpace(1)) return rhs;
    vmCode[nextPC++].Action= previousOp= &DropTop;
    WillPopStack(1L);
  } else {
    if (CheckCodeSpace(4)) return lhs;
    vmCode[nextPC++].Action= previousOp= &EvalUnder;
    WillPushStack();
    WillPushStack();  /* EvalUnder pushes 2 new stack elements */
    vmCode[nextPC++].Action= assop? &Subtract : &Add;
    vmCode[nextPC++].Action= previousOp= &Assign;
    vmCode[nextPC++].Action= previousOp= &DropTop;
    WillPopStack(3L);
    rhs= lhs;
  }
  return rhs;
}

static VMaction *Unaries[]= { &Deref, &Address, 0, &Negate,
                              &Complement, &Not };

CodeBlock YpUnop(int which, CodeBlock op)
{
  /* which is 0-7:
     *  &  +  -  ~  ! ++ --   */
  if (which==2) return op;  /* unary + is no-op */
  if (which<6) {
    if (which==3 && previousOp==&Eval2) {
      /* unary - must be deferred to after matrix multiply */
      matrixMarkers[nMatrixMarkers-1].evalled= 2;
      return op;    /* no-op for now, reinsert in YpMultop */
    }
    if (IsPushConst(previousOp)) {
      long i= vmCode[nextPC-1].index;
      if (which<2) YpError("unary * or & cannot be applied to a constant");
      else if (constantTable[i].ops==&longScalar) {
        SymbolValue value;
        if (which==3) value.l= -constantTable[i].value.l;
        else if (which==4) value.l= ~constantTable[i].value.l;
        else value.l= !constantTable[i].value.l;
        vmCode[nextPC-1].index= ReplaceConstant(i, value);
      } else if (constantTable[i].ops==&doubleScalar) {
        if (which==3) {
          SymbolValue value;
          value.d= -constantTable[i].value.d;
          vmCode[nextPC-1].index= ReplaceConstant(i, value);
        } else {
          YpError("unary ~ or ! not allowed on float or double constant");
        }
      } else {
        YpError("unary -, ~, or ! not allowed on string constant");
      }
    } else {
      if (CheckCodeSpace(1)) return op;
      vmCode[nextPC++].Action= previousOp= Unaries[which];
    }
    return op;

  } else {
    CodeBlock rhs;
    if (IS_LITERAL(op)) {  /* e.g.-  ++x */
      Literal name= CBL_VALUE(op);
      op= YpVariable(name);
    } else {               /* e.g.-  ++x(i) */
      op= CBL_VALUE(op);
    }
    if (CheckCodeSpace(1)) return op;
    rhs= nextPC;
    vmCode[nextPC++].Action= previousOp= &Push1;
    WillPushStack();
    return YpIncrement(op, which-6, rhs);
  }
}

static VMaction *Binaries[]= { &Power, &Multiply, &Divide, &Modulo,
  &Add, &Subtract, &ShiftL, &ShiftR, &Less, &Greater, &LessEQ, &GreaterEQ,
  &Equal, &NotEqual, &AndBits, &XorBits, &OrBits };

CodeBlock YpBinop(CodeBlock lop, int which)
{
  /* which is 0-16:
     ^ * / % + -  << >>  < > <= >= == !=  & ~ |    */
  if (CheckCodeSpace(1)) return lop;
  vmCode[nextPC++].Action= previousOp= Binaries[which];
  WillPopStack(1L);
  return lop;
}

CodeBlock YpMultop(CodeBlock lop)
{
  /* which is 0-16:
     ^ * / % + -  << >>  < > <= >= == !=  & ~ |    */
  if (CheckCodeSpace(1)) return lop;
  if (!nMatrixMarkers ||
      matrixMarkers[nMatrixMarkers-1].stackDepth<stackDepth-1) {
    vmCode[nextPC++].Action= previousOp= &Multiply;
    WillPopStack(1L);

  } else {
    /* top two matrixMarkers must be at stackDepth and stackDepth-2
       (recall stack looks like this: leftOp, leftMark, rOp, rMark) */
    long rm= --nMatrixMarkers;
    long lm= nMatrixMarkers-1;
    if (matrixMarkers[rm].stackDepth==stackDepth &&
        matrixMarkers[rm].evalled && lm>=0 &&
        matrixMarkers[lm].stackDepth==stackDepth-2 &&
        matrixMarkers[lm].evalled) {
      nMatrixMarkers--;
      vmCode[nextPC++].Action= previousOp= &MatrixMult;
      WillPopStack(3L);
      if (matrixMarkers[lm].evalled==2) {
        /* restore the deferred unary - operation */
        if (CheckCodeSpace(1)) return lop;
        vmCode[nextPC++].Action= previousOp= &Negate;
      }
    } else {
      YpError("both matrix multiply operands need an index marker (+)");
      WillPopStack(2L);  /* just a guess -- parse has failed anyway */
    }
  }
  return lop;
}

CodeBlock YpLogop(CodeBlock lop, int which, CodeBlock rop)
{
  /* which is 0-1:
     && ||   */
  if (CheckCodeSpace(2)) return lop;
  vmCode[nextPC++].Action= &AndOrLogical;
  /* AndLogical and OrLogical require a branch operation */
  SetBranchTarget(rop-1, nextPC);
  /* AndLogical (or OrLogical) skips next instruction */
  vmCode[nextPC++].Action= previousOp= which? &Push1 : &Push0;
  /* YpBranch already popped stack */
  return lop;
}

CodeBlock YpTernop(CodeBlock cond, CodeBlock iftrue, CodeBlock iffalse)
{
  SetBranchTarget(iftrue-1, iffalse);
  SetBranchTarget(iffalse-1, nextPC);
  previousOp= 0;
  return cond;
}

CBorLit YpCheckDefine(CodeBlock lhs)
{
  CBorLit value;
  if (previousOp==&Eval || previousOp==&GetMember ||
      previousOp==&DerefMember || previousOp==&Deref) {
    /* the preceding operation will leave an lvalue on the stack */
    value= CB_OR_LITERAL(lhs, 0);
  } else if (previousOp==&PushVariable) {
    /* remove the preceding PushVariable instruction */
    Literal name= (Literal)vmCode[nextPC-1].index;
    previousOp= 0;
    nVariableRefs--;
    nextPC= lhs;
    if (wasUndecided) literalTypes[name]^= L_REFERENCE;
    stackDepth--;
    if (didMaxDepth) maxStackDepth--;
    value= CB_OR_LITERAL(name, 1);
    if (stackDepth==0 && reparsing) nYpReList-= 2;
  } else {
    YpError("lhs of = (or ++ or -- operand) not a variable or lvalue");
    value= CB_OR_LITERAL(lhs, 0);
  }
  return value;   /* used by YpAssign */
}

CodeBlock YpAssign(CBorLit lhs, CodeBlock rhs)
{
  long initialPC;
  if (IS_LITERAL(lhs)) {
    /* this is definition of a variable */
    long name= CBL_VALUE(lhs);
    int undecided= !(literalTypes[name]&L_REFERENCE);
    initialPC= rhs;
    if (CheckCodeSpace(2)) return initialPC;
    vmCode[nextPC++].Action= previousOp= &Define;
    VariableReference(name);
    if (undecided) {
      literalTypes[name]|= L_LOCAL;
      nLocal++;
    }

  } else {
    /* this is assignment to an lvalue */
    initialPC= CBL_VALUE(lhs);
    if (CheckCodeSpace(1)) return initialPC;
    vmCode[nextPC++].Action= previousOp= &Assign;
    WillPopStack(1L);
  }
  return initialPC;
}

/* Map from assop into Binaries array */
static int assopMap[]= { 4, 5, 1, 2, 3, 6, 7, 14, 15, 16 };

CodeBlock YpIncrement(CodeBlock lhs, int assop, CodeBlock rhs)
{
  /* assop is 0-9:
     += -= *= /= %= <<= >>= &= ~= |=   */
  if (lhs+2==rhs && vmCode[lhs].Action==&PushVariable) {
    /* this is increment and redefine a variable */
    if (CheckCodeSpace(3)) return lhs;
    vmCode[nextPC++].Action= Binaries[assopMap[assop]];
    vmCode[nextPC++].Action= previousOp= &Define;
    VariableReference(vmCode[lhs+1].index);
    WillPopStack(1L);

  } else {
    /* this is increment of an lvalue */
    if (CheckCodeSpace(3)) return lhs;
    vmCode[nextPC++].Action= &DupUnder;
    WillPushStack();
    vmCode[nextPC++].Action= Binaries[assopMap[assop]];
    vmCode[nextPC++].Action= previousOp= &Assign;
    WillPopStack(2L);
  }
  return lhs;
}

/* ------------------------------------------------------------------------ */

static void SetBranchTarget(long pc, long targetPC)
{
  vmCode[pc].displace= targetPC-pc;
}

static void PushBreak(int isBreak, long pc)
{
  if (nextBSP >= breakStackSize) {
    breakStack= p_realloc(breakStack, sizeof(BreakStack)*(breakStackSize+16));
    breakStackSize+= 16;
  }
  breakStack[nextBSP].id= (loopDepth<<1)|isBreak;
  breakStack[nextBSP].pc= pc;
  nextBSP++;
}

static void PopBreak(long brkTarget, long cntTarget)
{
  int mask= loopDepth<<1;
  int isBreak;
  while (nextBSP && (isBreak= breakStack[nextBSP-1].id ^ mask)<2) {
    nextBSP--;
    SetBranchTarget(breakStack[nextBSP].pc, isBreak? brkTarget : cntTarget);
  }
  loopDepth--;
}

/* ------------------------------------------------------------------------ */

void YpBranch(int cond)
{   /* Note: cond==3 used as branch_pop for ternary operator */
  if (CheckCodeSpace(2)) return;
  if (cond && previousOp==&Not) {
    cond= 3-cond;
    nextPC--;
  }
  if (cond==1)      vmCode[nextPC++].Action= previousOp= &BranchFalse;
  else if (cond==2) vmCode[nextPC++].Action= previousOp= &BranchTrue;
  else              vmCode[nextPC++].Action= previousOp= &Branch;
  vmCode[nextPC++].displace= 0;
  if (cond) WillPopStack(1L);
}

void YpLoop(int type)    /* 0 while, 1 do, 2 for */
{
  loopDepth++;
}

CodeBlock YpIfElse(CodeBlock cond, CodeBlock ifStmnt, CodeBlock elseStmnt)
{
  if (elseStmnt==NONE) {
    SetBranchTarget(ifStmnt-1, nextPC);
  } else {
    SetBranchTarget(ifStmnt-1, elseStmnt);
    SetBranchTarget(elseStmnt-1, nextPC);
  }
  previousOp= 0;
  return cond;
}

CodeBlock YpWhile(CodeBlock cond, CodeBlock body)
{
  if (CheckCodeSpace(2)) return cond;
  vmCode[nextPC++].Action= previousOp= &Branch;
  SetBranchTarget(nextPC++, cond);
  SetBranchTarget(body-1, nextPC);
  PopBreak(nextPC, cond);
  return cond;
}

CodeBlock YpDo(CodeBlock body, CodeBlock cond)
{
  if (CheckCodeSpace(2)) return cond;
  if (previousOp==&Not) { nextPC--; previousOp= &BranchFalse; }
  else previousOp= &BranchTrue;
  vmCode[nextPC++].Action= previousOp;
  SetBranchTarget(nextPC++, body);
  WillPopStack(1L);
  PopBreak(nextPC, cond);
  return body;
}

static long firstVariable, firstConstant;

void YpBeginInc(void)
{
  firstVariable= nVariableRefs;
  firstConstant= nConstantRefs;
}

void YpEndInc(CodeBlock inc)
{
  long nLineNums= 0;
  long initialPC= inc;
  long n= nextPC-initialPC;
  nextPC= initialPC;    /* set vmCode pc to overwrite for-increment code */

  if (reparsing) {
    long i= nYpReList-2;
    while (nLineNums<nYpReList && ypReList[i-nLineNums]>=initialPC)
      nLineNums+= 2;
    nLineNums++;  /* 1 extra to record count */
  }

  if (nextInc+n+nLineNums+6 > incCodeSize) {
    long newSize= (nextInc+n+nLineNums+76);
    incCode= p_realloc(incCode, newSize*sizeof(Instruction));
    incCodeSize= newSize;
  }

  /* move for-increment code out of vmCode buffer */
  while (n--) incCode[nextInc++]= vmCode[initialPC++];
  /* move line number information out of ypReList */
  if (reparsing) {
    long i;
    nLineNums--;
    nYpReList-= nLineNums;
    for (i=0 ; i<nLineNums ; i++)
      incCode[nextInc+i].index= ypReList[nYpReList+i];
    nextInc+= nLineNums;
    incCode[nextInc++].index= nLineNums;
  }
  /* variable reference PCs will need adjustment later */
  incCode[nextInc++].index= firstVariable;
  incCode[nextInc++].index= firstConstant;
  incCode[nextInc++].index= nVariableRefs;
  incCode[nextInc++].index= nConstantRefs;
  incCode[nextInc++].index= initialPC-nextPC; /* they've switched places */
  incCode[nextInc++].count= loopDepth;
}

CodeBlock YpFor(CodeBlock init, CodeBlock test, CodeBlock body)
{
  long inc= nextPC;

  if (nextInc && incCode[nextInc-1].count==loopDepth) {
    /* move for-increment code out of incCode into vmCode */
    long delta= inc-body;   /* distance inc block will move */
    long n= incCode[nextInc-=2].index;
    long lastConstant= incCode[--nextInc].index;
    long lastVariable= incCode[--nextInc].index;
    long firstConstant= incCode[--nextInc].index;
    long firstVariable= incCode[--nextInc].index;
    long i;
    /* patch up line number information */
    if (reparsing) {
      long nLineNums= incCode[--nextInc].index;
      long size= nYpReList>0? ((nYpReList-1)&0xffffff00)+0x100 : 0;
      if (nYpReList+nLineNums > size) {
        size= ((nYpReList+nLineNums-1)&0xffffff00)+0x100;
        ypReList= p_realloc(ypReList, sizeof(long)*size);
      }
      nextInc-= nLineNums;
      for (i=0 ; i<nLineNums ; i+=2) {
        ypReList[nYpReList+i]= incCode[nextInc+i].index + delta;    /* pc */
        ypReList[nYpReList+i+1]= incCode[nextInc+i+1].index;    /* line # */
      }
      nYpReList+= nLineNums;
    }
    /* move code */
    i= (nextInc-= n);   /* nextInc points to first word of inc */
    if (CheckCodeSpace(n)) return init;
    while (n--) vmCode[nextPC++]= incCode[i++];
    /* adjust constant and variable references */
    while (firstConstant<lastConstant) constantRefs[firstConstant++]+= delta;
    while (firstVariable<lastVariable) variableRefs[firstVariable++]+= delta;
  }

  if (CheckCodeSpace(2)) return init;
  vmCode[nextPC++].Action= previousOp= &Branch;
  if (test==NONE) {
    SetBranchTarget(nextPC++, body);
  } else {
    SetBranchTarget(nextPC++, test);
    SetBranchTarget(body-1, nextPC);
  }
  PopBreak(nextPC, inc);
  return init;
}

CodeBlock YpContinue(void)
{
  long initialPC= nextPC;
  if (loopDepth>0) {
    if (CheckCodeSpace(2)) return initialPC;
    vmCode[nextPC++].Action= previousOp= &Branch;
    PushBreak(0, nextPC);
    vmCode[nextPC++].displace= 0;
  } else {
    YpError("no loop for continue statment");
  }
  return initialPC;
}

CodeBlock YpBreak(void)
{
  long initialPC= nextPC;
  if (loopDepth>0) {
    if (CheckCodeSpace(2)) return initialPC;
    vmCode[nextPC++].Action= previousOp= &Branch;
    PushBreak(1, nextPC);
    vmCode[nextPC++].displace= 0;
  } else {
    YpError("no loop for break statment");
  }
  return initialPC;
}

CodeBlock YpReturn(CodeBlock value)
{
  if (value==NONE) value= YpNil();
  if (CheckCodeSpace(1)) return value;
  vmCode[nextPC++].Action= previousOp= &Return;
  WillPopStack(1L);  /* Of course, the true effect on the VM stack is
                        complicated, but as far as the following
                        instruction is concerned, the stack is one shorter.
                        It is an error if stackDepth!=0 after this.  */
  return value;
}

CodeBlock YpGoto(Literal label)
{
  long literalType= literalTypes[label];
  long initialPC= nextPC;
  if (CheckCodeSpace(2)) return initialPC;
  vmCode[nextPC++].Action= previousOp= &Branch;
  if (!(literalType&L_LABEL)) TargetReference(label);
  else SetBranchTarget(nextPC++, (literalType>>3));
  return initialPC;
}

CodeBlock YpLabel(Literal label)
{
  long literalType= literalTypes[label];
  /*long targetPC= literalType>>3;*/
  if (!(literalType&L_LABEL)) {
    if (literalType&L_TARGET) {
      literalTypes[label]&= (L_REFERENCE|L_LOCAL);
      nTarget--;  /* forward references to this label now satisfied */
    }
    literalTypes[label]|= L_LABEL;
  } else {
    YpError("duplicate statement label");
  }
  literalTypes[label]|= (nextPC<<3);
  return nextPC;
}

void YpQuine(char *name, int nQuined)
{
  if (!HashAdd(&quineTable, name, 0L)) {
    HASH_MANAGE(quineTable, int, nQuinedArgs);
  }
  nQuinedArgs[hashIndex]= nQuined;
}

int YpCallInit(CodeBlock name)
{
  /* initialize argument parsing for special functions */
  if (HashFind(&quineTable, literalTable.names[vmCode[name+1].index], 0L))
    return nQuinedArgs[hashIndex];
  else
    return 0;
}

CodeBlock YpCall(CodeBlock name, int nArgs)
{
  if (CheckCodeSpace(3)) return name;
  vmCode[nextPC++].Action= previousOp= &Eval;
  vmCode[nextPC++].count= nArgs;
  vmCode[nextPC++].Action= previousOp= &DropTop;
  WillPopStack(nArgs+1L);
  return name;
}

CodeBlock YpAssignOrPrint(CodeBlock aexpr)
{
  if (previousOp==&Assign || previousOp==&Define || previousOp==&DropTop) {
    if (CheckCodeSpace(1)) return aexpr;
  } else {
    if (CheckCodeSpace(2)) return aexpr;
    /* Note: Print leaves its argument on the stack so that functions
       invoked with no actual parameters can always check whether the
       next action will be DropTop to tell that they have been invoked
       as subroutines.  */
    vmCode[nextPC++].Action= previousOp= &Print;
  }
  vmCode[nextPC++].Action= previousOp= &DropTop;
  WillPopStack(1L);
  return aexpr;
}

void YpExtern(Literal name)
{
  long literalType= literalTypes[name];
  if ((literalType&(L_REFERENCE|L_LOCAL)) == (L_REFERENCE|L_LOCAL)) {
    YpError("extern variable conflicts with local variable");
  } else {
    /* presume that it WILL be referenced, otherwise it could revert
       to local without comment */
    literalTypes[name] |= L_REFERENCE;
    literalTypes[name] &= ~L_LOCAL;
  }
  if (!insideFunc) {
    /* Outside a func body, extern is used to point to variables
       whose definitions are to be accessible via Yorick's help
       mechanism.  This is one part kludge/hack, one part slick.
       struct and func definitions are automatically accessible
       (see the end of YpFunc).  */
    RecordSource(Globalize(literalTable.names[name], 0L));
  }
}

void YpLocal(Literal name)
{
  long literalType= literalTypes[name];
  if (literalType&L_REFERENCE) {
    if (!(literalType&L_LOCAL))
      YpError("local variable conflicts with extern variable");
  } else {
    /* presume that it WILL be referenced -- once nLocal is incremented,
       the name has to be globalized anyway */
    nLocal++;
    literalTypes[name]|= (L_REFERENCE | L_LOCAL);
  }
  if (!insideFunc) {
    /* Outside a func body, local is used to point to variables
       whose definitions are to be accessible via Yorick's help
       mechanism.  Unlike extern, local is NOT used by Codger, so
       it can be used to provide documentation strings for objects
       which are not built-in functions, functions, or structs.  */
    RecordSource(Globalize(literalTable.names[name], 0L));
  }
}

void YpSpecial(int which)
{
  if (!insideFunc && which<14) {
    /* Special form allows builtins with names the same as range functions
       to be declared and documented in std.i.  */
    RecordSource(Globalize(rfNames[which], 0L));
  } else {
    YpError("illegal name in extern statement");
  }
}

CodeBlock YpSyscall(Quote sysline)
{
  long initialPC= nextPC;
  if (CheckCodeSpace(2)) return initialPC;
  if (stackDepth==0 && reparsing) RecordLineNumber(nextPC);
  vmCode[nextPC++].Action= previousOp= &CallShell;
  ConstantReference(nextPC);
  vmCode[nextPC++].index= sysline;
  return initialPC;
}

CodeBlock YpNoop(void)
{
  previousOp= 0;
  return nextPC;
}

void YpDrop(void)
{
  if (CheckCodeSpace(1)) return;
  vmCode[nextPC++].Action= previousOp= &DropTop;
  WillPopStack(1L);
}

/* ------------------------------------------------------------------------ */

void YpDataType(Literal name)
{
  currentDataType= name;
}

CodeBlock YpDeclarator(int pointer, CodeBlock name, int nDims)
{
  YpVariable(pointer? YpName("pointer", 7L) : currentDataType);
  if (CheckCodeSpace(2)) return name;
  vmCode[nextPC++].Action= previousOp= &DeclareMember;
  vmCode[nextPC++].count= nDims;
  WillPopStack(nDims+2L);  /* name, dimlist, type */
  return name;
}

void YpStruct(CodeBlock name, int eol)
{
  if (name!=NONE && !CheckCodeSpace(1)) {
    vmCode[nextPC++].Action= previousOp= &CloseStruct;
    WillPopStack(1L);   /* StructDef left by OpenStruct */
    /* guaranteed that name points to a PushVariable instruction --
       change to OpenStruct */
    vmCode[name].Action= &OpenStruct;  /* this leaves reference on stack
                                          for DeclareMember, cleaned off
                                          by CloseStruct... */
  }

  YpFunc(2, eol);  /* name of struct will temporarily be name of function
                      which builds the StructDef -- this can automatically
                      prevent self-reference, since until the CloseStruct,
                      the name of the struct will NOT be a data type... */
}

/* ------------------------------------------------------------------------ */

void YpInitFunc(Literal name)
{
  /* the function name is the first entry in literalTable --
     note that a parameter may not have the same name as its function */
  literalTypes[name]|= L_REFERENCE|L_LOCAL;  /* name is always 0 */
  nLocal= 0;
  previousOp= 0;
  nPos= nKey= 0;
  hasPosList= 0;
  nTarget= 0;
  insideFunc= 1;
}

void YpPosParam(Literal name, int output)
{
  nPos++;
  if (!(literalTypes[name]&L_REFERENCE))
    literalTypes[name]|= L_REFERENCE|L_LOCAL;
  else
    /* Note- parameter with same name as function is treated as duplicate */
    YpError("duplicate positional parameter name");

  if (output) {
    if (nPos<31) hasPosList|= (1<<nPos);
    else YpError("(BUG/LIMIT) only the first 30 parameters can be outputs");
  }
}

void YpKeyParam(Literal name)
{
  nKey++;
  if (!(literalTypes[name]&L_REFERENCE))
    literalTypes[name]= L_REFERENCE|L_LOCAL;
  else
    YpError("duplicate keyword parameter name");
  /* parameter with same name as function is treated as duplicate */
}

void YpHasPosList(void)
{
  hasPosList|= 1;
  YpLocal(YpName("*va*", 4L));
  nLocal--;
}

void YpFunc(int isMain, int eol)
{
  long codeSize, i, pc;
  Function *parsedFunc= 0;
  DataBlock *oldDB;
  Symbol *cTable;

  if (!eol || ypErrors) {
    if (!eol) YpError("garbage after func or struct definition");
    ClearParser((void *)0);
    return;
  }

  if (isMain && nextPC==0) return;

  /* NOTE- if stackDepth!=0 here, there is a bug in the parser...
           (unless a previous error involving matrix multiplication?) */
  if (previousOp!=&Return) YpReturn(NONE);

  if (isMain) nLocal= 0;

  /* end of function marked by code.Action==&Return followed by
     code.Action==0, followed by code.index==codeSize to enable
     error recovery to find beginning of function */
  if (CheckCodeSpace(2)) {
    ClearParser((void *)0);
    return;
  }
  vmCode[nextPC++].Action= previousOp= 0;
  vmCode[nextPC].index= codeSize= 1+nPos+(hasPosList&1)+nKey+nLocal+ nextPC;
  nextPC++;

  /* fill in all forward-referenced goto targets */
  if (nTarget) {
    YpError("missing goto label at end of func");
    ClearParser((void *)0);
    return;
  }
  for (i=0 ; i<nGotoTargets ; i++) {
    pc= gotoTargets[i];
    SetBranchTarget(pc, (literalTypes[vmCode[pc].index]>>3));
  }
  nGotoTargets= 0;

  /* shorten constant table to its final size */
  if (nConstants) {
    constantTable= p_realloc(constantTable, nConstants*sizeof(Symbol));
    maxConstants= nConstants;
  }

  /* fill in all references to constants */
  if (!reparsing) cTable= constantTable;
  else cTable= reparsing->constantTable;
  for (i=0 ; i<nConstantRefs ; i++) {
    pc= constantRefs[i];
    vmCode[pc].constant= cTable+vmCode[pc].index;
  }
  nConstantRefs= 0;

  /* locate or create all referenced variable names in globTab --
     reuse literalTypes array to hold globTab index */
  if (CheckCodeSpace(1+nPos+(hasPosList&1)+nKey+nLocal)) {
    ClearParser((void *)0);
    return;
  }
  if (isMain) {
    for (i=0 ; i<literalTable.nItems ; i++)
      if (literalTypes[i]&(L_REFERENCE|L_LOCAL))
        literalTypes[i]= Globalize(literalTable.names[i], 0L);
    vmCode[nextPC++].index=
      Globalize(isMain==1? "*main*" : literalTable.names[0], 0L);
  } else {
    for (i=0 ; i<literalTable.nItems ; i++) {
      /* Note that function name is first, then positional parameters,
         then optional *va* parameter, then keyword parameters.  */
      if (literalTypes[i]&(L_REFERENCE|L_LOCAL)) {
        if ((literalTypes[i]&(L_REFERENCE|L_LOCAL)) == (L_REFERENCE|L_LOCAL))
          vmCode[nextPC++].index=
            literalTypes[i]= Globalize(literalTable.names[i], 0L);
        else
          literalTypes[i]= Globalize(literalTable.names[i], 0L);
      }
    }
  }

  /* fill in all references to variables */
  for (i=0 ; i<nVariableRefs ; i++) {
    pc= variableRefs[i];
    vmCode[pc].index= literalTypes[vmCode[pc].index];
  }
  nVariableRefs= 0;

  /* done with literal table */
  HashClear(&literalTable);  /* sets literalTable.maxItems==0 */
  p_free(literalTypes);
  literalTypes= 0;

  if (!reparsing)
    parsedFunc= NewFunction(constantTable, nConstants, nPos, nKey, nLocal,
                            hasPosList, maxStackDepth, vmCode, codeSize);
  else
    ypReMatch=
      YpReCompare(reparsing, constantTable, nConstants, nPos, nKey, nLocal,
                  hasPosList, maxStackDepth, vmCode, codeSize);
  nConstants= maxConstants= 0;
  constantTable= 0;

  if (reparsing) return;

  i= parsedFunc->code[0].index;
  oldDB= globTab[i].value.db;
  globTab[i].value.db= (DataBlock *)parsedFunc;
  if (globTab[i].ops==&dataBlockSym) { Unref(oldDB); }
  else globTab[i].ops= &dataBlockSym;

  /* A main function must be pushed onto the stack here;
     anything else (func or struct definitions) is recorded in
     the sourceList for the current include file (see also YpExtern).  */
  if (isMain) PushTask(parsedFunc);
  else parsedFunc->isrc = RecordSource(i);
}

/* ------------------------------------------------------------------------ */

CodeBlock YpReset(void)
{
  return nextPC;
}

/* ------------------------------------------------------------------------ */
