/*
 * $Id: debug.c,v 1.3 2007-04-06 22:04:33 thiebaut Exp $
 *
 * Define Yorick debugging functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "ydata.h"
#include "yio.h"
#include "pstdlib.h"
#include "play.h"
#include <string.h>

extern int YpReCompare(Function *func,
                       Symbol *consts, long nConsts, int nPos, int nKey,
                       int nLocal, long hasPL, int maxStackDepth,
                       Instruction *code, long codeSize);

/* ------------------------------------------------------------------------ */

extern BuiltIn Y_disassemble;
extern int LookupAction(VMaction *Action);
extern int StackChange(int meaning, int count);

/* Better idea:
   Disassemble should not produce text, but rather a "meaning list"
   for the instructions, in which all actions have been converted to
   an index into the "meaning table".  The meaning table can contain
   the Action pointer (or nil for data), the string naming the Action,
   the string for formatting a disassembler line, etc.

   With appropriate low level calls, perhaps the debugging functions
   can be written in the interpreter...
 */
/* ------------------------------------------------------------------------ */

/* range functions */
extern RangeFunc RFmin, RFmax, RFptp, RFsum, RFavg, RFrms, RFmnx, RFmxx,
  RFpsum, RFdif, RFzcen, RFpcen, RFuncp, RFcum;

extern char *GetRFName(RangeFunc *rfTarget);  /* from yio.c */
extern int PutsAsArray(char *s);              /* from yio.c */

extern UnaryOp PrintFN;

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

/* vmMeaning describes a Virtual Machine instruction (type Instruction) */
struct {
  VMaction *Action;  /* address of the Action, or 0 if data */
  char *actionName;  /* name of the Action or type of data */
  char *fullFormat;  /* pcval  sp--spval actionName(data) */

  int flags;         /* OR of following flags */
#define CATEGORY(stack, pc, special) ((stack)|((pc)<<4)|((special)<<8))
#define STACK_CATEGORY(category) ((category)&15)
#define PC_CATEGORY(category) (((category)>>4)&15)
#define SPECIAL_CATEGORY(category) (((category)>>8)&15)

#define STACK_FIXED 0
#define STACK_INC   1
#define STACK_INC2  2
#define STACK_DEC   3
#define STACK_DEC3  4
#define STACK_DECC  5
#define STACK_DECC1 6
#define STACK_DECC2 7

#define PC_UNUSED   0
#define PC_INTEGER  1
#define PC_REAL     2
#define PC_STRING   3
#define PC_INDEX    4
#define PC_COUNT    5
#define PC_DISPLACE 6
#define PC_RF       7

#define IS_NORMAL   0
#define IS_DATA     1
#define IS_ALT_FORM 2

#define N_DATA_TYPES 7
} vmMeaning[]= {
  { 0, "(integer const)", "", CATEGORY(0, PC_INTEGER, IS_DATA) },
  { 0, "(real const)", "", CATEGORY(0, PC_REAL, IS_DATA) },
  { 0, "(string const)", "", CATEGORY(0, PC_STRING, IS_DATA) },
  { 0, "(global index)", "", CATEGORY(0, PC_INDEX, IS_DATA) },
  { 0, "(parameter count)", "", CATEGORY(0, PC_COUNT, IS_DATA) },
  { 0, "(branch offset)", "", CATEGORY(0, PC_DISPLACE, IS_DATA) },
  { 0, "(range function)", "", CATEGORY(0, PC_RF, IS_DATA) },

  { 0, "Halt-Virtual-Machine", "%4ld sp==%-4d %s", 0 }, 

  { &PushChar, "PushChar", "%4ld sp->%-4d %s(0x%02lx)",
      CATEGORY(STACK_INC, PC_INTEGER, 0) },
  { &PushShort, "PushShort", "%4ld sp+>%-4d %s(%ld)",
      CATEGORY(STACK_INC, PC_INTEGER, 0) },
  { &PushInt, "PushInt", "%4ld sp+>%-4d %s(%ld)",
      CATEGORY(STACK_INC, PC_INTEGER, 0) },
  { &PushLong, "PushLong", "%4ld sp+>%-4d %s(%ld)",
      CATEGORY(STACK_INC, PC_INTEGER, 0) },
  { &PushFloat, "PushFloat", "%4ld sp+>%-4d %s(%g)",
      CATEGORY(STACK_INC, PC_REAL, 0) },
  { &PushDouble, "PushDouble", "%4ld sp+>%-4d %s(%g)",
      CATEGORY(STACK_INC, PC_REAL, 0) },
  { &PushImaginary, "PushImaginary", "%4ld sp+>%-4d %s(%gi)",
      CATEGORY(STACK_INC, PC_REAL, 0) },
  { &PushString, "PushString", "%4ld sp+>%-4d %s(\"%s\"%s)",
      CATEGORY(STACK_INC, PC_STRING, 0) },
  { &Push0, "Push0", "%4ld sp+>%-4d %s",
      CATEGORY(STACK_INC, PC_UNUSED, 0) },
  { &Push1, "Push1", "%4ld sp+>%-4d %s",
      CATEGORY(STACK_INC, PC_UNUSED, 0) },
  { &PushVariable, "PushVariable", "%4ld sp+>%-4d %s(%s)",
      CATEGORY(STACK_INC, PC_INDEX, 0) },
  { &PushReference, "PushReference", "%4ld sp+>%-4d %s(%s)",
      CATEGORY(STACK_INC, PC_INDEX, 0) },
  { &PushNil, "PushNil", "%4ld sp+>%-4d %s",
      CATEGORY(STACK_INC, PC_UNUSED, 0) },

  { &FormKeyword, "FormKeyword", "%4ld sp+>%-4d %s(%s)",
      CATEGORY(STACK_INC, PC_INDEX, 0) },
  { &FormRange, "FormRange", "%4ld sp->%-4d %s(%d)",
      CATEGORY(STACK_DECC1, PC_COUNT, 0) },
  { &FormRangeFunc, "FormRangeFunc", "%4ld sp+>%-4d %s(%s)",
      CATEGORY(STACK_INC, PC_RF, 0) },
  { &FormRangeFlag, "FormRangeFlag", "%4ld sp+>%-4d %s(%s)",
      CATEGORY(STACK_INC, PC_COUNT, IS_ALT_FORM) },
  { &AddRangeFunc, "AddRangeFunc", "%4ld sp0>%-4d %s(%s)",
      CATEGORY(STACK_FIXED, PC_RF, 0) },
  { &AddRangeFlag, "AddRangeFlag", "%4ld sp0>%-4d %s(%s)",
      CATEGORY(STACK_FIXED, PC_COUNT, IS_ALT_FORM) },

  { &Eval, "Eval", "%4ld sp->%-4d %s(%d)",
      CATEGORY(STACK_DECC, PC_COUNT, 0) },
  { &Eval2, "Eval2", "%4ld sp->%-4d %s(%d)",
      CATEGORY(STACK_DECC1, PC_COUNT, 0) },
  { &GetMember, "GetMember", "%4ld sp0>%-4d %s(%s%s)",
      CATEGORY(STACK_FIXED, PC_STRING, 0) },
  { &DerefMember, "DerefMember", "%4ld sp0>%-4d %s(%s%s)",
      CATEGORY(STACK_FIXED, PC_STRING, 0) },
  { &Deref, "Deref", "%4ld sp0>%-4d %s",
      CATEGORY(STACK_FIXED, PC_UNUSED, 0) },

  { &Address, "Address", "%4ld sp0>%-4d %s",
      CATEGORY(STACK_FIXED, PC_UNUSED, 0) },

  { &Negate, "Negate", "%4ld sp0>%-4d %s",
      CATEGORY(STACK_FIXED, PC_UNUSED, 0) },
  { &Complement, "Complement", "%4ld sp0>%-4d %s",
      CATEGORY(STACK_FIXED, PC_UNUSED, 0) },
  { &Not, "Not", "%4ld sp0>%-4d %s",
      CATEGORY(STACK_FIXED, PC_UNUSED, 0) },
  { &True, "True", "%4ld sp0>%-4d %s",
      CATEGORY(STACK_FIXED, PC_UNUSED, 0) },

  { &Power, "Power", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &Multiply, "Multiply", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &Divide, "Divide", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &Modulo, "Modulo", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &Add, "Add", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &Subtract, "Subtract", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &ShiftL, "ShiftL", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &ShiftR, "ShiftR", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &Less, "Less", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &Greater, "Greater", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &LessEQ, "LessEQ", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &GreaterEQ, "GreaterEQ", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &Equal, "Equal", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &NotEqual, "NotEqual", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &AndBits, "AndBits", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &XorBits, "XorBits", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &OrBits, "OrBits", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },

  { &AndOrLogical, "AndOrLogical", "%4ld sp==%-4d %s for %s",
      CATEGORY(STACK_DEC, PC_UNUSED, IS_ALT_FORM) },

  { &Define, "Define", "%4ld sp0>%-4d %s(%s)",
      CATEGORY(STACK_FIXED, PC_INDEX, 0) },
  { &Assign, "Assign", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },
  { &DupUnder, "DupUnder", "%4ld sp+>%-4d %s",
      CATEGORY(STACK_INC, PC_UNUSED, 0) },
  { &EvalUnder, "EvalUnder", "%4ld sp+>%-4d %s",
      CATEGORY(STACK_INC2, PC_UNUSED, 0) },

  { &DropTop, "DropTop", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },

  { &BranchFalse, "BranchFalse", "%4ld sp->%-4d %s to pc= %ld",
      CATEGORY(STACK_DEC, PC_DISPLACE, 0) },
  { &BranchTrue, "BranchTrue", "%4ld sp->%-4d %s to pc= %ld",
      CATEGORY(STACK_DEC, PC_DISPLACE, 0) },
  { &Branch, "Branch", "%4ld sp0>%-4d %s to pc= %ld",
      CATEGORY(STACK_FIXED, PC_DISPLACE, 0) },
  { &Return, "Return", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },

  { &OpenStruct, "OpenStruct", "%4ld sp+>%-4d %s(%s)",
      CATEGORY(STACK_INC, PC_INDEX, 0) },
  { &DeclareMember, "DeclareMember", "%4ld sp->%-4d %s(%d)",
      CATEGORY(STACK_DECC2, PC_COUNT, 0) },
  { &CloseStruct, "CloseStruct", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC, PC_UNUSED, 0) },

  { &MatrixMult, "MatrixMult", "%4ld sp->%-4d %s",
      CATEGORY(STACK_DEC3, PC_UNUSED, 0) },
  { &Build, "Build", "%4ld sp->%-4d %s(%d)",
      CATEGORY(STACK_DECC1, PC_COUNT, 0) },
  { &CallShell, "CallShell", "%4ld sp0>%-4d %s(\"%s\"%s)",
      CATEGORY(STACK_FIXED, PC_STRING, 0) },
  { &Print, "Print", "%4ld sp0>%-4d %s",
      CATEGORY(STACK_FIXED, PC_UNUSED, 0) },
  { &NextArg, "NextArg", "%4ld sp+>%-4d %s",
      CATEGORY(STACK_INC, PC_INDEX, IS_ALT_FORM) },
  { &MoreArgs, "MoreArgs", "%4ld sp+>%-4d %s",
      CATEGORY(STACK_INC, PC_INDEX, IS_ALT_FORM) },

  { 0, "(illegal action)", "***** ILLEGAL ACTION *****", 0 }
};

int LookupAction(VMaction *Action)
{
  int i= N_DATA_TYPES;
  if (!Action) return i;
  while (vmMeaning[++i].Action) if (vmMeaning[i].Action==Action) break;
  return i;
}

static int stackDelta[]= { 0, 1, 2, -1, -3, 0, 1, -2 };

int StackChange(int meaning, int count)
{
  int sc= STACK_CATEGORY(vmMeaning[meaning].flags);
  int delta= stackDelta[sc];
  if (sc>=STACK_DECC) delta-= count;
  return delta;
}

/* Action categories:
   C_PUSH_CONST      ++stack  (pc++)->constant->value.(l,d,db)
     (char, short, int, long) (float, double, imaginary) (string)
   C_PUSH_VAR        ++stack  globalTable.names[(pc++)->index]
     (variable, reference, formkeyword, openstruct) (nextargs, moreargs)
   (formrangefunc)   ++stack  GetRFName((pc++)->rf)
   (formrangeflag)   ++stack  (pc++)->count (as flag)
   C_PUSH_VAL        ++stack  pc
     (0, 1, nil, dupunder)
   (evalunder)       stack+=2 pc

   (addrangefunc)    stack    GetRFName((pc++)->rf)
   (addrangeflag)    stack    (pc++)->count (as flag)
   C_MEMBER          stack    (pc++)->constant->value.db->value.q[0]
     (getmember, derefmember) (callshell)
   (branch)          stack    (pc++)->displace
   (define)          stack    globalTable.names[(pc++)->index]
   C_UNARY           stack    pc
     (deref, address, negate, complement, not, true, print)

   C_BINARY          --stack  pc
     (power, multiply, divide, modulo, add, subtract, shiftl, shiftr,
      less, greater, lesseq, greatereq, equal, notequal, andbits,
      xorbits, orbits, assign, droptop, return, closestruct)
   (andorlogical)    --stack  pc
   C_BRANCH          --stack  (pc++)->displace
     (branchfalse, branchtrue)

   (matrixmult)      stack-=3 pc

   C_BUILD           stack-=count-1  (pc++)->count
     (formrange, eval2, build)
   C_EVAL            stack-=count    (pc++)->count
     (eval)
   (declaremember)   stack-=count+2  (pc++)->count

format strings:
 "%4ld sp==%-4d %s"           (halt)
 "%4ld sp+>%-4d %s"           (unused) (index, nextargs and moreargs)
 "%4ld sp0>%-4d %s"
 "%4ld sp->%-4d %s"
 "%4ld sp==%-4d %s for %s"    (unused, andorlogical)

 "%4ld sp+>%-4d %s(0x%02lx)"  (long)
 "%4ld sp+>%-4d %s(%ld)"

 "%4ld sp+>%-4d %s(%g)"       (real)
 "%4ld sp+>%-4d %s(%gi)"

 "%4ld sp+>%-4d %s(\"%s\"%s)" (string)
 "%4ld sp0>%-4d %s(%s)"
 "%4ld sp0>%-4d %s(\"%s\"%s)"

 "%4ld sp0>%-4d %s(%s)"       (index)
 "%4ld sp+>%-4d %s(%s)"       (index) (rf) (count as flag)
 "%4ld sp0>%-4d %s(%s)"       (rf) (count as flag)

 "%4ld sp->%-4d %s(%d)"       (count)

 "%4ld sp->%-4d %s to pc= %ld"  (displace)
 "%4ld sp0>%-4d %s to pc= %ld"

 */

/* ------------------------------------------------------------------------ */

static char lineBuf[256];
static char stringBuf[32];

static int GetCString(char *q);
static int GetCString(char *q)
{
  char *s= ScanForEscape(q);
  long n, i= 0;
  while ((n= 24-i) > 0) {
    if (!*s || s-q >= n) {
      strncpy(stringBuf+i, q, n);
      stringBuf[i+n]= '\0';
      q= (s-q > n)? q+n : s;
      break;
    }
    strncpy(stringBuf+i, q, s-q);
    i+= s-q;
    i+= AddEscapeSeq(stringBuf+i, (int)(*s));
    q= s+1;
    s= ScanForEscape(q);
  }
  return (*q != 0);
}

static Instruction *PrintInstruction(int iAction, int category,
                                     Instruction *pc, long ipc, int stack);

void Y_disassemble(int nArgs)
{
  Function *f;
  Instruction *pc;
  VMaction *Action;
  int stack, iAction, category;
  long ipc;
  Operand op;   /* PrintFN uses only op.value */

  if (nArgs > 1) YError("disassemble takes a single argument");
  if (nArgs==1 && sp->ops==&referenceSym) ReplaceRef(sp);
  f= nArgs==1? (Function *)sp->value.db : 0;
  if (!f || sp->ops!=&dataBlockSym || f->ops!=&functionOps) {
    if (!f || f->ops==&voidOps) {
      if (!HashFind(&globalTable, "*main*", 0L) ||
          globTab[hashIndex].ops!=&dataBlockSym ||
          (f= (Function *)globTab[hashIndex].value.db)->ops!=&functionOps)
        YError("no *main* for disassemble");
    } else {
      YError("disassemble argument is not a function");
    }
  }
  op.value= f;

  if (CalledAsSubroutine()) {
    PrintInit(YputsOut);
    PushDataBlock(RefNC(&nilDB));
  } else {
    PrintInit(&PutsAsArray);
    PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  }

  PrintFN(&op);
  ForceNewline();

  /* skip past local variables */
  pc= f->code;
  pc+= 1+f->nPos+(f->hasPosList&1)+f->nKey+f->nLocal;

  stack= 0;
  ipc= pc - f->code;
  while ((Action= (pc++)->Action)) {
    iAction= LookupAction(Action);
    if (!vmMeaning[iAction].Action) break;
    category= vmMeaning[iAction].flags;
    stack+= StackChange(iAction, pc->count);

    pc= PrintInstruction(iAction, category, pc, ipc, stack);
    if (Action==&Branch && stack>0) {
      /* This must be ? ... <this branch>: ... construction.
         The stack is now one deeper than it was at the ? branch,
         so we need to decrement it here...  */
      stack--;
    }

    PrintFunc(lineBuf);
    ForceNewline();
    ipc= pc - f->code;
  }

  if (Action) {
    PrintFunc("********** Blows up on unknown pc->Action **************");
  } else {
    iAction= LookupAction(Action);
    sprintf(lineBuf, vmMeaning[iAction].fullFormat, ipc, stack,
            vmMeaning[iAction].actionName);
    PrintFunc(lineBuf);
  }
  ForceNewline();
}

static Instruction *PrintInstruction(int iAction, int category,
                                     Instruction *pc, long ipc, int stack)
{
  long displace;
  int flag;

  switch PC_CATEGORY(category) {
  case PC_UNUSED:
    if (SPECIAL_CATEGORY(category)==IS_ALT_FORM) {
      flag= (pc->Action==&Push0);
      sprintf(lineBuf, vmMeaning[iAction].fullFormat, ipc, stack,
              vmMeaning[iAction].actionName, flag? "&&" : "||");
    } else {
      sprintf(lineBuf, vmMeaning[iAction].fullFormat, ipc, stack,
              vmMeaning[iAction].actionName);
    }
    break;
  case PC_INTEGER:
    sprintf(lineBuf, vmMeaning[iAction].fullFormat, ipc, stack,
            vmMeaning[iAction].actionName, (pc++)->constant->value.l);
    break;
  case PC_REAL:
    sprintf(lineBuf, vmMeaning[iAction].fullFormat, ipc, stack,
            vmMeaning[iAction].actionName, (pc++)->constant->value.d);
    break;
  case PC_STRING:
    flag= GetCString(((Array *)(pc++)->constant->value.db)->value.q[0]);
    sprintf(lineBuf, vmMeaning[iAction].fullFormat, ipc, stack,
            vmMeaning[iAction].actionName, stringBuf, (flag? "..." : ""));
    break;
  case PC_INDEX:
    if (SPECIAL_CATEGORY(category)==IS_ALT_FORM) {
      sprintf(lineBuf, vmMeaning[iAction].fullFormat, ipc, stack,
              vmMeaning[iAction].actionName);
      pc++;                     /* index of *va* variable */
    } else {
      sprintf(lineBuf, vmMeaning[iAction].fullFormat, ipc, stack,
              vmMeaning[iAction].actionName,
              globalTable.names[(pc++)->index]);
    }
    break;
  case PC_COUNT:
    if (SPECIAL_CATEGORY(category)==IS_ALT_FORM) {
      flag= (pc++)->count;
      sprintf(lineBuf, vmMeaning[iAction].fullFormat, ipc, stack,
              vmMeaning[iAction].actionName,
              flag==R_MARKED? "+:" : flag==R_PSEUDO? "-:" : "..");
    } else {
      sprintf(lineBuf, vmMeaning[iAction].fullFormat, ipc, stack,
              vmMeaning[iAction].actionName, (pc++)->count);
    }
    break;
  case PC_DISPLACE:
    displace= (pc++)->displace;
    sprintf(lineBuf, vmMeaning[iAction].fullFormat, ipc, stack,
            vmMeaning[iAction].actionName, ipc+1+displace);
    break;
  case PC_RF:
    sprintf(lineBuf, vmMeaning[iAction].fullFormat, ipc, stack,
            vmMeaning[iAction].actionName, GetRFName((pc++)->rf));
    break;
  }

  return pc;
}

/* ------------------------------------------------------------------------ */

extern long *ypReList, nYpReList;
extern int ypReMatch;

/* Returns 1 if the existing function func matches its reparse (from YpFunc).
   This result is stored in the global variable ypReMatch by YpFunc.
   The line number data is nYpReList values in ypReList, alternating
   a pc (relative to frameSize) and its corresponding line number.
   As a side effect, the pc values (even elements) of ypReList are
   adjusted to reflect their values in the finished function.  */
int YpReCompare(Function *func,
                Symbol *consts, long nConsts, int nPos, int nKey,
                int nLocal, long hasPL, int maxStackDepth,
                Instruction *code, long codeSize)
{
  int match;
  long frameSize= 1+nPos+(hasPL&1)+nKey+nLocal;
  Instruction *fcode= &func->code[frameSize];
  Symbol *fconsts= func->constantTable;
  long i;
  OpTable *ops;

  /* adjust ypReList pc values to be absolute */
  for (i=0 ; i<nYpReList ; i+=2) ypReList[i]+= frameSize;

  match= (func->nPos==nPos && func->nKey==nKey && func->nLocal==nLocal &&
          func->hasPosList==hasPL && func->nConstants==nConsts &&
          func->nReq==frameSize+maxStackDepth+10);

  if (match) {
    codeSize-= frameSize-1;
    /* YpFunc puts the frame variables (parameters and locals) at the end
       of the code.  Also, when reparsing, Instruction.constant values were
       filled in relative to func->constantTable.  */
    for (i=0 ; i<codeSize ; i++) if (fcode[i].Action!=code[i].Action) break;
    match= (match && i>=codeSize);
    if (match) {
      code+= codeSize;
      fcode= func->code;
      for (i=0 ; i<frameSize ; i++) if (fcode[i].Action!=code[i].Action) break;
      match= (match && i>=frameSize);
    }
  }

  /* check the constant table, then discard it */
  for (i=0 ; i<nConsts ; i++) {
    ops= consts[i].ops;
    match= (match && ops==fconsts->ops);
    if (ops==&dataBlockSym) {
      if (match) {
        char *q1= ((Array *)consts[i].value.db)->value.q[0];
        char *q2= ((Array *)fconsts->value.db)->value.q[0];
        match= (q1==0 || q2==0)? q1==q2 : strcmp(q1,q2)==0;
      }
      Unref(consts[i].value.db);
    } else if (match) {
      if (ops==&longScalar)
        match= (consts[i].value.l==fconsts->value.l);
      else if (ops==&doubleScalar)
        match= (consts[i].value.d==fconsts->value.d);
    }
    if (match) fconsts++;
  }
  p_free(consts);

  return match;
}

/* ------------------------------------------------------------------------ */
/* Define opaque object DebugBlk as a foreign Yorick data type.  */

typedef struct DebugBlk DebugBlk;
struct DebugBlk {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  Function *func;      /* the function being debugged --
                          NO increment of func->references, since on stack */
  long pcerr;          /* pc at which error was detected */
  char *errFile;       /* full filename in which source resides */
  long *pcList;        /* list of pc values where stack is empty */
  long nPClist;        /* length of pcEmpty list */
  long iPClist;        /* current position in pcList */
  long *pcLine;        /* pc/line number data from YpReParse */
  long nPCline;        /* length of pcLine array */
  int changed;         /* 1 if source has definitely changed */
  Symbol *retValue;    /* temporary pointer to stack element for dbret */
  long lenCode;        /* total length of func->code */
};

extern BuiltIn Y_dbexit, Y_dbcont, Y_dbret, Y_dbskip, Y_dbup;
extern BuiltIn Y_dbinfo, Y_dbdis, Y_dbauto, Y_dbwhere;

extern Instruction *AbortReturn(void);
extern int yAutoDebug;
extern char *MakeErrorLine(long lineNumber, const char *filename);

extern DebugBlk *NewDebugBlk(Function *f, long pcerr, char *errFile,
                             long lnum);
extern void FreeDebugBlk(void *dbug);

static long FindErrLine(DebugBlk *dbg);
static DebugBlk *FindDebugBlk(void);
static void AnnouncePC(DebugBlk *dbg, int dontPush);

static UnaryOp PrintDBG;

extern Operations debugOps;
Operations debugOps = {
  &FreeDebugBlk, T_OPAQUE, 0, T_STRING, "Debug-Block",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &EvalX, &SetupX, &GetMemberX, &MatMultX, &PrintDBG
};

DebugBlk *NewDebugBlk(Function *f, long pcerr, char *errFile, long lnum)
{
  DebugBlk *dbg= (DebugBlk *)p_malloc(sizeof(DebugBlk));
  long frameSize= 1+f->nPos+(f->hasPosList&1)+f->nKey+f->nLocal;
  Instruction *pc= &f->code[frameSize];
  VMaction *Action;
  int stack, iAction;
  long n, i, ipc;

  dbg->references= 0;
  dbg->ops= &debugOps;
  dbg->retValue= 0;

  dbg->func= f;         /* NOT NOT NOT Ref(f) */
  dbg->errFile= p_strcpy(errFile);

  /* steal the ypReList and ypReMatch value on the assumption that
     YpReParse has just been called.  */
  if (errFile && !lnum) {
    dbg->nPCline= nYpReList;
    nYpReList= 0;
    dbg->pcLine= ypReList;
    ypReList= 0;
    dbg->changed= !ypReMatch;
  } else {
    dbg->nPCline= 0;
    dbg->pcLine= 0;
    dbg->changed= 0;
  }

  /* count number of logical lines (where stack drops to zero) */
  pcerr--;  /* also try to zero in on pc where error occurred --
               pc was incremented in YRun before calling Action */
  n= 1;
  stack= 0;
  while ((Action= (pc++)->Action)) {
    iAction= LookupAction(Action);
    if (!vmMeaning[iAction].Action) break;
    stack+= StackChange(iAction, pc->count);
    if (stack==0 && Action!=&Branch) n++;
    if (PC_CATEGORY(vmMeaning[iAction].flags)!=PC_UNUSED) {
      if (pc==&f->code[pcerr]) pcerr--;
      pc++;
    }
    if (Action==&Branch && stack>0) {
      /* This must be ? ... <this branch>: ... construction.
         The stack is now one deeper than it was at the ? branch,
         so we need to decrement it here...  */
      stack--;
    }
  }
  dbg->pcerr= pcerr;

  /* allocate list of pc values where stack goes to zero */
  dbg->pcList= (long *)p_malloc(sizeof(long)*n);
  dbg->nPClist= n;

  /* set values of pc where stack goes to zero */
  dbg->pcList[0]= frameSize;
  i= 0;
  n= 1;
  pc= &f->code[frameSize];
  stack= 0;
  while ((Action= (pc++)->Action)) {
    iAction= LookupAction(Action);
    if (!vmMeaning[iAction].Action) break;
    stack+= StackChange(iAction, pc->count);
    if (PC_CATEGORY(vmMeaning[iAction].flags)!=PC_UNUSED) pc++;
    if (stack==0 && Action!=&Branch) {
      ipc= pc - f->code;
      if (ipc < pcerr) i= n;
      dbg->pcList[n++]= ipc;
    }
    if (Action==&Branch && stack>0) {
      /* This must be ? ... <this branch>: ... construction.
         The stack is now one deeper than it was at the ? branch,
         so we need to decrement it here...  */
      stack--;
    }
  }
  dbg->iPClist= i;

  if (Action) {
    /* The Function has been corrupted -- don't try to debug it.  */
    FreeDebugBlk(dbg);
    return 0;
  }
  dbg->lenCode= pc - f->code;

  /* print line number and file, and maybe a warning */
  if (errFile && dbg->pcLine) {
    if (dbg->changed)
      YputsErr("WARNING source code has changed since function was parsed");
    YputsErr(MakeErrorLine(FindErrLine(dbg), dbg->errFile));
  } else {
    sprintf(lineBuf, "now at pc= %ld (of %ld), failed at pc= %ld",
            dbg->pcList[dbg->iPClist], dbg->lenCode, dbg->pcerr);
    if (errFile)
      YputsErr("WARNING detailed line number information unavailable");
    else
      YputsErr("WARNING source code unavailable (try dbdis function)");
    YputsErr(lineBuf);
    if (errFile)
      YputsErr(MakeErrorLine(lnum, dbg->errFile));
  }

  yDebugLevel++;
  return dbg;
}

void FreeDebugBlk(void *dbug)
{
  DebugBlk *dbg= dbug;
  Symbol *retValue= dbg->retValue;
  p_free(dbg->errFile);
  dbg->errFile= 0;
  p_free(dbg->pcList);
  dbg->pcList= 0;
  p_free(dbg->pcLine);
  dbg->pcLine= 0;
  dbg->retValue= 0;
  if (retValue && retValue->ops==&dataBlockSym) Unref(retValue->value.db);
  p_free(dbg);
  yDebugLevel--;
  if (yDebugLevel<0) yDebugLevel= 0;
}

static void PrintDBG(Operand *op)
{
  /* DebugBlk *dbg= op->value; */
  ForceNewline();
  PrintFunc("<debug block>");
  ForceNewline();
}

/* ------------------------------------------------------------------------ */

static long FindErrLine(DebugBlk *dbg)
{
  long *pcLine= dbg->pcLine;
  long nPCline= dbg->nPCline;
  long pc= dbg->pcList[dbg->iPClist];
  long i;
  if (!pcLine || nPCline<=0) return 0;
  for (i=2 ; i<nPCline ; i+=2) if (pcLine[i]>pc) break;
  return pcLine[i-1];
}

static DebugBlk *FindDebugBlk(void)
{
  Symbol *stack= sp;
  while (stack>spBottom &&
         (stack->ops!=&dataBlockSym || stack->value.db->ops!=&debugOps))
    stack--;
  if (stack<=spBottom) return 0;
  return (DebugBlk *)stack->value.db;
}

static void AnnouncePC(DebugBlk *dbg, int dontPush)
{
  if (dontPush==1) {
    PrintInit(YputsOut);
    PushDataBlock(RefNC(&nilDB));
  } else if (dontPush==0) {
    PrintInit(&PutsAsArray);
    PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  }

  sprintf(lineBuf, "now at pc= %ld (of %ld)",
          dbg->pcList[dbg->iPClist], dbg->lenCode);
  PrintFunc(lineBuf);
  ForceNewline();
  if (dbg->errFile && dbg->pcLine) {
    PrintFunc(MakeErrorLine(FindErrLine(dbg), dbg->errFile));
    ForceNewline();
  }
}

/* ------------------------------------------------------------------------ */

void Y_dbexit(int nArgs)
{
  extern void YHalt(void);
  extern void yg_got_expose(void);
  extern void yr_reset(void);
  extern char *y_read_prompt;
  long n= 1;    /* default is to back out of 1 level */
  if (nArgs>1) YError("dbexit takes 0 or 1 argument (number of levels)");
  if (nArgs==1) n= YGetInteger(sp);
  if (n<=0 || y_read_prompt!=0) {
    /* Clear stack completely -- wipes out all levels of debug blocks.  */
    ResetStack(1);
  } else {
    /* First, clear stack back to current debug block.  */
    ResetStack(0);
    while (n--) {
      /* Next, eliminate everything back to previous debug block.  */
      ResetStack(0);
      if (sp==spBottom) break; /* gone about as fer as we can go, yes sir */
    }
  }
  yr_reset();
  yg_got_expose();  /* in case window,wait=1 or pause */
  p_clr_alarm(0, 0);
  YHalt();  /* the *main* that called this is gone -- halt VM */
}

void Y_dbcont(int nArgs)
{
  Instruction *pcResume;
  DebugBlk *dbg;
  if (nArgs>0) YError("dbcont takes no argument");
  /* First, clear stack back to current debug block.  */
  ResetStack(0);
  /* Check for debug block.  */
  if (sp->ops!=&dataBlockSym || sp->value.db->ops!=&debugOps)
    YError("stack corrupted -- dbcont cannot find debug block");
  dbg= (DebugBlk *)sp->value.db;
  pcResume= dbg->func->code + dbg->pcList[dbg->iPClist];
  Drop(1);           /* drop current debug block... */
  pc= pcResume;      /* ...and continue */
}

void Y_dbret(int nArgs)
{
  Symbol *stack;
  Instruction *pcResume;
  DebugBlk *dbg= FindDebugBlk();
  if (!dbg) YError("dbret cannot find debug block");
  if (nArgs>1) YError("dbret takes 0 or 1 argument");
  if (nArgs<1) PushDataBlock(RefNC(&nilDB));

  /* temporarily remove return value from top of stack --
     save pointer back to it as a failsafe */
  stack= sp--;
  dbg->retValue= stack;
  /* then clear stack back to current debug block --
     this always clears at least the main program off the stack */
  ResetStack(0);
  /* check for debug block */
  if (sp->ops!=&dataBlockSym || sp->value.db!=(DataBlock *)dbg)
    YError("stack corrupted -- dbret cannot find debug block");
  pcResume= dbg->func->code + dbg->pcList[dbg->iPClist];

  /* move debug block up on stack, and copy return value under it */
  sp[1].ops= &dataBlockSym;
  sp[1].value.db= (DataBlock *)dbg;
  sp[0].ops= &intScalar;
  sp++;
  sp[-1].value= stack->value;
  dbg->retValue= 0;
  sp[-1].ops= stack->ops;

  Drop(1);           /* drop current debug block... */

  /* set pc to value it would have on dbcont --
     this way, if there is an error during the return, it will have
     a sensible value */
  pc= pcResume;
  /* since the return value is at the top of the stack, and the
     return Symbol is directly under it, the virtual machine can be
     fooled into doing an ordinary return operation */
  Return();
}

extern Instruction *yErrorPC;

void Y_dbup(int nArgs)
{
  if (nArgs>0) YError("dbup takes no arguments");

  /* clear stack back to current debug block --
     this always clears at least the main program off the stack */
  ResetStack(0);
  /* check for debug block */
  if (sp->ops!=&dataBlockSym || sp->value.db->ops!=&debugOps)
    YError("stack corrupted -- dbup cannot find debug block");

  Drop(1);           /* drop current debug block... */

  yErrorPC= AbortReturn();
  YError("<not an error -- ignored>");
}

void Y_dbskip(int nArgs)
{
  long n= 1;   /* default number of lines to skip */
  long i;
  DebugBlk *dbg= FindDebugBlk();
  if (!dbg) YError("dbskip cannot find debug block");
  if (nArgs>1) YError("dbskip takes at most one argument");
  if (nArgs==1) n= YGetInteger(sp);
  i= dbg->iPClist + n;
  if (i<0) i= 0;
  else if (i>=dbg->nPClist) i= dbg->nPClist-1;
  dbg->iPClist= i;
  AnnouncePC(dbg, CalledAsSubroutine()? 1 : 0);
}

/* ------------------------------------------------------------------------ */

void Y_dbinfo(int nArgs)
{
  DebugBlk *dbg= FindDebugBlk();
  Operand op;
  if (!dbg) YError("dbinfo cannot find debug block");

  if (CalledAsSubroutine()) {
    PrintInit(YputsOut);
    PushDataBlock(RefNC(&nilDB));
  } else {
    PrintInit(&PutsAsArray);
    PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  }

  op.value= dbg->func;   /* PrintFN only uses this member of op */
  PrintFN(&op);
  ForceNewline();
  sprintf(lineBuf, "Current debug level is: %d", yDebugLevel);
  PrintFunc(lineBuf);
  ForceNewline();

  AnnouncePC(dbg, 2);
}

void Y_dbdis(int nArgs)
{
  Function *f;
  Instruction *pc;
  VMaction *Action;
  int stack, iAction, category, lastPass;
  long ipc, ipc0;
  DebugBlk *dbg= FindDebugBlk();
  if (!dbg) YError("dbdis cannot find debug block");

  f= dbg->func;
  pc= f->code + dbg->pcList[dbg->iPClist];

  if (CalledAsSubroutine()) {
    PrintInit(YputsOut);
    PushDataBlock(RefNC(&nilDB));
  } else {
    PrintInit(&PutsAsArray);
    PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  }

  lastPass= 0;
  stack= 0;
  ipc= ipc0= pc - f->code;
  while ((Action= (pc++)->Action)) {
    iAction= LookupAction(Action);
    if (!vmMeaning[iAction].Action) break;
    category= vmMeaning[iAction].flags;
    stack+= StackChange(iAction, pc->count);

    pc= PrintInstruction(iAction, category, pc, ipc, stack);
    if (lastPass) break;
    if (stack<=0) lastPass= 1;
    if (Action==&Branch && stack>0) {
      /* This must be ? ... <this branch>: ... construction.
         The stack is now one deeper than it was at the ? branch,
         so we need to decrement it here...  */
      stack--;
    }

    PrintFunc(lineBuf);
    ForceNewline();
    ipc= pc - f->code;
  }

  if (dbg->pcerr>ipc0 && dbg->pcerr<=ipc) {
    sprintf(lineBuf, "***the error occurred near pc= %ld", dbg->pcerr);
    PrintFunc(lineBuf);
    ForceNewline();
  }
}

void Y_dbauto(int nArgs)
{
  long n= 2;
  if (nArgs>1) YError("dbauto takes at most one argument");
  if (nArgs==1) n= YGetInteger(sp);
  if (n==1) yAutoDebug= 1;
  else if (n==2) yAutoDebug= !yAutoDebug;
  else yAutoDebug= 0;
}

/* ------------------------------------------------------------------------ */

static Function *ydb_funcof(Instruction *pc);  /* see task.c:FuncContaining */

void
Y_dbwhere(int nArgs)
{
  extern void YHalt(void);
  Symbol *stack = sp;
  Function *f = ydb_funcof(pc);

  if (CalledAsSubroutine()) {
    PrintInit(YputsOut);
    PushDataBlock(RefNC(&nilDB));
  } else {
    PrintInit(&PutsAsArray);
    PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  }

  if (f) {
    if (f->code->index < 0) PrintFunc("*anon*");
    else PrintFunc(globalTable.names[f->code->index]);
  } else {
    PrintFunc("*lost*");
  }
  /* pc[-2] must equal &Eval that created this returnSym */
  sprintf(lineBuf, "[%ld]", f? (long)(pc-f->code)-2L : 0L);
  PrintFunc(lineBuf);
  ForceNewline();

  for (; stack>spBottom ; stack--) {
    if (stack->ops == &returnSym) {
      if (stack->value.pc->Action == &YHalt)
        continue;  /* crucial to detect and avoid task.c:taskCode */
      f = ydb_funcof(stack->value.pc);
      if (f) {
        if (f->code->index < 0) PrintFunc("*anon*");
        else PrintFunc(globalTable.names[f->code->index]);
      } else {
        PrintFunc("*lost*");
      }
      /* pc[-2] must equal &Eval that created this returnSym */
      sprintf(lineBuf, "[%ld]", f? (long)(stack->value.pc-f->code)-2L : 0L);
      PrintFunc(lineBuf);
      ForceNewline();
    } else if (stack->ops==&dataBlockSym && stack->value.db->ops==&debugOps) {
      PrintFunc("*dbug*[0]");
      ForceNewline();
    }
  }
}

#include <stddef.h>
static Function *
ydb_funcof(Instruction *pc)
{
  Function *f = 0;
  if (pc) {
    long i = -1;
    for (;; i++) {
      while (pc[i].Action) i++;
      if (pc[i-1].Action==&Return) break;
    }
    i++;
    /* Now pc[i] is the Instruction generated by following line
       in parse.c (YpFunc):
          vmCode[nextPC].index= codeSize= nPos+nKey+nLocal+ nextPC;
       (nextPC does NOT include the parameters or locals)
     */
    i -= pc[i].index;
    if (i<0) f = (Function *)((char *)(pc+i) - offsetof(Function, code));
  }
  return f;
}
