/*
 * $Id: parse.h,v 1.1 2005-09-18 22:04:12 dhmunro Exp $
 *
 * Declare functions required to parse Yorick grammar.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef PARSE_H
#define PARSE_H

#include "plugin.h"

PLUG_API int YpParseInit(void *func);

/* ------------------------------------------------------------------------ */

/* When the graphics package initializes, it calls YpQuine for each
   function whose first nQuined actual parameters are to be quined
   (that is, each of the first nQuined parameters will be followed
   by a parameter which is the literal string that produced the
   parameter).  Potentially, other packages might want this.
   Any such function must ensure that it fails if invoked as a
   function (it must always be called as a subroutine).  (Without this
   restriction, the parser must allow for the possibility of a quined
   call whose parameters are themselves quined calls...)  */
PLUG_API void YpQuine(char *name, int nQuined);

/* ------------------------------------------------------------------------ */

/* The context argument to YpNextLine determines how that routine will
   prompt for input, if input is from a terminal.
   NL_MAIN      -- read first line for parse
   NL_NOINPUT   -- no non-comment input yet,
                     read next line (if file) or return EOF (if terminal)
   NL_CONTINUE  -- need more input to complete program unit (cont>)
   NL_QUOTE     -- inside multi-line quoted string (quot>)
   NL_COMMENT   -- inside multi-line comment (comm>)
 */
PLUG_API char *YpNextLine(int context);
#define NL_MAIN 0
#define NL_NOINPUT 1
#define NL_CONTINUE 2
#define NL_QUOTE 3
#define NL_COMMENT 4

/* YpNextLine will use YaltNextLine instead of its default algorithm,
   if non-0.  YaltNextLine will be zeroed if it ever returns 0 (the
   EOF signal).  It is also zeroed by ClearIncludes().  */
PLUG_API char *(*YaltNextLine)(int context);

PLUG_API int yp_parse_keybd(char *line, int first);

/*--------------------------------------------------------------------------*/

PLUG_API int YpParse(void *func);   /* wrapper for yyparse */

/*--------------------------------------------------------------------------*/

typedef long Quote;
typedef long CodeBlock;
typedef long Literal;
typedef long CBorLit;

#define CB_OR_LITERAL(x,islit) (((x)<<1)|(islit!=0))
#define IS_LITERAL(x) ((x)&1)
#define CBL_VALUE(x) ((x)>>1)

#define NONE (-1L)

/* ------------------------------------------------------------------------ */

extern CodeBlock YpChar(long c);
extern CodeBlock YpShort(long s);
extern CodeBlock YpInt(long i);
extern CodeBlock YpLong(long l);
extern CodeBlock YpFloat(double f);
extern CodeBlock YpDouble(double d);
extern CodeBlock YpString(Quote q);
extern CodeBlock YpImaginary(double d);

extern Literal YpName(char *name, long len);
extern Quote YpQuoteConst(char *q);

extern CodeBlock YpVariable(Literal name);
extern CodeBlock YpQuoted(Literal name);
extern void YpCheckRef(CodeBlock cb);

extern CodeBlock YpPushRF(int which);
/* which is 0-14:
   avg dif max min mnx mxx pcen psum ptp rms sum uncp zcen + -   */

extern CodeBlock YpNil(void);
extern void YpDotDot(int which);
extern void YpKeyword(Literal name, CodeBlock value);

extern CodeBlock YpRange(CodeBlock min, int hasInc);
extern CodeBlock YpRangeFunc(int which, CodeBlock range);
/* which is 0-14:
   avg dif max min mnx mxx pcen psum ptp rms sum uncp zcen + -   */

/* ------------------------------------------------------------------------ */

extern void YpEvalInit(CodeBlock obj);
extern CodeBlock YpEval(CodeBlock obj, int nArgs);
extern CodeBlock YpNextArg(int which);
extern CodeBlock YpBuild(CodeBlock lop, int nArgs);
extern CodeBlock YpMember(CodeBlock obj, int pointer, Literal name);
extern CodeBlock YpPostfix(CBorLit lhs, int assop);

/* Note: 2nd parameter is CBorLit for ++ and -- */
extern CodeBlock YpUnop(int which, CodeBlock op);
/* which is 0-7:
   *  &  +  -  ~  ! ++ --   */
extern CodeBlock YpBinop(CodeBlock lop, int which);
/* which is 0-16:
   ^ * / % + -  << >>  < > <= >= == !=  & ~ |    */
extern CodeBlock YpMultop(CodeBlock lop);
extern CodeBlock YpLogop(CodeBlock lop, int which, CodeBlock rop);
/* which is 0-1:
   && ||   */
extern CodeBlock YpTernop(CodeBlock cond, CodeBlock iftrue, CodeBlock iffalse);

extern CBorLit YpCheckDefine(CodeBlock lhs);
extern CodeBlock YpAssign(CBorLit lhs, CodeBlock rhs);
extern CodeBlock YpIncrement(CodeBlock lhs, int assop, CodeBlock rhs);
/* assop is 0-9:
   += -= *= /= %= <<= >>= &= ~= |=   */

/* ------------------------------------------------------------------------ */

extern void YpBranch(int cond);  /* 0 unconditional, 1 if false, 2 if true */
extern void YpLoop(int type);    /* 0 while, 1 do, 2 for */

extern CodeBlock YpIfElse(CodeBlock cond, CodeBlock ifStmnt,
                          CodeBlock elseStmnt);
extern CodeBlock YpWhile(CodeBlock cond, CodeBlock body);
extern CodeBlock YpDo(CodeBlock body, CodeBlock cond);
extern CodeBlock YpFor(CodeBlock init, CodeBlock test, CodeBlock body);

extern void YpBeginInc(void);
extern void YpEndInc(CodeBlock inc);

extern CodeBlock YpContinue(void);
extern CodeBlock YpBreak(void);

extern CodeBlock YpReturn(CodeBlock value);
extern CodeBlock YpGoto(Literal label);
extern CodeBlock YpLabel(Literal label);

extern int YpCallInit(CodeBlock name);
extern CodeBlock YpCall(CodeBlock name, int nArgs);
extern CodeBlock YpAssignOrPrint(CodeBlock aexpr);

extern void YpExtern(Literal name);
extern void YpLocal(Literal name);
extern void YpSpecial(int which); /* special version of YpExtern for std.i */
/* which is 0-14:
   avg dif max min mnx mxx pcen psum ptp rms sum uncp zcen + -   */

extern CodeBlock YpSyscall(Quote sysline);

extern CodeBlock YpNoop(void);
extern void YpDrop(void);

/* ------------------------------------------------------------------------ */

extern void YpDataType(Literal name);
extern CodeBlock YpDeclarator(int pointer, CodeBlock name, int nDims);
extern void YpStruct(CodeBlock name, int eol);

/* ------------------------------------------------------------------------ */

extern void YpInitFunc(Literal name);
extern void YpPosParam(Literal name, int output);
extern void YpKeyParam(Literal name);
extern void YpHasPosList(void);
extern void YpFunc(int isMain, int eol);

/* ------------------------------------------------------------------------ */

extern int ypErrors;    /* error count for most recent call to YpParse */
extern int ypMaxErrors; /* give up after this many */
extern void YpError(char *msg);
extern CodeBlock YpReset(void);

/* ------------------------------------------------------------------------ */

extern int YpEscapeSeq(const char *s, char **endp);  /* also in yio.h */

extern void YpNoun(char *noun);

/* ------------------------------------------------------------------------ */
/* communication between yinput.c and yorick.c */

extern int ypSkipIncludes;   /* flag set when scanning for var definition */
extern long YpStandby(void); /* used by ScanForFunc in yorick.c */
extern long ScanForFunc(const char *fname, int notExtern);

extern char *YpLitName(Literal);  /* ScanForFunc - parse.c communication */

/* ------------------------------------------------------------------------ */
#endif
