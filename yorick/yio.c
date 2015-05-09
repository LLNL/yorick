/*
 * $Id: yio.c,v 1.4 2010-02-28 21:32:23 dhmunro Exp $
 * Implement Yorick I/O functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "yio.h"
#include "defmem.h"

#include "ydata.h"
#include "pstdlib.h"
#include "play.h"

#include <string.h>

/* Print action defined here */
extern VMaction Print;

extern UnaryOp EvalFN, EvalBI, eval_auto;   /* required by Print */
extern BuiltIn Y_print, Y_print_format, Y_rangeof;

/* required by print() called as function */
extern Array *GrowArray(Array *array, long extra);

extern char *MakeErrorLine(long lineNumber, const char *filename);

/* Non-blocking input tester defined in sysdep.c */
extern int YstdinNB(int noWait);

/*--------------------------------------------------------------------------*/

extern char *GetRFName(RangeFunc *rfTarget);

extern long ReopenSource(long index, int notExtern, long isrc);
extern int YpParse(void *func);

/*--------------------------------------------------------------------------*/

/* Yorick wrappers for fgets, feof, and ferror.  */

char *Yfgets(char *s, int n, p_file *stream)
{
  if (stream) return p_fgets(stream, s, n);
  YError("(BUG) Yfgets with stream==0"); return 0;
}

int Yfeof(p_file *stream)
{
  if (stream) return p_feof(stream);
  YError("(BUG) Yfeof with stream==0"); return 0;
}

int Yferror(p_file *stream)
{
  if (stream) return p_ferror(stream);
  YError("(BUG) Yferror with stream==0"); return 0;
}

int YDPrompt(char *s)
{
  p_stdout(s);
  return 0;
}

int YDputsOut(char *s)
{
  p_stdout(s);
  p_stdout("\n");
  return 0;
}

int YDputsErr(char *s)
{
  p_stderr(s);
  p_stderr("\n");
  return 0;
}

int (*YPrompt)(char *s)= &YDPrompt;
int (*YputsOut)(char *s)= &YDputsOut;
int (*YputsErr)(char *s)= &YDputsErr;

/*--------------------------------------------------------------------------*/
/* Yorick interface to gets() allows for virtually unlimited length lines.
   The price is the necessity of passing a struct* instead of a simple
   char*...  The case of file==0 is special -- file==0 means "stdin",
   but Yorick allows for the possibility of more complex arrangements
   for "stdin", including the possibility that keyboard input stream
   is not coming from a standard C library input stream at all (it could
   come, e.g., from some sort of X window message).  */
#define MIN_LINE 256
#define BIG_LINE 1024
#define MAX_LINE 16384
#define INC_LINE 256

char *
Ygets(YgetsLine *getsLine, p_file *file)
{
  char *line = getsLine->line;
  int ateof, n, maxChars = getsLine->max;

  if (!file) YError("(BUG) file==0 to Ygets no longer legal");

  if (maxChars<MIN_LINE || maxChars>BIG_LINE) {
    p_free(line);
    getsLine->line = line = p_malloc(MIN_LINE+1);
    getsLine->max = maxChars = MIN_LINE;
    line[maxChars] = '\0';   /* just in case... */
  }

  /* Take care not to try to read a stream that has already returned EOF.  */
  if (Yfeof(file) || !Yfgets(line, maxChars+1, file)) goto abort;
  ateof = Yfeof(file);
  if (!ateof && Yferror(file)) goto abort;

  n = strlen(line);
  while (line[n-1]!='\n' && !ateof) {
    if (maxChars>=MAX_LINE) goto abort;
    if (n >= maxChars-8) {
      /* on any reasonable file system, n == maxChars here
       * one some massively parallel file systems, however, fgets can
       * return before end-of-line even though not at end-of-file
       * - hence fgets should always be called in a loop until either
       *   \n or EOF
       */
      char *prev = line;
      getsLine->line = line = p_malloc(maxChars+INC_LINE+1);
      strcpy(line, prev);
      p_free(prev);
      getsLine->max = (maxChars += INC_LINE);
      line[maxChars] = '\0';   /* just in case... */
    }

    /* EOF is possible, in which case Yfgets returns 0 (ignored here)
       and leaves line unmodified.  The following strlen is guaranteed
       to return 0, and the loop terminates on the ateof test above.  */
    Yfgets(&line[n], maxChars+1-n, file);
    ateof = Yfeof(file);
    if (!ateof && Yferror(file)) goto abort;
    n += strlen(&line[n]);   /* faster than n= strlen(line) */
  }

  if (line[n-1]=='\n') line[--n] = '\0';
  getsLine->n = n;
  return line;

 abort:
  /* If neither Yferror(file) nor Yfeof(file), then line-too-long abort.  */
  if (maxChars>MIN_LINE) {
    p_free(line);
    getsLine->line = line = p_malloc(MIN_LINE+1);
    getsLine->max = maxChars = MIN_LINE;
  }
  line[0] = '\0';
  getsLine->n = 0;
  return 0;
}

/*--------------------------------------------------------------------------*/

/* Scan for C-style escape sequences in quoted strings (e.g.- \n, \t),
   returning the (single character) value of the escape sequence, and,
   if the 2nd parameter is non-0, the character which stopped the scan.
   Thus, if s=="tXYZ", then YpEscapeSeq returns 9 (ASCII tab), and
   endp=="XYZ"; the same results would obtain if s=="009XYZ".  */
int YpEscapeSeq(const char *s, char **endp)
{
  unsigned char c= *s++;
  int val;

  /* 1, 2, or 3 octal digits */
  if (c<='7' && c>='0') {
    int i= 2;
    val= c-'0';
    while ((c=*s)<='7' && c>='0' && i--) {
      val<<= 3;
      val|= c-'0';
      s++;
    }

  /* any number of hex digits (according to appendix A2.5.2 2nd ed K+R) */
  } else if (c=='x') {
    int isD, isU= 0;
    val= 0;
    while ((isD= ((c= *s)<='9' && c>='0')) ||
           (isU= (c<='F' && c>='A')) ||
           (c<='f' && c>='a')) {
      val<<= 4;
      if (isD) val|= c-'0';
      else if (isU) val|= c-('A'-10);
      else val|= c-('a'-10);
      s++;
    }

  /* symbolic escapes in rough order of frequency I use them */
  } else if (c=='n') val= '\n';
  else if (c=='t') val= '\t';
  else if (c=='a') val= '\a';
  else if (c=='f') val= '\f';
  else if (c=='r') val= '\r';
  else if (c=='v') val= '\v';
  else if (c=='b') val= '\b';

  /* Note that \ ? ' and " are just self-insertion like anything else.  */
  else val= c;

  if (endp) *endp= (char *)s;  /* sigh */
  return val;
}

/*--------------------------------------------------------------------------*/

extern UnaryOp PrintC, PrintS, PrintI, PrintL, PrintF, PrintD, PrintZ,
  PrintQ, PrintP, PrintSI, PrintR, PrintVD, PrintSD, PrintFN, PrintBI,
  PrintIO;

int printLength= 79;   /* maximum number of characters to put on a line */
long maxPrintLines= 5000;

static char nBuffer[120];   /* buffer to hold numbers from sprintf */

static int (*RawPrinter)(char *s);
extern int PutsAsArray(char *s);

static void (*PrintRaw)(Operand *at);
static void PrintRawC(Operand *at);
static void PrintRawS(Operand *at);
static void PrintRawI(Operand *at);
static void PrintRawL(Operand *at);
static void PrintRawF(Operand *at);
static void PrintRawD(Operand *at);
static void PrintRawZ(Operand *at);
static void PrintRawQ(Operand *at);
static void PrintRawP(Operand *at);
static void PrintRawSI(Operand *at);

static void PrintArray(Operand *at);

static char *GetTempBuffer(long len);
static void ClearTempBuffer(int force);

static void PrintDims(Dimension *dims);
static int PDRecurse(Dimension *dims, long *length, int last);

void y_setup_func_hack(Operand *op)
{
  YError("bad data type for array index");
}

/* Print calls functions with no arguments, or prints anything else using
   YputsOut.  */
void Print(void)
{
  Operand op;
  P_SOFTFPE_TEST;
  sp->ops->FormOperand(sp, &op);
  if (op.ops->Setup == &y_setup_func_hack) {
    /* put operand into bogus form expected by Eval (see Eval in ops3.c) */
    op.references= 0;   /* intentionally misused */
    op.ops->Eval(&op);
  } else {
    PrintInit(YputsOut);
    op.ops->Print(&op);
    /* Drop(1);
       Print will be followed by DropTop so that CalledAsSubroutine always
       works.  See parse.c.  */
    ForceNewline();
  }
}

/* Y_print is a built in function which prints each of its arguments
   in turn in the manner of Print if called as a function, or returns a
   string vector if called as a function.  */
void Y_print(int nArgs)
{
  Operand op;
  Symbol *stack= sp-nArgs+1;
  if (CalledAsSubroutine()) {
    PrintInit(YputsOut);
    PushDataBlock(RefNC(&nilDB));
  } else {
    PrintInit(&PutsAsArray);
    PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  }
  while (nArgs--) {
    if (!stack->ops) YError("print accepts no keywords (do you mean write?)");
    stack->ops->FormOperand(stack, &op);
    op.ops->Print(&op);
    if (nArgs) PermitNewline(2);
    stack++;
  }
  ForceNewline();
}

/* PutsAsArray appends string to string array on top of stack --
   intended as a plug-in replacement for YputsOut */
int PutsAsArray(char *s)
{
  Array *array= (Array *)sp->value.db;
  Dimension *dims= array->type.dims;
  long number;
  if (dims) {
    number= dims->number;
    sp->value.db= (DataBlock *)GrowArray(array, 1L);
    Unref(array);
    array= (Array *)sp->value.db;
  } else {
    array->type.dims= NewDimension(1L, 1L, (Dimension *)0);
    number= 0;
  }
  array->value.q[number]= p_strcpy(s);
  return 0;
}

static char *printBuf= 0;
static int lenPrintBuf= 0;
static int printNow, permitNow;
static long printLines;

void PrintInit(int (*puts_fun)(char *))
{
  RawPrinter= puts_fun;
  if (lenPrintBuf<printLength || (printLength<=79 && lenPrintBuf>79)) {
    char *p= printBuf;
    printBuf= 0;
    p_free(p);
    if (printLength<39) printLength= 39;
    else if (printLength>256) printLength= 256;
    printBuf= p_malloc(printLength+2);  /* allow for newline, \0 */
    lenPrintBuf= printLength;
  }
  printNow= permitNow= 0;
  printLines= 0;
  printBuf[0]= '\0';
}

void PrintFunc(const char *s)
{
  long len= strlen(s);
  while (printNow+len > printLength) {
    if (p_signalling) p_abort();
    if (permitNow) {
      char savec[2];
      int i= permitNow, j= 1;
      savec[0]= printBuf[i];
      printBuf[i++]= '\0';
      if (printLines++ < maxPrintLines) RawPrinter(printBuf);
      printBuf[0]= savec[0];
      while (i<=printNow) printBuf[j++]= printBuf[i++];
      printNow-= permitNow;
      permitNow= 0;
    } else {
      long nhere= printLength-printNow-1;
      char movec= '\0';
      if (nhere>0) {
        strncpy(&printBuf[printNow], s, nhere);
        s+= nhere;
        len-= nhere;
      } else if (nhere<0) {  /* only -1 is possible */
        movec= printBuf[printLength-1];
      }
      strcpy(&printBuf[printLength-1], "\\");
      if (printLines++ < maxPrintLines) RawPrinter(printBuf);
      if (nhere >= 0) {
        printNow= 0;
        printBuf[0]= '\0';
      } else {
        printNow= 1;
        printBuf[0]= movec;
        printBuf[1]= '\0';
      }
    }
  }
  strcpy(&printBuf[printNow], s);
  printNow+= len;
}

void PermitNewline(int nSpaces)
{
  if (printNow+nSpaces > printLength) ForceNewline();
  else while (nSpaces--) printBuf[printNow++]= ' ';
  printBuf[printNow]= '\0';
  permitNow= printNow;
}

void ForceNewline(void)
{
  if (p_signalling) p_abort();
  if (printNow) {
    if (printLines++ < maxPrintLines) RawPrinter(printBuf);
    printNow= permitNow= 0;
    printBuf[0]= '\0';
  }
}

void PrintFN(Operand *op)
{
  Function *f= op->value;
  Instruction *pc= f->code;
  long posList;
  int n;

  PrintFunc("func ");
  if (pc->index < 0) PrintFunc("<nameless>");
  else PrintFunc(globalTable.names[pc->index]);
  pc++;

  PrintFunc("(");
  PermitNewline(0);
  n= f->nPos;
  posList= f->hasPosList;
  while (n--) {
    posList>>= 1;
    if (posList&1) PrintFunc("&");
    PrintFunc(globalTable.names[(pc++)->index]);
    if (n || (f->hasPosList&1) || f->nKey)
      { PrintFunc(","); PermitNewline(0); }
  }
  n= f->nKey;
  if (f->hasPosList&1) {
    pc++;   /* skip *va* parameter */
    PrintFunc("..");
    if (n) { PrintFunc(","); PermitNewline(0); }
  }
  while (n--) {
    PrintFunc(globalTable.names[(pc++)->index]);
    if (n) { PrintFunc("=,"); PermitNewline(0); }
    else PrintFunc("=");
  }
  PrintFunc(")");
}

void PrintBI(Operand *op)
{
  BIFunction *bif= op->value;
  char *name;
  long len;
  name= bif->index>=0? globalTable.names[bif->index] : "<nameless>";
  len= strlen(name);
  strcpy(nBuffer, "builtin ");
  strncat(nBuffer+8, name, 64);
  if (len>64) strcpy(nBuffer+72, "...");
  strcat(nBuffer, "()");
  PrintFunc(nBuffer);
}

void PrintX(Operand *op)
{
  DataBlock *db= op->value;
  char *name= db? db->ops->typeName : "<unknown>";
  long len;
  len= strlen(name);
  strcpy(nBuffer, "Object of type: ");
  strncat(nBuffer+16, name, 64);
  if (len>56) strcpy(nBuffer+72, "...");
  PrintFunc(nBuffer);
}

static void PrintArray(Operand *at)
{
  Dimension *dims= at->type.dims;
  if (!dims) {
    PrintRaw(at);
  } else {
    void (*OrigPrintRaw)(Operand *)= PrintRaw;
    long n= dims->number;
    at->type.dims= dims->next;
    PrintFunc("[");
    for (;;) {
      PrintArray(at);
      PrintRaw= OrigPrintRaw;  /* PrintRawSI changes this */
      if (!(--n)) break;
      PrintFunc(",");
      PermitNewline(0);
    }
    PrintFunc("]");
    at->type.dims= dims;
  }
}

void PrintC(Operand *op) { PrintRaw= PrintRawC; PrintArray(op); }
void PrintS(Operand *op) { PrintRaw= PrintRawS; PrintArray(op); }
void PrintI(Operand *op) { PrintRaw= PrintRawI; PrintArray(op); }
void PrintL(Operand *op) { PrintRaw= PrintRawL; PrintArray(op); }
void PrintF(Operand *op) { PrintRaw= PrintRawF; PrintArray(op); }
void PrintD(Operand *op) { PrintRaw= PrintRawD; PrintArray(op); }
void PrintZ(Operand *op) { PrintRaw= PrintRawZ; PrintArray(op); }
void PrintQ(Operand *op) { PrintRaw= PrintRawQ; PrintArray(op); }
void PrintP(Operand *op) { PrintRaw= PrintRawP; PrintArray(op); }
void PrintSI(Operand *op) { PrintRaw= PrintRawSI; PrintArray(op); }

#define DEFAULT_CHAR "0x%02x"
#define DEFAULT_SHORT "%d"
#define DEFAULT_INT "%d"
#define DEFAULT_LONG "%ld"
#define DEFAULT_FLOAT "%g"
#define DEFAULT_DOUBLE "%g"
#define DEFAULT_COMPLEX "%g%+gi"
#define DEFAULT_POINTER "%p"

static char *typeDefault[8]= {
  DEFAULT_CHAR, DEFAULT_SHORT, DEFAULT_INT, DEFAULT_LONG,
  DEFAULT_FLOAT, DEFAULT_DOUBLE, DEFAULT_COMPLEX, DEFAULT_POINTER };

void DefaultPrintFormat(int type)
{
  if (type & (1<<T_CHAR)) yCharFormat= typeDefault[0];
  if (type & (1<<T_SHORT)) yShortFormat= typeDefault[1];
  if (type & (1<<T_INT)) yIntFormat= typeDefault[2];
  if (type & (1<<T_LONG)) yLongFormat= typeDefault[3];
  if (type & (1<<T_FLOAT)) yFloatFormat= typeDefault[4];
  if (type & (1<<T_DOUBLE)) yDoubleFormat= typeDefault[5];
  if (type & (1<<T_COMPLEX)) yComplexFormat= typeDefault[6];
  if (type & (1<<T_POINTER)) yPointerFormat= typeDefault[7];
}

char *yCharFormat= DEFAULT_CHAR;
static void PrintRawC(Operand *at)
{ unsigned char *pv= at->value; sprintf(nBuffer, yCharFormat, (int)*pv);
  PrintFunc(nBuffer); at->value= pv+1; }

char *yShortFormat= DEFAULT_SHORT;
static void PrintRawS(Operand *at)
{ short *pv= at->value; sprintf(nBuffer, yShortFormat, (int)*pv);
  PrintFunc(nBuffer); at->value= pv+1; }

char *yIntFormat= DEFAULT_INT;
static void PrintRawI(Operand *at)
{ int *pv= at->value; sprintf(nBuffer, yIntFormat, *pv);
  PrintFunc(nBuffer); at->value= pv+1; }

char *yLongFormat= DEFAULT_LONG;
static void PrintRawL(Operand *at)
{ long *pv= at->value; sprintf(nBuffer, yLongFormat, *pv);
  PrintFunc(nBuffer); at->value= pv+1; }

char *yFloatFormat= DEFAULT_FLOAT;
static void PrintRawF(Operand *at)
{ float *pv= at->value; sprintf(nBuffer, yFloatFormat, (double)*pv);
  PrintFunc(nBuffer); at->value= pv+1; }

char *yDoubleFormat= DEFAULT_DOUBLE;
static void PrintRawD(Operand *at)
{ double *pv= at->value; sprintf(nBuffer, yDoubleFormat, *pv);
  PrintFunc(nBuffer); at->value= pv+1; }

char *yComplexFormat= DEFAULT_COMPLEX;
static void PrintRawZ(Operand *at)
{ double *pv= at->value; sprintf(nBuffer, yComplexFormat, pv[0], pv[1]);
  PrintFunc(nBuffer); at->value= pv+2; }

char *yPointerFormat= DEFAULT_POINTER;
static void PrintRawP(Operand *at)
{ void **pv= at->value; sprintf(nBuffer, yPointerFormat, *pv);
  PrintFunc(nBuffer); at->value= pv+1; }

static long typeIndex[8]= { -1, -1, -1, -1, -1, -1, -1, -1 };
static char **typeFormat[8]= {
  &yCharFormat, &yShortFormat, &yIntFormat, &yLongFormat,
  &yFloatFormat, &yDoubleFormat, &yComplexFormat, &yPointerFormat };

void Y_print_format(int nArgs)
{
  Symbol *arg = sp-nArgs+1;
  int npos = 0;

  if (typeIndex[0]<0) {
    typeIndex[0]= Globalize("char", 0L);
    typeIndex[1]= Globalize("short", 0L);
    typeIndex[2]= Globalize("int", 0L);
    typeIndex[3]= Globalize("long", 0L);
    typeIndex[4]= Globalize("float", 0L);
    typeIndex[5]= Globalize("double", 0L);
    typeIndex[6]= Globalize("complex", 0L);
    typeIndex[7]= Globalize("pointer", 0L);
  }

  if (nArgs < 1) {
    DefaultPrintFormat(~0);
    printLength= 79;
    maxPrintLines= 5000;
  } else do {
    if (arg->ops) {
      if (++npos > 2) YError("print_format expects at most two positional arguments");
      if (YNotNil(arg)) {
	long i = YGetInteger(arg);
	if (npos == 1) {
	  if (i>256) printLength = 256;
	  else if (i>39) printLength = i;
	  else if (i>0) printLength = 39;
	  else printLength = 79;
	} else {
	  if (i < 10) maxPrintLines = (i<=0)? 5000 : 10;
	  else maxPrintLines = i;
	}
      }
      arg++;
    } else {
      long index= (arg++)->index;
      int i;
      char *string;
      for (i=0 ; i<8 ; i++) if (typeIndex[i]==index) break;
      if (i>=8) YError("unrecognized keyword in print_format");
      string= *typeFormat[i];
      *typeFormat[i]= typeDefault[i];
      if (string!=typeDefault[i]) p_free(string);
      string= YGetString(arg++);
      nArgs--;
      if (string && strlen(string)) *typeFormat[i]= p_strcpy(string);
    }
  } while (--nArgs);
}

static char *sPart= 0;
static long sPartLen= 0;

static char *GetTempBuffer(long len)
{
  if (len>=sPartLen) {
    long newlen= ((len-1)/80 + 1)*80;
    ClearTempBuffer(1);
    sPart= p_malloc(newlen+1);
    sPartLen= newlen;
  }
  return sPart;
}

static void ClearTempBuffer(int force)
{
  if (force || sPartLen>80) {
    char *part= sPart;
    sPartLen= 0;
    sPart= 0;
    p_free(part);
  }
}

char *ScanForEscape(char *s)
{
  char c;
  if (!s) return 0;
  while ((c= *s) && c>=' ' && c<'\177' && c!='\\' && c!='\"') s++;
  return s;
}

int AddEscapeSeq(char *s, int esc)
{
  int n= 2;
  if (esc=='\\') strcpy(s, "\\\\");
  else if (esc=='\"') strcpy(s, "\\\"");
  else if (esc=='\n') strcpy(s, "\\n");
  else if (esc=='\t') strcpy(s, "\\t");
  else if (esc=='\a') strcpy(s, "\\a");
  else if (esc=='\f') strcpy(s, "\\f");
  else if (esc=='\r') strcpy(s, "\\r");
  else if (esc=='\v') strcpy(s, "\\v");
  else if (esc=='\b') strcpy(s, "\\b");
  else { sprintf(s, "\\%03o", esc&0xff); n= 4; }
  return n;
}

static void PrintRawQ(Operand *at)
{
  char **pv= at->value;
  char *s= *pv++;
  char *esc;
  if (!s) {
    PrintRawP(at);
  } else {
    PrintFunc("\"");
    if (*(esc= ScanForEscape(s))) {
      char *part;
      do {
        part= GetTempBuffer(esc-s + 4);
        strncpy(part, s, esc-s);
        AddEscapeSeq(part + (esc-s), (int)(*esc));
        PrintFunc(part);
        s= esc+1;
      } while (*(esc= ScanForEscape(s)));
      ClearTempBuffer(0);
    }
    if (*s) PrintFunc(s);
    PrintFunc("\"");
  }
  at->value= pv;
}

static void PrintRawSI(Operand *at)
{
  char *pv= at->value;
  StructDef *base= at->type.base;
  Operand subOp;
  long n= base->table.nItems;
  char **name= base->table.names;
  Member *member= base->members;
  long *offset= base->offsets;

  if (base->file) YError("(BUG?) can't print an instance of a disk struct");

  PrintFunc(StructName(base));
  PrintFunc("(");
  PermitNewline(0);
  for (;;) {
    PrintFunc(*name);
    PrintFunc("=");
    PermitNewline(0);
    subOp.type.base= member->base;
    subOp.type.dims= member->dims;
    subOp.value= pv + (*offset);
    subOp.type.base->dataOps->Print(&subOp);
    if (!(--n)) break;
    PrintFunc(",");
    PermitNewline(0);
    name++;
    member++;
    offset++;
  }
  PrintFunc(")");

  at->value= pv + base->size;
}

/* range functions */
extern RangeFunc RFmin, RFmax, RFptp, RFsum, RFavg, RFrms, RFmnx, RFmxx,
  RFpsum, RFdif, RFzcen, RFpcen, RFuncp, RFcum;

static char *rfNames[]= { "avg:","cum:","dif:","max:","min:","mnx:","mxx:",
                "pcen:","psum:","ptp:","rms:","sum:","uncp:","zcen:","??:" };

static RangeFunc *RFs[]= { &RFavg,&RFcum,&RFdif,&RFmax,&RFmin,&RFmnx,&RFmxx,
                    &RFpcen,&RFpsum,&RFptp,&RFrms,&RFsum,&RFuncp,&RFzcen,0 };

static int rfindex[] = { 9,18,14,6,5,11,12,16,13,7,10,8,17,15, 19 };
static int rfxedni[] = { 4, 3, 9, 11, 0, 10, 5, 6, 8, 2, 13, 7, 12, 1, -1 };

char *GetRFName(RangeFunc *rfTarget)
{
  RangeFunc **rf= RFs;
  while (*rf && *rf!=rfTarget) rf++;
  return rfNames[rf-RFs];
}

void
Y_rangeof(int argc)
{
  Range *rng;
  Dimension *dims;
  long *nrng;
  if (sp->ops==&referenceSym) ReplaceRef(sp);
  rng = (sp->ops==&dataBlockSym
         && sp->value.db->ops==&rangeOps)? (Range *)sp->value.db : 0;
  nrng = rng? 0 : YGet_L(sp, 0, &dims);
  if (argc!=1 || (!rng && (!nrng || !dims || dims->number!=4 || dims->next)))
    YError("rangeof expecting index range or array of 4 longs");
  if (rng) {  /* convert rf:min:max:step to [flag, min, max, step] */
    int i = 0, j = 0, k = 0;
    if (rng->rf) {
      for (i=0 ; i<14 ; i++) if (RFs[i] == rng->rf) break;
      i = rfindex[i];
    }
    if (rng->nilFlags & R_PSEUDO) j = (rng->nilFlags & R_RUBBER)? 3 : 1;
    else if (rng->nilFlags & R_RUBBER) j = 2;
    else if (rng->nilFlags & R_NULLER) j = 4;
    k = ((rng->nilFlags & R_MINNIL)!=0) | (((rng->nilFlags & R_MAXNIL)!=0)<<1);
    dims = tmpDims;  tmpDims = 0;  FreeDimension(dims);
    tmpDims = NewDimension(4L, 1L, 0);
    nrng = ((Array*)PushDataBlock(NewArray(&longStruct, tmpDims)))->value.l;
    nrng[0] = rng->min;
    nrng[1] = rng->max;
    nrng[2] = rng->inc;
    nrng[3] = k | ((j + ((!j)? i : 0))<<2);
  } else {    /* convert [flag, min, max, step] to rf:min:max:step */
    long inc = nrng[2]? nrng[2] : 1;
    int flags = ((nrng[3]&1)? R_MINNIL : 0) | ((nrng[3]&2)? R_MAXNIL : 0);
    long f = (nrng[3] >> 2);
    if (f == 1) flags |= R_PSEUDO;
    else if (f == 2) flags |= R_RUBBER;
    else if (f == 3) flags |= R_RUBBER | R_PSEUDO;
    else if (f == 4) flags |= R_NULLER;
    rng = PushDataBlock(NewRange(nrng[0], nrng[1], inc, flags));
    if (f > 4) {
      f = rfxedni[((f<20)?f:19)-5];
      if (f < 0) {  /* make illegal range */
        rng->min = 2;  rng->max = 1;  rng->inc = 1;  rng->nilFlags = 0;
        f = 0;
      } else {
        rng->rf = RFs[f];
      }
    }
  }
}

void PrintR(Operand *op)
{
  Range *range= op->value;
  int flags= range->nilFlags;
  long len;
  if (range->rf) {
    strcpy(nBuffer, GetRFName(range->rf));
    len= strlen(nBuffer);
  } else if (flags&R_MARKED) {
    strcpy(nBuffer, "+:");
    len= 2;
  } else if (flags&R_PSEUDO) {
    strcpy(nBuffer, "-:");
    len= 2;
  } else if (flags&R_RUBBER) {
    PrintFunc("<..>:");
    return;
  } else if (flags&R_NULLER) {
    PrintFunc("<nuller>:");
    return;
  } else {
    nBuffer[0]= '\0';
    len= 0;
  }
  if (flags&R_MINNIL) {
    if (flags&R_MAXNIL) {
      strcpy(nBuffer+len, ":");
    } else {
      sprintf(nBuffer+len, ":%ld", range->max);
    }
  } else if (flags&R_MAXNIL) {
    sprintf(nBuffer+len, "%ld:", range->min);
  } else {
    sprintf(nBuffer+len, "%ld:%ld", range->min, range->max);
  }
  if (range->inc!=1) {
    len= strlen(nBuffer);
    sprintf(nBuffer+len, ":%ld", range->inc);
  }
  PrintFunc(nBuffer);
}

/* ARGSUSED */
void PrintVD(Operand *op)
{
  PrintFunc("[]");
}

static int PDRecurse(Dimension *dims, long *length, int last)
{
  long len;
  int count;

  if (!dims) return 0;
  count= PDRecurse(dims->next, length, 0);
  if (count<0) return count-1;
  len= *length;

  if (dims->origin!=1L) {
    if (len>45) return -1;
    sprintf(nBuffer+len, "%ld:%ld",
            dims->origin, dims->origin+dims->number-1);
  } else {
    if (len>60) return -1;
    sprintf(nBuffer+len, "%ld", dims->number);
  }
  len+= strlen(nBuffer+len);
  if (!last) { strcpy(nBuffer+len, ","); len++; }

  *length= len;
  return count+1;
}

static void PrintDims(Dimension *dims)
{
  if (dims) {
    long len= 1;
    strcpy(nBuffer, "(");
    if (PDRecurse(dims, &len, 1)<0) {
      strcpy(nBuffer+len, "...");
      len+= 3;
    }
    strcpy(nBuffer+len, ")");
    PrintFunc(nBuffer);
  }
}

void PrintSD(Operand *at)
{
  StructDef *base= at->value;
  long n= base->table.nItems;
  Member *member= base->members;
  char **name= base->table.names;

  ForceNewline();
  PrintFunc("struct ");
  PrintFunc(StructName(base));
  PrintFunc(" {");
  ForceNewline();
  while (n--) {
    PrintFunc("  ");
    PrintFunc(StructName(member->base));
    PrintFunc(" ");
    PrintFunc(*name);
    PrintDims(member->dims);
    PrintFunc(";");
    ForceNewline();
    member++;
    name++;
  }
  PrintFunc("}");
  ForceNewline();
}

/*--------------------------------------------------------------------------*/

static char *printText= 0, *tmpText= 0;
static char *ioStatus[]=
  { "<illegal>", "read-only", "write-only", "read-write" };

static void SafeFree(char **s);
static void SafeFree(char **s)
{
  char *t= *s;
  *s= 0;
  p_free(t);
}

void PrintIO(Operand *op)
{
  IOStream *file= op->value;
  char text[80];
  SafeFree(&printText);
  SafeFree(&tmpText);

  /* read-write binary stream: <tail of fullname>
       In directory: <head of fullname>
       Current record is number 53 of 53
       File of current record: <tail of child's name>
       Time, cycle of current record: 1.234567e+00, 9876
   */

  ForceNewline();
  if (file->stream) {
    sprintf(text, "%s binary stream: ", ioStatus[file->permissions&3]);
    tmpText= YNameTail(file->fullname);
  } else {
    strcpy(text, "binary stream <closed>: ");
  }
  printText= p_strncat(text, tmpText, 0);
  SafeFree(&tmpText);
  PrintFunc(printText);
  SafeFree(&printText);
  ForceNewline();

  tmpText= YNameHead(file->fullname);
  printText= p_strncat("  In directory: ", tmpText, 0);
  SafeFree(&tmpText);
  PrintFunc(printText);
  SafeFree(&printText);
  ForceNewline();

  if (file->history) {
    HistoryInfo *history= file->history;

    if (history->recNumber>=0) {
      long i= history->recNumber;

      sprintf(text, "  Current record is number %ld of %ld",
              i+1, history->nRecords);
      PrintFunc(text);
      ForceNewline();

      tmpText= YNameTail(history->child->fullname);
      printText= p_strncat("  File of current record: ", tmpText, 0);
      SafeFree(&tmpText);
      PrintFunc(printText);
      SafeFree(&printText);
      ForceNewline();

      if (history->time) {
        if (history->ncyc) {
          sprintf(text, "  Time, cycle of current record: %.6e, %ld",
                  history->time[i], history->ncyc[i]);
        } else {
          sprintf(text, "  Time of current record: %.6e",
                  history->time[i]);
        }
      } else {
        if (history->ncyc) {
          sprintf(text, "  Cycle of current record: %ld",
                  history->ncyc[i]);
        } else {
          strcpy(text, "  <No time or cycle available>");
        }
      }
      PrintFunc(text);
      ForceNewline();

    } else {
      PrintFunc("  <No current record>");
      ForceNewline();
    }
  }
}

/*--------------------------------------------------------------------------*/

/* Set up a block allocator which grabs space for 16 IOFileLink objects
   at a time.  Since IOFileLink contains several pointers, the alignment
   of an IOFileLink must be at least as strict as a void*.  */
static MemryBlock ioflBlock= {0, 0, sizeof(IOFileLink),
                                  16*sizeof(IOFileLink)};

void AddIOLink(IOFileLink** list, void *ios)
{
  IOFileLink *first= *list;
  IOFileLink *iofl= NextUnit(&ioflBlock);
  iofl->next= first;
  iofl->prev= list;
  iofl->ios= ios;
  *list= iofl;
  if (first) first->prev= &iofl->next;
}

void RemoveIOLink(IOFileLink* iofl, void *ios)
{
  for ( ; iofl ; iofl=iofl->next) if (iofl->ios==ios) break;
  if (iofl) {
    *iofl->prev= iofl->next;
    if (iofl->next) iofl->next->prev= iofl->prev;
    FreeUnit(&ioflBlock, iofl);
  }
}

/*--------------------------------------------------------------------------*/

char *YpReparse(void *function)
{
  Function *func= function;
  long index= func? func->code[0].index : -1;
  long position= ReopenSource(index, 1, (func?func->isrc:-1));
  char *msg;
  if (position>=0) {
    YpParse(func);  /* This had better not generate any new tasks... */
    msg= MakeErrorLine(ypBeginLine,
                       nYpIncludes? ypIncludes[nYpIncludes-1].filename : 0);
    if (nYpIncludes && ypIncludes[nYpIncludes-1].file) {
      p_fclose(ypIncludes[nYpIncludes-1].file);
      ypIncludes[nYpIncludes-1].file= 0;
    }
  } else if (position<-3) {
    msg= MakeErrorLine(-4,
                 "*****fseek failed scanning source file for function");
  } else if (position<-2) {
    msg= MakeErrorLine(-3,
                 "*****source file changed, no longer contains function");
  } else if (position<-1) {
    msg= MakeErrorLine(-2,
                 "*****source file for function no longer exists");
  } else {
    msg= MakeErrorLine(-1,
                 "*****source file for function unknown");
  }
  return msg;
}

/*--------------------------------------------------------------------------*/
