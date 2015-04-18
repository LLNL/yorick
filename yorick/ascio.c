/*
 * $Id: ascio.c,v 1.8 2010-07-03 19:42:31 dhmunro Exp $
 * Define standard Yorick built-in functions for ASCII I/O
 *
 * See std.i for documentation on the interface functions defined here.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "ydata.h"
#include "yio.h"
#include "defmem.h"
#include "pstdlib.h"
#include "play.h"
#include <string.h>
#include <errno.h>

extern BuiltIn Y_open, Y_close, Y_read, Y_write, Y_sread, Y_swrite;
extern BuiltIn Y_rdline, Y_bookmark, Y_backup, Y_popen, Y_fflush;
extern BuiltIn Y_filepath;

extern char *MakeErrorLine(long lineNumber, const char *filename);

extern void YErrorIO(const char *msg);

/*--------------------------------------------------------------------------*/

static char *CheckBuf(long len);
static char *CrackScan(char *format);
static char *CrackPrint(char *format);
static int CrackFormat(char *format, char *(*Cracker)(char *));
static void AddFormat(char *format, int type, int danger);
static void FreeFormats(void);
static void CheckOps(int nArgs);
static char *NextInLine(Operand *op, long n);
static void ReadWorker(Operand *sourceOp, Symbol *stack, char *format,
                       Symbol *stack0);
static void WriteWorker(Operand *sinkOp, Symbol *stack, char *format);
static char *CheckOut(long len);

static char *fmtBuf= 0;
static long fmtLen= 0;
static int fmtType;
#define IO_NONE 0
#define IO_STRING 1
#define IO_LONG 2
#define IO_DOUBLE 3
#define IO_POINTER 4
#define IO_CHAR 5

struct FormatList { char *format; int type; int typeID; int danger; };
static struct FormatList *fmtList= 0;
static long fmtMax= 0;
static long fmtNow= 0;

static long fmtAssigns;
static long fmtWidth, fmtTotal;
static int fmtDanger;

struct io_operand {
  Operand o;
  Symbol s;
};
static struct io_operand *ioOps= 0;
static long maxIOops= 0;

static int typeMatch[]= { IO_LONG, IO_LONG, IO_LONG, IO_LONG,
                IO_DOUBLE, IO_DOUBLE, IO_NONE, IO_STRING, IO_POINTER };
static char *scanDefaults[]= { 0, "%s%n", "%ld%n", "%le%n" };
static char *printDefaults[]= { 0, " %s", " %8ld", " %#14.6g", " %8p" };

typedef int ScanFunc(Operand *op, char *format, char **text);
static ScanFunc CScanner, SScanner, IScanner, LScanner,
  FScanner, DScanner, QScanner, NilScanner;
static ScanFunc *Scanner[]= { &CScanner, &SScanner, &IScanner, &LScanner,
  &FScanner, &DScanner, 0, &QScanner, &NilScanner };

static YgetsLine inputBuffer= { 0, 0, 0 };

static Dimension *pDims= 0;
static char *outBuf= 0;
static long outLen= 0;
static long lineSize;

typedef void PrtFunc(Operand *op, char *format, char *text);
static PrtFunc CPrinter, SPrinter, IPrinter, LPrinter,
  FPrinter, DPrinter, QPrinter, PPrinter;
static PrtFunc *Printer[]= { &CPrinter, &SPrinter, &IPrinter, &LPrinter,
  &FPrinter, &DPrinter, 0, &QPrinter, &PPrinter };
static PrtFunc CPrintC, SPrintC, IPrintC, LPrintC;
static PrtFunc *PrintC[]= { &CPrintC, &SPrintC, &IPrintC, &LPrintC };
static PrtFunc FPrintD, DPrintD;
static PrtFunc *PrintD[]= { &FPrintD, &DPrintD };

/*--------------------------------------------------------------------------*/

/* Two data types which are "foreign" to Yorick are defined in this
   file: the TextStream and the Bookmark.  */

static UnaryOp PrintBM, PrintTX;

/* Implement text streams as a foreign Yorick data type.  */
struct TextStream {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  p_file *stream;      /* 0 indicates file has been closed */
  char *fullname;      /* filename after YExpandName */
  int permissions;     /* +1 read permission, +2 write permission
                          +4 append mode, +8 binary mode,
                          +16 not seekable, +32 pipe */
  /* ------ begin specific text stream part ------- */
  long lastLineRead;   /* 1-origin line number of last line read */
  long readPosition;   /* file position (ftell) after lastLineRead */
  long lastPosition;   /* file position (ftell) before lastLineRead --
                          after backup, lastPosition==readPosition,
                          and lastPosition is not valid */
  int readWrite;       /* 0 initially, 1 after read, 2 after write */
  long fileID;         /* unique number used to recognize this file */
};

extern TextStream *NewTextStream(char *fullname,
                                 void *stream, int permissions,
                                 long line, long pos);
extern void FreeTextStream(void *ts);  /* ******* Use Unref(ts) ******* */

Operations textOps = {
  &FreeTextStream, T_OPAQUE, 0, T_STRING, "text_stream",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &EvalX, &SetupX, &GetMemberX, &MatMultX, &PrintTX
};

/* Implement bookmarks as a foreign Yorick data type.  */
typedef struct Bookmark Bookmark;
struct Bookmark {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  long lastLineRead;
  long lastPosition, readPosition;
  long fileID;
};

extern Bookmark *NewBookmark(long line, long last, long next, long id);
extern void FreeBookmark(void *bm);  /* ******* Use Unref(bm) ******* */

Operations bookOps = {
  &FreeBookmark, T_OPAQUE, 0, T_STRING, "bookmark",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &EvalX, &SetupX, &GetMemberX, &MatMultX, &PrintBM
};

/*--------------------------------------------------------------------------*/

void Y_open(int nArgs)
{
  Symbol *stack= sp-nArgs+1;
  char *filename, *fmode, *fullname, filemode[8];
  int errmode, permissions;
  p_file *file;

  if (nArgs<1 || nArgs>3) YError("bad argument list to open function");
  filename= YGetString(stack);
  if (nArgs<2) fmode= 0;
  else fmode= YGetString(stack+1);
  if (nArgs<3) errmode= 0;
  else errmode= YGetInteger(stack+2);

  memset(filemode, 0, 8);
  if (!fmode || !fmode[0])
    strcpy(filemode, "r");
  else if (fmode[0]!='r' && fmode[0]!='w' && fmode[0]!='a')
    YError("2nd argument to open must begin with r, w, or a");
  else
    strncat(filemode, fmode, 7);
  if (filemode[0]=='r') permissions= 1;
  else if (filemode[0]=='w') permissions= 2;
  else permissions= 6;
  if (filemode[1]=='+' || filemode[2]=='+') permissions|= 3;
  if (filemode[1]=='b' || filemode[2]=='b') {
    permissions|= 8;
    if (filemode[2]=='c') {
      if (permissions&2) permissions|= 64;
      filemode[2]= '\0';
    } else if (filemode[3]=='c') {
      if (permissions&2) permissions|= 64;
      filemode[3]= '\0';
    }
  }

  fullname= YExpandName(filename);
  file= p_fopen(fullname, filemode);

  if (file) {
    /* set permission bits and push result IOStream */
    if (permissions&8) {
      IOStream *ios= NewIOStream(fullname, file, permissions);
      PushDataBlock(ios);
      if (permissions&64) CLupdate(ios);
    } else {
      PushDataBlock(NewTextStream(fullname, file, permissions, 0L, 0L));
    }

  } else if (errmode) {
    /* fail silently if optional errmode flag is set */
    p_free(fullname);
    permissions= 0;
    PushDataBlock(RefNC(&nilDB));

  } else {
    /* blow up if optional errmode flag is not set */
    char *dots= strlen(filename)>100? "..." : "";
    char message[140];
    p_free(fullname);
    sprintf(message, "cannot open file %.100s%s (mode %.6s)",
            filename, dots, filemode);
    YErrorIO(message);
    return;
  }

  PopTo(sp-nArgs-1);
  Drop(nArgs);
  return;
}

PLUG_API int psckt_0close(void);  /* from socky.c */

void Y_close(int nArgs)
{
  DataBlock *db;
  IOStream *binary= 0;
  long index = -1;
  if (nArgs!=1) YError("close function takes exactly one argument");

  /* If argument is a simple variable reference, nil the variable.  */
  if (sp->ops==&referenceSym) {
    Symbol *s= &globTab[(index = sp->index)];
    ReplaceRef(sp);
    if (s->ops==&dataBlockSym &&
        (s->value.db->ops==&textOps || s->value.db->ops==&streamOps)) {
      s->ops= &intScalar;
      Unref(s->value.db);
      s->value.db= RefNC(&nilDB);
      s->ops= &dataBlockSym;
    }
  }
  db= sp->value.db;

  if (db->ops==&textOps) {
    TextStream *text= (TextStream *)db;
    if (text->stream)
      p_fclose(text->stream);
    text->stream= 0;
  } else if (db->ops==&streamOps) {
    /* Make sure that the binary->stream gets closed, even if the IOStream
       itself is not freed by the Drop() below.  */
    if (db->references) binary= (IOStream *)db;
  } else if (db->ops!=&voidOps) {
    if (psckt_0close())
      YError("bad argument type to close function");
    if (index >= 0) {
      ypush_nil();
      yput_global(index, 0);
      yarg_drop(1);
    }
    yarg_drop(1);
    ypush_nil();
    return;
  }

  if (db->references && db->ops!=&voidOps) {
    char message[80];
    sprintf(message, "%d outstanding references to closed file",
            db->references);
    YWarning(message);
  }

  Drop(1);
  if (binary) {
    HistoryInfo *history = binary->history;
    ClearPointees(binary, 0);
    if (history) {
      IOStream *child = history->child;
      ClearPointees(child, 0);
      if (child->CloseHook) {
        child->CloseHook(child);
        child->CloseHook = 0;
      }
      if (child->stream) {
        if (child->stream == binary->stream) FlushFile(child, 1);
        else if (child->stream) child->ioOps->Close(child);
        child->stream = 0;
      }
      if (child->contentsLog != binary->contentsLog) FreeClogFile(child);
    } else if (binary->CloseHook) {
      binary->CloseHook(binary);
      binary->CloseHook = 0;
    }
    if (binary->stream) binary->ioOps->Close(binary);
    binary->stream = 0;
    if (binary->contentsLog) FreeClogFile(binary);
  }
  ypush_nil();
}

void Y_filepath(int argc)
{
  Dimension *dims;
  Operand op;
  char **input, **output;
  long i, n;

  if (argc != 1) YError("filepath function takes exactly one argument");
  op.ops= 0;
  if (sp->ops) sp->ops->FormOperand(sp, &op);
  if (op.ops == &stringOps) {
    input = YGet_Q(sp, 0, &dims);
    n = TotalNumber(dims);
    output = ((Array *)PushDataBlock(NewArray(&stringStruct, dims)))->value.q;
    for (i = 0; i < n; ++i) {
      output[i] = (input[i] ? YExpandName(input[i]) : 0);
    }
  } else if (op.ops == &streamOps) {
    output = ypush_q(0);
    output[0] = p_strcpy(((IOStream *)op.value)->fullname);
  } else if (op.ops == &textOps) {
    output = ypush_q(0);
    output[0] = p_strcpy(((TextStream *)op.value)->fullname);
  } else if (op.ops == &voidOps) {
    PushDataBlock(RefNC(&nilDB));
  } else {
    YError("bad argument: expecting text/binary file or file name(s)");
  }
}

/*--------------------------------------------------------------------------*/

/* a read operation from the keyboard actually suspends the virtual
 * machine until the next keyboard input event
 * the pc is saved so that other interpreted tasks can run while waiting
 * for keyboard input
 */

extern void yr_reader(char *input_line);
extern Instruction *ym_suspend(void);
extern void ym_resume(Instruction *pc);
extern char *y_read_prompt;
extern int yp_continue;
char *y_read_prompt = 0;

static Instruction *yr_pc_resume= 0;
static char *yr_dflt_prompts[] = { "read> ", "" };
static char **yr_result = 0;
static long yr_n, yr_nlines;
static int yr_j, yr_nargs;

void
yr_reset(void)
{
  int i;
  char *prompt = y_read_prompt;
  y_read_prompt = 0;
  yr_pc_resume = 0;
  if (prompt) p_free(prompt);
  for (i=0 ; i<yr_nargs ; i++) {
    if (ioOps[i].s.ops == &dataBlockSym) {
      Unref(ioOps[i].s.value.db);
      ioOps[i].s.value.db = 0;
    } else if (ioOps[i].s.index >= 0) {
      long index = ioOps[i].s.index;
      if (globTab[index].ops == &dataBlockSym) {
        /* scalar asynchronously redefined since ym_suspend */
        globTab[index].ops = &intScalar;
        Unref(globTab[index].value.db);
      }
      globTab[index] = ioOps[i].s;
    }
  }
  yr_nargs = 0;
  FreeFormats();
}

void
yr_reader(char *input_line)
{
  if (yr_result) {                 /* rdline */
    *yr_result++ = p_strcpy(input_line);
    yr_n--;
  } else if (yr_n > 0) {           /* read */
    yr_nlines++;
    /* Scanner returns 0 if found, 1 if not found,
     * sets input_line==0 on matching failure, which should abort read */
    while (!Scanner[fmtList[yr_j].typeID](&ioOps[yr_j].o,
                                          fmtList[yr_j].format,
                                          &input_line)) {
      yr_j++;
      if (yr_j >= yr_nargs) {
        yr_n--;
        if (!yr_n) break;
        yr_j = 0;
      }
    }
    if (!input_line) yr_n = 0;  /* halt on matching failure */
  }
  if (p_signalling) p_abort();
  if (yr_n <= 0) {
    ym_resume(yr_pc_resume);
    /* read() returns count -- rdline() already pushed its result */
    if (!yr_result && sp->ops==&longScalar) sp->value.l = fmtAssigns;
    yr_reset();
  }
}

void Y_read(int nArgs)
{
  Symbol *stack, *s;
  char *format, *keyNames[3];
  Symbol *keySymbols[2];
  Operand sourceOp;
  Symbol *stack0= sp-nArgs;

  keyNames[0]= "format";
  keyNames[1]= "prompt";
  keyNames[2]= 0;
  stack= YGetKeywords(stack0+1, nArgs, keyNames, keySymbols);

  /* Get 1st argument if it is an IOStream or nil, otherwise will use
     nil stream (keyboard).  */
  if (stack>sp) YError("read function takes at least one argument");
  /* treat references gently -- don't want to suck reference to
     scalar int, long, or double onto the stack */
  if (stack->ops==&referenceSym) s= &globTab[stack->index];
  else s= stack;
  if (s->ops==&dataBlockSym &&
      (s->value.db->ops==&textOps || s->value.db->ops==&voidOps)) {
    stack->ops->FormOperand(stack, &sourceOp);
    stack++;
    if (sourceOp.ops==&voidOps) sourceOp.ops= 0;
    else {
      TextStream *ts= sourceOp.value;
      p_file *file= ts->stream;
      if (!file)
        YErrorIO("attempt to read from closed I/O stream");
      else if (!(ts->permissions & 1))
        YErrorIO("attempt to read from file opened in w or a mode");
      if (ts->readWrite&2 && p_fseek(file, ts->readPosition)) {
        p_ferror(file);       /* don't prejudice future I/O attempts */
        YErrorIO("fseek failed to find current position in ASCII read");
      }
      ts->readWrite= 1;
    }
  } else {
    sourceOp.ops= 0;  /* special form recognized by NextInLine */
  }
  if (!sourceOp.ops) {
    /* set prompt string for NextInLine.  */
    sourceOp.value=
      keySymbols[1]? YGetString(keySymbols[1]) : yr_dflt_prompts[0];
    if (!sourceOp.value) sourceOp.value= yr_dflt_prompts[1];
  }

  /* get format keyword, if any */
  format= keySymbols[0]? YGetString(keySymbols[0]) : 0;

  ReadWorker(&sourceOp, stack, format, stack0);
}

void Y_sread(int nArgs)
{
  Symbol *stack;
  char *format, *keyNames[2];
  Symbol *keySymbols[1];
  Operand sourceOp;

  keyNames[0]= "format";
  keyNames[1]= 0;
  stack= YGetKeywords(sp-nArgs+1, nArgs, keyNames, keySymbols);

  /* Get 1st argument if it is an IOStream or nil, otherwise will use
     nil stream (keyboard).  */
  if (stack>sp) YError("sread function takes at least one argument");
  stack->ops->FormOperand(stack, &sourceOp);
  if (sourceOp.ops!=&stringOps)
    YError("1st argument to sread must be source string or string array");
  stack++;

  /* get format keyword, if any */
  format= keySymbols[0]? YGetString(keySymbols[0]) : 0;

  ReadWorker(&sourceOp, stack, format, 0);
}

static void
ReadWorker(Operand *sourceOp, Symbol *stack, char *format, Symbol *stack0)
{
  Symbol *s;
  char *text;
  Dimension *dims= 0;
  Operand *op;
  long i, number, lineCount;
  int j, nConversions, nArgs, typeID;
  int from_keybd = !sourceOp->ops;

  /* crack format string */
  nConversions= CrackFormat(format, &CrackScan);

  /* First pass through arguments counts them, checks data types and
     conformability, and matches them to conversions in the format list.  */
  CheckOps((int)(sp-stack+2));
  nArgs= 0;
  for ( ; stack<=sp ; stack++) {
    if (!stack->ops) { /* skip keywords */
      stack++;
      continue;
    }
    if (stack->ops==&referenceSym &&
        globTab[stack->index].ops!=&dataBlockSym) s= &globTab[stack->index];
    else s= stack;
    op= &ioOps[nArgs].o;
    s->ops->FormOperand(s, op);
    typeID= op->ops->typeID;
    if (typeID>T_DOUBLE && typeID!=T_STRING)
      YError("read cannot handle non-array, complex, pointer, or structure");
    if (from_keybd) {
      ioOps[nArgs].s.value.db = 0;
      ioOps[nArgs].s.index = -1;
      ioOps[nArgs].s.ops = ioOps[nArgs].o.owner->ops;
      if (ioOps[nArgs].s.ops != &dataBlockSym) {
        /* int, long, double scalars read directly into ioOps */
        if (s!=stack) ioOps[nArgs].s.index = stack->index,
                        ioOps[nArgs].s.value = globTab[stack->index].value;
        ioOps[nArgs].o.value = &ioOps[nArgs].s.value;
      }
    }
    if (nArgs<nConversions) {
      if (typeMatch[typeID]!=fmtList[nArgs].type)
        YError("read format/read output data type mismatch");
    } else if (!nArgs && !nConversions && fmtNow==1) {
      format= fmtList[0].format;
      fmtList[0].format= p_strncat(format, scanDefaults[typeMatch[typeID]], 0);
      p_free(format);
    } else {
      AddFormat(scanDefaults[typeMatch[typeID]], typeMatch[typeID], 0);
    }
    fmtList[nArgs].typeID= typeID;
    if (nArgs) {
      Dimension *tmp= op->type.dims;
      while (tmp && dims && tmp->number==dims->number) {
        tmp= tmp->next;
        dims= dims->next;
      }
      if (tmp || dims)
        YError("all outputs from formatted read must have same dimensions");
    }
    dims= op->type.dims;
    nArgs++;
  }
  number= TotalNumber(dims);

  if (nArgs<fmtNow) {
    /* must scan for matching junk after all arguments read */
    fmtList[nArgs].typeID= 8;  /* NilScanner */
    i= strlen(fmtList[nArgs].format);
    while (i && (fmtList[nArgs].format[i-1]=='\012' ||
                 fmtList[nArgs].format[i-1]=='\015' ||
                 fmtList[nArgs].format[i-1]==' '))
      fmtList[nArgs].format[--i]= '\0';  /* strip off trailing newlines */
    if (i) {
      if (from_keybd) {
        ioOps[nArgs].s.ops = 0;
        ioOps[nArgs].s.index = -1;
      }
      nArgs++;
    }
  }

  if (from_keybd) {
    if (p_signalling) p_abort();
    if (y_read_prompt)
      YError("read() while waiting for read() or rdline()");
    if (yp_continue)
      YError("read() while waiting for continued input for parser");
    yr_result = 0;
    yr_n = number;
    yr_j = 0;
    yr_nargs = nArgs;
    yr_nlines = fmtAssigns = 0;
    y_read_prompt = p_strcpy(sourceOp->value);
    PushLongValue(0);  /* actual result filled in later */
    /* get an extra use for result arrays so they will survive
     * being dropped off stack */
    for (j=0 ; j<nArgs ; j++)
      if (ioOps[j].s.ops==&dataBlockSym)
        ioOps[j].s.value.db = Ref(ioOps[j].o.owner->value.db);
    yr_pc_resume = ym_suspend();
    return;
  }

  /* outer loop is on input array elements */
  lineCount= 0;
  text= NextInLine(sourceOp, lineCount++);
  fmtAssigns= 0;
  for (i=0 ; i<number ; i++) {
    if (p_signalling) p_abort();
    /* inner loop is on arguments to be read */
    for (j=0 ; j<nArgs ; j++) {
      do {
        while (text && !text[0]) text= NextInLine(sourceOp, lineCount++);
        /* If input exhausted, NextInLine returns text==0.
         * If matching failure, Scanner returns text==0.  */

        /* Scanner returns zero if object was found, non-zero if not
         * found.  The text pointer is advanced past the number of
         * characters scanned, or set to zero if a matching failure
         * occurred.  */
      } while (Scanner[fmtList[j].typeID](&ioOps[j].o, fmtList[j].format,
                                          &text));
    }
  }

  /* release excessive temporary space */
  FreeFormats();

  /* return total number of objects actually assigned */
  PushLongValue(fmtAssigns);
}

void Y_rdline(int nArgs)
{
  Symbol *stack;
  Operand op;
  long i, nLines= 0;
  Array *result;
  Dimension *dims;
  char *keyNames[2], *q;
  Symbol *keySymbols[1];
  Symbol *stack0= sp-nArgs;

  keyNames[0]= "prompt";
  keyNames[1]= 0;
  stack= YGetKeywords(stack0+1, nArgs, keyNames, keySymbols);

  for (nArgs=0 ; stack<=sp ; stack++) {
    if (!stack->ops) {
      stack++;
      continue;
    }
    if (nArgs==1) nLines= YGetInteger(stack);
    else if (nArgs==0) {
      stack->ops->FormOperand(stack, &op);
      if (op.ops!=&textOps && op.ops!=&voidOps)
        YError("1st argument to rdline function not a text stream or nil");
    } else {
      YError("rdline function takes exactly one or two arguments");
    }
    nArgs++;
  }

  if (!nArgs || op.ops==&voidOps) op.ops= 0;
  else {
    TextStream *ts= op.value;
    p_file *file= ts->stream;
    if (!file)
      YErrorIO("attempt to read from closed I/O stream");
    else if (!(ts->permissions & 1))
      YErrorIO("attempt to read from file opened in w or a mode");
    if (ts->readWrite&2 && p_fseek(file, ts->readPosition)) {
      p_ferror(file);         /* don't prejudice future I/O attempts */
      YErrorIO("fseek failed to find current position in ASCII read");
    }
    ts->readWrite= 1;
  }
  if (!op.ops) {
    /* set prompt string for keyboard input */
    op.value= keySymbols[0]? YGetString(keySymbols[0]) : yr_dflt_prompts[0];
    if (!op.value) op.value= yr_dflt_prompts[1];
  }

  dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  if (nLines>0) tmpDims= NewDimension(nLines, 1L, tmpDims);
  else nLines= 1;
  result= PushDataBlock(NewArray(&stringStruct, tmpDims));

  if (!op.ops) {
    if (p_signalling) p_abort();
    if (y_read_prompt)
      YError("rdline() while waiting for read() or rdline()");
    if (yp_continue)
      YError("rdline() while waiting for continued input for parser");
    yr_result = result->value.q;
    yr_n = nLines;
    y_read_prompt = p_strcpy(op.value);
    yr_pc_resume = ym_suspend();
    return;
  }

  for (i=0 ; i<nLines ; i++) {
    q = NextInLine(&op, i);
    if (!q) break;
    result->value.q[i] = p_strcpy(q);
  }
}

/*--------------------------------------------------------------------------*/

void Y_write(int nArgs)
{
  Symbol *stack;
  char *format, *keyNames[3];
  Symbol *keySymbols[2];
  Operand sinkOp;

  keyNames[0]= "format";
  keyNames[1]= "linesize";
  keyNames[2]= 0;
  stack= YGetKeywords(sp-nArgs+1, nArgs, keyNames, keySymbols);

  /* Get 1st argument if it is an IOStream or nil, otherwise will use
     nil stream (keyboard).  */
  if (stack>sp) YError("write function takes at least one argument");
  stack->ops->FormOperand(stack, &sinkOp);
  if (sinkOp.ops==&textOps) {
    TextStream *ts= sinkOp.value;
    p_file *file= ts->stream;
    stack++;
    if (!file)
      YErrorIO("attempt to write to closed I/O stream");
    else if (!(ts->permissions & 2))
      YErrorIO("attempt to write to file opened in r mode");
    if (ts->readWrite&1 && p_fseek(file, p_fsize(file))) {
      p_ferror(file);          /* don't prejudice future I/O attempts */
      YErrorIO("fseek failed to find current position in ASCII write");
    }
    ts->readWrite= 2;
  } else {
    if (sinkOp.ops==&voidOps) stack++;
    sinkOp.ops= 0;
  }

  /* get format keyword, if any */
  format= keySymbols[0]? YGetString(keySymbols[0]) : 0;

  /* get linesize keyword, if any */
  lineSize= keySymbols[1]? YGetInteger(keySymbols[1]) : 80;

  WriteWorker(&sinkOp, stack, format);
}

void Y_swrite(int nArgs)
{
  Symbol *stack;
  char *format, *keyNames[2];
  Symbol *keySymbols[1];

  keyNames[0]= "format";
  keyNames[1]= 0;
  stack= YGetKeywords(sp-nArgs+1, nArgs, keyNames, keySymbols);

  /* get format keyword, if any */
  format= keySymbols[0]? YGetString(keySymbols[0]) : 0;

  WriteWorker((Operand *)0, stack, format);
}

static void
WriteWorker(Operand *sinkOp, Symbol *stack, char *format)
{
  Dimension *dims;
  Operand *op;
  long i, number;
  int j, nConversions, nArgs, typeID, nChars, nLine;
  char *text;
  Array *result;  /* for swrite only (sinkOp==0) */
  p_file *file= (sinkOp && sinkOp->ops)?
    ((TextStream *)sinkOp->value)->stream : (p_file *)0;

  /* crack format string, set fmtTotal */
  nConversions= CrackFormat(format, &CrackPrint);

  dims= pDims;
  pDims= 0;
  FreeDimension(dims);

  /* First pass through arguments counts them, checks data types and
     conformability, and matches them to conversions in the format list.  */
  CheckOps((int)(sp-stack+1));
  nArgs= 0;
  for ( ; stack<=sp ; stack++) {
    if (!stack->ops) { /* skip keywords */
      stack++;
      continue;
    }
    op= &ioOps[nArgs].o;
    stack->ops->FormOperand(stack, op);
    typeID= op->ops->typeID;
    if (typeID>T_DOUBLE && typeID!=T_STRING && typeID!=T_POINTER)
      YError("write cannot handle non-array, complex, or structure");
    if (nArgs<nConversions) {
      int ftype= fmtList[nArgs].type;
      if (ftype==IO_CHAR) ftype= IO_LONG;
      if (typeMatch[typeID]!=ftype)
        YError("write format/write input data type mismatch");
    } else if (!nArgs && !nConversions && fmtNow==1) {
      format= fmtList[0].format;
      fmtList[0].format=
        p_strncat(format, printDefaults[typeMatch[typeID]], 0);
      p_free(format);
    } else {
      AddFormat(printDefaults[typeMatch[typeID]], typeMatch[typeID], 0);
    }
    fmtList[nArgs].typeID= typeID;
    if (typeID==T_STRING) {
      /* A string field might expand by as much as the length
         of the longest string to be printed.  */
      char **q= op->value;
      long len;
      fmtWidth= 0;
      number= op->type.number;
      for (i=0 ; i<number ; i++)
        if (q[i] && (len= strlen(q[i]))>fmtWidth) fmtWidth= len;
      fmtTotal+= fmtWidth;
    } else {
      fmtTotal+= 25;  /* difficult to imagine longer numeric field */
    }
    if (nArgs) {
      if (Conform(pDims, op->type.dims) & 4)
        YError("all inputs to formatted write must be conformable");
      dims= pDims;
      pDims= Ref(tmpDims);
      FreeDimension(dims);
    } else {
      pDims= Ref(op->type.dims);
    }
    nArgs++;
  }
  number= TotalNumber(pDims);

  /* second pass broadcasts arguments to same size */
  for (j=0 ; j<nArgs ; j++) RightConform(pDims, &ioOps[j].o);

  /* Make sure the outBuf has at least fmtTotal characters, which should
     be a very conservative estimate of the maximum number of characters
     required by the Printer output routines.  */
  CheckOut(fmtTotal>80? fmtTotal : 80);
  outBuf[0]= '\0';

  if (!sinkOp)
    /* this is swrite call -- it's time to create the result array */
    result= PushDataBlock(NewArray(&stringStruct, pDims));
  else
    result= 0;

  /* outer loop is on output array elements */
  nLine= 0;
  fmtTotal= 0;
  for (i=0 ; i<number ; i++) {
    if (p_signalling) p_abort();
    /* inner loop is on arguments to be written */
    text= outBuf;
    text[0]= '\0';
    nChars= 0;
    for (j=0 ; j<nArgs ; j++) {
      if (fmtList[j].type == IO_CHAR)
        PrintC[fmtList[j].typeID](&ioOps[j].o, fmtList[j].format, text);
      else if (fmtList[j].danger)
        PrintD[fmtList[j].typeID-T_FLOAT](&ioOps[j].o,
                                          fmtList[j].format, text);
      else
        Printer[fmtList[j].typeID](&ioOps[j].o, fmtList[j].format, text);
      nChars= (int)strlen(text);  /* can't rely on sprintf to return this */
      text+= nChars;
      fmtTotal+= nChars;
      nLine+= nChars;
    }

    /* one result printed/stored per array element */
    if (!sinkOp) {
      result->value.q[i]= p_strcpy(outBuf);
    } else {
      /* extra line break explained in help,write documentation
       * nLine>nChars not mentioned in documentation prevents
       * extra line break if single argument is longer than lineSize
       */
      if (i && ((nArgs>nConversions && nArgs>1) ||
                (nLine>lineSize && nLine>nChars))) {
        if (sinkOp->ops && file) p_fputs(file, "\n");
        else p_stdout("\n");
        fmtTotal++;
        nLine= strlen(outBuf);
      }
      if (sinkOp->ops && file) p_fputs(file, outBuf);
      else p_stdout(outBuf);
      if (nChars && text[-1]=='\n') nLine= 0;
    }
  }

  /* add trailing newline if it seems reasonable to do so */
  if (sinkOp && nArgs>nConversions) {
    if (sinkOp->ops && file) p_fputs(file, "\n");
    else p_stdout("\n");
    fmtTotal++;
  }

  /* release excessive temporary space */
  FreeFormats();
  CheckOut(0L);

  /* if this is write (not swrite), result is character count */
  if (!result) PushLongValue(fmtTotal);
}

/*--------------------------------------------------------------------------*/

static long aFileID= 0;  /* unique file ID number for ASCII files */
IOFileLink *yTextFiles= 0;

/* Set up a block allocator which grabs space for 16 TextStream objects
   at a time.  Since TextStream contains several pointers, the alignment
   of an TextStream must be at least as strict as a void*.  */
static MemryBlock txtsBlock= {0, 0, sizeof(TextStream),
                              16*sizeof(TextStream)};

TextStream *NewTextStream(char *fullname, void *stream, int permissions,
                          long line, long pos)
{
  TextStream *ios= NextUnit(&txtsBlock);
  p_file *file= stream;

  ios->references= 0;
  ios->ops= &textOps;
  ios->stream= file;
  ios->fullname= fullname;
  ios->permissions= permissions;
  ios->lastLineRead= line;
  ios->readPosition= ios->lastPosition= pos;
  if (file && !(permissions&16) &&
      p_fseek(file, pos)) ios->permissions|= 16;
  ios->readWrite= 0;
  ios->fileID= aFileID++;

  AddIOLink(&yTextFiles, ios);
  return ios;
}

void FreeTextStream(void *ios)
{
  TextStream *io= ios;
  p_file *stream= io->stream;
  if (stream) p_fclose(stream);
  p_free(io->fullname);
  RemoveIOLink(yTextFiles, io);
  FreeUnit(&txtsBlock, io);
}

static char *txStatus[]=
  { "<illegal>", "read-only", "write-only", "read-write" };

static void PrintTX(Operand *op)
{
  TextStream *ts= op->value;
  long line= ts->lastLineRead+1;
  ForceNewline();
  if (ts->stream) {
    char text[32];
    sprintf(text, "%s text stream at:", txStatus[ts->permissions&3]);
    PrintFunc(text);
  } else {
    PrintFunc("text stream <closed> was:");
    line= 0;
  }
  ForceNewline();
  PrintFunc(MakeErrorLine(line, ts->fullname));
  ForceNewline();
}

/* Set up a block allocator which grabs space for 16 bookmark objects
   at a time.  Since Bookmark contains an ops pointer, the alignment
   of a Bookmark must be at least as strict as a void*.  */
static MemryBlock bookBlock= {0, 0, sizeof(Bookmark),
                                 16*sizeof(Bookmark)};

Bookmark *NewBookmark(long line, long last, long next, long id)
{
  Bookmark *bookmark= NextUnit(&bookBlock);
  bookmark->references= 0;
  bookmark->ops= &bookOps;
  bookmark->lastLineRead= line;
  bookmark->lastPosition= last;
  bookmark->readPosition= next;
  bookmark->fileID= id;
  return bookmark;
}

void FreeBookmark(void *bm)  /* ******* Use Unref(bm) ******* */
{
  FreeUnit(&bookBlock , bm);
}

static void PrintBM(Operand *op)
{
  Bookmark *bm= op->value;
  IOFileLink *iofl;
  for (iofl=yTextFiles ; iofl ; iofl=iofl->next)
    if (((TextStream *)iofl->ios)->fileID == bm->fileID) break;
  if (iofl) {
    TextStream *ts= iofl->ios;
    ForceNewline();
    PrintFunc("bookmark at:");
    ForceNewline();
    PrintFunc(MakeErrorLine(bm->lastLineRead+1, ts->fullname));
    ForceNewline();
  } else {
    PrintFunc("<lost bookmark>");
  }
}

void Y_bookmark(int nArgs)
{
  Operand op;
  TextStream *ios;
  if (nArgs!=1) YError("bookmark function takes exactly one argument");
  sp->ops->FormOperand(sp, &op);
  ios= op.value;
  if (op.ops!=&textOps)
    YError("argument to bookmark function not a text stream");
  if (ios->permissions&16) YError("can't place a bookmark in a pipe");
  PushDataBlock(NewBookmark(ios->lastLineRead, ios->lastPosition,
                            ios->readPosition, ios->fileID));
}

void Y_backup(int nArgs)
{
  Operand op;
  TextStream *ios;
  Bookmark *bm= 0;
  p_file *file;

  if (nArgs!=1 && nArgs!=2)
    YError("backup function takes exactly one or two arguments");
  if (nArgs==2) {
    sp->ops->FormOperand(sp, &op);
    if (op.ops==&bookOps)
      bm= op.value;
    else if (op.ops!=&voidOps)
      YError("2nd argument to backup function is not nil or bookmark");
    Drop(1);
  }
  sp->ops->FormOperand(sp, &op);
  ios= op.value;
  if (op.ops!=&textOps)
    YError("1st argument to backup function not a text stream");
  if (ios->permissions&16) YError("can't backup a pipe");
  file= ios->stream;

  /* don't try to detect no-op, as side effect of this routine is
     to ensure that fseek is called to make a read operation
     legal (previous operation may have been a write) */

  if (bm) {
    /* reset state to bookmark */
    struct IOFileLink *iofl;
    for (iofl=yTextFiles ; iofl ; iofl=iofl->next)
      if (((TextStream *)iofl->ios)->fileID == bm->fileID) break;
    if (!iofl) YError("no file for bookmark passed to backup function");
    if (iofl->ios!=ios)
      YError("wrong file for bookmark passed to backup function");
    if (p_fseek(file, bm->readPosition))
      YErrorIO("fseek failed in backup function");
    ios->lastLineRead= bm->lastLineRead;
    ios->lastPosition= bm->lastPosition;
    ios->readPosition= bm->readPosition;

  } else {
    /* back up to previous line */
    if (p_fseek(file, ios->lastPosition))
      YErrorIO("fseek failed in backup function");
    ios->readPosition= ios->lastPosition;
    ios->lastLineRead--;
  }

  ios->readWrite= 1;   /* fseek equivalent to read here */
}

/*--------------------------------------------------------------------------*/

static char *NextInLine(Operand *op, long n)
{
  char *text;
  if (op->ops==&textOps) {
    TextStream *stream= op->value;
    p_file *file= stream->stream;
    text= Ygets(&inputBuffer, file);
    if (!text) {
      int hadEOF= Yfeof(file);
      int hadError= Yferror(file);
      p_ferror(file);  /* don't prejudice later I/O attempts */
      if (hadError)
        YErrorIO("****ABORTING READ**** error reading input file");
      if (!hadEOF)
        YErrorIO("****ABORTING READ**** input file not ASCII text");
    } else {
      stream->lastLineRead++;
      stream->lastPosition= stream->readPosition;
      if (!(stream->permissions&16)) stream->readPosition= p_ftell(file);
    }
  } else if (op->ops==&stringOps) {
    char **q= op->value;
    if (n<op->type.number) text= q[n];
    else text= 0;
  } else {
    YError("(BUG) impossible operand to NextInLine");
    text= 0;
  }
  return text;
}

/*--------------------------------------------------------------------------*/

#undef OPERATION
#define OPERATION(opname, type1, type2) \
static int opname(Operand *op, char *format, char **text) \
{ \
  type1 *x= op->value; \
  type2 v;  int i, n; \
  if (*text) { \
    i= sscanf(*text, format, &v, &n); \
    if (i==1) { *x= (type1)v; fmtAssigns++; *text+= n; i= 0; \
    } else if (i==0)    {         *text= 0;     i= 1; \
    } else {         *text+= strlen(*text);     i= 1; \
    } \
  } else { \
    *x= 0; i= 0; \
  } \
  if (!i) op->value= x+1; \
  return i; \
}

OPERATION(CScanner, unsigned char, long)
OPERATION(SScanner, short, long)
OPERATION(IScanner, int, long)
OPERATION(LScanner, long, long)

/* floating point read operations cope with Fortran "D" exponent format */
static int retry_sscanf(char *text, char *format, double *pv, int *pn);
#undef OPERATION
#define OPERATION(opname, type1, type2) \
static int opname(Operand *op, char *format, char **text) \
{ \
  type1 *x= op->value; \
  type2 v;  int i, n; \
  if (*text) { \
    i= retry_sscanf(*text, format, &v, &n); \
    if (i==1) { *x= (type1)v; fmtAssigns++; *text+= n; i= 0; \
    } else if (i==0)    {         *text= 0;     i= 1; \
    } else {         *text+= strlen(*text);     i= 1; \
    } \
  } else { \
    *x= 0; i= 0; \
  } \
  if (!i) op->value= x+1; \
  return i; \
}

OPERATION(FScanner, float, double)
OPERATION(DScanner, double, double)

static int QScanner(Operand *op, char *format, char **text)
{
  char **x= op->value;
  char *v; int i, n;
  if (*x) { p_free(*x); *x= 0; }
  if (*text) {
    long len= strlen(*text);
    v= CheckBuf(len);  /* allow enough space for worst case */
    i= sscanf(*text, format, v, &n);
    if (i==1) { *x= p_strcpy(v); fmtAssigns++; *text+= n; i= 0;
    } else if (i==0)    {                   *text= 0;     i= 1;
    } else {                             *text+= len;     i= 1;
    }
  } else {
    i= 0;
  }
  if (!i) op->value= x+1;
  return i;
}

/* ARGSUSED */
static int NilScanner(Operand *op, char *format, char **text)
{
  if (!*text) return 0;
  if (strcmp(format,*text)) {
    *text= 0;
    return 1;
  } else {
    *text+= strlen(*text);
    return 0;
  }
}

/* Try to recognize things like 1.234d-21 or 1.234D-21 that Fortran emits.
 * The only reasonably cheap recourse is also a bit dangerous:
 *    We temporarily modify the text buffer and attempt a
 *    rescan.  Of course, if we're interrupted before we restore
 *    the modified text buffer, we may have modified the caller's
 *    text illegally.
 * Hopefully, a significant performance penalty accrues only if
 * the text you are reading contains things like "1.234e-21dumb".
 * However, a file full of "1.234D-21" style numbers will take
 * twice as long to read as one with ANSI C acceptable formats.
 *
 * New problem: Strings which overflow, e.g.- "1.e1000", insert Inf
 * and potentially mess with SIGFPE trap masking.  According to C9x
 * draft, this is supposed to set errno to ERANGE
 */
static int
retry_sscanf(char *text, char *format, double *pv, int *pn)
{
  int i;
  errno = 0;
  i = sscanf(text, format, pv, pn);
  if (i == 1) {
    int n = *pn;
    char c = text[n];
    if (c=='D' || c=='d') {
      /* this may represent only limited success if the scan
       * stopped at a "d" or "D" character */
      text[n] = 'e';
      errno = 0;
      i = sscanf(text, format, pv, pn);
      text[n] = c;
    }
  }
  if (errno) {  /* treat e.g. 1e1000 like non-numeric sequence */
#if !defined(_WIN32) && !defined(__CYGWIN__)
    extern void u_fpu_setup(int when);  /* playu.h */
    u_fpu_setup(-1);  /* on some Linux platforms resets SIGFPE trap mask */
#endif
    i = 0;
    *pv = 0.;
  }
  return i;
}

/*--------------------------------------------------------------------------*/

#undef OPERATION
#define OPERATION(opname, type1, type2) \
static void opname(Operand *op, char *format, char *text) \
{ \
  type1 *x= op->value; \
  type2 v= *x;  op->value= x+1; \
  sprintf(text, format, v); \
}

OPERATION(CPrinter, unsigned char, long)
OPERATION(SPrinter, short, long)
OPERATION(IPrinter, int, long)
OPERATION(LPrinter, long, long)
OPERATION(FPrinter, float, double)
OPERATION(DPrinter, double, double)
OPERATION(PPrinter, void *, void *)

static void QPrinter(Operand *op, char *format, char *text)
{
  char **x= op->value;
  char *v= *x;  op->value= x+1;
  sprintf(text, format, v? v : "");
}

#undef OPERATION
#define OPERATION(opname, type1) \
static void opname(Operand *op, char *format, char *text) \
{ \
  type1 *x= op->value; \
  double v= *x;  op->value= x+1; \
  if (v>1.e15) v= 1.e15;  else if (v<-1.e15) v= -1.e15; \
  sprintf(text, format, v); \
}

OPERATION(FPrintD, float)
OPERATION(DPrintD, double)

#undef OPERATION
#define OPERATION(opname, type1) \
static void opname(Operand *op, char *format, char *text) \
{ \
  type1 *x= op->value; \
  int v= *x;  op->value= x+1; \
  sprintf(text, format, v); \
}

OPERATION(CPrintC, unsigned char)
OPERATION(SPrintC, short)
OPERATION(IPrintC, int)
OPERATION(LPrintC, long)

/*--------------------------------------------------------------------------*/

static void CheckOps(int nArgs)
{
  if (nArgs >= maxIOops) {
    long newMax= maxIOops+16;
    while (nArgs >= newMax) newMax+= 16;
    ioOps= p_realloc(ioOps, sizeof(struct io_operand)*newMax);
    maxIOops= newMax;
  }
}

static void AddFormat(char *format, int type, int danger)
{
  if (fmtNow >= fmtMax) {
    fmtList= p_realloc(fmtList, sizeof(struct FormatList)*(fmtMax+16));
    fmtMax+= 16;
  }
  fmtList[fmtNow].format= p_strcpy(format);
  fmtList[fmtNow].type= type;
  fmtList[fmtNow].typeID= -1;
  fmtList[fmtNow++].danger= danger;
}

static void FreeFormats(void)
{
  while (fmtNow) p_free(fmtList[--fmtNow].format);
  if (fmtMax>32) {
    fmtList= p_realloc(fmtList, sizeof(struct FormatList)*32);
    fmtMax= 32;
  }
  if (maxIOops>32) {
    ioOps= p_realloc(ioOps, sizeof(struct io_operand)*32);
    maxIOops= 32;
  }
}

static int CrackFormat(char *format, char *(*Cracker)(char *))
{
  int nConversions= 0;
  fmtTotal= 0;

  /* free fmtList left over from last time, if any */
  while (fmtNow) p_free(fmtList[--fmtNow].format);

  /* Use either CrackScan or CrackPrint to split the format into pieces
     containing a single conversion specification corresponding to one
     item in the read or write argument list.  After this, either
     fmtNow==nConversions, or possibly fmtNow==1 and nConversions==0.  */
  if (format) {
    while (format[0]) {
      format= Cracker(format);
      if (fmtType!=IO_NONE) nConversions++;
      AddFormat(fmtBuf, fmtType, fmtDanger);
      fmtTotal+= fmtWidth;
    }
  }
  return nConversions;
}

static char *CheckBuf(long len)
{
  if (len+4 > fmtLen) {
    long newSize= 80*(1 + (len+4)/80);
    fmtBuf= p_realloc(fmtBuf, newSize);
    fmtLen= newSize;
  } else if (fmtLen>80 && len+4<=80) {
    fmtBuf= p_realloc(fmtBuf, 80L);
    fmtLen= 80L;
  }
  return fmtBuf;  /* used by CrackScan and CrackPrint for format strings */
}

static char *CheckOut(long len)
{
  if (len > outLen) {
    long newSize= 256*(1 + (len-1)/256);
    outBuf= p_realloc(outBuf, newSize);
    outLen= newSize;
  } else if (outLen>512 && len<=512) {
    outBuf= p_realloc(outBuf, 512L);
    outLen= 512L;
  }
  return outBuf;  /* used by Printer routines to hold sprintf results */
}

static char *CrackScan(char *format)
{
  int got_one;
  long i, n;
  char *part= CheckBuf(strlen(format));
  part[0]= '\0';
  fmtType= IO_NONE;
  got_one= 0;
  fmtWidth= 0;
  fmtDanger= 0;

  while (!got_one) { /* loop on conversion specifiers which do not assign */
    /* copy format until first conversion specifier */
    i= strcspn(format, "%");
    strncat(part, format, i);
    part+= i;
    format+= i;
    if (got_one || !format[0]) break;
    *part++= '%';
    *part= '\0';
    format++;

    /* find conversion type character */
    i= strcspn(format, "diouxXfeEgGs[cpn%");
    if (!format[i]) {
      strncat(part, format, i);
      break;
    }
    got_one= (format[i]!='%' && format[0]!='*');

    switch (format[i]) {
    case '%':
      i++;
      strncat(part, format, i);
      part+= i;
      format+= i;
      break;
    case '[':
      if (format[i+1]==']') i+= 2;
      else if (format[i+1]=='^' && format[i+2]==']') i+= 3;
      i+= strcspn(&format[i], "]");
    case 's':    /* actually can use case drop through here... */
      i++;
      strncat(part, format, i);
      part+= i;
      format+= i;
      fmtType= IO_STRING;
      break;
    case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
      /* all integers are handled as longs */
      if (format[i-1]=='h' || format[i-1]=='l') n= i-1;
      else n= i;
      strncat(part, format, n);
      part+= n;
      *part++= 'l';
      *part++= format[i];
      *part= '\0';
      format+= i+1;
      fmtType= IO_LONG;
      break;
    case 'e': case 'E': case 'f': case 'g': case 'G':
      /* all reals are handled as doubles */
      if (format[i-1]=='h' || format[i-1]=='l' ||
          format[i-1]=='L') n= i-1;
      else n= i;
      strncat(part, format, n);
      part+= n;
      *part++= 'l';
      *part++= format[i];
      *part= '\0';
      format+= i+1;
      fmtType= IO_DOUBLE;
      break;
    case 'p':
      YError("Yorick read cannot handle %p format, use %i");
      break;
    case 'c':
      YError("Yorick read cannot handle %c format, use %1s or %1[...]");
      break;
    case 'n':
      YError("Yorick read cannot handle %n format");
      break;
    }
  }

  /* append final character count to be able to advance input pointer */
  if (got_one) strcat(part, "%n");

  return format;
}

static char *CrackPrint(char *format)
{
  int got_one;
  long i, n;
  char *part= CheckBuf(strlen(format));
  part[0]= '\0';
  fmtType= IO_NONE;
  got_one= 0;
  fmtWidth= 0;
  fmtDanger= 0;

  for (;;) { /* loop on conversion specifiers which do not eat arguments */
    /* copy format until first conversion specifier */
    i= strcspn(format, "%");
    while (format[i]=='%' && format[i+1]=='%')
      i+= 2+strcspn(format+i+2, "%");    /* skip %% immediately */
    strncat(part, format, i);
    part+= i;
    format+= i;
    if (got_one || !format[0]) break;
    *part++= '%';
    *part= '\0';
    format++;

    /* find conversion type character */
    i= strcspn(format, "diouxXfeEgGscpn");
    if (!format[i]) break;
    for (n=0 ; n<i ; n++) {
      if (format[n] == '*')
        YError("Yorick write cannot handle %*.* format, compute format");
      if (!fmtWidth && format[n]>='1' && format[n]<='9') {
        /* get minimum field width, if specified */
        fmtWidth= format[n]-'0';
        for (n++ ; n<i && format[n]>='0' && format[n]<='9' ; n++)
          fmtWidth= 10*fmtWidth + format[n]-'0';
      }
    }
    got_one= 1;

    switch (format[i]) {
    case 's':
      i++;
      strncat(part, format, i);
      part+= i;
      format+= i;
      fmtType= IO_STRING;
      break;
    case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
      /* all integers are handled as longs */
      if (format[i-1]=='h' || format[i-1]=='l') n= i-1;
      else n= i;
      strncat(part, format, n);
      part+= n;
      *part++= 'l';
      *part++= format[i];
      *part= '\0';
      format+= i+1;
      fmtType= IO_LONG;
      break;
    case 'f':
      fmtDanger= 1;
    case 'e': case 'E': case 'g': case 'G':
      /* all reals are handled as doubles */
      if (format[i-1]=='L') n= i-1;
      else n= i;
      strncat(part, format, n);
      part+= n;
      *part++= format[i];
      *part= '\0';
      format+= i+1;
      fmtType= IO_DOUBLE;
      break;
    case 'c':
      i++;
      strncat(part, format, i);
      part+= i;
      format+= i;
      fmtType= IO_CHAR;
      break;
    case 'p':
      i++;
      strncat(part, format, i);
      part+= i;
      format+= i;
      fmtType= IO_POINTER;
      break;
    case 'n':
      YError("Yorick write cannot handle %n format, use strlen(swrite())");
      break;
    }
  }

  fmtWidth+= strlen(fmtBuf);
  return format;
}

/*--------------------------------------------------------------------------*/

void YErrorIO(const char *msg)
{
  extern int y_catch_category;
  y_catch_category= 0x02;
  YError(msg);
}

/*--------------------------------------------------------------------------*/

void Y_popen(int nArgs)
{
  char *command;
  int mode;
  p_file *file;
  if (nArgs!=2) YError("popen needs exactly two arguments");

  command= YGetString(sp-1);
  mode= (int)YGetInteger(sp);
  Drop(1);
  file= p_popen(command, mode?"w":"r");
  if (!file) YError("system popen function failed");
  PushDataBlock(NewTextStream(p_strcpy(command), file, mode?50:49, 0L, 0L));
}

void
Y_fflush(int nArgs)
{
  Operand op;
  if (nArgs!=1) YError("fflush accepts exactly one argument");
  sp->ops->FormOperand(sp, &op);
  if (sp->value.db->ops==&textOps) {
    TextStream *ts = (TextStream *)sp->value.db;
    if (ts->stream) p_fflush(ts->stream);
  } else if (sp->value.db->ops==&streamOps) {
    IOStream *file = (IOStream *)sp->value.db;
    if (file->permissions & 2) {
      ClearPointees(file, 1);
      FlushFile(file, 0);
    }
  } else {
    YError("fflush expecting file handle as argument");
  }
}

/*--------------------------------------------------------------------------*/
