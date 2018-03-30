/*
 * $Id: clog.c,v 1.6 2010-07-03 19:42:31 dhmunro Exp $
 * Define routines to handle Contents Log (CLOG) language
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */


#ifdef NOT_YORICK
#include "binio.h"
#else
#include "ydata.h"
#endif
#include "yio.h"
#include "pstdlib.h"
#include <string.h>
#include <errno.h>

/* ------------------------------------------------------------------------ */

extern Converter PDBconvert;

extern int DumpClogFile(IOStream *file, const char *clogName);

struct CLbuffer {
  /* public interface */
  unsigned char *nextByte;  /* current position within buffer */
  long nextAddress;         /* address corresponding to current position */
  unsigned char *(*ReadBuffer)(CLbuffer *);  /* read full buffer beginning
                                                at nextAddress; put 0-byte
                                                after last byte read */

  /* public value of most recently fetched token */
  int tokType;    /* 0 for EOF, 1-255 for single character, else one of: */
#define TOK_IDENTIFIER 256
#define TOK_INTEGER 257
#define TOK_REAL 258
  union {
    long l;     /* value of a TOK_INTEGER */
    double d;   /* value of a TOK_REAL */
    struct {
      char *begin;    /* first character of identifier (in a CLbuffer) */
      long length;    /* number of characters in identifier */
    } id;       /* value of a TOK_IDENTIFIER */
  } tok;

  /* private state information */
  unsigned char *buffer;    /* the buffer itself */
  long address;             /* address corresponding to buffer[0]
                               ReadBuffer returns 0 if this matches
                               nextAddress (signals token longer than
                               size) */
  long size;                /* block size -- buffer is size+1 chars */
  int atEOF;                /* set by ReadBuffer if fewer than size
                               chars available -- ReadBuffer returns 0
                               if this is set and nextAddress>address
                               (signals end-of-file) */

  void *stream;     /* normally a FILE* (for ReadBuffer) */

  long nStructs;    /* number of +struct and +define definitions */
  long nVars;       /* number of variable declarations */
  long nRecords;    /* number of +record declarations, -1 if none,
                       0 if +record begin only */
  long eodValue;    /* most recent +eod address */
};

/* ------------------------------------------------------------------------ */

static char *CLgetName(IOStream *file);
static unsigned char *CLtextRead(CLbuffer *clBuffer);
static unsigned char *CLbinaryRead(CLbuffer *clBuffer);

static void CLinit(CLbuffer *clBuffer, p_file *stream,
                   long size, int isBinary);
static void CLzero(CLbuffer *clBuffer);
static int CLfind(CLbuffer *clBuffer);
static int CLnextToken(CLbuffer *clBuffer);
static int CLidMatch(const char *id, const CLbuffer *clBuffer);
static int CLsquare(CLbuffer *clBuffer, long *value);
static int CLdimension(CLbuffer *clBuffer, long *length,
                       int *flag, long *origin);
static int CLdims(CLbuffer *clBuffer);
static int CLfile(CLbuffer *clBuffer, IOStream *file);
static int CLrecord(CLbuffer *clBuffer, IOStream *file);
static int CLhistory(CLbuffer *clBuffer, HistoryInfo *history);
static int CLvariable(CLbuffer *clBuffer, IOStream *file);
static int CLstruct(CLbuffer *clBuffer, IOStream *file);
static int CLmembers(CLbuffer *clBuffer, StructDef *base);
static int CLdefine(CLbuffer *clBuffer, IOStream *file);
static int CLalign(CLbuffer *clBuffer, IOStream *file);
static int CLunknown(CLbuffer *clBuffer);
static int CLpublic(CLbuffer *clBuffer, IOStream *file);
static int CLprivate(CLbuffer *clBuffer, IOStream *file);

static int CLeod(CLbuffer *clBuffer, long *address);

static void CLdump(IOStream *file, CLbuffer *clBuffer);
static void CLputAlign(CLbuffer *clBuffer, int dataAlign, int structAlign);
static void CLputStruct(CLbuffer *clBuffer, StructDef *base);
static void CLputVar(CLbuffer *clBuffer, Member *type, char *name,
                     long address, int indent);
static void CLputIdent(p_file *stream, char *identifier);
static void CLputChunk(p_file *stream, char *identifier, long n);
static void CLputBegin(CLbuffer *clBuffer);
static void CLputRecord(CLbuffer *clBuffer, long rec, long *addr,
                        double *time, long *ncyc);
static void CLputEOD(CLbuffer *clBuffer, long address);

static int OpenWorker(IOStream *file, int familyOK,
                      p_file *stream, int isBinary);

/* ------------------------------------------------------------------------ */

static char *clogName= 0;

void CLupdate(IOStream *file)
{
  HistoryInfo *history= file->history;
  CLbuffer *clBuffer= file->contentsLog;

  if (file->fullname && y_vopen_file(file->stream)) return;

  if (history) {
    file= history->parent;
    if (!clBuffer) clBuffer= history->child->contentsLog;
  }

  if (!clBuffer) {
    p_file *stream;
    char *name= CLgetName(file);
    stream= p_fopen(name, "w+");  /* create the Clog file */
    clogName= 0;
    p_free(name);
    if (!stream) YError("unable to open Clog file to update binary file");
    clBuffer= p_malloc(sizeof(CLbuffer));
    CLinit(clBuffer, stream, 1024L, 0);
    if (history) history->child->contentsLog= clBuffer;
    else file->contentsLog= clBuffer;
  }

  CLdump(file, clBuffer);
  p_fflush((p_file *)clBuffer->stream);
}

int DumpClogFile(IOStream *file, const char *name)
{
  HistoryInfo *history= file->history;
  CLbuffer clBuffer;
  p_file *stream;

  if (file->fullname && y_vopen_file(file->stream)) return 0;

  stream= p_fopen(name, "w+");  /* create the Clog file */
  if (!stream) return 1;

  CLinit(&clBuffer, stream, 1024L, 0);
  if (history) CLdump(history->parent, &clBuffer);
  else CLdump(file, &clBuffer);
  CLzero(&clBuffer);
  return 0;
}

void FreeClogFile(IOStream *file)
{
  HistoryInfo *history= file->history;
  CLbuffer *clBuffer= file->contentsLog;

  if (!clBuffer) return;
  if (history) {
    IOStream *parent= history->parent;
    CLdump(parent, clBuffer);
    /* be careful not to kill the buffer if referenced twice */
    if (history->child->contentsLog==parent->contentsLog) clBuffer= 0;
  } else {
    CLdump(file, clBuffer);
  }
  file->contentsLog= 0;
  if (clBuffer) {
    CLzero(clBuffer);
    p_free(clBuffer);
  }
}

void ZapClogFile(IOStream *file)
{
  HistoryInfo *history= file->history;
  CLbuffer *childCL, *clBuffer;
  IOStream *child;
  char *name;
  if (history) {
    child= history->child;
    childCL= child->contentsLog;
    file= history->parent;
    clBuffer= file->contentsLog;
    if (childCL && childCL==clBuffer) childCL= child->contentsLog= 0;
  } else {
    child= 0;
    childCL= 0;
    clBuffer= file->contentsLog;
  }
  /* The removal of the log file must take place whether or not the
     contentsLog member is set.  */
  name= CLgetName(file);
  if (clBuffer) {
    file->contentsLog= 0;
    CLzero(clBuffer);
    p_free(clBuffer);
  }
  if (file->permissions&64) p_remove(name);
  clogName= 0;
  p_free(name);
  if (child) {
    name= CLgetName(child);
    if (childCL) {
      child->contentsLog= 0;
      CLzero(childCL);
      p_free(childCL);
    }
    if (file->permissions&64) p_remove(name);
    clogName= 0;
    p_free(name);
  }
}

static CLbuffer closeBuffer;

/* CLclose is the "native" CloseHook for Clog files */
void CLclose(IOStream *file)
{
  p_file *stream= file->stream;
  HistoryInfo *history= file->history;
  int oops;
  closeBuffer.stream= 0;
  CLzero(&closeBuffer);

  FreeClogFile(file);  /* don't want to advance eod here... */

  /* Even though we are writing a binary file, set the isBinary flag to
     zero in CLinit -- otherwise it would read file->stream.  */
  CLinit(&closeBuffer, stream, 1024L, 0);

  /* Since CLdump does not use file->ioOps, no point in using it here.  */
  p_fseek(stream, file->nextAddress);

  /* Dump the Clog description at the end of the file itself.  */
  if (history) CLdump(history->parent, &closeBuffer);
  else CLdump(file, &closeBuffer);
  oops= p_ferror(stream);
  closeBuffer.stream= 0;
  CLzero(&closeBuffer);

  /* If the Clog description was written successfully,
     remove file->contentsLog.  */
  if (!oops) ZapClogFile(file);
}

int CLopen(IOStream *file, int familyOK)
{
  if (OpenWorker(file, familyOK, (p_file *)file->stream, 2)) return 1;
  file->CloseHook= &CLclose;
  return 0;
}

int ReadClogFile(IOStream *file, const char *name)
{
  p_file *stream= p_fopen(name, "r");
  if (!stream) return 1;
  return OpenWorker(file, 0, stream, 0);
}

static int OpenWorker(IOStream *file, int familyOK,
                      p_file *stream, int isBinary)
{
  int notClog= 1;
  closeBuffer.stream= 0;
  CLzero(&closeBuffer);

  CLinit(&closeBuffer, stream, 1024L, isBinary);

  if (closeBuffer.atEOF==0 &&
      CLnextToken(&closeBuffer)==TOK_IDENTIFIER &&
      CLidMatch("Contents Log", &closeBuffer)) {
    notClog= 0;
    if (CLfile(&closeBuffer, file))
      YWarning("syntax error in Contents Log");
  }

  if (stream==file->stream) closeBuffer.stream= 0;
  CLzero(&closeBuffer);

  if (familyOK && !notClog && file->history) {
    HistoryInfo *history= file->history;
    IOStream *child= history->child;

    while (AddNextFile(history, (char *)0, 0)==0) {
      CLinit(&closeBuffer, child->stream, 1024L, 2);
      if (closeBuffer.atEOF==0) CLhistory(&closeBuffer, history);
      closeBuffer.stream= 0;
      CLzero(&closeBuffer);
    }

    JumpRecord(history, 0);  /* position to first record */
  }

  return notClog;
}

static char *CLgetName(IOStream *file)
{
  HistoryInfo *history= file->history;
  char *name= clogName;
  clogName= 0;
  p_free(name);
  if (history) name= history->famNames[history->nFamily-1];
  else name= file->fullname;
  return clogName= p_strncat(name, "L", 0);
}

/* ------------------------------------------------------------------------ */

/* CLtextRead is the ReadBuffer member of a CLbuffer which corresponds
   to an open text file.  In this case, the file is read one line
   at a time using p_fgets, with line size limited by the buffer size.  */
static unsigned char *CLtextRead(CLbuffer *clBuffer)
{
  long address= clBuffer->nextAddress;
  unsigned char *buffer= clBuffer->buffer;

  /* check for EOF or token-too-long error */
  if (address==clBuffer->address ||
      (clBuffer->atEOF && address>clBuffer->address)) return 0;

  /* upate the state information */
  clBuffer->address= address;  /* this may not actually be an address */

  /* read the next line -- don't bother trying to fill more of the buffer */
  if (!p_fgets(clBuffer->stream, (char *)buffer, clBuffer->size+1)) return 0;

  return clBuffer->nextByte= buffer;
}

/* CLbinaryRead is the ReadBuffer member of a CLbuffer which corresponds
   to an open binary file.  In this case, the file is read one buffer
   at a time using fread.  */
static unsigned char *CLbinaryRead(CLbuffer *clBuffer)
{
  long address= clBuffer->nextAddress;
  unsigned char *buffer= clBuffer->buffer;
  long nBytes, size= clBuffer->size;
  p_file *stream= clBuffer->stream;

  /* check for EOF or token-too-long error */
  if (address==clBuffer->address ||
      (clBuffer->atEOF && address>clBuffer->address)) return 0;

  /* upate the state information */
  clBuffer->address= address;

  /* read the next bufferfull, overlapping the previous buffer if
     a token was split */
  if (clBuffer->atEOF) p_ferror(stream);
  if (p_fseek(stream, address)) return 0;
  nBytes= p_fread(stream, buffer, size);
  if (nBytes<0) nBytes= 0;
  buffer[nBytes]= '\0';
  if (nBytes<size) {
    /* if (!p_feof(stream)) return 0; */
    clBuffer->atEOF= 1;
  }

  return clBuffer->nextByte= buffer;
}

/* ------------------------------------------------------------------------ */

static void CLinit(CLbuffer *clBuffer, p_file *stream, long size, int isBinary)
{
  clBuffer->nextAddress= 0;
  clBuffer->address= -1;
  clBuffer->ReadBuffer= isBinary? &CLbinaryRead : &CLtextRead;
  clBuffer->atEOF= 0;
  clBuffer->stream= stream;
  clBuffer->size= size;
  clBuffer->nextByte= clBuffer->buffer= p_malloc(size+1);
  clBuffer->buffer[0]= '\0';
  clBuffer->nStructs= clBuffer->nVars= clBuffer->eodValue= 0;
  clBuffer->nRecords= -1;

  /* optionally try to find +eod statement in final 80 characters of file,
     and position the file accordingly if found */
  if (isBinary==2) CLfind(clBuffer);
}

static void CLzero(CLbuffer *clBuffer)
{
  p_file *stream= clBuffer->stream;
  unsigned char *buffer= clBuffer->buffer;
  clBuffer->stream= 0;
  clBuffer->buffer= 0;
  clBuffer->size= 0;
  clBuffer->nextByte= 0;
  p_free(buffer);
  if (stream) p_fclose(stream);
}

static int CLfind(CLbuffer *clBuffer)
{
  p_file *stream= clBuffer->stream;
  int failure= 1;
  long eof= p_fsize(stream);

  if (eof!=-1L && eof>80) {
    int tokType;
    clBuffer->nextAddress= eof-80;

    for (;;) {
      while ((tokType= CLnextToken(clBuffer)))
        if (tokType=='+') break;
      if (!tokType) {
        break;
      }
      if (CLnextToken(clBuffer)==TOK_IDENTIFIER &&
          CLidMatch("eod", clBuffer)) {
        long address;
        if (CLeod(clBuffer, &address)==0) {
          if (!p_fseek(stream, address)) {
            clBuffer->atEOF= 0;
            clBuffer->nextAddress= address;
            failure= 0;
          }
        }
        break;
      }
    }
  }

  clBuffer->nextByte[0]= '\0'; /* force immediate read */
  if (failure) {
    clBuffer->atEOF= 1;
    clBuffer->nextAddress= clBuffer->address+1;
  }

  if (eof<0) p_ferror(stream);

  return failure;
}

/* ------------------------------------------------------------------------ */

static int CLnextToken(CLbuffer *clBuffer)
{
  unsigned char c, *nextByte= clBuffer->nextByte;
  unsigned char *now= nextByte;
  int tokType;

  clBuffer->tokType= 0;  /* invalidate previous token */

  /* skip past anything remotely resembling whitespace */
  for (;;) {
    while ((c= *nextByte) && (c<=0x20 || c>=0x7f)) nextByte++;
    clBuffer->nextAddress+= (nextByte-now);
    now= nextByte;

    if (c=='/') {
      if (nextByte[1]=='*') {
        /* a comment has been introduced, scan until concluded */
        nextByte+= 2;
        for (;;) {
          while ((c= *nextByte++) && (c!='*' || *nextByte!='/'));
          if (c) break;
          nextByte= clBuffer->ReadBuffer(clBuffer);
          if (!nextByte) return 0;
        }
        nextByte++;   /* skip over closing slash */
        continue;     /* scan past any more whitespace */
      } else if (nextByte[1]) {
        break;
      }
      /* drop through if '/' is final character in this buffer, since it
         may introduce a comment */
    } else if (c) {
      break;
    }

    now= nextByte= clBuffer->ReadBuffer(clBuffer);
    if (!nextByte) return 0;
  }

  /* try to make a token after whitespace has been passed
     -- if buffer runs out before token definitely finished, reread
        the buffer starting at the beginning of the token */
  for (;;) { /* (at most two passes) */

    if ((c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_') {
      /* ordinary identifier */
      while ((c= *(++nextByte)) &&
             ((c>='a' && c<='z') || (c>='A' && c<='Z') ||
              (c>='0' && c<='9') ||
              c=='_' || c=='-' || c=='+' || c=='.' || c==','));
      if (c) {
        tokType= TOK_IDENTIFIER;
        clBuffer->tok.id.begin= (char *)now;
        clBuffer->tok.id.length= nextByte-now;
        break;
      }

    } else if (c=='"') {
      /* quoted identifier string must handle escape sequences */
      unsigned char *compress= nextByte;
      while ((c= *(++nextByte)) && c!='"') {
        if (c!='\\') {
          *compress++= c;
        } else {
          if (!(c= *(++nextByte))) break;
          if (c=='"') {
            *compress++= c;     /* \" escape --> " */
          } else if (c=='\\') {
            *compress++= c;     /* \\ escape --> \ */
          } else if (c>='0' && c<='7') {
            /* \o or \oo or \ooo escape where o is any octal digit */
            int cv= (int)(c-'0');
            if (!(c= *(++nextByte))) break;
            if (c>='0' && c<='7') {
              cv= (cv<<3) + (int)(c-'0');
              if (!(c= *(++nextByte))) break;
              if (c>='0' && c<='7') {
                cv= (cv<<3) + (int)(c-'0');
                c= *(++nextByte);
              }
            }
            *compress++= cv;
          }
        }
      }
      if (c) {
        nextByte++;       /* skip past terminal " character (c=='"') */
        tokType= TOK_IDENTIFIER;
        clBuffer->tok.id.begin= (char *)now;
        clBuffer->tok.id.length= compress-now;
        break;
      }

    } else if ((c>='0' && c<='9') || c=='+' || c=='-') {
      /* either naked + or -, or some sort of a number */
      char *stopByte;
      int i;
      int i0= (nextByte[0]!='+' && nextByte[0]!='-')? 0 : 1;
      for (i=1 ; (c=nextByte[i]) ; i++) if (c<'0' || c>'9') break;
      if (c=='.' &&
          (i>i0 ||
           (nextByte[i+1]>='0' && nextByte[i+1]<='9'))) {
        /* floating point constants must contain a decimal point
           with at least one digit on either side */
        tokType = TOK_REAL;
        errno = 0;
        clBuffer->tok.d = strtod((char *)nextByte, &stopByte);
        nextByte = (unsigned char *)stopByte;
        if (errno)
          clBuffer->tok.d = 0., tokType = '?';

      } else if (i>i0) {
        /* integer constants must have at least one digit */
        tokType= TOK_INTEGER;
        clBuffer->tok.l= strtol((char *)nextByte, &stopByte, 10);
        nextByte= (unsigned char *)stopByte;

      } else {
        /* just a + or - that is not part of a number */
        tokType= *nextByte++;
      }
      if (nextByte[0]) break;

    } else if (nextByte[1]) {
      /* any other character must be punctuation of some sort */
      tokType= *nextByte++;
      break;
    }

    /* Since nextAddress hasn't been updated, ReadBuffer will return
       0 if we reach this point on second pass through this loop.  */
    now= nextByte= clBuffer->ReadBuffer(clBuffer);
    if (!nextByte) return 0;
    c= *nextByte;
  }

  /* update address in clBuffer */
  clBuffer->nextAddress+= (nextByte-now);
  clBuffer->nextByte= nextByte;

  return clBuffer->tokType= tokType;
}

/* ------------------------------------------------------------------------ */

static int CLidMatch(const char *id, const CLbuffer *clBuffer)
{
  return strncmp(id, clBuffer->tok.id.begin, clBuffer->tok.id.length)==0;
}

static int CLsquare(CLbuffer *clBuffer, long *value)
{
  int tokType= CLnextToken(clBuffer);
  if (tokType!='[') return tokType;

  if (CLnextToken(clBuffer)!=TOK_INTEGER) return -1;
  *value= clBuffer->tok.l;

  tokType= CLnextToken(clBuffer);
  if (tokType!=']') return -3;

  /* CLsquare returns 0 if "[value]" was found --
     CLnextToken should be called to find the token following the
     close square bracket */
  return clBuffer->tokType= 0;  /* invalidate the close bracket */
}

static int CLdimension(CLbuffer *clBuffer, long *length,
                       int *flag, long *origin)
{
  int tokType= CLnextToken(clBuffer);
  if (tokType!='[') return tokType;

  if (CLnextToken(clBuffer)!=TOK_INTEGER) return -1;
  *length= clBuffer->tok.l;

  tokType= CLnextToken(clBuffer);
  if (tokType==':') {
    long first= *origin= *length;
    if (CLnextToken(clBuffer)!=TOK_INTEGER) return -2;
    *length= clBuffer->tok.l - first + 1;
    tokType= CLnextToken(clBuffer);
    *flag= 1;
  } else {
    *flag= 0;
  }

  if (tokType!=']') {
    if (tokType!=TOK_IDENTIFIER) return -3;
    /* clBuffer->tok.id points to dimension_name -- ignore here */
    tokType= CLnextToken(clBuffer);
    if (tokType!=']') return -4;
  }

  /* CLdimension returns 0 if a dimension descriptor was found --
     CLnextToken should be called to find the token following the
     close square bracket */
  return clBuffer->tokType= 0;  /* invalidate the close bracket */
}

static int CLdims(CLbuffer *clBuffer)
{
  /* fetch a dimension_spec -- a sequence of dimension specifiers */
  long origin=0, length=0;
  int tokType, flag=0;
  Dimension *prev, *dims= tmpDims;

  /* clear any previous temporary dimension */
  tmpDims= prev= 0;
  FreeDimension(dims);

  /* loop past all dimension specifiers, building tmpDims */
  for (tokType= CLdimension(clBuffer, &length, &flag, &origin) ;
       tokType==0 ;
       tokType= CLdimension(clBuffer, &length, &flag, &origin)) {
    dims= NewDimension(length, flag? origin : 1L, (Dimension *)0);
    if (prev) prev->next= dims;
    else tmpDims= dims;
    prev= dims;
  }

  return tokType;
}

/* ------------------------------------------------------------------------ */

static int CLfile(CLbuffer *clBuffer, IOStream *file)
{
  int tokType= CLnextToken(clBuffer);

  while (tokType>0) {

    if ((tokType=='+' || tokType=='-') &&
        CLnextToken(clBuffer)==TOK_IDENTIFIER) {
      if (tokType=='+') {
        /* +identifier */
        if (CLidMatch("struct", clBuffer))         /* +struct */
          tokType= CLstruct(clBuffer, file);
        else if (CLidMatch("define", clBuffer))    /* +define */
          tokType= CLdefine(clBuffer, file);
        else if (CLidMatch("record", clBuffer))    /* +record */
          tokType= CLrecord(clBuffer, file);
        else if (CLidMatch("align", clBuffer))     /* +align */
          tokType= CLalign(clBuffer, file);
        else if (CLidMatch("eod", clBuffer))       /* +eod */
          {long address;
           tokType= CLeod(clBuffer, &address);
           if (tokType) tokType= -2000;
           else file->nextAddress= address;}
        else
          tokType= CLpublic(clBuffer, file);

      } else {
        /* -identifier   (private extension syntax) */
        tokType= CLprivate(clBuffer, file);
      }

    } else if (tokType==TOK_IDENTIFIER) {
      /* variable declaration */
      tokType= CLvariable(clBuffer, file);

    } else {
      /* syntax error, give up */
      tokType= -1000;
    }
  }

  return tokType;  /* 0 on success */
}

static int CLhistory(CLbuffer *clBuffer, HistoryInfo *history)
{
  IOStream *child= history->child;

  int tokType= CLnextToken(clBuffer);

  while (tokType>0) {

    if (tokType=='+' && CLnextToken(clBuffer)==TOK_IDENTIFIER &&
        CLidMatch("record", clBuffer)) {
      /* +record */
      tokType= CLrecord(clBuffer, child);
      /* Note-- +record begin generates an error here, but should
         recover and continue without any problem */

    } else {
      int depth= (tokType=='{')? 1 : 0;
      while (tokType && depth) {
        tokType= CLnextToken(clBuffer);
        if (tokType=='{') depth++;
        else if (tokType=='}') depth--;
      }
    }
  }

  return tokType;  /* 0 on success */
}

/* ------------------------------------------------------------------------ */

static char *tmpName= 0;  /* temporary to hold names */

static int CLvariable(CLbuffer *clBuffer, IOStream *file)
{
  /* already got type_name, format is:
        type_name variable_name[dim1][dim2] @address
   */
  int tokType;
  long address;
  char *name= tmpName;
  StructDef *base;

  /* clear previous scratch name, if any */
  tmpName= 0;
  p_free(name);

  /* type_name must have been previously defined */
  if (!HashFind(&file->structTable,
                clBuffer->tok.id.begin, clBuffer->tok.id.length))
    return -100;
  base= file->structList[hashIndex];

  do {
    /* get variable name */
    if (CLnextToken(clBuffer)!=TOK_IDENTIFIER) return -101;
    tmpName= p_strncat(0, clBuffer->tok.id.begin, clBuffer->tok.id.length);

    /* get dimension specifications (sets tmpDims) */
    tokType= CLdims(clBuffer);
    if (tokType<0) return tokType-110;

    /* get address if given */
    if (tokType=='@') {
      if (CLnextToken(clBuffer)!=TOK_INTEGER) return -102;
      address= clBuffer->tok.l;
      if (address<0) return -103;
      tokType= CLnextToken(clBuffer);
    } else {
      address= -1;  /* default value */
    }

    /* add the variable to the file */
    if (AddVariable(file, address, tmpName, base, tmpDims)) return -104;

    /* clear the name scratch */
    name= tmpName;
    tmpName= 0;
    p_free(name);

  } while (tokType==',');

  return tokType;  /* token after these variables has been fetched */
}

/* ------------------------------------------------------------------------ */

static int CLstruct(CLbuffer *clBuffer, IOStream *file)
{
  /* already got +struct, format is:
        +struct type_name { member_definition member_definition* }
   */
  StructDef *base;

  /* type_name must NOT have been previously defined --
     except possibly string or pointer */
  if (CLnextToken(clBuffer)!=TOK_IDENTIFIER) return -200;
  base= AddStruct(file, clBuffer->tok.id.begin, clBuffer->tok.id.length);
  if (!base) return -201;

  return CLmembers(clBuffer, base);
}

static int CLmembers(CLbuffer *clBuffer, StructDef *base)
{
  /* fill a StructDef from a member list:
        {full_member_definition member_defintion*}
   */
  int tokType;
  IOStream *file= base->file;
  StructDef **structList= file->structList;
  long offset;
  char *name= tmpName;
  tmpName= 0;
  p_free(name);

  if (CLnextToken(clBuffer)!='{') return -202;

  tokType= CLnextToken(clBuffer);
  while (tokType && tokType!='}') {
    if (tokType==TOK_IDENTIFIER) {
      /* member type_name must already exist */
      if (!HashFind(&file->structTable,
                    clBuffer->tok.id.begin, clBuffer->tok.id.length))
        return -203;

    } else if (tokType==',') {
      /* comma means this member has same type_name as previous member */
      long nItems= base->table.nItems;
      if (nItems<=0) return -204;    /* this is first member */
      /* fake the side effect of HashFind */
      hashIndex= base->members[nItems-1].base->index;

    } else {
      return -205;
    }

    /* member_name is next */
    if (CLnextToken(clBuffer)!=TOK_IDENTIFIER) return -206;
    tmpName= p_strncat(0, clBuffer->tok.id.begin, clBuffer->tok.id.length);

    /* followed by optional dimension specifier(s) */
    tokType= CLdims(clBuffer);
    if (tokType<0) return tokType-210;

    /* finally, an optional offset may override the default member
       placement within the struct */
    if (tokType=='@') {
      if (CLnextToken(clBuffer)!=TOK_INTEGER) return -207;
      offset= clBuffer->tok.l;
      tokType= CLnextToken(clBuffer);
    } else {
      offset= -1;
    }

    /* now add this member to the struct */
    if (AddMember(base, offset, tmpName,
                  structList[hashIndex], tmpDims)) return -208;

    name= tmpName;
    tmpName= 0;
    p_free(name);
  }

  if (tokType!='}' || base->table.nItems<=0) return -209;

  InstallStruct(base, (StructDef *)0);
  return CLnextToken(clBuffer);
}

/* ------------------------------------------------------------------------ */

static int CLdefine(CLbuffer *clBuffer, IOStream *file)
{
  /* already got +define, format is:
        +define type_name [size_value][alignment_value][order_value]
                          { sa ea es ma ms mf bias }
     special forms:
        +define type_name [size_value][alignment_value][sequential]
        +define type_name [size_value][alignment_value][pdbpointer]
        +define string standard
        +define pointer standard
   */
  long size=0, alignment=0, order;
  int replacing, flag, tokType, seqFlag;
  StructDef *base;
  FPLayout fpLayout;

  if (CLnextToken(clBuffer)!=TOK_IDENTIFIER) return -300;
  if (HashFind(&file->structTable,
               clBuffer->tok.id.begin, clBuffer->tok.id.length)) {
    base= file->structList[hashIndex];
    if (base->references || hashIndex>=8) return -301;
    replacing= 1;
  } else {
    base= AddStruct(file, clBuffer->tok.id.begin, clBuffer->tok.id.length);
    if (!base) return -301;
    replacing= 0;
  }

  tokType= CLsquare(clBuffer, &size);
  if (tokType==TOK_IDENTIFIER && CLidMatch("standard", clBuffer) &&
      replacing && (hashIndex==6 || hashIndex==7)) {
    /* standard string or pointer */
    return CLnextToken(clBuffer);
  } else if (tokType) {
    return -302;
  } else {
    tokType= CLsquare(clBuffer, &alignment);
    if (tokType) return -303;
  }

  seqFlag= 0;
  tokType= CLnextToken(clBuffer);
  if (tokType=='[') {
    tokType= CLnextToken(clBuffer);
    if (tokType==TOK_IDENTIFIER &&
        (CLidMatch("sequential", clBuffer) ||
         CLidMatch("pdbpointer", clBuffer))) {
      seqFlag= clBuffer->tok.id.begin[0]=='s'? 1 : 2;
      order= 0;  flag= 0;
      if (CLnextToken(clBuffer)!=']') return -306;
      tokType= CLnextToken(clBuffer);
    } else if (tokType==TOK_INTEGER) {
      order= clBuffer->tok.l;
      if (CLnextToken(clBuffer)!=']') return -304;
      /* explicit order value found */
      tokType= CLnextToken(clBuffer);
      if (tokType=='{') {
        /* fpLayout is present */
        if (CLnextToken(clBuffer)!=TOK_INTEGER) return -305;
        fpLayout.sgnAddr= clBuffer->tok.l;
        if (CLnextToken(clBuffer)!=TOK_INTEGER) return -305;
        fpLayout.expAddr= clBuffer->tok.l;
        if (CLnextToken(clBuffer)!=TOK_INTEGER) return -305;
        fpLayout.expSize= clBuffer->tok.l;
        if (CLnextToken(clBuffer)!=TOK_INTEGER) return -305;
        fpLayout.manAddr= clBuffer->tok.l;
        if (CLnextToken(clBuffer)!=TOK_INTEGER) return -305;
        fpLayout.manSize= clBuffer->tok.l;
        if (CLnextToken(clBuffer)!=TOK_INTEGER) return -305;
        fpLayout.manNorm= clBuffer->tok.l;
        if (CLnextToken(clBuffer)!=TOK_INTEGER) return -305;
        fpLayout.expBias= clBuffer->tok.l;
        if (CLnextToken(clBuffer)!='}') return -306;
        tokType= CLnextToken(clBuffer);
        flag= 1;
      } else {
        flag= 0;
      }
    } else {
      return -304;
    }
  } else {
    /* default order value represents opaque primitive data type */
    order= 0;  flag= 0;
  }

  if (replacing) {
    Unref(base->model);
    base->model= 0;
    base->dataOps= 0;
    base->Convert= 0;
  }
  base->size= size;
  base->alignment= alignment;
  base->order= order;
  if (seqFlag) base->addressType= 2;
  if (seqFlag==2) {
    base->model= RefNC(&pointerStruct);
    base->Convert= &PDBconvert;
  }
  if (flag) base->fpLayout= MakeFPLayout(&fpLayout, size);
  InstallStruct(base, (StructDef *)0);

  return tokType;
}

/* ------------------------------------------------------------------------ */

static int CLrecord(CLbuffer *clBuffer, IOStream *file)
{
  /* already got +record, formats are:
          +record begin
     first time, or, subsequently,
          +record {time_float, ncyc_long} @address
     */
  HistoryInfo *history= file->history;
  int tokType, flags= 3;
  double time=0.0;
  long cycle=0, address;

  tokType= CLnextToken(clBuffer);
  if (tokType==TOK_IDENTIFIER) {
    if (!CLidMatch("begin", clBuffer) || history) return -400;
    history= AddHistory(file, 0L);
    return CLnextToken(clBuffer);
  }

  /* this is not the first +record */
  if (tokType!='{') return -401;
  if (!history) history= AddHistory(file, 0L);

  tokType= CLnextToken(clBuffer);
  if (tokType==TOK_REAL) time= clBuffer->tok.d;
  else if (tokType==TOK_INTEGER) time= (double)clBuffer->tok.l;
  else if (tokType==',') flags^= 1;
  else return -402;
  if (tokType!=',' && CLnextToken(clBuffer)!=',') return -403;

  tokType= CLnextToken(clBuffer);
  if (tokType==TOK_INTEGER) cycle= clBuffer->tok.l;
  else if (tokType=='}') flags^= 2;
  else return -404;
  if (tokType!='}' && CLnextToken(clBuffer)!='}') return -405;

  tokType= CLnextToken(clBuffer);
  if (tokType=='@') {
    if (CLnextToken(clBuffer)!=TOK_INTEGER) return -406;
    address= clBuffer->tok.l;
    tokType= CLnextToken(clBuffer);
  } else {
    address= -1;
  }

  if (AddRecord(history, flags, time, cycle, address)) return -407;

  return tokType;
}

/* ------------------------------------------------------------------------ */

static int CLalign(CLbuffer *clBuffer, IOStream *file)
{
  /* already got +align, format is:
        +align ("variables" | "structs") [alignment]
   */
  int flag;
  long alignment=0;

  if (CLnextToken(clBuffer)!=TOK_IDENTIFIER) return -500;
  if (CLidMatch("variables", clBuffer)) flag= 1;
  else if (CLidMatch("structs", clBuffer)) flag= 0;
  else return -501;

  if (CLsquare(clBuffer, &alignment)) return -502;

  if (flag) {
    if (alignment<0) return -503;
    file->dataAlign= (int)alignment;
  } else {
    if (alignment==0 || alignment<-2) return -504;
    file->structAlign= (int)alignment;
  }

  return CLnextToken(clBuffer);
}

/* ------------------------------------------------------------------------ */

static int CLunknown(CLbuffer *clBuffer)
{
  /* already got +public_extension or -private_extension, skip
        [extension_id] "{" extension_data "}" ["@" disk_address]
   */
  int tokType, count;

  tokType= CLnextToken(clBuffer);
  if (tokType==TOK_IDENTIFIER) tokType= CLnextToken(clBuffer);
  if (tokType!='{') return -600;

  count= 1;
  do {
    tokType= CLnextToken(clBuffer);
    if (tokType<=0) return -601;
    if (tokType=='{') count++;
    else if (tokType=='}') count--;
  } while (count);

  tokType= CLnextToken(clBuffer);
  if (tokType=='@') {
    if (CLnextToken(clBuffer)!=TOK_INTEGER) return -602;
    tokType= CLnextToken(clBuffer);
  }

  return tokType;
}

static int CLpublic(CLbuffer *clBuffer, IOStream *file)
{
  int tokType;

  if (CLidMatch("pedigree", clBuffer)) tokType= CLunknown(clBuffer);
  else if (CLidMatch("attributes", clBuffer)) tokType= CLunknown(clBuffer);
  else if (CLidMatch("value", clBuffer)) tokType= CLunknown(clBuffer);
  else if (CLidMatch("PDBcast", clBuffer)) tokType= CLunknown(clBuffer);
  else if (CLidMatch("PDBpointer", clBuffer)) tokType= CLunknown(clBuffer);

  else tokType= CLunknown(clBuffer);

  return tokType;
}

static int CLprivate(CLbuffer *clBuffer, IOStream *file)
{
  return CLunknown(clBuffer);
}

/* ------------------------------------------------------------------------ */

static int CLeod(CLbuffer *clBuffer, long *address)
{
  /* already got +eod, format is:    +eod @address  */

  if (CLnextToken(clBuffer)!='@') return -904;
  if (CLnextToken(clBuffer)!=TOK_INTEGER) return -905;
  *address= clBuffer->tok.l;
  return CLnextToken(clBuffer);
}

/* ------------------------------------------------------------------------ */
/* ------------------------------------------------------------------------ */

char *idScratch= 0;

static void CLdump(IOStream *file, CLbuffer *clBuffer)
{
  HistoryInfo *history= file->history;
  IOStream *child= history? history->child : 0;
  StructDef **structList;
  Member *types;
  long *addresses, offset, rec0= 0, i;
  char **names;

  long nRecords= history? history->nRecords : -1;
  long nrStructs= file->structTable.nItems;
  long nrVars= file->dataTable.nItems;
  long eodNow= file->nextAddress;
  long rStructs= 0, rVars= 0;
  long clStructs= clBuffer->nStructs;
  long clVars= clBuffer->nVars;
  long clRecords= clBuffer->nRecords;
  long eodThen= clBuffer->eodValue;

  int touched= 0;

  if (idScratch) {
    /* this can happen only if a previous CLdump call was interrupted */
    char *scratch= idScratch;
    idScratch= 0;
    p_free(scratch);
  }

  if (child) {
    rStructs= child->structTable.nItems;
    rVars= child->dataTable.nItems;
    if (nRecords>0) {
      int *ifile= history->ifile;
      int current= history->nFamily-1;

      for (i=nRecords-1 ; i>=0 ; i--) if (ifile[i]!=current) break;
      rec0= i+1;          /* index of 1st record in current file */
      nRecords-= rec0;    /* number of records in current file */

      eodNow= child->nextAddress;

      if (current>0 && !history->copyParent) nrStructs= nrVars= 0;
    }
  }

  if (nrStructs+rStructs+nrVars+rVars<=0) return;

  if (clStructs<1)
    CLputAlign(clBuffer, file->dataAlign, file->structAlign);

  if (nrStructs>clStructs) {
    if (!idScratch) idScratch= p_malloc(1024L);
    structList= file->structList;
    for (i=clStructs ; i<nrStructs ; i++)
      CLputStruct(clBuffer, structList[i]);
    clStructs= nrStructs;
    touched= 1;
  }

  if (nrVars>clVars) {
    if (!idScratch) idScratch= p_malloc(1024L);
    names= file->dataTable.names;
    types= file->types;
    addresses= file->addresses;
    offset= file->offset;
    for (i=clVars ; i<nrVars ; i++)
      CLputVar(clBuffer, &types[i], names[i], addresses[i]+offset, 0);
    clVars= 0;
    touched= 1;
  } else {
    clVars-= nrVars;
  }

  if (child) {
    long *addr= history->offset;
    double *time= history->time;
    long *ncyc= history->ncyc;
    if (clRecords<0) {
      CLputBegin(clBuffer);
      clRecords= rec0;
      touched= 1;
    }

    if (rStructs>clStructs) {
      if (!idScratch) idScratch= p_malloc(1024L);
      structList= child->structList;
      for (i=clStructs ; i<rStructs ; i++)
        CLputStruct(clBuffer, structList[i]);
      touched= 1;
    }

    if (rVars>clVars) {
      if (!idScratch) idScratch= p_malloc(1024L);
      names= child->dataTable.names;
      types= child->types;
      addresses= child->addresses;
      for (i=clVars ; i<rVars ; i++)
        CLputVar(clBuffer, &types[i], names[i], addresses[i], 0);
      touched= 1;
    }

    if (nRecords>clRecords) {
      for (i=clRecords ; i<nRecords ; i++)
        CLputRecord(clBuffer, i+rec0, addr, time, ncyc);
      touched= 1;
    }
  }

  if (touched || eodNow>eodThen) CLputEOD(clBuffer, eodNow);

  if (idScratch) {
    char *scratch= idScratch;
    idScratch= 0;
    p_free(scratch);
  }
}

/* ------------------------------------------------------------------------ */

static void CLputAlign(CLbuffer *clBuffer, int dataAlign, int structAlign)
{
  p_file *stream= clBuffer->stream;
  sprintf(p_wkspc.c, "\"Contents Log\"\n"); p_fputs(stream, p_wkspc.c);
  sprintf(p_wkspc.c, "+align variable [%d]\n+align struct [%d]\n",
          dataAlign, structAlign); p_fputs(stream, p_wkspc.c);
}

/* ------------------------------------------------------------------------ */

static void CLputStruct(CLbuffer *clBuffer, StructDef *base)
{
  p_file *stream= clBuffer->stream;
  long i, nItems= base->table.nItems;

  clBuffer->nStructs++;

  if (nItems>0) {
    /* compound data structure */
    char **names= base->table.names;
    Member *members= base->members;
    long *offsets= base->offsets;
    long off, expect= 0;

    sprintf(p_wkspc.c, "+struct "); p_fputs(stream, p_wkspc.c);
    CLputIdent(stream, StructName(base));
    sprintf(p_wkspc.c, " {\n"); p_fputs(stream, p_wkspc.c);
    for (i=0 ; i<nItems ; i++) {
      base= members[i].base;
      off= offsets[i];
      if (AlignAdjust(expect, base->alignment)==off)
        CLputVar(clBuffer, &members[i], names[i], -1L, 1);
      else
        CLputVar(clBuffer, &members[i], names[i], off, 1);
      off+= members[i].number*base->size;
      if (off>expect) expect= off;
    }
    sprintf(p_wkspc.c, "}\n"); p_fputs(stream, p_wkspc.c);

  } else {
    /* primitive data type */
    sprintf(p_wkspc.c, "+define "); p_fputs(stream, p_wkspc.c);
    CLputIdent(stream, StructName(base));
    if ((base->index==6 && base->Convert==&ConvertQ) ||
        (base->index==7 && base->Convert==&ConvertP)) {
      sprintf(p_wkspc.c, " standard\n"); p_fputs(stream, p_wkspc.c);
    } else {
      sprintf(p_wkspc.c, " [%ld][%d]", base->size, base->alignment);
      p_fputs(stream, p_wkspc.c);
      if (base->addressType==2) {
        if (base->Convert!=&PDBconvert) sprintf(p_wkspc.c, "[sequential]");
        else sprintf(p_wkspc.c, "[pdbpointer]");
      } else {
        sprintf(p_wkspc.c, "[%d]", base->order);
      }
      p_fputs(stream, p_wkspc.c);
      if (base->fpLayout) {
        FPLayout *l= base->fpLayout;
        sprintf(p_wkspc.c,
                " {%d %d %d %d %d %d %ld}\n", l->sgnAddr, l->expAddr,
                l->expSize, l->manAddr, l->manSize, l->manNorm, l->expBias);
      } else {
        sprintf(p_wkspc.c, "\n");
      }
      p_fputs(stream, p_wkspc.c);
    }
  }
}

/* ------------------------------------------------------------------------ */

static void CLputVar(CLbuffer *clBuffer, Member *type, char *name,
                     long address, int indent)
{
  p_file *stream= clBuffer->stream;
  Dimension *dims= type->dims;

  clBuffer->nVars++;

  if (indent) { sprintf(p_wkspc.c, "  "); p_fputs(stream, p_wkspc.c); }
  CLputIdent(stream, StructName(type->base));
  sprintf(p_wkspc.c, " "); p_fputs(stream, p_wkspc.c);
  CLputIdent(stream, name);
  while (dims) {
    if (dims->origin!=1L)
      sprintf(p_wkspc.c, "[%ld:%ld]",
              dims->origin, dims->origin+dims->number-1);
    else
      sprintf(p_wkspc.c, "[%ld]", dims->number);
    p_fputs(stream, p_wkspc.c);
    dims= dims->next;
  }
  if (address>=0) sprintf(p_wkspc.c, "@%ld\n", address);
  else sprintf(p_wkspc.c, "\n");
  p_fputs(stream, p_wkspc.c);
}

static void CLputIdent(p_file *stream, char *identifier)
{
  long i= 0;
  unsigned char c;

  while ((c=(unsigned char)identifier[i]) &&
         ((c>='a' && c<='z') || (c>='0' && c<='9') || (c>='A' && c<='Z') ||
          c=='_' || c=='-' || c=='+' || c=='.' || c==',')) i++;

  if (identifier[i] || (identifier[0]<='9' && identifier[0]>='0')) {
    /* must quote this string */
    sprintf(p_wkspc.c, "\""); p_fputs(stream, p_wkspc.c);
    if (i>0) CLputChunk(stream, identifier, i);
    identifier+= i;
    c= (unsigned char )identifier[0];
    if (c) {
      if (c=='\"') sprintf(p_wkspc.c, "\\\"");
      else if (c=='\\') sprintf(p_wkspc.c, "\\\\");
      else if (c<0x20 || c>=0x7f) sprintf(p_wkspc.c, "\\%o", (int)c);
      else sprintf(p_wkspc.c, "%c", (int)c);
      p_fputs(stream, p_wkspc.c);
      identifier++;
      i= 0;
      while ((c= identifier[i])) {
        if (c=='\"') {
          if (i>0) CLputChunk(stream, identifier, i);
          sprintf(p_wkspc.c, "\\\"");
          identifier+= i+1;
          i= 0;
        } else if (c=='\\') {
          if (i>0) CLputChunk(stream, identifier, i);
          sprintf(p_wkspc.c, "\\\\");
          identifier+= i+1;
          i= 0;
        } else if (c<0x20 || c>=0x7f) {
          if (i>0) CLputChunk(stream, identifier, i);
          sprintf(p_wkspc.c, "\\%o", (int)c);
          identifier+= i+1;
          i= 0;
        } else {
          i++;
        }
      }
      /* FIXME: buffer overrun? */
      sprintf(p_wkspc.c, "%s\"", identifier);
      p_fputs(stream, p_wkspc.c);
    }

  } else {
    /* no need for quotes */
    /* FIXME: buffer overrun? */
    sprintf(p_wkspc.c, "%s", identifier);
    p_fputs(stream, p_wkspc.c);
  }
}

static void CLputChunk(p_file *stream, char *identifier, long n)
{
  if (n>=1024) YError("impossibly long identifier in CLputChunk");
  idScratch[0]= '\0';
  strncat(idScratch, identifier, n);
  sprintf(p_wkspc.c, "%s", idScratch);
  p_fputs(stream, p_wkspc.c);
}

/* ------------------------------------------------------------------------ */

static void CLputBegin(CLbuffer *clBuffer)
{
  p_file *stream= clBuffer->stream;
  clBuffer->nRecords= 0;
  sprintf(p_wkspc.c, "+record begin\n"); p_fputs(stream, p_wkspc.c);
}

static void CLputRecord(CLbuffer *clBuffer, long rec, long *addr,
                        double *time, long *ncyc)
{
  p_file *stream= clBuffer->stream;

  clBuffer->nRecords++;

  if (time) {
    if (ncyc) sprintf(p_wkspc.c, "+record {%.9e,%ld} @%ld\n",
                      time[rec], ncyc[rec], addr[rec]);
    else sprintf(p_wkspc.c, "+record {%.9e,} @%ld\n", time[rec], addr[rec]);
  } else {
    if (ncyc) sprintf(p_wkspc.c,
                      "+record {,%ld} @%ld\n", ncyc[rec], addr[rec]);
    else sprintf(p_wkspc.c, "+record {,} @%ld\n", addr[rec]);
  }
  p_fputs(stream, p_wkspc.c);
}

/* ------------------------------------------------------------------------ */

static void CLputEOD(CLbuffer *clBuffer, long address)
{
  p_file *stream= clBuffer->stream;
  long backup= p_ftell(stream);
  if (backup<0) p_ferror(stream);
  sprintf(p_wkspc.c, "+eod @%ld\n", address); p_fputs(stream, p_wkspc.c);
  if (p_fseek(stream, backup)) p_ferror(stream);
  clBuffer->eodValue= address;
}

/* ------------------------------------------------------------------------ */
