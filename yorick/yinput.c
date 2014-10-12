/*
 * $Id: yinput.c,v 1.4 2010-02-15 05:17:57 dhmunro Exp $
 * Implement Yorick program text reader.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "yio.h"
#include "parse.h"
#include "hash.h"
#include "pstdlib.h"
#include "play.h"
#include <string.h>

#ifndef PATH_SEP_DELIMIT
#define PATH_SEP_DELIMIT ":;"
#endif

extern void YError(const char *msg);
extern int yDebugLevel;        /* declared in ydata.h */
extern HashTable globalTable;  /* declared in ydata.h */

extern char *MakeErrorLine(long lineNumber, const char *filename);

extern long YpLineNumber(void);

extern void y_on_stdin(char *input_line);

/*--------------------------------------------------------------------------*/

IncludeFile *ypIncludes= 0;
int nYpIncludes= 0;
static int maxYpIncludes= 0;

char **ypInputs= 0;
int nYpInputs= 0;
static int maxYpInputs= 0;

HashTable sourceTab;
long **sourceList= 0;
long *lenSourceList= 0;

char **ypPrefixes= 0;
int nYpPrefixes= 0;
static int maxYpPrefixes= 0;

static long rememberLine;  /* set in YpNextLine, used in YpStandby */

static void ClearPrefixes(void);
static void AddPrefix(char *prefix);

static void ClearSourceList(const char *name);

static int GetNextLine(p_file *file, int context);

static long FindSource(long index);
extern long ReopenSource(long index, int notExtern, long isrc);

extern int y_pending_stdin(void);
static void y_add_line(char *line);
static void y_remove_line(int reset);
typedef struct y_line_t y_line_t;
struct y_line_t {
  y_line_t *next;
  char *line;
};
static y_line_t *y_lhead=0, *y_ltail=0;

static int yp_chk_hash(p_file *file, int context, char *line);
static char *yp_strtok_text = 0;
static char *yp_strtok(char *text, char *delim);
static char *
yp_strtok(char *text, char *delim)
{
  char *start = 0;
  int i;
  if (text) yp_strtok_text = text;
  if (!yp_strtok_text) return 0;
  text = yp_strtok_text;
  /* skip leading delimiters */
  while (text[0]) {
    for (i=0 ; delim[i] ; i++) if (text[0]==delim[i]) break;
    if (!delim[i]) break;
    text++;
  }
  if (!text[0]) {
    yp_strtok_text = 0;
    return 0;
  }
  start = text;
  /* the whole point is to skip Windows drive letter */
  if (text[1]==':' &&
      ((text[0]>='A' && text[0]<='Z') ||
       (text[0]>='a' && text[0]<='z'))) text += 2;
  /* scan until delimiter */
  while (text[0]) {
    for (i=0 ; delim[i] ; i++) if (text[0]==delim[i]) break;
    if (delim[i]) break;
    text++;
  }
  if (!text[0]) {
    yp_strtok_text = 0;
  } else {
    text[0] = '\0';
    yp_strtok_text = text+1;
  }
  return start;
}

void YpSetPaths(const char *pathlist)
{
  char *paths= p_strcpy(pathlist);
  char *token= yp_strtok(paths, PATH_SEP_DELIMIT);
  char *prefix;

  ClearPrefixes();

  /* crack colon-or-space-delimited list of directory pathnames */
  while (token) {
    if (YIsDotRelative(token)) prefix= p_strcpy(token);
    else prefix= YExpandName(token);
    AddPrefix(prefix);
    token= yp_strtok((char *)0, PATH_SEP_DELIMIT);
  }

  p_free(paths);

  /* Set yCWD and yHOME if they haven't been initialized.  */
  if (!yCWD) YSetCWD((char *)0);
  if (!yHOME) YGetHOME();
}

static void ClearPrefixes(void)
{
  int i, n= nYpPrefixes;
  char **prefixes= ypPrefixes;
  nYpPrefixes= 0;
  for (i=0 ; i<n ; i++) p_free(prefixes[i]);
  maxYpPrefixes= 0;
  ypPrefixes= 0;
  p_free(prefixes);
}

static void AddPrefix(char *prefix)
{
  if (nYpPrefixes>=maxYpPrefixes) {
    int newSize= maxYpPrefixes+4;
    ypPrefixes= p_realloc(ypPrefixes, sizeof(char *)*newSize);
    maxYpPrefixes= newSize;
  }
  YNameToHead(&prefix);
  ypPrefixes[nYpPrefixes++]= prefix;
}

/* state for YpError initialized in YpPushInclude */
static long prevErrLine;

static p_file *PushInclude(const char *filename, int fullparse);

p_file *YpPushInclude(const char *filename)
{
  return PushInclude(filename, 1);
}

static p_file *default_on_include(const char *filename, int fullparse);
static yon_include_cb *open_include = &default_on_include;
/* ARGSUSED */
static p_file *
default_on_include(const char *filename, int fullparse)
{
  return p_fopen(filename, "r");
}

yon_include_cb *
ycall_on_include(yon_include_cb *on_include)
{
  yon_include_cb *old = open_include;
  open_include = on_include? on_include : &default_on_include;
  return old;
}

static p_file *PushInclude(const char *filename, int fullparse)
{
  p_file *file= 0;
  char *name= 0;
  long i;

  if (YIsAbsolute(filename)) {
    /* absolute pathname doesn't need any prefix */
    file= open_include(filename, fullparse);
    if (!file) return 0;
    name= p_strcpy(filename);

  } else {
    char *tmp;
    for (i=0 ; i<=nYpPrefixes ; i++) {
      if (i<nYpPrefixes) {
        tmp= p_strncat(ypPrefixes[i], filename, 0);
        name= YExpandName(tmp);
        p_free(tmp);
      } else {
        /* this branch is probably a bug --
         * if . is not on path probably should not find file...
         * maybe protects against empty path?
         */
        name= YExpandName(filename);
        if (!YIsAbsolute(name)) break;
      }
      file= open_include(name, fullparse);
      if (file) break;
      p_free(name);
    }
    if (!file) return 0;
  }

  if (nYpIncludes>=maxYpIncludes) {
    int newSize= maxYpIncludes+4;
    ypIncludes= p_realloc(ypIncludes, sizeof(IncludeFile)*newSize);
    maxYpIncludes= newSize;
  }

  if (fullparse) ClearSourceList(name);

  ypIncludes[nYpIncludes].file= file;
  ypIncludes[nYpIncludes].filename= name;
  ypIncludes[nYpIncludes].lastLineRead= 0;
  ypIncludes[nYpIncludes++].index = -1;
  prevErrLine= -1;
  return file;
}

void
y_push_include(p_file *file, const char *filename)
{
  if (nYpIncludes >= maxYpIncludes) {
    int newSize = maxYpIncludes + 4;
    ypIncludes = p_realloc(ypIncludes, sizeof(IncludeFile)*newSize);
    maxYpIncludes = newSize;
  }

  ypIncludes[nYpIncludes].file = file;
  ypIncludes[nYpIncludes].filename = p_strcpy(filename);
  ypIncludes[nYpIncludes].lastLineRead = 0;
  ypIncludes[nYpIncludes++].index = -1;
  prevErrLine= -1;
}

static int need_endif= 0;

void YpClearIncludes(void)
{
  need_endif= 0;
  if (nYpIncludes>0) {
    do {
      nYpIncludes--;
      if (ypIncludes[nYpIncludes].file)
        p_fclose(ypIncludes[nYpIncludes].file);
      p_free(ypIncludes[nYpIncludes].filename);
    } while (nYpIncludes);
  } else {
    nYpIncludes= 0;
  }
  YaltNextLine= 0;
  if (nYpInputs>0) do p_free(ypInputs[--nYpInputs]); while (nYpInputs);
  else nYpInputs= 0;

  /* also clear pending stdin lines */
  y_remove_line(1);
}

static void ClearSourceList(const char *name)
{
  if (HashFind(&sourceTab, name, 0L)) {
    p_free(sourceList[hashIndex]);
    sourceList[hashIndex]= 0;
    lenSourceList[hashIndex]= 0;
  }
}

/* Record the given globTab index in the sourceList.  This index
   corresponds to either a func definition, a struct definition, or an
   extern statement outside of any functions.  */
long RecordSource(long index)
{
  long isrc = -1;
  if (nYpIncludes) {
    long *list, len;
    if (HashAdd(&sourceTab, ypIncludes[nYpIncludes-1].filename, 0L)) {
      list = sourceList[hashIndex];
      len = lenSourceList[hashIndex];
    } else {
      HASH_MANAGE(sourceTab, long *, sourceList);
      HASH_MANAGE(sourceTab, long, lenSourceList);
      sourceList[hashIndex] = list = 0;
      lenSourceList[hashIndex] = len = 0;
    }
    if (!(len&7))
      sourceList[hashIndex] = list = p_realloc(list, sizeof(long)*(len+8));
    list[len++] = index;
    lenSourceList[hashIndex] = len;
    isrc = hashIndex;
  }
  return isrc;
}

void YpPush(const char *input)
{
  if (nYpInputs>=maxYpInputs) {
    int newSize= maxYpInputs+4;
    ypInputs= p_realloc(ypInputs, sizeof(char *)*newSize);
    maxYpInputs= newSize;
  }
  ypInputs[nYpInputs++]= p_strcpy(input);
}

p_file *YpPop(void)
{
  char *filename;
  p_file *file;
  if (nYpInputs<=0) return 0;
  filename= ypInputs[--nYpInputs];
  file= YpPushInclude(filename);
  if (!file) {
    char *msg;
    msg= p_strncat("missing include file ", filename, 0);
    YpError(msg);
    p_free(msg);
  }
  p_free(filename);
  return file;
}

/*--------------------------------------------------------------------------*/

long ypBeginLine= 0;
int ypSkipIncludes= 0;

extern int yImpossible;   /* used by YError, task.c */
extern char *y_read_prompt;  /* ascio.c */
extern int yg_blocking;      /* graph.c */

static YgetsLine ypBuffer;

char *(*YaltNextLine)(int context)= 0;

static int GetNextLine(p_file *file, int context)
{
  /* assert file!=0 */
  if (file && (yg_blocking || y_read_prompt)) return 0;

  if (!Ygets(&ypBuffer, file)) {
    if (file) {
      int hadEOF= Yfeof(file);
      int hadError= Yferror(file);
      p_fclose(file);
      ypIncludes[nYpIncludes-1].file= 0;
      /* Any errors here are serious enough to warrant a panic stop.  */
      if (hadError)
        YError("****ABORTING PARSE**** error reading include file");
      if (!hadEOF)
        YError("****ABORTING PARSE**** include file not ASCII text");
    }
    return 0;                   /* just a normal EOF */
  }
  if (nYpIncludes) {
    long lnum= ++ypIncludes[nYpIncludes-1].lastLineRead;
    if (context==NL_MAIN || context==NL_NOINPUT) ypBeginLine= lnum;
  } else {
    if (context==NL_MAIN || context==NL_NOINPUT) ypBeginLine= 0;
    else ypBeginLine--;
  }
  return 1;
}

long YpLineNumber(void)
{
  if (nYpIncludes>0) return ypIncludes[nYpIncludes-1].lastLineRead-1;
  else return -1;
}

/* YpStandby returns the file position remembered before the previous
   line was read by YpNextLine.  */
long YpStandby(void)
{
  return rememberLine;
}

extern void yr_reader(char *input_line);
extern int ym_dbenter;
extern int yp_continue;
int yp_continue = 0;
extern int yp_did_prompt;

static void
y_add_line(char *line)
{
  y_line_t *yline = p_malloc(sizeof(y_line_t));
  if (yline) {
    yline->next = 0;
    yline->line = p_strcpy(line);
    if (y_ltail) {
      y_ltail->next = yline;
      y_ltail = yline;
    } else {
      y_lhead = y_ltail = yline;
    }
  }
}

static void
y_remove_line(int reset)
{
  y_line_t *yline, *next;
  char *line;
  while (y_lhead) {
    yline = y_lhead;
    next = yline->next;
    line = yline->line;
    y_lhead = next;
    if (next)
      yline->next = 0;
    else
      y_ltail = 0;
    yline->line = 0;
    p_free(yline);
    if (line) p_free(line);
    if (!reset) break;
  }
}

void
y_on_stdin(char *input_line)
{
  int flag = ym_dbenter;
  ym_dbenter = yp_did_prompt = 0;
  if (!y_read_prompt) {
    if (input_line)
      while (input_line[0]==' ' || input_line[0]=='\t') input_line++;
    if (flag && input_line && input_line[0]) {
      extern void yg_got_expose(void);
      yp_continue = 0;
      if (p_signalling) p_abort();
      /* drop current debug block plus task and clean up vm state */
      ResetStack(0);
      yr_reset();
      yg_got_expose();
      p_clr_alarm(0, 0);
    }
    if (yg_blocking) {
      if (yg_blocking==2) {
        extern void yg_got_expose(void);
        yg_got_expose();  /* escape from pause on keyboard input */
        if (input_line && input_line[0])
          YputsErr("WARNING discarding keyboard input that aborts pause");
        return;
      } else {
        YputsErr("WARNING discarding keyboard input during window wait");
        return;
      }
    }
    y_add_line(input_line);
  } else {
    yr_reader(input_line);
  }
}

int
y_pending_stdin(void)
{
  int flag;
  if (y_lhead) {
    do {
      flag = yp_chk_hash(0, yp_continue, y_lhead->line);
      flag = yp_parse_keybd((flag&1)? 0 : y_lhead->line, !yp_continue);
      yp_continue = (flag!=0 && flag!=1);
      y_remove_line(0);
    } while (y_lhead && yp_continue);
    return (y_lhead!=0);
  } else {
    return 0;
  }
}

/* possible results:
 * 0 -- this line was not #include (or ypSkipIncludes), #if, or #endif
 *      or was a syntax error
 * 1 bit means get next line (successful include, #if 1, or #endif)
 * 2 -- this line was #include, file missing (YpError called)
 * 3 -- this line was #include, file pushed successfully
 * 4 -- this line was #if 0, no matching #endif
 * 5 -- this line was #if 0, skipped past matching #endif (in ypBuffer)
 * 7 -- this line was #if 1
 * 9 -- this line was #endif
 */
static int
yp_chk_hash(p_file *file, int context, char *line)
{
  /* Check whether this is an include line or an if line.
   * Correct format is ([OWS] means optional whitespace characters):
   * [OWS]#[OWS]include[OWS]"filename"[OWS]
   * or
   * [OWS]#[OWS]include[OWS]<filename>[OWS]
   */
  while (*line && (*line==' ' || *line=='\t' || *line=='\f')) line++;
  if (*line=='#' && line[1]) {
    char *incl= "include";
    line++;
    while (*line && (*line==' ' || *line=='\t')) line++;
    while (*line && *incl && (*line++ == *incl++));
    if (!*incl && context<NL_CONTINUE) {
      char delim;
      if (ypSkipIncludes) return 0;
      while (*line && (*line==' ' || *line=='\t')) line++;
      delim= *line;
      if (delim=='\"' || delim=='<') {
        char *filename= ++line;
        if (delim=='<') delim = '>';
        while (*line && *line!=delim) line++;
        if (*line && line>filename) {
          *line++= '\0';  /* 0-terminate filename */
          while (*line && (*line==' ' || *line=='\t')) line++;
          if (!*line) {
            char *msg;
            if ((file= YpPushInclude(filename))) return 3;
            msg= p_strncat("missing include file ", filename, 0);
            YpError(msg);
            p_free(msg);
            return 2;
          }
        }
      }

    } else if (incl[-1]=='n' && line[-1]=='f' &&
               (line[0]==' ' || line[0]=='\t') && file) {
      /* this is #if line, maybe should skip to matching #endif */
      line++;
      while (*line && (*line==' ' || *line=='\t')) line++;
      if ((line[0]=='0' || line[0]=='1')
          && (!line[1] || line[1]==' ' || line[1]=='\t')) {
        if (line[0]=='0') {
          int count = 0;
          for (;;) {
            if (p_signalling) p_abort();
            if (!GetNextLine(file, context)) return 4;
            line = ypBuffer.line;
            while (*line && (*line==' ' || *line=='\t' || *line=='\f'))
              line++;
            if (*line=='#') {
              line++;
              while (*line && (*line==' ' || *line=='\t')) line++;
              if (line[0]=='i' && line[1]=='f' &&
                  (line[2]==' ' || line[2]=='\t')) {
                count++;        /* nested #if (don't bother checking 0) */
              } else {
                char *endi = "endif";
                while (*line && *endi && (*line++ == *endi++));
                if (!*endi &&
                    (!line[0] || line[0]==' ' || line[0]=='\t') &&
                    !count--) return 5;
              }
            }
          }
        } else {  /* #if 1 */
          need_endif++;
          return 7;
        }
      }

    } else if (need_endif && incl[-1]=='i' && line[-1]=='e' &&
               line[0]=='n' && line[1]=='d' && line[2]=='i' &&
               line[3]=='f' &&
               (!line[4] || line[4]==' ' || line[4]=='\t')) {
      need_endif--;
      return 9;  /* read line after #endif */
    }
  }
  return 0;
}

char *
YpNextLine(int context)
{
  p_file *file;
  char *line;

  /* In skip-includes mode, remember where each line begins for
   * use by YpStandby.  */
  if (ypSkipIncludes && nYpIncludes && ypIncludes[nYpIncludes-1].file)
    rememberLine = p_ftell(ypIncludes[nYpIncludes-1].file);
  else
    rememberLine = -1;

  /* If there is an alternate input source, use it.  */
  if (YaltNextLine) {
    line = YaltNextLine(context);
    if (!line) YaltNextLine = 0;  /* "close" alternate input source */
    return line;
  }

  /* get the current include file */
  if (nYpIncludes==0) {
    file = 0;
  } else for (;;) {
    if ((file=ypIncludes[nYpIncludes-1].file)) break;
    need_endif = 0;
    p_free(ypIncludes[nYpIncludes-1].filename);
    if (!(--nYpIncludes)) break;
  }
  if (!file && nYpInputs) do file = YpPop(); while (!file && nYpInputs);

  /* quit if input is not from include file.  */
  if (!file) return 0;

  for (;;) {
    if (p_signalling) p_abort();
    /* Get an arbitrary (okay, < MAX_LINE) length input line.  */
    if (!GetNextLine(file, context)) return 0;
    /* if first line of file begins with #! discard it
     * (allows yorick source files to be scripts on most UNIX systems) */
    if (nYpIncludes && ypIncludes[nYpIncludes-1].lastLineRead==1 &&
        ypBuffer.line[0]=='#' && ypBuffer.line[1]=='!') {
      if (!GetNextLine(file, context)) return 0;
    }

    line = ypBuffer.line;
    if (line && context<=NL_CONTINUE) {
      int flag = yp_chk_hash(file, context, line);
      if (flag&1) {
        file = ypIncludes[nYpIncludes-1].file;
        continue;
      }
    }
    break;
  }

  yImpossible = 0;  /* tell YError that a line has come in */
  return line;
}

/*--------------------------------------------------------------------------*/

static char pErrorMsg[128];
static char *errorLoc= 0;

int ypErrors;        /* error count for most recent call to YpParse */
int ypMaxErrors= 16; /* give up after this many */

char *MakeErrorLine(long lineNumber, const char *filename)
{
  char *tmp= errorLoc;
  errorLoc= 0;
  p_free(tmp);
  if (lineNumber<0) pErrorMsg[0]= '\0';
  else sprintf(pErrorMsg, "  LINE: %ld  FILE: ", lineNumber);
  errorLoc= p_strncat(pErrorMsg, filename, 0);
  return errorLoc;
}

extern int yBatchMode;  /* may be set with -batch, see std0.c */
PLUG_API int yerror_flags;  /* see task.c */

void YpError(char *msg)
{
  if (ypSkipIncludes) return;  /* this is not a real parse */
  ypErrors++;
  strcpy(pErrorMsg, "SYNTAX: ");
  strncat(pErrorMsg, msg, 110);
  if (!(yerror_flags&1)) {
    YputsErr(pErrorMsg);
    if (nYpIncludes) {
      long lineNumber= ypIncludes[nYpIncludes-1].lastLineRead;
      if (lineNumber!=prevErrLine) {
        char *filename= ypIncludes[nYpIncludes-1].filename;
        YputsErr(MakeErrorLine(lineNumber, filename));
        prevErrLine= lineNumber;
      }
    }
  }
  if (yBatchMode || ypErrors>=ypMaxErrors || (yerror_flags&1)) {
    ypErrors= 0;
    if (ypMaxErrors<1) ypMaxErrors= 1;
    if (yerror_flags&1) YError(pErrorMsg);
    else YError("****ABORTING PARSE**** too many errors");
  }
}

/*--------------------------------------------------------------------------*/

static long FindSource(long index)
{
  long i, j, len, *list;

  /* Each source file i has an associated list of indices of the variables
     which were defined as func, struct, or *main* extern in that source
     file.  Search them in reverse order of the first time they were
     included -- note that this probably, but not necessarily, picks up
     the most recent definition.  This quirk constrains Yorick programming
     style somewhat -- you can't be as free in reusing a single function
     name as you might like...  */
  for (i=sourceTab.nItems-1 ; i>=0 ; i--) {
    list= sourceList[i];
    len= lenSourceList[i];
    /* inner loop is on variables defined in this source file */
    for (j=len-1 ; j>=0 ; j--) if (list[j]==index) break;
    if (j>=0) break;
  }

  return i;  /* -1 on failure */
}

long ReopenSource(long index, int notExtern, long isrc)
{
  long source, position;
  p_file *file;

  source = (isrc<0)? FindSource(index) : isrc;
  if (source<0) return -1;    /* source of func unknown */

  file= PushInclude(sourceTab.names[source], 0);
  if (!file) return -2;       /* unable to open source file */

  position= ScanForFunc(globalTable.names[index], notExtern);
  if (position<0 || p_fseek(file, position)) {
    if (ypIncludes[nYpIncludes-1].file) {
      p_fclose(file);
      ypIncludes[nYpIncludes-1].file= 0;
    }
    if (position<0) return -3;  /* func no longer in source file */
    else return -4;             /* seek error */
  }

  return position;
}

p_file *OpenSource(long index, long isrc)
{
  long position= ReopenSource(index, 0, isrc);
  p_file *file;
  if (position>=0) {
    file= ypIncludes[nYpIncludes-1].file;
    ypIncludes[nYpIncludes-1].file= 0;
  } else {
    file= 0;
  }
  return file;
}

/*--------------------------------------------------------------------------*/
