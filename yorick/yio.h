/*
 * $Id: yio.h,v 1.4 2010-01-01 00:59:21 dhmunro Exp $
 * Declare Yorick I/O functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef YIO_H
#define YIO_H

#include "hash.h"

/*--------------------------------------------------------------------------*/

#include "pstdio.h"
#include <stdio.h>

/*--------------------------------------------------------------------------*/

/* Yorick wrappers for fgets, feof, and ferror.  */
PLUG_API char *Yfgets(char *s, int n, p_file *stream);
PLUG_API int Yfeof(p_file *stream);
PLUG_API int Yferror(p_file *stream);

/* Yorick routines use the following virtual functions to do fgets and
   fputs to stdin, stdout, and stderr.  Other packages may replace the
   default routines when initializing if more sophistocated I/O is required,
   as for an X window graphics package.  */
PLUG_API int (*YPrompt)(char *s);      /* no \n appended, like fputs */
PLUG_API int (*YputsOut)(char *s);     /* \n appended, like puts */
PLUG_API int (*YputsErr)(char *s);     /* \n appended, like puts */
PLUG_API int YDPrompt(char *s);
PLUG_API int YDputsOut(char *s);
PLUG_API int YDputsErr(char *s);

/*--------------------------------------------------------------------------*/
/* The gets function is handy, but dicey if you aren't sure how long
   the input line is.  Ygets circumvents this problem at the cost of
   a new data type, the YgetsLine, which manages an input line buffer.
   Each call to Ygets returns either getsLine->line, or 0 if EOF or
   an error:
      if (!Ygets(&buf, file)) {
         if (Yferror(file))     { there really was a read error }
         else if (!Yfeof(file)) { no \n after MAX_LINE chars read }
         else                   { end-of-file (no characters first) }
      }
   The returned buf.line contains one line of input, with the trailing
   \n stripped (if EOF before EOL, nothing was stripped).
   Ygets keeps buf.max between MIN_LINE and BIG_LINE; if it is bigger
   than BIG_LINE initially, it is reallocated with length MIN_LINE.
   More characters are added to the buffer as needed in increments of
   INC_LINE.  If more than MAX_LINE characters are read without a \n,
   Ygets gives up, sets buf.line="", and returns 0.
   Ygets calls Yfgets, and so obeys the latter's convention for stream==0.
 */
typedef struct YgetsLine YgetsLine;
struct YgetsLine {
  char *line;    /* pointer to current line buffer, no '\n' */
  int max;       /* line created with p_malloc(max) */
  int n;         /* strlen(line) */
};

PLUG_API char *Ygets(YgetsLine *getsLine, p_file *stream);

/*--------------------------------------------------------------------------*/

/* Filename gymnastics are inevitable, but system dependent (at least
   on non-UNIX platforms).  The following are intended to help.
   Free char* results with p_free when finished.  */
PLUG_API char *YExpandName(const char *name);    /* return absolute pathname */
PLUG_API char *YNameTail(const char *name);
PLUG_API char *YNameHead(const char *name);           /* includes trailing / */
PLUG_API void YNameToHead(char **name);         /* ensure trailing / present */
PLUG_API int YIsAbsolute(const char *name);       /* Does name start with /? */
PLUG_API int YIsDotRelative(const char *name);  /* ., .., or with ./ or ../? */

PLUG_API char *yCWD;      /* current working directory, including trailing / */
PLUG_API char *yHOME;     /* home directory from $HOME, including trailing / */

/* Use p_free to get rid of return value from Ygetenv.  */
PLUG_API char *Ygetenv(const char *name);

/* YSetCWD returns non-0 if operation fails, resets yCWD on success.
   If name==0, just sets yCWD to ".".  */
PLUG_API int YSetCWD(const char *name);
PLUG_API void YGetHOME(void);  /* sets yHOME */

/*--------------------------------------------------------------------------*/

/* Scan for C-style escape sequences in quoted strings (e.g.- \n, \t),
   returning the (single character) value of the escape sequence, and,
   if the 2nd parameter is non-0, the character which stopped the scan.
   Thus, if s=="tXYZ", then YpEscapeSeq returns 9 (ASCII tab), and
   endp=="XYZ"; the same results would obtain if s=="011XYZ".  */
PLUG_API int YpEscapeSeq(const char *s, char **endp);

/*--------------------------------------------------------------------------*/
/* Yorick include files are managed using the following variables
   and functions.  */

/* ypIncludes-- the stack of currently open include files, maintain using
   YpPushInclude and YpClearIncludes */
typedef struct IncludeFile IncludeFile;
struct IncludeFile {
  p_file *file;
  char *filename;     /* expanded filename (allocated with p_malloc) */
  long lastLineRead;  /* number of times Ygets has been called */
  long index;         /* into globTab of current variable */
};

PLUG_API IncludeFile *ypIncludes;
PLUG_API int nYpIncludes;

/* YpPushInclude opens a file and pushes it onto the include stack.
      The very next input will be taken from that file.
   IncludeNow parses the file just pushed by YpPushInclude
   YpPush pushes a filename onto the pending input sources list.
      When all other sources of input are exhausted, Yorick will
      attempt to include these files in reverse order to the YpPush calls.
   YpClearIncludes closes all files on the current include stack, and
      forgets the contents of the pending input list.  */
PLUG_API p_file *YpPushInclude(const char *filename);
PLUG_API void IncludeNow(void);
PLUG_API void YpPush(const char *filename);      /* filename will be copied */
PLUG_API p_file *YpPop(void);  /* call YpPushInclude with top of input list */
PLUG_API void YpClearIncludes(void);
/* like YpPushInclude, but does not open file */
PLUG_API void y_push_include(p_file *file, const char *filename);
/*
  The on_include callback returns a p_file* on success, or 0 on failure.
  ycall_on_include sets this callback, returing the old callback.
  ycall_on_include(0) resets the default include file open function,
    which is simply p_fopen
  (This is an esoteric function needed by mpy.)
*/
typedef p_file *yon_include_cb(const char *filename, int fullparse);
PLUG_API yon_include_cb *ycall_on_include(yon_include_cb *on_include);

PLUG_API void ResetStack(int hard);
PLUG_API void yr_reset(void);

/* sourceTab-- a permanent list of all files which have ever been included */
PLUG_API HashTable sourceTab;
PLUG_API long **sourceList;   /* maxItems array of pointers to lists of
                                 globTab indices of func/struct/extern
                                 definitions in corresponding source file */
PLUG_API long *lenSourceList; /* length of sourceList[i] is lenSourceList[i] */

/* Record the given globTab index in the sourceList.  This index
   corresponds to either a func definition, a struct definition, or an
   extern statement outside of any functions.  */
PLUG_API long RecordSource(long index);

PLUG_API long ypBeginLine;  /* source line number at which current parse
                               began (<=0 if input from keyboard) */

/* Given an index into globTab, OpenSource searches through the source
   lists to find the the source file in which the corresponding func,
   struct, or variable was defined, then returns the source file,
   positioned for read at the beginning of the definition.  The
   number of that line is in ypBeginLine.  */
PLUG_API p_file *OpenSource(long index, long isrc);

/* Given a function, YpReparse searches through the source lists to find
   the the source file in which the corresponding func or struct was
   defined.  It then reparses this func or struct definition, replacing
   the corresponding globTab entry with the new definition, and
   returns a pointer to an error message string which begins with "*****"
   if YpReparse failed, otherwise gives the line number and filename
   of the function.  */
PLUG_API char *YpReparse(void *function);

/* ypPrefixes-- the ordered list of directories which Yorick will search
     to locate an include file (./ for CWD), set using YpSetPaths */
PLUG_API char **ypPrefixes;
PLUG_API int nYpPrefixes;

/* Yorick has two search paths for include files.  The first is the
   startup search path, which must allow std.i and other compiled-in
   package include files to be found.  After initialization, the
   normal search path is installed.  The first path is:
      yLaunchDir:ySiteDir:ySiteDir/contrib
   where yLaunchDir is the directory containing the executable, as
   determined at runtime, and ySiteDir is a compiled-in value (set
   by Codger from the value in the Makefile).  If yLaunchDir contains
   a paths.i file (the first file Yorick includes), that file may set
   the interpreted variable Y_SITE in order to change ySiteDir in both
   the first and second search paths.
   The second path is initialized in stdx.i to:
      .:~/Yorick:ySiteDir/include:ySiteDir/contrib
   This can be overridden by an interpreted command to set the search
   path (in, say, custom.i).  Alternatively, yLaunchDir can contain a
   special version of stdx.i.  */
PLUG_API void YpSetPaths(const char *pathlist);    /* also sets yCWD, yHOME */
PLUG_API char *yLaunchDir, *ySiteDir, *yHomeDir, *defaultPath;
PLUG_API char *y_user_dir, *y_gist_dir, *y_home_pkg;

/*--------------------------------------------------------------------------*/

/* PrintFunc prints a string (as in the Y_print built-in function),
   breaking the result into multiple lines if required.  To accomplish this,
   PrintFunc remembers the most recent call to PermitLine and first flushes
   the line to that point.  If s still doesn't fit within printLength
   characters, then PrintFunc prints the first printLength-1, a '\',
   and so on until s is exhausted.  ForceNewline immediately flushes any
   pending line.
   PrintInit initializes PrintFunc (e.g.- PrintInit(YputsOut)).
   The Print and Y_print commands will not print more than
   maxPrintLines lines of output.  */
PLUG_API void PrintFunc(const char *s);
PLUG_API void PermitNewline(int nSpaces);
PLUG_API void ForceNewline(void);
PLUG_API void PrintInit(int (*puts)(char *));
PLUG_API int printLength;     /* forced between 40 and 256 inclusive */
PLUG_API long maxPrintLines;  /* default 5000 */

/* formats used by Print and Y_print (be careful...) */
PLUG_API char *yCharFormat, *yShortFormat, *yIntFormat, *yLongFormat;
PLUG_API char *yFloatFormat, *yDoubleFormat, *yComplexFormat, *yPointerFormat;
PLUG_API void DefaultPrintFormat(int type);  /* type == (1<<T_CHAR)|etc */

PLUG_API char *ScanForEscape(char *s);
PLUG_API int AddEscapeSeq(char *s, int esc);

/*--------------------------------------------------------------------------*/

/* linked list of open files */
typedef struct IOFileLink IOFileLink;
struct IOFileLink {
  struct IOFileLink *next;
  struct IOFileLink **prev;  /* for unlinking only, can't go backwards */
  void *ios;                 /* either TextStream or IOStream */
};

PLUG_API IOFileLink *yTextFiles, *yBinaryFiles;

PLUG_API void AddIOLink(IOFileLink** list, void *ios);
PLUG_API void RemoveIOLink(IOFileLink* list, void *ios);

/*--------------------------------------------------------------------------*/

#endif
