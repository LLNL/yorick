/*
 * $Id: funcdef.c,v 1.4 2008-10-20 00:46:43 dhmunro Exp $
 * mini-parser converting simple command line to interpreted function
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "pstdlib.h"
#include "ydata.h"
#include <errno.h>

extern BuiltIn Y_funcdef;

extern VMaction PushVariable, PushReference, PushLong, PushDouble, PushString;
extern VMaction Eval, DropTop, PushNil, Return;

typedef struct yfd_tmp_t yfd_tmp_t;
struct yfd_tmp_t {
  Symbol *ctab;
  Instruction *code;
};
static void yfd_tmp_free(void *vtmp);
static void
yfd_tmp_free(void *vtmp)
{
  yfd_tmp_t *tmp = vtmp;
  if (tmp) {
    if (tmp->ctab) {
      Symbol *s = tmp->ctab;
      while (s->ops) {
        if (s->ops == &dataBlockSym) {
          s->ops = &intScalar;
          Unref(s->value.db);
        }
        s++;
      }
      p_free(tmp->ctab), tmp->ctab = 0;
    }
    if (tmp->code) p_free(tmp->code), tmp->code = 0;
  }
}

void
Y_funcdef(int argc)
{
  char *q = ygets_q(0);
  if (argc!=1 || !q)
    y_error("funcdef accepts only single scalar string argument");
  ypush_func(q);
}

int
ypush_func(char *line)
{
  char c, *line0;
  int pc = 0, mxp = 0, nc = 0, mxc = 0;
  yfd_tmp_t *tmp = 0;
  Array *qa;
  int nargs = -1;

  if (!line) goto parserr;

  for (;;) {
    while (line[0]==' ' || line[0]=='\t') line++;
    c = line[0];
    if (c>='A' && (c<='Z' || c=='_' || (c>='a' && c<='z'))) {
      /* token is a symbol */
      line0 = line;
      do {
        c = *(++line);
      } while (c>='0' &&
               (c<='9' ||
                (c>='A' || (c<='Z' || c=='_' || (c>='a' && c<='z')))));
      if (!tmp) {
        tmp = ypush_scratch(sizeof(yfd_tmp_t), yfd_tmp_free);
        tmp->ctab = 0;
        mxp = 32;
        tmp->code = p_malloc(sizeof(Instruction)*mxp);
      } else if (pc+10 >= mxp) {
        mxp += mxp;
        tmp->code = p_realloc(tmp->code, sizeof(Instruction)*mxp);
      }
      tmp->code[pc].Action = pc? &PushReference : &PushVariable;
      pc++;
      tmp->code[pc++].index = yget_global(line0, line-line0);

    } else {
      if (!tmp) goto parserr;   /* first token must be a symbol */
      if (!c) break;
      if (pc+10 >= mxp) {
        mxp += mxp;
        tmp->code = p_realloc(tmp->code, sizeof(Instruction)*mxp);
      }
      if (nc+1 >= mxc) {
        int i = mxc;
        mxc = mxc? mxc+mxc : 8;
        tmp->ctab = p_realloc(tmp->ctab, sizeof(Symbol)*(mxc+1));
        while (i <= mxc) tmp->ctab[i++].ops = 0;
      }

      if (c=='"') {
        /* token is quoted string */
        char *c0, *c1;
        line0 = ++line;
        for (;;) {
          while (line[0] && line[0]!='\\' && line[0]!='"') line++;
          if (line[0]!='\\') break;
          line++;
          if (line[0]) line++;
        }
        if (line[0]!='"') goto parserr;
        qa = NewArray(&stringStruct, (Dimension *)0);
        qa->value.q[0] = c0 = c1 =
          p_strncat(0, (line>line0)? line0 : "", line-line0);
        line++;
        while (c1[0]) {
          if (c1[0] == '\\') c1++;
          if (c1 != c0) c0[0] = c1[0];
          c0++, c1++;
        }
        if (c1 != c0) c0[0] = '\0';
        tmp->ctab[nc].ops = &dataBlockSym;
        tmp->ctab[nc].value.db = (DataBlock *)qa;
        tmp->code[pc++].Action = &PushString;
        tmp->code[pc++].index = nc++;

      } else if ((c>='0' && c<='9') || c=='-' || c=='.' || c=='+') {
        /* token is a number */
        /* avoid buggy strtol and strtod functions on some platforms,
         * which do not correctly set the endptr in all situations
         */
        int real = 0;
        if (c=='+') line++;
        line0 = line;
        if (c=='-') line++;
        while (line[0]>='0' && line[0]<='9') line++;
        if (line[0]=='.') {
          real = 1;
          do { line++; } while (line[0]>='0' && line[0]<='9');
        }
        if (line == line0+(c=='-')+real) goto parserr;
        if (line[0]=='e' || line[0]=='E') {
          real = 1;
          line++;
          if (c=='+' || c=='-') line++;
          while (line[0]>='0' && line[0]<='9') line++;
        }
        /* now that line is properly past the end of number, convert it */
        if (real) {
          tmp->ctab[nc].ops = &doubleScalar;
          errno = 0;
          tmp->ctab[nc].value.d = strtod(line0, 0);
          if (errno) {
            tmp->ctab[nc].value.d = 0.;
            goto parserr;
          }
          tmp->code[pc++].Action = &PushDouble;
          tmp->code[pc++].index = nc++;
        } else {
          tmp->ctab[nc].ops = &longScalar;
          tmp->ctab[nc].value.l = strtol(line0, 0, 10);
          tmp->code[pc++].Action = &PushLong;
          tmp->code[pc++].index = nc++;
        }

      } else {
        goto parserr;
      }
    }

    nargs++;

    /* find and skip delimiter between arguments */
    line0 = line;
    while (line[0]==' ' || line[0]=='\t') line++;
    /* if (line[0]==',') line++;  optional comma between arguments */
    if (line[0] && line==line0) goto parserr; /* missing delimiter */
  }

  /* shorten constant table (leave extra entry for yfd_tmp_free) */
  if (tmp->ctab) {
    long index;
    int i;
    tmp->ctab = p_realloc(tmp->ctab, sizeof(Symbol)*(nc+1));
    for (i=3 ; i<pc ; i+=2) {
      if (tmp->code[i-1].Action == &PushReference) continue;
      index = tmp->code[i].index;
      tmp->code[i].constant = &tmp->ctab[index];
    }
  }

  /* generate the end of the function */
  tmp->code[pc++].Action = &Eval;
  tmp->code[pc++].count = nargs;
  tmp->code[pc++].Action = &DropTop;
  tmp->code[pc++].Action = &PushNil;
  tmp->code[pc++].Action = &Return;
  tmp->code[pc++].Action = 0;
  tmp->code[pc].index = pc+1;
  /* NewFunction moves following code to beginning */
  tmp->code[++pc].index = yget_global("*anonymous*", 0L);

  {
    Symbol *ctab = tmp->ctab;
    DataBlock *dbtmp = sp->value.db;
    sp->value.db =
      (DataBlock *)NewFunction(ctab, nc, 0, 0, 0, 0, nargs+1, tmp->code, pc);
    tmp->ctab = 0;  /* new function takes over reference to this table */
    Unref(dbtmp);
  }
  return 0;

 parserr:
  if (tmp) yarg_drop(1);
  PushDataBlock(RefNC(&nilDB));
  return 1;
}
