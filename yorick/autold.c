/*
 * $Id: autold.c,v 1.5 2010-02-28 21:52:28 dhmunro Exp $
 * autoload Yorick .i files
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "ydata.h"
#include "yio.h"
#include "defmem.h"
#include "hash.h"
#include "pstdlib.h"

#include <string.h>

extern BuiltIn Y_autoload;

extern void IncludeNow(void);
extern DataBlock *ForceToDB(Symbol *s);

static void auto_syminit(long ifile);

extern autoload_t *new_auto(char *file, long isymbol);
extern void free_auto(void *list);  /* ******* Use Unref(list) ******* */

extern UnaryOp eval_auto;
static UnaryOp print_auto;

Operations auto_ops = {
  &free_auto, T_OPAQUE, 0, T_STRING, "autoload",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &TrueX, &NotX,        /* not and true reversed */
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &eval_auto, &y_setup_func_hack, &GetMemberX, &MatMultX, &print_auto
};

/* Set up a block allocator which grabs space for 64 autoload_t objects
   at a time.  Since autoload_t contains an ops pointer, the alignment
   of a autoload_t must be at least as strict as a void*.  */
static MemryBlock auto_block= {0, 0, sizeof(autoload_t),
                               64*sizeof(autoload_t)};

static HashTable auto_table;
static autoload_t **auto_list = 0;

autoload_t *
new_auto(char *file, long isymbol)
{
  autoload_t *autl = NextUnit(&auto_block);
  autl->references = 0;
  autl->ops = &auto_ops;
  autl->ifile = -1;
  autl->isymbol = isymbol;
  autl->next = 0;

  if (!HashAdd(&auto_table, file, 0L)) {
    HASH_MANAGE(auto_table, autoload_t *, auto_list);
  } else {
    autl->next = auto_list[hashIndex];
  }
  /* auto_list[ifile] does not own a use of the auto pointer */
  auto_list[hashIndex] = autl;
  autl->ifile = hashIndex;

  return autl;
}

void
free_auto(void *vauto)  /* ******* Use Unref(autl) ******* */
{
  autoload_t *autl = vauto;
  long ifile = autl->ifile;
  /* freeing any one autoload for a file frees them all (note recursion) */
  if (ifile >= 0) auto_syminit(ifile);
  else FreeUnit(&auto_block, autl);
  if (autl->ifile >= 0) {
    char msg[120];
    char *afl = auto_table.names[autl->ifile];
    char *ifl = nYpIncludes? ypIncludes[nYpIncludes-1].filename : 0;
    if (ifl && afl) {
      int a=0, i=0;
      while (afl[a]) a++;
      while (ifl[i]) i++;
      for (a--,i-- ; a>=0 && i>=0 && afl[a]==ifl[i] ; a--,i--)
        if (afl[a]=='/' || afl[a]=='\\') return;
      if (i<0) {
        if (a<0 || afl[a]=='/' || afl[a]=='\\') return;
      } else {
        if (a<0 && (ifl[i]=='/' || ifl[i]=='\\')) return;
      }
    }
    /* arguably this should be a full fledged error */
    strcpy(msg, "autoload defined before triggered include: ");
    strncat(msg, globalTable.names[autl->isymbol], 64);
    YWarning(msg);
  }
}

void
eval_auto(Operand *op)
{
  Symbol *owner = op->owner;
  long istack = owner-spBottom;    /* stack may move during include */
  autoload_t *autl = op->value;
  long ifile = autl->ifile;
  long isymbol = autl->isymbol;
  DataBlock *db;

  if (!owner || owner>sp || istack<0) {
    /* owner should be on stack if this called from Eval or Print */
    YError("autoload eval in illegal situation");
    return;
  }

  if (ifile >= 0) {    /* this Eval triggers the autoload include */
    if (!YpPushInclude(auto_table.names[ifile])) {
      YError("missing include file specified in autoload symbol");
      return;
    }
    IncludeNow();
  }

  /* redefine owner to newly autoloaded symbol */
  op->owner = owner = spBottom + istack;
  if (owner->ops == &dataBlockSym) {
    /* always take this branch, and Unref usually does free_auto */
    owner->ops = &intScalar;
    Unref(owner->value.db);
  }
  if (globTab[isymbol].ops == &dataBlockSym) {
    owner->value.db = Ref(globTab[isymbol].value.db);
    owner->ops = globTab[isymbol].ops;
    db = globTab[isymbol].value.db;
  } else {
    owner->value = globTab[isymbol].value;
    owner->ops = globTab[isymbol].ops;
    db = ForceToDB(owner);
  }
  op->ops = db->ops;
  op->value = db;

  /* return to interrupted Eval operation */
  op->ops->Eval(op);
}

void
print_auto(Operand *op)
{
  autoload_t *autl = op->value;
  char *file = (autl->ifile>=0)?auto_table.names[autl->ifile]:"<triggered>";
  char *symb = globalTable.names[autl->isymbol];
  ForceNewline();
  PrintFunc("autoload of: ");
  PrintFunc(symb);
  PrintFunc("    from: ");
  PrintFunc(file);
  ForceNewline();
}

static void
auto_syminit(long ifile)
{
  /* initialize all symbols in auto_list[ifile] to nil [] */
  autoload_t *autl = (ifile>=0)? auto_list[ifile] : 0;
  long isymbol;
  while (autl) {
    auto_list[ifile] = autl->next;
    isymbol = autl->isymbol;
    autl->ifile = -1;
    if (isymbol>=0 && globTab[isymbol].ops==&dataBlockSym &&
        globTab[isymbol].value.db==(DataBlock *)autl) {
      globTab[isymbol].value.db = RefNC(&nilDB);
      Unref(autl);   /* globTab use */
    }
    /* uses may still be on stack, will have had ifile set to -1 */
    autl = auto_list[ifile];
  }
}

void
Y_autoload(int nArgs)
{
  Symbol *s = sp-nArgs+1;
  long isymbol;
  int i;
  char *file = (nArgs>=1)? YGetString(s) : 0;
  char *name, *tail=0;
  if (!file) YError("autoload needs non-0 filename argument");
  /* if file has already been included, ignore (same as require) */
  name = YNameTail(file);
  for (i=0 ; i<sourceTab.nItems ; i++) {
    tail = YNameTail(sourceTab.names[i]);
    if (name && tail && strcmp(tail, name)==0) break;
    p_free(tail);
    tail = 0;
  }
  p_free(name);
  p_free(tail);
  if (i < sourceTab.nItems) return;
  /* first pass checks validity of all arguments */
  for (i=1 ; i<nArgs ; i++) {
    isymbol = YGet_Ref(s+i);
    if (globTab[isymbol].ops != &dataBlockSym ||
        globTab[isymbol].value.db != &nilDB) {
      YError("autoload variable must be nil initially");
    }
  }
  /* second pass creates autoload variables */
  for (i=1 ; i<nArgs ; i++) {
    isymbol = YGet_Ref(s+i);
    globTab[isymbol].value.db = (DataBlock *)new_auto(file, isymbol);
    UnrefNC(&nilDB);
  }
  /* no symbols means to cancel autoload */
  if (nArgs<2 && HashFind(&auto_table, file, 0L))
    auto_syminit(hashIndex);
}
